# ==============================================================================
# 05_delivery.R
# Lager utlevering, skriver filer som CSV (UTF-8 med BOM) og Excel, og logger
# ============================================================================== 

coerce_to_integer <- function(x) {
  x2 <- stringr::str_trim(as.character(x))
  x2 <- dplyr::na_if(x2, "")
  suppressWarnings(as.integer(x2))
}

log_numeric_conversion <- function(colname, before, after, region) {
  before_nonempty <- sum(!is.na(before) & as.character(before) != "")
  after_na <- sum(is.na(after) & !is.na(before) & as.character(before) != "")
  rb_add(paste0("Numerisk konvertering ", colname, " (", region, "): ",
                "ikke-tom før=", before_nonempty, ", NA etter=", after_na))
}

# Skriv CSV med UTF-8 BOM for best mulig Excel-kompatibilitet på Windows.
# BOM gjør at Excel ofte automatisk velger riktig tegnsett (æ/ø/å).
write_delim_utf8_bom <- function(df, path, delim = ";") {
  # 1) Skriv BOM først
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeChar("\ufeff", con, eos = NULL, useBytes = TRUE)

  # 2) Skriv selve CSV-innholdet som UTF-8
  #    Vi åpner ny connection i append-modus for å unngå å overskrive BOM.
  con2 <- file(path, open = "ab")
  on.exit(close(con2), add = TRUE)
  readr::write_delim(df, con2, delim = delim, na = "")
}

ensure_final_columns_exist <- function(USER, df, region, suffix) {
  missing <- setdiff(FINAL_COLS, names(df))

  if (length(missing) > 0) {
    log_warn(USER, "Mangler %s kolonner i %s (%s). Lager tomme kolonner: %s",
             length(missing), region, suffix, paste(missing, collapse = ", "))
    for (m in missing) df[[m]] <- NA_character_

    qc_path <- file.path(USER$out_dir, paste0("QC_missing_columns_", region, "_", suffix, ".csv"))
    readr::write_csv(tibble::tibble(region=region, suffix=suffix, missing_column=missing), qc_path)
    rb_add(paste0("QC missing-columns written: ", qc_path))
  }

  df
}

make_delivery <- function(USER, report_df, id_map, geo_fixes, region, suffix) {

  df <- report_df %>%
    dplyr::filter(!is.na(NPRId), NPRId != "", NPRId != "NPRId", NPRId != "NULL") %>%
    dplyr::left_join(id_map, by = "NPRId")

  if (!"År" %in% names(df) || all(is.na(df$År) | df$År == "")) {
    sd <- parse_any_date(df$StartDato)
    df$År <- ifelse(!is.na(sd), as.character(lubridate::year(sd)), df$År)
  }

  if (isTRUE(USER$fill_missing_kommune)) {
    df <- df %>%
      dplyr::left_join(geo_fixes, by = "NPRId") %>%
      dplyr::mutate(
        KommuneNr = dplyr::if_else(is.na(KommuneNr) | KommuneNr == "", dplyr::coalesce(komnrhjem2, ""), KommuneNr),
        BydelNr   = dplyr::if_else(KommuneNr == komnrhjem2 & is.na(BydelNr), bydel2, BydelNr)
      ) %>%
      dplyr::select(-dplyr::any_of(c("komnrhjem2","bydel2","aar")))
  }

  df <- ensure_final_columns_exist(USER, df, region, suffix)
  df <- df %>% dplyr::select(dplyr::all_of(FINAL_COLS))

  # Numeriske krav
  if ("institusjonId" %in% names(df)) {
    before <- df$institusjonId
    df$institusjonId <- coerce_to_integer(df$institusjonId)
    log_numeric_conversion("institusjonId", before, df$institusjonId, region)
  }

  if ("lopenr" %in% names(df)) {
    before <- df$lopenr
    df$lopenr <- coerce_to_integer(df$lopenr)
    log_numeric_conversion("lopenr", before, df$lopenr, region)
  }

  df
}

write_delivery_csv <- function(USER, df, region, suffix) {
  ensure_dir(USER$out_dir)
  out_path <- delivery_file_path_csv(USER, region, suffix)

  # CSV skrives som UTF-8 med BOM
  write_delim_utf8_bom(df, out_path, delim = ";")

  log_info(USER, "Lagret CSV-utlevering (UTF-8 BOM): %s (rader=%s)", out_path, nrow(df))
  rb_add(paste0("Wrote delivery CSV (UTF-8 BOM): ", out_path, " | rows=", nrow(df)))
  out_path
}

write_delivery_xlsx <- function(USER, df, region, suffix) {
  ensure_dir(USER$out_dir)
  out_path <- delivery_file_path_xlsx(USER, region, suffix)

  if (requireNamespace("openxlsx", quietly = TRUE)) {
    openxlsx::write.xlsx(df, out_path, overwrite = TRUE)
    rb_add(paste0("Wrote delivery Excel (openxlsx): ", out_path, " | rows=", nrow(df)))
    log_info(USER, "Lagret Excel-utlevering: %s", out_path)
    return(out_path)
  }

  if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(df, out_path)
    rb_add(paste0("Wrote delivery Excel (writexl): ", out_path, " | rows=", nrow(df)))
    log_info(USER, "Lagret Excel-utlevering: %s", out_path)
    return(out_path)
  }

  log_warn(USER, "Kan ikke skrive Excel: verken 'openxlsx' eller 'writexl' er installert.")
  rb_add("WARNING: Excel output skipped (openxlsx/writexl not installed)")
  NA_character_
}

write_delivery <- function(USER, df, region, suffix) {
  csv_path <- write_delivery_csv(USER, df, region, suffix)
  xlsx_path <- write_delivery_xlsx(USER, df, region, suffix)
  list(csv = csv_path, xlsx = xlsx_path)
}

qc_simple_summary <- function(final_list, suffix) {
  purrr::imap_dfr(final_list, ~ tibble::tibble(
    RHF = .y,
    suffix = suffix,
    rows = nrow(.x),
    missing_kommune = sum(is.na(.x$KommuneNr) | .x$KommuneNr == "")
  ))
}
