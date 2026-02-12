# ==============================================================================
# 05_delivery.R
# Lager utlevering, skriver filer som CSV (UTF-8 med BOM) og Excel, og logger
# ------------------------------------------------------------------------------
# Hensikt (i enkel norsk):
# - Vi tar "rapport-data" (fra PowerBI-filer) og legger til løpenummer (lopenr)
#   ved å koble NPRId mot returfilen (id_map).
# - Vi fyller eventuelt manglende kommune/bydel (geo_fixes).
# - Vi sørger for at alle kolonner som må være med (FINAL_COLS) finnes.
# - Vi sorterer kolonnene i riktig rekkefølge.
# - Vi skriver ut ferdige leveransefiler: CSV + Excel (hvis mulig).
# ==============================================================================

coerce_to_integer <- function(x) {
  # Denne funksjonen prøver å gjøre en kolonne om til heltall (integer).
  # Hvorfor: Noen felt må være numeriske i leveransen (f.eks. institusjonId, lopenr).
  # Men input kan komme som tekst, med mellomrom eller tomme verdier.

  # Gjør alt om til tekst først (trygt), og fjern mellomrom i starten/slutten.
  x2 <- stringr::str_trim(as.character(x))

  # Tom streng ("") skal behandles som NA (mangler).
  x2 <- dplyr::na_if(x2, "")

  # as.integer kan gi warnings hvis noe ikke kan konverteres.
  # suppressWarnings skjuler disse warningene (vi logger heller i runbook).
  suppressWarnings(as.integer(x2))
}

log_numeric_conversion <- function(colname, before, after, region) {
  # Denne funksjonen logger hvor mange verdier som "forsvant" (ble NA)
  # etter numerisk konvertering.
  # Hvorfor: Det er nyttig QC (kvalitetskontroll) for å se om data ble ødelagt.

  # Teller hvor mange ikke-tomme verdier vi hadde før.
  before_nonempty <- sum(!is.na(before) & as.character(before) != "")

  # Teller hvor mange som ble NA etter konvertering, men var ikke-tomme før.
  after_na <- sum(is.na(after) & !is.na(before) & as.character(before) != "")

  # Skriv en linje i runbook-loggen.
  rb_add(paste0("Numerisk konvertering ", colname, " (", region, "): ",
                "ikke-tom før=", before_nonempty, ", NA etter=", after_na))
}

# Skriv CSV med UTF-8 BOM for best mulig Excel-kompatibilitet på Windows.
# BOM gjør at Excel ofte automatisk velger riktig tegnsett (æ/ø/å).
write_delim_utf8_bom <- function(df, path, delim = ";") {
  # Hvorfor egen funksjon?
  # - Excel på Windows kan noen ganger tolke UTF-8 feil hvis filen mangler BOM.
  # - Ved å skrive BOM først øker sjansen for korrekt tegnsett.

  # 1) Skriv BOM først
  # Åpner filen i "wb" (write binary) fordi BOM er en byte-sekvens.
  con <- file(path, open = "wb")
  # Sørger for at filen lukkes uansett om noe feiler.
  on.exit(close(con), add = TRUE)
  # Skriver BOM-tegnet.
  writeChar("\ufeff", con, eos = NULL, useBytes = TRUE)

  # 2) Skriv selve CSV-innholdet som UTF-8
  # Vi åpner ny connection i append-modus ("ab") for å ikke overskrive BOM.
  con2 <- file(path, open = "ab")
  # Sørger for at con2 også lukkes.
  on.exit(close(con2), add = TRUE)

  # write_delim skriver data som CSV med valgt delimiter (ofte semikolon i Norge).
  readr::write_delim(df, con2, delim = delim, na = "")
}

ensure_final_columns_exist <- function(USER, df, region, suffix) {
  # Denne funksjonen sjekker at df inneholder alle kolonner vi krever (FINAL_COLS).
  # Hvorfor: Inputfiler kan variere litt. Leveransen skal likevel ha fast struktur.

  # Finn hvilke kolonner som mangler i df.
  missing <- setdiff(FINAL_COLS, names(df))

  # Hvis noen mangler...
  if (length(missing) > 0) {
    # Logg advarsel (slik at vi vet at input ikke hadde alt).
    log_warn(USER, "Mangler %s kolonner i %s (%s). Lager tomme kolonner: %s",
             length(missing), region, suffix, paste(missing, collapse = ", "))

    # Lag tomme kolonner (NA) for hver manglende kolonne.
    for (m in missing) df[[m]] <- NA_character_

    # Skriv også en QC-fil som dokumentasjon på hvilke kolonner som manglet.
    qc_path <- file.path(USER$out_dir, paste0("QC_missing_columns_", region, "_", suffix, ".csv"))
    readr::write_csv(tibble::tibble(region=region, suffix=suffix, missing_column=missing), qc_path)
    rb_add(paste0("QC missing-columns written: ", qc_path))
  }

  # Returner df (nå med alle kolonner).
  df
}

make_delivery <- function(USER, report_df, id_map, geo_fixes, region, suffix) {
  # Denne funksjonen lager "ferdig leveranse-dataframe" for én region.

  # 1) Start med report_df (renset PowerBI-data) og filtrer bort ugyldige NPRId.
  # 2) Koble på løpenummer ved å join'e mot id_map (NPRId -> lopenr).
  df <- report_df %>%
    dplyr::filter(!is.na(NPRId), NPRId != "", NPRId != "NPRId", NPRId != "NULL") %>%
    dplyr::left_join(id_map, by = "NPRId")

  # Hvis År-kolonnen mangler eller er tom, prøver vi å lage den fra StartDato.
  # Hvorfor: År brukes ofte i rapportering, og kan av og til mangle i input.
  if (!"År" %in% names(df) ||

      all(is.na(df$År) |

      df$År == "")) {
    # parse_any_date prøver å tolke StartDato fra mange mulige formater.
    sd <- parse_any_date(df$StartDato)
    # Hvis StartDato kan tolkes, bruk year(sd). Ellers behold eksisterende År.
    df$År <- ifelse(!is.na(sd), as.character(lubridate::year(sd)), df$År)
  }

  # Hvis bruker har aktivert "fill_missing_kommune", fyll KommuneNr/BydelNr.
  # Hvorfor: Noen rader mangler kommune/bydel i input, og vi vil komplettere.
  if (isTRUE(USER$fill_missing_kommune)) {
    df <- df %>%
      # Koble på geo_fixes (oppslag fra databasen) basert på NPRId.
      dplyr::left_join(geo_fixes, by = "NPRId") %>%
      dplyr::mutate(
        # Hvis KommuneNr mangler (NA eller tom), bruk komnrhjem2 fra geo_fixes.
        KommuneNr = dplyr::if_else(is.na(KommuneNr) |

        KommuneNr == "", dplyr::coalesce(komnrhjem2, ""), KommuneNr),

        # Hvis kommune vi satte er den samme som komnrhjem2 og BydelNr mangler,
        # kan vi også fylle bydel fra bydel2.
        BydelNr = dplyr::if_else(KommuneNr == komnrhjem2 & is.na(BydelNr), bydel2, BydelNr)
      ) %>%
      # Rydd bort hjelpekolonnene etter at vi har brukt dem.
      dplyr::select(-dplyr::any_of(c("komnrhjem2","bydel2","aar")))
  }

  # Sørg for at alle nødvendige kolonner finnes.
  df <- ensure_final_columns_exist(USER, df, region, suffix)

  # Velg kolonnene i riktig rekkefølge (fast leveranseformat).
  df <- df %>% dplyr::select(dplyr::all_of(FINAL_COLS))

  # Numeriske krav
  # Noen mottakere/valideringer forventer at disse er integer, ikke tekst.

  if ("institusjonId" %in% names(df)) {
    # Ta vare på "før" for logging/QC.
    before <- df$institusjonId
    # Konverter.
    df$institusjonId <- coerce_to_integer(df$institusjonId)
    # Logg hvor mange som ble NA etter konvertering.
    log_numeric_conversion("institusjonId", before, df$institusjonId, region)
  }

  if ("lopenr" %in% names(df)) {
    # Samme for løpenummer.
    before <- df$lopenr
    df$lopenr <- coerce_to_integer(df$lopenr)
    log_numeric_conversion("lopenr", before, df$lopenr, region)
  }

  # Returner ferdig df.
  df
}

write_delivery_csv <- function(USER, df, region, suffix) {
  # Skriver leveransen som CSV.

  # Sørg for at output-mappen finnes.
  ensure_dir(USER$out_dir)

  # Bygg filsti basert på region og suffix.
  out_path <- delivery_file_path_csv(USER, region, suffix)

  # CSV skrives som UTF-8 med BOM (for Excel-kompatibilitet).
  write_delim_utf8_bom(df, out_path, delim = ";")

  # Logg for sporbarhet.
  log_info(USER, "Lagret CSV-utlevering (UTF-8 BOM): %s (rader=%s)", out_path, nrow(df))
  rb_add(paste0("Wrote delivery CSV (UTF-8 BOM): ", out_path, " \n rows=", nrow(df)))

  # Returner filsti.
  out_path
}

write_delivery_xlsx <- function(USER, df, region, suffix) {
  # Skriver leveransen som Excel hvis vi har riktig pakke installert.

  # Sørg for at output-mappen finnes.
  ensure_dir(USER$out_dir)

  # Bygg filsti for Excel.
  out_path <- delivery_file_path_xlsx(USER, region, suffix)

  # Prøv først openxlsx (vanlig og fleksibel).
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    openxlsx::write.xlsx(df, out_path, overwrite = TRUE)
    rb_add(paste0("Wrote delivery Excel (openxlsx): ", out_path, " \n rows=", nrow(df)))
    log_info(USER, "Lagret Excel-utlevering: %s", out_path)
    return(out_path)
  }

  # Hvis openxlsx ikke finnes, prøv writexl (enklere alternativ).
  if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(df, out_path)
    rb_add(paste0("Wrote delivery Excel (writexl): ", out_path, " \n rows=", nrow(df)))
    log_info(USER, "Lagret Excel-utlevering: %s", out_path)
    return(out_path)
  }

  # Hvis ingen Excel-pakker finnes, logg advarsel og returner NA.
  log_warn(USER, "Kan ikke skrive Excel: verken 'openxlsx' eller 'writexl' er installert.")
  rb_add("WARNING: Excel output skipped (openxlsx/writexl not installed)")
  NA_character_
}

write_delivery <- function(USER, df, region, suffix) {
  # Denne wrapperen skriver både CSV og Excel og returnerer filstier.

  csv_path <- write_delivery_csv(USER, df, region, suffix)
  xlsx_path <- write_delivery_xlsx(USER, df, region, suffix)

  # Returnerer en liten liste med begge filene.
  list(csv = csv_path, xlsx = xlsx_path)
}

qc_simple_summary <- function(final_list, suffix) {
  # Lager en enkel QC-oppsummering for alle regioner.
  # final_list er en liste der hver entry er et ferdig df for en region.

  purrr::imap_dfr(final_list, ~ tibble::tibble(
    # .y er navnet i listen (region/RHF).
    RHF = .y,
    # Legg ved suffix for å vite hvilken måned/kjøring QC gjelder.
    suffix = suffix,
    # Antall rader i leveransen.
    rows = nrow(.x),
    # Teller hvor mange rader som fortsatt mangler kommune etter all fylling.
    missing_kommune = sum(is.na(.x$KommuneNr) |

    .x$KommuneNr == "")
  ))
}