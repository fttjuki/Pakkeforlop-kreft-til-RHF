# ==============================================================================
# 03_lopenr.R
# Bestillingsfil + returfil (_lnr.csv) + validering
# ============================================================================== 

build_nprid_request <- function(all_rows) {
  all_rows %>%
    dplyr::distinct(NPRId) %>%
    dplyr::filter(!is.na(NPRId), NPRId != "", NPRId != "NPRId", NPRId != "NULL") %>%
    dplyr::mutate(Dummy = "")
}

write_nprid_request_file <- function(USER, nprid_request, suffix) {
  ensure_dir(USER$app_dir)
  out_path <- request_file_path(USER, suffix)
  readr::write_delim(nprid_request, out_path, delim = ";", na = "")
  log_info(USER, "Lagret bestillingsfil: %s (rader=%s)", out_path, nrow(nprid_request))
  rb_add(paste0("Wrote request file: ", out_path, " | rows=", nrow(nprid_request)))
  out_path
}

read_lopenr_file <- function(USER, suffix) {
  path <- lnr_file_path(USER, suffix)
  if (!file.exists(path)) stop_user("Mangler returfil med løpenr: %s", path)
  rb_add(paste0("Reading lopenr file: ", path))

  readr::read_delim(
    path,
    delim = ";",
    col_names = c("NPRId", "lopenr"),
    col_types = readr::cols(.default = readr::col_character()),
    na = character(),
    show_col_types = FALSE,
    progress = FALSE,
    trim_ws = TRUE
  ) %>%
    dplyr::mutate(
      NPRId  = stringr::str_trim(stringr::str_replace_all(NPRId, "\r", "")) %>% na_if(""),
      lopenr = stringr::str_trim(stringr::str_replace_all(lopenr, "\r", "")) %>%
        stringr::str_replace_all(";", "") %>% na_if("")
    )
}

validate_lopenr_map <- function(USER, all_rows, id_map, suffix) {

  needed <- all_rows %>%
    dplyr::filter(!is.na(NPRId), NPRId != "", NPRId != "NPRId", NPRId != "NULL") %>%
    dplyr::distinct(NPRId) %>%
    dplyr::mutate(NPRId = stringr::str_trim(as.character(NPRId)))

  if (nrow(id_map) == 0) stop_user("Returfilen er tom: %s", lnr_file_path(USER, suffix))

  dup <- id_map %>% dplyr::filter(!is.na(NPRId)) %>% dplyr::count(NPRId) %>% dplyr::filter(n > 1)
  if (nrow(dup) > 0) {
    stop_user("Duplikate NPRId i returfil. Eksempel: %s", paste(head(dup$NPRId, 10), collapse = ", "))
  }

  missing_map <- needed %>%
    dplyr::left_join(id_map, by = "NPRId") %>%
    dplyr::filter(is.na(lopenr) | lopenr == "" | lopenr == "NULL")

  if (nrow(missing_map) > 0) {
    ensure_dir(USER$out_dir)
    miss_path <- file.path(USER$out_dir, paste0("QC_missing_lopenr_NPRId_", suffix, ".csv"))
    readr::write_csv(missing_map, miss_path)
    rb_add(paste0("STOP: missing lopenr for ", nrow(missing_map), " NPRId. See: ", miss_path))
    stop_user("STOPP: %s NPRId mangler løpenummer. Se liste: %s", nrow(missing_map), miss_path)
  }

  log_info(USER, "OK: Alle NPRId har løpenr (%s)", suffix)
  rb_add(paste0("Lopenr validation OK | suffix=", suffix, " | NPRId count=", nrow(needed)))
  invisible(TRUE)
}
