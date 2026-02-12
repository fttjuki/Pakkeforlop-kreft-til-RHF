# ==============================================================================
# 03_lopenr.R
# Bestillingsfil + returfil (_lnr.csv) + validering
# ==============================================================================

# Lager en bestillingsliste med UNIKE NPRId.
build_nprid_request <- function(all_rows) {
  all_rows %>%
    # Tar bare unike NPRId (én per person).
    dplyr::distinct(NPRId) %>%
    # Fjerner ugyldige NPRId (NA, tom, og "feil-tekst").
    dplyr::filter(!is.na(NPRId), NPRId != "", NPRId != "NPRId", NPRId != "NULL") %>%
    # Lager en ekstra tom kolonne (ofte fordi mottaker forventer 2 kolonner).
    dplyr::mutate(Dummy = "")
}

# Skriver bestillingsfilen til disk (for å sende ut og få løpenummer tilbake).
write_nprid_request_file <- function(USER, nprid_request, suffix) {
  # Sørg for at app-mappen finnes.
  ensure_dir(USER$app_dir)

  # Bygg full filsti til bestillingsfilen.
  out_path <- request_file_path(USER, suffix)

  # Skriv til CSV med semikolon, og tom tekst for NA.
  readr::write_delim(nprid_request, out_path, delim = ";", na = "")

  # Logg hvor filen ble lagret og hvor mange rader den har.
  log_info(USER, "Lagret bestillingsfil: %s (rader=%s)", out_path, nrow(nprid_request))

  # Skriv også til en "runbook"/logg (rb_add må være definert et annet sted).
  rb_add(paste0("Wrote request file: ", out_path, " \n rows=", nrow(nprid_request)))

  # Returner filsti (praktisk for videre bruk).
  out_path
}

# Leser returfilen som skal inneholde mapping: NPRId -> lopenr.
read_lopenr_file <- function(USER, suffix) {
  # Filsti til returfilen.
  path <- lnr_file_path(USER, suffix)

  # Hvis den ikke finnes, stopp (kan ikke fortsette uten løpenummer).
  if (!file.exists(path)) stop_user("Mangler returfil med løpenr: %s", path)

  # Logg at vi leser filen.
  rb_add(paste0("Reading lopenr file: ", path))

  # Les filen med semikolon og med faste kolonnenavn.
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
    # Rens: fjern \r, trim whitespace, og gjør tom streng til NA.
    dplyr::mutate(
      NPRId = stringr::str_trim(stringr::str_replace_all(NPRId, "\\r", "")) %>% na_if(""),
      lopenr = stringr::str_trim(stringr::str_replace_all(lopenr, "\\r", "")) %>%
        stringr::str_replace_all(";", "") %>% na_if("")
    )
}

# Sjekker at alle NPRId i data har fått et løpenummer i returfilen.
validate_lopenr_map <- function(USER, all_rows, id_map, suffix) {

  # Lag liste over NPRId vi faktisk trenger (unike og gyldige).
  needed <- all_rows %>%
    dplyr::filter(!is.na(NPRId), NPRId != "", NPRId != "NPRId", NPRId != "NULL") %>%
    dplyr::distinct(NPRId) %>%
    dplyr::mutate(NPRId = stringr::str_trim(as.character(NPRId)))

  # Hvis returfilen er tom, stopp.
  if (nrow(id_map) == 0) stop_user("Returfilen er tom: %s", lnr_file_path(USER, suffix))

  # Sjekk duplikater: samme NPRId skal ikke forekomme flere ganger i returfil.
  dup <- id_map %>% dplyr::filter(!is.na(NPRId)) %>% dplyr::count(NPRId) %>% dplyr::filter(n > 1)
  if (nrow(dup) > 0) {
    stop_user("Duplikate NPRId i returfil. Eksempel: %s", paste(head(dup$NPRId, 10), collapse = ", "))
  }

  # Finn NPRId som mangler løpenummer ved å slå opp (left_join).
  missing_map <- needed %>%
    dplyr::left_join(id_map, by = "NPRId") %>%
    # Mangler hvis lopenr er NA, tom eller "NULL".
    dplyr::filter(is.na(lopenr) | lopenr == "" | lopenr == "NULL")

  # Hvis noen mangler, skriv QC-fil og stopp.
  if (nrow(missing_map) > 0) {
    # Sørg for at output-mappen finnes.
    ensure_dir(USER$out_dir)

    # Lag filnavn for QC-listen.
    miss_path <- file.path(USER$out_dir, paste0("QC_missing_lopenr_NPRId_", suffix, ".csv"))

    # Skriv ut listen over manglende NPRId.
    readr::write_csv(missing_map, miss_path)

    # Logg og stopp.
    rb_add(paste0("STOP: missing lopenr for ", nrow(missing_map), " NPRId. See: ", miss_path))
    stop_user("STOPP: %s NPRId mangler løpenummer. Se liste: %s", nrow(missing_map), miss_path)
  }

  # Hvis alt ok, logg OK.
  log_info(USER, "OK: Alle NPRId har løpenr (%s)", suffix)
  rb_add(paste0("Lopenr validation OK \n suffix=", suffix, " \n NPRId count=", nrow(needed)))

  # Returner TRUE usynlig (brukes som en “ok”-indikator).
  invisible(TRUE)
}