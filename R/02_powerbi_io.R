# ==============================================================================
# 02_powerbi_io.R
# Leser og renser PowerBI CSV, standardiserer kolonnenavn, og logger radtall
# ==============================================================================

# Finner om CSV-filen bruker semikolon eller komma som skilletegn.
detect_delim <- function(path) {
  # Leser bare første linje (header).
  header <- readLines(path, n = 1, warn = FALSE)
  # Fjerner evt. BOM-tegn (kan gi rare tegn i første kolonnenavn).
  header <- sub("^\\ufeff", "", header)

  # Teller antall felt hvis vi splitter på semikolon.
  semi_fields <- length(strsplit(header, ";")[[1]])
  # Teller antall felt hvis vi splitter på komma.
  comma_fields <- length(strsplit(header, ",")[[1]])

  # Hvis semikolon gir flest felt, antar vi semikolon, ellers komma.
  if (semi_fields > comma_fields) ";" else ","
}

# Fjerner kolonner som er helt tomme (kun NA eller tom tekst).
drop_fully_empty_columns <- function(USER, df) {
  # Finner navn på kolonner der ALLE verdier er tomme/NA.
  empty_cols <- names(df)[sapply(df, function(x) all(is.na(x) | x == ""))]

  # Hvis vi fant noen tomme kolonner...
  if (length(empty_cols) > 0) {
    # Logger hvilke kolonner som droppes.
    log_info(USER, "Dropper helt tomme kolonner: %s", paste(empty_cols, collapse = ", "))
    rb_add(paste0("Dropped fully empty columns: ", paste(empty_cols, collapse = ", ")))
    # Fjerner kolonnene fra datasettet.
    df <- df %>% dplyr::select(-dplyr::all_of(empty_cols))
  }

  # Returnerer ryddet df.
  df
}

# Fjerner kolonne nr 13 (ofte "M") hvis den finnes og er helt tom.
drop_column_M_if_present <- function(USER, df, file_label = "") {
  # Kjør bare hvis bruker har slått på dette og df har minst 13 kolonner.
  if (isTRUE(USER$drop_column_M) && ncol(df) >= 13) {

    # Henter kolonne 13.
    col13 <- df[[13]]
    # Sjekker om hele kolonnen er tom.
    is_empty <- all(is.na(col13) | col13 == "")

    # Hvis den er tom, fjern den.
    if (is_empty) {
      removed_name <- names(df)[13]
      df <- df[, -13, drop = FALSE]
      log_info(USER, "Fjernet kolonne M (#13) '%s' (tom) i %s", removed_name, file_label)
      rb_add(paste0("Dropped column M (#13): ", removed_name, " \n file=", file_label))
    } else {
      # Hvis ikke tom, behold og varsle.
      log_warn(USER, "Kolonne M (#13) er IKKE tom i %s. Den ble IKKE fjernet.", file_label)
      rb_add(paste0("Kept column M (#13) because it had values \n file=", file_label))
    }
  }

  # Returnerer df (endret eller uendret).
  df
}

# Standardiserer kolonnenavn slik at de blir like uansett PowerBI-variant.
standardize_powerbi_names <- function(USER, df, file_label = "") {

  # Lager en "nøkkel" av kolonnenavn: små bokstaver, uten spesialtegn.
  make_key <- function(x) {
    x <- stringr::str_replace_all(x, "\u00A0", " ")  # bytter NBSP med vanlig space
    x <- stringr::str_trim(x)                       # fjerner spaces i start/slutt
    x_ascii <- suppressWarnings(iconv(x, from = "", to = "ASCII//TRANSLIT")) # prøver å gjøre æøå -> a o a
    x_ascii[is.na(x_ascii)] <- x[is.na(x_ascii)]    # hvis iconv feiler, bruk original
    x_ascii %>% tolower() %>% stringr::str_replace_all("[^a-z0-9]", "")      # bare a-z og 0-9
  }

  # Lager nøkkel for alle kolonnenavn i df.
  keys <- make_key(names(df))

  # Oppslagsliste: fra "nøkkel" -> standard kolonnenavn vi vil ha.
  key_map <- c(
    "institusjonid"="institusjonId",
    "institusjon"="institusjon",
    "institusjonnavn"="institusjon",
    "reshidfagenhet"="reshIdFagEnhet",
    "fagenhet"="Fagenhet",
    "enhetnavn"="Fagenhet",
    "nprid"="NPRId",
    "kommunenr"="KommuneNr",
    "kommune"="KommuneNr",
    "bydelnr"="BydelNr",
    "bydel"="BydelNr",
    "fodselsdato"="FødselsDato",
    "dodsdato"="DødsDato",
    "doddato"="DødsDato",
    "pfinstansid"="pfInstansId",
    "forlop"="Forløp",
    "pakkeforlop"="Pakkeforløp",
    "antinst"="AntInst",
    "anthf"="AntHf",
    "ar"="År",
    "startar"="År",
    "startdato"="StartDato",
    "utrstartdato"="UtrStartDato",
    "ainst"="Ainst",
    "sinst"="Sinst",
    "kliniskbeslutn"="KliniskBeslutn",
    "typebeslutn"="TypeBeslutn",
    "cinst"="Cinst",
    "startbeh"="StartBeh",
    "typebeh"="TypeBeh",
    "finst"="Finst",
    "avslutn"="Avslutn",
    "xinst"="Xinst",
    "overf"="Overf",
    "ansienndato"="AnsiennDato",
    "mottaksdato"="MottaksDato",

    # Mapping for noen PowerBI “textbox”-navn som kan dukke opp.
    "pfforlopsamletid1"="pfInstansId",
    "pakkeforlopkode1"="Forløp",
    "pakkeforlopnavn1"="Pakkeforløp",
    "startdato2"="År",
    "textbox74"="reshIdFagEnhet",
    "textbox70"="AntInst",
    "textbox72"="AntHf",
    "textbox89"="Ainst",
    "textbox22"="UtrStartDato",
    "textbox44"="Sinst",
    "textbox24"="KliniskBeslutn",
    "textbox26"="TypeBeslutn",
    "textbox106"="Cinst",
    "textbox28"="StartBeh",
    "textbox30"="TypeBeh",
    "textbox114"="Finst",
    "textbox32"="Avslutn",
    "textbox122"="Xinst",
    "textbox54"="Overf",
    "textbox56"="AnsiennDato",
    "textbox58"="MottaksDato"
  )

  # Gå gjennom alle nøklene vi kjenner.
  for (k in names(key_map)) {
    # Finn indeksen til kolonnen som matcher nøkkelen.
    idx <- which(keys == k)

    # Hvis vi fant minst én match...
    if (length(idx) >= 1) {
      old <- names(df)[idx[1]]      # gammelt navn i filen
      new <- key_map[[k]]           # standard navn vi ønsker

      # Hvis navnet faktisk endres, gjør rename og logg.
      if (!identical(old, new)) {
        names(df)[idx[1]] <- new
        log_warn(USER, "Kolonnen '%s' ble tolket som '%s' (%s)", old, new, file_label)
        rb_add(paste0("Renamed column: ", old, " -> ", new, " \n file=", file_label))
      }
    }
  }

  # Sørg for at noen kolonner finnes, selv om de mangler i input (fyll med NA).
  if (!"KommuneNr" %in% names(df)) df$KommuneNr <- NA_character_
  if (!"BydelNr"   %in% names(df)) df$BydelNr   <- NA_character_

  # NPRId må finnes. Hvis ikke, stopp, fordi resten av pipeline trenger NPRId.
  if (!"NPRId" %in% names(df)) {
    stop_user("Fant ikke kolonnen 'NPRId' i %s. Kolonner: %s",
              file_label, paste(names(df), collapse=", "))
  }

  # Returnerer df med standardiserte kolonnenavn.
  df
}

# Leser en PowerBI-CSV robust (delimiter, encoding, rydding).
read_powerbi_csv <- function(USER, path) {
  # Hvis fil ikke finnes, stopp tidlig med god feilmelding.
  if (!file.exists(path)) stop_user("Filen finnes ikke: %s", path)

  # Finn delimiter automatisk.
  delim <- detect_delim(path)

  # Hjelpefunksjon: prøv å lese med en bestemt encoding.
  try_read <- function(enc) {
    readr::read_delim(
      file = path,
      delim = delim,
      col_types = readr::cols(.default = readr::col_character()), # alt som tekst først (trygt)
      locale = readr::locale(encoding = enc),
      na = character(),         # ingen standard NA-strings (vi håndterer selv)
      trim_ws = FALSE,
      show_col_types = FALSE,
      progress = FALSE
    )
  }

  # Prøv UTF-8 først, hvis det feiler, prøv Windows-1252.
  df <- tryCatch(try_read("UTF-8"), error = function(e) try_read("Windows-1252"))

  # Rydd kolonnenavn: erstatt NBSP og trim.
  names(df) <- names(df) %>% stringr::str_replace_all("\u00A0", " ") %>% stringr::str_trim()

  # Fjern carriage return (\r) i alle celler (kan komme fra eksport).
  df <- df %>% dplyr::mutate(dplyr::across(dplyr::everything(),
                                          ~ stringr::str_replace_all(.x, "\\r", "")))

  # Fjern kolonne M (#13) hvis den er tom (valgfritt).
  df <- drop_column_M_if_present(USER, df, basename(path))

  # Hvis første rad egentlig er en header/NULL-rad, fjern den.
  if (nrow(df) > 0 && "NPRId" %in% names(df)) {
    if (!is.na(df$NPRId[1]) && df$NPRId[1] %in% c("NPRId","NULL")) {
      log_info(USER, "Første rad ser ut som header/NULL-rad. Den fjernes (%s).", basename(path))
      rb_add(paste0("Dropped first row (header/NULL) \n file=", basename(path)))
      df <- df[-1, , drop = FALSE]
    }
  }

  # Fjern helt tomme kolonner.
  df <- drop_fully_empty_columns(USER, df)

  # Standardiser kolonnenavn.
  df <- standardize_powerbi_names(USER, df, basename(path))

  # Returner renset df.
  df
}

# Finner inputfil for en region basert på mønster og nyeste tidspunkt.
find_region_input_file <- function(USER, region, suffix) {
  # Region-mappen (base_dir/region).
  region_dir <- file.path(USER$base_dir, region)
  # Hvis mappen ikke finnes, return NA.
  if (!dir.exists(region_dir)) return(NA_character_)

  # Mønster 1: med suffix.
  pat1 <- paste0("Forlopstider.*RHF_", region, "_", suffix, "\\.csv$")
  hits <- list.files(region_dir, pattern = pat1, ignore.case = TRUE, full.names = TRUE)

  # Hvis ingen treff, prøv mønster 2: uten suffix.
  if (length(hits) == 0) {
    pat2 <- paste0("Forlopstider.*RHF_", region, "\\.csv$")
    hits <- list.files(region_dir, pattern = pat2, ignore.case = TRUE, full.names = TRUE)
  }

  # Hvis fortsatt ingen treff, return NA.
  if (length(hits) == 0) return(NA_character_)

  # Returner den filen som er sist endret (nyeste eksport).
  hits[which.max(file.info(hits)$mtime)]
}

# Laster data for en region, med valgfri Parquet-cache for fart.
load_region_dataset <- function(USER, region, suffix) {
  # Finn riktig CSV-fil.
  csv_path <- find_region_input_file(USER, region, suffix)

  # Hvis ingen fil, logg og return NULL.
  if (is.na(csv_path)) {
    log_warn(USER, "Fant ingen CSV for %s (suffix=%s)", region, suffix)
    rb_add(paste0("Input not found \n region=", region, " suffix=", suffix))
    return(NULL)
  }

  # Hvis bruker ønsker cache og arrow finnes, bruk Parquet-cache.
  if (isTRUE(USER$use_parquet_cache) && arrow_available()) {
    ensure_dir(cache_dir(USER))
    pq_path <- parquet_cache_path(USER, region, suffix)

    # Hvis cache finnes og er nyere enn CSV, les cache.
    if (file.exists(pq_path) && file.info(pq_path)$mtime >= file.info(csv_path)$mtime) {
      log_info(USER, "Leser Parquet-cache: %s", pq_path)
      rb_add(paste0("Input used (parquet): ", pq_path))
      df_cached <- arrow::read_parquet(pq_path) %>% tibble::as_tibble()
      rb_add(paste0("Rader i renset input (", region, "): ", nrow(df_cached)))
      return(list(data = df_cached, csv_path = csv_path, parquet_path = pq_path, used_parquet = TRUE))
    }

    # Ellers les CSV og skriv ny cache.
    log_info(USER, "Leser CSV: %s", csv_path)
    rb_add(paste0("Input used (csv): ", csv_path))
    df <- read_powerbi_csv(USER, csv_path)
    log_info(USER, "Skriver Parquet-cache: %s", pq_path)
    rb_add(paste0("Wrote parquet cache: ", pq_path))
    arrow::write_parquet(df, pq_path)
    rb_add(paste0("Rader i renset input (", region, "): ", nrow(df)))
    return(list(data = df, csv_path = csv_path, parquet_path = pq_path, used_parquet = TRUE))
  }

  # Hvis cache er ønsket, men arrow mangler, logg advarsel.
  if (isTRUE(USER$use_parquet_cache) && !arrow_available()) {
    log_warn(USER, "Parquet-cache ønsket, men pakken 'arrow' finnes ikke. Kjører uten cache.")
    rb_add("Parquet cache requested but arrow not installed -> running without cache")
  }

  # Standard: les CSV uten cache.
  log_info(USER, "Leser CSV: %s", csv_path)
  rb_add(paste0("Input used (csv): ", csv_path))
  df_nocache <- read_powerbi_csv(USER, csv_path)
  rb_add(paste0("Rader i renset input (", region, "): ", nrow(df_nocache)))

  # Returner data + info om hvilke filer som ble brukt.
  list(data = df_nocache, csv_path = csv_path, parquet_path = NA_character_, used_parquet = FALSE)
}