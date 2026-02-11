# ==============================================================================
# 02_powerbi_io.R
# Leser og renser PowerBI CSV, standardiserer kolonnenavn, og logger radtall
# ============================================================================== 

detect_delim <- function(path) {
  header <- readLines(path, n = 1, warn = FALSE)
  header <- sub("^\ufeff", "", header)
  semi_fields  <- length(strsplit(header, ";")[[1]])
  comma_fields <- length(strsplit(header, ",")[[1]])
  if (semi_fields > comma_fields) ";" else ","
}

drop_fully_empty_columns <- function(USER, df) {
  empty_cols <- names(df)[sapply(df, function(x) all(is.na(x) | x == ""))]
  if (length(empty_cols) > 0) {
    log_info(USER, "Dropper helt tomme kolonner: %s", paste(empty_cols, collapse = ", "))
    rb_add(paste0("Dropped fully empty columns: ", paste(empty_cols, collapse = ", ")))
    df <- df %>% dplyr::select(-dplyr::all_of(empty_cols))
  }
  df
}

drop_column_M_if_present <- function(USER, df, file_label = "") {
  if (isTRUE(USER$drop_column_M) && ncol(df) >= 13) {
    col13 <- df[[13]]
    is_empty <- all(is.na(col13) | col13 == "")
    if (is_empty) {
      removed_name <- names(df)[13]
      df <- df[, -13, drop = FALSE]
      log_info(USER, "Fjernet kolonne M (#13) '%s' (tom) i %s", removed_name, file_label)
      rb_add(paste0("Dropped column M (#13): ", removed_name, " | file=", file_label))
    } else {
      log_warn(USER, "Kolonne M (#13) er IKKE tom i %s. Den ble IKKE fjernet.", file_label)
      rb_add(paste0("Kept column M (#13) because it had values | file=", file_label))
    }
  }
  df
}

standardize_powerbi_names <- function(USER, df, file_label = "") {

  make_key <- function(x) {
    x <- stringr::str_replace_all(x, "\u00A0", " ")
    x <- stringr::str_trim(x)
    x_ascii <- suppressWarnings(iconv(x, from = "", to = "ASCII//TRANSLIT"))
    x_ascii[is.na(x_ascii)] <- x[is.na(x_ascii)]
    x_ascii %>% tolower() %>% stringr::str_replace_all("[^a-z0-9]", "")
  }

  keys <- make_key(names(df))

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

  for (k in names(key_map)) {
    idx <- which(keys == k)
    if (length(idx) >= 1) {
      old <- names(df)[idx[1]]
      new <- key_map[[k]]
      if (!identical(old, new)) {
        names(df)[idx[1]] <- new
        log_warn(USER, "Kolonnen '%s' ble tolket som '%s' (%s)", old, new, file_label)
        rb_add(paste0("Renamed column: ", old, " -> ", new, " | file=", file_label))
      }
    }
  }

  if (!"KommuneNr" %in% names(df)) df$KommuneNr <- NA_character_
  if (!"BydelNr"   %in% names(df)) df$BydelNr   <- NA_character_

  if (!"NPRId" %in% names(df)) {
    stop_user("Fant ikke kolonnen 'NPRId' i %s. Kolonner: %s", file_label, paste(names(df), collapse=", "))
  }

  df
}

read_powerbi_csv <- function(USER, path) {
  if (!file.exists(path)) stop_user("Filen finnes ikke: %s", path)

  delim <- detect_delim(path)

  try_read <- function(enc) {
    readr::read_delim(
      file = path,
      delim = delim,
      col_types = readr::cols(.default = readr::col_character()),
      locale = readr::locale(encoding = enc),
      na = character(),
      trim_ws = FALSE,
      show_col_types = FALSE,
      progress = FALSE
    )
  }

  df <- tryCatch(try_read("UTF-8"), error = function(e) try_read("Windows-1252"))

  names(df) <- names(df) %>% stringr::str_replace_all("\u00A0", " ") %>% stringr::str_trim()
  df <- df %>% dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringr::str_replace_all(.x, "\r", "")))

  df <- drop_column_M_if_present(USER, df, basename(path))

  if (nrow(df) > 0 && "NPRId" %in% names(df)) {
    if (!is.na(df$NPRId[1]) && df$NPRId[1] %in% c("NPRId","NULL")) {
      log_info(USER, "Første rad ser ut som header/NULL-rad. Den fjernes (%s).", basename(path))
      rb_add(paste0("Dropped first row (header/NULL) | file=", basename(path)))
      df <- df[-1, , drop = FALSE]
    }
  }

  df <- drop_fully_empty_columns(USER, df)
  df <- standardize_powerbi_names(USER, df, basename(path))
  df
}

find_region_input_file <- function(USER, region, suffix) {
  region_dir <- file.path(USER$base_dir, region)
  if (!dir.exists(region_dir)) return(NA_character_)

  pat1 <- paste0("Forlopstider.*RHF_", region, "_", suffix, "\\.csv$")
  hits <- list.files(region_dir, pattern = pat1, ignore.case = TRUE, full.names = TRUE)

  if (length(hits) == 0) {
    pat2 <- paste0("Forlopstider.*RHF_", region, "\\.csv$")
    hits <- list.files(region_dir, pattern = pat2, ignore.case = TRUE, full.names = TRUE)
  }

  if (length(hits) == 0) return(NA_character_)
  hits[which.max(file.info(hits)$mtime)]
}

load_region_dataset <- function(USER, region, suffix) {
  csv_path <- find_region_input_file(USER, region, suffix)

  if (is.na(csv_path)) {
    log_warn(USER, "Fant ingen CSV for %s (suffix=%s)", region, suffix)
    rb_add(paste0("Input not found | region=", region, " suffix=", suffix))
    return(NULL)
  }

  if (isTRUE(USER$use_parquet_cache) && arrow_available()) {
    ensure_dir(cache_dir(USER))
    pq_path <- parquet_cache_path(USER, region, suffix)

    if (file.exists(pq_path) && file.info(pq_path)$mtime >= file.info(csv_path)$mtime) {
      log_info(USER, "Leser Parquet-cache: %s", pq_path)
      rb_add(paste0("Input used (parquet): ", pq_path))
      df_cached <- arrow::read_parquet(pq_path) %>% tibble::as_tibble()
      rb_add(paste0("Rader i renset input (", region, "): ", nrow(df_cached)))
      return(list(data = df_cached, csv_path = csv_path, parquet_path = pq_path, used_parquet = TRUE))
    }

    log_info(USER, "Leser CSV: %s", csv_path)
    rb_add(paste0("Input used (csv): ", csv_path))
    df <- read_powerbi_csv(USER, csv_path)

    log_info(USER, "Skriver Parquet-cache: %s", pq_path)
    rb_add(paste0("Wrote parquet cache: ", pq_path))
    arrow::write_parquet(df, pq_path)

    rb_add(paste0("Rader i renset input (", region, "): ", nrow(df)))
    return(list(data = df, csv_path = csv_path, parquet_path = pq_path, used_parquet = TRUE))
  }

  if (isTRUE(USER$use_parquet_cache) && !arrow_available()) {
    log_warn(USER, "Parquet-cache ønsket, men pakken 'arrow' finnes ikke. Kjører uten cache.")
    rb_add("Parquet cache requested but arrow not installed -> running without cache")
  }

  log_info(USER, "Leser CSV: %s", csv_path)
  rb_add(paste0("Input used (csv): ", csv_path))

  df_nocache <- read_powerbi_csv(USER, csv_path)
  rb_add(paste0("Rader i renset input (", region, "): ", nrow(df_nocache)))

  list(data = df_nocache, csv_path = csv_path, parquet_path = NA_character_, used_parquet = FALSE)
}
