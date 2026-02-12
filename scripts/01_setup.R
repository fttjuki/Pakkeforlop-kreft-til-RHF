# ==============================================================================
# Pakkeforløp Kreft til RHF – R-pipeline som matcher SQL-gullstandard
# Gullstandard: Individ_PakkeforløpKreft_HSØ_HV_HMN.sql
#
# v6 – PRODUKSJONSKLAR:
#  * FIKS for Pakkeforløp/æøå som blir til "Ã¦/Ã¸" (mojibake)
#    - Årsak: UTF-8 tekst tolkes som Windows-1252.
#    - Løsning: (1) detekter encoding per fil, (2) les med riktig encoding,
#               (3) reparer mojibake defensivt hvis det finnes.
#  * Bevarer SQL-adferd: vi trimmer IKKE Pakkeforløp (SQL trimmer kun lopenr).
#  * Skriver ut CSV som UTF-8 med BOM (best for Excel + norske tegn).
#
# NB: DødsDato håndteres uendret (som i v5). Bruker kan ignorere den.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(DBI)
  library(odbc)
})

CFG <- list(
  regions    = c("HSØ", "HV", "HMN"),
  months_nor = c("jan","feb","mar","apr","mai","jun","jul","aug","sep","okt","nov","des")
)

FINAL_COLS <- c(
  "lopenr","institusjonId","institusjon","reshIdFagEnhet","Fagenhet",
  "KommuneNr","BydelNr","FødselsDato","DødsDato","pfInstansId","Forløp",
  "Pakkeforløp","AntInst","AntHf","År","StartDato","Ainst","UtrStartDato",
  "Sinst","KliniskBeslutn","TypeBeslutn","Cinst","StartBeh","TypeBeh","Finst",
  "Avslutn","Xinst","OF1","OF1std","OF2","OF2std","OF3","OF3std","OF4","OF4std",
  "Overf","AnsiennDato"
)

geo_overrides <- tibble::tribble(
  ~NPRId,    ~aar,   ~komnrhjem2,
  "1062924", 2008L, "0220",
  "2561864", 2008L, "0706",
  "3200706", 2008L, "0906",
  "3602577", 2008L, "1135"
)

log_info <- function(USER, ...) if (isTRUE(USER$verbose)) message("[INFO] ", sprintf(...))
log_warn <- function(USER, ...) message("[WARN] ", sprintf(...))
stop_user <- function(...) stop(sprintf(...), call. = FALSE)

sanitize_user_paths <- function(USER) {
  for (nm in c("base_dir", "app_dir", "out_dir")) {
    if (!is.null(USER[[nm]])) {
      USER[[nm]] <- trimws(USER[[nm]])
      USER[[nm]] <- gsub("\\\\", "/", USER[[nm]])
      USER[[nm]] <- sub("^/fihr\\.no/", "//fihr.no/", USER[[nm]])
    }
  }
  USER
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

month_to_suffix <- function(month_date) {
  d <- as.Date(month_date)
  paste0(CFG$months_nor[lubridate::month(d)], format(d, "%y"))
}

parse_any_date <- function(x) {
  out <- suppressWarnings(lubridate::parse_date_time(
    x,
    orders = c("Ymd","dmY","Y-m-d","d-m-Y","Y.m.d","d.m.Y",
               "Ymd HMS","dmY HMS","Y-m-d HMS","d-m-Y HMS","Y.m.d HMS","d.m.Y HMS")
  ))
  as.Date(out)
}

request_file_path <- function(USER, suffix) file.path(USER$app_dir, paste0("NPRId_RHF_Pakkeforløp_", suffix, ".csv"))
lnr_file_path     <- function(USER, suffix) file.path(USER$app_dir, paste0("NPRId_RHF_Pakkeforløp_", suffix, "_lnr.csv"))

delivery_file_path_csv  <- function(USER, region, suffix) file.path(USER$out_dir, paste0("Utlevering_", region, "_", suffix, ".csv"))

# -------------------------
# 0) Encoding utilities (NEW in v6)
# -------------------------

# Detect delimiter based on header line

detect_delim <- function(path) {
  header <- readLines(path, n = 1, warn = FALSE)
  header <- sub("^\\ufeff", "", header)
  semi_fields  <- length(strsplit(header, ";")[[1]])
  comma_fields <- length(strsplit(header, ",")[[1]])
  if (semi_fields >= comma_fields) ";" else ","
}

# Guess file encoding; prefer UTF-8 when detected.
# Falls back to Windows-1252 only when UTF-8 not detected.

guess_file_encoding <- function(path) {
  # User can override encoding explicitly
  # (e.g. USER$encoding_override = "Windows-1252")
  enc <- readr::guess_encoding(path, n_max = 10000)
  if (nrow(enc) == 0) return("UTF-8")
  if (any(tolower(enc$encoding) %in% c("utf-8", "utf8"))) return("UTF-8")
  enc$encoding[1]
}

# Repair mojibake: if UTF-8 got read as Windows-1252, strings contain Ã / Â
# Convert those strings from CP1252 -> UTF-8.

fix_mojibake_utf8 <- function(x) {
  x <- as.character(x)
  idx <- !is.na(x) & str_detect(x, "[ÃÂ]")
  if (any(idx)) {
    y <- iconv(x[idx], from = "Windows-1252", to = "UTF-8")
    ok <- !is.na(y)
    x[idx][ok] <- y[ok]
  }
  x
}

# Optional QC: count mojibake markers
count_mojibake <- function(df) {
  sum(vapply(df, function(col) {
    col <- as.character(col)
    sum(!is.na(col) & str_detect(col, "[ÃÂ]"))
  }, integer(1)))
}

# -------------------------
# 1) Lesing + kolonnenavn
# -------------------------

standardize_powerbi_names <- function(df) {
  make_key <- function(x) {
    x <- str_replace_all(x, "\u00A0", " ")
    x <- str_trim(x)
    x_ascii <- suppressWarnings(iconv(x, from = "", to = "ASCII//TRANSLIT"))
    x_ascii[is.na(x_ascii)] <- x[is.na(x_ascii)]
    x_ascii |> tolower() |> str_replace_all("[^a-z0-9]", "")
  }

  keys <- make_key(names(df))

  key_map <- c(
    institusjonid="institusjonId",
    institusjon="institusjon",
    institusjonnavn="institusjon",

    reshidfagenhet="reshIdFagEnhet",
    fagenhet="Fagenhet",
    enhetnavn="Fagenhet",

    nprid="NPRId",

    kommunenr="KommuneNr",
    kommune="KommuneNr",

    bydelnr="BydelNr",
    bydel="BydelNr",

    fodselsdato="FødselsDato",
    dodsdato="DødsDato",
    doddato="DødsDato",

    pfinstansid="pfInstansId",

    forlop="Forløp",
    pakkeforlop="Pakkeforløp",

    antinst="AntInst",
    anthf="AntHf",

    ar="År",
    startar="År",
    startår="År",
    startaar="År",
    startdato2="År",

    startdato="StartDato",
    utrstartdato="UtrStartDato",

    ainst="Ainst",
    sinst="Sinst",

    kliniskbeslutn="KliniskBeslutn",
    typebeslutn="TypeBeslutn",

    cinst="Cinst",
    startbeh="StartBeh",
    typebeh="TypeBeh",
    finst="Finst",
    avslutn="Avslutn",
    xinst="Xinst",

    of1="OF1",
    of1std="OF1std",
    of2="OF2",
    of2std="OF2std",
    of3="OF3",
    of3std="OF3std",
    of4="OF4",
    of4std="OF4std",

    overf="Overf",
    ansienndato="AnsiennDato",
    mottaksdato="MottaksDato",

    # PowerBI-spesifikke felter
    pfforlopsamletid1="pfInstansId",
    pakkeforlopkode1="Forløp",
    pakkeforlopnavn1="Pakkeforløp",

    textbox74="reshIdFagEnhet",
    textbox70="AntInst",
    textbox72="AntHf",
    textbox89="Ainst",
    textbox22="UtrStartDato",
    textbox44="Sinst",
    textbox24="KliniskBeslutn",
    textbox26="TypeBeslutn",
    textbox106="Cinst",
    textbox28="StartBeh",
    textbox30="TypeBeh",
    textbox114="Finst",
    textbox32="Avslutn",
    textbox122="Xinst",
    textbox54="Overf",
    textbox56="AnsiennDato",
    textbox58="MottaksDato"
  )

  for (k in names(key_map)) {
    idx <- which(keys == k)
    if (length(idx) == 1) names(df)[idx] <- key_map[[k]]
  }

  # kritiske kolonner
  if (!"KommuneNr" %in% names(df)) df$KommuneNr <- "NULL"
  if (!"BydelNr"   %in% names(df)) df$BydelNr   <- "NULL"

  if (!"NPRId" %in% names(df)) stop_user("Fant ikke kolonnen 'NPRId' i input.")
  df
}

# UPDATED: robust encoding read + mojibake repair
read_powerbi_csv <- function(path, USER = list()) {
  delim <- detect_delim(path)

  enc <- if (!is.null(USER$encoding_override) && nzchar(USER$encoding_override)) {
    USER$encoding_override
  } else {
    guess_file_encoding(path)
  }

  df <- read_delim(
    file = path,
    delim = delim,
    col_types = cols(.default = col_character()),
    locale = locale(encoding = enc),
    na = character(),
    trim_ws = FALSE,             # IMPORTANT: do not trim values (SQL keeps Pakkeforløp as-is)
    show_col_types = FALSE,
    progress = FALSE
  )

  # Normalize column names (BOM + NBSP)
  names(df) <- names(df) |>
    str_replace_all("\u00A0", " ") |>
    str_replace_all("^\\ufeff", "") |>
    str_trim()

  # Remove CR in values
  df <- df |> mutate(across(everything(), ~ str_replace_all(.x, "\r", "")))

  # Mojibake repair (optional, default TRUE)
  if (is.null(USER$repair_mojibake) || isTRUE(USER$repair_mojibake)) {
    df <- df |> mutate(across(everything(), fix_mojibake_utf8))
  }

  # Drop repeated header row if present
  if (nrow(df) > 0 && "NPRId" %in% names(df)) {
    if (!is.na(df$NPRId[1]) && df$NPRId[1] %in% c("NPRId", "NULL")) df <- df[-1, , drop = FALSE]
  }

  df <- standardize_powerbi_names(df)

  # QC: count remaining mojibake markers
  if (isTRUE(USER$fail_on_mojibake)) {
    n_bad <- count_mojibake(df)
    if (n_bad > 0) stop_user("Fant %s feltverdier med mojibake (Ã/Â) etter lesing. Sjekk encoding/eksport.", n_bad)
  }

  df
}

find_region_input_file <- function(USER, region, suffix) {
  region_dir <- file.path(USER$base_dir, region)
  if (!dir.exists(region_dir)) return(NA_character_)

  pat1 <- paste0(".*RHF_", region, "_", suffix, "\\.csv$")
  hits <- list.files(region_dir, pattern = pat1, ignore.case = TRUE, full.names = TRUE)

  # v6: optionally strict (recommended for production)
  if (length(hits) == 0) {
    if (isTRUE(USER$strict_input)) {
      stop_user("Fant ingen suffix-fil for %s (suffix=%s). Forventet mønster: %s", region, suffix, pat1)
    }
    # legacy fallback (kept only if strict_input = FALSE)
    pat2 <- paste0(".*RHF_", region, "\\.csv$")
    hits <- list.files(region_dir, pattern = pat2, ignore.case = TRUE, full.names = TRUE)
    if (length(hits) == 0) return(NA_character_)
  }

  hits[which.max(file.info(hits)$mtime)]
}

load_region_dataset <- function(USER, region, suffix) {
  csv_path <- find_region_input_file(USER, region, suffix)
  if (is.na(csv_path)) {
    log_warn(USER, "Fant ingen CSV for %s (suffix=%s)", region, suffix)
    return(NULL)
  }
  log_info(USER, "Leser CSV %s", csv_path)
  df <- read_powerbi_csv(csv_path, USER = USER)
  list(data = df, csv_path = csv_path)
}

# -------------------------
# 2) NPRId -> løpenr
# -------------------------

build_nprid_request <- function(all_rows) {
  all_rows |> distinct(NPRId) |>
    filter(!is.na(NPRId), NPRId != "", NPRId != "NPRId", NPRId != "NULL") |>
    mutate(Dummy = "")
}

write_nprid_request_file <- function(USER, nprid_request, suffix) {
  ensure_dir(USER$app_dir)
  out_path <- request_file_path(USER, suffix)
  write_delim(nprid_request, out_path, delim = ";", na = "")
  log_info(USER, "Lagret bestillingsfil %s (rader=%s)", out_path, nrow(nprid_request))
  out_path
}

read_lopenr_file <- function(USER, suffix) {
  path <- lnr_file_path(USER, suffix)
  if (!file.exists(path)) stop_user("Mangler returfil med løpenr: %s", path)

  read_delim(
    path,
    delim = ";",
    col_names = c("NPRId", "lopenr"),
    col_types = cols(.default = col_character()),
    na = character(),
    show_col_types = FALSE,
    progress = FALSE,
    trim_ws = FALSE
  ) |>
    mutate(
      NPRId  = str_trim(str_replace_all(NPRId, "\r", "")),
      lopenr = str_trim(str_replace_all(lopenr, "\r", "")),
      lopenr = str_replace_all(lopenr, ";", "")
    )
}

validate_lopenr_map <- function(USER, all_rows, id_map, suffix) {
  needed <- all_rows |>
    filter(!is.na(NPRId), NPRId != "", NPRId != "NPRId", NPRId != "NULL") |>
    distinct(NPRId)

  if (nrow(id_map) == 0) stop_user("Returfilen er tom: %s", lnr_file_path(USER, suffix))

  dup <- id_map |> filter(!is.na(NPRId), NPRId != "") |> count(NPRId) |> filter(n > 1)
  if (nrow(dup) > 0) stop_user("Duplikate NPRId i returfil. Eksempel: %s", paste(head(dup$NPRId, 10), collapse = ", "))

  missing_map <- needed |>
    left_join(id_map, by = "NPRId") |>
    filter(is.na(lopenr) | lopenr == "" | lopenr == "NULL")

  if (nrow(missing_map) > 0) {
    ensure_dir(USER$out_dir)
    miss_path <- file.path(USER$out_dir, paste0("QC_missing_lopenr_NPRId_", suffix, ".csv"))
    write_csv(missing_map, miss_path)
    log_warn(USER, "%s NPRId mangler løpenr (droppes som i SQL). Se %s", nrow(missing_map), miss_path)
    if (isTRUE(USER$strict_lopenr)) stop_user("STOPP: %s NPRId mangler løpenr. Se %s", nrow(missing_map), miss_path)
    return(FALSE)
  }

  log_info(USER, "OK: Alle NPRId har løpenr (%s)", suffix)
  TRUE
}

# -------------------------
# 3) GEO fix (SOMHoved)
# -------------------------

connect_somhoved <- function() {
  con <- DBI::dbConnect(
    odbc::odbc(),
    Driver   = "SQL Server",
    Server   = "NPRSQLprod",
    Database = "NPRNasjonaltDatagrunnlag",
    Trusted_Connection = "True"
  )
  DBI::dbExecute(con, "SET NOCOUNT ON;")
  con
}

detect_db_npr_col <- function(con) {
  cols <- names(DBI::dbGetQuery(con, "SELECT TOP 0 * FROM dbo.SOMHoved;"))
  if ("NPRId" %in% cols) return("NPRId")
  if ("NPRid" %in% cols) return("NPRid")
  stop_user("Fant verken NPRId eller NPRid i dbo.SOMHoved")
}

fetch_geo_fixes <- function(con, missing_ids, db_npr_col) {
  if (length(missing_ids) == 0) return(tibble(NPRId=character(), komnrhjem2=character(), bydel2=character(), aar=integer()))

  if (!requireNamespace("glue", quietly = TRUE)) stop_user("Pakken 'glue' trengs for DB-spørringen")

  temp_name <- paste0("#TempMissing_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1000:9999, 1))
  temp_tbl  <- tibble(!!db_npr_col := as.character(missing_ids))
  dplyr::copy_to(con, temp_tbl, name = temp_name, temporary = TRUE, overwrite = TRUE)

  sql_txt <- glue::glue(
    "WITH Kom AS (\n",
    "  SELECT s.{db_npr_col} AS NPRId, s.komnrhjem2, s.bydel2, s.aar,\n",
    "         ROW_NUMBER() OVER (PARTITION BY s.{db_npr_col} ORDER BY s.aar DESC) AS rn\n",
    "  FROM dbo.SOMHoved s INNER JOIN {temp_name} t ON s.{db_npr_col} = t.{db_npr_col}\n",
    ")\n",
    "SELECT NPRId, komnrhjem2, bydel2, aar FROM Kom WHERE rn = 1;"
  )

  DBI::dbGetQuery(con, sql_txt) |>
    as_tibble() |>
    mutate(
      NPRId      = as.character(NPRId),
      komnrhjem2 = as.character(komnrhjem2),
      bydel2     = as.character(bydel2),
      aar        = as.integer(aar)
    )
}

apply_geo_overrides <- function(geo_fixes) {
  geo_fixes |>
    left_join(geo_overrides, by = c("NPRId", "aar"), suffix = c("", "_override")) |>
    mutate(komnrhjem2 = coalesce(komnrhjem2_override, komnrhjem2)) |>
    select(-komnrhjem2_override)
}

# -------------------------
# 4) Leveranse (SQL-lik)
# -------------------------

ensure_final_columns_exist <- function(df, missing_token = "NULL") {
  missing <- setdiff(FINAL_COLS, names(df))
  if (length(missing) > 0) for (m in missing) df[[m]] <- missing_token
  df
}

force_output_shape <- function(df, missing_token = "NULL") {
  df |> mutate(across(everything(), ~ {
    x <- as.character(.x)
    x[is.na(x) | x == ""] <- missing_token
    x
  }))
}

fix_lopenr_name <- function(df) {
  if ("enr" %in% names(df) && !"lopenr" %in% names(df)) {
    df <- df |> rename(lopenr = enr)
  }
  if ("enr" %in% names(df) && "lopenr" %in% names(df)) {
    df$lopenr <- ifelse(df$lopenr == "" | is.na(df$lopenr) | df$lopenr == "NULL", df$enr, df$lopenr)
    df <- df |> select(-enr)
  }
  df
}

make_delivery <- function(USER, report_df, id_map, geo_fixes, region, suffix) {
  missing_token <- if (!is.null(USER$missing_token)) USER$missing_token else "NULL"

  df <- report_df |>
    filter(!is.na(NPRId), NPRId != "", NPRId != "NPRId", NPRId != "NULL") |>
    left_join(id_map, by = "NPRId")

  df <- fix_lopenr_name(df)

  # SQL: WHERE b.lopenr != 'NULL'
  df <- df |> filter(!is.na(lopenr), lopenr != "", lopenr != "NULL")

  # År fylles kun hvis mangler
  if (!"År" %in% names(df) || all(is.na(df$År) | df$År == "" | df$År == "NULL")) {
    sd <- parse_any_date(df$StartDato)
    df$År <- ifelse(!is.na(sd), as.character(lubridate::year(sd)), df$År)
  }

  if (isTRUE(USER$fill_missing_kommune)) {
    df <- df |>
      left_join(geo_fixes, by = "NPRId") |>
      mutate(
        KommuneNr_orig   = KommuneNr,
        Kommune_missing  = is.na(KommuneNr_orig) | KommuneNr_orig == "" | KommuneNr_orig == "NULL",
        Bydel_missing    = is.na(BydelNr) | BydelNr == "" | BydelNr == "NULL",

        KommuneNr = if_else(Kommune_missing, komnrhjem2, KommuneNr_orig),

        # Match SQL: bruker original KommuneNr i betingelsen for BydelNr
        BydelNr   = if_else(!Kommune_missing & KommuneNr_orig == komnrhjem2 & Bydel_missing,
                            bydel2, BydelNr)
      ) |>
      select(-KommuneNr_orig, -Kommune_missing, -Bydel_missing, -komnrhjem2, -bydel2, -aar)
  }

  df <- ensure_final_columns_exist(df, missing_token = missing_token)

  df_out <- df |>
    select(all_of(FINAL_COLS)) |>
    force_output_shape(missing_token = missing_token)

  # v6: ensure no mojibake remains in Pakkeforløp if requested
  if (isTRUE(USER$fail_on_mojibake_out)) {
    bad <- sum(!is.na(df_out$Pakkeforløp) & str_detect(df_out$Pakkeforløp, "[ÃÂ]"))
    if (bad > 0) stop_user("Output inneholder %s verdier i Pakkeforløp med mojibake (Ã/Â).", bad)
  }

  df_out
}

# UTF-8 + BOM writer for Excel compatibility
write_delim_utf8_bom <- function(df, path, delim = ";") {
  df <- df |> mutate(across(everything(), ~ enc2utf8(as.character(.x))))

  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeChar("\ufeff", con, eos = NULL, useBytes = TRUE)

  con2 <- file(path, open = "ab")
  on.exit(close(con2), add = TRUE)
  write_delim(df, con2, delim = delim, na = "")
}

write_delivery <- function(USER, df, region, suffix) {
  ensure_dir(USER$out_dir)
  out_csv <- delivery_file_path_csv(USER, region, suffix)
  write_delim_utf8_bom(df, out_csv, delim = ";")
  log_info(USER, "Lagret CSV %s (rader=%s)", out_csv, nrow(df))
  list(csv = out_csv)
}

# -------------------------
# 5) Orkestrering
# -------------------------

process_suffix <- function(USER, suffix) {
  log_info(USER, "Processing suffix %s", suffix)

  region_objs <- set_names(CFG$regions) |>
    map(~ load_region_dataset(USER, .x, suffix)) |>
    discard(is.null)

  if (length(region_objs) == 0) {
    log_warn(USER, "Ingen inputfiler funnet for %s", suffix)
    return(invisible(list(status = "no_inputs", suffix = suffix)))
  }

  reports  <- map(region_objs, "data")
  all_rows <- bind_rows(reports)

  req_tbl  <- build_nprid_request(all_rows)
  req_path <- write_nprid_request_file(USER, req_tbl, suffix)

  if (toupper(USER$step) == "STEP1") {
    log_info(USER, "STEP1 valgt – stopper etter bestillingsfil")
    return(invisible(list(status = "step1_done", suffix = suffix, req_path = req_path)))
  }

  id_map <- read_lopenr_file(USER, suffix)
  validate_lopenr_map(USER, all_rows, id_map, suffix)

  geo_fixes <- tibble(NPRId=character(), komnrhjem2=character(), bydel2=character(), aar=integer())
  if (isTRUE(USER$fill_missing_kommune)) {
    missing_geo_ids <- all_rows |>
      filter(!is.na(NPRId), NPRId != "", NPRId != "NPRId", NPRId != "NULL",
             is.na(KommuneNr) | KommuneNr == "" | KommuneNr == "NULL") |>
      distinct(NPRId) |>
      pull(NPRId)

    if (length(missing_geo_ids) > 0) {
      con <- connect_somhoved()
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      db_npr_col <- detect_db_npr_col(con)
      geo_fixes  <- fetch_geo_fixes(con, missing_geo_ids, db_npr_col) |>
        apply_geo_overrides()
    }
  }

  out_files <- list(); final_list <- list()
  for (region in names(reports)) {
    df_final <- make_delivery(USER, reports[[region]], id_map, geo_fixes, region, suffix)
    final_list[[region]] <- df_final
    out_files[[region]] <- write_delivery(USER, df_final, region, suffix)
  }

  invisible(list(status = "delivered", suffix = suffix, out_files = out_files, req_path = req_path, final = final_list))
}

main <- function(USER) {
  USER <- sanitize_user_paths(USER)
  ensure_dir(USER$out_dir)
  ensure_dir(USER$app_dir)

  if (!toupper(USER$step) %in% c("STEP1","STEP2")) stop_user("USER$step må være STEP1 eller STEP2")
  if (length(USER$months) == 0) stop_user("USER$months er tom")

  month_dates <- as.Date(USER$months)
  if (any(is.na(month_dates))) stop_user("Ugyldig dato i USER$months. Bruk format YYYY-MM-01")

  suffixes <- map_chr(month_dates, month_to_suffix)
  for (sfx in suffixes) process_suffix(USER, sfx)
  invisible(TRUE)
}


