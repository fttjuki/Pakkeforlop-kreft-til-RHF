
# ==============================================================================
# Pakkeforløp kreft til RHF
#
# TWO-RUN WORKFLOW:
#   RUN 1 (Source):
#     - Reads PowerBI exports (HSØ/HV/HMN): https://ssrsprod.fihr.no/Reports/report/PakkeforlopKreftRapporter/Forlopstider%20per%20pasient%20per%20organspesifikke%20pakkeforlop%20per%20RHF
#     - Extracts distinct NPRId
#     - Writes file(unique NPRID) to order løpenr
#     - STOPS ONLY if løpenr-file(s) *_lnr.csv are missing
#
#   RUN 2 (Source again after creating løpenr-file *_lnr.csv ):
#     - Reads mapping file(s) 
#     - Validates every NPRId has lopenr (STOP if incomplete)
#     - fix missing KommuneNr 
#     - Writes deliverie-files
## Reporting schedule rules:
# - Run in January  -> deliver December (previous year)
# - We do NOT run in February (script stops if run in Feb)
# - Run in March    -> deliver January and February (two separate deliveries)
# - Run Apr–Dec     -> deliver previous month
# ==============================================================================

start_tid <- Sys.time()
suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(stringr)
  library(lubridate)
  library(DBI)
  library(odbc)
  library(dbplyr)
})

CFG <- list(
  base_dir = "N:/Utleveringer/STYRINGSDATA/RHF Pakkeforløp kreft/R",
  app_dir  = "//fihr.no/dfs/NPR/Temp/NPR_RegistrerUtlevering",
  out_dir  = "N:/Utleveringer/STYRINGSDATA/RHF Pakkeforløp Kreft/R",
  
  regions = c("HSØ", "HV", "HMN"),
  months_nor = c("jan","feb","mar","apr","mai","jun","jul","aug","sep","okt","nov","des"),
  verbose = TRUE,
  
  # QC toggles
  run_qc_missing_kommune = TRUE,
  run_monthly_counts_reporting_year = TRUE,
  run_anomaly_checks = FALSE,     # ✅ DISABLED
  write_qc_files = TRUE,
  
  # Show in View() (interactive)
  show_qc_in_view = TRUE,
  
  # QC plots
  plot_missing_kommune = TRUE,
  plot_monthly_counts = TRUE,
  save_plots = TRUE
)

log_info <- function(...) if (isTRUE(CFG$verbose)) message("[INFO] ", sprintf(...))
log_warn <- function(...) message("[WARN] ", sprintf(...))

# ==============================================================================
# Helper: safe View() + assign
# ==============================================================================
safe_obj_name <- function(x) make.names(gsub("[^A-Za-z0-9_]", "_", x))

show_in_view <- function(df, title) {
  message("\n--- ", title, " ---")
  print(df)
  if (isTRUE(CFG$show_qc_in_view) && interactive()) {
    nm <- safe_obj_name(title)
    assign(nm, df, envir = .GlobalEnv)
    try(View(get(nm, envir = .GlobalEnv)), silent = TRUE)
  }
}

# ==============================================================================
# Calendar helpers
# ==============================================================================
get_reporting_months <- function(run_date = Sys.Date()) {
  run_month <- month(run_date)
  run_year  <- year(run_date)
  if (run_month == 2) stop("This pipeline is NOT scheduled to run in February.")
  if (run_month == 1) return(as.Date(make_date(run_year - 1, 12, 1)))
  if (run_month == 3) return(as.Date(c(make_date(run_year, 1, 1), make_date(run_year, 2, 1))))
  as.Date(seq.Date(floor_date(run_date, "month"), by = "-1 month", length.out = 2)[2])
}
make_suffix <- function(month_date) paste0(CFG$months_nor[month(month_date)], format(month_date, "%y"))

suffix_to_month_date <- function(suffix) {
  sfx <- tolower(suffix)
  mon_code <- stringr::str_sub(sfx, 1, 3)
  yy <- as.integer(stringr::str_extract(sfx, "[0-9]{2}$"))
  if (is.na(yy)) stop("Cannot parse year from suffix: ", suffix)
  mon_idx <- match(mon_code, CFG$months_nor)
  if (is.na(mon_idx)) stop("Cannot parse month from suffix: ", suffix)
  lubridate::make_date(2000 + yy, mon_idx, 1)
}

# ==============================================================================
# CSV reader helpers: delimiter + empty columns
# ==============================================================================
detect_delim_from_header <- function(path) {
  header <- readLines(path, n = 1, warn = FALSE)
  header <- sub("^\ufeff", "", header)  # strip UTF-8 BOM if present
  
  # choose delimiter that yields more fields
  semi_fields  <- length(str_split(header, ";", simplify = TRUE))
  comma_fields <- length(str_split(header, ",", simplify = TRUE))
  
  if (semi_fields > comma_fields) ";" else ","
}

drop_fully_empty_columns <- function(df, verbose_label = NULL) {
  empty_cols <- names(df)[sapply(df, function(x) all(is.na(x) | x == ""))]
  if (length(empty_cols) > 0) {
    if (!is.null(verbose_label)) {
      message("• Dropping fully empty columns in ", verbose_label, ": ", paste(empty_cols, collapse = ", "))
    }
    df <- df %>% select(-all_of(empty_cols))
  }
  df
}

read_csv_header_then_data <- function(path) {
  if (!file.exists(path)) stop("File does not exist: ", path)
  delim <- detect_delim_from_header(path)
  
  try_read <- function(enc) {
    readr::read_delim(
      file = path,
      delim = delim,
      col_types = readr::cols(.default = readr::col_character()),
      locale = readr::locale(encoding = enc),
      na = character(),          # keep "" as ""
      trim_ws = FALSE,
      show_col_types = FALSE,
      progress = FALSE
    )
  }
  
  df <- tryCatch(try_read("UTF-8"), error = function(e) try_read("Windows-1252"))
  
  names(df) <- names(df) %>%
    str_replace_all("\u00A0", " ") %>%
    str_trim()
  
  df <- df %>% mutate(across(everything(), ~ str_replace_all(.x, "\r", "")))
  df <- drop_fully_empty_columns(df, verbose_label = basename(path))
  df
}

# ==============================================================================
# Column standardization 
# ==============================================================================
standardize_powerbi_names <- function(df, file_label = "") {
  
  make_key <- function(x) {
    x <- str_replace_all(x, "\u00A0", " ")
    x <- str_trim(x)
    x_ascii <- suppressWarnings(iconv(x, from = "", to = "ASCII//TRANSLIT"))
    x_ascii[is.na(x_ascii)] <- x[is.na(x_ascii)]
    x_ascii %>% tolower() %>% str_replace_all("[^a-z0-9]", "")
  }
  keys <- make_key(names(df))
  
  key_map <- c(
    # canonical
    "institusjonid"="institusjonId",
    "institusjon"="institusjon",
    "reshidfagenhet"="reshIdFagEnhet",
    "fagenhet"="Fagenhet",
    "nprid"="NPRId",
    "kommunenr"="KommuneNr",
    "bydelnr"="BydelNr",
    "fodselsdato"="FødselsDato",
    "dodsdato"="DødsDato",
    "pfinstansid"="pfInstansId",
    "forlop"="Forløp",
    "pakkeforlop"="Pakkeforløp",
    "antinst"="AntInst",
    "anthf"="AntHf",
    "startar"="År",
    "ar"="År",
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
    
    # your header variants
    "institusjonnavn"="institusjon",
    "enhetnavn"="Fagenhet",
    "kommune"="KommuneNr",
    "bydel"="BydelNr",
    "doddato"="DødsDato",
    "pfforlopsamletid1"="pfInstansId",
    "pakkeforlopkode1"="Forløp",
    "pakkeforlopnavn1"="Pakkeforløp",
    "startdato2"="År",
    
    # Textbox mapping
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
      old_name <- names(df)[idx[1]]
      new_name <- key_map[[k]]
      if (!identical(old_name, new_name)) {
        names(df)[idx[1]] <- new_name
        log_warn("Renamed column '%s' to '%s'%s", old_name, new_name,
                 ifelse(file_label != "", paste0(" in ", file_label), ""))
      }
    }
  }
  
  if (!"NPRId" %in% names(df)) {
    stop("NPRId column not found after reading file: ", file_label,
         "\nColumns found:\n", paste(names(df), collapse = ", "))
  }
  
  if (!"KommuneNr" %in% names(df)) df$KommuneNr <- NA_character_
  if (!"BydelNr" %in% names(df))   df$BydelNr   <- NA_character_
  df
}

read_and_clean_csv_input <- function(path) {
  df <- read_csv_header_then_data(path)
  standardize_powerbi_names(df, basename(path))
}

# ==============================================================================
# Find region file under base_dir/<RHF>/
# ==============================================================================
find_region_input_file <- function(region, suffix) {
  region_dir <- file.path(CFG$base_dir, region)
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

load_region_dataset <- function(region, suffix) {
  path <- find_region_input_file(region, suffix)
  if (is.na(path)) {
    log_warn("No input CSV found for region %s (suffix=%s).", region, suffix)
    return(NULL)
  }
  log_info("Reading input: %s", path)
  read_and_clean_csv_input(path)
}

# ==============================================================================
# Mapping NPRId -> lopenr
# ==============================================================================
build_nprid_request <- function(all_rows) {
  all_rows %>% distinct(NPRId) %>% filter(!is.na(NPRId), NPRId != "NPRId", NPRId != "NULL") %>% mutate(Dummy = "")
}

write_nprid_request_file <- function(nprid_request, suffix) {
  out_path <- file.path(CFG$app_dir, paste0("NPRId_RHF_Pakkeforløp_", suffix, ".csv"))
  readr::write_delim(nprid_request, out_path, delim = ";", na = "")
  log_info("Saved NPRId request file: %s (rows=%s)", out_path, nrow(nprid_request))
  out_path
}

lopenr_file_path <- function(suffix) {
  file.path(CFG$app_dir, paste0("NPRId_RHF_Pakkeforløp_", suffix, "_lnr.csv"))
}

# IMPORTANT: *_lnr.csv has NO header (as you stated)
read_lopenr_file <- function(suffix) {
  path <- lopenr_file_path(suffix)
  if (!file.exists(path)) stop("Løpenummer file not found: ", path)
  
  df <- readr::read_delim(
    path,
    delim = ";",
    col_names = c("NPRId", "lopenr"),
    col_types = readr::cols(.default = readr::col_character()),
    na = character(),                 # keep blanks as ""
    show_col_types = FALSE,
    progress = FALSE,
    trim_ws = TRUE
  )
  
  df %>% mutate(
    NPRId  = str_replace_all(NPRId, "\r", "") %>% str_trim(),
    lopenr = str_replace_all(lopenr, "\r", "") %>% str_replace_all(";", "") %>% str_trim()
  )
}

# validate mapping completeness (STOP if any NPRId lacks lopenr)
validate_lopenr_map <- function(all_rows, id_map, suffix) {
  
  id_map2 <- id_map %>%
    transmute(
      NPRId  = str_trim(as.character(NPRId))  %>% na_if(""),
      lopenr = str_trim(as.character(lopenr)) %>% na_if("")
    )
  
  if (nrow(id_map2) == 0) {
    stop("STOP: Løpenummerfil er tom: ", lopenr_file_path(suffix), call. = FALSE)
  }
  
  if (all(is.na(id_map2$lopenr))) {
    stop("STOP: Løpenummer er ikke generert (alle lopenr er tom/NA) i: ",
         lopenr_file_path(suffix), call. = FALSE)
  }
  
  dup <- id_map2 %>% filter(!is.na(NPRId)) %>% count(NPRId) %>% filter(n > 1)
  if (nrow(dup) > 0) {
    stop("STOP: Duplikate NPRId i løpenummerfil. Eksempel: ",
         paste(head(dup$NPRId, 10), collapse = ", "),
         call. = FALSE)
  }
  
  needed <- all_rows %>%
    filter(!is.na(NPRId), NPRId != "NPRId", NPRId != "NULL") %>%
    distinct(NPRId) %>%
    mutate(NPRId = str_trim(as.character(NPRId)))
  
  missing_map <- needed %>%
    left_join(id_map2, by = "NPRId") %>%
    filter(is.na(lopenr))
  
  if (nrow(missing_map) > 0) {
    miss_path <- file.path(CFG$out_dir, paste0("QC_missing_lopenr_NPRId_", suffix, ".csv"))
    readr::write_csv(missing_map, miss_path)
    
    stop(
      "STOP: ", nrow(missing_map), " NPRId mangler løpenummer i returfilen.\n",
      "Se liste: ", miss_path, "\n",
      "Eksempel NPRId: ", paste(head(missing_map$NPRId, 10), collapse = ", "),
      call. = FALSE
    )
  }
  
  log_info("Løpenummer mapping OK: alle NPRId har lopenr (%s).", suffix)
  invisible(TRUE)
}

# ==============================================================================
#  geo-fix missing kommune
# ==============================================================================
connect_somhoved <- function() {
  con <- DBI::dbConnect(
    odbc::odbc(),
    Driver = "SQL Server",
    Server = "NPRSQLprod",
    Database = "NPRNasjonaltDatagrunnlag",
    Trusted_connection = "True"
  )
  DBI::dbExecute(con, "SET NOCOUNT ON;")
  con
}

detect_db_npr_col <- function(con) {
  cols <- names(DBI::dbGetQuery(con, "SELECT TOP 0 * FROM dbo.SOMHoved;"))
  if ("NPRId" %in% cols) return("NPRId")
  if ("NPRid" %in% cols) return("NPRid")
  stop("Neither NPRId nor NPRid exists in dbo.SOMHoved.")
}

fetch_geo_fixes <- function(con, missing_ids, db_npr_col) {
  if (length(missing_ids) == 0) return(tibble(NPRId=character(), komnrhjem2=character(), bydel2=character(), aar=integer()))
  temp_name <- paste0("#TempMissing_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1000:9999, 1))
  temp_tbl <- tibble(!!db_npr_col := as.character(missing_ids))
  dplyr::copy_to(con, temp_tbl, name = temp_name, temporary = TRUE, overwrite = TRUE)
  
  sql_txt <- paste0(
    "WITH Kom AS (",
    " SELECT s.", db_npr_col, " AS NPRId, s.komnrhjem2, s.bydel2, s.aar,",
    " ROW_NUMBER() OVER (PARTITION BY s.", db_npr_col, " ORDER BY s.aar DESC) AS rn",
    " FROM dbo.SOMHoved s INNER JOIN ", temp_name, " t ON s.", db_npr_col, " = t.", db_npr_col,
    ") SELECT NPRId, komnrhjem2, bydel2, aar FROM Kom WHERE rn = 1;"
  )
  
  as_tibble(DBI::dbGetQuery(con, sql_txt)) %>%
    mutate(NPRId=as.character(NPRId), komnrhjem2=as.character(komnrhjem2),
           bydel2=as.character(bydel2), aar=as.integer(aar))
}

geo_overrides <- tibble::tribble(
  ~NPRId, ~aar, ~komnrhjem2,
  "1062924", 2008L, "0220",
  "2561864", 2008L, "0706",
  "3200706", 2008L, "0906",
  "3602577", 2008L, "1135"
)

apply_geo_overrides <- function(geo_fixes) {
  geo_fixes %>%
    left_join(geo_overrides, by=c("NPRId","aar"), suffix=c("","_override")) %>%
    mutate(komnrhjem2 = coalesce(komnrhjem2_override, komnrhjem2)) %>%
    select(-komnrhjem2_override)
}

# ==============================================================================
# Final delivery
# ==============================================================================
FINAL_COLS <- c(
  "lopenr","institusjonId","institusjon","reshIdFagEnhet","Fagenhet",
  "KommuneNr","BydelNr","FødselsDato","DødsDato","pfInstansId","Forløp",
  "Pakkeforløp","AntInst","AntHf","År","StartDato","Ainst","UtrStartDato",
  "Sinst","KliniskBeslutn","TypeBeslutn","Cinst","StartBeh","TypeBeh","Finst",
  "Avslutn","Xinst","OF1","OF1std","OF2","OF2std","OF3","OF3std","OF4","OF4std",
  "Overf","AnsiennDato"
)

validate_input_columns <- function(report_df, region) {
  needed <- setdiff(FINAL_COLS, "lopenr")
  missing <- setdiff(needed, names(report_df))
  if (length(missing) > 0) {
    stop("Missing required columns in region file: ", region,
         "\nMissing: ", paste(missing, collapse = ", "),
         "\nColumns present:\n", paste(names(report_df), collapse = ", "))
  }
}

make_delivery <- function(report_df, id_map, geo_fixes, region) {
  validate_input_columns(report_df, region)
  
  # Note: validate_lopenr_map() ensures mapping completeness; no silent dropping should occur.
  report_df %>%
    filter(!is.na(NPRId), NPRId != "NPRId", NPRId != "NULL") %>%
    left_join(id_map, by = "NPRId") %>%
    left_join(geo_fixes, by = "NPRId") %>%
    mutate(
      # fill KommuneNr when NA OR ""
      KommuneNr = if_else(is.na(KommuneNr) | KommuneNr == "", coalesce(komnrhjem2, ""), KommuneNr),
      
      # keep bydel rule conservative (only fill when BydeNr is NA)
      BydelNr   = if_else(KommuneNr == komnrhjem2 & is.na(BydelNr), bydel2, BydelNr)
    ) %>%
    select(-any_of(c("komnrhjem2","bydel2","aar"))) %>%
    select(all_of(FINAL_COLS))
}

write_delivery <- function(df, region, suffix) {
  out_path <- file.path(CFG$out_dir, paste0("Utlevering_", region, "_", suffix, ".csv"))
  readr::write_delim(df, out_path, delim = ";", na = "")
  log_info("Saved delivery: %s (rows=%s)", out_path, nrow(df))
  out_path
}

# ==============================================================================
# QC: Missing kommune + plots + View()
# ==============================================================================
qc_missing_kommune <- function(df, rhf_label) {
  tibble(
    RHF = rhf_label,
    n_rows = nrow(df),
    missing_kommune = sum(is.na(df$KommuneNr) | df$KommuneNr == "")
  )
}

qc_missing_kommune_before_after <- function(reports_raw, finals) {
  before <- bind_rows(imap(reports_raw, ~ qc_missing_kommune(.x, .y)))
  after  <- bind_rows(imap(finals,      ~ qc_missing_kommune(.x, .y)))
  delta <- before %>%
    select(RHF, missing_kommune) %>% rename(missing_kommune_before = missing_kommune) %>%
    left_join(after %>% select(RHF, missing_kommune) %>% rename(missing_kommune_after = missing_kommune), by = "RHF") %>%
    mutate(fixed_kommune = missing_kommune_before - missing_kommune_after)
  list(before = before, after = after, delta = delta)
}

plot_missing_kommune_before_after <- function(qc_before, qc_after) {
  df <- qc_before %>%
    select(RHF, before = missing_kommune) %>%
    left_join(qc_after %>% select(RHF, after = missing_kommune), by = "RHF") %>%
    pivot_longer(cols = c(before, after), names_to = "stage", values_to = "missing_kommune")
  
  ggplot(df, aes(x = RHF, y = missing_kommune, fill = stage)) +
    geom_col(position = "dodge") +
    labs(title = "Missing KommuneNr: before vs after geo-fix", x = "RHF", y = "Missing KommuneNr", fill = "") +
    theme_minimal(base_size = 12)
}

plot_missing_kommune_delta <- function(qc_delta) {
  ggplot(qc_delta, aes(x = RHF, y = fixed_kommune, fill = RHF)) +
    geom_col() +
    labs(title = "Fixed KommuneNr (before - after)", x = "RHF", y = "Fixed KommuneNr") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")
}

# ==============================================================================
# Monthly counts 
# ==============================================================================
parse_startdato <- function(x) {
  out <- suppressWarnings(lubridate::parse_date_time(
    x, orders = c("Ymd","dmY","Y.m.d","d.m.Y","Ymd HMS","dmY HMS","Y-m-d","d-m-Y")
  ))
  as.Date(out)
}

monthly_counts_reporting_year <- function(final_named_list, reporting_year) {
  all <- bind_rows(imap(final_named_list, ~ mutate(.x, RHF = .y))) %>%
    mutate(StartDato_parsed = parse_startdato(StartDato),
           month = floor_date(StartDato_parsed, "month")) %>%
    filter(!is.na(month), year(month) == reporting_year)
  
  full_months <- seq.Date(as.Date(sprintf("%d-01-01", reporting_year)),
                          as.Date(sprintf("%d-12-01", reporting_year)), by = "month")
  
  all %>% count(RHF, month, name = "n_rows") %>%
    tidyr::complete(RHF, month = full_months, fill = list(n_rows = 0)) %>%
    arrange(RHF, month)
}

plot_monthly_counts <- function(monthly_tbl, reporting_year) {
  ggplot(monthly_tbl, aes(x = month, y = n_rows, color = RHF)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
    labs(title = paste0("Monthly pakkeforløp rows per RHF (", reporting_year, ")"),
         x = "Month", y = "Rows", color = "RHF") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# ==============================================================================
# Main processing
# ==============================================================================
process_one_suffix <- function(suffix) {
  log_info("==========================================")
  log_info("Processing suffix: %s", suffix)
  log_info("==========================================")
  
  reporting_year <- year(suffix_to_month_date(suffix))
  
  reports <- CFG$regions %>% set_names() %>% map(~ load_region_dataset(.x, suffix))
  reports <- reports[!map_lgl(reports, is.null)]
  if (length(reports) == 0) {
    log_warn("No region input files found for suffix %s. Skipping.", suffix)
    return(invisible(list(status = "no_inputs", suffix = suffix)))
  }
  
  all_rows <- bind_rows(reports)
  
  # Always create request file
  req_path <- write_nprid_request_file(build_nprid_request(all_rows), suffix)
  
  # If mapping missing -> return needs_lnr (main() will stop after loop)
  lnr_path <- lopenr_file_path(suffix)
  if (!file.exists(lnr_path)) {
    log_warn("Løpenummerfil mangler for suffix %s", suffix)
    return(invisible(list(status = "needs_lnr", suffix = suffix, req_path = req_path, lnr_path = lnr_path)))
  }
  
  # Read mapping (no header) + validate completeness
  id_map <- read_lopenr_file(suffix)
  validate_lopenr_map(all_rows, id_map, suffix)
  
  # Geo-fix request: include NA OR "" kommune
  missing_geo_ids <- all_rows %>%
    filter(!is.na(NPRId), NPRId != "NPRId", NPRId != "NULL",
           is.na(KommuneNr) | KommuneNr == "") %>%
    distinct(NPRId) %>%
    pull(NPRId)
  
  con <- connect_somhoved()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  db_npr_col <- detect_db_npr_col(con)
  
  geo_fixes <- fetch_geo_fixes(con, missing_geo_ids, db_npr_col) %>% apply_geo_overrides()
  
  final_named_list <- list()
  out_files <- c()
  
  walk(names(reports), function(region) {
    df_final <- make_delivery(reports[[region]], id_map, geo_fixes, region)
    final_named_list[[region]] <<- df_final
    out_files <<- c(out_files, write_delivery(df_final, region, suffix))
  })
  
  # QC: Missing kommune
  if (isTRUE(CFG$run_qc_missing_kommune)) {
    qc_kommune <- qc_missing_kommune_before_after(reports, final_named_list)
    
    show_in_view(qc_kommune$before, paste0("QC_missing_kommune_BEFORE_", suffix))
    show_in_view(qc_kommune$after,  paste0("QC_missing_kommune_AFTER_",  suffix))
    show_in_view(qc_kommune$delta,  paste0("QC_missing_kommune_DELTA_",  suffix))
    
    if (isTRUE(CFG$write_qc_files)) {
      write_csv(qc_kommune$before, file.path(CFG$out_dir, paste0("QC_missing_kommune_before_", suffix, ".csv")))
      write_csv(qc_kommune$after,  file.path(CFG$out_dir, paste0("QC_missing_kommune_after_",  suffix, ".csv")))
      write_csv(qc_kommune$delta,  file.path(CFG$out_dir, paste0("QC_missing_kommune_delta_",  suffix, ".csv")))
    }
    
    if (isTRUE(CFG$plot_missing_kommune)) {
      p1 <- plot_missing_kommune_before_after(qc_kommune$before, qc_kommune$after)
      p2 <- plot_missing_kommune_delta(qc_kommune$delta)
      print(p1); print(p2)
      
      if (isTRUE(CFG$save_plots)) {
        ggsave(file.path(CFG$out_dir, paste0("PLOT_missing_kommune_before_after_", suffix, ".png")),
               p1, width = 8, height = 4, dpi = 150)
        ggsave(file.path(CFG$out_dir, paste0("PLOT_missing_kommune_delta_", suffix, ".png")),
               p2, width = 6, height = 4, dpi = 150)
      }
    }
  }
  
  # Monthly counts (reporting year)
  if (isTRUE(CFG$run_monthly_counts_reporting_year)) {
    monthly_tbl <- monthly_counts_reporting_year(final_named_list, reporting_year)
    show_in_view(monthly_tbl, paste0("QC_monthly_counts_", reporting_year, "_", suffix))
    
    if (isTRUE(CFG$write_qc_files)) {
      write_csv(monthly_tbl, file.path(CFG$out_dir, paste0("QC_monthly_counts_", reporting_year, "_", suffix, ".csv")))
    }
    
    if (isTRUE(CFG$plot_monthly_counts)) {
      p <- plot_monthly_counts(monthly_tbl, reporting_year)
      print(p)
      if (isTRUE(CFG$save_plots)) {
        ggsave(file.path(CFG$out_dir, paste0("PLOT_monthly_counts_", reporting_year, "_", suffix, ".png")),
               p, width = 10, height = 5, dpi = 150)
      }
    }
  }
  
  invisible(list(status = "delivered", suffix = suffix, out_files = out_files))
}

# ==============================================================================
# MAIN: stop ONLY when mapping is missing (after generating request files)
# ==============================================================================
main <- function(run_date = Sys.Date()) {
  log_info("Run date: %s", as.character(run_date))
  suffixes <- map_chr(get_reporting_months(run_date), make_suffix)
  log_info("Suffixes to process: %s", paste(suffixes, collapse = ", "))
  
  needs <- list()
  
  for (sfx in suffixes) {
    res <- process_one_suffix(sfx)
    if (is.list(res) && identical(res$status, "needs_lnr")) {
      needs[[sfx]] <- res
      next
    }
  }
  
  if (length(needs) > 0) {
    msg <- paste0(
      "STOP: Mangler løpenummerfil(er). Bestill løpenummer i app og kjør scriptet på nytt.\n\n",
      paste(
        map_chr(names(needs), function(sfx) {
          paste0(
            "- Suffix ", sfx,
            "\n  Request file:  ", needs[[sfx]]$req_path,
            "\n  Expected file: ", needs[[sfx]]$lnr_path
          )
        }),
        collapse = "\n\n"
      )
    )
    stop(msg, call. = FALSE)
  }
  
  log_info("All done.")
}

# ==============================================================================
# RUN
# ==============================================================================
main(Sys.Date())

# Reproducible manual runs:
# main(as.Date("2026-01-19"))  # processes des25

slutt_tid <- Sys.time()
elapsed_sec <- as.numeric(difftime(slutt_tid, start_tid, units = "secs"))
elapsed_min <- elapsed_sec %/% 60
elapsed_rem <- round(elapsed_sec %% 60, 1)
message("Script total time: ", elapsed_min, " min ", elapsed_rem, " sec")
