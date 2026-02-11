# ==============================================================================
# 01_config.R
# Felles konstanter og hjelpefunksjoner
# ============================================================================== 

CFG <- list(
  regions = c("HSØ", "HV", "HMN"),
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
  ~NPRId, ~aar, ~komnrhjem2,
  "1062924", 2008L, "0220",
  "2561864", 2008L, "0706",
  "3200706", 2008L, "0906",
  "3602577", 2008L, "1135"
)

log_info <- function(USER, ...) if (isTRUE(USER$verbose)) message("[INFO] ", sprintf(...))
log_warn <- function(USER, ...) message("[WARN] ", sprintf(...))
stop_user <- function(...) stop(sprintf(...), call. = FALSE)

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

month_to_suffix <- function(month_date) {
  d <- as.Date(month_date)
  paste0(CFG$months_nor[lubridate::month(d)], format(d, "%y"))
}

request_file_path <- function(USER, suffix) file.path(USER$app_dir, paste0("NPRId_RHF_Pakkeforløp_", suffix, ".csv"))
lnr_file_path     <- function(USER, suffix) file.path(USER$app_dir, paste0("NPRId_RHF_Pakkeforløp_", suffix, "_lnr.csv"))

delivery_file_path_csv  <- function(USER, region, suffix) file.path(USER$out_dir, paste0("Utlevering_", region, "_", suffix, ".csv"))
delivery_file_path_xlsx <- function(USER, region, suffix) file.path(USER$out_dir, paste0("Utlevering_", region, "_", suffix, ".xlsx"))

cache_dir <- function(USER) file.path(USER$out_dir, "_cache_parquet")
parquet_cache_path <- function(USER, region, suffix) file.path(cache_dir(USER), paste0("clean_", region, "_", suffix, ".parquet"))
arrow_available <- function() requireNamespace("arrow", quietly = TRUE)

excel_available <- function() {
  requireNamespace("openxlsx", quietly = TRUE) || requireNamespace("writexl", quietly = TRUE)
}

parse_any_date <- function(x) {
  out <- suppressWarnings(lubridate::parse_date_time(
    x,
    orders = c("Ymd","dmY","Y-m-d","d-m-Y","Y.m.d","d.m.Y",
               "Ymd HMS","dmY HMS","Y-m-d HMS","d-m-Y HMS")
  ))
  as.Date(out)
}
