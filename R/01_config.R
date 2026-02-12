## ==============================================================================
# 01_config.R
# Felles konstanter og hjelpefunksjoner
# ==============================================================================

# Vi lager en "liste" (som en samleboks) med faste verdier vi bruker flere steder.
CFG <- list(
  # Liste over regionkoder vi bruker i løkker/kjøringer.
  regions = c("HSØ", "HV", "HMN"),
  # Norske månedsforkortelser (brukes for filnavn-suffix).
  months_nor = c("jan","feb","mar","apr","mai","jun","jul","aug","sep","okt","nov","des")
)

# Her definerer vi standard rekkefølge på kolonner i sluttresultatet.
# Dette gjør at filer blir like hver gang (lett å sammenligne og levere).
FINAL_COLS <- c(
  "lopenr","institusjonId","institusjon","reshIdFagEnhet","Fagenhet",
  "KommuneNr","BydelNr","FødselsDato","DødsDato","pfInstansId","Forløp",
  "Pakkeforløp","AntInst","AntHf","År","StartDato","Ainst","UtrStartDato",
  "Sinst","KliniskBeslutn","TypeBeslutn","Cinst","StartBeh","TypeBeh","Finst",
  "Avslutn","Xinst","OF1","OF1std","OF2","OF2std","OF3","OF3std","OF4","OF4std",
  "Overf","AnsiennDato"
)

# Noen få spesielle unntak/overstyringer for geografi.
# Dette er "manuelle rettelser" for enkelte NPRId og år.
geo_overrides <- tibble::tribble(
  # Kolonnenavnene i tabellen:
  ~NPRId, ~aar, ~komnrhjem2,
  # Radene (eksempler på overstyring):
  "1062924", 2008L, "0220",
  "2561864", 2008L, "0706",
  "3200706", 2008L, "0906",
  "3602577", 2008L, "1135"
)

# Funksjon som skriver INFO-melding bare hvis USER$verbose er TRUE.
log_info <- function(USER, ...) if (isTRUE(USER$verbose)) message("[INFO] ", sprintf(...))

# Funksjon som alltid skriver en WARN-melding.
log_warn <- function(USER, ...) message("[WARN] ", sprintf(...))

# Funksjon som stopper programmet med en ryddig feilmelding (uten callstack).
stop_user <- function(...) stop(sprintf(...), call. = FALSE)

# Sikrer at en mappe finnes. Hvis ikke, opprettes den.
ensure_dir <- function(path) {
  # Sjekk om mappen finnes.
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  # Returnerer path usynlig (praktisk i pipelines).
  invisible(path)
}

# Lager et kort suffix for en måned, f.eks. "jan26".
month_to_suffix <- function(month_date) {
  # Gjør input om til Date.
  d <- as.Date(month_date)
  # Tar månedstallet og slår opp norsk måned + 2-sifret år.
  paste0(CFG$months_nor[lubridate::month(d)], format(d, "%y"))
}

# Bygger filsti til bestillingsfilen (unik NPRId-liste) som skal sendes ut.
request_file_path <- function(USER, suffix)
  file.path(USER$app_dir, paste0("NPRId_RHF_Pakkeforløp_", suffix, ".csv"))

# Bygger filsti til returfilen som kommer tilbake med løpenummer.
lnr_file_path <- function(USER, suffix)
  file.path(USER$app_dir, paste0("NPRId_RHF_Pakkeforløp_", suffix, "_lnr.csv"))

# Bygger filsti til utleveringsfil (CSV) for én region.
delivery_file_path_csv <- function(USER, region, suffix)
  file.path(USER$out_dir, paste0("Utlevering_", region, "_", suffix, ".csv"))

# Bygger filsti til utleveringsfil (Excel) for én region.
delivery_file_path_xlsx <- function(USER, region, suffix)
  file.path(USER$out_dir, paste0("Utlevering_", region, "_", suffix, ".xlsx"))

# Mappe for cache i Parquet-format (raskere lesing).
cache_dir <- function(USER) file.path(USER$out_dir, "_cache_parquet")

# Full filsti til Parquet-cache for en region og suffix.
parquet_cache_path <- function(USER, region, suffix)
  file.path(cache_dir(USER), paste0("clean_", region, "_", suffix, ".parquet"))

# Sjekker om pakken "arrow" er installert (trengs for Parquet).
arrow_available <- function() requireNamespace("arrow", quietly = TRUE)

# Sjekker om vi har Excel-pakker installert (for å skrive xlsx).
excel_available <- function() {
  requireNamespace("openxlsx", quietly = TRUE) ||
  requireNamespace("writexl", quietly = TRUE)
}

# Leser dato fra mange mulige formater og gjør det om til Date.
parse_any_date <- function(x) {
  # Prøver å tolke dato/tid på flere måter uten å spamme warnings.
  out <- suppressWarnings(lubridate::parse_date_time(
    x,
    orders = c("Ymd","dmY","Y-m-d","d-m-Y","Y.m.d","d.m.Y",
               "Ymd HMS","dmY HMS","Y-m-d HMS","d-m-Y HMS")
  ))
  # Returnerer bare dato-delen.
  as.Date(out)
}