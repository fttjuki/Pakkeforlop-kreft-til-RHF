# ==============================================================================
# 00_run.R  (START HER)
# Ikke-koder: Endre kun USER-blokken. Kjør filen med "Source".
# ============================================================================== 

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(DBI)
  library(odbc)
})

USER <- list(
  step = "STEP2",                      # "STEP1" eller "STEP2"
  months = c("2025-12-01"),            # format "YYYY-MM-01"

  base_dir = "N:/Utleveringer/STYRINGSDATA/RHF Pakkeforløp kreft/R",
  app_dir  = "//fihr.no/dfs/NPR/Temp/NPR_RegistrerUtlevering",
  out_dir  = "N:/Utleveringer/STYRINGSDATA/RHF Pakkeforløp Kreft/R",

  fill_missing_kommune = TRUE,
  drop_column_M = TRUE,
  use_parquet_cache = TRUE,
  qc_simple = TRUE,
  verbose = TRUE
)

SCRIPT_DIR <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)),
                       error = function(e) getwd())
MODULE_DIR <- file.path(SCRIPT_DIR, "R")

source(file.path(MODULE_DIR, "01_config.R"))
source(file.path(MODULE_DIR, "07_runbook.R"))
source(file.path(MODULE_DIR, "02_powerbi_io.R"))
source(file.path(MODULE_DIR, "03_lopenr.R"))
source(file.path(MODULE_DIR, "04_geo_fix.R"))
source(file.path(MODULE_DIR, "05_delivery.R"))
source(file.path(MODULE_DIR, "06_pipeline.R"))

start_tid <- Sys.time()
main(USER)
slutt_tid <- Sys.time()
message("Total tid: ", round(difftime(slutt_tid, start_tid, units = "mins"), 2), " minutter")
