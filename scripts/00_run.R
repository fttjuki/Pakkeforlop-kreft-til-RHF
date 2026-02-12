# ==============================================================================
# 00_run.R (START HER)
# Ikke-koder: Endre kun USER-blokken. Kjør filen med "Source".
# ------------------------------------------------------------------------------
# Dette er "startfila" / inngangspunktet for hele prosjektet.
# Tenk på dette som en oppskrift:
# 1) Last inn nødvendige pakker (tidyverse, dato-håndtering, database osv.)
# 2) Definer innstillinger (USER) som styrer hele kjøringen
# 3) Finn hvor skriptet ligger, og hvor modul-filene ligger (R-mappen)
# 4) "source" alle modulene (funksjoner) vi trenger
# 5) Kjør main(USER) som gjør jobben
# 6) Print hvor lang tid det tok
# ==============================================================================

suppressPackageStartupMessages({
  # suppressPackageStartupMessages():
  # - Skjuler "oppstarts-meldinger" fra pakker når de lastes
  # - Hvorfor: Konsollen blir ryddigere og lettere å lese for nybegynnere

  library(tidyverse)
  # tidyverse: samling av pakker for dataarbeid (dplyr, readr, tidyr, ggplot2, osv.)
  # Hvorfor: Vi bruker %>% / dplyr funksjoner, lesing av CSV, og generell datarydding

  library(lubridate)
  # lubridate: enklere håndtering av datoer (år, måned, parse dato-format)
  # Hvorfor: Vi lager suffix (f.eks. jan26) og trekker ut år fra datoer

  library(DBI)
  # DBI: standard grensesnitt for database-tilkoblinger i R
  # Hvorfor: Vi bruker DBI::dbConnect, dbGetQuery, dbExecute osv.

  library(odbc)
  # odbc: gjør at R kan koble seg til SQL Server via ODBC
  # Hvorfor: geo-fix (kommune/bydel) hentes fra SQL Server-tabellen SOMHoved
})

USER <- list(
  # USER-listen er den viktigste delen du skal endre.
  # Dette er innstillinger som bestemmer hva som skjer.
  # Tips: Endre bare verdier til høyre for "=".

  step = "STEP2", # "STEP1" eller "STEP2"
  # step:
  # - STEP1: lager bare bestillingsfil (unik NPRId-liste) og stopper
  # - STEP2: krever at returfil med løpenr finnes, og lager full utlevering

  months = c("2025-12-01"), # format "YYYY-MM-01"
  # months:
  # - Hvilke måneder du vil kjøre for
  # - Må være første dag i måned (YYYY-MM-01)
  # - Koden lager suffix ut fra dette (f.eks. "des25")

  base_dir = "N:/Utleveringer/STYRINGSDATA/RHF Pakkeforløp kreft/R",
  # base_dir:
  # - Hovedmappen der inputfiler ligger, typisk sortert per region
  # - For eksempel kan du ha undermapper HSØ, HV, HMN osv.

  app_dir = "//fihr.no/dfs/NPR/Temp/NPR_RegistrerUtlevering",
  # app_dir:
  # - "Arbeidsmappe" / app-mappe
  # - Her lagres bl.a. bestillingsfilen med NPRId og forventet returfil med løpenr
  # - Tenk på den som en "mellomlager-mappe" for filer som sendes/returneres

  out_dir = "N:/Utleveringer/STYRINGSDATA/RHF Pakkeforløp Kreft/R",
  # out_dir:
  # - Der sluttresultat (utlevering CSV/Excel) og QC-filer og runbook lagres
  # - Dette er den viktigste mappen å sjekke etter kjøring

  fill_missing_kommune = TRUE,
  # fill_missing_kommune:
  # - TRUE: prøver å fylle KommuneNr/BydelNr som mangler ved databaseoppslag
  # - FALSE: gjør ikke databaseoppslag; leveranse kan få flere manglende kommuner

  drop_column_M = TRUE,
  # drop_column_M:
  # - TRUE: fjerner kolonne nr 13 (ofte "M") hvis den er tom i input
  # - Hvorfor: Noen PowerBI-eksporter får en ekstra tom kolonne som skaper støy

  use_parquet_cache = TRUE,
  # use_parquet_cache:
  # - TRUE: lagrer en "cache" i Parquet-format for raskere kjøring neste gang
  # - Krever at pakken "arrow" er installert
  # - Hvis arrow mangler, logger koden at cache ikke kan brukes

  qc_simple = TRUE,
  # qc_simple:
  # - TRUE: skriver en enkel QC-oppsummering (antall rader og manglende kommune)
  # - FALSE: hopper over denne QC-filen

  verbose = TRUE
  # verbose:
  # - TRUE: skriver flere INFO-meldinger til konsollen (mer detaljer)
  # - FALSE: mer stille kjøring (mindre tekst)
)

SCRIPT_DIR <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)),
                       error = function(e) getwd())
# SCRIPT_DIR:
# - Vi prøver å finne mappen der denne 00_run.R-filen ligger
# - sys.frame(1)$ofile finnes ofte når du "Source" i RStudio
# - normalizePath gjør det til en full sti
# - dirname tar bare mappen (uten filnavn)
# - tryCatch: hvis dette feiler (f.eks. ved annen kjøremåte), bruk getwd()
# Hvorfor: Da kan vi alltid finne modul-mappen relativt til denne fila.

MODULE_DIR <- file.path(SCRIPT_DIR, "R")
# MODULE_DIR:
# - Modulene (01_config.R, 02_powerbi_io.R, osv.) ligger typisk i en undermappe "R"
# - file.path bygger korrekt filsti for OS

source(file.path(MODULE_DIR, "01_config.R"))
# source:
# - Leser inn og kjører filen
# - Hvorfor: Denne filen definerer felles konstanter og hjelpefunksjoner
# - Etter source kan vi bruke funksjoner/objekter derfra (CFG, ensure_dir, osv.)

source(file.path(MODULE_DIR, "07_runbook.R"))
# Runbook-funksjoner (rb_init, rb_add, rb_write) lastes inn.
# Hvorfor: Vi vil logge alt som skjer til en tekstfil i out_dir.

source(file.path(MODULE_DIR, "02_powerbi_io.R"))
# Leser og renser PowerBI CSV-filer.

source(file.path(MODULE_DIR, "03_lopenr.R"))
# Lager bestillingsfil med unike NPRId + leser/validerer returfil med løpenr.

source(file.path(MODULE_DIR, "04_geo_fix.R"))
# Henter kommune/bydel fra database for NPRId som mangler.

source(file.path(MODULE_DIR, "05_delivery.R"))
# Lager leveranseformat + skriver CSV/Excel.

source(file.path(MODULE_DIR, "06_pipeline.R"))
# Hovedpipeline: orchestrerer STEP1/STEP2 for alle måneder.

start_tid <- Sys.time()
# Start tidtaking før main() kjører.
# Hvorfor: For å se hvor lang tid hele jobben tar.

main(USER)
# Kjør hele prosessen med innstillingene i USER.

slutt_tid <- Sys.time()
# Slutt tidtaking etter main() er ferdig.

message("Total tid: ", round(difftime(slutt_tid, start_tid, units = "mins"), 2), " minutter")
# Skriver total tid til konsollen.
# difftime beregner tidsforskjell, units="mins" gir minutter.
# round(..., 2) runder til 2 desimaler for pen utskrift.