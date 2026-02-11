# RHF Pakkeforløp kreft – R-pipeline (modulbasert)

Dette repoet inneholder en modulbasert R-pipeline for å lage utleveringsfiler for pakkeforløp kreft til RHF (HSØ, HV, HMN).

## Hva skriptet gjør
- **STEP1**: Leser PowerBI-eksport (CSV) → lager **bestillingsfil** med NPRId.
- **STEP2**: Leser returfil med løpenr (`*_lnr.csv`) → lager **utlevering** (CSV + Excel) + **Runbook**.


## Kom i gang 
1. Kopier `config/USER_example.R` → `config/USER_local.R`
2. Åpne `config/USER_local.R` og fyll inn dine **lokale/intern** stier.
3. Åpne `scripts/00_run.R` og trykk **Source**.

### STEP1 (før bestilling av løpenr)
- Sett i `config/USER_local.R`:
  - `step = "STEP1"`
  - `months = c("YYYY-MM-01")`
- Kjør `scripts/00_run.R`

### STEP2 (etter returfil)
- Legg `*_lnr.csv` i `app_dir`.
- Sett i `config/USER_local.R`:
  - `step = "STEP2"`
  - `months = c("YYYY-MM-01")`
- Kjør `scripts/00_run.R`

## Mappestruktur 
```
repo/
  scripts/00_run.R
  R/              # moduler
  config/         # USER_example.R (commit), USER_local.R (ignored)
  docs/           # veiledning
  output/         # lokal output (ignored)
```

## Avhengigheter
- Obligatorisk: `tidyverse`, `lubridate`, `DBI`, `odbc`
- Valgfritt: `arrow` (parquet-cache), `openxlsx` eller `writexl` (Excel-export)

## Lisens
Legg inn ønsket lisens (f.eks. MIT) i `LICENSE`.
