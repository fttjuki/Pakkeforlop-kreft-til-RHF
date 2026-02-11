# ==============================================================================
# config/USER_example.R  (TEMPLATE)
#
# Slik bruker du denne:
#   1) Kopier denne filen til: config/USER_local.R
#   2) Rediger USER_local.R med dine interne stier.
#   3) Kjør scripts/00_run.R
#
# Viktig i public repo:
#   - Ikke legg interne stier/servernavn i filer som committes.
#   - USER_local.R er i .gitignore og kan inneholde interne stier.
# ============================================================================== 

USER <- list(
  # STEP1 = lager bestillingsfil, STEP2 = lager utlevering
  step = "STEP1",

  # Måned(er) som skal behandles. Format: "YYYY-MM-01"
  months = c("2025-12-01"),

  # ---- Stier (Fyll inn i USER_local.R) ----
  base_dir = "PATH_TO_POWERBI_EXPORTS",  # forventer undermapper: HSØ / HV / HMN
  app_dir  = "PATH_TO_APP_DIR",          # bestillingsfil + *_lnr.csv
  out_dir  = "PATH_TO_OUTPUT_DIR",       # utlevering + runbook + QC

  # Valg
  fill_missing_kommune = TRUE,
  drop_column_M = TRUE,
  use_parquet_cache = TRUE,
  qc_simple = TRUE,
  verbose = TRUE
)
