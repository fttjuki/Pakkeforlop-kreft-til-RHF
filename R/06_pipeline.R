# ==============================================================================
# 06_pipeline.R
# Orkestrerer STEP1/STEP2 og skriver alltid Runbook
# ============================================================================== 

process_suffix <- function(USER, suffix) {

  rb_section(paste0("PROCESS SUFFIX: ", suffix))
  log_info(USER, "Processing suffix: %s", suffix)

  region_objs <- CFG$regions %>%
    rlang::set_names() %>%
    purrr::map(~ load_region_dataset(USER, .x, suffix))

  region_objs <- region_objs[!purrr::map_lgl(region_objs, is.null)]

  if (length(region_objs) == 0) {
    log_warn(USER, "No input files found for %s. Skipping.", suffix)
    rb_add("No input files found. Nothing to do.")
    return(invisible(list(status = "no_inputs", suffix = suffix)))
  }

  reports <- purrr::map(region_objs, "data")
  all_rows <- dplyr::bind_rows(reports)

  rb_add(paste0("Total rader (alle RHF samlet): ", nrow(all_rows)))
  rb_add(paste0("Unike NPRId (alle RHF samlet): ", dplyr::n_distinct(all_rows$NPRId)))

  req_tbl <- build_nprid_request(all_rows)
  rb_add(paste0("Rader i bestillingsfil (unike NPRId): ", nrow(req_tbl)))
  req_path <- write_nprid_request_file(USER, req_tbl, suffix)

  if (toupper(USER$step) == "STEP1") {
    rb_add("STEP1 selected -> stopping after writing request file.")
    return(invisible(list(status = "step1_done", suffix = suffix, req_path = req_path)))
  }

  id_map <- read_lopenr_file(USER, suffix)
  rb_add(paste0("Rader i løpenr-returfil: ", nrow(id_map)))
  validate_lopenr_map(USER, all_rows, id_map, suffix)

  geo_fixes <- tibble::tibble(NPRId=character(), komnrhjem2=character(), bydel2=character(), aar=integer())

  if (isTRUE(USER$fill_missing_kommune)) {
    rb_section("GEO FIX (SOMHoved)")

    missing_geo_ids <- all_rows %>%
      dplyr::filter(!is.na(NPRId), NPRId != "", NPRId != "NPRId", NPRId != "NULL",
                    is.na(KommuneNr) | KommuneNr == "") %>%
      dplyr::distinct(NPRId) %>%
      dplyr::pull(NPRId)

    rb_add(paste0("Missing KommuneNr - unique NPRId: ", length(missing_geo_ids)))

    con <- connect_somhoved()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    db_npr_col <- detect_db_npr_col(con)

    geo_fixes <- fetch_geo_fixes(con, missing_geo_ids, db_npr_col) %>%
      apply_geo_overrides()

    rb_add(paste0("Geo fixes fetched rows: ", nrow(geo_fixes)))
  } else {
    rb_add("Geo fix disabled by USER setting")
  }

  rb_section("WRITE DELIVERIES")
  final_list <- list()
  out_files <- list()

  for (region in names(reports)) {
    df_final <- make_delivery(USER, reports[[region]], id_map, geo_fixes, region, suffix)
    rb_add(paste0("Rader i utlevering (", region, "): ", nrow(df_final)))

    final_list[[region]] <- df_final

    paths <- write_delivery(USER, df_final, region, suffix)
    out_files[[region]] <- paths
  }

  if (isTRUE(USER$qc_simple)) {
    rb_section("QC")
    qc <- qc_simple_summary(final_list, suffix)
    qc_path <- file.path(USER$out_dir, paste0("QC_summary_", suffix, ".csv"))
    readr::write_csv(qc, qc_path)
    rb_add(paste0("QC summary written: ", qc_path))
    print(qc)
  }

  invisible(list(status = "delivered", suffix = suffix, out_files = out_files, req_path = req_path))
}

main <- function(USER) {

  rb_init(USER)
  on.exit({
    rb_section("END")
    rb_add(paste0("Finished: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    rb_write()
  }, add = TRUE)

  ensure_dir(USER$out_dir)
  ensure_dir(USER$app_dir)

  if (!toupper(USER$step) %in% c("STEP1","STEP2")) stop_user("USER$step must be STEP1 or STEP2")
  if (length(USER$months) == 0) stop_user("USER$months is empty")

  month_dates <- as.Date(USER$months)
  if (any(is.na(month_dates))) stop_user("Invalid date in USER$months. Use format YYYY-MM-01")

  suffixes <- purrr::map_chr(month_dates, month_to_suffix)

  rb_section("PLAN")
  rb_add(paste0("Suffixes: ", paste(suffixes, collapse = ", ")))

  if (isTRUE(USER$use_parquet_cache)) {
    if (arrow_available()) {
      ensure_dir(cache_dir(USER))
      rb_add(paste0("Parquet cache: ON (", cache_dir(USER), ")"))
    } else {
      rb_add("Parquet cache: requested but arrow not installed -> OFF")
    }
  } else {
    rb_add("Parquet cache: OFF")
  }

  if (!excel_available()) {
    rb_add("NOTE: Excel output requires package openxlsx or writexl. Not detected at startup.")
  }

  for (sfx in suffixes) {
    tryCatch(
      process_suffix(USER, sfx),
      error = function(e) {
        rb_error(conditionMessage(e))
        rb_write()
        stop(e)
      }
    )
  }

  invisible(TRUE)
}
