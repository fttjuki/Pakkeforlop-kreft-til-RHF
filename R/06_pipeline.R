# ==============================================================================
# 06_pipeline.R
# Orkestrerer STEP1/STEP2 og skriver alltid Runbook
# ------------------------------------------------------------------------------
# Hensikt (i enkel norsk):
# - Dette er "hovedmotoren" som kjører hele prosessen for én eller flere måneder.
# - Den gjør:
#   1) Leser alle regionfiler
#   2) Lager bestillingsfil med unike NPRId (STEP1)
#   3) Leser returfil med løpenr og validerer (STEP2)
#   4) Fyller ev. manglende kommune ved databaseoppslag
#   5) Lager og skriver ut leveransefiler per region
#   6) Skriver alltid runbook/logg for sporbarhet
# ==============================================================================

process_suffix <- function(USER, suffix) {
  # Starter en ny seksjon i runbook for denne suffix (måned).
  rb_section(paste0("PROCESS SUFFIX: ", suffix))

  # Info-logg for hvilken suffix som behandles.
  log_info(USER, "Processing suffix: %s", suffix)

  # Leser data for hver region i CFG$regions.
  # set_names() gir navn på listen slik at vi kan referere til region senere.
  # purrr::map kjører load_region_dataset for hver region.
  region_objs <- CFG$regions %>%
    rlang::set_names() %>%
    purrr::map(~ load_region_dataset(USER, .x, suffix))

  # Fjern regioner der det ikke fantes input (NULL).
  region_objs <- region_objs[!purrr::map_lgl(region_objs, is.null)]

  # Hvis ingen inputfiler ble funnet for denne suffix, hopp over.
  if (length(region_objs) == 0) {
    log_warn(USER, "No input files found for %s. Skipping.", suffix)
    rb_add("No input files found. Nothing to do.")
    return(invisible(list(status = "no_inputs", suffix = suffix)))
  }

  # region_objs er en liste med strukturer; vi trenger bare selve data-delen.
  reports <- purrr::map(region_objs, "data")

  # Slå sammen alle regioner til én stor tabell for å lage NPRId-bestilling.
  all_rows <- dplyr::bind_rows(reports)

  # Logg total rader og antall unike NPRId (for oversikt).
  rb_add(paste0("Total rader (alle RHF samlet): ", nrow(all_rows)))
  rb_add(paste0("Unike NPRId (alle RHF samlet): ", dplyr::n_distinct(all_rows$NPRId)))

  # Lag bestillingstabell med unike NPRId.
  req_tbl <- build_nprid_request(all_rows)
  rb_add(paste0("Rader i bestillingsfil (unike NPRId): ", nrow(req_tbl)))

  # Skriv bestillingsfil til disk og få tilbake filsti.
  req_path <- write_nprid_request_file(USER, req_tbl, suffix)

  # Hvis bruker har valgt STEP1:
  # - Vi stopper etter at bestillingsfilen er skrevet.
  # - Dette er nyttig fordi man ofte må sende filen ut og vente på returfil.
  if (toupper(USER$step) == "STEP1") {
    rb_add("STEP1 selected -> stopping after writing request file.")
    return(invisible(list(status = "step1_done", suffix = suffix, req_path = req_path)))
  }

  # STEP2: Les returfil med løpenummer.
  id_map <- read_lopenr_file(USER, suffix)
  rb_add(paste0("Rader i lĂ¸penr-returfil: ", nrow(id_map)))

  # Valider at alle NPRId som trengs faktisk finnes i returfilen.
  # Hvis ikke, stopper koden og skriver en QC-liste.
  validate_lopenr_map(USER, all_rows, id_map, suffix)

  # Start med tom geo_fixes (hvis vi ikke fyller kommune, forblir denne tom).
  geo_fixes <- tibble::tibble(NPRId=character(), komnrhjem2=character(), bydel2=character(), aar=integer())

  # Hvis bruker vil fylle manglende kommune, gjør database-oppslag.
  if (isTRUE(USER$fill_missing_kommune)) {
    rb_section("GEO FIX (SOMHoved)")

    # Finn NPRId som mangler KommuneNr i all_rows.
    missing_geo_ids <- all_rows %>%
      dplyr::filter(!is.na(NPRId), NPRId != "", NPRId != "NPRId", NPRId != "NULL",

      is.na(KommuneNr) |

      KommuneNr == "") %>%
      dplyr::distinct(NPRId) %>%
      dplyr::pull(NPRId)

    # Logg hvor mange unike NPRId som mangler kommune.
    rb_add(paste0("Missing KommuneNr - unique NPRId: ", length(missing_geo_ids)))

    # Koble til databasen.
    con <- connect_somhoved()
    # Sørg for at tilkoblingen alltid lukkes når funksjonen er ferdig.
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    # Finn riktig kolonnenavn for NPRId i databasen (NPRId eller NPRid).
    db_npr_col <- detect_db_npr_col(con)

    # Hent geo-fikser fra databasen og bruk eventuelle manuelle overstyringer.
    geo_fixes <- fetch_geo_fixes(con, missing_geo_ids, db_npr_col) %>%
      apply_geo_overrides()

    # Logg hvor mange rader geo_fixes inneholder (ofte 1 per NPRId).
    rb_add(paste0("Geo fixes fetched rows: ", nrow(geo_fixes)))
  } else {
    # Hvis ikke aktivert, logg at geo-fix er av.
    rb_add("Geo fix disabled by USER setting")
  }

  # Nå skal vi lage og skrive leveranser.
  rb_section("WRITE DELIVERIES")

  # final_list: holder ferdig df per region (brukes til QC).
  final_list <- list()

  # out_files: holder filstier for leveransefiler per region.
  out_files <- list()

  # Lag leveranse for hver region og skriv filene.
  for (region in names(reports)) {
    # make_delivery legger til lopenr og fyller evt kommune, og sikrer kolonner.
    df_final <- make_delivery(USER, reports[[region]], id_map, geo_fixes, region, suffix)

    # Logg radtall for leveransen.
    rb_add(paste0("Rader i utlevering (", region, "): ", nrow(df_final)))

    # Lagre df i listen.
    final_list[[region]] <- df_final

    # Skriv CSV/Excel og lagre filstiene.
    paths <- write_delivery(USER, df_final, region, suffix)
    out_files[[region]] <- paths
  }

  # Hvis enkel QC er aktivert, lag en QC-oppsummering og skriv den som CSV.
  if (isTRUE(USER$qc_simple)) {
    rb_section("QC")
    qc <- qc_simple_summary(final_list, suffix)
    qc_path <- file.path(USER$out_dir, paste0("QC_summary_", suffix, ".csv"))
    readr::write_csv(qc, qc_path)
    rb_add(paste0("QC summary written: ", qc_path))
    print(qc)
  }

  # Returner status + filstier (usynlig, men kan brukes av den som kaller funksjonen).
  invisible(list(status = "delivered", suffix = suffix, out_files = out_files, req_path = req_path))
}

main <- function(USER) {
  # Starter runbook/logg for hele kjøringen.
  rb_init(USER)

  # on.exit kjører uansett hvordan main avsluttes (suksess eller error).
  # Hvorfor: Vi vil ALLTID skrive runbook til slutt.
  on.exit({
    rb_section("END")
    rb_add(paste0("Finished: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    rb_write()
  }, add = TRUE)

  # Sørg for at output- og app-mapper finnes.
  ensure_dir(USER$out_dir)
  ensure_dir(USER$app_dir)

  # Sjekk at USER$step er riktig, ellers stopp tidlig.
  if (!toupper(USER$step) %in% c("STEP1","STEP2")) stop_user("USER$step must be STEP1 or STEP2")

  # Sjekk at vi faktisk har måneder å kjøre.
  if (length(USER$months) == 0) stop_user("USER$months is empty")

  # Gjør om strengene til Date.
  month_dates <- as.Date(USER$months)

