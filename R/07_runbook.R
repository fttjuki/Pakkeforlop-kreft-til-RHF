# ==============================================================================
# 07_runbook.R
# Runbook = automatisk loggfil (tekst) som skrives til out_dir
# ------------------------------------------------------------------------------
# Hensikt (i enkel norsk):
# - En runbook er en logg som dokumenterer hva skriptet gjorde.
# - Den brukes for sporbarhet: hvilke innstillinger ble brukt, hvilke filer
#   ble skrevet, hvilke QC-steg ble gjort, og om det oppstod feil.
# - Runbook skrives til en tekstfil i USER$out_dir.
# ==============================================================================

rb_init <- function(USER) {
  # rb_init starter en ny runbook for en hel kjøring.

  # Sørg for at output-mappen finnes, ellers kan vi ikke skrive runbook.
  ensure_dir(USER$out_dir)

  # Lag et tidsstempel slik at runbook-filnavnet blir unikt.
  # Format: YYYYMMDD_HHMMSS (lett å sortere).
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")

  # Bygg full filsti til runbook-filen i out_dir.
  path <- file.path(USER$out_dir, paste0("Runbook_PakkeforlopKreft_", ts, ".txt"))

  # Vi lager et "environment" (rb) som en intern "boks" som holder runbook-data.
  # Hvorfor environment:
  # - kan oppdateres inni funksjoner uten å sende rundt hele objektet hele tiden
  # - fungerer som et globalt "state" for runbook
  rb <- new.env(parent = emptyenv())

  # Lagre filsti og tomme linjer/feil-lister.
  rb$path <- path
  rb$lines <- character()
  rb$errors <- character()

  # options() brukes her for å lagre runbook-objektet globalt under et navn.
  # Hvorfor: Da kan rb_add/rb_write hente runbook uten at vi må sende rb rundt.
  options(npr_runbook = rb)

  # Legg inn overskrift og metadata i runbook.
  rb_add("RUNBOOK - PakkeforlĂ¸p kreft til RHF")
  rb_add(paste0("Created: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  rb_add("")

  # Skriv inn USER-innstillinger for sporbarhet.
  rb_add("USER SETTINGS")
  rb_add(paste0(" step: ", USER$step))
  rb_add(paste0(" months: ", paste(USER$months, collapse = ", ")))
  rb_add(paste0(" base_dir: ", USER$base_dir))
  rb_add(paste0(" app_dir: ", USER$app_dir))
  rb_add(paste0(" out_dir: ", USER$out_dir))
  rb_add(paste0(" fill_missing_kommune: ", USER$fill_missing_kommune))
  rb_add(paste0(" drop_column_M: ", USER$drop_column_M))
  rb_add(paste0(" use_parquet_cache: ", USER$use_parquet_cache))
  rb_add(paste0(" qc_simple: ", USER$qc_simple))
  rb_add("")

  # Returner rb usynlig (slik at den ikke printer masse i konsollen).
  invisible(rb)
}

rb_get <- function() getOption("npr_runbook", NULL)
# rb_get:
# - Henter runbook-objektet (environment) fra options.
# - Hvis det ikke finnes, returnerer NULL.
# Hvorfor: rb_add og rb_write trenger en måte å finne runbooken på.

rb_add <- function(...) {
  # rb_add legger til en ny tekstlinje i runbook.

  # Hent runbook-objektet.
  rb <- rb_get()

  # Hvis runbook ikke er initialisert, gjør ingenting.
  if (is.null(rb)) return(invisible(FALSE))

  # ... betyr at funksjonen kan ta inn flere biter tekst.
  # paste0(...) limer dem sammen uten mellomrom.
  txt <- paste0(...)

  # Legg den nye linjen til i runbook-listen.
  rb$lines <- c(rb$lines, txt)

  # Returner TRUE usynlig for å indikere "lagt til".
  invisible(TRUE)
}

rb_section <- function(title) {
  # rb_section lager en tydelig seksjonsoverskrift i runbook.
  # Hvorfor: Runbook blir lettere å lese med blokker/overskrifter.

  rb_add("")
  rb_add(paste0("--- ", title, " ---"))
}

rb_error <- function(msg) {
  # rb_error brukes når vi får en feil i pipeline.
  # Den lagrer feilen i rb$errors og skriver en [ERROR]-linje i runbook.

  rb <- rb_get()

  # Hvis runbook finnes, legg feilen i error-listen.
  if (!is.null(rb)) rb$errors <- c(rb$errors, msg)

  # Skriv feilmelding som en linje i runbook.
  rb_add(paste0("[ERROR] ", msg))
}

rb_write <- function() {
  # rb_write skriver runbooken til disk (til rb$path).

  rb <- rb_get()

  # Hvis runbook ikke finnes, kan vi ikke skrive noe.
  if (is.null(rb)) return(invisible(FALSE))

  # Legg på en oppsummering til slutt.
  rb_add("")
  rb_add("SUMMARY")
  rb_add(paste0(" Total lines: ", length(rb$lines)))
  rb_add(paste0(" Errors: ", length(rb$errors)))

  # Hvis vi har feil, skriv dem også ut i runbooken.
  if (length(rb$errors) > 0) {
    rb_add(" Error messages:")
    rb$lines <- c(rb$lines, paste0(" - ", rb$errors))
  }

  # writeLines skriver alle linjene til tekstfil.
  # useBytes=TRUE gjør at R prøver å skrive bytes direkte (nyttig for spesialtegn).
  writeLines(rb$lines, rb$path, useBytes = TRUE)

  # Returner filstien usynlig.
  invisible(rb$path)
}
``