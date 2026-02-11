# ==============================================================================
# 07_runbook.R
# Runbook = automatisk loggfil (tekst) som skrives til out_dir
# ============================================================================== 

rb_init <- function(USER) {
  ensure_dir(USER$out_dir)
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  path <- file.path(USER$out_dir, paste0("Runbook_PakkeforlopKreft_", ts, ".txt"))

  rb <- new.env(parent = emptyenv())
  rb$path <- path
  rb$lines <- character()
  rb$errors <- character()

  options(npr_runbook = rb)

  rb_add("RUNBOOK - Pakkeforløp kreft til RHF")
  rb_add(paste0("Created: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  rb_add("")
  rb_add("USER SETTINGS")
  rb_add(paste0("  step: ", USER$step))
  rb_add(paste0("  months: ", paste(USER$months, collapse = ", ")))
  rb_add(paste0("  base_dir: ", USER$base_dir))
  rb_add(paste0("  app_dir: ", USER$app_dir))
  rb_add(paste0("  out_dir: ", USER$out_dir))
  rb_add(paste0("  fill_missing_kommune: ", USER$fill_missing_kommune))
  rb_add(paste0("  drop_column_M: ", USER$drop_column_M))
  rb_add(paste0("  use_parquet_cache: ", USER$use_parquet_cache))
  rb_add(paste0("  qc_simple: ", USER$qc_simple))
  rb_add("")
  invisible(rb)
}

rb_get <- function() getOption("npr_runbook", NULL)

rb_add <- function(...) {
  rb <- rb_get()
  if (is.null(rb)) return(invisible(FALSE))
  txt <- paste0(...)
  rb$lines <- c(rb$lines, txt)
  invisible(TRUE)
}

rb_section <- function(title) {
  rb_add("")
  rb_add(paste0("--- ", title, " ---"))
}

rb_error <- function(msg) {
  rb <- rb_get()
  if (!is.null(rb)) rb$errors <- c(rb$errors, msg)
  rb_add(paste0("[ERROR] ", msg))
}

rb_write <- function() {
  rb <- rb_get()
  if (is.null(rb)) return(invisible(FALSE))

  rb_add("")
  rb_add("SUMMARY")
  rb_add(paste0("  Total lines: ", length(rb$lines)))
  rb_add(paste0("  Errors: ", length(rb$errors)))
  if (length(rb$errors) > 0) {
    rb_add("  Error messages:")
    rb$lines <- c(rb$lines, paste0("   - ", rb$errors))
  }

  writeLines(rb$lines, rb$path, useBytes = TRUE)
  invisible(rb$path)
}
