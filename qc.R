
# ==============================================================================
# COMPARE SQL vs R (HSØ / HV / HMN) 
#
# PASS/FAIL rule:
#   - PASS/FAIL depends ONLY on whether number of columns match:
#       ncol(SQL) == ncol(R)
#
# Informational-only QC added:
#   - Check missing KommuneNr counts match between SQL and R (NA/""/"NULL")
#     -> shown in summary but does NOT affect PASS/FAIL
#
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(stringr)
  library(lubridate)
})

# -----------------------
# SETTINGS 
# -----------------------
suffix <- "des25"
year_sql <- "2025"
month_folder <- "12 Desember"

r_dir <- "N:/Utleveringer/STYRINGSDATA/RHF Pakkeforløp Kreft/R"
sql_base <- "N:/Utleveringer/STYRINGSDATA/RHF Pakkeforløp Kreft/Utleveringer"

sql_filename <- function(rhf) {
  paste0("Utlevering_", rhf, "_PakkeforløpKreft_IndividData_Desember25.csv")
}

NORMALIZE_DATES <- TRUE
N_EXAMPLES <- 200

out_dir <- file.path(r_dir, paste0("QC_compare_SQL_vs_R_", suffix))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------
# Helpers: printing
# -----------------------
safe_obj_name <- function(title) make.names(gsub("[^A-Za-z0-9_]", "_", title))

show_table <- function(df, title) {
  message("\n--- ", title, " ---")
  print(df)
  if (interactive()) {
    nm <- safe_obj_name(title)
    assign(nm, df, envir = .GlobalEnv)
    try(View(get(nm, envir = .GlobalEnv)), silent = TRUE)
  }
}

show_table_if_any <- function(df, title) {
  if (nrow(df) > 0) show_table(df, title)
  else message("\n--- ", title, " ---\nNo differences found.")
}

# -----------------------
# Robust delimiter detect
# -----------------------
detect_delim <- function(path) {
  header <- readLines(path, n = 1, warn = FALSE)
  header <- sub("^\ufeff", "", header)  # strip UTF-8 BOM
  n_semi  <- str_count(header, fixed(";"))
  n_comma <- str_count(header, fixed(","))
  if (n_semi >= n_comma) ";" else ","
}

# -----------------------
# Fix: sanitize column names (prevents zero-length variable name error)
# -----------------------
sanitize_colnames <- function(nm) {
  nm <- as.character(nm)
  nm <- sub("^\ufeff", "", nm)         # remove BOM
  nm <- str_replace_all(nm, "\r", "")
  nm <- str_trim(nm)
  
  # Replace empty names "" with blank_1, blank_2...
  if (any(nm == "")) {
    idx <- which(nm == "")
    nm[idx] <- paste0("blank_", seq_along(idx))
  }
  
  make.unique(nm)
}

# -----------------------
# NORMALIZATION (values)
# -----------------------
normalize_text_df <- function(df) {
  df <- df %>% mutate(across(everything(), as.character))
  
  df %>%
    mutate(
      across(
        everything(),
        ~ .x %>%
          str_replace_all("\r", "") %>%
          na_if("NULL") %>%
          na_if("NaN") %>%
          na_if("NA") %>%
          str_trim(side = "both") %>%
          str_replace_all("[ \t]+", " ")
      )
    ) %>%
    mutate(across(everything(), ~ replace_na(.x, "")))  # treat NA == ""
}

read_any_csv <- function(path) {
  delim <- detect_delim(path)
  
  try_read <- function(enc) {
    readr::read_delim(
      path,
      delim = delim,
      col_types = cols(.default = col_character()),
      locale = locale(encoding = enc),
      show_col_types = FALSE,
      progress = FALSE,
      trim_ws = FALSE
    )
  }
  
  df <- tryCatch(try_read("UTF-8"), error = function(e) try_read("Windows-1252"))
  
  # sanitize names BEFORE using them
  names(df) <- sanitize_colnames(names(df))
  
  normalize_text_df(df)
}

# -----------------------
# Date normalization (optional)
# -----------------------
normalize_dates <- function(df) {
  date_cols <- intersect(
    names(df),
    c("FødselsDato","DødsDato","StartDato","UtrStartDato","KliniskBeslutn",
      "StartBeh","Avslutn","AnsiennDato","MottaksDato")
  )
  if (length(date_cols) == 0) return(df)
  
  parse_one <- function(x) {
    d <- suppressWarnings(parse_date_time(
      x,
      orders = c(
        "Ymd","Y-m-d","d.m.Y","dmY",
        "Ymd HMS","Y-m-d HMS","d.m.Y HMS","dmY HMS"
      )
    ))
    ifelse(is.na(d), x, format(as.Date(d), "%Y-%m-%d"))
  }
  
  df %>% mutate(across(all_of(date_cols), parse_one))
}

# -----------------------
# Informational QC: missing KommuneNr count
# -----------------------
missing_kommune_count <- function(df, col = "KommuneNr") {
  if (!col %in% names(df)) return(NA_integer_)
  x <- as.character(df[[col]])
  x <- str_trim(x)
  sum(is.na(x) | x == "" | toupper(x) == "NULL")
}

# -----------------------
# Signature comparison (diagnostic only; NOT used for PASS/FAIL)
# -----------------------
make_row_signature <- function(df) {
  sep <- "\u001F"
  if (ncol(df) == 0) return(paste0("ROW_", seq_len(nrow(df))))
  do.call(paste, c(df[names(df)], sep = sep))
}

compare_by_signatures <- function(df_sql, df_r) {
  sig_sql <- tibble(sig = make_row_signature(df_sql)) %>% count(sig, name = "n_sql")
  sig_r   <- tibble(sig = make_row_signature(df_r))   %>% count(sig, name = "n_r")
  
  full_join(sig_sql, sig_r, by = "sig") %>%
    mutate(
      n_sql = replace_na(n_sql, 0L),
      n_r   = replace_na(n_r, 0L),
      diff  = n_sql - n_r
    )
}

extract_examples <- function(df, sig_diff, side = c("sql","r"), max_rows = 200) {
  side <- match.arg(side)
  if (nrow(sig_diff) == 0) return(tibble())
  
  df2 <- df %>% mutate(sig = make_row_signature(df))
  df2 %>%
    semi_join(sig_diff %>% select(sig), by = "sig") %>%
    slice_head(n = max_rows) %>%
    mutate(source = side)
}

# -----------------------
# lopenr comparison 
# -----------------------
compare_keys_summary <- function(df_sql, df_r, key = "lopenr") {
  if (!(key %in% names(df_sql)) || !(key %in% names(df_r))) {
    return(tibble(note = paste0("Column '", key, "' is not present in both files.")))
  }
  
  a <- df_sql %>% count(.data[[key]], name = "n_sql")
  b <- df_r   %>% count(.data[[key]], name = "n_r")
  
  full_join(a, b, by = key) %>%
    mutate(
      n_sql = replace_na(n_sql, 0L),
      n_r   = replace_na(n_r, 0L),
      diff  = n_sql - n_r
    )
}

# -----------------------
# Build paths
# -----------------------
paths <- tibble(RHF = c("HSØ", "HV", "HMN")) %>%
  mutate(
    r_file   = file.path(r_dir, paste0("Utlevering_", RHF, "_", suffix, ".csv")),
    sql_file = file.path(sql_base, year_sql, RHF, month_folder, sql_filename(RHF))
  )

missing <- paths %>%
  pivot_longer(cols = c(sql_file, r_file), names_to = "type", values_to = "path") %>%
  mutate(exists = file.exists(path)) %>%
  filter(!exists)

if (nrow(missing) > 0) {
  print(missing %>% select(RHF, type, path), n = Inf)
  stop("Some files are missing. Fix paths/filenames in SETTINGS and rerun.", call. = FALSE)
}

# -----------------------
# Run comparisons
# -----------------------
all_summary_display <- list()
details_text <- c()

for (i in seq_len(nrow(paths))) {
  rhf <- paths$RHF[i]
  
  message("\n============================================================")
  message("Sammenligner RHF: ", rhf)
  message("============================================================")
  message("SQL: ", paths$sql_file[i])
  message("R:   ", paths$r_file[i])
  
  df_sql <- read_any_csv(paths$sql_file[i])
  df_r   <- read_any_csv(paths$r_file[i])
  
  if (NORMALIZE_DATES) {
    df_sql <- normalize_dates(df_sql)
    df_r   <- normalize_dates(df_r)
  }
  
  # PASS/FAIL criterion (ONLY): number of columns
  sql_cols_n <- ncol(df_sql)
  r_cols_n   <- ncol(df_r)
  colcount_match <- (sql_cols_n == r_cols_n)
  status_one <- ifelse(colcount_match, "PASS", "FAIL")
  
  # Informational-only: missing KommuneNr match
  miss_komm_sql <- missing_kommune_count(df_sql, "KommuneNr")
  miss_komm_r   <- missing_kommune_count(df_r,   "KommuneNr")
  miss_komm_match <- identical(miss_komm_sql, miss_komm_r)
  
  # Optional diagnostics still computed
  common_cols <- intersect(names(df_sql), names(df_r))
  common_cols <- common_cols[common_cols != ""]
  df_sql_c <- df_sql %>% select(all_of(common_cols))
  df_r_c   <- df_r   %>% select(all_of(common_cols))
  
  sig_all <- compare_by_signatures(df_sql_c, df_r_c)
  sig_only_sql <- sig_all %>% filter(diff > 0)
  sig_only_r   <- sig_all %>% filter(diff < 0)
  
  key_all <- compare_keys_summary(df_sql_c, df_r_c, key = "lopenr")
  key_only_sql  <- if ("lopenr" %in% names(key_all)) key_all %>% filter(n_sql > 0 & n_r == 0) else tibble()
  key_only_r    <- if ("lopenr" %in% names(key_all)) key_all %>% filter(n_r > 0 & n_sql == 0) else tibble()
  key_countdiff <- if ("lopenr" %in% names(key_all)) key_all %>% filter(n_sql > 0 & n_r > 0 & diff != 0) else tibble()
  
  # Summary shown (no hidden columns)
  sum_display <- tibble(
    RHF = rhf,
    sql_rows = nrow(df_sql),
    r_rows = nrow(df_r),
    sql_cols = sql_cols_n,
    r_cols = r_cols_n,
    colcount_match = colcount_match,              # <-- ONLY PASS/FAIL criterion
    common_cols = length(common_cols),            # informational
    missing_kommune_sql = miss_komm_sql,          # informational
    missing_kommune_r = miss_komm_r,              # informational
    missing_kommune_match = miss_komm_match,      # informational
    lopenr_only_in_sql = nrow(key_only_sql),      # informational
    lopenr_only_in_r = nrow(key_only_r),          # informational
    lopenr_count_diff = nrow(key_countdiff),      # informational
    status = status_one
  )
  
  all_summary_display[[rhf]] <- sum_display
  show_table(sum_display, paste0("SAMMENLIGNING_OPPSUMMERING_", rhf, "_", suffix))
  
  # Informational details tables (optional)
  show_table(key_all %>% slice_head(n = 200), paste0("LØPENR_SJEKK_", rhf, "_", suffix, "_topp200"))
  
  if (nrow(sig_only_sql) > 0 || nrow(sig_only_r) > 0) {
    sig_top <- bind_rows(
      sig_only_sql %>% mutate(hvor = "SQL_har_flere"),
      sig_only_r   %>% mutate(hvor = "R_har_flere")
    ) %>%
      mutate(abs_diff = abs(diff)) %>%
      arrange(desc(abs_diff)) %>%
      select(hvor, n_sql, n_r, diff, sig) %>%
      slice_head(n = 100)
    
    show_table(sig_top, paste0("RADAVVIK_SIGNATURER_", rhf, "_", suffix, "_topp100"))
    
    ex_sql <- extract_examples(df_sql_c, sig_only_sql, side = "sql", max_rows = N_EXAMPLES)
    ex_r   <- extract_examples(df_r_c,   sig_only_r,   side = "r",   max_rows = N_EXAMPLES)
    
    show_table_if_any(ex_sql, paste0("EKSEMPLER_KUN_I_SQL_", rhf, "_", suffix))
    show_table_if_any(ex_r,   paste0("EKSEMPLER_KUN_I_R_",   rhf, "_", suffix))
  }
  
  details_text <- c(
    details_text,
    paste0(
      rhf,
      ": sql_cols=", sql_cols_n,
      ", r_cols=", r_cols_n,
      ", colcount_match=", colcount_match,
      ", missingKommune(SQL/R)=", miss_komm_sql, "/", miss_komm_r,
      ", status=", status_one
    )
  )
}

summary_all <- bind_rows(all_summary_display)


# PASS/FAIL 
PASS <- all(summary_all$colcount_match)

show_table(summary_all, paste0("SAMLET_OPPSUMMERING_ALLE_", suffix))

status_txt <- if (PASS) "PASS" else "FAIL"

final_text <- c(
  "=== SAMMENLIGNING SQL vs R (tolerant) ===",
  paste0("Suffix: ", suffix),
  paste0("Run time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("Result: ", status_txt),
  "",
  "PASS/FAIL criteria (ONLY):",
  "- For all RHF: number of columns in SQL file must equal number of columns in R file (sql_cols == r_cols).",
  "",
  "Informational-only QC:",
  "- missing_kommune_match checks whether missing KommuneNr counts are equal between SQL and R.",
  "- This does NOT affect PASS/FAIL.",
  "",
  "Per RHF:",
  details_text,
  "",
  "Notes:",
  "- Normalizes NA/\"\"/\"NULL\"; removes carriage returns (\\r); trims whitespace.",
  "- Other diagnostics (lopenr diffs, row-signature diffs) are informational only."
)

message("\n============================================================")
message("SLUTTRESULTAT: ", status_txt)
message("============================================================")
cat(paste(final_text, collapse = "\n"), "\n")

txt_path <- file.path(out_dir, paste0("OPPSUMMERING_SQL_vs_R_", suffix, "_tolerant.txt"))
writeLines(final_text, txt_path)
message("\nSummary saved to:\n", txt_path)
