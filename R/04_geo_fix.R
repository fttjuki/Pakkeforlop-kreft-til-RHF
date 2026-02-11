# ==============================================================================
# 04_geo_fix.R
# Kommune-fiks via SOMHoved
# ============================================================================== 

connect_somhoved <- function() {
  con <- DBI::dbConnect(
    odbc::odbc(),
    Driver = "SQL Server",
    Server = "NPRSQLprod",
    Database = "NPRNasjonaltDatagrunnlag",
    Trusted_connection = "True"
  )
  DBI::dbExecute(con, "SET NOCOUNT ON;")
  con
}

detect_db_npr_col <- function(con) {
  cols <- names(DBI::dbGetQuery(con, "SELECT TOP 0 * FROM dbo.SOMHoved;"))
  if ("NPRId" %in% cols) return("NPRId")
  if ("NPRid" %in% cols) return("NPRid")
  stop_user("Fant verken NPRId eller NPRid i dbo.SOMHoved.")
}

fetch_geo_fixes <- function(con, missing_ids, db_npr_col) {
  if (length(missing_ids) == 0) {
    return(tibble::tibble(NPRId=character(), komnrhjem2=character(), bydel2=character(), aar=integer()))
  }

  temp_name <- paste0("#TempMissing_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1000:9999, 1))
  temp_tbl <- tibble::tibble(!!db_npr_col := as.character(missing_ids))
  dplyr::copy_to(con, temp_tbl, name = temp_name, temporary = TRUE, overwrite = TRUE)

  sql_txt <- paste0(
    "WITH Kom AS (",
    " SELECT s.", db_npr_col, " AS NPRId, s.komnrhjem2, s.bydel2, s.aar,",
    " ROW_NUMBER() OVER (PARTITION BY s.", db_npr_col, " ORDER BY s.aar DESC) AS rn",
    " FROM dbo.SOMHoved s INNER JOIN ", temp_name, " t ON s.", db_npr_col, " = t.", db_npr_col,
    ") SELECT NPRId, komnrhjem2, bydel2, aar FROM Kom WHERE rn = 1;"
  )

  as_tibble(DBI::dbGetQuery(con, sql_txt)) %>%
    dplyr::mutate(
      NPRId=as.character(NPRId),
      komnrhjem2=as.character(komnrhjem2),
      bydel2=as.character(bydel2),
      aar=as.integer(aar)
    )
}

apply_geo_overrides <- function(geo_fixes) {
  geo_fixes %>%
    dplyr::left_join(geo_overrides, by=c("NPRId","aar"), suffix=c("","_override")) %>%
    dplyr::mutate(komnrhjem2 = dplyr::coalesce(komnrhjem2_override, komnrhjem2)) %>%
    dplyr::select(-komnrhjem2_override)
}
