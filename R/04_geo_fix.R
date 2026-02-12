# ==============================================================================
# 04_geo_fix.R
# Kommune-fiks via SOMHoved
# ------------------------------------------------------------------------------
# Hensikt (i enkel norsk):
# - Noen rader mangler KommuneNr/BydelNr i PowerBI-data.
# - Da kan vi slå opp i databasen (dbo.SOMHoved) for samme NPRId og finne
#   siste kjente kommune/bydel (basert på høyeste år).
# - Vi henter kun 1 rad per NPRId (den nyeste).
# - Til slutt kan vi overstyre enkelte spesielle tilfeller (geo_overrides).
# ==============================================================================

connect_somhoved <- function() {
  # Denne funksjonen lager en database-tilkobling (connection) til SQL Server.
  # Hvorfor: Vi må kunne hente kommune/bydel-informasjon direkte fra databasen.
  con <- DBI::dbConnect(
    # Bruk ODBC-driver for å snakke med SQL Server.
    odbc::odbc(),
    # Angir at driveren er SQL Server.
    Driver = "SQL Server",
    # Navn på serveren vi kobler oss til (her: produksjonsserver).
    Server = "NPRSQLprod",
    # Navn på databasen vi vil lese fra.
    Database = "NPRNasjonaltDatagrunnlag",
    # "Trusted_connection" betyr at vi bruker Windows/AD-innlogging (ikke passord i kode).
    Trusted_connection = "True"
  )

  # Dette SQL-kommandoet gjør at SQL Server ikke sender ekstra "rowcount"-meldinger.
  # Hvorfor: Det kan gjøre spørringer litt ryddigere og noen ganger raskere.
  DBI::dbExecute(con, "SET NOCOUNT ON;")

  # Returnerer tilkoblingen slik at andre funksjoner kan bruke den.
  con
}

detect_db_npr_col <- function(con) {
  # Denne funksjonen finner ut om kolonnen i dbo.SOMHoved heter "NPRId" eller "NPRid".
  # Hvorfor: Noen databaser/tabeller kan ha ulik store/små bokstaver i kolonnenavnet.

  # Vi leser 0 rader (TOP 0) bare for å få kolonnenavnene, uten å hente data.
  cols <- names(DBI::dbGetQuery(con, "SELECT TOP 0 * FROM dbo.SOMHoved;"))

  # Hvis tabellen har kolonnen "NPRId", bruk den.
  if ("NPRId" %in% cols) return("NPRId")

  # Hvis tabellen har kolonnen "NPRid" (annen casing), bruk den.
  if ("NPRid" %in% cols) return("NPRid")

  # Hvis ingen av dem finnes, stopper vi, fordi vi ikke kan slå opp på NPRId.
  stop_user("Fant verken NPRId eller NPRid i dbo.SOMHoved.")
}

fetch_geo_fixes <- function(con, missing_ids, db_npr_col) {
  # Denne funksjonen henter kommune/bydel for NPRId vi mangler KommuneNr for.
  # Den tar inn:
  # - con: database-tilkobling
  # - missing_ids: en liste med NPRId som mangler kommune
  # - db_npr_col: hvilket kolonnenavn databasen bruker ("NPRId" eller "NPRid")

  # Hvis listen er tom, trenger vi ikke spørre databasen.
  # Vi returnerer en tom tabell med riktige kolonnenavn.
  if (length(missing_ids) == 0) {
    return(tibble::tibble(NPRId=character(), komnrhjem2=character(), bydel2=character(), aar=integer()))
  }

  # Vi lager et unikt navn for en midlertidig tabell (#temp) i SQL Server.
  # Hvorfor: Vi vil sende inn missing_ids til SQL på en trygg måte, uten å bygge
  # en stor "IN (...)"-streng som kan bli tung eller feil.
  temp_name <- paste0("#TempMissing_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1000:9999, 1))

  # Vi bygger en liten tabell i R som bare inneholder NPRId-ene vi mangler.
  # !!db_npr_col betyr at vi bruker variabelen som kolonnenavn i tibble.
  temp_tbl <- tibble::tibble(!!db_npr_col := as.character(missing_ids))

  # Vi kopierer denne tabellen inn i SQL Server som en midlertidig tabell.
  # temporary = TRUE gjør at tabellen blir en temp-tabell som forsvinner etterpå.
  dplyr::copy_to(con, temp_tbl, name = temp_name, temporary = TRUE, overwrite = TRUE)

  # Vi bygger SQL-teksten som skal:
  # - slå opp i dbo.SOMHoved for disse NPRId-ene
  # - velge nyeste år per NPRId (ROW_NUMBER() ... ORDER BY aar DESC)
  # - returnere kun den nyeste raden (WHERE rn = 1)
  sql_txt <- paste0(
    "WITH Kom AS (",
    " SELECT s.", db_npr_col, " AS NPRId, s.komnrhjem2, s.bydel2, s.aar,",
    " ROW_NUMBER() OVER (PARTITION BY s.", db_npr_col, " ORDER BY s.aar DESC) AS rn",
    " FROM dbo.SOMHoved s INNER JOIN ", temp_name, " t ON s.", db_npr_col, " = t.", db_npr_col,
    ") SELECT NPRId, komnrhjem2, bydel2, aar FROM Kom WHERE rn = 1;"
  )

  # Kjør SQL-spørringen og gjør resultatet om til tibble (en pen data.frame).
  # Deretter sørger vi for at datatypene er det vi forventer (tekst/nummer).
  as_tibble(DBI::dbGetQuery(con, sql_txt)) %>%
    dplyr::mutate(
      NPRId=as.character(NPRId),
      komnrhjem2=as.character(komnrhjem2),
      bydel2=as.character(bydel2),
      aar=as.integer(aar)
    )
}

apply_geo_overrides <- function(geo_fixes) {
  # Denne funksjonen bruker en liten "override-tabell" (geo_overrides)
  # for spesielle kjente feil/avvik.
  # Hvorfor: Av og til vet vi at databasen har feil for enkelte NPRId+år,
  # og da vil vi tvinge inn riktig kommune.

  geo_fixes %>%
    # Vi kobler på geo_overrides ved match på NPRId og aar.
    dplyr::left_join(geo_overrides, by=c("NPRId","aar"), suffix=c("","_override")) %>%
    # coalesce betyr: "bruk override hvis den finnes, ellers bruk original".
    dplyr::mutate(komnrhjem2 = dplyr::coalesce(komnrhjem2_override, komnrhjem2)) %>%
    # Vi fjerner hjelpekolonnen etter at vi har brukt den.
    dplyr::select(-komnrhjem2_override)
}