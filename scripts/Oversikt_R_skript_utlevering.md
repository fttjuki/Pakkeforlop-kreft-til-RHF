## **Oversikt - Hva gjør skripten?**


---

## **Topplinjen - Slik ser den ut:**

### **1. STARTEN (Biblioteker)**
```r
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(DBI)
  library(odbc)
})
```
**Forklaring:** Her laster skripten "hjelpepakker" (som verktøykasser) som trengs for å:
- `dplyr` = arbeide med data tabeller
- `readr` = lese CSV-filer
- `stringr` = håndtere tekst
- `lubridate` = arbeide med datoer
- `DBI` og `odbc` = prate med databasen som inneholder pasientdata

---

### **2. INNSTILLINGER (CFG)**
```r
CFG <- list(
  regions    = c("HSØ", "HV", "HMN"),
  months_nor = c("jan","feb","mar","apr","mai","jun","jul","aug","sep","okt","nov","des")
)
```
**Forklaring:** Dette definerer:
- Hvilke **3 helseregioner** skal få data
- Norske navn på måneder (for når filer skal hete "jan25" osv.)

---

### **3. KOLONNENE SOM SKAL VÆRE MED (FINAL_COLS)**
```r
FINAL_COLS <- c(
  "lopenr","institusjonId","institusjon","reshIdFagEnhet","Fagenhet",
  "KommuneNr","BydelNr","FødselsDato","DødsDato",...
)
```
**Forklaring:** Dette er en liste over alle **feltene/kolonnene** som må være i den endelige filen:
- `lopenr` = løpenummer for pasienten
- `institusjonId` = hvilket sykehus
- `FødselsDato` = når pasienten ble født
- `Pakkeforløp` = hvilken behandlingssekvens pasienten går gjennom osv.

---

## **HOVEDFUNKSJONENE - Hva gjør hvem:**

### **A) LESINGSFUNKSJONER** 📖

#### `detect_delim()` - Finne skilletegnet
```r
detect_delim <- function(path) {
  header <- readLines(path, n = 1)
  semi_fields <- length(strsplit(header, ";")[[1]])
  comma_fields <- length(strsplit(header, ",")[[1]])
  if (semi_fields >= comma_fields) ";" else ","
}
```
**Hva gjør det:** Leser første linje av CSV-filen og sjekker:
- Brukes `;` eller `,` som skilletegn mellom kolonnene?
- Velger det som ser ut til å være riktig

#### `guess_file_encoding()` - Finne språkkodingen
```r
guess_file_encoding <- function(path) {
  enc <- readr::guess_encoding(path, n_max = 10000)
  if (any(tolower(enc$encoding) %in% c("utf-8", "utf8"))) return("UTF-8")
  enc$encoding[1]
}
```
**Hva gjør det:** Sjekker om filen er lagret som:
- UTF-8 (moderne, riktig for norske tegn æøå)
- Windows-1252 (gammel, kan gi problemer)
- Velger UTF-8 om mulig

#### `fix_mojibake_utf8()` - Fikse ødelagte bokstaver
```r
fix_mojibake_utf8 <- function(x) {
  x <- as.character(x)
  idx <- !is.na(x) & str_detect(x, "[ÃÂ]")
  if (any(idx)) {
    y <- iconv(x[idx], from = "Windows-1252", to = "UTF-8")
    x[idx][ok] <- y[ok]
  }
  x
}
```
**Hva gjør det:** Hvis norske bokstaver ble ødelagt og viser seg som `Ã` eller `Â` istedenfor `æ`, så reparerer denne funksjonen det! 


---

### **B) FILLESING MED NORMALISERING** 📥

#### `read_powerbi_csv()` - Les en CSV-fil riktig
```r
read_powerbi_csv <- function(path, USER = list()) {
  delim <- detect_delim(path)
  enc <- guess_file_encoding(path)
  
  df <- read_delim(
    file = path,
    delim = delim,
    locale = locale(encoding = enc),
    trim_ws = FALSE  # VIKTIG: ikke fjern mellomrom fra verdier
  )
  
  # Normaliser kolonnenavn
  names(df) <- names(df) |>
    str_replace_all("\u00A0", " ") |>
    str_replace_all("^\\ufeff", "") |>
    str_trim()
  
  # Fiks ødelagte bokstaver
  df <- df |> mutate(across(everything(), fix_mojibake_utf8))
  
  df <- standardize_powerbi_names(df)
  df
}
```
**Hva gjør det:** 
1. Oppdag skilletegn og språkkoding
2. Les CSV-filen
3. Vasker opp kolonnenavnene (fjern usynlige tegn)
4. Fikser ødelagte norske bokstaver
5. Standardiserer alle kolonnenavnene (f.eks. `Institusjon_navn` → `institusjon`)

---

### **C) NPRId → løpenummer** 🔄

#### `build_nprid_request()` - Lag bestillingsliste
```r
build_nprid_request <- function(all_rows) {
  all_rows |> distinct(NPRId) |>
    filter(!is.na(NPRId), NPRId != "", NPRId != "NPRId", NPRId != "NULL")
}
```
**Hva gjør det:** 
- Tar alle unike pasientnummer (NPRId) fra filene
- Fjerner tomme/ugyldige verdier
- Lager en liste som skal sendes til NPR-databasen: "gi meg løpenummeret for disse pasientene"

#### `write_nprid_request_file()` - Lagre bestillingsfil
```r
write_nprid_request_file <- function(USER, nprid_request, suffix) {
  out_path <- request_file_path(USER, suffix)
  write_delim(nprid_request, out_path, delim = ";")
}
```
**Hva gjør det:** Skriver bestillingslisten til en fil (f.eks. `NPRId_RHF_Pakkeforløp_des25.csv`)

#### `read_lopenr_file()` - Les løpenummersvar
```r
read_lopenr_file <- function(USER, suffix) {
  path <- lnr_file_path(USER, suffix)
  read_delim(path, delim = ";", col_names = c("NPRId", "lopenr"))
}
```
**Hva gjør det:** 
- Leser løpenr-filen 
- Den inneholder koblingen: `NPRId` → `løpenr`

#### `validate_lopenr_map()` - Sjekk om alle pasientene fikk løpenummer
```r
validate_lopenr_map <- function(USER, all_rows, id_map, suffix) {
  missing_map <- needed |>
    left_join(id_map, by = "NPRId") |>
    filter(is.na(lopenr) | lopenr == "" | lopenr == "NULL")
  
  if (nrow(missing_map) > 0) {
    log_warn(USER, "%s NPRId mangler løpenr (droppes som i SQL)")
  }
}
```
**Hva gjør det:** 
- Sjekker: "Fikk alle pasientene sitt løpenummer?"
- Hvis noen mangler → lagrer en QC-rapport som viser hvilke som mangler
- Disse rader droppes fra videre behandling (som SQL gjør)

---

### **D) GEO-FIKSER (Kommune/Bydel)** 🗺️

#### `connect_somhoved()` - Koble til NPR-databasen
```r
connect_somhoved <- function() {
  con <- DBI::dbConnect(
    odbc::odbc(),
    Driver = "SQL Server",
    Server = "NPRSQLprod",
    Database = "NPRNasjonaltDatagrunnlag"
  )
}
```
**Hva gjør det:** Åpner en forbindelse til NPR-databasen på serveren `NPRSQLprod`

#### `fetch_geo_fixes()` - Hent kommune/bydel fra databasen
```r
fetch_geo_fixes <- function(con, missing_ids, db_npr_col) {
  # Hent fra SOMHoved-tabellen: kommune og bydel for pasienter som mangler det
  DBI::dbGetQuery(con, sql_txt)
}
```
**Hva gjør det:** 
- For pasienter som **mangler kommune/bydel** i input-filen
- Spør NPR-databasen: "Hva var hjemmekommunen for denne pasienten?"
- Henter den nyeste informasjonen fra databasen

#### `apply_geo_overrides()` - Bruk spesialregler
```r
apply_geo_overrides <- function(geo_fixes) {
  geo_fixes |>
    left_join(geo_overrides, by = c("NPRId", "aar")) |>
    mutate(komnrhjem2 = coalesce(komnrhjem2_override, komnrhjem2))
}
```
**Hva gjør det:** 
- Det finnes noen **unntak/spesialregler** definert i `geo_overrides`
- Hvis det finnes en spesialregel for en pasient → bruk den istedenfor databasen
- Eksempel: pasienten bodde egentlig i kommune X men ble registrert feil

---

### **E) LEVERINGS-FUNKSJONER** 📤

#### `ensure_final_columns_exist()` - Sikr at alle kolonner finnes
```r
ensure_final_columns_exist <- function(df, missing_token = "NULL") {
  missing <- setdiff(FINAL_COLS, names(df))
  if (length(missing) > 0) for (m in missing) df[[m]] <- missing_token
}
```
**Hva gjør det:** 
- Sjekker: finnes alle de 30+ nødvendige kolonnene?
- Hvis en kolonne mangler → legg den til med verdien `"NULL"`

#### `force_output_shape()` - Gjør alt til tekst
```r
force_output_shape <- function(df, missing_token = "NULL") {
  df |> mutate(across(everything(), ~ {
    x <- as.character(.x)
    x[is.na(x) | x == ""] <- missing_token
    x
  }))
}
```
**Hva gjør det:** 
- Konverterer alle kolonner til tekst (alle tall blir bokstaver)
- Tomme celler → `"NULL"`
- Gjør output ensartet

#### `write_delim_utf8_bom()` - Skriv fil som Excel liker
```r
write_delim_utf8_bom <- function(df, path, delim = ";") {
  con <- file(path, open = "wb")
  writeChar("\ufeff", con, eos = NULL, useBytes = TRUE)  # BOM for UTF-8
  write_delim(df, con2, delim = delim)
}
```
**Hva gjør det:** 
- Skriver CSV-filen som UTF-8 **med BOM** (Byte Order Mark)
- BOM = `"\ufeff"` = et lite "tegn" som forteller Excel: "Dette er UTF-8!"
- Dermed vises norske tegn ( æøå) riktig i Excel

#### `write_delivery()` - Lagre leveringsfilen
```r
write_delivery <- function(USER, df, region, suffix) {
  ensure_dir(USER$out_dir)
  out_csv <- delivery_file_path_csv(USER, region, suffix)
  write_delim_utf8_bom(df, out_csv, delim = ";")
}
```
**Hva gjør det:** 
- Lagrer den ferdige CSV-filen
- Filnavnet blir f.eks. `Utlevering_HSØ_des25.csv`

---

### **F) HOVEDDELEN - Orchestrering** 🎼

#### `make_delivery()` - Lag leveringsfil for én region
```r
make_delivery <- function(USER, report_df, id_map, geo_fixes, region, suffix) {
  df <- report_df |>
    filter(!is.na(NPRId), NPRId != "", NPRId != "NPRId", NPRId != "NULL") |>
    left_join(id_map, by = "NPRId")  # Legg til løpenummeret
  
  df <- fix_lopenr_name(df)
  df <- df |> filter(!is.na(lopenr), lopenr != "", lopenr != "NULL")
  
  # Fyll År fra StartDato hvis det mangler
  if (all(is.na(df$År))) {
    sd <- parse_any_date(df$StartDato)
    df$År <- as.character(lubridate::year(sd))
  }
  
  # Fyll kommune/bydel fra databasen hvis det mangler
  if (isTRUE(USER$fill_missing_kommune)) {
    df <- df |>
      left_join(geo_fixes, by = "NPRId") |>
      mutate(
        KommuneNr = if_else(is.na(KommuneNr), komnrhjem2, KommuneNr),
        BydelNr = if_else(..., bydel2, BydelNr)
      )
  }
  
  # Sikr riktige kolonner + gjør alt til tekst
  df <- ensure_final_columns_exist(df)
  df_out <- df |>
    select(all_of(FINAL_COLS)) |>
    force_output_shape()
  
  df_out
}
```
**Hva gjør det - Trinn for trinn:**
1. **Filtrer ut tomme NPRId** → bare gyldige pasientnummere
2. **Legg til løpenummeret** fra `id_map`
3. **Fjern rader uten løpenummer** (som SQL gjør)
4. **Fyll År** hvis det mangler (hentes fra `StartDato`)
5. **Fyll kommune/bydel** fra databasen hvis det mangler
6. **Sikr at alle kolonner finnes** (legger til `"NULL"` hvor det mangler)
7. **Konverterer alt til tekst** og returnerer ferdig leveringsfil

#### `process_suffix()` - Behandl en måned
```r
process_suffix <- function(USER, suffix) {
  # 1) Les alle CSV-filer for denne måned fra alle 3 regioner
  region_objs <- set_names(CFG$regions) |>
    map(~ load_region_dataset(USER, .x, suffix))
  
  all_rows <- bind_rows(map(region_objs, "data"))
  
  # 2) Lag bestillingsliste over unike NPRId
  req_tbl <- build_nprid_request(all_rows)
  req_path <- write_nprid_request_file(USER, req_tbl, suffix)
  
  # 3) HVIS STEP1: stopp her (bare lagr bestillingsfil)
  if (toupper(USER$step) == "STEP1") {
    return(invisible(list(status = "step1_done", req_path = req_path)))
  }
  
  # 4) Les løpenummersvar fra databasen
  id_map <- read_lopenr_file(USER, suffix)
  validate_lopenr_map(USER, all_rows, id_map, suffix)
  
  # 5) Hent geo-fikser hvis nødvendig
  geo_fixes <- tibble(...)
  if (isTRUE(USER$fill_missing_kommune)) {
    con <- connect_somhoved()
    geo_fixes <- fetch_geo_fixes(con, missing_ids) |>
      apply_geo_overrides()
  }
  
  # 6) Lag leveringsfiler for hver region
  for (region in names(reports)) {
    df_final <- make_delivery(USER, reports[[region]], id_map, geo_fixes, region, suffix)
    write_delivery(USER, df_final, region, suffix)
  }
}
```
**Hva gjør det - Tverrregionalt arbeid:**

**STEP 1** (bestilling):
- Les alle input-filer fra alle 3 regioner
- Trekk ut unike pasientnummere (NPRId)
- Skriv til bestillingsfil som sendes til NPR

**STEP 2** (levering):
- Les løpenummersvar fra NPR
- Hent geo-fikser fra databasen
- Lag ferdig leveringsfiler for hver region separat

#### `main()` - Hovedfunksjonen
```r
main <- function(USER) {
  USER <- sanitize_user_paths(USER)
  ensure_dir(USER$out_dir)
  
  if (!toupper(USER$step) %in% c("STEP1","STEP2")) 
    stop_user("USER$step må være STEP1 eller STEP2")
  if (length(USER$months) == 0) 
    stop_user("USER$months er tom")
  
  month_dates <- as.Date(USER$months)
  suffixes <- map_chr(month_dates, month_to_suffix)
  
  for (sfx in suffixes) process_suffix(USER, sfx)
}
```
**Hva gjør det:**
1. Vasker opp filstiene (convert `\` til `/`, fix nettverkspath)
2. Sjekker at `USER$step` er riktig (`STEP1` eller `STEP2`)
3. Sjekker at `USER$months` er fylt inn
4. Konverterer måner til "suffix" format (f.eks. `2025-12-01` → `des25`)
5. Kjører `process_suffix()` for hver måned

---

## **EKSEMPEL - Slik bruker du det** 💻

```r
USER <- list(
  step = "STEP2",                          # STEP1=bestilling, STEP2=levering
  months = c("2025-12-01"),                # Hvilke måneder skal behandles
  base_dir = "N:/RHF Pakkeforløp kreft/R", # Hvor input-filene ligger
  app_dir = "//fihr.no/Temp/NPR",          # Midlertidig arbeidsmappe
  out_dir = "N:/RHF/output",               # Hvor output skal lagres
  fill_missing_kommune = TRUE,              # Fyll kommune fra databasen
  strict_lopenr = FALSE,                   # Ikke stopp hvis noen mangler løpenr
  verbose = TRUE,                          # Skriv detaljert logge
  repair_mojibake = TRUE,                  # Fiks Ã/Â-problemer
  fail_on_mojibake_out = TRUE              # Stopp hvis output har ødelagte tegn
)

source("Ny_versjon.R")
main(USER)
```

**Hva skjer:**
1. ✅ Leser `Utleveringer/Pakkeforløp_HSØ_des25.csv`, `Pakkeforløp_HV_des25.csv`, `Pakkeforløp_HMN_des25.csv`
2. ✅ Lager bestillingsfil → ligger i `//fihr.no/Temp/NPR/`
3. ✅ Du sender bestillingsfil til NPR, får svar med løpenummeret
4. ✅ Skript leser svar, henter geo-data fra databasen
5. ✅ Skriver 3 leveringsfiler: `Utlevering_HSØ_des25.csv`, `Utlevering_HV_des25.csv`, `Utlevering_HMN_des25.csv`

---

## **OPPSUMMERING** 📋

| Del | Hva den gjør |
|-----|------------|
| **Lesing** | Leser CSV-filer, fikser encoding-problemer, normaliser kolonnenavnene |
| **Bestilling** | Trekk ut unike pasientnummere, lag bestillingsfil til NPR |
| **Levering** | Tar løpenummer fra NPR, fyller kommune/bydel fra databasen, lagrer ferdig filer |
| **Kvalitet** | Sjekker mojibake (ødelagte tegn), validerer at alle får løpenummer |

### Hvirdan å kjøre skript?
### **1. Åpne R‑prosjektet – dobbeltklikk på .Rproj-filen.
### ** 2. Åpne skriptet – klikk på Ny_versjon.R i filpanelet (eller dra den inn).
### ** 3. Endre months – skriv inn riktig måned (f.eks. "2026-02-01").
### ** 4. Endre step – velg STEP1 eller STEP2.
### ** 5. Lagre – Ctrl+S.
 ### ** 6. Kjør skriptet – klikk på Source-knappen øverst i høyre hjørne.
 ### ** 7. Ferdig! 🚀

