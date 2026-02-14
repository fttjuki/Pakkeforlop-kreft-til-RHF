# SQL‑rutine vs R‑pipeline 
> **Mål:** Vise forskjellen mellom dagens SQL‑rutine og en automatisert R‑pipeline, med eksempler fra begge. 

---

## 1) Kort oppsummering (30 sek)
- **I dag:** SQL‑scriptet krever at man endrer måned/år og filnavn manuelt, og gjør mange like steg for flere regioner. citeturn6search1
- **Med R:** Man endrer bare noen få innstillinger (USER), og trykker «Run». Resten går automatisk: riktig periode, riktig fil, alle regioner, lagring, QC‑sjekk. citeturn5search1turn4search2
- **Resultat:** Samme leveranseinnhold, men mindre manuelt arbeid og lavere feilrisiko. citeturn1search1turn6search1turn4search2

---

## 2) Hvor er «manuell risiko» i SQL‑rutinen? (konkrete eksempler)

### 2.1 Måned/år må endres manuelt (hver gang)
SQL‑scriptet sier eksplisitt at perioden må endres hver måned/år. citeturn6search1

```sql
/*
  Data for desember 2025. --må endres hver måned (og år)
*/
```

**Hvorfor dette er en risiko:**
- Det er lett å glemme å oppdatere noe, eller oppdatere noe *delvis*.

---

### 2.2 Måned ligger også i tabellnavn og filnavn
SQL har månedsspesifikk tabell og forventer månedsspesifikk returfil fra Fihr. citeturn6search1

```sql
DROP TABLE IF EXISTS #NPRId_desember_2025; --måneden må endres hver måned

BULK INSERT #NPRId_desember_2025 FROM '\\fihr.no\\...\\NPRId_RHF_Pakkeforløp_des25_lnr.csv'
```

**Hvorfor dette er en risiko:**
- Tabellnavn + filnavn må «matche». En liten skrivefeil gir feil kjøring eller feil input.

---

### 2.3 Samme innlesing gjentas tre ganger (HSØ/HV/HMN)
SQL leser tre filer med tre nesten like blokker. citeturn6search1

```sql
BULK INSERT #RapportHSØ FROM '...RHF_HSØ.csv' WITH (FIRSTROW = 2);
BULK INSERT #RapportHV  FROM '...RHF_HV.csv'  WITH (FIRSTROW = 2);
BULK INSERT #RapportHMN FROM '...RHF_HMN.csv' WITH (FIRSTROW = 2);
```

**Hvorfor dette er en risiko:**
- Endringer må gjøres flere steder → større sjanse for at regioner blir behandlet ulikt ved en feil.

---

### 2.4 Manuell «feilretting» med hardkodede unntak
SQL har hardkodede oppdateringer for enkelte NPRId/år der kommune mangler. citeturn6search1

```sql
UPDATE #Fiks_HSØ_Kom SET komnrhjem2 = '0906' WHERE NPRId = '3200706' AND aar = 2008;
UPDATE #Fiks_HV_Kom  SET komnrhjem2 = '1135' WHERE NPRId = '3602577' AND aar = 2008;
```

**Hvorfor dette er en risiko:**
- Slike «spesialtilfeller» kan bli glemt eller kopiert feil i fremtiden.

---

## 3) Hva gjør R‑pipeline smartere (og tryggere)?

### 3.1 I R endrer man bare én ting: USER‑innstillinger (ikke masse kode)
Startfila sier tydelig at USER‑listen er det eneste man normalt skal endre. citeturn5search1

```r
USER <- list(
  step   = "STEP2",
  months = c("2025-12-01"),
  base_dir = "N:/.../R",
  app_dir  = "//fihr.no/dfs/NPR/Temp/NPR_RegistrerUtlevering",
  out_dir  = "N:/.../R",
  strict_input = TRUE
)
```

**Hvorfor dette er smart:**
- «Måned» settes som en dato én gang.
- Koden bruker dette konsekvent videre.

---

### 3.2 Smart kalender: lager `des25` automatisk fra dato
I R lages fil‑suffix automatisk fra `months` (YYYY‑MM‑01). citeturn5search1turn4search2

```r
month_to_suffix <- function(month_date) {
  d <- as.Date(month_date)
  paste0(CFG$months_nor[lubridate::month(d)], format(d, "%y"))
}
```

**Hvorfor dette er smart:**
- Man slipper å skrive `des25` manuelt i flere filnavn.
- Mindre risiko for at bestilling/retur/utlevering får ulike navn.

---

### 3.3 Loop: samme behandling for alle regioner automatisk
R har regionliste én gang, og behandler alle likt. citeturn4search2

```r
CFG <- list(regions = c("HSØ", "HV", "HMN"))

region_objs <- set_names(CFG$regions) |> 
  map(~ load_region_dataset(USER, .x, suffix)) |> 
  discard(is.null)
```

**Hvorfor dette er smart:**
- Ingen «tre separate kjøringer».
- Endringer gjøres én gang.

---

### 3.4 R finner riktig inputfil og kan stoppe hvis noe mangler
Dette reduserer risiko for å bruke feil fil eller feil periode. citeturn5search1turn4search2

```r
if (length(hits) == 0 && isTRUE(USER$strict_input)) {
  stop_user("Fant ingen suffix-fil for %s (suffix=%s).", region, suffix)
}
```

---

### 3.5 Norske tegn (æøå): R håndterer encoding og reparerer typiske feil
R har logikk for å oppdage/rette «Ã¦/Ã¸»‑problemer. citeturn4search2

```r
fix_mojibake_utf8 <- function(x) {
  idx <- !is.na(x) & str_detect(x, "[ÃÂ]")
  if (any(idx)) {
    y <- iconv(x[idx], from = "Windows-1252", to = "UTF-8")
    x[idx][!is.na(y)] <- y[!is.na(y)]
  }
  x
}
```

---

### 3.6 QC: R kan automatisk lage avvikslister (f.eks. mangler løpenr)
R kan skrive en QC‑fil med NPRId som mangler løpenr, og eventuelt stoppe. citeturn4search2turn5search1

```r
if (nrow(missing_map) > 0) {
  miss_path <- file.path(USER$out_dir, paste0("QC_missing_lopenr_NPRId_", suffix, ".csv"))
  write_csv(missing_map, miss_path)
  if (isTRUE(USER$strict_lopenr)) stop_user("STOPP: mangler løpenr")
}
```

---

de
