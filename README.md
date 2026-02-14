#  SQL‑rutine vs R‑pipeline 

## 1) process i dag
-  Første rad slettes manuelt fordi Power BI la inn en ekstra header‑linje.
-  Kolonne M slettes manuelt fordi den ofte var tom eller ubrukelig.
-  Datoer skrives inn manuelt i SQL‑koden, noe som skapte høy risiko for feil.
-  I mars må prosessen kjøres to ganger (for januar og februar) pga. manuelle endringer.
-  Alle resultat-tabeller lagres manuelt.
-  KommuneNr fylles inn manuelt for rader der det manglet.

---

## 2) Hvor er «manuell risiko» i SQL‑rutinen? 

### 2.1 Måned/år må endres manuelt (hver gang)
SQL‑scriptet sier eksplisitt at perioden må endres hver måned/år. citeturn6search1

```sql
/*
  Data for desember 2025. --må endres hver måned (og år)
*/
```

**Hvorfor dette er en risiko:**
- Det er lett å glemme å oppdatere noe, eller oppdatere noe *delvis*. citeturn6search1

---

### 2.2 Måned ligger også i tabellnavn og filnavn
SQL har månedsspesifikk tabell og forventer månedsspesifikk returfil fra Fihr. 

```sql
DROP TABLE IF EXISTS #NPRId_desember_2025; --måneden må endres hver måned

BULK INSERT #NPRId_desember_2025 FROM '\\fihr.no\\...\\NPRId_RHF_Pakkeforløp_des25_lnr.csv'
```

**Hvorfor dette er en risiko:**
- Tabellnavn + filnavn må «matche». En liten skrivefeil gir feil kjøring eller feil input. 

---

### 2.3 Samme innlesing gjentas tre ganger (HSØ/HV/HMN)
SQL leser tre filer med tre nesten like blokker. 

```sql
BULK INSERT #RapportHSØ FROM '...RHF_HSØ.csv' WITH (FIRSTROW = 2);
BULK INSERT #RapportHV  FROM '...RHF_HV.csv'  WITH (FIRSTROW = 2);
BULK INSERT #RapportHMN FROM '...RHF_HMN.csv' WITH (FIRSTROW = 2);
```

**Hvorfor dette er en risiko:**
- Endringer må gjøres flere steder → større sjanse for at regioner blir behandlet ulikt ved en feil. 

---

### 2.4 Manuell «feilretting» med hardkodede unntak
SQL har hardkodede oppdateringer for enkelte NPRId/år der kommune mangler. 

```sql
UPDATE #Fiks_HSØ_Kom SET komnrhjem2 = '0906' WHERE NPRId = '3200706' AND aar = 2008;
UPDATE #Fiks_HV_Kom  SET komnrhjem2 = '1135' WHERE NPRId = '3602577' AND aar = 2008;
```

**Hvorfor dette er en risiko:**
- Slike «spesialtilfeller» kan bli glemt eller kopiert feil i fremtiden. 
---

## 2b) Hvor mange ganger må man endre «måned/dato» i SQL og hvor mye gjentar seg?

### I SQL: minst 7 manuelle endringer + 3× kopiert logikk per leveranse. 

### 2b.1 Minst **7 steder** per måned (ofte 8 i praksis)
I SQL‑rutinen ligger måned/år spredt i kommentar, tabellnavn, filnavn og i join‑punkter. Det betyr at man typisk må oppdatere **minst 7 steder** for én leveranse (ofte 8 hvis vi også teller «lagre med riktig månedsnavn»). 
**(1) Periode i kommentar (1 sted):**
```sql
/*
  Data for desember 2025. --må endres hver måned (og år)
*/
```


**(2–4) Måned i tabellnavn/filnavn (minst 3 steder):**
```sql
DROP TABLE IF EXISTS #NPRId_desember_2025; --måneden må endres hver måned
CREATE TABLE #NPRId_desember_2025 ( ... );
BULK INSERT #NPRId_desember_2025 FROM '\\fihr.no\\...\\NPRId_RHF_Pakkeforløp_des25_lnr.csv'
```

**(5–7) Den samme månedstabellen brukes i 3 utleveringer (3 steder):**
```sql
-- HSØ
LEFT JOIN #NPRId_desember_2025 AS b ON a.NPRId = b.NPRId
-- HV
LEFT JOIN #NPRId_desember_2025 AS b ON a.NPRId = b.NPRId
-- HMN
LEFT JOIN #NPRId_desember_2025 AS b ON a.NPRId = b.NPRId
```


**(8 – ofte i praksis) Manuell navngiving ved lagring:**
```sql
SELECT * FROM #NPRId WHERE NPRId IS NOT NULL;
-- (Lagre som "NPRId_RHF_Pakkeforløp_des25" i Uttrekksmappa)
```


---

### 2b.2 Hvor mange ganger gjentas samme prosess for HSØ/HV/HMN i SQL?
SQL‑rutinen kopierer samme mønster for hver region. I én leveranse blir det repetisjon i flere hovedsteg: innlesing, kommune‑fiks, utlevering og kontroll. 
**a) Innlesing (DROP + CREATE + BULK INSERT) gjentas 3 ganger:**
```sql
-- HSØ
DROP TABLE IF EXISTS #RapportHSØ;
CREATE TABLE #RapportHSØ ( ... );
BULK INSERT #RapportHSØ FROM '...RHF_HSØ.csv' WITH (FIRSTROW = 2);

-- HV
DROP TABLE IF EXISTS #RapportHV;
CREATE TABLE #RapportHV ( ... );
BULK INSERT #RapportHV  FROM '...RHF_HV.csv'  WITH (FIRSTROW = 2);

-- HMN
DROP TABLE IF EXISTS #RapportHMN;
CREATE TABLE #RapportHMN ( ... );
BULK INSERT #RapportHMN FROM '...RHF_HMN.csv' WITH (FIRSTROW = 2);
```

**b) Kommune‑fiks (bygg #*_Kom og #Fiks_*_Kom) gjentas 3 ganger:**
```sql
-- HSØ
DROP TABLE IF EXISTS #HSØ_Kom;
... INTO #HSØ_Kom ... WHERE KommuneNr IS NULL;
DROP TABLE IF EXISTS #Fiks_HSØ_Kom;
... INTO #Fiks_HSØ_Kom ... FROM SOMHoved ...;

-- HV
DROP TABLE IF EXISTS #HV_Kom;
... INTO #HV_Kom ... WHERE KommuneNr IS NULL;
DROP TABLE IF EXISTS #Fiks_HV_Kom;
... INTO #Fiks_HV_Kom ... FROM SOMHoved ...;

-- HMN
DROP TABLE IF EXISTS #HMN_Kom;
... INTO #HMN_Kom ... WHERE KommuneNr IS NULL;
DROP TABLE IF EXISTS #Fiks_HMN_Kom;
... INTO #Fiks_HMN_Kom ... FROM SOMHoved ...;
```

**c) Utlevering (SELECT … INTO #Region … JOIN … WHERE …) gjentas 3 ganger:**
```sql
-- HSØ
DROP TABLE IF EXISTS #HSØ;
SELECT ... INTO #HSØ
FROM #RapportHSØ a
LEFT JOIN #NPRId_desember_2025 b ON a.NPRId=b.NPRId
LEFT JOIN #Fiks_HSØ_Kom c ON a.NPRId=c.NPRId
WHERE b.lopenr != 'NULL' AND a.NPRId != 'NPRId';

-- HV
DROP TABLE IF EXISTS #HV;
SELECT ... INTO #HV
FROM #RapportHV a
LEFT JOIN #NPRId_desember_2025 b ON a.NPRId=b.NPRId
LEFT JOIN #Fiks_HV_Kom c ON a.NPRId=c.NPRId
WHERE b.lopenr != 'NULL' AND a.NPRId != 'NPRId';

-- HMN
DROP TABLE IF EXISTS #HMN;
SELECT ... INTO #HMN
FROM #RapportHMN a
LEFT JOIN #NPRId_desember_2025 b ON a.NPRId=b.NPRId
LEFT JOIN #Fiks_HMN_Kom c ON a.NPRId=c.NPRId
WHERE b.lopenr != 'NULL' AND a.NPRId != 'NPRId';
```


**d) Kontroller (MONTH(StartDato) …) gjentas 3 ganger:**
```sql
SELECT MONTH(StartDato) AS måned, count(*) AS AntallRader
FROM #HSØ WHERE År = 2025 GROUP BY MONTH(StartDato);

SELECT MONTH(StartDato) AS måned, count(*) AS AntallRader
FROM #HV WHERE År = 2025 GROUP BY MONTH(StartDato);

SELECT MONTH(StartDato) AS måned, count(*) AS AntallRader
FROM #HMN WHERE År = 2025 GROUP BY MONTH(StartDato);
```
citeturn6search1

**Lederpoeng:** Når samme ting gjentas 3 ganger, øker risikoen for små forskjeller og mer vedlikehold. R gjør dette i én loop med felles regler. 
---

## 3) Hva gjør R‑pipeline smartere (og tryggere)?

### 3.1 I R endrer man bare én ting: USER‑innstillinger (ikke masse kode)
Startfila sier tydelig at USER‑listen er det eneste man normalt skal endre. 
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
I R lages fil‑suffix automatisk fra `months` (YYYY‑MM‑01). 
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
R har regionliste én gang, og behandler alle likt. 
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
Dette reduserer risikoen for å bruke feil fil eller feil periode. citeturn5search1turn4search2

```r
if (length(hits) == 0 && isTRUE(USER$strict_input)) {
  stop_user("Fant ingen suffix-fil for %s (suffix=%s).", region, suffix)
}
```

---

### 3.5 Norske tegn (æøå): R håndterer encoding og reparerer typiske feil
R har logikk for å oppdage/rette «Ã¦/Ã¸»‑problemer. 
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
R kan skrive en QC‑fil med NPRId som mangler løpenr, og eventuelt stoppe. 

```r
if (nrow(missing_map) > 0) {
  miss_path <- file.path(USER$out_dir, paste0("QC_missing_lopenr_NPRId_", suffix, ".csv"))
  write_csv(missing_map, miss_path)
  if (isTRUE(USER$strict_lopenr)) stop_user("STOPP: mangler løpenr")
}
```

---

