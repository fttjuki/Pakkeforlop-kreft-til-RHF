# Notat til leder (enkelt språk): SQL‑rutine vs R‑pipeline (samme fag, bedre drift)

> **Mål:** Vise forskjellen mellom dagens manuelle SQL‑rutine og en automatisert R‑pipeline, med eksempler fra begge. Fokus er drift: tid, risiko og kvalitet – ikke «programmering». citeturn1search1turn6search1turn5search1turn4search2

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
- Det er lett å glemme å oppdatere noe, eller oppdatere noe *delvis*. citeturn6search1

---

### 2.2 Måned ligger også i tabellnavn og filnavn
SQL har månedsspesifikk tabell og forventer månedsspesifikk returfil fra Fihr. citeturn6search1

```sql
DROP TABLE IF EXISTS #NPRId_desember_2025; --måneden må endres hver måned

BULK INSERT #NPRId_desember_2025 FROM '\\fihr.no\\...\\NPRId_RHF_Pakkeforløp_des25_lnr.csv'
```

**Hvorfor dette er en risiko:**
- Tabellnavn + filnavn må «matche». En liten skrivefeil gir feil kjøring eller feil input. citeturn6search1

---

### 2.3 Samme innlesing gjentas tre ganger (HSØ/HV/HMN)
SQL leser tre filer med tre nesten like blokker. citeturn6search1

```sql
BULK INSERT #RapportHSØ FROM '...RHF_HSØ.csv' WITH (FIRSTROW = 2);
BULK INSERT #RapportHV  FROM '...RHF_HV.csv'  WITH (FIRSTROW = 2);
BULK INSERT #RapportHMN FROM '...RHF_HMN.csv' WITH (FIRSTROW = 2);
```

**Hvorfor dette er en risiko:**
- Endringer må gjøres flere steder → større sjanse for at regioner blir behandlet ulikt ved en feil. citeturn6search1

---

### 2.4 Manuell «feilretting» med hardkodede unntak
SQL har hardkodede oppdateringer for enkelte NPRId/år der kommune mangler. citeturn6search1

```sql
UPDATE #Fiks_HSØ_Kom SET komnrhjem2 = '0906' WHERE NPRId = '3200706' AND aar = 2008;
UPDATE #Fiks_HV_Kom  SET komnrhjem2 = '1135' WHERE NPRId = '3602577' AND aar = 2008;
```

**Hvorfor dette er en risiko:**
- Slike «spesialtilfeller» kan bli glemt eller kopiert feil i fremtiden. citeturn6search1

---

## 2b) (NYTT) Hvor mange ganger må man endre «måned/dato» i SQL – og hvor mye gjentar seg?

> **Kort og tydelig:** SQL: minst **7 manuelle endringer** + **3× kopiert logikk** per leveranse → **høy drift‑risiko**. citeturn6search1

### 2b.1 Minst **7 steder** per måned (ofte 8 i praksis)
I SQL‑rutinen ligger måned/år spredt i kommentar, tabellnavn, filnavn og i join‑punkter. Det betyr at man typisk må oppdatere **minst 7 steder** for én leveranse (ofte 8 hvis vi også teller «lagre med riktig månedsnavn»). citeturn6search1

**(1) Periode i kommentar (1 sted):**
```sql
/*
  Data for desember 2025. --må endres hver måned (og år)
*/
```
citeturn6search1

**(2–4) Måned i tabellnavn/filnavn (minst 3 steder):**
```sql
DROP TABLE IF EXISTS #NPRId_desember_2025; --måneden må endres hver måned
CREATE TABLE #NPRId_desember_2025 ( ... );
BULK INSERT #NPRId_desember_2025 FROM '\\fihr.no\\...\\NPRId_RHF_Pakkeforløp_des25_lnr.csv'
```
citeturn6search1

**(5–7) Den samme månedstabellen brukes i 3 utleveringer (3 steder):**
```sql
-- HSØ
LEFT JOIN #NPRId_desember_2025 AS b ON a.NPRId = b.NPRId
-- HV
LEFT JOIN #NPRId_desember_2025 AS b ON a.NPRId = b.NPRId
-- HMN
LEFT JOIN #NPRId_desember_2025 AS b ON a.NPRId = b.NPRId
```
citeturn6search1

**(8 – ofte i praksis) Manuell navngiving ved lagring:**
```sql
SELECT * FROM #NPRId WHERE NPRId IS NOT NULL;
-- (Lagre som "NPRId_RHF_Pakkeforløp_des25" i Uttrekksmappa)
```
citeturn6search1

---

### 2b.2 Hvor mange ganger gjentas samme prosess for HSØ/HV/HMN i SQL?
SQL‑rutinen kopierer samme mønster for hver region. I én leveranse blir det repetisjon i flere hovedsteg: innlesing, kommune‑fiks, utlevering og kontroll. citeturn6search1

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
citeturn6search1

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
citeturn6search1

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
citeturn6search1

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

**Lederpoeng:** Når samme ting gjentas 3 ganger, øker risikoen for små forskjeller og mer vedlikehold. R gjør dette i én loop med felles regler. citeturn4search2turn5search1

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
- Koden bruker dette konsekvent videre. citeturn5search1turn4search2

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
- Mindre risiko for at bestilling/retur/utlevering får ulike navn. citeturn4search2turn5search1

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
- Endringer gjøres én gang. citeturn4search2

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

## 4) «R erstatter ikke SQL» – et praktisk kompromiss
Hvis M ønsker å beholde SQL for database‑logikk, kan R fortsatt gi gevinst som **driftsmotor**:
- velger periode (months)
- finner inputfiler
- kjører likt for alle regioner
- lagrer resultat + QC

Dette følger også poenget i presentasjonen: SQL har flere manuelle steg, mens R kan automatisere dem (loop, dato‑logikk, automatisk lagring). citeturn1search1turn4search2turn5search1

---

## 5) Lav‑risiko pilot (1 leveranse)
1. Kjør SQL som i dag (referanse). citeturn6search1
2. Kjør R med samme måned (STEP2). citeturn5search1turn4search2
3. Sammenlign: antall rader per region + et par kontrollsummer.

**Hvis det matcher:** Vi har bevist at vi kan spare tid og redusere risiko uten å endre faglig innhold. citeturn1search1turn6search1turn4search2
