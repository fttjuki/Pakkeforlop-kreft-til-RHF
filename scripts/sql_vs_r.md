# SQL‑rutine vs R‑pipeline (samme leveranse, enklere drift)

**Hvem er dette for?** Ledere og kollegaer som ikke koder, og som ikke kjenner alle detaljer i NPR‑arbeidsflyten. citeturn1search1

**Mål:** Forklare *hvorfor* en automatisert R‑pipeline gir mindre manuelt arbeid og lavere risiko enn en manuell SQL‑rutine – uten å endre faglig innhold i leveransen. citeturn1search1turn6search1turn5search1turn4search2

---

## 0) Mini‑ordliste (for å forstå teksten)
- **RHF**: Regionale helseforetak (HSØ, HV, HMN). citeturn6search1turn4search2
- **NPRId**: ID som brukes for å identifisere person i rådata (skal ikke utleveres direkte). citeturn6search1turn4search2
- **løpenr**: Pseudonym/utleverings‑ID som bestilles via Fihr og brukes i leveransen. citeturn6search1turn4search2
- **Fihr**: Prosessen/systemet som gir løpenr tilbake via returfil. citeturn6search1turn5search1turn4search2
- **KommuneNr/BydelNr**: Geografi som noen ganger mangler og må fylles inn (via SOMHoved). citeturn6search1turn4search2

---

## 1) Hva gjør vi egentlig – uansett verktøy?
Dette er *den samme jobben* både i SQL og i R:
1. Les inn 3 filer (HSØ, HV, HMN). citeturn1search1turn6search1
2. Lag en liste over unike **NPRId** → send til Fihr for å bestille **løpenr**. citeturn1search1turn6search1turn4search2
3. Les inn returfil med løpenr. citeturn6search1turn4search2
4. Fyll inn manglende KommuneNr/BydelNr der det trengs. citeturn1search1turn6search1turn4search2
5. Lag 3 ferdige utleveringsfiler (én per RHF/region). citeturn1search1turn6search1turn4search2

**Poenget:** R endrer ikke *hva* vi leverer. R endrer *hvordan* vi produserer leveransen (mer automatisk, mindre manuelt). citeturn1search1turn5search1turn4search2

---

## 2) Hvorfor oppleves SQL‑rutinen tung i drift?
SQL‑scriptet er kraftig, men i praksis blir det mye manuelt:

### 2.1 Måned/år og filnavn må oppdateres manuelt
SQL sier selv at perioden må endres hver måned/år. citeturn6search1

```sql
/* Data for desember 2025. --må endres hver måned (og år) */
```

SQL bruker også måneden i tabellnavn og filnavn. citeturn6search1

```sql
DROP TABLE IF EXISTS #NPRId_desember_2025; --måneden må endres hver måned
BULK INSERT #NPRId_desember_2025 FROM '\\fihr.no\\...\\NPRId_RHF_Pakkeforløp_des25_lnr.csv'
```

**Hvorfor dette er viktig (leder‑språk):** Når måned ligger «spredt» i mange linjer, øker sjansen for små feil (feil måned, feil fil, feil navn). citeturn6search1

### 2.2 Samme prosess kopieres tre ganger (HSØ/HV/HMN)
Innlesing gjentas tre ganger (HSØ, HV, HMN). citeturn6search1

```sql
BULK INSERT #RapportHSØ FROM '...RHF_HSØ.csv' WITH (FIRSTROW = 2);
BULK INSERT #RapportHV  FROM '...RHF_HV.csv'  WITH (FIRSTROW = 2);
BULK INSERT #RapportHMN FROM '...RHF_HMN.csv' WITH (FIRSTROW = 2);
```

Utlevering bygges også i tre nesten like blokker. citeturn6search1

```sql
-- HSØ / HV / HMN har samme mønster med egne tabeller og joins
```

**Hvorfor dette er viktig:** Kopi‑lim gir mer vedlikehold og større risiko for at en liten endring blir gjort i én region, men glemt i en annen. citeturn6search1

---

## 3) Den mest direkte konklusjonen (én linje)
> **Kort og tydelig:** SQL: minst **7 manuelle endringer** + **3× kopiert logikk** per leveranse → **høy drift‑risiko**. citeturn6search1

*(Dette handler om drift og risiko – ikke om at SQL er «dårlig».)* citeturn1search1

---

## 4) Hvorfor R‑pipeline er enklere å bruke (for ikke‑kodere)

### 4.1 Du endrer bare en «innstillingsliste» (USER)
Startfila (`00_run.R`) sier rett ut at **USER‑listen er den viktigste delen man skal endre**, og at man normalt bare endrer verdier til høyre for `=`. citeturn5search1

Eksempel (forenklet): citeturn5search1
```r
USER <- list(
  step   = "STEP2",          # STEP1 = bestillingsfil, STEP2 = full leveranse
  months = c("2025-12-01"),  # hvilken måned som kjøres
  base_dir = "...",          # hvor inputfiler ligger
  app_dir  = "...",          # hvor bestilling/returfil ligger
  out_dir  = "...",          # hvor resultat + QC lagres
  strict_input = TRUE
)
```

**Hva betyr dette i praksis?**
- Bytt måned i én linje (`months = ...`). citeturn5search1
- Velg om du er i **STEP1** (lag bestillingsfil) eller **STEP2** (lag leveranse). citeturn5search1turn4search2

### 4.2 «Én knapp»: kjør alt via main(USER)
Startfila kjører prosessen slik: `source("01_setup.R")` → `main(USER)` og skriver total tid. citeturn5search1turn4search2

```r
source("01_setup.R")
main(USER)
```

---

## 5) R er «smartere» på tre konkrete ting (lett å forstå)

### 5.1 R lager riktig månedsnavn automatisk (f.eks. des25)
R lager suffix automatisk fra datoen du skriver inn. citeturn5search1turn4search2

```r
month_to_suffix <- function(month_date) {
  d <- as.Date(month_date)
  paste0(CFG$months_nor[lubridate::month(d)], format(d, "%y"))
}
```

**Hva du får:** mindre manuelt «navne‑arbeid» og mindre risiko for at bestilling/retur/utlevering får ulike navn. citeturn4search2turn5search1

### 5.2 R behandler alle regioner likt, automatisk
Regioner ligger i én liste, og pipeline går gjennom dem i loop. citeturn4search2

```r
CFG <- list(regions = c("HSØ", "HV", "HMN"))
```

**Hva du får:** ingen kopi‑lim‑blokker per region – én regel gjelder for alle. citeturn4search2

### 5.3 R har innebygde «sikkerhetsbelter» (QC + stop)
R kan stoppe tidlig hvis input mangler (strict_input), og kan skrive ut QC‑liste hvis løpenr mangler. citeturn5search1turn4search2

Eksempel (idé): citeturn4search2turn5search1
```r
if (length(hits) == 0 && isTRUE(USER$strict_input)) stop_user("Fant ingen suffix-fil...")
if (nrow(missing_map) > 0) write_csv(missing_map, "QC_missing_lopenr...")
```

**Hva du får:** avvik blir synlige før levering, ikke etterpå. citeturn4search2

---

## 6) Forslag til «trygg» innføring (lav risiko)
1. Kjør SQL som i dag (referanse). citeturn6search1
2. Kjør R på samme måned (STEP2). citeturn5search1turn4search2
3. Sammenlign 2–3 kontrolltall (antall rader per region + noen nøkkeltall).

Hvis resultatet matcher, kan R brukes som standard «driftsmotor» (mens SQL fortsatt kan beholdes som referanse/gullstandard). citeturn1search1turn6search1turn4search2
