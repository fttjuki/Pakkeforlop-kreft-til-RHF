# Notat til leder (enkelt språk): SQL‑rutine vs R‑pipeline
*Mål: Vise forskjellen mellom dagens manuelle SQL‑rutine og en automatisert R‑pipeline, med eksempler fra begge. Fokus er drift: tid, risiko og kvalitet – ikke «programmering».*

## 1) Kort oppsummering (30 sek)

**I dag:** SQL‑scriptet krever at man endrer måned/år og filnavn manuelt, og gjør mange like steg for flere regioner.  
**Med R:** Man endrer bare noen få innstillinger (USER) og trykker «Run». Resten går automatisk: riktig periode, riktig fil, alle regioner, lagring og QC‑sjekk.  
**Resultat:** Samme faglig innhold i leveransen, men mindre manuelt arbeid og lavere feilrisiko.

## 1.1 Slik foregikk prosessen tidligere (manuelt)

Denne delen beskriver hvordan prosessen foregikk før automatiseringen. Den viser hvorfor stabil drift med SQL var tidkrevende og håndarbeidsintensiv:

- Tre CSV‑filer måtte lastes ned fra Power BI – én for hvert RHF (HSØ, HV, HMN).
- Første rad måtte slettes manuelt fordi Power BI la inn en ekstra header‑linje.
- Kolonne M måtte slettes manuelt fordi den ofte var tom eller ubrukelig.
- Datoer måtte skrives inn manuelt i SQL‑koden, noe som skapte høy risiko for feil.
- I mars måtte prosessen kjøres to ganger (for januar og februar) pga. manuelle endringer.
- Alle CSV‑filer måtte åpnes og lagres manuelt etter at de var behandlet.
- KommuneNr måtte fylles inn manuelt for rader der det manglet.

R‑pipen eliminerer alle disse punktene ved å automatisere både filhåndtering, datovalg, rensing og kvalitetskontroller.

## 2) Hvor er «manuell risiko» i SQL‑rutinen? (konkrete eksempler)

### 2.1 Måned/år må endres manuelt (hver gang)

SQL‑scriptet krever manuell oppdatering av periode:

```
/* Data for desember 2025. --må endres hver måned (og år) */
```

**Risiko:** Ting kan glemmes, eller bare delvis oppdateres.

### 2.2 Måned ligger også i tabellnavn og filnavn

```
DROP TABLE IF EXISTS #NPRId_desember_2025;
BULK INSERT #NPRId_desember_2025
FROM "\\fihr.no\...\NPRId_RHF_Pakkeforløp_des25_lnr.csv"
```

**Risiko:** Tabellnavn og filnavn må matche perfekt → skrivefeil gir feil input eller stopp.

### 2.3 Samme innlesing gjentas tre ganger (HSØ/HV/HMN)

Tre nesten identiske blokker må vedlikeholdes:

```
BULK INSERT #RapportHSØ ...
BULK INSERT #RapportHV ...
BULK INSERT #RapportHMN ...
```

**Risiko:** Endringer må gjøres tre ganger → høy sjanse for inkonsistens.

### 2.4 Manuell «feilretting» med hardkodede unntak

```
UPDATE #Fiks_HSØ_Kom SET komnrhjem2 = '0906' WHERE ...
UPDATE #Fiks_HV_Kom  SET komnrhjem2 = '1135' WHERE ...
```

**Risiko:** Hardkoding blir lett oversett, glemt eller misplassert.

## 2b) Hvor mange ganger må man endre måned/dato i SQL – og hvor mye gjentar seg?

**Kort:** SQL krever *minst 7 manuelle endringer per måned*, og har *tre parallelle kopier* av samme logikk → høy drift‑risiko.

### 2b.1 Minst 7 steder per måned (ofte 8 i praksis)

Eksempler på hvor dato/måned ligger spredt:

- Kommentar
- Tabellnavn
- Filnavn
- Både i innlesing og joins
- Filnavn ved lagring

### 2b.2 Hvor mye gjentas for HSØ/HV/HMN?

For hver region (3 ganger) gjentas:

- **Innlesing:** DROP + CREATE + BULK INSERT  
- **Kommune‑fiks:** bygge #Kom og #Fiks_Kom  
- **Utlevering:** SELECT ... INTO #Region  
- **Kontroller:** månedssjekk + antall rader

Dette skaper både mye vedlikehold og høy risiko for at én region blir behandlet annerledes enn de andre.

## 3) Hva gjør R‑pipeline smartere (og tryggere)?

### 3.1 I R endrer man bare USER‑innstillinger

```r
USER <- list(
  step = "STEP2",
  months = c("2025-12-01"),
  base_dir = "...",
  out_dir = "...",
  strict_input = TRUE
)
```

### 3.2 Smart datohåndtering

Suffix som `des25` lages automatisk fra datoen → ingen manuell skriving.

### 3.3 Loop: Én logikk, flere regioner

```r
regions = c("HSØ", "HV", "HMN")
```

Én definisjon → behandles automatisk og likt.

### 3.4 Oppdager feil filer automatisk

R sjekker at korrekt fil finnes. Hvis ikke → stopp med god feilmelding.

### 3.5 Automatisk håndtering av æøå og encoding-feil

R reparerer kjente «mølje‑tekst»-problemer før prosessering.

### 3.6 QC: R lager avvikslister og kan stoppe

Hvis NPRId mangler løpenr → skrives QC‑fil + valg om å stoppe.

## 4) «R erstatter ikke SQL» – et praktisk kompromiss

SQL kan fortsatt gjøre databasearbeidet.  
R tar driftsjobben:

- velger periode
- finner filene
- kjører likt for alle regioner
- lagrer resultat
- QC‑sjekker

→ Mindre manuelt arbeid, mindre risiko, samme faglige innhold.

## 5) Lav‑risiko pilot (1 leveranse)

1. Kjør SQL som i dag (referanse).  
2. Kjør R med samme måned.  
3. Sammenlign radtall og kontrollsummer.

**Hvis det matcher:**  
→ Vi sparer tid, reduserer risiko og får en tryggere drift uten å endre faglig innhold.
