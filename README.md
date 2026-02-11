
# RHF Pakkeforløp Kreft – Modulbasert R‑pipeline

Denne repositoriet inneholder en modulbasert R‑pipeline som erstatter den gamle, manuelle SQL‑baserte prosessen som brukes for å lage utleveringsfiler for Pakkeforløp Kreft til RHF (HSØ, HV, HMN). Løsningen er laget for å redusere manuelt arbeid, sikre kvalitet, og gjøre prosessen enklere for brukere uten erfaring med R.

---

## 🏥 Bakgrunn – hvordan den gamle løsningen fungerte

I den tidligere arbeidsflyten ble data behandlet **manuelt** gjennom flere trinn før de kunne brukes til utlevering. Dette skapte risiko for feil, stort tidsforbruk og manglende sporbarhet.

### 🔧 Slik foregikk prosessen tidligere (manuelt)

1. **Tre CSV‑filer måtte lastes ned fra Power BI** – én for hvert RHF (HSØ, HV, HMN).
2. **Første rad måtte slettes manuelt** i alle tre filer fordi Power BI skrev en ekstra header‑linje.
3. **Kolonne M måtte slettes manuelt** fordi den ofte var tom/ubrukelig.
4. **Datoer måtte skrives inn manuelt i SQL‑koden**, noe som skapte fare for feil.
5. I **mars måtte prosessen gjøres to ganger** (for januar og februar) via manuelle kodeendringer.
6. **Alle CSV‑filer måtte åpnes og lagres manuelt** etter behandling.
7. **KommuneNr måtte fylles inn manuelt** for rader som manglet dette.
8. Ingen automatisk validering – bare manuell visuell kontroll.
9. Prosessen var **tung, tidkrevende og vanskelig å verifisere** for personer uten SQL‑kompetanse.

### ❗ Utfordringer med den manuelle prosessen
- Høy risiko for menneskelige feil
- Inkonsekvent databehandling
- Avhengighet av enkeltpersoner med SQL‑kompetanse
- Ingen automatisk logging eller dokumentasjon
- Tidkrevende hver måned – spesielt i mars
- Vanskelig å gjenskape eller kontrollere tidligere leveranser

---

## 🚀 Den nye R‑løsningen – fullautomatisert og sporbar

R‑pipeline erstatter hele den manuelle SQL‑prosessen med én knapp i RStudio.

### ✔ Hva R‑scriptet gjør automatisk

- Leser alle tre Power BI‑CSV‑filer direkte
- Fjerner første rad automatisk
- Fjerner kolonne M hvis den er tom
- Standardiserer kolonnenavn
- Kjør komplett prosess for alle RHF i én loop
- Automatisk håndtering av datoer og mars‑logikken
- Slår sammen alle data til én samlet tabell
- Lager bestillingsfil (STEP1)
- Leser inn `*_lnr.csv` (STEP2)
- Fyller manglende KommuneNr fra database
- Lager ferdige utleveringsfiler i:
  - CSV (UTF‑8 BOM – støtter æ/ø/å i Excel)
  - Excel (.xlsx)
- Lager QC‑oversikt
- Lager detaljert runbook som dokumenterer **alt** som skjedde

Alt du trenger er å trykke **Source**.

---

## 🧭 Kom i gang

### 1. Lag lokal konfigurasjon
Kopier:
```
config/USER_example.R → config/USER_local.R
```
Rediger `USER_local.R` og fyll inn dine lokale stier.

> `USER_local.R` er ignorert av git, så du kan trygt legge inn interne filbaner.

### 2. Kjør skriptet
Åpne i RStudio:
```
scripts/00_run.R
```
Trykk **Source**.

---

## ▶️ STEP1 – før du bestiller løpenummer
I `USER_local.R`, sett:
```r
step   = "STEP1"
months = c("YYYY-MM-01")
```
Kjør:
```
scripts/00_run.R
```
Output: bestillingsfil for NPRId.

---

## ▶️ STEP2 – etter at du mottar returfil
Plasser `*_lnr.csv` i `app_dir`.

I `USER_local.R`, sett:
```r
step   = "STEP2"
months = c("YYYY-MM-01")
```
Kjør:
```
scripts/00_run.R
```
Output:
- Utlevering (CSV + Excel)
- QC
- Runbook

---

## 📁 Mappestruktur
```
repo/
  scripts/00_run.R
  R/              # moduler
  config/         # USER_example.R (commit), USER_local.R (ignored)
  docs/           # veiledning
  output/         # lokal output (ignored)
```

---

## 📦 Avhengigheter
**Obligatorisk:**
- tidyverse
- lubridate
- DBI
- odbc

**Valgfritt:**
- arrow (parquet‑cache)
- openxlsx eller writexl (Excel‑eksport)

---

## 📜 Lisens
Legg ønsket lisens i `LICENSE` (f.eks. MIT).

