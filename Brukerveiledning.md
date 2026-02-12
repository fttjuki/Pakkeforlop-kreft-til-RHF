
# 📘 Enkel brukerveiledning – slik kjører du skriptet


---

## ✨ 1. Åpne R‑prosjektet 
1. Finn og dobbeltklikk på **R‑prosjektfila (.Rproj)**.
2. RStudio åpner seg i riktig prosjektmappe.
3. Dette sikrer at alle filer og moduler lastes korrekt.

> **Viktig:** Alltid åpne prosjektet først. Ikke åpne skript direkte fra filsystemet.

---


## 📝 2. Åpne kjøre‑fila
1. I RStudio: åpne **00_run.R**.
2. Dette er den eneste fila vi normalt trenger å endre.

---

## ⚙️ 3. Sett innstillinger i `USER`‑delen
Inne i `00_run.R` finnes blokka:

```r
USER <- list(
  step = "STEP2",             # "STEP1" eller "STEP2"
  months = c("2025-12-01"),  # alltid YYYY-MM-01
  base_dir = "...",
  app_dir  = "...",
  out_dir  = "...",
  ...
)
```

### 🔸 Velg steg (STEP1 eller STEP2)
- **STEP1**: lager kun NPRId‑fil for å bestille løpenr og stopper.
- **STEP2**: krever returfil (løpenr) og lager ferdige leveranser.

### 🔸 Velg måned(er)
- Endre måned, f.eks.:  
  `months = c("2025-12-01")`
- Flere måneder går fint:  
  `months = c("2025-12-01", "2026-01-01")`

---

## ▶️ 4. Kjør skriptet
1. Trykk **Source** (øverst til høyre i RStudio) mens `00_run.R` er åpen.
2. Vent til kjøringen er ferdig. I konsollen ser du til slutt:  
   `Total tid: X minutter`.

---

## 📬 5. Arbeidsflyt
### A) Første runde: Kjør **STEP1**
- Skriptet lager NPRID-fil i `app_dir`, f.eks.:  
  `NPRId_RHF_Pakkeforløp_nov25.csv`  
- **Send denne** for å få returfil med løpenr.

### B) Når returfil er mottatt: Kjør **STEP2**
- Legg returfilen i `app_dir` med navn:  
  `NPRId_RHF_Pakkeforløp_nov25_lnr.csv`
- Kjør `STEP2`. Ferdige leveranser havner i `out_dir`.

---

## 📑 6. Resultater 
Etter STEP2: CSV per RHF i `out_dir`, f.eks.:

- `Utlevering_HSØ_nov25.csv`
- `Utlevering_HV_nov25.csv`
- `Utlevering_HMN_nov25.csv`


---
