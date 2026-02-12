
# 🍳 Kokebok – slik kjører du leveransen (kortest mulig)

### 1. Åpne prosjektet
- Finn og dobbeltklikk på **R‑prosjektfila (.Rproj)**  
  → RStudio åpner seg riktig.

### 2. Åpne kjøre-fila
- I RStudio: åpne **00_run.R**

### 3. Sett måned
I toppen av fila, endre for eksempel:

```
months = c("2025-12-01")
```

### 4. Velg steg
- Førstegangs kjøring:
```
step = "STEP1"
```
- Når du har fått returfilen med løpenr:
```
step = "STEP2"
```

### 5. Sjekk mappene
- Inputfiler ligger i `base_dir/HSØ`, `HV`, `HMN`, med riktig suffix (f.eks. des25)
- `app_dir`: her kommer bestillingsfil / ligger returfil
- `out_dir`: hit kommer sluttfiler

### 6. Kjør
- Trykk **Source** (øverst til høyre i RStudio)

### 7. Resultater
- Etter STEP1 → bestillingsfil i `app_dir`
- Etter STEP2 → ferdige leveranser i `out_dir`, én per region
