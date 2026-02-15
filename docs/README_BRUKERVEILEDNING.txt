BRUKERVEILEDNING 
=================================

Dette er en 2-stegs prosess.
Målet er å lage utleveringsfiler til RHF (HSØ, HV, HMN).

VIKTIG OM NORSKE BOKSTAVER (æ, ø, å)
-----------------------------------
- CSV-filene skrives som UTF-8 med BOM slik at Excel vanligvis viser æ/ø/å riktig.
- Excel-filer (.xlsx) støtter alltid norske tegn.

FØR DU STARTER (gjelder både STEP1 og STEP2)
--------------------------------------------
1) Last ned 3 CSV-filer fra Power BI (en per RHF): HSØ, HV, HMN.
2) Legg filene i riktig mappe (base_dir). Det skal være en undermappe per RHF:
   - <base_dir>/HSØ/
   - <base_dir>/HV/
   - <base_dir>/HMN/
3) Åpne filen 00_run.R i RStudio.

STEP 1 (før du bestiller løpenr)
--------------------------------
Hensikt: Lage en bestillingsfil med NPRId som du bruker i "Registrer Utlevering".

A) I 00_run.R: finn USER-blokken.
B) Sett:
   - step   = "STEP1"
   - months = c("2025-12-01")  (eksempel)

C) Kjør skriptet:
   - I RStudio: Trykk "Source" (øverst til høyre) eller Ctrl+Shift+S.

D) Resultat:
   - Bestillingsfil lagres i app_dir:
     NPRId_RHF_Pakkeforløp_<suffix>.csv

STEP 2 (etter at du har mottatt ..._lnr.csv)
-------------------------------------------
Hensikt: Lage ferdige utleveringsfiler ved å koble på løpenr.

A) Legg returfilen i app_dir:
   - NPRId_RHF_Pakkeforløp_<suffix>_lnr.csv

B) I 00_run.R: sett:
   - step   = "STEP2"
   - months = c("2025-12-01")  (samme måned som i STEP1)

C) Kjør skriptet igjen ("Source").

D) Resultat i out_dir:
   - CSV-utlevering (UTF-8 med BOM, skilletegn ';')
   - Excel-utlevering (.xlsx)
   For hver region:
     Utlevering_HSØ_<suffix>.csv og Utlevering_HSØ_<suffix>.xlsx
     Utlevering_HV_<suffix>.csv  og Utlevering_HV_<suffix>.xlsx
     Utlevering_HMN_<suffix>.csv og Utlevering_HMN_<suffix>.xlsx

VIKTIG: Numeriske felter
------------------------
Skriptet forsøker å gjøre både institusjonId og lopenr numeriske i sluttfilene.
Hvis noen verdier ikke kan tolkes som tall, blir de satt til NA og dette logges i Runbook.

RUNBOOK (automatisk logg)
------------------------
Hver gang du kjører 00_run.R, lages en tekstfil i out_dir:
  Runbook_PakkeforlopKreft_YYYYMMDD_HHMMSS.txt
Den viser blant annet:
- hvilke innstillinger som ble brukt
- hvilke inputfiler som ble funnet
- radtall (input per RHF, total, bestillingsfil, løpenr-returfil, utlevering per RHF)
- hvilke filer som ble skrevet (CSV og Excel)
- eventuelle advarsler/feil

Anbefalt filtype for veiledning i sikker sone
---------------------------------------------
Anbefaling: README.txt + (valgfritt) Word.
- README.txt er alltid lesbar og enkel å versjonsstyre.
- Word er fint for prosedyrer.
