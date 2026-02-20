# ==============================================================================
# 01_run.R (START HER)

# ------------------------------------------------------------------------------
# Dette er inngangspunktet for hele prosjektet.

# 1) Last inn nødvendige pakker (tidyverse, dato-håndtering, database osv.)
# 2) Definer innstillinger (USER) som styrer hele kjøringen
# 3) Finn hvor skriptet ligger, og hvor modul-filene ligger (R-mappen)
# 4) "source" alle modulene (funksjoner) vi trenger
# 5) Kjør main(USER) som gjør jobben
# 6) Print hvor lang tid det tok
# ==============================================================================


 USER <- list(
   # USER-listen er den viktigste delen man skal endre.
  # Dette er innstillinger som bestemmer hva som skjer.
  # Tips: Endre bare verdier til høyre for "=".
   step = "STEP2",  # "STEP1" eller "STEP2"
  # step:
  # - STEP1: lager bare bestillingsfil (unik NPRId-liste) og stopper
  # - STEP2: krever at returfil med løpenr finnes, og lager full utlevering
   months = c("2025-12-01"), # format "YYYY-MM-01"
  # months:
  # - Hvilke måneder du vil kjøre for
  # - Må være første dag i måned (YYYY-MM-01)
  # - Koden lager suffix ut fra dette (f.eks. "des25")
  base_dir = "N:/Utleveringer/STYRINGSDATA/RHF Pakkeforløp kreft/R",
  # base_dir:
  # - Hovedmappen der inputfiler ligger, typisk sortert per region
  # - For eksempel kan du ha undermapper HSØ, HV, HMN osv.

  app_dir = "//fihr.no/dfs/NPR/Temp/NPR_RegistrerUtlevering",
  # app_dir:
  # - "Arbeidsmappe" / app-mappe
  # - Her lagres bl.a. bestillingsfilen med NPRId og forventet returfil med løpenr
  # - Tenk på den som en "mellomlager-mappe" for filer som sendes/returneres

  out_dir = "N:/Utleveringer/STYRINGSDATA/RHF Pakkeforløp Kreft/R",
  # out_dir:
  # - Der sluttresultat (utlevering CSV/Excel) og QC-filer og runbook lagres
  # - Dette er den viktigste mappen å sjekke etter kjøring

  fill_missing_kommune = TRUE,
  # fill_missing_kommune:
  # - TRUE: prøver å fylle KommuneNr/BydelNr som mangler ved databaseoppslag
  # - FALSE: gjør ikke databaseoppslag; leveranse kan få flere manglende kommuner
  strict_lopenr = FALSE,
  missing_token = "NULL",
  verbose = TRUE,
    # verbose:
  # - TRUE: skriver flere INFO-meldinger til konsollen (mer detaljer)
  # - FALSE: mer stille kjøring (mindre tekst)
 strict_input = TRUE,          # anbefalt: slutt å plukke "nyeste" feil fil
 repair_mojibake = TRUE,       # reparer Ã/Â dersom det finnes
fail_on_mojibake = FALSE,     # fail ved mojibake under lesing
fail_on_mojibake_out = TRUE,  # fail hvis output fortsatt har mojibake i Pakkeforløp
 encoding_override = NULL      # sett f.eks. "Windows-1252" hvis guess feiler
 )



start_tid <- Sys.time()
# Start tidtaking før main() kjører.
# Hvorfor: For å se hvor lang tid hele jobben tar.

source("01_setup.R")
main(USER) # Kjør hele prosessen med innstillingene i USER.

slutt_tid <- Sys.time()
# Slutt tidtaking etter main() er ferdig.

message("Total tid: ", round(difftime(slutt_tid, start_tid, units = "mins"), 2), " minutter")
# Skriver total tid til konsollen.
 
 







