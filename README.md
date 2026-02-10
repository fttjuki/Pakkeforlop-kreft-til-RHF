# Pakkeforlop-kreft-til-RHF
R-script basert på eksisterende SQL-skript for Pakkeforløp kreft til RHF (HSØ, HV, HMN). Målet er å automatisere arbeidsprosesser som i dag gjøres i SQL, slik at det blir mindre manuelt.

## Bakgrunn:
Utvikler et R-script basert på eksisterende SQL-skript for Pakkeforløp kreft til RHF (HSØ, HV, HMN). Målet er å automatisere arbeidsprosesser som i dag gjøres i SQL, slik at det blir mindre manuelt. 
## Slik foregår arbeidet i dag:
•	Vi laster ned tre ferdiglagde datafiler fra Power BI, en for hvert RHF (HSØ, HV, HMN), for videre bearbeiding i SQL. Power BI-rapporten er laget for sluttbrukere som ikke leverer data videre, og filene kan derfor ikke videresendes direkte.
•	Vi åpne hver av de tre Power BI-filene manuelt, slette første rad og kolonne M før de kan brukes i SQL.
•	Hver måned må datoene oppdateres manuelt i SQL-koden.
•	I mars må hele prosessen kjøres to ganger (først for januar, deretter for februar).
•	Til slutt må filene lagres manuelt.

##  Hva gjør R-scriptet automatisk:
•	Leser inn de tre CSV-filene direkte og håndterer rad 1 og kolonne M automatisk.
•	Kjører prosessen for alle tre RHF (HSØ, HV, HMN) i én operasjon via en R-loop.
•	Håndterer datoer og mars-logikken automatisk, uten behov for manuelle endringer i koden.
•	Søker etter manglende KommuneNr og fyller disse automatisk via databaseoppslag.
•	Lagrer ferdige utleveringsfiler i riktig mappe via koding.
•	Lager en enkel QC-oversikt, slik at vi raskt ser om tallene ser fornuftige ut.
##  Alt vi trenger å gjøre er å trykke på “Source”-knappen i RStudio, så utføres hele prosessen automatisk.

