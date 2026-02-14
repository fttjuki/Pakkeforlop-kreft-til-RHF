# Hvorfor SQL viser 14 manglende **KommuneNr**, men R ikke gjør det (enkelt forklart)

**Kort svar:**
R prøver aktivt å *fylle inn* manglende KommuneNr (og regner både tomt felt og teksten `"NULL"` som «mangler»). SQL fyller bare inn når feltet er *ekte* SQL-`NULL`. Når data kommer fra CSV, er «manglende» ofte lagret som tom tekst eller `"NULL"` (tekst), og da trigger ikke SQL-fiksingen. citeturn1search2turn1search1turn1search3

---

## 1) Hva er «manglende» i SQL vs R?

### SQL: Teller bare **ekte NULL** som manglende
I SQL-scriptet står logikken slik (for HSØ/HV/HMN):

```sql
CASE 
  WHEN KommuneNr IS NULL THEN c.komnrhjem2
  ELSE KommuneNr
END KommuneNr
```
SQL fyller altså *bare* når `KommuneNr IS NULL`. Hvis KommuneNr er **tom tekst** (`""`) eller teksten **`"NULL"`**, så er det **ikke** SQL-NULL, og da blir det ikke fylt. citeturn1search1

---

### R: Regner **NA, tomt og "NULL"** som manglende
I R-pipelinen defineres «manglende KommuneNr» bredere: det kan være **NA**, **tom streng** eller **teksten `"NULL"`**. Dette er (forenklet) det som sjekkes:

```r
Kommune_missing = is.na(KommuneNr_orig) | KommuneNr_orig == "" | KommuneNr_orig == "NULL"
KommuneNr = if_else(Kommune_missing, komnrhjem2, KommuneNr_orig)
```
Det betyr at R fanger opp flere «manglende»-varianter som ofte oppstår i CSV-filer. citeturn1search3

---

## 2) R er satt opp til å fylle manglende KommuneNr
I startfila (`00_run.R`) står denne innstillingen:

```r
fill_missing_kommune = TRUE
```
Dette betyr: **R skal prøve å fylle KommuneNr/BydelNr som mangler ved databaseoppslag**. citeturn1search2

SQL-scriptet har også en «fiks manglende kommuneNr»-del, men den virker i praksis bare når KommuneNr er *ekte* `NULL`. citeturn1search1

---

## 3) Hvorfor SQL ender med 14 «manglende»?

Når SQL leser inn CSV med `BULK INSERT`, blir «manglende verdi» ofte liggende igjen som:
- **tom celle** (som blir tom tekst), eller
- teksten **`"NULL"`**

Da er ikke feltet **SQL-NULL**, og denne betingelsen blir *ikke sann*:

```sql
WHEN KommuneNr IS NULL
```
Resultat: SQL fyller ikke, og dere ser fortsatt «manglende» i output. citeturn1search1

R derimot sjekker både tomt og `"NULL"`-tekst som manglende, og fyller derfor disse også. citeturn1search3turn1search2

---

## 4) Hvorfor R kan regnes som «riktig» her

I denne leveransen er målet tydelig: **manglende KommuneNr skal fylles ved oppslag** (SQL-delen heter til og med «Fikser manglende kommuneNr og BydelNr»). citeturn1search1

- **R gjør faktisk det målet beskriver**, fordi den gjenkjenner de vanligste «manglende»-formatene fra CSV og fyller dem. citeturn1search3turn1search2
- **SQL gjør det bare delvis**, fordi den bare fyller når verdien er ekte `NULL`, ikke når den er tom eller `"NULL"`-tekst. citeturn1search1

Derfor kan SQL-output bli «feil» (viser mangler) selv om informasjonen finnes og kunne vært fylt inn. citeturn1search1turn1search3

---

## 5) (Valgfritt) Hvordan gjøre SQL «like riktig» som R

For å få SQL til å oppføre seg mer som R, må SQL behandle tom tekst og `"NULL"`-tekst som manglende. I praksis betyr det å gjøre dem om til ekte `NULL` før `CASE`-logikken brukes.

**Eksempel-prinsipp (ikke nødvendig å kunne kode):**
- «Hvis KommuneNr er tom eller `"NULL"`, behandle det som NULL»
- «Så kan `WHEN KommuneNr IS NULL` fungere som ønsket»

Dette er akkurat den typen «mangler»-opprydding R allerede gjør. citeturn1search3turn1search1

---

## Oppsummert (1 linje)
**R er mest korrekt i denne situasjonen**, fordi den faktisk fyller KommuneNr når det står tomt eller `"NULL"` i CSV – mens SQL ofte ikke gjør det fordi den bare sjekker `IS NULL`. citeturn1search3turn1search1turn1search2
