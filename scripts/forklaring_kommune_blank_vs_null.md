# Hvorfor R får 0 manglende KommuneNr, men SQL får 14



---

## 1) To typer «tomt» som ser like ut i Excel/csv

I Excel/csv ser begge disse ofte ut som **blanke celler** ("(Blanks)"):

### A) **Tom celle** (ingen tegn)
- Det står **ingenting** i feltet.
- I en CSV kan det se slik ut:

```text
NPRId;KommuneNr
1;
```

### B) **Tom tekstverdi** (det finnes tegn, men ser tomt ut)
- Feltet inneholder **noe**, f.eks. **tom tekst** (`""`) eller **mellomrom** (`" "`).
- I en CSV kan det se slik ut:

```text
NPRId;KommuneNr
3;""
4; 
```

**Viktig:** Excel viser ofte både A og B som «blankt», men datasystemer kan behandle dem forskjellig.

---

## 2) Mini-eksempel: samme CSV, men ulike «tom»-typer

La oss si at input-filen har fire rader:

```text
NPRId;KommuneNr
1;0301
2;
3;""
4; 
```

- Rad 2: *helt tomt felt* (type A)
- Rad 3: *tom tekstverdi* (type B: `""`)
- Rad 4: *mellomrom* (type B: `" "`)

---

## 3) Hva SQL-skriptet deres gjør (enkelt forklart)

SQL-skriptet deres har en «fyll inn manglende kommune»-regel som i praksis sier:

> **Bare hvis KommuneNr er registrert som «mangler» i SQL**, så prøver jeg å hente kommune fra en annen tabell (SOMHoved).

Det ser vi i SQL-koden:
- Det sjekkes etter mangler med `KommuneNr IS NULL`.
- Og det fylles bare når `KommuneNr IS NULL`.

citeturn1search1

### Konsekvens i mini-eksempelet
- Hvis SQL tolker feltet som **«mangler» (NULL)**, da blir det fylt.
- Men hvis feltet blir liggende som **tom tekst** ("" eller mellomrom), da **oppfatter ikke SQL det som «mangler»**, og da blir det **ikke fylt**.

Derfor kan noen rader bli stående uten KommuneNr i SQL-resultatet.

---

## 4) Hva R-skriptet deres gjør (enkelt forklart)

R-pipelinen deres er laget slik at den regner disse som «mangler»:
- ekte mangler (`NA`)
- **tomt felt** (`""`)
- teksten `"NULL"`

Og hvis `fill_missing_kommune = TRUE`, så slår R opp i database og fyller inn KommuneNr.

Dette står i R-koden der «manglende kommune» defineres som `is.na(...)` **eller** `== ""` **eller** `== "NULL"`, og deretter fylles med oppslag (komnrhjem2).

citeturn1search4turn1search3

### Konsekvens i mini-eksempelet
- Rad 1002 (tom): R sier «mangler» → prøver å fylle → blir fylt.
- Rad 1003 (tom tekst `""`): R sier også «mangler» → prøver å fylle → blir fylt.

Så R kan ende med **0 manglende**, selv om input viste «(Blanks)».

---

## 5) Derfor får dere ulike tall (14 vs 0)

**Kort sagt:**

- **SQL fyller bare når verdien er registrert som «mangler» i SQL** (NULL). citeturn1search1
- **R fyller også når verdien bare er tom tekst** (`""` eller blankt felt). citeturn1search4turn1search3

Så hvis de 14 radene i input (som Excel viser som "(Blanks)") egentlig er **tom tekst** i stedet for «mangler» på SQL-måten, kan SQL la dem stå tomme, mens R fyller dem.

---

## 6) Hvordan kan dere se forskjellen i Excel (uten koding)

Hvis dere vil sjekke om en «blank» celle egentlig inneholder tegn:

1. Lag en hjelpekollonne i Excel.
2. Bruk formelen:

```text
=LEN(F2)
```

- Hvis resultatet er **0**: helt tom (type A).
- Hvis resultatet er **> 0**: cellen inneholder tegn (f.eks. mellomrom) (type B).

Dette forklarer hvorfor det kan se likt ut i Excel, men gi ulikt resultat i SQL og R.

---

## 7) (Valgfritt) Hvorfor «tomt felt» kan behandles spesielt ved bulk-import

Microsoft forklarer at ved bulk-import finnes det egne valg for hvordan tomme felt skal behandles (f.eks. `KEEPNULLS`). citeturn4search11

Poenget her er bare: **importen kan skille mellom «mangler» og «tom tekst»**, og det påvirker om SQL-fiksen deres slår inn.

---

### Oppsummering i én linje
**Excel viser «(Blanks)», men R fyller både helt tomt og tom tekst, mens SQL-fyllingen deres bare slår inn når feltet er registrert som «mangler» i SQL.** citeturn1search4turn1search1turn1search3
