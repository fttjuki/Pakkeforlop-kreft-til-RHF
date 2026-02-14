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
3;    ""
4; 
```

**Viktig:** Excel viser ofte både A og B som «blankt», men datasystemer kan behandle dem forskjellig.

---

## 2) Mini-eksempel: samme CSV, men ulike «tom»-typer

La oss si at input-filen har fire rader:

```text
NPRId;KommuneNr
1;    0301
2;
3;    ""
4; 
```

- Rad 2: *helt tomt felt* (type A)
- Rad 3: *tom tekstverdi* (type B: `""`)
- Rad 4: *mellomrom* (type B: `" "`)

---

## 3) Hva SQL-skriptet  gjør

SQL-skriptet har en «fyll inn manglende kommune»-regel som i praksis sier:

> **Bare hvis KommuneNr er registrert som «mangler» i SQL**, så prøver jeg å hente kommune fra en annen tabell (SOMHoved).

Det ser vi i SQL-koden:
- Det sjekkes etter mangler med `KommuneNr IS NULL`.
- Og det fylles bare når `KommuneNr IS NULL`.



### Konsekvens i mini-eksempelet
- Hvis SQL tolker feltet som **«mangler» (NULL)**, da blir det fylt.
- Men hvis feltet blir liggende som **tom tekst** ("" eller mellomrom), da **oppfatter ikke SQL det som «mangler»**, og da blir det **ikke fylt**.

Derfor kan noen rader bli stående uten KommuneNr i SQL-resultatet.

---

## 4) Hva R-skriptet  gjør 

R  regner disse som «mangler»:
- ekte mangler (`NA`)
- **tomt felt** (`""`)
- teksten `"NULL"`

Dette står i R-koden der «manglende kommune» defineres som `is.na(...)` **eller** `== ""` **eller** `== "NULL"`, og deretter fylles med oppslag (komnrhjem2).


---



**Kort sagt:**

- **SQL fyller bare når verdien er registrert som «mangler» i SQL** (NULL). 
- **R fyller også når verdien bare er tom tekst** (`""` eller blankt felt). 


---

