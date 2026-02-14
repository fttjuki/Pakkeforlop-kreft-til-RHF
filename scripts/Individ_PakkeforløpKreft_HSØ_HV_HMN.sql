/*
SAK:			Pakkeforløp kreft
SAKSNR:			20/40426 Rutinemessig overføring til RHF-ene
SØKER:			RHF (HSØ og HV)
SAKSBEHANDLER:	Michael Ogbe (21.05.24)
*/

--______________________________________________
/*
		Data for desember 2025.																				--må endres hver måned (og år)
*/
--______________________________________________
-------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------

--01 Les inn rapporten for HSØ
DROP TABLE IF EXISTS #RapportHSØ;

CREATE TABLE #RapportHSØ
			(
			institusjonId	VARCHAR(MAX),
			institusjon		VARCHAR(MAX),
			reshIdFagEnhet	VARCHAR(MAX),
			Fagenhet		VARCHAR(MAX),
			NPRId			VARCHAR(MAX),
			KommuneNr		VARCHAR(MAX),		--lagt til 16.07.25
			BydelNr			VARCHAR(MAX),		--lagt til 16.07.25
			FødselsDato		VARCHAR(MAX),		--lagt til 16.07.25
			DødsDato		VARCHAR(MAX),		--lagt til 16.07.25
			pfInstansId		VARCHAR(MAX),
			Forløp			VARCHAR(MAX),
			Pakkeforløp		VARCHAR(MAX),
			AntInst			VARCHAR(MAX),
			AntHf			VARCHAR(MAX),
			År				VARCHAR(MAX),
			StartDato		VARCHAR(MAX),
			Ainst			VARCHAR(MAX),
			UtrStartDato	VARCHAR(MAX),
			Sinst			VARCHAR(MAX),
			KliniskBeslutn	VARCHAR(MAX),
			TypeBeslutn		VARCHAR(MAX),
			Cinst			VARCHAR(MAX),
			StartBeh		VARCHAR(MAX),
			TypeBeh			VARCHAR(MAX),
			Finst			VARCHAR(MAX),
			Avslutn			VARCHAR(MAX),
			Xinst			VARCHAR(MAX),
			OF1				VARCHAR(MAX),
			OF1std			VARCHAR(MAX),
			OF2				VARCHAR(MAX),
			OF2std			VARCHAR(MAX),
			OF3				VARCHAR(MAX),
			OF3std			VARCHAR(MAX),
			OF4				VARCHAR(MAX),
			OF4std			VARCHAR(MAX),
			Overf			VARCHAR(MAX),
			AnsiennDato		VARCHAR(MAX),
			MottaksDato		VARCHAR(MAX)				
			);

BULK INSERT #RapportHSØ FROM '\\p-trd1ahrsvm01\NPR$\Utleveringer\STYRINGSDATA\RHF Pakkeforløp Kreft\Uttrekk\HSØ\Forlopstider per pasient per organspesifikke pakkeforlop per RHF_HSØ.csv'
WITH (FIELDTERMINATOR = ';', ROWTERMINATOR = '\n', CODEPAGE = 'ACP', FIRSTROW = 2);

--SELECT COUNT(*) FROM #RapportHSØ WHERE NPRId IS NOT NULL;												--79 734

--02 Les inn rapporten for HV
DROP TABLE IF EXISTS #RapportHV;

CREATE TABLE #RapportHV
			(
			institusjonId	VARCHAR(MAX),
			institusjon		VARCHAR(MAX),
			reshIdFagEnhet	VARCHAR(MAX),
			Fagenhet		VARCHAR(MAX),
			NPRId			VARCHAR(MAX),
			KommuneNr		VARCHAR(MAX),		--lagt til 16.07.25
			BydelNr			VARCHAR(MAX),		--lagt til 16.07.25
			FødselsDato		VARCHAR(MAX),		--lagt til 16.07.25
			DødsDato		VARCHAR(MAX),		--lagt til 16.07.25
			pfInstansId		VARCHAR(MAX),
			Forløp			VARCHAR(MAX),
			Pakkeforløp		VARCHAR(MAX),
			AntInst			VARCHAR(MAX),
			AntHf			VARCHAR(MAX),
			År				VARCHAR(MAX),
			StartDato		VARCHAR(MAX),
			Ainst			VARCHAR(MAX),
			UtrStartDato	VARCHAR(MAX),
			Sinst			VARCHAR(MAX),
			KliniskBeslutn	VARCHAR(MAX),
			TypeBeslutn		VARCHAR(MAX),
			Cinst			VARCHAR(MAX),
			StartBeh		VARCHAR(MAX),
			TypeBeh			VARCHAR(MAX),
			Finst			VARCHAR(MAX),
			Avslutn			VARCHAR(MAX),
			Xinst			VARCHAR(MAX),
			OF1				VARCHAR(MAX),
			OF1std			VARCHAR(MAX),
			OF2				VARCHAR(MAX),
			OF2std			VARCHAR(MAX),
			OF3				VARCHAR(MAX),
			OF3std			VARCHAR(MAX),
			OF4				VARCHAR(MAX),
			OF4std			VARCHAR(MAX),
			Overf			VARCHAR(MAX),
			AnsiennDato		VARCHAR(MAX),
			MottaksDato		VARCHAR(MAX)
			);

BULK INSERT #RapportHV FROM '\\p-trd1ahrsvm01\NPR$\Utleveringer\STYRINGSDATA\RHF Pakkeforløp Kreft\Uttrekk\HV\Forlopstider per pasient per organspesifikke pakkeforlop per RHF_HV.csv'
WITH (FIELDTERMINATOR = ';', ROWTERMINATOR = '\n', CODEPAGE = 'ACP', FIRSTROW = 2);

--SELECT COUNT(*) FROM #RapportHV WHERE NPRId IS NOT NULL;												--27 521


--03 Les inn rapporten for HMN (lagt til 14.11.2024)
DROP TABLE IF EXISTS #RapportHMN;

CREATE TABLE #RapportHMN
			(
			institusjonId	VARCHAR(MAX),
			institusjon		VARCHAR(MAX),
			reshIdFagEnhet	VARCHAR(MAX),
			Fagenhet		VARCHAR(MAX),
			NPRId			VARCHAR(MAX),
			KommuneNr		VARCHAR(MAX),		--lagt til 16.07.25
			BydelNr			VARCHAR(MAX),		--lagt til 16.07.25
			FødselsDato		VARCHAR(MAX),		--lagt til 16.07.25
			DødsDato		VARCHAR(MAX),		--lagt til 16.07.25
			pfInstansId		VARCHAR(MAX),
			Forløp			VARCHAR(MAX),
			Pakkeforløp		VARCHAR(MAX),
			AntInst			VARCHAR(MAX),
			AntHf			VARCHAR(MAX),
			År				VARCHAR(MAX),
			StartDato		VARCHAR(MAX),
			Ainst			VARCHAR(MAX),
			UtrStartDato	VARCHAR(MAX),
			Sinst			VARCHAR(MAX),
			KliniskBeslutn	VARCHAR(MAX),
			TypeBeslutn		VARCHAR(MAX),
			Cinst			VARCHAR(MAX),
			StartBeh		VARCHAR(MAX),
			TypeBeh			VARCHAR(MAX),
			Finst			VARCHAR(MAX),
			Avslutn			VARCHAR(MAX),
			Xinst			VARCHAR(MAX),
			OF1				VARCHAR(MAX),
			OF1std			VARCHAR(MAX),
			OF2				VARCHAR(MAX),
			OF2std			VARCHAR(MAX),
			OF3				VARCHAR(MAX),
			OF3std			VARCHAR(MAX),
			OF4				VARCHAR(MAX),
			OF4std			VARCHAR(MAX),
			Overf			VARCHAR(MAX),
			AnsiennDato		VARCHAR(MAX),
			MottaksDato		VARCHAR(MAX)
			);

BULK INSERT #RapportHMN FROM '\\p-trd1ahrsvm01\NPR$\Utleveringer\STYRINGSDATA\RHF Pakkeforløp Kreft\Uttrekk\HMN\Forlopstider per pasient per organspesifikke pakkeforlop per RHF_HMN.csv'
WITH (FIELDTERMINATOR = ';', ROWTERMINATOR = '\n', CODEPAGE = 'ACP', FIRSTROW = 2);

--SELECT COUNT(*) FROM #RapportHMN WHERE NPRId IS NOT NULL;											--14 569


--04 Velger alle distinkte NPRIder
DROP TABLE IF EXISTS #NPRId;

SELECT	DISTINCT NPRId, 
		'' AS Dummy
INTO #NPRId
FROM (
	SELECT DISTINCT 
			NPRId 
	FROM #RapportHSØ																				--HSØ

	UNION ALL

	SELECT DISTINCT 
			NPRId 
	FROM #RapportHV																					--HV

	UNION ALL

	SELECT DISTINCT 
			NPRId 
	FROM #RapportHMN																				--HMN	
	) AS a;

SELECT * FROM #NPRId WHERE NPRId IS NOT NULL;														--111 372 (Lagre som "NPRId_RHF_Pakkeforløp_des25" i Uttrekksmappa)

--05 Bestill løpenr i Fihr (Bruk samme info som RHF månedlige kjøringen)
/*
SaksNr		= 20/40426
VedtaksNr	= 1
*/

--06 Henter inn csv med løpenr
DROP TABLE IF EXISTS #NPRId_desember_2025;															--måneden må endres hver måned

CREATE TABLE #NPRId_desember_2025
			(
			NPRId VARCHAR(MAX),
			lopenr VARCHAR(MAX)
			);

BULK INSERT #NPRId_desember_2025 FROM '\\fihr.no\dfs\NPR\Temp\NPR_RegistrerUtlevering\NPRId_RHF_Pakkeforløp_des25_lnr.csv'
WITH (FIELDTERMINATOR = ';', ROWTERMINATOR = '\n', FIRSTROW = 1);									--111 372

--Sjekker
SELECT * FROM #NPRId_desember_2025 ORDER BY NPRId;
-------------------------------------------------------------------------------------------------------------------

--Sjekker manglende kommuneNr
SELECT * FROM #RapportHSØ	WHERE NPRId != 'NULL' AND KommuneNr IS NULL;		--200
SELECT * FROM #RapportHV	WHERE NPRId != 'NULL' AND KommuneNr IS NULL;		--40
SELECT * FROM #RapportHMN 	WHERE NPRId != 'NULL' AND KommuneNr IS NULL;		--20


--07 Fikser manglende kommuneNr og BydelNr
USE NPRNasjonaltDatagrunnlag;


--HSØ_Kom
DROP TABLE IF EXISTS #HSØ_Kom;

SELECT DISTINCT NPRId
INTO #HSØ_Kom
FROM #RapportHSØ 
WHERE	NPRId IS NOT NULL
		AND NPRId != 'NPRId'
		AND KommuneNr IS NULL;						--188

--Sjekker
SELECT * 
FROM #HSØ_Kom
ORDER BY NPRId;										--188

--Kobler #RapportHSØ mot SOMHoved for å hente KommuneNr og BydelNr
DROP TABLE IF EXISTS #Fiks_HSØ_Kom;

WITH Kom AS 
(
SELECT	NPRId, 
		komnrhjem2, 
		bydel2,
		aar,
		ROW_NUMBER() OVER (PARTITION BY NPRId ORDER BY aar DESC) AS radNr
FROM SOMHoved 
)
SELECT DISTINCT 
	a.NPRId,
	komnrhjem2, 
	bydel2,
	aar
INTO #Fiks_HSØ_Kom
FROM #HSØ_Kom AS a	
JOIN Kom AS b ON a.NPRId = b.NPRId
WHERE b.radNr = 1;									--174

--Sjekker
SELECT * 
FROM #Fiks_HSØ_Kom 
WHERE komnrhjem2 IS NULL
ORDER BY NPRId;										--1
/*
NPRId		komnrhjem2	bydel2	aar
1062924		NULL		NULL	2008
2561864		NULL		NULL	2008
3200706		NULL		NULL	2008
*/

--Sjekker mot SOMHoved
SELECT komnrhjem, komnrhjem2, bydel, bydel2
FROM SOMHoved
WHERE NPRid = '3200706' AND aar = 2008;				--mangler komnrhjem2, så bruker komnrhjem i stedet for og komnrhjem = 0220

--Legger til 1062924
UPDATE #Fiks_HSØ_Kom
SET komnrhjem2 = '0220'
WHERE NPRId = '1062924' AND aar = 2008;				--0

--Legger til 2561864
UPDATE #Fiks_HSØ_Kom
SET komnrhjem2 = '0706'
WHERE NPRId = '2561864' AND aar = 2008;				--0

--Legger til 3200706
UPDATE #Fiks_HSØ_Kom
SET komnrhjem2 = '0906'
WHERE NPRId = '3200706' AND aar = 2008;				--1

--Sjekker
SELECT * 
FROM #Fiks_HSØ_Kom 
WHERE komnrhjem2 IS NULL
ORDER BY NPRId;										--0 (OK!)



--HV_Kom
DROP TABLE IF EXISTS #HV_Kom;

SELECT DISTINCT NPRId
INTO #HV_Kom
FROM #RapportHV 
WHERE	NPRId IS NOT NULL
		AND NPRId != 'NPRId'
		AND KommuneNr IS NULL;						--38

--Sjekker
SELECT * 
FROM #HV_Kom
ORDER BY NPRId;										--38

--Kobler mot #RapportHSØ for å hente KommuneNr og BydelNr
DROP TABLE IF EXISTS #Fiks_HV_Kom;

WITH Kom AS 
(
SELECT	NPRId, 
		komnrhjem2, 
		bydel2,
		aar,
		ROW_NUMBER() OVER (PARTITION BY NPRId ORDER BY aar DESC) AS radNr
FROM SOMHoved 
)
SELECT DISTINCT 
	a.NPRId,
	komnrhjem2, 
	bydel2,
	aar
INTO #Fiks_HV_Kom
FROM #HV_Kom AS a	
JOIN Kom AS b ON a.NPRId = b.NPRId
WHERE b.radNr = 1;									--36

--Sjekker
SELECT * 
FROM #Fiks_HV_Kom 
WHERE komnrhjem2 IS NULL
ORDER BY NPRId;										--1 
/*
NPRId		komnrhjem2	bydel2	aar
3602577		NULL		NULL	2008
*/

--Sjekker mot SOMHoved
SELECT komnrhjem, komnrhjem2, bydel, bydel2
FROM SOMHoved
WHERE NPRid = '3602577' AND aar = 2008;				--mangler komnrhjem2, så bruker komnrhjem i stedet for og komnrhjem = 0220

--Legger til 3602577
UPDATE #Fiks_HV_Kom
SET komnrhjem2 = '1135'
WHERE NPRId = '3602577' AND aar = 2008;				--1
--Sjekker
SELECT * 
FROM #Fiks_HV_Kom 
WHERE komnrhjem2 IS NULL
ORDER BY NPRId;										--OK

--HMN_Kom
DROP TABLE IF EXISTS #HMN_Kom;

SELECT DISTINCT NPRId
INTO #HMN_Kom
FROM #RapportHMN 
WHERE	NPRId IS NOT NULL
		AND NPRId != 'NPRId'
		AND KommuneNr IS NULL;						--18

--Sjekker
SELECT * 
FROM #HMN_Kom
ORDER BY NPRId;										--18

--Kobler mot #RapportHMN for å hente KommuneNr og BydelNr
DROP TABLE IF EXISTS #Fiks_HMN_Kom;

WITH Kom AS 
(
SELECT	NPRId, 
		komnrhjem2, 
		bydel2,
		aar,
		ROW_NUMBER() OVER (PARTITION BY NPRId ORDER BY aar DESC) AS radNr
FROM SOMHoved 
)
SELECT DISTINCT 
	a.NPRId,
	komnrhjem2, 
	bydel2,
	aar
INTO #Fiks_HMN_Kom
FROM #HMN_Kom AS a	
JOIN Kom AS b ON a.NPRId = b.NPRId
WHERE b.radNr = 1;									--18

--Sjekker
SELECT * 
FROM #Fiks_HMN_Kom 
WHERE komnrhjem2 IS NULL
ORDER BY NPRId;										--0 (OK!)


-------------------------------------------------------------------------------------------------------------------
--08 Utlevering HSØ
DROP TABLE IF EXISTS #HSØ;

SELECT	RTRIM(REPLACE(b.lopenr,';','')) AS lopenr,
		a.institusjonId,	
		institusjon,	
		reshIdFagEnhet,	
		Fagenhet,
		CASE 
			WHEN KommuneNr IS NULL THEN c.komnrhjem2
			ELSE KommuneNr
		END KommuneNr,	
		CASE 
			WHEN KommuneNr = c.komnrhjem2 AND BydelNr IS NULL THEN c.bydel2
			ELSE BydelNr
		END	BydelNr,	
		FødselsDato,
		DødsDato,
		pfInstansId,		
		Forløp,			
		Pakkeforløp,		
		AntInst,			
		AntHf,			
		År,				
		StartDato,		
		Ainst,			
		UtrStartDato,	
		Sinst,			
		KliniskBeslutn,	
		TypeBeslutn,		
		Cinst,			
		StartBeh,		
		TypeBeh,			
		Finst,			
		Avslutn,			
		Xinst,			
		OF1,				
		OF1std,			
		OF2,				
		OF2std,			
		OF3,				
		OF3std,			
		OF4,				
		OF4std,			
		Overf,			
		AnsiennDato
INTO #HSØ
FROM #RapportHSØ AS a
LEFT JOIN #NPRId_desember_2025 AS b ON a.NPRId = b.NPRId
LEFT JOIN #Fiks_HSØ_Kom AS c ON a.NPRId = c.NPRId
WHERE b.lopenr != 'NULL'
		AND a.NPRId != 'NPRId';									--79 731

----Sjekker
SELECT MONTH(StartDato) AS måned, count(*) AS AntallRader
FROM #HSØ
WHERE År = 2025
GROUP BY MONTH(StartDato)
ORDER BY MONTH(StartDato);

--Utleveres
SELECT * FROM #HSØ;												--Lagre i arbeidsmappa 


--09 Utlevering HV
DROP TABLE IF EXISTS #HV;

SELECT	RTRIM(REPLACE(b.lopenr,';','')) AS lopenr,
		a.institusjonId,	
		institusjon,	
		reshIdFagEnhet,	
		Fagenhet,	
		CASE 
			WHEN KommuneNr IS NULL THEN c.komnrhjem2
			ELSE KommuneNr
		END KommuneNr,	
		CASE 
			WHEN KommuneNr = c.komnrhjem2 AND BydelNr IS NULL THEN c.bydel2
			ELSE BydelNr
		END	BydelNr,		
		FødselsDato,
		DødsDato,
		pfInstansId,		
		Forløp,			
		Pakkeforløp,		
		AntInst,			
		AntHf,			
		År,				
		StartDato,		
		Ainst,			
		UtrStartDato,	
		Sinst,			
		KliniskBeslutn,	
		TypeBeslutn,		
		Cinst,			
		StartBeh,		
		TypeBeh,			
		Finst,			
		Avslutn,			
		Xinst,			
		OF1,				
		OF1std,			
		OF2,				
		OF2std,			
		OF3,				
		OF3std,			
		OF4,				
		OF4std,			
		Overf,			
		AnsiennDato
INTO #HV
FROM #RapportHV AS a
LEFT JOIN #NPRId_desember_2025 AS b ON a.NPRId = b.NPRId
LEFT JOIN #Fiks_HV_Kom AS c ON a.NPRId = c.NPRId
WHERE b.lopenr != 'NULL'
		AND a.NPRId != 'NPRId';									--27 370

----Sjekker
SELECT MONTH(StartDato) AS måned, count(*) AS AntallRader
FROM #HV
WHERE År = 2025
GROUP BY MONTH(StartDato)
ORDER BY MONTH(StartDato);

--Haraldsplass
SELECT DISTINCT institusjon, MONTH(StartDato) AS mnd, COUNT(lopenr) AS Antall
FROM #HV
WHERE År = 2025
GROUP BY institusjon, MONTH(StartDato)
ORDER BY institusjon, MONTH(StartDato);							--Ser bra ut!

--Utleveres
SELECT * FROM #HV;												--Lagre i arbeidsmappa 


--09 Utlevering HMN
DROP TABLE IF EXISTS #HMN;

SELECT	RTRIM(REPLACE(b.lopenr,';','')) AS lopenr,
		a.institusjonId,	
		institusjon,	
		reshIdFagEnhet,	
		Fagenhet,
		CASE 
			WHEN KommuneNr IS NULL THEN c.komnrhjem2
			ELSE KommuneNr
		END KommuneNr,	
		CASE 
			WHEN KommuneNr = c.komnrhjem2 AND BydelNr IS NULL THEN c.bydel2
			ELSE BydelNr
		END	BydelNr,	
		FødselsDato,
		DødsDato,
		pfInstansId,		
		Forløp,			
		Pakkeforløp,		
		AntInst,			
		AntHf,			
		År,				
		StartDato,		
		Ainst,			
		UtrStartDato,	
		Sinst,			
		KliniskBeslutn,	
		TypeBeslutn,		
		Cinst,			
		StartBeh,		
		TypeBeh,			
		Finst,			
		Avslutn,			
		Xinst,			
		OF1,				
		OF1std,			
		OF2,				
		OF2std,			
		OF3,				
		OF3std,			
		OF4,				
		OF4std,			
		Overf,			
		AnsiennDato
INTO #HMN
FROM #RapportHMN AS a
LEFT JOIN #NPRId_desember_2025 AS b ON a.NPRId = b.NPRId
LEFT JOIN #Fiks_HMN_Kom AS c ON a.NPRId = c.NPRId
WHERE b.lopenr != 'NULL'
		AND a.NPRId != 'NPRId';									--14 108

----Sjekker
SELECT MONTH(StartDato) AS måned, count(*) AS AntallRader
FROM #HMN
WHERE År = 2025
GROUP BY MONTH(StartDato)
ORDER BY MONTH(StartDato);

--Utleveres
SELECT * FROM #HMN;												--Lagre i arbeidsmappa 

-------------------------------------------------------------------------- SLUTT ------------------------------------------------------------------------------------------------------