--------------------------------  creating temporary table for anti- Epileptic drug.from 
GP primary dataset.using event code 'dn%' or  'do%'

declare global temporary table session.AEDs (        
	alf_pe integer,
	event_dt date,
	event_cd varchar(10),
	WOB date,
	gndr_cd integer,
	age_AED integer
) with replace on commit preserve rows;  

insert into session.AEDs
SELECT * from
(SELECT DISTINCT ALF_PE, event_Dt, EVENT_CD, wob ,GNDR_CD, YEAR(EVENT_DT - WOB) AS Age_AED
FROM SAIL0895V.WLGP_GP_EVENT_ALF_CLEANSED_20180820
WHERE EVENT_CD LIKE 'dn%'
or event_cd like 'do%')
WHERE AGE_AED BETWEEN 0 AND 16;       -- only children

SELECT * FROM SESSION.AEDs
ORDER BY ALF_PE;

SELECT * FROM SAIL0895V.WLGP_GP_EVENT_ALF_CLEANSED_20180820;

----------------------------------------------- To GET Epilepsy diagnosis codes for Epilepsy
Where event_cd=¡¯F25%¡¯






declare global temporary table session.epilepsydiagcodes (        
	alf_pe integer,
	event_dt date,
	event_cd varchar(10),
	WOB date,
	gndr_cd integer,
	age_DIAG integer
) with replace on commit preserve rows;  

insert into session.epilepsydiagcodes
SELECT * FROM
(SELECT DISTINCT alf_pe, event_dt, event_cd, WOB, GNDR_CD, YEAR(EVENT_DT - WOB) AS Age_DIAG from SAIL0895V.WLGP_GP_EVENT_ALF_CLEANSED_20180820
WHERE EVENT_CD like 'F25%' 
or EVENT_CD like '1O30.%')
WHERE AGE_DIAG BETWEEN 0 AND 16;

SELECT * FROM SAIL0895V.WLGP_GP_EVENT_ALF_CLEANSED_20180820;

-----Fetching Children which are diagnosed with Epilepsy, is under medication for 12 or more months.

REATE TABLE sailw0895v.MEGNHA_NEW_EPILEPSIESOUTLOOK AS (
SELECT DISTINCT diag.*, (YEAR(diag.diag_Dt - diag.WOB)) AS DIAG_AGE FROM
(SELECT DISTINCT a.alf_pe, a.gndr_Cd, a.wob, aeddiag.DIAG_DT from
(SELECT DISTINCT a.ALF_PE, a.GNDR_CD, a.WOB from
SESSION.AEDs a -- first aed
inner JOIN 
SESSION.AEDs b -- repeat aed
ON a.alf_pe = b.alf_pe
and b.event_dt between a.event_dt + 1 DAY AND a.event_dt + 6 MONTHS
inner JOIN SESSION.epilepsydiagcodes C -- epilepsy codes
ON c.alf_pe = a.alf_pe
--they must have an ep code and aed code within 6 months, and so it won't bring back all AED dates, only those close to an ep diag
AND a.event_Dt BETWEEN c.event_Dt - 6 MONTHS AND c.event_Dt + 6 MONTHS 
WHERE a.EVENT_DT > a.WOB AND c.EVENT_DT > a.WOB) a

-- some people get put on AEDs for a while before diagnosed. Let's allow our code to account for this.
-- now we link back to diag and aed tables to get the first diag or aed code, whichever came first
INNER JOIN 
(SELECT ALF_PE, MIN(EVENT_DT) AS DIAG_DT FROM
((SELECT DISTINCT * FROM SESSION.aeds)
UNION
(SELECT DISTINCT * FROM SESSION.epilepsydiagcodes)) GROUP BY ALF_PE) aeddiag
ON aeddiag.ALF_PE = a.ALF_PE
) diag

-- it's much cleaner to use newly diagnosed people, rather than someone that moved here from a different country with epilepsy - we know how long they have it.
-- link to gp registration data to ensure before each "diag_dt", there is at least 6 months before it where there are no aeds or ep codes i.e no epilepsy
LEFT JOIN 
(SELECT * FROM SAIL0895V.WLGP_CLEAN_GP_REG_BY_PRAC_INCLNONSAIL_MEDIAN_20180820) gpreg
ON diag.alf_pe = gpreg.alf_pe
AND diag.diag_dt BETWEEN gpreg.start_date + 12 months AND gpreg.end_date  -- i.e if diag date is less than 12 months after start date, then doesn't meet our criteria
)
WITH DATA;




-----------------------------------------------Fetching variable PUPIL_ID_PE for Education details and merging with dataset with Epilepsy records, which will give us ALF_PE 

CREATE TABLE sailw0895v.PUPIL_EPI_EDU AS (SELECT a.PUPIL_ID_PE, a.ALF_PE,a.GNDR_CD,b.ALF_PE AS EPIL_ALF,b.WOB,b.DIAG_DT,b.DIAG_AGE 
FROM SAIL0895V.EDUC_PUPIL_ALF_20161123 a
INNER JOIN sailw0895v.MEGNHA_NEW_EPILEPSIESOUTLOOK b ON a.ALF_PE = b.ALF_PE)
WITH DATA;



-------   RESTRUCTURING EDUCATION DATASET WITH KEYSTAGE 4

SELECT * FROM SAIL0895V.EDUC_HIRU_KS4_20160906;

CREATE TABLE sailw0895v.MEGNHA_KStage4_new AS (SELECT PUPIL_ID_PE,YEAR AS School_YEAR,
(CASE WHEN (CSI=0) THEN 'N'
ELSE 'Y'
END ) AS CSI1
FROM SAIL0895V.EDUC_HIRU_KS4_20160906)
WITH DATA;

SELECT * FROM sailw0895v.MEGNHA_KStage4_new;



_________________Fetching Data for Key Stage 2 and 3

CREATE TABLE sailw0895v.MEGNHA_KStage23 AS
(SELECT CSI_TA AS CSI1,PUPIL_ID_PE,YEAR AS School_YEAR,KEYSTAGE FROM
SAIL0895V.EDUC_HIRU_KS123_20160906
WHERE KEYSTAGE NOT IN (1,0))
WITH DATA;


-----------Tailoring Education Dataset of key stage 2,3 and 4 


ALTER TABLE sailw0895v.MEGNHA_KStage4_new 
ADD COLUMN KEYSTAGE INT NOT NULL DEFAULT 4;

SELECT * FROM sailw0895v.MEGNHA_KStage4_new;

-------  CLEANING OF KEYSATGE 2 and 3 DATASET.

DROP TABLE sailw0895v.MEGNHA_KStage23;

CREATE TABLE sailw0895v.MEGNHA_KStage23 AS
(SELECT CSI_TA AS CSI1,PUPIL_ID_PE,YEAR AS School_YEAR,KEYSTAGE FROM
SAIL0895V.EDUC_HIRU_KS123_20160906
WHERE KEYSTAGE NOT IN (1,0))
WITH DATA;




-----COMBINING EDUCATION DATASET all together to form one dataset.

DROP TABLE sailw0895v.MEGNHA_KS_all1;

CREATE TABLE sailw0895v.MEGNHA_KS_all1 AS 
(SELECT a.CSI1,a.PUPIL_ID_PE, a.School_YEAR,a.KEYSTAGE FROM
sailw0895v.MEGNHA_KStage23 a
UNION ALL
SELECT b.CSI1,b.PUPIL_ID_PE, b.School_YEAR,b.KEYSTAGE FROM
sailw0895v.MEGNHA_KStage4_new b) 
WITH DATA ;

----COMBINING EDUCATION DATASET WITH EPILESPY DATASET.

CREATE TABLE sailw0895v.keystage_Epilespy1 AS (SELECT a.PUPIL_ID_PE, a.EPIL_ALF ,a.GNDR_CD,a.WOB,a.DIAG_DT,a.DIAG_AGE,
b.PUPIL_ID_PE AS PUPIL_ID_all,b.CSI1,b.School_YEAR,b.KEYSTAGE 
FROM sailw0895v.MEGNHA_KSDRAFT a
INNER JOIN sailw0895v.MEGNHA_KS_all1 b ON a.PUPIL_ID_PE = b.PUPIL_ID_PE)
WITH DATA;

-------------CONSTRUCTING WIMD DATASET.

CREATE TABLE sailw0895v.EDU_DEP_EPI AS (SELECT DISTINCT edu.*, wap.ALF_PE ,wap.PERS_ID_PE,woi.LSOA_CD,woi.OVERALL_INDEX_SCORE,woi.QUINTILE FROM  
sailw0895v.keystage_Epilespy1 edu INNER JOIN SAIL0895V.WDSD_AR_PERS_20190408 wap 
ON edu.EPIL_ALF= wap.ALF_PE
INNER JOIN SAIL0895V.WDSD_AR_PERS_ADD_20190408 AD
ON wap.PERS_ID_PE =AD.PERS_ID_PE INNER JOIN SAILREFRV.WIMD2005_OVERALL_INDEX woi
ON woi.LSOA_CD = AD.LSOA2011_CD
AND CONCAT (edu.School_Year,'-01-01') BETWEEN AD.FROM_DT AND AD.TO_DT)
WITH DATA;

-------------------MAKING DRUG COLLECTION CODE DATASET. USING DRUG DICTONARY

DROP TABLE sailw0895v. Drugcode_Collectioncode;
------- Creating dataset with drug name 
CREATE TABLE sailw0895v.Drugcode_Collectioncode AS (SELECT DISTINCT  EVENT_CD, 
(CASE WHEN (EVENT_CD LIKE 'dn2%')THEN 'BECLAMIDE'
WHEN (EVENT_CD LIKE 'dn3%')THEN 'ARBIL'
WHEN (EVENT_CD LIKE 'dnc%')THEN 'CLOBAZAM'
WHEN (EVENT_CD LIKE 'dn1%')THEN 'DIAZEPAM'
WHEN (EVENT_CD LIKE 'dn53%')THEN 'EMESIDE'
WHEN (EVENT_CD LIKE 'dn5%')THEN 'ETHOSUXIMIDE'
WHEN (EVENT_CD LIKE 'dn52%')THEN 'PARALDEHYDE'
WHEN (EVENT_CD LIKE 'dn9%')THEN 'PENTRAN'
WHEN (EVENT_CD LIKE 'dn1%')THEN 'ACETAZOLAMIDE'
WHEN (EVENT_CD LIKE 'dn3%')THEN 'CARBAMAZEPINE'
WHEN (EVENT_CD LIKE 'do3%')THEN 'CLOMETHIAZOLE EDISYLATE'
WHEN (EVENT_CD LIKE 'do4%')THEN 'CLOINAZEPAM'
WHEN (EVENT_CD LIKE 'dol%')THEN 'DIAZEPAM'
WHEN (EVENT_CD LIKE 'dni%')THEN 'FOSPHENYTOIN SODIUM'
WHEN (EVENT_CD LIKE 'dnj%')THEN 'GABAPENTINE'
WHEN (EVENT_CD LIKE 'dnl%')THEN 'GABITRIL'
WHEN (EVENT_CD LIKE 'dnt%')THEN 'LACOSAMIDE'
WHEN (EVENT_CD LIKE 'dnf%')THEN 'LOMOTRIGINE'
WHEN (EVENT_CD LIKE 'do4%')THEN 'LORAZEPAM'
WHEN (EVENT_CD LIKE 'dnox%' OR EVENT_CD LIKE 'dno%')THEN 'LEVETRICETAM'
WHEN (EVENT_CD LIKE 'dn6%')THEN 'METHYLPHENOBARBITONE'
WHEN (EVENT_CD LIKE 'dnm%')THEN 'OXCARBAZEPINE'
WHEN (EVENT_CD LIKE 'dn7%')THEN 'PHENOBARBITAL'
WHEN (EVENT_CD LIKE 'dn8%' OR EVENT_CD LIKE 'dn61%')THEN 'PHENYTOIN'
WHEN (EVENT_CD LIKE 'dng%')THEN 'PIRACETAM'
WHEN (EVENT_CD LIKE 'dnp%')THEN 'PREGABALINE'
WHEN (EVENT_CD LIKE 'dna%')THEN 'PRIMIDONE'
WHEN (EVENT_CD LIKE 'dnv%')THEN 'RETIGABINE'
WHEN (EVENT_CD LIKE 'dnr%')THEN 'RUFINAMIDE'
WHEN (EVENT_CD LIKE 'dnby%' OR EVENT_CD LIKE 'dnbv%' OR EVENT_CD LIKE 'dnb4%')THEN 'SODIUM VALPROATE'
WHEN (EVENT_CD LIKE 'dns%')THEN 'STIRIPENTOL'
WHEN (EVENT_CD LIKE 'dnk%')THEN 'TOPIRAMATE'
WHEN (EVENT_CD LIKE 'dnh%' OR EVENT_CD LIKE 'dnb%')THEN 'VALPROIC ACID'
WHEN (EVENT_CD LIKE 'dne%')THEN 'VIGABATRIN'
WHEN (EVENT_CD LIKE 'dnq%')THEN 'ZONISAMIDE'
WHEN (EVENT_CD LIKE 'dnu%')THEN 'ESLICARBAZEPINE ACETATE'
ELSE 'NO_DRUG'
END) AS DRUG_NAME  
FROM SESSION.AEDs)
WITH DATA;


--------------FINAL DATSET WITH DRUG INFORMATION.

CREATE TABLE sailw0895v.EPI_DRUGNAME AS (SELECT DISTINCT a.ALF_PE ,a.EVENT_CD ,
b.EVENT_CD AS Druge_code , b.DRUG_NAME FROM 
session.AEDs a INNER JOIN sailw0895v.Drugcode_Collectioncode b
ON a.EVENT_CD = b.EVENT_CD)
WITH DATA;

------------------MERGING DRUG DATASET WITH DATASET HAVING ALL THE DATA INFORMATION ABOUT EDUCATION, WIMD,EPILESPY.
CREATE TABLE sailw0895v.EPI_EDU_DEP_DRU AS (SELECT DISTINCT a.* , b.DRUG_NAME 
FROM sailw0895v.EDU_DEP_EPI a INNER JOIN sailw0895v.EPI_DRUGNAME b
ON a.ALF_PE =b.ALF_PE)
WITH DATA;

FINAL DATASET WILL ALL THE INFORMATION
CREATE TABLE Sailw0895v.EPIL_KEYSTAGE as (SELECT ALF_PE,CSI1,KEYSTAGE,SCHOOL_YEAR,QUINTILE,GNDR_CD,DRUG_NAME 
FROM sailw0895v.EPI_EDU_DEP_DRU )
WITH DATA;

------DATASET WITH KEYSATGE= 2 INFORMATION 
CREATE TABLE Sailw0895v.epi_Keysatge_2  AS (SELECT ks1.*  FROM Sailw0895v.EPIL_KEYSTAGE KS1
WHERE KEYSTAGE=2)
WITH DATA;

------DATASET WITH KEYSATGE= 3 INFORMATION 

CREATE TABLE Sailw0895v.epi_Keysatge_3  AS (SELECT ks1.*  FROM Sailw0895v.EPIL_KEYSTAGE KS1
WHERE KEYSTAGE=3)
WITH DATA;

------DATASET WITH KEYSATGE= 4 INFORMATION 
CREATE TABLE Sailw0895v.epi_Keysatge_4  AS (SELECT ks1.*  FROM Sailw0895v.EPIL_KEYSTAGE KS1
WHERE KEYSTAGE=4)
WITH DATA;

CONTROL GROUP PREPARATION.


FETCHING RECORDS OTHER THAN EPILEPSY.

--------------------------------------------------- creating Control group.
declare global temporary table session.Control (        
	alf_pe integer,
	event_dt date,
	event_cd varchar(50),
	WOB date,
	gndr_cd integer,
	age_AED integer
) with replace on commit preserve rows;  

insert into session.Control
SELECT * from
(SELECT DISTINCT ALF_PE, event_Dt, EVENT_CD, wob ,GNDR_CD, YEAR(EVENT_DT - WOB) AS Age_AED
FROM SAIL0895V.WLGP_GP_EVENT_ALF_CLEANSED_20180820
WHERE EVENT_CD  NOT LIKE 'dn%'
and EVENT_CD  NOT like 'do%')
WHERE AGE_AED BETWEEN 0 AND 16;  


----------------------------------------------------------TEMPORARY TABLE TO FETCHPUPIL_ID_PE for Education details in Control group
declare global temporary table session.Pupil(        
	alf_pe integer,
	pupil_id_pe integer
) with replace on commit preserve rows;  

insert into session.Pupil
SELECT * from
(SELECT DISTINCT ALF_PE,PUPIL_ID_PE
FROM SAIL0895V.EDUC_PUPIL_ALF_20161123)

SELECT count(DISTINCT ALF_PE) FROM sailw0895v.MEGNHA_NEW;


---------------------fetching pupil_id in control group.
CREATE TABLE sailw0895v.MEGNHA_PUL AS (SELECT DISTINCT a.*, b.PUPIL_ID_PE, b.ALF_PE AS edu_ALF
FROM sailw0895v.MEGNHA_NEW a INNER JOIN session.Pupil b ON a.ALF_PE=b.ALF_PE )
WITH DATA;

CREATE TABLE sailw0895v.Control_group AS (SELECT distinct edu.*,wind.* FROM sailw0895v.MEGNHA_PUL_edu_keystage edu INNER JOIN 
sailw0895v.EXP_data wind  ON edu.EDU_ALF= wind.WD_ALF
AND CONCAT (edu.School_Year,'-01-01') BETWEEN wind.FROM_DT AND wind.TO_DT)
WITH DATA;




-----COMBINING EDUCATION DATASET all together to form one dataset.


CREATE TABLE sailw0895v.MEGNHA_KS_all1 AS 
(SELECT a.CSI1,a.PUPIL_ID_PE, a.School_YEAR,a.KEYSTAGE FROM
sailw0895v.MEGNHA_KStage23 a
UNION ALL
SELECT b.CSI1,b.PUPIL_ID_PE, b.School_YEAR,b.KEYSTAGE FROM
sailw0895v.MEGNHA_KStage4_new b) 
WITH DATA ;


CREATE TABLE sailw0895v.MEGNHA_PUL_edu_keystage AS (SELECT a.*,b.CSI1,b.pupil_id_pe AS keystage_pul ,
b.keystage,b.school_year
FROM sailw0895v.MEGNHA_PUL a INNER JOIN sailw0895v.MEGNHA_KS_all1 b ON a.PUPIL_ID_PE=b.PUPIL_ID_PE )
WITH DATA;


------  Creating WIMD dataset for Control Group.

DROP TABLE sailw0895v.EXP_data;

CREATE TABLE sailw0895v.EXP_data AS (SELECT DISTINCT wap.ALF_PE AS WD_ALF ,wap.PERS_ID_PE,
woi.LSOA_CD,woi.OVERALL_INDEX_SCORE,woi.QUINTILE,AD.FROM_DT,AD.TO_DT FROM  
session.DEV_WIND wap  INNER JOIN session.ADD_PER AD
ON wap.PERS_ID_PE = AD.PERS_ID_PE INNER JOIN session.DEV_INDEX woi
ON woi.LSOA_CD = AD.LSOA2011_CD)
WITH DATA;



MERGING ALL THE DATASET TO FORM ONE DATASET.TO FOR CONTROL GROUP.

CREATE TABLE sailw0895v.Control_group AS (SELECT distinct edu.*,wind.* FROM sailw0895v.MEGNHA_PUL_edu_keystage edu INNER JOIN 
sailw0895v.EXP_data wind  ON edu.EDU_ALF= wind.WD_ALF
AND CONCAT (edu.School_Year,'-01-01') BETWEEN wind.FROM_DT AND wind.TO_DT)
WITH DATA;

CASE CONTROL GROUP FORMED.

CREATE TABLE Sailw0895v.combined_group AS  (SELECT epi.* FROM Sailw0895v.Epil_group epi
UNION all
SELECT cntrl. * FROM Sailw0895v.Control_grp cntrl )
WITH DATA;

COMBINED DATASET FOR KEYSTAGE 2.
CREATE TABLE Sailw0895v.control_case_Key2 AS (SELECT con.* FROM Sailw0895v.combined_group con
WHERE KEYSTAGE=2)
WITH DATA;

CREATE TABLE Sailw0895v.control_case_Key3 AS (SELECT con.* FROM Sailw0895v.combined_group con
WHERE KEYSTAGE=3)
WITH DATA;

CREATE TABLE Sailw0895v.control_case_Key4 AS (SELECT con.* FROM Sailw0895v.combined_group con
WHERE KEYSTAGE=4)
WITH DATA;

