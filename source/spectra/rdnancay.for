C++*********************************************************************
C RDNANCAY
C To read HI data from Nancay. 
C Read files R0xxxx_174.SIR as created by PUTF (SIR)
C and write the output in R0xxxx.PROF
C Use SIR/PUTF first to
C extract records from RESULTAT.STD.
C (Written for data from Nov 1989 to Sept 1990)
C
C From "SIR/GETFILE", Dominique Aubry (Version 02-04-90)
C
C JLP
C Version 24-09-90
C--*********************************************************************
	PROGRAM RDNANCAY
	INTEGER*4 LUNGET,LUNPUT,IER,IFIRST,ILAST
	CHARACTER*40 FILGET,FILPUT
10	FORMAT(A)
 
	WRITE(6,22)
22	FORMAT(' Program RDNANCAY Version 24-09-90',/,
     1	'  Read files R0xxxx_174.SIR as created by PUTF (SIR)',
     1	' and write the output in R0xxxx.PROF',/,
     1	' Example: R05969_174.SIR --> R05969.PROF',/,/,
     1	'   Enter first and last sequence number?',
     1	' (ex:  5969,6062)')
	READ(5,*) IFIRST,ILAST
 
	LUNGET=1
	LUNPUT=2
	
C Main loop on the file names:
	DO I=IFIRST,ILAST
 
C Reading input file:
	  WRITE(FILGET,12) I
12	  FORMAT('R0',I4,'_174.SIR')
	  WRITE(6,26) FILGET
26	  FORMAT(' RDNANCAY/ Processing file: ',A)
	  CALL GETRES(LUNGET,FILGET,IER)
 
C Creating the output (*.PROF), when input sucessfully written:
	  IF(IER.EQ.0)THEN
	    WRITE(FILPUT,13) I
13	    FORMAT('R0',I4,'.PROF')
	    CALL WRASCII(LUNPUT,FILPUT,IER)
	  ELSE
            WRITE(6,34) FILGET
34	    FORMAT(' RDNANCAY/Error reading input file: ',A,/,
     1	' ***      Giving up with this file')
	  ENDIF
	
	END DO
 
	STOP
	END
C
C*******************************************************************
C	SUBROUTINE GETRES(LUNGET,FILGET,IER)
C From "SIR", Dominique Aubry
C Version 02-04-90
C
C POUR OUVRIR ET LIRE UN FICHIER DE NOM 'FILGET' SUR L'UNITE LOGIQUE 'LUNGET'
C CE FICHIER EST EN BINAIRE ET DOIT AVOIR ETE CREE PAR LA FONCTION "PUTF" DE
C 'SIR' OU TOUT AUTRE PROGRAMME EQUIVALENT
C LA LECTURE DE CE FICHIER REMPLI LE COMMON 'COMRESX'
C	ARGUMENTS:
C		'LUNGET' NO UNITE LOGIQUE DU FICHIER (I*4)
C		'FILGET' NOM DU FICHIER (CHARACTER)
C		'IER'    CODE DE RETOUR: 0=OK    (I*4)
C
C
	SUBROUTINE GETRES(LUNGET,FILGET,IER)
C@	INCLUDE 'COMRESX.CMN'
 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C		EQUIVALENCE SUR LE TABLEAU "BUFRESX" REPRESENTANT
C               LES 10 BLOCS D'UN RESULTAT (2560 INTEGER*2)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	LOGICAL*1 LIDERE(32),LNOMSO(32),LTITMA(32)
	LOGICAL*1 LTYVEC(8),LKRVEC(8),LKVVEC(8),LPOVEC(8)
	INTEGER*2 BUFRESX(2560),LITSYM(26)
	INTEGER *2 NUMERE,IUICRE,NUMSCA,IANREF,NBRVEC
	INTEGER *2 NVFREE,IAHDRE,IFRJTU,ITYPOR,ITYSWI
	INTEGER*2 ITBVEC(8),ITSVEC(8),IDBVEC(8),NPTVEC(8)
	INTEGER *2 IDAPOU
	INTEGER*2 ITAUSO(8),NBRCYC(8),MASQUE(65)
	INTEGER*4 JDATRE,JDATMA
	REAL*4 VIVVEC(8),RESVEC(8),VITVEC(8),POIVEC(8),RUB31(8)
	REAL*4 RUB32(8),RUB33(8),RUB34(8),RUB35(8),RUB36(8)
	REAL*4 VALRES(1024),SYMBOR(26)
	REAL*8 FREVEC(8),FRELAB(8)
C
	EQUIVALENCE (BUFRESX(1),LIDERE(1))	!identification du resultat
	EQUIVALENCE (BUFRESX(17),LNOMSO(1))	!nom de la source
	EQUIVALENCE (BUFRESX(33),LTITMA(1))	!titre du map
	EQUIVALENCE (BUFRESX(49),NUMERE)	!numero du resultat
	EQUIVALENCE (BUFRESX(50),IUICRE)	!UIC du resultat
	EQUIVALENCE (BUFRESX(51),JDATRE)	!date du resultat
	EQUIVALENCE (BUFRESX(53),JDATMA)	!date de la manip
	EQUIVALENCE (BUFRESX(55),NUMSCA)	!numero de scan
	EQUIVALENCE (BUFRESX(56),IANREF)	!annee de ref. des coordonnees
	EQUIVALENCE (BUFRESX(57),ASDREF)	!asc. droite non reduite
	EQUIVALENCE (BUFRESX(59),DECREF)	!declinaison non reduite
	EQUIVALENCE (BUFRESX(61),NBRVEC)	!nombre de vecteurs
	EQUIVALENCE (BUFRESX(62),NVFREE)	!nbre de points libres de VALRES
	EQUIVALENCE (BUFRESX(63),IAHDRE)	!angle horaire depart
	EQUIVALENCE (BUFRESX(64),IFRJTU)	!TU passage au meridien
						!En 10/1000 jour
	EQUIVALENCE (BUFRESX(65),LITSYM(1))	!no des symboles de SIR sauves
	EQUIVALENCE (BUFRESX(91),SYMBOR(1))	!valeurs de ces symboles
	EQUIVALENCE (BUFRESX(143),ITYPOR)	!type de la poursuite
						!ITYPOU -> ITYPOR (conflit
						!avec ITYPOU CONMAP)
	EQUIVALENCE (BUFRESX(144),ITYSWI)	!type de 'switch' PO ou FR
	EQUIVALENCE (BUFRESX(145),IDAPOU)	!'DA' premiere phase en 1/10 sec
C
C   les elements suivants sont indices par le numero de vecteur (1-8)
C
	EQUIVALENCE (BUFRESX(151),VIVVEC(1))	!vitesse visee du vecteur(PAN)
	EQUIVALENCE (BUFRESX(167),IDBVEC(1))	!indice deb. du vec. dans VALRES
	EQUIVALENCE (BUFRESX(175),NPTVEC(1))	!nbre de points du vecteur
	EQUIVALENCE (BUFRESX(183),LTYVEC(1))	!type du vecteur
	EQUIVALENCE (BUFRESX(187),LKRVEC(1))	!code de la resolution
	EQUIVALENCE (BUFRESX(191),LKVVEC(1))	!code vitesse (PAN)
	EQUIVALENCE (BUFRESX(195),LPOVEC(1))	!code polar du vecteur
	EQUIVALENCE (BUFRESX(199),RESVEC(1))	!resolution du vecteur
	EQUIVALENCE (BUFRESX(215),VITVEC(1))	!abscisse du premier point
	EQUIVALENCE (BUFRESX(231),POIVEC(1))	!poids du vecteur
	EQUIVALENCE (BUFRESX(247),ITBVEC(1))	!temperature du TAB
	EQUIVALENCE (BUFRESX(255),ITSVEC(1))	!temperature de systeme
	EQUIVALENCE (BUFRESX(263),FREVEC(1))	!frequence ciel au repos
	EQUIVALENCE (BUFRESX(295),FRELAB(1))	!frequence labo depart manip
	EQUIVALENCE (BUFRESX(327),ITAUSO(1))	!cste d'integration
	EQUIVALENCE (BUFRESX(335),NBRCYC(1))	!nbre de cycles
	EQUIVALENCE (BUFRESX(343),RUB31(1))	!rubrique 31 de SIR
	EQUIVALENCE (BUFRESX(359),RUB32(1))	!........ 32 ......
	EQUIVALENCE (BUFRESX(375),RUB33(1))	!........ 33 ......
	EQUIVALENCE (BUFRESX(391),RUB34(1))	!........ 34 ......
	EQUIVALENCE (BUFRESX(407),RUB35(1))	!........ 35 ......
	EQUIVALENCE (BUFRESX(423),RUB36(1))	!........ 36 ......
C
	EQUIVALENCE (BUFRESX(448),MASQUE(1))	!bits du masque dans SIR
C
	EQUIVALENCE (BUFRESX(513),VALRES(1))	!valeurs des vecteurs
C
	COMMON /COMRESX/BUFRESX
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 
	INTEGER*4 LUNGET,IER
	CHARACTER*(*) FILGET
C
	IER=0
	OPEN(UNIT=LUNGET,FILE=FILGET,FORM='UNFORMATTED',
     1	STATUS='OLD',RECL=1280,ERR=990,RECORDTYPE='FIXED')
	READ(LUNGET,ERR=992)BUFRESX
	CLOSE(UNIT=LUNGET)
	RETURN
C
990	TYPE 991,FILGET
991	FORMAT(' *** RDNANCAY/GETRES  Error opening file: ',A)
	IER=1
	RETURN
992	TYPE 993,FILGET
993	FORMAT(' *** RDNANCAY/GETRES  Error reading file: ',A)
	IER=2
	CLOSE(UNIT=LUNGET)
	RETURN
	END
C************************************************************************
C WRASCII
C To write ASCII files with some lines of header:
C************************************************************************
	SUBROUTINE WRASCII(LUNPUT,FILPUT,IER)
	INTEGER*4 KCENT
	REAL*4 LIGHTVELO,VCENT,VELO1
	INTEGER*4 LUNPUT,IER
	CHARACTER*(*) FILPUT
	CHARACTER CLIDERE*32,CLNOMSO*32,CLTITMA*32
	CHARACTER CITYPOR*2,CITYSWI*2
C@	INCLUDE 'COMRESX.CMN'
 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C		EQUIVALENCE SUR LE TABLEAU "BUFRESX" REPRESENTANT
C               LES 10 BLOCS D'UN RESULTAT (2560 INTEGER*2)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	LOGICAL*1 LIDERE(32),LNOMSO(32),LTITMA(32)
	LOGICAL*1 LTYVEC(8),LKRVEC(8),LKVVEC(8),LPOVEC(8)
	INTEGER*2 BUFRESX(2560),LITSYM(26)
	INTEGER *2 NUMERE,IUICRE,NUMSCA,IANREF,NBRVEC
	INTEGER *2 NVFREE,IAHDRE,IFRJTU,ITYPOR,ITYSWI
	INTEGER*2 ITBVEC(8),ITSVEC(8),IDBVEC(8),NPTVEC(8)
	INTEGER *2 IDAPOU
	INTEGER*2 ITAUSO(8),NBRCYC(8),MASQUE(65)
	INTEGER*4 JDATRE,JDATMA
	REAL*4 VIVVEC(8),RESVEC(8),VITVEC(8),POIVEC(8),RUB31(8)
	REAL*4 RUB32(8),RUB33(8),RUB34(8),RUB35(8),RUB36(8)
	REAL*4 VALRES(1024),SYMBOR(26)
	REAL*8 FREVEC(8),FRELAB(8)
C
	EQUIVALENCE (BUFRESX(1),LIDERE(1))	!identification du resultat
	EQUIVALENCE (BUFRESX(17),LNOMSO(1))	!nom de la source
	EQUIVALENCE (BUFRESX(33),LTITMA(1))	!titre du map
	EQUIVALENCE (BUFRESX(49),NUMERE)	!numero du resultat
	EQUIVALENCE (BUFRESX(50),IUICRE)	!UIC du resultat
	EQUIVALENCE (BUFRESX(51),JDATRE)	!date du resultat
	EQUIVALENCE (BUFRESX(53),JDATMA)	!date de la manip
	EQUIVALENCE (BUFRESX(55),NUMSCA)	!numero de scan
	EQUIVALENCE (BUFRESX(56),IANREF)	!annee de ref. des coordonnees
	EQUIVALENCE (BUFRESX(57),ASDREF)	!asc. droite non reduite
	EQUIVALENCE (BUFRESX(59),DECREF)	!declinaison non reduite
	EQUIVALENCE (BUFRESX(61),NBRVEC)	!nombre de vecteurs
	EQUIVALENCE (BUFRESX(62),NVFREE)	!nbre de points libres de VALRES
	EQUIVALENCE (BUFRESX(63),IAHDRE)	!angle horaire depart
	EQUIVALENCE (BUFRESX(64),IFRJTU)	!TU passage au meridien
						!En 10/1000 jour
	EQUIVALENCE (BUFRESX(65),LITSYM(1))	!no des symboles de SIR sauves
	EQUIVALENCE (BUFRESX(91),SYMBOR(1))	!valeurs de ces symboles
	EQUIVALENCE (BUFRESX(143),ITYPOR)	!type de la poursuite
						!ITYPOU -> ITYPOR (conflit
						!avec ITYPOU CONMAP)
	EQUIVALENCE (BUFRESX(144),ITYSWI)	!type de 'switch' PO ou FR
	EQUIVALENCE (BUFRESX(145),IDAPOU)	!'DA' premiere phase en 1/10 sec
C
C   les elements suivants sont indices par le numero de vecteur (1-8)
C
	EQUIVALENCE (BUFRESX(151),VIVVEC(1))	!vitesse visee du vecteur(PAN)
	EQUIVALENCE (BUFRESX(167),IDBVEC(1))	!indice deb. du vec. dans VALRES
	EQUIVALENCE (BUFRESX(175),NPTVEC(1))	!nbre de points du vecteur
	EQUIVALENCE (BUFRESX(183),LTYVEC(1))	!type du vecteur
	EQUIVALENCE (BUFRESX(187),LKRVEC(1))	!code de la resolution
	EQUIVALENCE (BUFRESX(191),LKVVEC(1))	!code vitesse (PAN)
	EQUIVALENCE (BUFRESX(195),LPOVEC(1))	!code polar du vecteur
	EQUIVALENCE (BUFRESX(199),RESVEC(1))	!resolution du vecteur
	EQUIVALENCE (BUFRESX(215),VITVEC(1))	!abscisse du premier point
	EQUIVALENCE (BUFRESX(231),POIVEC(1))	!poids du vecteur
	EQUIVALENCE (BUFRESX(247),ITBVEC(1))	!temperature du TAB
	EQUIVALENCE (BUFRESX(255),ITSVEC(1))	!temperature de systeme
	EQUIVALENCE (BUFRESX(263),FREVEC(1))	!frequence ciel au repos
	EQUIVALENCE (BUFRESX(295),FRELAB(1))	!frequence labo depart manip
	EQUIVALENCE (BUFRESX(327),ITAUSO(1))	!cste d'integration
	EQUIVALENCE (BUFRESX(335),NBRCYC(1))	!nbre de cycles
	EQUIVALENCE (BUFRESX(343),RUB31(1))	!rubrique 31 de SIR
	EQUIVALENCE (BUFRESX(359),RUB32(1))	!........ 32 ......
	EQUIVALENCE (BUFRESX(375),RUB33(1))	!........ 33 ......
	EQUIVALENCE (BUFRESX(391),RUB34(1))	!........ 34 ......
	EQUIVALENCE (BUFRESX(407),RUB35(1))	!........ 35 ......
	EQUIVALENCE (BUFRESX(423),RUB36(1))	!........ 36 ......
C
	EQUIVALENCE (BUFRESX(448),MASQUE(1))	!bits du masque dans SIR
C
	EQUIVALENCE (BUFRESX(513),VALRES(1))	!valeurs des vecteurs
C
	COMMON /COMRESX/BUFRESX
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
 
	EQUIVALENCE (LIDERE(1),CLIDERE(1:1))
	EQUIVALENCE (LNOMSO(1),CLNOMSO(1:1))
	EQUIVALENCE (LTITMA(1),CLTITMA(1:1))
	EQUIVALENCE (ITYPOR,CITYPOR)
	EQUIVALENCE (ITYSWI,CITYSWI)
 
C Light velocity: (km/s)
	LIGHTVELO=299776.
C
	IER=0
	OPEN(UNIT=LUNPUT,FILE=FILPUT,ACCESS='SEQUENTIAL',
     1	STATUS='NEW',ERR=990)
 
C General header:
	WRITE(LUNPUT,45,ERR=992) CLIDERE,CLNOMSO,CLTITMA,NUMERE,
C1     1	IUICRE,
     1	NUMSCA,JDATRE,JDATMA,IANREF,ASDREF,DECREF,
     1	NBRVEC,
C2      1	NVFREE,
     1	IAHDRE,IFRJTU,
C3     1	(LITSYM(I),SYMBOR(I),I=1,5),
     1	CITYPOR,CITYSWI,IDAPOU
 
45	FORMAT(' Identification: ',A,/,' Object: ',A,/,
     1	' Map: ',A,'  Result number: ',I6,
     1	' Scan number: ',I5,/,
C1     1	' UIC number: ',I5,/,
     1	' Date (result): ',I9,'  Date (observ): ',I9,/,
     1	' Equinox: ',I4,' R. A. (sec)',G14.8,
     1	' Dec (arcsec)',G14.8,/,
     1	' Number of vectors: ',I3,/,
C2     1	' Number of free points: ',I6,/,
     1	' Hour angle (start), (min):',
     1	I6,' Meridian passage (UT in 1/10000 day):',I6,/,
C3     1	5(' Symbol (SIR): ',I5,' Value: ',G12.4,/),
     1	' Tracking mode: ',A2,
     1	'  Switch (PO or FR): ',A2,
     1	' First phase (tracking), (.1s):',I5)
 
C Conversion to mJy
C Gain is computed from the declination (in degrees):
	DECLIN=DECREF/3600.
	IF(DECLIN.GT.15)THEN
	  GAIN=1.19-3.73E-03*DECLIN-4.3E-05*DECLIN*DECLIN
	ELSEIF(DECLIN.LT.-15.)THEN
	  GAIN=1.81+0.0975*DECLIN+4.33E-03*DECLIN*DECLIN
     1	+6.37E-05*DECLIN*DECLIN*DECLIN
	ELSE
	  GAIN=1.1125+5.E-04*DECLIN
	ENDIF
 
C Loop on the vectors:
	DO I=1,NBRVEC
	  WRITE(LUNPUT,55,ERR=992) VIVVEC(I),
C1     1	IDBVEC(I),
     1	NPTVEC(I),LTYVEC(I),
     1	LKRVEC(I),LKVVEC(I),LPOVEC(I),RESVEC(I),VITVEC(I),
     1	POIVEC(I),ITBVEC(I),ITSVEC(I),FREVEC(I),FRELAB(I),
C2     1	ITAUSO(I),
     1	NBRCYC(I)
55	  FORMAT(/,'## Target velocity: ',F10.3,
C1     1	/,' Index of 1st pixel: ',I5,
     1	' Number of pixels: ',I5,/,
     1	' Vector type: ',A,'  Resol. code: ',A,
     1	'  Velocity code (SPAN): ',A,
     1	'  Polar. code: ',A,/,' Vel. step (km/s):',
     1	F9.4,' Vel. of 1st pixel:',F10.3,
     1	' Vector weight:',F9.2,/,
     1	' Calib. noise source temp. (1/10 K): ',I6,
     1	' System temp. (1/10 K): ',I6,/,
     1	' Rest sky freq. (Hz):',
     1	F14.2,'  Laboratory freq. (Hz): ',F14.2,/,
C2     1	' Exp. time for calibration in 1/100 s): ',I6,/,
     1	' Number of cycles :',I5,/)
 
C Generating the velocity axis, and writing the data in the file:
 
C Central velocity:
	  KCENT=(NPTVEC(I)/2)+1
	  VCENT=VIVVEC(I)
 
	  DO K=1,NPTVEC(I)
C First approximation is:
	    VELO1=VITVEC(I)+FLOAT(K-1)*RESVEC(I)
C First order correction (by P. Chamaraux):
	    VELO1=VELO1+(VELO1-VCENT)*(VELO1+VCENT)/LIGHTVELO
C Flux:
	    KK=K+IDBVEC(I)-1
	    FLUX1=VALRES(KK)*GAIN
C Output:
	    WRITE(LUNPUT,65,ERR=992) VELO1,FLUX1
65	    FORMAT(2(G12.5,X))
	  END DO
 
	ENDDO
 
	RETURN
990	TYPE 991,FILPUT
991	FORMAT(' *** RDNANCAY/WRASCII  Error opening file: ',A)
	IER=1
	RETURN
992	TYPE 993,FILPUT
993	FORMAT(' *** RDNANCAY/WRASCII  Error writing in file: ',A)
	IER=2
	CLOSE(UNIT=LUNPUT)
	RETURN
	END
