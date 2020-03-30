C++************************************************************************
C PROGRAM STRUCBA1
C EN AUTOMATIQUE RCA OU THOMSOM
C	VERSION MOUNT STROMLO DU 07-03-86
C	LECTURE DU FICHIER CREE PAR LA CAMERA CCD SUR BANDE
C	ET CREATION SUR DISQUE D"UN FICHIER STRUCTURE DE 512 LI DE 320 PIXELS
C	POUR LA RCA ET DE 576 LI DE 410 PIX POUR LA THOMSOM
C       ( with a "CCD format")
C	Output file for the details of the input parameters: strucba1.log
C--************************************************************************
	PROGRAM STRUCBA1
C Warning : NLT and NPLT in integer*2 for the header of the CCD file !!!
	INTEGER*2 ILUN,IFUNC,NCARA,IER,ISKIP,NLT,NPLT
	INTEGER*2 IBUF(256),MES(416),LIGNE(576)
     1	,MES0(410),IBUFT(5120),INUM(20)
	CHARACTER IDENT*80,FICH1*20,FICH2*20,REP*1,MDENT*80
	CHARACTER*80 KDENT(20),SFICH(20)*20
	COMMON /BLOC1/ILUN
C	DATA IEX1/'106000'O/,IEX/'160000'O/,LXA/'144000'O/
	DATA IEX1/-29696/,IEX/-8192/,LXA/-12336/
 
C----------------------------------------------------------------------
C I - Access to the tape :
C----------------------------------------------------------------------
	PRINT 6809,IEX1,IEX,LXA
6809	FORMAT(' CODES TO BE FOUND IN THE HEADER:',/,
     1	' IEX1=',I10,' IEX=',I10,' LXA=',I10)
C IFUNC=5 : MONTAGE DE LA BANDE NOM LOGIQUE: CCD
	IFUNC=5
	NCARA=5120
	PRINT *,' (YOU SHOULD HAVE TYPED:'
	PRINT *,' $ASSIGN MUA0: CCD'
	PRINT *,' $MOU/FOR MUA0:)'
	PRINT *,' '
	CALL MTACCESS(ILUN,IFUNC,IBUFT,NCARA,0,IER,'CCD')
	PRINT *,' ILUN= ',ILUN
	PRINT *,' NCARA= ',NCARA
	PRINT *,' IER= ',IER
	IF(IER.NE.0)PRINT *,'PROBLEME AU MONTAGE'
 
C IFUNC=1 : REMBOBINAGE A TOUT HAZARD...
	IFUNC=1
	CALL MTACCESS(ILUN,IFUNC,IBUFT,NCARA,0,IER,'CCD')
	OPEN(3,STATUS='unknown',FILE='strucba1.log')
	PRINT 1015
1015	FORMAT(/)
 
 
C----------------------------------------------------------------------
C II - Input of the commands :
C----------------------------------------------------------------------
	PRINT *,' NOMBRE DE FICHIERS A LIRE SUR LA BANDE  (20 MAX)'
	READ(5,*) NFICH
	PRINT 1015
	PRINT *,' DONNER LES NUMEROS DE FICHIERS  PAR ORDRE CROISSANT'
	PRINT 1015
 
	DO 250 IF=1,NFICH
	PRINT 270,IF
270	FORMAT(I4,'    FICHIER BANDE NUMERO:',$)
	READ(5,*) NUM
	PRINT 271
271	FORMAT(' NOM A DONNER AU FICHIER DISQUE:',$)
	ACCEPT 203,FICH1
	PRINT *,' COMMENTAIRE POUR CE FICHIER:'
	ACCEPT 203, IDENT(13:80)
203	FORMAT(A)
	IDENT(1:1)='I'
	INUM(IF)=NUM
	SFICH(IF)=FICH1
	IDENT(2:12)='           '
	KDENT(IF)=IDENT
250	CONTINUE
 
C----------------------------------------------------------------------
C I - Main loop on the NFICH files :
C----------------------------------------------------------------------
	PRINT 1015
	PRINT *,'  DETAIL DES OPERATIONS DS LE FICHIER: strucba1.log'
	PRINT 1015
	WRITE(3,217)
217	FORMAT(/,'-------------    DETAIL DES OPERATIONS---------------',//)
 
	NUFIA=1
 
	DO 251 NF=1,NFICH
 
C Skipping to the next file :
	NUFIN=INUM(NF)
	NSAUT=NUFIN-NUFIA
	  IF(NSAUT.GT.0)THEN
	  IFUNC=3
	  CALL MTACCESS(ILUN,IFUNC,IBUFT,NCARA,NSAUT,IER,'CCD')
	  IF(IER.NE.0)PRINT *,' PB AVEC LE SAUT DE FICHIERS'
	  ENDIF
 
C Reading the first block:
	FICH2=SFICH(NF)
	LBA=0
	NBLOC=1
	CALL LECBAN(LBA,IBUFT,NB1,NB2,NCARA,NBLA)
	NP1=(NBLOC-1-(LBA-1)*NBLA)*256+1
	NP2=NP1+255
	K=0
	PRINT *,' NCARA=',NCARA
	PRINT *,' PASSAGE APRES LECBAN : ECRITURE DES 10 PREMIERS CAR.'
 
	DO 2000 II=1,10
	PRINT *,' II=',II,IBUFT(II)
2000	CONTINUE
 
	DO 111 KT=NP1,NP2
	K=K+1
	IBUF(K)=IBUFT(KT)
111	CONTINUE
 
	   DO IEC=1,20
	   IF(IBUF(IEC).EQ.IEX) THEN
	   KIEC=IEC
	   PRINT *,' IEX EST DETECTE , KIEC=',KIEC
	   GO TO 5020
	   ENDIF
	   END DO
 
C Decoding IPIC and NBPIX:
5020	IF(IEC.GE.20) THEN
	  PRINT *,' PROBLEM WITH THE FIRST BLOCK'
	  PRINT *,' IEX N"EST PAS DETECTE !'
	ELSE
	  IPIC=IBUF(KIEC+1)
	  NBPIX=IBUF(KIEC+1)-IEX1
	  PRINT 1014,KIEC,IPIC,NBPIX
1014	  FORMAT(' KIEC=',I5,2X,'IPIC=',I8,
     1	' NX:',I5)
	  PRINT 1015
	ENDIF
 
C Problem with NBPIX
	  IF(NBPIX.NE.323.AND.NBPIX.NE.410) THEN
c	  PRINT 5100,NUFIN
c	  WRITE(3,5100) NUFIN
c5100	  FORMAT(/,' LE 1ER BLOC DU FICHIER BANDE:',I3,' N"EST PAS CORRECT'
c     1	,' ON PASSE AU FICHIER SUIVANT',/)
c	  IFINI=0
c	  GO TO 5101
	  PRINT *,' WARNING : PB  NBPIX=',NBPIX
	  PRINT *,' ENTER NBPIX,IPIC : (RCA=323,-29373 THX=*,*)'
	  READ(5,*) NBPIX,IPIC
	  ENDIF
 
	IF(NBPIX.EQ.410) THEN
	PRINT *,'          DONNEEES ACQUISES PAR LA CAMERA THOMSOM'
	PRINT 1015
	NPLT=410
	NLT=576
	NBTOT=930
	KDECMI=1
	KDECMA=4
	IDENT(2:12)='   THOMSOM '
	ENDIF
 
C Camera RCA
	IF(NBPIX.EQ.323) THEN
	PRINT *,'         DONNEEES ACQUISES PAR LA CAMERA RCA'
	PRINT 1015
	NPLT=320
	NLT=512
	NBTOT=660
	KDECMI=4
	KDECMA=7
	IDENT(2:12)='   RCA     '
	ENDIF
 
C Opening the output file :
	VINIT=(FLOAT(NPLT)*2.*FLOAT(NLT))/512.
	INIT=NINT(VINIT)+1
	IREC=NPLT/2
262	OPEN(1,FILE=FICH2,ACCESS='DIRECT',STATUS='NEW',
     1	RECL=IREC,INITIALSIZE=-INIT,ERR=1003)
	MDENT=KDENT(NF)
	MDENT(2:12)=IDENT(2:12)
	KDENT(NF)=MDENT
	WRITE(3,260) NUFIN,FICH2,KDENT(NF)
260	FORMAT(' NUMERO FICHIER BANDE:',I3,9X,' NOM SUR DISQUE:',A20,
	1/,' COMMENTAIRE:',A)
	PRINT 260,NUFIN,FICH2,KDENT(NF)
 
C Writing the header :
	WRITE(1'1)KDENT(NF),NPLT,NLT
 
	NCAS=0
 
C Initialization of MES0 :
	DO II=1,NPLT
	MES0(II)=0
	END DO
 
C Looking for IPIC and getting the starting point for KS1 :
	DO 10 I=1,256
	IF(IBUF(I).NE.IPIC) GO TO 10
	KS1=I
10	CONTINUE
C	PRINT *,' KS1',KS1
	IF(KS1.EQ.0)THEN
C	PRINT *,' IPIC not found !'
	GO TO 777	! If IPIC is not found, reading the next block
	ENDIF
 
	KDEC=KDECMI
	IF(IBUF(KS1+1).LE.LXA) KDEC=KDECMA
	NPL=0		! Resetting the pixel counter
	LLIN=1		! Resetting the line counter
	NCO=1		! Resetting the column counter
 
C Reading the number of the number of the line to be decoded :
	LIGNE(NCO)=IBUF(KS1-1)-IEX+1
 
	DO MN=KS1+KDEC,256
	  NPL=NPL+1
	  MES(NPL)=IBUF(MN)
	END DO
 
C Reading the next bloc to get the end of the line :
777	NBLOC=NBLOC+1
	  IF(NBLOC.GT.NB2) THEN
	  CALL LECBAN(LBA,IBUFT,NB1,NB2,NCARA,NBLA)
	  ENDIF
	NP1=(NBLOC-1-(LBA-1)*NBLA)*256+1
	NP2=NP1+255
	K=0
 	   DO KT=NP1,NP2
	   K=K+1
	   IBUF(K)=IBUFT(KT)
	   END DO
	IF(NCAS.EQ.1) GO TO 888
	IF(NCAS.EQ.2) GO TO 999
	KS1=0
 
C Looking for IPIC in the next block :
	DO 11 I=1,256
	IF(IBUF(I).NE.IPIC) GO TO 11
	KS1=I
11	CONTINUE
 
	IF(KS1.EQ.0)THEN
C	PRINT *,' IPIC not found !'
	GO TO 777	! Reading the next block
	ENDIF
 
	NCO=NCO+1
	KLI=KS1-1
	  IF(KLI.GE.1) THEN
	    IVL=IBUF(KLI)
	  ELSE
	   IVL=IDER
	  ENDIF
	LIGNE(NCO)=IVL-IEX+1
	IDINO=LIGNE(NCO)-NCO
C	PRINT *,' LIGNE(NCO),NCO =',LIGNE(NCO),NCO
	IDINO=0		! @
 
	IF(IDINO.EQ.-1) THEN
	PRINT *,' BLOC SUREMENT INCORRECT AVANT LE 1ER BLOC '
	PRINT *,' SEULES LES 2 PREMIERES LIGNES SERONT FAUSSES'
	WRITE(3,4507) IDINO
4507	FORMAT(/,' IDINO=',I4,' BLOC SUREMENT INCORRECT AVANT LE 1ER BLOC')
	WRITE(3,4508)
4508	FORMAT(' SEULES LES 2 PREMIERES SONT FAUSSES',/)
	NCO=NCO-1
	GO TO 777	! Reading the next block
	ENDIF
 
	IF(IDINO.LT.-1) THEN
	PRINT *,' PB SUR LA BANDE,DECODAGE DIFFICILE'
	WRITE(3,4509)
4509	FORMAT(' PB SUR LA BANDE DECODAGE DIFFICILE')
	PRINT *,' ON CHANGE DE FICHIER'
	GO TO 251		! End with this file
	ENDIF
 
	IDER=IBUF(256)
 
	IF(KS1.EQ.0)THEN
	  DO M=1,256
	   NPL=NPL+1
	   IF(NPL.GT.NPLT) GO TO 666	! Reading the next bloc
	   MES(NPL)=IBUF(M)
	  END DO
	  GO TO 666			! Reading the next bloc
	ENDIF
 
	KF=KS1-2
	IF(KF.GE.1)THEN
	  DO 20 M=1,KF
	  NPL=NPL+1
	  IF(NPL.GT.NPLT) GO TO 40
	  MES(NPL)=IBUF(M)
20	  CONTINUE
	ENDIF
 
C Checking the errors in the line number with IDIFI :
C The number of the line which is decoded is LIGNE(NCO-1)
C The current value of the line index in the program is LLIN
C	PRINT *,' LIGNE(NCO-1),LLIN',LIGNE(NCO-1),LLIN
40	IDIFI=LIGNE(NCO-1)-LLIN
 
C Case when a line is missing :
	IF(IDIFI.EQ.1) THEN
	PRINT 1015
	PRINT *,'-----ATTENTION PROBLEME A L"ENREGISTREMENT--------'
	WRITE(3,*)'-----ATTENTION PROBLEME A L"ENREGISTREMENT--------'
	PRINT 1015
	  PRINT *,' LA LIGNE:',LLIN-1,' EST INCORRECTE'
	  PRINT *,' LA LIGNE :',LLIN,'  A SAUTE'
	  PRINT *,' ELLE EST REMPLACEE PAR DES 0'
	  WRITE(3,3011) LLIN-1,LLIN
3011	  FORMAT(' LA LIGNE:',I3,' EST INCORRECTE',' ,LA LIGNE:',I3,
     1	' A SAUTE, ELLE EST REMPLACEE PAR DES 0',/)
	  WRITE(1'LLIN+1) (MES0(IU),IU=1,NPLT)
	  LLIN=LLIN+1
 
C Case when something completely wrong is decoded for the line index :
	ELSEIF(IDIFI.NE.0.AND.IDIFI.NE.1)THEN
	  PRINT 1015
	  PRINT *,'-----ATTENTION PROBLEME A L"ENREGISTREMENT--------'
	  WRITE(3,*)'-----ATTENTION PROBLEME A L"ENREGISTREMENT--------'
	  PRINT 1015
	  PRINT *,' ATTENTION A LA LIGNE:',LLIN
	  PRINT *,' DECHIFFRAGE LIGNE:',LIGNE(NCO-1)
	  PRINT *,' EN PRINCIPE LES VALEURS DE CETTE LIGNE SONT CORRECTES'
	  WRITE(3,3012) LLIN
3012	  FORMAT(' ATTENTION A LA LIGNE:',I3,' VALEURS PEUT ETRE '
     1	,' INCORRECTES',/)
	ENDIF
 
C Checking if the number of pixels is correct :
	IF(NPL.LT.NPLT)THEN
	PRINT *,' Warning : only',NPL,' pixels read ',
     1	'in line',LLIN
	ENDIF
 
C Writing the line LLIN
C3333	PRINT 48,LLIN
C48	FORMAT(' LINE =',I6)
3333	WRITE(1'LLIN+1) (MES(IJ),IJ=1,NPLT)
 
	IF(LLIN.EQ.NLT-1) GO TO 100		! Writing the last line
 
	KS2=KS1+1			! Positioning KS2 after the liacom index
 
	   IF(KS2.EQ.257) THEN		! When the liacom index is just at the
	   NCAS=2			! end of the bloc : Case=2
	   GO TO 777			! Reading the next block
	   ENDIF
	KDEC=KDECMI
	IF(IBUF(KS1+1).LE.LXA) KDEC=KDECMA
	KD=KS1+KDEC
	   IF(KD.GT.256) THEN
	   NCAS=1
	   GO TO 777		! Reading the next block
	   ENDIF
 
C Reading the next line
	NPL=0		!resetting the pixel counter
	LLIN=LLIN+1	! Incrementing the line index
	  DO M=KD,256
	    NPL=NPL+1
	    MES(NPL)=IBUF(M)
	  END DO
	GO TO 666	! Reading the next bloc
 
999	NDEC=KDECMI-1
	IF(IBUF(1).LE.LXA) NDEC=KDECMA-1
	NDE=NDEC+1
	GO TO 15
888	NDE=KD-256
15	NPL=0
	LLIN=LLIN+1
	  DO M=NDE,256
	   NPL=NPL+1
	   MES(NPL)=IBUF(M)
	  END DO
666	NCAS=0
	IF(NBLOC.LT.NBTOT)GO TO 777	! Reading the next block
100	LLIN=NLT
C Writing the last line :
	WRITE(1'LLIN+1) (MES(MV),MV=1,NPLT)
	CLOSE(1)
5101	NUFIA=NUFIN
 
251	CONTINUE		! End : going to the next file
 
3010	PRINT *,' END ...'
	CALL MTACCESS(ILUN,1,IBUFT,NCARA,0,IER,'CCD')
	STOP
 
C Set the name of the file if problem when opening it
1003	WRITE(3,261) NUFIN
261	FORMAT(/,' LE FICHIER ',I3,' A POUR NOM: FOR001.DAT')
	FICH2='FOR001.DAT'
	GO TO 262
 
	END
C***********************************************************
C Input :
C LBA : Index
C
C Output:
C IBUFT : Buffer where to read the line
C NBLA : Size of the segment in integer*2 values
C LBA : Index
C NB1 : starting position to read the corresponding line
C NB2 : ending position to read the corresponding line
C**************************************************************
	SUBROUTINE LECBAN(LBA,IBUFT,NB1,NB2,NCARA,NBLA)
	INTEGER*2 IBUFT(5120)
	COMMON /BLOC1/ ILUN
	IFUNC=2
	CALL MTACCESS(ILUN,IFUNC,IBUFT,NCARA,0,IER,'CCD')
	IF(IER.NE.0)PRINT *,' PB LORS DE LA LECTURE'
	VNBLA=NCARA/2./256.
	NBLA=NINT(VNBLA)
	LBA=LBA+1
	NB1=(LBA-1)*NBLA+1
	NB2=NB1+NBLA-1
	RETURN
	END
C********************************************************************
C MTACCESS :
C CETTE ROUTINE TRAVAILLE SUR LES BANDES MAGNETIQUES EN
C MODE PHYSIQUE.
C OPERATIONS:
C		IFUNC=1 --> REMBOBINAGE
C		IFUNC=2 --> LECTURE D'UN ENREGISTREMENT
C		IFUNC=3 --> SKIP DE "ISKIP" FICHIERS (TAPE MARK)
C		IFUNC=4 --> ECRITURE D'UN ENREGISTREMENT
C		IFUNC=5 --> ASSIGNATION D'UN LUN A UN "LOGICAL"
C		IFUNC=6 --> ECRITURE FIN DE FICHIER (TAPE MARK)
C
C	APPEL:
C
C	CALL MTACCESS (ILUN,IFUNC,IBUF,INCAR,ISKIP,IEOF,DEVNAM)
C
C
C	ILUN= NO. UNITE LOGIQUE ASSIGNEE A UN NOM "LOGICAL"
C		INTEGER*2
C
C	IFUNC=FONCTION A REALISER SUR LA BANDE
C		INTEGER*2
C
C	IBUF = BUFFER EN ENTREE OU EN SORTIE
C		LOGICAL*1
C
C	INCAR=NOMBRE DE CARACTERES EN ENTREE OU SORTIE < 32767 CAR.
C		INTEGER*2
C
C	ISKIP=NOMBRE DE FICHIERS A SKIPER >0 EN AVANT , <0 EN ARRIERE
C	DANS CE CAS ON EST JUSTE A LA FIN D'UN FICHIER .
C	LE NOMBRE DE FICHIERS EST PHYSIQUE (I.E NOMBRE DE TAPE MARK)
C		INTEGER*2
C
C	IEOF : CODE RETOUR D'UNE FONCTION DECRITE CI-DESSUS
C		INTEGER*2
C
C	IEOF=1   INDICATEUR FIN DE FICHIER EN ENTREE
C	    =2   ERREUR DE LONGUEUR EN LECTURE DONNEE<REELLE
C	    =3   ERREUR DE PARITE EN LECTURE
C	    =4   FIN DE BANDE PHYSIQUEMENT
C	    =5   VOLUME NON MONTEE MAIS ASSIGNE AVEC UN LOGICAL
C	    =6   DATA CHECK EN ECRITURE (VOIR MAINTENANCE)
C	    =7   FIN DE VOLUME I.E DEUX 'TAPE MARK'
C
C
C	DEVNAM= NOM "LOGICAL" ASSIGNE AU MOMENT DU MOUNT DE LA BANDE
C		CHARACTER* DEVNAM   *=LONGUEUR DU NOM DONNE AU MOMENT
C				      DU MOUNT DE LA BANDE
C
C
C CE PROGRAMME TRAVAILLERA AVEC LE NOM "LOGICAL" DONNE AU MOUNT
C
C
C
C
C***********************************************************************
C
C            EXEMPLE D'UTILISATION DU PROGRAMME :
C	A) LES DECLARATIONS DANS LE PROGRAMME APPELANT :
C		INTEGER*2 ILUN,IFUNC,INCAR,ISKIP,IEOF
C		LOGICAL*1 IBUF( )  ! DONNER LONGUEUR DE IBUF
C		CHARACTER*5 DEVNAM ! ICI 5 CAR LE "LOGICAL" EST TAPE1
C		DATA DEVNAM/'TAPE1'/ ! LE NOM "LOGICAL" ASSIGNE AU MOUNT
C                      		.......
C				........
C 	B) IL FAUT FAIRE LA CORRESPONDANCE POUR AVOIR LE NO. LOGIQUE
C	                   DE ILUN DONNE PAR LE SYSTEME
C		IFUNC=5		! FONCTION ASSIGNATION
C		CALL MTACCESS(ILUN,IFUNC,IBUF,INCAR,ISKIP,IEOF,DEVNAM)
C		!!!!!ATTENTION IL NE FAUT PAS DETRUIRE  ..ILUN...
C		!!!!! IL SERVIRA POUR TOUTE LA SUITE DE VOS OPERATIONS
C				.......
C				........
C		!  MAINTENANT VOUS POUVEZ EXECUTER TOUTES LES AUTRES
C		!  FONCTIONS A VOTRE SOUHAIT ...............
C		! ..........................................
C	C) MAINTENANT IL FAUT LINKER AVEC VOTRE PROGRAMME :
C
C		LINK PROG,.........,'UTL'/LIBRARY
C 		PROG EST VOTRE PROGRAMME
C		............ET LES AUTRES MODULES DE VOTRE ENSEMBLE
C		'UTL'/LIBRARY   LA LIBRAIRIE DES UTILITAIRES DE MEUDON
C
C
C	D) A L'EXECUTION AVANT DE FAIRE LE RUN EXECUTER :
C		- ALLOCATE MT:
C		-MOU 'DEV'/FOR/DENSITY=XXXX
C		-ASSIGN 'DEV' "LOGICAL"
C
C 		'DEV' UNITE ALLOUEE
C		XXXX  LA DENSITE DESIREE
C		"LOGICAL" LE NOM "LOGICAL" DEFINI DANS VOTRE PROG......
C
C
C***********************************************************************
C
	SUBROUTINE MTACCESS (ILUN,IFUNC,IBUF,INCAR,ISKIP,IEOF,DEVNAM)
C
C	DEFINITIONS:
C***********************************************************************
	PARAMETER BUFSIZE=1
	PARAMETER EOF='870'X
	PARAMETER NOLOGNAM='1BC'X
	PARAMETER IVDEVNAM='144'X
 
	PARAMETER NOPRIV='24'X
	PARAMETER IO$_READLBLK='21'X
	PARAMETER IO$_REWIND='24'X
	PARAMETER IO$_WRITELBLK='20'X
	PARAMETER IO$_WRITEMARK='1B'X
	PARAMETER IO$_WRITEOF='28'X
	PARAMETER IO$_SKIPFILE='25'X
C
C
	INTEGER*2 ENDFLAG,IOSB(4),ILUN,ISTAT
	INTEGER*2 IFUNC,INCAR,ISKIP,IEOF
	INTEGER*4 SYS$ASSIGN,SYS$QIOW,RETCODE
	LOGICAL*1 IBUF(1)
	CHARACTER*(*) DEVNAM
C
C
C********************************************
C FONCTION A REALISER:
C********************************************
	GOTO (100,200,300,400,500,600) IFUNC
C********************************************
C REWIND TAPE:
C********************************************
100	RETCODE=SYS$QIOW(,%VAL(ILUN),%VAL(IO$_REWIND),IOSB,,,,,,,,)
	IF (RETCODE.NE.1) GO TO 9000
	RETURN
C********************************************
C READ RECORD TAPE:
C********************************************
200	RETCODE=SYS$QIOW(,%VAL(ILUN),%VAL(IO$_READLBLK),IOSB,,,
     * %REF(IBUF),%VAL(INCAR),,,,)
	IEOF=0
C	WRITE (6,777) RETCODE,IOSB
777	FORMAT (1X,'RET=',I5,4('SS',I5))
	IF (RETCODE.NE.1) GO TO 9000
	IF (IOSB(1) .EQ. EOF)  IEOF=1
	IF (IOSB(1) .EQ. 2104) IEOF=2	!2104=X'838' DATA OVERUN
	IF (IOSB(1) .EQ. 500)  IEOF=3	!500=X'1F4' PARITY
	IF (IOSB(1) .EQ. 2168 ) IEOF=4	!2168=X'878' END OF TAPE
C	 ICI IL Y A ERREUR DE LONGUEUR DONNEE<REELLE
	IF (IOSB(1) .EQ. 596 ) IEOF=5	!596=X'254' NO VOLUME MOUNTED
	INCAR=IOSB(2)	! NOMBRE DE CARACTERES DANS RECORD BANDE
C	IF (IOSB(1) .NE. 1) WRITE (6,888)RETCODE,IOSB
C	WRITE (6,250) INCAR
250	FORMAT (1X, 'NOMBRES DE CARACTERES LUS :',I5)
	RETURN
C
C**********************************************************
C SKIP FILES:
C**********************************************************
300	RETCODE=SYS$QIOW(,%VAL(ILUN),%VAL(IO$_SKIPFILE),IOSB,,,
     * %VAL(ISKIP),,,,,)
C	WRITE (6,9999) RETCODE,ISKIP
C9999	FORMAT (1X,'RET=',I5,'ISKIP=',I5)
	IF (RETCODE.NE.1) GO TO 9000
	IF (IOSB(1) .EQ. 2168 ) IEOF=4	!2168=X'878' END OF TAPE
	IF (IOSB(1) .EQ. 2464 ) IEOF=7	! 2464=X'9A0' END OF VOLUME
	RETURN
C*********************************************
C	WRITE RECORD ON TAPE
C*********************************************
400	RETCODE=SYS$QIOW(,%VAL(ILUN),%VAL(IO$_WRITELBLK),IOSB,,,
     *%REF(IBUF),%VAL(INCAR),,,,)
	IEOF=0
	IF (RETCODE.NE.1) GO TO 9000
	IF (IOSB(1) .EQ. 2168 ) IEOF=4	!2168=X'878' END OF TAPE
	IF (IOSB(1) .EQ. 596) IEOF=5	!596=X'254' NO VOLUME MOUNTED
	IF (IOSB(1) .EQ. 92 ) IEOF=6 	!92=X'5C' DATA CHECK
C	WRITE(6,777) RETCODE,IOSB
	RETURN
C
C***********************************************
C	ASSIGNATION LUN A TAPE
C***********************************************
500	RETCODE=SYS$ASSIGN(%DESCR(DEVNAM),ILUN,,)
	IF (RETCODE.NE.1) GO TO 9000
	RETURN
C
C**********************************************
C	WRITE EOF (TAPE MARK)
C**********************************************
600	RETCODE=SYS$QIOW(,%VAL(ILUN),%VAL(IO$_WRITEOF),IOSB,,,,,,,,)
	IF (RETCODE.NE.1) GO TO 9000
	RETURN
C*********************************************
C	ERREURS:
C*********************************************
9000	IF (RETCODE.EQ.NOLOGNAM) STOP ' *** MT NOT ASSIGNED ***'
	IF (RETCODE.EQ.NOPRIV)   STOP ' *** MT NOT FOREIGN ***'
	IF (RETCODE.EQ.IVDEVNAM) STOP ' *** INVALID DEVICE NAME ***'
	IF (RETCODE.EQ.2312 ) WRITE (6,912)	! NO SUCH DEV
912	FORMAT(1X,'*** MAUVAISE ASSIGNATION OUBLI...')
C	WRITE (6,9001) RETCODE	!IMPRESSION CODE RETOUR
9001	FORMAT (' *** ERREUR I/O BANDE= ',Z8,' HEX ***')
C	WRITE (6,888) RETCODE,IOSB
888	FORMAT (1X,'RETCODE=',I5,4('SS',Z4))
	INCAR=IOSB(2)	!NOMBRE DE CARACTERES
	RETURN
	END