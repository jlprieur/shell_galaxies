C++*****************************************************************
C Program CALCUL
C
C JLP Version of 14-03-86
C--*****************************************************************
	CHARACTER REP*1,FICH*25
	REAL*4 VAL1(100),VAL2(100),VAL(100)
	REAL*4 X(30),Y(30),THETA(30)
	PRINT 21
21	FORMAT(' PROGRAMME DE CALCUL - VERSION DU 14-03-86 -',/,
     1	' OPTION 1 : ENTREE DES DONNEES DANS LE TABLEAU 1',/,
     1	' OPTION 2 : ENTREE DES DONNEES DANS LE TABLEAU 2',/,
     1	' OPERATIONS SUR CES TABLEAUX :',/,
     1	' OPTION 3: (2 - 1) ---> 1',/,
     1	' OPTION 4: CALCUL DE MOYENNE ET D"ECART-TYPE DU TABLEAU 1',/,
     1	' OPTION 5: (2 - 1)/ ((2+1)/2) ----> 1',/,
     1	' OPTION 6: CALCUL D"UNE ELLIPSE SYNTHETIQUE',/,
     1	' OPTION 7: CALCUL D"UN LOG OU EXP',/,
     1	' OPTION 8: CALCUL SQRT(TAB1**2 +TAB2**2) DANS TAB1',/,
     1	' OPTION 10: EXIT',/,
     1	' ENTREZ L"OPTION CHOISIE')
	READ(5,*) IOPT
	IF(IOPT.LE.2)THEN
	N=0
	PRINT *,' ENTREZ LES VALEURS (FORMAT: X.Y OU X.Y,S POUR SORTIR)'
10	PRINT 20
20	FORMAT(' ? ',$)
	N=N+1
	ACCEPT 11,VAL(N),REP
11	FORMAT(F9.3,A)
	IF(REP.NE.'S')GO TO 10
	IF(IOPT.EQ.2)THEN
	DO 12 I=1,N
	VAL2(I)=VAL(I)
12	CONTINUE
	N2=N
	ELSE
	N1=N
	DO 14 I=1,N
	VAL1(I)=VAL(I)
14	CONTINUE
	ENDIF
	GO TO 1
	ENDIF
C****************OPTION 3 DIFF 2-1 EN 1****************************
	IF(IOPT.EQ.3)THEN
	N1=MIN0(N1,N2)
	N=N1
	DO 21 I=1,N
	VAL1(I)=VAL2(I)-VAL1(I)
21	CONTINUE
	GOTO 1
	ENDIF
C*******************************************************************
C	CALCUL DE LA MOYENNE
C*******************************************************************
	IF(IOPT.EQ.4)THEN
	VV=0.
	N=N1
	DO 40 I=1,N
	VV=VV+VAL1(I)
40	CONTINUE
	VMOY=VV/FLOAT(N)
	PRINT *,' MOYENNE:',VMOY
C*******************************************************************
C	CALCUL DE L'ECART-TYPE
C*******************************************************************
	VV=0.
	DO 50 I=1,N
	VV=VV+(VAL1(I)-VMOY)**2
50	CONTINUE
	SIGMA=SQRT(VV/FLOAT(N))
	PRINT *,' ECART-TYPE :',SIGMA
	GO TO 1
	ENDIF
C****************OPTION 3  : (2-1)/((2+1)/2) EN 1****************************
	IF(IOPT.EQ.5)THEN
	N1=MIN0(N1,N2)
	N=N1
	DO I=1,N
	VAL1(I)=2.*(VAL2(I)-VAL1(I))/(VAL2(I)+VAL1(I))
	END DO
	GOTO 1
	ENDIF
C*******************************************************************
C	CALCUL D'UNE ELLIPSE SYNTHETIQUE
C*******************************************************************
	IF(IOPT.EQ.6) THEN
395	PRINT *,' NOM DU FICHIER DE SORTIE ?'
	ACCEPT 111,FICH
	OPEN(UNIT=1,FILE=FICH,STATUS='NEW',INITIALSIZE=-3,
     1	ERR=395)
	PRINT *,' COORDONNEES DU CENTRE X, Y ?'
	READ(5,*) XC,YC
	PRINT *,' ELLIPTICITE ? (10*(1-BB/AA)'
	READ(5,*) EE
	PRINT *,' VALEUR DU GRAND AXE ?'
	READ(5,*) AA
	PRINT *,' ORIENTATION DU GRAND-AXE (DEG)?'
	READ(5,*) THETA0
	WRITE(1,396)1,30
	DO 300 I=1,30
	THETA(I)=.2*FLOAT(I)
	XX=AA*COS(THETA(I))
	YY=AA*(1.-EE/10.)*SIN(THETA(I))
	X(I)=XC+COSD(THETA0)*XX-SIND(THETA0)*YY
	Y(I)=YC+SIND(THETA0)*XX+COSD(THETA0)*YY
	PRINT 1013,I,X(I),Y(I)
	WRITE(1,397) X(I),Y(I)
300	CONTINUE
111	FORMAT(A)
396	FORMAT(' SHELL NUMBER :',I3,'  POINTS :',I4,/,
     1	2X,'    X   ',2X,'    Y   ',/)
397	FORMAT(2X,F8.2,2X,F8.2)
1013	FORMAT(2X,' I=',I3,2X,'X ET Y :',2(2X,F8.3))	
	CLOSE(1)
	ENDIF
C*****************************************************************
	IF(IOPT.EQ.7)THEN
	PRINT *,' 1.LOG NEP     2.EXP BASE E ?'
	READ(5,*) II
	PRINT *,' VALEUR D"ENTREE ?'
	READ(5,*) XX
	IF(II.EQ.1)S=ALOG(XX)
	IF(II.EQ.2)S=EXP(XX)
	PRINT *,'LE RESULTAT EST ',S
	ENDIF
C********************************************************************
	IF(IOPT.EQ.8)THEN
	N1=MIN0(N1,N2)
	DO I=1,N1
	VAL1(I)=SQRT(VAL1(I)*VAL1(I)+VAL2(I)*VAL2(I))
	END DO
	GOTO 1
	ENDIF
 
	STOP
	END
