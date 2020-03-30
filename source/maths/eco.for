C++------------------------------------------------------------
C Program ECO
C To compute the price of a car, comparing with inflation, and
C interest rates
C
C JLP
C Version 02-05-89
C--------------------------------------------------------------
	PROGRAM ECO
	REAL MONEY,TIME,BUDGET
	REAL INI_PRICE,INF_RATE,SAV_RATE,KM_YEAR,INSUR
	REAL KM_MAX,L_100KM,L_PRICE
	REAL C_PETROL,EXPEND
	INTEGER LIFE
	CHARACTER NAME*40
	DATA KM_MAX/90000./
 
10	FORMAT(A)
	OPEN(1,FILE='eco.log',STATUS='unknown')
 
	PRINT 70
70	FORMAT(' Program to compute the following problem:',/,
     1	' Asssume that you have the amount MONEY now.',/,
     1	' To compare 2 solutions during the period TIME :',/,
     1	' Buying a new car, or old cars regularly')
 
	PRINT *,' Amount of money available:'
	READ(5,*) MONEY
	PRINT *,' Yearly budget for the car:'
	READ(5,*) BUDGET
	PRINT *,' Price of the car ?'
	READ(5,*) INI_PRICE
	PRINT *,' Is it :  1=new car   2=old car ?'
	READ(5,*) IOPTION
	PRINT *,' Inflation rate ? (%)'
	READ(5,*) INF_RATE
	PRINT *,' Interest rate in a saving account ? (%)'
	READ(5,*) SAV_RATE
	PRINT *,' Number of km/year :'
	READ(5,*) KM_YEAR
	PRINT *,' Cons. in liter/100km?'
	READ(5,*) L_100KM
	PRINT *,' Price per liter ?'
	READ(5,*) L_PRICE
	PRINT *,' Cost of insurance per year ?'
	READ(5,*) INSUR
	PRINT *,' Average expenditures per year (repairs, etc) ?'
	READ(5,*) EXPEND
 
	PRINT *,' Name of the car:'
	READ(5,10) NAME
	
	WRITE(1,88) MONEY,BUDGET,INF_RATE,SAV_RATE,NAME,
     1	INI_PRICE,KM_YEAR,L_100KM,L_PRICE,INSUR,EXPEND
88	FORMAT('       ECO Version 02-05-89',/,
     1	' Money available:',F9.0,T38,
     1	' Yearly budget for the car:',F9.0,/,
     1	' Inflation rate : ',F5.2,T38,
     1	' Saving interest rate : ',F5.2,/,
     1	'       NAME :',A,/,
     1	' Initial price of the car:',F9.0,T38,
     1	' Km/year : ',F7.0,/,
     1	' Cons. in liter/100km : ',F4.1,T38,
     1	' Price per liter :',F6.2,/,
     1	' Insurance per year :',F9.0,T38,
     1	' Average expen./year (repairs,...):',
     1	F8.0)
 
C******************************************************************
C 1. Computes the number of years before reaching KM_MAX:
	IF(IOPTION.EQ.1)THEN
	  LIFE=NINT(KM_MAX/KM_YEAR)
	  PRINT 80,LIFE
	  WRITE(1,*) 'New car'
	ELSE
	  PRINT *,' Number of years the old car is expected to live?'
	  READ(5,*) LIFE
	  WRITE(1,*) 'Old car'
	ENDIF
	  WRITE(1,80) LIFE
80	  FORMAT(5X,' Life time : ',I2,' years')
	
C******************************************************************
C Computes the yearly cost of the car:
	
C 2. Petrol:
	C_PETROL=L_PRICE*L_100KM*(KM_YEAR/100.)
	PRINT 82,C_PETROL,C_PETROL*LIFE,LIFE
	WRITE(1,82) C_PETROL,C_PETROL*LIFE,LIFE
82	FORMAT(5X,' Cost of petrol : ',F10.1,'/year, and ',
     1	F10.1,' for ',I2,' years')
 
C 3. Total integrated cost, per year, and after LIFE years:
	C_TOTAL=C_PETROL+(INI_PRICE/FLOAT(LIFE))+EXPEND+INSUR
	PRINT 83,C_TOTAL,C_TOTAL*LIFE,LIFE
	WRITE(1,83) C_TOTAL,C_TOTAL*LIFE,LIFE
83	FORMAT(5X,' Total cost : ',F10.1,'/year, and ',F10.1,
     1	' for ',I2,' years')
 
C******************************************************************
C Compares with another solution, and the implications of
C the immobilisation of the price of the car during this period:
	
C 4. Interests provided by the difference of money:
	SUM=MONEY-INI_PRICE
	DO I=1,LIFE
C Car expenditures, and budget:
	  SUM=SUM-C_TOTAL+BUDGET
C Interests:
	  SUM=SUM*(1.+(SAV_RATE-INF_RATE)/100)
	END DO
	PRINT 84,LIFE,SUM
	WRITE(1,84) LIFE,SUM
84	FORMAT(5X,' Total reactualised sum available after ',
     1	I2,' years:',F10.1)
 
        PRINT *,' Logfile in "eco.log"'
	END
