	PROGRAM TEST 
	REAL X,Y
        INTEGER*8 itest
	CHARACTER FILE(1000)*1,BUFFER*100
10	FORMAT(A)
 
        itest=32087654
        print *,'itest',itest
        DO J=1,10
         WRITE(BUFFER,23)J
23       FORMAT('Line :',I5,'Bonjour 9Bon c"est moi')
         DO I=1,80
          FILE(I+(J-1)*80)=BUFFER(I:I)
         END DO
        END DO
        PRINT *,' FILE: >',(FILE(I),I=1,1000),'<'
        K=JLP_INDEX_DESCR(FILE,1,'9 onjour',8)
        PRINT *,' K= ',K
	STOP
	END
