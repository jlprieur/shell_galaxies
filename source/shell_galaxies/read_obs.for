C++----------------------------------------------------------
C Program to read the shell observations in the
C general catalogue "SHELL.CAT"
C
C JLP Version 13-07-88
C---------------------------------------------------------
 	PROGRAM READ_OBS
	PARAMETER (IDIM1=50,IDIM2=200)
	CHARACTER NAME(IDIM2)*40,NGC_NAME(IDIM2)*40
	CHARACTER OBS(IDIM1,IDIM2)*80
	CHARACTER ANS*1
	COMMON/RDOBS_DATA/NAME,NGC_NAME,OBS
 
10	FORMAT(A)
 
C Reading the catalogue:
	CALL RDOBS_INPUT
 
	END
 
C----------------------------------------------------------
C Read the observation catalogue
C---------------------------------------------------------
	SUBROUTINE RDOBS_INPUT
	PARAMETER (IDIM1=50,IDIM2=200)
	CHARACTER NAME(IDIM2)*40,NGC_NAME(IDIM2)*40
	CHARACTER OBS(IDIM1,IDIM2)*80
	CHARACTER*80 BUFFER
	CHARACTER FILENAME*40,ANS*1
	COMMON/RDOBS_DATA/NAME,NGC_NAME,OBS
 
10	FORMAT(A)
 
	  DO K=1,IDIM2
	    NAME(K)=' '
	    NGC_NAME(K)=' '
	  END DO
 
20	PRINT *,' NAME OF THE CATALOGUE ?'
	READ(5,10) FILENAME
	OPEN(1,FILE=FILENAME,STATUS='OLD',ERR=20)
 
C**************************
C Reading the catalogue:
	KK=0
 
	DO I=1,IDIM1*IDIM2
	  READ(1,10,END=99) BUFFER
C	  PRINT *,BUFFER	
 
C Name:
	  IF(BUFFER(1:1).EQ.'I')THEN
	    KK=KK+1
	    NAME(KK)=BUFFER(3:42)
	    KOBS=0
 
C NGC_name:
	  ELSEIF(BUFFER(1:1).EQ.'A')THEN
	    NGC_NAME(KK)=BUFFER(3:42)
 
C Observations:
C Format: telescope, date, filter, exposure time, seeing (approx), zenith dist.,
C remarks, number/name
	  ELSEIF(BUFFER(1:1).EQ.'O')THEN
	    KOBS=KOBS+1
	    OBS(KOBS,KK)=' '
	    OBS(KOBS,KK)=BUFFER(3:80)
	  ENDIF
 
 
	END DO
	
99	CLOSE(1)
 
	NOBJECTS=KK
	PRINT *,' ',NOBJECTS,' OBJECTS READ'
 
	RETURN
	END	
