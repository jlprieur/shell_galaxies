      PROGRAMTEST
      PARAMETER(MAX_ARRAY=100)
      IMPLICITNONE
      INTEGERI,N
      REALARRAY(MAX_ARRAY,MAX_ARRAY)
      REALTHIS_IS_REAL,RESULT
      print*,' This is a test of lower case statement'
      DO80000I=1,MAX_ARRAY
      DO80001N=1,MAX_ARRAY
      IF(I.EQ.N)GOTO200
      ARRAY(I,N)=I*K
  200 CONTINUE
80001 CONTINUE
80000 CONTINUE
      RESULT=0.0E0
      DO80002I=1,MAX_ARRAY
      DO100,N=1,MAX_ARRAY
      THIS_IS_REAL=RESULT+ARRAY(I,N)
  100 CONTINUE
80002 CONTINUE
      CALLDUMMTEST(RESULT)
      STOP
      END
      SUBROUTINEDUMMTEST(X)
      IMPLICITNONE
      REALX
      X=X*X
      RETURN
      END
