C 
C 
C --------------------- MIDAS definition Table ---------------------------
C 	
C                       on file ST_DEF.INC
C 
C   to declare the variables
C   version [2.00]  880414:  creation  (KB)
C   version [2.20]  880908:  add definitions for filetype no's (KB)
C   version [2.40]  890203:  add definitions for table formats (JDP)
C 
C 
C   Mode definitions:   
C 
C
      INTEGER F_I_MODE                     ! map file for input only    
      INTEGER F_O_MODE                     ! map file for output        
      INTEGER F_IO_MODE                    ! map file for updating      
      INTEGER F_U_MODE                     ! map file for updating      
      INTEGER F_X_MODE                     ! create/map virtual memory  
      INTEGER F_D_MODE                     ! table descriptor access
C
C 
C   File type definitions:   
C 
C
      INTEGER F_IMA_TYPE                   ! image file type
      INTEGER F_TBL_TYPE                   ! table file type
      INTEGER F_FIT_TYPE                   ! fit file type
C
C 
C   Data Format definitions:   
C 
C
C
      INTEGER D_OLD_FORMAT     
      INTEGER D_I1_FORMAT     
      INTEGER D_I2_FORMAT     
      INTEGER D_I4_FORMAT     
      INTEGER D_R4_FORMAT    
      INTEGER D_R8_FORMAT
      INTEGER D_L1_FORMAT
      INTEGER D_L2_FORMAT
      INTEGER D_L4_FORMAT
      INTEGER D_C_FORMAT
      INTEGER D_X_FORMAT
      INTEGER D_P_FORMAT
C
C     table file format
C
      INTEGER F_TRANS                 ! transposed format (by columns)
      INTEGER F_RECORD                ! record format  
C  ------------------------------------------------------------------  
