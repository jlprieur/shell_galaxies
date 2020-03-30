C++**********************************************************
C JLP0_AC_VAX.FOR
C Set of subroutines to replace JLP VAX-dependent input/output routines 
C which access 2-D images in CCD, BDF, and CDCA format
C (i.e., which are only C valid on the Vax system) by dummy routines.
C UNIX version 
C
C Contains
C       JLP_RDICCD, JLP_RDCCD, JLP_RDIBDF, JLP_RDRBDF
C       JLP_RDCDCA, JLP_WRCDCA,
C       JLP_WRICCD, JLP_WRCCD, JLP_WRIBDF, JLP_WRRBDF,
C       JLP_VM_RDICCD, JLP_VM_RDCCD, JLP_VM_RDBDF, JLP_VM_RDIBDF
C       MTACCESS
C
C Version of 02-04-90
C JLP
C--*********************************************************
C----------------------------------------------------------
C Subroutine JLP_RDCCD
C----------------------------------------------------------
	SUBROUTINE JLP_RDCCD(IMAGE,NX,NY,IDIM,NAME,COMMENTS,ISTAT)
	INTEGER*4 NX,NY,IDIM,ISTAT
	CHARACTER NAME*(*),COMMENTS
	REAL*4 IMAGE(IDIM,*)
	WRITE(6,*) ' Sorry JLP_RDCCD not available for Unix'
	ISTAT=2
	RETURN	
        END
C----------------------------------------------------------
C Subroutine JLP_RDICCD
C----------------------------------------------------------
	SUBROUTINE JLP_RDICCD(IMAGE,NX,NY,IDIM,NAME,COMMENTS,ISTAT)
	INTEGER*4 NX,NY,IDIM,ISTAT
	CHARACTER NAME*(*),COMMENTS
	REAL*4 IMAGE(IDIM,*)
	WRITE(6,*) ' Sorry JLP_RDICCD not available for Unix'
	ISTAT=2
	RETURN	
        END
C----------------------------------------------------------
C Subroutine JLP_RDRBDF
C----------------------------------------------------------
	SUBROUTINE JLP_RDRBDF(IMAGE,NX,NY,IDIM,NAME,COMMENTS,ISTAT)
	INTEGER*4 NX,NY,IDIM,ISTAT
	CHARACTER NAME*(*),COMMENTS
	REAL*4 IMAGE(IDIM,*)
	WRITE(6,*) ' Sorry JLP_RDRBDF not available for Unix'
	ISTAT=2
	RETURN	
        END
C----------------------------------------------------------
C Subroutine JLP_RDIBDF
C----------------------------------------------------------
	SUBROUTINE JLP_RDIBDF(IMAGE,NX,NY,IDIM,NAME,COMMENTS,ISTAT)
	INTEGER*4 NX,NY,IDIM,ISTAT
	CHARACTER NAME*(*),COMMENTS
	REAL*4 IMAGE(IDIM,*)
	WRITE(6,*) ' Sorry JLP_RDIBDF not available for Unix'
	ISTAT=2
	RETURN	
        END
C----------------------------------------------------------
C Subroutine JLP_RDCDCA
C----------------------------------------------------------
	SUBROUTINE JLP_RDCDCA(IMAGE,NX,NY,IDIM,NAME,COMMENTS,ISTAT)
	INTEGER*4 NX,NY,IDIM,ISTAT
	CHARACTER NAME*(*),COMMENTS
	REAL*4 IMAGE(IDIM,*)
	WRITE(6,*) ' Sorry JLP_RDCDCA not available for Unix'
	ISTAT=2
	RETURN	
        END
C----------------------------------------------------------
C Subroutine JLP_WRCDCA
C----------------------------------------------------------
	SUBROUTINE JLP_WRCDCA(IMAGE,NX,NY,IDIM,NAME,COMMENTS,ISTAT)
	INTEGER*4 NX,NY,IDIM,ISTAT
	CHARACTER NAME*(*),COMMENTS
	REAL*4 IMAGE(IDIM,*)
	WRITE(6,*) ' Sorry JLP_WRCDCA not available for Unix'
	ISTAT=2
	RETURN	
        END
C----------------------------------------------------------
C Subroutine JLP_WRCCD
C----------------------------------------------------------
	SUBROUTINE JLP_WRCCD(IMAGE,NX,NY,IDIM,NAME,COMMENTS,ISTAT)
	INTEGER*4 NX,NY,IDIM,ISTAT
	CHARACTER NAME*(*),COMMENTS
	REAL*4 IMAGE(IDIM,*)
	WRITE(6,*) ' Sorry JLP_WRCCD not available for Unix'
	ISTAT=2
	RETURN	
        END
C----------------------------------------------------------
C Subroutine JLP_WRICCD
C----------------------------------------------------------
	SUBROUTINE JLP_WRICCD(IMAGE,NX,NY,IDIM,NAME,COMMENTS,ISTAT)
	INTEGER*4 NX,NY,IDIM,ISTAT
	CHARACTER NAME*(*),COMMENTS
	REAL*4 IMAGE(IDIM,*)
	WRITE(6,*) ' Sorry JLP_WRICCD not available for Unix'
	ISTAT=2
	RETURN	
        END
C----------------------------------------------------------
C Subroutine JLP_WRRBDF
C----------------------------------------------------------
	SUBROUTINE JLP_WRRBDF(IMAGE,NX,NY,IDIM,NAME,COMMENTS,ISTAT)
	INTEGER*4 NX,NY,IDIM,ISTAT
	CHARACTER NAME*(*),COMMENTS
	REAL*4 IMAGE(IDIM,*)
	WRITE(6,*) ' Sorry JLP_WRRBDF not available for Unix'
	ISTAT=2
	RETURN	
        END
C----------------------------------------------------------
C Subroutine JLP_WRIBDF
C----------------------------------------------------------
	SUBROUTINE JLP_WRIBDF(IMAGE,NX,NY,IDIM,NAME,COMMENTS,ISTAT)
	INTEGER*4 NX,NY,IDIM,ISTAT
	CHARACTER NAME*(*),COMMENTS
	REAL*4 IMAGE(IDIM,*)
	WRITE(6,*) ' Sorry JLP_WRIBDF not available for Unix'
	ISTAT=2
	RETURN	
        END
C----------------------------------------------------------
C Subroutine JLP_VM_RDCDCA
C----------------------------------------------------------
	SUBROUTINE JLP_VM_RDCDCA(IMAGE,NX,NY,IDIM,NAME,COMMENTS,ISTAT)
	INTEGER*4 NX,NY,IDIM,ISTAT
	CHARACTER NAME*(*),COMMENTS
	REAL*4 IMAGE(IDIM,*)
	WRITE(6,*) ' Sorry JLP_VM_RDCDCA not available for Unix'
	ISTAT=2
	RETURN	
        END
C----------------------------------------------------------
C Subroutine JLP_VM_RDICCD
C----------------------------------------------------------
	SUBROUTINE JLP_VM_RDICCD(IMAGE,NX,NY,IDIM,NAME,COMMENTS,ISTAT)
	INTEGER*4 NX,NY,IDIM,ISTAT
	CHARACTER NAME*(*),COMMENTS
	REAL*4 IMAGE(IDIM,*)
	WRITE(6,*) ' Sorry JLP_VM_RDICCD not available for Unix'
	ISTAT=2
	RETURN	
        END
C----------------------------------------------------------
C Subroutine JLP_VM_RDCCD
C----------------------------------------------------------
	SUBROUTINE JLP_VM_RDCCD(IMAGE,NX,NY,IDIM,NAME,COMMENTS,ISTAT)
	INTEGER*4 NX,NY,IDIM,ISTAT
	CHARACTER NAME*(*),COMMENTS
	REAL*4 IMAGE(IDIM,*)
	WRITE(6,*) ' Sorry JLP_VM_RDCCD not available for Unix'
	ISTAT=2
	RETURN	
        END
C----------------------------------------------------------
C Subroutine JLP_VM_RDBDF
C----------------------------------------------------------
	SUBROUTINE JLP_VM_RDBDF(IMAGE,NX,NY,IDIM,NAME,COMMENTS,ISTAT)
	INTEGER*4 NX,NY,IDIM,ISTAT
	CHARACTER NAME*(*),COMMENTS
	REAL*4 IMAGE(IDIM,*)
	WRITE(6,*) ' Sorry JLP_VM_RDBDF not available for Unix'
	ISTAT=2
	RETURN	
        END
C----------------------------------------------------------
C Subroutine JLP_VM_RDIBDF
C----------------------------------------------------------
	SUBROUTINE JLP_VM_RDIBDF(IMAGE,NX,NY,IDIM,NAME,COMMENTS,ISTAT)
	INTEGER*4 NX,NY,IDIM,ISTAT
	CHARACTER NAME*(*),COMMENTS
	REAL*4 IMAGE(IDIM,*)
	WRITE(6,*) ' Sorry JLP_VM_RDIBDF not available for Unix'
	ISTAT=2
	RETURN	
        END
C***************************************************************
C Subroutine MTACCESS
C***************************************************************
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
C	ILUN1= NO. UNITE LOGIQUE ASSIGNEE A UN NOM "LOGICAL"
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
***********************************************************************
C
	SUBROUTINE MTACCESS (ILUN,IFUNC,IBUF,INCAR,ISKIP,IEOF,DEVNAM)
C***********************************************************************
 
	INTEGER*4 ILUN
	INTEGER*2 IFUNC,INCAR,ISKIP,IEOF
	LOGICAL*1 IBUF(1)
	CHARACTER*(*) DEVNAM
C
	PRINT *,' Sorry, MTACCESS not available here'
	IEOF=4
	RETURN
	END
