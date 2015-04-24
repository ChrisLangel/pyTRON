      PROGRAM LISTPLOTVAR
C
#include "precis.h"
      CHARACTER(80) :: FILE,OFILE
      LOGICAL :: EXIST
      INTEGER :: IUNIT,OUNIT
      INTEGER :: NGRID,MGRID,NQ,NQC,N
      INTEGER :: NI,NJ,NK,NIDUMP,NJDUMP,NKDUMP
C
C    Get the input filename
C
      WRITE(*,20,ADVANCE='NO') 'Enter input PLOT3D Q filename: '
      READ(*,20,END=100) FILE
 20   FORMAT(A)
C
C    Check if file is there
C
      INQUIRE(FILE=FILE,EXIST=EXIST)
      IF(.NOT.EXIST) THEN
         WRITE(*,20) 'File not found'
         GOTO 100
      ENDIF
C
      IUNIT = 2
      OPEN(IUNIT,FILE=FILE,STATUS='OLD',FORM='UNFORMATTED')
C
      WRITE(*,20,ADVANCE='NO') 'Enter output file name: '
      READ(*,*,END=100) OFILE
      OUNIT = 801
C  Query for multiple-grid
C
      READ(IUNIT,ERR=30) NI,NJ,NK
      REWIND(IUNIT)
      NGRID = 1
      MGRID = 1
      GOTO 50
 30   REWIND(IUNIT)
      READ(2) NGRID
      WRITE(*,40) 'Total number of grids is ',NGRID
 40   FORMAT(A,I5)
      WRITE(*,20,ADVANCE='NO') 'Which grid to read? '
      READ(*,*,END=100) MGRID
 50   IF(NGRID.EQ.1) THEN
         READ(IUNIT,ERR=60) NI,NJ,NK,NQ,NQC
      ELSE
         READ(IUNIT,ERR=60) (NI,NJ,NK,N=1,MGRID),
     &     (NIDUMP,NJDUMP,NKDUMP,N=1,NGRID-MGRID),NQ,NQC
      ENDIF
      GOTO 70
 60   CONTINUE
      REWIND(IUNIT)
      IF(NGRID.EQ.1) THEN
         READ(IUNIT) NI,NJ,NK
      ELSE
         READ(IUNIT)
         READ(IUNIT) (NI,NJ,NK,N=1,MGRID)
      ENDIF
      NQ = 5
 70   CONTINUE
      IF(NQC.EQ.0) THEN
         WRITE(*,80) NI,NJ,NK,NQ
 80      FORMAT('Reading Q file, dimensioned ',4I7)
      ELSE
         WRITE(*,90) NI,NJ,NK,NQ,NQC
 90      FORMAT('Reading Q file, dimensioned ',4I7,',  nqc=',I4)
      ENDIF
C
C  Skipping unwanted grids (headers and q)
C
      DO N = 1,MGRID-1
         READ(IUNIT)
         READ(IUNIT)
      ENDDO
C
      CALL RDQ(NI,NJ,NK,NQ,NQC,IUNIT,OUNIT,OFILE)
 100  CONTINUE
      STOP
      END
C
C
C
      SUBROUTINE RDQ(NI,NJ,NK,NQ,NQC,IUNIT,OUNIT,OFILE)
#include "precis.h"
      CHARACTER(80), INTENT (IN) :: OFILE
      INTEGER, INTENT (IN) :: NI,NJ,NK,NQ,NQC,IUNIT,OUNIT
C
      __REAL,  DIMENSION(NI,NJ,NK,NQ) :: Q
      __REAL,  DIMENSION(:), ALLOCATABLE :: RGAS
      __REAL  :: REFMACH,ALFA,REY,TIME,GAMINF,BETA,TINF,
     &           HTINF,HT1,HT2,FSMACH,TVREF,DTVREF,U,V,W,CP,
     &           V2,P,RHO,GAMMA,RHOINF,CINF,PINF,VINF,QINF,RQINF,
     &           NDVM,WVAR,WVART
      INTEGER :: IGAM
C
      INTEGER :: IS,IE,II,I,JS,JE,JI,J,KS,KE,KI,K
      INTEGER :: NQS,NQE,CQ
      CHARACTER :: ANS
C
 2    FORMAT(A)
C
C   Read Q headers
C
      ALLOCATE( RGAS(MAX(2,NQC)) )
      READ(IUNIT) REFMACH,ALFA,REY,TIME,GAMINF,BETA,TINF,
     &            IGAM,HTINF,HT1,HT2,RGAS(1:MAX(2,NQC)),
     &            FSMACH,TVREF,DTVREF
      WRITE(*,40) 'refmach=',REFMACH,'alpha=',ALFA,'Re=',REY,
     &     'time=',TIME,'gaminf=',GAMINF,'beta=',BETA,'tinf=',TINF,
     &     'igam=',IGAM,'htinf=',HTINF,'ht1=',HT1,'ht2=',HT2,
     &     'rgas=',RGAS(1:MAX(2,NQC))
 40   FORMAT(A,F5.2,4X,A,F8.2,3X,A,G12.5,3X,A,G12.5,3X,A,F6.3,
     &       3X,A,F8.2,3X,A,F6.1/
     &       A,I2,3X,3(A,G11.3,3X)/A,(5(F8.4,3X)))
      WRITE(*,41) 'fsmach=',FSMACH,'tvref=',TVREF,'dtvref=',DTVREF
 41   FORMAT(A,F5.2,2(4X,A,G12.5))
      DEALLOCATE( RGAS )
C
      READ(IUNIT) Q
      CLOSE(IUNIT)
C     
C     Get subregion to write out.
C     
      OPEN(OUNIT,FILE=OFILE,STATUS='REPLACE',FORM='FORMATTED')
 5    WRITE(*,6,ADVANCE='NO')
    6 FORMAT('I-start,end,inc?  ')
      READ(*,*,END=200) IS,IE,II
      IF (IS.LT.0) IS = NI+1+IS
      IF (IE.LT.0) IE = NI+1+IE
      IF (IE.LT.IS .AND. II.GT.0) II = -II
      WRITE(*,7,ADVANCE='NO')
    7 FORMAT('J-start,end,inc?  ')
      READ(*,*,END=200) JS,JE,JI
      IF (JS.LT.0) JS = NJ+1+JS
      IF (JE.LT.0) JE = NJ+1+JE
      IF (JE.LT.JS .AND. JI.GT.0) JI = -JI
      WRITE(*,8,ADVANCE='NO')
    8 FORMAT('K-start,end,inc?  ')
      READ(*,*,END=200) KS,KE,KI
      IF (KS.LT.0) KS = NK+1+KS
      IF (KE.LT.0) KE = NK+1+KE
      IF (KE.LT.KS .AND. KI.GT.0) KI = -KI
      WRITE(*,9,ADVANCE='NO')
    9 FORMAT('Which FLow Quantity?  '/)
      READ(*,*,END=200) CQ
      DO K=KS,KE,KI
         DO J=JS,JE,JI
            DO I=IS,IE,II
               IF (CQ.EQ.15) THEN
                  GAMMA = GAMINF
                  RHOINF = 1.
                  CINF = 1.
                  PINF = RHOINF*CINF**2/GAMMA
                  VINF = FSMACH*CINF
                  QINF = 0.5*RHOINF*VINF**2
                  RQINF= 1./QINF                  
                  RHO = 1./Q(I,J,K,1)
                  U = Q(I,J,K,2)*RHO
                  V = Q(I,J,K,3)*RHO
                  W = Q(I,J,K,4)*RHO
                  V2= U**2+V**2+W**2
                  P = (GAMMA-1.)*(Q(I,J,K,5)-.5*Q(I,J,K,1)*V2)
                  CP = (P-PINF)*RQINF
                  WRITE(*,10) I,J,K,CP
                  WRITE(OUNIT,10) I,J,K,CP
               ELSEIF (CQ.EQ.16) THEN
                  RHO = 1./Q(I,J,K,1)
                  U = Q(I,J,K,2)*RHO
                  V = Q(I,J,K,3)*RHO
                  W = Q(I,J,K,4)*RHO
                  V2= U**2+V**2+W**2
                  WRITE(*,10) I,J,K,(V2**.5)
                  WRITE(OUNIT,10) I,J,K,(V2**.5)
               ELSEIF (CQ.EQ.17) THEN
                  RHO = 1./Q(I,J,K,1)
                  U = Q(I,J,K,2)*RHO
                  V = Q(I,J,K,3)*RHO
                  WRITE(*,10) I,J,K,U,V
                  WRITE(OUNIT,10) I,J,K,U,V
               ELSEIF (CQ.EQ.18) THEN
                  RHO = 1./Q(I,J,K,1)
                  U = Q(I,J,K,2)*RHO
                  V = Q(I,J,K,3)*RHO
                  W = Q(I,J,K,4)*RHO
                  V2= U**2+V**2+W**2
                  CP= Q(I,J,K,11)
                  WRITE(*,10) I,J,K,(CP*V2)
                  WRITE(OUNIT,10) I,J,K,(CP*V2)
               ELSEIF (CQ.EQ.19) THEN
                  RHO = 1./Q(I,J,K,1)
                  U = Q(I,J,K,2)*RHO
                  V = Q(I,J,K,3)*RHO
                  W = Q(I,J,K,4)*RHO
                  V2= Q(I,J,K,9) 
                  CP= 1.0/(Q(I,J,K,11))
                  WRITE(*,10) I,J,K,(CP*V2)*U**2
                  WRITE(OUNIT,10) I,J,K,(CP*V2)*U**2
               ELSEIF (CQ.EQ.20) THEN
                  RHO = 1./Q(I,J,K,1)
                  U = Q(I,J,K,2)*RHO
                  V = Q(I,J,K,3)*RHO
                  W = Q(I,J,K,4)*RHO
                  V2= U**2+V**2+W**2  
                  CP= Q(I,J,K,11)
                  NDVM=V2/(FSMACH**2) 
                  IF ( (CP-NDVM) .GT. .25) THEN 
                     WVAR = 0.0 
                  ELSE
                     WVAR = V2             
                  ENDIF
                  IF (Q(I,J,K,9) > .7) THEN
                     WVAR = V2*Q(I,J,K,9) 
                  ENDIF 
                  WRITE(*,10) I,J,K,WVAR
                  WRITE(OUNIT,10) I,J,K,WVAR
               ELSE
                  WRITE(*,10) I,J,K,Q(I,J,K,CQ)
                  WRITE(OUNIT,10) I,J,K,Q(I,J,K,CQ)
               ENDIF   
            ENDDO
         ENDDO
      ENDDO
 10   FORMAT(1X,3I4,8G13.5)
 11   FORMAT(13X,8G13.5)
 200  CONTINUE
      WRITE(*,2) 'List other ranges? (y/n) - '
      READ(*,2) ANS
      IF(ANS.EQ.'y'.OR.ANS.EQ.'Y') GOTO 5
      RETURN
      END
