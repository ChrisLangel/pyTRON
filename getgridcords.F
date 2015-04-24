      SUBROUTINE GETGRIDCORDS(GFILE,MGRID) 
C
#include "precis.h"
      CHARACTER(80), INTENT(IN) :: GFILE
      INTEGER, INTENT(IN) :: MGRID
      CHARACTER(80) :: FILE,OFILE
      LOGICAL :: EXIST
      INTEGER :: IUNIT,OUNIT,J,K,L
      INTEGER :: NGRID,NQ,NQC,N,IBLANK
      INTEGER :: NI,NJ,NK,NIDUMP,NJDUMP,NKDUMP,IG
      REAL,  DIMENSION(:,:,:), ALLOCATABLE :: X,Y,Z
      INTEGER,  DIMENSION(:), ALLOCATABLE :: JD,KD,LD
   
C
C    Get the input filename
C
C      WRITE(*,1)
C 1    FORMAT('Enter PLOT3D grid filename:  ',$)
C      READ(*,2,END=100) GFILE
 2    FORMAT(A)
C
      IUNIT = 1
      OPEN(1,FILE=GFILE,STATUS='OLD',FORM='UNFORMATTED')
      READ(1,ERR=30) NI,NJ,NK
      REWIND(1)
      NGRID = 1
      MGRID = 1
      GOTO 5
 30   REWIND(1)
      READ(1) NGRID
      WRITE(*,4) 'Total number of grids is ',NGRID
 4    FORMAT(A,I5)
      WRITE(*,2,ADVANCE='NO') 'Which grid to read? '
      READ(*,*,END=100) MGRID
 5    IF(NGRID.EQ.1) THEN
         READ(1,ERR=6) NI,NJ,NK
      ELSE
         READ(1,ERR=6) (NI,NJ,NK,N=1,MGRID)
      ENDIF      
      GOTO 7
 6    CONTINUE
      REWIND(1)
      IF(NGRID.EQ.1) THEN
         READ(1) NI,NJ,NK
      ELSE
         READ(1)
         READ(1) (NI,NJ,NK,N=1,MGRID)
      ENDIF
      NQ = 5
 7    CONTINUE
      WRITE(*,8) NI,NJ,NK
 8    FORMAT('Reading Grid file, dimensioned ',3I7)
C
C
      WRITE(*,20,ADVANCE='NO') 'Enter output file name: '
 20   FORMAT(A)
      READ(*,*,END=100) OFILE
      OUNIT = 801


      DO N = 1,MGRID-1
         READ(1)
C        READ(1)
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
      __REAL,  DIMENSION(NI,NJ,NK) :: X,Y,Z
C
      __REAL,  DIMENSION(NI,NJ,NK,NQ) :: Q
      __REAL,  DIMENSION(:), ALLOCATABLE :: RGAS
      __REAL  :: REFMACH,ALFA,REY,TIME,GAMINF,BETA,TINF,
     &           HTINF,HT1,HT2,FSMACH,TVREF,DTVREF
      INTEGER :: IGAM
C
      INTEGER :: IS,IE,II,I,JS,JE,JI,J,KS,KE,KI,K
      INTEGER :: NQS,NQE,CQ,JJ,LL,KK
      CHARACTER :: ANS

C
C  
      READ(1) (((X(JJ,KK,LL),JJ=1,NI),KK=1,NJ),LL=1,NK), 
     &        (((Y(JJ,KK,LL),JJ=1,NI),KK=1,NJ),LL=1,NK),
     &        (((Z(JJ,KK,LL),JJ=1,NI),KK=1,NJ),LL=1,NK)
C      CLOSE(IUNIT)
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
      DO K=KS,KE,KI
         DO J=JS,JE,JI
            DO I=IS,IE,II
C
               WRITE(*,10) I, J, K, X(I,J,K), Z(I,J,K), Y(I,J,K)
               WRITE(OUNIT,10) I, J, K, X(I,J,K), Z(I,J,K), Y(I,J,K)
C
            ENDDO
         ENDDO
      ENDDO
 10   FORMAT(1X,3I5,F16.9,F16.11,F16.11)
 11   FORMAT(13X,8G13.5)
 200  CONTINUE
      RETURN
      END
