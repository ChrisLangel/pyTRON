      PROGRAM GETQINFO
C
#include "precis.h"
      CHARACTER(80) :: FILE
      LOGICAL :: EXIST
      INTEGER :: IUNIT
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
      CALL RDQ(NI,NJ,NK,NQ,NQC,IUNIT,NGRID)
 100  CONTINUE
      STOP
      END
C
C
C
      SUBROUTINE RDQ(NI,NJ,NK,NQ,NQC,IUNIT,NGRID)
#include "precis.h"
      INTEGER, INTENT (IN) :: NI,NJ,NK,NQ,NQC,IUNIT,NGRID
C
      __REAL,  DIMENSION(NI,NJ,NK,NQ) :: Q
      __REAL,  DIMENSION(:), ALLOCATABLE :: RGAS
      __REAL  :: REFMACH,ALFA,REY,TIME,GAMINF,BETA,TINF,
     &           HTINF,HT1,HT2,FSMACH,TVREF,DTVREF
      INTEGER :: IGAM
C
      INTEGER :: IS,IE,II,I,JS,JE,JI,J,KS,KE,KI,K
      INTEGER :: NQS,NQE
      CHARACTER(80) :: ANS,QOUT
C
 2    FORMAT(A)
C
C   Read Q headers
C
      OPEN(801,FILE='QOUT.txt',STATUS='REPLACE',FORM='FORMATTED')    

      ALLOCATE( RGAS(MAX(2,NQC)) )
      READ(IUNIT) REFMACH,ALFA,REY,TIME,GAMINF,BETA,TINF,
     &            IGAM,HTINF,HT1,HT2,RGAS(1:MAX(2,NQC)),
     &            FSMACH,TVREF,DTVREF
C
      WRITE(801,40) REFMACH,ALFA,REY,TIME,GAMINF,BETA,TINF,
     &               NI,NJ,NK,NGRID
 40   FORMAT(7G13.6,4I7)
      DEALLOCATE( RGAS )
C
      READ(IUNIT) Q
      CLOSE(IUNIT)      
 200  CONTINUE
      CLOSE(801)
      RETURN
      END
