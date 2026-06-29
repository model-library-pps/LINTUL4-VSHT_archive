** $Id: movavr.for 1.1 1997/10/02 16:08:12 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE MOVAVR (ITASK,NAME,IP,IN,OUT)
      IMPLICIT NONE
      INTEGER ITASK,IP
      REAL IN, OUT
      CHARACTER*(*) NAME
      INTEGER MNP,MNNAM,NNAM
      PARAMETER (MNP=10,MNNAM=10)
      REAL ARR(MNP,MNNAM)
      INTEGER NPA(MNNAM)
      CHARACTER*31 NAMESA(MNNAM),LNAME
      INTEGER I1,N,IFINDC,NP
      SAVE
      IF (ITASK.EQ.1) THEN
      DO 10 I1=1,MNNAM
      NPA(I1)    = 0
      NAMESA(I1) = ' '
10    CONTINUE
      NNAM = 0
      ELSE
      IF (IP.GT.MNP) CALL FATALERR ('MOVAVR',
     &'maximum number of points exceeded')
      LNAME = NAME
      N = IFINDC (NAMESA,MNNAM,1,NNAM,LNAME)
      IF (N.EQ.0) THEN
      NNAM = NNAM+1
      IF (NNAM.GT.MNNAM) CALL FATALERR ('MOVAVR','too many names')
      N = NNAM
      NAMESA(N) = LNAME
      END IF
      NP = MIN (NPA(N)+1, IP)
      DO 20 I1=NP,2,-1
      ARR(I1,N) = ARR(I1-1,N)
20    CONTINUE
      ARR(1,N) = IN
      OUT = 0.
      DO 30 I1=1,NP
      OUT = OUT+ARR(I1,N)
30    CONTINUE
      OUT    = OUT/REAL (NP)
      NPA(N) = NP
      END IF
      RETURN
      END
