** $Id: usedun.for 1.1 1997/10/02 16:08:14 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE USEDUN (IST,IEND)
      IMPLICIT NONE
      INTEGER IST,IEND
      INTEGER ISTEP,UNIT,ILEN,IL
      LOGICAL OPEN,FOUND
      CHARACTER*80 FILE
      SAVE
      ISTEP = 1
      IF (IST.GT.IEND) ISTEP = -1
      IF (IST.LT.10.OR.IST.GT.99.OR.IEND.LT.10.OR.IEND.GT.99)
     &CALL FATALERR ('GETUN','unit number outside range')
      FOUND = .FALSE.
      DO UNIT=IST,IEND,ISTEP
      INQUIRE (UNIT=UNIT,OPENED=OPEN)
      IF (OPEN) THEN
      FILE = ' '
      INQUIRE (UNIT=UNIT,NAME=FILE)
      IL = ILEN (FILE)
      WRITE (*,'(A,I3,2A)')
     &' WARNING from USEDUN: Unit: ',UNIT,
     &' is in use for file: ',FILE(1:IL)
      FOUND = .TRUE.
      END IF
      END DO
      IF (.NOT.FOUND) WRITE (*,*) 'USEDUN: no open units found'
      RETURN
      END
