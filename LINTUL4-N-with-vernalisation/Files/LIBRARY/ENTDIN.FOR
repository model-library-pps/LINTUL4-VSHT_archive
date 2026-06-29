* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE ENTDIN (QUEST,IXDEF,IX)
      IMPLICIT NONE
      INTEGER IXDEF,IX
      CHARACTER*(*) QUEST
      INTEGER IDLOC,IL,IS,IWAR
      CHARACTER HULP*16,INPUT*20
      INTEGER ILEN, ISTART
      SAVE
      IDLOC = IXDEF
      WRITE (HULP,'(I16)',IOSTAT=IL) IDLOC
10    CONTINUE
      CALL ENTDCH (QUEST,HULP,INPUT)
      IS = ISTART (INPUT)
      IL = ILEN (INPUT)
      IF (IL.EQ.0) THEN
      IX = IDLOC
      RETURN
      END IF
      CALL DECINT (IWAR,INPUT(IS:IL),IX)
      IF (IWAR.NE.0) THEN
      WRITE (*,'(/,A,/,A,/)')
     $' Enter an INTEGER value !!',
     $' or <CTRL> Z  to STOP (<Command>. on Mac)'
      GOTO 10
      END IF
      RETURN
      END
