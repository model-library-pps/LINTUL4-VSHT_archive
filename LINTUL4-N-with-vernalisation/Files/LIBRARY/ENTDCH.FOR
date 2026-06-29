* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE ENTDCH (QUEST,SDEF,S)
      IMPLICIT NONE
      CHARACTER*(*) QUEST,SDEF,S
      INTEGER IL,IP1,IP2,ILEN,ISTART,LQ
      CHARACTER SDLOC*80,INPUT*80,VRAAG*50
      SAVE
      SDLOC = SDEF
      IP1 = ISTART (SDLOC)
      IP2 = ILEN (SDLOC)
      IP1 = MAX (1,IP1)
      IP2 = MAX (1,IP2)
      LQ = ILEN (QUEST)
      LQ = MAX (1,LQ)
      LQ = MIN (LQ,47)
10    CONTINUE
      VRAAG(1:LQ) = QUEST(1:LQ)
      IF (LQ+IP2-IP1+5.LE.50) THEN
      WRITE (*,'(A50,A,$)')
     $VRAAG(1:LQ)//' ['//SDLOC(IP1:IP2)//']',': '
      ELSE
      WRITE (*,'(A48,/,A50,A,$)') VRAAG(1:LQ),
     $' ['//SDLOC(IP1:IP2)//']',': '
      END IF
      READ (*,'(A)',ERR=20,END=30) INPUT
      IL = ILEN (INPUT)
      IF (IL.EQ.0) THEN
      S = SDLOC
      ELSE
      S = INPUT
      END IF
      RETURN
20    CONTINUE
      WRITE (*,'(/,A,/,A,/)')
     $' Enter a CHARACTER string !!',
     $' or <CTRL> Z  to STOP (<Command>. on Mac)'
      GOTO 10
30    CONTINUE
      STOP ' End_Of_File detected ; program STOP'
      END
