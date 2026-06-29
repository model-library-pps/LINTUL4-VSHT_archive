* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE ENTCHA (QUEST,X)
      IMPLICIT NONE
      CHARACTER QUEST*(*),X*(*)
      INTEGER LQ,LX
      SAVE
      LQ = LEN (QUEST)
      LX = LEN (X)
10    CONTINUE
      WRITE (*,'(A50,A,$)') QUEST(1:LQ),': '
      READ (*,'(A)',ERR=20,END=30) X(1:LX)
      RETURN
20    CONTINUE
      WRITE (*,'(/,A,/,A,/)')
     $' Enter a CHARACTER string !!',
     $' or <CTRL> Z  to STOP (<Command>. on Mac)'
      GOTO 10
30    CONTINUE
      STOP ' End_Of_File detected ; program STOP'
      END
