* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE EXTENS (FILEIN,NEWEXT,ICHECK,FILEOU)
      IMPLICIT NONE
      INTEGER ICHECK
      CHARACTER*(*) FILEIN,FILEOU,NEWEXT
      INTEGER I,ILEXT1,ILEXT2,ILFIL,IP,LOUT,ILEN,IS
      CHARACTER*1 OLD,NEW,CHR
      CHARACTER*132 FILBUF
      LOGICAL CHECK, HAAK, FOUND
      SAVE
      IF (NEWEXT(1:1).NE.'.') THEN
      IS = 1
      ELSE
      IS = 2
      END IF
      ILFIL  = ILEN (FILEIN)
      ILEXT2 = ILEN (NEWEXT) - IS + 1
      FILBUF = NEWEXT
      LOUT   = LEN (FILEOU)
      IF (ILFIL.EQ.0)  CALL FATALERR ('EXTENS','no filename supplied')
      IP     = ILFIL + 1
      ILEXT1 = 0
      HAAK   = .FALSE.
      FOUND  = .FALSE.
      I = ILFIL
10    IF (I.GT.0 .AND..NOT.(FOUND.OR.HAAK)) THEN
      CHR  = FILEIN(I:I)
      HAAK = CHR.EQ.CHAR(47) .OR. CHR.EQ.CHAR(58) .OR.
     $CHR.EQ.CHAR(92) .OR. CHR.EQ.CHAR(93)
      IF (CHR.EQ.'.') THEN
      IP = I
      ILEXT1 = ILFIL - IP
      FOUND  = .TRUE.
      END IF
      I = I - 1
      GOTO 10
      END IF
      IF (IP.GT.132) THEN
      WRITE (*,'(1X,2A)') 'Input file name:',FILEIN
      CALL FATALERR ('EXTENS','input filename too long')
      END IF
      FILBUF = ' '
      IF (IP.GE.2) FILBUF = FILEIN(1:IP-1)
      CALL UPPERC (FILBUF)
      IF (ILEXT2.GT.0) FILBUF(IP:IP) = '.'
      CHECK = (ILEXT1.EQ.ILEXT2) .AND. (ICHECK.NE.0)
      DO 20 I=IP+1,IP+ILEXT2
      IF (I.GT.132) THEN
      WRITE (*,'(1X,2A)') 'Input file name:',FILEIN
      CALL FATALERR ('EXTENS','new filename too long')
      END IF
      NEW = NEWEXT(I-IP+IS-1:I-IP+IS-1)
      CALL UPPERC (NEW)
      FILBUF(I:I) = NEW
      IF (CHECK) THEN
      OLD = FILEIN(I:I)
      CALL UPPERC (OLD)
      IF (OLD.NE.NEW) CHECK=.FALSE.
      END IF
20    CONTINUE
      IF (CHECK) THEN
      WRITE (*,'(1X,2A)') 'Input file name:',FILEIN
      CALL FATALERR ('EXTENS','old and new extension equal')
      END IF
      IF (LOUT.LT.IP+ILEXT2) THEN
      WRITE (*,'(1X,2A)') 'Input file name:',FILEIN
      CALL FATALERR ('EXTENS','output string too short for new name')
      END IF
      FILEOU = FILBUF(1:IP+ILEXT2)
      RETURN
      END
