* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE AFINVS (FUN,ILF,FUNINV,ILFI,EXIST)
      IMPLICIT NONE
      INTEGER ILF,ILFI
      REAL FUN,FUNINV
      DIMENSION FUN(2,ILF/2),FUNINV(2,ILFI/2)
      LOGICAL EXIST
      INTEGER I,IFROM
      REAL HELP1,HELP2
      LOGICAL INCR,INCR1,EQUAL,MATCH
      SAVE
      IF  (MOD(ILF,2).NE.0) CALL FATALERR('AFINVS','array size is odd')
      IF (MOD(ILFI,2).NE.0) CALL FATALERR('AFINVS','array size is odd')
      IF (ILF.NE.ILFI)      CALL FATALERR('AFINVS','ILF not equal ILFI')
      DO 10 I=2,ILF/2
      IF (FUN(1,I).LE.FUN(1,I-1))
     $CALL FATALERR ('AFINVS','input X values not increase')
10    CONTINUE
      EXIST = .TRUE.
      INCR  = (FUN(2,2).GT.FUN(2,1))
      I     = 3
20    IF (I.LE.ILF/2 .AND. EXIST) THEN
      INCR1 = (FUN(2,I).GT.FUN(2,I-1))
      MATCH = ((INCR1.AND.INCR) .OR. .NOT.(INCR1.OR.INCR))
      EQUAL = (FUN(2,I).EQ.FUN(2,I-1))
      EXIST = .NOT.EQUAL .AND. MATCH
      I = I + 1
      GOTO 20
      END IF
      IF (.NOT.EXIST) RETURN
      DO 30 I=1,ILF/2
      HELP1 = FUN(1,I)
      FUNINV(1,I) = FUN(2,I)
      FUNINV(2,I) = HELP1
30    CONTINUE
      IF (ILF.GE.4 .AND. .NOT.INCR) THEN
      DO 40 I=1,ILF/4
      IFROM = ILF/2 - I + 1
      HELP1 = FUN(1,I)
      HELP2 = FUN(2,I)
      FUNINV(1,I) = FUN(1,IFROM)
      FUNINV(2,I) = FUN(2,IFROM)
      FUNINV(1,IFROM) = HELP1
      FUNINV(2,IFROM) = HELP2
40    CONTINUE
      END IF
      RETURN
      END
