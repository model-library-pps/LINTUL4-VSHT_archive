* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE ENTDRE (QUEST,XDEF,X)
      IMPLICIT NONE
      REAL XDEF,X
      CHARACTER*(*) QUEST
      INTEGER I,IDEC,IDIGIT,IL,IT,IS,IWAR
      REAL XDLOC,LABS
      CHARACTER FORM*8,HULP*30,INPUT*20
      PARAMETER (IDIGIT=4)
      INTEGER ILEN, ISTART
      SAVE
      XDLOC = XDEF
      IF (XDLOC.NE.0.0) THEN
      LABS = LOG10 (ABS(XDLOC))
      IDEC = MAX (0,INT(REAL(IDIGIT)-LABS))
      IT = MAX (1,INT(1.+LABS)) + 2 + IDEC
      IF (XDLOC.LT.0.0) IT=IT+1
      ELSE
      IT = 2
      IDEC = 0
      END IF
      WRITE (FORM,'(A,I2,A,I2,A)') '(F',IT,'.',IDEC,')'
      WRITE (HULP,FORM,IOSTAT=I) XDLOC
      I = INDEX (HULP,' .')
      IF (I.GT.0) HULP(I:I) = '0'
      I = INDEX (HULP,'-.')
      IF (I.GT.0) HULP(I-1:I) = '-0'
10    IF (HULP(IT:IT).EQ.'0') THEN
      HULP(IT:IT) = ' '
      IT = IT - 1
      GOTO 10
      END IF
      IF (HULP(IT:IT).EQ.'.') HULP(IT:IT)=' '
20    CONTINUE
      CALL ENTDCH (QUEST,HULP,INPUT)
      IS = ISTART (INPUT)
      IL = ILEN (INPUT)
      IF (IL.EQ.0) THEN
      X = XDLOC
      RETURN
      END IF
      CALL DECREA (IWAR,INPUT(IS:IL),X)
      IF (IWAR.NE.0) THEN
      WRITE (*,'(/,A,/,A,/)')
     $' Enter a REAL value !!',
     $' or <CTRL> Z  to STOP (<Command>. on Mac)'
      GOTO 20
      END IF
      RETURN
      END
