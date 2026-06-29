** $Id: rddecd.for 1.1 1997/10/02 16:08:12 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDDECD (IB,IE)
      IMPLICIT NONE
      INTEGER IB, IE
*     Record info buffer. This file defines a common  buffer
*     this is meant to exchange record information with other
*     subroutines.
 
*     limit MS-FORTRAN w.b. declaratie strings: 32k characters
*     limit VAX-FORTRAN 64k characters
*     limit ABSOFT-FORTRAN staat niet in de manual
*     limit LS-FORTRAN 32K characters
 
*     RECLEN - Declared length of buffer
*     STBLEN - Actual length of buffer
*     STBUF  - Buffer itself
*     STBUF2 - Buffer as single character array (equivalenced with STBUF)
*     ASCII  - Array with ASCII values of the buffer
*     RECNO  - Record number of input file
 
*     Note:    RECLEN-1 is the actual length on file
*              (rdlex uses 1 extra character), so if RECLEN=256, significant
*              record length on file is 255
      INTEGER RECLEN, STBLEN
      PARAMETER (RECLEN=256)
      CHARACTER STBUF*256
      CHARACTER STBUF2(RECLEN)*1
      EQUIVALENCE (STBUF,STBUF2)
      INTEGER ASCII(RECLEN), RECNO
 
      COMMON /RDREC1/ STBLEN, ASCII, RECNO
      COMMON /RDREC2/ STBUF
*     Token info buffer. This file defines a common  buffer
*     this is meant to exchange token information with other
*     subroutines.
 
*     ISP    - Start position where parsing of word started
*     IP     - Position where parsing is taking place
*     TOKEN  - Token number
*     VINT   - Value in case token is integer value
*     VFLOAT - Value in case token is floating point value
*     VTIME  - Value in case token is date/time value
*     VLOGIC - Value in case token is logical value
*     VTYPE  - Value type of TOKEN, 'F' or 'I' or 'L' or 'T' or 'C'
*     BEGOLD - Flag that indicates whether backwards stepping reached
*              beginning of record, this flag is switched on if
*              if the parser is backwarded to the position BEFORE the
*              first character on the record, at the beginning of the
*              file, the token number is set to zero
*     BEGNEW - Flag that indicates whether the parser is at the
*              beginning without having yet parsed the record
      INTEGER ISP, IP, TOKEN, VINT
      DOUBLE PRECISION VFLOAT, VTIME
      LOGICAL VLOGIC, BEGOLD, BEGNEW
      CHARACTER VTYPE*1
 
      COMMON /RDTOK1/ ISP, IP, TOKEN, VINT, VFLOAT, VTIME, VLOGIC,
     &                BEGOLD, BEGNEW
      COMMON /RDTOK2/ VTYPE
      INTEGER SIGDGM
      PARAMETER (SIGDGM=15)
      INTEGER EXPOM
      PARAMETER (EXPOM=38)
      INTEGER I2,SIGN,SIGNE,ITMP,IDOT,ICHR,EXPO,IDIG(48:57),DIST,SIGDIG
      LOGICAL EXPFND,SGDFND
      INTEGER MULLB, MULUB
      PARAMETER (MULLB=-15, MULUB=15)
      DOUBLE PRECISION MULTIA(MULLB:MULUB),DDIG(48:57), MULTI
      SAVE
      DATA DDIG /0.D0,1.D0,2.D0,3.D0,4.D0,5.D0,6.D0,7.D0,8.D0,9.D0/
      DATA IDIG /0,1,2,3,4,5,6,7,8,9/
      DATA MULTIA /1.D-15, 1.D-14, 1.D-13, 1.D-12, 1.D-11,
     &1.D-10, 1.D-9 , 1.D-8 , 1.D-7 , 1.D-6 ,
     &1.D-5 , 1.D-4 , 1.D-3 , 1.D-2 , 1.D-1 ,
     &0.D+0 ,
     &1.D+0 , 1.D+1 , 1.D+2 , 1.D+3 , 1.D+4 ,
     &1.D+5 , 1.D+6 , 1.D+7 , 1.D+8 , 1.D+9 ,
     &1.D+10, 1.D+11, 1.D+12, 1.D+13, 1.D+14/
      IF (STBUF2(IB).EQ.'-') THEN
      SIGN = -1
      ITMP = IB+1
      ELSE IF (STBUF2(IB).EQ.'+') THEN
      SIGN = +1
      ITMP = IB+1
      ELSE
      SIGN = +1
      ITMP = IB
      END IF
      IDOT = INDEX (STBUF(IB:IE),'.')
      IF (IDOT.EQ.0) THEN
      write (*,*) stbuf(ib:ie)
      CALL FATALERR ('RDDECD','internal error')
      END IF
      IDOT = IB+IDOT-1
      EXPFND = .FALSE.
      SGDFND = .FALSE.
      SIGDIG = 0
      VFLOAT = 0.D0
      DO 10 I2=ITMP,IE
      ICHR = ASCII(I2)
      IF (ICHR.GE.49.AND.ICHR.LE.57) THEN
      IF (SGDFND) THEN
      SIGDIG = SIGDIG+1
      ELSE
      SGDFND = .TRUE.
      SIGDIG = 1
      END IF
      DIST = IDOT-I2
      IF (DIST.GE.MULLB.AND.DIST.LE.MULUB) THEN
      VFLOAT = VFLOAT+MULTIA(DIST)*DDIG(ICHR)
      ELSE IF (DIST.GT.MULUB) THEN
      MULTI  = 10.D0**(DIST-1)
      VFLOAT = VFLOAT+MULTI*DDIG(ICHR)
      ELSE IF (DIST.LT.MULLB) THEN
      MULTI  = 10.D0**DIST
      VFLOAT = VFLOAT+MULTI*DDIG(ICHR)
      END IF
      ELSE IF (ICHR.EQ.48) THEN
      IF (SGDFND) SIGDIG = SIGDIG+1
      ELSE IF (STBUF2(I2).EQ.'.') THEN
      CONTINUE
      ELSE IF (INDEX ('EeDd',STBUF2(I2)).NE.0) THEN
      ITMP   = I2+1
      EXPFND = .TRUE.
      GOTO 20
      END IF
10    CONTINUE
20    CONTINUE
      IF (EXPFND) THEN
      IF (STBUF2(ITMP).EQ.'-') THEN
      SIGNE = -1
      ITMP  = ITMP+1
      ELSE IF (STBUF2(ITMP).EQ.'+') THEN
      SIGNE = +1
      ITMP  = ITMP+1
      ELSE
      SIGNE = +1
      END IF
      EXPO = 0
      DO 30 I2=ITMP,IE
      EXPO = EXPO*10+IDIG(ASCII(I2))
30    CONTINUE
      END IF
      IF (SIGN.EQ.-1) VFLOAT = -VFLOAT
      IF (EXPFND) THEN
      IF (EXPO.LE.EXPOM) THEN
      VFLOAT = VFLOAT*10.D0**(SIGNE*EXPO)
      ELSE
      CALL RDERR (2,'Exponent outside limits')
      END IF
      END IF
      IF (SIGDIG.GT.SIGDGM)
     &CALL RDERR (3,
     &'Many digits, possible loss of accuracy, even with RD*DOU calls')
      RETURN
      END
