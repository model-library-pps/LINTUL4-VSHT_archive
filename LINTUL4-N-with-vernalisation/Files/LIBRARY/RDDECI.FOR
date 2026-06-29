** $Id: rddeci.for 1.1 1997/10/02 16:08:12 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDDECI (IB,IE)
      IMPLICIT NONE
      INTEGER IB,IE
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
      INTEGER ISPTMP,I2, SIGN, NDIG, ISPS, IPS
      INTEGER DIGITS(48:57), IVAL
      LOGICAL OVERFL
      SAVE
      DATA DIGITS /0,1,2,3,4,5,6,7,8,9/
      IF (STBUF2(IB).EQ.'-') THEN
      ISPTMP = IB+1
      SIGN   = -1
      ELSE IF (STBUF2(IB).EQ.'+') THEN
      ISPTMP = IB+1
      SIGN   = 1
      ELSE
      ISPTMP = IB
      SIGN   = 1
      END IF
      NDIG  = IE-ISPTMP+1
      IF (NDIG.LT.10) THEN
      OVERFL = .FALSE.
      ELSE IF (NDIG.EQ.10) THEN
      OVERFL = LGT (STBUF(ISPTMP:IE),'2147483647')
      ELSE
      OVERFL = .TRUE.
      END IF
      IF (.NOT.OVERFL) THEN
      IVAL = DIGITS(ASCII(ISPTMP))
      DO 10 I2=ISPTMP+1,IE
      IVAL = IVAL*10+DIGITS(ASCII(I2))
10    CONTINUE
      VINT = IVAL*SIGN
      ELSE
      VINT = 99
      ISPS = ISP
      IPS  = IP
      ISP  = IB
      IP   = IE
      CALL RDERR (2,
     &'Integer value too large')
      ISP  = ISPS
      IP   = IPS
      END IF
      RETURN
      END
