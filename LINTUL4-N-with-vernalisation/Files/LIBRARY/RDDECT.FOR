** $Id: rddect.for 1.1 1997/10/02 16:08:12 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDDECT (IB,IE)
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
      INTEGER INSEP,MNSEP
      PARAMETER (MNSEP=7)
      INTEGER I1, I2, IPSEP(0:MNSEP), DATEA(6),IFINDC
      INTEGER YRSEQ, DAYSEQ, HRSEQ
      CHARACTER MONN1(12)*3, MONN2(12)*9, MONTMP*9, MESSAG*80
      LOGICAL MONNUM,ERR
      REAL FSEC
      SAVE
      DATA MONN1 /'JAN','FEB','MAR','APR','MAY','JUN',
     &'JUL','AUG','SEP','OCT','NOV','DEC'/
      DATA MONN2 /'JANUARY','FEBRUARY','MARCH','APRIL',
     &'MAY','JUNE','JULY','AUGUST',
     &'SEPTEMBER','OCTOBER','NOVEMBER','DECEMBER'/
      DATEA(1) = 1900
      DATEA(2) = 1
      DATEA(3) = 1
      DATEA(4) = 0
      DATEA(5) = 0
      DATEA(6) = 0
      FSEC     = 0.
      I1       = IB
      IPSEP(0) = IB-1
      INSEP    = 0
10    IF (I1.LE.IE.AND.INSEP.LT.MNSEP-1) THEN
      IF (INDEX ('-/:_.',STBUF2(I1)).NE.0) THEN
      INSEP = INSEP+1
      IPSEP(INSEP) = I1
      END IF
      I1 = I1+1
      GOTO 10
      END IF
      IPSEP(INSEP+1) = IE+1
      IF (INDEX ('/-',STBUF2(IPSEP(1))).NE.0) THEN
      MONNUM = .TRUE.
      DO 20 I1=IPSEP(1)+1,IPSEP(2)-1
      I2 = ICHAR(STBUF2(I1))
      IF (MONNUM.AND.(I2.LT.48.OR.I2.GT.57)) MONNUM = .FALSE.
20    CONTINUE
      IF (MONNUM) THEN
      YRSEQ  = 1
      DAYSEQ = 3
      CALL RDDECI(IPSEP(1)+1,IPSEP(2)-1)
      DATEA(2) = VINT
      ELSE
      MONTMP = STBUF(IPSEP(1)+1:IPSEP(2)-1)
      CALL UPPERC (MONTMP)
      IF (IPSEP(2)-IPSEP(1)-1.EQ.3) THEN
      I1 = IFINDC (MONN1,12,1,12,MONTMP(1:3))
      ELSE IF (IPSEP(2)-IPSEP(1)-1.GT.3) THEN
      I1 = IFINDC (MONN2,12,1,12,MONTMP)
      ELSE
      CALL RDERR (2,'length of month not correct')
      END IF
      IF (I1.GT.0) THEN
      DATEA(2) = I1
      ELSE
      CALL RDERR (2,'illegal month name')
      END IF
      YRSEQ  = 3
      DAYSEQ = 1
      END IF
      CALL RDDECI (IPSEP(YRSEQ-1)+1,IPSEP(YRSEQ)-1)
      DATEA(1) = VINT
      CALL RDDECI (IPSEP(DAYSEQ-1)+1,IPSEP(DAYSEQ)-1)
      DATEA(3) = VINT
      END IF
      IF (STBUF2(IPSEP(1)).EQ.':') THEN
      HRSEQ    = 1
      DATEA(1) = 0
      DATEA(2) = 0
      DATEA(3) = 0
      ELSE IF (INSEP.GE.3) THEN
      HRSEQ = 4
      ELSE
      HRSEQ = 0
      END IF
      IF (HRSEQ.GT.0) THEN
      CALL RDDECI (IPSEP(HRSEQ-1)+1,IPSEP(HRSEQ)-1)
      DATEA(4) = VINT
      CALL RDDECI (IPSEP(HRSEQ)+1,IPSEP(HRSEQ+1)-1)
      DATEA(5) = VINT
      IF (INSEP.GE.HRSEQ+1) THEN
      CALL RDDECI(IPSEP(HRSEQ+1)+1,IPSEP(HRSEQ+2)-1)
      DATEA(6) = VINT
      IF (INSEP.GE.HRSEQ+2.AND.
     &IPSEP(HRSEQ+3)-IPSEP(HRSEQ+2).GT.1) THEN
      CALL RDDECD(IPSEP(HRSEQ+2),IPSEP(HRSEQ+3)-1)
      FSEC = REAL (VFLOAT)
      END IF
      END IF
      END IF
      CALL DTSYS (1,DATEA,FSEC,VTIME,ERR,MESSAG)
      IF (ERR) THEN
      VTIME = 0.D0
      CALL RDERR (2,MESSAG)
      END IF
      RETURN
      END
