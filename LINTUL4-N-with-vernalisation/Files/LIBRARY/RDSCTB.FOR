** $Id: rdsctb.for 1.1 1997/10/02 16:08:13 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDSCTB (EOF)
      IMPLICIT NONE
      LOGICAL EOF
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
*     table info common /RDTBL1/ and /RDTBL2/
*
*     TBLERR - flags the presence of an error
*     NAMERR - flags presence of error in name part
*     INAMES - number of names until first error
*     INLMX  - maximum length of variable names
*     NCOLMX - maximum number of columns
*     NCOL   - actual number of columns
*     ICOL   - column where processing takes place
*     NVAL   - number of values
*     TBLNAM - array with names
*     TBLPNT - array with pointers
*     TBLTYP - array with column datatypes
*     TBLLIN - array with line numbers for names of the table
 
      INTEGER INAMES, NCOLMX, INLMX, NCOL, ICOL, NVAL, TBLPNT, TBLLIN
      CHARACTER TBLTYP*1, TBLNAM*31
      LOGICAL TBLERR, NAMERR
 
      PARAMETER (NCOLMX=40)
      DIMENSION TBLNAM(NCOLMX), TBLPNT(NCOLMX)
      DIMENSION TBLTYP(NCOLMX), TBLLIN(NCOLMX)
 
      COMMON /RDTBL1/ TBLERR, NAMERR, INAMES, INLMX, NCOL, ICOL, NVAL,
     $                TBLPNT, TBLLIN
      COMMON /RDTBL2/ TBLNAM, TBLTYP
      INTEGER NTOK
      INTEGER TEOI
      INTEGER TEOL
      INTEGER TTIME
      INTEGER TINTEG
      INTEGER TFLOAT
      INTEGER TSPACE
      INTEGER TCOMMA
      INTEGER TEQUAL
      INTEGER TSEMIC
      INTEGER TMULT
      INTEGER TLPAR
      INTEGER TRPAR
      INTEGER TSCONC
      INTEGER TCMMNT
      INTEGER TIDENT
      INTEGER TSTRNG
      INTEGER TLOGIC
      INTEGER TMISS
      PARAMETER (NTOK   = 18)
      PARAMETER (TEOI   =  1)
      PARAMETER (TEOL   =  2)
      PARAMETER (TTIME  =  3)
      PARAMETER (TINTEG =  4)
      PARAMETER (TFLOAT =  5)
      PARAMETER (TSPACE =  6)
      PARAMETER (TCOMMA =  7)
      PARAMETER (TEQUAL =  8)
      PARAMETER (TSEMIC =  9)
      PARAMETER (TMULT  = 10)
      PARAMETER (TLPAR  = 11)
      PARAMETER (TRPAR  = 12)
      PARAMETER (TSCONC = 13)
      PARAMETER (TCMMNT = 14)
      PARAMETER (TIDENT = 15)
      PARAMETER (TSTRNG = 16)
      PARAMETER (TLOGIC = 17)
      PARAMETER (TMISS  = 18)
      CHARACTER TOKENN(NTOK)*6
      INTEGER TNEXT
      INTEGER NINPUT, NSTATS, EOTAB
      PARAMETER (NSTATS=16, NINPUT=18, EOTAB=16)
      INTEGER IS,ISOLD,JUMPTB
      DIMENSION JUMPTB(-1:NSTATS,0:NINPUT)
      CHARACTER MESSAG*35
      DIMENSION MESSAG(NSTATS-1)
      INTEGER IDUM, IREP, COLNUM, IWAR
      CHARACTER CDUM*1,CTRLZ*1
      LOGICAL TBCOMP, ON, OFF, NEWREC
      PARAMETER (ON=.TRUE., OFF=.FALSE.)
      SAVE
       DATA TOKENN /'TEOI  ','TEOL  ','TTIME ','TINTEG','TFLOAT',
     &'TSPACE','TCOMMA','TEQUAL','TSEMIC','TMULT ','TLPAR ','TRPAR ',
     &'TSCONC','TCMMNT','TIDENT','TSTRNG','TLOGIC','TMISS '/
      DATA JUMPTB/
     $-1, 0,-1,-1,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     $16,16,16,-1,-1,-1,-1,-1, 0,16, 0,16, 0,16, 0,16,16, 0,
     $6,15, 1, 3, 3, 4, 6, 6, 7,15, 9,15,11,15,13,15,15, 0,
     $-1,12,-1,-1,-1,-1,-1,12,12, 0,12, 0, 0, 0,12,12,12, 0,
     $-1, 8,-1,-1,-1,-1,-1, 8, 8, 0,12, 0, 0, 0, 8, 8, 8, 0,
     $-1,12,-1,-1,-1,-1,-1,12,12, 0,12, 0, 0, 0,12,12,12, 0,
     $-1, 0, 1, 2, 3, 4, 5, 6, 7,14, 9,14,11,14,13,14,15, 0,
     $4,13,-1, 4,-1,-1, 4,-1, 0,13, 0,13, 0,13, 0,13, 0, 0,
     $7, 0,-1, 7,-1,-1, 7,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     $16,16,-1,-1,-1,-1,-1,-1, 0,16, 0,16, 0,16, 0,16,16, 0,
     $-1, 0,-1,-1,-1,-1,-1,-1, 0, 9, 0, 0, 0, 0, 0, 0, 0, 0,
     $-1, 0,-1,-1,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     $-1, 0,-1,-1,-1,-1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     $-1, 0,-1,-1,-1,-1,-1,-1, 0, 0, 0,11, 0, 0, 0, 0, 0, 0,
     $-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15, 0,
     $5,16, 2, 5, 5, 5, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0,16, 0,
     $-1,10,-1,-1,-1,-1,-1,10,10, 0,10, 0,10, 0,10,10,10, 0,
     $-1,12,-1,-1,-1,-1,-1,12,12, 0,12, 0, 0, 0,12,12,12, 0,
     $-1,12,-1,-1,-1,-1,-1,12,12, 0,12, 0, 0, 0,12,12,12, 0/
      DATA MESSAG/
     $'Variable name expected             ',
     $'Next variable name or = expected   ',
     $'Without =, a next name is expected ',
     $'Next variable name expected        ',
     $'Next name, = or value on next line ',
     $'Next name or first value expected  ',
     $'Value expected                     ',
     $'Illegal after INTEGER value        ',
     $'After * a value is expected        ',
     $'Illegal after CHARACTER string     ',
     $'After // next string expected      ',
     $'No proper value separation         ',
     $'There must be another value        ',
     $'After a value this is illegal      ',
     $'New variable or next value expected'/
      EOF = OFF
      TBLERR = OFF
      NAMERR = OFF
      INAMES = 0
      NCOL   = 0
      NVAL   = 0
      TBCOMP = ON
      IREP   = 1
      IS     = 1
      IDUM   = 0
      CTRLZ  = CHAR (26)
      CALL RDTMP1 (2,IDUM,CDUM)
10    IF (IS.NE.EOTAB) THEN
      NEWREC = TOKEN.EQ.TEOL.OR.BEGNEW
20    IF (NEWREC) THEN
      CALL RECREAD (STBUF,RECLEN-1,STBLEN,EOF,IWAR)
      IF (.NOT.EOF) EOF = STBUF(1:1).EQ.CTRLZ
      IF (.NOT.EOF) THEN
      RECNO  = RECNO+1
      NEWREC = STBUF2(1).EQ.'*'.OR.
     &STBUF2(1).EQ.'!'.OR.
     &STBLEN.EQ.0
      IF (.NOT.NEWREC) THEN
      IF (IWAR.EQ.1) THEN
      ISP = STBLEN-1
      IP  = STBLEN-1
      CALL RDERR (2,'Input record truncated')
      END IF
      CALL RDLEX (2)
      END IF
      ELSE
      NEWREC = .FALSE.
      END IF
      GOTO 20
      END IF
      IF (.NOT.EOF) THEN
      CALL RDLEX (3)
      ELSE
      CALL RECREAD_TERM
      RECNO = 0
      STBUF  = ' '
      STBLEN = 0
      ISP    = 1
      IP     = 1
      TOKEN  = TEOI
      END IF
      ISOLD = IS
      IS    = JUMPTB (ISOLD,TOKEN)
      IF (ISOLD.GT.0 .AND. IS.LE.0) THEN
      TBLERR = ON
      IF (IS.EQ.-1) NAMERR = ON
      IF (IS.EQ. 0) IREP = 1
      IF (TOKEN.NE.TEOI) CALL RDERR (2,MESSAG(ISOLD))
      IF (TOKEN.EQ.TEOI) CALL RDERR (2,'Unexpected End_Of_File')
      END IF
      IF (TOKEN.EQ.TIDENT .AND. IS.NE.EOTAB) THEN
      IF (NCOL.GE.NCOLMX) THEN
      CALL RDERR (2,'Too many columns in table')
      WRITE (*,'(/,A,I3,2A,/,2A)')
     $' Table has more than',NCOLMX,' columns.',
     $' Increase the value',' of NCOLMX in the',
     $' subroutines RDSCTB and RDTMP1 (both!!).'
      CALL FATALERR ('RDSCTB','too many columns')
      ELSE IF (IP-ISP+1.GT.INLMX) THEN
      CALL RDERR (2,'Variable name too long')
      NAMERR = ON
      TBLERR = ON
      ELSE
      NCOL = NCOL + 1
      IF (.NOT.NAMERR) INAMES = INAMES + 1
      TBLNAM(NCOL) = STBUF(ISP:IP)
      CALL UPPERC (TBLNAM(NCOL))
      TBLLIN(NCOL) = RECNO
      TBLTYP(NCOL) = '-'
      CALL RDTMP1 (4,TBLPNT(NCOL),CDUM)
      END IF
      ELSE IF (TOKEN.EQ.TSTRNG .OR. TOKEN.EQ.TINTEG .OR.
     $TOKEN.EQ.TFLOAT .OR. TOKEN.EQ.TTIME  .OR.
     $TOKEN.EQ.TLOGIC .OR. TOKEN.EQ.TMISS) THEN
      IF (TOKEN.EQ.TINTEG) THEN
      CALL RDLEX (3)
      TNEXT = TOKEN
      CALL RDLEX (4)
      END IF
      IF (TOKEN.EQ.TINTEG .AND. TNEXT.EQ.TMULT) THEN
      IREP = VINT
      IF (IS.GT.0 .AND. NCOL.GT.1) THEN
      CALL RDERR (2,'Repeat value not allowed in table')
      TBLERR = ON
      ELSE IF (IREP.LE.0) THEN
      CALL RDERR (2,'Repeat value should be positive')
      TBLERR = ON
      END IF
      ELSE IF (ISOLD.EQ.11) THEN
      CALL RDTMP1 (7,IDUM,CDUM)
      ELSE
      CALL RDTMP1 (6,IREP,CDUM)
      NVAL   = NVAL + IREP
      COLNUM = 1 + MOD (NVAL-1,MAX(1,NCOL))
      IF (TOKEN.NE.TMISS) THEN
      IF (TBLTYP(COLNUM).EQ.'-') THEN
      TBLTYP(COLNUM) = VTYPE
      ELSE IF (
     $VTYPE.NE.TBLTYP(COLNUM).AND..NOT.NAMERR) THEN
      IF (NCOL.LE.1) THEN
      CALL RDERR (2,'Inconsistent variable type')
      TBLERR = ON
      ELSE IF (.NOT.TBLERR) THEN
      CALL RDERR (2,'Column type inconsistent ')
      TBLERR = ON
      END IF
      END IF
      END IF
      IREP = 1
      TBCOMP = COLNUM.EQ.NCOL
      END IF
      END IF
      GOTO 10
      END IF
      IF (.NOT.TBCOMP .AND. .NOT.TBLERR) THEN
      CALL RDERR (2,'Previous table is incomplete')
      TBLERR = ON
      END IF
      IF (TOKEN.EQ.TIDENT) THEN
      CALL RDLEX (4)
      ELSE IF (TOKEN.EQ.TEOI) THEN
      EOF = ON
      END IF
      CALL RDTMP1 (8,IDUM,CDUM)
      RETURN
      END
