** $Id: rderr.for 1.1 1997/10/02 16:08:12 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDERR (ITASK,MESSAG)
      IMPLICIT NONE
      INTEGER ITASK
      CHARACTER*(*) MESSAG
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
*     FILEIN - name of input file
*     ILFILE - length of name
*     IUDF   - unit number of datafile
 
      INTEGER   ILFILE, IUDF
      CHARACTER FILEIN*80
 
      COMMON /RDFIL1/ ILFILE, IUDF
      COMMON /RDFIL2/ FILEIN
*     status common /TTURDS/
*     ------------------------------------------------------
*     TOSCR  - flag enabling error message output to screen
*     TOLOG  - flag enabling error message output to logfile
*     IULOG  - unit number of logfile (when TOLOG is set)
*              if non-existent, RDINDX.LOG is created
*     INERR  - number of errors
*     INWAR  - number of warnings
 
 
      INTEGER INERR, INWAR, IULOG
      LOGICAL TOSCR, TOLOG
 
      COMMON /RDSTA/ TOSCR , TOLOG, IULOG, INERR, INWAR
      CHARACTER*30 PTRSTR, ERBUF
      CHARACTER LMESS*40, FORM1*26
      PARAMETER (FORM1='(1X,I4,1X,A,1X,3A,/,T49,A)')
      INTEGER I, I1, I2, IH, ILEN
      LOGICAL THERE
      SAVE
      IF (INERR.EQ.0 .AND. INWAR.EQ.0) THEN
      IF (TOSCR) WRITE (*,'(/,1X,2A,/,2(/,T2,A,T7,A,T48,A))')
     $'Error(s) while reading from ',FILEIN(1:ILFILE),
     $'Line','Error text','Somewhere here',
     $'====','==========','=============='
      IF (TOLOG) THEN
      INQUIRE (UNIT=IULOG,OPENED=THERE)
      IF (.NOT.THERE)
     $CALL FOPENS (IULOG,'RDINDX.LOG','NEW','DEL')
      WRITE (IULOG,'(/,1X,2A,/,2(/,T2,A,T7,A,T48,A))')
     $'Error(s) while reading from ',FILEIN(1:ILFILE),
     $'Line','Error text','Somewhere here',
     $'====','==========','=============='
      END IF
      END IF
      IF (ITASK.EQ.2 .OR. ITASK.EQ.3) THEN
      IF (ITASK.EQ.2) INERR = INERR + 1
      IF (ITASK.EQ.3) INWAR = INWAR + 1
      LMESS = MESSAG
      CALL UPPERC (LMESS(1:1))
      I2 = MIN (IP+5,STBLEN-1)
      IF (I2.GE.1) THEN
      I1     = MAX (I2-(LEN(ERBUF)-1),1)
      ERBUF  = STBUF(I1:I2)
      PTRSTR = ' '
      DO 10 I=I1,I2
      IH = I-I1+1
      IF (I.EQ.ISP .OR. I.EQ.IP) THEN
      PTRSTR(IH:IH) = '^'
      ELSE IF (I.GT.ISP .AND. I.LT.IP) THEN
      IF (IH.GT.1) THEN
      PTRSTR(IH:IH) = '-'
      ELSE
      PTRSTR(IH:IH) = '<'
      END IF
      END IF
10    CONTINUE
      IF (TOSCR) WRITE (*,FORM1)
     &RECNO,LMESS,'[',ERBUF(1:I2-I1+1),']',PTRSTR(1:I2-I1+1)
      IF (TOLOG) WRITE (IULOG,FORM1)
     &RECNO,LMESS,'[',ERBUF(1:I2-I1+1),']',PTRSTR(1:I2-I1+1)
      ELSE
      IF (TOSCR) WRITE (*    ,FORM1) RECNO,LMESS
      IF (TOLOG) WRITE (IULOG,FORM1) RECNO,LMESS
      END IF
      ELSE IF (ITASK.EQ.4) THEN
      INERR = INERR + 1
      I1 = ILEN (TBLNAM(ICOL))
      I2 = ILEN (MESSAG)
      IF (TOSCR) WRITE (*,'(1X,I5,4A,/)')
     $TBLLIN(ICOL),'  Variable ',
     $TBLNAM(ICOL)(1:I1),': ',MESSAG(1:I2)
      IF (TOLOG) WRITE (IULOG,'(1X,I5,4A,/)')
     $TBLLIN(ICOL),'  Variable ',
     $TBLNAM(ICOL)(1:I1),': ',MESSAG(1:I2)
      ELSE
      CALL FATALERR ('RDERR','internal error')
      END IF
      RETURN
      END
