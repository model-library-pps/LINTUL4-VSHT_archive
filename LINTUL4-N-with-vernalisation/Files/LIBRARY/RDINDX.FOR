* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDINDX (IERR,IUNIT,SETS,TOSCRX,TOLOGX,IUL,DATFIL,
     $TMPFIL,NAMLIS,VARTYP,ILIND,IARLEN,INDPNT,ILPNT,INFND,INSETS)
      IMPLICIT NONE
      INTEGER IERR,IUNIT,IUL,ILIND,IARLEN,INDPNT,ILPNT,INFND,INSETS
      CHARACTER DATFIL*(*),TMPFIL*(*),NAMLIS*(*),VARTYP*1
      DIMENSION NAMLIS(ILIND),VARTYP(ILIND)
      DIMENSION IARLEN(ILPNT),INDPNT(ILPNT)
      LOGICAL SETS,TOSCRX,TOLOGX
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
      INTEGER IDUM, NROW, INFLD, IVOLD, LNAME
      CHARACTER CDUM*1
      LOGICAL EOF, STCOMP
      INTEGER IFINDC, ILEN
      SAVE
      IDUM = 0
      INLMX = LEN (NAMLIS(1))
      IF (INLMX.GT.31) CALL FATALERR
     &('RDINDX','variable names too long')
      TOSCR  = TOSCRX
      TOLOG  = TOLOGX
      IULOG  = IUL
      INERR  = 0
      INWAR  = 0
      IUDF   = IUNIT + 1
      FILEIN = DATFIL
      ILFILE = ILEN (FILEIN)
      CALL RECREAD_INIT (IUDF,DATFIL)
      RECNO = 0
      CALL RDLEX (1)
      CALL RDTMP1 (1,IUNIT,TMPFIL)
      INFLD  = 0
      INFND  = 0
      INSETS = 0
      EOF    = .FALSE.
      STCOMP = .TRUE.
10    IF (.NOT.EOF) THEN
      CALL RDSCTB (EOF)
      NROW = NVAL / MAX (1,NCOL)
      DO 20 ICOL=1,INAMES
      INFLD = INFLD + 1
      LNAME = IFINDC
     $(NAMLIS,ILIND,1,INFND,TBLNAM(ICOL)(1:INLMX))
      IF (LNAME.EQ.0 .AND. INSETS.LE.1) THEN
      INFND = INFND + 1
      IF (INSETS.EQ.0) INSETS = 1
      IF (INFND.GT.ILIND) THEN
      CALL RDERR (4,'exceeded maximum number of names')
      IF (SETS) WRITE (*,'(/,2A,/,A)')
     $' Too many names occur in the sets of rerun file ',
     $DATFIL(1:ILFILE),
     $' Increase the value of ILNREP in RDMACHIN.INC.'
      IF (.NOT.SETS) WRITE (*,'(/,2A,/,A)')
     $' Too many names occur in the data file ',
     $DATFIL(1:ILFILE),
     $' Increase the value of ILNDAT in RDMACHIN.INC.'
      CALL FATALERR (' ',' ')
      END IF
      NAMLIS(INFND) = TBLNAM(ICOL)
      VARTYP(INFND) = ' '
      IVOLD = INFND
      LNAME = INFND
      ELSE IF (LNAME.GT.0 .AND. SETS) THEN
      IF (LNAME.EQ.1 .AND. STCOMP) THEN
      INSETS = INSETS + 1
      ELSE IF (LNAME.NE.IVOLD+1) THEN
      CALL RDERR (4,'wrong name order in set')
      END IF
      STCOMP = LNAME.EQ.INFND
      IVOLD  = LNAME
      ELSE IF (LNAME.GT.0) THEN
      CALL RDERR (4,'name already used')
      ELSE
      CALL RDERR (4,'name does not occur in previous set(s)')
      END IF
      IF (.NOT.SETS) THEN
      IARLEN(INFLD) = NROW
      INDPNT(INFLD) = TBLPNT(ICOL)
      ELSE
      IF (INFLD.LE.ILPNT) THEN
      IARLEN(INFLD) = NROW
      INDPNT(INFLD) = TBLPNT(ICOL)
      ELSE
      CALL SWPI4 (1,'RD\IARREP',IUNIT+2,
     &ILPNT,INFLD,NROW)
      CALL SWPI4 (1,'RD\IPTREP',IUNIT+3,
     &ILPNT,INFLD,TBLPNT(ICOL))
      END IF
      END IF
      IF (VARTYP(LNAME) .EQ. ' ') THEN
      VARTYP(LNAME) = TBLTYP(ICOL)
      ELSE IF (TBLTYP(ICOL).NE.' ') THEN
      IF (TBLTYP(ICOL).NE.VARTYP(LNAME))
     $CALL RDERR (4,'data type is not consistent')
      END IF
20    CONTINUE
      GOTO 10
      END IF
      CALL RDTMP1 (9,IDUM,CDUM)
      IERR = INERR
      RETURN
      END
