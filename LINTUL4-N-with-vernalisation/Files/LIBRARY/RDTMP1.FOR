** $Id: rdtmp1.for 1.1 1997/10/02 16:08:13 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDTMP1 (ITASK,INT1,STRG)
      IMPLICIT NONE
      INTEGER ITASK,INT1
      CHARACTER*(*) STRG
*     * * * * * * * * * * * * * * * * * * * * * * * * * * *
*     *                                                   *
*     *  M A C H I N E   D E P E N D E N T   V A L U E S  *
*     *  ===============================================  *
*     *                                                   *
*     * * * * * * * * * * * * * * * * * * * * * * * * * * *
 
*     IIL    - length in bytes of INTEGER datatype
*     IRL    - length in bytes of DOUBLE PRECISION datatype
*     ILL    - length in bytes of LOGICAL datatype
*     ILNREP - maximum number of names in each set of a rerun file
*     ILPREP - maximum number of assignments on a rerun file (40 sets
*              of 10 variables for instance, gives 400 assignments)
*     ILNDAT - maximum number of names in a data file
*              increase this number if you have more variables
*     INFDEC - maximum number of files in tmp file list
 
      INTEGER IIL, IRL, ILL, ILNREP, ILPREP, ILNDAT, INFDEC
      PARAMETER (IIL=4, IRL=8, ILL=4)
      PARAMETER (ILNREP=40,ILPREP=400)
      PARAMETER (ILNDAT=400)
      PARAMETER (INFDEC=10)
 
 
 
*     * * * * * * * * * * * * * * * * * * * * * * * * * * *
*     *                                                   *
*     *    B U F F E R   L E N G T H   I N   B Y T E S    *
*     *    ===========================================    *
*     *                                                   *
*     * * * * * * * * * * * * * * * * * * * * * * * * * * *
 
*     ILBUF - length of TMP file buffers
*
      INTEGER ILBUF
      PARAMETER (ILBUF=128)
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
      INTEGER IUN, TOPREC
      INTEGER NCOLMX, TBLWID, TBLREC, TBLFBT, ICOL
      PARAMETER (NCOLMX=40)
      DIMENSION TBLFBT(NCOLMX), TBLREC(NCOLMX)
      INTEGER IXL, IWN, NELMT, IBN, IW
      INTEGER           IBUF(0:ILBUF/IIL-1,NCOLMX)
      DOUBLE PRECISION  RBUF(0:ILBUF/IRL-1,NCOLMX)
      LOGICAL           LBUF(0:ILBUF/ILL-1,NCOLMX)
      EQUIVALENCE (IBUF,RBUF,LBUF)
      INTEGER I,J,K,ITOLD,NUL,ILRTOP,IRECL,IBYTE,IREP
      INTEGER IPC, IXLMAX
      LOGICAL INIT,CONCAT,REPEAT,WRTDAT
      DIMENSION IPC(0:IIL-1)
      SAVE
      DATA INIT /.FALSE./, NUL/0/, ITOLD /0/
      IF (.NOT.INIT) THEN
      IPC(IIL-1) = 1
      DO 10 I=IIL-2,0,-1
      IPC(I) = 256 * IPC(I+1)
10    CONTINUE
      IXLMAX = MAX(IIL,IRL,ILL)
      ILRTOP = ILBUF/IRL-1
      INIT  = .TRUE.
      END IF
      IF (ITASK.EQ.1) THEN
      IRECL = ILBUF + IIL
      IUN   = INT1
      CALL FOPENG (IUN,STRG,'NEW','UD',IRECL,'DEL')
      TOPREC    = 2
      TBLREC(1) = TOPREC
      TBLFBT(1) = 0
      ELSE IF (ITASK.EQ.2) THEN
      IF (.NOT.(ITOLD.EQ.1 .OR. ITOLD.EQ.8))
     $CALL FATALERR ('RDTMP1','Internal_1')
      TBLWID = 0
      ELSE IF (ITASK.EQ.4) THEN
      IF (INERR.EQ.0) THEN
      TBLWID = TBLWID + 1
      ICOL   = TBLWID
      IF (TBLWID.GT.NCOLMX) CALL FATALERR ('RDTMP1','Internal_2')
      IF (TBLWID.EQ.1) THEN
      TBLFBT(1) = IXLMAX * ((IXLMAX-1 + TBLFBT(1))/IXLMAX)
      IF (TBLFBT(1).EQ.ILBUF) THEN
      WRITE (IUN,REC=TBLREC(1)) (RBUF(K,1),K=0,ILRTOP),NUL
      TOPREC = TOPREC + 1
      TBLREC(1) = TOPREC
      TBLFBT(1) = 0
      END IF
      ELSE
      TOPREC = TOPREC + 1
      TBLREC(TBLWID) = TOPREC
      TBLFBT(TBLWID) = 0
      END IF
      INT1 = ILBUF * (TBLREC(TBLWID)-1) + TBLFBT(TBLWID)
      END IF
      ELSE IF (ITASK.EQ.6 .OR. ITASK.EQ.7) THEN
      CONCAT = ITASK.EQ.7
      IREP   = INT1
      STRG = ' '
      IF (CONCAT.AND.VTYPE.NE.'C')
     $CALL FATALERR ('RDTMP1','Internal_3')
      IF (INERR.EQ.0) THEN
      IF (CONCAT) THEN
      NELMT = IP - ISP
      IXL   = 1
      TBLFBT(ICOL) = TBLFBT(ICOL) - 1
      REPEAT = .FALSE.
      ELSE
      ICOL = 1 + MOD (ICOL,TBLWID)
      IF (VTYPE.EQ.'I') THEN
      NELMT = 1
      IXL   = IIL
      ELSE IF (VTYPE.EQ.'F') THEN
      NELMT = 1
      IXL   = IRL
      ELSE IF (VTYPE.EQ.'C') THEN
      NELMT = IP - ISP
      IXL   = 1
      ELSE IF (VTYPE.EQ.'L') THEN
      NELMT = 1
      IXL   = ILL
      ELSE IF (VTYPE.EQ.'T') THEN
      NELMT = 1
      IXL   = IRL
      ELSE IF (VTYPE.EQ.'-') THEN
      NELMT = 1
      IXL   = ILL
      IREP  = -1 * IREP
      ELSE
      CALL FATALERR ('RDTMP1','illegal variable type')
      END IF
      TBLFBT(ICOL) = IXLMAX* ((IXLMAX-1 + TBLFBT(ICOL))/IXLMAX)
      REPEAT = .TRUE.
      END IF
      J = 1
      WRTDAT = .TRUE.
80    IF (WRTDAT) THEN
      IF (TBLFBT(ICOL).GE.ILBUF) THEN
      TOPREC = TOPREC + 1
      WRITE (IUN,REC=TBLREC(ICOL))
     $(RBUF(K,ICOL),K=0,ILRTOP),(TOPREC-1)*ILBUF
      TBLREC(ICOL) = TOPREC
      TBLFBT(ICOL) = 0
      END IF
      IF (REPEAT) THEN
      IBUF(TBLFBT(ICOL)/IIL,ICOL) = IREP
      IF (IREP.GT.0) THEN
      TBLFBT(ICOL) = TBLFBT(ICOL) + MAX(IXL,IIL)
      REPEAT = .FALSE.
      ELSE
      TBLFBT(ICOL) = TBLFBT(ICOL) + IXLMAX
      WRTDAT = .FALSE.
      END IF
      GOTO 80
      END IF
      IWN  = TBLFBT(ICOL) / IXL
      IF (VTYPE.EQ.'F') THEN
      RBUF(IWN,ICOL) = VFLOAT
      ELSE IF (VTYPE.EQ.'I') THEN
      IBUF(IWN,ICOL) = VINT
      ELSE IF (VTYPE.EQ.'L') THEN
      LBUF(IWN,ICOL) = VLOGIC
      ELSE IF (VTYPE.EQ.'T') THEN
      RBUF(IWN,ICOL) = VTIME
      ELSE IF (VTYPE.EQ.'C') THEN
      IBN = MOD(IWN,IIL)
      IW  = IWN/IIL
      IF (J.LT.NELMT) THEN
      IBYTE = ASCII(ISP+J)
      ELSE
      IBYTE = 0
      END IF
      IF (IBN.EQ.0) THEN
      IBUF(IW,ICOL) = IPC(IBN) * IBYTE
      ELSE
      IBUF(IW,ICOL) = IBUF(IW,ICOL) + IPC(IBN) * IBYTE
      END IF
      END IF
      TBLFBT(ICOL) = TBLFBT(ICOL) + IXL
      J = J + 1
      WRTDAT = J.LE.NELMT
      GOTO 80
      END IF
      END IF
      ELSE IF (ITASK.EQ.8) THEN
      IF (INERR.EQ.0) THEN
      DO 50 I=1,TBLWID
      IF (TBLREC(I).LT.TOPREC) THEN
      WRITE (IUN,REC=TBLREC(I)) (RBUF(K,I),K=0,ILRTOP),NUL
      ELSE IF (I.GT.1) THEN
      DO 40 K=0,ILRTOP
      RBUF(K,1) = RBUF(K,I)
40    CONTINUE
      TBLREC(1) = TOPREC
      TBLFBT(1) = TBLFBT(I)
      END IF
50    CONTINUE
      END IF
      ELSE IF (ITASK.EQ.9) THEN
      IF (ITOLD.NE.8) CALL FATALERR ('RDTMP1','Internal_4')
      IF (INERR.EQ.0) THEN
      WRITE (IUN,REC=TBLREC(1)) (RBUF(K,1),K=0,ILRTOP),NUL
      WRITE (IUN,REC=1) TOPREC,ILBUF,IIL
      ELSE
      CLOSE (IUN,STATUS='DELETE')
      END IF
      ELSE
      CALL FATALERR ('RDTMP1','Internal_5')
      END IF
      ITOLD = ITASK
      RETURN
      END
