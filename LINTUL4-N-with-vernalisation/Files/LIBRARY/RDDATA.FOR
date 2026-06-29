* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDDATA (ITASK,CALPRG,IUNIT,IULOG,FILNAM,IS,
     $XNAME,VARTYP,DX,RX,IX,CX,LX,
     $NDDEC,NRDEC,NIDEC,NCDEC,NLDEC,NREQ)
      IMPLICIT NONE
      INTEGER ITASK,IUNIT,IULOG,IS
      INTEGER NDDEC,NRDEC,NIDEC,NCDEC,NLDEC,NREQ
      DOUBLE  PRECISION DX(NDDEC)
      REAL    RX(NRDEC)
      INTEGER IX(NIDEC)
      LOGICAL LX(NLDEC)
      CHARACTER*(*) CALPRG,CX(NCDEC),FILNAM,XNAME,VARTYP*1
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
*     index rerun file ; index available after CALL RDSETS
*     When you use the common blocks TTURR1 and TTURR2 the array lengths
*     in your declarations should correspond to the ones here !!!!!!
      INTEGER IARREP,IPTREP,INFREP
      CHARACTER REPLIS*31,REPTYP*1
      DIMENSION REPLIS(ILNREP),REPTYP(ILNREP)
      DIMENSION IARREP(ILPREP),IPTREP(ILPREP)
 
*     Active set number
      INTEGER ISCOM
 
      COMMON /TTURR1/ REPLIS,REPTYP
      COMMON /TTURR2/ IARREP,IPTREP,INFREP,ISCOM
 
*     index data file ; index available after CALL RDINIT
*     When you use the common blocks TTUDD1 and TTUDD2 the array length
*     in your declarations should correspond to the one here !!!!!
      INTEGER IARDAT,IPTDAT,INFDAT
      CHARACTER DATLIS*31,DATTYP*1
      DIMENSION DATLIS(ILNDAT),DATTYP(ILNDAT)
      DIMENSION IARDAT(ILNDAT),IPTDAT(ILNDAT)
      COMMON /TTUDD1/ DATLIS,DATTYP
      COMMON /TTUDD2/ IARDAT,IPTDAT,INFDAT
      INTEGER IUNREP,IULR,INR,ITR,IRREP,PNTRNX,ILR
      INTEGER IUNDAT,IULD,IND,ITD,IRDAT,PNTDNX,ILD
      INTEGER INSETS,ISLOC,IPP,IRCHK,IDUM,IPS
      CHARACTER RFIL*80,DFIL*80,REPTMP*80,DATTMP*80
      DIMENSION IRCHK(ILNREP)
      LOGICAL LOGR,REPL,LOGD,INIDAT
      INTEGER INLIST,INFILS
      CHARACTER FILLIS*80
      DIMENSION FILLIS(INFDEC)
      INTEGER          IRBUF(0:ILBUF/IIL-1)
      DOUBLE PRECISION RRBUF(0:ILBUF/IRL-1)
      LOGICAL          LRBUF(0:ILBUF/ILL-1)
      EQUIVALENCE (IRBUF,RRBUF,LRBUF)
      INTEGER          IDBUF(0:ILBUF/IIL-1)
      DOUBLE PRECISION RDBUF(0:ILBUF/IRL-1)
      LOGICAL          LDBUF(0:ILBUF/ILL-1)
      EQUIVALENCE (IDBUF,RDBUF,LDBUF)
      INTEGER I,J,IBN,IEL,N,ISX,ILX,IMES,IWN,IREC,ILTMP,IRECL
      INTEGER NDEC,NELMT,IXL,IT1,IT2,IWAR,IVT,IV1,IV2,IVL,IVP,IPN
      INTEGER IUL,ISTREC,ILA,ILT,ILW,IPNT,IREP,IPC,IXLMAX
      DOUBLE PRECISION AD
      CHARACTER ERRTXT*26,MESSAG*42,LXNM31*31,ONFILE*9
      CHARACTER BYTE*1,PPP*3,VALUE*12,LINE*76,ASKED*16,REQTYP*1,GETONF*5
      CHARACTER WHAT*35,TARGET*10,RECORD*79,ARROW*7
      DIMENSION MESSAG(26),ONFILE(5),ASKED(6),REQTYP(6),RECORD(4)
      LOGICAL OPUNIT,OVERWR,EMPTY,THERE,FATAL,FOUND,ON,OFF
      LOGICAL SINGLE,UNKNOW,FIXED,TEMP,GIVEN,FILLED,REPMES,LOGF
      LOGICAL INIT,DREAD
      DIMENSION IPC(0:IIL-1)
      REAL RM_D, RM
      INTEGER IM_D, IM
      DOUBLE PRECISION DM_D, DM, TM_D, TM
      CHARACTER*80 CM_D, CM
      LOGICAL LM_D, LM
      INTEGER ILEN, IFINDC, ISTART
      LOGICAL FLEXIST
      SAVE
      DATA IUNDAT/0/, IUNREP/0/
      DATA ONFILE /'CHARACTER','INTEGER  ','REAL     ',
     $'LOGICAL  ','DATE_TIME'/
      DATA GETONF /'CIFLT'/
      DATA ASKED  /'CHARACTER       ','DOUBLE PRECISION',
     $'INTEGER         ','REAL            ',
     $'LOGICAL         ','DATE_TIME       '/
      DATA REQTYP /'C','F','I','F','L','T'/
      DATA ISLOC/0/, INIDAT/.FALSE./, REPL/.FALSE./, THERE/.FALSE./
      DATA ON/.TRUE./, OFF/.FALSE./, INFILS/0/, GIVEN/.FALSE./
      DATA ERRTXT /'Routine RDDATA, called by '/
      DATA INIT /.FALSE./
      DATA RM_D /-99.99/ , IM_D /-99/, DM_D /-99.99D0/,
     &CM_D /'- MISSING -'/, LM_D /.FALSE./, TM_D /-99.99D0/
      DATA (MESSAG(I),I=1,13)
     $/'Variable name too long                    ',
     $'Under ITASK=5 declared length should be 1 ',
     $'Requested number of values not positive   ',
     $'Variable name not in data file            ',
     $'On data file this is a CHARACTER string   ',
     $'On data file this variable is INTEGER     ',
     $'On data file this variable is REAL        ',
     $'On data file this variable is LOGICAL     ',
     $'On data file this is a DATE_TIME          ',
     $'On data file this variable is an array    ',
     $'Array on data file not of requested size  ',
     $'Array on data file too large              ',
     $'The rerun file contains a CHARACTER string'/
      DATA (MESSAG(I),I=14,26)
     $/'The rerun file contains an INTEGER        ',
     $'The rerun file contains a REAL variable   ',
     $'The rerun file contains a LOGICAL variable',
     $'The rerun file contains a DATE_TIME       ',
     $'On rerun file this variable is an array   ',
     $'Array on rerun file not of requested size ',
     $'Array on rerun file too large to fit      ',
     $'Single precision underflow on data file   ',
     $'Single precision overflow on data file    ',
     $'String(s) on data file too long           ',
     $'Single precision underflow on rerun file  ',
     $'Single precision overflow on rerun file   ',
     $'String(s) on rerun file too long          '/
      IF (.NOT.INIT) THEN
      IPC(IIL-1) = 1
      DO 5 I=IIL-2,0,-1
      IPC(I) = 256 * IPC(I+1)
5     CONTINUE
      IXLMAX = MAX(IIL,IRL,ILL)
      RM = RM_D
      IM = IM_D
      DM = DM_D
      CM = CM_D
      LM = LM_D
      TM = TM_D
      ISCOM  = 0
      INFDAT = 0
      INFREP = 0
      INIT = .TRUE.
      END IF
      IF (ITASK.EQ.1) THEN
      IF (REPL) THEN
      INQUIRE (UNIT=IUNREP, OPENED=OPUNIT)
      IF (OPUNIT) CLOSE (IUNREP)
      END IF
      I = ILEN (FILNAM)
      EMPTY = I.EQ.0
      IF (.NOT.EMPTY) THERE = FLEXIST (FILNAM)
      IF (EMPTY .OR. .NOT.THERE) THEN
      REPL  = OFF
      IS    = 0
      ISLOC = 0
      CALL SWPI4 (3,'RD\IARREP',0,0,0,0)
      CALL SWPI4 (3,'RD\IPTREP',0,0,0,0)
      CALL AMBUSY (1,'RDFROM',ISLOC)
      RETURN
      END IF
      IUNREP = IUNIT
      LOGR   = IULOG.GT.0
      IULR   = IULOG
      RFIL = FILNAM
      ILR  = ILEN (RFIL)
      CALL UPPERC (RFIL)
      CALL EXTENS (RFIL,'TMP',1,REPTMP)
      CALL RDINDX (IWAR,IUNREP,ON,ON,LOGR,IULR,FILNAM,REPTMP,
     $REPLIS,REPTYP,ILNREP,IARREP,IPTREP,ILPREP,INFREP,INSETS)
      IF (IWAR.GT.0) THEN
      WRITE (*,'(/,/,3A,I4)')
     $' Number of errors in rerun file ',RFIL(1:ILR),':',IWAR
      CALL FATALERR (' ',' ')
      END IF
      ISLOC  = 0
      IRREP  = 0
      REPL   = ON
      CALL AMBUSY (1,'RDFROM',ISLOC)
      IS = INSETS
      IF (INSETS.GT.0) THEN
      IF (LOGR) THEN
      WRITE (IULR,'(/,1X,3A,I5,A,I3,A,/,1X,A)')
     $'File ',RFIL(1:ILR),' contains',INSETS,' set(s) of',
     $INFREP,' variable(s):',
     $'type variable name',
     $'---- -------------'
      DO 10 I=1,INFREP
      LXNM31 = REPLIS(I)
      WRITE (IULR,'(2X,A,3X,A)') REPTYP(I),REPLIS(I)
10    CONTINUE
      WRITE (IULR,'(/,1X,A)')
     $'= Data file contents (set 0) selected ='
      END IF
      DO 20 I=1,INFREP
      IRCHK(I) = 0
20    CONTINUE
      END IF
      ISCOM  = ISLOC
      ELSE IF (ITASK.EQ.2) THEN
      IF (IS.EQ.ISLOC) RETURN
      IF (.NOT.REPL) CALL FATALERR ('RDFROM','no rerun file active')
      FATAL = NREQ.GT.0
      IF (ISLOC.GE.1) THEN
      DO 30 I=1,INFREP
      IF (IRCHK(I).EQ.0) THEN
      ILX = ILEN (REPLIS(I))
      IF (LOGR) WRITE (IULR,'(3X,2A)')
     $REPLIS(I)(1:ILX),'   !!!!!!   NOT USED   !!!!!!'
      IF (FATAL) THEN
      WRITE (*,'(/,1X,A,I5,A,I5,3A,/,1X,3A,I5,A)')
     $'- Moving from set',ISLOC,' to set',IS,' of ',
     $RFIL(1:ILR),':','  Variable ',
     $REPLIS(I)(1:ILX),' of set',ISLOC,' was NOT USED'
      CALL FATALERR (' ',' ')
      END IF
      END IF
      IRCHK(I) = 0
30    CONTINUE
      END IF
      IF (IS.GE.0 .AND. IS.LE.INSETS) THEN
      ISLOC = IS
      ELSE
      CALL FATALERR ('RDFROM','illegal set number requested')
      END IF
      CALL AMBUSY (1,'RDFROM',IS)
      IF (IS.EQ.0 .AND. LOGR) WRITE (IULR,'(/,1X,A)')
     $'= Data file contents (set 0) selected ='
      IF (IS.GE.1 .AND. LOGR) WRITE (IULR,'(/,1X,A,I5,3A)')
     $'= Set',IS,' of rerun file ',RFIL(1:ILR),' selected ='
      ISCOM = ISLOC
      ELSE IF (ITASK.EQ.3 .OR. ITASK.EQ.4) THEN
      IF (INIDAT) THEN
      INQUIRE (UNIT=IUNDAT, OPENED=OPUNIT)
      IF (OPUNIT) CLOSE(IUNDAT)
      END IF
      IUNDAT = IUNIT
      LOGD   = IULOG.GT.0
      IULD   = IULOG
      DFIL = FILNAM
      ILD  = ILEN (DFIL)
      CALL UPPERC (DFIL)
      IPS = MAX (ILD-11,1)
      INLIST = IFINDC (FILLIS,INFDEC,1,INFILS,DFIL)
      CALL EXTENS (DFIL,'TMP',1,DATTMP)
      ILTMP = ILEN (DATTMP)
      TEMP  = FLEXIST (DATTMP(1:ILTMP))
      IF (INLIST.EQ.0 .OR. .NOT.TEMP .OR. ITASK.EQ.4) THEN
      CALL RDINDX (IWAR,IUNDAT,OFF,ON,LOGD,IULD,FILNAM,DATTMP,
     $DATLIS,DATTYP,ILNDAT,IARDAT,IPTDAT,ILNDAT,INFDAT,IDUM)
      IF (IWAR.GT.0) THEN
      WRITE (*,'(/,/,3A,I4)')
     $' Number of errors in data file ',DFIL(1:ILD),':',IWAR
      CALL FATALERR (' ',' ')
      ELSE
      IDUM = 1
      CALL RDTMP2 (1,IUNDAT,
     $DATLIS,DATTYP,ILNDAT,IARDAT,IPTDAT,ILNDAT,INFDAT,IDUM)
      IF (LOGD) WRITE (IULD,'(3A,I4,A)')
     $' Data file   ',DFIL(IPS:ILD),
     $' with',INFDAT,' variables parsed by RDINDX'
      END IF
      IF (INLIST.EQ.0 .AND. ITASK.EQ.3) THEN
      IF (INFILS.LT.INFDEC) THEN
      INFILS = INFILS + 1
      FILLIS(INFILS) = DFIL
      ELSE IF (.NOT.GIVEN) THEN
      WRITE (*,'(9(/,2A),2(/,A))')
     $' MESSAGE from RDDATA: After reading with RDINIT',
     $' the data',' file ',DFIL(IPS:ILD),' the list of',
     $' file names appears to be full. Hence, the name',
     $' of the analyzed file cannot be included in the',
     $' list, which',' implies that RDINIT will not',
     $' recover the file contents from',' temporary file',
     $' when you initialize the same file once again.',
     $' When you, in a rerun loop for instance,',
     $' repeatedly use this',' file, the data will be',
     $' parsed again and again. To avoid that,',
     $' use RDPARS for files you initialize only once',
     $' and/or increase',' PARAMETER INFDEC in RDDATA.',
     $' This message will not be repeated for other files.'
      GIVEN = .TRUE.
      END IF
      END IF
      ELSE
      IRECL = ILBUF + IIL
      CALL FOPENG (IUNIT,DATTMP,'OLD','UD',IRECL,' ')
      CALL RDTMP2 (2,IUNDAT,
     $DATLIS,DATTYP,ILNDAT,IARDAT,IPTDAT,ILNDAT,INFDAT,IDUM)
      IF (LOGD) WRITE (IULD,'(1X,3A)') 'Contents of ',
     $DFIL(1:ILD),' recovered from TMP file'
      END IF
      IRDAT  = 0
      INIDAT = ON
      REPMES = OFF
      IF (REPL) THEN
      DO 60 I=1,INFDAT
      LXNM31 = DATLIS(I)
      INR = IFINDC (REPLIS,ILNREP,1,INFREP,LXNM31)
      IF (INR.GT.0) THEN
      IF (DATTYP(I).NE.'-' .AND. REPTYP(INR).NE.'-') THEN
      IF (DATTYP(I).NE.REPTYP(INR)) THEN
      ITD = INDEX (GETONF,DATTYP(I))
      ITR = INDEX (GETONF,REPTYP(INR))
      ILX = ILEN (LXNM31)
      IT1 = ILEN (ONFILE(ITD))
      IT2 = ILEN (ONFILE(ITR))
      WRITE (*,'(/,1X,6A,/,5A)')
     $ONFILE(ITD)(1:IT1),' variable ',LXNM31(1:ILX),
     $' on data file ',DFIL(1:ILD),' occurs',
     $' on rerun file ',RFIL(1:ILR),' as ',
     $ONFILE(ITR)(1:IT2),' variable'
      CALL FATALERR (' ',' ')
      END IF
      END IF
      END IF
60    CONTINUE
      END IF
      ELSE IF (ITASK.EQ.5 .OR. ITASK.EQ.6 .OR. ITASK.EQ.7) THEN
      IF (.NOT.INIDAT) CALL FATALERR ('RDDATA','no data file active')
      SINGLE = ITASK.EQ.5
      UNKNOW = ITASK.EQ.6
      FIXED  = ITASK.EQ.7
      IF (VARTYP.EQ.'C') THEN
      IVT   = 1
      NDEC  = NCDEC
      IXL   = 1
      NELMT = LEN(CX(1))
      ELSE IF (VARTYP.EQ.'D') THEN
      IVT  = 2
      NDEC = NDDEC
      IXL  = IRL
      ELSE IF (VARTYP.EQ.'I') THEN
      IVT  = 3
      NDEC = NIDEC
      IXL  = IIL
      ELSE IF (VARTYP.EQ.'R') THEN
      IVT  = 4
      NDEC = NRDEC
      IXL  = IRL
      ELSE IF (VARTYP.EQ.'L') THEN
      IVT  = 5
      NDEC = NLDEC
      IXL  = ILL
      ELSE IF (VARTYP.EQ.'T') THEN
      IVT   = 6
      NDEC  = NDDEC
      IXL   = IRL
      END IF
      ISX    = MAX (1,ISTART (XNAME))
      ILX    = MAX (1,ILEN (XNAME))
      LXNM31 = XNAME(ISX:ILX)
      CALL UPPERC (LXNM31)
      IND  = IFINDC (DATLIS,ILNDAT,1,INFDAT,LXNM31)
      INR = 0
      IF (REPL) INR = IFINDC (REPLIS,ILNREP,1,INFREP,LXNM31)
      OVERWR = INR.GT.0 .AND. ISLOC.GT.0
      IMES   = 0
      IF (ILX.GT.31) THEN
      IMES = 1
      ELSE IF (SINGLE .AND. NDEC.NE.1) THEN
      IMES = 2
      ELSE IF (FIXED .AND. NREQ.LE.0) THEN
      IMES = 3
      ELSE IF (IND.EQ.0) THEN
      IMES = 4
      ELSE IF (DATTYP(IND).NE.'-' .AND.
     $DATTYP(IND).NE.REQTYP(IVT)) THEN
      IMES = 4 + INDEX (GETONF,DATTYP(IND))
      ELSE
      N = IARDAT(IND)
      IF (SINGLE .AND. N.GT.1) THEN
      IMES = 10
      ELSE IF (FIXED .AND. N.NE.NREQ .AND. .NOT.OVERWR) THEN
      IMES = 11
      ELSE IF (N.GT.NDEC) THEN
      IMES = 12
      END IF
      IF (IMES.EQ.0 .AND. INR.GT.0) THEN
      IF (REPTYP(INR).NE.'-' .AND.
     $REPTYP(INR).NE.REQTYP(IVT)) THEN
      IMES = 12 + INDEX (GETONF,REPTYP(INR))
      END IF
      END IF
      IF (IMES.EQ.0 .AND. OVERWR) THEN
      IPP = (ISLOC-1) * INFREP + INR
      IF (IPP.LE.ILPREP) THEN
      N = IARREP(IPP)
      ELSE
      CALL SWPI4 (2,'RD\IARREP',0,ILPREP,IPP,N)
      END IF
      IF (SINGLE .AND. N.GT.1) THEN
      IMES = 18
      ELSE IF (FIXED .AND. N.NE.NREQ) THEN
      IMES = 19
      ELSE IF (N.GT.NDEC) THEN
      IMES = 20
      END IF
      END IF
      END IF
      IF (IMES.EQ.0) THEN
      IF (.NOT.OVERWR) THEN
      IPNT = IPTDAT(IND)
      IEL  = 0
80    IF (IEL.LT.N) THEN
      IREC = 1 + IPNT/ILBUF
      IF (IREC.NE.IRDAT) THEN
      READ (IUNDAT,REC=IREC) RDBUF,PNTDNX
      IRDAT = IREC
      END IF
      IWN = MOD(IPNT,ILBUF) / IIL
      IREP = IDBUF(IWN)
      IPNT = IPNT + MAX(IXL,IIL)
      IF (MOD(IPNT,ILBUF).EQ.0) IPNT=PNTDNX
      IF (IREP.GT.0) THEN
      FOUND  = OFF
      J      = 0
70    IF (.NOT.FOUND) THEN
      IREC = 1 + IPNT/ILBUF
      IF (IREC.NE.IRDAT) THEN
      READ (IUNDAT,REC=IREC) RDBUF,PNTDNX
      IRDAT = IREC
      END IF
      IWN = MOD(IPNT,ILBUF) / IXL
      IF (VARTYP.EQ.'R') THEN
      AD = ABS(RDBUF(IWN))
      IF (AD.EQ.0.0D0 .OR.
     $(AD.GT.1.0D-38 .AND. AD.LT.1.0D+38)) THEN
      DO 400 I=IEL+1,IEL+IREP
      RX(I) = REAL (RDBUF(IWN))
400   CONTINUE
      ELSE IF (AD.LT.1.0D-38) THEN
      IMES = 21
      ELSE  IF (AD.GT.1.0D+38) THEN
      IMES = 22
      END IF
      FOUND = ON
      ELSE IF (VARTYP.EQ.'I') THEN
      DO 401 I=IEL+1,IEL+IREP
      IX(I) = IDBUF(IWN)
401   CONTINUE
      FOUND   = ON
      ELSE IF (VARTYP.EQ.'D' .OR. VARTYP.EQ.'T') THEN
      DO 404 I=IEL+1,IEL+IREP
      DX(I) = RDBUF(IWN)
404   CONTINUE
      FOUND   = ON
      ELSE IF (VARTYP.EQ.'L') THEN
      DO 405 I=IEL+1,IEL+IREP
      LX(I) = LDBUF(IWN)
405   CONTINUE
      FOUND   = ON
      ELSE IF (VARTYP.EQ.'C') THEN
      IBN  = MOD(IWN,IIL)
      BYTE = CHAR(MOD(IDBUF(IWN/IIL)/IPC(IBN),256))
      IF (BYTE.NE.CHAR(0)) THEN
      IF (J.EQ.0) THEN
      DO 403 I=IEL+1,IEL+IREP
      CX(I) = ' '
403   CONTINUE
      END IF
      J = J + 1
      IF (J.LE.NELMT) THEN
      DO 402 I=IEL+1,IEL+IREP
      CX(I)(J:J) = BYTE
402   CONTINUE
      ELSE
      IMES = 23
      END IF
      ELSE
      FOUND = ON
      END IF
      END IF
      IPNT = IPNT + IXL
      IF (MOD(IPNT,ILBUF).EQ.0) IPNT=PNTDNX
      IF (FOUND) IEL=IEL+IREP
      GOTO 70
      END IF
      ELSE
      DO 408 I=IEL+1,IEL-IREP
      IF (VARTYP.EQ.'R') THEN
      RX(I) = RM
      ELSE IF (VARTYP.EQ.'I') THEN
      IX(I) = IM
      ELSE IF (VARTYP.EQ.'D') THEN
      DX(I) = DM
      ELSE IF (VARTYP.EQ.'C') THEN
      CX(I) = CM
      ELSE IF (VARTYP.EQ.'L') THEN
      LX(I) = LM
      ELSE IF (VARTYP.EQ.'T') THEN
      DX(I) = TM
      END IF
408   CONTINUE
      IEL=IEL-IREP
      END IF
      IPNT = IXLMAX * ((IXLMAX-1 + IPNT)/IXLMAX)
      IF (MOD(IPNT,ILBUF).EQ.0) IPNT=PNTDNX
      GOTO 80
      END IF
      ELSE
      IF (IPP.LE.ILPREP) THEN
      IPNT = IPTREP(IPP)
      ELSE
      CALL SWPI4 (2,'RD\IPTREP',0,ILPREP,IPP,IPNT)
      END IF
      IEL  = 0
100   IF (IEL.LT.N) THEN
      IREC = 1 + IPNT/ILBUF
      IF (IREC.NE.IRREP) THEN
      READ (IUNREP,REC=IREC) RRBUF,PNTRNX
      IRREP = IREC
      END IF
      IWN = MOD(IPNT,ILBUF) / IIL
      IREP = IRBUF(IWN)
      IPNT = IPNT + MAX(IXL,IIL)
      IF (MOD(IPNT,ILBUF).EQ.0) IPNT=PNTRNX
      IF (IREP.GT.0) THEN
      DREAD  = .TRUE.
      FOUND  = OFF
      J      = 0
90    IF (.NOT.FOUND) THEN
      IREC = 1 + IPNT/ILBUF
      IF (IREC.NE.IRREP) THEN
      READ (IUNREP,REC=IREC) RRBUF,PNTRNX
      IRREP = IREC
      END IF
      IWN = MOD(IPNT,ILBUF) / IXL
      IF (VARTYP.EQ.'R') THEN
      AD = ABS(RRBUF(IWN))
      IF (AD.EQ.0.0D0 .OR.
     $(AD.GT.1.0D-38 .AND. AD.LT.1.0D+38)) THEN
      DO 500 I=IEL+1,IEL+IREP
      RX(I) = REAL (RRBUF(IWN))
500   CONTINUE
      ELSE IF (AD.LT.1.0D-38) THEN
      IMES = 24
      ELSE  IF (AD.GT.1.0D+38) THEN
      IMES = 25
      END IF
      FOUND = ON
      ELSE IF (VARTYP.EQ.'I') THEN
      DO 501 I=IEL+1,IEL+IREP
      IX(I) = IRBUF(IWN)
501   CONTINUE
      FOUND   = ON
      ELSE IF (VARTYP.EQ.'D' .OR. VARTYP.EQ.'T') THEN
      DO 504 I=IEL+1,IEL+IREP
      DX(I) = RRBUF(IWN)
504   CONTINUE
      FOUND   = ON
      ELSE IF (VARTYP.EQ.'L') THEN
      DO 505 I=IEL+1,IEL+IREP
      LX(I) = LRBUF(IWN)
505   CONTINUE
      FOUND   = ON
      ELSE IF (VARTYP.EQ.'C') THEN
      IBN  = MOD(IWN,IIL)
      BYTE = CHAR(MOD(IRBUF(IWN/IIL)/IPC(IBN),256))
      IF (BYTE.NE.CHAR(0)) THEN
      IF (J.EQ.0) THEN
      DO 503 I=IEL+1,IEL+IREP
      CX(I) = ' '
503   CONTINUE
      END IF
      J = J + 1
      IF (J.LE.NELMT) THEN
      DO 502 I=IEL+1,IEL+IREP
      CX(I)(J:J) = BYTE
502   CONTINUE
      ELSE
      IMES = 26
      END IF
      ELSE
      FOUND = ON
      END IF
      END IF
      IPNT = IPNT + IXL
      IF (MOD(IPNT,ILBUF).EQ.0) IPNT=PNTRNX
      IF (FOUND) IEL=IEL+IREP
      GOTO 90
      END IF
      ELSE
      DREAD = .FALSE.
      DO 508 I=IEL+1,IEL-IREP
      IF (VARTYP.EQ.'R') THEN
      RX(I) = RM
      ELSE IF (VARTYP.EQ.'I') THEN
      IX(I) = IM
      ELSE IF (VARTYP.EQ.'D') THEN
      DX(I) = DM
      ELSE IF (VARTYP.EQ.'C') THEN
      CX(I) = CM
      ELSE IF (VARTYP.EQ.'L') THEN
      LX(I) = LM
      ELSE IF (VARTYP.EQ.'T') THEN
      DX(I) = TM
      END IF
508   CONTINUE
      IEL=IEL-IREP
      END IF
      IPNT = IXLMAX * ((IXLMAX-1 + IPNT)/IXLMAX)
      IF (MOD(IPNT,ILBUF).EQ.0) IPNT=PNTRNX
      GOTO 100
      END IF
      IRCHK(INR) = IRCHK(INR) + 1
      IF (LOGR.AND.IMES.EQ.0) THEN
      IF (.NOT.REPMES) THEN
      WRITE (IULR,'(3X,2A,I5,A)')
     $'Variable values have been overwritten by ',
     $'rerun set',ISLOC,':'
      REPMES = ON
      END IF
      IF (DREAD) THEN
      ARROW = ' ----->'
      ELSE
      ARROW = ' -DEF->'
      END IF
      WRITE (LINE,'(3X,2A)') LXNM31(1:ILX),ARROW
      PPP    = ' '
      I      = 0
      IVP    = ILEN(LINE)
      IVL    =  LEN(LINE) - 1
      FILLED = OFF
110   IF (I.LT.N .AND. .NOT.FILLED) THEN
      IF (VARTYP.EQ.'D'.OR.VARTYP.EQ.'T') THEN
      WRITE (VALUE,'(1P,G12.4)') DX(I+1)
      ELSE IF (VARTYP.EQ.'R') THEN
      WRITE (VALUE,'(1P,G12.4)') RX(I+1)
      ELSE IF (VARTYP.EQ.'I') THEN
      WRITE (VALUE,'(I12)') IX(I+1)
      ELSE IF (VARTYP.EQ.'C') THEN
      J = ILEN (CX(I+1))
      J = MIN (J,10)
      VALUE = CHAR(39)//CX(I+1)(1:J)//CHAR(39)
      ELSE IF (VARTYP.EQ.'L') THEN
      IF (LX(I+1)) THEN
      VALUE = '.TRUE.'
      ELSE
      VALUE = '.FALSE.'
      END IF
      END IF
      IV1 = ISTART (VALUE)
      IV2 = ILEN (VALUE)
      IPN = IVP + 2 + IV2 - IV1
      FILLED = IPN.GT.IVL
      IF (.NOT.FILLED) THEN
      LINE(IVP+2:IPN) = VALUE(IV1:IV2)
      I   = I + 1
      IVP = IPN
      IF (I.LT.N) THEN
      IVP = IVP + 1
      LINE(IVP:IVP) = ','
      END IF
      END IF
      GOTO 110
      END IF
      IF (I.LT.N) PPP='...'
      WRITE (IULR,'(2A)') LINE(1:IVP),PPP
      END IF
      END IF
      IF (UNKNOW) NREQ = N
      END IF
      IF (IMES.GT.0) THEN
      LOGF = (REPL.AND.LOGR) .OR. LOGD
      IF (REPL.AND.LOGR) THEN
      IUL = IULR
      ELSE IF (LOGD) THEN
      IUL = IULD
      END IF
      IF (SINGLE) THEN
      WHAT   = 'a single value'
      TARGET = ' variable '
      ISTREC = 2
      RECORD(1) = ' '
      ELSE IF (UNKNOW) THEN
      WHAT   = 'an unspecified number of values'
      TARGET = ' array '
      ISTREC = 1
      ELSE IF (FIXED) THEN
      WRITE (WHAT,'(A,I6,A)') 'precisely',NREQ,' values'
      TARGET = ' array '
      ISTREC = 1
      END IF
      ILA = ILEN (ASKED(IVT))
      ILW = ILEN (WHAT)
      ILT = ILEN (TARGET) + 1
      RECORD(ISTREC) =
     $ERRTXT//CALPRG//', attempts to read'
      RECORD(ISTREC+1) =
     $WHAT(1:ILW)//' from file '//DFIL(1:ILD)
      RECORD(ISTREC+2) =
     $'into '//ASKED(IVT)(1:ILA)//TARGET(1:ILT)//LXNM31(1:ILX)
      IF (.NOT.SINGLE) WRITE (RECORD(4),'(A,I5,A)')
     $'(with declared length',NDEC,') :'
      WRITE (*,'(4(/,1X,A))') RECORD
      IF (LOGF) WRITE (IUL,'(6(/,1X,A))')
     $'INPUT ERROR','===========',RECORD
      IF (OVERWR) THEN
      RECORD(1) =
     $'   | RDDATA was instructed to replace data file contents by'
      WRITE (RECORD(2),'(A,I5,3A)')
     $'   | value(s) in set',ISLOC,' of rerun file ',RFIL(1:ILR),'.'
      RECORD(3) =
     $'   | Data are replaced only if variable type is correct '
      RECORD(4) =
     $'   | on both data file and rerun file.'
      WRITE (*,'(1X,A,3(/,1X,A))') RECORD
      IF (LOGF) WRITE (IUL,'(1X,A,3(/,1X,A))') RECORD
      END IF
      IF (LOGF) WRITE (IUL,'(1X,2A)')
     $'ERROR in RDDATA: ',MESSAG(IMES)
      CALL FATALERR ('RDDATA',MESSAG(IMES))
      END IF
      ELSE IF (ITASK.EQ.8) THEN
      IF (.NOT.INIDAT) CALL FATALERR ('RDINQR','no data file active')
      ISX    = MAX (1,ISTART (XNAME))
      ILX    = MAX (1,ILEN (XNAME))
      LXNM31 = XNAME(ISX:ILX)
      CALL UPPERC (LXNM31)
      IS  = IFINDC (DATLIS,ILNDAT,1,INFDAT,LXNM31)
      ELSE IF (ITASK.EQ.9) THEN
      IRECL = ILBUF + IIL
      IF (INIDAT) THEN
      INQUIRE (UNIT=IUNDAT, OPENED=OPUNIT)
      IF (OPUNIT) CLOSE(IUNDAT)
      END IF
      IF (REPL) THEN
      INQUIRE (UNIT=IUNREP, OPENED=OPUNIT)
      IF (OPUNIT) CLOSE (IUNREP)
      END IF
      IF (INIDAT) THEN
      IF (LOGD) WRITE (IULD,'(1X,A)') ' '
      DO 120 I=1,INFILS
      CALL EXTENS (FILLIS(I),'TMP',1,DATTMP)
      ILTMP = ILEN (DATTMP)
      TEMP  = FLEXIST (DATTMP(1:ILTMP))
      IF (TEMP) THEN
      CALL FOPENG (IUNIT,DATTMP,'OLD','UD',IRECL,' ')
      CLOSE (IUNIT,STATUS='DELETE')
      IF (LOGD) WRITE (IULD,'(1X,3A)') 'Temporary file ',
     $DATTMP(1:ILTMP),' deleted by RDDTMP'
      END IF
120   CONTINUE
      INIDAT = OFF
      END IF
      IF (REPL) THEN
      CALL EXTENS (RFIL,'TMP',1,REPTMP)
      ILTMP = ILEN (REPTMP)
      TEMP  = FLEXIST (REPTMP(1:ILTMP))
      IF (TEMP) THEN
      CALL FOPENG (IUNIT,REPTMP,'OLD','UD',IRECL,' ')
      CLOSE (IUNIT,STATUS='DELETE')
      IF (LOGR) WRITE (IULR,'(1X,3A)') 'Temporary file ',
     $REPTMP(1:ILTMP),' deleted by RDDTMP'
      END IF
      REPL = OFF
      CALL AMBUSY (1,'RDFROM',0)
      END IF
      ELSE IF (ITASK.EQ.10) THEN
      IF (VARTYP.EQ.'R') THEN
      RM = RX(1)
      ELSE IF (VARTYP.EQ.'I') THEN
      IM = IX(1)
      ELSE IF (VARTYP.EQ.'D') THEN
      DM = DX(1)
      ELSE IF (VARTYP.EQ.'C') THEN
      CM = CX(1)
      ELSE IF (VARTYP.EQ.'L') THEN
      LM = LX(1)
      ELSE IF (VARTYP.EQ.'T') THEN
      TM = DX(1)
      END IF
      ELSE IF (ITASK.EQ.11) THEN
      RM = RM_D
      IM = IM_D
      DM = DM_D
      CM = CM_D
      LM = LM_D
      TM = TM_D
      ELSE
      CALL FATALERR ('RDDATA','illegal ITASK value')
      END IF
      RETURN
      END
