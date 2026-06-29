* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE OUTDAT (ITASK, IUNIT, RN, R)
      IMPLICIT NONE
      INTEGER ITASK, IUNIT
      REAL R
      CHARACTER*(*) RN
      INTEGER NAMES_MXN, VARL_M
      INTEGER NAMES_IB_MXN_4,NAMES_IB_MXN_5
      INTEGER NAMES_IB_MXN_7,NAMES_IB_MXN_8
      PARAMETER (NAMES_IB_MXN_4=8)
      PARAMETER (NAMES_IB_MXN_5=38)
      PARAMETER (NAMES_IB_MXN_7=8)
      PARAMETER (NAMES_IB_MXN_8=NAMES_IB_MXN_5)
      PARAMETER (NAMES_MXN=500,VARL_M=36)
      CHARACTER*(VARL_M) AVN(NAMES_MXN)
      INTEGER            IAVNL(NAMES_MXN)
      CHARACTER*(VARL_M) ASELN(NAMES_MXN)
      INTEGER            BLK(NAMES_MXN)
      LOGICAL            MGIVEN(NAMES_MXN)
      INTEGER            SEQ2(NAMES_MXN)
      INTEGER            FND(NAMES_MXN)
      CHARACTER*(VARL_M) LXN, LN
      REAL               AVV(NAMES_IB_MXN_5)
      LOGICAL            FNDA(NAMES_IB_MXN_5)
      INTEGER            SEQ(NAMES_IB_MXN_5)
      INTEGER            EXTRA_SP(NAMES_IB_MXN_5),EXTRA_SPX
      INTEGER COL_WIDTH_MNN,LINE_LEN,CENTRE
      INTEGER LINE_LEN_4,LINE_LEN_5,LINE_LEN_7,LINE_LEN_8
      PARAMETER (COL_WIDTH_MNN=12,CENTRE=8)
      PARAMETER (LINE_LEN_4=
     &COL_WIDTH_MNN+2+NAMES_IB_MXN_4*(COL_WIDTH_MNN+1))
      PARAMETER (LINE_LEN_5=
     &COL_WIDTH_MNN+2+NAMES_IB_MXN_5*(COL_WIDTH_MNN+1))
      PARAMETER (LINE_LEN_7=
     &COL_WIDTH_MNN+2+NAMES_IB_MXN_7*(COL_WIDTH_MNN+1))
      PARAMETER (LINE_LEN_8=LINE_LEN_5)
      CHARACTER*(LINE_LEN_5) LINE
      INTEGER IFND
      INTEGER IFND2
      LOGICAL FULL_BLOCK
      INTEGER LINE_L
      LOGICAL EXTRA_F
      INTEGER ILU1, ILU2, ICHECK, IOS, INSEL, ISEL, ISAVE
      INTEGER IRUN1, IRUN2, IRUN3, IRUN4, IRUN5
      INTEGER ITOLD, ILTASK, INAME, IBLOK, ILEN, ILXN, IR
      INTEGER ISREC, IREC, IEREC, IB, I1, I2, I3, I4, I5, I6, IFINDC
      REAL LV, LVO
      CHARACTER CHR*1, RUNTYP*1, RUNDUM*1, TEXT(4:8)*18, COMMCHR*1
      LOGICAL OPEND, OK, YFND, RECOVR, SELECTED, FIRSTIT8
      CHARACTER*80 SPACE
      SAVE
      DATA TEXT /'Table output','Spreadsheet output',
     &'2 column output','ICASA output',
     &'End of run output'/
      DATA ITOLD /0/, IRUN1 /0/, IRUN2 /0/, INSEL /0/, RECOVR /.FALSE./
      DATA FIRSTIT8 /.TRUE./
      IF (ITASK.EQ.1) THEN
      IF (ITOLD.EQ.0) THEN
      IF (IUNIT.EQ.0) CALL FATALERR
     &('OUTDAT','no unit number supplied')
      ILU2 = IUNIT
      ELSE
      IF (IUNIT.NE.0.AND.IUNIT.NE.ILU2) CALL FATALERR
     &('OUTDAT','change of unit number not allowed')
      IF (ITOLD.EQ.1) THEN
      WRITE (*,'(2A)')    ' WARNING from OUTDAT:',
     &' ignoring repeated initialization'
      WRITE (ILU2,'(2A)') ' WARNING from OUTDAT:',
     &' ignoring repeated initialization'
      RETURN
      ELSE IF (ITOLD.EQ.2) THEN
      CONTINUE
      ELSE IF (ITOLD.EQ.3) THEN
      WRITE (*,'(2A)')    ' WARNING from OUTDAT:',
     &' selected variables discarded'
      WRITE (ILU2,'(2A)') ' WARNING from OUTDAT:',
     &' selected variables discarded'
      ELSE IF (ITOLD.GE.4) THEN
      IF (RECOVR) CALL FATALERR ('OUTDAT',
     &'normal OUTDAT call impossible after recovery')
      END IF
      END IF
      ILU1 = ILU2+1
      CALL AMBUSY (1,'OUTDAT',ILU2)
      INQUIRE (UNIT=ILU2, OPENED=OPEND)
      IF (.NOT.OPEND) CALL FOPENG (ILU2,'RES.DAT','NEW','SF',0,'DEL')
      INQUIRE (UNIT=ILU1, OPENED=OPEND)
      IF (.NOT.OPEND) THEN
      CALL FOPENG (ILU1,'RES.BIN','NEW','UD',45,'DEL')
      IRUN4 = 0
      IREC  = 1
      ISREC = 0
      ELSE
      IF (ITOLD.EQ.0) CALL FATALERR ('OUTDAT',
     &'temporary file may not be opened outside OUTDAT')
      END IF
      CALL OUTCOM ('<INIT$$$>')
      DO I1=1,NAMES_MXN
      AVN(I1)    = ' '
      FND(I1)    = 0
      MGIVEN(I1) = .FALSE.
      END DO
      CALL AMBUSY (2,'RDFROM',IRUN2)
      IF (IRUN2.EQ.0) THEN
      IRUN3  = IRUN1
      RUNTYP = 'N'
      ELSE
      IRUN3  = IRUN2
      RUNTYP = 'R'
      END IF
      IRUN1 = IRUN1+1
      IRUN4 = IRUN4+1
      WRITE (ILU1,REC=1)
     &'-',IRUN4,'....................................',0.
      IF (IRUN4.GE.2) WRITE (ILU1,REC=ISREC)
     &' ',IREC,'....................................',0.
      IREC  = IREC+1
      ISREC = IREC
      IF (LEN (RN).LE.VARL_M) THEN
      LXN = RN
      ELSE
      I1 = ILEN (RN)
      WRITE (*,'(2A,/,2A)')
     &' WARNING from OUTDAT: variable ',RN(1:I1),
     &' is too long for OUTDAT, it is truncated to: ',
     &RN(1:VARL_M)
      WRITE (ILU2,'(2A,/,2A)')
     &' WARNING from OUTDAT: variable ',RN(1:I1),
     &' is too long for OUTDAT, it is truncated to: ',
     &RN(1:VARL_M)
      LXN = RN(1:VARL_M)
      END IF
      CALL UPPERC (LXN)
      ILXN  = ILEN (LXN)
      WRITE (ILU1, REC=IREC)
     &' ',-99,'....................................',0.
      IREC  = IREC+1
      WRITE (ILU1, REC=IREC) RUNTYP,IRUN3,LXN,R
      IFND  = 0
      INSEL = 0
      ISAVE = 0
      SPACE = ' '
      ELSE IF (ITASK.EQ.2) THEN
      IF (ITOLD.GE.3) CALL FATALERR
     &('OUTDAT','Initialization not done')
      IF (LEN (RN).LE.VARL_M) THEN
      LN = RN
      ELSE
      I1 = ILEN (RN)
      WRITE (*,'(2A,/,2A)')
     &' WARNING from OUTDAT: variable ',RN(1:I1),
     &' is too long for OUTDAT, it is truncated to: ',
     &RN(1:VARL_M)
      WRITE (ILU2,'(2A,/,2A)')
     &' WARNING from OUTDAT: variable ',RN(1:I1),
     &' is too long for OUTDAT, it is truncated to: ',
     &RN(1:VARL_M)
      LN = RN(1:VARL_M)
      END IF
      CALL UPPERC (LN)
      OK    = .TRUE.
      INAME = 0
      IF (LN.EQ.LXN) THEN
      DO I1=1,IFND
      FND(I1) = 0
      END DO
      ISAVE = 0
      ELSE
      I1 = ISAVE+1
      IF (I1.GT.IFND) I1 = 1
      IF (I1.GT.ISAVE) THEN
30    IF (I1.LE.IFND.AND.INAME.EQ.0) THEN
      IF (LN.EQ.AVN(I1)) INAME = I1
      I1 = I1+1
      GOTO 30
      END IF
      END IF
      IF (INAME.EQ.0) THEN
      I1 = 1
35    IF (I1.LE.ISAVE.AND.INAME.EQ.0) THEN
      IF (LN.EQ.AVN(I1)) INAME = I1
      I1 = I1+1
      GOTO 35
      END IF
      END IF
      IF (INAME.EQ.0) THEN
      IFND = IFND+1
      IF (IFND.GT.NAMES_MXN) CALL FATALERR
     &('OUTDAT','too many variables for output')
      AVN(IFND)  = LN
      INAME      = IFND
      FND(INAME) = 1
      ELSE
      FND(INAME) = FND(INAME)+1
      IF (FND(INAME).EQ.2) THEN
      OK = .FALSE.
      IF (.NOT.MGIVEN(INAME)) THEN
      MGIVEN(INAME) = .TRUE.
      I1 = ILEN (LN)
      WRITE (*,'(4A)')
     &' WARNING from OUTDAT: variable ',LN(1:I1),
     &' supplied without new independent ',
     &LXN(1:ILXN)
      WRITE (ILU2,'(4A)')
     &' WARNING from OUTDAT: variable ',LN(1:I1),
     &' supplied without new independent ',
     &LXN(1:ILXN)
      END IF
      END IF
      END IF
      END IF
      IF (OK) THEN
      IREC = IREC+1
      WRITE (ILU1,REC=IREC) ' ',INAME, LN, R
      END IF
      ISAVE = INAME
      ELSE IF (ITASK.EQ.3) THEN
      IF (ITOLD.EQ.1.OR.ITOLD.EQ.99) THEN
      WRITE (*,'(2A)')    ' WARNING from OUTDAT:',
     &' wrong task preceeds selection of output'
      WRITE (ILU2,'(2A)') ' WARNING from OUTDAT:',
     &' wrong task preceeds selection of output'
      RETURN
      ELSE IF (ITOLD.EQ.2.OR.(ITOLD.GE.4.AND.ITOLD.LE.16)) THEN
      INSEL = 0
      END IF
      LN = RN
      CALL UPPERC (LN)
      IF (INSEL.EQ.0) THEN
      I1 = 0
      ELSE
      I1 = IFINDC (ASELN,INSEL,1,INSEL,LN)
      END IF
      IF (I1.EQ.0) THEN
      INSEL = INSEL+1
      IF (INSEL.GT.NAMES_MXN) CALL FATALERR
     &('OUTDAT','too many variables selected')
      ASELN(INSEL) = LN
      END IF
      ELSE IF ((ITASK.GE.4 .AND.ITASK.LE.8).OR.
     &(ITASK.GE.14.AND.ITASK.LE.18)) THEN
      IF (ITOLD.EQ.0) THEN
      RECOVR = .TRUE.
      IF (IUNIT.LE.10) CALL FATALERR
     &('OUTDAT','illegal unit number')
      ILU2 = IUNIT
      ILU1 = ILU2+1
      CALL FOPENG (ILU1,'RES.BIN','OLD','UD',45,' ')
      INQUIRE (UNIT=ILU2, OPENED=OPEND)
      IF (.NOT.OPEND) CALL FOPENG
     &(ILU2,'RECOVER.DAT','NEW','SF',0,'DEL')
      READ (ILU1,REC=1) RUNTYP,IRUN5
      IF (RUNTYP.EQ.'-') THEN
      IEREC = 1
      DO I1=1,IRUN5-1
      ISREC = IEREC+1
      READ (ILU1,REC=ISREC) RUNDUM,IEREC
      END DO
      IOS = 0
      IR = IEREC+2
45    IF (IOS.EQ.0) THEN
      READ (ILU1,REC=IR,IOSTAT=IOS) RUNDUM
      IF (IOS.EQ.0) IR = IR+1
      GOTO 45
      END IF
      WRITE (ILU1, REC=IEREC+1)
     &' ',IR-1 ,'....................................',0.
      WRITE (ILU1, REC=1)
     &'+',IRUN5,'....................................',0.
      END IF
      ELSE IF (ITOLD.EQ.ITASK) THEN
      WRITE (*,'(2A)')    ' WARNING from OUTDAT:',
     &' new output and previous output have same format'
      WRITE (ILU2,'(2A)') ' WARNING from OUTDAT:',
     &' new output and previous output have same format'
      ELSE IF (ITOLD.EQ.99) THEN
      WRITE (*,'(2A)')    ' WARNING from OUTDAT:',
     &' output table cannot be created, no RES.BIN file'
      WRITE (ILU2,'(2A)') ' WARNING from OUTDAT:',
     &' output table cannot be created, no RES.BIN file'
      RETURN
      ELSE IF (ITOLD.EQ.3.OR.ITOLD.EQ.2.OR.ITOLD.EQ.1) THEN
      IEREC = IREC
      WRITE (ILU1, REC=ISREC)
     &' ',IREC ,'....................................',0.
      READ  (ILU1, REC=1)     RUNDUM,IRUN5
      WRITE (ILU1, REC=1)
     &'+',IRUN5,'....................................',0.
      END IF
      ILTASK = MOD (ITASK,10)
      IF (ITASK.GE.14) THEN
      I6 = 1
      ELSE
      I6 = IRUN5
      END IF
      DO I5=I6,IRUN5
      IF (ITASK.GE.14.OR.ITOLD.EQ.0) THEN
      IEREC = 1
      DO I1=1,I5
      ISREC = IEREC+1
      READ (ILU1,REC=ISREC) RUNDUM,IEREC
      END DO
      READ (ILU1,REC=ISREC+1) RUNTYP, IRUN3, LXN
      ILXN  = ILEN (LXN)
      IFND  = 0
      DO IR=ISREC+2,IEREC
      READ (ILU1,REC=IR) RUNDUM, INAME, LN
      IFND = MAX (INAME, IFND)
      IF (INAME.GT.0) AVN(INAME) = LN
      END DO
      END IF
      FULL_BLOCK = .TRUE.
      IBLOK      = 0
      I2         = 1+MAX (COL_WIDTH_MNN+1,ILXN)
      IF (ILTASK.EQ.4) THEN
      LINE_LEN = LINE_LEN_4
      ELSE IF (ILTASK.EQ.5) THEN
      LINE_LEN = LINE_LEN_5
      ELSE IF (ILTASK.EQ.6) THEN
      LINE_LEN = LINE_LEN_4
      ELSE IF (ILTASK.EQ.7) THEN
      LINE_LEN = LINE_LEN_7
      ELSE IF (ILTASK.EQ.8) THEN
      LINE_LEN = LINE_LEN_8
      END IF
      DO I1=1,IFND
      IF (INSEL.EQ.0) THEN
      SELECTED = .TRUE.
      ELSE
      ISEL = IFINDC (ASELN,INSEL,1,INSEL,AVN(I1))
      IF (ISEL.GT.0) THEN
      SELECTED = .TRUE.
      ELSE
      SELECTED = .FALSE.
      END IF
      END IF
      IF (SELECTED) THEN
      IF (FULL_BLOCK) THEN
      IBLOK      = IBLOK+1
      FULL_BLOCK = .FALSE.
      END IF
      IAVNL(I1) = ILEN (AVN(I1)(1:35))
      IF (IAVNL(I1).EQ.0) THEN
      WRITE (*,'(A)')
     &' WARNING from OUTDAT: zero length variable name'
      WRITE (ILU2,'(A)')
     &' WARNING from OUTDAT: zero length variable name'
      IAVNL(I1) = 1
      END IF
      IF (ILTASK.EQ.6) THEN
      FULL_BLOCK   = .TRUE.
      ELSE
      I3 = 1+MAX (COL_WIDTH_MNN,IAVNL(I1))
      IF (I2+I3.GT.LINE_LEN) THEN
      IBLOK = IBLOK+1
      I2    = 1+MAX (COL_WIDTH_MNN+1,ILXN)
      END IF
      I2 = I2+I3
      FULL_BLOCK = .FALSE.
      END IF
      BLK(I1) = IBLOK
      ELSE
      BLK(I1) = 0
      END IF
      END DO
      IF (IBLOK.EQ.0) THEN
      WRITE (*,'(A)')
     &' WARNING from OUTDAT: no output values given'
      WRITE (ILU2,'(/,A)')
     &' WARNING from OUTDAT: no output values given'
      ELSE IF (IBLOK.GT.1.AND.ILTASK.EQ.8) THEN
      CALL FATALERR ('OUTDAT',
     &'more than one block with end of run option')
      END IF
      IF (ILTASK.EQ.4.OR.ILTASK.EQ.5.OR.ILTASK.EQ.7) THEN
      COMMCHR = '*'
      IF (ILTASK.EQ.7) COMMCHR = '!'
      WRITE (ILU2,'(A,76A1)') COMMCHR,('-',I1=1,76)
      IF (IRUN3.EQ.0) THEN
      WRITE (ILU2,'(2A)')
     &COMMCHR,' Output table number  :  0 (=first output table)'
      ELSE IF (RUNTYP.EQ.'R') THEN
      WRITE (ILU2,'(2A,I5)')
     &COMMCHR,' Output from rerun set:',IRUN3
      ELSE IF (RUNTYP.EQ.'N') THEN
      WRITE (ILU2,'(2A,I5)')
     &COMMCHR,' Output table number  :',IRUN3
      ELSE
      CALL FATALERR ('OUTDAT','unknown run type')
      END IF
      WRITE (ILU2,'(3A,/,3A)')
     &COMMCHR,' Output table format  : ',TEXT(ILTASK),
     &COMMCHR,' ',RN
      IF (ILTASK.NE.7) THEN
      CALL OUTCOM ('<PRINT$$$>')
      END IF
      ELSE IF (ILTASK.EQ.8) THEN
      IF (FIRSTIT8) THEN
      WRITE (ILU2,'(A,76A1)') '*',('-',I1=1,76)
      WRITE (ILU2,'(2A)')
     &'* Output table format  : ',TEXT(ILTASK)
      CALL OUTCOM ('<PRINT$$$>')
      END IF
      END IF
      DO IB=1,IBLOK
      IFND2 = 0
      DO I1=1,IFND
      IF (BLK(I1).EQ.IB) THEN
      IFND2      = IFND2+1
      SEQ(IFND2) = I1
      SEQ2(I1)   = IFND2
      END IF
      END DO
      IF (ILTASK.EQ.4.OR.ILTASK.EQ.7) THEN
      WRITE (ILU2,'(A)') ' '
      CHR = ' '
      LINE    = ' '
      LINE_L  = 1
      IF (ILTASK.EQ.4) THEN
      IF (ILXN.LE.CENTRE+1) THEN
      I3 = LINE_L+1+CENTRE+1-ILXN
      I4 = LINE_L+CENTRE+1
      LINE(I3:I4) = LXN(1:ILXN)
      LINE_L = LINE_L+COL_WIDTH_MNN+1
      EXTRA_SPX = 0
      ELSE
      LINE(LINE_L+1:LINE_L+ILXN) = LXN(1:ILXN)
      EXTRA_SPX = MAX (ILXN-COL_WIDTH_MNN+1,0)
      LINE_L = LINE_L+COL_WIDTH_MNN+1+EXTRA_SPX
      END IF
      ELSE IF (ILTASK.EQ.7) THEN
      LINE   = '@'
      LINE_L = 1
      CALL ADDSTR (LINE,LINE_L,LXN)
      CALL ADDSTR (LINE,LINE_L,'>')
      LINE_L    = 14
      EXTRA_SPX = 0
      END IF
      EXTRA_F = EXTRA_SPX.GT.0
      DO I1=1,IFND2
      I2 = SEQ(I1)
      LINE_L = LINE_L+1
      IF (IAVNL(I2).LE.CENTRE) THEN
      I3 = LINE_L+1+CENTRE-IAVNL(I2)
      I4 = LINE_L+CENTRE
      LINE(I3:I4) = AVN(I2)(1:IAVNL(I2))
      LINE_L = LINE_L+COL_WIDTH_MNN
      EXTRA_SP(I1) = 0
      ELSE
      LINE(LINE_L+1:LINE_L+IAVNL(I2)) =
     &AVN(I2)(1:IAVNL(I2))
      EXTRA_SP(I1) = MAX (IAVNL(I2)-COL_WIDTH_MNN,0)
      LINE_L = LINE_L+COL_WIDTH_MNN+EXTRA_SP(I1)
      END IF
      IF (EXTRA_SP(I1).GT.0) EXTRA_F = .TRUE.
      END DO
      WRITE (ILU2,'(A)') LINE(1:LINE_L)
      WRITE (ILU2,'(A)') ' '
      EXTRA_SPX = EXTRA_SPX+1
      DO I1=1,IFND2
      EXTRA_SP(I1) = EXTRA_SP(I1)+1
      END DO
      ELSE IF (ILTASK.EQ.5.OR.
     &(ILTASK.EQ.8.AND.FIRSTIT8)) THEN
      WRITE (ILU2,'(A)') ' '
      CHR = CHAR (9)
      LINE   = ' '
      LINE_L = 0
      CALL ADDSTR (LINE,LINE_L,LXN)
      DO I1=1,IFND2
      CALL ADDSTR (LINE,LINE_L,CHR)
      CALL ADDSTR (LINE,LINE_L,
     &AVN(SEQ(I1))(1:IAVNL(SEQ(I1))))
      END DO
      EXTRA_F = .FALSE.
      WRITE (ILU2,'(A)') LINE(1:LINE_L)
      WRITE (ILU2,'(A)') ' '
      IF (ILTASK.EQ.8.AND.FIRSTIT8) FIRSTIT8 = .FALSE.
      ELSE IF (ILTASK.EQ.6) THEN
      CHR = ' '
      WRITE (ILU2,'(4A,/,A,/,1X,4A)')
     &'* ',LXN(1:ILXN),CHR,AVN(SEQ(1))(1:IAVNL(SEQ(1))),
     &' 1 1 1',
     &AVN(SEQ(1))(1:IAVNL(SEQ(1))),'(',LXN(1:ILXN),')'
      EXTRA_F = .FALSE.
      END IF
      YFND = .FALSE.
      IF (ILTASK.EQ.8) THEN
      LN = ' '
      IR = IEREC
26    IF (LN.NE.LXN.AND.IREC.GT.ISREC) THEN
      IR = IR-1
      READ (ILU1,REC=IR) RUNDUM, INAME, LN
      GOTO 26
      END IF
      ISREC = IR-1
      END IF
      DO IR=ISREC+1,IEREC
      READ (ILU1,REC=IR) RUNDUM, INAME, LN, LV
      IF (LN.EQ.LXN) THEN
      IF (YFND) THEN
      IF (ICHECK.EQ.IFND2) THEN
      IF (.NOT.EXTRA_F) THEN
      WRITE (ILU2,'(1X,1P,G13.6,255(A,G12.5))')
     &LVO,(CHR,AVV(I1),I1=1,IFND2)
      ELSE
      WRITE (ILU2,'(A,1P,G13.6,255(A,G12.5))')
     &SPACE(1:EXTRA_SPX),LVO,
     &(SPACE(1:EXTRA_SP(I1)),AVV(I1),I1=1,IFND2)
      END IF
      ELSE
      IF (.NOT.EXTRA_F) THEN
      LINE   = ' '
      LINE_L = 1
      CALL ADDREF (LINE,LINE_L,LVO,'1P,G13.6')
      DO I1=1,IFND2
      CALL ADDSTF (LINE,LINE_L,CHR)
      IF (FNDA(I1)) THEN
      CALL ADDREF (LINE,LINE_L,
     &AVV(I1),'1P,G12.5')
      ELSE
      LINE(LINE_L+CENTRE:LINE_L+CENTRE) = '-'
      LINE_L = LINE_L+COL_WIDTH_MNN
      END IF
      END DO
      ELSE
      LINE   = ' '
      LINE_L = EXTRA_SPX
      CALL ADDREF (LINE,LINE_L,LVO,'1P,G13.6')
      DO I1=1,IFND2
      LINE_L = LINE_L+EXTRA_SP(I1)
      IF (FNDA(I1)) THEN
      CALL ADDREF (LINE,LINE_L,
     &AVV(I1),'1P,G12.5')
      ELSE
      LINE(LINE_L+CENTRE:LINE_L+CENTRE) = '-'
      LINE_L = LINE_L+COL_WIDTH_MNN
      END IF
      END DO
      END IF
      WRITE (ILU2,'(A)') LINE(1:LINE_L)
      END IF
      YFND = .FALSE.
      END IF
      DO I1=1,IFND2
      FNDA(I1) = .FALSE.
      AVV(I1)  = -99.
      END DO
      ICHECK = 0
      LVO = LV
      ELSE
      IF (BLK(INAME).EQ.IB) THEN
      AVV(SEQ2(INAME))  = LV
      YFND              = .TRUE.
      ICHECK            = ICHECK+1
      FNDA(SEQ2(INAME)) = .TRUE.
      END IF
      END IF
      END DO
      IF (YFND) THEN
      IF (ICHECK.EQ.IFND2) THEN
      IF (.NOT.EXTRA_F) THEN
      WRITE (ILU2,'(1X,1P,G13.6,255(A,G12.5))')
     &LVO,(CHR,AVV(I1),I1=1,IFND2)
      ELSE
      WRITE (ILU2,'(A,1P,G13.6,255(A,G12.5))')
     &SPACE(1:EXTRA_SPX),LVO,
     &(SPACE(1:EXTRA_SP(I1)),
     &AVV(I1),I1=1,IFND2)
      END IF
      ELSE
      IF (.NOT.EXTRA_F) THEN
      LINE   = ' '
      LINE_L = 1
      CALL ADDREF (LINE,LINE_L,LVO,'1P,G13.6')
      DO I1=1,IFND2
      CALL ADDSTF (LINE,LINE_L,CHR)
      IF (FNDA(I1)) THEN
      CALL ADDREF (LINE,LINE_L,AVV(I1),'1P,G12.5')
      ELSE
      LINE(LINE_L+CENTRE:LINE_L+CENTRE) = '-'
      LINE_L = LINE_L+COL_WIDTH_MNN
      END IF
      END DO
      ELSE
      LINE   = ' '
      LINE_L = EXTRA_SPX
      CALL ADDREF (LINE,LINE_L,LVO,'1P,G13.6')
      DO I1=1,IFND2
      LINE_L = LINE_L+EXTRA_SP(I1)
      IF (FNDA(I1)) THEN
      CALL ADDREF (LINE,LINE_L,
     &AVV(I1),'1P,G12.5')
      ELSE
      LINE(LINE_L+CENTRE:LINE_L+CENTRE) = '-'
      LINE_L = LINE_L+COL_WIDTH_MNN
      END IF
      END DO
      END IF
      WRITE (ILU2,'(A)') LINE(1:LINE_L)
      END IF
      END IF
      END DO
      IF (ILTASK.EQ.4.OR.ILTASK.EQ.5) WRITE (ILU2,'(/,/,/,1X)')
      END DO
      ELSE IF (ITASK.EQ.99) THEN
      IF (ITOLD.EQ.99) CALL FATALERR ('OUTDAT',
     &'Temporary file already deleted')
      CLOSE (ILU1, STATUS='DELETE')
      ELSE IF (ITASK.EQ.0) THEN
      CONTINUE
      ELSE
      CALL FATALERR ('OUTDAT','wrong ITASK')
      END IF
      ITOLD = ITASK
      RETURN
      END
