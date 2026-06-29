** $Id: dtdpst.for 1.1 1997/10/02 16:08:11 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE DTDPST (FORM,DPDTTM,STRNG)
      IMPLICIT NONE
      CHARACTER*(*) FORM, STRNG
      DOUBLE PRECISION DPDTTM
      INTEGER DATEA(6)
      REAL FSEC
      INTEGER MNW
      PARAMETER (MNW=20)
      INTEGER IWBEG(MNW),IWEND(MNW)
      INTEGER I1, I2, IFNDO, IS, IE, ILEN, ILTMPS, ILFORM, IC, IFND
      INTEGER IFINDG, ILSTR,IDUM
      LOGICAL CHRNOW,WASCHR,FSFND
      CHARACTER LFORM*80
      CHARACTER YR*4,MO*2,DAY*2,HR*2,MN*2,SEC*2,FS*8
      CHARACTER TMPS*9
      INTEGER NEDNAM
      PARAMETER (NEDNAM=9)
      CHARACTER EDNAM(NEDNAM)*8
      CHARACTER MONN(12)*9
      SAVE
      DATA  EDNAM /'YEAR','MONTH','DAY',
     &'HOUR','MINUTE','SECONDS','FSECONDS',
     &'MONTHST','MONTHLT'/
      DATA MONN /'January','February','March','April',
     &'May','June','July','August',
     &'September','October','November','December'/
      CALL DTDPAR (DPDTTM,DATEA,FSEC)
      DO 10 I1=1,MNW
      IWBEG(I1) = 0
      IWEND(I1) = 0
10    CONTINUE
      ILFORM = ILEN (FORM)
      IF (ILFORM.EQ.0) CALL FATALERR ('DTDPST','empty format string')
      IF (ILFORM.GT.LEN(FORM)) CALL FATALERR ('DTDPST',
     &'format string too long')
      LFORM = ' '
      LFORM = FORM(1:ILFORM)
      CALL UPPERC (LFORM)
      IFND   = 0
      WASCHR = .FALSE.
      DO 20 I1=1,ILFORM
      IC = ICHAR (LFORM(I1:I1))
      CHRNOW = (IC.GE.65.AND.IC.LE.90).OR.(IC.GE.97.AND.IC.LE.122)
      IF (.NOT.WASCHR .AND. CHRNOW) THEN
      IFND = IFND + 1
      IWBEG(IFND) = I1
      ELSE IF (WASCHR.AND..NOT.CHRNOW) THEN
      IWEND(IFND) = I1 - 1
      IF (IFND.EQ.MNW) CALL FATALERR ('DTDPST',
     &'date/time format too long')
      END IF
      WASCHR = CHRNOW
20    CONTINUE
      IF (WASCHR) IWEND(IFND) = ILFORM
      FSFND = .FALSE.
      DO 30 I1=1,IFND
      CALL SFINDG (EDNAM,NEDNAM,1,NEDNAM,
     &LFORM(IWBEG(I1):IWEND(I1)),2,
     &IFINDG,IDUM)
      IF (IFINDG.EQ.7) FSFND = .TRUE.
30    CONTINUE
      WRITE (YR ,'(I4.4)') DATEA(1)
      WRITE (MO ,'(I2.2)') DATEA(2)
      WRITE (DAY,'(I2.2)') DATEA(3)
      WRITE (HR ,'(I2.2)') DATEA(4)
      WRITE (MN ,'(I2.2)') DATEA(5)
      IF (FSFND) THEN
      WRITE (SEC,'(I2.2)') DATEA(6)
      WRITE (FS ,'(F8.6)') FSEC
      IF (FS(1:1).EQ.' ') FS(1:1) = '0'
      ELSE
      I2 = DATEA(6)
      IF (FSEC.GE.0.5) I2 = I2+1
      WRITE (SEC,'(I2.2)') I2
      FS = ' '
      END IF
      STRNG  = ' '
      ILSTR  = LEN (STRNG)
      ILTMPS = 0
      IS     = 0
      IE     = 0
      IF (IWBEG(1).GT.1) THEN
      IS = IE+1
      IE = IS+(IWBEG(1)-1)-1
      IF (IE.GT.ILSTR) CALL FATALERR
     &('DTDPST','output string too small')
      STRNG(IS:IE) = LFORM(1:IWBEG(1)-1)
      END IF
      IFINDG = 6
      DO 40 I1=1,IFND
      IFNDO = IFINDG
      CALL SFINDG (EDNAM,NEDNAM,1,NEDNAM,
     &LFORM(IWBEG(I1):IWEND(I1)),2,
     &IFINDG,IDUM)
      IF (IFINDG.EQ.1) THEN
      TMPS = YR
      ELSE IF (IFINDG.EQ.2) THEN
      TMPS = MO
      ELSE IF (IFINDG.EQ.3) THEN
      TMPS = DAY
      ELSE IF (IFINDG.EQ.4) THEN
      TMPS = HR
      ELSE IF (IFINDG.EQ.5) THEN
      TMPS = MN
      ELSE IF (IFINDG.EQ.6) THEN
      TMPS = SEC
      ELSE IF (IFINDG.EQ.7) THEN
      IF (IFNDO.NE.6) CALL FATALERR ('DTDPST',
     &'Fractional seconds not allowed after previous descriptor')
      I2 = ILEN (FS)
      IF (IE.GE.1) THEN
      IF (STRNG(IE:IE).EQ.'.') THEN
      TMPS = FS(3:I2)
      ELSE
      TMPS = FS(1:I2)
      END IF
      ELSE
      TMPS = FS(1:I2)
      END IF
      ELSE IF (IFINDG.EQ.8) THEN
      TMPS = MONN(DATEA(2))(1:3)
      ELSE IF (IFINDG.EQ.9) THEN
      TMPS = MONN(DATEA(2))
      ELSE
      WRITE (*,*) 'Edit descriptor: ',LFORM(IWBEG(I1):IWEND(I1))
      CALL FATALERR ('DTDPST','Date/time edit descriptor unknown')
      END IF
      ILTMPS = ILEN (TMPS)
      IS     = IE+1
      IE     = IS+ILTMPS-1
      IF (IE.GT.ILSTR) CALL FATALERR
     &('DTDPST','output string too small')
      STRNG(IS:IE) = TMPS(1:ILTMPS)
      IF (I1.LT.IFND) THEN
      IS = IE+1
      IE = IS+(IWBEG(I1+1)-IWEND(I1)-1)-1
      IF (IE.GT.ILSTR) CALL FATALERR
     &('DTDPST','output string too small')
      STRNG(IS:IE) = LFORM(IWEND(I1)+1:IWBEG(I1+1)-1)
      ELSE IF (IWEND(IFND).LT.ILFORM) THEN
      IS = IE+1
      IE = IS+(ILFORM+1-IWEND(I1)-1)-1
      IF (IE.GT.ILSTR) CALL FATALERR
     &('DTDPST','output string too small')
      STRNG(IS:IE) = LFORM(IWEND(I1)+1:ILFORM)
      END IF
40    CONTINUE
      RETURN
      END
