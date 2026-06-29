* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE FOPENG (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV)
      IMPLICIT NONE
      INTEGER IUNIT,IRECL
      CHARACTER*(*) FILNAM, STATUS, PRIV, TYPE
      INTEGER ILF,ILFIL,ITMP,IWL,IOS
      CHARACTER CHOICE*1,CREAT*4,EXT*3,FFF*11,FTYPE*4
      CHARACTER LFNAME*132,LPRIV*3,LSTAT*3,LTYPE*2
      LOGICAL DELOLD,KNPRIV,OPENU,OPENF,SEQ,THERE,OK
      INTEGER ILEN
      SAVE
      IOS    = 0
      CALL STR_COPY (FILNAM,LFNAME,OK)
      IF (.NOT.OK) THEN
      WRITE (*,'(/,A)')
     &'ERROR in FOPENG: File name too long for internal buffer'
      CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF
      CALL FLNAME (LFNAME)
      ILFIL = ILEN (LFNAME)
      IF (IUNIT.LT.10.OR.IUNIT.GT.99) THEN
      WRITE (*,'(/,A)')
     &' ERROR in FOPENG: Unit number is < 10 or > 99 !'
      CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF
      INQUIRE (UNIT=IUNIT,OPENED=OPENU)
      IF (OPENU) THEN
      INQUIRE (UNIT=IUNIT, NAME=LFNAME)
      ILF = ILEN (LFNAME)
      WRITE (*,'(/,A,/,1X,A)')
     &' ERROR in FOPENG: Unit number is already in use for file',
     &LFNAME(1:ILF)
      CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF
      LSTAT  = STATUS
      CALL UPPERC (LSTAT)
      IF (LSTAT.EQ.'RDO') LSTAT = 'OLD'
      IF (ILEN (STATUS).NE.3.OR.
     &(LSTAT.NE.'OLD'.AND.LSTAT.NE.'NEW')) THEN
      WRITE (*,'(/,A)') ' ERROR in FOPENG: illegal file status'
      CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF
      LTYPE  = TYPE
      CALL UPPERC (LTYPE)
      IF (ILEN (TYPE).NE.2) THEN
      WRITE (*,'(/,A)') ' ERROR in FOPENG: Illegal TYPE description'
      CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      ELSE IF (INDEX ('FUB',LTYPE(1:1)).EQ.0.AND.
     &INDEX ('FUB',LTYPE(2:2)).EQ.0) THEN
      WRITE (*,'(/,A)') ' ERROR in FOPENG: No FORM specified'
      CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      ELSE IF (INDEX ('SD',LTYPE(1:1)).EQ.0.AND.
     &INDEX ('SD',LTYPE(2:2)).EQ.0) THEN
      WRITE (*,'(/,A)') ' ERROR in FOPENG: No ACCESS specified'
      CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF
      IF (INDEX (LTYPE,'F').GT.0) THEN
      FFF = 'FORMATTED'
      ELSE IF (INDEX (LTYPE,'U').GT.0) THEN
      FFF = 'UNFORMATTED'
      ELSE IF (INDEX (LTYPE,'B').GT.0) THEN
      FFF = 'BINARY'
      END IF
      SEQ = INDEX (LTYPE,'S') .GT. 0
      IF ((.NOT.SEQ .AND. IRECL.LE.0).OR.
     &(SEQ .AND. IRECL.NE.0)) THEN
      WRITE (*,'(/,2A)')
     &' ERROR in FOPENG:',
     &' Illegal record length for direct access file'
      CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF
      LPRIV  = PRIV
      CALL UPPERC (LPRIV)
      IF (LSTAT.EQ.'OLD'.AND.LPRIV.NE.' ') THEN
      CALL WARNING ('FOPENG',
     &'status OLD does not require a privilege')
      ELSE IF (LSTAT.EQ.'NEW') THEN
      KNPRIV = LPRIV.EQ.'DEL' .OR. LPRIV.EQ.'NOD' .OR. LPRIV.EQ.'UNK'
      IF (ILEN (PRIV).NE.3.OR..NOT.KNPRIV) THEN
      WRITE (*,'(/,A)') ' ERROR in FOPENG: illegal privilege'
      CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF
      END IF
      THERE  = .FALSE.
      OPENF  = .FALSE.
      IF (ILFIL.EQ.0) THEN
      WRITE (*,'(/,A)') ' ERROR in FOPENG: zero length file name'
      CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      ELSE
      INQUIRE (FILE=LFNAME(1:ILFIL),EXIST=THERE,IOSTAT=IOS)
      IF (IOS.NE.0) THEN
      WRITE (*,'(/,2A)')
     &' ERROR in FOPENG: INQUIRE for file existence not',
     &' successfull, check IOS value'
      CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF
      IF (THERE) INQUIRE (FILE=LFNAME(1:ILFIL),OPENED=OPENF)
      IF (THERE.AND.OPENF) THEN
      WRITE (*,'(/,A)') ' ERROR in FOPENG: File is already open'
      CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF
      END IF
      IF (LSTAT.EQ.'OLD'.AND..NOT.THERE) THEN
      WRITE (*,'(/,A)') ' ERROR in FOPENG: File does not exist'
      CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF
      IF (LSTAT.EQ.'NEW' .AND. THERE) THEN
      DELOLD = .FALSE.
      IF (LPRIV.EQ.'UNK') THEN
      ITMP = 0
10    IF (THERE) THEN
      WRITE (*,'(3A)')
     &' File ',LFNAME(1:ILFIL),' already exists'
20    CONTINUE
      CALL ENTDCH ('Overwrite (Y/N)','N',CHOICE)
      CALL UPPERC (CHOICE)
      IF (CHOICE.EQ.'Y') THEN
      DELOLD = .TRUE.
      ELSE IF (CHOICE.EQ.'N') THEN
      ITMP = ITMP + 1
      WRITE (EXT,'(I3.3)') ITMP
      CALL EXTENS (FILNAM,EXT,0,LFNAME)
      CALL ENTDCH ('Enter new file name',LFNAME,LFNAME)
      ILFIL = ILEN (LFNAME)
      THERE = .FALSE.
      INQUIRE (FILE=LFNAME(1:ILFIL),EXIST=THERE,IOSTAT=IOS)
      IF (IOS.NE.0) THEN
      WRITE (*,'(/,2A)')
     &' ERROR in FOPENG: INQUIRE for file existence not',
     &' successfull, check IOS value'
      CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,
     &PRIV,IOS)
      END IF
      GOTO 10
      ELSE
      GOTO 20
      END IF
      END IF
      ELSE IF (LPRIV.EQ.'DEL') THEN
      DELOLD  = .TRUE.
      ELSE IF (LPRIV.EQ.'NOD') THEN
      WRITE (*,'(/,A)')
     &' ERROR in FOPENG: Existing file cannot be deleted'
      CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF
      IF (DELOLD) THEN
      OPEN (IUNIT,FILE=LFNAME(1:ILFIL),STATUS='OLD',
     &ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
      CLOSE (IUNIT,STATUS='DELETE',IOSTAT=IOS)
      IF (IOS.NE.0) THEN
      WRITE (*,'(/,A)')
     &' ERROR in FOPENG: existing file cannot be deleted'
      CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF
      END IF
      END IF
      ILF = ILEN (FFF)
      CALL LOWERC (LFNAME)
      IWL = IRECL
      IF (SEQ) OPEN (IUNIT,FILE=LFNAME(1:ILFIL),STATUS=LSTAT,
     &ACCESS='SEQUENTIAL',FORM=FFF(1:ILF),IOSTAT=IOS)
      IF (.NOT.SEQ) OPEN (IUNIT,FILE=LFNAME(1:ILFIL),STATUS=LSTAT,
     &ACCESS='DIRECT',FORM=FFF(1:ILF),RECL=IWL,IOSTAT=IOS)
      IF (IOS.NE.0) THEN
      WRITE (*,'(/,A)')
     &' ERROR in FOPENG: system error while trying to open file'
      CALL FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      END IF
      RETURN
      END
      SUBROUTINE FOPERR (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV,IOS)
      IMPLICIT NONE
      INTEGER IUNIT,IRECL,IOS
      CHARACTER*(*) FILNAM, STATUS, PRIV, TYPE
      INTEGER ILEN, ILFIL
      SAVE
      ILFIL = MAX (ILEN (FILNAM),1)
      WRITE (*,'(/,A,/,A,I5,3(/,2A),/,A,I4,/,2A)')
     &' Arguments of the CALL to FOPENG leading to this error:',
     &'   Unit             = ',IUNIT,
     &'   File name        =    ',FILNAM(1:ILFIL),
     &'   File status      =    ',STATUS,
     &'   File type        =    ',TYPE,
     &'   Record length    = ',IRECL,
     &'   Delete privilege =    ',PRIV
      IF (IOS.NE.0) THEN
      WRITE (*,'(A,I6,A)')
     &'   IOSTAT           = ',IOS,
     &' <-- system I/O status code'
      END IF
      CALL FATALERR (' ',' ')
      RETURN
      END
