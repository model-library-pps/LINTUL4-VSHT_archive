* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDSETS (IUNIT,IULOG,SETFIL,INS)
      IMPLICIT NONE
      INTEGER IUNIT,IULOG,INS
      CHARACTER*(*) SETFIL
      DOUBLE PRECISION D(1)
      REAL             R(1)
      INTEGER          I(1)
      CHARACTER*1      C(1)
      LOGICAL          L(1)
      INTEGER IL,IS,ILF,ILEN
      CHARACTER*132 SCRREC
      LOGICAL WGIVEN
      INTEGER SCRREC_L
      SAVE
      DATA WGIVEN /.FALSE./
      IL = 0
      CALL RDDATA (1,'RDSETS',IUNIT,IULOG,SETFIL,INS,' ',' ',
     $D,R,I,C,L,1,1,1,1,1,IL)
      IS = 0
      CALL AMBUSY (1,'RDFROM',IS)
      IF (IULOG.GT.0 .AND. INS.GT.0) THEN
      ILF = ILEN (SETFIL)
      WRITE (*,'(1X,A)')
     $'Message from RDSETS: A logfile report is written'
      SCRREC_L = 21
      SCRREC = 'about the use of the '
      CALL ADDINT (SCRREC,SCRREC_L,INS)
      CALL ADDSTF (SCRREC,SCRREC_L,' parameter sets on ')
      CALL ADDSTR (SCRREC,SCRREC_L,SETFIL(1:ILF))
      WRITE (*,'(1X,A)') SCRREC(1:SCRREC_L)
      WGIVEN = .FALSE.
      ELSE IF (INS.GT.0.AND..NOT.WGIVEN) THEN
      WRITE (*,'(1X,2A)') 'WARNING from RDSETS: ',
     $'no logfile is used !!'
      WGIVEN = .TRUE.
      END IF
      RETURN
      END
