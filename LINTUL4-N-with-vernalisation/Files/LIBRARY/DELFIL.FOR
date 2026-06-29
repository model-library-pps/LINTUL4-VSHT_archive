** $Id: delfil.for 1.1 1997/10/02 16:08:11 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE DELFIL (FILE_NAME,NOT_EX_ERR)
      IMPLICIT NONE
      CHARACTER*(*) FILE_NAME
      LOGICAL NOT_EX_ERR
      LOGICAL THERE
      INTEGER UN,GETUN,ILEN,IL,IOS
      CHARACTER*132 LFNAME
      SAVE
      IL = ILEN (FILE_NAME)
      IF (IL.EQ.0) CALL FATALERR ('DELFIL','empty file name')
      LFNAME = FILE_NAME
      CALL FLNAME (LFNAME)
      INQUIRE (FILE=LFNAME(1:IL),EXIST=THERE)
      IF (THERE) THEN
      UN = GETUN (10,99)
      OPEN (UN,FILE=LFNAME(1:IL),STATUS='OLD',
     &ACCESS='SEQUENTIAL',FORM='UNFORMATTED',IOSTAT=IOS)
      IF (IOS.NE.0) CALL FATALERR ('DELFIL','cannot open file')
      CLOSE (UN,STATUS='DELETE',IOSTAT=IOS)
      IF (IOS.NE.0) CALL FATALERR ('DELFIL','cannot open file')
      ELSE IF (.NOT.THERE.AND.NOT_EX_ERR) THEN
      CALL FATALERR ('DELFIL','file to be deleted does not exist')
      END IF
      RETURN
      END
