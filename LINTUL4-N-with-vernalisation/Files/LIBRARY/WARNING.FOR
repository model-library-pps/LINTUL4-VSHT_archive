** $Id: warning.for 1.1 1997/10/02 16:08:14 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE WARNING (MODULE,MESSAG)
      IMPLICIT NONE
      CHARACTER*(*) MODULE, MESSAG
      INTEGER IL1,IL2,ILEN
      CHARACTER*1 DUMMY
      SAVE
      IL1 = ILEN (MODULE)
      IL2 = ILEN (MESSAG)
      IF (IL1.EQ.0.AND.IL2.EQ.0) THEN
      WRITE (*,'(A)')
     &' WARNING from unspecified source'
      ELSE IF (IL1.GT.0.AND.IL2.EQ.0) THEN
      WRITE (*,'(2A)')
     &' WARNING from ',MODULE(1:IL1)
      ELSE
      WRITE (*,'(4A)')
     &' WARNING from ',MODULE(1:IL1),': ',MESSAG(1:IL2)
      END IF
      READ (*,'(A)') DUMMY
      RETURN
      END
