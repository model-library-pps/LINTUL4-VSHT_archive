* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE ERROR (MODULE,MESSAG)
      IMPLICIT NONE
      CHARACTER*(*) MODULE, MESSAG
      INTEGER I,IL1,IL2,ILEN
      CHARACTER*1 DUMMY
      SAVE
      I = 0
      IF (I.EQ.0) THEN
      IL1 = ILEN (MODULE)
      IL2 = ILEN (MESSAG)
      IF (IL1.EQ.0.AND.IL2.EQ.0) THEN
      WRITE (*,'(A)')
     &' Fatal execution error, press <Enter>'
      ELSE IF (IL1.GT.0.AND.IL2.EQ.0) THEN
      WRITE (*,'(3A)')
     &' Fatal execution error in ',MODULE(1:IL1),', press <Enter>'
      ELSE
      WRITE (*,'(4A,/,A)')
     &' ERROR in ',MODULE(1:IL1),': ',MESSAG(1:IL2),
     &' Press <Enter>'
      END IF
      WRITE (*,*) '(Use the FATALERR routine in the future !)'
      READ (*,'(A)') DUMMY
      STOP
      END IF
      RETURN
      END
