** $Id: ttuver.for 1.1 1997/10/02 16:08:14 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE TTUVER (MIN_V)
      IMPLICIT NONE
      REAL MIN_V,CUR_V
      PARAMETER (CUR_V=4.07)
      IF (MIN_V.GT.CUR_V) THEN
      WRITE (*,'(1X,A,/,1X,A,F5.2,/,1X,A,F5.2,A)')
     &'This program is not linked with the minimal TTUTIL version',
     &'This is TTUTIL version:',CUR_V,
     &'At least version      :',MIN_V,' is required'
      CALL FATALERR ('TTUVER',' ')
      END IF
      RETURN
      END
