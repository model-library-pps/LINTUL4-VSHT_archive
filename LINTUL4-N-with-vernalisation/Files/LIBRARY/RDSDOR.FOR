** $Id: rdsdor.for 1.1 1997/10/02 16:08:13 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDSDOR (XNAME,XMIN,XMAX,X)
      IMPLICIT NONE
      DOUBLE PRECISION X, XMIN, XMAX
      CHARACTER*(*) XNAME
      DOUBLE PRECISION D(1)
      REAL             R(1)
      INTEGER          I(1)
      CHARACTER*1      C(1)
      LOGICAL          L(1)
      INTEGER IL,IS,ILX,ILEN
      CHARACTER Q*1,LXNAME*31
      SAVE
      DATA Q /''''/
      IL = 0
      CALL RDDATA (5,'RDSDOR',0,0,' ',IS,XNAME,'D',D,R,I,C,L,
     $1,1,1,1,1,IL)
      X      = D(1)
      LXNAME = XNAME
      ILX    = ILEN (LXNAME)
      IF (XMAX.LT.XMIN) THEN
      WRITE (*,'(1X,4A)')
     &'ERROR in RDSDOR: Range not valid of identifier ',
     &Q,LXNAME(1:ILX),Q
      CALL FATALERR (' ',' ')
      ELSE IF (X.LT.XMIN.OR.X.GT.XMAX) THEN
      WRITE (*,'(1X,5A,/,T19,A,G12.5,A,G12.5,A,G12.5,A)')
     &'ERROR in RDSDOR: Range error of identifier ',
     &Q,LXNAME(1:ILX),Q,',',
     &'value =',X,', range = [',XMIN,',',XMAX,']'
      CALL FATALERR (' ',' ')
      END IF
      RETURN
      END
