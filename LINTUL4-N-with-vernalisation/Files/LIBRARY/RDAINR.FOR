** $Id: rdainr.for 1.1 1997/10/02 16:08:12 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDAINR (XNAME,XMIN,XMAX,X,ILDEC,IFND)
      IMPLICIT NONE
      INTEGER ILDEC,IFND
      INTEGER X,XMIN,XMAX
      CHARACTER*(*) XNAME
      DIMENSION X(ILDEC)
      DOUBLE PRECISION D(1)
      REAL             R(1)
      CHARACTER*1      C(1)
      LOGICAL          L(1)
      INTEGER IS,I1,ILX,ILEN
      INTEGER XL
      CHARACTER Q*1, LXNAME*31
      SAVE
      DATA Q /''''/
      CALL RDDATA (6,'RDAINR',0,0,' ',IS,XNAME,'I',
     $D,R,X,C,L,1,1,ILDEC,1,1,IFND)
      LXNAME = XNAME
      ILX    = ILEN (LXNAME)
      DO 10 I1=1,IFND
      XL = X(I1)
      IF (XMAX.LT.XMIN) THEN
      WRITE (*,'(1X,4A)')
     &'ERROR in RDAINR: Range not valid of identifier ',
     &Q,LXNAME(1:ILX),Q
      CALL FATALERR (' ',' ')
      ELSE IF (XL.LT.XMIN.OR.XL.GT.XMAX) THEN
      WRITE (*,'(1X,4A,I3,3A,/,T19,A,I8,A,I8,A,I8,A)')
     &'ERROR in RDAINR: Range error of identifier ',
     &Q,LXNAME(1:ILX),'(',I1,')',Q,',',
     &'value =',XL,', range = [',XMIN,',',XMAX,']'
      CALL FATALERR (' ',' ')
      END IF
10    CONTINUE
      RETURN
      END
