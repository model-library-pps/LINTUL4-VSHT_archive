** $Id: dtnow.for 1.1 1997/10/02 16:08:11 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE DTNOW (DATEA)
      IMPLICIT NONE
      INTEGER DATEA(6)
      INTEGER*2 IYR, IMON, IDAY, IHR, IMIN, ISEC, I100TH
      SAVE
      CALL GETDAT (IYR,IMON,IDAY)
      CALL GETTIM (IHR,IMIN,ISEC,I100TH)
      DATEA(1) = IYR
      DATEA(2) = IMON
      DATEA(3) = IDAY
      DATEA(4) = IHR
      DATEA(5) = IMIN
      DATEA(6) = ISEC
      RETURN
      END
