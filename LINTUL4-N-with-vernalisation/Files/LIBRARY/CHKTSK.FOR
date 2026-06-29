* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE CHKTSK (MODULE, IULOG, ITOLD, ITASK)
      IMPLICIT NONE
      INTEGER IULOG, ITOLD, ITASK
      CHARACTER*(*) MODULE
      INTEGER ITABLE(4,4), ISTAT, IMES
      CHARACTER MESSAG(10)*58
      SAVE
      DATA ITABLE /1,  0, 10,  7,
     &2,  3,  0,  0,
     &2,  0, -5,  8,
     &0, -4, -6, -9/
      DATA MESSAG/
     &'Repeated initialization !                                 ',
     &'No Terminal call done before initialization !             ',
     &'Repeated rate calculation !                               ',
     &'Rate calculation impossible after terminal call !         ',
     &'Repeated integration not allowed !                        ',
     &'Integration impossible after terminal call !              ',
     &'Terminal call without simulation !                        ',
     &'Terminal call after integration, some output may be lost !',
     &'Repeated terminal call not allowed !                      ',
     &'Integration after initialization, no rates available !    '/
      IF (ITASK.LT.1.OR.ITASK.GT.4) CALL FATALERR (MODULE,'wrong ITASK')
      IF (ITOLD.LT.1.OR.ITOLD.GT.4) CALL FATALERR (MODULE,'wrong ITOLD')
      ISTAT = ITABLE(ITASK, ITOLD)
      IMES  = ABS (ISTAT)
      IF (ISTAT.LT.0) THEN
      CALL FATALERR (MODULE, MESSAG(IMES))
      ELSE IF (ISTAT.GT.0) THEN
      WRITE (*,'(4A)')
     &' WARNING from ',MODULE,': ',MESSAG(IMES)
      IF (IULOG.NE.0) WRITE (IULOG,'(4A)')
     &' WARNING from ',MODULE,': ',MESSAG(IMES)
      END IF
      RETURN
      END
