* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE FOPENS (IUNIT,FILNAM,STATUS,PRIV)
      IMPLICIT NONE
      INTEGER IUNIT
      CHARACTER*(*) FILNAM, STATUS, PRIV
      INTEGER IRECL
      CHARACTER*2 TYPE
      SAVE
      TYPE  = 'FS'
      IRECL = 0
      CALL FOPENG (IUNIT,FILNAM,STATUS,TYPE,IRECL,PRIV)
      RETURN
      END
