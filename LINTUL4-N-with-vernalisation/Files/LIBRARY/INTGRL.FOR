* This file was intentionally removed of
* comment lines and indentation
      REAL FUNCTION INTGRL (STATE, RATE, DELT)
      IMPLICIT NONE
      REAL STATE, RATE, DELT
      SAVE
      INTGRL = STATE + RATE * DELT
      RETURN
      END
