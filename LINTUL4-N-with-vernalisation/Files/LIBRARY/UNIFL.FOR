* This file was intentionally removed of
* comment lines and indentation
      REAL FUNCTION UNIFL()
      IMPLICIT NONE
      INTEGER JX,K
      DIMENSION JX(3)
      LOGICAL INIT
      SAVE
      DATA INIT/.FALSE./
      IF (.NOT.INIT) THEN
      JX(2) = 1122334455
      JX(3) = 1408222472
      INIT = .TRUE.
      END IF
      K = JX(2) / 53668
      JX(2) = 40014 * (JX(2) - K * 53668)  -  K * 12211
      IF (JX(2).LT.0)  JX(2) = JX(2) + 2147483563
      K = JX(3) / 52774
      JX(3) = 40692 * (JX(3) - K * 52774)  -  K * 3791
      IF (JX(3).LT.0)  JX(3) = JX(3) + 2147483399
      K = JX(3) - JX(2)
      IF (K.LE.0)  K = K + 2147483562
      JX(1) = K
      UNIFL = K * 4.656613E-10
      RETURN
      END
