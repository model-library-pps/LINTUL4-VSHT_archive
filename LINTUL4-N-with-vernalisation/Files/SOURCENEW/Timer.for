*---------------------------------------------------------------------------*
*  SUBROUTINE TIMER                                                         *
*  Author : Rob Groot                                                       *
*  Date   : december 1986                                                   *
*  Purpose: This subroutine updates TIME with time step of 1 day            *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                    units  class         *
*  ----     -------                                    -----  -----         *
*  IFINIT   maximum number of days for single run        d      I,T         *
*  INITI    logical, indicates start of simulation run          I           *
*  DVS      development stage                            -      I           *
*  DVSEND   development stage at maturity (=2.0) or harvest -   I           *
*  IYEAR    running value for year                       -      O,T         *
*  IDAY     julian daynumber                             -      O,T         *
*  TERMIN   logical, simulation is finished when .TRUE.         O           *
*  YCH      indicates change of year                     -      O           *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : none                                                        *
*---------------------------------------------------------------------------*

      SUBROUTINE TIMER(IDAY,IYEAR,IFINIT,TERMIN,INITI,DVS,DVSEND,YCH)

      IMPLICIT REAL (A-Z)
      INTEGER IDAY,IYEAR,IFINIT,IRUN
      LOGICAL TERMIN,INITI,YCH


*---- write daynumber to screen

      CALL CLS      
      WRITE (*,*) IDAY
      IF (INITI) IRUN = 0

*---- update daynumber

      IDAY = IDAY+1

*---- running value for time
      IRUN = IRUN+1


*---- end of simulation is induced when finish time is reached          

      IF (IRUN.EQ.IFINIT .OR. DVS .GE. DVSEND) THEN
         TERMIN=.TRUE.
         WRITE (*,'(///A)') ' finish conditions reached'
      ENDIF


*---- January 1 is defined as DAY=1

      IF (IDAY.GT.365) THEN
         IDAY = 1
         IYEAR = IYEAR+1
	   YCH= .TRUE.
      END IF


      RETURN 
      END
