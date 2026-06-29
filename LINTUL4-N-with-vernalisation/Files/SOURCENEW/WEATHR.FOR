*------------------------------------------------------------------------*
*  SUBROUTINE WEATHR                                                     *
*  Authors: Daniel van Kraalingen                                        *
*  Date   : 17-Feb-1988                                                  *
*  Purpose: This subroutine returns weather data from a standard         *
*           weather data file. This subroutine is made transparent       *
*           in the sense that subsequent calls to this subroutine        *
*           may contain different station names, years or days for       *
*           which weather data are to be returned. No initialization     *
*           is to be carried out by the calling program.                 *
*                                                                        *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)       *
*  name   type meaning                                      units  class *
*  ----   ---- -------                                      -----  ----- *
*  IUNIT   I4  Unit number that can be used to open weather  -      I    * 
*              data file                                     -      C,I  *
*  IWAR    I4  warning variable                              -      C,O  *
*  STATR   C*  Station name of weather data file             -      I    *
*  IYEARR  I4  Year for which weather data are requested                 *
*              (do not use 19 as a prefix, i.e. 86 is a                  *
*              valid year, 1986 is not !)                    y      I    *
*  IDAYR   I4  Day for which weather data are requested      d      I    *  
*              (1 <= IDAYR <= 365)                                       *
*  IGAP    I4  The maximum difference in days that is allowed            *
*              between subsequent data file records. (if                 *
*              this is exceeded IWAR is set to '1'           -      I    *
*  LONGIE  R4  Longitude of the weather station,                         *
*              (East is positive)                        Degrees    O    *
*  LATIN   R4  Latitude of the weather station,                          *
*              (North is positive)                       Degrees    O    *
*  ALTI    R4  Altitude of the weather station,              m      O    *
*  TMIN    R4  Minimum temperature                       Celsius    O    *
*  TMAX    R4  Maximum temperature                       Celsius    O    *
*  DTR     R4  Daily total radiation                      J/m2/d    O    *
*  RAIN    R4  Rainfall                                    cm/d     O    *
*  VAP     R4  Vapour pressure at 9:00 in the morning      mbar     O    *
*  WIND    R4  Average wind speed                          m/s      O    *
*  ISTN    I4  station number                                            *
*  WTRDIR  C*  directory of weather data                            I    *   
*                                                                        *
*  FATAL ERROR CHECKS (execution terminated, message):                   *
*  IDAYR < 1,  IDAYR > 365                                               *
*  IDAYR < day of first weather record                                   *
*  IDAYR > day of last weather record                                    *
*                                                                        *
*  WARNINGS                                   value of IWAR returned     *
*  If difference in days between weather records                         *
*  is larger than IGAP                                   1               *
*                                                                        *
*  SUBROUTINES and FUNCTIONS called : MOFILP, ILEN                       *
*                                                                        *
*  FILE usage : A weather data file opened with UNIT=10                  *
*------------------------------------------------------------------------*
*-------------------------------------------------------------------------*
* Copyright 2013. Wageningen University, Plant Production Systems group,  *
* P.O. Box 430, 6700 AK Wageningen, The Netherlands.                      *
* You may not use this work except in compliance with the Licence.        *
* You may obtain a copy of the Licence at:                                *
*                                                                         *
* http://models.pps.wur.nl/content/licence-agreement                      *
*                                                                         *
* Unless required by applicable law or agreed to in writing, software     *
* distributed under the Licence is distributed on an "AS IS" basis,       *
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.*
*-------------------------------------------------------------------------*

      SUBROUTINE WEATHR (IWAR,WTRDIR,STATR,IYEARR,IDAYR,IGAP,LONGIE,
     $                LATIN,ALTI,TMIN,TMAX,DTR,RAIN,VAP,WIND)

      IMPLICIT REAL (A-Z)
      CHARACTER*(*) STATR
      CHARACTER HULP*2, FILE*80, STATO*80, WTRDIR*80
      INTEGER ISR, ISO, IGAP, ILEN, IWAR,
     &        IYEARO, IYEARR, IYEARN, ISTN, 
     &        IDAYO, IDAYR, IDAYN, IYEARC
      LOGICAL OPENED

      DATA OPENED /.FALSE./
      DATA ISO /1/, STATO /'X'/
      DATA IYEARO /-1/

*---- the requested day should be between 1 and 365

      IF (IDAYR.LT.1.OR.IDAYR.GT.365) 
     &    STOP 'ERROR IN WEATHR: DAY < 1 OR DAY > 365'

      ISR = LEN_TRIM (STATR)

*---- if a new station name is requested and/or a new year is requested,
*     a new weather data file will be opened, if a weather data file 
*     was already opened is is first closed.

      IF (STATO(1:ISO).NE.STATR(1:ISR).OR.IYEARO.NE.IYEARR) THEN

*------- construction of file name from station name and year number 
*        by making use of an 'internal write' on the string: HULP;
*        for actual year 2000 or later, IYEARR = 100, 101, etc. and IYEARC= 0, 1, etc. for file name 

         IYEARC= IYEARR
	   IF (IYEARR .GE. 100) IYEARC= IYEARR - 100
         WRITE (HULP,'(I2)') IYEARC
         IF (HULP(1:1).EQ.' ') HULP(1:1) = '0'

         FILE = STATR(1:ISR)//HULP//'.DAT'

*------- before opening a weather data file, it is checked if a file was 
*        already open, if true, it is closed

         IF (OPENED) CLOSE (10)
         
         I1= LEN_TRIM (WTRDIR)
           IF (I1 .GT. 0) THEN
           OPEN (10,FILE= WTRDIR(1:I1)//FILE(1:ISR+6), STATUS='OLD')
           OPENED = .TRUE.
           ELSE
           OPEN (10,FILE= FILE(1:ISR+6), STATUS='OLD')
           OPENED = .TRUE.
           ENDIF  

*------- comment lines (coded with '*') are skipped and longitude, 
*        latitude and altitude are read

         CALL MOFILP (10)
         READ (10,*,ERR=200) LONGIE, LATIN, ALTI, A, B

         STATO(1:ISR) = STATR(1:ISR)
         ISO = ISR
         IYEARO = IYEARR
         IDAYN = 0
         IDAYO = 0
      END IF

*---- if the requested station name and year remain the same, but the 
*     requested day is less than where the file pointer currently is 
*     located, the file is rewound

      IF (IDAYR.LT.IDAYO) THEN
         REWIND (10)

*------- comment lines (coded with '*') are skipped and longitude, 
*        latitude and altitude are read

         CALL MOFILP (10)
         READ (10,*,ERR=200) LONGIE, LATIN, ALTI, A, B

         IDAYN = 0
         IDAYO = 0
      END IF

*---- in this loop, the file is read until the requested day is reached 
*     or exceeded, each weather variable is saved to allow  interpolation 
*     later

10    IF (IDAYN.LT.IDAYR) THEN
         IDAYO = IDAYN
         TMINO = TMINN
         TMAXO = TMAXN
         DTRO  = DTRN
         RAINO = RAINN
         VAPO  = VAPN
         WINDO = WINDN
         READ (10,*,END=100,ERR=200) ISTN, IYEARN, IDAYN, DTRN, TMINN, 
     &                        TMAXN, VAPN, WINDN, RAINN
        
         GOTO 10
      ELSE
         IF (IDAYO.EQ.0.AND.IDAYN.GT.IDAYR) STOP 
     &      'ERROR in WEATHR: interpolation attempted below data'
         IF (IDAYN.GT.IDAYR.AND.(IDAYN-IDAYO).GT.IGAP) IWAR = 1
      END IF

*---- intermediate variable used for interpolation

      FRACT = FLOAT (IDAYR-IDAYO)/FLOAT (IDAYN-IDAYO)

*---- actual interpolation of weather data

      TMIN = TMINO + FRACT*(TMINN-TMINO)
      TMAX = TMAXO + FRACT*(TMAXN-TMAXO)
      DTR  = DTRO  + FRACT*(DTRN -DTRO)

      IF (IDAYR.EQ.IDAYN) THEN
         RAIN = RAINN
      ELSE
         RAIN = 0.
      END IF

      VAP  = VAPO  + FRACT*(VAPN -VAPO)
      WIND = WINDO + FRACT*(WINDN-WINDO)

*---- conversion to cm/d, to J/m2/d and to mbar

      RAIN = RAIN/10.
      DTR  = DTR*1.E3
      VAP  = VAP * 10.
      
      RETURN

*---- fatal error is reached at an unexpected end of file

100   STOP 'ERROR in WEATHR: interpolation attempted above data'
200   STOP 'ERROR in WEATHR: unable to read from data file'

      END
