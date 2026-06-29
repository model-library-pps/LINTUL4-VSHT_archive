*---------------------------------------------------------------------*
*                                                                     *
*     PROGRAM LINTUL4V.FOR                                            *
*     Author : joost wolf                                             *
*     Date of last revision:  October 2011                            *
*                                                                     *
*     Purpose: This model is the LINTUL-3 fst model but in FORTRAN    *
*              It simulates the growth of a crop as                   *
*              function of intercepted radiation, temperature and     *
*              light use efficiency. Soil water (free drainage) and   *
*              simple nitrogen balances                               *
*              are simulated and also the effects of water and        *
*              nitrogen supply on crop growth. This version includes  *
*              vernalisation effect on phenological development       *
*---------------------------------------------------------------------*
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

      PROGRAM LINTUL4V

      IMPLICIT REAL (A-Z)
      INTEGER ISYR,ISYEAR,IUWE,IIYR,INYEAR,IDPL,IDEM,IFINIT,IGAP,ISOIL
      INTEGER ICROP,IOPT,IOUT,IDAY,IYEAR,IWAR,IRRI,IDEMERG,IDFLOW
      INTEGER IDHALT,IAIRDU

      CHARACTER RUNNAME*5,REMARK*80,STATR*5,CONTIN*1,WTRDIR*80
      COMMON /ITIME/ ISYR, ISYEAR

      LOGICAL TERMIN,INITI,TERMY,PL,EMERG,YCH      
      
*-----iunit number for weather
      DATA IUWE/65/

*---- initialize program
*     LOOP for different weather stations

      
1      CALL INITP (IIYR,INYEAR,IDPL,IDEM,IFINIT,IGAP,REMARK,RUNNAME,
     $  STATR,CONTIN,SENSP,SENSV,SENSW,SENSR,SENST,CO,
     $  ISOIL,ICROP,IOPT,IRRI,IOUT,WTRDIR)


       TERMY= .FALSE.

*---- LOOP for a number of years

      IYEAR = IIYR

5     CONTINUE
            
      TERMIN = .FALSE.

      IF (IDPL .LE. 0) THEN
        IDAY= IDEM
        PL= .FALSE.
      ELSE 
        IDAY= IDPL
        PL= .TRUE.
      ENDIF
      
      ISYR = IYEAR 
      ISYEAR= IYEAR + 1900
      INITI = .TRUE.

*---- LOOP for one growth period (year)
        
	 YCH= .FALSE.
        
10       CONTINUE

*------- read weather data
         CALL WEATHR (IWAR,WTRDIR,STATR,IYEAR,IDAY,IGAP,LONGIE,LATIN,
     $                ALTI,TMIN,TMAX,DTR,RAIN,VAP,WIND)


* ----- to prevent temperature effect on relative humidity
          
          TMPA = (TMIN + TMAX)/2.
          SVAP1  = 6.10588 * EXP (17.32491*TMPA/(TMPA+238.102))
          VAP= AMIN1(VAP,SVAP1)
          RH= VAP/SVAP1

* ------  sensitivity analyses
        
           TMIN= TMIN + SENST
           TMAX= TMAX + SENST  
         
            TMPA = (TMIN + TMAX)/2.
           SVAP2  = 6.10588 * EXP (17.32491*TMPA/(TMPA+238.102))
           VAP= RH*SVAP2
         
               
           VAP= AMIN1(SVAP2, VAP * SENSV)
           WIND= WIND * SENSW
           DTR= DTR * SENSR
           RAIN= RAIN * SENSP
          
*------- calculate daylength

         CALL ASTRO (IDAY,LATIN,DAYL,DAYLP,SINLD,COSLD)                    


*------- calculate potential soil evaporation and crop transpiration

         CALL PENMAN (IDAY,DAYL,SINLD,COSLD,ALTI,TMIN,TMAX,
     $                DTR,WIND,VAP,CO,E0,ES0,ETC,AVRAD)


*------- calculate soil water balance  

         CALL WATBALS(ICROP,ISOIL,INITI,IOPT, IRRI,TERMIN,EMERG,IDAY,
     $              ES0,ETC,RAIN, FINTT,DEPNR,RD,RDMCR,RR,RDM,
     $              CFET,IAIRDU,SMACT,TTRANS,TDRAIN,TRAIN,TESOIL,TRUNOF,
     $                 TIRR,TRANRF,RUNFR,WTOT, WTOTL,WAVT,WAVTL)


*------- when finish conditions are reached (TERMIN .TRUE.)

*-------- calculate crop growth
         
         IF (TERMIN) THEN
         CALL CROPV(ICROP,INITI,IOPT, IDAY,IDEM,IDEMERG,IDPL,IDFLOW,
     $     IDHALT,PL,TERMIN,EMERG,TMIN,TMAX,AVRAD,CO,TRANRF,RDMSO,DAYLP,
     $	    TAGB,WLVG, WLVD, WST,WRT,WSO,RD,RDMCR,RR,RDM,LAI,
     $       CFET,DEPNR,IAIRDU,TSUM,DVS,DVSEND,TSULP,FINT,FINTT,TPARINT,
     $        TPAR,TSUML,NNI,NMINT,NMIN,NUPTT,NFIXTT,NLIVT,NLOSST,YCH)

*------- daily output
           CALL DAILOUT (IOPT,IDAY,IOUT,IDEM,IDPL,IDEMERG,
     $                   WLVG, WLVD, WST,WSO,TAGB,TPARINT,TPAR,
     $                   REMARK,DVS,RUNNAME,LAI,
     $                   SMACT,TTRANS,TDRAIN,TRAIN,TESOIL,TRUNOF,TIRR,
     $                   TRANRF,TERMIN,STATR,ISOIL,ICROP, IRRI,RUNFR,CO,
     $                   WTOT,WAVT,TSUML,NNI,NMINT,NMIN,NUPTT,NFIXTT,
     $                   NLIVT,NLOSST)


*------- yearly output
           CALL STOUT (RUNNAME,REMARK,STATR,ISOIL,ICROP,TERMY,
     $         IDEMERG,IDPL,IDFLOW,IDHALT,CO,IOPT)

         GOTO 20

         ENDIF


*-------- calculate crop growth

         CALL CROPV(ICROP,INITI,IOPT, IDAY,IDEM,IDEMERG,IDPL,IDFLOW,
     $    IDHALT,PL,TERMIN,EMERG,TMIN,TMAX,AVRAD,CO,TRANRF,RDMSO,DAYLP,
     $	    TAGB,WLVG, WLVD, WST,WRT,WSO,RD,RDMCR,RR,RDM,LAI,
     $      CFET,DEPNR,IAIRDU,TSUM,DVS,DVSEND,TSULP,FINT,FINTT,TPARINT,
     $        TPAR,TSUML,NNI,NMINT,NMIN,NUPTT,NFIXTT,NLIVT,NLOSST,YCH)

   
*------- daily output
         CALL DAILOUT (IOPT,IDAY,IOUT,IDEM,IDPL,IDEMERG,
     $                   WLVG, WLVD, WST,WSO,TAGB,TPARINT,TPAR,
     $                   REMARK,DVS,RUNNAME,LAI,
     $                   SMACT,TTRANS,TDRAIN,TRAIN,TESOIL,TRUNOF,TIRR,
     $                   TRANRF,TERMIN,STATR,ISOIL,ICROP, IRRI,RUNFR,CO,
     $                   WTOT,WAVT,TSUML,NNI,NMINT,NMIN,NUPTT,NFIXTT,
     $                   NLIVT,NLOSST)



         IF (TERMIN) GOTO 15   


*------- update daynumber and year

         CALL TIMER (IDAY,IYEAR,IFINIT,TERMIN,INITI,DVS,DVSEND,YCH)

         INITI = .FALSE.                
 
 
15      GOTO 10

20      CONTINUE
      
*-----  For winter crops the end of first crop and the start of second crop is in the same year, so no updating of year number is needed
*-----  For spring crops the new crop starts in a new year	     
        IF (YCH) THEN
          IYEAR= IYEAR 
	  ELSE
	    IYEAR= IYEAR + 1
	  ENDIF         

        IF (IYEAR .NE. IIYR + INYEAR)  GO TO 5

*------- when finish conditions are reached for total number of years (TERMY= .TRUE.)

         TERMY= .TRUE.

*------- statistical analysis
         CALL STOUT (RUNNAME,REMARK,STATR,ISOIL,ICROP,TERMY,
     $         IDEMERG,IDPL,IDFLOW,IDHALT,CO,IOPT)


*------- runs for new site or years ?

         IF (CONTIN .EQ. 'Y' .OR. CONTIN .EQ. 'y') GOTO 1
      END
