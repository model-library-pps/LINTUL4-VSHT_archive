
*---------Subroutines in LINTULSUB.for--------------------------------------*
* routines needed for LINTUL4 model (May 2011, Joost Wolf)                  *
*                                                                           *
*---------------------------------------------------------------------------*
*  SUBROUTINE ASTRO                                                         *
*  Author : Daniel van Kraalingen                                           *
*           documented by Rob Groot                                         * 
*  Date   : 15 march 1987                                                   *
*  Purpose: This subroutine calculates astronomical daylength (DAYL)        *
*           and photoperiodically active daylength (DAYLP), which           *
*           are used in the calculation of canopy assimilation and in the   *
*           calculation of the development rate.                            *
*           References to literature:                                       *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                    units  class         *
*  ----     -------                                    -----  -----         *
*  IDAY     Julian daynumber                                    I,T         *
*  LAT      latitude                                  degrees   I           *
*  DAYL     astronomical daylength                       h      O           *
*  DAYLP    photoperiodically active daylength           h      O           *
*  SINLD    intermediate variable                        =      O           *
*  COSLD    intermediate variable                        =      O           *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : none                                                        *
*---------------------------------------------------------------------------*

      SUBROUTINE ASTRO (IDAY,LAT,DAYL,DAYLP,SINLD,COSLD)                     
 
      IMPLICIT REAL (A-Z)
      INTEGER IDAY

      PARAMETER (PI=3.1415926)


*---- conversion factor from degrees to radians                         

      RAD=PI/180.


*---- declination of the sun as function of daynumber (IDAY)

      DEC=-ASIN(SIN(23.45*RAD)*COS(2.*PI*(IDAY+10.)/365.))


*---- SINLD, COSLD are intermediate variables

      SINLD=SIN(RAD*LAT)*SIN(DEC)
      COSLD=COS(RAD*LAT)*COS(DEC)


*---- daylength (DAYL) and photoperiodically active daylength (DAYLP)

      DAYL =12.0*(1.+2.*ASIN(LIMIT(-1.,+1.,SINLD/COSLD))/PI)
      DAYLP=12.0*(1.+2.*ASIN(LIMIT(-1.,+1.,
     $  (-SIN(-4.*RAD)+SINLD)/COSLD))/PI)

      RETURN                                                            
      END



* ---------------------------------------------------------------------------*
*  SUBROUTINE PENMAN                                                        *
*  Author : Daniel van Kraalingen                                           *
*           based on an earlier version written by: Kees van Diepen         *
*           and Gert-Jan Nooy                                               *
*  Date   : 9-JAN-1987; CO2 update in 1995                                                     *
*  Purpose: This subroutine calculates atmospheric transmission,            * 
*           potential evaporation from a free water surface (E0),           *
*           a bare soil surface (ES0) and the potential                     *
*           transpiration of a closed crop canopy (ET0); CO2 effect on      *
*           crop transpiration is included.
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name    meaning                                     units  class         *
*  ----    -------                                     -----  -----         *
*  IDAY    Julian day number                             -      I,T         *
*  DAYL    Astronomical day length                       h      I           *
*  SINLD   Intermediate variable                         -      I           *
*  COSLD   Intermediate variable                         -      I           *
*  ALTI    Altitude                                      m      I           *
*  TMIN    Minimum temperature during day                C      I           *
*  TMAX    Maximum temperature during day                C      I           *
*  DTR     Daily total irradiation                     J m-2 d-1  I         *
*  VAP     Vapour pressure                             mbar     I           *
*  WIND    Average windspeed                           m s-1    I           *
*  CO      Atmospheric CO2 concentration               mbar     I           *
*  E0      Potential evaporation of open water         cm d-1   O           *
*  ES0     Potential evaporation of soil               cm d-1   O           *
*  ETC     Potential evapotranspiration of crop (CO2 corr.)  cm d-1   O     *
*  AVRAD   Daily total irradiation                     J m-2 d-1  O         *
*                                                                           *
*  FATAL ERROR CHECKS (execution terminated, message)                       *
*  condition                                                                *
*  ---------                                                                *
*  TMIN > TMAX                                                              *
*  WIND < 0                                                                 *
*  AVRAD < 0                                                                *
*  VAP < 0                                                                  *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : none                                                        *
*---------------------------------------------------------------------------*

      SUBROUTINE PENMAN (IDAY,DAYL,SINLD,COSLD,ALTI,TMIN,TMAX,
     $    DTR,WIND,VAP,CO,E0,ES0,ETC,AVRAD)

      IMPLICIT REAL (A-Z)
      INTEGER IDAY,IFPEN
      DIMENSION FPENMTB(10)
      LOGICAL INIT

      PARAMETER (PI=3.1415926)

      DATA INIT/.TRUE./
      DATA ATMTR/0.5/

*----- correction factor for Penman evapo-transpiration rate as a function
*       of atmospheric CO2 concentration (increase in LAI and light interception 
*       with CO2 increase taken into account in function values).
      DATA FPENMTB/40.,1.05,360.,1.00,720.,0.95,1000.,0.92,2000.,0.92/
      DATA IFPEN/10/

*---- initial section

      IF (.NOT.INIT) GOTO 100
      INIT = .FALSE.
      A = 0.20
      B = 0.56

*---- Albedo for water surface, soil surface and canopy

      REFCFW = 0.05
      REFCFS = 0.15
      REFCFC = 0.25

*---- Latent heat of evaporation of water (J kg-1=J mm-1 m-2) and 
*     Stefan Boltzmann constant (J m-2 d-1 K-1)) Psychrometric 
*     instrument constant (K-1) 

      LHVAP  = 2.45E6
      STBC   = 4.9E-3
      PSYCON = 0.000662

100   CONTINUE

*---- average daily temperature

      TMPA = (TMIN+TMAX)/2.

*---- error checks on some input variable ranges
      
      IF (TMIN.GT.TMAX) STOP 'ERR in PENMAN:TMIN > TMAX'
      IF (WIND.LT.0.)   STOP 'ERR in PENMAN:WIND < 0'
      IF (DTR.LT.0.)  STOP 'ERR in PENMAN:AVRAD < 0'
      IF (VAP.LT.0.)  STOP 'ERR in PENMAN:VAP < 0'

*---- Temperature difference (Celsius) 

      TDIF = TMAX-TMIN
 
*---- Coefficient Bu in wind function, dependent on temperature difference

      BU = 0.54+0.35*LIMIT(0.,1.,(TDIF-12.)/4.)

*---- Barometric pressure (mbar), Psychrometric constant (mbar K-1)

      PBAR = 1013.*EXP(-0.034*ALTI/(TMPA+273.))
      GAMMA = PSYCON*PBAR

*---- Saturated vapour pressure according to equation of Goudriaan (1977)

      SVAP = 6.11*EXP(17.4*TMPA/(TMPA+239.))

*---- Measured vapour pressure should not be greater than saturated
*     vapour pressure

      VAP  = AMIN1(SVAP,VAP)

*---- Derivative of SVAP with respect to temperature, i.e. slope of the 
*     SVAP-temperature curve (mbar K-1)

      DELTA = 239.*17.4*SVAP/(TMPA+239.)**2

*---- integral of SINB (DSINB) with SINB equal to sine of solar elevation

      AOB= LIMIT(-1.,+1.,SINLD/COSLD)
      DSINB= 3600.*(DAYL*SINLD+24.*COSLD*SQRT(1.-AOB*AOB)/PI) 

*---- solar constant (SC) and daily extraterrestrial radiation (ANGOT)

      SC= 1370.*(1.+0.033*COS(2.*PI*IDAY/365.))
      ANGOT= AMAX1(0.0001,SC*DSINB) 

*---- atmospheric transmission (ATMTR)
      
      AVRAD= AMIN1(0.80*ANGOT, DTR)
      ATMTR= AVRAD/ANGOT
      

*---- The expression n/N (RELSSD) from the Penman formula is estimated
*     from the Angstrom formula: RI=RA(A+B.n/N) -> n/N=(RI/RA-A)/B,
*     where AVRAD=RI and DSO=RA, the Angot radiation, obtained by a CALL 
*     to ASTRO and three statements from RADIAT:

      RELSSD = LIMIT (0.,1.,(ATMTR-A)/B)

*---- Terms of the Penman formula, for water surface, soil 
*     surface and canopy 
*     Net outgoing long-wave radiation (J m-2 d-1) according to Brunt (1932)

      RB  = STBC*(TMPA+273.)**4*(0.56-0.079*SQRT(VAP))*(0.1+0.9*RELSSD)  

*---- Net absorbed radiation

      RNW = AVRAD*(1.-REFCFW)-RB
      RNS = AVRAD*(1.-REFCFS)-RB
      RNC = AVRAD*(1.-REFCFC)-RB

*---- Evaporative demand of the atmosphere (mm d-1)

      EA  = 0.26*(SVAP-VAP)*(0.5+BU*WIND)
      EAC = 0.26*(SVAP-VAP)*(1.0+BU*WIND)

*---- Penman formula (1948), and conversion to cm d-1

      E0  = 0.1*(DELTA*(RNW/LHVAP)+GAMMA*EA)/(DELTA+GAMMA)
      ES0 = 0.1*(DELTA*(RNS/LHVAP)+GAMMA*EA)/(DELTA+GAMMA)
      ET0 = 0.1*(DELTA*(RNC/LHVAP)+GAMMA*EAC)/(DELTA+GAMMA)


*---- Evapo(transpi)ration is limited in order to prevent negative
*     values

      E0  = AMAX1(0.,E0)
      ES0 = AMAX1(0.,ES0)
      ET0 = AMAX1(0.,ET0)
  
*---- correction of potential evapo-transpiration for atmospheric 
*     CO2 concentration
      ETC = ET0 * LINT(FPENMTB,IFPEN,CO)
    
      RETURN
      END

* ---------------------------------------------------------------------* ---------------------------------------------------------------------** ---------------------------------------------------------------------*
*  SUBROUTINE GLA                                                      *
*  Purpose: This subroutine computes daily increase of leaf area index *
*           (ha leaf area/ ha ground area/ d)                          *
* ---------------------------------------------------------------------*

      SUBROUTINE GLA(DAY,EMERG,DTEFF,LAII,RGRLAI,DELT,SLA,LAI,GLV,NLAI,
     $               DVS,TRANRF,NNI,GLAI)
      IMPLICIT REAL (A-Z)
	LOGICAL  EMERG
      SAVE


      IF (EMERG) THEN
*---- Growth during maturation stage:
        GLAI = SLA * GLV

*---- Growth during juvenile stage:
        IF ((DVS .LT. 0.2).AND.(LAI .LT. 0.75))
     $   GLAI =(LAI * (EXP(RGRLAI * DTEFF * DELT) - 1.)/ DELT )* TRANRF*
     $           EXP(-NLAI* (1.0 - NNI))

*---- Growth at day of seedling emergence:
        IF (LAI.EQ.0.)
     $   GLAI = LAII / DELT
      ENDIF  

*---- Growth before seedling emergence:
      IF (.NOT. EMERG) GLAI = 0.

      RETURN
      END

*----------------------------------------------------------------------*
*  SUBROUTINE SUBPAR                                                   *
*  Purpose: Modification of dry matter partitioning to leaves, stems,  *
*        roots and storage organs in dependence of water and N stress  *
*----------------------------------------------------------------------*
      SUBROUTINE SUBPAR (NPART,TRANRF,NNI,FRTWET,FLVT,FSTT,FSOT,
     $FSHMOD,FLVMOD,FRT,FLV,FST,FSO)
      IMPLICIT REAL (A-Z)
      SAVE

      IF(TRANRF .LT. NNI) THEN
*     Water stress is more severe as compared to nitrogen stress and
*     partitioning will follow the original assumptions of LINTUL2

      FRTMOD = MAX( 1., 1./(TRANRF+0.5))
      FRT    = MIN(0.6, FRTWET * FRTMOD)
      FLV    = FLVT 
      FST    = FSTT 
      FSO    = FSOT 

      ELSE

*    Nitrogen stress is more severe as compared to water stress resulting in
*    less partitioning to leaves and more to stems

      FLVMOD = EXP(-NPART* (1.0-NNI))
      FLV    = FLVT * FLVMOD
      FST    = FSTT + FLVT - FLV 
      FRT    = FRTWET
      FSO    = FSOT 

      ENDIF

      RETURN
      END

* ---------------------------------------------------------------------*
*  SUBROUTINE GROWTH                                                   *
*  Purpose: To compute the total growth rate (kg DM ha-1 d-1)                          *
* ---------------------------------------------------------------------*

      SUBROUTINE GROWTH(DAY,EMERG,PAR,KDIF,NLUE,LAI,RUE,RTMCO,TRANRF,
     $                                     FINT,FINTT,NNI,PARINT,GRT)
      IMPLICIT REAL (A-Z)
	LOGICAL EMERG
      SAVE
      
      TINY=0.001
      IF (EMERG) THEN
	 PARINT = PAR * (1.- EXP(-KDIF*LAI)) 
	ELSE
	 PARINT= 0.
	END IF

*     Fractional light interception  for resp. PAR and total radiation[-]
	FINT= PARINT/PAR
	KGLOB= 0.75 * KDIF
	FINTT= 1.- EXP(-KGLOB*LAI)
  
*     Nitrogen reduction factor (new relationship included for NREF)      
*       NREF= EXP(-NLUE*(1.0-NNI))
        NREF= LIMIT(0., 1.0, 1.-NLUE*(1.0001-NNI)**2)

*     Growth rate GRT in kg DM ha-1 d-1 calculated from PARINT in
*     MJ PAR/m2 and RUE in g DM/ MJ PAR --> multiply with 10
 
      IF(TRANRF .LE. NREF) THEN
*     Water stress is more severe as compared to nitrogen stress 

       GRT = 10*RTMCO * RUE * PARINT * TRANRF

      ELSE

*     Nitrogen stress is more severe as compared to water stress 

       GRT = 10*RTMCO * RUE * PARINT * NREF

      ENDIF

      RETURN
      END

* ---------------------------------------------------------------------*
*  SUBROUTINE RELGR                                                    *
*  Purpose: To compute the relative growth rate of roots, leaves, stem *
*           and storage organs (kg DM ha-1 d-1)                        *
* ---------------------------------------------------------------------*

      SUBROUTINE RELGR(DAY,DAYEM,EMERG,GRT,
     $                 FLV,FRT,FST,FSO,DLV,DRRT,DRST,
     $                 RWLVG,RWRT,RWST,RWSO)
      IMPLICIT REAL (A-Z)
	LOGICAL EMERG
      SAVE

      IF (EMERG) THEN
          RWRT  = GRT * FRT - DRRT
	    AGRT  = GRT * (1. - FRT)
          RWLVG = AGRT * FLV - DLV
          RWST  = AGRT * FST - DRST
          RWSO  = AGRT * FSO

      ELSE
          RWLVG = 0.
          RWRT  = 0.
          RWST  = 0.
          RWSO  = 0.

      ENDIF

      RETURN
      END
* ---------------------------------------------------------------------*
*  SUBROUTINE RNUSUB                                                   *
*  Purpose: To compute the partitioning of the total N uptake rate     *
*           (NUPTR) over leaves, stem, and roots (kg N ha-1 d-1)       *
* ---------------------------------------------------------------------*

      SUBROUTINE RNUSUB(DAY,DAYEM,EMERG,NDEML,NDEMS,NDEMR,NUPTR,
     $                  NFIXTR,NDEMTO, RNULV,RNUST,RNURT)
      IMPLICIT REAL (A-Z)
	LOGICAL EMERG
      SAVE
      IF (EMERG) THEN
          RNULV = (NDEML / NOTNUL(NDEMTO))* (NUPTR+NFIXTR)
          RNUST = (NDEMS / NOTNUL(NDEMTO))* (NUPTR+NFIXTR)
          RNURT = (NDEMR / NOTNUL(NDEMTO))* (NUPTR+NFIXTR)

      ELSE
          RNULV = 0.
          RNUST = 0.
          RNURT = 0.

      ENDIF

      RETURN
      END

* ---------------------------------------------------------------------*
*  SUBROUTINE NOPTM                                                    *
*  Purpose: To compute the optimal nitrogen concentration of crop organs (kg N kg-1 DM)*
* ---------------------------------------------------------------------*

      SUBROUTINE NOPTM(FRNX,NMAXLV,NMAXST, NOPTLV,NOPTST)
      IMPLICIT REAL (A-Z)
      SAVE

      NOPTLV= FRNX * NMAXLV
      NOPTST= FRNX * NMAXST

      RETURN
      END

* ---------------------------------------------------------------------*
*  SUBROUTINE NDEMND                                                   *
*  Purpose: To compute the nitrogen demand of crop organs (kg N ha-1)  *
* ---------------------------------------------------------------------*

      SUBROUTINE NDEMND(NMAXLV,NMAXST,NMAXRT,NMAXSO,WLVG,WST,WRT,WSO,
     $                  ANLV,ANST,ANRT,ANSO,TCNT,
     $                  NDEML,NDEMS,NDEMR,NDEMSO)
      IMPLICIT REAL (A-Z)
      SAVE

      NDEML  =  AMAX1 (NMAXLV*WLVG -  ANLV, 0.)
      NDEMS  =  AMAX1 (NMAXST*WST  - ANST, 0.)
      NDEMR  =  AMAX1 (NMAXRT*WRT  - ANRT, 0.)
      NDEMSO =  AMAX1 (NMAXSO*WSO  - ANSO, 0.)/TCNT

      RETURN
      END

* ---------------------------------------------------------------------*
*  SUBROUTINE NTRLOC                                                   *
*  Purpose: To compute the translocatable N in the organs (kg N ha-1)  *
* ---------------------------------------------------------------------*

      SUBROUTINE NTRLOC(ANLV,ANST,ANRT,WLVG,WST,WRT,RNFLV,RNFST,RNFRT,
     $                  FNTRT,
     $                  ATNLV,ATNST,ATNRT,ATN)
      IMPLICIT REAL (A-Z)
      SAVE

      ATNLV = AMAX1 (0. , ANLV-WLVG*RNFLV)
      ATNST = AMAX1 (0. , ANST-WST*RNFST)
      ATNRT = AMIN1((ATNLV + ATNST) * FNTRT, ANRT-WRT*RNFRT)
      ATN   = ATNLV +  ATNST + ATNRT

      RETURN
      END
* ------------------------------------------------------------------------------*
*  SUBROUTINE NTRANS                                                            *
*  Purpose: To compute the N translocated from different organs (kg N ha-1 d-1) *
* ------------------------------------------------------------------------------*

      SUBROUTINE NTRANS(RNSO,ATNLV,ATNST,ATNRT,ATN,  RNTLV,RNTST,RNTRT)
      IMPLICIT REAL (A-Z)
      SAVE

      RNTLV= RNSO* ATNLV/ NOTNUL(ATN)
      RNTST= RNSO* ATNST/ NOTNUL(ATN)
      RNTRT= RNSO* ATNRT/ NOTNUL(ATN)

      RETURN
      END
* ---------------------------------------------------------------------*
*  SUBROUTINE RNLD                                                     *
*  Purpose: To compute the N loss due to death of leaves, stems        *
*               and roots (kg N ha-1 d-1)                              *
* ---------------------------------------------------------------------*

      SUBROUTINE RNLD(DVS,WRT,WST,RDRRT,RDRST,RNFLV,DLV,RNFRT,
     $ RNFST,DVSDR,DRRT,DRST,RNLDLV,RNLDRT,RNLDST)
      IMPLICIT REAL (A-Z)
      SAVE

      IF (DVS .LT. DVSDR)THEN
         DRRT= 0.
	   DRST= 0.
      ELSE
         DRRT= WRT * RDRRT
	   DRST= WST * RDRST
      ENDIF

      RNLDLV= RNFLV* DLV
      RNLDRT= RNFRT* DRRT
      RNLDST= RNFST* DRST
      RETURN
      END
* ---------------------------------------------------------------------*
*  SUBROUTINE NNINDX                                                   *
*  Purpose: To compute Nitrogen Nutrition Index (-)                    *
* ---------------------------------------------------------------------*

      SUBROUTINE NNINDX(DAY,DAYEM,EMERG,NFGMR,NRMR,NOPTMR,NNI)
      IMPLICIT REAL (A-Z)
	LOGICAL EMERG
      SAVE

      TINY=0.001

      IF (EMERG) THEN

         NNI= LIMIT (TINY,1.0, ((NFGMR-NRMR)/NOTNUL(NOPTMR-NRMR)))
      ELSE
         NNI = 0.0

      ENDIF

      RETURN
      END

* ---------------------------------------------------------------------*
*  SUBROUTINE DEATHL                                                   *
*  Purpose: To compute the relative death rate (d-1) of leaves due to age,   *
*           shading and drought and due to nitrogen stress and the     *
*           death rate of leaves in total (kg ha-1 d-1)                *
* ---------------------------------------------------------------------*
      SUBROUTINE DEATHL(DAY,EMERG,DVS,DVSDLT,RDRTMP,RDRSHM,RDRL,TRANRF,
     $                  LAI,LAICR,WLVG,RDRNS,NNI,SLA,
     $                 RDRDV,RDRSH,RDR,DLV,DLVS,DLVNS,DLAIS,DLAINS,DLAI)
      IMPLICIT REAL (A-Z)
	LOGICAL EMERG
      SAVE

      IF (DVS .LT. DVSDLT)THEN
         RDRDV = 0.
      ELSE
         RDRDV = RDRTMP
      ENDIF

      RDRSH = MAX(0.,RDRSHM * (LAI-LAICR) / LAICR)
	RDRDRY= (1. - TRANRF) * RDRL
      RDR   = MAX(RDRDV, RDRSH, RDRDRY)

      IF (EMERG .AND. NNI .LT. 1.) THEN
          DLVNS   = WLVG *RDRNS * (1.-NNI)
          DLAINS  = DLVNS * SLA
      ELSE
          DLVNS   = 0.
          DLAINS  = 0.
      ENDIF

      IF (EMERG) THEN
      DLVS  = WLVG * RDR
      DLAIS = LAI *  RDR
      ELSE
	DLVS= 0.
	DLAIS=0.
	END IF 

      DLV   = DLVS + DLVNS
      DLAI  = DLAIS + DLAINS

      RETURN
      END