*------------------------------------------------------------------------- *
*  SUBROUTINE CROPV                                                        *
*  Author: Joost Wolf                                                      *
*  Date: Crop model developed on the basis of LINTUL3.fst in May 2011      * 
*  Version with vernalisation effect on phenological development (Oct.2011)*
*  Purpose: This subroutine simulates the dry matter increase of a         *
*           crop as function of intercepted radiation, temperature,        *
*           radiation use efficiency, water and nitrogen availability.     *
*                                                                          *
*                                                                          *
*  FORMAL PARAMETERS:(I= input, O= output, C= control, IN= init., T-time)  *
*  name     meaning                                  units       class     *
*  ----     -------                                  -----       -----     *
*  ICROP    number of crop data set                   -           I        *
*  INITI    indicates initialization of run           -           I,C      *
*  IOPT     indicates optimal (=1), water limited (=2)                     *
*            or water and N limited run (=3)          -           I,C      *
*  IDAY     julian day number                         -           I        *
*  IDEM     date of emergence                         -           I        *
*  IDEMERG  date of emergence                         -           O        *
*  IDPL     date of planting/sowing                          -           I        *
*  IDFLOW   date of flowering                                     O        *
*  IDHALT   end date of crop growth                               O        *
*  PL       indicates planting as start of simulation -           I,C      *
*  TERMIN   indicates terminal section                -           I,C      *
*  EMERG    indicates crop emergence                  -           I,C      *
*  TMIN     minimum air temperature                   C           I        *
*  TMAX     maximum air temperature                   C           I        *
*  AVRAD    daily total irradiation                   J m-2 d-1   I        *
*  CO       atmospheric CO2 concentration             ppmv        I        *
*  TRANRF   reduction factor due to drought/wetness   -           I        *  
*  RDMSO    soil related maxiumum rooting depth       cm          I        *
*  DAYLP    photoperiodically active daylength        h           I        *
*  TAGB     total above-ground biomass                kg DM ha-1  O        *
*  WLVG      weight of living leaves                  kg DM ha-1  O        *
*  WLVD     weight of dead leaves                     kg DM ha-1  O        *
*  WST      weight of stems                           kg DM ha-1  O        *
*  WRT      weight of roots                           kg DM ha-1  O        *
*  WSO      weight of storage organs                  kg DM ha-1  O        *
*  RD       actual rooting depth                      cm          O        *
*  RDMCR    crop specific maximum rooting depth       cm          O        *
*  RR       root growth rate                          cm d-1      O        *
*  RDM      soil/crop related maximal rooting depth   cm          I        *
*  LAI      leaf area index                           m2 m-2      O        *
*  DEPNR   crop group number for soil water depletion -           O        *
*  CFET    crop specific correction for transpiration -           O        * 
*  IAIRDU  air ducts in roots present (=1) or not(=0)  -          O,C      *
*  TSUM     temperature sum from emergence            C d         O        *
*  DVS      development stage                         -           O        *
*  DVSEND   development stage at end of growth period -           O        *
*  TSULP    temperature sum from sowing/planting      C d         O        *
*  FINT     fractional light interception for PAR     -           O        *
*  FINTT     fractional light interception for total radiation -  O        *
*  TPARINT  total intercepted radiation (PAR)         MJ m-2      O        *
*  TPAR     total photosynthetically active radiation MJ m-2      O        *
*  TSUML    temperature sum from emergence incl. dayl.effect C.d  O        *
*  NNI      nitrogen nutrition index                  -           O        *
*  NMINT    total mineral N from soil and fertiliser  kg N ha-1   O        *
*  NMIN     mineral N available from soil for crop    kg N ha-1   O        *
*  NUPTT    total N uptake by crop from soil          kg N ha-1   O        *
*  NFIXTT   total N uptake by crop from biol.fixation kg N ha-1   O        *
*  NLIVT    Amount of N in living crop organs         kg N ha-1   O        *
*  NLOSST   Amount of N in dead crop organs           kg N ha-1   O        *
*  YCH      indicates year change                     -           I        * 
*                                                                          *
*  File usage: LINTER                                                      *
*------------------------------------------------------------------------
       SUBROUTINE CROPV(ICROP,INITI,IOPT, IDAY,IDEM,IDEMERG,IDPL,IDFLOW,
     $    IDHALT,PL,TERMIN,EMERG,TMIN,TMAX,AVRAD,CO,TRANRF,RDMSO,DAYLP,
     $	    TAGB,WLVG, WLVD, WST,WRT,WSO,RD,RDMCR,RR,RDM,LAI,
     $      CFET,DEPNR,IAIRDU,TSUM,DVS,DVSEND,TSULP,FINT,FINTT,TPARINT,
     $        TPAR,TSUML,NNI,NMINT,NMIN,NUPTT,NFIXTT,NLIVT,NLOSST,YCH)

       IMPLICIT REAL (A-Z)
       INTEGER ICROP,IDAY,IDEM,IDEMERG,IDPL,IDFLOW,IDHALT,IDSL
       INTEGER ILCO,IOPT,IAIRDU,ILDTSM,ILSLA,ILSSA,ILKDIF,ILRUE
	 INTEGER ILTMPF,ILTMNF,ILFR,ILFL,ILFS,ILFO,ILRDRL
	 INTEGER ILRDRR,ILRDRS,ILNMXL,ILPHOT,ILFERT,ILNRFT, ILVER 

       LOGICAL TERMIN,INITI,PL,FLOW,EMERG,YCH
       REAL DTSMTB (30), SLATB (30),SSATB (30),KDIFTB (30)  
       REAL TMPFTB (30), TMNFTB (30), RUETB (30), FLTB (30), RDRLTB (30) 
       REAL COTB (30), RDRRTB (30), RDRSTB (30), NMXLV (30), PHOTTB (30)
	 REAL FSTB (30), FOTB (30), FERTAB(30), NRFTAB(30), VERNRT(30)
	 REAL FRTB (30) 

       COMMON /CROPOUT/ TAGB1,WSO1,TPARINT1, HI1, RUEC
	 COMMON /CROPOUT/ NUPTT1,NFIXTT1,NLIV1,NLOSS1,NROOT1,TRANRF1,NNI1

*----------------------------------------------------
*      INITIALIZATION
*----------------------------------------------------

       IF (INITI) THEN
         FLOW= .FALSE.
         EMERG= .FALSE.
       ELSE
         GOTO 200
       ENDIF

*---- Reading crop data from file CROPP.DAT
       
       IF (ICROP .EQ. 1) CALL RDINIT (14,0, 'CROPP1.DAT')
       IF (ICROP .EQ. 2) CALL RDINIT (14,0, 'CROPP2.DAT')
       IF (ICROP .EQ. 3) CALL RDINIT (14,0, 'CROPP3.DAT')
       IF (ICROP .EQ. 4) CALL RDINIT (14,0, 'CROPP4.DAT')
       IF (ICROP .EQ. 5) CALL RDINIT (14,0, 'CROPP5.DAT')
       IF (ICROP .EQ. 6) CALL RDINIT (14,0, 'CROPP6.DAT')

      
!    Read initial states and parameter values
	call RDSINT ('IDSL', IDSL)
      CALL RDSREA ('TSUM1',TSUM1)
      CALL RDSREA ('TSUM2',TSUM2)

      CALL RDSREA ('DVSI', DVSI)
      CALL RDSREA ('DVSEND',DVSEND)
      CALL RDSREA ('TDWI',TDWI)
      CALL RDSREA ('RGRLAI',RGRLAI)
      
      CALL RDSREA ('SPA',SPA)
      CALL RDAREA ('DTSMTB',DTSMTB,30,ILDTSM)      
      CALL RDAREA ('SLATB',SLATB,30,ILSLA)
      CALL RDAREA ('SSATB',SSATB,30,ILSSA)
      CALL RDSREA ('TBASE',TBASE)
	CALL RDAREA ('KDIFTB',KDIFTB,30,ILKDIF)
      CALL RDAREA ('RUETB',RUETB,30,ILRUE)
      CALL RDAREA ('TMPFTB',TMPFTB,30,ILTMPF)
      CALL RDAREA ('TMNFTB',TMNFTB,30,ILTMNF)
      CALL RDAREA ('COTB',COTB,30,ILCO)
      CALL RDAREA ('FRTB',FRTB,30,ILFR)
      CALL RDAREA ('FLTB',FLTB,30,ILFL)
      CALL RDAREA ('FSTB',FSTB,30,ILFS)
      CALL RDAREA ('FOTB',FOTB,30,ILFO)
      CALL RDSREA ('RDRL',RDRL)
      CALL RDAREA ('RDRLTB',RDRLTB,30,ILRDRL)
      call RDSREA ('RDRSHM', RDRSHM)
      call RDSREA ('RDRNS', RDRNS)
      CALL RDAREA ('RDRRTB',RDRRTB,30,ILRDRR)
      CALL RDAREA ('RDRSTB',RDRSTB,30,ILRDRS)
      CALL RDSREA ('CFET',CFET)
      CALL RDSREA ('DEPNR',DEPNR)
      CALL RDSINT ('IAIRDU',IAIRDU)
      CALL RDSREA ('RDI',RDI)
      CALL RDSREA ('RRI',RRI)
      CALL RDSREA ('RDMCR',RDMCR)
      CALL RDSREA ('DVSDR', DVSDR)
      CALL RDSREA ('DVSDLT', DVSDLT)
      CALL RDSREA ('DVSNLT', DVSNLT)
      CALL RDSREA ('DVSNT', DVSNT)
      CALL RDSREA ('TBASEM',TBASEM)
      CALL RDSREA ('TEFFMX',TEFFMX)
      CALL RDSREA ('TSUMEM',TSUMEM)
      call RDSREA ('FNTRT', FNTRT)
      call RDSREA ('FRNX', FRNX)
      call RDSREA ('LAICR', LAICR)
      call RDSREA ('LRNR', LRNR)
      call RDSREA ('LSNR', LSNR)
      call RDSREA ('NLAI', NLAI)
      call RDSREA ('NLUE', NLUE)
      call RDSREA ('NMAXSO', NMAXSO)
      call RDSREA ('NPART', NPART)
      call RDSREA ('NFIXF', NFIXF)
      call RDSREA ('NSLA', NSLA)
      call RDSREA ('RNFLV', RNFLV)
      call RDSREA ('RNFRT', RNFRT)
      call RDSREA ('RNFST', RNFST)
      call RDSREA ('TCNT', TCNT) 
      call RDAREA ('NMXLV', NMXLV, 30, ILNMXL)
      call RDAREA ('PHOTTB', PHOTTB, 30, ILPHOT)
	call RDSREA ('VERSAT', VERSAT)
	call RDSREA ('VBASE', VBASE)
	call RDAREA ('VERNRT', VERNRT, 30, ILVER) 

      
      CLOSE (14, STATUS= 'DELETE')

*     Read management data from file MANAGE.DAT
      Call RDINIT (15,0, 'MANAGE.DAT')
      call RDAREA ('FERTAB', FERTAB, 30, ILFERT) 
      call RDAREA ('NRFTAB', NRFTAB, 30, ILNRFT)
        
	call RDSREA ('NMINS', NMINS)
	call RDSREA  ('RTMINS', RTMINS)

      CLOSE (15, STATUS= 'DELETE')


*----- initialization of state variables

       TDW= TDWI
	 DVS= DVSI
	 DVR= 0.
	 NMINI=NMINS
	 NMIN=NMINI
	 WRTI=  LINT (FRTB, ILFR, DVSI) * TDW
	 WRT= WRTI
	 TAGB= TDW- WRT 
	 WLVGI= LINT (FLTB, ILFL, DVSI) * TAGB
	 WLVG= WLVGI
	 LAII= WLVGI * LINT(SLATB,ILSLA,DVSI) 
	 LAI= LAII
	 RLAI= 0.
	 WLVD= 0.
	 WSTI=  LINT (FSTB, ILFS, DVSI) * TAGB
	 WST= WSTI 
       WSOI=  LINT (FOTB, ILFO, DVSI) * TAGB
	 WSO= WSOI
	 WSTD= 0.
	 WRTD= 0.
	 RWLVG= 0.
	 RWST=0.
	 RWRT=0.
	 RWSO= 0.
	 DLV=0.
	 DRST=0.
	 DRRT= 0.
       GRT= 0.
       TSULP= 0. 
       TSUM= 0.
	 TSUML= 0.
       DTSULP= 0. 
       DTSUM= 0.
	 DTSUML= 0.
	 VERN= 0.
	 RVERNR= 0.
	 DVRED= 1.
       TPAR= 0.
       TPARINT= 0.
       PAR= 0.
       PARINT= 0. 
	 ATN= 0. 
	 GTSUM= 0.  
	 RD= RDI 
	 DELT= 1. 
	 NMINT= 0.
	 NMAXLVI= LINT (NMXLV, ILNMXL, DVSI)
       NMAXSTI= LSNR * NMAXLVI
	 NMAXRTI= LRNR * NMAXLVI
       ANLVI= NMAXLVI * WLVGI
	 ANSTI= NMAXSTI * WSTI
	 ANRTI= NMAXRTI * WRTI
	 ANLV= ANLVI
	 ANST= ANSTI
	 ANRT= ANRTI
	 ANSOI= 0.
	 ANSO= ANSOI
	 NLOSSL= 0.
	 NLOSSR= 0.
	 NLOSSS= 0.
	 NUPTT= 0.
	 NFIXTT= 0.
	 RNMINS= 0.
	 RNMINT= 0.
	 CTRAN= 0.
       CNNI= 0.

       DAYPL= REAL(IDPL)
	 TRANRF= 1.
	 NNI= 1.
       
       IF (.NOT. PL) THEN
          DAYEM= REAL(IDEM)
	    EMERG= .TRUE.
       ELSE
          DAYEM= 999.
       ENDIF


200     CONTINUE

        


        IF (TERMIN) GOTO 999


*---------------------------------------------------------
*      INTEGRATION
*---------------------------------------------------------

*----- Temperature sums (C.d) from sowing/planting (P) and from emergence with and without daylength effect 

       TSULP= INTGRL(TSULP, DTSULP, 1.)
       TSUM= INTGRL(TSUM, DTSUM, 1.)
	 TSUML= INTGRL(TSUML, DTSUML, 1.)

*----- Vernalisation effect accumulated
       VERN= INTGRL(VERN, RVERNR, 1.)

*----- Start of flowering
       IF (.NOT. FLOW .AND. TSUML .GE. TSUM1)  IDFLOW= IDAY
       IF (.NOT. FLOW .AND. TSUML .GE. TSUM1)  FLOW= .TRUE.

*---- Total photosynthetically active radiation (MJ/m2)
      TPAR= INTGRL(TPAR, PAR, 1.)

*---- Total intercepted radiation (MJ/m2)
      TPARINT= INTGRL(TPARINT, PARINT, 1.)

*---- Dry weights of total biomass, living crop organs, and total above-ground living biomass (kg DM/ha)
	GTSUM= INTGRL(GTSUM, GRT, 1.)
      WLVG= INTGRL(WLVG, RWLVG, 1.) 
	WST=  INTGRL(WST, RWST, 1.)
	WRT=  INTGRL(WRT,RWRT, 1.)
	WSO=  INTGRL(WSO, RWSO, 1.)
	TAGBG= WLVG+WST+WSO

*---- Development stage (-)
	DVS=  INTGRL(DVS, DVR, 1.)
      
*---- Dry weights of dead crop organs and total above-ground biomass incl. dead crop organs (kg DM/ha)
      WLVD= INTGRL(WLVD, DLV, 1.)
	WSTD= INTGRL(WSTD,DRST, 1.)
	WRTD= INTGRL(WRTD, DRRT, 1.)
	TAGB= TAGBG + WLVD + WSTD


*----- Rooting depth and Leaf area index
       RD= INTGRL(RD, RR, 1.)
	 LAI= INTGRL(LAI, RLAI, 1.)

*     Soil mineral N  and Total mineral N available from both fertiliser and soil (kg N ha-1)
       NMIN= INTGRL(NMIN, RNMINS, 1.)
	 NMINT= INTGRL(NMINT, RNMINT,1.) 

*----- Total N uptake by crop over time (kg N ha-1) from soil and by biological fixation
       NUPTT= INTGRL(NUPTT, NUPTR, 1.)
	 NFIXTT= INTGRL(NFIXTT, NFIXTR, 1.)

*-----Actual N amount in various living organs and total living N amount(kg N ha-1)
      ANLV =  INTGRL (ANLV,RNLV, 1.)
      ANST =  INTGRL (ANST,RNST, 1.)
      ANRT =  INTGRL (ANRT,RNRT, 1.)
      ANSO =  INTGRL (ANSO,RNSO, 1.)
	NLIVT= ANLV+ANST+ANRT+ANSO

*-----N losses from leaves, roots and stems due to senescence and total N loss (kg N ha-1)
      NLOSSL=  INTGRL(NLOSSL, RNLDLV, 1.)
	NLOSSR=  INTGRL(NLOSSR, RNLDRT, 1.)
	NLOSSS=  INTGRL(NLOSSS, RNLDST, 1.)
      NLOSST=  NLOSSL+NLOSSR+NLOSSS

*---- total N in living and dead roots
      NROOT= ANRT + NLOSSR
*-----------------------------------------------------------------------------
*      RATE CALCULATIONS
*-----------------------------------------------------------------------------


*------ Weather calculations

*------ Daily photosynthetically active radiation (PAR, MJ/m2)
        PAR= AVRAD/1.0E6 * 0.50

*------ Average daily temperature (C)
        TMPA= 0.5 *(TMIN + TMAX)
        DAY= REAL(IDAY)

*------ date of planting/sowing
        IF (PL) THEN
          IF (.NOT. YCH) PUSHPL= INSW(DAY - DAYPL, 0., 1.)
	    IF (YCH) PUSHPL= INSW(365. + DAY - DAYPL, 0., 1.)
        ELSE
          PUSHPL= 0.
        ENDIF

        IF (PL .AND. (TSULP .GE. TSUMEM) .AND. (.NOT. EMERG)) THEN
           IDEMERG= IDAY
           DAYEM= REAL(IDEMERG)
           EMERG= .TRUE.
        ENDIF 

*-----  Reduction of development rate until flowering by  day length

        DVRED= LINT(PHOTTB, ILPHOT, DAYLP)

        IF (IDSL .EQ. 1 .AND. .NOT. FLOW) THEN
	      RDAYL= DVRED 
        ELSEIF (IDSL .EQ. 2 .AND. .NOT. FLOW) THEN
           RDAYL= DVRED
        ELSE
           RDAYL= 1.
        ENDIF

*----- Reduction of development rate until flowering by vernalisation

        VERNR= LINT(VERNRT,ILVER,TMPA)
	  IF (IDSL .EQ. 2) THEN
           RVERNR= INSW(DVS-0.3, VERNR, 0.)
	     VERNF= LIMIT(0., 1., (VERN-VBASE)/(VERSAT-VBASE) )
	     VERNF1= INSW(DVS-0.3, VERNF, 1.)
        ELSE
	     VERNF1= 1.
	  ENDIF

*       emergence date set
        IF (.NOT. PL) IDEMERG= IDEM

*------ Change in temperature sums from sowing/planting (P) and from emergence for crop development without and with day length and vernal. effects
        DTSULP= LIMIT(0., TEFFMX-TBASEM, TMPA-TBASEM) * PUSHPL
        IF (.NOT. YCH) PUSHEM= INSW(DAY - DAYEM, 0., 1.)
        IF (YCH) PUSHEM=INSW(365. + DAY-DAYEM, 0., 1.)
        DTSU=  MAX(0.,LINT(DTSMTB, ILDTSM, TMPA))
	  DTSUM= DTSU * PUSHEM
        DTSUML= DTSU * PUSHEM * RDAYL * VERNF1

*       Calculation of development stage        
        IF (DVS .LT. 1.0) THEN
*       effects of daylength, vernalisation and tenmperature on development during vegetative phase
	    DVR= DTSUML/TSUM1 
	  ELSE
*       development during generative phase
          DVR= DTSUML/TSUM2
	  END IF
		   

*------ Plant growth
        
*------ Radiation use efficiency as dependent on development stage (g DM MJ-1)
        RUE= LINT(RUETB,ILRUE,DVS)
        
*------ Correction of radiation use efficiency for change in atmospheric CO2 concentration (-)
        RCO= LINT(COTB,ILCO,CO)
        
*------ Reduction of radiation use efficiency for non-optimal day-time temperatures and for low minimum temperature
        DTEMP= TMAX - 0.25*(TMAX-TMIN)
        RTMP= LINT(TMPFTB,ILTMPF,DTEMP) * LINT(TMNFTB,ILTMNF,TMIN)
*------ Correction of RUE for both non-optimal temperatures and atmospheric CO2
	  RTMCO= RTMP * RCO
        
!      Calling the subroutine for translocatable N in leaves, stem, roots and
!      storage organs (kg N ha-1)
       CALL NTRLOC(ANLV,ANST,ANRT,WLVG,WST,WRT,RNFLV,RNFST,RNFRT,FNTRT,
     $        ATNLV,ATNST,ATNRT,ATN)
     
!    * Total vegetative living above-ground biomass (kg DM ha-1)
        TBGMR =WLVG+WST
    
!      N concentration (kg N kg-1 DM) in the living leaves, stem, roots and storage
!      organs
       NFLV = ANLV/ NOTNUL(WLVG)
       NFST = ANST/ NOTNUL(WST)
       NFRT = ANRT/ NOTNUL(WRT)
       NFSO = ANSO/ NOTNUL(WSO)
    
!    Total N in vegetative living above-ground biomass (kg N ha-1)
       NUPGMR = ANLV + ANST
     
!    Fertilizer N application (kg N ha-1 d-1) and its recovery fraction (-)---------------------------------*
       FERTN  = LINT (FERTAB,ILFERT, DAY)
       NRF    = LINT (NRFTAB,ILNRFT, DAY)
       FERTNS = FERTN * NRF

!      Check on N balance
       NBALAN = ABS(NUPTT+NFIXTT+(ANLVI+ANSTI+ANRTI+ANSOI)-(ANLV
     $          +ANST+ANRT+ANSO+NLOSSL+NLOSSR+NLOSSS))  

      IF (NBALAN .GE. 1.) STOP
     $    ' nitrogen balance NBALAN not 0, program aborted'
   
!    Total leaf weight, both green and dead (kg DM ha-1)
       WLV    = WLVG + WLVD
  
!     Relative death rate of roots (d-1)
       RDRRT = LINT(RDRRTB, ILRDRR, DVS)
	 RDRST = LINT(RDRSTB, ILRDRS, DVS)
   
!     Total N in living above-ground crop organs (kg N ha-1)   
       NTAG   = ANLV   + ANST   + ANSO
   
!     Relative death rate of leaves due to senescence/ageing as dependent on mean daily temperature (d-1)
       RDRTMP = LINT(RDRLTB,ILRDRL,TMPA)
  
!     Maximum N concentration in the leaves, from which the N conc. in the
!     stem and roots are derived, as a function of development stage (kg N kg-1 DM)  
       NMAXLV = LINT (NMXLV, ILNMXL, DVS)
     
!    * N concentration in above-ground living biomass (kg N kg-1 DM)
	 NTAC  = NTAG/TAGBG
    
!    N supply to the storage organs (kg N ha-1 d-1)
       NSUPSO = INSW (DVS-DVSNT,0.,ATN/TCNT)

!      N concentration in total vegetative living above-ground biomass  (kg N kg-1 DM) 
       NFGMR  = NUPGMR/NOTNUL(TBGMR)
     
!    * Residual N concentration in total vegetative living above-ground biomass  (kg N kg-1 DM) 
       NRMR   = (WLVG*RNFLV+WST*RNFST)/NOTNUL(TBGMR)
  
!     Nitrogen uptake limiting factor (-) at low moisture conditions in the
!     rooted soil layer before anthesis. After anthesis there is no
!     N uptake from the soil
       NLIMIT = INSW(DVS-DVSNLT, INSW(TRANRF-0.01,0.,1.) , 0.0)
     

!     Biomass partitioning functions under non-stressed situations (-)
       FRTWET = LINT(FRTB,ILFR, DVS )
       FLVT   = LINT(FLTB,ILFL, DVS )
       FSTT   = LINT(FSTB,ILFS, DVS)
       FSOT   = LINT(FOTB,ILFO, DVS )
 
*----  Carbon balance check  
       CBALAN = ABS(GTSUM + (WRTI+WLVGI+WSTI+WSOI)-(WLVG+WST+WSO+WRT
     $           +WLVD+WRTD+WSTD))	

       IF (CBALAN .GE. 1.) STOP
     $    ' carbon balance CBALAN not 0, program aborted'
    
*----  Maximum N concentrations in stems and roots (kg N kg-1 DM)
       NMAXST = LSNR * NMAXLV
       NMAXRT = LRNR * NMAXLV

*----- Root growth (cm d-1)
       IF (EMERG) RR = MIN(RRI * INSW( TRANRF-0.01, 0., 1. ),  RDM-RD)
        
!    Calling the subroutine for calculating optimal nitrogen concentrations in leaves
!    and stems
       CALL NOPTM(FRNX,NMAXLV,NMAXST, NOPTLV,NOPTST)
    
!    Optimal amount of N in vegetative above-ground living biomass and its N concentration
       NOPTS = NOPTST* WST
       NOPTL = NOPTLV* WLVG 
       NOPTMR = (NOPTL+ NOPTS)/NOTNUL(TBGMR)
    
!    Calling the subroutine for calculating the Nitrogen Nutrition Index (NNI)
       CALL NNINDX(DAY,DAYEM,EMERG,NFGMR,NRMR,NOPTMR,NNI)
!    With potential and water limited conditions there is no N stress and NNI is set to 1   
       IF (IOPT .EQ. 1 .or. IOPT .EQ. 2) NNI= 1.

!    Calling the subroutine for calculating the relative modification for root and shoot
!    allocation.
       CALL SUBPAR (NPART,TRANRF,NNI,FRTWET,FLVT,FSTT,FSOT,
     $              FSHMOD,FLVMOD,FRT,FLV,FST,FSO)

       FCHECK  = ABS(FRT + (FLV+ FST+ FSO) * (1.- FRT) -1.)

       IF (FCHECK .GE. 0.05) STOP
     $ ' assimilate allocation check over crop organs FCHECK '
     $'not 0, program aborted'

    
!    Calling the subroutine for total growth rate (kg DM ha-1 d-1)
       KDIF= LINT(KDIFTB,ILKDIF,DVS)
       CALL GROWTH(DAY,EMERG,PAR,KDIF,NLUE,LAI,RUE,RTMCO,TRANRF,FINT,
     $	 FINTT,NNI,PARINT,GRT)
  
!     Water-Nitrogen stress factor
       RNW = MIN(TRANRF,NNI)
     
!     cumulative values for TRANRF and NNI over growth period
       IF (EMERG) CTRAN= CTRAN + TRANRF
	 IF (EMERG) CNNI= CNNI + NNI
      

!        Specific Leaf area(ha/kg).
       SLA = LINT (SLATB,ILSLA,DVS)*EXP(-NSLA * (1.-NNI))
    
!    Calling the subroutine for relative death rate of leaves.
       CALL DEATHL(DAY,EMERG,DVS,DVSDLT,RDRTMP,RDRSHM,RDRL,TRANRF,LAI,
     $	 LAICR,WLVG,RDRNS,NNI,
     $     SLA,RDRDV,RDRSH,RDR,DLV,DLVS,DLVNS,DLAIS,DLAINS,DLAI)
     
!    ** Leaf growth
       GLV    = FLV * GRT * (1-FRT)
    
!    Calling the subroutine for calculating the daily increase of leaf area index (m2 m-2 d-1).
       DTEFF= MAX(0., TMPA-TBASE)
       CALL GLA(DAY,EMERG,DTEFF,LAII,RGRLAI,DELT,SLA,LAI,GLV,NLAI,
     $      DVS,TRANRF,NNI,GLAI)
    
!    Calling the subroutine for N loss due to death of leaves,stems and roots (kg N ha-1 d-1)
       CALL RNLD (DVS,WRT,WST,RDRRT,RDRST,RNFLV,DLV,RNFRT,
     $ RNFST,DVSDR,DRRT,DRST,RNLDLV,RNLDRT,RNLDST)
  
!    Net rate of change of Leaf area (m2 leaf area m-2 d-1)
       RLAI   = GLAI - DLAI
    
!    Calling the subroutine for calculating the relative growth rate of roots, leaves, stem
!    and storage organs (kg ha-1 d-1)
       CALL RELGR(DAY,DAYEM,EMERG,GRT,FLV,FRT,FST,
     $ FSO,DLV,DRRT,DRST,RWLVG,RWRT,RWST,RWSO)
    
!    Calling the subroutine for N demand of leaves, roots and stem storage
!    organs (kg N ha-1 d-1)
       CALL NDEMND(NMAXLV,NMAXST,NMAXRT,NMAXSO,WLVG,WST,WRT,WSO,
     $   ANLV,ANST,ANRT,ANSO,TCNT,NDEML,NDEMS,
     $   NDEMR,NDEMSO)
     
!        Total Nitrogen demand (kg N ha-1)
       NDEMTO = MAX (0.0,(NDEML + NDEMS + NDEMR))
    
!    Rate of N uptake in grains (kg N ha-1 d-1)
       RNSO =  AMIN1 (NDEMSO,NSUPSO)
      
	IF (EMERG) THEN
!        Total N uptake (kg N ha-1 d-1) from soil and by biological fixation
         NUPTR = (MAX (0., MIN ((1.-NFIXF)*NDEMTO, NMINT))* NLIMIT)/DELT
	   NFIXTR= MAX (0., NUPTR * NFIXF / MAX(0.02,1-NFIXF) )

!        No N limitation for optimal and water limited production
	   IF (IOPT .EQ. 1 .OR. IOPT .EQ. 2) NUPTR=
     $    (MAX (0., (1.-NFIXF)*NDEMTO)* NLIMIT ) / DELT 
         IF (IOPT .EQ. 1 .OR. IOPT .EQ. 2) NFIXTR=
     $    (MAX (0., NFIXF*NDEMTO)* NLIMIT ) / DELT 
      ELSE
	  NUPTR= 0.
	  NFIXTR= 0.
	END IF    

!    Calling the subroutine for calculating N translocated from leaves, stem, and roots (kg N ha-1 d-1)
       CALL NTRANS(RNSO,ATNLV,ATNST,ATNRT,ATN, RNTLV,RNTST,RNTRT)
    
!    Calling the subroutine to compute the partitioning of the total
!    N uptake rate (NUPTR) over the leaves, stem and roots (kg N ha-1 d-1)
       CALL RNUSUB(DAY,DAYEM,EMERG,NDEML,NDEMS,NDEMR,NUPTR,NFIXTR,
     $ NDEMTO,RNULV,RNUST,RNURT)
 
!     Soil N supply (g N m-2 d-1) through mineralization during crop growth
       IF (EMERG) RNMINS  = -MAX(0.,MIN( RTMINS * NMINI * NLIMIT, NMIN))
!     Change in total inorganic N in soil as function of fertilizer
!     input, soil N mineralization and crop uptake.
       RNMINT = FERTNS/DELT -NUPTR - RNMINS
     
*----Rate of change of N in crop organs   
       RNST = RNUST-RNTST-RNLDST
       RNRT = RNURT-RNTRT-RNLDRT
       RNLV = RNULV-RNTLV-RNLDLV       

999      CONTINUE

* ----- Data for summary output

        IF (TERMIN) THEN
        WRITE (*, '(//2A)') ' Crop development completed'

        TAGB1= TAGB
        WSO1= WSO
        TPARINT1= TPARINT
        HI1= WSO/TAGB
        IDHALT= IDAY
        NUPTT1= NUPTT
	  NFIXTT1= NFIXTT
	  NLIV1= NLIVT
	  NLOSS1= NLOSST
	  NROOT1= NROOT
	    IF (IDAY .GT. IDEMERG) THEN
	    TRANRF1= CTRAN/(IDAY -IDEMERG)
	    NNI1= CNNI/(IDAY-IDEMERG)
	    ELSE
	    TRANRF1= CTRAN/ (IDAY+365-IDEMERG)
          NNI1= CNNI/(IDAY+365-IDEMERG)
	    END IF

*----- calculated radiation use efficiency in g above-ground D.M./radiation intercepted in MJ PAR
        RUEC= (TAGB/10.)/TPARINT
        END IF


*---- Finish conditions
      IF (FINT .LT. 0.05 .AND.  TAGB .GT. 200.) TERMIN= .TRUE.


         RETURN
         END
