*---------------------------------------------------------------------------*
*  SUBROUTINE DAILOUT                                                       *
*  Author : Joost Wolf                                                      *
*  Date   : May 1996                                                        *
*  Purpose: This subroutine writes crop and soil water status to the        *
*	    output file Run/PTS.OUT for given time interval                   *
*                                                                           *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)          *
*  name     meaning                                    units     class      *
*  ----     -------                                    -----     -----      *
*  IOPT     indicates optimal (=1) or water limited (=2)           I,C      *
*           or water and nitrogen limited run (=3)                          *
*  IDAY     julian daynumber                             =         I,T      *
*  IOUT     time interval for output                    d          C        *
*  IDEM     date of emergence                           -          I        *
*  IDPL     date of planting                            -          I        *
*  IDEMERG  date of emergence                           -          O        *
*  WLVG	  weight of green leaves                     kg DM/ha    O        *
*  WLVD     weight of dead leaves                      kg DM/ha    O        * 
*  WST      weight of stems                            kg DM/ha    O        *
*  WSO      weight of storage organs                   kg DM/ha    I        *
*  TAGB     total above-ground biomass                 kg DM/ha    I        *
*  TPARINT  total intercepted radiation (PAR)          MJ/m2       I        *          
*  TPAR     total photosynthetically active radiation  MJ/m2       I        * 
*  REMARK   output remarks                               =         I        *
*  DVS      development stage of crop (from 0 to 2)    -           I        *
*  RUNNAME  first 5 characters for name of output files            I        *
*  LAI      leaf area index                            cm2/cm2     I        *
*  SMACT    actual soil moisture content               cm3/cm3     I        *
*  TTRANS   cumulative crop transpiration              cm          I        *
*  TDRAIN   cumulative drainage to the sub-soil        cm          I        *
*  TRAIN    cumulative precipitation                   cm          I        *
*  TESOIL   cumulative soil evaporation                cm          I        *
*  TRUNOF   cumulative surface runoff                  cm          I        *
*  TIRR     cumulative irrigation                      cm          I        *
*  TRANRF   reduction factor for crop growth            -          I        *
*	      as a result of drought/wetness              -          I        *
*  TERMIN   indicates terminal section                  -          I,C      *
*  STATR    name of weather station (CHAR*5)            -          I        *
*  ISOIL    number of soil type                         -          I        *
*  ICROP    number of crop variety                      -          I        *
*  IRRI     0= irrigation; 1= irrigation                -          I        *
*  RUNFR    fraction of precipitation lost by runoff    -          I        *
*  CO       atmospheric CO2 concentration               ppmv       I        *
*  WTOT     total water in maximum rooted zone          cm         I        *
*  WAVT     total available water in maximum rooted zone cm        I        *
*  TSUML    temperature sum for development since emergence C.d    I        *
*  NNI      nitrogen nutrition index for crop           -          I        *
*  NMINT    mineral N available from soil and fertiliser kg N/ha   I        *
*  NMIN     mineral N available from soil for crop     kg N/ha     I        *
*  NUPTT    total N uptake by crop from soil           kg N/ha     I        *
*  NFIXTT   total N uptake by crop from biol.fixation  kg N/ha     I        *
*  NLIVT    total N in living crop organs              kg N/ha     I        *
*  NLOSST   total N in dead crop organs                kg N/ha     I        *
*                                                                           *
*  SUBROUTINES called : none                                                *
*  FILE usage : crop status is written to output file                       *
*	    CRPOUT.DAT                                                        *
*---------------------------------------------------------------------------*

      SUBROUTINE DAILOUT (IOPT,IDAY,IOUT,IDEM,IDPL,IDEMERG,
     $                   WLVG, WLVD, WST,WSO,TAGB,TPARINT,TPAR,
     $                   REMARK,DVS,RUNNAME,LAI,
     $                   SMACT,TTRANS,TDRAIN,TRAIN,TESOIL,TRUNOF,TIRR,
     $                   TRANRF,TERMIN,STATR,ISOIL,ICROP, IRRI,RUNFR,CO,
     $                   WTOT,WAVT,TSUML,NNI,NMINT,NMIN,NUPTT,NFIXTT,
     $                   NLIVT,NLOSST)

      IMPLICIT REAL (A-Z)
      INTEGER IDAY,IOUT,IDEM,IDPL,IDEMERG,ISOIL,ICROP,IRRI,ISYR,ISYEAR
      INTEGER IDEM1,IDPL1,ICOUNT,IOPT

      LOGICAL TERMIN, OPEN
      CHARACTER REMARK*80,RUNNAME*5,OUTFIL*12,STATR*5
      COMMON /ITIME/ ISYR, ISYEAR
      DATA OPEN /.TRUE./
      

      IF (OPEN) THEN

*---- open output file Run/PTS.OUT

        OUTFIL = RUNNAME//'PTS.OUT'
        OPEN(13,FILE=OUTFIL,STATUS='NEW')
      
        OPEN = .FALSE.

      ENDIF
      
      IF ((IDPL .LE. 0 .AND. IDAY .EQ. IDEM) .OR. 
     $     (IDPL .GT. 0 .AND. IDAY .EQ. IDPL)) THEN 
 
      IDEM1= IDEM
      IDPL1= IDPL
      IF(IDPL .LE. 0) IDPL1= 0
      IF (IDPL .GT. 0) IDEM1= 0
      
*---- write filename and start date on top of output-file

      WRITE (13,'(//A,A)') 'DATA-FILE  ', OUTFIL
      WRITE (13,'(//A,A)') 'Name of run: ', RUNNAME 
      WRITE (13,'(//A,A,A,I3,A,I3,A,I3,A,I3,A,F4.2,A,F4.0)') 
     $   'Station=  ',STATR,', Soil=',ISOIL,', Crop=',ICROP,', 
     $   Irr.=',IRRI,', IOPT=',IOPT,', Runof=',RUNFR,', CO2=',CO 
      WRITE (13,'(/(A,I3,A,I4))')
     $      ' Planting on day ',IDPL1,', ',ISYEAR,
     $      ' Crop emergence on day ',IDEM1,', ',ISYEAR


*---- Write output remarks to file Run/PTS.OUT

      WRITE (13,'(1X,A)') REMARK


*---- writes header for to output file Run/PTS.OUT
      WRITE (13,'(/A)') ' Crop PRODUCTION AND SOIL WATER STATUS'
      WRITE (13,'(A)')  ' ======================================='



         WRITE (13,'(4A)')
     $  ' DAY   WLVG    WLVD    WST    WSO     TAGB    TSUML   DVS  ',
     $  '   LAI  TRANRF  SMACT  TRAIN   TIRR  TTRANS   TESOIL   ',
     $  ' TDRAIN  TRUNOF   WTOT  WAVT   PARAB    PAR    NNI   NMINT   ',
     $  ' NMIN    NUPT    NFIX    NLIV    NLOSS   '  

      
            
      ICOUNT = -1
      
      ENDIF
 

      ICOUNT = ICOUNT+1

*----- Components of water balance in mm
       TRAIN1= 10.* TRAIN
       TIRR1= 10.* TIRR
       TTRANS1= 10.* TTRANS        
       TESOIL1= 10.* TESOIL
       TDRAIN1= 10.* TDRAIN
       TRUNOF1= 10.* TRUNOF
       WTOT1= 10.* WTOT
       WAVT1= 10.* WAVT
 
      IF (MOD(ICOUNT,IOUT) .EQ. 0 .OR. (IDAY .EQ. IDEMERG) 
     $      .OR. TERMIN) THEN

      WRITE (13,'(1X,I3,6F8.0,4F7.2,6F8.0,2F8.0,2F7.0,F7.2, 6F8.0))')
     $     IDAY,WLVG, WLVD, WST,WSO,TAGB,TSUML,DVS,LAI,TRANRF,SMACT,
     $       TRAIN1,TIRR1,TTRANS1,TESOIL1,
     $       TDRAIN1,TRUNOF1,WTOT1,WAVT1,TPARINT,TPAR,
     $       NNI,NMINT,NMIN, NUPTT,NFIXTT,NLIVT,NLOSST

      ENDIF
      RETURN
      END
