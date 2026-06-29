*--------------------------------------------------------------------*
*  SUBROUTINE INITP                                                  *
*  Author : Joost Wolf, based on routine of Rob Groot                *
*  Date   : 29-april-1996                                            *
*  Purpose: This subroutine reads data required to run program       *
*           interactively from  screen  or in batch-mode             *
*                                                                    *
*  FORMAL PARAMETERS:  (I=input,O=output,C=control,IN=init,T=time)   *
*  name    meaning                                     units  class  *
*  ----    -------                                     -----  -----  *
*  IIYEAR   year in which simulation starts              -      T,O  *
*  INYEAR  number of simulation runs (i.e. years)        -      T,O  *
*  IDPL    day number of planting                        -      T,O  *
*  IDEM    day number of plant emergence                 -      T,O  *
*  IFINIT  maximum number of days for single run         d      T,O  *
*  IGAP    The maximum difference in days that is allowed            *
*          between subsequent data file records.         d      T,O  *
*  REMARK  remark, printed on top of output files        -      O    *
*  RUNNAME first 5 char's of output file-name (CHAR*5)   -      O    *
*  STATR   name of weather station (CHAR*5)              -      O    *
*  CONTIN  more weather stations                                O    *
*  SENSP   sensitivity parameter for precipitation       -      O    *
*  SENSR    idem for radiation                           -      O    *
*  SENSW    idem for windspeed                           -      O    *
*  SENSV    idem for vapour pressure                     -      O    *
*  SENST    idem for temperature                         -      O    *
*  CO       atmospheric CO2 concentration                ppmv   O    *
*  ISOIL   number of soil type                                  O    *
*  ICROP   number of crop variety                               O    *
*  IOPT    indicates optimal (=1), water limited (=2)                *
*           or water and N limited run (=3)               -     O    *
*  IRRI    automatic irrigation (=1), actual irrigation              * 
*           from table (=2) or non irrigated(=0)          -     O    *
*  IOUT    time interval for output                       d     O    *
*  WTRDIR  directory where weather data are stored                   *
*  SUBROUTINES and FUNCTIONS called : none                           *
*                                                                    *
*  FILE usage : none                                                 *
*--------------------------------------------------------------------*

      SUBROUTINE INITP (IIYR,INYEAR,IDPL,IDEM,IFINIT,IGAP,REMARK,
     $  RUNNAME,STATR,CONTIN,SENSP,SENSV,SENSW,SENSR,SENST,CO,
     $  ISOIL,ICROP,IOPT,IRRI,IOUT,WTRDIR)


      IMPLICIT REAL (A-Z)
      INTEGER IIYR,INYEAR,IDPL,IDEM,IFINIT,IGAP,ISOIL,ICROP,IOPT,IRRI
      INTEGER IOUT,IIYEAR

      CHARACTER REMARK*80,RUNNAME*5,STATR*5,WTRDIR*80
      CHARACTER ANS*1,CONTIN*1
      LOGICAL BATCH,START    
      DATA START /.TRUE./    
      DATA BATCH /.FALSE./

*---- maximum difference in days that is allowed between subsequent
*     data in weather-data-files (igap)

      IGAP  = 10
      IOUT= 10
      ISOIL= 1
      ICROP= 1
      IOPT= 1 
      IRRI= 0     
      WTRDIR= ' '
      SENSP= 1.
      SENSV= 1.
      SENSW= 1.
      SENSR= 1.
      SENST= 0.
      CO= 370.
      CALL CLS

      IF (.NOT. START) GOTO 10 

*    ask user if batch processing is required
     
      WRITE (*, '(/A$)') '  Do you want to run from a batch file',
     $  '  yes (Y) or no (N) ?            ' 
      READ (*,'(A)') ANS
      IF (ANS .EQ. 'Y' .OR. ANS .EQ. 'y') THEN
      BATCH = .TRUE.
      ENDIF

      IF (BATCH) THEN 
*     batch input
      OPEN (99,FILE= 'BATCH.INP',STATUS= 'OLD')
      CALL MOFILP (99)
      START= .FALSE.
      ENDIF      

10    IF (BATCH) READ (99, *) RUNNAME, IIYEAR, INYEAR, IDPL, IDEM, 
     $   IFINIT, STATR, SENSP, SENSV, SENSW, SENSR, SENST, CO, 
     $   ISOIL, ICROP,
     $   IOPT, IRRI, IOUT,WTRDIR, REMARK, CONTIN


      IF (BATCH)  GOTO 100
      CONTIN= 'N'     
      WRITE (*,'(////////A)') ' Crop and soil water dynamics'
      WRITE (*,'(/////A$)') ' start year for simulation         : '
      READ  (*,'(I4)') IIYEAR

      WRITE (*,'(A$)') ' simulation during how many years      : '
      READ  (*,'(I4)') INYEAR
      
      WRITE (*,'(a/a$)')      ' sowing/planting day number for crop ',
     $      '     ( -99 --> simulation from date of emergence) : '
      READ  (*,'(I3)') IDPL

      WRITE (*,'(a/a$)')      ' day number for crop emergence ',
     $     '    (not used if planting date was already entered) : '
      READ  (*,'(I3)') IDEM                

      WRITE (*,'(2A$)') ' maximal length of simulation since planting',
     $     ' or emergence (days)  : '
      READ  (*,'(I4)') IFINIT

      WRITE (*,'(A$)') ' type of run, optimal (1),',
     $  ' waterlimited (2) or N and water limited conditions (3)  : '
      READ (*,'(I1)') IOPT

      WRITE (*,'(A$)') ' weather station (max.5 characters): '
      READ  (*,'(a)') STATR

      WRITE (*,'(a$)') ' output filename (max.5 characters): '
      READ  (*,'(a)') RUNNAME

      WRITE (*,'(A$)') ' output remarks                    : '
      READ  (*,'(a)') REMARK

100   CONTINUE

      IIYR  = IIYEAR - 1900
     

      RETURN
      END