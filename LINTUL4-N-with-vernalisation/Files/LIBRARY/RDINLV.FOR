** $Id: rdinlv.for 1.1 1997/10/02 16:08:12 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE RDINLV (SETFLAG,VARLIS,VARLIS_MN,VARLIS_AN)
      IMPLICIT NONE
      LOGICAL SETFLAG
      INTEGER VARLIS_MN,VARLIS_AN
      CHARACTER*(*) VARLIS(VARLIS_MN)
*     * * * * * * * * * * * * * * * * * * * * * * * * * * *
*     *                                                   *
*     *  M A C H I N E   D E P E N D E N T   V A L U E S  *
*     *  ===============================================  *
*     *                                                   *
*     * * * * * * * * * * * * * * * * * * * * * * * * * * *
 
*     IIL    - length in bytes of INTEGER datatype
*     IRL    - length in bytes of DOUBLE PRECISION datatype
*     ILL    - length in bytes of LOGICAL datatype
*     ILNREP - maximum number of names in each set of a rerun file
*     ILPREP - maximum number of assignments on a rerun file (40 sets
*              of 10 variables for instance, gives 400 assignments)
*     ILNDAT - maximum number of names in a data file
*              increase this number if you have more variables
*     INFDEC - maximum number of files in tmp file list
 
      INTEGER IIL, IRL, ILL, ILNREP, ILPREP, ILNDAT, INFDEC
      PARAMETER (IIL=4, IRL=8, ILL=4)
      PARAMETER (ILNREP=40,ILPREP=400)
      PARAMETER (ILNDAT=400)
      PARAMETER (INFDEC=10)
 
 
 
*     * * * * * * * * * * * * * * * * * * * * * * * * * * *
*     *                                                   *
*     *    B U F F E R   L E N G T H   I N   B Y T E S    *
*     *    ===========================================    *
*     *                                                   *
*     * * * * * * * * * * * * * * * * * * * * * * * * * * *
 
*     ILBUF - length of TMP file buffers
*
      INTEGER ILBUF
      PARAMETER (ILBUF=128)
*     index rerun file ; index available after CALL RDSETS
*     When you use the common blocks TTURR1 and TTURR2 the array lengths
*     in your declarations should correspond to the ones here !!!!!!
      INTEGER IARREP,IPTREP,INFREP
      CHARACTER REPLIS*31,REPTYP*1
      DIMENSION REPLIS(ILNREP),REPTYP(ILNREP)
      DIMENSION IARREP(ILPREP),IPTREP(ILPREP)
 
*     Active set number
      INTEGER ISCOM
 
      COMMON /TTURR1/ REPLIS,REPTYP
      COMMON /TTURR2/ IARREP,IPTREP,INFREP,ISCOM
 
*     index data file ; index available after CALL RDINIT
*     When you use the common blocks TTUDD1 and TTUDD2 the array length
*     in your declarations should correspond to the one here !!!!!
      INTEGER IARDAT,IPTDAT,INFDAT
      CHARACTER DATLIS*31,DATTYP*1
      DIMENSION DATLIS(ILNDAT),DATTYP(ILNDAT)
      DIMENSION IARDAT(ILNDAT),IPTDAT(ILNDAT)
      COMMON /TTUDD1/ DATLIS,DATTYP
      COMMON /TTUDD2/ IARDAT,IPTDAT,INFDAT
      INTEGER I1,I2,I3,ILEN
      SAVE
      IF (.NOT.SETFLAG.OR.ISCOM.EQ.0) THEN
      IF (INFDAT.EQ.0) CALL FATALERR ('RDINLV','no data file active')
      IF (INFDAT.GT.VARLIS_MN) CALL FATALERR
     &('RDINLV','external list does not have enough elements')
      I3 = LEN (VARLIS(1))
      DO I1=1,INFDAT
      I2 = ILEN (DATLIS (I1))
      IF (I2.LE.I3) THEN
      VARLIS(I1) = DATLIS(I1)(1:I2)
      ELSE
      CALL FATALERR ('RDINLV',
     &'variable name to long for external list')
      END IF
      END DO
      VARLIS_AN = INFDAT
      ELSE IF (SETFLAG) THEN
      IF (INFREP.EQ.0) CALL FATALERR
     &('RDINLV','no rerun file active')
      IF (INFREP.GT.VARLIS_MN) CALL FATALERR
     &('RDINLV','external list does not have enough elements')
      I3 = LEN (VARLIS(1))
      DO I1=1,INFREP
      I2 = ILEN (REPLIS (I1))
      IF (I2.LE.I3) THEN
      VARLIS(I1) = REPLIS(I1)(1:I2)
      ELSE
      CALL FATALERR ('RDINLV',
     &'variable name to long for external list')
      END IF
      END DO
      VARLIS_AN = INFREP
      END IF
      RETURN
      END
