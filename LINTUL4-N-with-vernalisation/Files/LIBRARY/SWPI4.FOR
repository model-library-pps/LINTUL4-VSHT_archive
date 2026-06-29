** $Id: swpi4.for 1.1 1997/10/02 16:08:14 LEM release $
* This file was intentionally removed of
* comment lines and indentation
      SUBROUTINE SWPI4 (ITASK,NAME,IUNIT,DECL,IP,VAL)
      IMPLICIT NONE
      INTEGER ITASK,IUNIT,DECL,IP
      CHARACTER*(*) NAME
      INTEGER MNA
      PARAMETER (MNA=10)
      CHARACTER*62 ANAME(MNA),LNAME,EMPTY
      INTEGER AUNIT(MNA),NREC(MNA),DECLA(MNA)
      INTEGER I1,IN,IFINDC,RECN,TYPLEN
      CHARACTER MODNAM*6
      LOGICAL UNITOP
      INTEGER VAL,ZERO
      SAVE
      DATA ZERO /0/, TYPLEN /4/, MODNAM /'SWPI4'/
      DATA ANAME /MNA*' '/,EMPTY /' '/
      LNAME = NAME
      IF (LNAME.EQ.' ') CALL FATALERR (MODNAM,'empty array name')
      IN    = IFINDC (ANAME,MNA,1,MNA,LNAME)
      IF (ITASK.EQ.1) THEN
      IF (IN.EQ.0) THEN
      IF (DECL.LT.1) CALL FATALERR (MODNAM,
     &'illegal declared length')
      IF (IUNIT.LT.10.OR.IUNIT.GT.99) CALL FATALERR
     &(MODNAM,'illegal unit number')
      INQUIRE (UNIT=IUNIT,OPENED=UNITOP)
      IF (UNITOP) CALL FATALERR (MODNAM,'unit is already in use')
      IN = IFINDC (ANAME,MNA,1,MNA,EMPTY)
      IF (IN.EQ.0) CALL FATALERR
     &(MODNAM,'internal name array full')
      ANAME(IN) = LNAME
      DECLA(IN) = DECL
      NREC(IN)  = 0
      AUNIT(IN) = IUNIT
      OPEN (AUNIT(IN),STATUS='SCRATCH',
     &ACCESS='DIRECT',FORM='UNFORMATTED',RECL=TYPLEN)
      END IF
      IF (IP.LE.DECLA(IN)) CALL FATALERR (MODNAM,
     &'pointer less than declared value')
      RECN = IP-DECLA(IN)
      IF (RECN.GT.NREC(IN)) THEN
      DO 10 I1=NREC(IN)+1,RECN-1
      WRITE (AUNIT(IN),REC=I1) ZERO
10    CONTINUE
      NREC(IN) = RECN
      END IF
      WRITE (AUNIT(IN),REC=RECN) VAL
      ELSE IF (ITASK.EQ.2) THEN
      IF (IN.GT.0) THEN
      RECN = IP-DECLA(IN)
      IF (RECN.GE.1.AND.RECN.LE.NREC(IN)) THEN
      READ (AUNIT(IN),REC=RECN) VAL
      ELSE IF (RECN.GT.NREC(IN)) THEN
      CALL WARNING (MODNAM,'returning uninitialized element')
      VAL = ZERO
      ELSE IF (RECN.LT.1) THEN
      CALL FATALERR (MODNAM,
     &'array index below declared length')
      END IF
      ELSE
      CALL FATALERR (MODNAM,'unknown array name')
      END IF
      ELSE IF (ITASK.EQ.3) THEN
      IF (IN.GT.0) THEN
      ANAME(IN) = ' '
      CLOSE (AUNIT(IN))
      AUNIT(IN) = 0
      END IF
      ELSE
      CALL FATALERR (MODNAM,'illegal itask')
      END IF
      RETURN
      END
