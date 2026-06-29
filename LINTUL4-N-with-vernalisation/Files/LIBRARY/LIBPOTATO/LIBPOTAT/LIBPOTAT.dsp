# Microsoft Developer Studio Project File - Name="LIBPOTAT" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=LIBPOTAT - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "LIBPOTAT.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "LIBPOTAT.mak" CFG="LIBPOTAT - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "LIBPOTAT - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "LIBPOTAT - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "LIBPOTAT - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /compile_only /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "LIBPOTAT - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "LIBPOTAT - Win32 Release"
# Name "LIBPOTAT - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=..\..\ADDINF.FOR
# End Source File
# Begin Source File

SOURCE=..\..\ADDINT.FOR
# End Source File
# Begin Source File

SOURCE=..\..\ADDREA.FOR
# End Source File
# Begin Source File

SOURCE=..\..\ADDREF.FOR
# End Source File
# Begin Source File

SOURCE=..\..\ADDSTF.FOR
# End Source File
# Begin Source File

SOURCE=..\..\ADDSTR.FOR
# End Source File
# Begin Source File

SOURCE=..\..\AFINVS.FOR
# End Source File
# Begin Source File

SOURCE=..\..\AMBUSY.FOR
# End Source File
# Begin Source File

SOURCE=..\..\CHKTSK.FOR
# End Source File
# Begin Source File

SOURCE=..\..\CLS.FOR
# End Source File
# Begin Source File

SOURCE=..\..\COPFIL.FOR
# End Source File
# Begin Source File

SOURCE=..\..\COPFL2.FOR
# End Source File
# Begin Source File

SOURCE=..\..\DECCHK.FOR
# End Source File
# Begin Source File

SOURCE=..\..\DECINT.FOR
# End Source File
# Begin Source File

SOURCE=..\..\DECREA.FOR
# End Source File
# Begin Source File

SOURCE=..\..\DECREC.FOR
# End Source File
# Begin Source File

SOURCE=..\..\DELFIL.FOR
# End Source File
# Begin Source File

SOURCE=..\..\DTARDP.FOR
# End Source File
# Begin Source File

SOURCE=..\..\DTDPAR.FOR
# End Source File
# Begin Source File

SOURCE=..\..\DTDPST.FOR
# End Source File
# Begin Source File

SOURCE=..\..\DTFSECMP.FOR
# End Source File
# Begin Source File

SOURCE=..\..\DTFSEDP.FOR
# End Source File
# Begin Source File

SOURCE=..\..\DTLEAP.FOR
# End Source File
# Begin Source File

SOURCE=..\..\DTNOW.FOR
# End Source File
# Begin Source File

SOURCE=..\..\DTSYS.FOR
# End Source File
# Begin Source File

SOURCE=..\..\ENTCHA.FOR
# End Source File
# Begin Source File

SOURCE=..\..\ENTDCH.FOR
# End Source File
# Begin Source File

SOURCE=..\..\ENTDIN.FOR
# End Source File
# Begin Source File

SOURCE=..\..\ENTDRE.FOR
# End Source File
# Begin Source File

SOURCE=..\..\ENTINT.FOR
# End Source File
# Begin Source File

SOURCE=..\..\ENTREA.FOR
# End Source File
# Begin Source File

SOURCE=..\..\ERROR.FOR
# End Source File
# Begin Source File

SOURCE=..\..\EXTENS.FOR
# End Source File
# Begin Source File

SOURCE=..\..\FATALERR.FOR
# End Source File
# Begin Source File

SOURCE=..\..\FCNSW.FOR
# End Source File
# Begin Source File

SOURCE=..\..\FLEXIST.FOR
# End Source File
# Begin Source File

SOURCE=..\..\FLNAME.FOR
# End Source File
# Begin Source File

SOURCE=..\..\FOPENG.FOR
# End Source File
# Begin Source File

SOURCE=..\..\FOPENS.FOR
# End Source File
# Begin Source File

SOURCE=..\..\GETCHR.FOR
# End Source File
# Begin Source File

SOURCE=..\..\GETREC.FOR
# End Source File
# Begin Source File

SOURCE=..\..\GETUN.FOR
# End Source File
# Begin Source File

SOURCE=..\..\GETUN2.FOR
# End Source File
# Begin Source File

SOURCE=..\..\IFINDC.FOR
# End Source File
# Begin Source File

SOURCE=..\..\IFINDI.FOR
# End Source File
# Begin Source File

SOURCE=..\..\ILEN.FOR
# End Source File
# Begin Source File

SOURCE=..\..\INSW.FOR
# End Source File
# Begin Source File

SOURCE=..\..\INTGRL.FOR
# End Source File
# Begin Source File

SOURCE=..\..\ISTART.FOR
# End Source File
# Begin Source File

SOURCE=..\..\IUNIFL.FOR
# End Source File
# Begin Source File

SOURCE=..\..\LIMIT.FOR
# End Source File
# Begin Source File

SOURCE=..\..\LINT.FOR
# End Source File
# Begin Source File

SOURCE=..\..\LINT2.FOR
# End Source File
# Begin Source File

SOURCE=..\..\LOWERC.FOR
# End Source File
# Begin Source File

SOURCE=..\..\MOFILP.FOR
# End Source File
# Begin Source File

SOURCE=..\..\MOVAVR.FOR
# End Source File
# Begin Source File

SOURCE=..\..\NOTNUL.FOR
# End Source File
# Begin Source File

SOURCE=..\..\OUTAR2.FOR
# End Source File
# Begin Source File

SOURCE=..\..\OUTARR.FOR
# End Source File
# Begin Source File

SOURCE=..\..\OUTCOM.FOR
# End Source File
# Begin Source File

SOURCE=..\..\OUTDAT.FOR
# End Source File
# Begin Source File

SOURCE=..\..\OUTPLT.FOR
# End Source File
# Begin Source File

SOURCE=..\..\OUTSEL.FOR
# End Source File
# Begin Source File

SOURCE=..\..\PLTFUN.FOR
# End Source File
# Begin Source File

SOURCE=..\..\PLTHIS.FOR
# End Source File
# Begin Source File

SOURCE=..\..\POS.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDACHA.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDADOR.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDADOU.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDAINR.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDAINT.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDALOG.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDAREA.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDARER.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDATIM.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDDATA.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDDECD.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDDECI.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDDECL.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDDECT.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDDTMP.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDERR.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDFCHA.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDFDOR.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDFDOU.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDFINR.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDFINT.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDFLOG.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDFREA.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDFRER.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDFROM.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDFTIM.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDINDT.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDINDX.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDINIT.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDINLV.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDINNE.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDINQR.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDINQR2.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDLEX.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDMCHA.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDMDEF.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDMDOU.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDMINT.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDMLOG.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDMREA.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDMTIM.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDPARS.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDSCHA.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDSCTB.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDSDOR.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDSDOU.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDSETS.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDSINR.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDSINT.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDSLOG.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDSREA.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDSRER.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDSTIM.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDTMP1.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RDTMP2.FOR
# End Source File
# Begin Source File

SOURCE=..\..\REAAND.FOR
# End Source File
# Begin Source File

SOURCE=..\..\REANOR.FOR
# End Source File
# Begin Source File

SOURCE=..\..\RECREAD.FOR
# End Source File
# Begin Source File

SOURCE=..\..\REMOVE.FOR
# End Source File
# Begin Source File

SOURCE=..\..\SFINDG.FOR
# End Source File
# Begin Source File

SOURCE=..\..\SORTCH.FOR
# End Source File
# Begin Source File

SOURCE=..\..\STR_COPY.FOR
# End Source File
# Begin Source File

SOURCE=..\..\STRIP.FOR
# End Source File
# Begin Source File

SOURCE=..\..\SWPI4.FOR
# End Source File
# Begin Source File

SOURCE=..\..\TIMER2.FOR
# End Source File
# Begin Source File

SOURCE=..\..\TTUVER.FOR
# End Source File
# Begin Source File

SOURCE=..\..\UNIFL.FOR
# End Source File
# Begin Source File

SOURCE=..\..\UPPERC.FOR
# End Source File
# Begin Source File

SOURCE=..\..\USEDUN.FOR
# End Source File
# Begin Source File

SOURCE=..\..\VER4_07.FOR
# End Source File
# Begin Source File

SOURCE=..\..\WARNING.FOR
# End Source File
# Begin Source File

SOURCE=..\..\WORDS.FOR
# End Source File
# Begin Source File

SOURCE=..\..\WRACHA.FOR
# End Source File
# Begin Source File

SOURCE=..\..\WRAINT.FOR
# End Source File
# Begin Source File

SOURCE=..\..\WRALOG.FOR
# End Source File
# Begin Source File

SOURCE=..\..\WRAREA.FOR
# End Source File
# Begin Source File

SOURCE=..\..\WRINIT.FOR
# End Source File
# Begin Source File

SOURCE=..\..\WRSCHA.FOR
# End Source File
# Begin Source File

SOURCE=..\..\WRSINT.FOR
# End Source File
# Begin Source File

SOURCE=..\..\WRSLOG.FOR
# End Source File
# Begin Source File

SOURCE=..\..\WRSREA.FOR
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# End Target
# End Project
