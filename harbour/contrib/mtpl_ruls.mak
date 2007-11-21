#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2007 Marek Paliwoda (mpaliwoda "at" interia "dot" pl)
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

# --------------------------------------------------------
# Makefile common section for Harbour Project Contrib libs
# for Microsoft Visual C/Borland C/C++ - common rules
# -----------------------------------------------------------------

ALL_HEADERS = $(PRG_HEADERS) $(C_HEADERS)

#**********************************************************
# CLEAN rule(s)
#**********************************************************

clean: doClean
Clean: doClean
CLEAN: doClean

doClean:
   @if exist $(LIB_PATH) $(DEL) $(LIB_PATH)   > nul
   @$(ECHO) @echo off   					  > delone.bat
   @$(ECHO) if """%%1""" == """""" goto skip >> delone.bat
   @$(ECHO) if exist %%1.c   $(DEL) %%1.c    >> delone.bat
   @$(ECHO) if exist %%1.obj $(DEL) %%1.obj  >> delone.bat
   @$(ECHO) :skip   						 >> delone.bat
   @<<delall.bat
@%%COMSPEC%% /c delone.bat $(LIB_OBJS:.obj=^
@%%COMSPEC%% /c delone.bat )
<<KEEP
   @if exist delall.bat $(DEL) delall.bat > nul
   @if exist delone.bat $(DEL) delone.bat > nul
   @if exist delall.bat $(DEL) delall.bat > nul

!if "$(HB_INSTALL_PREFIX)" == "$(HB_ROOT)"
   @if exist $(HB_LIB_INSTALL)\$(LIBNAME)$(LIBEXT) $(DEL) $(HB_LIB_INSTALL)\$(LIBNAME)$(LIBEXT) > nul
   @$(ECHO) @echo off   					  > delone.bat
   @$(ECHO) if """%%1""" == """""" goto skip >> delone.bat
   @$(ECHO) if exist $(HB_INC_INSTALL)\%%1 $(DEL) $(HB_INC_INSTALL)\%%1 >> delone.bat
   @$(ECHO) :skip   						 >> delone.bat
   @<<delall.bat
@%%COMSPEC%% /c delone.bat $(ALL_HEADERS: =^
@%%COMSPEC%% /c delone.bat )
<<KEEP
   @if exist delall.bat $(DEL) delall.bat > nul
   @if exist delone.bat $(DEL) delone.bat > nul
   @if exist delall.bat $(DEL) delall.bat > nul
!endif

#**********************************************************

#**********************************************************
# INSTALL rule(s)
#**********************************************************

install: doInstall
Install: doInstall
INSTALL: doInstall

doInstall:
   @if exist $(LIB_PATH) copy $(LIB_PATH) $(HB_LIB_INSTALL) > nul
   @$(ECHO) @echo off   							 > cpyone.bat
   @$(ECHO) if """%%1""" == """""" goto skip		>> cpyone.bat
   @$(ECHO) if exist %%1 copy %%1 $(HB_INC_INSTALL) >> cpyone.bat
   @$(ECHO) :skip   								>> cpyone.bat
   @<<cpyall.bat
@%%COMSPEC%% /c cpyone.bat $(ALL_HEADERS: =^
@%%COMSPEC%% /c cpyone.bat )
<<KEEP
   @if exist cpyall.bat $(DEL) cpyall.bat > nul
   @if exist cpyone.bat $(DEL) cpyone.bat > nul
   @if exist cpyall.bat $(DEL) cpyall.bat > nul

#**********************************************************
