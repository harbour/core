#
# $Id: make_all.mak 7958 2007-11-12 15:10:28Z druzus $
#

# Makefile for buildning all buildable contribs.
# It is common for Bcc and Msvc

#**********************************************************

!ifndef HB_CC_NAME
!error  HB_CC_NAME environment variable not defined
!endif

#**********************************************************

!ifndef ECHO
ECHO = echo
!endif
!ifndef DEL
DEL = del
!endif

#**********************************************************

SHEET=\
hbclip\

DIRS=\
adordd\
bmdbfcdx\
btree\
examples\
hbzlib\
hgf\
htmllib\
libct\
libgt\
libmisc\
libnf\
msql\
odbc\
ole\
rdd_ads\
samples\
telepath\
tip\
win32\
xhb\

!if "$(APOLLO_DIR)" != ""
DIRS=$(DIRS) apollo
!endif

!if "$(DIRECTX_DIR)" != ""
DIRS=$(DIRS) directx
!endif

!if "$(FIREBIRD_DIR)" != ""
DIRS=$(DIRS) firebird
!endif

!if "$(GD_DIR)" != ""
DIRS=$(DIRS) gd
!endif

!if "$(FREEIMAGE_DIR)" != ""
DIRS=$(DIRS) freeimage
!endif

!if "$(MYSQL_DIR)" != ""
DIRS=$(DIRS) mysql
!endif

!if "$(PDFLIB_DIR)" != ""
DIRS=$(DIRS) pdflib
!endif

!if "$(PGSQL_DIR)" != ""
DIRS=$(DIRS) pgsql
!endif

#**********************************************************

all : prebuild exec

prebuild :
   @echo @set HB_BUILD_TARGET=all     > mk_trg.bat

#**********************************************************

clean : preclean exec
Clean : preclean exec
CLEAN : preclean exec

preclean :
   @echo @set HB_BUILD_TARGET=clean   > mk_trg.bat

#**********************************************************

install : preinstall exec
Install : preinstall exec
INSTALL : preinstall exec

preinstall :
   @echo @set HB_BUILD_TARGET=install > mk_trg.bat

#**********************************************************

exec :
   @$(ECHO) @echo off   					  > mk_one.bat
   @$(ECHO) if """%%1""" == """""" goto skip >> mk_one.bat
   @$(ECHO) echo Entering %%1 directory 	 >> mk_one.bat
   @$(ECHO) rem set HB_SHOW_ERRORS=no   		 >> mk_one.bat
   @$(ECHO) cd %%1  						 >> mk_one.bat
   @$(ECHO) call ..\mk_trg.bat  			 >> mk_one.bat
   @$(ECHO) if exist make_$(HB_CC_NAME).bat call make_$(HB_CC_NAME).bat %%%HB_BUILD_TARGET%%% >> mk_one.bat
   @$(ECHO) cd ..   						 >> mk_one.bat
   @$(ECHO) :skip   						 >> mk_one.bat
   @<<mk_all.bat
@%%COMSPEC%% /c mk_one.bat $(DIRS: =^
@%%COMSPEC%% /c mk_one.bat )
<<KEEP
   @if exist mk_all.bat $(DEL) mk_all.bat  > nul
   @if exist mk_one.bat $(DEL) mk_one.bat  > nul
   @if exist mk_trg.bat $(DEL) mk_trg.bat  > nul

#**********************************************************
