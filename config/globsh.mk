# ---------------------------------------------------------------
# Copyright 2009 Viktor Szakats (vszakats.net/harbour) (rework)
# See COPYING.txt for licensing terms.
# ---------------------------------------------------------------

# ---------------------------------------------------------------
# See batch docs here:
#    http://www.computerhope.com/batch.htm
#    http://www.robvanderwoude.com/batchcommands.php
# ---------------------------------------------------------------

ifneq ($(HB_SHELL),sh)

TOOL_DIR := $(subst /,\,$(TOP)$(ROOT)config/)

# Have to use '=' operator here for rules to work
PKG_DIR_OS = $(subst /,\,$(PKG_DIR))
OBJ_DIR_OS = $(subst /,\,$(OBJ_DIR))
LIB_DIR_OS = $(subst /,\,$(LIB_DIR))
LIB_FILE_OS = $(subst /,\,$(LIB_FILE))
BIN_DIR_OS = $(subst /,\,$(BIN_DIR))
BIN_FILE_OS = $(subst /,\,$(BIN_FILE))
DYN_DIR_OS = $(subst /,\,$(DYN_DIR))
DYN_FILE_OS = $(subst /,\,$(DYN_FILE))
IMP_DIR_OS = $(subst /,\,$(IMP_DIR))
IMP_FILE_OS = $(subst /,\,$(IMP_FILE))

endif

ifeq ($(HB_SHELL),sh)

MK := $(MAKE)
RM := rm -f
RDP := rm -f -r
CP := cp -f
LN := ln -sf
MD := mkdir
MDP := mkdir -p
ECHO := echo
ECHOQUOTE := "
TRUE := TRUE=true
FALSE := false

dirbase::
	@[ -d "$(OBJ_DIR)" ] || $(MDP) $(OBJ_DIR)
	@[ -z "$(LIB_FILE)" ] || [ -d "$(LIB_DIR)" ] || $(MDP) $(LIB_DIR)
	@[ -z "$(BIN_FILE)" ] || [ -d "$(BIN_DIR)" ] || $(MDP) $(BIN_DIR)
	@[ -z "$(DYN_FILE)" ] || [ -d "$(DYN_DIR)" ] || $(MDP) $(DYN_DIR)
	@[ -z "$(IMP_FILE)" ] || [ -d "$(IMP_DIR)" ] || $(MDP) $(IMP_DIR)

clean::
	-@$(RDP) $(PKG_DIR) $(OBJ_DIR) $(LIB_FILE) $(BIN_FILE) $(DYN_FILE) $(IMP_FILE); \
	if [ -n "$(LIB_FILE)" ]; then \
	   $(RM) $(basename $(LIB_FILE)).bak; \
	   [ "`$(ECHO) $(LIB_DIR)/*`" != "$(LIB_DIR)/*" ] || $(RDP) $(LIB_DIR); \
	fi ; \
	if [ -n "$(BIN_FILE)" ]; then \
	   $(RM) $(basename $(BIN_FILE)).tds; \
	   $(RM) $(basename $(BIN_FILE)).pch; \
	   $(RM) $(basename $(BIN_FILE)).pdb; \
	   $(RM) $(basename $(BIN_FILE)).ilk; \
	   [ "`$(ECHO) $(BIN_DIR)/*`" != "$(BIN_DIR)/*" ] || $(RDP) $(BIN_DIR); \
	fi ; \
	if [ -n "$(DYN_FILE)" ]; then \
	   $(RM) $(basename $(DYN_FILE)).tds; \
	   $(RM) $(basename $(DYN_FILE)).pch; \
	   $(RM) $(basename $(DYN_FILE)).pdb; \
	   $(RM) $(basename $(DYN_FILE)).ilk; \
	   $(RM) $(basename $(DYN_FILE)).def; \
	   $(RM) $(basename $(DYN_FILE)).exp; \
	   $(RM) $(DYN_FILE_NVR); \
	   $(RM) $(DYN_FILE_CPT); \
	   [ "`$(ECHO) $(DYN_DIR)/*`" != "$(DYN_DIR)/*" ] || $(RDP) $(DYN_DIR); \
	fi ; \
	if [ -n "$(IMP_FILE)" ]; then \
	   $(RM) $(basename $(IMP_FILE)).exp; \
	   [ "`$(ECHO) $(IMP_DIR)/*`" != "$(IMP_DIR)/*" ] || $(RDP) $(IMP_DIR); \
	fi

endif

ifeq ($(HB_SHELL),nt)

# NOTE: According to http://support.microsoft.com/kb/830473
#       The command line length limit for cmd.exe is 8191 chars
#       for Windows XP and upper and 2047 for Windows NT and 2000.
#       This is true for commands which are recognized as shell
#       commands (by using an internal list). For the rest
#       CreateProcess() is used, where the maximum length is 32767.
#       [vszakats]

MK := $(subst \,/,$(MAKE))
RM := del /q /f
RDP := rmdir /q /s
CP := copy
LN :=
MD := mkdir
MDP := mkdir
ECHO := echo
ECHOQUOTE :=
TRUE := $(ECHO) > nul
FALSE := $(MD) . 2> nul

dirbase::
	-@if not exist "$(OBJ_DIR_OS)" $(MDP) "$(OBJ_DIR_OS)"
	$(if $(LIB_FILE),-@if not exist "$(LIB_DIR_OS)" $(MDP) "$(LIB_DIR_OS)",)
	$(if $(BIN_FILE),-@if not exist "$(BIN_DIR_OS)" $(MDP) "$(BIN_DIR_OS)",)
	$(if $(DYN_FILE),-@if not exist "$(DYN_DIR_OS)" $(MDP) "$(DYN_DIR_OS)",)
	$(if $(IMP_FILE),-@if not exist "$(IMP_DIR_OS)" $(MDP) "$(IMP_DIR_OS)",)

clean::
	-@if exist "$(OBJ_DIR_OS)" $(RDP) "$(OBJ_DIR_OS)"
	-@if exist "$(PKG_DIR_OS)" $(RDP) "$(PKG_DIR_OS)"
	$(if $(LIB_FILE),-@if exist "$(LIB_FILE_OS)" $(RM) "$(LIB_FILE_OS)",)
	$(if $(LIB_FILE),-@if exist "$(basename $(LIB_FILE_OS)).bak" $(RM) "$(basename $(LIB_FILE_OS)).bak",)
	$(if $(LIB_FILE),-@if exist "$(LIB_DIR_OS)" if not exist "$(LIB_DIR_OS)\*" $(RDP) "$(LIB_DIR_OS)",)
	$(if $(BIN_FILE),-@if exist "$(BIN_FILE_OS)" $(RM) "$(BIN_FILE_OS)",)
	$(if $(BIN_FILE),-@if exist "$(basename $(BIN_FILE_OS)).tds" $(RM) "$(basename $(BIN_FILE_OS)).tds",)
	$(if $(BIN_FILE),-@if exist "$(basename $(BIN_FILE_OS)).pch" $(RM) "$(basename $(BIN_FILE_OS)).pch",)
	$(if $(BIN_FILE),-@if exist "$(basename $(BIN_FILE_OS)).pdb" $(RM) "$(basename $(BIN_FILE_OS)).pdb",)
	$(if $(BIN_FILE),-@if exist "$(basename $(BIN_FILE_OS)).ilk" $(RM) "$(basename $(BIN_FILE_OS)).ilk",)
	$(if $(BIN_FILE),-@if exist "$(BIN_DIR_OS)" if not exist "$(BIN_DIR_OS)\*" $(RDP) "$(BIN_DIR_OS)",)
	$(if $(DYN_FILE),-@if exist "$(DYN_FILE_OS)" $(RM) "$(DYN_FILE_OS)",)
	$(if $(DYN_FILE),-@if exist "$(basename $(DYN_FILE_OS)).tds" $(RM) "$(basename $(DYN_FILE_OS)).tds",)
	$(if $(DYN_FILE),-@if exist "$(basename $(DYN_FILE_OS)).pch" $(RM) "$(basename $(DYN_FILE_OS)).pch",)
	$(if $(DYN_FILE),-@if exist "$(basename $(DYN_FILE_OS)).pdb" $(RM) "$(basename $(DYN_FILE_OS)).pdb",)
	$(if $(DYN_FILE),-@if exist "$(basename $(DYN_FILE_OS)).ilk" $(RM) "$(basename $(DYN_FILE_OS)).ilk",)
	$(if $(DYN_FILE),-@if exist "$(basename $(DYN_FILE_OS)).def" $(RM) "$(basename $(DYN_FILE_OS)).def",)
	$(if $(DYN_FILE),-@if exist "$(basename $(DYN_FILE_OS)).exp" $(RM) "$(basename $(DYN_FILE_OS)).exp",)
	$(if $(DYN_FILE),-@if exist "$(DYN_DIR_OS)" if not exist "$(DYN_DIR_OS)\*" $(RDP) "$(DYN_DIR_OS)",)
	$(if $(IMP_FILE),-@if exist "$(IMP_FILE_OS)" $(RM) "$(IMP_FILE_OS)",)
	$(if $(IMP_FILE),-@if exist "$(basename $(IMP_FILE_OS)).exp" $(RM) "$(basename $(IMP_FILE_OS)).exp",)
	$(if $(IMP_FILE),-@if exist "$(IMP_DIR_OS)" if not exist "$(IMP_DIR_OS)\*" $(RDP) "$(IMP_DIR_OS)",)

endif

ifeq ($(HB_SHELL),os2)

# NOTE: Maximum size of command line in OS/2 is limited to 1024
#       characters.

# NOTE: Comment on included OS/2 GNU tools:
#       os2mkdir and os2cp expects forward slashes, while
#       os2rm expects backslashes in filenames. [vszakats]

MK := $(subst \,/,$(MAKE))
RM := $(TOOL_DIR)os2rm -f
RDP := $(TOOL_DIR)os2rm -fr
CP := $(TOOL_DIR)os2cp -f
LN :=
MD := $(TOOL_DIR)os2mkdir
MDP := $(TOOL_DIR)os2mkdir -p
ECHO := echo
ECHOQUOTE :=
TRUE := $(ECHO) > nul
# TODO
FALSE := $(TRUE)

dirbase::
	-@$(MDP) $(OBJ_DIR)
	$(if $(LIB_FILE),-@$(MDP) $(LIB_DIR),)
	$(if $(BIN_FILE),-@$(MDP) $(BIN_DIR),)
	$(if $(DYN_FILE),-@$(MDP) $(DYN_DIR),)
	$(if $(IMP_FILE),-@$(MDP) $(IMP_DIR),)

clean::
	-@$(RDP) $(PKG_DIR_OS) $(OBJ_DIR_OS) $(LIB_FILE_OS) $(BIN_FILE_OS) $(DYN_FILE_OS) $(IMP_FILE_OS)
	$(if $(LIB_FILE),-@$(RM) $(basename $(LIB_FILE_OS)).bak,)
	$(if $(LIB_FILE),$(if $(wildcard $(LIB_DIR)/*.*),,-@$(RDP) $(LIB_DIR_OS)),)
	$(if $(BIN_FILE),-@$(RM) $(basename $(BIN_FILE_OS)).tds,)
	$(if $(BIN_FILE),-@$(RM) $(basename $(BIN_FILE_OS)).pch,)
	$(if $(BIN_FILE),-@$(RM) $(basename $(BIN_FILE_OS)).pdb,)
	$(if $(BIN_FILE),-@$(RM) $(basename $(BIN_FILE_OS)).ilk,)
	$(if $(BIN_FILE),$(if $(wildcard $(BIN_DIR)/*.*),,-@$(RDP) $(BIN_DIR_OS)),)
	$(if $(DYN_FILE),-@$(RM) $(basename $(DYN_FILE_OS)).tds,)
	$(if $(DYN_FILE),-@$(RM) $(basename $(DYN_FILE_OS)).pch,)
	$(if $(DYN_FILE),-@$(RM) $(basename $(DYN_FILE_OS)).pdb,)
	$(if $(DYN_FILE),-@$(RM) $(basename $(DYN_FILE_OS)).ilk,)
	$(if $(DYN_FILE),-@$(RM) $(basename $(DYN_FILE_OS)).def,)
	$(if $(DYN_FILE),-@$(RM) $(basename $(DYN_FILE_OS)).exp,)
	$(if $(DYN_FILE),$(if $(wildcard $(DYN_DIR)/*.*),,-@$(RDP) $(DYN_DIR_OS)),)
	$(if $(IMP_FILE),-@$(RM) $(basename $(IMP_FILE_OS)).exp,)
	$(if $(IMP_FILE),$(if $(wildcard $(IMP_DIR)/*.*),,-@$(RDP) $(IMP_DIR_OS)),)

endif

ifeq ($(HB_SHELL),dos)

# NOTE: MS-DOS command line length has a limit of 126 characters.
#       When using DJGPP GNU Make to invoke other DJGPP tools this limit
#       is about 13KB, as they do special trick to overcome it.
#       See these DJGPP FAQs:
#          http://www.delorie.com/djgpp/v2faq/faq16_4.html
#          http://www.delorie.com/djgpp/v2faq/faq16_5.html
#       [vszakats]

MK := $(subst \,/,$(MAKE))
RM := $(TOOL_DIR)dosrm -f
RDP := $(TOOL_DIR)dosrm -fr
CP := $(TOOL_DIR)doscp -f
LN :=
MD := $(TOOL_DIR)dosmkdir
MDP := $(TOOL_DIR)dosmkdir -p
ECHO := $(TOOL_DIR)dosecho
ECHOQUOTE := "
TRUE := $(ECHO) > nul
# TODO
FALSE := $(TRUE)

dirbase::
	-@$(MDP) $(OBJ_DIR_OS)
	$(if $(LIB_FILE),-@$(MDP) $(LIB_DIR_OS),)
	$(if $(BIN_FILE),-@$(MDP) $(BIN_DIR_OS),)
	$(if $(DYN_FILE),-@$(MDP) $(DYN_DIR_OS),)
	$(if $(IMP_FILE),-@$(MDP) $(IMP_DIR_OS),)

clean::
	-@$(RDP) $(PKG_DIR_OS) $(OBJ_DIR_OS) $(LIB_FILE_OS) $(BIN_FILE_OS) $(DYN_FILE_OS) $(IMP_FILE_OS)
	$(if $(LIB_FILE),-@$(RM) $(basename $(LIB_FILE_OS)).bak,)
	$(if $(LIB_FILE),$(if $(wildcard $(LIB_DIR)/*.*),,-@$(RDP) $(LIB_DIR_OS)),)
	$(if $(BIN_FILE),-@$(RM) $(basename $(BIN_FILE_OS)).tds,)
	$(if $(BIN_FILE),-@$(RM) $(basename $(BIN_FILE_OS)).pch,)
	$(if $(BIN_FILE),-@$(RM) $(basename $(BIN_FILE_OS)).pdb,)
	$(if $(BIN_FILE),-@$(RM) $(basename $(BIN_FILE_OS)).ilk,)
	$(if $(BIN_FILE),$(if $(wildcard $(BIN_DIR)/*.*),,-@$(RDP) $(BIN_DIR_OS)),)
	$(if $(DYN_FILE),-@$(RM) $(basename $(DYN_FILE_OS)).tds,)
	$(if $(DYN_FILE),-@$(RM) $(basename $(DYN_FILE_OS)).pch,)
	$(if $(DYN_FILE),-@$(RM) $(basename $(DYN_FILE_OS)).pdb,)
	$(if $(DYN_FILE),-@$(RM) $(basename $(DYN_FILE_OS)).ilk,)
	$(if $(DYN_FILE),-@$(RM) $(basename $(DYN_FILE_OS)).def,)
	$(if $(DYN_FILE),-@$(RM) $(basename $(DYN_FILE_OS)).exp,)
	$(if $(DYN_FILE),$(if $(wildcard $(DYN_DIR)/*.*),,-@$(RDP) $(DYN_DIR_OS)),)
	$(if $(IMP_FILE),-@$(RM) $(basename $(IMP_FILE_OS)).exp,)
	$(if $(IMP_FILE),$(if $(wildcard $(IMP_DIR)/*.*),,-@$(RDP) $(IMP_DIR_OS)),)

endif
