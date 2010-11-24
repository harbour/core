#
# $Id$
#

INSTALL_RULE :=

INSTALL_FILES := $(strip $(INSTALL_FILES))
INSTALL_DIR := $(strip $(INSTALL_DIR))

ifneq ($(INSTALL_FILES),) # Empty install list

ifeq ($(INSTALL_DIR),) # Empty install dir
   INSTALL_RULE := @$(ECHO) $(ECHOQUOTE)! Can't install, install dir isn't set$(ECHOQUOTE)
else

_SAME_DIR :=
# Check if $(abspath)/$(realpath) functions are supported
ifneq ($(abspath .),)
   ifeq ($(realpath $(INSTALL_DIR)),$(realpath .))
      _SAME_DIR := yes
   endif
endif

ifeq ($(_SAME_DIR),yes)
   INSTALL_RULE := @$(ECHO) $(ECHOQUOTE)! Skip install, destination dir '$(INSTALL_DIR)' is the same as source$(ECHOQUOTE)
else

ifneq ($(HB_SHELL),sh)
   ifneq ($(HB_SHELL),os2)
      INSTALL_DIR_OS := $(subst /,\,$(INSTALL_DIR))
      INSTALL_FILES_OS := $(subst /,\,$(INSTALL_FILES))
   else
      # $(CP) and $(MDP) require forward slashes
      INSTALL_DIR_OS := $(subst \,/,$(INSTALL_DIR))
      INSTALL_FILES_OS := $(subst \,/,$(INSTALL_FILES))
   endif
else
   INSTALL_DIR_OS := $(subst \,/,$(INSTALL_DIR))
endif

ifeq ($(HB_SHELL),sh)

   INSTALL_RULE := \
      @$(MDP) $(INSTALL_DIR_OS); \
      if [ ! -d "$(INSTALL_DIR_OS)" ]; \
      then \
         $(ECHO) "! Can't install, path not found: '$(INSTALL_DIR_OS)'" 1>&2; \
         $(FALSE); \
      else \
         for i in $(INSTALL_FILES); \
         do \
            if [ -r "$$i" ]; \
            then \
               $(ECHO) "! Installing $$i on $(INSTALL_DIR_OS)"; \
               $(CP) $$i $(INSTALL_DIR_OS); \
               true; \
            else \
               $(ECHO) "! Can't install $$i, not found" 1>&2; \
            fi \
         done \
      fi

endif

ifeq ($(HB_SHELL),nt)

   define inst_file_all
      -@if not exist "$(INSTALL_DIR_OS)" $(MDP) "$(INSTALL_DIR_OS)"
      -@for %%f in ($(INSTALL_FILES_OS)) do $(CP) "%%f" "$(INSTALL_DIR_OS)"
   endef

   INSTALL_RULE := $(inst_file_all)

endif

ifeq ($(HB_SHELL),os2)

   define inst_file_all
      -@$(MDP) $(INSTALL_DIR_OS)
      $(foreach file,$(INSTALL_FILES_OS),$(inst_file))
   endef

   # NOTE: The empty line directly before 'endef' HAVE TO exist!
   #       It causes that every command will be separated by LF
   define inst_file
      -@$(CP) $(file) $(INSTALL_DIR_OS)

   endef

   INSTALL_RULE := $(inst_file_all)

endif

ifeq ($(HB_SHELL),dos)

   define inst_file_all
      -@$(MDP) $(INSTALL_DIR_OS)
      $(foreach file,$(INSTALL_FILES_OS),$(inst_file))
   endef

   # NOTE: The empty line directly before 'endef' HAVE TO exist!
   #       It causes that every command will be separated by LF
   define inst_file
      -@$(CP) $(file) $(INSTALL_DIR_OS)

   endef

   INSTALL_RULE := $(inst_file_all)

endif

endif # Source and destination directories are equal

endif # Empty install dir

endif # Empty install list
