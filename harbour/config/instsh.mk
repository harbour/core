#
# $Id$
#

ifeq ($(INSTALL_FILES),) # Empty install list
   INSTALL_RULE := @$(ECHO) $(ECHOQUOTE)! Nothing to install$(ECHOQUOTE)
else
ifeq ($(INSTALL_DIR),) # Empty install dir
   INSTALL_RULE := @$(ECHO) $(ECHOQUOTE)! Can't install, install dir isn't set$(ECHOQUOTE)
else

ifneq ($(HB_SHELL),sh)
   INSTALL_DIR_OS := $(subst /,\,$(INSTALL_DIR))
   INSTALL_FILES_OS := $(subst /,\,$(INSTALL_FILES))
endif

ifeq ($(HB_SHELL),sh)

   INSTALL_RULE := \
      @$(MDP) $(subst \,/,$(INSTALL_DIR)); \
      if [ ! -d "$(subst \,/,$(INSTALL_DIR))" ]; \
      then \
         $(ECHO) "! Can't install, path not found: '$(subst \,/,$(INSTALL_DIR))'" 1>&2; \
         false; \
      else \
         for i in $(INSTALL_FILES); \
         do \
            if [ -r "$$i" ]; \
            then \
               $(ECHO) "! Installing $$i on $(subst \,/,$(INSTALL_DIR))"; \
               $(CP) $$i $(subst \,/,$(INSTALL_DIR)); \
               true; \
            else \
               $(ECHO) "! Can't install $$i, not found" 1>&2; \
            fi \
         done \
      fi

endif

ifeq ($(HB_SHELL),nt)

   define inst_file_all
      $(if $(wildcard $(subst $(subst x,x, ),\ ,$(INSTALL_DIR))),,-@$(MDP) "$(INSTALL_DIR_OS)")
      -@for %%f in ($(INSTALL_FILES_OS)) do $(CP) "%%f" "$(INSTALL_DIR_OS)"
   endef

   INSTALL_RULE := $(inst_file_all)

endif

ifeq ($(HB_SHELL),os2)

   # We have to use script to overcome the max command size limit

   # 05/Aug/2009 - <maurilio.longo@libero.it>
   #               I don't like doing the "if not exist..." test for every file copied
   #               but I could not find any other way to make this work around the shell
   #               command line length limit.
   #               'dos' method causes freeze on OS/2 (with or without COMPSPEC). [vszakats]
   #               $(COMSPEC) /C is needed to overcome freezes. [vszakats]

   # NOTE: The empty line directly before 'endef' HAVE TO exist!
   #       It causes that every commands will be separated by LF
   define inst_file
      $(if $(wildcard $(INSTALL_DIR)),,-@$(MDP) $(INSTALL_DIR))
      -@$(COMSPEC) /C $(CP) $(file) $(INSTALL_DIR_OS)

   endef

   INSTALL_RULE := $(foreach file,$(INSTALL_FILES_OS),$(inst_file))

endif

ifeq ($(HB_SHELL),dos)

   define inst_file_all
      -@$(MDP) $(INSTALL_DIR_OS)
      $(foreach file,$(INSTALL_FILES_OS),$(inst_file))
   endef

   # We have to use script to overcome the DOS limit of max 128 characters
   # NOTE: The empty line directly before 'endef' HAVE TO exist!
   #       It causes that every command will be separated by LF
   define inst_file
      -@$(CP) $(file) $(INSTALL_DIR_OS)

   endef

   INSTALL_RULE := $(inst_file_all)

endif

endif # Empty install dir

endif # Empty install list
