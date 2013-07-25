ifneq ($(HB_SHELL),sh)
   MK_OS := $(subst /,\,$(MK))
   DIRS_OS := $(subst /,\,$(DIRS))
endif

ifeq ($(DIRS),) # Empty directory list
   DIR_RULE := @$(ECHO) $(ECHOQUOTE)! Done$(ECHOQUOTE)
else

ifeq ($(HB_SHELL),sh)
   DIR_RULE = \
      @for d in $(DIRS); do \
         if [ -d "$$d" ]; then \
            $(MK) $(MKFLAGS) -C $$d $@; \
         fi \
      done
endif

ifeq ($(HB_SHELL),nt)
   DIR_RULE = @for %%d in ($(DIRS_OS)) do $(MK_OS) $(MKFLAGS) -C %%d $@
endif

ifeq ($(HB_SHELL),os2)
   DIR_RULE = @for %d in ($(DIRS_OS)) do $(MK_OS) $(MKFLAGS) -C %d $@
endif

ifeq ($(HB_SHELL),dos)

# NOTE: The empty line directly before 'endef' HAVE TO exist!
#       It causes that every command will be separated by LF
define dir_mk
   @$(MK_OS) $(MKFLAGS) -C $(file) $@

endef

DIR_RULE = $(foreach file,$(DIRS_OS),$(dir_mk))

endif

endif # ! Empty directory list
