#
# $Id$
#

include $(TOP)$(ROOT)config/global.mk

ifneq ($(HB_PLATFORM),)
ifneq ($(HB_COMPILER),)

ifeq ($(HB_HOST_PLAT),dos)
   # do not use rules for parallel processing in MS-DOS
   JOB_SRV := NO
else
   # these make versions does not work correctly with
   # parallel execution rules below
   JOB_SRV := $(MAKE_VERSION:3.7%=NO)
   JOB_SRV := $(JOB_SRV:3.80%=NO)
endif

ifeq ($(JOB_SRV),NO)

   DIRS := $(filter-out {%},$(subst {, {,$(DIRS)))
   include $(TOP)$(ROOT)config/dirsh.mk

else

   # NOTE: The empty line directly before 'endef' HAVE TO exist!
   #       It causes that every command will be separated by LF
   define dir_mk
      @$(MK) $(MKFLAGS) -C $(dir) $@

   endef

   DIRS_PURE := $(filter-out {%},$(subst {, {,$(DIRS)))
   DIRS_DEP  := $(filter-out $(DIRS_PURE),$(DIRS))
   DIRS_MK   := $(foreach d, $(DIRS_PURE), $(if $(wildcard $(d)/Makefile),$(d),))
   DIR_RULE  = $(foreach dir, $(DIRS_MK), $(dir_mk))

endif

all : first

ifeq ($(JOB_SRV),NO)

first clean install::
	+$(DIR_RULE)

else

DIRS_CLEAN := $(foreach dir, $(DIRS_MK), $(dir).clean)
DIRS_INST  := $(foreach dir, $(DIRS_MK), $(dir).inst)

ifneq ($(_HB_BLD),yes)
first   :: $(DIRS_MK)
endif
install :: $(DIRS_INST)
clean   :: $(DIRS_CLEAN)

comma := ,
define dep_rule
   $(subst $(comma),$(2) ,$(subst },$(2),$(subst {,$(2)::|,$(1))))
endef

$(foreach dep, $(DIRS_DEP), $(eval $(call dep_rule,$(dep),.clean)))
$(foreach dep, $(DIRS_DEP), $(eval $(call dep_rule,$(dep),.inst)))
$(foreach dep, $(DIRS_DEP), $(eval $(call dep_rule,$(dep),)))

$(DIRS_CLEAN) ::
	+@$(MK) $(MKFLAGS) -C $(@:.clean=) clean

$(DIRS_INST) ::
	+@$(MK) $(MKFLAGS) -C $(@:.inst=) install _HB_BLD=yes

$(DIRS_MK) ::
	+@$(MK) $(MKFLAGS) -C $(@)

endif

endif
endif
