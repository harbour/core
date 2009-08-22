#
# $Id$
#

# How to run yacc.
YACC := bison
YACC_FLAGS := -d $(YACC_FLAGS)

# How to run lex.
LEX := flex
LEX_FLAGS := -i -8 $(LEX_FLAGS)

ifeq ($(HB_BUILD_DEBUG),yes)
   HB_CFLAGS := -DHB_TR_LEVEL_DEBUG $(HB_CFLAGS)
else
   HB_PRGFLAGS := -l $(HB_PRGFLAGS)
endif

ifeq ($(HB_BUILD_UNICODE),yes)
   HB_CFLAGS := -DUNICODE $(HB_CFLAGS)
endif

# How to run Harbour
HB := $(HB_HOST_BIN_DIR)/harbour$(HB_HOST_BIN_EXT)
HB_FLAGS := -n1 $(HB_INC_DEPEND) -i$(HB_INC_COMPILE) -q0 -w3 -es2 -kmo $(HB_PRGFLAGS)
HB_RULE = $(HB) $? $(HB_FLAGS) $(HB_USER_PRGFLAGS)

# Use default rules if architecture/compiler specific rule is not defined

CC_FLAGS := $(HB_INC_DEPEND) $(CPPFLAGS) $(CFLAGS) $(HB_CFLAGS)

# The rule to compile a C source file.
ifeq ($(CC_RULE),)
   CC_RULE = $(CC) $(CC_FLAGS) $(HB_USER_CFLAGS) $(CC_OUT)$(<F:.c=$(OBJ_EXT)) $(CC_IN) $<
   ifneq ($(HB_BUILD_DLL),no)
      ifneq ($(HB_DYN_COPT),)
         ifneq ($(LIBNAME),)
            ifneq ($(filter $(LIBNAME),$(HB_DYN_LIBS)),)
               define cc_comp_all
                  $(CC) $(CC_FLAGS) $(HB_USER_CFLAGS) $(CC_OUT)$(<F:.c=$(OBJ_EXT)) $(CC_IN) $<
                  $(CC) $(CC_FLAGS) $(HB_USER_CFLAGS) $(CC_OUT)$(<F:.c=$(OBJ_DYN_POSTFIX)$(OBJ_EXT)) $(HB_DYN_COPT) $(CC_IN) $<
               endef
               CC_RULE = $(cc_comp_all)
            endif
         endif
      endif
   endif
endif

# The rule to compile a C++ source file.
ifeq ($(CPP_RULE),)
   CPP_RULE = $(CC) $(CC_FLAGS) $(HB_USER_CFLAGS) $(CC_OUT)$(<F:.cpp=$(OBJ_EXT)) $(CC_IN) $<
endif

# The rule to link an executable.
ifeq ($(LD_RULE),)
   LD_RULE = $(LD) $(CFLAGS) $(LD_OUT)$(subst /,$(DIRSEP),$(BIN_DIR)/$@) $(^F) $(LDFLAGS) $(HB_USER_LDFLAGS) $(LDLIBS)
endif

# Eliminate these rules.
%.c : %.y

%.c : %.l

# Rule to generate an object file from a C source file in the parent.
%$(OBJ_EXT) : $(GRANDP)%.c
	$(CC_RULE)

# Rule to generate an object file from a C source file.
%$(OBJ_EXT) : %.c
	$(CC_RULE)

# Rules for CPP files
%$(OBJ_EXT) : $(GRANDP)%.cpp
	$(CPP_RULE)

%$(OBJ_EXT) : %.cpp
	$(CPP_RULE)

# Rule to generate an executable file from an object file.
%$(BIN_EXT) : %$(OBJ_EXT)
	$(LD_RULE)

# Rule to generate a C file from a PRG file.
%.c : $(GRANDP)%.prg
	$(HB_RULE)
