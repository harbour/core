#
# $Id$
#

# How to run yacc.
YACC := bison
YACC_FLAGS := -d $(YACC_FLAGS)

ifeq ($(HB_BUILD_DEBUG),yes)
   HB_CFLAGS := -DHB_TR_LEVEL_DEBUG $(HB_CFLAGS)
else
   ifneq ($(HB_COMPILER),pcc)
      HB_PRGFLAGS := -l $(HB_PRGFLAGS)
   endif
endif

# Handle it here, so that it can be disabled for individual libs
ifeq ($(HB_PLATFORM),win)
   ifneq ($(__HB_BUILD_WINUNI),no)
      HB_CFLAGS += -DUNICODE
   endif
endif

ifeq ($(HB_DYN_COPT),)
   OBJ_DYN_POSTFIX :=
else
   OBJ_DYN_POSTFIX := _dyn
endif

# How to run Harbour
HB := $(HB_HOST_BIN_DIR)/harbour$(HB_HOST_BIN_EXT)
HB_FLAGS := -n1 -q0 -w3 -es2 -kmo -i- $(HB_PRGFLAGS)
HB_RULE = $(HB) $? $(HB_INC_DEPEND) -i$(HB_HOST_INC) $(HB_FLAGS) $(HB_USER_PRGFLAGS)

# Use default rules if platform/compiler specific rule is not defined

CC_FLAGS := $(HB_INC_DEPEND) $(CFLAGS) $(HB_CFLAGS)

# The rule to compile a C source file.
ifeq ($(CC_RULE),)
   CC_RULE = $(CC) $(subst $(CC_DIRSEPFROM),$(CC_DIRSEPTO),$(CC_FLAGS) $(HB_USER_CFLAGS) $(CC_OUT)$(<F:.c=$(OBJ_EXT)) $(HB_CFLAGS_STA) $(CC_IN) $<)
   ifneq ($(HB_BUILD_DYN),no)
      ifneq ($(HB_DYN_COPT),)
         ifneq ($(LIBNAME),)
            ifneq ($(filter $(LIBNAME),$(HB_DYN_LIBS)),)
               define cc_comp_all
                  $(CC) $(subst $(CC_DIRSEPFROM),$(CC_DIRSEPTO),$(CC_FLAGS) $(HB_USER_CFLAGS) $(CC_OUT)$(<F:.c=$(OBJ_EXT)) $(HB_CFLAGS_STA) $(CC_IN) $<)
                  $(CC) $(subst $(CC_DIRSEPFROM),$(CC_DIRSEPTO),$(CC_FLAGS) $(HB_USER_CFLAGS) $(CC_OUT)$(<F:.c=$(OBJ_DYN_POSTFIX)$(OBJ_EXT)) $(HB_DYN_COPT) $(HB_CFLAGS_DYN) $(CC_IN) $<)
               endef
               CC_RULE = $(cc_comp_all)
            endif
         endif
      endif
   endif
endif

ifeq ($(S_RULE),)
   S_RULE = $(CC) $(subst $(CC_DIRSEPFROM),$(CC_DIRSEPTO),$(CC_FLAGS) $(HB_USER_CFLAGS) $(CC_OUT)$(<F:.s=$(OBJ_EXT)) $(HB_CFLAGS_STA) $(CC_IN) $<)
endif
ifeq ($(SX_RULE),)
   SX_RULE = $(CC) $(subst $(CC_DIRSEPFROM),$(CC_DIRSEPTO),$(CC_FLAGS) $(HB_USER_CFLAGS) $(CC_OUT)$(<F:.sx=$(OBJ_EXT)) $(HB_CFLAGS_STA) -x assembler-with-cpp $(CC_IN) $<)
endif

# The rule to compile a C++ source file.
ifeq ($(CPP_RULE),)
   ifeq ($(CXX),)
      CXX := $(CC)
   endif
   CPP_RULE = $(CXX) $(CC_FLAGS) $(HB_USER_CFLAGS) $(CC_OUT)$(<F:.cpp=$(OBJ_EXT)) $(CC_IN) $<
endif

# The rule to compile an Objective C source file.
ifeq ($(OBJC_RULE),)
   OBJC := $(CC)
   OBJC_RULE = $(OBJC) $(CC_FLAGS) $(HB_USER_CFLAGS) $(CC_OUT)$(<F:.m=$(OBJ_EXT)) $(CC_IN) $<
endif

# The rule to compile resources.
ifneq ($(RC),)
   ifeq ($(RC_RULE),)
      RC_RULE = $(RC) $(RCFLAGS) $(HB_RCFLAGS) $(HB_USER_RESFLAGS) $(RC_OUT)$(<F:.rc=$(RES_EXT)) $<
   endif
endif

# The rule to link an executable.
ifeq ($(LD_RULE),)
   LD_RULE = $(LD) $(LDFLAGS) $(HB_LDFLAGS) $(HB_USER_LDFLAGS) $(LD_OUT)$(subst /,$(DIRSEP),$(BIN_DIR)/$@) $(^F) $(LDLIBS) $(LDSTRIP)
endif

# Eliminate these rules.
%.c : %.y

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

# Rules for Objective C files
%$(OBJ_EXT) : $(GRANDP)%.m
	$(OBJC_RULE)

%$(OBJ_EXT) : %.m
	$(OBJC_RULE)

# Rules for resource files
%$(RES_EXT) : $(GRANDP)%.rc
	$(RC_RULE)

# Rule to generate an object file from a assembler .s file.
%$(OBJ_EXT) : $(GRANDP)%.s
	$(S_RULE)

%$(OBJ_EXT) : %.s
	$(S_RULE)

# Rule to generate an object file from a assembler .sx file.
%$(OBJ_EXT) : $(GRANDP)%.sx
	$(SX_RULE)

%$(OBJ_EXT) : %.sx
	$(SX_RULE)

%$(RES_EXT) : %.rc
	$(RC_RULE)

_RES := no
ifneq ($(RES_EXT),)
   ifneq ($(ALL_RC_OBJS),)
      _RES := yes
   endif
endif

# Rule to generate an executable file from an object file.
ifeq ($(_RES),yes)
%$(BIN_EXT) : %$(OBJ_EXT) %$(RES_EXT)
	$(LD_RULE)
else
%$(BIN_EXT) : %$(OBJ_EXT)
	$(LD_RULE)
endif

# Rule to generate a C file from a PRG file.
%.c : $(GRANDP)%.prg
	$(HB_RULE)
