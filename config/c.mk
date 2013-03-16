
C_OBJS := $(C_SOURCES:.c=$(OBJ_EXT)) $(CPP_SOURCES:.cpp=$(OBJ_EXT)) $(OBJC_SOURCES:.m=$(OBJ_EXT))
S_OBJS := $(S_SOURCES:.s=$(OBJ_EXT)) $(SX_SOURCES:.sx=$(OBJ_EXT))

C_MAIN_OBJ := $(C_MAIN:.c=$(OBJ_EXT))

ifdef YACC_SOURCE
   YACC_BASE := $(YACC_SOURCE:.y=)
   YACC_C := $(YACC_BASE)y.c
   YACC_H_TMP := $(YACC_C:.c=.h)
   YACC_H := y_tab.h
   YACC_OUTPUT := $(YACC_C:.c=.out)
   YACC_OBJ := $(YACC_C:.c=$(OBJ_EXT))
   YACC_HB_H := $(foreach h, $(YACC_HEADERS), $(HB_HOST_INC)/$(h))
   ifneq ($(YACC_DEPEND),)
      $(foreach f, $(YACC_DEPEND), $(f:.c=$(OBJ_EXT))) : $(YACC_C)
   endif
endif

ALL_C_OBJS := $(YACC_OBJ) $(C_OBJS) $(C_MAIN_OBJ) $(S_OBJS)

$(YACC_OBJ) : $(YACC_C)

ifeq ($(HB_REBUILD_PARSER),yes)
$(YACC_C) : $(GRANDP)$(YACC_SOURCE) $(YACC_HB_H)
	$(YACC) $(YACC_FLAGS) -o$@ $<
else
%y.c : $(GRANDP)%.yyc
	$(CP) $(subst /,$(DIRSEP),$<) $@
	$(CP) $(subst /,$(DIRSEP),$(<:.yyc=.yyh)) $(@:.c=.h)
endif
