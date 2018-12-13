all : first

BIN_EXT :=
DYN_EXT := .dylib
DYN_PREF := lib

HB_GT_LIBS += gttrm

ifneq ($(filter $(HB_BUILD_STRIP),all lib),)
   ARSTRIP = && strip -S $(LIB_DIR)/$@
endif
ifneq ($(filter $(HB_BUILD_STRIP),all bin),)
   LDSTRIP = && strip $(BIN_DIR)/$@
   DYSTRIP = && strip -S $(DYN_DIR)/$@
endif

AR := libtool
AR_RULE = ( $(AR) -static \
   -no_warning_for_no_symbols $(ARFLAGS) $(HB_AFLAGS) $(HB_USER_AFLAGS) \
   -o $(LIB_DIR)/$@ $(^F) $(ARSTRIP) ) \
   || ( $(RM) $(LIB_DIR)/$@ && $(FALSE) )

DY_OUT := -o$(subst x,x, )

DY_RULE = $(DY) -dynamiclib -flat_namespace $(DFLAGS) \
   -install_name "$(DYN_NAME_NVR)" \
   -compatibility_version $(HB_VER_MAJOR).$(HB_VER_MINOR) \
   -current_version $(HB_VER_MAJOR).$(HB_VER_MINOR).$(HB_VER_RELEASE) \
   $(HB_USER_DFLAGS) $(DY_OUT)$(DYN_DIR)/$@ $^ $(DLIBS) $(DYSTRIP) \
   && $(LN) $(@F) $(DYN_FILE_NVR) \
   && $(LN) $(@F) $(DYN_FILE_CPT)
