#
# $Id$
#

ROOT := ../

# contribs _without_ external dependencies
DIRS := \
   hbcomm \
   hbct \
   hbfoxpro \
   hbfship \
   hbgt \
   hbmemio \
   hbmisc \
   hbmzip \
   hbnf \
   hbodbc \
   hbsms \
   hbsqlit3 \
   hbtip \
   hbtpathy \
   hbwin \
   hbxpp \
   hbziparc \
   xhb \

# under testing in make.hbs
#  gtwvg \
#  hbbz2 \
#  hbblink \
#  hbclipsm \
#  hbnetio \
#  rddbmcdx \

# contribs _with_ external dependencies
DIRS += \
   hbcairo \
   hbcups \
   hbcurl \
   hbfbird \
   hbfimage \
   hbgd \
   hbhpdf \
   hbmysql \
   hbpgsql \
   hbssl \
   rddads \
   rddsql \
   sddfb \
   sddmy \
   sddoci \
   sddodbc \
   sddpg \
   sddsqlt3 \

# under testing in make.hbs
#  hbblat \
#  hbqt \
#  gtalleg \

# contribs dependent on above
#DIRS += \
#   hbxbp \

ifeq ($(HB_CONTRIBLIBS),no)
   DIRS :=
else
ifeq ($(firstword $(HB_CONTRIBLIBS)),no)
   DIRS := $(filter-out $(HB_CONTRIBLIBS),$(DIRS))
else
ifneq ($(HB_CONTRIBLIBS),)
   DIRS := $(HB_CONTRIBLIBS)
endif
endif
endif

ifneq ($(DIRS),)
   include $(TOP)$(ROOT)config/dir.mk
else
   include $(TOP)$(ROOT)config/none.mk
endif

clean::
	$(if $(wildcard $(HB_HOST_BIN_DIR)/hbrun$(HB_HOST_BIN_EXT)),+$(HB_HOST_BIN_DIR)/hbrun$(HB_HOST_BIN_EXT) --hb:gtcgi $(TOP)$(ROOT)contrib/make.hbs clean migr,@$(ECHO) $(ECHOQUOTE)! Warning: hbrun not found, contrib/make.hbs skipped.$(ECHOQUOTE))

install::
	$(if $(wildcard $(HB_HOST_BIN_DIR)/hbrun$(HB_HOST_BIN_EXT)),+$(HB_HOST_BIN_DIR)/hbrun$(HB_HOST_BIN_EXT) --hb:gtcgi $(TOP)$(ROOT)contrib/make.hbs install migr,@$(ECHO) $(ECHOQUOTE)! Warning: hbrun not found, contrib/make.hbs skipped.$(ECHOQUOTE))

all:
	$(if $(wildcard $(HB_HOST_BIN_DIR)/hbrun$(HB_HOST_BIN_EXT)),+$(HB_HOST_BIN_DIR)/hbrun$(HB_HOST_BIN_EXT) --hb:gtcgi $(TOP)$(ROOT)contrib/make.hbs all migr,@$(ECHO) $(ECHOQUOTE)! Warning: hbrun not found, contrib/make.hbs skipped.$(ECHOQUOTE))
