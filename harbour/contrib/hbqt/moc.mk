#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
# See COPYING for licensing terms.
# ---------------------------------------------------------------

MOC_CPP_SOURCES := $(foreach dir,$(MOC_HEADERS),moc_$(dir:.h=.cpp))

$(MOC_CPP_SOURCES) : moc_%.cpp : $(GRANDP)%.h

CPP_SOURCES += $(MOC_CPP_SOURCES)

moc_%.cpp : $(GRANDP)%.h
	$(MOC_BIN) $? -o $@
