#
# $Id$
#

MOC_CPP_SOURCES := $(foreach dir,$(MOC_HEADERS),moc_$(dir:.h=.cpp))

$(MOC_CPP_SOURCES) : moc_%.cpp : $(GRANDP)%.h

CPP_SOURCES += $(MOC_CPP_SOURCES)

moc_%.cpp : $(GRANDP)%.h
	$(MOC_BIN) $? -o $@
