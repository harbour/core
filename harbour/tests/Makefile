#
# $Id$
#

ifeq ($(HB_MAIN),)
HB_MAIN = std
endif

ROOT = ../

CONTRIBS=\
	libmisc\

LIBS=\
	debug \
	vm \
	rtl \
	lang \
	rdd \
	rtl \
	vm \
	macro \
	pp \
	common \

ifeq ($(PM),)
  PM := $(pm)
endif

ifeq ($(PM),) # PM not defined = build all files

PRG_SOURCES=\
	ac_test.prg \
	adirtest.prg \
	ainstest.prg \
	and_or.prg \
	array16.prg \
	arrayidx.prg \
	arrays.prg \
	arreval.prg \
	arrindex.prg \
	atest.prg \
	begin.prg \
	box.prg \
	boxtest.prg \
	byref.prg \
	calling.prg \
	cdow.prg \
	clasname.prg \
	classch.prg \
	classes.prg \
	clsdata.prg \
	cmphello.prg \
	codebl.prg \
	codebloc.prg \
	comments.prg \
	curdirt.prg \
	cursrtst.prg \
	dates.prg \
	dates2.prg \
	dates3.prg \
	db_brows.prg \
	dbevalts.prg \
	delimtst.prg \
	devtest.prg \
	dirtest.prg \
	disptest.prg \
	docase.prg \
	dosshell.prg \
	dynsym.prg \
	exittest.prg \
	fib.prg \
	fornext.prg \
	fortest.prg \
	fsplit.prg \
	hello.prg \
	ifelse.prg \
	ifinline.prg \
	initexit.prg \
	inkeytst.prg \
	inline.prg \
	iotest.prg \
	iotest2.prg \
	langapi.prg \
	lnlenli2.prg \
	longdev.prg \
	longstr.prg \
	longstr2.prg \
	memfile.prg \
	memory.prg \
	memvar.prg \
	menutest.prg \
	mousetst.prg \
	multiarg.prg \
	nums.prg \
	objects.prg \
	onidle.prg \
	os.prg \
	output.prg \
	overload.prg \
	parexpr.prg \
	passref.prg \
	procline.prg \
	procname.prg \
	readfile.prg \
	readhrb.prg \
	recursiv.prg \
	returns.prg \
	round.prg \
	say.prg \
	scroll.prg \
	seconds.prg \
	setkeys.prg \
	set_num.prg \
	set_test.prg \
	sound.prg \
	statinit.prg \
	statfun.prg \
	statics.prg \
	strdelim.prg \
	t1.prg \
	test_all.prg \
	testbrdb.prg \
	testbrw.prg \
	testcdx.prg \
	testcgi.prg \
	testdbf.prg \
	testdecl.prg \
	testerro.prg \
	testfor.prg \
	testhtml.prg \
	testid.prg \
	testinc.prg \
	testmem.prg \
	testntx.prg \
	testpre.prg \
	testrdd.prg \
	testread.prg \
	teststr.prg \
	testvars.prg \
	testwarn.prg \
	tstalias.prg \
	tstasort.prg \
	tstcolor.prg \
	tstdbi.prg \
	tstdspac.prg \
	version.prg \
	vmasort.prg \
	while.prg \
	newrdd.prg \

PRG_HEADERS=\
	db_brows.ch \
	cgi.ch \
	keywords.ch \
	test.ch \

BAD_PRG_SOURCES=\
	alias.prg \
	clasinit.prg \
	dates4.prg \
	debugtst.prg \
	dupvars.prg \
	dynobj.prg \
	extend1.prg \
	funcarr.prg \
	inherit.prg \
	inifiles.prg \
	keywords.prg \
	linecont.prg \
	lnlenli1.prg \
	mathtest.prg \
	objarr.prg \
	objasign.prg \
	rtfclass.prg \
	spawn.prg \
	spawn2.prg \
	statics1.prg \
	statics2.prg \
	stripem.prg \
	test.prg \
	test10.prg \
	testgt.prg \
	testhbf.prg \
	testtok.prg \
	tstprag.prg \
	twirl.prg \
	vec1.prg \

C_SOURCES=\

C_HEADERS=\

BAD_C_SOURCES=\
	extend2.c \

include $(TOP)$(ROOT)config/test.cf

else #PM defined = build specified file

ifneq ($(findstring .prg,$(PM)),)
  PRG_MAIN := $(PM)
else
  ifneq ($(findstring .PRG,$(PM)),)
    PRG_MAIN := $(PM)
  else
    PRG_MAIN := $(PM).prg
  endif
endif
include $(TOP)$(ROOT)config/bin.cf

endif

