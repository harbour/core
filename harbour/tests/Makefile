#
# $Id$
#

ifeq ($(HB_MAIN),)
HB_MAIN = std
endif

ROOT = ../

LIBS=\
        tools \
        debug \
        rtl \
        rdd \
        vm \
        rdd \
        rtl \
        pp \
        runner \
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
        clasinit.prg \
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
        dates4.prg \
        db_brows.prg \
        debugtst.prg \
        devtest.prg \
        dirtest.prg \
        disptest.prg \
        docase.prg \
        dosshell.prg \
        dynobj.prg \
        dynsym.prg \
        exittest.prg \
        fib.prg \
        fornext.prg \
        fortest.prg \
        funcarr.prg \
        hardcr.prg \
        hello.prg \
        ifelse.prg \
        ifinline.prg \
        inherit.prg \
        inifiles.prg \
        initexit.prg \
        inkeytst.prg \
        inline.prg \
        iotest.prg \
        iotest2.prg \
        longdev.prg \
        longstr.prg \
        longstr2.prg \
        mathtest.prg \
        memfile.prg \
        memory.prg \
        memvar.prg \
        menutest.prg \
        mtran.prg \
        multiarg.prg \
        nums.prg \
        objarr.prg \
        objasign.prg \
        objects.prg \
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
        rtfclass.prg \
        rtl_test.prg \
        say.prg \
        scroll.prg \
        seconds.prg \
        set_num.prg \
        set_test.prg \
        sound.prg \
        statinit.prg \
        statfun.prg \
        statics.prg \
        strdelim.prg \
        strings.prg \
        strings2.prg \
        strings3.prg \
        strip.prg \
        t1.prg \
        test.prg \
        test_all.prg \
        testbrdb.prg \
        testbrw.prg \
        testcgi.prg \
        testdbf.prg \
        testerro.prg \
        testfor.prg \
        testgt.prg \
        testhbf.prg \
        testhtml.prg \
        testinc.prg \
        testmem.prg \
        teststr.prg \
        testtok.prg \
        testpre.prg \
        testread.prg \
        testrdd.prg \
        testvars.prg \
        testwarn.prg \
        tstalias.prg \
        tstcolor.prg \
        version.prg \
        while.prg \

PRG_HEADERS=\
        db_brows.ch \
        cgi.ch \
        keywords.ch \
        test.ch \

BAD_PRG_SOURCES=\
        alias.prg \
        clasname.prg \
        dupvars.prg \
        extend1.prg \
        keywords.prg \
        linecont.prg \
        lnlenli1.prg \
        lnlenli2.prg \
        setkeys.prg \
        spawn.prg \
        spawn2.prg \
        statics1.prg \
        statics2.prg \
        test10.prg \
        testid.prg \
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
