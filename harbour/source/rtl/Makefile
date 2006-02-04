#
# $Id$
#

ROOT = ../../

C_SOURCES=\
    abs.c        \
    accept.c     \
    ampm.c       \
    at.c         \
    binnum.c     \
    binnumx.c    \
    box.c        \
    cdpapi.c     \
    chrasc.c     \
    colorind.c   \
    console.c    \
    copyfile.c   \
    datec.c      \
    dates.c      \
    dateshb.c    \
    datesx.c     \
    defpath.c    \
    descend.c    \
    dirdrive.c   \
    direct.c     \
    diskspac.c   \
    disksphb.c   \
    do.c         \
    empty.c      \
    errorapi.c   \
    errorint.c   \
    file.c       \
    filehb.c     \
    filesys.c    \
    fkmax.c      \
    fnsplit.c    \
    fserror.c    \
    fssize.c     \
    fstemp.c     \
    gete.c       \
    gt.c         \
    gtapi.c      \
    gtsys.c      \
    gtfunc.c     \
    gx.c         \
    hardcr.c     \
    hbffind.c    \
    hbgtcore.c   \
    hbrandom.c   \
    idle.c       \
    inkey.c      \
    is.c         \
    isprint.c    \
    langapi.c    \
    left.c       \
    len.c        \
    lennum.c     \
    math.c       \
    maxrow.c     \
    memofile.c   \
    memoline.c   \
    minmax.c     \
    mlcount.c    \
    mlctopos.c   \
    mlpos.c      \
    mod.c        \
    mouseapi.c   \
    mousex.c     \
    mpostolc.c   \
    mtran.c      \
    natmsg.c     \
    net.c        \
    oemansi.c    \
    oldbox.c     \
    oldclear.c   \
    pad.c        \
    padc.c       \
    padl.c       \
    padr.c       \
    philes.c     \
    philes53.c   \
    philesx.c    \
    rat.c        \
    replic.c     \
    right.c      \
    round.c      \
    run.c        \
    samples.c    \
    saverest.c   \
    scroll.c     \
    seconds.c    \
    set.c        \
    setcolor.c   \
    setcurs.c    \
    setkey.c     \
    setpos.c     \
    setposbs.c   \
    shadow.c     \
    soundex.c    \
    space.c      \
    spfiles.c    \
    str.c        \
    strcase.c    \
    strings.c    \
    strmatch.c   \
    strpeek.c    \
    strtran.c    \
    strzero.c    \
    stuff.c      \
    substr.c     \
    teditorl.c   \
    tone.c       \
    trace.c      \
    transfrm.c   \
    trim.c       \
    type.c       \
    val.c        \
    valtostr.c   \
    valtype.c    \
    version.c    \
    word.c       \
    xhelp.c      \
    xsavescr.c   \

PRG_SOURCES=\
    achoice.prg  \
    adir.prg     \
    altd.prg     \
    alert.prg    \
    browdb.prg   \
    browdbx.prg  \
    browse.prg   \
    checkbox.prg \
    color53.prg  \
    dbdelim.prg  \
    dbedit.prg   \
    dbsdf.prg    \
    devoutp.prg  \
    dircmd.prg   \
    dummy.prg    \
    errorsys.prg \
    fieldbl.prg  \
    getlist.prg  \
    getsys.prg   \
    input.prg    \
    listbox.prg  \
    memoedit.prg \
    memvarbl.prg \
    menuto.prg   \
    mssgline.prg \
    objfunc.prg  \
    perfuncs.prg \
    persist.prg  \
    profiler.prg \
    pushbtn.prg  \
    radiobtn.prg \
    radiogrp.prg \
    readkey.prg  \
    readvar.prg  \
    scrollbr.prg \
    setfunc.prg  \
    setta.prg    \
    symbol.prg   \
    tclass.prg   \
    tbcolumn.prg \
    tbrowse.prg  \
    teditor.prg  \
    terror.prg   \
    text.prg     \
    tget.prg     \
    tgetint.prg  \
    tgetlist.prg \
    tlabel.prg   \
    tmenuitm.prg \
    tobject.prg  \
    tpopup.prg   \
    treport.prg  \
    ttextlin.prg \
    ttopbar.prg  \
    typefile.prg \
    wait.prg     \

LIBNAME=rtl

# The list of all valid GT drivers is defined in config/<arch>/global.cf.
DIRS=$(HB_GT_LIBS)

include $(TOP)$(ROOT)config/lib.cf
include $(TOP)$(ROOT)config/dir.cf

ifneq ($(HB_GT_DEFAULT),)
CFLAGS += -DHB_GT_DEFAULT=$(HB_GT_DEFAULT:gt%=%)
endif
ifneq ($(HB_GT_LIB),)
CFLAGS += -DHB_GT_LIB=$(HB_GT_LIB:gt%=%)
endif
