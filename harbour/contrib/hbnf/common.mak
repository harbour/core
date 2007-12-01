#
# $Id$
#

LIBNAME = hbnf

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

PRG_HEADERS = \
    ftmenuto.ch \

LIB_OBJS = \
    $(OBJ_DIR)\caplock$(OBJEXT) \
    $(OBJ_DIR)\color2n$(OBJEXT) \
    $(OBJ_DIR)\descend$(OBJEXT) \
    $(OBJ_DIR)\numlock$(OBJEXT) \
    $(OBJ_DIR)\proper$(OBJEXT) \
    $(OBJ_DIR)\shift$(OBJEXT) \
    $(OBJ_DIR)\mouse$(OBJEXT) \
    $(OBJ_DIR)\getvid$(OBJEXT) \
    $(OBJ_DIR)\setkeys$(OBJEXT) \
    $(OBJ_DIR)\setlastk$(OBJEXT) \
    $(OBJ_DIR)\ftisprn$(OBJEXT) \
    $(OBJ_DIR)\ftidle$(OBJEXT) \
    $(OBJ_DIR)\iamidle$(OBJEXT) \
    $(OBJ_DIR)\chdir$(OBJEXT) \
    $(OBJ_DIR)\rmdir$(OBJEXT) \
    $(OBJ_DIR)\alt$(OBJEXT) \
    $(OBJ_DIR)\ctrl$(OBJEXT) \
    $(OBJ_DIR)\getenvrn$(OBJEXT) \
    $(OBJ_DIR)\n2color$(OBJEXT) \
    $(OBJ_DIR)\origin$(OBJEXT) \
    $(OBJ_DIR)\prtscr$(OBJEXT) \
    $(OBJ_DIR)\stod$(OBJEXT) \
    $(OBJ_DIR)\kspeed$(OBJEXT) \
    $(OBJ_DIR)\mkdir$(OBJEXT) \
    $(OBJ_DIR)\getver$(OBJEXT) \
    $(OBJ_DIR)\ftattr$(OBJEXT) \
    $(OBJ_DIR)\dispc$(OBJEXT) \
    $(OBJ_DIR)\fttext$(OBJEXT) \
    $(OBJ_DIR)\ftshadow$(OBJEXT) \
    $(OBJ_DIR)\putkey$(OBJEXT) \
    \
    $(OBJ_DIR)\aading$(OBJEXT) \
    $(OBJ_DIR)\aavg$(OBJEXT) \
    $(OBJ_DIR)\acctadj$(OBJEXT) \
    $(OBJ_DIR)\acctmnth$(OBJEXT) \
    $(OBJ_DIR)\acctqtr$(OBJEXT) \
    $(OBJ_DIR)\acctweek$(OBJEXT) \
    $(OBJ_DIR)\acctyear$(OBJEXT) \
    $(OBJ_DIR)\adessort$(OBJEXT) \
    $(OBJ_DIR)\aemaxlen$(OBJEXT) \
    $(OBJ_DIR)\aeminlen$(OBJEXT) \
    $(OBJ_DIR)\amedian$(OBJEXT) \
    $(OBJ_DIR)\anomatch$(OBJEXT) \
    $(OBJ_DIR)\any2any$(OBJEXT) \
    $(OBJ_DIR)\aredit$(OBJEXT) \
    $(OBJ_DIR)\asum$(OBJEXT) \
    $(OBJ_DIR)\at2$(OBJEXT) \
    $(OBJ_DIR)\bitclr$(OBJEXT) \
    $(OBJ_DIR)\bitset$(OBJEXT) \
    $(OBJ_DIR)\blink$(OBJEXT) \
    $(OBJ_DIR)\byt2bit$(OBJEXT) \
    $(OBJ_DIR)\byt2hex$(OBJEXT) \
    $(OBJ_DIR)\byteand$(OBJEXT) \
    $(OBJ_DIR)\byteneg$(OBJEXT) \
    $(OBJ_DIR)\bytenot$(OBJEXT) \
    $(OBJ_DIR)\byteor$(OBJEXT) \
    $(OBJ_DIR)\bytexor$(OBJEXT) \
    $(OBJ_DIR)\calendar$(OBJEXT) \
    $(OBJ_DIR)\clrsel$(OBJEXT) \
    $(OBJ_DIR)\cntryset$(OBJEXT) \
    $(OBJ_DIR)\d2e$(OBJEXT) \
    $(OBJ_DIR)\datecnfg$(OBJEXT) \
    $(OBJ_DIR)\dayofyr$(OBJEXT) \
    $(OBJ_DIR)\daytobow$(OBJEXT) \
    $(OBJ_DIR)\dectobin$(OBJEXT) \
    $(OBJ_DIR)\diskfunc$(OBJEXT) \
    $(OBJ_DIR)\dispmsg$(OBJEXT) \
    $(OBJ_DIR)\dosver$(OBJEXT) \
    $(OBJ_DIR)\e2d$(OBJEXT) \
    $(OBJ_DIR)\easter$(OBJEXT) \
    $(OBJ_DIR)\elapmil$(OBJEXT) \
    $(OBJ_DIR)\elapsed$(OBJEXT) \
    $(OBJ_DIR)\eltime$(OBJEXT) \
    $(OBJ_DIR)\findith$(OBJEXT) \
    $(OBJ_DIR)\firstday$(OBJEXT) \
    $(OBJ_DIR)\gcd$(OBJEXT) \
    $(OBJ_DIR)\hex2dec$(OBJEXT) \
    $(OBJ_DIR)\invclr$(OBJEXT) \
    $(OBJ_DIR)\isbit$(OBJEXT) \
    $(OBJ_DIR)\isbiton$(OBJEXT) \
    $(OBJ_DIR)\isshare$(OBJEXT) \
    $(OBJ_DIR)\lastday$(OBJEXT) \
    $(OBJ_DIR)\linked$(OBJEXT) \
    $(OBJ_DIR)\madd$(OBJEXT) \
    $(OBJ_DIR)\menu1$(OBJEXT) \
    $(OBJ_DIR)\menuto$(OBJEXT) \
    $(OBJ_DIR)\metaph$(OBJEXT) \
    $(OBJ_DIR)\miltime$(OBJEXT) \
    $(OBJ_DIR)\min2dhm$(OBJEXT) \
    $(OBJ_DIR)\month$(OBJEXT) \
    $(OBJ_DIR)\mouse1$(OBJEXT) \
    $(OBJ_DIR)\netpv$(OBJEXT) \
    $(OBJ_DIR)\nooccur$(OBJEXT) \
    $(OBJ_DIR)\ntow$(OBJEXT) \
    $(OBJ_DIR)\nwlstat$(OBJEXT) \
    $(OBJ_DIR)\page$(OBJEXT) \
    $(OBJ_DIR)\pchr$(OBJEXT) \
    $(OBJ_DIR)\pegs$(OBJEXT) \
    $(OBJ_DIR)\pending$(OBJEXT) \
    $(OBJ_DIR)\pickday$(OBJEXT) \
    $(OBJ_DIR)\popadder$(OBJEXT) \
    $(OBJ_DIR)\prtesc$(OBJEXT) \
    $(OBJ_DIR)\pvid$(OBJEXT) \
    $(OBJ_DIR)\qtr$(OBJEXT) \
    $(OBJ_DIR)\rand1$(OBJEXT) \
    $(OBJ_DIR)\restsets$(OBJEXT) \
    $(OBJ_DIR)\ftround$(OBJEXT) \
    $(OBJ_DIR)\savearr$(OBJEXT) \
    $(OBJ_DIR)\savesets$(OBJEXT) \
    $(OBJ_DIR)\scregion$(OBJEXT) \
    $(OBJ_DIR)\sinkey$(OBJEXT) \
    $(OBJ_DIR)\sleep$(OBJEXT) \
    $(OBJ_DIR)\sqzn$(OBJEXT) \
    $(OBJ_DIR)\tbwhile$(OBJEXT) \
    $(OBJ_DIR)\tempfile$(OBJEXT) \
    $(OBJ_DIR)\vertmenu$(OBJEXT) \
    $(OBJ_DIR)\vidmode$(OBJEXT) \
    $(OBJ_DIR)\wda$(OBJEXT) \
    $(OBJ_DIR)\week$(OBJEXT) \
    $(OBJ_DIR)\workdays$(OBJEXT) \
    $(OBJ_DIR)\woy$(OBJEXT) \
    $(OBJ_DIR)\xbox$(OBJEXT) \
    $(OBJ_DIR)\year$(OBJEXT)

#   $(OBJ_DIR)\sysmem$(OBJEXT) \
#   $(OBJ_DIR)\dfile$(OBJEXT) \
#   $(OBJ_DIR)\vidcur$(OBJEXT) \
#   $(OBJ_DIR)\scancode$(OBJEXT) \
#   $(OBJ_DIR)\setdate$(OBJEXT) \
#   $(OBJ_DIR)\settime$(OBJEXT) \
#   $(OBJ_DIR)\nwsem$(OBJEXT) \
#   $(OBJ_DIR)\nwuid$(OBJEXT) \

#   $(OBJ_DIR)\floptst$(OBJEXT) \

all: \
    $(LIB_PATH) \
