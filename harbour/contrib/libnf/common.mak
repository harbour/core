#
# $Id$
#

LIBNAME = nf

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

#
# LIB rules
#

LIB_OBJS = \
   $(OBJ_DIR)\caplock.obj \
   $(OBJ_DIR)\color2n.obj \
   $(OBJ_DIR)\descend.obj \
   $(OBJ_DIR)\numlock.obj \
   $(OBJ_DIR)\proper.obj \
   $(OBJ_DIR)\shift.obj \
   $(OBJ_DIR)\mouse.obj \
   $(OBJ_DIR)\getvid.obj \
   $(OBJ_DIR)\setkeys.obj \
   $(OBJ_DIR)\setlastk.obj \
   $(OBJ_DIR)\ftisprn.obj \
   $(OBJ_DIR)\ftidle.obj \
   $(OBJ_DIR)\iamidle.obj \
   $(OBJ_DIR)\chdir.obj \
   $(OBJ_DIR)\rmdir.obj \
   $(OBJ_DIR)\alt.obj \
   $(OBJ_DIR)\ctrl.obj \
   $(OBJ_DIR)\getenvrn.obj \
   $(OBJ_DIR)\n2color.obj \
   $(OBJ_DIR)\origin.obj \
   $(OBJ_DIR)\prtscr.obj \
   $(OBJ_DIR)\stod.obj \
   $(OBJ_DIR)\kspeed.obj \
   $(OBJ_DIR)\mkdir.obj \
   $(OBJ_DIR)\getver.obj \
   $(OBJ_DIR)\ftattr.obj \
   $(OBJ_DIR)\dispc.obj \
   $(OBJ_DIR)\fttext.obj \
   $(OBJ_DIR)\ftshadow.obj \
   $(OBJ_DIR)\putkey.obj \
   \
   $(OBJ_DIR)\aading.obj \
   $(OBJ_DIR)\aavg.obj \
   $(OBJ_DIR)\acctadj.obj \
   $(OBJ_DIR)\acctmnth.obj \
   $(OBJ_DIR)\acctqtr.obj \
   $(OBJ_DIR)\acctweek.obj \
   $(OBJ_DIR)\acctyear.obj \
   $(OBJ_DIR)\adessort.obj \
   $(OBJ_DIR)\aemaxlen.obj \
   $(OBJ_DIR)\aeminlen.obj \
   $(OBJ_DIR)\amedian.obj \
   $(OBJ_DIR)\anomatch.obj \
   $(OBJ_DIR)\any2any.obj \
   $(OBJ_DIR)\aredit.obj \
   $(OBJ_DIR)\asum.obj \
   $(OBJ_DIR)\at2.obj \
   $(OBJ_DIR)\bitclr.obj \
   $(OBJ_DIR)\bitset.obj \
   $(OBJ_DIR)\blink.obj \
   $(OBJ_DIR)\byt2bit.obj \
   $(OBJ_DIR)\byt2hex.obj \
   $(OBJ_DIR)\byteand.obj \
   $(OBJ_DIR)\byteneg.obj \
   $(OBJ_DIR)\bytenot.obj \
   $(OBJ_DIR)\byteor.obj \
   $(OBJ_DIR)\bytexor.obj \
   $(OBJ_DIR)\calendar.obj \
   $(OBJ_DIR)\clrsel.obj \
   $(OBJ_DIR)\cntryset.obj \
   $(OBJ_DIR)\d2e.obj \
   $(OBJ_DIR)\datecnfg.obj \
   $(OBJ_DIR)\dayofyr.obj \
   $(OBJ_DIR)\daytobow.obj \
   $(OBJ_DIR)\dectobin.obj \
   $(OBJ_DIR)\diskfunc.obj \
   $(OBJ_DIR)\dispmsg.obj \
   $(OBJ_DIR)\dosver.obj \
   $(OBJ_DIR)\e2d.obj \
   $(OBJ_DIR)\easter.obj \
   $(OBJ_DIR)\elapmil.obj \
   $(OBJ_DIR)\elapsed.obj \
   $(OBJ_DIR)\eltime.obj \
   $(OBJ_DIR)\findith.obj \
   $(OBJ_DIR)\firstday.obj \
   $(OBJ_DIR)\gcd.obj \
   $(OBJ_DIR)\hex2dec.obj \
   $(OBJ_DIR)\invclr.obj \
   $(OBJ_DIR)\isbit.obj \
   $(OBJ_DIR)\isbiton.obj \
   $(OBJ_DIR)\isshare.obj \
   $(OBJ_DIR)\lastday.obj \
   $(OBJ_DIR)\linked.obj \
   $(OBJ_DIR)\madd.obj \
   $(OBJ_DIR)\menu1.obj \
   $(OBJ_DIR)\menuto.obj \
   $(OBJ_DIR)\metaph.obj \
   $(OBJ_DIR)\miltime.obj \
   $(OBJ_DIR)\min2dhm.obj \
   $(OBJ_DIR)\month.obj \
   $(OBJ_DIR)\mouse1.obj \
   $(OBJ_DIR)\netpv.obj \
   $(OBJ_DIR)\nooccur.obj \
   $(OBJ_DIR)\ntow.obj \
   $(OBJ_DIR)\nwlstat.obj \
   $(OBJ_DIR)\page.obj \
   $(OBJ_DIR)\pchr.obj \
   $(OBJ_DIR)\pegs.obj \
   $(OBJ_DIR)\pending.obj \
   $(OBJ_DIR)\pickday.obj \
   $(OBJ_DIR)\popadder.obj \
   $(OBJ_DIR)\prtesc.obj \
   $(OBJ_DIR)\pvid.obj \
   $(OBJ_DIR)\qtr.obj \
   $(OBJ_DIR)\rand1.obj \
   $(OBJ_DIR)\restsets.obj \
   $(OBJ_DIR)\ftround.obj \
   $(OBJ_DIR)\savearr.obj \
   $(OBJ_DIR)\savesets.obj \
   $(OBJ_DIR)\scregion.obj \
   $(OBJ_DIR)\sinkey.obj \
   $(OBJ_DIR)\sleep.obj \
   $(OBJ_DIR)\sqzn.obj \
   $(OBJ_DIR)\tbwhile.obj \
   $(OBJ_DIR)\tempfile.obj \
   $(OBJ_DIR)\vertmenu.obj \
   $(OBJ_DIR)\vidmode.obj \
   $(OBJ_DIR)\wda.obj \
   $(OBJ_DIR)\week.obj \
   $(OBJ_DIR)\workdays.obj \
   $(OBJ_DIR)\woy.obj \
   $(OBJ_DIR)\xbox.obj \
   $(OBJ_DIR)\year.obj

#   $(OBJ_DIR)\sysmem.obj \
#   $(OBJ_DIR)\dfile.obj \
#   $(OBJ_DIR)\vidcur.obj \
#   $(OBJ_DIR)\scancode.obj \
#   $(OBJ_DIR)\setdate.obj \
#   $(OBJ_DIR)\settime.obj \
#   $(OBJ_DIR)\nwsem.obj \
#   $(OBJ_DIR)\nwuid.obj \

#   $(OBJ_DIR)\floptst.obj

all: \
   $(LIB_PATH) \
