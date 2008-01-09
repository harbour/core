#
# $Id$
#

LIBNAME = hbct

LIB_PATH = $(LIB_DIR)\$(LIBNAME)$(LIBEXT)

PRG_HEADERS = \
    ct.ch \
    ctdisk.ch \
    cterror.ch \
    ctextern.ch \
    ctvideo.ch \

LIB_OBJS = \
    $(OBJ_DIR)\addascii$(OBJEXT) \
    $(OBJ_DIR)\asciisum$(OBJEXT) \
    $(OBJ_DIR)\ascpos$(OBJEXT) \
    $(OBJ_DIR)\atadjust$(OBJEXT) \
    $(OBJ_DIR)\atnum$(OBJEXT) \
    $(OBJ_DIR)\atrepl$(OBJEXT) \
    $(OBJ_DIR)\bitnum$(OBJEXT) \
    $(OBJ_DIR)\charevod$(OBJEXT) \
    $(OBJ_DIR)\charlist$(OBJEXT) \
    $(OBJ_DIR)\charmirr$(OBJEXT) \
    $(OBJ_DIR)\charmix$(OBJEXT) \
    $(OBJ_DIR)\charone$(OBJEXT) \
    $(OBJ_DIR)\charonly$(OBJEXT) \
    $(OBJ_DIR)\charop$(OBJEXT) \
    $(OBJ_DIR)\charrepl$(OBJEXT) \
    $(OBJ_DIR)\charsort$(OBJEXT) \
    $(OBJ_DIR)\charsprd$(OBJEXT) \
    $(OBJ_DIR)\charswap$(OBJEXT) \
    $(OBJ_DIR)\color$(OBJEXT) \
    $(OBJ_DIR)\count$(OBJEXT) \
    $(OBJ_DIR)\ctc$(OBJEXT) \
    $(OBJ_DIR)\ctchksum$(OBJEXT) \
    $(OBJ_DIR)\ctcrypt$(OBJEXT) \
    $(OBJ_DIR)\ctmath$(OBJEXT) \
    $(OBJ_DIR)\ctmath2$(OBJEXT) \
    $(OBJ_DIR)\ctnet$(OBJEXT) \
    $(OBJ_DIR)\ctpad$(OBJEXT) \
    $(OBJ_DIR)\ctset$(OBJEXT) \
    $(OBJ_DIR)\ctstr$(OBJEXT) \
    $(OBJ_DIR)\ctstrfil$(OBJEXT) \
    $(OBJ_DIR)\ctwfunc$(OBJEXT) \
    $(OBJ_DIR)\ctwin$(OBJEXT) \
    $(OBJ_DIR)\cursor$(OBJEXT) \
    $(OBJ_DIR)\datetime$(OBJEXT) \
    $(OBJ_DIR)\dattime2$(OBJEXT) \
    $(OBJ_DIR)\dattime3$(OBJEXT) \
    $(OBJ_DIR)\dbftools$(OBJEXT) \
    $(OBJ_DIR)\disk$(OBJEXT) \
    $(OBJ_DIR)\expand$(OBJEXT) \
    $(OBJ_DIR)\exponent$(OBJEXT) \
    $(OBJ_DIR)\files$(OBJEXT) \
    $(OBJ_DIR)\finan$(OBJEXT) \
    $(OBJ_DIR)\ftoc$(OBJEXT) \
    $(OBJ_DIR)\justify$(OBJEXT) \
    $(OBJ_DIR)\keyset$(OBJEXT) \
    $(OBJ_DIR)\like$(OBJEXT) \
    $(OBJ_DIR)\lton$(OBJEXT) \
    $(OBJ_DIR)\maxline$(OBJEXT) \
    $(OBJ_DIR)\misc1$(OBJEXT) \
    $(OBJ_DIR)\misc2$(OBJEXT) \
    $(OBJ_DIR)\misc3$(OBJEXT) \
    $(OBJ_DIR)\num1$(OBJEXT) \
    $(OBJ_DIR)\numat$(OBJEXT) \
    $(OBJ_DIR)\numcount$(OBJEXT) \
    $(OBJ_DIR)\numline$(OBJEXT) \
    $(OBJ_DIR)\pack$(OBJEXT) \
    $(OBJ_DIR)\pos1$(OBJEXT) \
    $(OBJ_DIR)\pos2$(OBJEXT) \
    $(OBJ_DIR)\posdiff$(OBJEXT) \
    $(OBJ_DIR)\print$(OBJEXT) \
    $(OBJ_DIR)\range$(OBJEXT) \
    $(OBJ_DIR)\relation$(OBJEXT) \
    $(OBJ_DIR)\remove$(OBJEXT) \
    $(OBJ_DIR)\replace$(OBJEXT) \
    $(OBJ_DIR)\screen1$(OBJEXT) \
    $(OBJ_DIR)\screen2$(OBJEXT) \
    $(OBJ_DIR)\setlast$(OBJEXT) \
    $(OBJ_DIR)\setrc$(OBJEXT) \
    $(OBJ_DIR)\strdiff$(OBJEXT) \
    $(OBJ_DIR)\strswap$(OBJEXT) \
    $(OBJ_DIR)\tab$(OBJEXT) \
    $(OBJ_DIR)\token1$(OBJEXT) \
    $(OBJ_DIR)\token2$(OBJEXT) \
    $(OBJ_DIR)\trig$(OBJEXT) \
    $(OBJ_DIR)\video$(OBJEXT) \
    $(OBJ_DIR)\wordrepl$(OBJEXT) \
    $(OBJ_DIR)\wordtoch$(OBJEXT) \
    \
    $(OBJ_DIR)\blank$(OBJEXT) \
    $(OBJ_DIR)\ct$(OBJEXT) \
    $(OBJ_DIR)\ctmisc$(OBJEXT) \
    $(OBJ_DIR)\ctrand$(OBJEXT) \
    $(OBJ_DIR)\cttime$(OBJEXT) \
    $(OBJ_DIR)\fcopy$(OBJEXT) \
    $(OBJ_DIR)\getinfo$(OBJEXT) \
    $(OBJ_DIR)\getinput$(OBJEXT) \
    $(OBJ_DIR)\getsecrt$(OBJEXT) \
    $(OBJ_DIR)\keysave$(OBJEXT) \
    $(OBJ_DIR)\keysec$(OBJEXT) \
    $(OBJ_DIR)\keytime$(OBJEXT) \
    $(OBJ_DIR)\numconv$(OBJEXT) \
    $(OBJ_DIR)\screen3$(OBJEXT) \
    $(OBJ_DIR)\scrmark$(OBJEXT) \
    $(OBJ_DIR)\showtime$(OBJEXT) \

all: \
    $(LIB_PATH) \
