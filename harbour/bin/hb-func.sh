#!/bin/sh
[ "$BASH" ] || exec bash `which $0` ${1+"$@"}
#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@priv.onet.pl>
# small set of functions used by Harbour scripts
# warning: some bash extensions are used
#
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

get_hbplatform()
{
    local id

    if [ "$OSTYPE" = "msdosdjgpp" ]; then
        id="djgpp"
    else
        # please add your distro suffix if it not belong to the one recognized below
        # and remember that order checking can be important
        [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' mandriva-release-One 2>/dev/null) && echo "mdk$rel"|tr -d "."`
        [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' mandriva-release 2>/dev/null) && echo "mdk$rel"|tr -d "."`
        [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' mandrake-release 2>/dev/null) && echo "mdk$rel"|tr -d "."`
        [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' redhat-release 2>/dev/null) && echo "rh$rel"|tr -d "."`
        [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' fedora-release 2>/dev/null) && echo "fc$rel"|tr -d "."`
        [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' suse-release 2>/dev/null) && echo "sus$rel"|tr -d "."`
        [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' openSUSE-release 2>/dev/null) && echo "sus$rel"|tr -d "."`
        [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' conectiva-release 2>/dev/null) && echo "cl$rel"|tr -d "."`
        [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' aurox-release 2>/dev/null) && echo "cl$rel"|tr -d "."`
        [ "${id}" = "" ] && id=`[ -f /etc/pld-release ] && cat /etc/pld-release|sed -e '/1/ !d' -e 's/[^0-9]//g' -e 's/^/pld/'`
        [ "${id}" = "" ] && id=`uname -s | tr '[ A-Z]' '[_a-z]'`
        case "${id}" in
            mingw*) id="mingw" ;;
            *) ;;
        esac
    fi
    echo "${id}"
}

get_hbver()
{
    local FVER MAJOR MINOR REVIS hb_rootdir

    hb_rootdir="${1-.}"
    FVER="${hb_rootdir}/include/hbver.h"
    MAJOR=`sed -e '/HB_VER_MAJOR/   !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${FVER}"`
    MINOR=`sed -e '/HB_VER_MINOR/   !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${FVER}"`
    RELEA=`sed -e '/HB_VER_RELEASE/ !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${FVER}"`
    echo "${MAJOR}.${MINOR}.${RELEA}"
}

get_hbverstat()
{
    local FVER VERSTAT hb_rootdir

    hb_rootdir="${1-.}"
    FVER="${hb_rootdir}/include/hbver.h"
    VERSTAT=`sed -e '/HB_VER_STATUS/ !d' -e 's/[^\"]*\"\([^\"]*\).*/\1/g' "${FVER}"`
    echo "${VERSTAT}"
}

get_solibname()
{
    local name

    name="${HB_SHAREDLIB_NAME}"
    [ -z "${name}" ] && name="harbour"
    echo "${name}"
}

mk_hbgetlibs()
{
    local libs

    if [ -z "$@" ]
    then
        libs=""
        if [ "$HB_COMPILER" != "mingwce" ]
        then
            libs="$libs gtwin"
        fi
        echo "hbextern hbvm hbpp hbrtl hbrdd rddfpt rddcdx rddnsx rddntx hbhsx hbsix hbusrrdd ${HB_DB_DRVEXT} hbmacro hbcommon hblang hbcpage gtcrs gtsln gtxvt gtxwc gtalleg gtcgi gtstd gtpca gttrm $libs gtwvt gtgui gtdos gtos2 hbdebug profiler hbcplr hbpcre hbzlib"
    else
        echo "$@"
    fi
}

mk_hbgetlibsctb()
{
    local libs

    if [ -z "$@" ]
    then
        libs=""
        if [ "$HB_COMPILER" = "mingwce" ]
        then
            libs="$libs gtwin"
        fi
        echo "$libs hbct hbnf hbmzip hbcrypt hbtip xhb hbgd hbfimage rddsql sddfb sddmy sddpg hbodbc hbpgsql hbmysql hbfbird rddads rddado hbhpdf hbvpdf hbcurl hbwin gtwvg gtalleg hbsqlit3 hbbtree $HB_USER_LIBS"
        #"hbgf hbgt hbbmcdx hbmisc hbole hbtpathy hbwhat hbziparc hbmsql"
    else
        echo "$@"
    fi
}

mk_hbtools()
{
    local name hb_pref hb_tool hb_libs hb_libsc hb_hbmkcfg hb_gt_ori

    name=`get_solibname`
    hb_pref="$4"
    [ -z "${hb_pref}" ] && hb_pref="${HB_TOOLS_PREF-hb}"
    hb_cmpname="${HB_CMPNAME-harbour}"
    if [ "${HB_ARCHITECTURE}" = "dos" ]; then
        hb_tool="$1/${hb_pref}-bld"
        hb_path_separator=";"
        hb_static="yes"
        hb_static_default=" (default)"
        hb_exesuf=".exe"
    elif [ "${HB_ARCHITECTURE}" = "win" ]; then
        hb_tool="$1/${hb_pref}-build"
        hb_path_separator=":"
        if [ "${HB_MK_STATIC}" = "yes" ]; then
            hb_static="yes"
            hb_static_default=" (default)"
        else
            hb_static="no"
            hb_shared_default=" (default)"
        fi
        hb_exesuf=".exe"
    elif [ "${HB_ARCHITECTURE}" = "darwin" ]; then
        hb_tool="$1/${hb_pref}-build"
        hb_path_separator=":"
        if [ "${HB_MK_STATIC}" = "yes" ]; then
            hb_static="yes"
            hb_static_default=" (default)"
        else
            hb_static="no"
            hb_shared_default=" (default)"
        fi
        hb_exesuf=""
    else
        hb_tool="$1/${hb_pref}-build"
        hb_path_separator=":"
        if [ "${HB_MK_STATIC}" = "yes" ]; then
            hb_static="yes"
            hb_static_default=" (default)"
        else
            hb_static="no"
            hb_shared_default=" (default)"
        fi
        hb_exesuf=""
    fi
    hb_libs=`mk_hbgetlibs "$2"`
    hb_libsc=`mk_hbgetlibsctb "$3"`
    hb_gt_ori=${HB_GT_LIB}
    [ -z "${HB_GT_LIB}" ] && HB_GT_LIB="gtstd"

    [ -z "${_DEFAULT_BIN_DIR}" ] && _DEFAULT_BIN_DIR="${HB_BIN_INSTALL}"
    [ -z "${_DEFAULT_INC_DIR}" ] && _DEFAULT_INC_DIR="${HB_INC_INSTALL}"
    [ -z "${_DEFAULT_LIB_DIR}" ] && _DEFAULT_LIB_DIR="${HB_LIB_INSTALL}"

    HB_SYS_LIBS="-lm"
    HB_CRS_LIB=""
    HB_SLN_LIB=""
    if [ "${HB_USER_CFLAGS//-DHB_PCRE_REGEX/}" != "${HB_USER_CFLAGS}" ]; then
        HB_SYS_LIBS="-lpcre ${HB_SYS_LIBS}"
        hb_libs="${hb_libs//hbpcre/}"
    elif [ "${HB_USER_CFLAGS//-DHB_POSIX_REGEX/}" != "${HB_USER_CFLAGS}" ]; then
        hb_libs="${hb_libs//hbpcre/}"
    fi
    if [ "${HB_USER_CFLAGS//-DHB_EXT_ZLIB/}" != "${HB_USER_CFLAGS}" ]; then
        HB_SYS_LIBS="-lz ${HB_SYS_LIBS}"
        hb_libs="${hb_libs//hbzlib/}"
    fi
    if [ "${HB_COMPILER}" = "mingw" ]; then
        HB_SYS_LIBS="${HB_SYS_LIBS} -luser32 -lwinspool -lgdi32 -lcomctl32 -lcomdlg32 -lole32 -loleaut32 -luuid -lwsock32 -lws2_32"
        HB_WITHOUT_X11="yes"
    elif [ "${HB_COMPILER}" = "mingwce" ]; then
        HB_SYS_LIBS="${HB_SYS_LIBS} -lwininet -lws2 -lcommdlg -lcommctrl -luuid -lole32"
        HB_WITHOUT_X11="yes"
    elif [ "${HB_COMPILER}" = "djgpp" ]; then
        HB_SYS_LIBS="${HB_SYS_LIBS}"
        HB_WITHOUT_X11="yes"
    else
        HB_CRS_LIB=""
        if [ "${HB_ARCHITECTURE}" = "linux" ]; then
            HB_SYS_LIBS="${HB_SYS_LIBS} -ldl -lrt"
        elif [ "${HB_ARCHITECTURE}" = "sunos" ]; then
            HB_SYS_LIBS="${HB_SYS_LIBS} -lrt"
            HB_SYS_LIBS="${HB_SYS_LIBS} -lsocket -lnsl -lresolv"
            HB_CRS_LIB="curses"
        elif [ "${HB_ARCHITECTURE}" = "hpux" ]; then
            HB_SYS_LIBS="${HB_SYS_LIBS} -lrt"
        fi
        if [ -n "${HB_CURSES_VER}" ]; then
            HB_CRS_LIB="${HB_CURSES_VER}"
        elif [ "${HB_NCURSES_194}" = "yes" ]; then
            HB_CRS_LIB="ncur194"
        elif [ -z "${HB_CRS_LIB}" ]; then
            HB_CRS_LIB="ncurses"
        fi
        HB_SLN_LIB="slang"
    fi
    if [ "${HB_USER_CFLAGS//-mlp64/}" != "${HB_USER_CFLAGS}" ]; then
        CC_HB_USER_CFLAGS="${CC_HB_USER_CFLAGS} -mlp64"
        CC_HB_USER_LDFLAGS="${CC_HB_USER_LDFLAGS} -mlp64"
    elif [ "${HB_USER_CFLAGS//-mlp32/}" != "${HB_USER_CFLAGS}" ]; then
        CC_HB_USER_CFLAGS="${CC_HB_USER_CFLAGS} -mlp32"
        CC_HB_USER_LDFLAGS="${CC_HB_USER_LDFLAGS} -mlp32"
    elif [ "${HB_USER_CFLAGS//-m64/}" != "${HB_USER_CFLAGS}" ]; then
        CC_HB_USER_CFLAGS="${CC_HB_USER_CFLAGS} -m64"
        CC_HB_USER_LDFLAGS="${CC_HB_USER_LDFLAGS} -m64"
    elif [ "${HB_USER_CFLAGS//-m32/}" != "${HB_USER_CFLAGS}" ]; then
        CC_HB_USER_CFLAGS="${CC_HB_USER_CFLAGS} -m32"
        CC_HB_USER_LDFLAGS="${CC_HB_USER_LDFLAGS} -m32"
    fi
    if [ "${HB_USER_CFLAGS//-fPIC/}" != "${HB_USER_CFLAGS}" ]; then
        CC_HB_USER_CFLAGS="${CC_HB_USER_CFLAGS} -fPIC"
    elif [ "${HB_USER_CFLAGS//-fpic/}" != "${HB_USER_CFLAGS}" ]; then
        CC_HB_USER_CFLAGS="${CC_HB_USER_CFLAGS} -fpic"
    fi

    hb_hbmkcfg="$1/hbmk.cfg"
    echo "Generating ${hb_hbmkcfg}... "
    echo "# Harbour Make configuration"> ${hb_hbmkcfg}
    echo "# Generated by Harbour build process">> ${hb_hbmkcfg}
    if [ -n "${hb_gt_ori}" ]; then
        echo "gtdef=${hb_gt_ori}">> ${hb_hbmkcfg}
    fi
    if [ -n "${CC_HB_USER_CFLAGS}" ]; then
        echo "cflags=${CC_HB_USER_CFLAGS}">> ${hb_hbmkcfg}
    fi
    if [ -n "${CC_HB_USER_LDFLAGS}" ]; then
        echo "ldflags=${CC_HB_USER_LDFLAGS}">> ${hb_hbmkcfg}
    fi
    if [ "${HB_GPM_MOUSE}" = "yes" ]; then
        echo "libs=gpm">> ${hb_hbmkcfg}
    fi
    if [ "${HB_WITHOUT_X11}" != "yes" ]; then
        echo "libdynhas=gtxwc">> ${hb_hbmkcfg}
    fi
    if [ "${HB_WITHOUT_GTCRS}" != "yes" ] && [ -n "${HB_CRS_LIB}" ]; then
        echo "libdynhas=gtcrs">> ${hb_hbmkcfg}
    fi
    if [ "${HB_WITHOUT_GTSLN}" != "yes" ] && [ -n "${HB_SLN_LIB}" ]; then
        echo "libdynhas=gtsln">> ${hb_hbmkcfg}
    fi

    echo "Generating ${hb_tool}... "
    cat > ${hb_tool} <<EOF
#!/bin/sh
[ "\$BASH" ] || exec bash \`which \$0\` \${1+"\$@"}
#
# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@priv.onet.pl>
# simple script to build binaries .tgz from Harbour sources
#
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------
#

# set environment variables
export HB_ARCHITECTURE="${HB_ARCHITECTURE}"
export HB_COMPILER="${HB_COMPILER}"
[ -z "\${HB_BIN_INSTALL}" ] && export HB_BIN_INSTALL="${_DEFAULT_BIN_DIR}"
[ -z "\${HB_INC_INSTALL}" ] && export HB_INC_INSTALL="${_DEFAULT_INC_DIR}"
[ -z "\${HB_LIB_INSTALL}" ] && export HB_LIB_INSTALL="${_DEFAULT_LIB_DIR}"

# be sure that ${name} binaries are in your path
export PATH="\${HB_BIN_INSTALL}${hb_path_separator}${CCPATH}\${PATH}"

if [ "\${HB_COMPILER}" == "gpp" ]; then
   HB_CC="g++"
else
   HB_CC="gcc"
fi

if [ \$# = 0 ]; then
    echo "syntax: \$0 [<options,...>] <file>[.prg|.o]

\"${hb_pref}cc\", \"${hb_pref}cmp\", \"${hb_pref}lnk\" and \"${hb_pref}mk\" parameters:
    -o<outputfilename>  # output file name
\"${hb_pref}lnk\" and \"${hb_pref}mk\" parameters:
    -static             # link with static ${name} libs${hb_static_default}
    -fullstatic         # link with all static libs
    -shared             # link with shared libs${hb_shared_default}
    -mt                 # link with multi-thread libs
    -gt<hbgt>           # link with <hbgt> GT driver, can be repeated to
                        # link with more GTs. The first one will be
                        #      the default at runtime
    -xbgtk              # link with xbgtk library (xBase GTK+ interface)
    -xhgtk              # link with xHGtk library (GTK+ interface)
    -hwgui              # link with HWGUI library (GTK+ interface)
    -l<libname>         # link with <libname> library
    -L<libpath>         # additional path to search for libraries
    -fmstat             # link with the memory statistics lib
    -nofmstat           # do not link with the memory statistics lib (default)
    -[no]strip          # strip (no strip) binaries
    -main=<main_func>   # set the name of main program function/procedure.
                        # if not set then 'MAIN' is used or if it doesn't
                        # exist the name of first public function/procedure
                        # in first linked object module (link)
"
    exit 1
elif [ "\$*" = "mk-links" ]; then
    DIR="\${0%/*}"
    NAME="\${0##*/}"
    if [ "\${DIR}" != "\${NAME}" ]; then
        (cd "\${DIR}"
        for n in ${hb_pref}cc ${hb_pref}cmp ${hb_pref}mk ${hb_pref}lnk; do
            if [ "\${HB_ARCHITECTURE}" = "dos" ]; then
                cp -f "\${NAME}" "\${n}"
            else
                ln -sf "\${NAME}" "\${n}"
            fi
        done
        )
    fi
    exit
fi

## check basename
case "\${0##*/}" in
    *cc)    HB=cc  ;;
    *cmp)   HB=cmp ;;
    *lnk)   HB=lnk ;;
    *mk)    HB=mk  ;;
    *)      exit 1 ;;
esac

## default parameters
HB_STATIC="${hb_static}"
HB_MT=""
HB_GT="${HB_GT_LIB#gt}"

HB_GPM_MOUSE="${HB_GPM_MOUSE}"

HB_GT_REQ=""
HB_FM_REQ=""
HB_STRIP="yes"
HB_MAIN_FUNC=""
HB_XBGTK=""
HB_XHGTK=""
HB_HWGUI=""
HB_USRLIBS=""
HB_USRLPATH=""
HB_GEN=""
HB_MODE=""
LN_OPT="${CC_HB_USER_LDFLAGS}"
CC_OPT="${CC_HB_USER_CFLAGS}"
HB_OPT="${CC_HB_USER_PRGFLAGS}"

[ -n "\$TMPDIR" ] || TMPDIR="\$TMP"
[ -n "\$TMPDIR" ] || TMPDIR="\$TEMP"
[ -n "\$TMPDIR" ] || TMPDIR="/tmp"
_TMP_FILE_="\${TMPDIR}/hb-build-\$USER-\$\$.c"

## parse params
P=( "\$@" ); n=0; DIROUT="."; FILEOUT=""
while [ \$n -lt \${#P[@]} ]; do
    v=\${P[\$n]}; p=""
    case "\$v" in
        -o*)
            d="\${v#-o}"; p="\${v}"
            if [ -d "\${d}" ]; then
                DIROUT="\${d%/}"
            elif [ -d "\${d%/*}" ]; then
                DIROUT="\${d%/*}"; FILEOUT="\${d##*/}"; p="-o\${d}"
                [ \${HB} = "cc" ] || p="-o\${d%.*}"
            elif [ -n "\${d}" ]; then
                FILEOUT="\${d}"; p="-o\${d}"
                [ \${HB} = "cc" ] || p="-o\${d%.*}"
            fi ;;
        -static)     HB_STATIC="yes" ;;
        -fullstatic) HB_STATIC="full" ;;
        -shared)     HB_STATIC="no" ;;
        -xbgtk)      HB_XBGTK="yes" ;;
        -xhgtk)      HB_XHGTK="yes" ;;
        -hwgui)      HB_HWGUI="yes" ;;
        -mt)         HB_MT="MT" ;;
        -gt*)        HB_GT_REQ="\${HB_GT_REQ} \${v#-gt}" ;;
        -fmstat)     HB_FM_REQ="STAT" ;;
        -nofmstat)   HB_FM_REQ="NOSTAT" ;;
        -strip)      HB_STRIP="yes" ;;
        -nostrip)    HB_STRIP="no" ;;
        -l[^-]*)     HB_USRLIBS="\${HB_USRLIBS} \${v}" ;;
        -L[^-]*)     HB_USRLPATH="\${HB_USRLPATH} \${v}" ;;
        -mwindows)   LN_OPT="\${LN_OPT} \${v}"; HB_MODE="gui" ;;
        -mconsole)   LN_OPT="\${LN_OPT} \${v}"; HB_MODE="std" ;;
        -main=*)     HB_MAIN_FUNC="\${v#*=}" ;;
        -g[cohwij])  HB_GEN="\${v#-g}"; p="\${v}" ;;
        -gc[0-9])    HB_GEN="c"; p="\${v}" ;;
        -go[0-9])    HB_GEN="o"; p="\${v}" ;;
        -*)          p="\${v}" ;;
        *)           [ -z \${FILEOUT} ] && FILEOUT="\${v##*/}"; p="\${v}" ;;
    esac
    [ -n "\$p" ] && PP[\$n]="\$p"
    n=\$[\$n + 1]
done
P=( "\${PP[@]}" )

case "\${HB_MT}" in
    [Mm][Tt]|[Yy][Ee][Ss]|1)  HB_MT="MT";;
    *)  HB_MT="";;
esac

SYSTEM_LIBS="${HB_SYS_LIBS}"
# use pthread system library for MT programs
if [ "\${HB_MT}" = "MT" ]; then
    case "\${HB_ARCHITECTURE}" in
        dos|win|os2)
            ;;
        *)
            SYSTEM_LIBS="-lpthread \${SYSTEM_LIBS}"
            ;;
    esac
fi

HB_GT_STAT=""
[ -z "\${HB_GT_REQ}" ] && HB_GT_REQ="\${HB_GT}"
HB_GT_REQ=\`echo \${HB_GT_REQ}|tr '[a-z]' '[A-Z]'\`
HB_MAIN_FUNC=\`echo \${HB_MAIN_FUNC}|tr '[a-z]' '[A-Z]'\`

HB_PATHS="-I\${HB_INC_INSTALL}"
GCC_PATHS="\${HB_PATHS} -L\${HB_LIB_INSTALL}"

HB_GPM_LIB=""
if [ -f "\${HB_LIB_INSTALL}/libgtsln.a" ]; then
    if [ "\${HB_ARCHITECTURE}" = "darwin" ]; then
        SYSTEM_LIBS="\${SYSTEM_LIBS} -L/sw/lib -L/opt/local/lib"
    elif [ "\${HB_ARCHITECTURE}" = "bsd" ]; then
        SYSTEM_LIBS="\${SYSTEM_LIBS} -L/usr/local/lib"
    fi
    SYSTEM_LIBS="-l${HB_SLN_LIB:-slang} \${SYSTEM_LIBS}"
    [ "\${HB_GPM_MOUSE}" = "yes" ] && HB_GPM_LIB="gpm"
fi
if [ -f "\${HB_LIB_INSTALL}/libgtcrs.a" ]; then
    SYSTEM_LIBS="-l${HB_CRS_LIB:-ncurses} \${SYSTEM_LIBS}"
    [ "\${HB_GPM_MOUSE}" = "yes" ] && HB_GPM_LIB="gpm"
fi
if [ "\${HB_WITHOUT_X11}" != "yes" ]; then
    if [ -f "\${HB_LIB_INSTALL}/libgtxvt.a" ] || [ -f "\${HB_LIB_INSTALL}/libgtxwc.a" ]; then
        [ -d "/usr/X11R6/lib64" ] && SYSTEM_LIBS="\${SYSTEM_LIBS} -L/usr/X11R6/lib64"
        SYSTEM_LIBS="-L/usr/X11R6/lib -lX11 \${SYSTEM_LIBS}"
    fi
fi
[ -n "\${HB_GPM_LIB}" ] && SYSTEM_LIBS="-l\${HB_GPM_LIB} \${SYSTEM_LIBS}"

if [ "\${HB_XBGTK}" = "yes" ]; then
    SYSTEM_LIBS="\${SYSTEM_LIBS} \`pkg-config --libs gtk+-2.0\`"
elif [ "\${HB_XHGTK}" = "yes" ]; then
    SYSTEM_LIBS="\${SYSTEM_LIBS} \`pkg-config --libs gtk+-2.0 libglade-2.0\`"
elif [ "\${HB_HWGUI}" = "yes" ]; then
    SYSTEM_LIBS="\${SYSTEM_LIBS} \`pkg-config --libs gtk+-2.0 --libs libgnomeprint-2.2\`"
fi

if [ "\${HB_STATIC}" = "full" ]; then
    if [ "\${HB_ARCHITECTURE}" = "linux" ]; then
        SYSTEM_LIBS="\${SYSTEM_LIBS} -lpthread -ldl"
    fi
    LN_OPT="\${LN_OPT} -static"
    HB_STATIC="yes"
fi

HB_LNK_REQ=""
for gt in \${HB_GT_REQ}; do
#    if [ "\${HB_STATIC}" = "yes" ] || [ "\${gt}" = "ALLEG" ]; then
        HB_LNK_REQ="\${HB_LNK_REQ} HB_GT_\${gt}"
        if [ "\${gt}" = "ALLEG" ]; then
            if [ "\${HB_STATIC}" = "yes" ]; then
                SYSTEM_LIBS="\`allegro-config --static 2>/dev/null\` \${SYSTEM_LIBS}"
            else
                SYSTEM_LIBS="\`allegro-config --libs 2>/dev/null\` \${SYSTEM_LIBS}"
            fi
        fi
#    fi
done
[ -n "\${HB_FM_REQ}" ] && HB_LNK_REQ="\${HB_LNK_REQ} HB_FM_\${HB_FM_REQ}"

HB_LNK_ATTR=""
HARBOUR_LIBS=""
if [ "\${HB_STATIC}" = "yes" ]; then
    libs="${hb_libs} ${hb_libsc}"
else
    l="${name}"
    if [ "\${HB_ARCHITECTURE}" = "darwin" ]; then
        pref="lib"
        ext=".dylib"
        LN_OPT="\${LN_OPT} -bind_at_load -multiply_defined suppress"
    elif [ "\${HB_ARCHITECTURE}" = "win" ]; then
        pref=""
        ext=".dll"
        HB_LNK_ATTR="__attribute__ ((dllimport))"
    elif [ "\${HB_ARCHITECTURE}" = "hpux" ]; then
        pref="lib"
        ext=".sl"
    else
        pref="lib"
        ext=".so"
    fi
    if [ "\${HB_MT}" = "MT" ]; then
        if [ -f "\${HB_LIB_INSTALL}/\${pref}\${l}mt\${ext}" ]; then
            HARBOUR_LIBS="\${HARBOUR_LIBS} -l\${l}mt"
            l=""
        elif [ -f "\${HB_BIN_INSTALL}/\${pref}\${l}mt\${ext}" ]; then
            HARBOUR_LIBS="\${HARBOUR_LIBS} -L\${HB_BIN_INSTALL} -l\${l}mt"
            l=""
        fi
    fi
    if [ -n "\${l}" ]; then
        if [ -f "\${HB_LIB_INSTALL}/\${pref}\${l}\${ext}" ]; then
            HARBOUR_LIBS="\${HARBOUR_LIBS} -l\${l}"
        elif [ -f "\${HB_BIN_INSTALL}/\${pref}\${l}\${ext}" ]; then
            HARBOUR_LIBS="\${HARBOUR_LIBS} -L\${HB_BIN_INSTALL} -l\${l}"
        fi
    fi
    libs="gtalleg hbdebug profiler hbcplr ${hb_libsc}"
fi
for l in \${libs}
do
    [ "\${HB_MT}" = "MT" ] && [ -f "\${HB_LIB_INSTALL}/lib\${l}mt.a" ] && l="\${l}mt"
    if [ -f "\${HB_LIB_INSTALL}/lib\${l}.a" ]; then
        HARBOUR_LIBS="\${HARBOUR_LIBS} -l\${l}"
    fi
done
if [ "\${HB_XBGTK}" = "yes" ]; then
    HARBOUR_LIBS="\${HARBOUR_LIBS} -lxbgtk"
    HB_PATHS="\${HB_PATHS} -I\`PKG_CONFIG_PATH=/usr/local/lib/pkgconfig pkg-config --variable=xbgtkincludedir xbgtk\`"
fi
if [ "\${HB_XHGTK}" = "yes" ]; then
    HARBOUR_LIBS="\${HARBOUR_LIBS} -lxhgtk"
fi
if [ "\${HB_HWGUI}" = "yes" ]; then
    HARBOUR_LIBS="\${HARBOUR_LIBS} -lhwgui -lprocmisc -lhbxml"
fi
if [ "\${HB_ARCHITECTURE}" = "darwin" ] || \\
   [ "\${HB_ARCHITECTURE}" = "sunos" ] || \\
   [ "\${HB_ARCHITECTURE}" = "hpux" ]; then
    HARBOUR_LIBS="\${HARBOUR_LIBS} \${HARBOUR_LIBS}"
else
    HARBOUR_LIBS="-Wl,--start-group \${HARBOUR_LIBS} -Wl,--end-group"
fi

l=""
if [ "\${HB_COMPILER}" = "mingw" ]; then
    if [ -z "\${HB_MODE}" ]; then
        LN_OPT="\${LN_OPT} -mwindows"
        l="hbmainwin"
    elif [ "\${HB_MODE}" = "gui" ]; then
        l="hbmainwin"
    elif [ "\${HB_MODE}" = "std" ]; then
        l="hbmainstd"
    fi
elif [ "\${HB_COMPILER}" = "mingwce" ]; then
    if [ "\${HB_MODE}" = "std" ]; then
        l="hbmainstd"
    else
        l="hbmainwin"
    fi
fi
if [ -n "\${l}" ]; then
    [ "\${HB_MT}" = "MT" ] && [ -f "\${HB_LIB_INSTALL}/lib\${l}mt.a" ] && l="\${l}mt"
    [ -f "\${HB_LIB_INSTALL}/lib\${l}.a" ] && HARBOUR_LIBS="\${HARBOUR_LIBS} -l\${l}"
fi

l="hbfm"
[ "\${HB_MT}" = "MT" ] && [ -f "\${HB_LIB_INSTALL}/lib\${l}mt.a" ] && l="\${l}mt"
if [ -f "\${HB_LIB_INSTALL}/lib\${l}.a" ] && \\
   ( [ -n "\${HB_FM_REQ}" ] || [ "\${HB_STATIC}" = "yes" ] ) && \\
   ( [ "\${HB_COMPILER}" != "mingwce" ] || [ "\${HB_FM_REQ}" = "STAT" ] ); then
    if [ "\${HB_STATIC}" = "yes" ] && [ "\${HB_FM_REQ}" = "STAT" ]; then
        HARBOUR_LIBS="-l\${l} \${HARBOUR_LIBS}"
    else
        HARBOUR_LIBS="\${HARBOUR_LIBS} -l\${l}"
    fi
fi

if [ "\${HB_ARCHITECTURE}" = "darwin" ]; then
    CC_OPT="\${CC_OPT} -no-cpp-precomp -Wno-long-double"
elif [ "\${HB_ARCHITECTURE}" = "sunos" ]; then
    HB_STRIP="no"
fi

FOUTC="\${DIROUT}/\${FILEOUT%.*}.c"
FOUTO="\${DIROUT}/\${FILEOUT%.*}.o"
FOUTE="\${DIROUT}/\${FILEOUT%.[Pp][Rr][Gg]}"
FOUTE="\${FOUTE%.[oc]}"
FOUTE="\${FOUTE%${hb_exesuf}}${hb_exesuf}"

hb_cc()
{
    local LNK_OPT P n

    LNK_OPT="\${LN_OPT} \${HB_USRLPATH} \${HB_USRLIBS} \${HARBOUR_LIBS} \${SYSTEM_LIBS}"
    P=( "\$@" ); n=0
    while [ \$n -lt \${#P[@]} ]; do
        if [ "\${P[\$n]}" = "-c" ]; then
            LNK_OPT=""
            n=\${#P[@]}
        fi
        n=\$[\$n + 1]
    done

    ${CCPREFIX}\${HB_CC} "\$@" \${CC_OPT} \${GCC_PATHS} \${LNK_OPT}
}

hb_cmp()
{
    ${hb_cmpname} "\$@" \${HB_OPT} \${HB_PATHS} && \\
    ( [ "\${HB_GEN//c/}" != "" ] || \\
    ( [ -f "\${FOUTC}" ] && \\
    hb_cc -c "\${FOUTC}" -o "\${FOUTO}" && \\
    ( [ "\${HB_GEN}" = "c" ] || rm -f "\${FOUTC}" ) ) )
}

hb_link()
{
    if [ -n "\${HB_MAIN_FUNC}" ]; then
        HB_MAIN_FUNC="@\${HB_MAIN_FUNC}"
    elif [ "\${HB_COMPILER}" != "djgpp" ] && [ -f "\${FOUTO}" ]; then
        HB_MAIN_FUNC=\`hb_lnk_main "\${FOUTO}"\`
    fi
    if [ -n "\${HB_LNK_REQ}" ] || [ -n "\${HB_GT_REQ}" ] || [ -n "\${HB_MAIN_FUNC}" ]; then
        hb_lnk_request > \${_TMP_FILE_}
        LN_OPT="\${_TMP_FILE_} \${LN_OPT}"
    fi
    hb_cc "\$@" -o "\${FOUTE}"
}

hb_lnk_request()
{
    echo "#include \\"hbapi.h\\""
    if [ -n "\${HB_LNK_REQ}" ]; then
        for fn in \${HB_LNK_REQ}; do
            echo "HB_FUNC_EXTERN( \${fn} );"
        done
        echo "void _hb_lnk_ForceLink_build( void )"
        echo "{"
        for fn in \${HB_LNK_REQ}; do
            echo "   HB_FUNC_EXEC( \${fn} );"
        done
        echo "}"
    fi
    gt="\${HB_GT_REQ%% *}"
    if [ -n "\$gt" ] || [ -n "\${HB_MAIN_FUNC}" ]; then
        echo "#include \\"hbinit.h\\""
        echo "HB_EXTERN_BEGIN"
        echo "extern \${HB_LNK_ATTR} const char * hb_gt_szNameDefault;"
        echo "extern \${HB_LNK_ATTR} void hb_vmSetLinkedMain( const char * szMain );"
        echo "HB_EXTERN_END"
        echo "HB_CALL_ON_STARTUP_BEGIN( hb_lnk_SetDefault_build )"
        if [ -n "\$gt" ]; then
            echo "   hb_gt_szNameDefault = \\"\$gt\\";"
        fi
        if [ -n "\${HB_MAIN_FUNC}" ]; then
            echo "   hb_vmSetLinkedMain( \\"\${HB_MAIN_FUNC}\\" );"
        fi
        echo "HB_CALL_ON_STARTUP_END( hb_lnk_SetDefault_build )"
    fi
}

hb_lnk_main()
{
    (${CCPREFIX}nm \$1 -g -n --defined-only -C|sed -e '/ HB_FUN_/ ! d' -e 's/^[0-9a-fA-F]* T HB_FUN_\([A-Z0-9_]*\).*/\1/'|head -1|grep -v '^MAIN\$')2>/dev/null
#    (${CCPREFIX}nm \$1 -n --defined-only|sed -e '/HB_FUN_/ ! d' -e 's/^[0-9a-fA-F]* [Tt] HB_FUN_//'|head -1|grep -v '^MAIN\$')2>/dev/null
}

hb_cleanup()
{
    rm -f "\${_TMP_FILE_}"
}

trap hb_cleanup EXIT &>/dev/null

case "\${HB}" in
    *cc)
        hb_cc "\${P[@]}"
        ;;
    *cmp)
        hb_cmp "\${P[@]}"
        ;;
    *lnk)
        hb_link "\${P[@]}" && \\
        ( [ "\${HB_STRIP}" != "yes" ] || ${CCPREFIX}strip "\${FOUTE}" )
        ;;
    *mk)
        hb_cmp "\${P[@]}" && \\
        hb_link "\${FOUTO}" && \\
        ( [ "\${HB_STRIP}" != "yes" ] || ${CCPREFIX}strip "\${FOUTE}" ) && \\
        rm -f "\${FOUTO}"
        ;;
esac
EOF
    chmod 755 ${hb_tool}
    echo "Creating links..."
    ${hb_tool} mk-links
}

mk_hblibso()
{
    local LIBS LIBSMT l lm ll dir hb_rootdir hb_ver hb_libs full_lib_name full_lib_name_mt linker_options

    dir=`pwd`
    name=`get_solibname`
    hb_rootdir="${1-.}"

    hb_ver=`get_hbver "${hb_rootdir}"`
    hb_libs=`mk_hbgetlibs "$2"`
    [ -z "${HB_GT_LIB}" ] && HB_GT_LIB="gtstd"

    (cd $HB_LIB_INSTALL
    LIBS=""
    LIBSMT=""
    for l in ${hb_libs}
    do
        case $l in
            hbdebug|profiler|hbcplr|hbfm|hbodbc|gtalleg|rddads) ;;
            *)
                ls="lib${l}.a"
                if [ -f lib${l}mt.a ]
                then
                    lm="lib${l}mt.a"
                else
                    lm="${ls}"
                fi
                if [ -f $ls ]
                then
                    LIBS="$LIBS $ls"
                fi
                if [ -f $lm ]
                then
                    LIBSMT="$LIBSMT $lm"
                fi
                if [ "${HB_ARCHITECTURE}" = "darwin" ]; then
                    if [ "${l}" = gtcrs ]; then
                        linker_options="$linker_options -lncurses"
                    elif [ "${l}" = gtsln ]; then
                        if [ "${HB_WITHOUT_GTSLN}" != "yes" ]; then
                            linker_options="$linker_options -lslang"
                        fi
                    fi
                fi
                ;;
        esac
    done
    if [ "${HB_ARCHITECTURE}" = "darwin" ]; then
        lib_ext=".dylib"
        full_lib_name="lib${name}.${hb_ver}${lib_ext}"
        full_lib_name_mt="lib${name}mt.${hb_ver}${lib_ext}"
        linker_options="-L/sw/lib -L/opt/local/lib $linker_options"
    elif [ "${HB_ARCHITECTURE}" = "win" ]; then
        lib_ext=".dll"
        full_lib_name="${name}${lib_ext}"
        full_lib_name_mt="${name}mt${lib_ext}"
    elif [ "${HB_ARCHITECTURE}" = "hpux" ]; then
        lib_ext=".sl"
        full_lib_name="lib${name}-${hb_ver}${lib_ext}"
        full_lib_name_mt="lib${name}mt-${hb_ver}${lib_ext}"
    else
        lib_ext=".so"
        full_lib_name="lib${name}-${hb_ver}${lib_ext}"
        full_lib_name_mt="lib${name}mt-${hb_ver}${lib_ext}"
    fi
    if [ -n "${HB_TOOLS_PREF}" ]; then
        hb_mkdyn="${HB_BIN_INSTALL}/${HB_TOOLS_PREF}-mkdyn"
    else
        hb_mkdyn="${HB_BIN_INSTALL}/hb-mkdyn"
    fi
    echo "Making ${full_lib_name}..."
    ${hb_mkdyn} ${full_lib_name} $LIBS ${linker_options}
    if [ "${HB_ARCHITECTURE}" != "dos" ]; then
        echo "Making ${full_lib_name_mt}..."
        ${hb_mkdyn} ${full_lib_name_mt} $LIBSMT ${linker_options}
    fi
    for l in ${full_lib_name} ${full_lib_name_mt}
    do
        if [ -f $l ]
        then
            if [ "${HB_ARCHITECTURE}" = "win" ]; then
                if [ "${HB_XBUILD}" = "" ]; then
                   (cd "$dir"
                   mv "${HB_LIB_INSTALL}/$l" "${HB_BIN_INSTALL}")
                fi
            else
                if [ "${HB_ARCHITECTURE}" = "darwin" ]; then
                    ll=${l%.${hb_ver}${lib_ext}}${lib_ext}
                else
                    ll=${l%-${hb_ver}${lib_ext}}${lib_ext}
                    ln -sf $l $ll
                fi
                case $HB_LIB_INSTALL in
                    */usr/lib/*|*/usr/lib64/*|*/usr/local/lib/*|*/usr/local/lib64/*)
                        ln -sf ${name}/$l ../$ll
                        ;;
                    */usr/local/*)
                        mkdir -p ../../lib
                        ln -sf ../${name}/lib/$l ../../lib/$ll
                        ;;
                    *)
                        ;;
                esac
            fi
        fi
    done
    )
    #export LD_LIBRARY_PATH="$HB_LIB_INSTALL:$LD_LIBRARY_PATH"
}
