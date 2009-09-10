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
# See COPYING for licensing terms.
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
    local FVER MAJOR MINOR RELEA hb_rootdir

    hb_rootdir="${1-.}"
    FVER="${hb_rootdir}/include/hbver.h"
    MAJOR=`sed -e '/HB_VER_MAJOR/   !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${FVER}"`
    MINOR=`sed -e '/HB_VER_MINOR/   !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${FVER}"`
    RELEA=`sed -e '/HB_VER_RELEASE/ !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${FVER}"`
    echo "${MAJOR}.${MINOR}.${RELEA}"
}

get_hbver_win()
{
    local FVER MAJOR MINOR hb_rootdir

    hb_rootdir="${1-.}"
    FVER="${hb_rootdir}/include/hbver.h"
    MAJOR=`sed -e '/HB_VER_MAJOR/   !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${FVER}"`
    MINOR=`sed -e '/HB_VER_MINOR/   !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${FVER}"`
    echo "${MAJOR}${MINOR}"
}

get_hbver_so()
{
    local hb_ver hb_rootdir

    hb_rootdir="${1-.}"
    if [ "${HB_PLATFORM}" = "win" ] || \
       [ "${HB_PLATFORM}" = "wce" ]; then
        hb_ver=`get_hbver_win "${hb_rootdir}"`
        if [ "${HB_COMPILER}" = "mingw64" ]; then
            hb_ver="${hb_ver}-x64"
        elif [ "${HB_COMPILER}" = "mingwarm" ]; then
            hb_ver="${hb_ver}-wce-arm"
        fi
    else
        hb_ver=`get_hbver "${hb_rootdir}"`
    fi
    echo "${hb_ver}"
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
        if [ "$HB_PLATFORM" != "wce" ]
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
        if [ "$HB_PLATFORM" = "wce" ]
        then
            libs="$libs gtwin"
        fi
        echo "$libs hbct hbnf hbmzip hbnetio hbtip xhb hbgd hbfimage rddsql sddfb sddmy sddpg hbodbc hbpgsql hbmysql hbfbird rddads rddado hbhpdf hbvpdf hbcurl hbwin gtwvg gtalleg hbsqlit3 hbbtree $HB_USER_LIBS"
        #"hbgf hbgt hbbmcdx hbmisc hbtpathy hbwhat hbziparc hbmsql"
    else
        echo "$@"
    fi
}

mk_hbtools()
{
    local name hb_pref hb_tool hb_libs hb_libsc hb_hbmkcfg hb_mkdef hb_gt_ori hb_ccpath

    name=`get_solibname`
    hb_pref="$4"
    [ -z "${hb_pref}" ] && hb_pref="${HB_TOOLS_PREF-hb}"
    hb_cmpname="${HB_CMPNAME-harbour}"
    if [ "${HB_PLATFORM}" = "dos" ]; then
        hb_tool="$1/${hb_pref}-bld"
        hb_path_separator=";"
        hb_static="yes"
        hb_static_default=" (default)"
        hb_exesuf=".exe"
    elif [ "${HB_PLATFORM}" = "win" ] || \
         [ "${HB_PLATFORM}" = "wce" ]; then
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
    elif [ "${HB_PLATFORM}" = "darwin" ]; then
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
    if [ "${HB_COMPILER}" = "mingw" ] || \
       [ "${HB_COMPILER}" = "mingw64" ]; then
        HB_SYS_LIBS="${HB_SYS_LIBS} -luser32 -lwinspool -lgdi32 -lcomctl32 -ladvapi32 -lcomdlg32 -lole32 -loleaut32 -luuid -lws2_32"
        HB_INC_X11="no"
    elif [ "${HB_COMPILER}" = "mingwarm" ]; then
        HB_SYS_LIBS="${HB_SYS_LIBS} -lwininet -lws2 -lcommdlg -lcommctrl -luuid -lole32 -loleaut32"
        HB_INC_X11="no"
    elif [ "${HB_COMPILER}" = "djgpp" ]; then
        HB_SYS_LIBS="${HB_SYS_LIBS}"
        HB_INC_X11="no"
    else
        HB_CRS_LIB=""
        if [ "${HB_PLATFORM}" = "linux" ]; then
            HB_SYS_LIBS="${HB_SYS_LIBS} -ldl -lrt"
        elif [ "${HB_PLATFORM}" = "sunos" ]; then
            HB_SYS_LIBS="${HB_SYS_LIBS} -lrt"
            HB_SYS_LIBS="${HB_SYS_LIBS} -lsocket -lnsl -lresolv"
            HB_CRS_LIB="curses"
        elif [ "${HB_PLATFORM}" = "hpux" ]; then
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
    hb_mkdef="{${HB_PLATFORM}&${HB_COMPILER}}"
    echo "Making ${hb_hbmkcfg}... "
    echo "# hbmk2 configuration"> ${hb_hbmkcfg}
    echo "# Generated by Harbour build process">> ${hb_hbmkcfg}
    echo "">> ${hb_hbmkcfg}
    echo "libpaths=../contrib/%{hb_name}">> ${hb_hbmkcfg}
    echo "libpaths=../contrib/rddsql/%{hb_name}">> ${hb_hbmkcfg}
    echo "libpaths=../addons/%{hb_name}">> ${hb_hbmkcfg}
    echo "libpaths=../examples/%{hb_name}">> ${hb_hbmkcfg}
    echo "">> ${hb_hbmkcfg}
    if [ -n "${hb_gt_ori}" ]; then
        echo "gtdef=${hb_mkdef}${hb_gt_ori}">> ${hb_hbmkcfg}
    fi
    if [ -n "${CC_HB_USER_CFLAGS}" ]; then
        echo "cflags=${hb_mkdef}${CC_HB_USER_CFLAGS}">> ${hb_hbmkcfg}
    fi
    if [ -n "${CC_HB_USER_LDFLAGS}" ]; then
        echo "ldflags=${hb_mkdef}${CC_HB_USER_LDFLAGS}">> ${hb_hbmkcfg}
    fi
    if [ -n "${CC_HB_USER_LIBS}" ]; then
        echo "libs=${hb_mkdef}${CC_HB_USER_LIBS}">> ${hb_hbmkcfg}
    fi
    if [ -n "${HB_HAS_GPM}" ]; then
        echo "syslibs=${hb_mkdef}gpm">> ${hb_hbmkcfg}
    fi

    hb_ccpath="${HB_CCPATH%/}"
    [ -z "${hb_ccpath}" ] || hb_ccpath="${hb_ccpath}${hb_path_separator}"

    echo "Making ${hb_tool}... "
    cat > ${hb_tool} <<EOF
#!/bin/sh
[ "\$BASH" ] || exec bash \`which \$0\` \${1+"\$@"}
#
# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@priv.onet.pl>
# simple script to build binaries .tgz from Harbour sources
#
# See COPYING for licensing terms.
# ---------------------------------------------------------------
#

# set environment variables
export HB_PLATFORM="${HB_PLATFORM}"
export HB_COMPILER="${HB_COMPILER}"
[ -z "\${HB_BIN_INSTALL}" ] && export HB_BIN_INSTALL="${_DEFAULT_BIN_DIR}"
[ -z "\${HB_INC_INSTALL}" ] && export HB_INC_INSTALL="${_DEFAULT_INC_DIR}"
[ -z "\${HB_LIB_INSTALL}" ] && export HB_LIB_INSTALL="${_DEFAULT_LIB_DIR}"

# be sure that ${name} binaries are in your path
export PATH="\${HB_BIN_INSTALL}${hb_path_separator}${hb_ccpath}\${PATH}"

if [ "\${HB_COMPILER}" == "gpp" ]; then
   HB_CC="g++"
elif [ "\${HB_COMPILER}" == "icc" ]; then
   HB_CC="icc"
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
            if [ "\${HB_PLATFORM}" = "dos" ]; then
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

if [ -z "$HB_INC_GPM" ]; then
    if [ "$HB_PLATFORM" = "linux" ] && \\
       ( [ -f /usr/include/gpm.h ] || [ -f /usr/local/include/gpm.h ]); then
        HB_INC_GPM=yes
    else
        HB_INC_GPM=no
    fi
    export HB_INC_GPM
fi

if [ -z "${HB_INC_SLANG}" ]; then
    HB_INC_SLANG=no
    case "$HB_PLATFORM" in
        linux|bsd|darwin|hpux|sunos)
            for dir in /usr /usr/local /sw /opt/local
            do
                if [ -f ${dir}/include/slang.h ] || \\
                   [ -f ${dir}/include/slang/slang.h ]; then
                    HB_INC_SLANG=yes
                fi
            done
            ;;
    esac
fi

if [ -z "${HB_INC_CURSES}" ]; then
    HB_INC_CURSES=no
    case "$HB_PLATFORM" in
        linux|bsd|darwin|hpux|sunos)
            for dir in /usr /usr/local /sw /opt/local
            do
                if [ -f ${dir}/include/curses.h ]; then
                    HB_INC_CURSES=yes
                fi
            done
            ;;
    esac
fi

if [ -z "$HB_COMMERCE" ]; then
    HB_COMMERCE=no;
fi

if [ "$HB_COMMERCE" = yes ]; then
    HB_INC_GPM=no
    HB_INC_SLANG=no
fi

HB_GT_REQ=""
HB_STRIP="yes"
HB_MAIN_FUNC=""
HB_XBGTK=""
HB_XHGTK=""
HB_HWGUI=""
HB_USRLIBS=""
HB_USRLPATH=""
HB_GEN=""
HB_MODE=""
HB_EXIT=""
LN_OPT="${CC_HB_USER_LDFLAGS}"
CC_OPT="-O3 ${CC_HB_USER_CFLAGS}"
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
        -strip)      HB_STRIP="yes" ;;
        -nostrip)    HB_STRIP="no" ;;
        -l[^-]*)     HB_USRLIBS="\${HB_USRLIBS} \${v}" ;;
        -L[^-]*)     HB_USRLPATH="\${HB_USRLPATH} \${v}" ;;
        -I*)         [ \${HB} = "cc" ] || CC_OPT="\${CC_OPT} \${v}"; p="\${v}" ;;
        -mwindows)   LN_OPT="\${LN_OPT} \${v}"; HB_MODE="gui" ;;
        -mconsole)   LN_OPT="\${LN_OPT} \${v}"; HB_MODE="std" ;;
        -main=*)     HB_MAIN_FUNC="\${v#*=}" ;;
        -g[cohwij])  HB_GEN="\${v#-g}"; p="\${v}" ;;
        -gc[0-9])    HB_GEN="c"; p="\${v}" ;;
        -go[0-9])    HB_GEN="o"; p="\${v}" ;;
        --hbdirlib)  echo "\${HB_LIB_INSTALL}"; HB_EXIT="yes" ;;
        --hbdirinc)  echo "\${HB_INC_INSTALL}"; HB_EXIT="yes" ;;
        --hbdirbin)  echo "\${HB_BIN_INSTALL}"; HB_EXIT="yes" ;;
        -*)          p="\${v}" ;;
        *)           [ -z \${FILEOUT} ] && FILEOUT="\${v##*/}"; p="\${v}" ;;
    esac
    [ -n "\$p" ] && PP[\$n]="\$p"
    n=\$[\$n + 1]
done
P=( "\${PP[@]}" )

[ "\${P}" != "" ] && HB_EXIT="no"
[ "\${HB_EXIT}" = "yes" ] && exit

case "\${HB_MT}" in
    [Mm][Tt]|[Yy][Ee][Ss]|1)  HB_MT="MT";;
    *)  HB_MT="";;
esac

SYSTEM_LIBS="${HB_SYS_LIBS}"
# use pthread system library for MT programs
if [ "\${HB_MT}" = "MT" ]; then
    case "\${HB_PLATFORM}" in
        dos|win|wce|os2)
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
    if [ "\${HB_PLATFORM}" = "darwin" ]; then
        SYSTEM_LIBS="\${SYSTEM_LIBS} -L/sw/lib -L/opt/local/lib"
    elif [ "\${HB_PLATFORM}" = "bsd" ]; then
        SYSTEM_LIBS="\${SYSTEM_LIBS} -L/usr/local/lib"
    fi
    SYSTEM_LIBS="-l${HB_SLN_LIB:-slang} \${SYSTEM_LIBS}"
    [ "\${HB_INC_GPM}" != "no" ] && HB_GPM_LIB="gpm"
fi
if [ -f "\${HB_LIB_INSTALL}/libgtcrs.a" ]; then
    SYSTEM_LIBS="-l${HB_CRS_LIB:-ncurses} \${SYSTEM_LIBS}"
    [ "\${HB_INC_GPM}" != "no" ] && HB_GPM_LIB="gpm"
fi
if [ "\${HB_INC_X11}" != "no" ]; then
    if [ -f "\${HB_LIB_INSTALL}/libgtxvt.a" ] || [ -f "\${HB_LIB_INSTALL}/libgtxwc.a" ]; then
        [ -d "/usr/X11R6/lib64" ] && SYSTEM_LIBS="\${SYSTEM_LIBS} -L/usr/X11R6/lib64"
        SYSTEM_LIBS="-L/usr/X11R6/lib -lX11 \${SYSTEM_LIBS}"
    fi
fi
[ -n "\${HB_GPM_LIB}" ] && SYSTEM_LIBS="-l\${HB_GPM_LIB} \${SYSTEM_LIBS}"


if [ "\${HB_STATIC}" = "no" ] && \\
   [ "\${HB_PLATFORM}" != "win" ] && \\
   [ "\${HB_PLATFORM}" != "wce" ]; then
    SYSTEM_LIBS=""
fi

if [ "\${HB_XBGTK}" = "yes" ]; then
    SYSTEM_LIBS="\${SYSTEM_LIBS} \`pkg-config --libs gtk+-2.0\`"
elif [ "\${HB_XHGTK}" = "yes" ]; then
    SYSTEM_LIBS="\${SYSTEM_LIBS} \`pkg-config --libs gtk+-2.0 libglade-2.0\`"
elif [ "\${HB_HWGUI}" = "yes" ]; then
    SYSTEM_LIBS="\${SYSTEM_LIBS} \`pkg-config --libs gtk+-2.0 --libs libgnomeprint-2.2\`"
fi

if [ "\${HB_STATIC}" = "full" ]; then
    if [ "\${HB_PLATFORM}" = "linux" ]; then
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

HB_LNK_ATTR=""
HARBOUR_LIBS=""

l=""
HB_MAIN_REQ=""
if [ "\${HB_COMPILER}" = "mingw" ] || [ "\${HB_COMPILER}" = "mingw64" ]; then
    if [ -z "\${HB_MODE}" ]; then
        LN_OPT="\${LN_OPT} -mwindows"
        l="hbmainwin"
        HB_MAIN_REQ="hb_forceLinkMainWin"
    elif [ "\${HB_MODE}" = "gui" ]; then
        l="hbmainwin"
        HB_MAIN_REQ="hb_forceLinkMainWin"
    elif [ "\${HB_MODE}" = "std" ]; then
        l="hbmainstd"
    fi
elif [ "\${HB_COMPILER}" = "mingwarm" ]; then
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

if [ "\${HB_STATIC}" = "yes" ]; then
    libs="${hb_libs} ${hb_libsc}"
else
    l="${name}"
    ver=""
    if [ "\${HB_PLATFORM}" = "darwin" ]; then
        pref="lib"
        ext=".dylib"
        LN_OPT="\${LN_OPT} -bind_at_load -multiply_defined suppress"
    elif [ "\${HB_PLATFORM}" = "win" ] || \\
         [ "\${HB_PLATFORM}" = "wce" ]; then
        pref=""
        ext=".dll"
        ver=`get_hbver_so`
        HB_LNK_ATTR="__attribute__ ((dllimport))"
    elif [ "\${HB_PLATFORM}" = "hpux" ]; then
        pref="lib"
        ext=".sl"
    else
        pref="lib"
        ext=".so"
    fi
    if [ "\${HB_MT}" = "MT" ]; then
        if [ -f "\${HB_LIB_INSTALL}/\${pref}\${l}mt\${ext}" ] || \\
           [ -f "\${HB_LIB_INSTALL}/lib\${l}mt.a" ]; then
            HARBOUR_LIBS="\${HARBOUR_LIBS} -l\${l}mt"
            l=""
        elif [ -f "\${HB_BIN_INSTALL}/\${pref}\${l}mt\${ext}" ]; then
            HARBOUR_LIBS="\${HARBOUR_LIBS} -L\${HB_BIN_INSTALL} -l\${l}mt"
            l=""
        elif [ -n "\${ver}" ]; then
            if [ -f "\${HB_LIB_INSTALL}/\${pref}\${l}mt-\${ver}\${ext}" ] || \\
               [ -f "\${HB_LIB_INSTALL}/lib\${l}mt-\${ver}.a" ]; then
                HARBOUR_LIBS="\${HARBOUR_LIBS} -l\${l}mt-\${ver}"
                l=""
            elif [ -f "\${HB_BIN_INSTALL}/\${pref}\${l}mt-\${ver}\${ext}" ]; then
                HARBOUR_LIBS="\${HARBOUR_LIBS} -L\${HB_BIN_INSTALL} -l\${l}mt-\${ver}"
                l=""
            fi
        fi
    fi
    if [ -n "\${l}" ]; then
        if [ -f "\${HB_LIB_INSTALL}/\${pref}\${l}\${ext}" ] || \\
           [ -f "\${HB_LIB_INSTALL}/lib\${l}.a" ]; then
            HARBOUR_LIBS="\${HARBOUR_LIBS} -l\${l}"
        elif [ -f "\${HB_BIN_INSTALL}/\${pref}\${l}\${ext}" ]; then
            HARBOUR_LIBS="\${HARBOUR_LIBS} -L\${HB_BIN_INSTALL} -l\${l}"
        elif [ -n "\${ver}" ]; then
            if [ -f "\${HB_LIB_INSTALL}/\${pref}\${l}-\${ver}\${ext}" ] || \\
               [ -f "\${HB_LIB_INSTALL}/lib\${l}-\${ver}.a" ]; then
                HARBOUR_LIBS="\${HARBOUR_LIBS} -l\${l}-\${ver}"
            elif [ -f "\${HB_BIN_INSTALL}/\${pref}\${l}-\${ver}\${ext}" ]; then
                HARBOUR_LIBS="\${HARBOUR_LIBS} -L\${HB_BIN_INSTALL} -l\${l}-\${ver}"
            fi
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

if [ "\${HB_PLATFORM}" = "darwin" ] || \\
   [ "\${HB_PLATFORM}" = "sunos" ] || \\
   [ "\${HB_PLATFORM}" = "hpux" ]; then
    HARBOUR_LIBS="\${HARBOUR_LIBS} \${HARBOUR_LIBS}"
else
    HARBOUR_LIBS="-Wl,--start-group \${HARBOUR_LIBS} -Wl,--end-group"
fi

if [ "\${HB_PLATFORM}" = "darwin" ]; then
    CC_OPT="\${CC_OPT} -no-cpp-precomp -Wno-long-double"
elif [ "\${HB_PLATFORM}" = "sunos" ]; then
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

    ${HB_CCPREFIX}\${HB_CC} "\$@" \${CC_OPT} \${GCC_PATHS} \${LNK_OPT}
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
    if [ -n "\${HB_LNK_REQ}" ] || [ -n "\${HB_GT_REQ}" ] || \\
       [ -n "\${HB_MAIN_FUNC}" ] || [ -n "\${HB_MAIN_REQ}" ]; then
        hb_lnk_request > \${_TMP_FILE_}
        LN_OPT="\${_TMP_FILE_} \${LN_OPT}"
    fi
    hb_cc "\$@" -o "\${FOUTE}"
}

hb_lnk_request()
{
    echo "#include \\"hbapi.h\\""
    if [ -n "\${HB_LNK_REQ}" ] || [ -n "\${HB_MAIN_REQ}" ]; then
        for fn in \${HB_LNK_REQ}; do
            echo "HB_FUNC_EXTERN( \${fn} );"
        done
        [ -z "\${HB_MAIN_REQ}" ] || echo "HB_EXTERN_C \${HB_MAIN_REQ}( void );"
        echo "void _hb_lnk_ForceLink_build( void )"
        echo "{"
        for fn in \${HB_LNK_REQ}; do
            echo "   HB_FUNC_EXEC( \${fn} );"
        done
        [ -z "\${HB_MAIN_REQ}" ] || echo "   \${HB_MAIN_REQ}();"
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
            if [ \${HB_MAIN_FUNC} != \${HB_MAIN_FUNC/x/y} ]; then
               HB_MAIN_FUNC=\`echo "\${HB_MAIN_FUNC}"|sed -e 's/x\\(..\\)/\\\\\\\\x\\1" "/'\`
            fi
            echo "   hb_vmSetLinkedMain( \\"\${HB_MAIN_FUNC}\\" );"
        fi
        echo "HB_CALL_ON_STARTUP_END( hb_lnk_SetDefault_build )"
    fi
}

hb_lnk_main()
{
    (${HB_CCPREFIX}nm \$1 -g -n --defined-only -C|sed -e '/ HB_FUN_/ ! d' -e 's/^[0-9a-fA-F]* T HB_FUN_\([A-Z0-9_]*\).*/\1/'|head -1|grep -v '^MAIN\$')2>/dev/null
#    (${HB_CCPREFIX}nm \$1 -n --defined-only|sed -e '/HB_FUN_/ ! d' -e 's/^[0-9a-fA-F]* [Tt] HB_FUN_//'|head -1|grep -v '^MAIN\$')2>/dev/null
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
        ( [ "\${HB_STRIP}" != "yes" ] || ${HB_CCPREFIX}strip "\${FOUTE}" )
        ;;
    *mk)
        hb_cmp "\${P[@]}" && \\
        hb_link "\${FOUTO}" && \\
        ( [ "\${HB_STRIP}" != "yes" ] || ${HB_CCPREFIX}strip "\${FOUTE}" ) && \\
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
    local LIBS LIBSMT l lm ll dir hb_rootdir hb_ver hb_libs full_lib_name full_lib_name_mt linker_options linker_mtoptions gpm lib_ext lib_pref lib_suff

    dir=`pwd`
    name=`get_solibname`
    hb_rootdir="${1-.}"

    hb_ver=`get_hbver_so "${hb_rootdir}"`
    hb_libs=`mk_hbgetlibs "$2"`
    [ -z "${HB_GT_LIB}" ] && HB_GT_LIB="gtstd"

    (cd $HB_LIB_INSTALL
    LIBS=""
    LIBSMT=""
    gpm="${HB_HAS_GPM}"
    linker_options="-lm"
    linker_mtoptions=""
    if [ "${HB_USER_CFLAGS//-DHB_PCRE_REGEX/}" != "${HB_USER_CFLAGS}" ]; then
        linker_options="-lpcre ${linker_options}"
        hb_libs="${hb_libs//hbpcre/}"
    elif [ "${HB_USER_CFLAGS//-DHB_POSIX_REGEX/}" != "${HB_USER_CFLAGS}" ]; then
        hb_libs="${hb_libs//hbpcre/}"
    fi
    if [ "${HB_USER_CFLAGS//-DHB_EXT_ZLIB/}" != "${HB_USER_CFLAGS}" ]; then
        linker_options="-lz ${linker_options}"
        hb_libs="${hb_libs//hbzlib/}"
    fi
    if [ "${HB_COMPILER}" = "mingw" ] || [ "${HB_COMPILER}" = "mingw64" ]; then
        linker_options="${linker_options} -luser32 -lwinspool -lgdi32 -lcomctl32 -ladvapi32 -lcomdlg32 -lole32 -loleaut32 -luuid -lws2_32"
    elif [ "${HB_COMPILER}" = "mingwarm" ]; then
        linker_options="${linker_options} -lwininet -lws2 -lcommdlg -lcommctrl -luuid -lole32 -loleaut32"
    elif [ "${HB_COMPILER}" = "djgpp" ]; then
        linker_options="${linker_options}"
    elif [ "${HB_PLATFORM}" = "linux" ]; then
        linker_options="${linker_options} -ldl -lrt"
        linker_mtoptions="${linker_mtoptions} -lpthread"
    elif [ "${HB_PLATFORM}" = "sunos" ]; then
        linker_options="${linker_options} -lrt -lsocket -lnsl -lresolv"
        linker_mtoptions="${linker_mtoptions} -lpthread"
    elif [ "${HB_PLATFORM}" = "hpux" ]; then
        linker_options="${linker_options} -lrt"
        linker_mtoptions="${linker_mtoptions} -lpthread"
    elif [ "${HB_PLATFORM}" = "bsd" ]; then
        linker_options="$-L/usr/local/lib {linker_options}"
        linker_mtoptions="${linker_mtoptions} -lpthread"
    elif [ "${HB_PLATFORM}" = "darwin" ]; then
        linker_options="-L/sw/lib -L/opt/local/lib ${linker_options}"
        linker_mtoptions="${linker_mtoptions} -lpthread"
    fi

    for l in ${hb_libs}
    do
        case $l in
            hbdebug|profiler|hbcplr|hbodbc|gtalleg|rddads) ;;
            *)
                ls="lib${l}.a"
                if [ -f lib${l}mt.a ]
                then
                    lm="lib${l}mt.a"
                else
                    lm="${ls}"
                fi
                if [ -f $lm ]
                then
                    LIBSMT="$LIBSMT $lm"
                fi
                if [ -f $ls ]
                then
                    LIBS="$LIBS $ls"
                    if [ "${l}" = gtcrs ]; then
                        if [ "${HB_PLATFORM}" = "sunos" ]; then
                            linker_options="$linker_options -lcurses"
                        else
                            linker_options="$linker_options -lncurses"
                        fi
                    elif [ "${l}" = gtsln ]; then
                        if [ "${HB_INC_SLANG}" != "no" ]; then
                            linker_options="$linker_options -lslang"
                        fi
                    elif [ "${l}" = gtxwc ]; then
                        [ -d "/usr/X11R6/lib" ] && \
                           linker_options="$linker_options -L/usr/X11R6/lib"
                        [ -d "/usr/X11R6/lib64" ] && \
                           linker_options="$linker_options -L/usr/X11R6/lib64"
                        linker_options="$linker_options -lX11"
                    fi
                    if [ -n "${gpm}" ] && ( [ "${l}" = gtcrs ] || \
                       [ "${l}" = gtsln ] || [ "${l}" = gttrm ] ); then
                        linker_options="$linker_options -lgpm"
                        gpm=""
                    fi
                fi
                ;;
        esac
    done
    if [ "${HB_PLATFORM}" = "darwin" ]; then
        lib_ext=".dylib"
        lib_pref="lib"
        lib_suff=".${hb_ver}${lib_ext}"
    elif [ "${HB_PLATFORM}" = "win" ] || \
         [ "${HB_PLATFORM}" = "wce" ]; then
        lib_ext=".dll"
        lib_pref=""
        lib_suff="-${hb_ver}${lib_ext}"
    elif [ "${HB_PLATFORM}" = "hpux" ]; then
        lib_ext=".sl"
        lib_pref="lib"
        lib_suff="-${hb_ver}${lib_ext}"
    else
        lib_ext=".so"
        lib_pref="lib"
        lib_suff="-${hb_ver}${lib_ext}"
    fi
    full_lib_name="${lib_pref}${name}${lib_suff}"
    full_lib_name_mt="${lib_pref}${name}mt${lib_suff}"
    if [ -n "${HB_TOOLS_PREF}" ]; then
        hb_mkdyn="${HB_BIN_INSTALL}/${HB_TOOLS_PREF}-mkdyn"
    else
        hb_mkdyn="${HB_BIN_INSTALL}/hb-mkdyn"
    fi
#   echo "Making ${full_lib_name}..."
#   ${hb_mkdyn} ${full_lib_name} ${LIBS} ${linker_options}
#   if [ "${LIBS}" != "${LIBSMT}" ]; then
#       echo "Making ${full_lib_name_mt}..."
#       ${hb_mkdyn} ${full_lib_name_mt} ${LIBSMT} ${linker_mtoptions} ${linker_options}
#   fi
    for l in ${full_lib_name} ${full_lib_name_mt}
    do
        if [ -f $l ]
        then
            ll=${l%${lib_suff}}${lib_ext}
            ln -sf $l $ll
            if [ "${HB_PLATFORM}" = "win" ] || \
               [ "${HB_PLATFORM}" = "wce" ]; then
                if [ "${HB_PLATFORM}" = "${HB_HOST_PLAT}" ]; then
                   (cd "$dir"
                   mv "${HB_LIB_INSTALL}/$l" "${HB_BIN_INSTALL}"
                   mv "${HB_LIB_INSTALL}/$ll" "${HB_BIN_INSTALL}")
                fi
            else
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
