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

    # please add your distro suffix if it not belong to the one recognized below
    # and remember that order checking can be important

    [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' mandrake-release 2>/dev/null) && echo "mdk$rel"|tr -d "."`
    [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' redhat-release 2>/dev/null) && echo "rh$rel"|tr -d "."`
    [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' fedora-release 2>/dev/null) && echo "fc$rel"|tr -d "."`
    [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' suse-release 2>/dev/null) && echo "fc$rel"|tr -d "."`
    [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' conectiva-release 2>/dev/null) && echo "cl$rel"|tr -d "."`
    [ "${id}" = "" ] && id=`rel=$(rpm -q --queryformat='.%{VERSION}' aurox-release 2>/dev/null) && echo "cl$rel"|tr -d "."`
    [ "${id}" = "" ] && id=`[ -f /etc/pld-release ] && cat /etc/pld-release|sed -e '/1/ !d' -e 's/[^0-9]//g' -e 's/^/pld/'`
    [ "${id}" = "" ] && id=`uname -sr | tr '[A-Z]' '[a-z]' | tr -d " "`
    echo -n "${id}"
}

get_hbver()
{
    local FVER MAJOR MINOR REVIS hb_rootdir

    hb_rootdir="${1-.}"
    FVER="${hb_rootdir}/include/hbver.h"
    MAJOR=`sed -e '/HB_VER_MAJOR/    !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${FVER}"`
    MINOR=`sed -e '/HB_VER_MINOR/    !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${FVER}"`
    REVIS=`sed -e '/HB_VER_REVISION/ !d' -e 's/[^0-9]*\([^ ]*\).*/\1/g' "${FVER}"`
    echo -n "${MAJOR}.${MINOR}.${REVIS}"
}

get_solibname()
{
    local name

    name="${HB_SHAREDLIB_NAME}"
    [ -z "${name}" ] && name="harbour"
    echo -n "${name}"
}

mk_hbgetlibs()
{
    if [ -z "$@" ]
    then
        echo -n "vm pp rtl rdd dbfdbt dbffpt dbfcdx dbfntx ${HB_DB_DRVEXT} macro common lang codepage gtnul gtcrs gtsln gtxvt gtxwc gtalleg gtcgi gtstd gtpca gtwin gtwvt gtdos gtos2 debug profiler"
    else
        echo -n "$@"
    fi
}

mk_hbgetlibsctb()
{
    if [ -z "$@" ]
    then
        echo -n "ct rddads"
    else
        echo -n "$@"
    fi
}

mk_hbtools()
{
    local name hb_pref hb_tool hb_libs hb_libsc

    name=`get_solibname`
    hb_pref="$4"
    [ -z "${hb_pref}" ] && hb_pref="${HB_TOOLS_PREF-hb}"
    hb_cmpname="${HB_CMPNAME-harbour}"
    if [ "${HB_ARCHITECTURE}" = "dos" ]; then
        hb_tool="$1/${hb_pref}-bld"
        hb_path_separator=";"
        hb_static="yes"
        hb_static_default=" (default)"
    else
        hb_tool="$1/${hb_pref}-build"
        hb_path_separator=":"
        hb_static="no"
        hb_shared_default=" (default)"
    fi
    hb_libs=`mk_hbgetlibs "$2"`
    hb_libsc=`mk_hbgetlibsctb "$3"`
    [ -z "${HB_GT_LIB}" ] && HB_GT_LIB="gtstd"

    [ -z "${_DEFAULT_BIN_DIR}" ] && _DEFAULT_BIN_DIR="${HB_BIN_INSTALL}"
    [ -z "${_DEFAULT_INC_DIR}" ] && _DEFAULT_INC_DIR="${HB_INC_INSTALL}"
    [ -z "${_DEFAULT_LIB_DIR}" ] && _DEFAULT_LIB_DIR="${HB_LIB_INSTALL}"

    HB_SYS_LIBS="-lm"
    HB_CRS_LIB=""
    HB_SLN_LIB=""
    if [ "${HB_COMPILER}" = "mingw32" ]; then
        HB_SYS_LIBS="${HB_SYS_LIBS} -luser32 -lwinspool -lgdi32 -lcomctl32 -lcomdlg32 -lole32 -loleaut32 -luuid -lwsock32 -lws2_32"
    elif [ "${HB_COMPILER}" = "djgpp" ]; then
        HB_SYS_LIBS="${HB_SYS_LIBS}"
    else
        if [ "${HB_ARCHITECTURE}" = "sunos" ]; then
            HB_SYS_LIBS="${HB_SYS_LIBS} -lrt"
            HB_CRS_LIB="curses"
        elif [ "${HB_NCURSES_194}" = "yes" ]; then
            HB_CRS_LIB="ncur194"
        else
            HB_CRS_LIB="ncurses"
        fi
        HB_SLN_LIB="slang"
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
        for n in ${hb_pref}cc ${hb_pref}cmp ${hb_pref}mk ${hb_pref}lnk gharbour harbour-link; do
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

## default parameters
HB_STATIC="${hb_static}"
HB_MT=""
HB_GT="${HB_GT_LIB#gt}"
HB_MG="${HB_MULTI_GT}"

HB_GPM_MOUSE="${HB_GPM_MOUSE}"

HB_GT_REQ=""
HB_FM_REQ=""
HB_STRIP="yes"
HB_MAIN_FUNC=""
HB_XBGTK=""
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
                DIROUT="\${d%/*}"; FILEOUT="\${d##*/}"; p="-o\${d%.*}"
            elif [ -n "\${d}" ]; then
                FILEOUT="\${d}"; p="-o\${d%.*}"
            fi ;;
        -static)     HB_STATIC="yes" ;;
        -fullstatic) HB_STATIC="full" ;;
        -shared)     HB_STATIC="no" ;;
        -xbgtk)      HB_XBGTK="yes" ;;
        -mt)         HB_MT="MT" ;;
        -gt*)        HB_GT_REQ="\${HB_GT_REQ} \${v#-gt}" ;;
        -fmstat)     HB_FM_REQ="STAT" ;;
        -nofmstat)   HB_FM_REQ="NOSTAT" ;;
        -strip)      HB_STRIP="yes" ;;
        -nostrip)    HB_STRIP="no" ;;
        -main=*)     HB_MAIN_FUNC="\${v#*=}" ;;
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
    SYSTEM_LIBS="-lpthread \${SYSTEM_LIBS}"
fi

HB_GT_STAT=""
[ -z "\${HB_GT_REQ}" ] && HB_GT_REQ="\${HB_GT}"
if [ "\${HB_MG}" != "yes" ]; then
    if [ "\${HB_STATIC}" = "yes" ] || [ "\${HB_STATIC}" = "full" ]; then
        HB_GT_STAT=\`echo \${HB_GT_REQ}|tr '[A-Z]' '[a-z]'\`
    fi
    HB_GT_REQ=""
else
    HB_GT_REQ=\`echo \${HB_GT_REQ}|tr '[a-z]' '[A-Z]'\`
fi
HB_MAIN_FUNC=\`echo \${HB_MAIN_FUNC}|tr '[a-z]' '[A-Z]'\`

# set environment variables
export HB_ARCHITECTURE="${HB_ARCHITECTURE}"
export HB_COMPILER="${HB_COMPILER}"
[ -z "\${HB_BIN_INSTALL}" ] && export HB_BIN_INSTALL="${_DEFAULT_BIN_DIR}"
[ -z "\${HB_INC_INSTALL}" ] && export HB_INC_INSTALL="${_DEFAULT_INC_DIR}"
[ -z "\${HB_LIB_INSTALL}" ] && export HB_LIB_INSTALL="${_DEFAULT_LIB_DIR}"

# be sure that ${name} binaries are in your path
export PATH="\${HB_BIN_INSTALL}${hb_path_separator}\${PATH}"

HB_PATHS="-I\${HB_INC_INSTALL}"
GCC_PATHS="\${HB_PATHS} -L\${HB_LIB_INSTALL}"
LINK_OPT=""
CC_OPT=""

HB_GPM_LIB=""
if [ -f "\${HB_LIB_INSTALL}/libgtsln.a" ]; then
    if [ "\${HB_ARCHITECTURE}" = "darwin" ]; then
        SYSTEM_LIBS="\${SYSTEM_LIBS} -L/sw/lib"
    fi
    SYSTEM_LIBS="\${SYSTEM_LIBS} -l${HB_SLN_LIB:-slang}"
    [ "\${HB_GPM_MOUSE}" = "yes" ] && HB_GPM_LIB="gpm"
fi
if [ -f "\${HB_LIB_INSTALL}/libgtcrs.a" ]; then
    SYSTEM_LIBS="\${SYSTEM_LIBS} -l${HB_CRS_LIB:-ncurses}"
    [ "\${HB_GPM_MOUSE}" = "yes" ] && HB_GPM_LIB="gpm"
fi
if [ "\${HB_WITHOUT_X11}" != "yes" ]; then
    if [ -f "\${HB_LIB_INSTALL}/libgtxvt.a" ] || [ -f "\${HB_LIB_INSTALL}/libgtxwc.a" ]; then
        [ -d "/usr/X11R6/lib64" ] && SYSTEM_LIBS="\${SYSTEM_LIBS} -L/usr/X11R6/lib64"
        SYSTEM_LIBS="\${SYSTEM_LIBS} -L/usr/X11R6/lib -lX11"
    fi
fi
[ -n "\${HB_GPM_LIB}" ] && SYSTEM_LIBS="\${SYSTEM_LIBS} -l\${HB_GPM_LIB}"

if [ "\${HB_STATIC}" = "full" ]; then
    if [ "\${HB_STATIC}" = "full" ]; then
        SYSTEM_LIBS="\${SYSTEM_LIBS} -ldl"
    fi
    LINK_OPT="\${LINK_OPT} -static"
    HB_STATIC="yes"
fi
if [ "\${HB_XBGTK}" = "yes" ]; then
    SYSTEM_LIBS="\${SYSTEM_LIBS} \`pkg-config --libs gtk+-2.0\`"
fi

HB_LNK_REQ=""
for gt in \${HB_GT_REQ}; do
    if [ "\${HB_STATIC}" = "yes" ] || [ "\${gt}" = "ALLEG" ]; then
        HB_LNK_REQ="\${HB_LNK_REQ} HB_GT_\${gt}"
        if [ "\${gt}" = "ALLEG" ]; then
            if [ "\${HB_STATIC}" = "yes" ]; then
                SYSTEM_LIBS="\`allegro-config --static 2>/dev/null\` \${SYSTEM_LIBS}"
            else
                SYSTEM_LIBS="\`allegro-config --libs 2>/dev/null\` \${SYSTEM_LIBS}"
            fi
        fi
    fi
done
[ -n "\${HB_FM_REQ}" ] && HB_LNK_REQ="\${HB_LNK_REQ} HB_FM_\${HB_FM_REQ}"

HARBOUR_LIBS=""
if [ "\${HB_STATIC}" = "yes" ]; then
    libs="${hb_libs} ${hb_libsc}"
else
    l="${name}"
    if [ "\${HB_ARCHITECTURE}" = "darwin" ]; then
        ext="dylib"
        LINK_OPT="-bind_at_load -multiply_defined suppress"
    else
        ext="so"
    fi
    [ "\${HB_MT}" = "MT" ] && [ -f "\${HB_LIB_INSTALL}/lib\${l}mt.\${ext}" ] && l="\${l}mt"
    [ -f "\${HB_LIB_INSTALL}/lib\${l}.\${ext}" ] && HARBOUR_LIBS="\${HARBOUR_LIBS} -l\${l}"
    libs="gtalleg hbodbc debug profiler ${hb_libsc}"
fi
for l in \${libs}
do
    if [ "\${HB_MG}" = "yes" ] || [ "\${l#gt}" = "\${l}" ] || [ "\${l}" = "gt\${HB_GT_STAT}" ]; then
        [ "\${HB_MT}" = "MT" ] && [ -f "\${HB_LIB_INSTALL}/lib\${l}mt.a" ] && l="\${l}mt"
        if [ -f "\${HB_LIB_INSTALL}/lib\${l}.a" ]; then
            HARBOUR_LIBS="\${HARBOUR_LIBS} -l\${l}"
        fi
    fi
done
if [ "\${HB_XBGTK}" = "yes" ]; then
    HARBOUR_LIBS="\${HARBOUR_LIBS} -lxbgtk"
fi
if [ "\${HB_ARCHITECTURE}" = "darwin" ] || [ "\${HB_ARCHITECTURE}" = "sunos" ]; then
    HARBOUR_LIBS="\${HARBOUR_LIBS} \${HARBOUR_LIBS}"
else
    HARBOUR_LIBS="-Wl,--start-group \${HARBOUR_LIBS} -Wl,--end-group"
fi
l="fm"
[ "\${HB_MT}" = "MT" ] && [ -f "\${HB_LIB_INSTALL}/lib\${l}mt.a" ] && l="\${l}mt"
if [ -f "\${HB_LIB_INSTALL}/lib\${l}.a" ]; then
    if [ "\${HB_STATIC}" = "yes" ] && [ "\${HB_FM_REQ}" = "STAT" ]; then
        HARBOUR_LIBS="-l\${l} \${HARBOUR_LIBS}"
    else
        HARBOUR_LIBS="\${HARBOUR_LIBS} -l\${l}"
    fi
fi

if [ "\${HB_ARCHITECTURE}" = "darwin" ]; then
    CC_OPT="-no-cpp-precomp -Wno-long-double"
elif [ "\${HB_ARCHITECTURE}" = "sunos" ]; then
    HB_STRIP="no"
fi

FOUTC1="\${FILEOUT%.*}.c"
FOUTO1="\${FILEOUT%.*}.o"
FOUTE1="\${FILEOUT%.[Pp][Rr][Gg]}"
FOUTE1="\${FOUTE1%.[oc]}"
FOUTC="\${DIROUT}/\${FILEOUT%.*}.c"
FOUTO="\${DIROUT}/\${FILEOUT%.*}.o"
FOUTE="\${DIROUT}/\${FILEOUT%.[Pp][Rr][Gg]}"
FOUTE="\${FOUTE%.[oc]}"

hb_cc()
{
    ${hb_cmpname} "\$@" \${HB_PATHS} && [ -f "\${FOUTC}" ] 
}

hb_link()
{
    if [ "\${HB_COMPILER}" != "djgpp" ]; then
        if [ -n "\${HB_MAIN_FUNC}" ]; then
            HB_MAIN_FUNC="@\${HB_MAIN_FUNC}"
        elif [ -f "\${FOUTO}" ]; then
            HB_MAIN_FUNC=\`hb_lnk_main "\${FOUTO}"\`
        fi
    fi
    if [ -n "\${HB_LNK_REQ}" ] || [ -n "\${HB_GT_REQ}" ] || [ -n "\${HB_MAIN_FUNC}" ]; then
        hb_lnk_request > \${_TMP_FILE_} && \\
        gcc "\$@" \${CC_OPT} "\${_TMP_FILE_}" \${LINK_OPT} \${GCC_PATHS} \${HARBOUR_LIBS} \${SYSTEM_LIBS} -o "\${FOUTE}"
    else
        gcc "\$@" \${LINK_OPT} \${GCC_PATHS} \${HARBOUR_LIBS} \${SYSTEM_LIBS} -o "\${FOUTE}"
    fi
}

hb_cmp()
{
    hb_cc "\$@" && \\
    gcc -c \${CC_OPT} "\${FOUTC}" -o "\${FOUTO}" \${GCC_PATHS} && \\
    rm -f "\${FOUTC}"
}

hb_lnk_request()
{
    echo "#include \\"hbapi.h\\""
    if [ -n "\${HB_LNK_REQ}" ]; then
        for fn in \${HB_LNK_REQ}; do
            echo "extern HB_FUNC( \${fn} );"
        done
        echo "void hb_lnk_ForceLink_build( void )"
        echo "{"
        for fn in \${HB_LNK_REQ}; do
            echo "   HB_FUNCNAME( \${fn} )();"
        done
        echo "}"
    fi
    gt="\${HB_GT_REQ%% *}"
    if [ -n "\$gt" ] || [ -n "\${HB_MAIN_FUNC}" ]; then
        echo "#include \\"hbinit.h\\""
        echo "extern char * s_defaultGT;"
        echo "extern char * s_pszLinkedMain;"
        echo "HB_CALL_ON_STARTUP_BEGIN( hb_lnk_SetDefault_build )"
        if [ -n "\$gt" ]; then
            echo "   s_defaultGT = \\"\$gt\\";"
        fi
        if [ -n "\${HB_MAIN_FUNC}" ]; then
            echo "   s_pszLinkedMain = \\"\${HB_MAIN_FUNC}\\";"
        fi
        echo "HB_CALL_ON_STARTUP_END( hb_lnk_SetDefault_build )"
    fi
}

hb_lnk_main()
{
    (nm \$1 -g -n --defined-only|sed -e '/HB_FUN_/ ! d' -e 's/^[0-9a-fA-F]* T HB_FUN_//'|head -1|grep -v '^MAIN\$')2>/dev/null
#    (nm \$1 -n --defined-only|sed -e '/HB_FUN_/ ! d' -e 's/^[0-9a-fA-F]* [Tt] HB_FUN_//'|head -1|grep -v '^MAIN\$')2>/dev/null
}

hb_cleanup()
{
    rm -f "\${_TMP_FILE_}"
}

trap hb_cleanup EXIT &>/dev/null

## get basename
HB="\${0##*/}"

case "\${HB}" in
    *cc)
        hb_cc "\${P[@]}"
        ;;
    *cmp|gharbour)
        hb_cmp "\${P[@]}"
        ;;
    *lnk|harbour-link)
        hb_link "\${P[@]}" && \\
        ( [ "\${HB_STRIP}" != "yes" ] || strip "\${FOUTE}" )
        ;;
    *mk)
        hb_cmp "\${P[@]}" && \\
        hb_link "\${FOUTO}" && \\
        ( [ "\${HB_STRIP}" != "yes" ] || strip "\${FOUTE}" ) && \\
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
    local LIBS LIBSMT l lm ll hb_rootdir hb_ver hb_libs full_lib_name full_lib_name_mt linker_options

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
            debug|profiler|fm|hbodbc|gtalleg|rddads) ;;
            *)
                ls="lib${l}.a"
                if [ -f lib${l}mt.a ]
                then
                    lm="lib${l}mt.a"
                else
                    lm="${ls}"
                fi
                if [ "${HB_MULTI_GT}" = "yes" ] || \
                   [ "${l#gt}" = "${l}" ] || \
                   [ "${l}" = "${HB_GT_LIB}" ]
                then
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
                fi
                ;;
        esac
    done
    if [ "${HB_ARCHITECTURE}" = "darwin" ]; then
        full_lib_name="lib${name}.${hb_ver}.dylib"
        full_lib_name_mt="lib${name}mt.${hb_ver}.dylib"
        linker_options="-L/sw/lib $linker_options"
    else
        full_lib_name="lib${name}-${hb_ver}.so"
        full_lib_name_mt="lib${name}mt-${hb_ver}.so"
    fi
    echo "Making ${full_lib_name}..."
    $HB_BIN_INSTALL/hb-mkslib ${full_lib_name} $LIBS ${linker_options}
    if [ "$HB_MT" = "MT" ]; then
        echo "Making ${full_lib_name_mt}..."
        $HB_BIN_INSTALL/hb-mkslib ${full_lib_name_mt} $LIBSMT ${linker_options}
    fi
    for l in ${full_lib_name} ${full_lib_name_mt}
    do
        if [ -f $l ]
        then
            if [ "${HB_ARCHITECTURE}" = "darwin" ]; then
                ll=${l%.${hb_ver}.dylib}.dylib
            else
                ll=${l%-${hb_ver}.so}.so
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
    done
    )
    #export LD_LIBRARY_PATH="$HB_LIB_INSTALL:$LD_LIBRARY_PATH"
}
