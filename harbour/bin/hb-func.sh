#!/bin/sh
[ "$BASH" ] || exec bash `which $0` ${1+"$@"}
#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@priv.onet.pl>
# small set of functions used by Harbour scripts
# warnig: some bash extensions are used
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
    echo -n "${id}"
}

get_hbver()
{
    local FVER MAJOR MINOR REVIS hb_rootdir

    hb_rootdir="${1-.}"
    FVER="${hb_rootdir}/include/hbver.h"
    MAJOR=`sed -e '/HB_VER_MAJOR/    s/[^0-9]*\([^ ]*\).*/\1/g p' -e 'd' "${FVER}"`
    MINOR=`sed -e '/HB_VER_MINOR/    s/[^0-9]*\([^ ]*\).*/\1/g p' -e 'd' "${FVER}"`
    REVIS=`sed -e '/HB_VER_REVISION/ s/[^0-9]*\([^ ]*\).*/\1/g p' -e 'd' "${FVER}"`
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
    else
        hb_tool="$1/${hb_pref}-build"
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
        if [ "${HB_NCURSES_194}" = "yes" ]; then
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
    -static             # link with static ${name} libs
    -fullstatic         # link with all static libs
    -shared             # link with shared libs (default)
    -mt                 # link with multi-thread libs
    -gt<hbgt>           # link with <hbgt> GT driver, can be repeated to
                        # link with more GTs. The first one will be
                        #      the default at runtime
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
HB_STATIC="no"
HB_MT=""
HB_GT="${HB_GT_LIB#gt}"
HB_MG="${HB_MULTI_GT}"

HB_GPM_MOUSE="${HB_GPM_MOUSE}"

HB_GT_REQ=""
HB_FM_REQ=""
HB_STRIP="yes"
HB_MAIN_FUNC=""
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
        HB_GT_STAT=\`echo \${HB_GT_REQ}|tr A-Z a-z\`
    fi
    HB_GT_REQ=""
else
    HB_GT_REQ=\`echo \${HB_GT_REQ}|tr a-z A-Z\`
fi
HB_MAIN_FUNC=\`echo \${HB_MAIN_FUNC}|tr a-z A-Z\`

# set environment variables
export HB_ARCHITECTURE="${HB_ARCHITECTURE}"
export HB_COMPILER="${HB_COMPILER}"
[ -z "\${HB_BIN_INSTALL}" ] && export HB_BIN_INSTALL="${_DEFAULT_BIN_DIR}"
[ -z "\${HB_INC_INSTALL}" ] && export HB_INC_INSTALL="${_DEFAULT_INC_DIR}"
[ -z "\${HB_LIB_INSTALL}" ] && export HB_LIB_INSTALL="${_DEFAULT_LIB_DIR}"

# be sure that ${name} binaries are in your path
export PATH="\${HB_BIN_INSTALL}:\${PATH}"

HB_PATHS="-I\${HB_INC_INSTALL}"
GCC_PATHS="\${HB_PATHS} -L\${HB_LIB_INSTALL}"
LINK_OPT=""

HB_GPM_LIB=""
if [ -f "\${HB_LIB_INSTALL}/libgtsln.a" ]; then
    SYSTEM_LIBS="\${SYSTEM_LIBS} -l${HB_SLN_LIB:-slang}"
    [ "\${HB_GPM_MOUSE}" = "yes" ] && HB_GPM_LIB="gpm"
fi
if [ -f "\${HB_LIB_INSTALL}/libgtcrs.a" ]; then
    SYSTEM_LIBS="\${SYSTEM_LIBS} -l${HB_CRS_LIB:-ncurses}"
    [ "\${HB_GPM_MOUSE}" = "yes" ] && HB_GPM_LIB="gpm"
fi
if [ "\${HB_WITHOUT_X11}" != "yes" ]; then
    if [ -f "\${HB_LIB_INSTALL}/libgtxvt.a" ] || [ -f "\${HB_LIB_INSTALL}/libgtxwc.a" ]; then
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
    [ "\${HB_MT}" = "MT" ] && [ -f "\${HB_LIB_INSTALL}/lib\${l}mt.so" ] && l="\${l}mt"
    [ -f "\${HB_LIB_INSTALL}/lib\${l}.so" ] && HARBOUR_LIBS="\${HARBOUR_LIBS} -l\${l}"
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
if [ "\${HB_ARCHITECTURE}" = "darwin" ]; then
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
    if [ -n "\${HB_MAIN_FUNC}" ]; then
        HB_MAIN_FUNC="@\${HB_MAIN_FUNC}"
    elif [ -f "\${FOUTO}" ]; then
        HB_MAIN_FUNC=\`hb_lnk_main "\${FOUTO}"\`
    fi
    if [ -n "\${HB_LNK_REQ}" ] || [ -n "\${HB_GT_REQ}" ] || [ -n "\${HB_MAIN_FUNC}" ]; then
        hb_lnk_request > \${_TMP_FILE_} && \\
        gcc "\$@" "\${_TMP_FILE_}" \${LINK_OPT} \${GCC_PATHS} \${HARBOUR_LIBS} \${SYSTEM_LIBS} -o "\${FOUTE}"
    else
        gcc "\$@" \${LINK_OPT} \${GCC_PATHS} \${HARBOUR_LIBS} \${SYSTEM_LIBS} -o "\${FOUTE}"
    fi
}

hb_cmp()
{
    hb_cc "\$@" && \\
    gcc -c "\${FOUTC}" -o "\${FOUTO}" \${GCC_PATHS} && \\
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
    local LIBS LIBSMT l lm ll hb_rootdir hb_ver hb_libs

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
                fi
                ;;
        esac
    done
    echo "Making lib${name}-${hb_ver}.so..."
    $HB_BIN_INSTALL/hb-mkslib lib${name}-${hb_ver}.so $LIBS
    [ "$HB_MT" != "MT" ] || $HB_BIN_INSTALL/hb-mkslib lib${name}mt-${hb_ver}.so $LIBSMT
    for l in lib${name}-${hb_ver}.so lib${name}mt-${hb_ver}.so
    do
        if [ -f $l ]
        then
            ll=${l%-${hb_ver}.so}.so
            ln -sf $l $ll
            case $HB_LIB_INSTALL in
                */usr/lib/*|*/usr/local/lib/*)
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
