#!/bin/sh
#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@polbox.com>
# simple script to build binaries .tgz from Harbour sources
#
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

name="harbour"
hb_ver="0.42.0"
hb_lnkso="yes"
hb_pref="hb"
hb_libs="vm pp rtl rdd dbfdbt dbffpt dbfcdx dbfntx macro common lang codepage gtnul gtcrs gtsln gtcgi gtstd gtpca odbc ct debug profiler"
export C_USR="-DHB_FM_STATISTICS_OFF -O3"
if [ -z "$HB_ARCHITECTURE" ]; then export HB_ARCHITECTURE=linux; fi
if [ -z "$HB_COMPILER" ]; then export HB_COMPILER=gcc; fi
if [ -z "$HB_GPM_MOUSE" ]; then export HB_GPM_MOUSE=yes; fi
if [ -z "$HB_GT_LIB" ]; then export HB_GT_LIB=gtcrs; fi
if [ -z "$HB_MULTI_GT" ]; then export HB_MULTI_GT=no; fi
if [ -z "$HB_MT" ]; then export HB_MT=no; fi

export HB_BIN_INSTALL="/usr/bin"
export HB_INC_INSTALL="/usr/include/${name}"
export HB_LIB_INSTALL="/usr/lib/${name}"

# buid
umask 022
make clean
make
pushd contrib/libct
    make clean
    make
popd

# install
if [ -z "$TMPDIR" ]; then TMPDIR="/tmp"; fi
HB_INST_PREF="$TMPDIR/$name.bin.$USER.$$"
rm -fR "${HB_INST_PREF}"

_DEFAULT_BIN_DIR=$HB_BIN_INSTALL
_DEFAULT_INC_DIR=$HB_INC_INSTALL
_DEFAULT_LIB_DIR=$HB_LIB_INSTALL
export HB_BIN_INSTALL="$HB_INST_PREF/$HB_BIN_INSTALL"
export HB_INC_INSTALL="$HB_INST_PREF/$HB_INC_INSTALL"
export HB_LIB_INSTALL="$HB_INST_PREF/$HB_LIB_INSTALL"

mkdir -p $HB_BIN_INSTALL
mkdir -p $HB_INC_INSTALL
mkdir -p $HB_LIB_INSTALL
make -i install
pushd contrib/libct
    make -i install
popd

# build fm lib with memory statistic
pushd source/vm
    TMP_C_USR=$C_USR
    C_USR=${C_USR//-DHB_FM_STATISTICS_OFF/-DHB_PARANOID_MEM_CHECK}
    rm -f fm.o
    make fm.o
    ar -r $HB_LIB_INSTALL/libfm.a fm.o
    rm -f fm.o
    if [ $HB_MT = "MT" ]; then
        make fm.o 'HB_LIBCOMP_MT=YES'
        ar -r $HB_LIB_INSTALL/libfmmt.a fm.o
        rm -f fm.o
    fi
    C_USR=$TMP_C_USR
popd

# Keep the size of the binaries to a minimim.
strip $HB_BIN_INSTALL/*
# Keep the size of the libraries to a minimim.
strip --strip-debug $HB_LIB_INSTALL/*

install -m755 bin/hb-mkslib.sh $HB_BIN_INSTALL/hb-mkslib

pushd $HB_LIB_INSTALL
LIBS=""
LIBSMT=""
for l in ${hb_libs}
do
    case $l in
        debug|profiler) ;;
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
               [ "${l}" == "${HB_GT_LIB}" ]
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
$HB_BIN_INSTALL/hb-mkslib lib${name}-${hb_ver}.so $LIBS
[ $HB_MT != "MT" ] || $HB_BIN_INSTALL/hb-mkslib lib${name}mt-${hb_ver}.so $LIBSMT
for l in lib${name}-${hb_ver}.so lib${name}mt-${hb_ver}.so
do
    if [ -f $l ]
    then
        ll=${l%-${hb_ver}.so}.so
        ln -s $l $ll && ln -s ${name}/$l $HB_INST_PREF/usr/lib/$ll
    fi
done
#export LD_LIBRARY_PATH="$HB_LIB_INSTALL:$LD_LIBRARY_PATH"
popd

# Add a harbour compiler wrapper.
cat > $HB_BIN_INSTALL/${hb_pref}-build <<EOF
#!/bin/bash

if [ \$# == 0 ]; then
    echo "syntax: \$0 [<options,...>] <file>[.prg|.o]

\"${hb_pref}cc\", \"${hb_pref}cmp\", \"${hb_pref}lnk\" and \"${hb_pref}mk\" parameters:
    -o<outputfilename>      # output file name
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
    -main=<main_func>   # set the name of main program function/procedure.
                        # if not set then 'MAIN' is used or if it doesn't
                        # exist the name of first public function/procedure
                        # in first linked object module (link)
"
    exit 1
elif [ "\$*" == "mk-links" ]; then
    DIR="\${0%/*}"
    NAME="\${0##*/}"
    if [ "\${DIR}" != "\${NAME}" ]; then
	for n in ${hb_pref}cc ${hb_pref}cmp ${hb_pref}mk ${hb_pref}lnk gharbour harbour-link; do
	    ln -sf "\${NAME}" "\${DIR}/\${n}"
	done
    fi
    exit
fi

## default parameters
HB_STATIC="no"
HB_MT=""
HB_GT="${HB_GT_LIB#gt}"
HB_MG="${HB_MULTI_GT}"

HB_GT_REQ=""
HB_FM_REQ=""
HB_MAIN_FUNC=""
_TMP_FILE_="/tmp/hb-build-\$USER-\$\$.c"

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

SYSTEM_LIBS="-lm -lncurses -lslang -lgpm"
# use pthread system library for MT programs
if [ "\${HB_MT}" = "MT" ]; then
    SYSTEM_LIBS="-lpthread \${SYSTEM_LIBS}"
fi

HB_GT_STAT=""
[ -z "\${HB_GT_REQ}" ] && HB_GT_REQ="\${HB_GT}"
if [ "\${HB_MG}" != "yes" ]; then
    [ "\${HB_STATIC}" = "yes" ] && HB_GT_STAT=\`echo \${HB_GT_REQ}|tr A-Z a-z\`
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
if [ "\${HB_STATIC}" = "full" ]; then
    LINK_OPT="\${LINK_OPT} -static"
    HB_STATIC="yes"
fi

HARBOUR_LIBS=""
if [ "\${HB_STATIC}" = "yes" ]; then
    libs="${hb_libs}"
else
    l="${name}"
    [ "\${HB_MT}" = "MT" ] && [ -f "\${HB_LIB_INSTALL}/lib\${l}mt.so" ] && l="\${l}mt"
    [ -f "\${HB_LIB_INSTALL}/lib\${l}.so" ] && HARBOUR_LIBS="\${HARBOUR_LIBS} -l\${l}"
    libs="debug profiler"
fi
for l in \${libs}
do
    if [ "\${HB_MG}" = "yes" ] || [ "\${l#gt}" = "\${l}" ] || [ "\${l}" == "gt\${HB_GT_STAT}" ]; then
        [ "\${HB_MT}" = "MT" ] && [ -f "\${HB_LIB_INSTALL}/lib\${l}mt.a" ] && l="\${l}mt"
        [ -f "\${HB_LIB_INSTALL}/lib\${l}.a" ] && HARBOUR_LIBS="\${HARBOUR_LIBS} -l\${l}"
    fi
done
HARBOUR_LIBS="-Wl,--start-group \${HARBOUR_LIBS} -Wl,--end-group"
l="fm"
[ "\${HB_MT}" = "MT" ] && [ -f "\${HB_LIB_INSTALL}/lib\${l}mt.a" ] && l="\${l}mt"
if [ -f "\${HB_LIB_INSTALL}/lib\${l}.a" ]; then
    if [ "\${HB_STATIC}" = "yes" ] && [ "\${HB_FM_REQ}" = "STAT" ]; then
        HARBOUR_LIBS="-l\${l} \${HARBOUR_LIBS}"
    else
        HARBOUR_LIBS="\${HARBOUR_LIBS} -l\${l}"
    fi
fi

FOUTC="\${DIROUT}/\${FILEOUT%.*}.c"
FOUTO="\${DIROUT}/\${FILEOUT%.*}.o"
FOUTE="\${DIROUT}/\${FILEOUT%.[Pp][Rr][Gg]}"
FOUTE="\${FOUTE%.[oc]}"

hb_cc()
{
    harbour "\$@" \${HB_PATHS} && [ -f "\${FOUTC}" ] 
}

hb_link()
{
    if [ -n "\${HB_MAIN_FUNC}" ]; then
        HB_MAIN_FUNC="@\${HB_MAIN_FUNC}"
    elif [ -f "\${FOUTO}" ]; then
        HB_MAIN_FUNC=\`hb_lnk_main "\${FOUTO}"\`
    fi
    if [ -n "\${HB_GT_REQ}" ] || [ -n "\${HB_FM_REQ}" ] || [ -n "\${HB_MAIN_FUNC}" ]; then
        hb_lnk_request > \${_TMP_FILE_} && \\
        gcc "\$@" "\${_TMP_FILE_}" \${LINK_OPT} \${GCC_PATHS} \${HARBOUR_LIBS} \${SYSTEM_LIBS} -o "\${FOUTE}"
    else
        gcc "\$@" \${LINK_OPT} \${GCC_PATHS} \${HARBOUR_LIBS} \${SYSTEM_LIBS} -o "\${FOUTE}"
    fi
}

hb_cmp()
{
    hb_cc "\$@" && \\
    gcc -g -c "\${FOUTC}" -o "\${FOUTO}" \${GCC_PATHS} && \\
    rm -f "\${FOUTC}"
}

hb_lnk_request()
{
    echo "#include \\"hbapi.h\\""
    if [ "\${HB_STATIC}" = "yes" ] || [ -n "\${HB_FM_REQ}" ]; then
        for gt in \${HB_GT_REQ}; do
            echo "extern HB_FUNC( HB_GT_\${gt} );"
        done
        if [ -n "\${HB_FM_REQ}" ]; then
            echo "extern HB_FUNC( HB_FM_\${HB_FM_REQ} );"
        fi
        echo "void hb_lnk_ForceLink_build( void )"
        echo "{"
        for gt in \${HB_GT_REQ}; do
            echo "   HB_FUNCNAME( HB_GT_\${gt} )();"
        done
        if [ -n "\${HB_FM_REQ}" ]; then
            echo "   HB_FUNCNAME( HB_FM_\${HB_FM_REQ} )();"
        fi
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
        hb_link "\${P[@]}"
        ;;
    *mk)
        hb_cmp "\${P[@]}" && \\
        hb_link "\${FOUTO}" && \\
        strip "\${FOUTE}" && \\
        rm -f "\${FOUTO}"
        ;;
esac
EOF
chmod 755 $HB_BIN_INSTALL/${hb_pref}-build
$HB_BIN_INSTALL/${hb_pref}-build mk-links

mkdir -p $HB_INST_PREF/etc/harbour
install -m644 source/rtl/gtcrs/hb-charmap.def $HB_INST_PREF/etc/harbour/hb-charmap.def
cat > $HB_INST_PREF/etc/harbour.cfg <<EOF
CC=gcc
CFLAGS=-c -I$_DEFAULT_INC_DIR -O2
VERBOSE=YES
DELTMP=YES
EOF

# Create PP
pushd contrib/dot
$HB_BIN_INSTALL/${hb_pref}mk pp -n -w -D_DEFAULT_INC_DIR=\"${_DEFAULT_INC_DIR}\"
install -m755 -s pp $HB_BIN_INSTALL/pp
ln -s pp $HB_BIN_INSTALL/pprun
install -m644 rp_dot.ch $HB_INC_INSTALL/
rm -f pp
popd

# check if we should rebuild tools with shared libs
if [ "${hb_lnkso}" = yes ]
then
    export L_USR="-L${HB_LIB_INSTALL} -l${name} -lncurses -lslang -lgpm"

    for utl in hbmake hbrun hbpp hbdoc hbtest
    do
        pushd utils/${utl}
        rm -fR "./${HB_ARCHITECTURE}"
        make install
        strip ${HB_BIN_INSTALL}/${utl}
        popd
    done
fi

tar -czvf ${name}-${hb_ver}.bin.tar.gz --owner=root --group=root -C "${HB_INST_PREF}" .
rm -fR "${HB_INST_PREF}"
