#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@polbox.com>,
# Dave Pearson <davep@davep.org>
# Harbour RPM spec file
#
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

######################################################################
## Definitions.
######################################################################

%define platform %(release=$(rpm -q --queryformat='.%{VERSION}' mandrake-release 2>/dev/null) && echo "mdk$release"|tr -d ".")
%if "%{platform}" == ""
  %define platform %(release=$(rpm -q --queryformat='.%{VERSION}' redhat-release 2>/dev/null) && echo "rh$release"|tr -d ".")
%endif

%define name     harbour
%define dname    Harbour
%define version  0.44.0
%define releasen 0
%define prefix   /usr
%define hb_pref  hb
%define hb_gt    crs
%define hb_gpm   yes
%define hb_mt    no
%define hb_mgt   no
%define hb_lnkso yes
%define hb_libs  vm pp rtl rdd dbfdbt dbffpt dbfcdx dbfntx macro common lang codepage gtnul gtcrs gtsln gtcgi gtstd gtpca odbc ct debug profiler
%define hb_cc    export HB_COMPILER=gcc
%define hb_cflag export C_USR="-DHB_FM_STATISTICS_OFF -O2"
%define hb_arch  export HB_ARCHITECTURE=linux
%define hb_cmt   export HB_MT=%{hb_mt}
%define hb_cgt   export HB_GT_LIB=gt%{hb_gt}
%define hb_cgpm  export HB_GPM_MOUSE=%{hb_gpm}
%define hb_cmgt  export HB_MULTI_GT=%{hb_mgt}
%define hb_bdir  export HB_BIN_INSTALL=%{prefix}/bin
%define hb_idir  export HB_INC_INSTALL=%{prefix}/include/%{name}
%define hb_ldir  export HB_LIB_INSTALL=%{prefix}/lib/%{name}
%define hb_env   %{hb_cc} ; %{hb_cflag} ; %{hb_arch} ; %{hb_cmt} ; %{hb_cgt} ; %{hb_cgpm} ; %{hb_cmgt} ; %{hb_bdir} ; %{hb_idir} ; %{hb_ldir}
%define hb_host  www.harbour-project.org
%define readme   README.RPM
######################################################################
## Preamble.
######################################################################

Summary:        Free software Clipper compatible compiler
Summary(pl):    Darmowy kompilator kompatybilny z jêzykiem Clipper.
Summary(pt_BR): Um compilador Clipper compativel Gratis
Name:           %{name}
Version:        %{version}
Release:        %{releasen}%{platform}
Prefix:         %{prefix}
Copyright:      GPL (plus exception)
Group:          Development/Languages
Vendor:         %{hb_host}
URL:            http://%{hb_host}/
Source:         %{name}-%{version}.src.tar.gz
Packager:       Przemys³aw Czerpak <druzus@polbox.com> Luiz Rafael Culik Guimaraes <culikr@uol.com.br>
BuildPrereq:    gcc binutils bash bison ncurses ncurses-devel slang-devel gpm-devel
Requires:       gcc binutils bash sh-utils %{name}-lib = %{version}
Provides:       %{name} harbour
BuildRoot:      /tmp/%{name}-%{version}-root

%description
%{dname} is a CA-Clipper compatible compiler for multiple platforms. This
package includes a compiler, pre-processor, header files, virtual machine
and documentation.

See README.RPM in the documentation directory for information specific to
this RPM distribution.

%description -l pl
%{dname} to kompatybilny z jêzykiem CA-Clipper kompilator rozwijany na
wielu ró¿nych platformach. Ten pakiet zawiera kompilator, preprocesor,
zbiory nag³ówkowe, wirtualn± maszynê oraz dokumentacjê.

%description -l pt_BR
%{dname} ‚ um compilador Clipper compativel para multiplas plataformas.
Esse pacote contem um compilador ,um pr‚-processador, arquivos de cabe‡alho
uma maquina virtual e documenta‡Æo


######################################################################
## main shared lib
######################################################################

%package lib
Summary:        Shared runtime libaries for %{dname} compiler
Summary(pl):    Dzielone bilioteki dla kompilatora %{dname}
Group:          Development/Languages
Provides:       lib%{name}.so lib%{name}mt.so

%description lib
%{dname} is a Clipper compatible compiler.
This package provides %{dname} runtime shared libraries for programs
linked dynamically.

%description -l pl lib
%{dname} to kompatybilny z jêzykiem CA-Clipper kompilator.
Ten pakiet udostêpnia dzielone bilioteki kompilatora %{dname}
dla programów konsolidowanych dynamicznie.

%description -l pt_BR lib
%{dname} ‚ um compilador compativel com o Clipper.
Esse pacote %{dname} provem as bibliotecas compartilhadas para programas
linkados dinamicamente


######################################################################
## static libs
######################################################################

%package static
Summary:        Static runtime libaries for %{dname} compiler
Summary(pl):    Statyczne bilioteki dla kompilatora %{dname}
Group:          Development/Languages
Requires:       %{name} = %{version}

%description static
%{dname} is a Clipper compatible compiler.
This package provides %{dname} static runtime libraries for static
program linking.

%description -l pl static
%{dname} to kompatybilny z jêzykiem CA-Clipper kompilator.
Ten pakiet udostêpnia statyczne bilioteki dla kompilatora %{dname}
niezbêdne do statycznej konsolidacji programów.

%description -l pt_BR static
%{dname} ‚ um compilador compativel com o clippe.
Esse pacote %{dname} provem as bibliotecas  de run time staticas para linkagem
dos os programas


######################################################################
## PP
######################################################################

%package pp
Summary:        Clipper/Harbour/xBase compatible Pre-Processor, DOT prompt and interpreter
Summary(pl):    Kompatybilny z Clipper/Harbour/xBase Preprocesor i interpreter
Copyright:      GPL
Group:          Development/Languages
Requires:       %{name} = %{version}

%description pp
%{dname} is a Clipper compatible compiler.
This package provides %{dname} PP. It has 3 personalities which are tied
tightly together.
1. What is supposed to be 100% Clipper compatible Pre-Processor
   (with some extensions).
2. DOT prompt, which suppose to allow most of Clipper syntax.
3. Finally, PP is a limited Clipper/Harbour/xBase Interpreter. Subject
   to those same few limitations it can execute most of Harbour syntax.
   You can write your own xBase scripts by adding to your .prg files
   #!/usr/bin/pprun

%description -l pl pp
%{dname} to kompatybilny z jêzykiem CA-Clipper kompilator.
Ten pakiet udostêpnia %{dname} PP, który daje trzy narzêdzia w jednym.
1. W 100% kompatybilny z Clipperem preprocesor (z pewnymi rozeszerzeniami)
2. ¦rodowisko DOT, w którym mo¿na u¿ywaæ wiêkszo¶ci sk³adni Clippera
3. PP to tak¿e nieco ograniczony interpreter Clippera. Z uwzglêdnieniem
   wspomnianych kilku ograniczeñ potrafi on uruchomiæ wiêkszo¶æ sk³adni
   Harbour. Mo¿esz napisaæ swój w³asny skrypt xBase dodaj±c do pliku .prg
   #!/usr/bin/pprun

%description -l pt_BR pp
%{dname} ‚ um compilador Clipper compativel.
Esse pacote provem o %{dname} PP. Ele tem 3 caracteristicas dependentes
uma da outra.
1. Que e supostamente ser um Pre-Processor 100% compativel com o Clipper
   (com algumas extenssäes).
2. DOT prompt, que supostamente permite a maioria das syntaxes do Clipper.
3. Finalmente, PP ‚ um limitado Interpretador Clipper/Harbour/xBase . Sujeito
   com algumas limita‡äes que pode executar a maioria da syntaxe do Harbour.
   Voce pode escrever seus proprios scritps em .prg ao adicionar as seus arquivos
   .prg #!/usr/bin/pprun


######################################################################
## Preperation.
######################################################################

%prep
%setup -c %{name}
rm -rf $RPM_BUILD_ROOT

######################################################################
## Build.
######################################################################

%build
%{hb_env}

make

# build CT lib
pushd contrib/libct
    make
popd

######################################################################
## Install.
######################################################################

%install

# Install harbour itself.

%{hb_env}

_DEFAULT_INC_DIR=$HB_INC_INSTALL
export HB_BIN_INSTALL=$RPM_BUILD_ROOT/$HB_BIN_INSTALL
export HB_INC_INSTALL=$RPM_BUILD_ROOT/$HB_INC_INSTALL
export HB_LIB_INSTALL=$RPM_BUILD_ROOT/$HB_LIB_INSTALL

mkdir -p $HB_BIN_INSTALL
mkdir -p $HB_INC_INSTALL
mkdir -p $HB_LIB_INSTALL
mkdir -p $RPM_BUILD_ROOT/usr/lib

make -i install

# install CT lib
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
for l in %{hb_libs}
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
$HB_BIN_INSTALL/hb-mkslib lib%{name}-%{version}.so $LIBS
[ $HB_MT != "MT" ] || $HB_BIN_INSTALL/hb-mkslib lib%{name}mt-%{version}.so $LIBSMT
for l in lib%{name}-%{version}.so lib%{name}mt-%{version}.so
do
    if [ -f $l ]
    then
        ll=${l%%-%{version}.so}.so
        ln -s $l $ll && ln -s %{name}/$l $RPM_BUILD_ROOT/usr/lib/$ll
    fi
done
#export LD_LIBRARY_PATH="$HB_LIB_INSTALL:$LD_LIBRARY_PATH"
popd

# Add a harbour compiler wrapper.
cat > $HB_BIN_INSTALL/%{hb_pref}-build <<EOF
#!/bin/bash

if [ \$# == 0 ]; then
    echo "syntax: \$0 [<options,...>] <file>[.prg|.o]

\"%{hb_pref}cc\", \"%{hb_pref}cmp\", \"%{hb_pref}lnk\" and \"%{hb_pref}mk\" parameters:
    -o<outputfilename>      # output file name
\"%{hb_pref}lnk\" and \"%{hb_pref}mk\" parameters:
    -static             # link with static %{dname} libs
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
        for n in %{hb_pref}cc %{hb_pref}cmp %{hb_pref}mk %{hb_pref}lnk gharbour harbour-link; do
            ln -sf "\${NAME}" "\${DIR}/\${n}"
        done
    fi
    exit
fi

## default parameters
HB_STATIC="no"
HB_MT=""
HB_GT="%{hb_gt}"
HB_MG="%{hb_mgt}"

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
%{hb_arch}
%{hb_cc}
[ -z "\${HB_BIN_INSTALL}" ] && %{hb_bdir}
[ -z "\${HB_INC_INSTALL}" ] && %{hb_idir}
[ -z "\${HB_LIB_INSTALL}" ] && %{hb_ldir}

# be sure that %{name} binaries are in your path
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
    libs="%{hb_libs}"
else
    l="%{name}"
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
    gt="\${HB_GT_REQ%%%% *}"
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
#    (nm \$1 -g -n --defined-only|sed -e '/HB_FUN_/ ! d' -e 's/^[0-9a-fA-F]* T HB_FUN_//'|head -1|grep -v '^MAIN\$')2>/dev/null
    (nm \$1 -n --defined-only|sed -e '/HB_FUN_/ ! d' -e 's/^[0-9a-fA-F]* [Tt] HB_FUN_//'|head -1|grep -v '^MAIN\$')2>/dev/null
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
chmod 755 $HB_BIN_INSTALL/%{hb_pref}-build
$HB_BIN_INSTALL/%{hb_pref}-build mk-links

mkdir -p $RPM_BUILD_ROOT/etc/harbour
#install -m644 source/rtl/gtcrs/hb-charmap.def $RPM_BUILD_ROOT/etc/harbour/hb-charmap.def
cat > $RPM_BUILD_ROOT/etc/harbour.cfg <<EOF
CC=gcc
CFLAGS=-c -I$_DEFAULT_INC_DIR -O2
VERBOSE=YES
DELTMP=YES
EOF

# Create PP
pushd contrib/dot
$HB_BIN_INSTALL/%{hb_pref}mk pp -n -w -D_DEFAULT_INC_DIR=\"$_DEFAULT_INC_DIR\"
install -m755 -s pp $HB_BIN_INSTALL/pp
ln -s pp $HB_BIN_INSTALL/pprun
install -m644 rp_dot.ch $HB_INC_INSTALL/
popd

# check if we should rebuild tools with shared libs
if [ "%{hb_lnkso}" = yes ]
then
    export L_USR="-L${HB_LIB_INSTALL} -l%{name} -lncurses -lslang -lgpm"

    for utl in hbmake hbrun hbpp hbdoc
    do
        pushd utils/${utl}
        rm -fR "./${HB_ARCHITECTURE}"
        make install
        strip ${HB_BIN_INSTALL}/${utl}
        popd
    done
fi

# remove unused files
rm -f ${HB_BIN_INSTALL}/hbdoc ${HB_BIN_INSTALL}/hbtest

# Create a README file for people using this RPM.
cat > doc/%{readme} <<EOF
This RPM distribution of %{dname} includes extra commands to make compiling
and linking with harbour a little easier. There are compiler and linker
wrappers called "%{hb_pref}cc", "%{hb_pref}cmp", "%{hb_pref}lnk" and "%{hb_pref}mk".

"%{hb_pref}cc" is a wrapper to the harbour compiler only. It only sets environment
variables. The result of its work is a C file.

Use "%{hb_pref}cmp" exactly as you would use the harbour compiler itself.
The main difference with %{hb_pref}cmp is that it results in an object file,
not a C file that needs compiling down to an object. %{hb_pref}cmp also
ensures that the harbour include directory is seen by the harbour compiler.

"%{hb_pref}lnk" simply takes a list of object files and links them together
with the harbour virtual machine and run-time library to produce an
executable. The executable will be given the basename of the first object
file if not directly set by the "-o" command line switch.

"%{hb_pref}mk" tries to produce an executable from your .prg file. It's a simple
equivalent of cl.bat from the CA-Clipper distribution.

All these scripts accept command line switches:
-o<outputfilename>      # output file name
-static                 # link with static %{dname} libs
-fullstatic             # link with all static libs
-shared                 # link with shared libs (default)
-mt                     # link with multi-thread libs
-gt<hbgt>               # link with <hbgt> GT driver, can be repeated to
                        # link with more GTs. The first one will be
                        #      the default at runtime
-fmstat                 # link with the memory statistics lib
-nofmstat               # do not link with the memory statistics lib (default)
-main=<main_func>       # set the name of main program function/procedure.
                        # if not set then 'MAIN' is used or if it doesn't
                        # exist the name of first public function/procedure
                        # in first linked object module (link)

Link options work only with "%{hb_pref}lnk" and "%{hb_pref}mk" and have no effect
in "%{hb_pref}cc" and "%{hb_pref}cmp".
Other options are passed to %{dname}/C compiler/linker.
To save compatibility with older rpm distributions, "gharbour" can be used
as a synonym of "%{hb_pref}cmp", and "harbour-link" as synonym of "%{hb_pref}lnk"

An example compile/link session looks like:
----------------------------------------------------------------------
druzus@uran:~/tmp$ cat foo.prg
function main()
? "Hello, World!"
return nil

druzus@uran:~/tmp$ %{hb_pref}cmp foo
xHarbour Compiler build 0.80.0 (SimpLex)
Copyright 1999-2003, http://www.xharbour.org http://www.harbour-project.org/
Compiling 'foo.prg'...
Lines 5, Functions/Procedures 2
Generating C source output to 'foo.c'... Done.

druzus@uran:~/tmp$ %{hb_pref}lnk foo.o

druzus@uran:~/tmp$ strip foo

druzus@uran:~/tmp$ ls -l foo
-rwxrwxr-x    1 druzus   druzus       3824 maj 17 02:46 foo
----------------------------------------------------------------------

or using %{hb_pref}mk only:
----------------------------------------------------------------------
druzus@uran:~/tmp$ cat foo.prg
function main()
? "Hello, World!"
return nil

druzus@uran:~/tmp$ %{hb_pref}mk foo
xHarbour Compiler build 0.80.0 (SimpLex)
Copyright 1999-2003, http://www.xharbour.org http://www.harbour-project.org/
Compiling 'foo.prg'...
Lines 5, Functions/Procedures 2
Generating C source output to 'foo.c'... Done.

druzus@uran:~/tmp$ ls -l foo
-rwxrwxr-x    1 druzus   druzus       3824 maj 17 02:46 foo
----------------------------------------------------------------------


In this RPM you will find additional wonderful tools: /usr/bin/pprun
You can run clipper/xbase compatible source files with it if you only
put in their first line:
#!/usr/bin/pprun

For example:
----------------------------------------------------------------------
druzus@uran:~/tmp$ cat foo.prg
#!/usr/bin/pprun
function main()
? "Hello, World!, This is a script !!! :-)"
return nil

druzus@uran:~/tmp$ chmod +x foo.prg

druzus@uran:~/tmp$ ./foo.prg


I hope this RPM is useful. Have fun with %{dname}.

Many thanks to Dave Pearson <davep@davep.org>

Przemyslaw Czerpak <druzus@polbox.com>
EOF

######################################################################
## Post install
######################################################################
#%post lib
#/sbin/ldconfig

######################################################################
## Post uninstall
######################################################################
#%postun lib
#/sbin/ldconfig

######################################################################
## Clean.
######################################################################

%clean
rm -rf $RPM_BUILD_ROOT

######################################################################
## File list.
######################################################################

%files
%defattr(-,root,root,755)
%doc ChangeLog
%doc doc/*.txt
%doc doc/%{readme}
%doc doc/en/
%doc doc/es/

%dir /etc/harbour
/etc/harbour.cfg
#/etc/harbour/hb-charmap.def
%{prefix}/bin/harbour
%{prefix}/bin/hb-mkslib
%{prefix}/bin/%{hb_pref}-build
%{prefix}/bin/%{hb_pref}cc
%{prefix}/bin/%{hb_pref}cmp
%{prefix}/bin/%{hb_pref}lnk
%{prefix}/bin/%{hb_pref}mk
%{prefix}/bin/gharbour
%{prefix}/bin/harbour-link
#%{prefix}/bin/hbtest
%{prefix}/bin/hbrun
%{prefix}/bin/hbpp
%{prefix}/bin/hbmake
%dir %{prefix}/include/%{name}
%{prefix}/include/%{name}/*

%files static
%defattr(-,root,root,755)
%dir %{prefix}/lib/%{name}
%{prefix}/lib/%{name}/*.a

%files lib
%defattr(-,root,root,755)
%dir %{prefix}/lib/%{name}
%{prefix}/lib/%{name}/*.so
%{prefix}/lib/*.so

%files pp
%defattr(-,root,root,755)
%doc contrib/dot/pp.txt
%{prefix}/bin/pp
%{prefix}/bin/pprun

######################################################################
## Spec file Changelog.
######################################################################

%changelog
* Sat Aug 09 2003 Przemyslaw Czerpak <druzus@polbox.com>
- removed ${RPM_OPT_FLAGS} from C_USR

* Wed Jul 23 2003 Przemyslaw Czerpak <druzus@polbox.com>
- fixed file (user and group) owner for RPMs builded from non root account
- shared lib names changed from [x]harbour{mt,}.so to
  [x]harbour{mt,}-<version>.so and soft links with short names created
- 0.82 version set

* Wed Apr 30 2003 Przemyslaw Czerpak <druzus@polbox.com>
- new tool "%{hb_pref}-build" (%{hb_pref}cmp, %{hb_pref}lnk, %{hb_pref}mk) added -
  compiler/linker wrapper.
- new tool "hb-mkslib" (build shared libraries from static ones and object
  files).
- shared libraries added.
- binary package divided.

* Fri Mar 08 2002 Dave Pearson <davep@davep.org>
- Fixed gharbour so that it should work no matter the case of the name of
  the PRG file.

* Wed Feb 13 2002 Dave Pearson <davep@davep.org>
- Fixed a bug in harbour-link which meant that, since the environment
  changes of Jan 17 2002, users could not specify which GT library they
  wanted their application linked against.

* Tue Jan 22 2002 Dave Pearson <davep@davep.org>
- Used the "name" macro a lot more, especially in some paths.

* Thu Jan 17 2002 Dave Pearson <davep@davep.org>
- Removed the use of the /etc/profile.d scripts for setting the
  harbour environment variables. The settings are now placed
  directly in gharbour and harbour-link. This means that this .spec
  file should be more useful on RPM using platforms other than RedHat.

* Wed Dec 19 2001 Dave Pearson <davep@davep.org>
- Added a platform ID to the name of the RPM.

* Mon Dec 17 2001 Dave Pearson <davep@davep.org>
- todo.txt is now called TODO.

* Tue Aug 21 2001 Dave Pearson <davep@davep.org>
- Added todo.txt as a doc file.

* Sun Jul 22 2001 Dave Pearson <davep@davep.org>
- harbour-link now fully respects the setting of $HB_GT_LIB.
- HB_GT_LIB wasn't set in the csh startup script. Fixed.

* Fri Jul 20 2001 Dave Pearson <davep@davep.org>
- Added the setting of $HB_GT_LIB to the environment (ncurses is used).
- Added support for installing hbmake.

* Mon Jun 28 2001 Dave Pearson <davep@davep.org>
- Changed the gharbour script so that it only invokes the C compiler if a C
  file was output. This stops any error messages when someone is using the
  -g option to output other target types.

* Mon Mar 19 2001 Dave Pearson <davep@davep.org>
- Reinstated hbrun in the files section.

* Tue Feb 20 2001 Dave Pearson <davep@davep.org>
- Added README.RPM to the documentation directory.

* Sat Jan 06 2001 Dave Pearson <davep@davep.org>
- The gharbour script now passes the harbour include directory, using -I,
  to harbour.

* Thu Aug 24 2000 Dave Pearson <davep@davep.org>
- Changed the files section so that hbrun doesn't get installed. It isn't
  useful on GNU/Linux systems.

* Tue Aug 22 2000 Dave Pearson <davep@davep.org>
- Changed the 'egcs' requirement to 'gcc'.

* Mon Aug 21 2000 Przemyslaw Czerpak <druzus@polbox.com>
- Polish translation added
- BuildRoot marco added. Now you can build the package from normal user
  account.
- bison and flex added to BuildPrereq list
- Debug information is stripped from installed files.

* Wed Aug 02 2000 Dave Pearson <davep@davep.org>
- Removed hbtest from the list of files installed into the bin directory.
- Added 'bash' and 'sh-utils' to the list of required packages.

* Tue Aug 01 2000 Dave Pearson <davep@davep.org>
- Added harbour environment scripts to /etc/profile.d.
- Added generation of gharbour and harbour-link commands.

* Mon Jul 31 2000 Dave Pearson <davep@davep.org>
- Re-worked the layout of the spec file to make it cleaner and easier to
  read and maintain.
- The latest harbour ChangeLog is now installed into the RPM's doc
  directory.
- The content of the RPM's doc directory reflects the layout and content of
  the harbour source's doc directory.
