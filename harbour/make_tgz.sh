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

cd `dirname $0`
. bin/hb-func.sh

name="harbour"
hb_ver=`get_hbver`
hb_platform=`get_hbplatform`
[ "${hb_platform}" = "" ] || hb_platform="-${hb_platform}"
hb_archfile="${name}-${hb_ver}${hb_platform}.bin.tar.gz"
hb_instfile="${name}-${hb_ver}${hb_platform}.inst.sh"
hb_lnkso="yes"
hb_pref="hb"
hb_contrib="libct rdd_ads"
export C_USR="-DHB_FM_STATISTICS_OFF -O2"
if [ -z "$HB_ARCHITECTURE" ]; then export HB_ARCHITECTURE=linux; fi
if [ -z "$HB_COMPILER" ]; then export HB_COMPILER=gcc; fi
if [ -z "$HB_GPM_MOUSE" ]; then export HB_GPM_MOUSE=yes; fi
if [ -z "$HB_GT_LIB" ]; then export HB_GT_LIB=gtcrs; fi
if [ -z "$HB_MULTI_GT" ]; then export HB_MULTI_GT=no; fi
if [ -z "$HB_MT" ]; then export HB_MT=no; fi
if [ -z "$HB_COMMERCE" ]; then export HB_COMMERCE=no; fi

if [ "$HB_COMMERCE" = yes ]
then
   export HB_GPM_MOUSE=no
   export HB_WITHOUT_GTSLN=yes
fi

export HB_BIN_INSTALL="/usr/bin"
export HB_INC_INSTALL="/usr/include/${name}"
export HB_LIB_INSTALL="/usr/lib/${name}"

# buid
umask 022
make -r clean
make -r
for l in ${hb_contrib}
do
    (cd "contrib/$l"
     make -r clean
     make -r)
done

# install
if [ -z "$TMPDIR" ]; then TMPDIR="/tmp"; fi
HB_INST_PREF="$TMPDIR/$name.bin.$USER.$$"
rm -fR "${HB_INST_PREF}"

export _DEFAULT_BIN_DIR=$HB_BIN_INSTALL
export _DEFAULT_INC_DIR=$HB_INC_INSTALL
export _DEFAULT_LIB_DIR=$HB_LIB_INSTALL
export HB_BIN_INSTALL="$HB_INST_PREF/$HB_BIN_INSTALL"
export HB_INC_INSTALL="$HB_INST_PREF/$HB_INC_INSTALL"
export HB_LIB_INSTALL="$HB_INST_PREF/$HB_LIB_INSTALL"

mkdir -p $HB_BIN_INSTALL
mkdir -p $HB_INC_INSTALL
mkdir -p $HB_LIB_INSTALL
make -r -i install
for l in ${hb_contrib}
do
    (cd "contrib/$l"
     make -r -i install)
done

# Keep the size of the binaries to a minimim.
strip $HB_BIN_INSTALL/harbour
# Keep the size of the libraries to a minimim.
strip --strip-debug $HB_LIB_INSTALL/*

install -m755 bin/hb-mkslib.sh $HB_BIN_INSTALL/hb-mkslib

mkdir -p $HB_INST_PREF/etc/harbour
install -m644 source/rtl/gtcrs/hb-charmap.def $HB_INST_PREF/etc/harbour/hb-charmap.def
cat > $HB_INST_PREF/etc/harbour.cfg <<EOF
CC=gcc
CFLAGS=-c -I$_DEFAULT_INC_DIR -O2
VERBOSE=YES
DELTMP=YES
EOF

# check if we should rebuild tools with shared libs
if [ "${hb_lnkso}" = yes ]
then
    export L_USR="-L${HB_LIB_INSTALL} -l${name} -lncurses -lslang -lgpm -L/usr/X11R6/lib -lX11"
    export PRG_USR="\"-D_DEFAULT_INC_DIR='${_DEFAULT_INC_DIR}'\""

    for utl in hbmake hbrun hbpp hbdoc hbtest
    do
        (cd "utils/${utl}"
         rm -fR "./${HB_ARCHITECTURE}"
         make -r install
         strip "${HB_BIN_INSTALL}/${utl}")
    done
fi


# Create PP
pushd contrib/dot
$HB_BIN_INSTALL/${hb_pref}mk pp -n -w -D_DEFAULT_INC_DIR=\"${_DEFAULT_INC_DIR}\"
install -m755 -s pp $HB_BIN_INSTALL/pp
ln -s pp $HB_BIN_INSTALL/pprun
install -m644 rp_dot.ch $HB_INC_INSTALL/
rm -f pp
popd

tar -czvf "${hb_archfile}" --owner=root --group=root -C "${HB_INST_PREF}" .
rm -fR "${HB_INST_PREF}"

cat > "${hb_instfile}" <<EOF
#!/bin/sh
if [ "\$1" == "--extract" ]; then
    sed -e '1,/^HB_INST_EOF\$/ d' \$0 > "${hb_archfile}"
    exit
fi
if [ \`id -u\` != 0 ]; then
    echo "This package has to be installed from root account."
    exit 1
fi
echo "Do you want to install ${name} (y/n)"
read ASK
if [ "\${ASK}" != "y" ] && [ "\${ASK}" != "Y" ]; then
    exit 1
fi
(sed -e '1,/^HB_INST_EOF\$/ d' \$0 | gzip -cd | tar xvf - -C /) && ldconfig
exit \$?
HB_INST_EOF
EOF
cat "${hb_archfile}" >> "${hb_instfile}"
chmod +x "${hb_instfile}"
rm -f "${hb_archfile}"
