#!/bin/sh
[ "$BASH" ] || exec bash `which $0` ${1+"$@"}
#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2003 Przemyslaw Czerpak <druzus@polbox.com>
# simple script to build shared libraries from static ones and
# object files
#
# See doc/license.txt for licensing terms.
# ---------------------------------------------------------------

if [ `uname` = "Darwin" ]; then
   SLIB_EXT=".dylib"
else
   SLIB_EXT=".so"
fi

NAME="${1%${SLIB_EXT}}"
LIB_NAME="${NAME##*/}"
DSTDIR="${NAME%${LIB_NAME}}"
[ -n "${DSTDIR}" ] || DSTDIR="./"

if [ $# -lt 2 ] || [ -z "${LIB_NAME}" ]
then
    echo "usage: `basename $0` <target[${SLIB_EXT}]> [link options] src1.a .. srcN.a [obj1.o .. objN.o]"
    exit 1
fi

shift

BASE=`echo ${LIB_NAME} | sed "s/\([^.-]*\)[.-][0-9.]*/\1/g"`
VERSION="${LIB_NAME#${BASE}}"
VERSION="${VERSION#[.-]}"
REVIS="${VERSION}"
MAJOR="${REVIS%%.*}"
REVIS="${REVIS#${MAJOR}}"
REVIS="${REVIS#.}"
MINOR="${REVIS%%.*}"
REVIS="${REVIS#${MINOR}}"
REVIS="${REVIS#.}"
REVIS="${REVIS%%.*}"
[ -n "${MAJOR}" ] || MAJOR=0
[ -n "${MINOR}" ] || MINOR=1
[ -n "${REVIS}" ] || REVIS=0
VERSION="${MAJOR}.${MINOR}.${REVIS}"

OTMPDIR="/tmp/hb-mkslib-$$"
dir=`pwd`

cleanup()
{
    rm -fR "${OTMPDIR}"
}

trap cleanup EXIT &>/dev/null

rm -fR "${OTMPDIR}"
mkdir -p "${OTMPDIR}"
cd "${OTMPDIR}"

for f in $*
do
    case "${f}" in
	*.o)
            if [ ! -r "${dir}/${f}" ]
	    then
	        echo "cannot read file: ${f}"
	        exit 1
	    fi
	    cp "${dir}/${f}" "${OTMPDIR}" || exit 1
	    ;;
	*.a)
            if [ ! -r "${dir}/${f}" ]
	    then
	        echo "cannot read file: ${f}"
	        exit 1
	    fi
	    d="${f%.a}"
	    d="${f##*/}"
	    mkdir $d
	    cd $d
	    ar -x "${dir}/${f}" || exit 1
	    cd ..
	    ;;
	*)
            linker_options="${linker_options} ${f}"
	    ;;
    esac
done
OBJLST=`find . -name \*.o`

cd "${OTMPDIR}"
if [ `uname` = "Darwin" ]; then
    FULLNAME="${BASE}.${VERSION}${SLIB_EXT}"
    ld -r -o "${FULLNAME}.o" $OBJLST && \
    gcc -dynamiclib -install_name "${BASE}.${MAJOR}${SLIB_EXT}" \
        -compatibility_version ${MAJOR}.${MINOR} -current_version ${VERSION} \
        -flat_namespace -undefined warning -multiply_defined suppress \
        -o "${FULLNAME}" "${FULLNAME}.o" ${linker_options} && \
    cd "${dir}" && \
    mv -f "${OTMPDIR}/${FULLNAME}" "${DSTDIR}${FULLNAME}" && \
    ln -sf "${FULLNAME}" "${DSTDIR}${BASE}.${MAJOR}${SLIB_EXT}" && \
    ln -sf "${FULLNAME}" "${DSTDIR}${BASE}${SLIB_EXT}"
else
    #FULLNAME="${BASE}-${VERSION}${SLIB_EXT}"
    #FULLNAME="${BASE}{SLIB_EXT}.${VERSION}"
    FULLNAME="${LIB_NAME}${SLIB_EXT}"
    gcc -shared -o "${FULLNAME}" $OBJLST ${linker_options} && \
        cd "${dir}" && \
        mv -f "${OTMPDIR}/${FULLNAME}" "${DSTDIR}${FULLNAME}"
fi

stat="$?"
[ $stat != 0 ] && cd "${dir}" && rm -f "${DSTDIR}${FULLNAME}"
cleanup
exit "${stat}"
