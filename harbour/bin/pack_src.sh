#!/bin/sh
#
# $Id$
#
# This script requires "TAR" utilities for compression.

hb_ver="0.42.0"

hb_archbin="tar"
hb_archopt="-cz --ignore-failed-read -f"
hb_ext=".tar.gz"
hb_filename="harbour-${hb_ver}.src${hb_ext}"
[ -f $hb_filename ] && rm -f $hb_filename

if [ ! -f bin/pack_src.sh ]; then
  hb_rootdir=`dirname $0`
  hb_rootdir="${hb_rootdir}/../"
  hb_archopt="-C $hb_rootdir $hb_archopt"
else
  hb_rootdir="."
fi

#[ -z "$TZ" ] && export TZ=PST8PDT

hb_collect_all()
{

hb_collect="echo"

# README.TXT

# ROOT
$hb_collect *.bat
$hb_collect *.sh
$hb_collect *.cmd
$hb_collect *.spec
$hb_collect [Mm]akefile*
$hb_collect Change[Ll]og*
$hb_collect COPYING TODO ERRATA

# BIN
$hb_collect bin/*.bat
$hb_collect bin/*.sh

# CONFIG
$hb_collect config/*.cf
$hb_collect config/bsd/*.cf
$hb_collect config/dos/*.cf
$hb_collect config/linux/*.cf
$hb_collect config/os2/*.cf
$hb_collect config/w32/*.cf

# DOC
$hb_collect doc/*.txt
$hb_collect doc/en/*.txt
$hb_collect doc/es/*.txt

# INCLUDE
$hb_collect include/Makefile
$hb_collect include/*.[ch]
$hb_collect include/*.api
$hb_collect include/*.ch

# SOURCE\COMMON
$hb_collect source/common/Makefile
$hb_collect source/common/*.[ch]

# SOURCE
$hb_collect source/Makefile

# SOURCE\COMPILER
$hb_collect source/compiler/Makefile
$hb_collect source/compiler/*.[cyl]
$hb_collect source/compiler/*.simple
$hb_collect source/compiler/*.sl[xy]

# SOURCE\DEBUG
$hb_collect source/debug/Makefile
$hb_collect source/debug/*.prg

# SOURCE\LANG
$hb_collect source/lang/Makefile
$hb_collect source/lang/*.[ch]

# SOURCE\CODEPAGE
$hb_collect source/codepage/Makefile
$hb_collect source/codepage/*.[ch]

# SOURCE\MACRO
$hb_collect source/macro/Makefile
$hb_collect source/macro/*.[cyl]
$hb_collect source/macro/*.slx

# SOURCE\PP
$hb_collect source/pp/Makefile
$hb_collect source/pp/*.[ch]

# SOURCE\RDD
$hb_collect source/rdd/Makefile
$hb_collect source/rdd/*.[ch]
$hb_collect source/rdd/*.prg

# SOURCE\RDD\DBFCDX
$hb_collect source/rdd/dbfcdx/Makefile
$hb_collect source/rdd/dbfcdx/*.[ch]
$hb_collect source/rdd/dbfcdx/*.prg

# SOURCE\RDD\DBFNTX
$hb_collect source/rdd/dbfntx/Makefile
$hb_collect source/rdd/dbfntx/*.[ch]
$hb_collect source/rdd/dbfntx/*.prg

# SOURCE\RDD\NULSYS
$hb_collect source/rdd/nulsys/Makefile
$hb_collect source/rdd/nulsys/*.prg

# SOURCE\RTL
$hb_collect source/rtl/Makefile
$hb_collect source/rtl/*.[ch]
$hb_collect source/rtl/*.prg

# SOURCE\RTL\GT_TPL
$hb_collect source/rtl/gt_tpl/Makefile
$hb_collect source/rtl/gt_tpl/*.[ch]

# SOURCE\RTL\GTNUL
$hb_collect source/rtl/gtnul/Makefile*
$hb_collect source/rtl/gtnul/*.[ch]

# SOURCE\RTL\GTCGI
$hb_collect source/rtl/gtcgi/Makefile
$hb_collect source/rtl/gtcgi/*.[ch]

# SOURCE\RTL\GTCRS
$hb_collect source/rtl/gtcrs/Makefile
$hb_collect source/rtl/gtcrs/*.[ch]
$hb_collect source/rtl/gtcrs/*.def

# SOURCE\RTL\GTDOS
$hb_collect source/rtl/gtdos/Makefile
$hb_collect source/rtl/gtdos/*.[ch]

# SOURCE\RTL\GTOS2
$hb_collect source/rtl/gtos2/Makefile
$hb_collect source/rtl/gtos2/*.[ch]
$hb_collect source/rtl/gtos2/*.gcc

# SOURCE\RTL\GTPCA
$hb_collect source/rtl/gtpca/Makefile
$hb_collect source/rtl/gtpca/*.[ch]

# SOURCE\RTL\GTSLN
$hb_collect source/rtl/gtsln/Makefile
$hb_collect source/rtl/gtsln/*.[ch]

# SOURCE\RTL\GTSTD
$hb_collect source/rtl/gtstd/Makefile
$hb_collect source/rtl/gtstd/*.[ch]

# SOURCE\RTL\GTWIN
$hb_collect source/rtl/gtwin/Makefile
$hb_collect source/rtl/gtwin/*.[ch]

# SOURCE\VM
$hb_collect source/vm/Makefile
$hb_collect source/vm/*.[ch]
$hb_collect source/vm/*.prg

# TESTS
$hb_collect tests/*.bat
$hb_collect tests/*.ch
$hb_collect tests/*.dbf
$hb_collect tests/*.fpt
$hb_collect tests/*.prg
$hb_collect tests/*.txt

# UTILS
$hb_collect utils/Makefile*

# UTILS\HBDOC
$hb_collect utils/hbdoc/Makefile
$hb_collect utils/hbdoc/*.ch
$hb_collect utils/hbdoc/*.prg

# UTILS\HBEXTERN
$hb_collect utils/hbextern/Makefile
$hb_collect utils/hbextern/*.bat
$hb_collect utils/hbextern/*.prg

# UTILS\HBMAKE
$hb_collect utils/hbmake/Makefile
$hb_collect utils/hbmake/*.ch
$hb_collect utils/hbmake/*.prg
$hb_collect utils/hbmake/*.[ch]

# UTILS\HBPP
$hb_collect utils/hbpp/Makefile
$hb_collect utils/hbpp/*.[ch]

# UTILS\HBRUN
$hb_collect utils/hbrun/Makefile
$hb_collect utils/hbrun/*.prg

# UTILS\HBTEST
$hb_collect utils/hbtest/Makefile
$hb_collect utils/hbtest/*.ch
$hb_collect utils/hbtest/*.cmd
$hb_collect utils/hbtest/*.prg

# CONTRIB\LIBCT
$hb_collect contrib/libct/Makefile
$hb_collect contrib/libct/*.[ch]
$hb_collect contrib/libct/*.prg
$hb_collect contrib/libct/*.ch

# CONTRIB\DOT
$hb_collect contrib/dot/*.prg
$hb_collect contrib/dot/*.ch
$hb_collect contrib/dot/*.txt

}

hb_flst=`cd "$hb_rootdir";hb_collect_all|grep -v "[*?[]"`

$hb_archbin $hb_archopt $hb_filename $hb_flst
