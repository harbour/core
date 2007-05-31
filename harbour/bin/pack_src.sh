#!/bin/sh
#
# $Id$
#
# This script requires "TAR" utilities for compression.

hb_archbin="tar"
hb_archopt="-cz --ignore-failed-read -f"
hb_ext=".tar.gz"
if [ -f bin/hb-func.sh ]; then
  hb_rootdir="."
else
  hb_rootdir=`dirname $0`
  hb_rootdir="${hb_rootdir}/.."
  hb_archopt="-C $hb_rootdir $hb_archopt"
fi
. ${hb_rootdir}/bin/hb-func.sh

hb_ver=`get_hbver ${hb_rootdir}`
hb_filename="harbour-${hb_ver}.src${hb_ext}"
[ -f $hb_filename ] && rm -f $hb_filename

#[ -z "$TZ" ] && export TZ=PST8PDT

hb_collect_all()
{

hb_collect="echo"

# README.TXT

# ROOT
$hb_collect *.bat
$hb_collect *.sh
$hb_collect *.cmd
$hb_collect *.mak
$hb_collect *.spec
$hb_collect [Mm]akefile*
$hb_collect Change[Ll]og*
$hb_collect COPYING TODO ERRATA

# BIN
$hb_collect bin/*.bat
$hb_collect bin/*.cmd
$hb_collect bin/*.sh

# CONFIG
$hb_collect config/*.cf
$hb_collect config/dos/*.cf
$hb_collect config/linux/*.cf
$hb_collect config/bsd/*.cf
$hb_collect config/darwin/*.cf
$hb_collect config/hpux/*.cf
$hb_collect config/sunos/*.cf
$hb_collect config/os2/*.cf
$hb_collect config/w32/*.cf

# DOC
$hb_collect doc/[Mm]akefile*
$hb_collect doc/*.txt
$hb_collect doc/genhtm.*
$hb_collect doc/en/[Mm]akefile*
$hb_collect doc/en/*.txt
$hb_collect doc/es/[Mm]akefile*
$hb_collect doc/es/*.txt

# INCLUDE
$hb_collect include/Makefile
$hb_collect include/*.[ch]
$hb_collect include/*.api
$hb_collect include/*.ch

# SOURCE
$hb_collect source/Makefile

# SOURCE\MAIN
$hb_collect source/main/Makefile
$hb_collect source/main/*.[ch]

# SOURCE\COMPILER
$hb_collect source/compiler/Makefile
$hb_collect source/compiler/*.[cylh]
$hb_collect source/compiler/*.yy[ch]

# SOURCE\COMMON
$hb_collect source/common/Makefile
$hb_collect source/common/*.[ch]

# SOURCE\PP
$hb_collect source/pp/Makefile
$hb_collect source/pp/*.[ch]

# SOURCE\DEBUG
$hb_collect source/debug/Makefile
$hb_collect source/debug/*.[ch]
$hb_collect source/debug/*.prg

# SOURCE\LANG
$hb_collect source/lang/Makefile
$hb_collect source/lang/*.[ch]

# SOURCE\CODEPAGE
$hb_collect source/codepage/Makefile
$hb_collect source/codepage/*.[ch]

# SOURCE\MACRO
$hb_collect source/macro/Makefile
$hb_collect source/macro/*.[cylh]
$hb_collect source/macro/*.yy[ch]

# SOURCE\HBPCRE
$hb_collect source/hbpcre/Makefile
$hb_collect source/hbpcre/ChangeLog*
$hb_collect source/hbpcre/*.[ch]

# SOURCE\RDD
$hb_collect source/rdd/Makefile
$hb_collect source/rdd/*.[ch]
$hb_collect source/rdd/*.prg

# SOURCE\RDD\DBFDBT
#$hb_collect source/rdd/dbfdbt/Makefile
#$hb_collect source/rdd/dbfdbt/*.[ch]
#$hb_collect source/rdd/dbfdbt/*.prg

# SOURCE\RDD\DBFFPT
$hb_collect source/rdd/dbffpt/Makefile
$hb_collect source/rdd/dbffpt/*.[ch]
$hb_collect source/rdd/dbffpt/*.prg

# SOURCE\RDD\DBFCDX
$hb_collect source/rdd/dbfcdx/Makefile
$hb_collect source/rdd/dbfcdx/*.[ch]
$hb_collect source/rdd/dbfcdx/*.prg

# SOURCE\RDD\DBFNTX
$hb_collect source/rdd/dbfntx/Makefile
$hb_collect source/rdd/dbfntx/*.[ch]
$hb_collect source/rdd/dbfntx/*.prg

# SOURCE\RDD\HBSIX
$hb_collect source/rdd/hbsix/Makefile
$hb_collect source/rdd/hbsix/*.[ch]
$hb_collect source/rdd/hbsix/*.ch
$hb_collect source/rdd/hbsix/*.prg

# SOURCE\RDD\HSX
$hb_collect source/rdd/hsx/Makefile
$hb_collect source/rdd/hsx/*.[ch]
$hb_collect source/rdd/hsx/*.ch
$hb_collect source/rdd/hsx/*.prg

# SOURCE\RDD\USRRDD
$hb_collect source/rdd/usrrdd/Makefile
$hb_collect source/rdd/usrrdd/*.c
$hb_collect source/rdd/usrrdd/example/*.prg
$hb_collect source/rdd/usrrdd/rdds/Makefile
$hb_collect source/rdd/usrrdd/rdds/*.prg

# SOURCE\RDD\NULSYS
$hb_collect source/rdd/nulsys/Makefile
$hb_collect source/rdd/nulsys/*.c

for d in ${HB_DB_DRVEXT}
do
  $hb_collect source/rdd/$d/Makefile
  $hb_collect source/rdd/$d/*.[ch]
  $hb_collect source/rdd/$d/*.ch
  $hb_collect source/rdd/$d/*.prg
done

# SOURCE\RTL
$hb_collect source/rtl/Makefile
$hb_collect source/rtl/*.[ch]
$hb_collect source/rtl/*.prg

# SOURCE\RTL\GT_TPL
$hb_collect source/rtl/gt_tpl/Makefile
$hb_collect source/rtl/gt_tpl/*.[ch]

# SOURCE\RTL\GTCGI
$hb_collect source/rtl/gtcgi/Makefile
$hb_collect source/rtl/gtcgi/*.[ch]

# SOURCE\RTL\GTCRS
$hb_collect source/rtl/gtcrs/Makefile
$hb_collect source/rtl/gtcrs/*.[ch]
$hb_collect source/rtl/gtcrs/*.def
$hb_collect source/rtl/gtcrs/*.map
$hb_collect source/rtl/gtcrs/*.prg

# SOURCE\RTL\GTDOS
$hb_collect source/rtl/gtdos/Makefile
$hb_collect source/rtl/gtdos/*.[ch]

# SOURCE\RTL\GTGUI
$hb_collect source/rtl/gtgui/Makefile
$hb_collect source/rtl/gtgui/*.[ch]

# SOURCE\RTL\GTOS2
$hb_collect source/rtl/gtos2/Makefile
$hb_collect source/rtl/gtos2/*.[ch]

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

# SOURCE\RTL\GTWVT
$hb_collect source/rtl/gtwvt/Makefile*
$hb_collect source/rtl/gtwvt/*.[ch]

# SOURCE\RTL\GTXVT
$hb_collect source/rtl/gtxvt/Makefile*
$hb_collect source/rtl/gtxvt/*.[ch]

# SOURCE\RTL\GTXWC
$hb_collect source/rtl/gtxwc/Makefile*
$hb_collect source/rtl/gtxwc/*.[ch]

# SOURCE\RTL\GTALLEG
$hb_collect source/rtl/gtalleg/Makefile*
$hb_collect source/rtl/gtalleg/*.[ch]
$hb_collect source/rtl/gtalleg/*.sfc

# SOURCE\VM
$hb_collect source/vm/Makefile
$hb_collect source/vm/*.[ch]
$hb_collect source/vm/*.prg

# SOURCE\VM\MAINSTD
$hb_collect source/vm/mainstd/Makefile

# TESTS
$hb_collect tests/Makefile
$hb_collect tests/*.bat
$hb_collect tests/*.ch
$hb_collect tests/*.cfm
$hb_collect tests/*.ini
$hb_collect tests/*.dbf
$hb_collect tests/*.fpt
$hb_collect tests/*.frm
$hb_collect tests/*.lbl
$hb_collect tests/*.prg
$hb_collect tests/*.src
$hb_collect tests/*.txt
$hb_collect tests/*.[ch]

# TESTS\BLDTEST
$hb_collect tests/bldtest/Makefile
$hb_collect tests/bldtest/*.[ch]

# UTILS
$hb_collect utils/Makefile

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

# UTILS\HBPPTEST
$hb_collect utils/hbpptest/Makefile
$hb_collect utils/hbpptest/*.bat
$hb_collect utils/hbpptest/*.prg

# UTILS\HBRUN
$hb_collect utils/hbrun/Makefile
$hb_collect utils/hbrun/*.prg

# UTILS\HBTEST
$hb_collect utils/hbtest/Makefile
$hb_collect utils/hbtest/*.ch
$hb_collect utils/hbtest/*.cmd
$hb_collect utils/hbtest/*.prg

# UTILS\HBVER
$hb_collect utils/hbver/Makefile
$hb_collect utils/hbver/*.[ch]

# CONTRIB
$hb_collect contrib/Makefile

# CONTRIB\RDD_ADS
$hb_collect contrib/rdd_ads/Makefile
$hb_collect contrib/rdd_ads/*.[ch]
$hb_collect contrib/rdd_ads/*.prg
$hb_collect contrib/rdd_ads/*.ch

# CONTRIB\LIBCT
$hb_collect contrib/libct/Makefile
$hb_collect contrib/libct/*.[ch]
$hb_collect contrib/libct/*.prg
$hb_collect contrib/libct/*.ch

# CONTRIB\LIBNF
$hb_collect contrib/libnf/Makefile
$hb_collect contrib/libnf/*.[ch]
$hb_collect contrib/libnf/*.prg
$hb_collect contrib/libnf/*.ch

# CONTRIB\DOT
$hb_collect contrib/dot/*.prg
$hb_collect contrib/dot/*.ch
$hb_collect contrib/dot/*.txt

# CONTRIB\TIP
$hb_collect contrib/tip/Makefile
$hb_collect contrib/tip/Changelog
$hb_collect contrib/tip/*.[ch]
$hb_collect contrib/tip/*.prg
$hb_collect contrib/tip/*.ch
$hb_collect contrib/tip/*.txt

# CONTRIB\ODBC
$hb_collect contrib/odbc/Makefile
$hb_collect contrib/odbc/*.[ch]
$hb_collect contrib/odbc/*.ch
$hb_collect contrib/odbc/*.prg
$hb_collect contrib/odbc/*.txt

# CONTRIB\PGSQL
$hb_collect contrib/pgsql/[mM]akefile*
$hb_collect contrib/pgsql/Changelog
$hb_collect contrib/pgsql/README
$hb_collect contrib/pgsql/*.[ch]
$hb_collect contrib/pgsql/*.ch
$hb_collect contrib/pgsql/*.prg
$hb_collect contrib/pgsql/*.txt
$hb_collect contrib/pgsql/*.bat

# CONTRIB\MYSQL
$hb_collect contrib/mysql/[mM]akefile*
$hb_collect contrib/mysql/*.[ch]
$hb_collect contrib/mysql/*.ch
$hb_collect contrib/mysql/*.prg
$hb_collect contrib/mysql/*.txt
$hb_collect contrib/mysql/*.bat

# CONTRIB\BTREE
$hb_collect contrib/btree/[mM]akefile*
$hb_collect contrib/btree/*.[ch]
$hb_collect contrib/btree/*.api
$hb_collect contrib/btree/*.ch
$hb_collect contrib/btree/*.prg
$hb_collect contrib/btree/*.bat
$hb_collect contrib/btree/doc/*.txt

# CONTRIB\HTMLLIB
$hb_collect contrib/htmllib/[mM]akefile*
$hb_collect contrib/htmllib/*.ch
$hb_collect contrib/htmllib/*.prg
$hb_collect contrib/htmllib/*.bat

# CONTRIB\LIBGT
$hb_collect contrib/libgt/[Mm]akefile*
$hb_collect contrib/libgt/*.[ch]
$hb_collect contrib/libgt/*.bat
$hb_collect contrib/libgt/doc/gen*
$hb_collect contrib/libgt/doc/lib*
$hb_collect contrib/libgt/doc/en/*.txt

# CONTRIB\LIBMISC
$hb_collect contrib/libmisc/[mM]akefile*
$hb_collect contrib/libmisc/*.[ch]
$hb_collect contrib/libmisc/*.ch
$hb_collect contrib/libmisc/*.prg
$hb_collect contrib/libmisc/doc/gen*
$hb_collect contrib/libmisc/doc/lib*
$hb_collect contrib/libmisc/doc/en/*.txt

# CONTRIB\SAMPLES
$hb_collect contrib/samples/[mM]akefile*
$hb_collect contrib/samples/*.[ch]
$hb_collect contrib/samples/*.ch
$hb_collect contrib/samples/*.prg

}

hb_flst=`cd "$hb_rootdir";hb_collect_all|grep -v "[*?[]"`

$hb_archbin $hb_archopt $hb_filename $hb_flst
