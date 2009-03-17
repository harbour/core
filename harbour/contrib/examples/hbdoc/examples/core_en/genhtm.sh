#!/bin/sh
#
# $Id$
#

../../bin/hbdoc -htm genhtm.lnk genhtm.rsp
cd htm
#
echo "renaming harbour.htm to index.htm"
mv harbour.htm index.htm
rm -f genwww.lnk
mv genwww.old genwww.lnk
