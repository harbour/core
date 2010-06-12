#!/bin/sh
[ "$BASH" ] || exec bash $0 "$@"
#
# $Id$
#

[ -n harbour-nightly-src.zip ] || rm harbour-nightly-src.zip
[ -n harbour-nightly.tar.bz2 ] || rm harbour-nightly.tar.bz2
[ -n harbour-nightly.tar.gz ]  || rm harbour-nightly.tar.gz

rm -f -r _mk_nightly
mkdir _mk_nightly
cd _mk_nightly

rm -f -r harbour
svn export --native-eol LF http://harbour-project.svn.sourceforge.net/svnroot/harbour-project/trunk/harbour
tar -c harbour/* > harbour-nightly.tar
bzip2 -c -z harbour-nightly.tar > ../harbour-nightly.tar.bz2
gzip -c harbour-nightly.tar > ../harbour-nightly.tar.gz
rm harbour-nightly.tar

rm -f -r harbour
svn export --native-eol CRLF http://harbour-project.svn.sourceforge.net/svnroot/harbour-project/trunk/harbour
zip -X -r -o ../harbour-nightly-src.zip harbour/*

cd ..
rm -f -r _mk_nightly

scp harbour-nightly-src.zip $USER,harbour-project@frs.sourceforge.net:/home/frs/project/h/ha/harbour-project/source/nightly/
scp harbour-nightly.tar.bz2 $USER,harbour-project@frs.sourceforge.net:/home/frs/project/h/ha/harbour-project/source/nightly/
scp harbour-nightly.tar.gz  $USER,harbour-project@frs.sourceforge.net:/home/frs/project/h/ha/harbour-project/source/nightly/
