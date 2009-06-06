#!/bin/sh
#

aMainDirs="samples tests"

formats='gnu msvc mingw borland'

echo "Building makefiles for wxHarbour library"

for fmt in $formats ; do
  bakefile -f $fmt -D FORMAT_HAS_MAKE_INSTALL=1 wxharbour.bkl
done

for mainDir in $aMainDirs ; do

  adirs="$(find $mainDir -maxdepth 1 -mindepth 1 -type d \! -name .svn)"

  echo "Building makefiles for $mainDir"

  for DIR in $adirs ; do

    ndir=${DIR/$mainDir\//}
    cp samples/template.bkl $mainDir/$ndir/$ndir.bkl
    SEDCMD=s/__SAMPLE_NAME__/$ndir/
    sed -i $SEDCMD $mainDir/$ndir/$ndir.bkl

    for fmt in $formats ; do
      bakefile -f $fmt $mainDir/$ndir/$ndir.bkl -DFORMAT_HAS_MAKE_INSTALL=1
    done

  done

done
