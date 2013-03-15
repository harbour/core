#!/bin/sh
#
# $Id$
#

# ---------------------------------------------------------------
# Copyright 2010 Viktor Szakats (harbour syenar.net)
# See COPYING.txt for licensing terms.
# ---------------------------------------------------------------

#
# HOWTO upload unattended to sourceforge.net
# ==========================================
#
# 1. Generate SSH key pair:
#
#    $ ssh-keygen -t dsa -C "$HB_SFNET_USER,harbour-project@frs.sourceforge.net"
#
#    where '$HB_SFNET_USER' is your sf.net username.
#
#    IMPORTANT: Leave pass-phrase empty
#
# 2. Paste content of generated public key file (by default 'id_dsa.pub') into
#    'Authorized keys' edit box and press 'Update' button on this page:
#
#    https://sourceforge.net/account/ssh
#
#    You have to be logged in to sf.net.
#
# 3. Wait up to 10 minutes for key to be activated
#
# 4. Now you can use your private key file (by default 'id_dsa') instead of
#    password to upload files. To use this script as is, set 'HB_SFNET_FRS_PRIVATE_KEY'
#    envvar to your private key file and 'HB_SFNET_USER' envvar to your
#    sf.net username, f.e.:
#
#    $ export HB_SFNET_FRS_PRIVATE_KEY=~/.ssh/id_dsa
#    $ export HB_SFNET_USER=myuser
#    $ ./mpkg_src_nightly.sh
#
# Official link with more detailed procedures:
#    http://sourceforge.net/apps/trac/sourceforge/wiki/SSH%20keys
#

echo Starting Harbour nightly source package creation...

rm -f harbour-nightly-src.zip harbour-nightly.tar.bz2 harbour-nightly.tar.gz harbour-nightly.tar.xz

rm -f -r _hb_mpkg_src
mkdir _hb_mpkg_src || {
   echo "Failed to create directory _hb_mpkg_src"
   exit 1
}
cd _hb_mpkg_src

echo Downloading sources with LF line ending...

svn export -q --native-eol LF http://harbour-project.svn.sourceforge.net/svnroot/harbour-project/trunk/harbour

echo Creating bz2, gz packages...

tar -c harbour/* > harbour-nightly.tar
bzip2 -c -z harbour-nightly.tar > ../harbour-nightly.tar.bz2
gzip -c harbour-nightly.tar > ../harbour-nightly.tar.gz
xz -c harbour-nightly.tar > ../harbour-nightly.tar.xz
rm harbour-nightly.tar

echo Downloading sources with CRLF line ending...

rm -f -r harbour
svn export -q --native-eol CRLF http://harbour-project.svn.sourceforge.net/svnroot/harbour-project/trunk/harbour

echo Creating zip package...

zip -q -X -r -o ../harbour-nightly-src.zip harbour/*

cd ..
rm -f -r _hb_mpkg_src

destdir="/home/frs/project/h/ha/harbour-project/source/nightly/"

if [ -d $destdir ]
then
   echo Copying packages to sf.net file release area...

   cp harbour-nightly-src.zip $destdir
   cp harbour-nightly.tar.bz2 $destdir
   cp harbour-nightly.tar.gz  $destdir
   cp harbour-nightly.tar.xz  $destdir
else
   if [ "$HB_SFNET_FRS_PRIVATE_KEY" -a "$HB_SFNET_USER" ]
   then

      echo Uploading packages to sf.net file release area...

      desthost=",harbour-project@frs.sourceforge.net:"
      scp -i $HB_SFNET_FRS_PRIVATE_KEY harbour-nightly-src.zip $HB_SFNET_USER$desthost$destdir
      scp -i $HB_SFNET_FRS_PRIVATE_KEY harbour-nightly.tar.bz2 $HB_SFNET_USER$desthost$destdir
      scp -i $HB_SFNET_FRS_PRIVATE_KEY harbour-nightly.tar.gz  $HB_SFNET_USER$desthost$destdir
      scp -i $HB_SFNET_FRS_PRIVATE_KEY harbour-nightly.tar.xz  $HB_SFNET_USER$desthost$destdir
   fi
fi

rm -f harbour-nightly-src.zip harbour-nightly.tar.bz2 harbour-nightly.tar.gz harbour-nightly.tar.xz

echo Ended Harbour nightly source package creation.
