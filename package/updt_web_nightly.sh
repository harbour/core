#!/bin/sh

# ---------------------------------------------------------------
# Copyright 2012-2013 Viktor Szakats (harbour syenar.net)
# See COPYING.txt for licensing terms.
# ---------------------------------------------------------------

#
# HOWTO upload unattended to sourceforge.net
# ==========================================
#
# 1. Generate SSH key pair:
#
#    $ ssh-keygen -t dsa -C "$HB_SFNET_USER,harbour-project@web.sourceforge.net"
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
#    $ export HB_SFNET_WEB_PRIVATE_KEY=~/.ssh/id_dsa
#    $ export HB_SFNET_USER=myuser
#    $ ./updt_web_nightly.sh
#
# Official link with more detailed procedures:
#    http://sourceforge.net/apps/trac/sourceforge/wiki/SSH%20keys
#

echo Starting Harbour website update...

rm -f -r _hb_web_src
mkdir _hb_web_src || {
   echo "Failed to create directory _hb_web_src"
   exit 1
}
cd _hb_web_src

echo Downloading sources...

wget https://github.com/harbour/website/archive/master.tar.gz
tar -zxvf master.tar.gz

echo Updating website...

destdir="/home/groups/h/ha/harbour-project/htdocs/"

if [ -d $destdir ]
then
   echo Copying site to sf.net web area...

   rsync -r website-master/* $destdir
else
   if [ "$HB_SFNET_WEB_PRIVATE_KEY" -a "$HB_SFNET_USER" ]
   then
      echo Uploading site to sf.net web area...
      desthost=",harbour-project@web.sourceforge.net:"
      rsync -zr -e "ssh -i $HB_SFNET_WEB_PRIVATE_KEY" website/* $HB_SFNET_USER$desthost$destdir
   fi
fi

cd ..
rm -f -r _hb_web_src

echo Ended Harbour website update.
