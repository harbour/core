Name:                  Advantage Client Engine API [win, linux, free, closed-source]
URL:                   http://www.sybase.com/products/databasemanagement/advantagedatabaseserver/client-engine-api
Environment variable:  HB_WITH_ADS=C:\ads\acesdk
Install (Linux):       Download 'Advantage Client Engine API for Linux' (f.e. 'aceapi-11.10.0.10.tar.gz')
mpkg_rpm.sh option:    --with ads


RDD for Advantage Database Server
Alexander Kresin <alex@belacy.belgorod.su>


For using this RDD you need to have all required dynamic libraries
installed on your system.

For building executables don't forget to include rddads.hbc in your
hbmk2 project.

You need to include in your prg file the following lines:

   REQUEST ADS

You can also use:

   REQUEST ADT | ADSNTX | ADSCDX | ADSVFP

and then you can set default RDD using one of the following functions:

   rddSetDefault( "ADT" )
   rddSetDefault( "ADSNTX" )
   rddSetDefault( "ADSCDX" )
   rddSetDefault( "ADSVFP" )

and then
   #include "ads.ch"
   SET FILETYPE TO NTX | CDX | ADT | VFP
command, or function
   AdsSetFileType()
to set table type (default is CDX)

By default RDD is tuned for remote server. To change this you may
use commands, defined in ads.ch:

   SET SERVER LOCAL
command, or function
   AdsSetServerType()
