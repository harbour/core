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
