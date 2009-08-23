/*
 * $Id$
 */

 RDD for Advantage Database Server
 Alexander Kresin <alex@belacy.belgorod.su>


 For using this RDD you need to have:
   ace32.dll  ( Advantage Client Engine ),
   axcws32.dll ( communication layer for remote server ) or
   adsloc32.dll ( local server )

   You need also to create ace32.lib with the help of implib.exe:
     implib ace32.lib ace32.dll

   For building executables don't forget to include the ace32.lib and
   rddads.lib to the make file or link script.

   You need also to include to your prg file following lines:

      REQUEST ADS

   and then you can set default RDD using one of the following functions:

      rddsetdefault( "ADT" )
      rddsetdefault( "ADSNTX" )
      rddsetdefault( "ADSCDX" )
      rddsetdefault( "ADSVFP" )

   You can also use:

      REQUEST ADT | ADSNTX | ADSCDX | ADSVFP

   instead of REQUEST ADS.

   for backward compatibility with old code it's possible to use also:
      rddsetdefault( "ADS" )
   and then
     SET FILETYPE TO NTX | CDX | ADT | VFP
   command or AdsSetFileType() function to set table type (default is CDX)

   By default RDD is tuned for remote server. To change this you may
   use commands, defined in ads.ch:

     SET SERVER LOCAL

   or function AdsSetServerType().
