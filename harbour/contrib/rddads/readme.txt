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

   Then build rddads.lib using make_b32.bat or make_vc.bat.

   For building executables don't forget to include the ace32.lib and
   rddads.lib to the make file or link script.

   You need also to include to your prg file following lines:

      REQUEST ADS
      rddRegister( "ADS", 1 )
      rddsetdefault( "ADS" )

   By default RDD is tuned for remote server and cdx indexes. To
   change this you may use commands, defined in ads.ch:

     SET SERVER LOCAL
     SET FILETYPE TO NTX / SET FILETYPE TO ADT

   or functions AdsSetServerType(), AdsSetFileType().
