/*
 * $DOC$
 * $FUNCNAME$
 *      Overview
 * $CATEGORY$
 *      Document
 * $ONELINER$
 *      Rdd ads Read me
 * $DESCRIPTION$
 *      RDD for Advantage Database Server
 *      Alexander Kresin <alex@belacy.belgorod.su>
 *
 *      <b>For using this RDD you need to have:   </b></par>
 *      <b>ace32.dll  ( Advantage Client Engine ),   </b></par>
 *      <b>AXCWS32.DLL ( communication layer for remote server ) or   </b></par>
 *      <b>ADSLOC32.DLL ( local server )   </b></par>
 *   
 *      You need also to create ace32.lib with the help of implib.exe:
 *      implib ace32.lib ace32.dll
 *
 *      Then build rddads.lib using make_b32.bat or make_vc.bat.
 *
 *      For building executables don't forget to include the ace32.lib and
 *      rddads.lib to the make file or link script.
 *
 *      You need also to include to your prg file following lines:
 *
 *      REQUEST _ADS   </par>
 *      rddRegister( "ADS", 1 )   </par>
 *      rddsetdefault( "ADS" )   </par>
 *
 *      By default RDD is tuned for remote server and cdx indexes. To
 *      change this you may use commands, defined in ads.ch:
 *
 *      SET SERVER LOCAL
 *
 *      SET FILETYPE TO NTX ( SET FILETYPE TO ADT )
 *
 *      or functions AdsSetServerType(), AdsSetFileType().
 * $END$
 */
