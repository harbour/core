/*
 * $Id$
 */
/*
 * $DOC$
 * $FUNCNAME$
 *      ADS Overview
 * $CATEGORY$
 *      Document
 * $ONELINER$
 *      Advantage Database Server RDD
 * $DESCRIPTION$
 *      RDDADS is an RDD for the Advantage Database Server, an xBase data
 *      server by Extended Systems <www.advantagedatabase.com>.
 *      The RDD was written by Alexander Kresin <alex@belacy.belgorod.su>
 *
 *      Your Harbour application can access a remote database server for a
 *      true client/server architecture, or it can use the "local server"
 *      ADSLOC32.DLL for stand-alone or even small network installations.
 *
 *      <b>For using this RDD you need to have:   </b></par>
 *      <b>ACE32.DLL    ( Advantage Client Engine ),   </b></par>
 *      <b>AXCWS32.DLL  ( communication layer for remote server ) or   </b></par>
 *      <b>ADSLOC32.DLL ( local server )   </b></par>
 *
 *      You need also to create ace32.lib with the help of implib.exe:
 *      implib ace32.lib ace32.dll
 *
 *      Then build rddads.lib using make_b32.bat or make_vc.bat.
 *
 *      For building executables don't forget to include the ace32.lib and
 *      rddads.lib in the make file or link script.
 *
 *      You also need to include in your PRG file following lines:
 *
 *      REQUEST _ADS   </par>
 *      rddRegister( "ADS", 1 )   </par>
 *      rddsetdefault( "ADS" )   </par>
 *
 *      By default RDDADS is tuned for remote server and cdx indexes. To
 *      change this you may use these commands defined in ads.ch:
 *
 *      SET SERVER LOCAL
 *      SET SERVER REMOTE
 *
 *      SET FILETYPE TO NTX
 *      SET FILETYPE TO ADT
 *      SET FILETYPE TO CDX
 *
 *      or functions AdsSetServerType(), AdsSetFileType().
 *      See the header file ADS.CH for details.
 *
 *      Note that the default local server (ADSLOC32.DLL) is useable for
 *      file sharing on a small network.  The default DLL is limited to
 *      5 users, but an unlimited version is available from Extended Systems.
 * $END$
 */
