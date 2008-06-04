/*
 * $Id$
 */

/* NOTE: Redirect STDERR to a file to see the verbose output. */

#include "hbcurl.ch"

#include "common.ch"

#define UPLOAD_FILE_AS      "test_ul.bin"
#define REMOTE_URL          "ftp://harbour:power@localhost/" + UPLOAD_FILE_AS
#define RENAME_FILE_TO      "test_ul_renamed.bin"

FUNCTION Main( cDL, cUL )
   LOCAL curl
   LOCAL info
   LOCAL tmp
   LOCAL tmp1

   ? curl_version()
   ? curl_getdate( "Sun, 1 Jun 2008 02:10:58 +0200" )
   
   info := curl_version_info()

   FOR tmp := 1 TO Len( info )
      IF tmp == 8
         ? tmp, ""
         FOR tmp1 := 1 TO Len( info[ 8 ] )
            ?? info[ 8 ][ tmp1 ] + " "
         NEXT
      ELSE
         ? tmp, info[ tmp ]
      ENDIF
   NEXT

   Inkey( 0 )

   ? "INIT:", curl_global_init()

   IF ! Empty( curl := curl_easy_init() )

      ? "ESCAPE:", tmp := curl_easy_escape( curl, "http://domain.com/my dir with space&more/" )
      ? "UNESCAPE:", curl_easy_unescape( curl, tmp )

      Inkey( 0 )

      DEFAULT cUL TO "ftp_uldl.prg"

      ? curl_easy_setopt( curl, HB_CURLOPT_UPLOAD )
      ? curl_easy_setopt( curl, HB_CURLOPT_URL, REMOTE_URL )
      ? curl_easy_setopt( curl, HB_CURLOPT_SETUPLOADFILE, cUL )
      ? curl_easy_setopt( curl, HB_CURLOPT_INFILESIZE, hb_FSize( cUL ) ), hb_FSize( cUL )
//    ? curl_easy_setopt( curl, HB_CURLOPT_INFILESIZE_LARGE, hb_FSize( cUL ) ), hb_FSize( cUL ) // TOFIX
//    ? curl_easy_setopt( curl, HB_CURLOPT_USERPWD, "harbour:power" ) /* May use this instead of embedding in URL */
      ? curl_easy_setopt( curl, HB_CURLOPT_SETPROGRESS, {| nPos, nLen | a := CurGet(), DispOutAt( 10, 10, Str( ( nPos / nLen ) * 100, 6, 2 ) + "%" ), CurSet( a ) } )
      ? curl_easy_setopt( curl, HB_CURLOPT_NOPROGRESS, .F. )
      ? curl_easy_setopt( curl, HB_CURLOPT_POSTQUOTE, { "RNFR " + UPLOAD_FILE_AS, "RNTO " + RENAME_FILE_TO } )
      ? curl_easy_setopt( curl, HB_CURLOPT_VERBOSE, .F. )

      ? "UPLOAD:", curl_easy_perform( curl )

      ? curl_easy_getinfo( curl, HB_CURLINFO_EFFECTIVE_URL )
      ? curl_easy_getinfo( curl, HB_CURLINFO_TOTAL_TIME )

      info := curl_easy_getinfo( curl, HB_CURLINFO_SSL_ENGINES )
      ?
      FOR tmp := 1 TO Len( info )
         ?? info[ tmp ] + " "
      NEXT

      curl_easy_reset( curl )

      Inkey( 0 )

      DEFAULT cDL TO "ftp://ftp.cisco.com/README"

      /* Now let's download something */

      ? curl_easy_setopt( curl, HB_CURLOPT_DOWNLOAD )
      ? curl_easy_setopt( curl, HB_CURLOPT_URL, cDL )
      ? curl_easy_setopt( curl, HB_CURLOPT_SSL_VERIFYPEER, .F. )
      ? curl_easy_setopt( curl, HB_CURLOPT_SSL_VERIFYHOST, .F. )
      ? curl_easy_setopt( curl, HB_CURLOPT_SETDOWNLOADFILE, "test_dl.bin" )
      ? curl_easy_setopt( curl, HB_CURLOPT_SETPROGRESS, {| nPos, nLen | a := CurGet(), DispOutAt( 11, 10, Str( ( nPos / nLen ) * 100, 6, 2 ) + "%" ), CurSet( a ) } )
      ? curl_easy_setopt( curl, HB_CURLOPT_NOPROGRESS, .F. )
      ? curl_easy_setopt( curl, HB_CURLOPT_VERBOSE, .F. )

      ? "DOWNLOAD:", curl_easy_perform( curl )

      /* Cleanup session */

      curl_easy_cleanup( curl )
   ENDIF

   curl_global_cleanup()

   RETURN NIL

STATIC FUNCTION CurGet()
   RETURN { Row(), Col() }

STATIC PROCEDURE CurSet( a )
   SetPos( a[ 1 ], a[ 2 ] )
   RETURN
