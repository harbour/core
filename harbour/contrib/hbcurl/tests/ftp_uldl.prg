/*
 * $Id$
 */

/* NOTE: Redirect STDERR to a file to see the verbose output. */

#require "hbcurl"

#include "fileio.ch"

#define UPLOAD_FILE_AS      "test_ul.bin"
#define RENAME_FILE_TO      "test_ul_renamed.bin"
#define REMOTE_URL          "ftp://harbour:power@localhost/" + UPLOAD_FILE_AS
#define REMOTE_URL_DEL      "ftp://harbour:power@localhost/" + RENAME_FILE_TO
#define REMOTE_URL_MEM      "ftp://harbour:power@localhost/from_mem.txt"

PROCEDURE Main( cDL, cUL )

   LOCAL curl
   LOCAL info
   LOCAL tmp
   LOCAL tmp1
   LOCAL f
   LOCAL a

   LOCAL lVerbose := .F.

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

   ? "Press key..."
   Inkey( 0 )

   ? "INIT:", curl_global_init()

   IF ! Empty( curl := curl_easy_init() )

      ? "ESCAPE:", tmp := curl_easy_escape( curl, "http://domain.com/my dir with space&more/" )
      ? "UNESCAPE:", curl_easy_unescape( curl, tmp )

      ? "Press key..."
      Inkey( 0 )

      hb_default( @cUL, __FILE__ )

      ? curl_easy_setopt( curl, HB_CURLOPT_UPLOAD )
      ? curl_easy_setopt( curl, HB_CURLOPT_URL, REMOTE_URL )
      ? curl_easy_setopt( curl, HB_CURLOPT_UL_FILE_SETUP, cUL )
      ? curl_easy_setopt( curl, HB_CURLOPT_INFILESIZE, hb_FSize( cUL ) ), hb_FSize( cUL )
#if 0
      /* May use this instead of embedding in URL */
      ? curl_easy_setopt( curl, HB_CURLOPT_USERPWD, "harbour:power" )
#endif
      ? curl_easy_setopt( curl, HB_CURLOPT_PROGRESSBLOCK, {| nPos, nLen | a := CurGet(), hb_DispOutAt( 10, 10, Str( ( nPos / nLen ) * 100, 6, 2 ) + "%" ), CurSet( a ) } )
      ? curl_easy_setopt( curl, HB_CURLOPT_NOPROGRESS, .F. )
      ? curl_easy_setopt( curl, HB_CURLOPT_POSTQUOTE, { "RNFR " + UPLOAD_FILE_AS, "RNTO " + RENAME_FILE_TO } )
      ? curl_easy_setopt( curl, HB_CURLOPT_VERBOSE, lVerbose )

      ? "UPLOAD FILE:", curl_easy_perform( curl )

      ? curl_easy_getinfo( curl, HB_CURLINFO_EFFECTIVE_URL )
      ? curl_easy_getinfo( curl, HB_CURLINFO_TOTAL_TIME )

      info := curl_easy_getinfo( curl, HB_CURLINFO_SSL_ENGINES, @tmp )
      ? "SSL ENGINES: ", tmp, Len( info )
      FOR tmp := 1 TO Len( info )
         ?? info[ tmp ] + " "
      NEXT

      curl_easy_reset( curl )

      ? "Press key..."
      Inkey( 0 )

      /* Delete file */

      ? curl_easy_setopt( curl, HB_CURLOPT_UPLOAD )
      ? curl_easy_setopt( curl, HB_CURLOPT_UL_NULL_SETUP )
      ? curl_easy_setopt( curl, HB_CURLOPT_URL, REMOTE_URL_DEL )
#if 0
      /* May use this instead of embedding in URL */
      ? curl_easy_setopt( curl, HB_CURLOPT_USERPWD, "harbour:power" )
#endif
      ? curl_easy_setopt( curl, HB_CURLOPT_NOPROGRESS )
      ? curl_easy_setopt( curl, HB_CURLOPT_POSTQUOTE, { "DELE " + RENAME_FILE_TO } )
      ? curl_easy_setopt( curl, HB_CURLOPT_VERBOSE, lVerbose )

      ? "DELETE FILE:", curl_easy_perform( curl )

      curl_easy_reset( curl )

      ? "Press key..."
      Inkey( 0 )

      /* Upload file from memory */

      tmp := "This will be the content of the file"

      ? curl_easy_setopt( curl, HB_CURLOPT_UPLOAD )
      ? curl_easy_setopt( curl, HB_CURLOPT_URL, REMOTE_URL_MEM )
      ? curl_easy_setopt( curl, HB_CURLOPT_UL_BUFF_SETUP, tmp )
      ? curl_easy_setopt( curl, HB_CURLOPT_INFILESIZE, Len( tmp ) ), Len( tmp )
#if 0
      /* May use this instead of embedding in URL */
      ? curl_easy_setopt( curl, HB_CURLOPT_USERPWD, "harbour:power" )
#endif
      ? curl_easy_setopt( curl, HB_CURLOPT_PROGRESSBLOCK, {| nPos, nLen | a := CurGet(), hb_DispOutAt( 10, 10, Str( ( nPos / nLen ) * 100, 6, 2 ) + "%" ), CurSet( a ) } )
      ? curl_easy_setopt( curl, HB_CURLOPT_NOPROGRESS, .F. )
      ? curl_easy_setopt( curl, HB_CURLOPT_VERBOSE, lVerbose )

      ? "UPLOAD FILE FROM MEMORY:", curl_easy_perform( curl )

      ? curl_easy_getinfo( curl, HB_CURLINFO_EFFECTIVE_URL )
      ? curl_easy_getinfo( curl, HB_CURLINFO_TOTAL_TIME )

      curl_easy_reset( curl )

      ? "Press key..."
      Inkey( 0 )

      hb_default( @cDL, "ftp://ftp.cisco.com/README" )

      /* Now let's download to a file */

      ? curl_easy_setopt( curl, HB_CURLOPT_DOWNLOAD )
      ? curl_easy_setopt( curl, HB_CURLOPT_URL, cDL )
      ? curl_easy_setopt( curl, HB_CURLOPT_SSL_VERIFYPEER, .F. )
      ? curl_easy_setopt( curl, HB_CURLOPT_SSL_VERIFYHOST, .F. )
      ? curl_easy_setopt( curl, HB_CURLOPT_DL_FILE_SETUP, "test_dl.bin" )
      ? curl_easy_setopt( curl, HB_CURLOPT_PROGRESSBLOCK, {| nPos, nLen | a := CurGet(), hb_DispOutAt( 11, 10, Str( ( nPos / nLen ) * 100, 6, 2 ) + "%" ), CurSet( a ) } )
      ? curl_easy_setopt( curl, HB_CURLOPT_NOPROGRESS, .F. )
      ? curl_easy_setopt( curl, HB_CURLOPT_VERBOSE, lVerbose )

      ? "DOWNLOAD FILE:", curl_easy_perform( curl )

      curl_easy_reset( curl )

      ? "Press key..."
      Inkey( 0 )

      /* Now let's download to memory */

      ? curl_easy_setopt( curl, HB_CURLOPT_DOWNLOAD )
      ? curl_easy_setopt( curl, HB_CURLOPT_URL, cDL )
      ? curl_easy_setopt( curl, HB_CURLOPT_SSL_VERIFYPEER, .F. )
      ? curl_easy_setopt( curl, HB_CURLOPT_SSL_VERIFYHOST, .F. )
      ? curl_easy_setopt( curl, HB_CURLOPT_DL_BUFF_SETUP )
      ? curl_easy_setopt( curl, HB_CURLOPT_PROGRESSBLOCK, {| nPos, nLen | a := CurGet(), hb_DispOutAt( 11, 10, Str( ( nPos / nLen ) * 100, 6, 2 ) + "%" ), CurSet( a ) } )
      ? curl_easy_setopt( curl, HB_CURLOPT_NOPROGRESS, .F. )
      ? curl_easy_setopt( curl, HB_CURLOPT_VERBOSE, lVerbose )

      ? "DOWNLOAD FILE TO MEM:", curl_easy_perform( curl )

      tmp := "test_dlm.bin"
      ? "WRITING TO FILE: ", tmp
      f := FCreate( tmp, FC_NORMAL )
      IF f != F_ERROR
         FWrite( f, curl_easy_dl_buff_get( curl ) )
         FClose( f )
      ENDIF

      curl_easy_reset( curl )

      ? "Press key..."
      Inkey( 0 )

      hb_default( @cDL, "ftp://ftp.cisco.com/" )

      /* Now let's download a dirlist to memory */

      ? curl_easy_setopt( curl, HB_CURLOPT_DOWNLOAD )
      ? curl_easy_setopt( curl, HB_CURLOPT_DIRLISTONLY )
      ? curl_easy_setopt( curl, HB_CURLOPT_URL, cDL )
      ? curl_easy_setopt( curl, HB_CURLOPT_SSL_VERIFYPEER, .F. )
      ? curl_easy_setopt( curl, HB_CURLOPT_SSL_VERIFYHOST, .F. )
      ? curl_easy_setopt( curl, HB_CURLOPT_DL_BUFF_SETUP )
      ? curl_easy_setopt( curl, HB_CURLOPT_PROGRESSBLOCK, {| nPos, nLen | a := CurGet(), hb_DispOutAt( 11, 10, Str( ( nPos / nLen ) * 100, 6, 2 ) + "%" ), CurSet( a ) } )
      ? curl_easy_setopt( curl, HB_CURLOPT_NOPROGRESS, .F. )
      ? curl_easy_setopt( curl, HB_CURLOPT_VERBOSE, lVerbose )

      ? "DOWNLOAD DIRLIST TO STRING:", curl_easy_perform( curl )

      ? "RESULT 1: " + curl_easy_dl_buff_get( curl )
      ? curl_easy_setopt( curl, HB_CURLOPT_DL_BUFF_GET, @tmp )
      ? "RESULT 2: " + tmp

      /* Cleanup session */

      curl_easy_cleanup( curl )

   ENDIF

   curl_global_cleanup()

   RETURN

STATIC FUNCTION CurGet()
   RETURN { Row(), Col() }

STATIC PROCEDURE CurSet( a )

   SetPos( a[ 1 ], a[ 2 ] )

   RETURN
