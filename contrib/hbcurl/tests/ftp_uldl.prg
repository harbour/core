/* NOTE: Redirect STDERR to a file to see the verbose output. */

#require "hbcurl"

#define UPLOAD_FILE_AS      "test_ul.bin"
#define RENAME_FILE_TO      "test_ul_renamed.bin"
#define REMOTE_URL          "ftp://username:password@localhost/" + UPLOAD_FILE_AS
#define REMOTE_URL_DEL      "ftp://username:password@localhost/" + RENAME_FILE_TO
#define REMOTE_URL_MEM      "ftp://username:password@localhost/from_mem.txt"

PROCEDURE Main( cDL, cUL )

   LOCAL curl
   LOCAL info
   LOCAL tmp
   LOCAL tmp1

   LOCAL lVerbose := .F.

   ? curl_version()
   ? curl_getdate( "Sun, 1 Jun 2008 02:10:58 +0200" )

   FOR EACH tmp IN curl_version_info()
      IF tmp:__enumIndex() == 8
         ? tmp:__enumIndex(), ""
         FOR EACH tmp1 IN tmp
            ?? tmp1, ""
         NEXT
      ELSE
         ? tmp:__enumIndex(), tmp
      ENDIF
   NEXT

   WAIT

   ? "INIT:", curl_global_init()

   IF ! Empty( curl := curl_easy_init() )

      ? "ESCAPE:", tmp := curl_easy_escape( curl, "http://example.com/my dir with space&more/" )
      ? "UNESCAPE:", curl_easy_unescape( curl, tmp )

      WAIT

      hb_default( @cUL, __FILE__ )

      ? curl_easy_setopt( curl, HB_CURLOPT_UPLOAD )
      ? curl_easy_setopt( curl, HB_CURLOPT_URL, REMOTE_URL )
      ? curl_easy_setopt( curl, HB_CURLOPT_UL_FILE_SETUP, cUL )
      ? curl_easy_setopt( curl, HB_CURLOPT_INFILESIZE, hb_FSize( cUL ) ), hb_FSize( cUL )
#if 0
      /* May use this instead of embedding in URL */
      ? curl_easy_setopt( curl, HB_CURLOPT_USERPWD, "username:password" )
#endif
      ? curl_easy_setopt( curl, HB_CURLOPT_XFERINFOBLOCK, {| nPos, nLen | hb_DispOutAt( 10, 10, Str( ( nPos / nLen ) * 100, 6, 2 ) + "%" ) } )
      ? curl_easy_setopt( curl, HB_CURLOPT_NOPROGRESS, 0 )
      ? curl_easy_setopt( curl, HB_CURLOPT_POSTQUOTE, { "RNFR " + UPLOAD_FILE_AS, "RNTO " + RENAME_FILE_TO } )
      ? curl_easy_setopt( curl, HB_CURLOPT_VERBOSE, lVerbose )

      ? "UPLOAD FILE:", curl_easy_perform( curl )

      ? curl_easy_getinfo( curl, HB_CURLINFO_EFFECTIVE_URL )
      ? curl_easy_getinfo( curl, HB_CURLINFO_TOTAL_TIME )

      info := curl_easy_getinfo( curl, HB_CURLINFO_SSL_ENGINES, @tmp )
      ? "SSL ENGINES:", tmp, Len( info )
      FOR EACH tmp IN info
         ?? tmp, ""
      NEXT

      curl_easy_reset( curl )

      WAIT

      /* Delete file */

      ? curl_easy_setopt( curl, HB_CURLOPT_UPLOAD )
      ? curl_easy_setopt( curl, HB_CURLOPT_UL_NULL_SETUP )
      ? curl_easy_setopt( curl, HB_CURLOPT_URL, REMOTE_URL_DEL )
#if 0
      /* May use this instead of embedding in URL */
      ? curl_easy_setopt( curl, HB_CURLOPT_USERPWD, "username:password" )
#endif
      ? curl_easy_setopt( curl, HB_CURLOPT_NOPROGRESS )
      ? curl_easy_setopt( curl, HB_CURLOPT_POSTQUOTE, { "DELE " + RENAME_FILE_TO } )
      ? curl_easy_setopt( curl, HB_CURLOPT_VERBOSE, lVerbose )

      ? "DELETE FILE:", curl_easy_perform( curl )

      curl_easy_reset( curl )

      WAIT

      /* Upload file from memory */

      tmp := "This will be the content of the file"

      ? curl_easy_setopt( curl, HB_CURLOPT_UPLOAD )
      ? curl_easy_setopt( curl, HB_CURLOPT_URL, REMOTE_URL_MEM )
      ? curl_easy_setopt( curl, HB_CURLOPT_UL_BUFF_SETUP, tmp )
      ? curl_easy_setopt( curl, HB_CURLOPT_INFILESIZE, Len( tmp ) ), Len( tmp )
#if 0
      /* May use this instead of embedding in URL */
      ? curl_easy_setopt( curl, HB_CURLOPT_USERPWD, "username:password" )
#endif
      ? curl_easy_setopt( curl, HB_CURLOPT_XFERINFOBLOCK, {| nPos, nLen | hb_DispOutAt( 10, 10, Str( ( nPos / nLen ) * 100, 6, 2 ) + "%" ) } )
      ? curl_easy_setopt( curl, HB_CURLOPT_NOPROGRESS, 0 )
      ? curl_easy_setopt( curl, HB_CURLOPT_VERBOSE, lVerbose )

      ? "UPLOAD FILE FROM MEMORY:", curl_easy_perform( curl )

      ? curl_easy_getinfo( curl, HB_CURLINFO_EFFECTIVE_URL )
      ? curl_easy_getinfo( curl, HB_CURLINFO_TOTAL_TIME )

      curl_easy_reset( curl )

      WAIT

      hb_default( @cDL, "ftp://ftp.mozilla.org/README" )

      /* Now let's download to a file */

      ? curl_easy_setopt( curl, HB_CURLOPT_DOWNLOAD )
      ? curl_easy_setopt( curl, HB_CURLOPT_URL, cDL )
      ? curl_easy_setopt( curl, HB_CURLOPT_DL_FILE_SETUP, "test_dl.bin" )
      ? curl_easy_setopt( curl, HB_CURLOPT_XFERINFOBLOCK, {| nPos, nLen | hb_DispOutAt( 11, 10, Str( ( nPos / nLen ) * 100, 6, 2 ) + "%" ) } )
      ? curl_easy_setopt( curl, HB_CURLOPT_NOPROGRESS, 0 )
      ? curl_easy_setopt( curl, HB_CURLOPT_VERBOSE, lVerbose )

      ? "DOWNLOAD FILE:", curl_easy_perform( curl )

      curl_easy_reset( curl )

      WAIT

      /* Now let's download to memory */

      ? curl_easy_setopt( curl, HB_CURLOPT_DOWNLOAD )
      ? curl_easy_setopt( curl, HB_CURLOPT_URL, cDL )
      ? curl_easy_setopt( curl, HB_CURLOPT_DL_BUFF_SETUP )
      ? curl_easy_setopt( curl, HB_CURLOPT_XFERINFOBLOCK, {| nPos, nLen | hb_DispOutAt( 11, 10, Str( ( nPos / nLen ) * 100, 6, 2 ) + "%" ) } )
      ? curl_easy_setopt( curl, HB_CURLOPT_NOPROGRESS, 0 )
      ? curl_easy_setopt( curl, HB_CURLOPT_VERBOSE, lVerbose )

      ? "DOWNLOAD FILE TO MEM:", curl_easy_perform( curl )

      tmp := "test_dlm.bin"
      ? "WRITING TO FILE:", tmp

      hb_MemoWrit( tmp, curl_easy_dl_buff_get( curl ) )

      curl_easy_reset( curl )

      WAIT

      hb_default( @cDL, "ftp://ftp.mozilla.org/" )

      /* Now let's download a dirlist to memory */

      ? curl_easy_setopt( curl, HB_CURLOPT_DOWNLOAD )
      ? curl_easy_setopt( curl, HB_CURLOPT_DIRLISTONLY )
      ? curl_easy_setopt( curl, HB_CURLOPT_URL, cDL )
      ? curl_easy_setopt( curl, HB_CURLOPT_DL_BUFF_SETUP )
      ? curl_easy_setopt( curl, HB_CURLOPT_XFERINFOBLOCK, {| nPos, nLen | hb_DispOutAt( 11, 10, Str( ( nPos / nLen ) * 100, 6, 2 ) + "%" ) } )
      ? curl_easy_setopt( curl, HB_CURLOPT_NOPROGRESS, 0 )
      ? curl_easy_setopt( curl, HB_CURLOPT_VERBOSE, lVerbose )

      ? "DOWNLOAD DIRLIST TO STRING:", curl_easy_perform( curl )

      ? "RESULT 1:", curl_easy_dl_buff_get( curl )
      ? curl_easy_setopt( curl, HB_CURLOPT_DL_BUFF_GET, @tmp )
      ? "RESULT 2:", tmp

      /* Cleanup session */

      curl_easy_cleanup( curl )

   ENDIF

   curl_global_cleanup()

   RETURN
