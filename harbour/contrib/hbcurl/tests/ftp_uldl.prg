/*
 * $Id$
 */

/* Redirect STDERR to a file to see the verbose output. */

#include "hbcurl.ch"

#define LOCAL_FILE          "ftp_uldl.prg"
#define UPLOAD_FILE_AS      "ftp_upped.prg"
#define REMOTE_URL          "ftp://harbour:power@localhost/" + UPLOAD_FILE_AS
#define RENAME_FILE_TO      "ftp_upped_renamed.prg"

FUNCTION Main()
   LOCAL curl

   ? "INIT:", curl_global_init()

   IF ! Empty( curl := curl_easy_init() )

      ? curl_easy_setopt( curl, HB_CURLOPT_UPLOAD )
      ? curl_easy_setopt( curl, HB_CURLOPT_URL, REMOTE_URL )
      ? curl_easy_setopt( curl, HB_CURLOPT_SETUPLOADFILE, LOCAL_FILE )
      ? curl_easy_setopt( curl, HB_CURLOPT_VERBOSE, .T. )
      ? curl_easy_setopt( curl, HB_CURLOPT_USERPWD, "harbour:power" ) /* May use this instead of embedding in URL */
      ? curl_easy_setopt( curl, HB_CURLOPT_SETPROGRESS, {| nPos, nLen | DispOutAt( 10, 10, Str( ( nPos / nLen ) * 100, 6, 2 ) + "%" ) } )
      ? curl_easy_setopt( curl, HB_CURLOPT_NOPROGRESS, .F. )
      ? curl_easy_setopt( curl, HB_CURLOPT_POSTQUOTE, { "RNFR " + UPLOAD_FILE_AS, "RNTO " + RENAME_FILE_TO } )

      ? "UPLOAD:", curl_easy_perform( curl )

      curl_easy_cleanup( curl )
   ENDIF

   ? "-------------------------"

   IF ! Empty( curl := curl_easy_init() )

      /* Now let's download something */

      ? curl_easy_setopt( curl, HB_CURLOPT_DOWNLOAD )
      ? curl_easy_setopt( curl, HB_CURLOPT_URL, "ftp://ftp.cisco.com/README" )
      ? curl_easy_setopt( curl, HB_CURLOPT_SETDOWNLOADFILE, "test_dl.bin" )
      ? curl_easy_setopt( curl, HB_CURLOPT_VERBOSE, .T. )

      ? "DOWNLOAD:", curl_easy_perform( curl )

      /* Cleanup session */

      curl_easy_cleanup( curl )
   ENDIF

   curl_global_cleanup()

   RETURN NIL
