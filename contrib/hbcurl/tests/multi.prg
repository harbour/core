#define REMOTE_1          "https://github.com/"
#define REMOTE_2          "https://github.com/harbour/core/"
#define REMOTE_3          "https://raw.githubusercontent.com/harbour/core/refs/heads/master/contrib/hbcurl/hbcurl.hbm"

#require "hbcurl"

#include "inkey.ch"

PROCEDURE Main( cRemote4 )
   LOCAL hMulti
   LOCAL hEasy, aResult, nKey := 0
   LOCAL aUrls := { REMOTE_1, REMOTE_2, REMOTE_3 }
   LOCAL aHandles, i, nErr, nRun := 0, nLastRun := 0

   /* add a remote from command-line if you want */

   IF ! Empty( cRemote4 )
      AAdd( aUrls, cRemote4 )
   ENDIF

   /* add the individual transfers */

   aHandles := Array( Len( aUrls ) )

   curl_global_init()

   hMulti := curl_multi_init()

   FOR i := 1 TO Len( aUrls )
      hEasy := curl_easy_init()
      IF hEasy != NIL
         curl_easy_setopt( hEasy, HB_CURLOPT_URL, aUrls[ i ] )
         curl_easy_setopt( hEasy, HB_CURLOPT_DOWNLOAD )
         curl_easy_setopt( hEasy, HB_CURLOPT_DL_BUFF_SETUP )
//       curl_easy_setopt( hEasy, HB_CURLOPT_VERBOSE, .T. )
         curl_easy_setopt( hEasy, HB_CURLOPT_SSL_OPTIONS, HB_CURLSSLOPT_NATIVE_CA )
         curl_multi_add_handle( hMulti, hEasy )
         aHandles[ i ] := hEasy
      ENDIF
   NEXT

   CLS

   nErr := curl_multi_perform( hMulti, @nLastRun )

   IF nErr != HB_CURLM_OK
      ? "curl_multi_perform() failed, err=", nErr
      Cleanup( aHandles, hMulti )
      RETURN
   ENDIF

   SetPos( 3, 0 )

   DO WHILE curl_multi_perform( hMulti, @nRun ) == HB_CURLM_OK

      aResult := CurGet()
      @ 1, 1 SAY Time()
      @ 2, 1 SAY "downloading... (" + hb_ntos( nRun ) + " transfers left)"
      CurSet( aResult )

      IF nRun < nLastRun
         FOR i := 1 TO nLastRun - nRun

            /*
             * passing possible aHandles is required to receive curl_easy_*
             * compatible GC-pointer in aResult[ HB_CURLMSG_RESP_HANDLE ]
             */

            aResult := curl_multi_info_read( hMulti, aHandles )

            IF aResult # NIL
               /*
                * instead of FOR loop you could possibly
                * DO WHILE HB_IsArray( aResult := curl_multi_info_read( hMulti ) ) .AND. ;
                *          aResult[ HB_CURLMSG_RESP_LEN ] > 0
                */
               ? "HB_CURLMSG_RESP_LEN          :", aResult[ HB_CURLMSG_RESP_LEN ]
               ? "HB_CURLMSG_RESP_RESPONSE_CODE:", aResult[ HB_CURLMSG_RESP_RESPONSE_CODE ]
               ? "HB_CURLMSG_RESP_MSG          :", aResult[ HB_CURLMSG_RESP_MSG ]
               ? "HB_CURLMSG_RESP_RESULT       :", aResult[ HB_CURLMSG_RESP_RESULT ]
               ? "HB_CURLMSG_RESP_HPOS         :", aResult[ HB_CURLMSG_RESP_HPOS ]
               IF aResult[ HB_CURLMSG_RESP_HPOS ] > 0
                  ? "URL                          :", aUrls[ aResult[ HB_CURLMSG_RESP_HPOS ] ]
                  IF ! Empty( curl_easy_getinfo( aResult[ HB_CURLMSG_RESP_HANDLE ], HB_CURLINFO_REDIRECT_URL ) )
                     ? "HB_CURLINFO_REDIRECT_URL     :", curl_easy_getinfo( aResult[ HB_CURLMSG_RESP_HANDLE ], HB_CURLINFO_REDIRECT_URL )
                  ENDIF
               ENDIF
               ? "--"
            ENDIF
         NEXT
      ENDIF

      IF nRun == 0 .OR. nKey == K_ESC
         EXIT
      ENDIF

      nLastRun := nRun

      nKey := Inkey( 0.1 )

   ENDDO

   PrintAllDownloaded( aHandles )

   Cleanup( aHandles, hMulti )

   RETURN

STATIC PROCEDURE PrintAllDownloaded( aHandles )
   LOCAL h

   FOR EACH h IN aHandles

      IF h == NIL
         LOOP
      ENDIF

      ?  "Transfer #" + hb_ntos( h:__enumIndex ) + " (" + hb_ntos( curl_easy_getinfo( h, HB_CURLINFO_RESPONSE_CODE ) ) + "): "
      IF curl_easy_getinfo( h, HB_CURLINFO_RESPONSE_CODE ) == 0
        ?? "aborted"
      ELSE
        ?? Left( curl_easy_dl_buff_get( h ), 40 ) + IIF( Len( curl_easy_dl_buff_get( h ) ) > 40, "...", "" )
      ENDIF
   NEXT

   RETURN


STATIC PROCEDURE Cleanup( aHandles, hMulti )
   LOCAL h

   FOR EACH h IN aHandles

      IF h == NIL
         LOOP
      ENDIF

      curl_multi_remove_handle( hMulti, h )
      curl_easy_cleanup( h )

   NEXT

   curl_multi_cleanup( hMulti )

   curl_global_cleanup()

   RETURN

STATIC FUNCTION CurGet()
   RETURN { Row(), Col() }

STATIC PROCEDURE CurSet( a )

   SetPos( a[ 1 ], a[ 2 ] )

   RETURN
