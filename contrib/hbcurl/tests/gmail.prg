/* Copyright 2014 Viktor Szakats (vszakats.net/harbour) */

#require "hbcurl"
#require "hbtip"

#include "simpleio.ch"

#define _CA_FN_  "cacert.pem"

PROCEDURE Main( cFrom, cPassword, cTo )

   LOCAL curl
   LOCAL cMessage

   IF hb_AScan( curl_version_info()[ 8 ], "smtps",,, .T. ) == 0
      ? "Error: Requires libcurl 7.20.0 or newer, built with SSL and smtp protocol support"
      RETURN
   ENDIF

   hb_default( @cFrom    , "from@gmail.com" )
   hb_default( @cPassword, "password" )
   hb_default( @cTo      , "to@example.com" )

   cMessage := tip_MailAssemble( cFrom, ;
      { cTo }, ;
      NIL /* aCC */, ;
      "test: body", ;
      "test: subject", ;
      NIL /* attachment */, ;
      NIL /* nPriority */, ;
      NIL /* lRead */, ;
      NIL /* cReplyTo */, ;
      NIL /* cCharset */, ;
      NIL /* cEncoding */, ;
      .F. /* lBodyHTML */, ;
      NIL /* bSMIME */ )

   curl_global_init()

   IF ! Empty( curl := curl_easy_init() )
      #if ! defined( __PLATFORM__UNIX )
         IF ! hb_FileExists( _CA_FN_ )
            ? "Downloading", _CA_FN_
            curl_easy_setopt( curl, HB_CURLOPT_DOWNLOAD )
            curl_easy_setopt( curl, HB_CURLOPT_URL, "http://curl.haxx.se/ca/cacert.pem" )
            curl_easy_setopt( curl, HB_CURLOPT_DL_FILE_SETUP, _CA_FN_ )
            curl_easy_perform( curl )
            curl_easy_reset( curl )
         ENDIF
         curl_easy_setopt( curl, HB_CURLOPT_CAINFO, _CA_FN_ )
      #endif
      curl_easy_setopt( curl, HB_CURLOPT_UPLOAD )
      curl_easy_setopt( curl, HB_CURLOPT_URL, "smtps://smtp.gmail.com:465" )
      curl_easy_setopt( curl, HB_CURLOPT_USERNAME, cFrom )
      curl_easy_setopt( curl, HB_CURLOPT_PASSWORD, cPassword )
      curl_easy_setopt( curl, HB_CURLOPT_UL_BUFF_SETUP, cMessage )
      curl_easy_setopt( curl, HB_CURLOPT_INFILESIZE_LARGE, hb_BLen( cMessage ) )
      curl_easy_setopt( curl, HB_CURLOPT_MAIL_FROM, cFrom )
      curl_easy_setopt( curl, HB_CURLOPT_MAIL_RCPT, { cTo } )
      curl_easy_setopt( curl, HB_CURLOPT_VERBOSE, .T. )

      ? "Result:", curl_easy_perform( curl )

      curl_easy_cleanup( curl )
   ELSE
      ? "Failed to init"
   ENDIF

   RETURN
