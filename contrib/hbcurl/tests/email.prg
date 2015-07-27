/* Copyright 2014 Viktor Szakats (vszakats.net/harbour) */

#require "hbcurl"
#require "hbtip"

#include "simpleio.ch"

#define _CA_FN_  "cacert.pem"

PROCEDURE Main( cFrom, cPassword, cTo, cHost )

   LOCAL curl
   LOCAL cMessage

   /* Require STARTTLS on port 587 (true) or allow it to proceed without it (false) */
   LOCAL lSTARTTLS_force

   IF hb_AScan( curl_version_info()[ HB_CURLVERINFO_PROTOCOLS ], "smtps",,, .T. ) == 0
      ? "Error: Requires libcurl 7.20.0 or newer, built with SSL and smtp protocol support"
      RETURN
   ENDIF

   hb_default( @cFrom    , "from@example.net" )
   hb_default( @cPassword, "password" )
   hb_default( @cTo      , "to@example.com" )
   hb_default( @cHost    , "localhost" )

   cHost := Lower( cHost )

   lSTARTTLS_force := .F.

   /* NOTE: Consult your provider for updated settings
            and make a pull request if necessary. */

   DO CASE
   CASE cHost == "apple" .OR. "@icloud.com" $ cFrom .OR. "@mac.com" $ cFrom .OR. "@me.com" $ cFrom
      cHost := "smtp://smtp.mail.me.com:587"; lSTARTTLS_force := .T.
   CASE cHost == "fastmail" .OR. "@fastmail.com" $ cFrom .OR. "@fastmail.fm" $ cFrom
      cHost := "smtps://mail.messagingengine.com"
   CASE cHost == "gmx.net" .OR. "@gmx.net" $ cFrom .OR. "@gmx.ch" $ cFrom .OR. "@gmx.de" $ cFrom
      cHost := "smtp://mail.gmx.net:587"; lSTARTTLS_force := .T.
   CASE cHost == "google" .OR. "@gmail.com" $ cFrom .OR. "@googlemail.com" $ cFrom
      cHost := "smtps://smtp.gmail.com"
   CASE cHost == "mail.ru" .OR. "@mail.ru" $ cFrom
      cHost := "smtps://smtp.mail.ru"
   CASE cHost == "netease" .OR. "@163.com" $ cFrom
      cHost := "smtps://smtp.163.com"
   CASE cHost == "office365"
      cHost := "smtp://smtp.office365.com:587"; lSTARTTLS_force := .T.
   CASE cHost == "outlook" .OR. "@outlook.com" $ cFrom .OR. "@hotmail.com" $ cFrom
      cHost := "smtp://smtp-mail.outlook.com:587"; lSTARTTLS_force := .T.
   CASE cHost == "sina" .OR. "@sina.com" $ cFrom
      cHost := "smtps://smtp.vip.sina.com"
   CASE cHost == "uol" .OR. "@uol.com.br" $ cFrom
      cHost := "smtps://smtps.uol.com.br"
   CASE cHost == "yahoo" .OR. "@yahoo.com" $ cFrom
      cHost := "smtps://smtp.mail.yahoo.com"
   ENDCASE

   ? "Host:", cHost, iif( lSTARTTLS_force, "(must STARTTLS)", "" )

   cMessage := tip_MailAssemble( cFrom, ;
      { cTo }, ;
      /* aCC */, ;
      "test: body", ;
      "test: subject", ;
      /* attachment */, ;
      /* nPriority */, ;
      /* lRead */, ;
      /* cReplyTo */, ;
      /* cCharset */, ;
      /* cEncoding */, ;
      .F. /* lBodyHTML */, ;
      /* bSMIME */ )

   curl_global_init()

   IF ! Empty( curl := curl_easy_init() )
      #if ! defined( __PLATFORM__UNIX )
         IF ! hb_vfExists( _CA_FN_ )
            ? "Downloading", _CA_FN_
            curl_easy_setopt( curl, HB_CURLOPT_DOWNLOAD )
            curl_easy_setopt( curl, HB_CURLOPT_URL, "http://curl.haxx.se/ca/cacert.pem" )
            curl_easy_setopt( curl, HB_CURLOPT_DL_FILE_SETUP, _CA_FN_ )
            curl_easy_perform( curl )
            curl_easy_reset( curl )
         ENDIF
         curl_easy_setopt( curl, HB_CURLOPT_CAINFO, _CA_FN_ )
      #endif
      curl_easy_setopt( curl, HB_CURLOPT_USE_SSL, ;
         iif( lSTARTTLS_force, HB_CURLUSESSL_ALL, HB_CURLUSESSL_TRY ) )
      curl_easy_setopt( curl, HB_CURLOPT_UPLOAD )
      curl_easy_setopt( curl, HB_CURLOPT_URL, cHost )
      curl_easy_setopt( curl, HB_CURLOPT_PROTOCOLS, hb_bitOr( HB_CURLPROTO_SMTPS, HB_CURLPROTO_SMTP ) )
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
