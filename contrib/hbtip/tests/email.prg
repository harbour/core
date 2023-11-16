/* Copyright 2009 Viktor Szakats (vszakats.net/harbour) */

#require "hbssl"
#require "hbtip"

#if ! defined( __HBSCRIPT__HBSHELL )
REQUEST __HBEXTERN__HBSSL__
#endif

#include "simpleio.ch"

PROCEDURE Main( cFrom, cPassword, cTo, cHost )

   LOCAL nPort
   LOCAL lSTARTTLS := .F.

   IF ! tip_SSL()
      ? "Error: Requires SSL support"
      RETURN
   ENDIF

   hb_default( @cFrom    , "<from@example.net>" )
   hb_default( @cPassword, "password" )
   hb_default( @cTo      , "to@example.com" )
   hb_default( @cHost    , "localhost" )

   cHost := Lower( cHost )

   /* SMTPS works with SSL/TLS on port 465 and with STARTTLS
      on port 587. STARTTLS mode is fully automatic and requires
      SSL/TLS be disabled at first (it will be activated on
      request after STARTTLS command) */

   DO CASE
   CASE cHost == "apple" .OR. "@icloud.com" $ cFrom .OR. "@mac.com" $ cFrom .OR. "@me.com" $ cFrom
      cHost := "smtp.mail.me.com"; lSTARTTLS := .T.
   CASE cHost == "fastmail" .OR. "@fastmail.com" $ cFrom .OR. "@fastmail.fm" $ cFrom
      cHost := "smtp.fastmail.com"
   CASE cHost == "gmx.net" .OR. "@gmx.net" $ cFrom .OR. "@gmx.ch" $ cFrom .OR. "@gmx.de" $ cFrom
      cHost := "mail.gmx.net"; lSTARTTLS := .T.
   CASE cHost == "google" .OR. "@gmail.com" $ cFrom .OR. "@googlemail.com" $ cFrom
      cHost := "smtp.gmail.com"
   CASE cHost == "mail.ru" .OR. "@mail.ru" $ cFrom
      cHost := "smtp.mail.ru"
   CASE cHost == "netease" .OR. "@163.com" $ cFrom
      cHost := "smtp.163.com"
   CASE cHost == "office365"
      cHost := "smtp.office365.com"; lSTARTTLS := .T.
   CASE cHost == "outlook" .OR. "@outlook.com" $ cFrom .OR. "@hotmail.com" $ cFrom
      cHost := "smtp-mail.outlook.com"; lSTARTTLS := .T.
   CASE cHost == "sina" .OR. "@sina.com" $ cFrom
      cHost := "smtp.vip.sina.com"
   CASE cHost == "uol" .OR. "@uol.com.br" $ cFrom
      cHost := "smtps.uol.com.br"
   CASE cHost == "yahoo" .OR. "@yahoo.com" $ cFrom
      cHost := "smtp.mail.yahoo.com"
   CASE ":" $ cHost
      IF TUrl():New( cHost ):nPort != -1
         nPort := TUrl():New( cHost ):nPort
      ENDIF
      lSTARTTLS := ( nPort == 587 )
      cHost := TUrl():New( cHost ):cServer
   ENDCASE

   hb_default( @nPort, iif( lSTARTTLS, 587, 465 ) )

   ? "Host:", cHost, hb_ntos( nPort ), iif( lSTARTTLS, "(STARTTLS)", "" )

   ? tip_MailSend( ;
      cHost, ;
      nPort, ;
      cFrom, ;
      cTo, ;
      NIL /* CC */, ;
      {} /* BCC */, ;
      "test: body", ;
      "test: subject", ;
      NIL /* attachment */, ;
      cFrom, ;
      cPassword, ;
      "", ;
      NIL /* nPriority */, ;
      NIL /* lRead */, ;
      .T. /* lTrace */, ;
      .F., ;
      NIL /* lNoAuth */, ;
      NIL /* nTimeOut */, ;
      NIL /* cReplyTo */, ;
      nPort == 465 )

   RETURN
