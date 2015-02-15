/* Copyright 2009 Viktor Szakats (vszakats.net/harbour) */

#require "hbssl"
#require "hbtip"

#if ! defined( __HBSCRIPT__HBSHELL )
REQUEST __HBEXTERN__HBSSL__
#endif

#include "simpleio.ch"

PROCEDURE Main( cFrom, cPassword, cTo, cProvider )

   LOCAL cHost
   LOCAL lSTARTTLS := .F.

   IF ! tip_SSL()
      ? "Error: Requires SSL support"
      RETURN
   ENDIF

   hb_default( @cFrom    , "<from@example.net>" )
   hb_default( @cPassword, "password" )
   hb_default( @cTo      , "to@example.com" )
   hb_default( @cProvider, "" )

   cProvider := Lower( cProvider )

   DO CASE
   CASE cProvider == "apple" .OR. "@icloud.com" $ cFrom .OR. "@mac.com" $ cFrom .OR. "@me.com" $ cFrom
      cHost := "smtp.mail.me.com"; lSTARTTLS := .T.
   CASE cProvider == "fastmail" .OR. "@fastmail.com" $ cFrom .OR. "@fastmail.fm" $ cFrom
      cHost := "mail.messagingengine.com"
   CASE cProvider == "gmx.net" .OR. "@gmx.net" $ cFrom .OR. "@gmx.ch" $ cFrom .OR. "@gmx.de" $ cFrom
      cHost := "mail.gmx.net"; lSTARTTLS := .T.
   CASE cProvider == "google" .OR. "@gmail.com" $ cFrom .OR. "@googlemail.com" $ cFrom
      cHost := "smtp.gmail.com"
   CASE cProvider == "mail.ru" .OR. "@mail.ru" $ cFrom
      cHost := "smtp.mail.ru"
   CASE cProvider == "netease" .OR. "@163.com" $ cFrom
      cHost := "smtp.163.com"
   CASE cProvider == "office365"
      cHost := "smtp.office365.com"; lSTARTTLS := .T.
   CASE cProvider == "outlook" .OR. "@outlook.com" $ cFrom .OR. "@hotmail.com" $ cFrom
      cHost := "smtp-mail.outlook.com"; lSTARTTLS := .T.
   CASE cProvider == "sina" .OR. "@sina.com" $ cFrom
      cHost := "smtp.vip.sina.com"
   CASE cProvider == "uol" .OR. "@uol.com.br" $ cFrom
      cHost := "smtps.uol.com.br"
   CASE cProvider == "yahoo" .OR. "@yahoo.com" $ cFrom
      cHost := "smtp.mail.yahoo.com"
   OTHERWISE
      ? "Error: Unknown provider"
      RETURN
   ENDCASE

   ? tip_MailSend( ;
      cHost, ;
      iif( lSTARTTLS, 587, 465 ), ;
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
      ! lSTARTTLS )

   RETURN
