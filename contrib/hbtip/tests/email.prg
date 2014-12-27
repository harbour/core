/* Copyright 2009 Viktor Szakats (vszakats.net/harbour) */

#require "hbssl"
#require "hbtip"

#if ! defined( __HBSCRIPT__HBSHELL )
REQUEST __HBEXTERN__HBSSL__
#endif

#include "simpleio.ch"

PROCEDURE Main( cFrom, cPassword, cTo, cProvider )

   LOCAL cSMTPHost
   LOCAL nSMTPPort
   LOCAL lTLS

   IF ! tip_SSL()
      ? "Error: Requires SSL support"
      RETURN
   ENDIF

   hb_default( @cFrom    , "<from@example.net>" )
   hb_default( @cPassword, "password" )
   hb_default( @cTo      , "to@example.com" )
   hb_default( @cProvider, "" )

   DO CASE
   CASE cProvider == "fastmail" .OR. "@fastmail.com" $ cFrom .OR. "@fastmail.fm" $ cFrom
      lTLS := .T.; nSMTPPort := 465; cSMTPHost := "mail.messagingengine.com"
   CASE cProvider == "google" .OR. "@gmail.com" $ cFrom .OR. "@googlemail.com" $ cFrom
      lTLS := .T.; nSMTPPort := 465; cSMTPHost := "smtp.gmail.com"
   CASE cProvider == "yahoo" .OR. "@yahoo.com" $ cFrom
      lTLS := .T.; nSMTPPort := 465; cSMTPHost := "smtp.mail.yahoo.com"
   OTHERWISE
      ? "Error: Unknown provider"
      RETURN
   ENDCASE

   ? tip_MailSend( ;
      cSMTPHost, ;
      nSMTPPort, ;
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
      lTLS )

   RETURN
