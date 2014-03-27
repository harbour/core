/*
 * Copyright 2009 Viktor Szakats (vszakats.net/harbour)
 * www - http://harbour-project.org
 */

#require "hbssl"
#require "hbtip"

REQUEST __HBEXTERN__HBSSL__

#include "simpleio.ch"

PROCEDURE Main( cFrom, cPassword, cTo )

   IF ! tip_SSL()
      ? "Error: Requires SSL support"
      RETURN
   ENDIF

   hb_default( @cFrom    , "<from@gmail.com>" )
   hb_default( @cPassword, "password" )
   hb_default( @cTo      , "to@example.com" )

   ? tip_MailSend( ;
      "smtp.gmail.com", ;
      465, ;
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
      .T. )

   RETURN
