/*
 * Copyright 2009 Viktor Szakats (vszakats.net/harbour)
 *
 * Gmail work with ssl on port 465 and with tls on port 587
 * tls mode is fully automatic and require that ssl must be disabled at first (We will activate it on request after STARTTLS command)
 */

#require "hbssl"
#require "hbtip"

REQUEST __HBEXTERN__HBSSL__

#include "simpleio.ch"

PROCEDURE Main( cFrom, cPassword, cTo, cPort)

   IF ! tip_SSL()
      ? "Error: Requires SSL support"
      RETURN
   ENDIF

   hb_default( @cFrom    , "<myname@gmail.com>" )
   hb_default( @cPassword, "<mypassword>" )
   hb_default( @cTo      , "addressee@domain.com" )
   hb_default( @cPort    , "465" )

   ? hb_SendMail( ;
      "smtp.gmail.com", ;
      Val(cPort), ;
      cFrom, ;
      cTo, ;
      NIL /* CC */, ;
      {} /* BCC */, ;
      "test: body", ;
      "test: port "+cPort, ;
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
      iif(cPort=="465",.T.,.F.) /* lSSL */  )

   RETURN
   
