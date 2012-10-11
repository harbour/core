/*
 * $Id$
 */

/*
 * Copyright 2009 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 */

#require "hbtip"

#include "simpleio.ch"

PROCEDURE Main( cFrom, cPassword, cTo )

   hb_default( @cFrom    , "<myname@gmail.com>" )
   hb_default( @cPassword, "<mypassword>" )
   hb_default( @cTo      , "addressee@domain.com" )

   ? hb_SendMail( "smtp.gmail.com",;
                  465,;
                  cFrom,;
                  cTo,;
                  NIL /* CC */,;
                  {} /* BCC */,;
                  "test: body",;
                  "test: subject",;
                  NIL /* attachment */,;
                  cFrom,;
                  cPassword,;
                  "",;
                  NIL /* nPriority */,;
                  NIL /* lRead */,;
                  .T. /* lTrace */,;
                  .F.,;
                  NIL /* lNoAuth */,;
                  NIL /* nTimeOut */,;
                  NIL /* cReplyTo */,;
                  .T. )

   RETURN
