/*
 * $Id$
 */

/*
 * Copyright 2009 Viktor Szakats (harbour syenar.hu)
 * www - http://harbour-project.org
 */

#include "common.ch"
#include "simpleio.ch"

PROCEDURE Main( cFrom, cPassword, cTo )

   DEFAULT cFrom     TO "<myname@gmail.com>"
   DEFAULT cPassword TO "<mypassword>"
   DEFAULT cTo       TO "addressee@domain.com"

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
