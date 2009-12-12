/*
 * $Id$
 */

PROCEDURE Main( cSubject, cBody, lMailConf, lFromUser, aSender, aDest, aFiles )

   ? win_MAPISendMail( cSubject,                        ; // subject
                       cBody,                           ; // menssage
                       NIL,                             ; // type of message
                       DToS( Date() ) + " " + Time(),   ; // send date
                       "",                              ; // conversation ID
                       lMailConf,                       ; // acknowledgment
                       lFromUser,                       ; // user intervention
                       aSender,                         ; // sender
                       aDest,                           ; // destinators
                       aFiles                           ; // attach
                     )

   RETURN
