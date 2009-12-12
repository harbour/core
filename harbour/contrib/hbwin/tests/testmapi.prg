/*
 * $Id$
 */

PROCEDURE Main()

   cSubject := "Test subject"
   cBody := "Test body"
   lMailConf := .F.
   lFromUser := .T.
   aSender := { "test from", "from@test.com" }
   aDest := { { "test to", "to@test.com" } }
   aFiles := { { "testmapi.prg", "" } }

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
