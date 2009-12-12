/*
 * $Id$
 */

PROCEDURE Main()

   LOCAL cSubject := "Test subject"
   LOCAL cBody := "Test body"
   LOCAL lMailConf := .F.
   LOCAL lFromUser := .T.
   LOCAL aSender := { "test from", "from@test.com" }
   LOCAL aDest := { { "test to", "to@test.com" } }
   LOCAL aFiles := { { "testmapi.prg", "" } }

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
