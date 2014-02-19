/*
 * TIP Mail - reading and writing multipart mails
 *
 * Test for reading a multipart message (that must already
 * be in its canonical form, that is, line terminator is
 * CRLF and it must have no headers other than SMTP/MIME).
 */

#require "hbtip"

PROCEDURE Main( cFileName )

   LOCAL oMail, cData, i

   IF ! HB_ISSTRING( cFileName )
      cData := MemoRead( cFileName )
      IF FError() > 0
         ? "Can't open", cFileName
         RETURN
      ENDIF
   ENDIF
   oMail := TIPMail():New()
   IF oMail:FromString( cData ) == 0
      ? "Malformed mail. Dumping up to where parsed"
   ENDIF

   ? "-------------============== HEADERS =================--------------"
   FOR EACH i IN oMail:hHeaders
      ? i:__enumKey(), ":", i
   NEXT
   ?

   ? "-------------============== RECEIVED =================--------------"
   FOR EACH cData IN oMail:aReceived
      ? cData
   NEXT
   ?

   ? "-------------============== BODY =================--------------"
   ? oMail:GetBody()
   ?

   DO WHILE oMail:GetAttachment() != NIL
      ? "-------------============== ATTACHMENT =================--------------"
      ? oMail:NextAttachment():GetBody()
      ?
   ENDDO

   ? "DONE"
   ?
   /* Writing stream */
#if 0
   OutStd( oMail:ToString() )
#endif

   RETURN
