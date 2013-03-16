/******************************************
 * TIP test
 * Mail - reading and writing multipart mails
 *
 * Test for reading a multipart message (that must already
 * be in its canonical form, that is, line terminator is
 * CRLF and it must have no headers other than SMTP/Mime).
 *
 * This test writes data to standard output, and is
 * compiled only under GTCGI;
 ******************************************/

#require "hbtip"

PROCEDURE Main( cFileName )

   LOCAL oMail, cData, i

   IF cFileName != NIL
      cData := MemoRead( cFileName )
      IF FError() > 0
         ? "Can't open", cFileName
         QUIT
      ENDIF
   ENDIF
   oMail := TIPMail():New()
   IF oMail:FromString( cData ) == 0
      ? "Malformed mail. Dumping up to where parsed"
   ENDIF

   ? "-------------============== HEADERS =================--------------"
   FOR i := 1 TO Len( oMail:hHeaders )
      ? hb_HKeyAt( oMail:hHeaders, i ), ":", hb_HValueAt( oMail:hHeaders, i )
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
   /* FWrite( 1, oMail:ToString() ) */

   RETURN
