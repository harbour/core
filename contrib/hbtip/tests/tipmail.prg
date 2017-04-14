/* TIP Mail - reading and writing multipart mails
 *
 * Test for reading a multipart message (that must already
 * be in its canonical form, that is, line terminator is
 * CRLF and it must have no headers other than SMTP/MIME).
 */

#require "hbtip"

PROCEDURE Main( cFileName )

   LOCAL oMail, cData, i

   IF ! HB_ISSTRING( cFileName ) .OR. ;
      ( cData := hb_MemoRead( cFileName ) ) == ""

      ? "Cannot open", cFileName
      RETURN
   ENDIF

   oMail := TIPMail():New()
   IF oMail:FromString( cData ) == 0
      ? "Malformed mail. Dumping up to where parsed"
   ENDIF

   ? PadC( " HEADERS ", 60, "-" )
   FOR EACH i IN oMail:hHeaders
      ? i:__enumKey(), ":", i
   NEXT
   ?

   ? PadC( " RECEIVED ", 60, "-" )
   FOR EACH cData IN oMail:aReceived
      ? cData
   NEXT
   ?

   ? PadC( " BODY ", 60, "-" )
   ? oMail:GetBody()
   ?

   DO WHILE oMail:GetAttachment() != NIL
      ? PadC( " ATTACHMENT ", 60, "-" )
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
