/* TIP Mail - reading and writing multipart mails
 *
 * Creating a mail message.
 * This will create a valid mail message, using
 * the set of files given in the command-line.
 */

#require "hbtip"

PROCEDURE Main( ... )

   LOCAL oMail, cData, i, oAttach
   LOCAL cFname

   IF PCount() == 0
      Usage()
      RETURN
   ENDIF

   oMail := TIPMail():New( "This is the body of the mail" )
   oMail:hHeaders[ "Content-Type" ] := "text/plain; charset=utf-8"
   oMail:hHeaders[ "Date" ] := tip_TimeStamp()

   FOR i := 1 TO PCount()

      SWITCH Lower( cData := hb_PValue( i ) )
      CASE "-h"
         Usage()
         RETURN
      CASE "-f"
         IF HB_ISSTRING( cData := hb_PValue( ++i ) )
            oMail:hHeaders[ "From" ] := hb_StrToUTF8( cData )
         ENDIF
         EXIT
      CASE "-t"
         IF HB_ISSTRING( cData := hb_PValue( ++i ) )
            oMail:hHeaders[ "To" ] := hb_StrToUTF8( cData )
         ENDIF
         EXIT
      CASE "-s"
         IF HB_ISSTRING( cData := hb_PValue( ++i ) )
            oMail:hHeaders[ "Subject" ] := hb_StrToUTF8( cData )
         ENDIF
         EXIT
      CASE "-b"
         IF HB_ISSTRING( cData := hb_PValue( ++i ) )
            oMail:SetBody( hb_StrToUTF8( cData ) + e"\r\n" )
         ENDIF
         EXIT
      CASE "-m"
         IF HB_ISSTRING( cData := hb_PValue( ++i ) )
            IF ( cData := hb_MemoRead( cData ) ) == ""
               ? "Fatal: Could not read", hb_PValue( i )
               RETURN
            ENDIF
            oMail:SetBody( cData + e"\r\n" )
         ENDIF
         EXIT
      OTHERWISE  // it is an attachment file
         IF ( cData := hb_MemoRead( cData ) ) == ""
            ? "Fatal: Could not read attachment or attachment empty", hb_PValue( i )
            RETURN
         ENDIF

         oAttach := TIPMail():New()
         oAttach:SetEncoder( "base64" )
         // TODO: mime type magic auto-finder
         cFName := hb_FNameNameExt( hb_PValue( i ) )
         // Some EMAIL readers use Content-Type to check for filename
         oAttach:hHeaders[ "Content-Type" ] := ;
            "application/X-TIP-Attachment; filename=" + cFName
         // But usually, original filename is set here
         oAttach:hHeaders[ "Content-Disposition" ] := ;
            "attachment; filename=" + cFname
         oAttach:SetBody( cData )

         oMail:Attach( oAttach )
      ENDSWITCH
   NEXT

   /* Writing stream */
   OutStd( oMail:ToString() )

   RETURN

STATIC PROCEDURE Usage()

   ? "Usage:"
   ? "   tipmmail [options] attachment1, attachment2..."
   ? "Options:"
   ? "   -h              Help"
   ? '   -f "from"       Set "mail from" field'
   ? '   -t "to"         Set "mail to" field'
   ? '   -s "subject"    Set mail subject'
   ? '   -b "body"       Set mail body (or message)'
   ? '   -m "bodyfile"   Set mail body using a file'
   ?

   RETURN
