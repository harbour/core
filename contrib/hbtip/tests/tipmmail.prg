/*
 * TIP Mail - reading and writing multipart mails
 *
 * Creating a mail message.
 * This will create a valid mail message, using
 * the set of files given in the command line.
 */

#require "hbtip"

PROCEDURE Main( ... )

   LOCAL oMail, cData, i, oAttach
   LOCAL cFname, cFExt

   IF PCount() == 0
      Usage()
      RETURN
   ENDIF

   oMail := TIPMail( "This is the body of the mail" )
   oMail:hHeaders[ "Content-Type" ] := "text/plain; charset=utf-8"
   oMail:hHeaders[ "Date" ] := tip_TimeStamp()

   FOR i := 1 TO PCount()

      cData := hb_PValue( i )

      IF Lower( cData ) == "-h"
         Usage()
         RETURN
      ENDIF

      IF Lower( cData ) == "-f"
         IF HB_ISSTRING( cData := hb_PValue( ++i ) )
            oMail:hHeaders[ "From" ] := hb_StrToUTF8( cData )
         ENDIF
      ELSEIF Lower( cData ) == "-t"
         IF HB_ISSTRING( cData := hb_PValue( ++i ) )
            oMail:hHeaders[ "To" ] := hb_StrToUTF8( cData )
         ENDIF
      ELSEIF Lower( cData ) == "-s"
         IF HB_ISSTRING( cData := hb_PValue( ++i ) )
            oMail:hHeaders[ "Subject" ] := hb_StrToUTF8( cData )
         ENDIF
      ELSEIF Lower( cData ) == "-b"
         IF HB_ISSTRING( cData := hb_PValue( ++i ) )
            oMail:SetBody( hb_StrToUTF8( cData ) + e"\r\n" )
         ENDIF
      ELSEIF Lower( cData ) == "-m"
         IF HB_ISSTRING( cData := hb_PValue( ++i ) )
            cData := MemoRead( cData )
            IF Empty( cData )
               ? "Fatal: Can't read", hb_PValue( i )
               RETURN
            ENDIF
            oMail:SetBody( cData + e"\r\n" )
         ENDIF
      ELSE  // it is an attachment file
         cData := MemoRead( cData )
         IF Empty( cData )
            ? "Fatal: Can't read attachment", hb_PValue( i )
            RETURN
         ENDIF
         oAttach := TIPMail():New()

         oAttach:SetEncoder( "base64" )
         // TODO: mime type magic auto-finder
         hb_FNameSplit( hb_PValue( i ), , @cFname, @cFext )
         // Some EMAIL readers use Content-Type to check for filename
         oAttach:hHeaders[ "Content-Type" ] := ;
            "application/X-TIP-Attachment; filename=";
            + cFname + cFext
         // But usually, original filename is set here
         oAttach:hHeaders[ "Content-Disposition" ] := ;
            "attachment; filename=" + cFname + cFext
         oAttach:SetBody( cData )

         oMail:Attach( oAttach )
      ENDIF
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
