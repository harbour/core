/*
 * $Id$
 */

/******************************************
 * TIP test
 * Mail - reading and writing multipart mails
 *
 * Creating a mail message.
 * This will create a valid mail message, using
 * the set of files given in the command line.
 *
 * Usage:
 * tipmmail [options] attachment1, attachment2...
 *  options:
 *    -h              Help
 *    -f "from"       Set "mail from" field
 *    -t "to"         Set "mail to" field
 *    -s "subject"    Set mail subject
 *    -b "body"       Set mail body (or message)
 *    -m "bodyfile"   Set mail body using a file
 *
 *
 * This test writes data to standard output, and is
 * compiled only under GTCGI;
 ******************************************/

#require "hbtip"

PROCEDURE Main( ... )

   LOCAL oMail, cData, i, oAttach
   LOCAL cFname, cFExt

   IF PCount() == 0
      Usage()
      QUIT
   ENDIF

   oMail := TipMail( "This is the body of the mail" )
   oMail:hHeaders[ "Content-Type" ] := "text/plain; charset=iso8851"
   oMail:hHeaders[ "Date" ]  := Tip_Timestamp()

   i := 1
   DO WHILE i < PCount()
      cData := hb_PValue( i )

      IF Lower( cData ) == "-h"
         Usage()
         QUIT
      ENDIF

      IF Lower( cData ) == "-f"
         i ++
         cData := hb_PValue( i )
         IF cData != NIL
            oMail:hHeaders[ "From" ] := cData
         ENDIF
      ELSEIF Lower( cData ) == "-t"
         i ++
         cData := hb_PValue( i )
         IF cData != NIL
            oMail:hHeaders[ "To" ] := cData
         ENDIF
      ELSEIF Lower( cData ) == "-s"
         i ++
         cData := hb_PValue( i )
         IF cData != NIL
            oMail:hHeaders[ "Subject" ] := cData
         ENDIF
      ELSEIF Lower( cData ) == "-b"
         i ++
         cData := hb_PValue( i )
         IF cData != NIL
            oMail:SetBody( cData + e"\r\n" )
         ENDIF
      ELSEIF Lower( cData ) == "-m"
         i ++
         cData := hb_PValue( i )
         IF cData != NIL
            cData := MemoRead( cData )
            IF Empty( cData )
               ? "FATAL: Can't read", hb_PValue( i )
               QUIT
            ENDIF
            oMail:SetBody( cData + e"\r\n" )
         ENDIF
      ELSE  // it is an attachment file
         cData := MemoRead( cData )
         IF Empty( cData )
            ? "FATAL: Can't read attachment", hb_PValue( i )
            QUIT
         ENDIF
         oAttach := TipMail():New()

         oAttach:SetEncoder( "base64" )
         //TODO: mime type magic auto-finder
         HB_FNameSplit( hb_PValue( i ), , @cFname, @cFext )
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

      i ++
   ENDDO

   /* Writing stream */
   FWrite( 1, oMail:ToString() )

   RETURN

PROCEDURE Usage()

   ? "Usage:"
   ? "testmmail [options] attachment1, attachment2..."
   ? "  options:"
   ? "    -h              Help"
   ? '    -f "from"       Set "mail from" field'
   ? '    -t "to"         Set "mail to" field'
   ? '    -s "subject"    Set mail subject'
   ? '    -b "body"       Set mail body (or message)'
   ? '    -m "bodyfile"   Set mail body using a file'
   ?
   ?

   RETURN
