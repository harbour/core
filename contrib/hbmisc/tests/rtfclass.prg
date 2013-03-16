/*
 * Harbour rtfclass demo
 * notes : - raw enough but it works
           - rtf is assumed to have association
 * initial release : 1999-06-23 Andi Jahja
 * works with printable ascii only
 * placed in the public domain
*/

#require "hbmisc"

#include "hbclass.ch"

PROCEDURE Main()

   LOCAL cTemp := hb_FNameExtSet( __FILE__, ".txt" )

   LOCAL oRtf
   LOCAL cTest

   // create a plain text file
   cTest := ""
   cTest += "This is +bHarbour (C) RTF Class-b" + hb_eol()
   cTest += "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG" + hb_eol()
   cTest += "+bTHE QUICK BROWN FOX JUMPS OVER THE LAZY DOG-b" + hb_eol()
   cTest += "+iTHE QUICK BROWN FOX JUMPS OVER THE LAZY DOG-i" + hb_eol()
   cTest += "+buTHE QUICK BROWN FOX JUMPS OVER THE LAZY DOG-bu" + hb_eol()
   cTest += "+buiTHE QUICK BROWN FOX JUMPS OVER THE LAZY DOG-bui" + hb_eol()
   cTest += "THE +bQUICK-b +buBROWN-bu +buiFOX-bui +iJUMPS-i +uOVER-u +ilTHE-il +uLAZY-u +buDOG-bu" + hb_eol()

   hb_MemoWrit( cTemp, cTest )

   // convert text file to rtf
   oRtf := TRtf():new( hb_FNameExtSet( __FILE__, ".rtf" ) )
   oRtf:write( cTemp )
   oRtf:close()

   RETURN

CREATE CLASS TRtf

   VAR nHandle

   METHOD new( cFilename )
   METHOD write( cSource )
   METHOD CLOSE()

END CLASS

METHOD new( cFilename ) CLASS TRtf

   ::nHandle := FCreate( cFilename )
   FWrite( ::nHandle, ;
      "{\rtf1\ansi\deff0{\fonttbl {\f0\fnil\fcharset0 Courier New;}{\f1\fnil\fcharset0 Arial;}}" + ;
      "\uc1\pard\lang1033\ulnone\f0\fs20" + hb_eol() )

   RETURN self

METHOD write( cSource ) CLASS TRtf

   LOCAL cChar, cLine, xAtt, i
   LOCAL nChar, y

   // These are character attributes, self-defined
   // + means a turn-on
   // - means a turn-off
   LOCAL attrib := { ;
      { "+b"  , "\b "            }, ; /* turn bold on*/
      { "+bu" , "\ul\b "         }, ; /* turn bold_underline on */
      { "+bi" , "\b\i "          }, ; /* turn bold_italic on */
      { "+bui", "\ul\b\i "       }, ; /* turn bold_underline_italic on */
      { "+i"  , "\i "            }, ; /* turn italic on */
      { "+il" , "\ul\i "         }, ; /* turn italic_underline on */
      { "+u"  , "\ul "           }, ; /* turn underline on */
      { "-b"  , "\b0 "           }, ; /* turn bold off */
      { "-bu" , "\b0\ulnone "    }, ; /* turn bold_underline off */
      { "-bi" , "\b0\i0 "        }, ; /* turn bold_italic off */
      { "-bui", "\b0\i0\ulnone " }, ; /* turn bold_underline_italic off */
      { "-i"  , "\i0 "           }, ; /* turn italic off */
      { "-il" , "\ulnone\i0 "    }, ; /* turn italic_underline off */
      { "-u"  , "\ulnone "       } } /* turn underline off */

   hb_FUse( cSource )  // open source file
   WHILE ! hb_FAtEof()  // read the file line by line
      cLine := hb_FReadLn() + "\par"
      y     := Len( cLine )
      FOR nChar := 1 TO y
         cChar := SubStr( cLine, nChar, 1 )

         // todo : i need function dec2hex()
         // to convert ascii to 2-characters hex
         // ie   : dec2hex( "H" ) -> 48
         IF cChar == "+" .OR. cChar == "-"
            xAtt := cChar + ;
               SubStr( cLine, nChar + 1, 1 ) + ;
               SubStr( cLine, nChar + 2, 1 ) + ;
               SubStr( cLine, nChar + 3, 1 )
            IF ( i := AScan( attrib, {| e | e[ 1 ] == xAtt } ) ) > 0
               FWrite( ::nhandle, attrib[ i ][ 2 ] )
               nChar += Len( xAtt ) - 1
            ELSE
               // 3 attributes
               xatt := Left( xAtt, 3 )
               IF ( i := AScan( attrib, {| e | e[ 1 ] == xAtt } ) ) > 0
                  FWrite( ::nHandle, attrib[ i ][ 2 ] )
                  nChar += Len( xAtt ) - 1
               ELSE
                  // 2 attributes
                  xAtt := Left( xAtt, 2 )
                  IF ( i := AScan( attrib, {| e | e[ 1 ] == xAtt } ) ) > 0
                     FWrite( ::nHandle, attrib[ i ][ 2 ] )
                     nChar += Len( xAtt ) - 1
                  ELSE
                     FWrite( ::nHandle, cChar )
                  ENDIF
               ENDIF
            ENDIF
         ELSE
            FWrite( ::nHandle, cChar )
         ENDIF
      NEXT
      FWrite( ::nHandle, hb_eol() )
      hb_FSkip() // read next line
   ENDDO
   hb_FUse()

   RETURN self

METHOD CLOSE() CLASS TRtf

   FWrite( ::nHandle, "\f1\fs16\par" + hb_eol() + "}" )
   FClose( ::nHandle )

   RETURN self
