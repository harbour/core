/* Harbour rtfclass demo
 * notes: - raw enough but it works
 * initial release: 1999-06-23 Andi Jahja
 * works with printable ASCII only
 * placed in the public domain
 */

#require "hbnf"

#include "hbclass.ch"
#include "fileio.ch"

PROCEDURE Main()

   LOCAL cTemp := hb_FNameExtSet( __FILE__, ".txt" )
   LOCAL oRtf

   // create a plain text file
   hb_MemoWrit( cTemp, ;
      "This is +bHarbour (C) RTF Class-b" + hb_eol() + ;
      "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG" + hb_eol() + ;
      "+bTHE QUICK BROWN FOX JUMPS OVER THE LAZY DOG-b" + hb_eol() + ;
      "+iTHE QUICK BROWN FOX JUMPS OVER THE LAZY DOG-i" + hb_eol() + ;
      "+buTHE QUICK BROWN FOX JUMPS OVER THE LAZY DOG-bu" + hb_eol() + ;
      "+buiTHE QUICK BROWN FOX JUMPS OVER THE LAZY DOG-bui" + hb_eol() + ;
      "THE +bQUICK-b +buBROWN-bu +buiFOX-bui +iJUMPS-i +uOVER-u +ilTHE-il +uLAZY-u +buDOG-bu" + hb_eol() )

   // convert text file to rtf
   oRtf := TRtf():new( hb_FNameExtSet( __FILE__, ".rtf" ) )
   oRtf:write( cTemp )
   oRtf:close()

   RETURN

CREATE CLASS TRtf

   METHOD new( cFilename )
   METHOD write( cSource )
   METHOD close()

   PROTECTED:

   VAR hFile

ENDCLASS

METHOD new( cFilename ) CLASS TRtf

   IF ( ::hFile := hb_vfOpen( cFilename, FO_CREAT + FO_TRUNC + FO_WRITE ) ) != NIL
      hb_vfWrite( ::hFile, ;
         "{\rtf1\ansi\deff0{\fonttbl {\f0\fnil\fcharset0 Courier New;}{\f1\fnil\fcharset0 Arial;}}" + ;
         "\uc1\pard\lang1033\ulnone\f0\fs20" + hb_eol() )
   ENDIF

   RETURN self

METHOD write( cSource ) CLASS TRtf

   // These are character attributes, self-defined
   // + means a turn on
   // - means a turn off
   STATIC s_attrib := { ;
      { "+b"  , "\b "            }, ;  /* turn bold on*/
      { "+bu" , "\ul\b "         }, ;  /* turn bold_underline on */
      { "+bi" , "\b\i "          }, ;  /* turn bold_italic on */
      { "+bui", "\ul\b\i "       }, ;  /* turn bold_underline_italic on */
      { "+i"  , "\i "            }, ;  /* turn italic on */
      { "+il" , "\ul\i "         }, ;  /* turn italic_underline on */
      { "+u"  , "\ul "           }, ;  /* turn underline on */
      { "-b"  , "\b0 "           }, ;  /* turn bold off */
      { "-bu" , "\b0\ulnone "    }, ;  /* turn bold_underline off */
      { "-bi" , "\b0\i0 "        }, ;  /* turn bold_italic off */
      { "-bui", "\b0\i0\ulnone " }, ;  /* turn bold_underline_italic off */
      { "-i"  , "\i0 "           }, ;  /* turn italic off */
      { "-il" , "\ulnone\i0 "    }, ;  /* turn italic_underline off */
      { "-u"  , "\ulnone "       } }   /* turn underline off */

   LOCAL cChar, cLine, xAtt, i
   LOCAL nChar, y

   IF ::hFile != NIL

      ft_FUse( cSource )  // open source file
      DO WHILE ! ft_FEof()  // read the file line by line
         cLine := ft_FReadLn() + "\par"
         y     := Len( cLine )
         FOR nChar := 1 TO y
            cChar := SubStr( cLine, nChar, 1 )

            // TODO: I need function dec2hex()
            // to convert ASCII to 2-characters hex
            // ie  : dec2hex( "H" ) -> 48
            IF cChar == "+" .OR. cChar == "-"
               xAtt := cChar + ;
                  SubStr( cLine, nChar + 1, 1 ) + ;
                  SubStr( cLine, nChar + 2, 1 ) + ;
                  SubStr( cLine, nChar + 3, 1 )
               IF ( i := AScan( s_attrib, {| e | e[ 1 ] == xAtt } ) ) > 0
                  hb_vfWrite( ::hFile, s_attrib[ i ][ 2 ] )
                  nChar += Len( xAtt ) - 1
               ELSE
                  // 3 attributes
                  xatt := Left( xAtt, 3 )
                  IF ( i := AScan( s_attrib, {| e | e[ 1 ] == xAtt } ) ) > 0
                     hb_vfWrite( ::hFile, s_attrib[ i ][ 2 ] )
                     nChar += Len( xAtt ) - 1
                  ELSE
                     // 2 attributes
                     xAtt := Left( xAtt, 2 )
                     IF ( i := AScan( s_attrib, {| e | e[ 1 ] == xAtt } ) ) > 0
                        hb_vfWrite( ::hFile, s_attrib[ i ][ 2 ] )
                        nChar += Len( xAtt ) - 1
                     ELSE
                        hb_vfWrite( ::hFile, cChar )
                     ENDIF
                  ENDIF
               ENDIF
            ELSE
               hb_vfWrite( ::hFile, cChar )
            ENDIF
         NEXT
         hb_vfWrite( ::hFile, hb_eol() )
         ft_FSkip()  // read next line
      ENDDO
      ft_FUse()
   ENDIF

   RETURN self

METHOD close() CLASS TRtf

   IF ::hFile != NIL
      hb_vfWrite( ::hFile, "\f1\fs16\par" + hb_eol() + "}" )
      hb_vfClose( ::hFile )
   ENDIF

   RETURN self
