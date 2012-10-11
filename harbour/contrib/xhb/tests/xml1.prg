/*
 * $Id$
 */

#require "xhb"

#include "hbxml.ch"

PROCEDURE Main()
   LOCAL cFile := hb_dirBase() + "test.xml"
   LOCAL cString
   LOCAL cNote, cDiscount
   LOCAL oDoc, oBook, oIterator, oCurrent

   cString := MemoRead( cFile )

   IF Empty( cString )
      WAIT "xml file unavailable"
      RETURN
   ENDIF

   oDoc := TXmlDocument():New( cString, HBXML_STYLE_NOESCAPE )
   IF oDoc:nError != HBXML_ERROR_NONE
      WAIT "xml file parsing error " + Str( oDoc:nError )
      RETURN
   ENDIF

   oBook := oDoc:findfirst( "book" )
   IF oBook == NIL
      WAIT "no books found"
      RETURN
   ENDIF

   DO WHILE .T.

      IF "id" $ oBook:aAttributes
         ? "book ID : " + oBook:aAttributes[ "id" ]
      ELSE
         ? "no attribute book ID"
      ENDIF

      cNote := ""
      cDiscount := ""
      oIterator := TXmlIterator():New( oBook )

      DO WHILE .T.
         oCurrent := oIterator:Next()
         IF oCurrent == NIL
            ? "end branch"
            WAIT "values : " + cNote + " " + cDiscount
            EXIT
         ELSE
            ? "current tag : " + oCurrent:cName
            IF oCurrent:cName == "note"
               cNote := oCurrent:cData
            ELSEIF oCurrent:cName == "discount"
               cDiscount := oCurrent:cData
            ENDIF
         ENDIF
      ENDDO

      oBook := oDoc:findnext()
      IF oBook == NIL
         WAIT "no more books found"
         EXIT
      ENDIF

   ENDDO

   RETURN
