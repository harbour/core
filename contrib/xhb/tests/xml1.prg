#require "xhb"

#include "hbxml.ch"

PROCEDURE Main()

   LOCAL cFile := hb_DirBase() + "test.xml"
   LOCAL cString
   LOCAL cNote, cDiscount
   LOCAL oDoc, oBook, oIterator, oCurrent

   cString := MemoRead( cFile )

   IF Empty( cString )
      ? "xml file unavailable"
      RETURN
   ENDIF

   oDoc := TXMLDocument():New( cString, HBXML_STYLE_NOESCAPE )
   IF oDoc:nError != HBXML_ERROR_NONE
      ? "xml file parsing error", hb_ntos( oDoc:nError )
      RETURN
   ENDIF

   oBook := oDoc:findfirst( "book" )
   IF oBook == NIL
      ? "no books found"
      RETURN
   ENDIF

   DO WHILE .T.

      IF "id" $ oBook:aAttributes
         ? "book ID:", oBook:aAttributes[ "id" ]
      ELSE
         ? "no attribute book ID"
      ENDIF

      cNote := ""
      cDiscount := ""
      oIterator := TXMLIterator():New( oBook )

      DO WHILE .T.
         oCurrent := oIterator:Next()
         IF oCurrent == NIL
            ? "end branch"
            ? "values:", cNote, cDiscount
            Inkey( 0 )
            EXIT
         ELSE
            ? "current tag:", oCurrent:cName
            IF oCurrent:cName == "note"
               cNote := oCurrent:cData
            ELSEIF oCurrent:cName == "discount"
               cDiscount := oCurrent:cData
            ENDIF
         ENDIF
      ENDDO

      oBook := oDoc:findnext()
      IF oBook == NIL
         ? "no more books found"
         EXIT
      ENDIF

   ENDDO

   RETURN
