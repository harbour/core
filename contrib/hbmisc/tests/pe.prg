#require "hbmisc"

PROCEDURE Main( cFile )

   LOCAL cText
   LOCAL lEdit := .T.

   IF cFile == NIL
      cText := Example_Text()
   ELSE
      IF hb_FileMatch( cFile, __FILE__ )
         lEdit := .F.
      ENDIF
      cText := MemoRead( cFile )
   ENDIF

   cText := MyMemoEdit( cText, 0, 0, MaxRow(), MaxCol(), lEdit )
   hb_MemoWrit( hb_FNameExtSet( __FILE__, ".out" ), cText )

   RETURN

STATIC FUNCTION MyMemoEdit( cText, nTop, nLeft, nBottom, nRight, lEdit )

   LOCAL oED

   /* NOTE: In current design of editor it doesn't reallocate the memory
      buffer used to hold the text
   */

   oED := EditorNew( nTop, nLeft, nBottom, nRight, 254, , , , Len( cText ) * 2, 168 )
   IF oED != NIL
      EditorSetText( oED, cText )
      EditorEdit( oED, lEdit, .T. )
      cText := EditorGetText( oED )
      EditorKill( oED )
   ELSE
      ? "Editor not created"
   ENDIF

   RETURN cText

STATIC FUNCTION Example_Text()
   RETURN StrTran( Example_Text_Raw(), "~", hb_BChar( 168 ) )

STATIC FUNCTION Example_Text_Raw()
#pragma __cstream | RETURN %s

 ~2Lorem ipsum dolor sit amet, consectetur adipiscing elit.~1
~3 ========================================================

 Integer a dolor eu lorem luctus cursus. Proin varius dui quis felis.
 Sed vitae lacus ac mauris cursus vehicula. Nulla sodales fringilla
 lorem. Praesent odio mi, interdum eget, facilisis et, elementum
 luctus, enim. Vivamus sed est. Cras leo dolor, mollis et, suscipit
 sed, interdum ac, elit. Phasellus cursus, arcu nec bibendum cursus,
 dolor mi vestibulum leo, ut cursus orci sem id dolor. Morbi porttitor
 libero nec risus. Duis diam justo, blandit sed, fermentum eu,
 consectetur at, arcu. Quisque felis ipsum, facilisis quis, ultrices
 sed, venenatis aliquam, urna. Cras sit amet risus. Pellentesque
 dictum. In hac habitasse platea dictumst.

 Nunc odio elit, mattis et, rutrum id, faucibus vitae, ipsum. Donec
 aliquet sem sed augue. Aliquam erat volutpat. Etiam ultrices, metus
 a tristique ornare, tellus urna congue nunc, vitae iaculis risus
 enim quis magna.

~5This file have to be separated with CR/LF characters~1
#pragma __endtext
