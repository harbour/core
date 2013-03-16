
#require "hbmisc"

PROCEDURE Main( cFile )

   LOCAL cText
   LOCAL lEdit := .T.

   IF cFile == NIL
      cFile := __FILE__
      lEdit := .F.
   ENDIF

   cText := MemoRead( cFile )
   cText := MyMemoEdit( cText, 0, 0, MaxRow(), MaxCol(), lEdit )
   MemoWrit( hb_FNameExtSet( __FILE__, ".out" ), cText )

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
