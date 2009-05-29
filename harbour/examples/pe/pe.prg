/*
 * $Id$
 */

#define EDIT_EDIT .T.
#define EDIT_VIEW .F.

PROCEDURE Main( cFile )
   LOCAL cText
   LOCAL lMode := EDIT_EDIT

   IF cFile == NIL
      cFile := "license.txt"
      IF !File( cFile )
         cFile := "../../../license.txt"
      ENDIF
      lMode := EDIT_VIEW
   ENDIF
  
   cText := MemoRead( cFile )
   cText := MemoEditor( cText, 0, 0, MaxRow(), MaxCol(), lMode )
   MemoWrit( "output.txt", cText )
 
   RETURN

STATIC FUNCTION MEMOEDITOR( cText, nTop, nLeft, nBottom, nRight, lMode )
   LOCAL oED

   /* NOTE: In current design of editor it doesn't reallocate the memory
      buffer used to hold the text
   */
   oED := EditorNew( nTop, nLeft, nBottom, nRight, 254, , , , Len( cText ) * 2, 168 )
   IF oED != NIL
      EditorSetText( oED, cText )
      EditorEdit( oED, lMode, .T. )
      cText := EditorGetText( oED )
      EditorKill( oED )
   ELSE
      ? "Editor not created"
   ENDIF

   RETURN cText
