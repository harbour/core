/*
 * $Id$
 */
#define EDIT_EDIT .T.
#define EDIT_VIEW .F.

PROCEDURE MAIN( cFile )
LOCAL cText
LOCAL lMode:=EDIT_EDIT

  IF( cFile == NIL )
    cFile ="license.txt"
    IF( !FILE(cFile) )
      cFile ="../../license.txt"
    ENDIF
    lMode =EDIT_VIEW
  ENDIF

  cText =MEMOREAD( cFile )
  cText =MEMOEDITOR( cText, 0,0, MAXROW(), MAXCOL(), lMode )
  MEMOWRIT( "OUTPUT.TXT", cText )
 
RETURN

STATIC FUNCTION MEMOEDITOR( cText, nTop, nLeft, nBottom, nRight, lMode )
LOCAL oED

  /* NOTE: In current design of editor it doesn't reallocate the memory
     buffer used to hold the text
  */
  oED :=EditorNew( nTop, nLeft, nBottom, nRight, 254, , , , LEN(cText)*2, 168 )
  IF( oED != NIL )
    EditorSetText( oED, cText )
    EditorEdit( oED, lMode, .T. )
    cText :=EditorGetText( oED )
    EditorKill( oED )
  ELSE
    ? "Editor not created"
  ENDIF

RETURN( cText )