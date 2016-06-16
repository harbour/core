/* Copyright 1999 Ryszard Glab */

#include "inkey.ch"
#include "setcurs.ch"
#include "fileio.ch"
#include "box.ch"

#define EDIT_LOWER      0       // convert to lowercase
#define EDIT_UPPER      1       // convert to uppercase
#define EDIT_SAME       2       // no convertion

#define EDIT_HARD       13      // hard cariage

#define EDIT_EDIT       .T.     // full edit mode
#define EDIT_VIEW       .F.     // view only mode

// The editor structure
//
#define E_EDIT          1           // pointer returned be ED_NEW
#define E_TOP           2           // position on the screen
#define E_LEFT          3
#define E_BOTTOM        4
#define E_RIGHT         5
#define E_TITLE         6           // title
#define E_COLOR         7           // used colors set
#define E_FRAME         8           // frame around the editor
#define E_LINELEN       9           // maximal line length
#define E_MODE          10          // editor mode (edit/view)
#define E_INSERT        11          // insert state
#define E_CARGO         12          // cargo slot
#define E_STRUCT_LEN    12

THREAD STATIC t_nESize := 4096       // default buffer size

// 1993-03-06 19:52
//
// nTop, nLeft, nBottom, nRight - position on the screen
// nLength - the line length
// cFrame - the frame to be drawed around the editor
// cTitle - comment displayed in upper, left corner
// cColor - colors used to draw the editor
// nSize - the size of memory buffer that holds the edited text - the buffer
//         will not grow at current design
// nEscape - the character code used as a marker of color highlighing
// For example if its value is 126 '~' then the following text:
// normal text ~2text in bold~1 back to normal text
// will be displayed with 'text in bold' highlighted using the second
// color specified by 'cColor' parameter
//
FUNCTION EditorNew( nTop, nLeft, nBottom, nRight, nLength, cFrame, cTitle, cColor, nSize, nEscape )

   LOCAL pEdit, oEdit

   hb_default( @nLength, 80 )

   IF ! Empty( pEdit := ed_New( nLength, 4, hb_defaultValue( nSize, t_nESize ), nEscape ) )

      oEdit := Array( E_STRUCT_LEN )
      oEdit[ E_EDIT ]    := pEdit
      oEdit[ E_TOP ]     := nTop
      oEdit[ E_LEFT ]    := nLeft
      oEdit[ E_BOTTOM ]  := nBottom
      oEdit[ E_RIGHT ]   := nRight
      oEdit[ E_LINELEN ] := nLength
      oEdit[ E_FRAME ]   := hb_defaultValue( cFrame, HB_B_DOUBLE_UNI )
      oEdit[ E_TITLE ]   := cTitle
      oEdit[ E_COLOR ]   := hb_defaultValue( cColor, "W/N,W+/N,W+/R,GR+/N,G+/N" )
      oEdit[ E_MODE ]    := EDIT_VIEW

      ed_Config( pEdit, nTop, nLeft, nBottom, nRight, 0, 0 )
   ENDIF

   RETURN oEdit

PROCEDURE EditorKill( oEdit )

   oEdit[ E_EDIT ] := NIL

   RETURN

FUNCTION EditorCargo( oEdit, xCargo )

   LOCAL _xCargo := oEdit[ E_CARGO ]

   IF PCount() >= 2
      oEdit[ E_CARGO ] := xCargo
   ENDIF

   RETURN _xCargo

FUNCTION EditorTitle( oEdit, cTitle )

   LOCAL _cTitle := oEdit[ E_TITLE ]

   IF HB_ISSTRING( cTitle )
      oEdit[ E_TITLE ] := cTitle
   ENDIF

   RETURN _cTitle

// Sets
// EDIT_EDIT - full edit mode
// EDIT_VIEW - view only mode (no changes in text are allowed)
//
FUNCTION EditorMode( oEdit, lMode )

   LOCAL _lMode := oEdit[ E_MODE ]

   IF HB_ISLOGICAL( lMode )
      oEdit[ E_MODE ] := lMode
   ENDIF

   RETURN _lMode

FUNCTION EditorSize( nSize )

   LOCAL _nSize := t_nESize

   IF HB_ISNUMERIC( nSize )
      t_nESize := nSize
   ENDIF

   RETURN _nSize

// Appends passed text to the text already stored in editor
//
PROCEDURE EditorAddText( oEdit, cText )

   ed_AddText( oEdit[ E_EDIT ], cText )

   RETURN

// Sets new text in editor
//
PROCEDURE EditorSetText( oEdit, cText )

   ed_SetText( oEdit[ E_EDIT ], cText )

   RETURN

// Inserts passed text into editor starting from passed line number
//
PROCEDURE EditorInsText( oEdit, cText, nLine )

   IF ! HB_ISNUMERIC( nLine )
      nLine := ed_LCount( oEdit[ E_EDIT ] )
   ENDIF

   ed_InsText( oEdit[ E_EDIT ], cText, nLine )

   RETURN

// Retrieves the text from editor
// nCarret - specifies if soft carriage return (141/10) should be replaced by
//    hard carriage returns (13/10)
//
FUNCTION EditorGetText( oEdit, nCarret )
   RETURN ed_GetText( oEdit[ E_EDIT ], hb_defaultValue( nCarret, EDIT_HARD ) )

// Returns the line count stored in editor
//
FUNCTION EditorLCount( oEdit )
   RETURN ed_LCount( oEdit[ E_EDIT ] )

// Returns the specified line of text from the editor
//
FUNCTION EditorGetLine( oEdit, nLine )
   RETURN ed_GetLine( oEdit[ E_EDIT ], nLine )

// Returns the next line of text
//
// It can be used:
// nLCount := EditorLCount( oEdit )
// cLine := EditorGetLine( oEdit, 1 )
// FOR i := 2 TO nLCount
//    cLine := EditorNextLine( oEdit )
// NEXT
//
FUNCTION EditorNextLine( oEdit )
   RETURN ed_GetNext( oEdit[ E_EDIT ] )

// Edit the specified file
//
// xInput - the filename to edit or a handle to a file retrned by FOPEN
// cOutput - the name of the file created in 'save' operation
// nLineLen - the line length
// nHelp - the index into help subsystem
// lPrint - specifies if edited file can be printed
// lConv - it was used to convert some unprintable characters
// nEscape - the code of color escape character
//
FUNCTION EditorFile( xInput, cOutput, nLineLen, lConv, nEscape )

   LOCAL xFileHandle, nLen, oEdit, lSaved, lClose := .F.
   LOCAL lHandleOk := .F.

   IF HB_ISSTRING( xInput )
      xFileHandle := hb_vfOpen( xInput )
      lClose := .T.
   ELSE
      xFileHandle := xInput
   ENDIF

   DO CASE
   CASE HB_ISNUMERIC( xFileHandle ) .AND. xFileHandle != F_ERROR
      nLen := FSeek( xFileHandle, 0, FS_END )
      lHandleOk := .T.
   CASE HB_ISPOINTER( xFileHandle ) .AND. xFileHandle != NIL
      nLen := hb_vfSize( xFileHandle )
      lHandleOk := .T.
   ENDCASE

   nLen := Max( nLen, t_nESize )

   oEdit := EditorNew( 1, 0, 23, 79, nLineLen, "---      ", cOutput, , ;
      iif( nLen < 8192, nLen * 2, Int( nLen * 1.5 ) ), nEscape )

   IF lHandleOk
      ed_ReadText( oEdit[ E_EDIT ], xFileHandle, 0, nLen, hb_defaultValue( lConv, .F. ) )
      IF lClose
         hb_vfClose( xFileHandle )
      ENDIF
   ELSE
      EditorSetText( oEdit, " " )
   ENDIF

   EditorCargo( oEdit, cOutput )

   lSaved := EditorEdit( oEdit, EDIT_EDIT, .F. )
   EditorKill( oEdit )

   RETURN lSaved

// Reads a text from a file into the editor
//
// oEditor  - existing editor
// xFileHandle -  handle to an open file to read from
// nOffset - the starting offset
// nLen - the number of characters to read
// lConv - specifies if some unprintable characters should be converted
//    (NOTE: it was used to allow display charcters with ASCII code 27 and 26)
//
FUNCTION EditorRead( oEditor, xFileHandle, nOffset, nLen, lConv )
   RETURN ed_ReadText( oEditor[ E_EDIT ], xFileHandle, nOffset, nLen, ;
      hb_defaultValue( lConv, .T. ) )

// Start the editor
//
// oEdit - the editor object
// lEdit - .T. edit allowed, .F. view only mode
// lFrame - specifies if the frame around the editor should be displayed
// nHelp - the help index into help subsystem
//
FUNCTION EditorEdit( oEdit, lEdit, lFrame )

   LOCAL nRow, nCol := 0, nKey, nKeyStd, bKey, oBox, nCursor, nState
   LOCAL nTop, nLeft, nBottom, nRight
   LOCAL lSaveAllowed, lSaved := .F.

   oBox := SaveBox( ;
      oEdit[ E_TOP ], oEdit[ E_LEFT ], ;
      oEdit[ E_BOTTOM ], oEdit[ E_RIGHT ], ;
      oEdit[ E_COLOR ], oEdit[ E_FRAME ] )

   oEdit[ E_INSERT ] := Set( _SET_INSERT )
#if 0
   SayInsert()
#endif
   nCursor := SetCursor( iif( oEdit[ E_INSERT ], SC_NORMAL, SC_SPECIAL1 ) )
   IF HB_ISLOGICAL( lEdit )
      oEdit[ E_MODE ] := lEdit
   ENDIF
   lSaveAllowed := ( SetKey( K_F2 ) == NIL )

   nTop    := oEdit[ E_TOP ] + 1
   nLeft   := oEdit[ E_LEFT ] + 1
   nBottom := oEdit[ E_BOTTOM ] - 1
   nRight  := oEdit[ E_RIGHT ] - 1
   IF ! hb_defaultValue( lFrame, .T. )
      nLeft--
      nBottom++
      nRight++
   ENDIF
   nState := oEdit[ E_RIGHT ] - 8

   /* The position of the editor can be changed (in a windowed environment)
      then it sets current position of editor.
      It also sets the current editor as the working one. This means that
      all next ED_* functions will used the editor handle specified
      by oEditor[ E_EDIT ] - it is tricky solution to speed access (we
      don't need to pass the editor handle with every ed_*() call
      (Well... this editor was created when AT-286 computers worked in
      its full glory :) */
   ed_Config( oEdit[ E_EDIT ], nTop, nLeft, nBottom, nRight, 0, 0 )

   DO WHILE .T.
      nRow := ed_Stabilize( oEdit[ E_EDIT ] )    // displays all visible lines
      // It doesn't uses incremantal stabilization for performance reasons

      IF nRow != ed_Row( oEdit[ E_EDIT ] )
         nRow := ed_Row( oEdit[ E_EDIT ] )
         hb_DispOutAt( oEdit[ E_TOP ], nState, StrZero( nRow, 4 ) )
      ENDIF
      IF nCol != ed_Col( oEdit[ E_EDIT ] )
         nCol := ed_Col( oEdit[ E_EDIT ] )
         hb_DispOutAt( oEdit[ E_TOP ], nState + 5, StrZero( nCol, 3 ) )
      ENDIF
      SetPos( nTop + ed_WinRow( oEdit[ E_EDIT ] ), nLeft + ed_WinCol( oEdit[ E_EDIT ] ) )

      nKeyStd := hb_keyStd( nKey := Inkey( 0, hb_bitOr( Set( _SET_EVENTMASK ), HB_INKEY_EXT ) ) )

      DO CASE
      CASE !( hb_keyChar( nKeyStd ) == "" )
         IF oEdit[ E_MODE ]
            ed_PutChar( oEdit[ E_EDIT ], Asc( hb_keyChar( nKeyStd ) ), oEdit[ E_INSERT ] )
         ENDIF

      CASE nKeyStd == K_F2 .AND. lSaveAllowed
         lSaved := EditorSave( oEdit )     // save the copy of edited buffer

      CASE EditorMove( oEdit[ E_EDIT ], nKeyStd )

      CASE nKeyStd == K_DOWN
         IF ! ed_Down( oEdit[ E_EDIT ] )
            hb_Scroll( nTop, nLeft, nBottom, nRight, 1 )
         ENDIF

      CASE nKeyStd == K_UP
         IF ! ed_Up( oEdit[ E_EDIT ] )
            hb_Scroll( nTop, nLeft, nBottom, nRight, -1 )
         ENDIF

      CASE nKeyStd == K_ESC
         EXIT

      OTHERWISE
         IF HB_ISEVALITEM( bKey := SetKey( nKey ) ) .OR. ;
            HB_ISEVALITEM( bKey := SetKey( nKeyStd ) )
            Eval( bKey, oEdit )
         ELSE
            IF oEdit[ E_MODE ]
               EditorKeys( oEdit, nKeyStd )
            ENDIF
         ENDIF
      ENDCASE
   ENDDO

   SetCursor( nCursor )
   RestBox( oBox )

   RETURN lSaved

STATIC PROCEDURE EditorKeys( oEdit, nKeyStd )

   LOCAL i

   SWITCH nKeyStd
   CASE K_CTRL_Y
      ed_DelLine( oEdit[ E_EDIT ] )
      EXIT

   CASE K_CTRL_T
      ed_DelWord( oEdit[ E_EDIT ] )
      EXIT

   CASE K_DEL
      ed_DelChar( oEdit[ E_EDIT ] )
      EXIT

   CASE K_BS
      ed_BSpace( oEdit[ E_EDIT ], oEdit[ E_INSERT ] )
      EXIT

   CASE K_ENTER
      ed_Return( oEdit[ E_EDIT ], oEdit[ E_INSERT ] )
      EXIT

   CASE K_TAB
#if 0
      ed_Tab( oEdit[ E_EDIT ], oEdit[ E_INSERT ] )
#endif
      FOR i := 1 TO 4
         ed_PutChar( oEdit[ E_EDIT ], Asc( " " ), oEdit[ E_INSERT ] )
      NEXT
      EXIT

   CASE K_INS
      oEdit[ E_INSERT ] := ! oEdit[ E_INSERT ]
      Set( _SET_INSERT, oEdit[ E_INSERT ] )
      SetCursor( iif( oEdit[ E_INSERT ], SC_NORMAL, SC_SPECIAL1 ) )
#if 0
      SayInsert()
#endif
      EXIT

   ENDSWITCH

   RETURN

STATIC FUNCTION EditorMove( pEdit, nKeyStd )

   SWITCH nKeyStd
   CASE K_PGDN       ; ed_PgDown( pEdit ) ; EXIT
   CASE K_PGUP       ; ed_PgUp( pEdit ) ; EXIT
   CASE K_CTRL_PGUP  ; ed_Top( pEdit ) ; EXIT
   CASE K_CTRL_PGDN  ; ed_Bottom( pEdit ) ; EXIT
   CASE K_RIGHT      ; ed_Right( pEdit ) ; EXIT
   CASE K_LEFT       ; ed_Left( pEdit ) ; EXIT
   CASE K_HOME       ; ed_Home( pEdit ) ; EXIT
   CASE K_CTRL_HOME  ; ed_Home( pEdit ) ; EXIT
   CASE K_END        ; ed_End( pEdit ) ; EXIT
   CASE K_CTRL_END   ; ed_End( pEdit ) ; EXIT
   CASE K_CTRL_RIGHT // ; ed_NWord( pEdit ) ; EXIT   // there are some problems with it
   CASE K_CTRL_LEFT  ; ed_PWord( pEdit ) ; EXIT
   OTHERWISE         ; RETURN .F.
   ENDSWITCH

   RETURN .T.

STATIC FUNCTION EditorSave( oEdit )

   LOCAL hFile, cFile

   cFile := EditorCargo( oEdit )
   IF ! HB_ISSTRING( cFile ) .OR. HB_ISNULL( cFile )
      RETURN .F.
   ENDIF

   IF ( hFile := hb_vfOpen( cFile, FO_CREAT + FO_TRUNC + FO_WRITE ) ) != NIL
      hb_vfWrite( hFile, EditorGetText( oEdit ) )
      hb_vfClose( hFile )
   ENDIF

   RETURN hFile != NIL

FUNCTION SaveBox( top, left, bott, right, color, patt )

   LOCAL cBox := SaveScreen( top, left, bott, right )
   LOCAL cClr

   IF PCount() > 4
      cClr := SetColor( color )
      hb_DispBox( top, left, bott, right, patt )
   ELSE
      cClr := SetColor()
   ENDIF

   RETURN { top, left, bott, right, cBox, cClr }

PROCEDURE RestBox( oBox )

   RestScreen( oBox[ 1 ], oBox[ 2 ], oBox[ 3 ], oBox[ 4 ], oBox[ 5 ] )
   SetColor( oBox[ 6 ] )

   RETURN
