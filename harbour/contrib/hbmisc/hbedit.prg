/*
 * $Id$
 */

#include "inkey.ch"
#include "setcurs.ch"
#include "fileio.ch"
#include "box.ch"


#define IIFNIL( isnil, notnil ) iif( notnil == NIL, isnil, notnil )

#define EDIT_LOWER      0       // convert to lowercase
#define EDIT_UPPER      1       // convert to uppercase
#define EDIT_SAME       2       // no convertion

#define EDIT_HARD       13      // hard cariage

#define EDIT_EDIT       .T.     // full edit mode
#define EDIT_VIEW       .F.     // view only mode

//  The editor structure
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

THREAD STATIC s_nESize := 4096       // default buffer size

//
// 03-06-93 07:52pm
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

FUNCTION EditorNew( nTop, nLeft, nBottom, nRight, nLength, ;
      cFrame, cTitle, cColor, nSize, nEscape )

   LOCAL pEdit, oEdit

   IF ! HB_ISNUMERIC( nLength )
      nLength := 80
   ENDIF

   pEdit := ed_New( nLength, 4, IIFNIL( s_nESize, nSize ), nEscape )
   IF ! Empty( pEdit )
      oEdit := Array( E_STRUCT_LEN )
      oEdit[ E_EDIT ]    := pEdit
      oEdit[ E_TOP ]     := nTop
      oEdit[ E_LEFT ]    := nLeft
      oEdit[ E_BOTTOM ]  := nBottom
      oEdit[ E_RIGHT ]   := nRight
      oEdit[ E_LINELEN ] := nLength
      oEdit[ E_FRAME ]   := IIFNIL( HB_B_DOUBLE_UNI, cFrame )
      oEdit[ E_TITLE ]   := cTitle
      oEdit[ E_COLOR ]   := IIFNIL( "W/N,W+/N,W+/R,GR+/N,G+/N", cColor )
      oEdit[ E_MODE ]    := EDIT_VIEW

      ed_Config( pEdit, nTop, nLeft, nBottom, nRight, 0, 0 )
   ENDIF

   RETURN oEdit

//
// 03-06-93 09:16pm
//

PROCEDURE EditorKill( oEdit )

   oEdit[ E_EDIT ] := NIL

   RETURN

//
// 03-06-93 10:20pm
//

FUNCTION EditorCargo( oEdit, xCargo )

   LOCAL _xCargo := oEdit[ E_CARGO ]

   IF PCount() >= 2
      oEdit[ E_CARGO ] := xCargo
   ENDIF

   RETURN _xCargo

//
// 19-07-93 01:08am
//

FUNCTION EditorTitle( oEdit, cTitle )

   LOCAL _cTitle := oEdit[ E_TITLE ]

   IF HB_ISSTRING( cTitle )
      oEdit[ E_TITLE ] := cTitle
   ENDIF

   RETURN _cTitle

//
// 04-06-93 02:18am
//
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

//
// 28-05-92 09:31am
//

FUNCTION EditorSize( nSize )

   LOCAL _nSize := s_nESize

   IF nSize != NIL
      s_nESize := nSize
   ENDIF

   RETURN _nSize

//
// 28-02-92 10:57pm
//
// Appends passed text to the text already stored in editor
//

PROCEDURE EditorAddText( oEdit, cText )

   ed_AddText( oEdit[ E_EDIT ], cText )

   RETURN

//
// 05-03-92 10:21pm
//
// Sets new text in editor
//

PROCEDURE EditorSetText( oEdit, cText )

   ed_SetText( oEdit[ E_EDIT ], cText )

   RETURN

//
// 05-03-92 10:23pm
//
// Inserts passed text into editor starting from passed line number
//

PROCEDURE EditorInsText( oEdit, cText, nLine )

   LOCAL nNum := IIFNIL( ed_LCount( oEdit[ E_EDIT ] ), nLine )

   ed_InsText( oEdit[ E_EDIT ], cText, nNum )

   RETURN

//
// 02-03-92 07:53pm
//
// Retrieves the text from editor
// nCarret - specifies if soft carriage return (141/10) should be replaced by
//    hard carriage returns (13/10)
//

FUNCTION EditorGetText( oEdit, nCarret )

   IF ! HB_ISNUMERIC( nCarret )
      nCarret := EDIT_HARD
   ENDIF

   RETURN ed_GetText( oEdit[ E_EDIT ], nCarret )

//
// 04-03-92 02:35pm
//
// Returns the line count stored in editor
//

FUNCTION EditorLCount( oEdit )

   RETURN ed_LCount( oEdit[ E_EDIT ] )

//
// 06-03-92 07:09pm
//
// Returns the specified line of text from the editor
//

FUNCTION EditorGetLine( oEdit, nLine )

   RETURN ed_GetLine( oEdit[ E_EDIT ], nLine )

//
// 06-03-92 07:10pm
//
// Returns the next line of text
//
// It can be used:
// nLCount := EditorLCount( oEdit )
// cLine := EditorGetLine( oEdit, 1 )
// FOR i := 2 TO nLCount
//   cLine := EditorNextLine( oEdit )
// NEXT
//

FUNCTION EditorNextLine( oEdit )

   RETURN ed_GetNext( oEdit[ E_EDIT ] )

//
// 03-06-93 10:11pm
//
// Edit the specified file
//
// xInput - the filename to edit or a handle to a file retrned by FOPEN
// cOutput - the name of the file created in 'save' operation
// nLineLen - the line length
// nHelp - the index into help subsystem
// lPrint - specifies if edited file can be printed
// lConv - it was used to convert some unprintable characters
// nEscape - the code of color escape character
// lSave - specifies if edited file can be saved under a different name
//

FUNCTION EditorFile( xInput, cOutput, nLineLen, ;
      lConv, nEscape, lSave )

   LOCAL nHandle, nLen, oEdit, lSaved, lClose := .F.
   LOCAL nSize

   IF ! HB_ISLOGICAL( lSave )
      lSave := .T.
   ENDIF

   IF HB_ISSTRING( xInput )
      nHandle := FOpen( xInput )
      lClose := .T.
   ELSE
      nHandle := xInput
   ENDIF

   IF nHandle > 0
      nLen := Max( FileLength( nHandle ), s_nESize )
   ELSE
      nLen := s_nESize
   ENDIF

   nSize := iif( nLen < 8192, nLen * 2, Int( nLen * 1.5 ) )
   oEdit := EditorNew( 1, 0, 23, 79, nLineLen, "---      ", cOutput, , ;
      nSize, nEscape )

   IF nHandle > 0
      ed_ReadText( oEdit[ E_EDIT ], nHandle, 0, nLen, ;
         iif( lConv == NIL, .F., lConv ) )
      IF lClose
         FClose( nHandle )
      ENDIF
   ELSE
      EditorSetText( oEdit, " " )
   ENDIF

   EditorCargo( oEdit, cOutput )

   lSaved := EditorEdit( oEdit, EDIT_EDIT, .F. )
   EditorKill( oEdit )

   RETURN lSaved

//
// 06-07-93 06:05pm
//
// Reads a text from a file into the editor
//
// oEditor  - existing editor
// nHandle -  handle to an open file to read from
// nOffset - the starting offset
// nLen - the number of characters to read
// lConv - specifies if some unprintable characters should be converted
//    (NOTE: it was used to allow display charcters with ASCII code 27 and 26)
//

FUNCTION EditorRead( oEditor, nHandle, nOffset, nLen, lConv )

   RETURN ed_ReadText( oEditor[ E_EDIT ], nHandle, nOffset, nLen, ;
      iif( lConv == NIL, .T., lConv ) )

//
// 03-06-93 08:31pm
//
// Start the editor
//
// oEdit - the editor object
// lEdit - .T. = edit allowed, .F. = view only mode
// lFrame - specifies if the frame around the editor should be displayed
// nHelp - the help index into help subsystem
//

FUNCTION EditorEdit( oEdit, lEdit, lFrame )

   LOCAL nRow, nCol := 0, nKey, bKey, oBox, nCursor, nState
   LOCAL nTop, nLeft, nBottom, nRight
   LOCAL lSaveAllowed, lSaved := .F.

   oBox := SaveBox( oEdit[ E_TOP ], oEdit[ E_LEFT ], ;
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
#if 0
   IF lSaveAllowed
      DisplayHelp( 73 )     // F2-save
   ENDIF
#endif

   nTop    := oEdit[ E_TOP ] + 1
   nLeft   := oEdit[ E_LEFT ] + 1
   nBottom := oEdit[ E_BOTTOM ] - 1
   nRight  := oEdit[ E_RIGHT ] - 1
   IF lFrame != NIL .AND. ! lFrame
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
      don't need to pass the editor handle with every ED_*() call
      (Well... this editor was created when AT-286 computers worked in
      its full glory :)
   */
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

#if 0
      nKey := WaitForKey()
#endif
      nKey := Inkey( 0 )

      DO CASE
      CASE !( hb_keyChar( nKey ) == "" )
         IF oEdit[ E_MODE ]
            ed_PutChar( oEdit[ E_EDIT ], Asc( hb_keyChar( nKey ) ), oEdit[ E_INSERT ] )
         ENDIF

      CASE nKey == K_F2 .AND. lSaveAllowed
         lSaved := EditorSave( oEdit )     // save the copy of edited buffer

      CASE EditorMove( oEdit[ E_EDIT ], nKey )

      CASE nKey == K_DOWN
         IF ! ed_Down( oEdit[ E_EDIT ] )
            hb_Scroll( nTop, nLeft, nBottom, nRight, 1 )
         ENDIF

      CASE nKey == K_UP
         IF ! ed_Up( oEdit[ E_EDIT ] )
            hb_Scroll( nTop, nLeft, nBottom, nRight, -1 )
         ENDIF

      CASE nKey == K_ESC
         EXIT

      OTHERWISE
         bKey := SetKey( nKey )
         IF HB_ISBLOCK( bKey )
            Eval( bKey, oEdit )
         ELSE
            IF oEdit[ E_MODE ]
               EditorKeys( oEdit, nKey )
            ENDIF
         ENDIF
      ENDCASE
   ENDDO

   SetCursor( nCursor )
   RestBox( oBox )
   // HELPREST.

   RETURN lSaved

//
// *
//

//
// 03-06-93 08:35pm
//

STATIC PROCEDURE EditorKeys( oEdit, nKey )

   LOCAL i

   DO CASE
   CASE nKey == K_CTRL_Y
      ed_DelLine( oEdit[ E_EDIT ] )

   CASE nKey == K_CTRL_T
      ed_DelWord( oEdit[ E_EDIT ] )

   CASE nKey == K_DEL
      ed_DelChar( oEdit[ E_EDIT ] )

   CASE nKey == K_BS
      ed_BSpace( oEdit[ E_EDIT ], oEdit[ E_INSERT ] )

   CASE nKey == K_ENTER
      ed_Return( oEdit[ E_EDIT ], oEdit[ E_INSERT ] )

   CASE nKey == K_TAB
#if 0
      ed_Tab( oEdit[ E_EDIT ], oEdit[ E_INSERT ] )
#endif
      FOR i := 1 TO 4
         ed_PutChar( oEdit[ E_EDIT ], Asc( " " ), oEdit[ E_INSERT ] )
      NEXT

   CASE nKey == K_INS
      oEdit[ E_INSERT ] := ! oEdit[ E_INSERT ]
      Set( _SET_INSERT, oEdit[ E_INSERT ] )
      SetCursor( iif( oEdit[ E_INSERT ], SC_NORMAL, SC_SPECIAL1 ) )
#if 0
      SayInsert()
#endif

   ENDCASE

   RETURN

//
// 04-06-93 02:06am
//

STATIC FUNCTION EditorMove( pEdit, nKey )

   LOCAL lMoved := .T.

   DO CASE
   CASE nKey == K_PGDN       ; ed_PgDown( pEdit )
   CASE nKey == K_PGUP       ; ed_PgUp( pEdit )
   CASE nKey == K_CTRL_PGUP  ; ed_Top( pEdit )
   CASE nKey == K_CTRL_PGDN  ; ed_Bottom( pEdit )
   CASE nKey == K_RIGHT      ; ed_Right( pEdit )
   CASE nKey == K_LEFT       ; ed_Left( pEdit )
   CASE nKey == K_HOME       ; ed_Home( pEdit )
   CASE nKey == K_CTRL_HOME  ; ed_Home( pEdit )
   CASE nKey == K_END        ; ed_End( pEdit )
   CASE nKey == K_CTRL_END   ; ed_End( pEdit )
   CASE nKey == K_CTRL_RIGHT // ; ed_NWord( pEdit )        //there are some problems with it
   CASE nKey == K_CTRL_LEFT  ; ed_PWord( pEdit )
   OTHERWISE                 ; lMoved := .F.
   ENDCASE

   RETURN lMoved

//
// 03-06-93 10:23pm
//

STATIC FUNCTION EditorSave( oEdit )

   LOCAL nHandle, cFile

   cFile := EditorCargo( oEdit )
   IF Empty( cFile )
      cFile := "testfile.txt"     // GetFileName( 10, 10 )
   ENDIF

   IF Empty( cFile )
      RETURN .F.
   ENDIF

   nHandle := FCreate( cFile, FC_NORMAL )
   IF nHandle > 0
      FWrite( nHandle, EditorGetText( oEdit ) )

      FClose( nHandle )
   ENDIF

   RETURN nHandle > 0

//
// 09/29/91 08:40pm
//

FUNCTION SaveBox( top, left, bott, right, kolor, patt )

   LOCAL cBox, cClr, nBottom, nRight

   IF PCount() > 4
      cClr    := SetColor( kolor )
      cBox    := SaveScreen( top, left, bott, right )
      hb_DispBox( top, left, bott, right, patt )
   ELSE
      cClr    := SetColor()
      cBox    := SaveScreen( top, left, bott, right )
      nBottom := bott
      nRight  := right
   ENDIF

   RETURN { top, left, nBottom, nRight, cBox, cClr }

//
// 09/29/91 08:42pm
//

PROCEDURE RestBox( oBox )

   RestScreen( oBox[ 1 ], oBox[ 2 ], oBox[ 3 ], oBox[ 4 ], oBox[ 5 ] )
   SetColor( oBox[ 6 ] )

   RETURN

STATIC FUNCTION FileLength( nH )

   LOCAL nPos := FSeek( nH, 0, FS_RELATIVE )
   LOCAL nLen := FSeek( nH, 0, FS_END )

   FSeek( nH, nPos, FS_SET )

   RETURN nLen
