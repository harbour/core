/*
 * $Id$
*/
//#include "expand.ch"
//#include "windows.ch"
//#include "print.ch"
//#include "helpsys.ch"

#include <inkey.ch>
#include <setcurs.ch>
#include <fileio.ch>
#include <box.ch>


#define EXPORT
#define IFANY( x ) IF( (x) != NIL )
#define IFNIL( x ) IF( (x) == NIL )
#define IIFNIL( isnil, notnil ) IIF(notnil==NIL, isnil, notnil)

#define EDIT_LOWER      0       // convert to lowercase
#define EDIT_UPPER      1       // convert to uppercase
#define EDIT_SAME       2       // no convertion

#define EDIT_HARD       13      // hard cariage
#define EDIT_SOFT       141     // soft cariage

#define EDIT_EDIT       .T.     // full edit mode
#define EDIT_VIEW       .F.     // view only mode

//  The editor structure
//
#define E_EDIT          1           // index returned be ED_NEW
#define E_TOP           2           // position on the screen
#define E_LEFT          3
#define E_BOTTOM        4
#define E_RIGHT         5
#define E_TITLE         6           // title
#define E_COLOR         7           // used colors set
#define E_FRAME         8           // frame around the editor
#define E_LINELEN       9           // maximal line length
#define E_MODE          10          // editor mode (edit/view)
#define E_CARGO         11          // cargo slot
#define E_STRUCT_LEN    11


STATIC nESize:=4096         // default buffer size
STATIC oEditor              // static variable used to speed access to the editor
STATIC aEdit:={ }           // the stack of used editors
STATIC lInsert    //current Insert state

//
**
//

//---------------------------------------------------------
//03-06-93 07:52pm
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
EXPORT FUNCTION EditorNew( nTop, nLeft, nBottom, nRight, nLength, ;
                           cFrame, cTitle, cColor, nSize, nEscape )
LOCAL nEdit, oEdit

    IF( nLength == NIL )
        nLength =80
    ENDIF
    nEdit =ED_New( nLength, 4, IIFNIL(nESize, nSize), nEscape )
    IF( nEdit >= 0 )
        oEdit :=ARRAY( E_STRUCT_LEN )
        oEdit[E_EDIT]   =nEdit

        oEdit[E_TOP]     =nTop
        oEdit[E_LEFT]    =nLeft
        oEdit[E_BOTTOM]  =nBottom
        oEdit[E_RIGHT]   =nRight
        oEdit[E_LINELEN] =nLength
        oEdit[E_FRAME]   =IIFNIL( B_DOUBLE, cFrame )
        oEdit[E_TITLE]   =cTitle
        oEdit[E_COLOR]   =IIFNIL( "W/N,W+/N,W+/R,GR+/N,G+/N", cColor )
        oEdit[E_MODE]    =EDIT_VIEW

        ED_Config( nEdit, nTop, nLeft, nBottom, nRight, 0, 0 )
    ENDIF

RETURN( oEdit )

//---------------------------------------------------------
//03-06-93 09:16pm
//
EXPORT PROCEDURE EditorKill( oEdit )

    ED_Kill( oEdit[E_EDIT] )
    MEMORY( -1 )

RETURN

//---------------------------------------------------------
//96-02-29 22:22
//
EXPORT PROCEDURE EditorUnlock( oEdit )

    ED_Unlock( oEdit[E_EDIT] )

RETURN

//---------------------------------------------------------
//03-06-93 10:20pm
//
EXPORT FUNCTION EditorCargo( oEdit, xCargo )
LOCAL _xCargo:=oEdit[E_CARGO]

    IFANY( xCargo )
        oEdit[E_CARGO] =xCargo
    ENDIF

RETURN( _xCargo )

//---------------------------------------------------------
//19-07-93 01:08am
//
EXPORT FUNCTION EditorTitle( oEdit, cTitle )
LOCAL _cTitle:=oEdit[E_TITLE]

    IFANY( cTitle )
        oEdit[E_TITLE] =cTitle
    ENDIF

RETURN( _cTitle )

//---------------------------------------------------------
//04-06-93 02:18am
//
// Sets 
// EDIT_EDIT - full edit mode
// EDIT_VIEW - view only mode (no changes in text are allowed)
//
EXPORT FUNCTION EditorMode( oEdit, lMode )
LOCAL _lMode:=oEdit[E_MODE]

    IFANY( lMode )
        oEdit[E_MODE] =lMode
    ENDIF

RETURN( _lMode )

//---------------------------------------------------------
//28-05-92 09:31am
//
EXPORT FUNCTION EditorSize( nSize )
LOCAL _nSize:=nESize

    IF nSize != NIL
        nESize =nSize
    ENDIF

RETURN( _nSize )

//---------------------------------------------------------
//28-02-92 10:57pm
//
// Appends passed text to the text already stored in editor
//
EXPORT PROCEDURE EditorAddText( oEdit, cText )

    ED_AddText( oEdit[E_EDIT], cText )

RETURN

//---------------------------------------------------------
//05-03-92 10:21pm
//
// Sets new text in editor
//
EXPORT PROCEDURE EditorSetText( oEdit, cText )

    ED_SetText( oEdit[E_EDIT], cText )

RETURN

//---------------------------------------------------------
//05-03-92 10:23pm
//
// Inserts passed text into editor starting from passed line number
//
EXPORT PROCEDURE EditorInsText( oEdit, cText, nLine )
LOCAL nNum:=IIFNIL( ED_LCount(oEdit[E_EDIT]), nLine )

    ED_InsText( oEdit[E_EDIT], cText, nNum )

RETURN

//---------------------------------------------------------
//02-03-92 07:53pm
//
// Retrieves the text from editor
// nUpper - specifies if text should be changed to uppercase, lowercase
//    or unchanged
// nCarret - specifies if soft carriage return (141/10) should be replaced by 
//    hard carriage returns (13/10)
//
EXPORT FUNCTION EditorGetText( oEdit, nUpper, nCarret )

    IFNIL( nUpper )
        nUpper =EDIT_SAME
    ENDIF
    IFNIL( nCarret )
        nCarret =EDIT_HARD
    ENDIF

RETURN( ED_GetText( oEdit[E_EDIT], nUpper, nCarret ) )

//---------------------------------------------------------
//04-03-92 02:35pm
//
// Returns the line count stored in editor
//
EXPORT FUNCTION EditorLCount( oEdit )

RETURN( ED_LCount( oEdit[E_EDIT] ) )

//---------------------------------------------------------
//06-03-92 07:09pm
//
// Returns the specified line of text from the editor
//
EXPORT FUNCTION EditorGetLine( oEdit, nLine )

RETURN( ED_GetLine( oEdit[E_EDIT], nLine ) )

//---------------------------------------------------------
//06-03-92 07:10pm
//
// Returns the next line of text
//
// It can be used:
// nLCount :=EditorLCount( oEdit )
// cLine :=EditorGetLine( oEdit, 1 )
// FOR i:=2 TO nLCount
//   cLine :=EditorNextLine( oEdit )
// NEXT
//
EXPORT FUNCTION EditorNextLine( oEdit )

RETURN( ED_GetNext(oEdit[E_EDIT]) )

//---------------------------------------------------------
//03-06-93 10:11pm
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
EXPORT FUNCTION EditorFile( xInput, cOutput, nLineLen, nHelp, ;
                            lPrint, lConv, nEscape, lSave )
LOCAL nHandle, nLen, oEdit, bF2, bF8, oHelp, lSaved:=.F., lClose:=.F.
LOCAL nSize

    IF( lSave == NIL )
        lSave =.T.
    ENDIF
    IF( VALTYPE(xInput) == 'C' )
        nHandle =FOPEN( xInput )
        lClose =.T.
    ELSE
        nHandle =xInput
    ENDIF

    IF( nHandle > 0 )
        nLen =MAX( FileLength( nHandle ), nESize )
    ELSE
        nLen =nESize
    ENDIF

    nSize =IIF( nLen < 8192, nLen*2, INT(nLen*1.5) )
    oEdit =EditorNew( 01,00,23,79, nLineLen, '---      ', cOutput, , ;
                      nSize, nEscape )

    IF( nHandle > 0 )
        ED_ReadText( oEdit[E_EDIT], nHandle, 0, nLen, ;
                     IIF( lConv==NIL, .F., lConv ) )
        IF( lClose )
            FCLOSE( nHandle )
        ENDIF
    ELSE
        EditorSetText( oEdit, ' ' )
    ENDIF

    EditorCargo( oEdit, cOutput )

//    SAVELINE 24 TO oHelp WITH 80, ColorHelp( ,COLOR_EXTEND )
//    IF( lSave )
//        DisplayHelp( 73 )     //F2-save
//        bF2 =SETKEY( K_F2, {|oE| lSaved:=EditorSave(oE)} )
//    ENDIF
//    IF( lPrint != NIL .AND. lPrint )
//        DisplayHelp( 74 )      //F8-print
//        bF8 =SETKEY( K_F8, {|oE| EditorPrint(oE)} )
//    ENDIF

    lSaved :=EditorEdit( oEdit, EDIT_EDIT, .F., nHelp )
    EditorKill( oEdit )

    IF( lSave )
        SETKEY( K_F2, bF2 )
    ENDIF

//    IF( lPrint != NIL .AND. lPrint )
//        SETKEY( K_F8, bF8 )
//    ENDIF
//    RESTLINE FROM oHelp

//    FT_IDLE()
    MEMORY(-1)

RETURN( lSaved )

//---------------------------------------------------------
//06-07-93 06:05pm
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
EXPORT FUNCTION EditorRead( oEditor, nHandle, nOffset, nLen, lConv )

RETURN( ED_ReadText( oEditor[E_EDIT], nHandle, nOffset, nLen, ;
                     IIF( lConv==NIL, .T., lConv ) ) )

//---------------------------------------------------------
//03-06-93 08:31pm
//
// Start the editor
//
// oEdit - the editor object
// lEdit - .T. = edit allowed, .F. = view only mode
// lFrame - specifies if the frame around the editor should be displayed
// nHelp - the help index into help subsystem
//
EXPORT FUNCTION EditorEdit( oEdit, lEdit, lFrame, nHelp )
LOCAL nRow:=0, nCol:=0, nKey, cLine, bKey, oBox, nCursor, nState
LOCAL nTop, nLeft, nBottom, nRight
LOCAL lSaveAllowed, lSaved:=.F.
//HELPSAVE( IIF(nHelp==NIL, H_EDITOR, nHelp ) )

    oBox =SAVEBOX( oEdit[E_TOP], oEdit[E_LEFT], ;
                   oEdit[E_BOTTOM], oEdit[E_RIGHT], ;
                   oEdit[E_COLOR], oEdit[E_FRAME], oEdit[E_TITLE], .F. )

    lInsert =SET( _SET_INSERT )
//    SayInsert()
    nCursor =SetCursor( IIF(lInsert, SC_NORMAL, SC_SPECIAL1) )
    IFANY( lEdit )
        oEdit[E_MODE] =lEdit
    ENDIF
    lSaveAllowed :=( SETKEY(K_F2) == NIL )
//    IF( lSaveAllowed )
//        DisplayHelp( 73 )     //F2-save
//    ENDIF

    nTop    =oEdit[E_TOP] +1
    nLeft   =oEdit[E_LEFT] +1
    nBottom =oEdit[E_BOTTOM] -1
    nRight  =oEdit[E_RIGHT] -1
    IF( lFrame != NIL .AND. !lFrame )
        nLeft--
        nBottom++
        nRight++
    ENDIF
    nState =oEdit[E_RIGHT] -8

    /* It uses static variable 'oEditor' to speed access to the editor
       If this function is called recursively then we have to push 
       currently used editor on the stack
    */
    EditorPush( oEdit )

    /* The position of the editor can be changed (in a windowed environment)
       then it sets current position of editor. 
       It also sets the current editor as the working one. This means that 
       all next ED_* functions will used the editor handle specified
       by oEditor[E_EDIT] - it is tricky solution to speed access (we
       don't need to pass the editor handle with every ED_*() call
       (Well... this editor was created when AT-286 computers worked in
       its full glory :)
    */
    ED_Config( oEditor[E_EDIT], nTop, nLeft, nBottom, nRight, 0, 0 )

    DO WHILE .T.
        nRow =ED_Stabilize()    //displays all visible lines
        // It don't uses incremantal stabilization for performance reasons

        IF( nRow != ED_Row() )
            nRow =ED_Row()
            @ oEditor[E_TOP], nState SAY STRZERO( nRow,4 )
        ENDIF
        IF( nCol != ED_Col() )
            nCol =ED_Col()
            @ oEditor[E_TOP], nState+5 SAY STRZERO( nCol,3 )
        ENDIF
        SETPOS( nTop+ED_WinRow(), nLeft+ED_WinCol() )

//        nKey =WaitForKey()
        nKey =INKEY(0)

        DO CASE
        CASE( nKey>=32 .AND. nKey<256 )
            IF( oEdit[E_MODE] )
                ED_PutChar( nKey, lInsert )
            ENDIF

        CASE( nKey == K_F2 .AND. lSaveAllowed )
            lSaved :=EditorSave( oEditor )     //save the copy of edited buffer

        CASE( EditorMove( nKey ) )

        CASE( nKey == K_DOWN )
            IF !ED_Down()
                SCROLL( nTop, nLeft, nBottom, nRight, 1 )
            ENDIF

        CASE( nKey == K_UP )
            IF !ED_Up()
                SCROLL( nTop, nLeft, nBottom, nRight, -1 )
            ENDIF

        CASE( nKey == K_ESC )
            EXIT

        OTHERWISE
            bKey =SETKEY( nKey )
            IFNIL( bKey )
                IF( oEdit[E_MODE] )
                    EditorKeys( nKey )
                ENDIF
            ELSE
                EVAL( bKey, oEditor )
            ENDIF
        ENDCASE
    ENDDO

    EditorPop()      //restore the proviously used editor

    SetCursor( nCursor )
    RESTBOX( oBox )
//    HELPREST.

//    FT_IDLE()
    MEMORY(-1)

RETURN( lSaved )


//
**
//

//---------------------------------------------------------
//03-06-93 08:35pm
//
STATIC PROCEDURE EditorKeys( nKey )
LOCAL i

    DO CASE
    CASE( nKey == K_CTRL_Y )
        ED_DelLine()

    CASE( nKey == K_CTRL_T )
        ED_DelWord()

    CASE( nKey == K_DEL )
        ED_DelChar()

    CASE( nKey == K_BS )
        ED_BSpace( lInsert )

    CASE( nKey == K_RETURN )
        ED_Return( lInsert )

    CASE( nKey == K_TAB )
//        ED_Tab( lInsert )
        FOR i=1 TO 4
            ED_PutChar( 32, lInsert )
        NEXT

    CASE( nKey == K_INS )
        lInsert = !lInsert
        SET( _SET_INSERT, lInsert )
        SetCursor( IIF(lInsert, SC_NORMAL, SC_SPECIAL1) )
//        SayInsert()

    ENDCASE

RETURN

//---------------------------------------------------------
//04-06-93 02:06am
//
STATIC FUNCTION EditorMove( nKey )
LOCAL lMoved:=.T.

    DO CASE
    CASE( nKey == K_PGDN )
        ED_PgDown()

    CASE( nKey == K_PGUP )
        ED_PgUp()

    CASE( nKey == K_CTRL_PGUP )
        ED_Top()

    CASE( nKey == K_CTRL_PGDN )
        ED_Bottom()

    CASE( nKey == K_RIGHT )
        ED_Right()

    CASE( nKey == K_LEFT )
        ED_Left()

    CASE( nKey == K_HOME )
        ED_Home()

    CASE( nKey == K_CTRL_HOME )
        ED_Home()

    CASE( nKey == K_END )
        ED_End()

    CASE( nKey == K_CTRL_END )
        ED_End()

    CASE( nKey == K_CTRL_RIGHT )
//        ED_NWord()        //there are some problems with it

    CASE( nKey == K_CTRL_LEFT )
        ED_PWord()

    OTHERWISE
        lMoved =.F.

    ENDCASE

RETURN( lMoved )


//---------------------------------------------------------
//03-06-93 09:11pm
//
STATIC PROCEDURE EditorPush( oEdit )

    ED_Push()
    AADD( aEdit, oEditor )
    oEditor =oEdit

RETURN

//---------------------------------------------------------
//03-06-93 09:12pm
//
STATIC PROCEDURE EditorPop( )
LOCAL nLen

    nLen =LEN( aEdit )
    oEditor =aEdit[ nLen ]
    ASIZE( aEdit, nLen-1 )
    ED_Pop()

RETURN

//---------------------------------------------------------
//03-06-93 10:23pm
//
STATIC FUNCTION EditorSave( oEdit )
LOCAL nHandle, cFile, cNew

    cFile =EditorCargo(oEdit)
    IF( EMPTY(cFile) )
        cFile ='TESTFILE.TXT'     //GetFileName( 10, 10 )
    ENDIF

    IF( EMPTY(cFile) )
        RETURN( .F. )
    ENDIF
/*
    WorkStart( 75 )
    IF( FILE(cFile) )
        cNew =FileExtension( cFile, 'BAK' )
        DELETEFILE( cNew )
        nHandle =RENAMEFILE( cFile, cNew )
        IF( nHandle < 0 )
            FileError( cFile, -nHandle )
            WorkEnd()

            RETURN( .F. )
        ENDIF
    ENDIF
*/
    nHandle =FCREATE( cFile, FC_NORMAL )
    IF( nHandle > 0 )
        FWRITE( nHandle, EditorGetText(oEdit) )

        FCLOSE( nHandle )
//    ELSE
//        FileError( cFile, FERROR() )
    ENDIF
//    WorkEnd()

RETURN( nHandle > 0 )

//---------------------------------------------------------
//19-08-93 02:05am
//
/*
STATIC PROCEDURE EditorPrint( oEdit )
LOCAL oParm

    IF PrintReady()
        PrintNewQue()
        PrintSetHead( AppsOwner(), , '-', PRN_OFF )

        PrintAddJob( oEdit, PRN_EDITOR )
        PrintJob(PRN_OFF)
        PrintDelQue()
    ENDIF

RETURN
*/

//---------------------------------------------------------
*09/29/91 08:40pm
*
EXPORT FUNCTION SaveBox( top,left,bott,right, kolor, patt, head, shadow )
LOCAL cBox, cClr, nBottom,nRight

    IF( PCOUNT() > 4 )
        cClr    =SETCOLOR(kolor)
        cBox    =SAVESCREEN(top,left,bott,right)
//        cBox    =BoxShadow(top,left,bott,right, , patt, head, shadow)
        @ top, left, bott, right BOX patt
//        nBottom =bott+1
//        nRight  =right+1
    ELSE
        cClr    =SETCOLOR()
        cBox    =SAVESCREEN(top,left,bott,right)
        nBottom =bott
        nRight  =right
    ENDIF

RETURN( {top,left,nBottom,nRight, cBox, cClr} )


//---------------------------------------------------------
*09/29/91 08:42pm
*
EXPORT PROCEDURE RestBox( oBox )

    RESTSCREEN( oBox[1], oBox[2], oBox[3], oBox[4], oBox[5] )
    SETCOLOR( oBox[6] )

RETURN

STATIC FUNCTION FileLength( nH )
LOCAL nPos:=FSEEK( nH, 0, FS_RELATIVE )
LOCAL nLen:=FSEEK( nH, 0, FS_END )

  FSEEK( nH, nPos, FS_SET )

RETURN( nLen )
