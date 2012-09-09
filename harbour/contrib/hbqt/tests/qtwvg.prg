/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2010 Carlos Bacco <carlosbacco at gmail.com>
 * www - http://harbour-project.org
 *
 */
/*----------------------------------------------------------------------*/

#include "hbqtgui.ch"
#include "hbtrace.ch"
#include "common.ch"
#include "inkey.ch"
#include "wvtwin.ch"
#include "hbgtinfo.ch"
#include "hbgtwvg.ch"
#include "wvgparts.ch"

/*----------------------------------------------------------------------*/

STATIC oSize
STATIC aStru1
STATIC nCX1
STATIC nCY1

STATIC oColorC
STATIC oColorN
STATIC oColorD
STATIC oColorLY
STATIC oColorLN

/*----------------------------------------------------------------------*/

#define IMAGE_VOUCH                hb_dirBase() + "..\..\gtwvg\tests\vouch1.bmp"
#define IMAGE_BROWSE               hb_dirBase() + "..\..\gtwvg\tests\v_browse.ico"
#define IMAGE_VR                   hb_dirBase() + "..\..\gtwvg\tests\vr_1.ico"
#define IMAGE_NOTES                hb_dirBase() + "..\..\gtwvg\tests\v_notes.ico"
#define IMAGE_TOOLS                hb_dirBase() + "..\..\gtwvg\tests\v_tools.ico"
#define IMAGE_HELP                 hb_dirBase() + "..\..\gtwvg\tests\v_notes.ico"

/*----------------------------------------------------------------------*/

PROCEDURE Main()
   LOCAL tb1, mo1, lay1, lay2, bt1, bt2, bt3, hd1, i
   LOCAL oWnd, oDA
   LOCAL oID, oSM

   hbqt_errorsys()

   SET DATE ANSI
   SET CENTURY ON

   oColorN := QColor( 100,   0,100 )
   oColorD := QColor( 150, 100,  0 )
   oColorLY:= QColor(   0, 150,  0 )
   oColorLN:= QColor( 200,   0,  0 )

   oWnd := QMainWindow()
   oWnd:resize(640,460 )

   oDA := QWidget()
   oWnd:setCentralWidget( oDA )
   lay1 := QVBoxLayout( oDA )

   DBUseArea( .T., NIL, "../../../tests/test.dbf", "T1", .T., .F. )
   aStru1 := DBStruct()
   nCX1 := 0
   nCY1 := 0
   tb1 := QTableView()
   mo1 := HBQAbstractItemModel( {| t, r, x, y| my_browse( 1, aStru1, t, r, x, y ) } )
   tb1:setModel( mo1 )

   oID := tb1:itemDelegate()
   oID:connect( "commitData(QWidget*)", {| w | my_save( w, 1, aStru1, @nCX1, @nCY1 ) } )

   oSM := tb1:selectionModel()
   oSM:connect( "currentChanged(QModelIndex,QModelIndex)", {| n | my_select( n, @nCX1, @nCY1 ) } )

   hd1 := tb1:horizontalHeader()
   FOR i := 1 To Len( aStru1 )
      hd1:resizeSection( i - 1, aStru1[ i, 3 ] * 6 + 60 )
   NEXT
   tb1:verticalHeader():setDefaultSectionSize( 24 )

   oSize := QSize(50,24)

   lay1:addWidget( tb1 )

   lay2 := QHBoxLayout()
   lay1:addlayout( lay2 )

   ( bt1 := QPushButton() ):SetText( "WVG Threaded Window" )
   ( bt2 := QPushButton() ):SetText( "Dummy 1" )
   ( bt3 := QPushButton() ):SetText( "Dummy 2" )

   bt1:connect( "clicked()", {|| hb_threadStart( {|| ExecWvgWindow() } ) } )

   lay2:addWidget( bt1 )
   lay2:addStretch()
   lay2:addWidget( bt2 )
   lay2:addWidget( bt3 )

   oWnd:Show()
   QApplication():exec()

   HB_TRACE( HB_TR_DEBUG, ( "my_select "+hb_ntos( nCX1 )+ "/"+hb_ntos( nCY1 ) ) )

   RETURN

/*----------------------------------------------------------------------*/

STATIC PROCEDURE my_save( qWidget, nArea, aStru, nCX, nCY )
   LOCAL cData := qWidget:property( "text" ):toString()

   DBSelectArea( nArea )
   DBGoto( nCY + 1 )

   SWITCH aStru[ nCX + 1, 2 ]
   CASE "C"
      FieldPut( nCX + 1, AllTrim( cData ) )
      EXIT
   CASE "N"
      FieldPut( nCX + 1, Val( cData ) )
      EXIT
   CASE "L"
      FieldPut( nCX + 1, Left( cData, 1 ) $ "YyTt" )
      EXIT
   CASE "D"
      FieldPut( nCX + 1, CToD( cData ) )
      EXIT
   ENDSWITCH
   RETURN

/*----------------------------------------------------------------------*/

STATIC PROCEDURE my_select( qModelIndex, nCX, nCY  )

   nCX := qModelIndex:column()
   nCY := qModelIndex:row()
   HB_TRACE( HB_TR_DEBUG, ( "my_select "+hb_ntos( nCX )+ "/"+hb_ntos( nCY ) ) )

   RETURN

/*----------------------------------------------------------------------*/

STATIC FUNCTION my_browse( nArea, aStru, t, role, x, y )
   DBSelectArea( nArea )

   SWITCH t
   CASE HBQT_QAIM_flags
      RETURN Qt_ItemIsEnabled + Qt_ItemIsSelectable + Qt_ItemIsEditable;

   CASE HBQT_QAIM_data

      SWITCH role
      CASE Qt_DisplayRole
         DBGoto( y + 1 )
         SWITCH aStru[ x + 1, 2 ]
         CASE "C"
            RETURN AllTrim( FieldGet( x + 1 ) )
         CASE "N"
            RETURN hb_NToS( FieldGet( x + 1 ) )
         CASE "L"
            RETURN iif( FieldGet( x + 1 ), "Yes", "No" )
         CASE "D"
            RETURN DToC( FieldGet( x + 1 ) )
         ENDSWITCH
         RETURN "?"

      CASE Qt_EditRole /* Here we can specify different formats for editing*/
         DBGoto( y + 1 )
         SWITCH aStru[ x + 1, 2 ]
         CASE "C"
            RETURN AllTrim( FieldGet( x + 1 ) )
         CASE "N"
            RETURN hb_NToS( FieldGet( x + 1 ) )
         CASE "L"
            RETURN iif( FieldGet( x + 1 ), "Y", "N" )
         CASE "D"
            RETURN DToC( FieldGet( x + 1 ) )
         ENDSWITCH
         RETURN "?"

      CASE Qt_ForegroundRole
         SWITCH aStru[ x + 1, 2 ]
         CASE "N"
            RETURN oColorN
         CASE "L"
            DBGoto( y + 1 )
            RETURN iif( FieldGet( x + 1 ), oColorLY, oColorLN )
         CASE "D"
            RETURN oColorD
         ENDSWITCH
         RETURN NIL

      CASE Qt_BackgroundRole
         RETURN NIL

      CASE Qt_TextAlignmentRole
         SWITCH aStru[ x + 1, 2 ]
         CASE "C"
            RETURN Qt_AlignVCenter + Qt_AlignLeft
         CASE "N"
            RETURN Qt_AlignVCenter + Qt_AlignRight
         ENDSWITCH
         RETURN Qt_AlignCenter
      ENDSWITCH
      RETURN NIL

   CASE HBQT_QAIM_headerData
      SWITCH role
      CASE Qt_DisplayRole
         IF x == Qt_Horizontal
            RETURN aStru[ y + 1, 1 ]
         ELSE
            RETURN hb_NToS( y + 1 )
         ENDIF

      CASE Qt_TextAlignmentRole
         IF x == Qt_Horizontal
            RETURN Qt_AlignCenter
         ELSE
            RETURN Qt_AlignVCenter + Qt_AlignRight
         ENDIF

      CASE Qt_SizeHintRole
         RETURN oSize
      ENDSWITCH
      RETURN NIL

   CASE HBQT_QAIM_rowCount
      RETURN LastRec()

   CASE HBQT_QAIM_columnCount
      RETURN Len( aStru )
   ENDSWITCH

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION ExecWvgWindow()
   LOCAL oCrt
   LOCAL dDate     := stod()
   LOCAL cName     := Pad( "Pritpal Bedi", 35 )
   LOCAL cAdd1     := Pad( "60, New Professor Colony", 35 )
   LOCAL cAdd2     := Pad( "Ludhiana, INDIA", 35 )
   LOCAL cAdd3     := Pad( "http://hbide.vouch.info", 35 )
   LOCAL nSlry     := 20000
   LOCAL nColGet   := 8
   LOCAL GetList   := {}
   LOCAL nTop      := 4
   LOCAL nLft      := 4
   LOCAL nBtm      := 20
   LOCAL nRgt      := 75
   LOCAL cLabel    := "Harbour simulated GUI"

   oCrt := WvgCrt():new( , , { 10,10 }, { 24,79 }, , .t. )
   oCrt:fontHeight := 16
   oCrt:fontWidth  := 10
   oCrt:resizable  := .T.
   oCrt:lModal     := .F.
   oCrt:create()

   SET DATE ANSI
   SET( _SET_EVENTMASK, INKEY_ALL + HB_INKEY_GTEVENT )

   Wvt_SetGui( .t. )
   Wvt_SetMouseMove( .t. )
   Wvt_SetFont( "Courier New", 18, 0, 0 )

   SetColor( "N/W" )
   CLS

   Wvt_ShowWindow( SW_RESTORE )

   SetKey( K_F12        , {|| hb_gtInfo( HB_GTI_ACTIVATESELECTCOPY ) } )
   SetKey( K_CTRL_V     , {|| __KeyBoard( hb_gtInfo( HB_GTI_CLIPBOARDDATA ) ) } )
   SetKey( K_RBUTTONDOWN, {|| __KeyBoard( hb_gtInfo( HB_GTI_CLIPBOARDDATA ) ) } )

   /*  Force mouse pointer right below the Harbour label */
   Wvt_SetMousePos( 2,40 )

   Wvg_SetPaint( "Main", 10001, {|| Wvt_SetIcon( IMAGE_VR )                                                                                           } )
   Wvg_SetPaint( "Main", 10002, {|| Wvt_SetTitle( "Vouch" )                                                                                           } )
   Wvg_SetPaint( "Main", 10003, {|| Wvt_DrawLabel( 1,40, cLabel, 6,, rgb(255,255,255), rgb(198,198,198), "Arial", 26, , , , , .t., .t. )              } )
   Wvg_SetPaint( "Main", 10004, {|| Wvt_DrawBoxRaised( nTop, nLft, nBtm, nRgt )                                                                       } )
   Wvg_SetPaint( "Main", 10005, {|| Wvt_DrawBoxRecessed( 7, 61, 13, 70 )                                                                              } )
   Wvg_SetPaint( "Main", 10006, {|| Wvt_DrawBoxGroup( 15, 59, 18, 72 )                                                                                } )
   Wvg_SetPaint( "Main", 10007, {|| Wvt_DrawBoxGroup( 5, 6, 19, 44 )                                                                                  } )
   Wvg_SetPaint( "Main", 10008, {|| Wvt_DrawImage( 8,62,12,69, IMAGE_VOUCH )                                                                          } )
   Wvg_SetPaint( "Main", 10009, {|| Wvt_DrawBoxRecessed( 7, 48, 13, 55 )                                                                              } )
   Wvg_SetPaint( "Main", 10010, {|| Wvt_DrawLine( maxrow()-2,0,maxrow()-2,maxcol(), WVT_LINE_HORZ, WVT_LINE_RECESSED, WVT_LINE_BOTTOM )               } )
   Wvg_SetPaint( "Main", 10011, {|| Wvt_DrawLine( maxrow()-1, 41, maxrow(), 41, WVT_LINE_VERT, WVT_LINE_RECESSED, WVT_LINE_CENTER )                   } )
   Wvg_SetPaint( "Main", 10012, {|| aEval( GetList, {|oGet| Wvt_DrawBoxGet( oGet:Row, oGet:Col, Len( Transform( oGet:VarGet(), oGet:Picture ) ) ) } ) } )

   WvtSetPaint( Wvg_GetPaint( "Main" ) )

   SetColor( "N/W" )
   CLS
   SetColor( "N/W,N/GR*,,,N/W*" )

   @  6, nColGet SAY "< Date >"
   @  9, nColGet SAY "<" + PadC( "Name", 33 ) + ">"
   @ 12, nColGet SAY "<" + PadC( "Address", 33 ) + ">"
   @ 16, 61      SAY "< Salary >"

   dDate := stod( "20040401" )

   @  7, nColGet GET dDate WHEN  DispStatusMsg( "Date must be valid"       ) VALID ClearStatusMsg()
   @ 10, nColGet GET cName WHEN  DispStatusMsg( "Must be one of the list!" ) VALID ( VouChoice() < 7 .AND. ClearStatusMsg() )
   @ 13, nColGet GET cAdd1
   @ 15, nColGet GET cAdd2
   @ 17, nColGet GET cAdd3
   @ 17, 61      GET nSlry PICTURE "@Z 9999999.99"

   READ

   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION Wvt_Paint()

   WvtPaintObjects()   /* The sole purpose of this FUNCTION is TO call this METHOD */

   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION VouChoice( aChoices )
   LOCAL scr, clr, nChoice

   DEFAULT aChoices TO { "One","Two","Three","Four","Five","Six","Seven" }

   scr := SaveScreen( 7,48,13,55 )
   clr := SetColor( "N/W*,GR+/B*,,,GR+/B" )

   nChoice := aChoice( 7, 48, 13, 55, aChoices )

   setColor( clr )
   RestScreen( 7, 48, 13, 55, scr )

   RETURN nChoice

/*----------------------------------------------------------------------*/

FUNCTION DispStatusMsg( cMsg )

   Wvt_DrawLabel( MaxRow(), 60, cMsg, 6, , 0, rgb( 198,198,198 ), "Arial", 18, , 900 )

   RETURN .t.

/*----------------------------------------------------------------------*/

FUNCTION ClearStatusMsg()
   LOCAL nRow := Row()
   LOCAL nCol := Col()

   DispOutAt( MaxRow(), 42, space( 37 ), "W/W" )

   SetPos( nRow, nCol )

   RETURN .t.

/*----------------------------------------------------------------------*/

FUNCTION rgb( r,g,b )
   RETURN r + ( g * 256 ) + ( b * 256 * 256 )

/*----------------------------------------------------------------------*/
