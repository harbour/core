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

#include "hbqtgui.ch"
#include "hbtrace.ch"
#include "common.ch"

STATIC oSize
STATIC nCX
STATIC nCY
STATIC aData
STATIC oColorC
STATIC oColorN
STATIC oColorD
STATIC oColorLY
STATIC oColorLN

PROCEDURE Main()
   LOCAL tb1, mo1, lay1, lay2, bt1, bt2, bt3, hd1, i
   LOCAL oWnd, oDA, aStruct
   LOCAL oID, oSM

   hbqt_errorsys()

   SET DATE ANSI
   SET CENTURY ON

   oColorN := QColor( 100,   0,100 )
   oColorD := QColor( 150, 100,  0 )
   oColorLY:= QColor(   0, 150,  0 )
   oColorLN:= QColor( 200,   0,  0 )

   aStruct := {}
   AAdd( aStruct, { "Name"   , "C", 8, 0 } )
   AAdd( aStruct, { "DOB"    , "D", 8, 0 } )
   AAdd( aStruct, { "PAY"    , "N", 5, 0 } )
   AAdd( aStruct, { "MARRIED", "L", 1, 0 } )

   aData := {}
   AAdd( aData, { "Andy"  , SToD( "20120525" ), 200, .T. } )
   AAdd( aData, { "Tomy"  , SToD( "20120617" ), 300, .F. } )
   AAdd( aData, { "Zindal", SToD( "20121213" ), 100, .F. } )

   oWnd := QMainWindow()
   oWnd:resize(640,460 )

   oDA := QWidget()
   oWnd:setCentralWidget( oDA )
   lay1 := QVBoxLayout( oDA )

   nCX := 0
   nCY := 0
   tb1 := QTableView()

   mo1 := HBQAbstractItemModel( {| t, r, x, y| my_browse( aStruct, t, r, x, y ) } )
   tb1:setModel( mo1 )

   oID := tb1:itemDelegate()
   oID:connect( "commitData(QWidget*)", {| w | my_save( w, aStruct, @nCX, @nCY ) } )

   oSM := tb1:selectionModel()
   oSM:connect( "currentChanged(QModelIndex,QModelIndex)", {| n | my_select( n, @nCX, @nCY ) } )

   hd1 := tb1:horizontalHeader()
   FOR i := 1 To Len( aStruct )
      hd1:resizeSection( i - 1, aStruct[ i, 3 ] * 6 + 60 )
   NEXT
   tb1:verticalHeader():setDefaultSectionSize( 24 )

   oSize := QSize(50,24)

   lay1:addWidget( tb1 )

   lay2 := QHBoxLayout()
   lay1:addlayout( lay2 )

   ( bt1 := QPushButton() ):SetText( "Add Row" )
   bt1:connect( "clicked()", {|| my_addRow( mo1, tb1 ) } )
   ( bt2 := QPushButton() ):SetText( "Ins Row" )
   bt2:connect( "clicked()", {|| my_insRow( mo1, tb1 ) } )
   ( bt3 := QPushButton() ):SetText( "Del Row" )
   bt3:connect( "clicked()", {|| my_delRow( mo1, tb1 ) } )

   lay2:addWidget( bt1 )
   lay2:addWidget( bt2 )
   lay2:addStretch()
   lay2:addWidget( bt3 )

   oWnd:Show()
   QApplication():exec()

   RETURN

STATIC PROCEDURE my_save( qWidget, aStru, nCX, nCY )
   LOCAL cData := qWidget:property( "text" ):toString()
   LOCAL nRow := nCY + 1
   LOCAL nCol := nCX + 1

   SWITCH aStru[ nCol, 2 ]
   CASE "C"
      aData[ nRow, nCol ] := AllTrim( cData )
      EXIT
   CASE "N"
      aData[ nRow, nCol ] := Val( cData )
      EXIT
   CASE "L"
      aData[ nRow, nCol ] := Left( cData, 1 ) $ "YyTt"
      EXIT
   CASE "D"
      aData[ nRow, nCol ] := CToD( cData )
      EXIT
   ENDSWITCH
   RETURN

STATIC PROCEDURE my_select( qModelIndex, nCX, nCY  )

   nCX := qModelIndex:column()
   nCY := qModelIndex:row()

   RETURN

STATIC FUNCTION my_browse( aStru, t, role, nX, nY )
   LOCAL nRow := nY + 1   /* Harbour array is 1 based */
   LOCAL nCol := nX + 1

   SWITCH t
   CASE HBQT_QAIM_flags
      RETURN Qt_ItemIsEnabled + Qt_ItemIsSelectable + Qt_ItemIsEditable;

   CASE HBQT_QAIM_data

      SWITCH role
      CASE Qt_DisplayRole
         SWITCH aStru[ nCol, 2 ]
         CASE "C"
            RETURN AllTrim( aData[ nRow, nCol ] )
         CASE "N"
            RETURN hb_ntos( aData[ nRow, nCol ] )
         CASE "L"
            RETURN iif( aData[ nRow, nCol ], "Yes", "No" )
         CASE "D"
            RETURN DToC( aData[ nRow, nCol ] )
         ENDSWITCH
         RETURN "?"

      CASE Qt_EditRole /* Here we can specify different formats for editing*/
         SWITCH aStru[ nCol, 2 ]
         CASE "C"
            RETURN AllTrim( aData[ nRow, nCol ] )
         CASE "N"
            RETURN hb_ntos( aData[ nRow, nCol ] )
         CASE "L"
            RETURN iif( aData[ nRow, nCol ], "Y", "N" )
         CASE "D"
            RETURN DToC( aData[ nRow, nCol ] )
         ENDSWITCH
         RETURN "?"

      CASE Qt_ForegroundRole
         SWITCH aStru[ nCol, 2 ]
         CASE "N"
            RETURN oColorN
         CASE "L"
            RETURN iif( aData[ nRow, nCol ], oColorLY, oColorLN )
         CASE "D"
            RETURN oColorD
         ENDSWITCH
         RETURN NIL

      CASE Qt_BackgroundRole
         RETURN NIL

      CASE Qt_TextAlignmentRole
         SWITCH aStru[ nCol, 2 ]
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
         IF nX == Qt_Horizontal
            RETURN aStru[ nRow, 1 ]
         ELSE
            RETURN hb_NToS( nRow )
         ENDIF

      CASE Qt_TextAlignmentRole
         IF nX == Qt_Horizontal
            RETURN Qt_AlignCenter
         ELSE
            RETURN Qt_AlignVCenter + Qt_AlignRight
         ENDIF

      CASE Qt_SizeHintRole
         RETURN oSize
      ENDSWITCH
      RETURN NIL

   CASE HBQT_QAIM_rowCount
   RETURN Len( aData )

   CASE HBQT_QAIM_columnCount
      RETURN Len( aStru )
   ENDSWITCH

   RETURN NIL

STATIC FUNCTION connect( tb1, aStruct, nCX1, nCY1 )
   LOCAL oID, oSM

   oID := tb1:itemDelegate()
   oID:connect( "commitData(QWidget*)", {| w | my_save( w, aStruct, @nCX1, @nCY1 ) } )

   oSM := tb1:selectionModel()
   oSM:connect( "currentChanged(QModelIndex,QModelIndex)", {| n | my_select( n, @nCX1, @nCY1 ) } )

   RETURN NIL

STATIC FUNCTION my_addRow( oHBQAbsModel, oTableView )
   STATIC nPay := 400

   nPay += 30

   AAdd( aData, { "Kitty", SToD( "20120625" ), nPay, .T. } )

   oHBQAbsModel:reset()   /* Re-populate the model */
   oTableView:selectRow( nCY )

   RETURN .T.

STATIC FUNCTION my_insRow( oHBQAbsModel, oTableView )
   STATIC nPay := 500

   nPay += 150
   aData := hb_AIns( aData, nCY + 1, { "Lovely", SToD( "20010513" ), nPay, .T. }, .T. )

   oHBQAbsModel:reset()
   oTableView:selectRow( nCY )

   RETURN .T.

STATIC FUNCTION my_delRow( oHBQAbsModel, oTableView )

   IF Len( aData ) > 1
      aData := hb_ADel( aData, nCY + 1, .T. )

      oHBQAbsModel:reset()
      oTableView:selectRow( Min( nCY, Len( aData ) - 1 ) )
   ENDIF

   RETURN .T.
