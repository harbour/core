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


PROCEDURE ExecOneMore()
   hb_gtReload( 'GUI' )
   BuildADialog()
   RETURN


STATIC PROCEDURE BuildADialog()
   LOCAL tb1, mo1, lay1, lay2, bt1, bt2, bt3, hd1, i, lExit
   LOCAL oWnd
   LOCAL oDA
   LOCAL aStru1
   LOCAL nCX1
   LOCAL nCY1
   LOCAL oEventLoop

   SET DATE ANSI
   SET CENTURY ON

   oWnd := QMainWindow()
   oWnd:resize(640,460 )

   oDA := QWidget()
   oWnd:setCentralWidget( oDA )
   lay1 := QVBoxLayout( oDA )

   DBUseArea( .T., NIL, "../../../tests/test.dbf", "T1", .F., .F. )
   aStru1 := DBStruct()
   nCX1 := 0
   nCY1 := 0
   tb1 := QTableView()
   mo1 := HBQAbstractItemModel( {| t, r, x, y| my_browse( 1, aStru1, t, r, x, y ) } )
   tb1:setModel( mo1 )

   tb1:itemDelegate():connect( "commitData(QWidget*)", {| w | my_save( w, 1, aStru1, @nCX1, @nCY1 ) } )
   tb1:selectionModel():connect( "currentChanged(QModelIndex,QModelIndex)", {| n | my_select( n, @nCX1, @nCY1 ) } )

   hd1 := tb1:horizontalHeader()
   FOR i := 1 To Len( aStru1 )
      hd1:resizeSection( i - 1, aStru1[ i, 3 ] * 6 + 60 )
   NEXT
   tb1:verticalHeader():setDefaultSectionSize( 24 )

   lay1:addWidget( tb1 )

   lay2 := QHBoxLayout()
   lay1:addlayout( lay2 )

   ( bt1 := QPushButton() ):SetText( "Dummy 1" )
   ( bt2 := QPushButton() ):SetText( "Dummy 2" )
   ( bt3 := QPushButton() ):SetText( "Dummy 3" )

   lay2:addWidget( bt1 )
   lay2:addStretch()
   lay2:addWidget( bt2 )
   lay2:addWidget( bt3 )

   oWnd:connect( QEvent_Close, {|| lExit := .t. } )
   oEventLoop := QEventLoop( oWnd )
   oWnd:Show()

   lExit := .f.
   DO WHILE ! lExit 
      oEventLoop:processEvents( QEventLoop_AllEvents )
   ENDDO
   oWnd:disconnect( QEvent_Close )
   oEventLoop:exit( 0 )

   DbCloseAll()

   tb1  := NIL
   lay1 := NIL
   lay2 := NIL
   mo1  := NIL
   bt1  := NIL
   bt2  := NIL
   bt3  := NIL
   hd1  := NIL
   oDA  := NIL
   oWnd := NIL

   oEventLoop := NIL

   RETURN

STATIC PROCEDURE my_save( qWidget, nArea, aStru, nCX, nCY )
   LOCAL cData := qWidget:text()

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

STATIC PROCEDURE my_select( qModelIndex, nCX, nCY  )

   nCX := qModelIndex:column()
   nCY := qModelIndex:row()
   RETURN

STATIC FUNCTION my_browse( nArea, aStru, t, role, x, y )
   THREAD STATIC lInit := .f.
   THREAD STATIC oColorN
   THREAD STATIC oColorD
   THREAD STATIC oColorLY
   THREAD STATIC oColorLN
   THREAD STATIC oSize

   IF ! lInit 
      lInit := .t.
      oColorN  := QColor( 100,   0,100 )
      oColorD  := QColor( 150, 100,  0 )
      oColorLY := QColor(   0, 150,  0 )
      oColorLN := QColor( 200,   0,  0 )

      oSize := QSize(50,24)
   ENDIF 

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
            RETURN IIf( FieldGet( x + 1 ), "Yes", "No" )
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
            RETURN IIf( FieldGet( x + 1 ), "Y", "N" )
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
            RETURN IIf( FieldGet( x + 1 ), oColorLY, oColorLN )
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
