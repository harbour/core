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

/*
 * NOTES:
 *
 * This is only a basic browse test. Production code must check
 * parameters, test if database really opened, and so on.
 *
 * Also, we are using absolute sizes. Proper Qt coding rely on
 * font metrics obtained by QFont and similar techniques. Modern
 * interfaces assume that font size and system colors is user
 * choice.
 *
 * For production code is advised that the user creates a basic
 * data caching system, to avoid unnecessary disk access while
 * browsing large data sets.
 *
 */

#include "hbqt.ch"
#include "common.ch"

STATIC qApp
STATIC oWnd

STATIC oDA
STATIC oSize
STATIC aStru1

STATIC oColorC
STATIC oColorN
STATIC oColorD
STATIC oColorLY
STATIC oColorLN

REQUEST HB_QT

INIT PROCEDURE Qt_Start()
   qApp := QApplication():new()
   RETURN

EXIT PROCEDURE Qt_End()
   qApp:quit()
   RETURN

PROCEDURE Main()
   LOCAL tb1, mo1, lay1, lay2, bt1, bt2, bt3, hd1, i

   SET DATE ANSI
   SET CENTURY ON

   oColorN := QColor():New( 100,   0,100 )
   oColorD := QColor():New( 150, 100,  0 )
   oColorLY:= QColor():New(   0, 150,  0 )
   oColorLN:= QColor():New( 200,   0,  0 )

   oWnd := QMainWindow():new()
   oWnd:resize(640,460 )

   oDA := QWidget():new()
   oWnd:setCentralWidget( oDA )
   lay1 := QVBoxLayout():new( oDA )

   DBUseArea( .T., NIL, '../../../tests/test.dbf','T1', .F., .T.)
   aStru1 := DBStruct()
   tb1 := QTableView():new()
   mo1 := HBQAbstractItemModel():New( {| t, r, x, y| my_browse( 1, aStru1, t, r, x, y ) } )
   tb1:setModel( mo1 )

   hd1 := QHeaderView():from(tb1:horizontalHeader())
   FOR i := 1 To Len( aStru1 )
      hd1:resizeSection( i-1, aStru1[ i ,3 ] * 6 + 60 )
   NEXT
   QHeaderView():from( tb1:verticalHeader() ):setDefaultSectionSize( 24 )

   oSize := QSize():new(50,24)

   lay1:addWidget( tb1 )

   lay2 := QHBoxLayout():new()
   lay1:addlayout( lay2 )

   ( bt1 := QPushButton():new() ):SetText( "Dummy 1" )
   ( bt2 := QPushButton():new() ):SetText( "Dummy 2" )
   ( bt3 := QPushButton():new() ):SetText( "Dummy 3" )

   lay2:addWidget( bt1 )
   lay2:addStretch()
   lay2:addWidget( bt2 )
   lay2:addWidget( bt3 )

   oWnd:Show()
   qApp:exec()

   RETURN

STATIC FUNCTION my_browse( nArea, aStru, t, role, x, y )
	DBSelectArea( nArea )

   SWITCH t
   CASE HBQT_QAIM_data

      SWITCH role
      CASE Qt_DisplayRole
         DBGoto( y + 1 )
         SWITCH aStru[ x + 1, 2 ]
         CASE 'C'
            RETURN AllTrim( FieldGet( x + 1 ) )
         CASE 'N'
            RETURN hb_NToS( FieldGet( x + 1 ) )
         CASE 'L'
            RETURN IIf( FieldGet( x + 1 ), 'Yes', 'No' )
         CASE 'D'
            RETURN DToC( FieldGet( x + 1 ) )
         ENDSWITCH
         RETURN '?'

      CASE Qt_ForegroundRole
         SWITCH aStru[ x + 1, 2 ]
         CASE 'N'
            RETURN oColorN
         CASE 'L'
            DBGoto( y + 1 )
            RETURN IIf( FieldGet( x + 1), oColorLY, oColorLN )
         CASE 'D'
            RETURN oColorD
         ENDSWITCH
         RETURN NIL

      CASE Qt_BackgroundRole
         RETURN NIL

      CASE Qt_TextAlignmentRole
         SWITCH aStru[ x + 1, 2 ]
         CASE 'C'
            RETURN Qt_AlignVCenter + Qt_AlignLeft
         CASE 'N'
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
      RETURN Len( aStru)
   ENDSWITCH

   RETURN NIL
