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

STATIC qApp
STATIC oWnd

STATIC oDA, oForeColor, oAlphaColor, oBackBrush, oSize, oPixmap, oIcon, oFont


REQUEST HB_QTGUI

INIT PROCEDURE Qt_Start()
   qApp := QApplication()
   RETURN

EXIT PROCEDURE Qt_End()
   qApp:quit()
   RETURN

PROCEDURE Main()
   LOCAL tb1, mo1, lay1, lay2, bt1, bt2, bt3

   oWnd := QMainWindow()
   oWnd:resize(600,400 )

   oDA := QWidget()
   oWnd:setCentralWidget( oDA )
   lay1 := QVBoxLayout( oDA )

   /* Here we define some HBQt objects to use in the callback */
   oForeColor := QColor( 0, 50, 100 )                              // Solid blue
   oAlphaColor := QColor( 255, 0, 0 )                              // Transparent Red
   oAlphaColor:setAlpha( 128 )
   oBackBrush := QBrush( QColor( 255, 255, 100 ), Qt_BDiagPattern ) // Yellow Diagonal lines
   oSize := QSize( 80, 30 )                                        // Default header sizes
   oPixmap := QPixMap( hb_dirBase() + "harbour-icon.png" )         // Image
   oIcon := QIcon( oPixMap )                                       // Icon (reusing the image)
   oFont := QFont()                                                // Bold Italic font
   oFont:setBold( .T. )
   oFont:setItalic( .T. )
   /* ------------------------------------------------------- */

   tb1 := QTableView()

   mo1 := HBQAbstractItemModel( {| t, r, x, y| my_aim( t, r, x, y ) } )
   tb1:setModel( mo1 )

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

   oWnd:Show()
   qApp:exec()

   RETURN

STATIC FUNCTION my_aim( t, role, x, y )

   SWITCH t
   CASE HBQT_QAIM_data

      SWITCH role
      CASE Qt_DisplayRole
         IF x == 2 .AND. y == 6
            RETURN "Unselectable"
         ENDIF
         RETURN hb_ntos( x ) + "," + hb_ntos( y )

      CASE Qt_ForegroundRole
         IF x == 7
            RETURN oAlphaColor
         ENDIF
         RETURN oForeColor /* Here we are sending a QColor */

      CASE Qt_BackgroundRole
         IF x == 2
            RETURN oBackBrush /* Here we are sending a QBrush */
         ENDIF
         RETURN NIL

      CASE Qt_TextAlignmentRole
         RETURN Qt_AlignCenter

      CASE Qt_DecorationRole
         IF ( x % 4 ) == 0 .AND. ( y % 4 ) == 0
            RETURN oPixmap /* Here we are sending a QPixmap */
         ELSEIF x == 2 .AND. y > 4 .AND. y < 10
            RETURN oIcon /* Here we are sending a QIcon */
         ELSEIF x == 2 .AND. y == 3
            RETURN oAlphaColor /* Same role, other type */
         ELSEIF x == 2 .AND. y == 4
            RETURN oForeColor /* Same role, other type */
         ENDIF

      CASE Qt_FontRole
         IF x == 1
            RETURN oFont
         ENDIF
         RETURN NIL

      ENDSWITCH

      RETURN NIL

   CASE HBQT_QAIM_flags
      IF x == 2 .AND. y == 6
         RETURN 0 /* cell is unselectable */
      ENDIF
      RETURN NIL

   CASE HBQT_QAIM_headerData
      SWITCH role
      CASE Qt_DisplayRole
         RETURN iif( x == Qt_Horizontal, "H", "V" ) + hb_ntos( y )

      CASE Qt_TextAlignmentRole
         RETURN Qt_AlignCenter

      CASE Qt_SizeHintRole
         RETURN oSize  /* Finally we are sending a QSize */
      ENDSWITCH
      RETURN NIL

   CASE HBQT_QAIM_rowCount
      RETURN 50000

   CASE HBQT_QAIM_columnCount
      RETURN 1000
   ENDSWITCH

   RETURN NIL
