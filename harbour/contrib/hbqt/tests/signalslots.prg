/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2012 Francessco Perillo
 */
 
#include "hbqtgui.ch"

PROCEDURE main()
   LOCAL oMain, oLabel, oScrollBar, oLayout

   oMain := QWidget()
   oMain:setMinimumHeight( 300 )
   oMain:setMinimumHeight( 300 )

   oLayout := QVBoxLayout( oMain )

   oScrollBar := QScrollBar(  )

   oLabel := QLabel( )
   oLabel:show()
   oLabel:setText("Move the slider")

   oLayout:addWidget( oScrollBar )
   oLayout:addWidget( oLabel )

   SIGNAL2SLOT( oScrollBar, "valueChanged(int)", oLabel, "setNum(int)" )

   oMain:show()

   QApplication():exec()

   RETURN

