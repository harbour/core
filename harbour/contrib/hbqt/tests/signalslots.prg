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
   LOCAL oMain, oLabel, oScrollBar, oLayout, qApp

   oMain := QWidget()
   oMain:setMinimumHeight( 300 )
   oMain:setMinimumHeight( 300 )

   oLayout := QVBoxLayout()

   oScrollBar := QScrollBar()

   oLabel := QLabel()
   oLabel:setText("Move the slider")

   oLayout:addWidget( oScrollBar )
   oLayout:addWidget( oLabel )

   hbqt_Connect( oScrollBar, "valueChanged(int)", oLabel, "setNum(int)" )

   oMain:setLayout( oLayout )
   oMain:show()

   qApp := QApplication()
   qApp:exec()

   RETURN

