/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2012 Francesco Perillo
 * www - http://harbour-project.org
 *
 */

#include "hbclass.ch"
#include "hbtrace.ch"

CREATE CLASS myCell FROM HB_QLabel

   DATA myValue INIT "Starting value"

   END CLASS


PROCEDURE MAIN
   LOCAL oWnd
   LOCAL oTB
   LOCAL oLbl
   LOCAL oRet

   oWnd := QMainWindow()

   oTB := QTableWidget( )
   oTB:setColumnCount( 1 )
   oTB:setRowCount( 1 )

   oWnd:setCentralWidget( oTB )

   oLbl := myCell():New( )
   oLbl:myValue = "new value"

   oLbl:setText( "Test for Bacco" )
   oTB:setCellWidget( 0, 0, oLbl )

   oWnd:show()

   QApplication():exec()

   RETURN

