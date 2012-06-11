/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2012 Carlos Bacco <carlosbacco at gmail.com>
 * www - http://harbour-project.org
 *
 */

#include "hbqtgui.ch"
#include "dbfbrowserclass.prg"


PROCEDURE Main()
   LOCAL oWid
   LOCAL oButton1
   LOCAL oButton2
   LOCAL oTable1
   LOCAL oLay1
   LOCAL oLay2

   Set( _SET_EXACT, .T. )

   oWid := QWidget()
   oWid:setWindowTitle( "DBF Browser" )

   oTable1 := QDBFBrowser()

   // NOTE: QDBFBrowser inherits QTableView, so we can use the methods below.
   oTable1:setAlternatingRowColors( .T. )
   oTable1:setShowGrid( .F. )
   oTable1:setTabKeyNavigation( .F. )
   oTable1:verticalHeader():setSelectionBehavior( QAbstractItemView_SelectRows )
   oTable1:verticalHeader():setSelectionMode( QAbstractItemView_SingleSelection )
   // ENDNOTE

   oButton1 := QPushButton()
   oButton1:setText( "Open DBF" )
   oButton1:connect( "clicked()", {|| OpenDBF( oWid, oTable1 ) } )

   oButton2 := QPushButton()
   oButton2:setText( "Close DBF" )
   oButton2:connect( "clicked()", {|| CloseDBF( oTable1 ) } )

   
   oLay1 := QVBoxLayout( oWid )
   oLay1:addWidget( oTable1 )
   olay1:addLayout( oLay2 := QHBoxLayout() )
   oLay2:addWidget( oButton1 )
   oLay2:addWidget( oButton2 )

   oWid:Show()
   QApplication():exec()
RETURN


PROCEDURE OpenDBF( oWid, oTable )
   STATIC cDir := "."
   LOCAL cFile

   cFile := QFileDialog():getOpenFileName( oWid, "Open file", cDir, "DBF files (*.dbf);;All files (*.*)" )

   IF cFile == ""
      RETURN
   END

   cDir := hb_FNameDir( cFile ) // To remember last used dir

   CloseDBF( oTable )
   BEGIN SEQUENCE WITH {||.F.}
      DBUseArea( .T., NIL, cFile, NIL, .F., .F. )
   END SEQUENCE

   If Used()
      oTable:attach()
   End
RETURN

PROCEDURE CloseDBF( oTable )
   oTable:detach()
   DBCloseArea()
RETURN


#include "dbfbrowserclass.prg"

