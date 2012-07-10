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

PROCEDURE Main()
   LOCAL oWid
   LOCAL oLabel1
   LOCAL oButton1
   LOCAL oButton2
   LOCAL oTable1
   LOCAL oLay1
   LOCAL oLay2

   Set( _SET_EXACT, .T. )

   oWid := QWidget()
   oWid:setWindowTitle( "Simple DBF Browser/Editor" )

   oLabel1 := QLabel()
   oLabel1:setText( 'Click in "Open DBF" to start' )

   oTable1 := QDBFBrowser() // QDBFBrowser inherits QTableView, so we can use the methods below.
   oTable1:setAlternatingRowColors( .T. )
   oTable1:setShowGrid( .F. )
   oTable1:setTabKeyNavigation( .F. )
   oTable1:verticalHeader():setSelectionBehavior( QAbstractItemView_SelectRows )
   oTable1:verticalHeader():setSelectionMode( QAbstractItemView_SingleSelection )

   oButton1 := QPushButton()
   oButton1:setText( "Open DBF" )
   oButton1:connect( "clicked()", {|| OpenDBF( oWid, oTable1, oLabel1 ) } )

   oButton2 := QPushButton()
   oButton2:setText( "Close DBF" )
   oButton2:connect( "clicked()", {|| CloseDBF( oTable1, oLabel1 ) } )

   oLay1 := QVBoxLayout( oWid )
   oLay1:addWidget( oLabel1 )
   oLay1:addWidget( oTable1 )
   olay1:addLayout( oLay2 := QHBoxLayout() )
   oLay2:addWidget( oButton1 )
   oLay2:addWidget( oButton2 )

   oWid:Show()
   QApplication():exec()

   RETURN

PROCEDURE OpenDBF( oWid, oTable, oLabel )
   STATIC cDir := "."
   LOCAL cFile
   Local cName

   cFile := QFileDialog():getOpenFileName( oWid, "Select database", cDir, "DBF files (*.dbf);;All files (*.*)" )
   IF cFile == ""
      RETURN
   ENDIF

   cDir := hb_FNameDir( cFile ) // To remember last used dir
   cName := hb_FNameNameExt( cFile )

   CloseDBF( oTable, oLabel )
   BEGIN SEQUENCE WITH {||.F.}
      DBUseArea( .T., NIL, cFile, NIL, .T., .F. )
   END SEQUENCE

   IF Used()
      oTable:attach()
      oLabel:setText( "Database: " + cName )
   ELSE
      oLabel:setText( "Could not open " + cName )
   ENDIF
   RETURN

PROCEDURE CloseDBF( oTable , oLabel )
   oTable:detach()
   oLabel:setText( "" )
   DBCloseArea()
   RETURN


#include "dbfbrowserclass.prg"

