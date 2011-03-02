#include "hbqtgui.ch"

PROCEDURE Main()

   LOCAL finestra
   LOCAL oMenuBar, oMenu1, oVoceIns, oVoceMod

   // ----------Impostazione Finestra-----------
   finestra := QmainWindow()
   finestra:SetFixedSize( 300, 200 )
   finestra:setWindowTitle( "Volto" )

   // ----------Impostazione Menu-----------
   oMenuBar := QMenuBar( finestra )
   oMenuBar:resize( 700, 22 )

   oMenu1 := QMenu()
   oMenu1:setTitle( "Anagrafica" )

   oVoceIns := QAction( oMenu1 )
   oVoceIns:setText( "Inserimento" )
   oVoceIns:connect( "triggered(bool)", { || inserimento() } )
   oVoceMod := QAction( oMenu1 )
   oVoceMod:setText( "Modifica" )
   oVoceMod:connect( "triggered(bool)", { || modifica() } )

   oMenu1:addAction( oVoceIns )
   oMenu1:addAction( oVoceMod )

   oMenuBar:addMenu( oMenu1 )

   finestra:Show()
   QApplication():exec()

   RETURN

PROCEDURE inserimento()

   STATIC finestra2

   finestra2 := QmainWindow()
   finestra2:SetFixedSize( 640, 480 )
   finestra2:setWindowTitle( "Inserimento Anagrafica" )
   finestra2:Show()

   RETURN

PROCEDURE modifica()

   STATIC finestra2

   finestra2 := QmainWindow()
   finestra2:SetFixedSize( 640, 480 )
   finestra2:setWindowTitle( "Modifica Anagrafica" )
   finestra2:Show()

   RETURN
