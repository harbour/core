#include "hbqtgui.ch"
STATIC palette
STATIC slider_rosso, slider_verde, slider_blu
STATIC testo

PROCEDURE Main()

   LOCAL finestra
   LOCAL font

   finestra := QMainWindow()
   finestra:resize( 320, 400 )
   finestra:setWindowTitle( "Giovanni" )

   font := QFont()
   font:setPointSize( 30 )
   font:setBold( .T. )

   testo := QLabel( finestra )
   testo:setText( "Colori" )
   testo:move( 100, 10 )
   testo:resize( 200, 100 )
   testo:setfont( font )

   slider_rosso := QSlider( finestra )
   slider_rosso:resize( 30, 200 )
   slider_rosso:move( 100, 120 )
   slider_rosso:setMinimum( 0 )
   slider_rosso:setMaximum( 255 )
   slider_rosso:setSingleStep( 1 )
   slider_rosso:setPageStep( 10 )
   slider_rosso:setValue( 0 )
   slider_rosso:Connect( "valueChanged(int)", { || cambia_colori() } )

   slider_verde := QSlider( finestra )
   slider_verde:resize( 30, 200 )
   slider_verde:move( 150, 120 )
   slider_verde:setMinimum( 0 )
   slider_verde:setMaximum( 255 )
   slider_verde:setSingleStep( 1 )
   slider_verde:setPageStep( 10 )
   slider_verde:setValue( 0 )
   slider_verde:Connect( "valueChanged(int)", { || cambia_colori() } )

   slider_blu := QSlider( finestra )
   slider_blu:resize( 30, 200 )
   slider_blu:move( 200, 120 )
   slider_blu:setMinimum( 0 )
   slider_blu:setMaximum( 255 )
   slider_blu:setSingleStep( 1 )
   slider_blu:setPageStep( 10 )
   slider_blu:setValue( 0 )
   slider_blu:Connect( "valueChanged(int)", { || cambia_colori() } )

   finestra:show()

   QApplication():exec()

   RETURN

PROCEDURE cambia_colori()

   palette := QPalette()
   palette:SetColor( QPalette_WindowText, QColor( slider_rosso:value , slider_verde:value , slider_blu:value ) )
   testo:setPalette( palette )

   RETURN
