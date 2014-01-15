#include "button.ch"
#include "inkey.ch"

#ifndef HB_MENU_SEPARATOR_UNI
   #define HB_MENU_SEPARATOR_UNI  MENU_SEPARATOR
#endif

PROCEDURE Main()

   LOCAL oMenu := Create( "N/W,W+/N,GR+/W,GR+/N,N+/W,N/W" )

   CLS

#ifdef _SET_EVENTMASK
   Set( _SET_EVENTMASK, INKEY_ALL )
   MSetCursor( .T. )
#endif

   DO WHILE MenuModal( oMenu, 1, MaxRow(), 0, MaxCol(), "W/B" ) != 999
   ENDDO

   RETURN

STATIC FUNCTION Create( cColor )

   LOCAL oTopBar, oPopUp
   LOCAL oPopUp1, oPopUp2, oPopUp3
   LOCAL oItem

   oTopBar := TopBar( 0, 0, MaxCol() )
   oTopBar:colorSpec := cColor

   oPopUp := Popup()
   oPopUp:colorSpec := cColor
   oPopUp:AddItem( MenuItem( "&Prima nota", {|| NIL },, "Some msg 1" ) )
   oPopUp:AddItem( MenuItem( HB_MENU_SEPARATOR_UNI ) )

   oPopUp1 := Popup()
   oPopUp1:colorSpec := cColor
   oPopUp1:AddItem( MenuItem( "Singolo", {|| NIL } ) )

   oPopUp2 := Popup()
   oPopUp2:colorSpec := cColor
   oPopUp2:AddItem( MenuItem( "Conti", {|| NIL } ) )
   oPopUp2:AddItem( MenuItem( "Clienti", {|| NIL } ) )
   oPopUp2:AddItem( MenuItem( HB_MENU_SEPARATOR_UNI ) )
   oPopUp2:AddItem( MenuItem( "Fornitori", {|| NIL } ) )

   oPopUp1:AddItem( MenuItem( "Multiplo", oPopUp2 ) )
   oPopUp1:AddItem( MenuItem( "Storico", {|| NIL } ) )
   oPopUp1:AddItem( MenuItem( "Estratto conto", {|| NIL } ) )
   oPopUp1:AddItem( MenuItem( "Per contropartita", {|| NIL } ) )

   oPopUp:AddItem( MenuItem( "P&artitari", oPopUp1,, "Some msg 2" ) )
   oPopUp:AddItem( oItem := MenuItem( "D&isabled", {|| NIL } ) )
   oItem:enabled := .F.

   oPopUp1 := Popup()
   oPopUp1:colorSpec := cColor
   oPopUp1:AddItem( MenuItem( "Totale vendite", {|| NIL } ) )
   oPopUp1:AddItem( MenuItem( "Totale acquisti", {|| NIL } ) )
   oPopUp1:AddItem( MenuItem( "Cerca importo", {|| NIL } ) )
   oPopUp1:AddItem( MenuItem( "Quadratura", {|| NIL } ) )
   oPopUp1:AddItem( MenuItem( "Congela movimenti", {|| NIL } ) )

   oPopUp:AddItem( MenuItem( "&Controlli", oPopUp1 ) )

   oPopUp1 := Popup()
   oPopUp1:colorSpec := cColor
   oPopUp1:AddItem( MenuItem( "Cerca aliquota", {|| NIL } ) )
   oPopUp1:AddItem( MenuItem( "Ventilazione", {|| NIL } ) )
   oPopUp1:AddItem( MenuItem( "Input corrispettivi", {|| .T. } ) )
   oPopUp1:AddItem( MenuItem( "Dati comunicazione annuale", {|| NIL } ) )

   oPopUp:AddItem( MenuItem( "Iva", oPopUp1 ) )

   oTopBar:AddItem( MenuItem( "Movimenti", oPopUp ) )

   oPopUp := Popup()
   oPopUp:colorSpec := cColor
   oPopUp:AddItem( MenuItem( "Contabile generale", {|| NIL } ) )

   oPopUp1 := Popup()
   oPopUp1:colorSpec := cColor

   oPopUp2 := Popup()
   oPopUp2:colorSpec := cColor
   oPopUp2:AddItem( MenuItem( "Completa", {|| NIL } ) )
   oPopUp2:AddItem( MenuItem( "Schede aperte", {|| NIL } ) )

   oPopUp1:AddItem( MenuItem( "Clienti", oPopUp2 ) )

   oPopUp2 := Popup()
   oPopUp2:colorSpec := cColor
   oPopUp2:AddItem( MenuItem( "Completa", {|| NIL } ) )
   oPopUp2:AddItem( MenuItem( "Schede aperte", {|| NIL } ) )

   oPopUp1:AddItem( MenuItem( "Fornitori", oPopUp2 ) )
   oPopUp1:AddItem( MenuItem( "Conti", {|| NIL } ) )

   oPopUp:AddItem( MenuItem( "Contabile particolare", oPopUp1 ) )

   oPopUp1 := Popup()
   oPopUp1:colorSpec := cColor

   oPopUp2 := Popup()
   oPopUp2:colorSpec := cColor
   oPopUp2:AddItem( MenuItem( "Per conto", {|| NIL } ) )
   oPopUp2:AddItem( MenuItem( "Per mastro", {|| NIL } ) )
   oPopUp2:AddItem( MenuItem( "Comparato", {|| NIL } ) )
   oPopUp2:AddItem( MenuItem( "C/Economico IRAP", {|| NIL } ) )

   oPopUp1:AddItem( MenuItem( "Tradizionale", oPopUp2 ) )

   oPopUp2 := Popup()
   oPopUp2:colorSpec := cColor
   oPopUp2:AddItem( MenuItem( "Preparazione", {|| NIL } ) )
   oPopUp2:AddItem( MenuItem( "Stampa", {|| NIL } ) )

   oPopUp1:AddItem( MenuItem( "Riclassificato", oPopUp2 ) )
   oPopUp1:AddItem( MenuItem( "Istanza XBRL", {|| NIL } ) )

   oPopUp:AddItem( MenuItem( "Bilancio d'esercizio", oPopUp1 ) )

   oPopUp1 := Popup()
   oPopUp1:colorSpec := cColor
   oPopUp1:AddItem( MenuItem( "Genera bilancio d'apertura", {|| NIL } ) )
   oPopUp1:AddItem( MenuItem( "Cancella bilancio d'apertura", {|| NIL } ) )
   oPopUp1:AddItem( MenuItem( "Genera bilancio di chiusura", {|| NIL } ) )
   oPopUp1:AddItem( MenuItem( "Cancella bilancio di chiusura", {|| NIL } ) )

   oPopUp:AddItem( MenuItem( "Chiusura ed apertura", oPopUp1 ) )

   oTopBar:AddItem( MenuItem( "Situazione", oPopUp ) )

   oPopUp := Popup()
   oPopUp:colorSpec := cColor

   oPopUp1 := Popup()
   oPopUp1:colorSpec := cColor
   oPopUp1:AddItem( MenuItem( "Esercizio", {|| NIL } ) )
   oPopUp1:AddItem( MenuItem( "Apertura", {|| NIL } ) )
   oPopUp1:AddItem( MenuItem( "Assestamento", {|| NIL } ) )
   oPopUp1:AddItem( MenuItem( "Chiusura", {|| NIL } ) )

   oPopUp:AddItem( MenuItem( "Libro giornale", oPopUp1 ) )

   oTopBar:AddItem( MenuItem( "E&xit", {|| .T. }, , "", 999 ) )

   RETURN oTopBar
