//
// $Id$
//
// Loading persistence files at runtime sample

#include "hbclass.ch"

function Main()

   local oForm3 := TForm3():New()

   oForm3:LoadFromFile( "form3.hbf" )

   oForm3:ShowModal()

return nil

CLASS TForm3 FROM TForm

   DATA   aTest PROPERTY

   METHOD FormClick( oSender, nX, nY ) INLINE MsgInfo( "Click" )

ENDCLASS