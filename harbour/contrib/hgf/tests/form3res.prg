//
// $Id$
//
// Loading persistence files at runtime sample

#include "hbclass.ch"

function Main()

   local oForm3 := HBForm3():New()

   oForm3:LoadFromFile( "form3.hbf" )

   oForm3:ShowModal()

return nil

CLASS HBForm3 FROM HBForm

   METHOD FormClick( oSender, nX, nY ) INLINE MsgInfo( "Click" )

ENDCLASS