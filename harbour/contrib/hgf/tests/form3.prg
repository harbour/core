//
// $Id$
//
// Use of persistence files to build a form

#include "hbclass.ch"
#include "hbpers.ch"

function Main()

   local oForm3 := HBForm3():New()

   oForm3:ShowModal()

return nil

CLASS HBForm3 FROM HBForm

   METHOD New()

   METHOD FormClick( oSender, nX, nY ) INLINE MsgInfo( "Click" )

ENDCLASS

METHOD New() CLASS HBForm3

   Super:New()

   #include "form3.hbf" // Notice this is just a persistence ascii file
                        // renamed as .hbf = Harbour form
return Self