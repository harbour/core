/*
 * Demonstration/test code for OLE server client which connects to
 * Harbour OLE server working like xHarbour.com OLE servers.
 * This client code is based on xHarbour.com client example published on
 * above WWW page.
 */

#require "hbwin"

PROCEDURE Main()

   LOCAL oObject

   BEGIN SEQUENCE WITH {|| Break() }
      oObject := CreateObject( "MyOleServer" )
      ? oObject:MyMethod( "Hello", 123, .T., ;
         { hb_DateTime(), 123.45, { Date(), 2, 3 } } )
      ? oObject:Property1
      oObject:Property1 := "!!! >>>" + Upper( oObject:Property1 ) + "<<< !!!"
      ? oObject:Property1
   RECOVER
      ? "Can not access 'MyOleServer' OLE server."
   END SEQUENCE

   WAIT

   RETURN
