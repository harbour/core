/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration/test code for OLE server client which connects to
 *    Harbour OLE server working like xHarbour.com OLE servers described at
 *    http://xharbour.com/index.asp?page=add_on_oleserver&show_sub=7&show_i=1
 *    This client code is based on xHarbour.com client example published on
 *    above WWW page.
 */

PROCEDURE Main()
   LOCAL oObject

   BEGIN SEQUENCE WITH {|| break() }
      oObject := CreateObject( "MyOleServer" )
      ? oObject:MyMethod( "Hello", 123, .T., ;
                          { hb_datetime(), 123.45, { date(), 2, 3 } } )
      ? oObject:Property1
      oObject:Property1 := "!!! >>>" + upper( oObject:Property1 ) + "<<< !!!"
      ? oObject:Property1
   RECOVER
      ? "Can not access 'MyOleServer' OLE server."
   END SEQUENCE

   WAIT
RETURN
