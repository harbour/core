/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration/test client code for OLE server using hash array with
 *    strict item order (associative hash array) to define OLE objects
 *    with fixed message numbers (DISPIDs)
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 */

PROCEDURE Main()

   LOCAL oObject

   oObject := win_OleCreateObject( "MyOleTimeServer" )

   IF !Empty( oObject )
      ? "DATE:", oObject:Date()
      ? "TIME:", oObject:Time()
      ? "DATTIME:", oObject:datetime()
      ? "VALUE:", oObject:value
      ? "GETDATA:", oObject:getdata()
      oObject:value := "hello"
      ? "VALUE:", oObject:value
      ? "GETDATA:", oObject:getdata()
   ELSE
      ? "Can not access 'MyOleTimeServer' OLE server."
   ENDIF

   WAIT

   RETURN
