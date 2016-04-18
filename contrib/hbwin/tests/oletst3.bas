/*
 * Demonstration/test code for OLE server client which connects to
 * Harbour OLE server working like xHarbour.com OLE servers.
 * This client code is based on xHarbour.com client example published on
 * above WWW page.
 */

DIM oObject

SET oObject = CreateObject( "MyOleServer" )

MsgBox oObject.MyFunc( "Hello", 123 )

MsgBox oObject.Property1
