/*
 * Demonstration/test client code for OLE server returning to client
 * HVM objects as OLE object. It's also test for parameters
 * passed by reference.
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *
 */

#require "hbwin"

PROCEDURE Main()

   LOCAL oObject, oTime, oInfo
   LOCAL p1, p2, p3, p4, p5, p6, p7

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   IF Empty( oObject := win_oleCreateObject( "MyOleObjServer" ) )
      ? "Can not access 'MyOleObjServer' OLE server."
   ELSE
      oTime := oObject:timer()
      ? "TIMER:"
      ? "      date:", oTime:date
      ? "      time:", oTime:time
      ? "       now:", oTime:now
      oInfo := oObject:info()
      ? "INFO:"
      ? "        os:", oInfo:os
      ? "       ver:", oInfo:ver
      ? "  compiler:", oInfo:compiler
      ? "     build:", oInfo:build
      ?
      ? "REF:", oObject:ref( @p1, @p2, @p3, @p4, @p5, @p6, @p7 )
      ? "        p1:", hb_ValToExp( p1 )
      ? "        p2:", hb_ValToExp( p2 )
      ? "        p3:", hb_ValToExp( p3 )
      ? "        p4:", hb_ValToExp( p4 )
      ? "        p5:", p5:className() // hb_ValToExp( p5 )
      ? "        p6:", hb_ValToExp( p6 )
      ? "        p7:", hb_ValToExp( p7 )
   ENDIF

   WAIT

   RETURN
