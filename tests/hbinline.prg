/*
 * Harbour Project source code:
 *    new hb_inline {} test
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

PROCEDURE Main()

   LOCAL hb_inline := "(var)"

   ? hb_inline { hb_retc("inline"); }, hb_inline( "parameter" ) {
        hb_retc( hb_parc( 1 ) );
        } + "!", hb_inline, hb_inline() { hb_retc( ":-)" ); }, ;
        hb_inline(), "{}"

   RETURN

STATIC FUNCTION hb_inline()
   RETURN "func()"
