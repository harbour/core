/*
 *
 * Copyright 2011 Viktor Szakats (vszakats.net/harbour)
 *
 */

#require "hbnf"

PROCEDURE Main()

   CLS

   ft_OnTick( {|| OutStd( hb_MilliSeconds(), hb_eol() ) } )

   Inkey( 0 )

   ft_OnTick( {|| OutStd( hb_MilliSeconds(), hb_eol() ) }, 18 )

   Inkey( 0 )

   ft_OnTick()

   Inkey( 0 )

   RETURN
