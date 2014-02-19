/*
 * Harbour Project source code:
 *
 * Copyright 2011 Viktor Szakats (vszakats.net/harbour)
 * www - http://harbour-project.org
 *
 */

#require "hbnf"

PROCEDURE Main()

   ft_OnTick( {|| QOut( hb_MilliSeconds() ) } )

   Inkey( 0 )

   ft_OnTick( {|| QOut( hb_MilliSeconds() ) }, 18 )

   Inkey( 0 )

   ft_OnTick()

   Inkey( 0 )

   RETURN
