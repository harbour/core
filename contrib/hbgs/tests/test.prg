/*
 * Harbour Project source code:
 *
 * Copyright 2011 Viktor Szakats (vszakats.net/harbour)
 * www - http://harbour-project.org
 *
 */

#require "hbgs"

#include "simpleio.ch"

PROCEDURE Main()

   LOCAL a, b, c, d

   ? hb_gsapi_revision( @a, @b, @c, @d )

   ? a
   ? b
   ? c
   ? d

   ? hb_gs( { "--version" } )

   RETURN
