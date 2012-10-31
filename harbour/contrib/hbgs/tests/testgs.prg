/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2011 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 */

#require "hbgs"

#include "simpleio.ch"

PROCEDURE Main()

   LOCAL a, b, c, d

   ? HB_GSAPI_REVISION( @a, @b, @c, @d )

   ? a
   ? b
   ? c
   ? d

   ? HB_GS( { "--version" } )

   RETURN
