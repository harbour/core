/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 */

#require "hbwin"

#include "simpleio.ch"

PROCEDURE Main()

   LOCAL a := win_printerGetDefault()

   ? ">" + a + "<"

   ? win_printerSetDefault( a )

   RETURN
