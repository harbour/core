/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Viktor Szakats (harbour syenar.hu)
 * www - http://harbour-project.org
 *
 */

#include "simpleio.ch"

PROCEDURE Main()
   LOCAL a := WIN_PRINTERGETDEFAULT()

   ? ">" + a + "<"

   ? WIN_PRINTERSETDEFAULT( a )

   RETURN
