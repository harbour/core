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

#require "hbwin"

#include "simpleio.ch"

PROCEDURE Main()

   ? ">" + wapi_GetWindowsDirectory() + "<"
   ? ">" + wapi_GetSystemDirectory() + "<"

   RETURN
