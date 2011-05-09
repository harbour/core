/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2011 Viktor Szakats (harbour.01 syenar.hu)
 * www - http://harbour-project.org
 *
 */

#include "simpleio.ch"

PROCEDURE Main()

    ? ">" + wapi_GetWindowsDirectory() + "<"
    ? ">" + wapi_GetSystemDirectory() + "<"

    RETURN
