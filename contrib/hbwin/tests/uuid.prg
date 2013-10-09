/*
 * Harbour Project source code:
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour)
 * www - http://harbour-project.org
 *
 */

#require "hbwin"

#include "simpleio.ch"

PROCEDURE Main()

   LOCAL nRPCStatus

   ? win_UuidCreateString( @nRPCStatus )
   ? nRPCStatus

   RETURN
