/* Copyright 2010 Viktor Szakats (vszakats.net/harbour) */

#require "hbwin"

#include "simpleio.ch"

PROCEDURE Main()

   LOCAL nRPCStatus

   ? win_UuidCreateString( @nRPCStatus )
   ? nRPCStatus

   RETURN
