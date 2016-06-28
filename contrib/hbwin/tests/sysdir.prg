/* Copyright 2011 Viktor Szakats (vszakats.net/harbour) */

#require "hbwin"

#include "simpleio.ch"

PROCEDURE Main()

   ? ">" + wapi_GetWindowsDirectory() + "<"
   ? ">" + wapi_GetSystemDirectory() + "<"

   RETURN
