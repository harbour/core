/* Copyright 2015 Viktor Szakats (vszakats.net/harbour) */

#require "hbwin"

#include "simpleio.ch"

PROCEDURE Main()

   LOCAL cUserName
   LOCAL nFormat

   FOR nFormat := 0 TO 12
      ? nFormat, wapi_GetUserNameEx( nFormat, @cUserName ), win_ErrorString(), hb_ValToExp( cUserName )
   NEXT

   RETURN
