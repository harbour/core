/* Copyright 2011 Viktor Szakats (vszakats.net/harbour) */

#require "hbwin"

PROCEDURE Main()

   ? WIN_LOWORD( 0x12345678 )         == wapi_LOWORD( 0x12345678 )
   ? WIN_HIWORD( 0x12345678 )         == wapi_HIWORD( 0x12345678 )

   ? WIN_MAKELPARAM( 0x1234, 0x5678 ) == wapi_MAKELPARAM( 0x1234, 0x5678 )
   ? WIN_MAKEWPARAM( 0x1234, 0x5678 ) == wapi_MAKEWPARAM( 0x1234, 0x5678 )

   ? WIN_RGB( 0x10, 0x20, 0x30 )      == wapi_RGB( 0x10, 0x20, 0x30 )

   RETURN
