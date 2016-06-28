/* Copyright 2014 Viktor Szakats (vszakats.net/harbour) */

#require "hbwin"

PROCEDURE Main()

   LOCAL xy, i, ii

#if defined( __HBSCRIPT__HBSHELL )
   hbshell_gtSelect( "GTWVT" )
#endif

   ? wapi_GetCursorPos()
   ? wapi_GetCursorPos( NIL )

   xy := NIL
   ? "@", wapi_GetCursorPos( @xy )
   ? hb_ValToExp( xy )

   xy := NIL
   ? wapi_GetCursorPos( xy )
   ? hb_ValToExp( xy )

   xy := { 0 }
   ? "@", wapi_GetCursorPos( @xy )
   ? hb_ValToExp( xy )

   xy := { 0 }
   ? wapi_GetCursorPos( xy )
   ? hb_ValToExp( xy )

   xy := { 1, 2, 3 }
   ? wapi_GetCursorPos( @xy )
   ? hb_ValToExp( xy )

   xy := { => }
   ? "@", wapi_GetCursorPos( @xy )
   ? hb_ValToExp( xy )

   xy := { => }
   ? wapi_GetCursorPos( xy )
   ? hb_ValToExp( xy )

   i := wapi_LoadIcon( , 32512 )
   ? ! Empty( i )
   ? wapi_GetLastError()

   ? wapi_GetIconInfo( i, @ii )
   ? hb_ValToExp( ii )

   Inkey( 0 )

   RETURN
