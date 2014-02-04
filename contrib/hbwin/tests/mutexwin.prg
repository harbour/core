#require "hbwin"

STATIC s_hMutex

PROCEDURE Main()

   // It may be a random program name :)
   LOCAL cAppName := hb_FNameName( hb_ProgName() )

   s_hMutex := wapi_CreateMutex( NIL, NIL, cAppName )
   IF ! Empty( s_hMutex ) .AND. wapi_GetLastError() == 0
      ? cAppName, "=> Hello World! OK."
   ELSE
      ? cAppName, "=> Application is running. Error", ;
         hb_ntos( wapi_GetLastError() ), win_ErrorString()
   ENDIF

   WAIT

   RETURN
