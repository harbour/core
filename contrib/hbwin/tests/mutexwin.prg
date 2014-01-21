#require "hbwin"

STATIC s_hMutex

PROCEDURE Main()

   LOCAL cAppName

   // It may be a random program name :)
   hb_FNameSplit( hb_ProgName(), NIL, @cAppName )

   s_hMutex := wapi_CreateMutex( NIL, NIL, cAppName )
   IF ! Empty( s_hMutex ) .AND. wapi_GetLastError() == 0
      ? cAppName, "=> Hello World! OK."
   ELSE
      ? cAppName, "=> Application is running. Error", wapi_GetLastError()
   ENDIF

   WAIT

   RETURN
