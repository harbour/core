/* TIP HTTP advanced operations test */

#require "hbssl"
#require "hbtip"

#if ! defined( __HBSCRIPT__HBSHELL )
REQUEST __HBEXTERN__HBSSL__
#endif

PROCEDURE Main( cURL )

   LOCAL oHTTP, oURL, i

   IF Empty( oURL := TUrl():New( cURL ) )
      ? "Invalid URL", cURL
      RETURN
   ENDIF

   IF ! oURL:cProto == "http" .AND. ;
      ! oURL:cProto == "https"
      ? "This is a header test for http/https."
      ? "Use an http/https address."
      RETURN
   ENDIF

   IF oURL:cProto == "https" .AND. ! tip_SSL()
      ? "Error: Requires SSL support"
      RETURN
   ENDIF

   oHTTP := TIPClientHTTP():New( oURL )
   oHTTP:nConnTimeout := 20000
   ? "Connecting with", oURL:cServer
   IF oHTTP:Open( cURL )
      ? "Connection eshtablished"
      ? "Retrieving", oURL:cPath, oURL:cFile, oURL:cQuery

      IF oHTTP:Get( oURL:cPath )
         ? "Get Successful"
         FOR EACH i IN oHTTP:hHeaders
            ? i:__enumKey() + ":", i
         NEXT
      ELSE
         ? "Get failure (server reply:", oHTTP:cReply, ")"
      ENDIF

      oHTTP:Close()
   ELSE
      ? "Could not connect with", oURL:cServer
      IF oHTTP:SocketCon == NIL
         ? "Connection not initiated"
      ELSEIF hb_inetErrorCode( oHTTP:SocketCon ) == 0
         ? "Server replied:", oHTTP:cReply
      ELSE
         ? "Error in connection:", hb_inetErrorDesc( oHTTP:SocketCon )
      ENDIF
   ENDIF

   ? "Done"

   RETURN
