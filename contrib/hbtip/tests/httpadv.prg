/* TIP HTTP advanced operations Test */

#require "hbssl"
#require "hbtip"

#if ! defined( __HBSCRIPT__HBSHELL )
REQUEST __HBEXTERN__HBSSL__
#endif

PROCEDURE Main( cUrl )

   LOCAL oCon, oUrl, i

   IF Empty( oUrl := TUrl():New( cUrl ) )
      ? "Invalid URL", cUrl
      RETURN
   ENDIF

   IF !( oUrl:cProto == "http" ) .AND. ;
      !( oUrl:cProto == "https" )
      ? "This is a header test for http/https."
      ? "Use an http/https address."
      RETURN
   ENDIF

   IF oUrl:cProto == "https" .AND. ! tip_SSL()
      ? "Error: Requires SSL support"
      RETURN
   ENDIF

   oCon := TIPClientHTTP():New( oUrl )
   oCon:nConnTimeout := 20000
   ? "Connecting with", oUrl:cServer
   IF oCon:Open( cUrl )
      ? "Connection eshtablished"
      ? "Retrieving", oUrl:cPath, oUrl:cFile, oUrl:cQuery

      IF oCon:Get( oUrl:cPath )
         ? "Get Successful"
         FOR EACH i IN oCon:hHeaders
            ? i:__enumKey() + ":", i
         NEXT
      ELSE
         ? "Get failure (server reply:", oCon:cReply, ")"
      ENDIF

      oCon:Close()
   ELSE
      ? "Can't connect with", oUrl:cServer
      IF oCon:SocketCon == NIL
         ? "Connection not initiated"
      ELSEIF hb_inetErrorCode( oCon:SocketCon ) == 0
         ? "Server replied:", oCon:cReply
      ELSE
         ? "Error in connection:", hb_inetErrorDesc( oCon:SocketCon )
      ENDIF
   ENDIF

   ? "Done"

   RETURN
