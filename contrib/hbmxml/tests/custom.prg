#require "hbmxml"

#xtranslate _ENCODE( <xData> ) => ( hb_base64Encode( hb_Serialize( mxmlGetCustom( <xData> ) ) ) )

PROCEDURE Main()

   LOCAL tree, node
   LOCAL xData

   mxmlSetErrorCallback( @my_mxmlError() )
   mxmlSetCustomHandlers( @load_c(), @save_c() )

   IF ! hb_FileExists( "cust.xml" )
      create_cust()
   ENDIF

   tree := mxmlLoadFile( tree, "cust.xml", @type_cb() )

   node := mxmlFindElement( tree, tree, "hash", NIL, NIL, MXML_DESCEND )
   IF Empty( node )
      OutErr( "Unable to find <hash> element in XML tree!" )
      mxmlDelete( tree )

      ErrorLevel( -1 )
      RETURN
   ENDIF

   IF !( hb_MD5( _ENCODE( node ) ) == mxmlElementGetAttr( node, "checksum" ) )
      OutErr( "Custom data of element <hash> is corrupted!" )
      mxmlDelete( tree )

      ErrorLevel( -1 )
      RETURN
   ENDIF

   xData := mxmlGetCustom( node )
   IF HB_ISHASH( xData ) .AND. hb_HHasKey( xData, "Today" )
      OutStd( xData[ "Today" ], hb_eol() )
   ENDIF

   mxmlSetErrorCallback( NIL )
   mxmlSetCustomHandlers( NIL, NIL )

   ErrorLevel( 0 )

   RETURN

STATIC PROCEDURE create_cust()

   LOCAL tree, group, element, node
   LOCAL hData := { => }

   hData[ "Today" ] := hb_TSToStr( hb_DateTime() )
   /* etc. */

   tree    := mxmlNewXML()
   group   := mxmlNewElement( tree, "group" )
   element := mxmlNewElement( group, "hash" )
   node    := mxmlNewCustom( element, hData )

   mxmlElementSetAttr( element, "type", "custom" )
   mxmlElementSetAttr( element, "checksum", hb_MD5( _ENCODE( node ) ) )

   mxmlSaveFile( tree, "cust.xml", @whitespace_cb() )

   RETURN

PROCEDURE my_mxmlError( cErrorMsg )

   OutErr( cErrorMsg, hb_eol() )

   RETURN

FUNCTION load_c( node, cString )

   mxmlSetCustom( node, hb_Deserialize( hb_base64Decode( cString ) ) )

   RETURN 0  /* 0 on success or non-zero on error */

FUNCTION save_c( node )

   RETURN _ENCODE( node ) /* string on success or NIL on error */

FUNCTION whitespace_cb( node, where )

   LOCAL parent        /* Parent node */
   LOCAL nLevel := -1  /* Indentation level */
   LOCAL name          /* Name of element */

   name := mxmlGetElement( node )

   IF Left( name, 4 ) == "?xml"
      IF where == MXML_WS_AFTER_OPEN
         RETURN hb_eol()
      ELSE
         RETURN NIL
      ENDIF

   ELSEIF where == MXML_WS_BEFORE_OPEN .OR. ;
         ( ( name == "choice" .OR. name == "option" ) .AND. ;
         where == MXML_WS_BEFORE_CLOSE )

      parent := mxmlGetParent( node )
      DO WHILE ! Empty( parent )
         nLevel++
         parent := mxmlGetParent( parent )
      ENDDO

      IF nLevel > 8
         nLevel := 8
      ELSEIF nLevel < 0
         nLevel := 0
      ENDIF

      RETURN Replicate( Chr( 9 ), nLevel )

   ELSEIF where == MXML_WS_AFTER_CLOSE .OR. ;
         ( ( name == "group" .OR. name == "option" .OR. name == "choice" ) .AND. ;
         where == MXML_WS_AFTER_OPEN )
      RETURN hb_eol()

   ELSEIF where == MXML_WS_AFTER_OPEN .AND. Empty( mxmlGetFirstChild( node ) )
      RETURN hb_eol()
   ENDIF

   RETURN NIL /* Return NIL for no added whitespace... */

FUNCTION type_cb( node )

   LOCAL nResult
   LOCAL cType

   IF Empty( cType := mxmlElementGetAttr( node, "type" ) )
      cType := mxmlGetElement( node )
   ENDIF

   SWITCH Lower( cType )
   CASE "custom"               /* don't forget */
   CASE "hash"
      nResult := MXML_CUSTOM
      EXIT

   OTHERWISE
      nResult := MXML_TEXT
      EXIT
   ENDSWITCH

   RETURN nResult
