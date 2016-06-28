/* Copyright (c) 2011 Petr Chornyj */

#require "hbmxml"

#xtranslate _ENCODE( <xData> ) => ( hb_base64Encode( hb_Serialize( mxmlGetCustom( <xData> ) ) ) )

PROCEDURE Main()

   LOCAL cFileName := hb_FNameExtSet( __FILE__, ".xml" )

   LOCAL tree, node
   LOCAL xData

   Set( _SET_OSCODEPAGE, hb_cdpOS() )

   mxmlSetErrorCallback( @my_mxmlError() )
   mxmlSetCustomHandlers( @load_c(), @save_c() )

   create_cust( cFileName )

   tree := mxmlLoadFile( tree, cFileName, @type_cb() )

   node := mxmlFindElement( tree, tree, "hash", , , MXML_DESCEND )
   IF Empty( node )
      ? "Cannot find <hash> element in XML tree!"
      mxmlDelete( tree )

      ErrorLevel( -1 )
      RETURN
   ENDIF

   IF !( HB_SHA256( _ENCODE( node ) ) == mxmlElementGetAttr( node, "checksum" ) )
      ? "Custom data of element <hash> is corrupted!"
      mxmlDelete( tree )

      ErrorLevel( -1 )
      RETURN
   ENDIF

   xData := mxmlGetCustom( node )
   IF HB_ISHASH( xData ) .AND. "Today" $ xData
      ? xData[ "Today" ]
   ENDIF

   mxmlSetErrorCallback( NIL )
   mxmlSetCustomHandlers( NIL, NIL )

   ErrorLevel( 0 )

   RETURN

STATIC PROCEDURE create_cust( cFileName )

   LOCAL tree, group, element, node
   LOCAL hData := { => }

   hData[ "Today" ] := hb_TSToStr( hb_DateTime() )
   /* etc. */

   tree    := mxmlNewXML()
   group   := mxmlNewElement( tree, "group" )
   element := mxmlNewElement( group, "hash" )
   node    := mxmlNewCustom( element, hData )

   mxmlElementSetAttr( element, "type", "custom" )
   mxmlElementSetAttr( element, "checksum", hb_SHA256( _ENCODE( node ) ) )

   mxmlSaveFile( tree, cFileName, @whitespace_cb() )

   RETURN

STATIC PROCEDURE my_mxmlError( cErrorMsg )

   ? cErrorMsg

   RETURN

STATIC FUNCTION load_c( node, cString )

   mxmlSetCustom( node, hb_Deserialize( hb_base64Decode( cString ) ) )

   RETURN 0  /* 0 on success or non-zero on error */

STATIC FUNCTION save_c( node )
   RETURN _ENCODE( node ) /* string on success or NIL on error */

STATIC FUNCTION whitespace_cb( node, where )

   LOCAL parent        /* Parent node */
   LOCAL nLevel := -1  /* Indentation level */
   LOCAL name          /* Name of element */

   name := mxmlGetElement( node )

   IF hb_LeftEq( name, "?xml" )
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

   RETURN NIL  /* Return NIL for no added whitespace... */

STATIC FUNCTION type_cb( node )

   LOCAL cType

   IF Empty( cType := mxmlElementGetAttr( node, "type" ) )
      cType := mxmlGetElement( node )
   ENDIF

   SWITCH Lower( cType )
   CASE "custom"               /* don't forget */
   CASE "hash"
      RETURN MXML_CUSTOM
   ENDSWITCH

   RETURN MXML_TEXT
