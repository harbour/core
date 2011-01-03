
#include "hbmxml.ch"

STATIC s_mxml_error := .f.
STATIC s_mxml_error_msg := ""

PROCEDURE main()

   LOCAL xml

   mxmlSetErrorCallback( @my_mxmlError() )

   IF hb_fileExists( 'rem.xml' )
      xml := simplexml_load_file( 'rem.xml' )
   ELSE
      RETURN -1
   ENDIF

   IF ! s_mxml_error
      OutStd( asXML( xml ), hb_eol() )
   ENDIF

   mxmlDelete( xml )

   IF hb_fileExists( 'rem_err.xml' )
      xml := simplexml_load_file( 'rem_err.xml' )

      IF s_mxml_error
         OutErr( "hbmxml:", s_mxml_error_msg, hb_eol() )
      ENDIF
   ENDIF

   RETURN

PROCEDURE my_mxmlError( cErrorMsg )

   s_mxml_error_msg := cErrorMsg
   s_mxml_error := .t.

   RETURN

STATIC FUNCTION simplexml_load_file( file )

   RETURN mxmlLoadString( NIL, hb_memoRead( file ), @type_cb() )

STATIC FUNCTION asXML( xml )

   LOCAL cText := "", c
   LOCAL wt := 3
   LOCAL node, subnode

   node := mxmlFindElement( xml, xml, "note", NIL, NIL, MXML_DESCEND )
   IF Empty( node )
      RETURN ""
   ENDIF

   subnode := mxmlGetFirstChild( node )
   DO WHILE ! Empty( subnode := mxmlGetNext( subnode ) )
      IF mxmlGetType( subnode ) == MXML_ELEMENT 
         IF mxmlGetElement( subnode ) == "body"
            c := mxmlGetOpaque( subnode )
         ELSE
            c := mxmlGetText( subnode, @wt )
         ENDIF
         cText += ( c + " " )
      ENDIF
   ENDDO   

   RETURN cText

FUNCTION type_cb( node )

   LOCAL nResult
   LOCAL cType

   /* You can lookup attributes and/or use the element name, hierarchy, etc... */
 
   IF Empty( cType := mxmlElementGetAttr( node, "type" ) )
      cType := mxmlGetElement( node )
   ENDIF

   SWITCH Lower( cType )
   CASE "note"
   CASE "to"
   CASE "from"
   CASE "heading"
      nResult := MXML_TEXT
      EXIT

   CASE "body"
   CASE "opaque"
      nResult := MXML_OPAQUE
      EXIT

   OTHERWISE
      nResult := MXML_IGNORE
      EXIT
   ENDSWITCH

   RETURN nResult
