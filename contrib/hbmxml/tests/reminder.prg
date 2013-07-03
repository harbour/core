/* Copyright (c) 2011 Petr Chornyj */

#require "hbmxml"

STATIC s_mxml_error := .F.
STATIC s_mxml_error_msg := ""

PROCEDURE Main()

   LOCAL xml
   LOCAL cFileName

   mxmlSetErrorCallback( @my_mxmlError() )

   IF hb_FileExists( cFileName := hb_DirBase() + "rem.xml" )
      xml := simplexml_load_file( cFileName )

      IF ! s_mxml_error
         OutStd( asXML( xml ), hb_eol() )
      ENDIF

      mxmlDelete( xml )
   ENDIF

   IF hb_FileExists( cFileName := hb_DirBase() + "rem_err.xml" )
      xml := simplexml_load_file( cFileName )

      IF s_mxml_error
         OutErr( "hbmxml:", s_mxml_error_msg, hb_eol() )
      ELSE
         OutStd( asXML( xml ), hb_eol() )
      ENDIF
   ENDIF

   RETURN

STATIC PROCEDURE my_mxmlError( cErrorMsg )

   s_mxml_error_msg := cErrorMsg
   s_mxml_error := .T.

   RETURN

STATIC FUNCTION simplexml_load_file( file )
   RETURN mxmlLoadString( NIL, hb_MemoRead( file ), @type_cb() )

STATIC FUNCTION asXML( xml )

   LOCAL cText := "", c
   LOCAL wt := 3
   LOCAL node, subnode

   node := mxmlFindElement( xml, xml, "note", NIL, NIL, MXML_DESCEND )
   IF Empty( node )
      RETURN ""
   ENDIF

   subnode := mxmlGetFirstChild( node )
   DO WHILE ! Empty( subnode := mxmlGetNextSibling( subnode ) )
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

STATIC FUNCTION type_cb( node )

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
