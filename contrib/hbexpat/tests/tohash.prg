/* Copyright 2010 Viktor Szakats (vszakats.net/harbour) */

#require "hbexpat"

#define _D_aTree            1
#define _D_aNode            2
#define _D_MAX_             2

#define _N_aParent          1
#define _N_hChild           2
#define _N_xValue           3
#define _N_hAttr            4
#define _N_MAX_             4

REQUEST HB_CODEPAGE_UTF8EX

PROCEDURE Main( cFileName )

   LOCAL p := XML_ParserCreate()
   LOCAL aUserData
   LOCAL aNode
   LOCAL v1, v2, v3

   hb_cdpSelect( "UTF8EX" )
   hb_SetTermCP( hb_cdpTerm() )

   hb_default( @cFileName, hb_DirBase() + "test.xml" )

   ? XML_ExpatVersion()
   XML_ExpatVersionInfo( @v1, @v2, @v3 )
   ? hb_ntos( v1 ) + "." + hb_ntos( v2 ) + "." + hb_ntos( v3 )
   hb_XML_ExpatVersionInfo( @v1, @v2, @v3 )
   ? hb_ntos( v1 ) + "." + hb_ntos( v2 ) + "." + hb_ntos( v3 )

   IF Empty( p )
      ? "Couldn't allocate memory for parser"
      ErrorLevel( -1 )
      RETURN
   ENDIF

   aNode := Array( _N_MAX_ )
   aNode[ _N_hChild ] := { => }
   aNode[ _N_xValue ] := NIL
   aNode[ _N_hAttr ] := NIL

   aUserData := Array( _D_MAX_ )
   aUserData[ _D_aTree ] := aNode
   aUserData[ _D_aNode ] := aUserData[ _D_aTree ]

   aNode[ _N_aParent ] := aUserData[ _D_aTree ]

   ? XML_GetUserData( p )
   XML_SetUserData( p, aUserData )
   ? ValType( XML_GetUserData( p ) )
   XML_SetElementHandler( p, {| x, e, a | cb_start( x, e, a ) }, {| x | cb_end( x ) } )
   XML_SetCharacterDataHandler( p, {| x, d | cb_data( x, d ) } )
   XML_SetUnknownEncodingHandler( p, {| x, e, i | cb_unknownencoding( x, e, i ) } )

   IF XML_Parse( p, MemoRead( cFileName ), .T. ) == HB_XML_STATUS_ERROR
      ? hb_StrFormat( e"Parse error at line %1$d:\n%2$s\n", ;
         XML_GetCurrentLineNumber( p ), ;
         XML_ErrorString( XML_GetErrorCode( p ) ) )
      ErrorLevel( -1 )
      RETURN
   ENDIF

   DUMP( aUserData[ _D_aTree ], 0 )

   hb_MemoWrit( "json_raw.txt", hb_jsonEncode( aUserData[ _D_aTree ], .F. ) )
   hb_MemoWrit( "json_hum.txt", hb_jsonEncode( aUserData[ _D_aTree ], .T. ) )

   RETURN

STATIC PROCEDURE DUMP( hTree, n )

   LOCAL aEl
   LOCAL aNode

   FOR EACH aEl IN hTree[ _N_hChild ]
      FOR EACH aNode IN aEl
         ? Replicate( "  ", n ) + aEl:__enumKey() + ":", "'" + aNode[ _N_xValue ] + "'" + DUMPATTR( aNode[ _N_hAttr ] )
         DUMP( aNode, n + 1 )
      NEXT
   NEXT

   RETURN

STATIC FUNCTION DUMPATTR( hAttr )

   LOCAL s := "", cValue

   IF ! Empty( hAttr )
      s += " ("
      FOR EACH cValue IN hAttr
         s += cValue:__enumKey() + "='" + cValue + "' "
      NEXT
      s := RTrim( s ) + ")"
   ENDIF

   RETURN s

STATIC FUNCTION cb_unknownencoding( xEData, cEncoding, aMap )

   LOCAL aMyMap

   HB_SYMBOL_UNUSED( xEData )

   aMyMap := hb_XML_get_unicode_table( cEncoding )
   IF ! Empty( aMyMap )
      ACopy( aMyMap, aMap )
      RETURN HB_XML_STATUS_OK
   ENDIF

   RETURN HB_XML_STATUS_ERROR

STATIC PROCEDURE cb_start( aUserData, cElement, aAttrList )

   LOCAL aAttr
   LOCAL aNode
   LOCAL aNewNode

   aNode := aUserData[ _D_aNode ]

   aNewNode := Array( _N_MAX_ )
   aNewNode[ _N_aParent ] := aNode
   aNewNode[ _N_hChild ] := { => }
   aNewNode[ _N_xValue ] := ""
   aNewNode[ _N_hAttr ] := { => }

   IF cElement $ aNode[ _N_hChild ]
      AAdd( aNode[ _N_hChild ][ cElement ], aNewNode )
   ELSE
      aNode[ _N_hChild ][ cElement ] := { aNewNode }
   ENDIF

   FOR EACH aAttr IN aAttrList
      aNewNode[ _N_hAttr ][ aAttr[ HB_XML_ATTR_cName ] ] := aAttr[ HB_XML_ATTR_cValue ]
   NEXT

   aUserData[ _D_aNode ] := aNewNode

   RETURN

STATIC PROCEDURE cb_end( aUserData )

   aUserData[ _D_aNode ] := aUserData[ _D_aNode ][ _N_aParent ]

   RETURN

STATIC PROCEDURE cb_data( aUserData, cData )

   aUserData[ _D_aNode ][ _N_xValue ] += cData

   /* Still not perfect. In unlucky case, it can strip valid whitespace */
   IF Empty( aUserData[ _D_aNode ][ _N_xValue ] )
      aUserData[ _D_aNode ][ _N_xValue ] := AllTrim( aUserData[ _D_aNode ][ _N_xValue ] )
   ENDIF

   RETURN
