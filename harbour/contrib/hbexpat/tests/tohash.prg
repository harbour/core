/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
 * www - http://harbour-project.org
 *
 */

#include "hbexpat.ch"

#define _D_aTree            1
#define _D_aNode            2
#define _D_MAX_             2

#define _N_aParent          1
#define _N_hChild           2
#define _N_xValue           3
#define _N_hAttr            4
#define _N_MAX_             4

#define _A_cName            1
#define _A_cValue           2

PROCEDURE Main( cFileName )
   LOCAL p := XML_ParserCreate()
   LOCAL aUserData
   LOCAL aNode
   LOCAL v1, v2, v3

   IF cFileName == NIL
      cFileName := "setup.ui"
   ENDIF

   OutStd( XML_ExpatVersion() + hb_osNewLine() )
   XML_ExpatVersionInfo( @v1, @v2, @v3 )
   OutStd( hb_ntos( v1 ) + "." + hb_ntos( v2 ) + "." + hb_ntos( v3 ) + hb_osNewLine() )
   hb_XML_ExpatVersionInfo( @v1, @v2, @v3 )
   OutStd( hb_ntos( v1 ) + "." + hb_ntos( v2 ) + "." + hb_ntos( v3 ) + hb_osNewLine() )

   IF Empty( p )
      OutErr( "Couldn't allocate memory for parser" + hb_osNewLine() )
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

   OutStd( XML_GetUserData( p ) ) ; OutStd( hb_osNewLine() )
   XML_SetUserData( p, aUserData )
   OutStd( ValType( XML_GetUserData( p ) ) + hb_osNewLine() )
   XML_SetElementHandler( p, {| x, e, a | cb_start( x, e, a ) }, {| x | cb_end( x ) } )
   XML_SetCharacterDataHandler( p, {| x, d | cb_data( x, d ) } )
   XML_SetUnknownEncodingHandler( p, {| x, e, i | cb_unknownencoding( x, e, i ) } )

   IF XML_Parse( p, MemoRead( cFileName ), .T. ) == HB_XML_STATUS_ERROR
      OutErr( hb_StrFormat( e"Parse error at line %s:\n%s\n",;
                 hb_ntos( XML_GetCurrentLineNumber( p ) ),;
                 XML_ErrorString( XML_GetErrorCode( p ) ) ) )
      ErrorLevel( -1 )
      RETURN
   ENDIF

   DUMP( aUserData[ _D_aTree ], 0 )
   hb_MemoWrit( "json.txt", hb_jsonEncode( aUserData[ _D_aTree ] ) )

   RETURN

PROCEDURE DUMP( hTree, n )
   LOCAL aEl
   LOCAL aNode

   FOR EACH aEl IN hTree[ _N_hChild ]
      FOR EACH aNode IN aEl
         OutStd( Replicate( "  ", n ) + aEl:__enumKey() + ": '" + aNode[ _N_xValue ] + "'" + DUMPATTR( aNode[ _N_hAttr ] ) + hb_osNewLine() )
         DUMP( aNode, n + 1 )
      NEXT
   NEXT

   RETURN

FUNCTION DUMPATTR( hAttr )
   LOCAL s := "", cValue
   IF ! Empty( hAttr )
      s += " ("
      FOR EACH cValue IN hAttr
         s += cValue:__enumKey() + "='" + cValue + "' "
      NEXT
      s := RTrim( s ) + ")"
   ENDIF
   RETURN s

FUNCTION cb_unknownencoding( xEData, cEncoding, aMap )
   LOCAL aMyMap

   HB_SYMBOL_UNUSED( xEData )

   aMyMap := hb_xml_get_unicode_table( cEncoding )
   IF ! Empty( aMyMap )
      ACopy( aMyMap, aMap )
      RETURN HB_XML_STATUS_OK
   ENDIF

   RETURN HB_XML_STATUS_ERROR

PROCEDURE cb_start( aUserData, cElement, aAttrList )
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
      aNewNode[ _N_hAttr ][ aAttr[ _A_cName ] ] := aAttr[ _A_cValue ]
   NEXT

   aUserData[ _D_aNode ] := aNewNode

   RETURN

PROCEDURE cb_end( aUserData )

   aUserData[ _D_aNode ] := aUserData[ _D_aNode ][ _N_aParent ]

   RETURN

PROCEDURE cb_data( aUserData, cData )

   aUserData[ _D_aNode ][ _N_xValue ] += cData

   /* Still not perfect. In unlucky case, it can strip valid whitespace */
   IF Empty( aUserData[ _D_aNode ][ _N_xValue ] )
      aUserData[ _D_aNode ][ _N_xValue ] := AllTrim( aUserData[ _D_aNode ][ _N_xValue ] )
   ENDIF

   RETURN
