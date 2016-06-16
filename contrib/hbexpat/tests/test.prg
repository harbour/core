/* Copyright 2010 Viktor Szakats (vszakats.net/harbour) */

#require "hbexpat"

REQUEST HB_CODEPAGE_UTF8EX

PROCEDURE Main( cFileName )

   LOCAL p := XML_ParserCreate()
   LOCAL aUserData
   LOCAL v1, v2, v3

   hb_cdpSelect( "UTF8EX" )
   hb_SetTermCP( hb_cdpTerm() )

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

   aUserData := Array( 1 )
   aUserData[ 1 ] := 1

   ? XML_GetUserData( p )
   XML_SetUserData( p, aUserData )
   ? ValType( XML_GetUserData( p ) )
   XML_SetElementHandler( p, {| x, e, a | cb_start( x, e, a ) }, {| x, e | cb_end( x, e ) } )
   XML_SetCharacterDataHandler( p, {| x, d | cb_data( x, d ) } )

   IF XML_Parse( p, MemoRead( hb_defaultValue( cFileName, hb_DirBase() + "test.xml" ) ), .T. ) == HB_XML_STATUS_ERROR
      ? hb_StrFormat( e"Parse error at line %1$d:\n%2$s", ;
         XML_GetCurrentLineNumber( p ), ;
         XML_ErrorString( XML_GetErrorCode( p ) ) )
      ErrorLevel( -1 )
      RETURN
   ENDIF

   RETURN

STATIC PROCEDURE cb_start( aUserData, cElement, aAttr )

   LOCAL aItem

   ? Replicate( "  ", aUserData[ 1 ] ), cElement

   IF ! Empty( aAttr )
      FOR EACH aItem IN aAttr
         ?? "", aItem[ HB_XML_ATTR_cName ] + "='" + aItem[ HB_XML_ATTR_cValue ] + "'"
      NEXT
   ENDIF

   ++aUserData[ 1 ]

   RETURN

STATIC PROCEDURE cb_end( aUserData, cElement )

   HB_SYMBOL_UNUSED( aUserData )
   HB_SYMBOL_UNUSED( cElement )

   --aUserData[ 1 ]

   RETURN

STATIC PROCEDURE cb_data( aUserData, cData )

   HB_SYMBOL_UNUSED( aUserData )

   IF ! Empty( cData )
      ?? "", "'" + cData + "'"
   ENDIF

   RETURN
