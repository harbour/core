/*
 * Harbour Project source code:
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour)
 * www - http://harbour-project.org
 *
 */

#require "hbexpat"

PROCEDURE Main( cFileName )

   LOCAL p := XML_ParserCreate()
   LOCAL aUserData
   LOCAL v1, v2, v3

   hb_default( @cFileName, hb_DirBase() + "test.xml" )

   OutStd( XML_ExpatVersion() + hb_eol() )
   XML_ExpatVersionInfo( @v1, @v2, @v3 )
   OutStd( hb_ntos( v1 ) + "." + hb_ntos( v2 ) + "." + hb_ntos( v3 ) + hb_eol() )
   hb_XML_ExpatVersionInfo( @v1, @v2, @v3 )
   OutStd( hb_ntos( v1 ) + "." + hb_ntos( v2 ) + "." + hb_ntos( v3 ) + hb_eol() )

   IF Empty( p )
      OutErr( "Couldn't allocate memory for parser" + hb_eol() )
      ErrorLevel( -1 )
      RETURN
   ENDIF

   aUserData := Array( 1 )
   aUserData[ 1 ] := 1

   OutStd( XML_GetUserData( p ) ) ; OutStd( hb_eol() )
   XML_SetUserData( p, aUserData )
   OutStd( ValType( XML_GetUserData( p ) ) + hb_eol() )
   XML_SetElementHandler( p, {| x, e, a | cb_start( x, e, a ) }, {| x, e | cb_end( x, e ) } )
   XML_SetCharacterDataHandler( p, {| x, d | cb_data( x, d ) } )

   IF XML_Parse( p, MemoRead( cFileName ), .T. ) == HB_XML_STATUS_ERROR
      OutErr( hb_StrFormat( e"Parse error at line %1$d:\n%2$s\n", ;
         XML_GetCurrentLineNumber( p ), ;
         XML_ErrorString( XML_GetErrorCode( p ) ) ) )
      ErrorLevel( -1 )
      RETURN
   ENDIF

   RETURN

STATIC PROCEDURE cb_start( aUserData, cElement, aAttr )

   LOCAL aItem

   OutStd( Replicate( "  ", aUserData[ 1 ] ), cElement )

   IF ! Empty( aAttr )
      FOR EACH aItem IN aAttr
         OutStd( " " + aItem[ HB_XML_ATTR_cName ] + "='" + aItem[ HB_XML_ATTR_cValue ] + "'" )
      NEXT
   ENDIF

   OutStd( hb_eol() )

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
      OutStd( ">" + cData + "<" )
   ENDIF

   RETURN
