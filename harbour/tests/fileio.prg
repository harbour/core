//
// $Id$
//

// Written by Victor Szel <info@szelvesz.hu>
// www - http://www.harbour-project.org
//
// Placed in the public domain

#include "fileio.ch"

FUNCTION Main()
   LOCAL cFileName := "TEST.TXT"
   LOCAL nFlags := FC_NORMAL

   LOCAL cBuffer
   LOCAL fhnd

   OutSpec("FCreate( cFileName, nFlags )"   , fhnd := FCreate( cFileName, nFlags ) )
   OutSpec("FWrite( fhnd, '>1234567890<' )" , FWrite( fhnd, ">1234567890<" ) )
   OutSpec("FWrite( fhnd, '(123.4567890)' )", FWrite( fhnd, "(123" + Chr(0) + "4567890)" ) )
   OutSpec("FSeek( fhnd )"                  , FSeek( fhnd ) )
   OutSpec("FSeek( fhnd, 5 )"               , FSeek( fhnd, 5 ) )
   OutSpec("FSeek( fhnd, -1, FS_SET )"      , FSeek( fhnd, -1, FS_SET ) )
   OutSpec("FSeek( fhnd, -10, FS_SET )"     , FSeek( fhnd, -10, FS_SET ) )
   OutSpec("FSeek( fhnd, -100, FS_SET )"    , FSeek( fhnd, -100, FS_SET ) )
   OutSpec("FWrite( fhnd, '!' )"            , FWrite( fhnd, "!" ) )
   OutSpec("FSeek( fhnd, 1 )"               , FSeek( fhnd, 1 ) )
   OutSpec("FWrite( fhnd, 'A' )"            , FWrite( fhnd, "A" ) )
   OutSpec("FSeek( fhnd, 2, FS_SET )"       , FSeek( fhnd, 2, FS_SET ) )
   OutSpec("FWrite( fhnd, 'B' )"            , FWrite( fhnd, "B" ) )
   OutSpec("FSeek( fhnd, 3, FS_RELATIVE )"  , FSeek( fhnd, 3, FS_RELATIVE ) )
   OutSpec("FWrite( fhnd, 'C' )"            , FWrite( fhnd, "C" ) )
   OutSpec("FSeek( fhnd, -1, FS_RELATIVE )" , FSeek( fhnd, -1, FS_RELATIVE ) )
   OutSpec("FWrite( fhnd, 'D' )"            , FWrite( fhnd, "D" ) )
   OutSpec("FSeek( fhnd, 3, FS_END )"       , FSeek( fhnd, 3, FS_END ) )
   OutSpec("FWrite( fhnd, 'E' )"            , FWrite( fhnd, "E" ) )
   OutSpec("FSeek( fhnd, -1, FS_END )"      , FSeek( fhnd, -1, FS_END ) )
   OutSpec("FWrite( fhnd, 'F' )"            , FWrite( fhnd, "F" ) )
   OutSpec("FSeek( fhnd, 0 )"               , FSeek( fhnd, 0 ) )
   cBuffer := 1000
   OutStd("cBuffer := 1000" + Chr( 13 ) + Chr( 10 ))
   OutSpec("FRead( fhnd, cBuffer )"         , FRead( fhnd, cBuffer ) )
   OutSpec("FRead( fhnd, @cBuffer, 2 )"     , FRead( fhnd, cBuffer, 2 ) )
   cBuffer := Space(4)
   OutStd("cBuffer := Space(4)" + Chr( 13 ) + Chr( 10 ))
   OutSpec("FRead( fhnd, cBuffer )"         , FRead( fhnd, cBuffer ) )
   OutSpec("FRead( fhnd, cBuffer, 2 )"      , FRead( fhnd, cBuffer, 2 ) )
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
   OutSpec("FRead( fhnd, @cBuffer, len+1 )" , FRead( fhnd, @cBuffer, Len( cBuffer ) + 1 ) )
#endif
   OutSpec("FRead( fhnd, @cBuffer, 1000 )"  , FRead( fhnd, @cBuffer, 1000 ) )
   OutSpec("FRead( fhnd, @cBuffer, 3 )"     , FRead( fhnd, @cBuffer, 3 ) )
   cBuffer := Space(100)
   OutStd("cBuffer := Space(100)" + Chr( 13 ) + Chr( 10 ))
   OutSpec("FRead( fhnd, @cBuffer, 100 )"   , FRead( fhnd, @cBuffer, 100 ) )
   OutSpec("FSeek( fhnd, 0 )"               , FSeek( fhnd, 0 ) )
   OutSpec("FReadStr( fhnd, 4 )"            , FReadStr( fhnd, 4 ) )
   OutSpec("FSeek( fhnd, 0 )"               , FSeek( fhnd, 0 ) )
   OutSpec("FReadStr( fhnd, 100 )"          , FReadStr( fhnd, 100 ) )
   OutSpec("FSeek( fhnd, -4, FS_END )"      , FSeek( fhnd, -4, FS_END ) )
   OutSpec("FReadStr( fhnd, 1 )"            , FReadStr( fhnd, 1 ) )
   OutSpec("FReadStr( fhnd, 20 )"           , FReadStr( fhnd, 20 ) )
   OutSpec("FSeek( fhnd, 0, FS_END )"       , FSeek( fhnd, 0, FS_END ) )
   OutSpec("FWrite( fhnd, '_-_-_-_-_-_-_' )", FWrite( fhnd, "_-_-_-_-_-_-_" ) )
   OutSpec("FSeek( fhnd, -4, FS_END )"      , FSeek( fhnd, -4, FS_END ) )
   OutSpec("FReadStr( fhnd, 1 )"            , FReadStr( fhnd, 1 ) )
   OutSpec("FReadStr( fhnd, 20 )"           , FReadStr( fhnd, 20 ) )
   OutSpec("FSeek( fhnd, 3, FS_END )"       , FSeek( fhnd, 3, FS_END ) )
   OutSpec("FWrite( fhnd, 'V' )"            , FWrite( fhnd, "V" ) )
   OutSpec("FSeek( fhnd, -3, FS_END )"      , FSeek( fhnd, -3, FS_END ) )
   OutSpec("FWrite( fhnd, 'W' )"            , FWrite( fhnd, "W" ) )

   OutSpec("FClose()"                       , FClose() )
   OutSpec("FClose( fhnd )"                 , FClose( fhnd ) )
   OutSpec("FClose( fhnd )"                 , FClose( fhnd ) )
   OutSpec("FErase( 'NOT_HERE.$$$' )"       , FErase( 'NOT_HERE.$$$' ) )
   OutSpec("FErase( 1 )"                    , FErase( 1 ) )
   OutSpec("FErase( 'NOT_HERE.$$$' )"       , FErase( 'NOT_HERE.$$$' ) )
   OutSpec("FRename( 'NOT_HERE.$$$', 'A' )" , FRename( 'NOT_HERE.$$$', 'A' ) )

   nFlags := FO_READWRITE

   OutSpec("FOpen( cFileName, nFlags )"     , fhnd := FOpen( cFileName, nFlags ) )
   OutSpec("FWrite( fhnd, '>1234567890<' )" , FWrite( fhnd, ">1234567890<" ) )
   OutSpec("FWrite( fhnd, '(123.4567890)' )", FWrite( fhnd, "(123" + Chr(0) + "4567890)" ) )
   OutSpec("FSeek( fhnd )"                  , FSeek( fhnd ) )
   OutSpec("FSeek( fhnd, 5 )"               , FSeek( fhnd, 5 ) )
   OutSpec("FSeek( fhnd, -1, FS_SET )"      , FSeek( fhnd, -1, FS_SET ) )
   OutSpec("FSeek( fhnd, -10, FS_SET )"     , FSeek( fhnd, -10, FS_SET ) )
   OutSpec("FSeek( fhnd, -100, FS_SET )"    , FSeek( fhnd, -100, FS_SET ) )
   OutSpec("FWrite( fhnd, '!' )"            , FWrite( fhnd, "!" ) )
   OutSpec("FSeek( fhnd, 1 )"               , FSeek( fhnd, 1 ) )
   OutSpec("FWrite( fhnd, 'A' )"            , FWrite( fhnd, "A" ) )
   OutSpec("FSeek( fhnd, 2, FS_SET )"       , FSeek( fhnd, 2, FS_SET ) )
   OutSpec("FWrite( fhnd, 'B' )"            , FWrite( fhnd, "B" ) )
   OutSpec("FSeek( fhnd, 3, FS_RELATIVE )"  , FSeek( fhnd, 3, FS_RELATIVE ) )
   OutSpec("FWrite( fhnd, 'C' )"            , FWrite( fhnd, "C" ) )
   OutSpec("FSeek( fhnd, -1, FS_RELATIVE )" , FSeek( fhnd, -1, FS_RELATIVE ) )
   OutSpec("FWrite( fhnd, 'D' )"            , FWrite( fhnd, "D" ) )
   OutSpec("FSeek( fhnd, 3, FS_END )"       , FSeek( fhnd, 3, FS_END ) )
   OutSpec("FWrite( fhnd, 'E' )"            , FWrite( fhnd, "E" ) )
   OutSpec("FSeek( fhnd, -1, FS_END )"      , FSeek( fhnd, -1, FS_END ) )
   OutSpec("FWrite( fhnd, 'F' )"            , FWrite( fhnd, "F" ) )
   OutSpec("FSeek( fhnd, 0 )"               , FSeek( fhnd, 0 ) )
   cBuffer := 1000
   OutStd("cBuffer := 1000" + Chr( 13 ) + Chr( 10 ))
   OutSpec("FRead( fhnd, cBuffer )"         , FRead( fhnd, cBuffer ) )
   OutSpec("FRead( fhnd, @cBuffer, 2 )"     , FRead( fhnd, cBuffer, 2 ) )
   cBuffer := Space(4)
   OutStd("cBuffer := Space(4)" + Chr( 13 ) + Chr( 10 ))
   OutSpec("FRead( fhnd, cBuffer )"         , FRead( fhnd, cBuffer ) )
   OutSpec("FRead( fhnd, cBuffer, 2 )"      , FRead( fhnd, cBuffer, 2 ) )
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
   OutSpec("FRead( fhnd, @cBuffer, len+1 )" , FRead( fhnd, @cBuffer, Len( cBuffer ) + 1 ) )
#endif
   OutSpec("FRead( fhnd, @cBuffer, 1000 )"  , FRead( fhnd, @cBuffer, 1000 ) )
   OutSpec("FRead( fhnd, @cBuffer, 3 )"     , FRead( fhnd, @cBuffer, 3 ) )
   cBuffer := Space(100)
   OutStd("cBuffer := Space(100)" + Chr( 13 ) + Chr( 10 ))
   OutSpec("FRead( fhnd, @cBuffer, 100 )"   , FRead( fhnd, @cBuffer, 100 ) )
   OutSpec("FSeek( fhnd, 0 )"               , FSeek( fhnd, 0 ) )
   OutSpec("FReadStr( fhnd, 4 )"            , FReadStr( fhnd, 4 ) )
   OutSpec("FSeek( fhnd, 0 )"               , FSeek( fhnd, 0 ) )
   OutSpec("FReadStr( fhnd, 100 )"          , FReadStr( fhnd, 100 ) )
   OutSpec("FSeek( fhnd, -4, FS_END )"      , FSeek( fhnd, -4, FS_END ) )
   OutSpec("FReadStr( fhnd, 1 )"            , FReadStr( fhnd, 1 ) )
   OutSpec("FReadStr( fhnd, 20 )"           , FReadStr( fhnd, 20 ) )
   OutSpec("FSeek( fhnd, 0, FS_END )"       , FSeek( fhnd, 0, FS_END ) )
   OutSpec("FWrite( fhnd, '_-_-_-_-_-_-_' )", FWrite( fhnd, "_-_-_-_-_-_-_" ) )
   OutSpec("FSeek( fhnd, -4, FS_END )"      , FSeek( fhnd, -4, FS_END ) )
   OutSpec("FReadStr( fhnd, 1 )"            , FReadStr( fhnd, 1 ) )
   OutSpec("FReadStr( fhnd, 20 )"           , FReadStr( fhnd, 20 ) )
   OutSpec("FSeek( fhnd, 3, FS_END )"       , FSeek( fhnd, 3, FS_END ) )
   OutSpec("FWrite( fhnd, 'V' )"            , FWrite( fhnd, "V" ) )
   OutSpec("FSeek( fhnd, -3, FS_END )"      , FSeek( fhnd, -3, FS_END ) )
   OutSpec("FWrite( fhnd, 'W' )"            , FWrite( fhnd, "W" ) )

   OutSpec("FClose()"                       , FClose() )
   OutSpec("FClose( fhnd )"                 , FClose( fhnd ) )
   OutSpec("FClose( fhnd )"                 , FClose( fhnd ) )
   OutSpec("FErase( 'NOT_HERE.$$$' )"       , FErase( 'NOT_HERE.$$$' ) )
   OutSpec("FErase( 1 )"                    , FErase( 1 ) )
   OutSpec("FErase( 'NOT_HERE.$$$' )"       , FErase( 'NOT_HERE.$$$' ) )
   OutSpec("FRename( 'NOT_HERE.$$$', 'A' )" , FRename( 'NOT_HERE.$$$', 'A' ) )

   OutSpec("File( cFileName )"              , File( cFileName ) )

   RETURN NIL

STATIC FUNCTION OutSpec( cWhat, xRetVal )

   OutStd( PadR( cWhat, 35 ) +;
           PadR( " e: " + LTrim( Str( FError() ) ), 9 ) +;
           PadR( " ret: " + XToStr( xRetVal ), 35 ) )
   OutStd( Chr(13) + Chr(10) )

   RETURN NIL

STATIC FUNCTION XToStr( xValue )
   LOCAL cType := ValType( xValue )

   DO CASE
   CASE cType == "C" ; RETURN "$" + xValue + "$"
   CASE cType == "N" ; RETURN LTrim( Str( xValue ) )
   CASE cType == "D" ; RETURN DToC( xValue )
   CASE cType == "L" ; RETURN iif( xValue, ".T.", ".F." )
   CASE cType == "O" ; RETURN xValue:className + " Object"
   CASE cType == "U" ; RETURN "NIL"
   CASE cType == "B" ; RETURN "{||...}"
   CASE cType == "A" ; RETURN "{...}"
   CASE cType == "M" ; RETURN xValue
   ENDCASE

   RETURN ""
