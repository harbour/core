/*
 * Testing Clipper Extended system.
 *
 * NOTE: build together with extend2.c
 *       In Harbour use command 'hbmk2 extend1.hbp'
 */

PROCEDURE Main()

   LOCAL uVar

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   ? "Testing Harbour Extended system:"
   ? "================================"
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?

   WAIT "Press any key to continue..."

   ? "Strings:"
   ? "========"
   ?
   ? '  _parc() and _retc() with Strings1( "Hello" ) =>', Strings1( "Hello" )
   ? '  _parc() and _retc() with Strings2( { "Hello" } ) =>', Strings2( { "Hello" } )
   ? '  _retclen() with Strings3( "Hello word", 5 ) =>', Strings3( "Hello", 5 )
   ? '  _parclen() with Strings4( "Hello word" ) => ', Strings4( "Hello word" )
   uVar := "Hello word"
   ? '  uVar := "Hello word"'
   Strings5( @uVar, "Harbour power!!!" )
   ? '  _storc() with Strings5( @uVar, "Harbour power!!!" ) => ', uVar
   Strings6( @uVar, "Harbour power!!!", 7 )
   ? '  _storclen() with Strings6( @uVar, "Harbour power!!!", 7 ) => ', uVar
   uVar := { "Hello word" }
   ? '  uVar := { "Hello word" }'
   Strings7( uVar, "Harbour power!!!" )
   ? '  _storc() with Strings7( uVar, "Harbour power!!!" ) => ', uVar[ 1 ]
   Strings8( uVar, "Harbour power!!!", 7 )
   ? '  _storclen() with Strings8( uVar, "Harbour power!!!", 7 ) => ', uVar[ 1 ]
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?

   WAIT "Press any key to continue..."

   ? "Logicals:"
   ? "========="
   ?
   ? '  _parl() and _retl() with Logical1( .T. ) =>', Logical1( .T. )
   ? '  _parl() and _parl() with Logical2( { .T. } ) =>', Logical2( { .T. } )
   uVar := .T.
   ? '  uVar := .T.'
   Logical3( @uVar, .F. )
   ? '  _storl() with Logical3( @uVar, .F. ) => ', uVar
   uVar := { .T. }
   ? '  uVar := { .T. }'
   Logical4( uVar, .F. )
   ? '  _storl() with Logical4( uVar, .F. ) => ', uVar[ 1 ]
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?

   WAIT "Press any key to continue..."

   ? "Dates: (SET CENTURY ON)"
   ? "======================="
   ?
   ? '  _pards() and _retds() with Date1( CToD( "2000-01-01" ) ) =>', Date1( CToD( "2000-01-01" ) )
   ? '  _pards() and retds() with Date2( { CToD( "2000-01-01" ) } ) =>', Date2( { CToD( "2000-01-01" ) } )
   uVar := CToD( "2000-01-01" )
   ? '  uVar := CToD( "2000-01-01" )'
   Date3( @uVar, CToD( "1999-12-31" ) )
   ? '  _stords() with Date3( @uVar, CToD( "1999-12-31" ) ) => ', uVar
   uVar := { CToD( "2000-01-01" ) }
   ? '  uVar := { CToD( "2000-01-01" ) }'
   Date4( uVar, CToD( "1999-12-31" ) )
   ? '  _stords() with Date4( uVar, CToD( "1999-12-31" ) ) => ', uVar[ 1 ]
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?

   WAIT "Press any key to continue..."

   ? "Numbers:"
   ? "========"
   ?
   ? '  _parni() and _retni() with Int1( 1 ) =>', Int1( 1 )
   ? '  _parni() and _retni() with Int2( { 1 } ) =>', Int2( { 1 } )
   ? '  _parnl() and _retnl() with Long1( 123456789 ) =>', Long1( 123456789 )
   ? '  _parnl() and _retnl() with Long2( { 123456789 } ) =>', Long2( { 123456789 } )
   ? '  _parnd() and _retnd() with Double1( 1234567.89 ) =>', Double1( 1234567.89 )
   ? '  _parnd() and _retnd() with Double2( { 1234567.89 } ) =>', Double2( { 1234567.89 } )
   uVar := 100
   ? '  uVar := 100'
   Int3( @uVar, 200 )
   ? '  _storni() with Int3( @uVar, 200 ) => ', uVar
   uVar := { 100 }
   ? '  uVar := { 100 }'
   Int4( uVar, 200 )
   ? '  _storni() with Int4( uVar, 200 ) => ', uVar[ 1 ]
   uVar := 123456789
   ? '  uVar := 123456789'
   Long3( @uVar, 987654321 )
   ? '  _stornl() with Long3( @uVar, 987654321 ) => ', uVar
   uVar := { 123456789 }
   ? '  uVar := { 123456789 }'
   Long4( uVar, 987654321 )
   ? '  _stornl() with Long4( uVar, 987654321 ) => ', uVar[ 1 ]
   uVar := 1234567.89
   ? '  uVar := 1234567.89'
   Double3( @uVar, 9876543.21 )
   ? '  _stornd() with Double3( @uVar, 9876543.21 ) => ', uVar
   uVar := { 1234567.89 }
   ? '  uVar := { 1234567.89 }'
   Double4( uVar, 9876543.21 )
   ? '  _stornl() with Double4( uVar, 9876543.21 ) => ', uVar[ 1 ]
   ?
   ?
   ?

   WAIT "Press any key to continue..."

   ? "Nil:"
   ? "===="
   ?
   ? '  _ret() with Nil1() =>', Nil1()
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?

   WAIT "Press any key to continue..."

   ? "Arrays:"
   ? "======="
   ?
   ? '  _reta() with Len( Arrays1( 100 ) ) =>', Len( Arrays1( 100 ) )
   ? '  _parinfa() with Arrays2( { 1, "a", .T. }, 0 ) =>', Arrays2( { 1, "a", .T. }, 0 )
   ? '  _parinfa() with Arrays2( { 1, "a", .T. }, 2 ) =>', Arrays2( { 1, "a", .T. }, 2 ), "( IT_STRING )"
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?

   WAIT "Press any key to continue..."

   ? "Parameters info:"
   ? "================"
   ?
   ? '  _pcount() with Params1( 1, "a", .T., 10 ) =>', Params1( 1, "a", .T., 10 )
   ? '  _parinfo() with Params2( 1, "a", .T., 0 ) =>', Params2( 1, "a", .T., 0 )
   ? '  _parinfo() with Params2( 1, "a", .T., 3 ) =>', Params2( 1, "a", .T., 3 ), "( IT_LOGICAL )"
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?
   ?

   WAIT "Press any key to continue..."

   ? "Undocumented features:"
   ? "======================"
   ?
   ? '  _parc( -1, ...) with Undoc2() =>', Undoc2()
   ? '  _parclen( -1, ...) with Undoc3() =>', Undoc3()
   ? '  _pards( -1, ...) with Undoc4( CToD( "2000-01-01" ) ) =>', Undoc4( CToD( "2000-01-01" ) )
   ? '  _parl( -1, ...) with Undoc5( .T. ) =>', Undoc5( .T. )
   ? '  _parnd( -1, ...) with Undoc6( 1234567.89 ) =>', Undoc6( 1234567.89 )
   ? '  _parni( -1, ...) with Undoc7( 1234 ) =>', Undoc7( 1234 )
   ? '  _parnl( -1, ...) with Undoc8( 123456789 ) =>', Undoc8( 123456789 )
   ? '  _parinfa( -1, ...) with Undoc9( 10 ) =>', Undoc9( 10 )
   ? '  _parinfo( -1 ) with Undoc10() =>', Undoc10(), "( IT_STRING )"
   ? '  _storc( szText, -1, ... ) with Undoc11( "Hello word" ) =>', Undoc11( "Hello word" )
   ? '  _storclen( szText, -1, ... ) with Undoc12( "Hello word", 7 ) =>', Undoc12( "Hello word", 7 )
   ? '  _stords( szDate, -1, ... ) with Undoc13( CToD( "2000-01-01" ) ) =>', Undoc13( CToD( "2000-01-01" ) )
   ? '  _storl( iLogical, -1 ) with Undoc14( .T. ) =>', Undoc14( .T. )
   ? '  _storni( iValue, -1 ) with Undoc15( 1234 ) =>', Undoc15( 1234 )
   ? '  _stornl( lValue, -1 ) with Undoc16( 123456789 ) =>', Undoc16( 123456789 )
   ? '  _stornd( dValue, -1 ) with Undoc17( 1234567.89 ) =>', Undoc17( 1234567.89 )
   ?
   ?
   ?
   ?
   ?

   RETURN
