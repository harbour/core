// Testing Harbour Extended system.
//
// NOTE: compile extend1.prg and extend2.c and link both files

function Main()

   SET CENTURY ON

   QOut( "Testing Harbour Extended system:" )
   QOut( "================================" )
   QOut( "" );

   QOut( "Strings:" )
   QOut( '  _parc() and _retc() with Strings1( "Hello" ) =>', Strings1( "Hello" ) )
   QOut( '  _parc() and _retc() with Strings2( { "Hello" } ) =>', Strings2( { "Hello" } ) )
   QOut( '  _parclen() with Length( "Hello word" ) => ', Length( "Hello word" ) )
   QOut( "" );

   QOut( "Logicals:" )
   QOut( '  _parl() and _retl() with Logical1( .T. ) =>', Logical1( .T. ) )
   QOut( '  _parl() and _parl() with Logical2( { .T. } ) =>', Logical2( { .T. } ) )
   QOut( "" );

   QOut( "Dates: (SET CENTURY ON)" )
   QOut( '  _pards() and _retds() with Date1( CToD( "01/01/2000" ) ) =>', Date1( CToD( "01/01/2000" ) ) )
   QOut( '  _pards() and retds() with Date2( { CToD( "01/01/2000" ) } ) =>', Date2( { CToD( "01/01/2000" ) } ) )
   QOut( "" );

   QOut( "Numbers:" )
   QOut( '  _parni() and _retni() with Int1( 1 ) =>', Int1( 1 ) )
   QOut( '  _parni() and _retni() with Int2( { 1 } ) =>', Int2( { 1 } ) )
   QOut( '  _parnl() and _retnl() with Long1( 123456789 ) =>', Long1( 123456789 ) )
   QOut( '  _parnl() and _retnl() with Long2( { 123456789 } ) =>', Long2( { 123456789 } ) )
   QOut( '  _parnd() and _retnd() with Double1( 1234567.89 ) =>', Double1( 1234567.89 ) )
   QOut( '  _parnd() and _retnd() with Double2( { 1234567.89 } ) =>', Double2( { 1234567.89 } ) )
   QOut( "" );

   __Accept( "Press return to continue..." )

   QOut( "Arrays:" )
   QOut( '  _parinfa() with Arrays( { 1, "a", .T. }, 0 ) =>', Arrays( { 1, "a", .T. }, 0 ) )
   QOut( '  _parinfa() with Arrays( { 1, "a", .T. }, 2 ) =>', Arrays( { 1, "a", .T. }, 2 ), "( IT_STRING )" )
   QOut( "" );

   QOut( "Parameters info:" )
   QOut( '  _parinfo() with Params( 1, "a", .T., 0 ) =>', Params( 1, "a", .T., 0 ) )
   QOut( '  _parinfo() with Params( 1, "a", .T., 3 ) =>', Params( 1, "a", .T., 3 ), "( IT_LOGICAL )" )
   QOut( "" );

return nil
