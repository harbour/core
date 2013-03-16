/*
 * Testing Clipper Extended system.
 *
 * NOTE: build together with extend1.prg
 *       In Harbour use command 'hbmk2 extend1.hbp'
 */

#include <string.h>
#include "extend.api"

#if defined(__HARBOUR__)
   #include "hbapi.h"
   #include "hbundoc.api"
   #define CLIPFUN( funcname )              HB_FUNC( funcname )
   #define CLIPFUNL( funcname, funcshort )  HB_FUNC( funcname )
#elif defined(__CLIPPER__)
   #define CLIPFUN( funcname )              CLIPPER funcname ( void )
   #define CLIPFUNL( funcname, funcshort )  CLIPPER funcshort ( void )
#endif

CLIPFUN( STRINGS1 )
{
   _retc( _parc( 1 ) );
}

CLIPFUN( STRINGS2 )
{
   _retc( _parc( 1, 1 ) );
}

CLIPFUN( STRINGS3 )
{
   _retclen( _parc( 1 ), _parnl( 2 ) );
}

CLIPFUN( STRINGS4 )
{
   _retni( _parclen( 1 ) );
}

CLIPFUN( STRINGS5 )
{
   _storc( _parc( 2 ), 1 );
}

CLIPFUN( STRINGS6 )
{
   _storclen( _parc( 2 ), _parnl( 3 ), 1 );
}

CLIPFUN( STRINGS7 )
{
   _storc( _parc( 2 ), 1, 1 );
}

CLIPFUN( STRINGS8 )
{
   _storclen( _parc( 2 ), _parnl( 3 ), 1, 1 );
}

CLIPFUN( LOGICAL1 )
{
   _retl( _parl( 1 ) );
}

CLIPFUN( LOGICAL2 )
{
   _retl( _parl( 1, 1 ) );
}

CLIPFUN( LOGICAL3 )
{
   _storl( _parl( 2 ), 1 );
}

CLIPFUN( LOGICAL4 )
{
   _storl( _parl( 2 ), 1, 1 );
}

CLIPFUN( DATE1 )
{
   _retds( _pards( 1 ) );
}

CLIPFUN( DATE2 )
{
   _retds( _pards( 1, 1 ) );
}

CLIPFUN( DATE3 )
{
   _stords( _pards( 2 ), 1 );
}

CLIPFUN( DATE4 )
{
   _stords( _pards( 2 ), 1, 1 );
}

CLIPFUN( INT1 )
{
   _retni( _parni( 1 ) );
}

CLIPFUN( INT2 )
{
   _retni( _parni( 1, 1 ) );
}

CLIPFUN( INT3 )
{
   _storni( _parni( 2 ), 1 );
}

CLIPFUN( INT4 )
{
   _storni( _parni( 2 ), 1, 1 );
}

CLIPFUN( LONG1 )
{
   _retnl( _parnl( 1 ) );
}

CLIPFUN( LONG2 )
{
   _retnl( _parnl( 1, 1 ) );
}

CLIPFUN( LONG3 )
{
   _stornl( _parnl( 2 ), 1 );
}

CLIPFUN( LONG4 )
{
   _stornl( _parnl( 2 ), 1, 1 );
}

CLIPFUN( DOUBLE1 )
{
   _retnd( _parnd( 1 ) );
}

CLIPFUN( DOUBLE2 )
{
   _retnd( _parnd( 1, 1 ) );
}

CLIPFUN( DOUBLE3 )
{
   _stornd( _parnd( 2 ), 1 );
}

CLIPFUN( DOUBLE4 )
{
   _stornd( _parnd( 2 ), 1, 1 );
}

CLIPFUN( NIL1 )
{
   _ret();
}

CLIPFUN( ARRAYS1 )
{
   _reta( _parnl( 1 ) );
}

CLIPFUN( ARRAYS2 )
{
   _retnl( _parinfa( 1, _parni( 2 ) ) );
}

CLIPFUN( PARAMS1 )
{
   _retni( _pcount() );
}

CLIPFUN( PARAMS2 )
{
   _retni( _parinfo( _parni( 4 ) ) );
}

CLIPFUN( UNDOC2 )
{
   char szText[ 25 ];

   _retc( "Hello word" );
   hb_strncpy( szText, _parc( -1 ), sizeof( szText ) - 1 );
   szText[ 5 ] = 0;
   _retc( szText );
}

CLIPFUN( UNDOC3 )
{
   _retc( "Hello word" );
   _retnl( _parclen( -1 ) );
}

CLIPFUN( UNDOC4 )
{
   char szText[ 25 ];

   _retds( _pards( 1 ) );
   hb_strncpy( szText, _pards( -1 ), sizeof( szText ) - 1 );
   szText[ 3 ] = '1';
   _retds( szText );
}

CLIPFUN( UNDOC5 )
{
   _retl( _parl( 1 ) );
   _retl( _parl( -1 ) - 1 );
}

CLIPFUN( UNDOC6 )
{
   _retnd( _parnd( 1 ) );
   _retnd( _parnd( -1 ) - 1234567 );
}

CLIPFUN( UNDOC7 )
{
   _retni( _parni( 1 ) );
   _retni( _parni( -1 ) / 10 );
}

CLIPFUN( UNDOC8 )
{
   _retnl( _parnl( 1 ) );
   _retnl( _parnl( -1 ) * 10 );
}

CLIPFUN( UNDOC9 )
{
   _reta( _parni( 1 ) );
   _retnl( _parinfa( -1, 0 ) );
}

CLIPFUN( UNDOC10 )
{
   _retc( "Clipper power!!!" );
   _retni( _parinfo( -1 ) );
}

CLIPFUN( UNDOC11 )
{
   _retc( _parc( 1 ) );
   _storc( "Clipper power!!!", -1 );
}

CLIPFUN( UNDOC12 )
{
   _retc( _parc( 1 ) );
   _storclen( "Clipper power!!!", _parni( 2 ), -1 );
}

CLIPFUN( UNDOC13 )
{
   _retds( _pards( 1 ) );
   _stords( "20010101", -1 );
}

CLIPFUN( UNDOC14 )
{
   _retl( _parl( 1 ) );
   _storl( 0, -1 );
}

CLIPFUN( UNDOC15 )
{
   _retni( _parni( 1 ) );
   _storni( 4321, -1 );
}

CLIPFUN( UNDOC16 )
{
   _retnl( _parnl( 1 ) );
   _stornl( 987654321, -1 );
}

CLIPFUN( UNDOC17 )
{
   _retnd( _parnd( 1 ) );
   _stornd( 9876543.21, -1 );
}
