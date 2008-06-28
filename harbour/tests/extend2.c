/*
 * $Id$
 */

/*
 * Testing CLIPPER Extended system.
 *
 * NOTE: compile extend1.prg and extend2.c and link both files
 */

#include <string.h>
#include "extend.api"

#ifdef __HARBOUR__
   #include "hbundoc.api"
#endif

CLIPPER HB_STRINGS1()
{
   _retc( _parc( 1 ) );
}

CLIPPER HB_STRINGS2()
{
   _retc( _parc( 1, 1 ) );
}

CLIPPER HB_STRINGS3()
{
   _retclen( _parc( 1 ), _parnl( 2 ) );
}

CLIPPER HB_STRINGS4()
{
   _retni( _parclen( 1 ) );
}

CLIPPER HB_STRINGS5()
{
   _storc( _parc( 2 ), 1 );
}

CLIPPER HB_STRINGS6()
{
   _storclen( _parc( 2 ), _parnl( 3 ), 1 );
}

CLIPPER HB_STRINGS7()
{
   _storc( _parc( 2 ), 1, 1 );
}

CLIPPER HB_STRINGS8()
{
   _storclen( _parc( 2 ), _parnl( 3 ), 1, 1 );
}

CLIPPER HB_LOGICAL1()
{
   _retl( _parl( 1 ) );
}

CLIPPER HB_LOGICAL2()
{
   _retl( _parl( 1, 1 ) );
}

CLIPPER HB_LOGICAL3()
{
   _storl( _parl( 2 ), 1 );
}

CLIPPER HB_LOGICAL4()
{
   _storl( _parl( 2 ), 1, 1 );
}

CLIPPER HB_DATE1()
{
   _retds( _pards( 1 ) );
}

CLIPPER HB_DATE2()
{
   _retds( _pards( 1, 1 ) );
}

CLIPPER HB_DATE3()
{
   _stords( _pards( 2 ), 1 );
}

CLIPPER HB_DATE4()
{
   _stords( _pards( 2 ), 1, 1 );
}

CLIPPER HB_INT1()
{
   _retni( _parni( 1 ) );
}

CLIPPER HB_INT2()
{
   _retni( _parni( 1, 1 ) );
}

CLIPPER HB_INT3()
{
   _storni( _parni( 2 ), 1 );
}

CLIPPER HB_INT4()
{
   _storni( _parni( 2 ), 1, 1 );
}

CLIPPER HB_LONG1()
{
   _retnl( _parnl( 1 ) );
}

CLIPPER HB_LONG2()
{
   _retnl( _parnl( 1, 1 ) );
}

CLIPPER HB_LONG3()
{
   _stornl( _parnl( 2 ), 1 );
}

CLIPPER HB_LONG4()
{
   _stornl( _parnl( 2 ), 1, 1 );
}

CLIPPER HB_DOUBLE1()
{
   _retnd( _parnd( 1 ) );
}

CLIPPER HB_DOUBLE2()
{
   _retnd( _parnd( 1, 1 ) );
}

CLIPPER HB_DOUBLE3()
{
   _stornd( _parnd( 2 ), 1 );
}

CLIPPER HB_DOUBLE4()
{
   _stornd( _parnd( 2 ), 1, 1 );
}

CLIPPER HB_NIL1()
{
   _ret();
}

CLIPPER HB_ARRAYS1()
{
   _reta( _parnl( 1 ) );
}

CLIPPER HB_ARRAYS2()
{
   _retnl( _parinfa( 1, _parni( 2 ) ) );
}

CLIPPER HB_PARAMS1()
{
   _retni( _pcount() );
}

CLIPPER HB_PARAMS2()
{
   _retni( _parinfo( _parni( 4 ) ) );
}

CLIPPER HB_UNDOC2()
{
   char szText[ 25 ];

   _retc( "Hello word" );
   hb_strncpy( szText, _parc( -1 ), sizeof( szText ) - 1 );
   szText[ 5 ] = 0;
   _retc( szText );
}

CLIPPER HB_UNDOC3()
{
   _retc( "Hello word" );
   _retnl( _parclen( -1 ) );
}

CLIPPER HB_UNDOC4()
{
   char szText[ 25 ];

   _retds( _pards( 1 ) );
   hb_strncpy( szText, _pards( -1 ), sizeof( szText ) - 1 );
   szText[ 3 ] = '1';
   _retds( szText );
}

CLIPPER HB_UNDOC5()
{
   _retl( _parl( 1 ) );
   _retl( _parl( -1 ) - 1 );
}

CLIPPER HB_UNDOC6()
{
   _retnd( _parnd( 1 ) );
   _retnd( _parnd( -1 ) - 1234567 );
}

CLIPPER HB_UNDOC7()
{
   _retni( _parni( 1 ) );
   _retni( _parni( -1 ) / 10 );
}

CLIPPER HB_UNDOC8()
{
   _retnl( _parnl( 1 ) );
   _retnl( _parnl( -1 ) * 10 );
}

CLIPPER HB_UNDOC9()
{
   _reta( _parni( 1 ) );
   _retnl( _parinfa( -1, 0 ) );
}

CLIPPER HB_UNDOC10()
{
   _retc( "CLIPPER power!!!" );
   _retni( _parinfo( -1 ) );
}

CLIPPER HB_UNDOC11()
{
   _retc( _parc( 1 ) );
   _storc( "CLIPPER power!!!", -1 );
}

CLIPPER HB_UNDOC12()
{
   _retc( _parc( 1 ) );
   _storclen( "CLIPPER power!!!", _parni( 2 ), -1 );
}

CLIPPER HB_UNDOC13()
{
   _retds( _pards( 1 ) );
   _stords( "20010101", -1 );
}

CLIPPER HB_UNDOC14()
{
   _retl( _parl( 1 ) );
   _storl( 0, -1 );
}

CLIPPER HB_UNDOC15()
{
   _retni( _parni( 1 ) );
   _storni( 4321, -1 );
}

CLIPPER HB_UNDOC16()
{
   _retnl( _parnl( 1 ) );
   _stornl( 987654321, -1 );
}

CLIPPER HB_UNDOC17()
{
   _retnd( _parnd( 1 ) );
   _stornd( 9876543.21, -1 );
}
