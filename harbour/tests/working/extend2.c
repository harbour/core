// Testing Harbour Extended system.
//
// NOTE: compile extend1.prg and extend2.c and link both files
#include <extend.api>

HARBOUR HB_STRINGS1()
{
   _retc( _parc( 1 ) );
}

HARBOUR HB_STRINGS2()
{
   _retc( _parc( 1, 1 ) );
}

HARBOUR HB_STRINGS3()
{
   _retclen( _parc( 1 ), _parnl( 2 ) );
}

HARBOUR HB_STRINGS4()
{
   _retni( _parclen( 1 ) );
}

HARBOUR HB_STRINGS5()
{
   _storc( _parc( 2 ), 1 );
}

HARBOUR HB_STRINGS6()
{
   _storclen( _parc( 2 ), _parnl( 3 ), 1 );
}

HARBOUR HB_STRINGS7()
{
   _storc( _parc( 2 ), 1, 1 );
}

HARBOUR HB_STRINGS8()
{
   _storclen( _parc( 2 ), _parnl( 3 ), 1, 1 );
}

HARBOUR HB_LOGICAL1()
{
   _retl( _parl( 1 ) );
}

HARBOUR HB_LOGICAL2()
{
   _retl( _parl( 1, 1 ) );
}

HARBOUR HB_LOGICAL3()
{
   _storl( _parl( 2 ), 1 );
}

HARBOUR HB_LOGICAL4()
{
   _storl( _parl( 2 ), 1, 1 );
}

HARBOUR HB_DATE1()
{
   _retds( _pards( 1 ) );
}

HARBOUR HB_DATE2()
{
   _retds( _pards( 1, 1 ) );
}

HARBOUR HB_DATE3()
{
   _stords( _pards( 2 ), 1 );
}

HARBOUR HB_DATE4()
{
   _stords( _pards( 2 ), 1, 1 );
}

HARBOUR HB_INT1()
{
   _retni( _parni( 1 ) );
}

HARBOUR HB_INT2()
{
   _retni( _parni( 1, 1 ) );
}

HARBOUR HB_INT3()
{
   _storni( _parni( 2 ), 1 );
}

HARBOUR HB_INT4()
{
   _storni( _parni( 2 ), 1, 1 );
}

HARBOUR HB_LONG1()
{
   _retnl( _parnl( 1 ) );
}

HARBOUR HB_LONG2()
{
   _retnl( _parnl( 1, 1 ) );
}

HARBOUR HB_LONG3()
{
   _stornl( _parnl( 2 ), 1 );
}

HARBOUR HB_LONG4()
{
   _stornl( _parnl( 2 ), 1, 1 );
}

HARBOUR HB_DOUBLE1()
{
   _retnd( _parnd( 1 ) );
}

HARBOUR HB_DOUBLE2()
{
   _retnd( _parnd( 1, 1 ) );
}

HARBOUR HB_DOUBLE3()
{
   _stornd( _parnd( 2 ), 1 );
}

HARBOUR HB_DOUBLE4()
{
   _stornd( _parnd( 2 ), 1, 1 );
}

HARBOUR HB_NIL1()
{
   _ret();
}

HARBOUR HB_ARRAYS1()
{
   _reta( _parnl( 1 ) );
}

HARBOUR HB_ARRAYS2()
{
   _retnl( _parinfa( 1, _parni( 2 ) ) );
}

HARBOUR HB_PARAMS1()
{
   _retni( _pcount() );
}

HARBOUR HB_PARAMS2()
{
   _retni( _parinfo( _parni( 4 ) ) );
}
