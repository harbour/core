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

HARBOUR HB_LENGTH()
{
   _retni( _parclen( 1 ) );
}

HARBOUR HB_LOGICAL1()
{
   _retl( _parl( 1 ) );
}

HARBOUR HB_LOGICAL2()
{
   _retl( _parl( 1, 1 ) );
}

HARBOUR HB_DATE1()
{
   _retds( _pards( 1 ) );
}

HARBOUR HB_DATE2()
{
   _retds( _pards( 1, 1 ) );
}

HARBOUR HB_INT1()
{
   _retni( _parni( 1 ) );
}

HARBOUR HB_INT2()
{
   _retni( _parni( 1, 1 ) );
}

HARBOUR HB_LONG1()
{
   _retnl( _parnl( 1 ) );
}

HARBOUR HB_LONG2()
{
   _retnl( _parnl( 1, 1 ) );
}

HARBOUR HB_DOUBLE1()
{
   _retnd( _parnd( 1 ) );
}

HARBOUR HB_DOUBLE2()
{
   _retnd( _parnd( 1, 1 ) );
}

HARBOUR HB_ARRAYS()
{
   _retnl( _parinfa( 1, _parni( 2 ) ) );
}

HARBOUR HB_PARAMS()
{
   _retni( _parinfo( _parni( 4 ) ) );
}