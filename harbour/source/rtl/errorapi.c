#include <extend.h>
#include <CToHarb.h>

extern ITEM errorBlock;
extern STACK stack;
extern SYMBOL symEval;

PITEM _errNew( void )
{
   PushSymbol( GetDynSym( "ERRORNEW" )->pSymbol );
   PushNil();
   Do( 0 );

   return &stack.Return;
}

void _errPutDescription( PITEM pError, char * szDescription )
{
   PushSymbol( GetDynSym( "_DESCRIPTION" )->pSymbol );
   Push( pError );
   PushString( szDescription, strlen( szDescription ) );
   Do( 1 );
}

WORD _errLaunch( PITEM pError )
{
   PushSymbol( &symEval );
   Push( &errorBlock );
   Push( pError );
   Do( 1 );

   return stack.Return.value.iNumber;  /* TODO: _parnl( -1 ) */
}

void _errRelease( PITEM pError )
{
   ItemRelease( pError );
}

