/*
 * $Id$
 */

#include <extend.h>
#include <ctoharb.h>

#define EF_CANRETRY     1
#define EF_CANDEFAULT	4

/* error codes ( returned from _errLaunch() ) */

#define E_BREAK			0xffff
#define E_RETRY			1
#define E_DEFAULT       0

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

char * _errGetDescription( PITEM pError )
{
   PushSymbol( GetDynSym( "DESCRIPTION" )->pSymbol );
   Push( pError );
   Do( 0 );
   return stack.Return.value.szText;
}

PITEM _errPutDescription( PITEM pError, char * szDescription )
{
   PushSymbol( GetDynSym( "_DESCRIPTION" )->pSymbol );
   Push( pError );
   PushString( szDescription, strlen( szDescription ) );
   Do( 1 );
   return pError;
}

char * _errGetFileName( PITEM pError )
{
   PushSymbol( GetDynSym( "FILENAME" )->pSymbol );
   Push( pError );
   Do( 0 );
   return stack.Return.value.szText;
}

PITEM _errPutFileName( PITEM pError, char * szFileName )
{
   PushSymbol( GetDynSym( "_FILENAME" )->pSymbol );
   Push( pError );
   PushString( szFileName, strlen( szFileName ) );
   Do( 1 );
   return pError;
}

USHORT _errGetGenCode( PITEM pError )
{
   PushSymbol( GetDynSym( "GENCODE" )->pSymbol );
   Push( pError );
   Do( 0 );
   return stack.Return.value.iNumber;
}

PITEM _errPutGenCode( PITEM pError, USHORT uiGenCode )
{
   PushSymbol( GetDynSym( "_GENCODE" )->pSymbol );
   Push( pError );
   PushInteger( uiGenCode );
   Do( 1 );
   return pError;
}

char * _errGetOperation( PITEM pError )
{
   PushSymbol( GetDynSym( "OPERATION" )->pSymbol );
   Push( pError );
   Do( 0 );
   return stack.Return.value.szText;
}

PITEM _errPutOperation( PITEM pError, char * szOperation )
{
   PushSymbol( GetDynSym( "_OPERATION" )->pSymbol );
   Push( pError );
   PushString( szOperation, strlen( szOperation ) );
   Do( 1 );
   return pError;
}

USHORT _errGetOsCode( PITEM pError )
{
   PushSymbol( GetDynSym( "OSCODE" )->pSymbol );
   Push( pError );
   Do( 0 );
   return stack.Return.value.iNumber;
}

PITEM _errPutOsCode( PITEM pError, USHORT uiOsCode )
{
   PushSymbol( GetDynSym( "_OSCODE" )->pSymbol );
   Push( pError );
   PushInteger( uiOsCode );
   Do( 1 );
   return pError;
}

PITEM _errPutSeverity( PITEM pError, USHORT uiSeverity )
{
   PushSymbol( GetDynSym( "_SEVERITY" )->pSymbol );
   Push( pError );
   PushInteger( uiSeverity );
   Do( 1 );
   return pError;
}

PITEM _errPutSubCode( PITEM pError, USHORT uiSubCode )
{
   PushSymbol( GetDynSym( "_SUBCODE" )->pSymbol );
   Push( pError );
   PushInteger( uiSubCode );
   Do( 1 );
   return pError;
}

PITEM _errPutSubSystem( PITEM pError, char * szSubSystem )
{
   PushSymbol( GetDynSym( "_SUBSYSTEM" )->pSymbol );
   Push( pError );
   PushString( szSubSystem, strlen( szSubSystem ) );
   Do( 1 );
   return pError;
}

PITEM _errPutTries( PITEM pError, USHORT uiTries )
{
   PushSymbol( GetDynSym( "_TRIES" )->pSymbol );
   Push( pError );
   PushInteger( uiTries );
   Do( 1 );
   return pError;
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

