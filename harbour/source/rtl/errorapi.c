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

extern HB_ITEM errorBlock;
extern STACK stack;
extern SYMBOL symEval;

PHB_ITEM _errNew( void )
{
   PushSymbol( GetDynSym( "ERRORNEW" )->pSymbol );
   PushNil();
   Do( 0 );

   return &stack.Return;
}

char * _errGetDescription( PHB_ITEM pError )
{
   PushSymbol( GetDynSym( "DESCRIPTION" )->pSymbol );
   Push( pError );
   Do( 0 );
   return stack.Return.value.szText;
}

PHB_ITEM _errPutDescription( PHB_ITEM pError, char * szDescription )
{
   PushSymbol( GetDynSym( "_DESCRIPTION" )->pSymbol );
   Push( pError );
   PushString( szDescription, strlen( szDescription ) );
   Do( 1 );
   return pError;
}

char * _errGetFileName( PHB_ITEM pError )
{
   PushSymbol( GetDynSym( "FILENAME" )->pSymbol );
   Push( pError );
   Do( 0 );
   return stack.Return.value.szText;
}

PHB_ITEM _errPutFileName( PHB_ITEM pError, char * szFileName )
{
   PushSymbol( GetDynSym( "_FILENAME" )->pSymbol );
   Push( pError );
   PushString( szFileName, strlen( szFileName ) );
   Do( 1 );
   return pError;
}

USHORT _errGetGenCode( PHB_ITEM pError )
{
   PushSymbol( GetDynSym( "GENCODE" )->pSymbol );
   Push( pError );
   Do( 0 );
   return stack.Return.value.iNumber;
}

PHB_ITEM _errPutGenCode( PHB_ITEM pError, USHORT uiGenCode )
{
   PushSymbol( GetDynSym( "_GENCODE" )->pSymbol );
   Push( pError );
   PushInteger( uiGenCode );
   Do( 1 );
   return pError;
}

char * _errGetOperation( PHB_ITEM pError )
{
   PushSymbol( GetDynSym( "OPERATION" )->pSymbol );
   Push( pError );
   Do( 0 );
   return stack.Return.value.szText;
}

PHB_ITEM _errPutOperation( PHB_ITEM pError, char * szOperation )
{
   PushSymbol( GetDynSym( "_OPERATION" )->pSymbol );
   Push( pError );
   PushString( szOperation, strlen( szOperation ) );
   Do( 1 );
   return pError;
}

USHORT _errGetOsCode( PHB_ITEM pError )
{
   PushSymbol( GetDynSym( "OSCODE" )->pSymbol );
   Push( pError );
   Do( 0 );
   return stack.Return.value.iNumber;
}

PHB_ITEM _errPutOsCode( PHB_ITEM pError, USHORT uiOsCode )
{
   PushSymbol( GetDynSym( "_OSCODE" )->pSymbol );
   Push( pError );
   PushInteger( uiOsCode );
   Do( 1 );
   return pError;
}

PHB_ITEM _errPutSeverity( PHB_ITEM pError, USHORT uiSeverity )
{
   PushSymbol( GetDynSym( "_SEVERITY" )->pSymbol );
   Push( pError );
   PushInteger( uiSeverity );
   Do( 1 );
   return pError;
}

PHB_ITEM _errPutSubCode( PHB_ITEM pError, USHORT uiSubCode )
{
   PushSymbol( GetDynSym( "_SUBCODE" )->pSymbol );
   Push( pError );
   PushInteger( uiSubCode );
   Do( 1 );
   return pError;
}

PHB_ITEM _errPutSubSystem( PHB_ITEM pError, char * szSubSystem )
{
   PushSymbol( GetDynSym( "_SUBSYSTEM" )->pSymbol );
   Push( pError );
   PushString( szSubSystem, strlen( szSubSystem ) );
   Do( 1 );
   return pError;
}

PHB_ITEM _errPutTries( PHB_ITEM pError, USHORT uiTries )
{
   PushSymbol( GetDynSym( "_TRIES" )->pSymbol );
   Push( pError );
   PushInteger( uiTries );
   Do( 1 );
   return pError;
}

WORD _errLaunch( PHB_ITEM pError )
{
   PushSymbol( &symEval );
   Push( &errorBlock );
   Push( pError );
   Do( 1 );

   return stack.Return.value.iNumber;  /* TODO: _parnl( -1 ) */
}

void _errRelease( PHB_ITEM pError )
{
   ItemRelease( pError );
}

