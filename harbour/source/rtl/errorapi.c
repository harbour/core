/*
 * $Id$
 */

#include <extend.h>
#include <ctoharb.h>
#include <itemapi.h>
#include <errorapi.h>

/* error codes ( returned from hb_errLaunch() ) */


extern HB_ITEM errorBlock;
extern STACK stack;
extern SYMBOL symEval;

PHB_ITEM hb_errNew( void )
{
   PHB_ITEM pReturn = hb_itemNew( NULL );

   PushSymbol( GetDynSym( "ERRORNEW" )->pSymbol );
   PushNil();
   Do( 0 );

   ItemCopy( pReturn, &stack.Return );

   return pReturn;
}

char * hb_errGetDescription( PHB_ITEM pError )
{
   PushSymbol( GetDynSym( "DESCRIPTION" )->pSymbol );
   Push( pError );
   Do( 0 );
   return stack.Return.value.szText;
}

PHB_ITEM hb_errPutDescription( PHB_ITEM pError, char * szDescription )
{
   PushSymbol( GetDynSym( "_DESCRIPTION" )->pSymbol );
   Push( pError );
   PushString( szDescription, strlen( szDescription ) );
   Do( 1 );

   return pError;
}

char * hb_errGetFileName( PHB_ITEM pError )
{
   PushSymbol( GetDynSym( "FILENAME" )->pSymbol );
   Push( pError );
   Do( 0 );
   return stack.Return.value.szText;
}

PHB_ITEM hb_errPutFileName( PHB_ITEM pError, char * szFileName )
{
   PushSymbol( GetDynSym( "_FILENAME" )->pSymbol );
   Push( pError );
   PushString( szFileName, strlen( szFileName ) );
   Do( 1 );
   return pError;
}

USHORT hb_errGetGenCode( PHB_ITEM pError )
{
   PushSymbol( GetDynSym( "GENCODE" )->pSymbol );
   Push( pError );
   Do( 0 );
   return stack.Return.value.iNumber;
}

PHB_ITEM hb_errPutGenCode( PHB_ITEM pError, USHORT uiGenCode )
{
   PushSymbol( GetDynSym( "_GENCODE" )->pSymbol );
   Push( pError );
   PushInteger( uiGenCode );
   Do( 1 );
   return pError;
}

char * hb_errGetOperation( PHB_ITEM pError )
{
   PushSymbol( GetDynSym( "OPERATION" )->pSymbol );
   Push( pError );
   Do( 0 );
   return stack.Return.value.szText;
}

PHB_ITEM hb_errPutOperation( PHB_ITEM pError, char * szOperation )
{
   PushSymbol( GetDynSym( "_OPERATION" )->pSymbol );
   Push( pError );
   PushString( szOperation, strlen( szOperation ) );
   Do( 1 );
   return pError;
}

USHORT hb_errGetOsCode( PHB_ITEM pError )
{
   PushSymbol( GetDynSym( "OSCODE" )->pSymbol );
   Push( pError );
   Do( 0 );
   return stack.Return.value.iNumber;
}

PHB_ITEM hb_errPutOsCode( PHB_ITEM pError, USHORT uiOsCode )
{
   PushSymbol( GetDynSym( "_OSCODE" )->pSymbol );
   Push( pError );
   PushInteger( uiOsCode );
   Do( 1 );
   return pError;
}

PHB_ITEM hb_errPutSeverity( PHB_ITEM pError, USHORT uiSeverity )
{
   PushSymbol( GetDynSym( "_SEVERITY" )->pSymbol );
   Push( pError );
   PushInteger( uiSeverity );
   Do( 1 );
   return pError;
}

PHB_ITEM hb_errPutSubCode( PHB_ITEM pError, USHORT uiSubCode )
{
   PushSymbol( GetDynSym( "_SUBCODE" )->pSymbol );
   Push( pError );
   PushInteger( uiSubCode );
   Do( 1 );
   return pError;
}

PHB_ITEM hb_errPutSubSystem( PHB_ITEM pError, char * szSubSystem )
{
   PushSymbol( GetDynSym( "_SUBSYSTEM" )->pSymbol );
   Push( pError );
   PushString( szSubSystem, strlen( szSubSystem ) );
   Do( 1 );
   return pError;
}

PHB_ITEM hb_errPutTries( PHB_ITEM pError, USHORT uiTries )
{
   PushSymbol( GetDynSym( "_TRIES" )->pSymbol );
   Push( pError );
   PushInteger( uiTries );
   Do( 1 );
   return pError;
}

WORD hb_errLaunch( PHB_ITEM pError )
{
   PushSymbol( &symEval );
   Push( &errorBlock );
   Push( pError );
   Do( 1 );

   return stack.Return.value.iNumber;  /* TODO: hb_parnl( -1 ) */
}

void hb_errRelease( PHB_ITEM pError )
{
   hb_itemRelease( pError );
}

