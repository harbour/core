/*
 * $Id$

   Copyright(C) 1999 by Antonio Linares.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public
   License along with this program; if not, write to:

   The Free Software Foundation, Inc.,
   675 Mass Ave, Cambridge, MA 02139, USA.

   You can contact me at: alinares@fivetech.com
 */

#include "extend.h"
#include "ctoharb.h"
#include "itemapi.h"
#include "errorapi.h"

PHB_ITEM hb_errNew( void )
{
   PHB_ITEM pReturn = hb_itemNew( NULL );

   PushSymbol( hb_GetDynSym( "ERRORNEW" )->pSymbol );
   PushNil();
   Do( 0 );

   hb_itemCopy( pReturn, &stack.Return );

   return pReturn;
}

WORD hb_errLaunch( PHB_ITEM pError )
{
   PushSymbol( &symEval );
   Push( &errorBlock );
   Push( pError );
   Do( 1 );

   return stack.Return.item.asInteger.value;  /* TODO: hb_parnl( -1 ) */
}

void hb_errRelease( PHB_ITEM pError )
{
   hb_itemRelease( pError );
}

char * hb_errGetDescription( PHB_ITEM pError )
{
   PushSymbol( hb_GetDynSym( "DESCRIPTION" )->pSymbol );
   Push( pError );
   Do( 0 );
   return stack.Return.item.asString.value;
}

PHB_ITEM hb_errPutDescription( PHB_ITEM pError, char * szDescription )
{
   PushSymbol( hb_GetDynSym( "_DESCRIPTION" )->pSymbol );
   Push( pError );
   PushString( szDescription, strlen( szDescription ) );
   Do( 1 );

   return pError;
}

char * hb_errGetFileName( PHB_ITEM pError )
{
   PushSymbol( hb_GetDynSym( "FILENAME" )->pSymbol );
   Push( pError );
   Do( 0 );
   return stack.Return.item.asString.value;
}

PHB_ITEM hb_errPutFileName( PHB_ITEM pError, char * szFileName )
{
   PushSymbol( hb_GetDynSym( "_FILENAME" )->pSymbol );
   Push( pError );
   PushString( szFileName, strlen( szFileName ) );
   Do( 1 );
   return pError;
}

USHORT hb_errGetGenCode( PHB_ITEM pError )
{
   PushSymbol( hb_GetDynSym( "GENCODE" )->pSymbol );
   Push( pError );
   Do( 0 );
   return stack.Return.item.asInteger.value;
}

PHB_ITEM hb_errPutGenCode( PHB_ITEM pError, USHORT uiGenCode )
{
   PushSymbol( hb_GetDynSym( "_GENCODE" )->pSymbol );
   Push( pError );
   PushInteger( uiGenCode );
   Do( 1 );
   return pError;
}

char * hb_errGetOperation( PHB_ITEM pError )
{
   PushSymbol( hb_GetDynSym( "OPERATION" )->pSymbol );
   Push( pError );
   Do( 0 );
   return stack.Return.item.asString.value;
}

PHB_ITEM hb_errPutOperation( PHB_ITEM pError, char * szOperation )
{
   PushSymbol( hb_GetDynSym( "_OPERATION" )->pSymbol );
   Push( pError );
   PushString( szOperation, strlen( szOperation ) );
   Do( 1 );
   return pError;
}

USHORT hb_errGetOsCode( PHB_ITEM pError )
{
   PushSymbol( hb_GetDynSym( "OSCODE" )->pSymbol );
   Push( pError );
   Do( 0 );
   return stack.Return.item.asInteger.value;
}

PHB_ITEM hb_errPutOsCode( PHB_ITEM pError, USHORT uiOsCode )
{
   PushSymbol( hb_GetDynSym( "_OSCODE" )->pSymbol );
   Push( pError );
   PushInteger( uiOsCode );
   Do( 1 );
   return pError;
}

USHORT hb_errGetSeverity( PHB_ITEM pError )
{
   PushSymbol( hb_GetDynSym( "SEVERITY" )->pSymbol );
   Push( pError );
   Do( 0 );
   return stack.Return.item.asInteger.value;
}

PHB_ITEM hb_errPutSeverity( PHB_ITEM pError, USHORT uiSeverity )
{
   PushSymbol( hb_GetDynSym( "_SEVERITY" )->pSymbol );
   Push( pError );
   PushInteger( uiSeverity );
   Do( 1 );
   return pError;
}

USHORT hb_errGetSubCode( PHB_ITEM pError )
{
   PushSymbol( hb_GetDynSym( "SUBCODE" )->pSymbol );
   Push( pError );
   Do( 0 );
   return stack.Return.item.asInteger.value;
}

PHB_ITEM hb_errPutSubCode( PHB_ITEM pError, USHORT uiSubCode )
{
   PushSymbol( hb_GetDynSym( "_SUBCODE" )->pSymbol );
   Push( pError );
   PushInteger( uiSubCode );
   Do( 1 );
   return pError;
}

char * hb_errGetSubSystem( PHB_ITEM pError )
{
   PushSymbol( hb_GetDynSym( "SUBSYSTEM" )->pSymbol );
   Push( pError );
   Do( 0 );
   return stack.Return.item.asString.value;
}

PHB_ITEM hb_errPutSubSystem( PHB_ITEM pError, char * szSubSystem )
{
   PushSymbol( hb_GetDynSym( "_SUBSYSTEM" )->pSymbol );
   Push( pError );
   PushString( szSubSystem, strlen( szSubSystem ) );
   Do( 1 );
   return pError;
}

USHORT hb_errGetTries( PHB_ITEM pError )
{
   PushSymbol( hb_GetDynSym( "TRIES" )->pSymbol );
   Push( pError );
   Do( 0 );
   return stack.Return.item.asInteger.value;
}

PHB_ITEM hb_errPutTries( PHB_ITEM pError, USHORT uiTries )
{
   PushSymbol( hb_GetDynSym( "_TRIES" )->pSymbol );
   Push( pError );
   PushInteger( uiTries );
   Do( 1 );
   return pError;
}

USHORT hb_errGetFlags( PHB_ITEM pError )
{
   USHORT uiFlags = EF_NONE;

   /* ; */

   PushSymbol( hb_GetDynSym( "CANRETRY" )->pSymbol );
   Push( pError );
   Do( 0 );

   if (stack.Return.item.asLogical.value) uiFlags |= EF_CANRETRY;

   /* ; */

   PushSymbol( hb_GetDynSym( "CANSUBSTITUTE" )->pSymbol );
   Push( pError );
   Do( 0 );

   if (stack.Return.item.asLogical.value) uiFlags |= EF_CANSUBSTITUTE;

   /* ; */

   PushSymbol( hb_GetDynSym( "CANDEFAULT" )->pSymbol );
   Push( pError );
   Do( 0 );

   if (stack.Return.item.asLogical.value) uiFlags |= EF_CANDEFAULT;

   /* ; */

   return uiFlags;
}

PHB_ITEM hb_errPutFlags( PHB_ITEM pError, USHORT uiFlags )
{
   PushSymbol( hb_GetDynSym( "_CANRETRY" )->pSymbol );
   Push( pError );
   PushLogical( uiFlags & EF_CANRETRY );
   Do( 1 );

   /* ; */

   PushSymbol( hb_GetDynSym( "_CANSUBSTITUTE" )->pSymbol );
   Push( pError );
   PushLogical( uiFlags & EF_CANSUBSTITUTE );
   Do( 1 );

   /* ; */

   PushSymbol( hb_GetDynSym( "_CANDEFAULT" )->pSymbol );
   Push( pError );
   PushLogical( uiFlags & EF_CANDEFAULT );
   Do( 1 );

   /* ; */

   return pError;
}

/* Wrappers for hb_errLaunch() */

WORD hb_errorRT_BASE( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation )
{
   PHB_ITEM pError = hb_errNew();
   WORD wRetVal;

   hb_errPutSeverity( pError, ES_ERROR );
   hb_errPutSubSystem( pError, HB_ERR_SS_BASE );
   hb_errPutGenCode( pError, ulGenCode );
   hb_errPutSubCode( pError, ulSubCode );
   if( szDescription )
      hb_errPutDescription( pError, szDescription );
   else
      hb_errPutDescription( pError, hb_errorNatDescription(ulGenCode) );
   hb_errPutOperation( pError, szOperation );

   wRetVal = hb_errLaunch( pError );

   hb_errRelease( pError );

   return wRetVal;
}

WORD hb_errorRT_TERMINAL( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation )
{
   PHB_ITEM pError = hb_errNew();
   WORD wRetVal;

   hb_errPutSeverity( pError, ES_ERROR );
   hb_errPutSubSystem( pError, HB_ERR_SS_TERMINAL );
   hb_errPutGenCode( pError, ulGenCode );
   hb_errPutSubCode( pError, ulSubCode );
   if( szDescription )
      hb_errPutDescription( pError, szDescription );
   else
      hb_errPutDescription( pError, hb_errorNatDescription(ulGenCode) );
   hb_errPutOperation( pError, szOperation );

   wRetVal = hb_errLaunch( pError );

   hb_errRelease( pError );

   return wRetVal;
}

WORD hb_errorRT_DBCMD( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation )
{
   PHB_ITEM pError = hb_errNew();
   WORD wRetVal;

   hb_errPutSeverity( pError, ES_ERROR );
   hb_errPutSubSystem( pError, HB_ERR_SS_DBCMD );
   hb_errPutGenCode( pError, ulGenCode );
   hb_errPutSubCode( pError, ulSubCode );
   if( szDescription )
      hb_errPutDescription( pError, szDescription );
   else
      hb_errPutDescription( pError, hb_errorNatDescription(ulGenCode) );
   hb_errPutOperation( pError, szOperation );

   wRetVal = hb_errLaunch( pError );

   hb_errRelease( pError );

   return wRetVal;
}

WORD hb_errorRT_TOOLS( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation )
{
   PHB_ITEM pError = hb_errNew();
   WORD wRetVal;

   hb_errPutSeverity( pError, ES_ERROR );
   hb_errPutSubSystem( pError, HB_ERR_SS_BASE );
   hb_errPutGenCode( pError, ulGenCode );
   hb_errPutSubCode( pError, ulSubCode );
   if( szDescription )
      hb_errPutDescription( pError, szDescription );
   else
      hb_errPutDescription( pError, hb_errorNatDescription(ulGenCode) );
   hb_errPutOperation( pError, szOperation );

   wRetVal = hb_errLaunch( pError );

   hb_errRelease( pError );

   return wRetVal;
}

