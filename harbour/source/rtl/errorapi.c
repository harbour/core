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
#include "langapi.h"

PHB_ITEM hb_errNew( void )
{
   PHB_ITEM pReturn = hb_itemNew( NULL );

   hb_vmPushSymbol( hb_dynsymGet( "ERRORNEW" )->pSymbol );
   hb_vmPushNil();
   hb_vmDo( 0 );

   hb_itemCopy( pReturn, &stack.Return );

   return pReturn;
}

/* QUESTION: When does Clipper show this: */
/*    TODO: Change to internal error: */
/*    printf("Error recovery failure, ???? (0)") */
/*    exit(1); */

WORD hb_errLaunch( PHB_ITEM pError )
{
   WORD wRetVal;

   if ( pError )
   {
      /* TODO: Determine if there was a BREAK in error handler. */
      BOOL bBreak = FALSE;
      WORD nSequenceLevel = 0;

      if ( ! IS_BLOCK( &errorBlock ) )
      {
         /* TODO: Change to internal error: */
         printf( "No ERRORBLOCK() for error at: ???? (0)" );
         exit( 1 );
      }

      hb_vmPushSymbol( &symEval );
      hb_vmPush( &errorBlock );
      hb_vmPush( pError );
      hb_vmDo( 1 );

      /* TODO: Handle the canSubstitute case somehow */
      /*       Clipper doesn't document the case where canSubstitute is set */

      if ( bBreak )
      {
         if ( nSequenceLevel )
         {
            wRetVal = E_BREAK;
         }
         else
         {
            exit( 1 ); /* TODO: quit correctly */
         }
      }
      else if ( IS_LOGICAL( &stack.Return ) )
         wRetVal = stack.Return.item.asLogical.value ? E_RETRY : E_DEFAULT;
      else
         wRetVal = E_DEFAULT;
   }
   else
      wRetVal = E_RETRY;

   return wRetVal;
}

/* TODO:
PHB_ITEM hb_errLaunchExt( PHB_ITEM pError )
{
}
*/

void hb_errRelease( PHB_ITEM pError )
{
   if ( pError )
      hb_itemRelease( pError );
}

char * hb_errGetDescription( PHB_ITEM pError )
{
   hb_vmPushSymbol( hb_dynsymGet( "DESCRIPTION" )->pSymbol );
   hb_vmPush( pError );
   hb_vmDo( 0 );
   return stack.Return.item.asString.value;
}

PHB_ITEM hb_errPutDescription( PHB_ITEM pError, char * szDescription )
{
   hb_vmPushSymbol( hb_dynsymGet( "_DESCRIPTION" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushString( szDescription, strlen( szDescription ) );
   hb_vmDo( 1 );

   return pError;
}

char * hb_errGetFileName( PHB_ITEM pError )
{
   hb_vmPushSymbol( hb_dynsymGet( "FILENAME" )->pSymbol );
   hb_vmPush( pError );
   hb_vmDo( 0 );
   return stack.Return.item.asString.value;
}

PHB_ITEM hb_errPutFileName( PHB_ITEM pError, char * szFileName )
{
   hb_vmPushSymbol( hb_dynsymGet( "_FILENAME" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushString( szFileName, strlen( szFileName ) );
   hb_vmDo( 1 );
   return pError;
}

USHORT hb_errGetGenCode( PHB_ITEM pError )
{
   hb_vmPushSymbol( hb_dynsymGet( "GENCODE" )->pSymbol );
   hb_vmPush( pError );
   hb_vmDo( 0 );
   return stack.Return.item.asInteger.value;
}

PHB_ITEM hb_errPutGenCode( PHB_ITEM pError, USHORT uiGenCode )
{
   hb_vmPushSymbol( hb_dynsymGet( "_GENCODE" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushInteger( uiGenCode );
   hb_vmDo( 1 );
   return pError;
}

char * hb_errGetOperation( PHB_ITEM pError )
{
   hb_vmPushSymbol( hb_dynsymGet( "OPERATION" )->pSymbol );
   hb_vmPush( pError );
   hb_vmDo( 0 );
   return stack.Return.item.asString.value;
}

PHB_ITEM hb_errPutOperation( PHB_ITEM pError, char * szOperation )
{
   hb_vmPushSymbol( hb_dynsymGet( "_OPERATION" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushString( szOperation, strlen( szOperation ) );
   hb_vmDo( 1 );
   return pError;
}

USHORT hb_errGetOsCode( PHB_ITEM pError )
{
   hb_vmPushSymbol( hb_dynsymGet( "OSCODE" )->pSymbol );
   hb_vmPush( pError );
   hb_vmDo( 0 );
   return stack.Return.item.asInteger.value;
}

PHB_ITEM hb_errPutOsCode( PHB_ITEM pError, USHORT uiOsCode )
{
   hb_vmPushSymbol( hb_dynsymGet( "_OSCODE" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushInteger( uiOsCode );
   hb_vmDo( 1 );
   return pError;
}

USHORT hb_errGetSeverity( PHB_ITEM pError )
{
   hb_vmPushSymbol( hb_dynsymGet( "SEVERITY" )->pSymbol );
   hb_vmPush( pError );
   hb_vmDo( 0 );
   return stack.Return.item.asInteger.value;
}

PHB_ITEM hb_errPutSeverity( PHB_ITEM pError, USHORT uiSeverity )
{
   hb_vmPushSymbol( hb_dynsymGet( "_SEVERITY" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushInteger( uiSeverity );
   hb_vmDo( 1 );
   return pError;
}

USHORT hb_errGetSubCode( PHB_ITEM pError )
{
   hb_vmPushSymbol( hb_dynsymGet( "SUBCODE" )->pSymbol );
   hb_vmPush( pError );
   hb_vmDo( 0 );
   return stack.Return.item.asInteger.value;
}

PHB_ITEM hb_errPutSubCode( PHB_ITEM pError, USHORT uiSubCode )
{
   hb_vmPushSymbol( hb_dynsymGet( "_SUBCODE" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushInteger( uiSubCode );
   hb_vmDo( 1 );
   return pError;
}

char * hb_errGetSubSystem( PHB_ITEM pError )
{
   hb_vmPushSymbol( hb_dynsymGet( "SUBSYSTEM" )->pSymbol );
   hb_vmPush( pError );
   hb_vmDo( 0 );
   return stack.Return.item.asString.value;
}

PHB_ITEM hb_errPutSubSystem( PHB_ITEM pError, char * szSubSystem )
{
   hb_vmPushSymbol( hb_dynsymGet( "_SUBSYSTEM" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushString( szSubSystem, strlen( szSubSystem ) );
   hb_vmDo( 1 );
   return pError;
}

USHORT hb_errGetTries( PHB_ITEM pError )
{
   hb_vmPushSymbol( hb_dynsymGet( "TRIES" )->pSymbol );
   hb_vmPush( pError );
   hb_vmDo( 0 );
   return stack.Return.item.asInteger.value;
}

PHB_ITEM hb_errPutTries( PHB_ITEM pError, USHORT uiTries )
{
   hb_vmPushSymbol( hb_dynsymGet( "_TRIES" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushInteger( uiTries );
   hb_vmDo( 1 );
   return pError;
}

USHORT hb_errGetFlags( PHB_ITEM pError )
{
   USHORT uiFlags = EF_NONE;

   /* ; */

   hb_vmPushSymbol( hb_dynsymGet( "CANRETRY" )->pSymbol );
   hb_vmPush( pError );
   hb_vmDo( 0 );

   if (stack.Return.item.asLogical.value) uiFlags |= EF_CANRETRY;

   /* ; */

   hb_vmPushSymbol( hb_dynsymGet( "CANSUBSTITUTE" )->pSymbol );
   hb_vmPush( pError );
   hb_vmDo( 0 );

   if (stack.Return.item.asLogical.value) uiFlags |= EF_CANSUBSTITUTE;

   /* ; */

   hb_vmPushSymbol( hb_dynsymGet( "CANDEFAULT" )->pSymbol );
   hb_vmPush( pError );
   hb_vmDo( 0 );

   if (stack.Return.item.asLogical.value) uiFlags |= EF_CANDEFAULT;

   /* ; */

   return uiFlags;
}

PHB_ITEM hb_errPutFlags( PHB_ITEM pError, USHORT uiFlags )
{
   hb_vmPushSymbol( hb_dynsymGet( "_CANRETRY" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushLogical( uiFlags & EF_CANRETRY );
   hb_vmDo( 1 );

   /* ; */

   hb_vmPushSymbol( hb_dynsymGet( "_CANSUBSTITUTE" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushLogical( uiFlags & EF_CANSUBSTITUTE );
   hb_vmDo( 1 );

   /* ; */

   hb_vmPushSymbol( hb_dynsymGet( "_CANDEFAULT" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushLogical( uiFlags & EF_CANDEFAULT );
   hb_vmDo( 1 );

   /* ; */

   return pError;
}

/* Wrappers for hb_errLaunch() */

static WORD hb_errorRT_New
(
   USHORT uiSeverity,
   char * szSubSystem,
   ULONG  ulGenCode,
   ULONG  ulSubCode,
   char * szDescription,
   char * szOperation,
   USHORT uiOsCode,
   USHORT uiFlags
)
{
   PHB_ITEM pError = hb_errNew();
   WORD wRetVal;

   hb_errPutSeverity( pError, uiSeverity );
   hb_errPutSubSystem( pError, szSubSystem );
   hb_errPutGenCode( pError, ulGenCode );
   hb_errPutSubCode( pError, ulSubCode );
   hb_errPutDescription( pError, szDescription ? szDescription : hb_langDGetErrorDesc(ulGenCode) );
   hb_errPutOperation( pError, szOperation );
   hb_errPutOsCode( pError, uiOsCode );
   hb_errPutFlags( pError, uiFlags );

   wRetVal = hb_errLaunch( pError );

   hb_errRelease( pError );

   return wRetVal;
}

WORD hb_errorRT_BASE( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation )
{
   return hb_errorRT_New( ES_ERROR, HB_ERR_SS_BASE, ulGenCode, ulSubCode, szDescription, szOperation, 0, EF_NONE );
}

WORD hb_errorRT_BASE_Ext1( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation, USHORT uiOsCode, USHORT uiFlags )
{
   return hb_errorRT_New( ES_ERROR, HB_ERR_SS_BASE, ulGenCode, ulSubCode, szDescription, szOperation, uiOsCode, uiFlags );
}

WORD hb_errorRT_TERMINAL( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation )
{
   return hb_errorRT_New( ES_ERROR, HB_ERR_SS_TERMINAL, ulGenCode, ulSubCode, szDescription, szOperation, 0, EF_NONE );
}

WORD hb_errorRT_DBCMD( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation )
{
   return hb_errorRT_New( ES_ERROR, HB_ERR_SS_DBCMD, ulGenCode, ulSubCode, szDescription, szOperation, 0, EF_NONE );
}

WORD hb_errorRT_TOOLS( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation )
{
   return hb_errorRT_New( ES_ERROR, HB_ERR_SS_BASE, ulGenCode, ulSubCode, szDescription, szOperation, 0, EF_NONE );
}

/* NOTES: Use as minimal calls from here, as possible. */
/*        Don't allocate memory from this function. */

void hb_errorInternal ( ULONG ulIntCode, char * szText, char * szModule, WORD wLine, char * szPar1, char * szPar2, char * szPar3 )
{
   char szError [ 256 ];

   if ( szModule )
      printf( "\n%s (%i)  ", szModule, wLine );

   sprintf( szError, szText ? szText : hb_langDGetErrorIntr( ulIntCode ), szPar1, szPar2, szPar3 );
   printf( "Internal error %lu: %s\n\n", ulIntCode, szError );

   exit( 1 );
}

