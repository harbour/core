/*
 * $Id$
 */

/*
   Harbour Project source code

   The Error API

   Copyright 1999  Antonio Linares <alinares@fivetech.com>
   www - http://www.harbour-project.org

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version, with one exception:

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).
*/

/* Harbour Project source code
   http://www.Harbour-Project.org/

   The following functions are Copyright 1999 Victor Szel <info@szelvesz.hu>:
      HB_DOSERROR()
      hb_errLaunch()
      hb_errLaunchSubst()
      hb_errGetFlags()
      hb_errPutFlags()
      hb_errRT_New()
      hb_errRT_New_Subst()
      HB___ERRRT_BASE()
      hb_errRT_BASE()
      hb_errRT_BASE_Ext1()
      hb_errRT_BASE_Subst()
      hb_errInternal()

   See doc/hdr_tpl.txt, Version 1.2 or later, for licensing terms.
*/

#include "extend.h"
#include "ctoharb.h"
#include "itemapi.h"
#include "errorapi.h"
#include "langapi.h"

/* This is added to be able to detect a recursive error, and not let Harbour
   go into an infinite loop, this is an emulated version of the Clipper
   "Unrecoverable error 650: Processor stack fault" internal error, but
   better shows what is really the problem */
#define HB_ERROR_LAUNCH_MAX 8

static HB_ITEM s_errorBlock;
static int     s_iLaunchCount = 0;
static USHORT  s_uiErrorDOS = 0; /* The value of DOSERROR() */

extern HARBOUR HB_ERRORNEW( void );

/* NOTE: This is called via its symbol name, so we should make sure */
/*       that it gets linked. WARNING ! DON'T make this function static. */
void hb_errForceLink()
{
   HB_ERRORNEW();
}

HARBOUR HB_ERRORBLOCK( void )
{
   HB_ITEM oldError;
   PHB_ITEM pNewErrorBlock = hb_param( 1, IT_BLOCK );

   hb_itemClear( &oldError );
   hb_itemCopy( &oldError, &s_errorBlock );

   if( pNewErrorBlock )
      hb_itemCopy( &s_errorBlock, pNewErrorBlock );

   hb_itemReturn( &oldError );
   hb_itemClear( &oldError );
}

/* TOFIX: Make it Clipper compatible */

HARBOUR HB_DOSERROR( void )
{
   hb_retni( s_uiErrorDOS );

   if( ISNUM( 1 ) )
      s_uiErrorDOS = ( USHORT ) hb_parni( 1 );
}

void hb_errInit( void )
{
   hb_itemClear( &s_errorBlock );
}

void hb_errExit( void )
{
   hb_itemClear( &s_errorBlock );
}

PHB_ITEM hb_errNew( void )
{
   PHB_ITEM pReturn = hb_itemNew( NULL );

   hb_vmPushSymbol( hb_dynsymGet( "ERRORNEW" )->pSymbol );
   hb_vmPushNil();
   hb_vmDo( 0 );

   hb_itemCopy( pReturn, &stack.Return );

   return pReturn;
}

WORD hb_errLaunch( PHB_ITEM pError )
{
   WORD wRetVal;

   if( pError )
   {
      PHB_ITEM pResult;

      /* Check if we have a valid error handler */

      if( hb_itemType( &s_errorBlock ) != IT_BLOCK )
         hb_errInternal( 9999, "No ERRORBLOCK() for error", NULL, NULL );

      /* Check if the error launcher was called too many times recursively */

      if( s_iLaunchCount == HB_ERROR_LAUNCH_MAX )
         hb_errInternal( 9999, "Too many recursive error handler calls", NULL, NULL );

      /* Launch the error handler: "lResult := EVAL( ErrorBlock(), oError )" */

      s_iLaunchCount++;

      pResult = hb_itemDo( &s_errorBlock, 1, pError );

      s_iLaunchCount--;

      /* Check results */

      if( hb_vmRequestQuery() == HB_QUIT_REQUESTED )
      {
         hb_itemRelease( pResult );
         hb_errRelease( pError );
         hb_vmQuit();
      }
      else if( hb_vmRequestQuery() == HB_BREAK_REQUESTED )
      {
         hb_itemRelease( pResult );
         wRetVal = E_BREAK;
      }
      else
      {
         BOOL bFailure = FALSE;
         USHORT uiFlags = hb_errGetFlags( pError );

         /* If the error block didn't return a logical value, */
         /* or the canSubstitute flag has been set, consider it as a failure */

         if( hb_itemType( pResult ) != IT_LOGICAL || ( uiFlags & EF_CANSUBSTITUTE ) )
            bFailure = TRUE;
         else
         {
            wRetVal = hb_itemGetL( pResult ) ? E_RETRY : E_DEFAULT;

            if( ( wRetVal == E_DEFAULT && !( uiFlags & EF_CANDEFAULT ) ) ||
                ( wRetVal == E_RETRY   && !( uiFlags & EF_CANRETRY   ) ) )
               bFailure = TRUE;
         }

         hb_itemRelease( pResult );

         if( bFailure )
            hb_errInternal( 9999, "Error recovery failure", NULL, NULL );

         /* Add one try to the counter. */

         if( wRetVal == E_RETRY )
            hb_errPutTries( pError, hb_errGetTries( pError ) + 1 );
      }
   }
   else
      wRetVal = E_RETRY; /* Clipper does this, undocumented */

   return wRetVal;
}

/* This error launcher should be used in those situations, where the error
   handler is expected to return a value to be substituted as the result of
   a failed operation. */

/* NOTE: This should only be called when the EF_CANSUBSTITUE flag was set
         Since it this case the error handler will return the value
         to be substituted */
/* NOTE: The item pointer returned should be hb_itemRelease()-d by the
         caller if it was not NULL. */

PHB_ITEM hb_errLaunchSubst( PHB_ITEM pError )
{
   PHB_ITEM pResult;

   if( pError )
   {
      /* Check if we have a valid error handler */

      if( hb_itemType( &s_errorBlock ) != IT_BLOCK )
         hb_errInternal( 9999, "No ERRORBLOCK() for error", NULL, NULL );

      /* Check if the error launcher was called too many times recursively */

      if( s_iLaunchCount == HB_ERROR_LAUNCH_MAX )
         hb_errInternal( 9999, "Too many recursive ERRORBLOCK() calls", NULL, NULL );

      /* Launch the error handler: "xResult := EVAL( ErrorBlock(), oError )" */

      s_iLaunchCount++;

      pResult = hb_itemDo( &s_errorBlock, 1, pError );

      s_iLaunchCount--;

      /* Check results */

      if( hb_vmRequestQuery() == HB_QUIT_REQUESTED )
      {
         hb_itemRelease( pResult );
         hb_errRelease( pError );
         hb_vmQuit();
      }
      else if( hb_vmRequestQuery() == HB_BREAK_REQUESTED )
      {
         hb_itemRelease( pResult );
         pResult = NULL;
      }
      else
      {
         /* If the canSubstitute flag has not been set,
            consider it as a failure. */

         if( ! ( hb_errGetFlags( pError ) & EF_CANSUBSTITUTE ) )
            hb_errInternal( 9999, "Error recovery failure", NULL, NULL );
      }
   }
   else
      pResult = hb_itemNew( NULL );

   return pResult;
}

void hb_errRelease( PHB_ITEM pError )
{
   /* NOTE: NULL pointer is checked by hb_itemRelease() */
   hb_itemRelease( pError );
}

char * hb_errGetDescription( PHB_ITEM pError )
{
   hb_vmPushSymbol( hb_dynsymGet( "DESCRIPTION" )->pSymbol );
   hb_vmPush( pError );
   hb_vmDo( 0 );

   return hb_itemGetCPtr( &stack.Return );
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

   return hb_itemGetCPtr( &stack.Return );
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

   return hb_itemGetNI( &stack.Return );
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

   return hb_itemGetCPtr( &stack.Return );
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

   return hb_itemGetNI( &stack.Return );
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

   return hb_itemGetNI( &stack.Return );
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

   return hb_itemGetNI( &stack.Return );
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

   return hb_itemGetCPtr( &stack.Return );
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

   return hb_itemGetNI( &stack.Return );
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

   if( hb_itemGetL( &stack.Return ) )
      uiFlags |= EF_CANRETRY;

   /* ; */

   hb_vmPushSymbol( hb_dynsymGet( "CANSUBSTITUTE" )->pSymbol );
   hb_vmPush( pError );
   hb_vmDo( 0 );

   if( hb_itemGetL( &stack.Return ) )
      uiFlags |= EF_CANSUBSTITUTE;

   /* ; */

   hb_vmPushSymbol( hb_dynsymGet( "CANDEFAULT" )->pSymbol );
   hb_vmPush( pError );
   hb_vmDo( 0 );

   if( hb_itemGetL( &stack.Return ) )
      uiFlags |= EF_CANDEFAULT;

   /* ; */

   return uiFlags;
}

PHB_ITEM hb_errPutFlags( PHB_ITEM pError, USHORT uiFlags )
{
   hb_vmPushSymbol( hb_dynsymGet( "_CANRETRY" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushLogical( ( uiFlags & EF_CANRETRY ) ? TRUE : FALSE );
   hb_vmDo( 1 );

   /* ; */

   hb_vmPushSymbol( hb_dynsymGet( "_CANSUBSTITUTE" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushLogical( ( uiFlags & EF_CANSUBSTITUTE ) ? TRUE : FALSE );
   hb_vmDo( 1 );

   /* ; */

   hb_vmPushSymbol( hb_dynsymGet( "_CANDEFAULT" )->pSymbol );
   hb_vmPush( pError );
   hb_vmPushLogical( ( uiFlags & EF_CANDEFAULT ) ? TRUE : FALSE );
   hb_vmDo( 1 );

   /* ; */

   return pError;
}

/* Wrappers for hb_errLaunch() */

PHB_ITEM hb_errRT_New(
   USHORT uiSeverity,
   char * szSubSystem,
   ULONG  ulGenCode,
   ULONG  ulSubCode,
   char * szDescription,
   char * szOperation,
   USHORT uiOsCode,
   USHORT uiFlags )
{
   PHB_ITEM pError = hb_errNew();

   hb_errPutSeverity( pError, uiSeverity );
   hb_errPutSubSystem( pError, szSubSystem ? szSubSystem : HB_ERR_SS_BASE );
   hb_errPutGenCode( pError, ulGenCode );
   hb_errPutSubCode( pError, ulSubCode );
   hb_errPutDescription( pError, szDescription ? szDescription : hb_langDGetErrorDesc( ulGenCode ) );
   hb_errPutOperation( pError, szOperation ? szOperation : "" );
   hb_errPutOsCode( pError, uiOsCode );
   hb_errPutFlags( pError, uiFlags );

   return pError;
}

PHB_ITEM hb_errRT_New_Subst(
   USHORT uiSeverity,
   char * szSubSystem,
   ULONG  ulGenCode,
   ULONG  ulSubCode,
   char * szDescription,
   char * szOperation,
   USHORT uiOsCode,
   USHORT uiFlags )
{
   PHB_ITEM pError = hb_errNew();

   hb_errPutSeverity( pError, uiSeverity );
   hb_errPutSubSystem( pError, szSubSystem ? szSubSystem : HB_ERR_SS_BASE );
   hb_errPutGenCode( pError, ulGenCode );
   hb_errPutSubCode( pError, ulSubCode );
   hb_errPutDescription( pError, szDescription ? szDescription : hb_langDGetErrorDesc( ulGenCode ) );
   hb_errPutOperation( pError, szOperation ? szOperation : "" );
   hb_errPutOsCode( pError, uiOsCode );
   hb_errPutFlags( pError, uiFlags | EF_CANSUBSTITUTE );

   return( pError );
}

HARBOUR HB___ERRRT_BASE( void )
{
   hb_errRT_BASE( ( ULONG ) hb_parnl( 1 ),
                  ( ULONG ) hb_parnl( 2 ),
                  ISCHAR( 3 ) ? hb_parc( 3 ) : NULL,
                  hb_parc( 4 ) );
}

WORD hb_errRT_BASE( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation )
{
   WORD wRetVal;
   PHB_ITEM pError =
      hb_errRT_New( ES_ERROR, HB_ERR_SS_BASE, ulGenCode, ulSubCode, szDescription, szOperation, 0, EF_NONE );

   wRetVal = hb_errLaunch( pError );

   hb_errRelease( pError );

   return wRetVal;
}

WORD hb_errRT_BASE_Ext1( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation, USHORT uiOsCode, USHORT uiFlags )
{
   WORD wRetVal;
   PHB_ITEM pError =
      hb_errRT_New( ES_ERROR, HB_ERR_SS_BASE, ulGenCode, ulSubCode, szDescription, szOperation, uiOsCode, uiFlags );

   wRetVal = hb_errLaunch( pError );

   hb_errRelease( pError );

   return wRetVal;
}

PHB_ITEM hb_errRT_BASE_Subst( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation )
{
   PHB_ITEM pRetVal;
   PHB_ITEM pError =
      hb_errRT_New_Subst( ES_ERROR, HB_ERR_SS_BASE, ulGenCode, ulSubCode, szDescription, szOperation, 0, EF_NONE );

   pRetVal = hb_errLaunchSubst( pError );

   hb_errRelease( pError );

   return pRetVal;
}

WORD hb_errRT_TERM( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation, USHORT uiOSCode, USHORT uiFlags )
{
   WORD wRetVal;
   PHB_ITEM pError =
      hb_errRT_New( ES_ERROR, HB_ERR_SS_TERMINAL, ulGenCode, ulSubCode, szDescription, szOperation, uiOSCode, uiFlags );

   wRetVal = hb_errLaunch( pError );

   hb_errRelease( pError );

   return wRetVal;
}

WORD hb_errRT_DBCMD( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation )
{
   WORD wRetVal;
   PHB_ITEM pError =
      hb_errRT_New( ES_ERROR, HB_ERR_SS_DBCMD, ulGenCode, ulSubCode, szDescription, szOperation, 0, EF_NONE );

   wRetVal = hb_errLaunch( pError );

   hb_errRelease( pError );

   return wRetVal;
}

WORD hb_errRT_TOOLS( ULONG ulGenCode, ULONG ulSubCode, char * szDescription, char * szOperation )
{
   WORD wRetVal;
   PHB_ITEM pError =
      hb_errRT_New( ES_ERROR, HB_ERR_SS_BASE, ulGenCode, ulSubCode, szDescription, szOperation, 0, EF_NONE );

   wRetVal = hb_errLaunch( pError );

   hb_errRelease( pError );

   return wRetVal;
}

/* NOTE: Use as minimal calls from here, as possible. */
/*       Don't allocate memory from this function. */

void hb_errInternal( ULONG ulIntCode, char * szText, char * szPar1, char * szPar2 )
{
   printf( hb_consoleGetNewLine() );
   printf( "Unrecoverable error %lu: ", ulIntCode );
   printf( szText != NULL ? szText : hb_langDGetErrorIntr( ulIntCode ), szPar1, szPar2 );
   printf( hb_consoleGetNewLine() );

   hb_stackDispCall();

   exit( EXIT_FAILURE );
}

