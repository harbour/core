/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Base RDD module
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include <ctype.h>
#include "hbvm.h"
#include "hbstack.h"
#include "hbapifs.h"
#include "hbset.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbapiitm.h"
#include "hbrddwrk.h"


typedef struct _AREANODE
{
   void * pArea;               /* WorkAreas with different sizes */
   struct _AREANODE * pPrev;   /* Prev WorkArea in the list */
   struct _AREANODE * pNext;   /* Next WorkArea in the list */
} AREANODE;

typedef AREANODE * LPAREANODE;



extern HB_FUNC( _DBF );
extern HB_FUNC( _SDF );
extern HB_FUNC( _DELIM );
extern HB_FUNC( RDDSYS );

static char * s_szDefDriver = NULL;    /* Default RDD name */
static USHORT s_uiCurrArea = 1;        /* Selectd area */
static LPRDDNODE s_pRddList = NULL;    /* Registered RDD's */
static BOOL s_bNetError = FALSE;       /* Error on Networked environments */

static LPAREANODE s_pWorkAreas = NULL; /* WorkAreas */
static LPAREANODE s_pCurrArea = NULL;  /* Pointer to a selected and valid area */

/*
 * -- DEFAULT METHODS --
 */

/*
 * Empty method.
 */
static ERRCODE hb_waNull( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_waNull(%p)", pArea));
   HB_SYMBOL_UNUSED( pArea );

   return SUCCESS;
}

/*
 * Raise a runtime error if an method is not defined.
 */
static ERRCODE hb_waUnsupported( AREAP pArea )
{
   PHB_ITEM pError;

   HB_TRACE(HB_TR_DEBUG, ("hb_waUnsupported(%p)", pArea));

   pError = hb_errNew();
   hb_errPutGenCode( pError, EG_UNSUPPORTED );
   hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_UNSUPPORTED ) );
   SELF_ERROR( pArea, pError );
   hb_errRelease( pError );

   return FAILURE;
}

/*
 * The default virtual method table for all WorkAreas.
 */
static RDDFUNCS waTable = { hb_waBof,
                            hb_waEof,
                            hb_waFound,
                            hb_waGoBottom,
                            hb_waGoTo,
                            hb_waGoToId,
                            hb_waGoTop,
                            hb_waSeek,
                            hb_waSkip,
                            hb_waSkipFilter,
                            hb_waSkipRaw,
                            hb_waAddField,
                            hb_waAppend,
                            hb_waCreateFields,
                            hb_waDeleteRec,
                            hb_waDeleted,
                            hb_waFieldCount,
                            hb_waFieldDisplay,
                            hb_waFieldInfo,
                            hb_waFieldName,
                            hb_waFlush,
                            hb_waGetRec,
                            hb_waGetValue,
                            hb_waGetVarLen,
                            hb_waGoCold,
                            hb_waGoHot,
                            hb_waPutRec,
                            hb_waPutValue,
                            hb_waRecall,
                            hb_waRecCount,
                            hb_waRecInfo,
                            hb_waRecNo,
                            hb_waSetFieldExtent,
                            hb_waAlias,
                            hb_waClose,
                            hb_waCreate,
                            hb_waInfo,
                            hb_waNewArea,
                            hb_waOpen,
                            hb_waRelease,
                            hb_waStructSize,
                            hb_waSysName,
                            hb_waEval,
                            hb_waPack,
                            hb_waPackRec,
                            hb_waSort,
                            hb_waTrans,
                            hb_waTransRec,
                            hb_waZap,
                            hb_waChildEnd,
                            hb_waChildStart,
                            hb_waChildSync,
                            hb_waSyncChildren,
                            hb_waClearRel,
                            hb_waForceRel,
                            hb_waRelArea,
                            hb_waRelEval,
                            hb_waRelText,
                            hb_waSetRel,
                            hb_waOrderListAdd,
                            hb_waOrderListClear,
                            hb_waOrderListDelete,
                            hb_waOrderListFocus,
                            hb_waOrderListRebuild,
                            hb_waOrderCondition,
                            hb_waOrderCreate,
                            hb_waOrderDestroy,
                            hb_waOrderInfo,
                            hb_waClearFilter,
                            hb_waClearLocate,
                            hb_waClearScope,
                            hb_waCountScope,
                            hb_waFilterText,
                            hb_waScopeInfo,
                            hb_waSetFilter,
                            hb_waSetLocate,
                            hb_waSetScope,
                            hb_waSkipScope,
                            hb_waCompile,
                            hb_waError,
                            hb_waEvalBlock,
                            hb_waRawLock,
                            hb_waLock,
                            hb_waUnLock,
                            hb_waCloseMemFile,
                            hb_waCreateMemFile,
                            hb_waGetValueFile,
                            hb_waOpenMemFile,
                            hb_waPutValueFile,
                            hb_waReadDBHeader,
                            hb_waWriteDBHeader,
                            hb_rddExit,
                            hb_rddDrop,
                            hb_rddExists,
                            hb_waWhoCares
                           };



/*
 * -- BASIC RDD METHODS --
 */

/*
 * Force link the built-in RDD's.
 */
static void hb_rddCheck( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_rddCheck()"));

   if( !s_szDefDriver )
   {
      s_szDefDriver = ( char * ) hb_xgrab( 1 );
      s_szDefDriver[ 0 ] = '\0';

      /* Force link the built-in RDD's */
      HB_FUNCNAME( _DBF )();
      HB_FUNCNAME( _SDF )();
      HB_FUNCNAME( _DELIM )();
      HB_FUNCNAME( RDDSYS )();
   }
}

/*
 * Closes all WorkAreas.
 */
static void hb_rddCloseAll( void )
{
   int nCycl = 0;
   LPAREANODE pAreaNode;
   HB_TRACE(HB_TR_DEBUG, ("hb_rddCloseAll()"));

   while( nCycl < 2 )
   {
      pAreaNode = s_pWorkAreas;
      while( pAreaNode )
      {
         s_pCurrArea = pAreaNode;
         pAreaNode = pAreaNode->pNext;
         if( ( !nCycl && ( ( AREAP ) s_pCurrArea->pArea )->lpdbRelations ) ||
             (  nCycl && s_pCurrArea->pArea ) )
         {
            SELF_CLOSE( ( AREAP ) s_pCurrArea->pArea );
            SELF_RELEASE( ( AREAP ) s_pCurrArea->pArea );
            // hb_xfree( s_pCurrArea->pArea );
            s_pCurrArea->pArea = NULL;
         }
         if( nCycl == 1 )
            hb_xfree( s_pCurrArea );
      }
      nCycl ++;
   }

   s_uiCurrArea = 1;
   s_pCurrArea = NULL;
   s_pWorkAreas = NULL;
}

/*
 * Find a RDD node.
 */
static LPRDDNODE hb_rddFindNode( char * szDriver, USHORT * uiIndex )
{
   LPRDDNODE pRddNode;
   USHORT uiCount;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddFindNode(%s, %p)", szDriver, uiIndex));

   uiCount = 0;
   pRddNode = s_pRddList;
   while( pRddNode )
   {
      if( strcmp( pRddNode->szName, szDriver ) == 0 ) /* Matched RDD */
      {
         if( uiIndex )
            * uiIndex = uiCount;
         return pRddNode;
      }
      pRddNode = pRddNode->pNext;
      uiCount++;
   }
   if( uiIndex )
      * uiIndex = 0;
   return NULL;
}

/*
 * Register a RDD driver.
 */
static int hb_rddRegister( char * szDriver, USHORT uiType )
{
   LPRDDNODE pRddNode, pRddNewNode;
   PHB_DYNS pGetFuncTable;
   char * szGetFuncTable;
   USHORT uiFunctions;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddRegister(%s, %hu)", szDriver, uiType));

   if( hb_rddFindNode( szDriver, NULL ) )    /* Duplicated RDD */
      return 1;

   szGetFuncTable = ( char * ) hb_xgrab( strlen( szDriver ) + 14 );
   strcpy( szGetFuncTable, szDriver );
   strcat( szGetFuncTable, "_GETFUNCTABLE" );
   pGetFuncTable = hb_dynsymFindName( szGetFuncTable );
   hb_xfree( szGetFuncTable );
   if( !pGetFuncTable )
      return 2;              /* Not valid RDD */

   /* Create a new RDD node */
   pRddNewNode = ( LPRDDNODE ) hb_xgrab( sizeof( RDDNODE ) );
   memset( pRddNewNode, 0, sizeof( RDDNODE ) );

   /* Fill the new RDD node */
   strncpy( pRddNewNode->szName, szDriver, HARBOUR_MAX_RDD_DRIVERNAME_LENGTH );
   pRddNewNode->uiType = uiType;

   /* Call <szDriver>_GETFUNCTABLE() */
   hb_vmPushSymbol( pGetFuncTable->pSymbol );
   hb_vmPushNil();
   hb_vmPushPointer( ( void * ) &uiFunctions );
   hb_vmPushPointer( ( void * ) &pRddNewNode->pTable );
   hb_vmDo( 2 );
   if ( hb_parni( -1 ) != SUCCESS )
   {
      hb_xfree( pRddNewNode );         /* Delete de new RDD node */
      return 3;                        /* Invalid FUNCTABLE */
   }

   if( !s_pRddList )                   /* First RDD node */
      s_pRddList = pRddNewNode;
   else
   {
      pRddNode = s_pRddList;
      while( pRddNode->pNext )
         pRddNode = pRddNode->pNext;   /* Locate the last RDD node */
      pRddNode->pNext = pRddNewNode;   /* Add the new RDD node */
   }
   return 0;                           /* Ok */
}

/*
 * Find a WorkArea by the alias.
 */
static USHORT hb_rddSelect( char * szAlias )
{
   PHB_DYNS pSymAlias;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelect(%s)", szAlias));

   pSymAlias = hb_dynsymFindName( szAlias );
   if( pSymAlias && pSymAlias->hArea )
      return ( USHORT ) pSymAlias->hArea;
   else
      return 0;
}

/*
 * Return the next free WorkArea for later use.
 */
static void hb_rddSelectFirstAvailable( void )
{
   LPAREANODE pAreaNode;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelectFirstAvailable()"));

   s_uiCurrArea = 1;
   pAreaNode = s_pWorkAreas;
   while( pAreaNode )
   {
      if( ( ( AREAP ) pAreaNode->pArea )->uiArea > s_uiCurrArea )
         break;
      else if( ( ( AREAP ) pAreaNode->pArea )->uiArea == s_uiCurrArea )
         s_uiCurrArea++;
      pAreaNode = pAreaNode->pNext;
   }
   s_pCurrArea = NULL;   /* Selected WorkArea must be created */
}

/*
 * pTable - a table in new RDDNODE that will be filled
 * pSubTable - a table with a list of supported functions
 * pSuperTable - a current table in a RDDNODE
 * szDrvName - a driver name that will be inherited
*/
ERRCODE hb_rddInherit( PRDDFUNCS pTable, PRDDFUNCS pSubTable, PRDDFUNCS pSuperTable, BYTE * szDrvName )
{
   char * szSuperName;
   LPRDDNODE pRddNode;
   USHORT uiCount;
   DBENTRYP_V * pFunction, * pSubFunction;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddInherit(%p, %p, %p, %s)", pTable, pSubTable, pSuperTable, szDrvName));

   if( !pTable )
      return FAILURE;

   /* Copy the pSuperTable into pTable */
   if( !szDrvName || ( uiCount = strlen( ( const char * ) szDrvName ) ) == 0 )
   {
      /* no name for inherited driver - use the default one */
      memcpy( pTable, &waTable, sizeof( RDDFUNCS ) );
      memcpy( pSuperTable, &waTable, sizeof( RDDFUNCS ) );
   }
   else
   {
      szSuperName = ( char * ) hb_xgrab( uiCount + 1 );
      hb_strncpyUpper( szSuperName, ( char * ) szDrvName, uiCount );
      pRddNode = hb_rddFindNode( szSuperName, NULL );
      hb_xfree( szSuperName );
      if( !pRddNode )
         return FAILURE;
      memcpy( pTable, &pRddNode->pTable, sizeof( RDDFUNCS ) );
      memcpy( pSuperTable, &pRddNode->pTable, sizeof( RDDFUNCS ) );
   }

   /* Copy the non NULL entries from pSubTable into pTable */
   pFunction = ( DBENTRYP_V * ) pTable;
   pSubFunction = ( DBENTRYP_V * ) pSubTable;
   for( uiCount = 0; uiCount < RDDFUNCSCOUNT; uiCount++ )
   {
      if( * pSubFunction )
         * pFunction = * pSubFunction;
      pFunction ++;
      pSubFunction ++;
   }
   return SUCCESS;
}

/*
 * Closes and releases the current WorkArea preparing it
 * to be used with a new database.
 */
void hb_rddReleaseCurrentArea( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_rddReleaseCurrentArea()"));
   SELF_CLOSE( ( AREAP ) s_pCurrArea->pArea );
   SELF_RELEASE( ( AREAP ) s_pCurrArea->pArea );

   if( s_pWorkAreas == s_pCurrArea )
   {
      s_pWorkAreas = s_pCurrArea->pNext;
      if( s_pWorkAreas )
         s_pWorkAreas->pPrev = NULL;
   }
   else
   {
      if( s_pCurrArea->pPrev )
         s_pCurrArea->pPrev->pNext = s_pCurrArea->pNext;
      if( s_pCurrArea->pNext )
         s_pCurrArea->pNext->pPrev = s_pCurrArea->pPrev;
   }

   hb_xfree( s_pCurrArea );
   s_pCurrArea = NULL;
}

/*
 * Prepares a new WorkArea node.
 */
LPAREANODE hb_rddNewAreaNode( LPRDDNODE pRddNode, USHORT uiRddID )
{
   LPAREANODE pCurrArea;
   USHORT uiSize;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddNewAreaNode(%p %d)", pRddNode,uiRddID));
   pCurrArea = ( LPAREANODE ) hb_xgrab( sizeof( AREANODE ) );
   if( pRddNode->uiAreaSize == 0 ) /* Calculate the size of WorkArea */
   {
      pCurrArea->pArea = ( AREAP ) hb_xgrab( sizeof( AREA ) );
      memset( pCurrArea->pArea, 0, sizeof( AREA ) );
      ( ( AREAP ) pCurrArea->pArea )->lprfsHost = &pRddNode->pTable;

      /* Need more space? */
      SELF_STRUCTSIZE( ( AREAP ) pCurrArea->pArea, &uiSize );
      if( uiSize > sizeof( AREA ) )   /* Size of Area changed */
      {
         pCurrArea->pArea = ( AREAP ) hb_xrealloc( pCurrArea->pArea, uiSize );
         memset( pCurrArea->pArea, 0, uiSize );
         ( ( AREAP ) pCurrArea->pArea )->lprfsHost = &pRddNode->pTable;
      }

      pRddNode->uiAreaSize = uiSize;  /* Update the size of WorkArea */
   }
   else
   {
      pCurrArea->pArea = ( AREAP ) hb_xgrab( pRddNode->uiAreaSize );
      memset( pCurrArea->pArea, 0, pRddNode->uiAreaSize );
      ( ( AREAP ) pCurrArea->pArea )->lprfsHost = &pRddNode->pTable;
   }

   ( ( AREAP ) pCurrArea->pArea )->rddID = uiRddID;

   pCurrArea->pPrev = NULL;
   pCurrArea->pNext = NULL;

   SELF_NEW( ( AREAP ) pCurrArea->pArea );
   return pCurrArea;
}

/*
 * Insert the new WorkArea node
 */
USHORT hb_rddInsertAreaNode( char *szDriver )
{
   USHORT uiRddID;
   LPRDDNODE pRddNode;
   LPAREANODE pAreaNode;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddInsertAreaNode(%s)", szDriver));
   pRddNode = hb_rddFindNode( szDriver, &uiRddID );
   if( !pRddNode )
      return FALSE;

   s_pCurrArea = hb_rddNewAreaNode( pRddNode, uiRddID );
   if( !s_pWorkAreas )
      s_pWorkAreas = s_pCurrArea;  /* The new WorkArea node is the first */
   else
   {
      pAreaNode = s_pWorkAreas;
      while( pAreaNode )
      {
         if( ( ( AREAP ) pAreaNode->pArea )->uiArea > s_uiCurrArea )
         {
            /* Insert the new WorkArea node */
            s_pCurrArea->pPrev = pAreaNode->pPrev;
            s_pCurrArea->pNext = pAreaNode;
            pAreaNode->pPrev = s_pCurrArea;
            if( s_pCurrArea->pPrev )
               s_pCurrArea->pPrev->pNext = s_pCurrArea;
            else
               s_pWorkAreas = s_pCurrArea;
            break;
         }
         if( pAreaNode->pNext )
            pAreaNode = pAreaNode->pNext;
         else
         {
            /* Append the new WorkArea node */
            pAreaNode->pNext = s_pCurrArea;
            s_pCurrArea->pPrev = pAreaNode;
            break;
         }
      }
   }
   return TRUE;
}

/*
 * Find a field.
 */
static USHORT hb_rddFieldIndex( AREAP pArea, char * szName )
{
   USHORT uiCount;
   LPFIELD pField;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddFieldIndex(%s)", szName));
   uiCount = 0;
   pField = pArea->lpFields;
   while( pField )
   {
      ++uiCount;
      if( strcmp( szName, ( ( PHB_DYNS ) pField->sym )->pSymbol->szName ) == 0 )
         return uiCount;
      pField = pField->lpfNext;
   }
   return 0;
}

/*
 * -- FUNCTIONS ACCESSED FROM VIRTUAL MACHINE --
 */

/*
 * Return the current WorkArea number.
 */
int  hb_rddGetCurrentWorkAreaNumber( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_rddGetCurrentWorkAreaNumber()"));

   return s_uiCurrArea;
}

/*
 * Select a WorkArea by the number.
 */
ERRCODE hb_rddSelectWorkAreaNumber( int iArea )
{
   LPAREANODE pAreaNode;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelectWorkAreaNumber(%d)", iArea));

   s_uiCurrArea = iArea;

   pAreaNode = s_pWorkAreas;
   while( pAreaNode )
   {
      if( ( ( AREAP ) pAreaNode->pArea )->uiArea == s_uiCurrArea )
      {
         s_pCurrArea = pAreaNode; /* Select a valid WorkArea */
         return SUCCESS;
      }
      pAreaNode = pAreaNode->pNext;
   }
   s_pCurrArea = NULL;            /* Selected WorkArea is closed */
   return FAILURE;
}

/*
 * Select a WorkArea by the symbol name.
 */
ERRCODE hb_rddSelectWorkAreaSymbol( PHB_SYMB pSymAlias )
{
   ERRCODE bResult;
   char * szName;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelectWorkAreaSymbol(%p)", pSymAlias));

   if( pSymAlias->pDynSym->hArea )
      bResult = hb_rddSelectWorkAreaNumber( pSymAlias->pDynSym->hArea );
   else
   {
      szName = pSymAlias->pDynSym->pSymbol->szName;

      if( strlen( szName ) == 1 && toupper( szName[ 0 ] ) >= 'A' && toupper( szName[ 0 ] ) <= 'K' )
         bResult = hb_rddSelectWorkAreaNumber( toupper( szName[ 0 ] ) - 'A' + 1 );
      else
      {
         /*
          * generate an error with retry possibility
          * (user created error handler can open a missing database)
          */
         USHORT uiAction = E_RETRY;
         HB_ITEM_PTR pError;

         pError = hb_errRT_New( ES_ERROR, NULL, EG_NOALIAS, EDBCMD_NOALIAS,
                                NULL, pSymAlias->szName, 0, EF_CANRETRY );

         bResult = FAILURE;
         while( uiAction == E_RETRY )
         {
            uiAction = hb_errLaunch( pError );
            if( uiAction == E_RETRY )
               if( pSymAlias->pDynSym->hArea )
               {
                  bResult = hb_rddSelectWorkAreaNumber( pSymAlias->pDynSym->hArea );
                  uiAction = E_DEFAULT;
               }
         }
         hb_errRelease( pError );
      }
   }
   return bResult;
}

/*
 * Select a WorkArea by the name.
 */
ERRCODE hb_rddSelectWorkAreaAlias( char * szName )
{
   ERRCODE bResult;
   ULONG ulLen;
   PHB_DYNS pSymArea;
   USHORT uiAction;
   HB_ITEM_PTR pError;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelectWorkAreaAlias(%s)", szName));

   ulLen = strlen( szName );
   if( ulLen >= 1 && toupper( szName[ 0 ] ) > '0' && toupper( szName[ 0 ] ) <= '9' )
      bResult = hb_rddSelectWorkAreaNumber( atoi( szName ) );
   else if( ulLen == 1 && toupper( szName[ 0 ] ) >= 'A' && toupper( szName[ 0 ] ) <= 'K' )
      bResult = hb_rddSelectWorkAreaNumber( toupper( szName[ 0 ] ) - 'A' + 1 );
   else
   {
      pSymArea = hb_dynsymFindName( szName );
      if( pSymArea && pSymArea->hArea )
         bResult = hb_rddSelectWorkAreaNumber( pSymArea->hArea );
      else
      {
         /*
          * generate an error with retry possibility
          * (user created error handler can open a missing database)
          */
         uiAction = E_RETRY;
         pError = hb_errRT_New( ES_ERROR, NULL, EG_NOALIAS, EDBCMD_NOALIAS,
                                NULL, szName, 0, EF_CANRETRY );

         bResult = FAILURE;
         while( uiAction == E_RETRY )
         {
            uiAction = hb_errLaunch( pError );
            if( uiAction == E_RETRY )
            {
               pSymArea = hb_dynsymFindName( szName );
               if( pSymArea && pSymArea->hArea )
               {
                  bResult = hb_rddSelectWorkAreaNumber( pSymArea->hArea );
                  uiAction = E_DEFAULT;
               }
            }
         }
         hb_errRelease( pError );
      }
   }

   return bResult;
}

/*
 *  Function for getting current workarea pointer
 */
void * hb_rddGetCurrentWorkAreaPointer( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_rddGetCurrentWorkAreaPointer()"));

   return s_pCurrArea->pArea;
}

/*
 * Obtain the current value of a field.
 */
ERRCODE hb_rddGetFieldValue( HB_ITEM_PTR pItem, PHB_SYMB pFieldSymbol )
{
   ERRCODE bSuccess;
   USHORT uiAction;
   HB_ITEM_PTR pError;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddGetFieldValue(%p, %p)", pItem, pFieldSymbol));

   bSuccess = hb_rddFieldGet( pItem, pFieldSymbol );
   if( bSuccess == FAILURE )
   {
      /*
       * generate an error with retry possibility
       * (user created error handler can make this field accessible)
       */
      uiAction = E_RETRY;
      pError = hb_errRT_New( ES_ERROR, NULL, EG_NOVAR, EDBCMD_NOVAR,
                             NULL, pFieldSymbol->szName, 0, EF_CANRETRY );

      while( uiAction == E_RETRY )
      {
         uiAction = hb_errLaunch( pError );
         if( uiAction == E_RETRY )
         {
            bSuccess = hb_rddFieldGet( pItem, pFieldSymbol );
            if( bSuccess == SUCCESS )
               uiAction = E_DEFAULT;
         }
      }
      hb_errRelease( pError );
   }
   return bSuccess;
}

/*
 * Assign a value to a field.
 */
ERRCODE hb_rddPutFieldValue( HB_ITEM_PTR pItem, PHB_SYMB pFieldSymbol )
{
   ERRCODE bSuccess;
   USHORT uiAction;
   HB_ITEM_PTR pError;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddPutFieldValue(%p, %p)", pItem, pFieldSymbol));

   bSuccess = hb_rddFieldPut( pItem, pFieldSymbol );
   if( bSuccess == FAILURE )
   {
      /*
       * generate an error with retry possibility
       * (user created error handler can make this field accessible)
       */
      uiAction = E_RETRY;
      pError = hb_errRT_New( ES_ERROR, NULL, EG_NOVAR, EDBCMD_NOVAR,
                             NULL, pFieldSymbol->szName, 0, EF_CANRETRY );

      while( uiAction == E_RETRY )
      {
         uiAction = hb_errLaunch( pError );
         if( uiAction == E_RETRY )
         {
            bSuccess = hb_rddFieldPut( pItem, pFieldSymbol );
            if( bSuccess == SUCCESS )
               uiAction = E_DEFAULT;
         }
      }
      hb_errRelease( pError );
   }
   return bSuccess;
}

/*
 * Assign a value to a field.
 */
ERRCODE hb_rddFieldPut( HB_ITEM_PTR pItem, PHB_SYMB pFieldSymbol )
{
   LPFIELD pField;
   USHORT uiField;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddFieldPut(%p, %p)", pItem, pFieldSymbol));

   if( s_pCurrArea )
   {
      uiField = 1;
      pField = ( ( AREAP ) s_pCurrArea->pArea )->lpFields;
      while( pField )
      {
         if( ( PHB_DYNS ) pField->sym == pFieldSymbol->pDynSym )
         {
            SELF_PUTVALUE( ( AREAP ) s_pCurrArea->pArea, uiField, pItem );
            return SUCCESS;
         }
         pField = pField->lpfNext;
         uiField++;
      }
   }
   return FAILURE;
}

/*
 * Obtain the current value of a field.
 */
ERRCODE hb_rddFieldGet( HB_ITEM_PTR pItem, PHB_SYMB pFieldSymbol )
{
   LPFIELD pField;
   USHORT uiField;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddFieldGet(%p, %p)", pItem, pFieldSymbol));

   if( s_pCurrArea )
   {
      uiField = 1;
      pField = ( ( AREAP ) s_pCurrArea->pArea )->lpFields;
      while( pField )
      {
         if( ( PHB_DYNS ) pField->sym == pFieldSymbol->pDynSym )
         {
            SELF_GETVALUE( ( AREAP ) s_pCurrArea->pArea, uiField, pItem );
            return SUCCESS;
         }
         pField = pField->lpfNext;
         uiField++;
      }
   }
   return FAILURE;
}

/*
 * Shutdown the RDD system.
 */
void hb_rddShutDown( void )
{
   LPRDDNODE pRddNode;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddShutDown()"));

   hb_rddCloseAll();
   if( s_szDefDriver )
      hb_xfree( s_szDefDriver );

   while( s_pRddList )
   {
      pRddNode = s_pRddList;
      s_pRddList = s_pRddList->pNext;
      if ( pRddNode->pTable.exit != NULL )
        SELF_EXIT( pRddNode );
      hb_xfree( pRddNode );
   }
}

/*
 * -- HARBOUR FUNCTIONS --
 */

HB_FUNC( AFIELDS )
{
   PHB_ITEM pName, pType, pLen, pDec, pItem;
   USHORT uiFields, uiArrayLen, uiCount;

   if( !s_pCurrArea )
   {
      hb_retni( 0 );
      return;
   }

   pName = hb_param( 1, HB_IT_ARRAY );
   pType = hb_param( 2, HB_IT_ARRAY );
   pLen = hb_param( 3, HB_IT_ARRAY );
   pDec = hb_param( 4, HB_IT_ARRAY );
   if( !pName && !pType && !pLen && !pDec )
   {
      hb_retni( 0 );
      return;
   }

   uiArrayLen = 0;
   pItem = hb_itemNew( NULL );
   SELF_FIELDCOUNT( ( AREAP ) s_pCurrArea->pArea, &uiFields );
   if( pName )
   {
      uiArrayLen = ( USHORT ) hb_arrayLen( pName );
      if( uiArrayLen > uiFields )
         uiArrayLen = uiFields;
      for( uiCount = 1; uiCount <= uiArrayLen; uiCount++ )
      {
         SELF_FIELDINFO( ( AREAP ) s_pCurrArea->pArea, uiCount, DBS_NAME, pItem );
         hb_arraySet( pName, uiCount, pItem );
      }
   }
   if( pType )
   {
      uiArrayLen = ( USHORT ) hb_arrayLen( pType );
      if( uiArrayLen > uiFields )
         uiArrayLen = uiFields;
      for( uiCount = 1; uiCount <= uiArrayLen; uiCount++ )
      {
         SELF_FIELDINFO( ( AREAP ) s_pCurrArea->pArea, uiCount, DBS_TYPE, pItem );
         hb_arraySet( pType, uiCount, pItem );
      }
   }
   if( pLen )
   {
      uiArrayLen = ( USHORT ) hb_arrayLen( pLen );
      if( uiArrayLen > uiFields )
         uiArrayLen = uiFields;
      for( uiCount = 1; uiCount <= uiArrayLen; uiCount++ )
      {
         SELF_FIELDINFO( ( AREAP ) s_pCurrArea->pArea, uiCount, DBS_LEN, pItem );
         hb_arraySet( pLen, uiCount, pItem );
      }
   }
   if( pDec )
   {
      uiArrayLen = ( USHORT ) hb_arrayLen( pDec );
      if( uiArrayLen > uiFields )
         uiArrayLen = uiFields;
      for( uiCount = 1; uiCount <= uiArrayLen; uiCount++ )
      {
         SELF_FIELDINFO( ( AREAP ) s_pCurrArea->pArea, uiCount, DBS_DEC, pItem );
         hb_arraySet( pDec, uiCount, pItem );
      }
   }

   hb_itemRelease( pItem );
   hb_retni( uiArrayLen );
}

HB_FUNC( ALIAS )
{
   USHORT uiArea;
   LPAREANODE pAreaNode;
   char * szAlias;

   uiArea = hb_parni( 1 );
   uiArea = uiArea ? uiArea : s_uiCurrArea;
   pAreaNode = s_pWorkAreas;
   while( pAreaNode )
   {
      if( ( ( AREAP ) pAreaNode->pArea )->uiArea == uiArea )
      {
         if( ( ( AREAP ) pAreaNode->pArea )->atomAlias &&
             ( ( PHB_DYNS ) ( ( AREAP ) pAreaNode->pArea )->atomAlias )->hArea )
         {
            szAlias = ( char * ) hb_xgrab( HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 );
            SELF_ALIAS( ( AREAP ) pAreaNode->pArea, ( BYTE * ) szAlias );
            hb_retc( szAlias );
            hb_xfree( szAlias );
            return;
         }
         break;
      }
      pAreaNode = pAreaNode->pNext;
   }
   hb_retc( "" );
}

HB_FUNC( DBEVAL )
{
   DBEVALINFO pEvalInfo;

   if( s_pCurrArea )
   {
      pEvalInfo.itmBlock = hb_param( 1, HB_IT_BLOCK );
      if( !pEvalInfo.itmBlock )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBEVAL" );
         return;
      }

      pEvalInfo.dbsci.itmCobFor = hb_param( 2, HB_IT_BLOCK );
      if( !pEvalInfo.dbsci.itmCobFor && !ISNIL( 2 ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBEVAL" );
         return;
      }

      pEvalInfo.dbsci.itmCobWhile = hb_param( 3, HB_IT_BLOCK );
      if( !pEvalInfo.dbsci.itmCobWhile && !ISNIL( 3 ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBEVAL" );
         return;
      }

      pEvalInfo.dbsci.lNext = hb_param( 4, HB_IT_NUMERIC );
      if( !pEvalInfo.dbsci.lNext && !ISNIL( 4 ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBEVAL" );
         return;
      }

      pEvalInfo.dbsci.itmRecID = hb_param( 5, HB_IT_NUMERIC );
      if( !pEvalInfo.dbsci.itmRecID && !ISNIL( 5 ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBEVAL" );
         return;
      }

      pEvalInfo.dbsci.fRest = hb_param( 6, HB_IT_LOGICAL );
      if( !pEvalInfo.dbsci.fRest && !ISNIL( 6 ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBEVAL" );
         return;
      }

      SELF_DBEVAL( ( AREAP ) s_pCurrArea->pArea, &pEvalInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBEVAL" );
}

HB_FUNC( DBF )
{
   LPAREANODE pAreaNode = s_pWorkAreas;
   char * szAlias;

   while( pAreaNode )
   {
      if( ( ( AREAP ) pAreaNode->pArea )->uiArea == s_uiCurrArea )
      {
         if( ( ( AREAP ) pAreaNode->pArea )->atomAlias &&
             ( ( PHB_DYNS ) ( ( AREAP ) pAreaNode->pArea )->atomAlias )->hArea )
         {
            szAlias = ( char * ) hb_xgrab( HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 );
            SELF_ALIAS( ( AREAP ) pAreaNode->pArea, ( BYTE * ) szAlias );
            hb_retc( szAlias );
            hb_xfree( szAlias );
            return;
         }
         break;
      }
      pAreaNode = pAreaNode->pNext;
   }
   hb_retc( "" );
}

HB_FUNC( BOF )
{
   BOOL bBof = TRUE;

   if( s_pCurrArea )
      SELF_BOF( ( AREAP ) s_pCurrArea->pArea, &bBof );
   hb_retl( bBof );
}

HB_FUNC( DBAPPEND )
{
   BOOL bUnLockAll;

   if( s_pCurrArea )
   {
      bUnLockAll = ISLOG( 1 ) ? hb_parl( 1 ) : TRUE;
      s_bNetError = FALSE;
      if( SELF_APPEND( ( AREAP ) s_pCurrArea->pArea, bUnLockAll ) == FAILURE )
      {
         s_bNetError = TRUE;           /* Temp fix! What about other types of errors? */
      }
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBAPPEND" );
}

HB_FUNC( DBCLEARFILTER )
{
   if( s_pCurrArea )
      SELF_CLEARFILTER( ( AREAP ) s_pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBCLEARFILTER" );
}

HB_FUNC( DBCLOSEALL )
{
   hb_rddCloseAll();
}

HB_FUNC( DBCLOSEAREA )
{
   if( s_pCurrArea )
      hb_rddReleaseCurrentArea();
}

HB_FUNC( DBCOMMIT )
{
   if( s_pCurrArea )
      SELF_FLUSH( ( AREAP ) s_pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBCOMMIT" );
}

HB_FUNC( DBCOMMITALL )
{
   LPAREANODE pAreaNode;

   pAreaNode = s_pWorkAreas;
   while( pAreaNode )
   {
      SELF_FLUSH( ( AREAP ) pAreaNode->pArea );
      pAreaNode = pAreaNode->pNext;
   }
}

HB_FUNC( __DBCONTINUE )
{
   if( !s_pCurrArea )
   {
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBCONTINUE" );
      return;
   }

   if( !( ( AREAP ) s_pCurrArea->pArea )->dbsi.itmCobFor )
      return;

   ( ( AREAP ) s_pCurrArea->pArea )->fFound = FALSE;
   SELF_SKIP( ( AREAP ) s_pCurrArea->pArea, 1 );
   if( ( ( AREAP ) s_pCurrArea->pArea )->fEof )
      return;

   ( ( AREAP ) s_pCurrArea->pArea )->fFound = hb_itemGetL( hb_vmEvalBlock( ( ( AREAP ) s_pCurrArea->pArea )->dbsi.itmCobFor ) );
   while( !( ( AREAP ) s_pCurrArea->pArea )->fEof && !( ( AREAP ) s_pCurrArea->pArea )->fFound )
   {
      SELF_SKIP( ( AREAP ) s_pCurrArea->pArea, 1 );
      ( ( AREAP ) s_pCurrArea->pArea )->fFound = hb_itemGetL( hb_vmEvalBlock( ( ( AREAP ) s_pCurrArea->pArea )->dbsi.itmCobFor ) );
   }
}

HB_FUNC( DBCREATE )
{
   char * szDriver, * szFileName;
   char cDriverBuffer[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH ];
   char szAlias[ HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 ];
   char szSavedFileName[ _POSIX_PATH_MAX + 1 ];
   USHORT uiSize, uiLen, uiPrevArea;
   DBOPENINFO pInfo;
   PHB_FNAME pFileName;
   PHB_ITEM pStruct, pFieldDesc, pFileExt;
   BOOL bOpen;

   hb_retl( FALSE );
   szFileName = hb_parc( 1 );
   pStruct = hb_param( 2 , HB_IT_ARRAY );
   if( pStruct )
      uiLen = ( USHORT ) hb_arrayLen( pStruct );
   else
      uiLen = 0;

   if( ( strlen( szFileName ) == 0 ) || !pStruct || uiLen == 0 )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, "DBCREATE" );
      return;
   }

   for( uiSize = 0; uiSize < uiLen; uiSize++ )
   {
      pFieldDesc = hb_arrayGetItemPtr( pStruct, uiSize + 1 );
      if( hb_arrayLen( pFieldDesc ) < 4 )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, "DBCREATE" );
         return;
      }

      /* Validate items types of fields */
      if( !( hb_arrayGetType( pFieldDesc, 1 ) & HB_IT_STRING ) ||
          !( hb_arrayGetType( pFieldDesc, 2 ) & HB_IT_STRING ) ||
          !( hb_arrayGetType( pFieldDesc, 3 ) & HB_IT_NUMERIC ) ||
          !( hb_arrayGetType( pFieldDesc, 4 ) & HB_IT_NUMERIC ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, "DBCREATE" );
         return;
      }
   }

   uiPrevArea = s_uiCurrArea;
   if( !ISLOG( 4 ) )
   {
      bOpen = FALSE;
      hb_rddSelectFirstAvailable();
   }
   else
   {
      bOpen = TRUE;
      if( hb_parl( 4 ) )
         hb_rddSelectFirstAvailable();
      else if( s_pCurrArea )                  /* If current WorkArea is used then close it */
         hb_rddReleaseCurrentArea();
   }

   hb_rddCheck();
   uiLen = ( USHORT ) hb_parclen( 3 );
   if( uiLen > 0 )
   {
      if( uiLen > HARBOUR_MAX_RDD_DRIVERNAME_LENGTH )
         uiLen = HARBOUR_MAX_RDD_DRIVERNAME_LENGTH;

      hb_strncpyUpper( cDriverBuffer, hb_parc( 3 ), uiLen );
      szDriver = cDriverBuffer;
   }
   else
      szDriver = s_szDefDriver;

   pFileName = hb_fsFNameSplit( szFileName );
   strncpy( szAlias, hb_parc( 5 ), HARBOUR_MAX_RDD_ALIAS_LENGTH );
   uiLen = strlen( szAlias );
   if( uiLen == 0 )
      strncpy( szAlias, pFileName->szName, HARBOUR_MAX_RDD_ALIAS_LENGTH );
   else if( uiLen == 1 )
   {
      /* Alias with a single letter. Only are valid 'L' and > 'M' */
      if( toupper( szAlias[ 0 ] ) < 'N' && toupper( szAlias[ 0 ] ) != 'L' )
      {
         hb_xfree( pFileName );
         hb_errRT_DBCMD( EG_DUPALIAS, EDBCMD_DUPALIAS, NULL, "DBCREATE" );
         return;
      }
   }

   /* Create a new WorkArea node */
   if( !hb_rddInsertAreaNode( szDriver ) )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, "DBCREATE" );
      return;
   }

   szFileName = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 1 );
   strncpy( szFileName, hb_parc( 1 ), _POSIX_PATH_MAX );
   if( !pFileName->szExtension )
   {
      pFileExt = hb_itemPutC( NULL, "" );
      SELF_INFO( ( AREAP ) s_pCurrArea->pArea, DBI_TABLEEXT, pFileExt );
      strncat( szFileName, hb_itemGetCPtr( pFileExt ), _POSIX_PATH_MAX -
               strlen( szFileName ) );
      hb_itemRelease( pFileExt );
   }
   hb_xfree( pFileName );

   /* Save filename for later use */
   strcpy( szSavedFileName, szFileName );

   /* Fill pInfo structure */
   pInfo.uiArea = s_uiCurrArea;
   pInfo.abName = ( BYTE * ) szFileName;
   pInfo.atomAlias = ( BYTE * ) szAlias;
   pInfo.fShared = FALSE;
   pInfo.fReadonly = FALSE;

   ( ( AREAP ) s_pCurrArea->pArea )->atomAlias = hb_dynsymGet( ( char * ) szAlias );
   ( ( PHB_DYNS ) ( ( AREAP ) s_pCurrArea->pArea )->atomAlias )->hArea = s_uiCurrArea;
   ( ( AREAP ) s_pCurrArea->pArea )->uiArea = s_uiCurrArea;

   if( SELF_CREATEFIELDS( ( AREAP ) s_pCurrArea->pArea, pStruct ) == FAILURE )
   {
      hb_xfree( szFileName );
      hb_rddReleaseCurrentArea();
      hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, "DBCREATE" );
      return;
   }

   if( SELF_CREATE( ( AREAP ) s_pCurrArea->pArea, &pInfo ) == FAILURE )
   {
      hb_xfree( szFileName );
      hb_rddReleaseCurrentArea();
      hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, "DBCREATE" );
      return;
   }

   hb_xfree( szFileName );
   if( !bOpen )
   {
      hb_rddReleaseCurrentArea();
      hb_rddSelectWorkAreaNumber( uiPrevArea );
   }
   else
   {
      USHORT uiAreaSize, uiRddID;
      struct _RDDFUNCS * lprfsHost = ( ( AREAP ) s_pCurrArea->pArea )->lprfsHost;
      uiRddID = ( ( AREAP ) s_pCurrArea->pArea )->rddID;
      SELF_STRUCTSIZE( ( AREAP ) s_pCurrArea->pArea, &uiAreaSize );
      /* Close and release WorkArea */
      SELF_CLOSE( ( AREAP ) s_pCurrArea->pArea );
      SELF_RELEASE( ( AREAP ) s_pCurrArea->pArea );

      /* Prepare WorkArea and open it */
      s_pCurrArea->pArea = ( AREAP ) hb_xgrab( uiAreaSize );
      memset( s_pCurrArea->pArea, 0, uiAreaSize );
      ( ( AREAP ) s_pCurrArea->pArea )->lprfsHost = lprfsHost;
      ( ( AREAP ) s_pCurrArea->pArea )->rddID = uiRddID;
      SELF_NEW( ( AREAP ) s_pCurrArea->pArea );
      //pInfo.abName = ( BYTE * )  hb_xgrab( _POSIX_PATH_MAX + 1 );
      szFileName = ( char * )  hb_xgrab( _POSIX_PATH_MAX + 1 );
      pInfo.abName = ( BYTE * ) szFileName;
      strcpy( ( char * ) pInfo.abName, szSavedFileName );
      pInfo.fShared = !hb_set.HB_SET_EXCLUSIVE;
      ( ( AREAP ) s_pCurrArea->pArea )->uiArea = s_uiCurrArea;
      if( SELF_OPEN( ( AREAP ) s_pCurrArea->pArea, &pInfo ) == FAILURE )
      {
         s_bNetError = TRUE;           /* Temp fix! What about other types of errors? */
         hb_rddReleaseCurrentArea();
      }
      else
         hb_retl( TRUE );
      //hb_xfree( pInfo.abName );
      hb_xfree( szFileName );
   }
   //hb_xfree( szFileName );
}

HB_FUNC( DBDELETE )
{
   if( s_pCurrArea )
      SELF_DELETE( ( AREAP ) s_pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBDELETE" );
}

HB_FUNC( DBFILTER )
{
   PHB_ITEM pFilter;

   if( s_pCurrArea )
   {
      pFilter = hb_itemPutC( NULL, "" );
      SELF_FILTERTEXT( ( AREAP ) s_pCurrArea->pArea, pFilter );
      hb_retc( hb_itemGetCPtr( pFilter ) );
      hb_itemRelease( pFilter );
   }
   else
      hb_retc( "" );
}

HB_FUNC( DBGOBOTTOM )
{
   if( s_pCurrArea )
      SELF_GOBOTTOM( ( AREAP ) s_pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBGOBOTTOM" );
}

HB_FUNC( DBGOTO )
{
   PHB_ITEM pItem;

   if( !s_pCurrArea )
   {
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBGOTO" );
      return;
   }

   pItem = hb_param( 1, HB_IT_ANY );
   if( !pItem )
      hb_errRT_DBCMD( EG_ARG, EDBCMD_NOVAR, NULL, "DBGOTO" );
   else
      SELF_GOTOID( ( AREAP ) s_pCurrArea->pArea, pItem );
}

HB_FUNC( DBGOTOP )
{
   if( s_pCurrArea )
      SELF_GOTOP( ( AREAP ) s_pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBGOTOP" );
}

HB_FUNC( __DBLOCATE )
{
   PHB_ITEM pFor, pNewFor, pWhile, pNext, pRecord, pRest, pNewRest;
   DBSCOPEINFO pScopeInfo;
   ULONG lNext;
   BOOL bFor, bWhile;

   if( !s_pCurrArea )
   {
      hb_errRT_DBCMD( EG_NOTABLE, EG_NOTABLE, NULL, "__DBLOCATE" );
      return;
   }

   ( ( AREAP ) s_pCurrArea->pArea )->fFound = FALSE;
   memset( &pScopeInfo, 0, sizeof( DBSCOPEINFO ) );
   pFor     = hb_param( 1, HB_IT_BLOCK );
   pWhile   = hb_param( 2, HB_IT_BLOCK );
   pNext    = hb_param( 3, HB_IT_NUMERIC );
   pRecord  = hb_param( 4, HB_IT_NUMERIC );
   pRest    = hb_param( 5, HB_IT_LOGICAL );
   pNewRest = NULL;
   if( pWhile )
   {
      pNewRest = hb_itemPutL( NULL, TRUE );
      pScopeInfo.fRest = pNewRest;
   }
   if( !pFor )
      pNewFor = hb_itemPutL( NULL, TRUE );
   else
      pNewFor = hb_itemNew( pFor );
   pScopeInfo.itmCobFor = pNewFor;
   if( !pRest )
   {
      pNewRest = hb_itemPutL( pNewRest, FALSE );
      pScopeInfo.fRest = pNewRest;
   }
   SELF_SETLOCATE( ( AREAP ) s_pCurrArea->pArea, &pScopeInfo );
   ( ( AREAP ) s_pCurrArea->pArea )->fFound = FALSE;
   if( pRecord )
   {
      SELF_GOTOID( ( AREAP ) s_pCurrArea->pArea, pRecord );
      if( ( ( AREAP ) s_pCurrArea->pArea )->fEof )
         goto ExitLocate ;
      if( hb_itemType( pWhile ) == HB_IT_BLOCK )
         bWhile = hb_itemGetL( hb_vmEvalBlock( pWhile ) );
      else
         bWhile = TRUE;
      if( hb_itemType( pNewFor ) == HB_IT_BLOCK )
      {
         bFor = hb_itemGetL( hb_vmEvalBlock( pNewFor ) );
         ( ( AREAP ) s_pCurrArea->pArea )->fFound = ( bWhile && bFor );
      }
      else
         ( ( AREAP ) s_pCurrArea->pArea )->fFound = ( bWhile && hb_itemGetL( pNewFor ) );
   }
   else if( pNext )
   {
      lNext = hb_parnl( 3 );
      if( ( ( AREAP ) s_pCurrArea->pArea )->fEof || lNext <= 0 )
         goto ExitLocate ;
      if( hb_itemType( pWhile ) == HB_IT_BLOCK )
         bWhile = hb_itemGetL( hb_vmEvalBlock( pWhile ) );
      else
         bWhile = TRUE;
      if( hb_itemType( pNewFor ) == HB_IT_BLOCK )
         bFor = hb_itemGetL( hb_vmEvalBlock( pNewFor ) );
      else
         bFor = hb_itemGetL( pNewFor );
      while( !( ( AREAP ) s_pCurrArea->pArea )->fEof && lNext-- > 0 && bWhile && !bFor )
      {
         SELF_SKIP( ( AREAP ) s_pCurrArea->pArea, 1 );
         if( ( ( AREAP ) s_pCurrArea->pArea )->fEof  )
            bFor = FALSE;
         else
         {
            if( hb_itemType( pWhile ) == HB_IT_BLOCK )
               bWhile = hb_itemGetL( hb_vmEvalBlock( pWhile ) );
            else
               bWhile = TRUE;
            if( hb_itemType( pNewFor ) == HB_IT_BLOCK )
               bFor = hb_itemGetL( hb_vmEvalBlock( pNewFor ) );
            else
               bFor = hb_itemGetL( pNewFor );
         }
      }
      ( ( AREAP ) s_pCurrArea->pArea )->fFound = bFor;
   }
   else if( hb_itemGetL( pRest ) )
   {
      if( ( ( AREAP ) s_pCurrArea->pArea )->fEof )
         goto ExitLocate ;
      if( hb_itemType( pWhile ) == HB_IT_BLOCK )
         bWhile = hb_itemGetL( hb_vmEvalBlock( pWhile ) );
      else
         bWhile = TRUE;
      if( hb_itemType( pNewFor ) == HB_IT_BLOCK )
         bFor = hb_itemGetL( hb_vmEvalBlock( pNewFor ) );
      else
         bFor = hb_itemGetL( pNewFor );
      while( !( ( AREAP ) s_pCurrArea->pArea )->fEof && bWhile && !bFor )
      {
         SELF_SKIP( ( AREAP ) s_pCurrArea->pArea, 1 );
         if( ( ( AREAP ) s_pCurrArea->pArea )->fEof  )
            bFor = FALSE;
         else
         {
            if( hb_itemType( pWhile ) == HB_IT_BLOCK )
               bWhile = hb_itemGetL( hb_vmEvalBlock( pWhile ) );
            else
               bWhile = TRUE;
            if( hb_itemType( pNewFor ) == HB_IT_BLOCK )
               bFor = hb_itemGetL( hb_vmEvalBlock( pNewFor ) );
            else
               bFor = hb_itemGetL( pNewFor );
         }
      }
      ( ( AREAP ) s_pCurrArea->pArea )->fFound = bFor;
   }
   else
   {
      SELF_GOTOP( ( AREAP ) s_pCurrArea->pArea );
      if( ( ( AREAP ) s_pCurrArea->pArea )->fEof )
         goto ExitLocate ;
      if( hb_itemType( pNewFor ) == HB_IT_BLOCK )
         bFor = hb_itemGetL( hb_vmEvalBlock( pNewFor ) );
      else
         bFor = hb_itemGetL( pNewFor );
      while( !( ( AREAP ) s_pCurrArea->pArea )->fEof && !bFor )
      {
         SELF_SKIP( ( AREAP ) s_pCurrArea->pArea, 1 );
         if( ( ( AREAP ) s_pCurrArea->pArea )->fEof  )
            bFor = FALSE;
         else
         {
            if( hb_itemType( pNewFor ) == HB_IT_BLOCK )
               bFor = hb_itemGetL( hb_vmEvalBlock( pNewFor ) );
            else
               bFor = hb_itemGetL( pNewFor );
         }
      }
      ( ( AREAP ) s_pCurrArea->pArea )->fFound = bFor;
   }

ExitLocate :
   /* Release items */
   hb_itemRelease( pNewFor );
   hb_itemRelease( pNewRest );
}

HB_FUNC( __DBSETLOCATE )
{
   PHB_ITEM pLocate;
   DBSCOPEINFO pScopeInfo;

   if( s_pCurrArea )
   {
      pLocate = hb_param( 1, HB_IT_BLOCK );
      if( pLocate )
      {
         memset( &pScopeInfo, 0, sizeof( DBSCOPEINFO ) );
         pScopeInfo.itmCobFor = pLocate;
         SELF_SETLOCATE( ( AREAP ) s_pCurrArea->pArea, &pScopeInfo );
      }
   }
}

HB_FUNC( __DBPACK )
{
   PHB_ITEM pBlock, pEvery;

   if( s_pCurrArea )
   {
      /*
       * Additional feature: __dbPack( [<bBlock>, [<nEvery>] )
       * Code Block to execute for every record.
       */
      pBlock = hb_param( 1, HB_IT_BLOCK );
      if( pBlock )
      {
         hb_itemRelease( ( ( AREAP ) s_pCurrArea->pArea )->valResult );
         ( ( AREAP ) s_pCurrArea->pArea )->valResult = hb_itemArrayNew( 2 );
         hb_arraySet( ( ( AREAP ) s_pCurrArea->pArea )->valResult, 1, pBlock );
         pEvery = hb_param( 2, HB_IT_ANY );
         if( pEvery && HB_IS_NUMERIC( pEvery ) )
            hb_arraySet( ( ( AREAP ) s_pCurrArea->pArea )->valResult, 2, pEvery );
      }
      else
         hb_itemClear( ( ( AREAP ) s_pCurrArea->pArea )->valResult );
      SELF_PACK( ( AREAP ) s_pCurrArea->pArea );
      if( pBlock )
         hb_itemClear( ( ( AREAP ) s_pCurrArea->pArea )->valResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "__DBPACK" );
}

HB_FUNC( DBRECALL )
{
   if( s_pCurrArea )
      SELF_RECALL( ( AREAP ) s_pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBRECALL" );
}

HB_FUNC( DBRLOCK )
{
   DBLOCKINFO dbLockInfo;

   dbLockInfo.fResult = FALSE;
   if( s_pCurrArea )
   {
      dbLockInfo.itmRecID = hb_param( 1, HB_IT_ANY );
      if( !dbLockInfo.itmRecID || !HB_IS_NUMERIC( dbLockInfo.itmRecID ) )
         dbLockInfo.uiMethod = DBLM_EXCLUSIVE;
      else
         dbLockInfo.uiMethod = DBLM_MULTIPLE;
      SELF_LOCK( ( AREAP ) s_pCurrArea->pArea, &dbLockInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBRLOCK" );

   hb_retl( dbLockInfo.fResult );
}

HB_FUNC( DBRLOCKLIST )
{
   PHB_ITEM pList;

   pList = hb_itemArrayNew( 0 );
   if( s_pCurrArea )
      SELF_INFO( ( AREAP ) s_pCurrArea->pArea, DBI_GETLOCKARRAY, pList );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBRLOCKLIST" );

   hb_itemRelease( hb_itemReturn( pList ) );
}

HB_FUNC( DBRUNLOCK )
{
   if( s_pCurrArea )
      SELF_UNLOCK( ( AREAP ) s_pCurrArea->pArea, hb_parnl( 1 ) );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBRUNLOCK" );
}

HB_FUNC( DBSEEK )
{
   PHB_ITEM pKey;
   BOOL bSoftSeek, bFindLast;

   if( s_pCurrArea )
   {
      if( !ISNIL( 1 ) )
      {
         pKey = hb_param( 1, HB_IT_ANY );
         bSoftSeek = ISLOG( 2 ) ? hb_parl( 2 ) : hb_set.HB_SET_SOFTSEEK;
         bFindLast = ISLOG( 3 ) ? hb_parl( 3 ) : FALSE;
         if( SELF_SEEK( ( AREAP ) s_pCurrArea->pArea, bSoftSeek, pKey, bFindLast ) == SUCCESS )
         {
            hb_retl( ( ( AREAP ) s_pCurrArea->pArea )->fFound );
            return;
         }
      }
      else
         hb_errRT_DBCMD( EG_ARG, EDBCMD_SEEK_BADPARAMETER, NULL, "DBSEEK" );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBSEEK" );

   hb_retl( FALSE );
}

HB_FUNC( DBSELECTAREA )
{
   USHORT uiNewArea;
   char * szAlias;
   LPAREANODE pAreaNode;
   ULONG ulLen;

   if( ISCHAR( 1 ) )
   {
      szAlias = hb_parc( 1 );
      ulLen = strlen( szAlias );
      if( ulLen >= 1 && szAlias[ 0 ] >= '0' && szAlias[ 0 ] <= '9' )
         uiNewArea = atoi( szAlias );
      else if( ulLen == 1 && toupper( szAlias[ 0 ] ) >= 'A' && toupper( szAlias[ 0 ] ) <= 'K' )
         uiNewArea = toupper( szAlias[ 0 ] ) - 'A' + 1;
      else
      {
         if( ( uiNewArea = hb_rddSelect( szAlias ) ) == 0 )
         {
            hb_errRT_BASE( EG_NOALIAS, EDBCMD_NOALIAS, NULL, szAlias, 0 );
            return;
         }
      }
   }
   else
      uiNewArea = hb_parni( 1 );

   if( uiNewArea == 0 )
      hb_rddSelectFirstAvailable();
   else
      s_uiCurrArea = uiNewArea;

   pAreaNode = s_pWorkAreas;
   while( pAreaNode )
   {
      if( ( ( AREAP ) pAreaNode->pArea )->uiArea == s_uiCurrArea )
      {
         s_pCurrArea = pAreaNode; /* Select a valid WorkArea */
         return;
      }
      pAreaNode = pAreaNode->pNext;
   }
   s_pCurrArea = NULL; /* Selected WorkArea is closed */
}

HB_FUNC( __DBSETFOUND )
{
   PHB_ITEM pFound;

   if( s_pCurrArea )
   {
      pFound = hb_param( 1, HB_IT_LOGICAL );
      if( pFound )
         ( ( AREAP ) s_pCurrArea->pArea )->fFound = hb_itemGetL( pFound );
   }
}

HB_FUNC( DBSKIP )
{
   if( s_pCurrArea )
      SELF_SKIP( ( AREAP ) s_pCurrArea->pArea, ISNUM( 1 ) ? hb_parnl( 1 ) : 1 );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBSKIP" );
}

HB_FUNC( DBSETFILTER )
{
   PHB_ITEM pBlock, pText;
   DBFILTERINFO pFilterInfo;

   if( s_pCurrArea )
   {
      pBlock = hb_param( 1, HB_IT_BLOCK );
      if( pBlock )
      {
         pText = hb_param( 2, HB_IT_STRING );
         pFilterInfo.itmCobExpr = pBlock;
         if( pText )
            pFilterInfo.abFilterText = pText;
         else
            pFilterInfo.abFilterText = hb_itemPutC( NULL, "" );
         SELF_SETFILTER( ( AREAP ) s_pCurrArea->pArea, &pFilterInfo );
         if( !pText )
            hb_itemRelease( pFilterInfo.abFilterText );
      }
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBSETFILTER" );
}

HB_FUNC( DBSTRUCT )
{
   PHB_ITEM pItem, pData;
   USHORT uiFields, uiCount;

   hb_arrayNew( &hb_stack.Return, 0 );

   if( s_pCurrArea )
   {
      SELF_FIELDCOUNT( ( AREAP ) s_pCurrArea->pArea, &uiFields );
      pData = hb_itemNew( NULL );
      pItem = hb_itemNew( NULL );
      for( uiCount = 1; uiCount <= uiFields; uiCount++ )
      {
         hb_arrayNew( pItem, 4 );
         SELF_FIELDINFO( ( AREAP ) s_pCurrArea->pArea, uiCount, DBS_NAME, pData );
         hb_arraySet( pItem, 1, pData );
         SELF_FIELDINFO( ( AREAP ) s_pCurrArea->pArea, uiCount, DBS_TYPE, pData );
         hb_arraySet( pItem, 2, pData );
         SELF_FIELDINFO( ( AREAP ) s_pCurrArea->pArea, uiCount, DBS_LEN, pData );
         hb_arraySet( pItem, 3, pData );
         SELF_FIELDINFO( ( AREAP ) s_pCurrArea->pArea, uiCount, DBS_DEC, pData );
         hb_arraySet( pItem, 4, pData );
         hb_arrayAdd( &hb_stack.Return, pItem );
      }
      hb_itemRelease( pItem );
      hb_itemRelease( pData );
   }
}

HB_FUNC( DBTABLEEXT )
{
   LPRDDNODE pRddNode;
   AREAP pTempArea;
   USHORT uiSize, uiRddID;
   PHB_ITEM pItem;

   if( !s_pCurrArea )
   {
      hb_rddCheck();
      pRddNode = hb_rddFindNode( s_szDefDriver, &uiRddID );
      if( !pRddNode )
      {
         hb_retc( "" );
         return;
      }
      uiSize = sizeof( AREA );    /* Default Size Area */
      pTempArea = ( AREAP ) hb_xgrab( uiSize );
      memset( pTempArea, 0, uiSize );
      pTempArea->lprfsHost = &pRddNode->pTable;

      /* Need more space? */
      SELF_STRUCTSIZE( ( AREAP ) pTempArea, &uiSize );
      if( uiSize > sizeof( AREA ) )   /* Size of Area changed */
      {
         pTempArea = ( AREAP ) hb_xrealloc( pTempArea, uiSize );
         memset( pTempArea, 0, uiSize );
         pTempArea->lprfsHost = &pRddNode->pTable;
      }

      pRddNode->uiAreaSize = uiSize; /* Update the size of WorkArea */
      pTempArea->rddID = uiRddID;

      if( SELF_NEW( ( AREAP ) pTempArea ) == FAILURE )
         hb_retc( "" );
      else
      {
         pItem = hb_itemPutC( NULL, "" );
         SELF_INFO( ( AREAP ) pTempArea, DBI_TABLEEXT, pItem );
         hb_retc( hb_itemGetCPtr( pItem ) );
         hb_itemRelease( pItem );
         SELF_RELEASE( pTempArea );
      }
   }
   else
   {
      pItem = hb_itemPutC( NULL, "" );
      SELF_INFO( ( AREAP ) s_pCurrArea->pArea, DBI_TABLEEXT, pItem );
      hb_retc( hb_itemGetCPtr( pItem ) );
      hb_itemRelease( pItem );
   }
}

HB_FUNC( DBUNLOCK )
{

   if( s_pCurrArea )
      SELF_UNLOCK( ( AREAP ) s_pCurrArea->pArea, 0 );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBUNLOCK" );
}

HB_FUNC( DBUNLOCKALL )
{
   LPAREANODE pTempArea;

   pTempArea = s_pWorkAreas;
   while( pTempArea )
   {
      SELF_UNLOCK( ( AREAP ) pTempArea->pArea, 0 );
      pTempArea = pTempArea->pNext;
   }
}

HB_FUNC( DBUSEAREA )
{
   char * szDriver, * szFileName;
   USHORT uiLen;
   DBOPENINFO pInfo;
   PHB_FNAME pFileName;
   PHB_ITEM pFileExt;
   char szDriverBuffer[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 ];
   char szAlias[ HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 ];

   s_bNetError = FALSE;

   /* New area? */
   if( hb_parl( 1 ) )
      hb_rddSelectFirstAvailable();
   else if( s_pCurrArea )                 /* If current WorkArea is in use then close it */
      hb_rddReleaseCurrentArea();

   hb_rddCheck();
   uiLen = ( USHORT ) hb_parclen( 2 );
   if( uiLen > 0 )
   {
      if( uiLen > HARBOUR_MAX_RDD_DRIVERNAME_LENGTH )
         uiLen = HARBOUR_MAX_RDD_DRIVERNAME_LENGTH;

      hb_strncpyUpper( szDriverBuffer, hb_parc( 2 ), uiLen );
      szDriver = szDriverBuffer;
   }
   else
      szDriver = s_szDefDriver;

   szFileName = hb_parc( 3 );
   if( strlen( szFileName ) == 0 )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_USE_BADPARAMETER, NULL, "DBUSEAREA" );
      return;
   }

   pFileName = hb_fsFNameSplit( szFileName );
   strncpy( szAlias, hb_parc( 4 ), HARBOUR_MAX_RDD_ALIAS_LENGTH );
   if( strlen( szAlias ) == 0 )
      strncpy( szAlias, pFileName->szName, HARBOUR_MAX_RDD_ALIAS_LENGTH );
   uiLen = strlen( szAlias );
   if( szAlias[ 0 ] >= '0' && szAlias[ 0 ] <= '9' )
   {
      hb_xfree( pFileName );
      hb_errRT_DBCMD( EG_DUPALIAS, EDBCMD_DUPALIAS, NULL, "DBUSEAREA" );
      return;
   }
   if( uiLen == 1 )
   {
      /* Alias with a single letter. Only are valid 'L' and > 'M' */
      if( toupper( szAlias[ 0 ] ) < 'N' && toupper( szAlias[ 0 ] ) != 'L' )
      {
         hb_xfree( pFileName );
         hb_errRT_DBCMD( EG_DUPALIAS, EDBCMD_DUPALIAS, NULL, "DBUSEAREA" );
         return;
      }
   }

   /* Create a new WorkArea node */
   if( !hb_rddInsertAreaNode( szDriver ) )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, "DBUSEAREA" );
      return;
   }

   szFileName = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 1 );
   strncpy( szFileName, hb_parc( 3 ), _POSIX_PATH_MAX );
   if( !pFileName->szExtension )
   {
      pFileExt = hb_itemPutC( NULL, "" );
      SELF_INFO( ( AREAP ) s_pCurrArea->pArea, DBI_TABLEEXT, pFileExt );
      strncat( szFileName, hb_itemGetCPtr( pFileExt ), _POSIX_PATH_MAX -
               strlen( szFileName ) );
      hb_itemRelease( pFileExt );
   }
   hb_xfree( pFileName );

   /* Fill pInfo structure */
   pInfo.uiArea = s_uiCurrArea;
   pInfo.abName = ( BYTE * ) szFileName;
   pInfo.atomAlias = ( BYTE * ) szAlias;
   pInfo.fShared = ISLOG( 5 ) ? hb_parl( 5 ) : !hb_set.HB_SET_EXCLUSIVE;
   pInfo.fReadonly = ISLOG( 6 ) ? hb_parl( 6 ) : FALSE;

   ( ( AREAP ) s_pCurrArea->pArea )->uiArea = s_uiCurrArea;

   /* Open file */
   if( SELF_OPEN( ( AREAP ) s_pCurrArea->pArea, &pInfo ) == FAILURE )
   {
      s_bNetError = TRUE;           /* Temp fix! What about other types of errors? */
      hb_xfree( pInfo.abName );
      hb_rddReleaseCurrentArea();
      return;
   }
   hb_xfree( szFileName );
}

HB_FUNC( __DBZAP )
{
   if( s_pCurrArea )
      SELF_ZAP( ( AREAP ) s_pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "__DBZAP" );
}

HB_FUNC( DELETED )
{
   BOOL bDeleted = FALSE;

   if( s_pCurrArea )
      SELF_DELETED( ( AREAP ) s_pCurrArea->pArea, &bDeleted );
   hb_retl( bDeleted );
}

HB_FUNC( EOF )
{
   BOOL bEof = TRUE;

   if( s_pCurrArea )
      SELF_EOF( ( AREAP ) s_pCurrArea->pArea, &bEof );
   hb_retl( bEof );
}

HB_FUNC( FCOUNT )
{
   USHORT uiFields = 0;

   if( s_pCurrArea )
      SELF_FIELDCOUNT( ( AREAP ) s_pCurrArea->pArea, &uiFields );
   hb_retni( uiFields );
}

HB_FUNC( FIELDGET )
{
   PHB_ITEM pItem;
   USHORT uiField, uiFields;

   pItem = hb_itemNew( NULL );
   uiField = hb_parni( 1 );

   if( s_pCurrArea && uiField )
   {
      if( SELF_FIELDCOUNT( ( AREAP ) s_pCurrArea->pArea, &uiFields ) == SUCCESS &&
          uiField > 0 && uiField <= uiFields )
         SELF_GETVALUE( ( AREAP ) s_pCurrArea->pArea, uiField, pItem );
   }

   hb_itemRelease( hb_itemReturn( pItem ) );
}

HB_FUNC( FIELDNAME )
{
   char * szName;
   USHORT uiFields, uiIndex;

   if( s_pCurrArea )
   {
      uiIndex = hb_parni( 1 );
      if( SELF_FIELDCOUNT( ( AREAP ) s_pCurrArea->pArea, &uiFields ) == SUCCESS &&
          uiIndex > 0 && uiIndex <= uiFields )
      {
         szName = ( char * ) hb_xgrab( HARBOUR_MAX_RDD_FIELDNAME_LENGTH + 1 );
         SELF_FIELDNAME( ( AREAP ) s_pCurrArea->pArea, hb_parni( 1 ), szName );
         hb_retc( szName );
         hb_xfree( szName );
         return;
      }
      hb_errRT_DBCMD( EG_ARG, EDBCMD_FIELDNAME_BADPARAMETER, NULL, "FIELDNAME" );
   }
   hb_retc( "" );
}

HB_FUNC( FIELDPOS )
{
   char szName[ HARBOUR_MAX_RDD_FIELDNAME_LENGTH ];

   if( s_pCurrArea )
   {
      hb_strncpyUpper( szName, hb_parc( 1 ), hb_parclen( 1 ) );
      hb_retni( hb_rddFieldIndex( ( AREAP ) s_pCurrArea->pArea, szName ) );
   }
   else
      hb_retni( 0 );
}

HB_FUNC( FIELDPUT )
{
   USHORT uiIndex;
   PHB_ITEM pItem;

   uiIndex = hb_parni( 1 );
   if( s_pCurrArea && uiIndex )
   {
      pItem = hb_param( 2, HB_IT_ANY );
      if( SELF_PUTVALUE( ( AREAP ) s_pCurrArea->pArea, uiIndex, pItem ) == SUCCESS )
         hb_itemReturn( pItem );
   }
}

HB_FUNC( FLOCK )
{
   DBLOCKINFO dbLockInfo;

   dbLockInfo.fResult = FALSE;
   if( s_pCurrArea )
   {
      dbLockInfo.uiMethod = DBLM_FILE;
      SELF_LOCK( ( AREAP ) s_pCurrArea->pArea, &dbLockInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "FLOCK" );

   hb_retl( dbLockInfo.fResult );
}

HB_FUNC( FOUND )
{
   BOOL bFound = FALSE;

   if( s_pCurrArea )
      SELF_FOUND( ( AREAP ) s_pCurrArea->pArea, &bFound );
   hb_retl( bFound );
}

HB_FUNC( HEADER )
{
   PHB_ITEM pRecSize;

   if( !s_pCurrArea )
      hb_retni( 0 );
   else
   {
      pRecSize = hb_itemNew( NULL );
      SELF_INFO( ( AREAP ) s_pCurrArea->pArea, DBI_GETHEADERSIZE, pRecSize );
      hb_itemRelease( hb_itemReturn( pRecSize ) );
   }
}

HB_FUNC( INDEXORD )
{
   DBORDERINFO pInfo;

   if( s_pCurrArea )
   {
      pInfo.itmResult = hb_itemPutNI( NULL, 0 );
      pInfo.itmOrder = NULL;
      SELF_ORDINFO( ( AREAP ) s_pCurrArea->pArea, DBOI_NUMBER, &pInfo );
      hb_retni( hb_itemGetNI( pInfo.itmResult ) );
      hb_itemRelease( pInfo.itmResult );
   }
   else
      hb_retni( 0 );
}

/* Same as RECCOUNT() */
HB_FUNC( LASTREC )
{
   ULONG ulRecCount = 0;

   if( s_pCurrArea )
      SELF_RECCOUNT( ( AREAP ) s_pCurrArea->pArea, &ulRecCount );
   hb_retnl( ulRecCount );
}

HB_FUNC( LOCK )
{
   DBLOCKINFO dbLockInfo;

   dbLockInfo.fResult = FALSE;
   if( s_pCurrArea )
   {
      dbLockInfo.uiMethod = DBLM_FILE;
      SELF_LOCK( ( AREAP ) s_pCurrArea->pArea, &dbLockInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "LOCK" );

   hb_retl( dbLockInfo.fResult );
}

HB_FUNC( LUPDATE )
{
   if( !s_pCurrArea )
      hb_itemPutDS( &hb_stack.Return, "" );
   else
      SELF_INFO( ( AREAP ) s_pCurrArea->pArea, DBI_LASTUPDATE, &hb_stack.Return );
}

HB_FUNC( NETERR )
{
   if( ISLOG( 1 ) )
      s_bNetError = hb_parl( 1 );

   hb_retl( s_bNetError );
}

HB_FUNC( ORDBAGEXT )
{
   DBORDERINFO pInfo;
   LPRDDNODE pRddNode;
   AREAP pTempArea;
   USHORT uiSize, uiRddID;

   pInfo.itmOrder = NULL;
   if( !s_pCurrArea )
   {
      hb_rddCheck();
      pRddNode = hb_rddFindNode( s_szDefDriver, &uiRddID );
      if( !pRddNode )
      {
         hb_retc( "" );
         return;
      }
      uiSize = sizeof( AREA );         /* Default Size Area */
      pTempArea = ( AREAP ) hb_xgrab( uiSize );
      memset( pTempArea, 0, uiSize );
      pTempArea->lprfsHost = &pRddNode->pTable;

      /* Need more space? */
      SELF_STRUCTSIZE( ( AREAP ) pTempArea, &uiSize );
      if( uiSize > sizeof( AREA ) )   /* Size of Area changed */
      {
         pTempArea = ( AREAP ) hb_xrealloc( pTempArea, uiSize );
         memset( pTempArea, 0, uiSize );
         pTempArea->lprfsHost = &pRddNode->pTable;
      }

      pRddNode->uiAreaSize = uiSize;  /* Update the size of WorkArea */
      pTempArea->rddID = uiRddID;

      if( SELF_NEW( ( AREAP ) pTempArea ) == FAILURE )
         hb_retc( "" );
      else
      {
         pInfo.itmResult = hb_itemPutC( NULL, "" );
         SELF_ORDINFO( pTempArea, DBOI_BAGEXT, &pInfo );
         hb_retc( hb_itemGetCPtr( pInfo.itmResult ) );
         hb_itemRelease( pInfo.itmResult );
         SELF_RELEASE( pTempArea );
      }
   }
   else
   {
      pInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( ( AREAP ) s_pCurrArea->pArea, DBOI_BAGEXT, &pInfo );
      hb_retc( hb_itemGetCPtr( pInfo.itmResult ) );
      hb_itemRelease( pInfo.itmResult );
   }
}

HB_FUNC( ORDBAGNAME )
{
   DBORDERINFO pOrderInfo;

   if( s_pCurrArea )
   {
      if ( ISNUM(1) || ISNIL(1) )
      {
         if ( hb_parnl(1) == 0 || ISNIL(1) )          /* if NIL or ask for 0, use current order  */
            pOrderInfo.itmOrder  = NULL;
         else
            pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );

      }else
      {
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
         if( !pOrderInfo.itmOrder )
         {
            hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "ORDBAGNAME" );
            return;
         }
      }
      pOrderInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( ( AREAP ) s_pCurrArea->pArea, DBOI_BAGNAME, &pOrderInfo );
      hb_retc( hb_itemGetCPtr( pOrderInfo.itmResult ) );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDBAGNAME" );
}

HB_FUNC( ORDCONDSET )
{
   LPDBORDERCONDINFO lpdbOrdCondInfo;
   char * szFor;
   ULONG ulLen;
   PHB_ITEM pItem;

   if( s_pCurrArea )
   {
      lpdbOrdCondInfo = ( LPDBORDERCONDINFO ) hb_xgrab( sizeof( DBORDERCONDINFO ) );
      szFor = hb_parc( 1 );
      ulLen = strlen( szFor );
      if( ulLen )
      {
         lpdbOrdCondInfo->abFor = ( BYTE * ) hb_xgrab( ulLen + 1 );
         strcpy( ( char * ) lpdbOrdCondInfo->abFor, szFor );
      }
      else
         lpdbOrdCondInfo->abFor = NULL;

      pItem = hb_param( 2, HB_IT_BLOCK );
      if( pItem )
      {
         lpdbOrdCondInfo->itmCobFor = hb_itemNew( NULL );
         hb_itemCopy( lpdbOrdCondInfo->itmCobFor, pItem );
      }
      else
         lpdbOrdCondInfo->itmCobFor = NULL;
      if( ISLOG( 3 ) )
         lpdbOrdCondInfo->fAll = hb_parl( 3 );
      else
         lpdbOrdCondInfo->fAll = TRUE;

      pItem = hb_param( 4, HB_IT_BLOCK );
      if( pItem )
      {
         lpdbOrdCondInfo->itmCobWhile = hb_itemNew( NULL );
         hb_itemCopy( lpdbOrdCondInfo->itmCobWhile, pItem );
      }
      else
         lpdbOrdCondInfo->itmCobWhile = NULL;

      pItem = hb_param( 5, HB_IT_BLOCK );
      if( pItem )
      {
         lpdbOrdCondInfo->itmCobEval = hb_itemNew( NULL );
         hb_itemCopy( lpdbOrdCondInfo->itmCobEval, pItem );
      }
      else
         lpdbOrdCondInfo->itmCobEval = NULL;

      lpdbOrdCondInfo->lStep       = hb_parnl( 6 );
      lpdbOrdCondInfo->lStartRecno = hb_parnl( 7 );
      lpdbOrdCondInfo->lNextCount  = hb_parnl( 8 );
      lpdbOrdCondInfo->lRecno      = hb_parnl( 9 );
      lpdbOrdCondInfo->fRest       = hb_parl( 10 );
      lpdbOrdCondInfo->fDescending = hb_parl( 11 );
      /* 12th parameter is always nil */
      lpdbOrdCondInfo->fAdditive   = hb_parl( 13 );
      lpdbOrdCondInfo->fUseCurrent = hb_parl( 14 );
      lpdbOrdCondInfo->fCustom     = hb_parl( 15 );
      lpdbOrdCondInfo->fNoOptimize = hb_parl( 16 );

      if( !lpdbOrdCondInfo->itmCobWhile )
         lpdbOrdCondInfo->fRest = TRUE;
      if( lpdbOrdCondInfo->lNextCount || lpdbOrdCondInfo->lRecno || lpdbOrdCondInfo->fRest )
         lpdbOrdCondInfo->fAll = FALSE;

      hb_retl( SELF_ORDSETCOND( ( AREAP ) s_pCurrArea->pArea, lpdbOrdCondInfo ) == SUCCESS );
   }
   else
      hb_retl( FALSE );
}

HB_FUNC( ORDCREATE )
{
   DBORDERCREATEINFO dbOrderInfo;

   if( s_pCurrArea )
   {
      dbOrderInfo.abBagName = ( BYTE * ) hb_parc( 1 );
      dbOrderInfo.atomBagName = ( BYTE * ) hb_parc( 2 );
      dbOrderInfo.abExpr = hb_param( 3, HB_IT_STRING );
      if( ( ( strlen( ( char * ) dbOrderInfo.abBagName ) == 0 ) &&
            ( strlen( ( char * ) dbOrderInfo.atomBagName ) == 0 ) ) ||
          !dbOrderInfo.abExpr )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "ORDCREATE" );
         return;
      }
      dbOrderInfo.itmCobExpr = hb_param( 4, HB_IT_BLOCK );
      if( ISLOG( 5 ) )
         dbOrderInfo.fUnique = hb_parl( 5 );
      else
         dbOrderInfo.fUnique = hb_set.HB_SET_UNIQUE;
      SELF_ORDCREATE( ( AREAP ) s_pCurrArea->pArea, &dbOrderInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDCREATE" );
}

HB_FUNC( ORDDESTROY )
{
   DBORDERINFO pOrderInfo;

   if( s_pCurrArea )
   {
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      if( !pOrderInfo.itmOrder )
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      SELF_ORDDESTROY( ( AREAP ) s_pCurrArea->pArea, &pOrderInfo );
   }
}

HB_FUNC( ORDFOR )
{
   DBORDERINFO pOrderInfo;

   if( s_pCurrArea )
   {
      if ( ISNUM(1) || ISNIL(1) )
      {
         if ( hb_parnl(1) == 0 || ISNIL(1) )          /* if NIL or ask for 0, use current order  */
            pOrderInfo.itmOrder  = NULL;
         else
            pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );

      }else
      {
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
         if( !pOrderInfo.itmOrder )
         {
            hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "ORDFOR" );
            return;
         }
      }

      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      pOrderInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( ( AREAP ) s_pCurrArea->pArea, DBOI_CONDITION, &pOrderInfo );
      hb_retc( hb_itemGetCPtr( pOrderInfo.itmResult ) );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDFOR" );
}

HB_FUNC( ORDKEY )
{
   DBORDERINFO pOrderInfo;

   if( s_pCurrArea )
   {
      if ( ISNUM(1) || ISNIL(1) )
      {
         if ( hb_parnl(1) == 0 || ISNIL(1) )          /* if NIL or ask for 0, use current order  */
            pOrderInfo.itmOrder  = NULL;
         else
            pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );

      }else
      {
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
         if( !pOrderInfo.itmOrder )
         {
            hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "ORDKEY" );
            return;
         }
      }

      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      pOrderInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( ( AREAP ) s_pCurrArea->pArea, DBOI_EXPRESSION, &pOrderInfo );
      hb_retc( hb_itemGetCPtr( pOrderInfo.itmResult ) );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDKEY" );
}

#ifdef HB_COMPAT_C53
HB_FUNC( ORDKEYCOUNT )
{
   DBORDERINFO pOrderInfo;

   if( s_pCurrArea )
   {
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      if( !pOrderInfo.itmOrder )
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      /* Either or both may be NIL */

      pOrderInfo.itmResult = hb_itemPutNL( NULL, 0 );
      SELF_ORDINFO( ( AREAP ) s_pCurrArea->pArea, DBOI_KEYCOUNT, &pOrderInfo );
      hb_retnl( hb_itemGetNL( pOrderInfo.itmResult ) );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDKEYCOUNT" );

}

HB_FUNC( ORDKEYNO )
{
   DBORDERINFO pOrderInfo;

   if( s_pCurrArea )
   {
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      if( !pOrderInfo.itmOrder )
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      /* Either or both may be NIL */

      pOrderInfo.itmResult = hb_itemPutNL( NULL, 0 );
      SELF_ORDINFO( ( AREAP ) s_pCurrArea->pArea, DBOI_POSITION, &pOrderInfo );
      hb_retnl( hb_itemGetNL( pOrderInfo.itmResult ) );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDKEYNO" );

}
#endif

HB_FUNC( ORDLISTADD )
{

   DBORDERINFO pOrderInfo;
   BOOL bFirst;

   if( s_pCurrArea )
   {
      /*  determine if there are existing orders; if not, this becomes the controlling order
      */
      pOrderInfo.atomBagName = NULL;
      pOrderInfo.itmResult = hb_itemPutNI( NULL, 0 );
      SELF_ORDINFO( ( AREAP ) s_pCurrArea->pArea, DBOI_ORDERCOUNT, &pOrderInfo );
      bFirst = ( pOrderInfo.itmResult->type & HB_IT_NUMERIC ) &&
                  hb_itemGetNI( pOrderInfo.itmResult ) == 0;


      pOrderInfo.atomBagName = hb_param( 1, HB_IT_STRING );
      pOrderInfo.itmOrder  = hb_param( 2, HB_IT_STRING );
      if( !pOrderInfo.atomBagName )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "ORDLISTADD" );
         return;
      }
      SELF_ORDLSTADD( ( AREAP ) s_pCurrArea->pArea, &pOrderInfo );

      if ( bFirst )                     /* set as controlling order and go top */
      {
         pOrderInfo.itmOrder  = hb_itemPutNI( NULL, 1 );
         SELF_ORDLSTFOCUS( ( AREAP ) s_pCurrArea->pArea, &pOrderInfo );
         hb_itemRelease( pOrderInfo.itmOrder );
         SELF_GOTOP( ( AREAP ) s_pCurrArea->pArea );
      }
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDLISTADD" );

}

HB_FUNC( ORDLISTCLEAR )
{
   if( s_pCurrArea )
      SELF_ORDLSTCLEAR( ( AREAP ) s_pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDLISTCLEAR" );
}

HB_FUNC( ORDLISTREBUILD )
{
   if( s_pCurrArea )
      SELF_ORDLSTREBUILD( ( AREAP ) s_pCurrArea->pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDLISTREBUILD" );
}

HB_FUNC( ORDNAME )
{
   DBORDERINFO pOrderInfo;

   if( s_pCurrArea )
   {
      if ( ISNUM(1) || ISNIL(1) )
      {
         if ( hb_parnl(1) == 0 || ISNIL(1) )          /* if NIL or ask for 0, use current order  */
            pOrderInfo.itmOrder  = NULL;
         else
            pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );

      }else
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "ORDNAME" );
         return;
      }

      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      pOrderInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( ( AREAP ) s_pCurrArea->pArea, DBOI_NAME, &pOrderInfo );
      hb_retc( hb_itemGetCPtr( pOrderInfo.itmResult ) );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDNAME" );
}

HB_FUNC( ORDNUMBER )
{
   DBORDERINFO pOrderInfo;

   if( s_pCurrArea )
   {
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      if( !pOrderInfo.itmOrder && ! ISNIL(1))
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "ORDNUMBER" );
         return;
      }
      pOrderInfo.itmResult = hb_itemPutNI( NULL, 0 );
      SELF_ORDINFO( ( AREAP ) s_pCurrArea->pArea, DBOI_NUMBER, &pOrderInfo );
      hb_retni( hb_itemGetNI( pOrderInfo.itmResult ) );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDNUMBER" );
}

HB_FUNC( ORDSETFOCUS )
{
   DBORDERINFO pInfo;

   if( s_pCurrArea )
   {
      pInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      if( !pInfo.itmOrder )
         pInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      pInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      pInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDLSTFOCUS( ( AREAP ) s_pCurrArea->pArea, &pInfo );
      hb_retc( hb_itemGetCPtr( pInfo.itmResult ) );
      hb_itemRelease( pInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDSETFOCUS" );
}


HB_FUNC( RDDLIST )
{
   USHORT uiType;
   PHB_ITEM pName;
   LPRDDNODE pRddNode;

   hb_rddCheck();
   hb_arrayNew( &hb_stack.Return, 0 );
   pName = hb_itemNew( NULL );
   pRddNode = s_pRddList;
   uiType = hb_parni( 1 );       /* 0 all types of RDD's */
   while( pRddNode )
   {
      if( ( uiType == 0 ) || ( pRddNode->uiType == uiType ) )
         hb_arrayAdd( &hb_stack.Return, hb_itemPutC( pName, pRddNode->szName ) );
      pRddNode = pRddNode->pNext;
   }
   hb_itemRelease( pName );
}

HB_FUNC( RDDNAME )
{
   char * pBuffer;

   if( s_pCurrArea )
   {
      pBuffer = ( char * ) hb_xgrab( HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 );
      pBuffer[ 0 ] = '\0';
      SELF_SYSNAME( ( AREAP ) s_pCurrArea->pArea, ( BYTE * ) pBuffer );
      hb_retc( pBuffer );
      hb_xfree( pBuffer );
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "RDDNAME" );
      hb_retc( "" );
   }
}

HB_FUNC( RDDREGISTER )
{
   USHORT uiLen;
   char szDriver[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 ];

   hb_rddCheck();

   uiLen = ( USHORT ) hb_parclen( 1 );
   if( uiLen > 0 )
   {
      if( uiLen > HARBOUR_MAX_RDD_DRIVERNAME_LENGTH )
         uiLen = HARBOUR_MAX_RDD_DRIVERNAME_LENGTH;

      hb_strncpyUpper( szDriver, hb_parc( 1 ), uiLen );
      /*
       * hb_rddRegister returns:
       *
       * 0: Ok, RDD registered
       * 1: RDD already registerd
       * > 1: error
       */
      if( hb_rddRegister( szDriver, hb_parni( 2 ) ) > 1 )
         hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );
   }
}

/* Same as LASTREC() */
HB_FUNC( RECCOUNT )
{
   ULONG ulRecCount = 0;

   if( s_pCurrArea )
      SELF_RECCOUNT( ( AREAP ) s_pCurrArea->pArea, &ulRecCount );
   hb_retnl( ulRecCount );
}

HB_FUNC( RECNO )
{
   PHB_ITEM pRecNo;

   pRecNo = hb_itemPutNL( NULL, 0 );
   if( s_pCurrArea )
      SELF_RECNO( ( AREAP ) s_pCurrArea->pArea, pRecNo );
   hb_itemRelease( hb_itemReturn( pRecNo ) );
}

HB_FUNC( RECSIZE )
{
   PHB_ITEM pRecSize;

   if( s_pCurrArea )
   {
      pRecSize = hb_itemNew( NULL );
      SELF_INFO( ( AREAP ) s_pCurrArea->pArea, DBI_GETRECSIZE, pRecSize );
      hb_itemRelease( hb_itemReturn( pRecSize ) );
   }
   else
      hb_retni( 0 );
}

HB_FUNC( RLOCK )
{
   DBLOCKINFO dbLockInfo;

   dbLockInfo.fResult = FALSE;
   if( s_pCurrArea )
   {
      dbLockInfo.uiMethod = DBLM_EXCLUSIVE;
      SELF_LOCK( ( AREAP ) s_pCurrArea->pArea, &dbLockInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "RLOCK" );

   hb_retl( dbLockInfo.fResult );
}

HB_FUNC( SELECT )
{
   char * szAlias;
   ULONG ulLen;

   szAlias = hb_parc( 1 );
   ulLen = strlen( szAlias );

   if( ulLen == 0 && ISCHAR( 1 ))
      hb_retni( 0 );
   else if( ulLen == 1 && toupper( szAlias[ 0 ] ) >= 'A' && toupper( szAlias[ 0 ] ) <= 'K' )
      hb_retni( toupper( szAlias[ 0 ] ) - 'A' + 1 );
   else if( ulLen > 0 )
      hb_retni( hb_rddSelect( szAlias ) );
   else
      hb_retni( s_uiCurrArea );
}

HB_FUNC( USED )
{
   hb_retl( s_pCurrArea != NULL );
}

/* NOTE: Same as dbSetDriver() and rddSetDefault(), but doesn't
         throw any error if the driver doesn't exist, this is
         required in the RDDSYS INIT function, since it's not guaranteed
         that the RDD is already registered at that point. [vszakats] */

HB_FUNC( __RDDSETDEFAULT )
{
   USHORT uiLen;

   hb_rddCheck();
   hb_retc( s_szDefDriver );

   uiLen = ( USHORT ) hb_parclen( 1 );
   if( uiLen > 0 )
   {
      if( uiLen > HARBOUR_MAX_RDD_DRIVERNAME_LENGTH )
         uiLen = HARBOUR_MAX_RDD_DRIVERNAME_LENGTH;

      s_szDefDriver = ( char * ) hb_xrealloc( s_szDefDriver, uiLen + 1 );
      hb_strncpyUpper( s_szDefDriver, hb_parc( 1 ), uiLen );
   }
}

HB_FUNC( RDDSETDEFAULT )
{
   USHORT uiLen;
   char szNewDriver[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH ];

   hb_rddCheck();
   hb_retc( s_szDefDriver );

   uiLen = ( USHORT ) hb_parclen( 1 );
   if( uiLen > 0 )
   {
      if( uiLen > HARBOUR_MAX_RDD_DRIVERNAME_LENGTH )
         uiLen = HARBOUR_MAX_RDD_DRIVERNAME_LENGTH;

      hb_strncpyUpper( szNewDriver, hb_parc( 1 ), uiLen );

      if( !hb_rddFindNode( szNewDriver, NULL ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, "RDDSETDEFAULT" );
         return;
      }

      s_szDefDriver = ( char * ) hb_xrealloc( s_szDefDriver, uiLen + 1 );
      strcpy( s_szDefDriver, szNewDriver );
   }
}

HB_FUNC( DBSETDRIVER )
{
   USHORT uiLen;
   char szNewDriver[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH ];

   hb_rddCheck();
   hb_retc( s_szDefDriver );

   uiLen = ( USHORT ) hb_parclen( 1 );
   if( uiLen > 0 )
   {
      if( uiLen > HARBOUR_MAX_RDD_DRIVERNAME_LENGTH )
         uiLen = HARBOUR_MAX_RDD_DRIVERNAME_LENGTH;

      hb_strncpyUpper( szNewDriver, hb_parc( 1 ), uiLen );

      if( !hb_rddFindNode( szNewDriver, NULL ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, "DBSETDRIVER" );
         return;
      }

      s_szDefDriver = ( char * ) hb_xrealloc( s_szDefDriver, uiLen + 1 );
      strcpy( s_szDefDriver, szNewDriver );
   }
}

HB_FUNC( ORDSCOPE )
{
   PHB_ITEM pScopeValue;
   DBORDSCOPEINFO sInfo;

   if( s_pCurrArea )
   {
      if( !ISNUM( 1 ) || (!ISNIL(2) && !( ISCHAR(2) || ISNUM(2) || ISDATE(2) || ISLOG(2) )))
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "ORDSCOPE" );
         return;
      }
      sInfo.nScope = hb_parni( 1 );
      pScopeValue = hb_itemPutC( NULL, "" );

      SELF_SCOPEINFO( ( AREAP ) s_pCurrArea->pArea, sInfo.nScope, pScopeValue );
      hb_retc( hb_itemGetCPtr( pScopeValue ) );
      hb_itemRelease( pScopeValue );

      if( hb_pcount() > 1 )
      {
         if ( ISNIL( 2 ) )                /* explicitly passed NIL, clear it */
            sInfo.scopeValue = NULL;
         else
            sInfo.scopeValue = (BYTE*) (ISCHAR( 2 ) ? hb_parc( 2 ) :
                                       (ISDATE( 2 ) ? hb_pards( 2 ) : hb_parc( 2 ) ) ) ;
         /* this is a temp fix until we decide if the item should be passed down or "parsed" here */


         SELF_SETSCOPE( ( AREAP ) s_pCurrArea->pArea, (LPDBORDSCOPEINFO) &sInfo );
      }else
         sInfo.scopeValue = NULL;
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDSCOPE" );
}

HB_FUNC( DBRELATION )  /* (<nRelation>) --> cLinkExp */
{
   char cExprBuff[ 256 ];  /*TODO: Correct buffer size initialization ??*/

   cExprBuff[ 0 ] = 0;
   if( s_pCurrArea )
      SELF_RELTEXT( ( AREAP ) s_pCurrArea->pArea, hb_parni(1), &cExprBuff ) ;

   hb_retc(cExprBuff);
}

HB_FUNC( DBRSELECT )  /* (<nRelation>) --> nWorkArea */
{
   USHORT uiWorkArea = 0;
   if( s_pCurrArea )
      SELF_RELAREA( ( AREAP ) s_pCurrArea->pArea, hb_parni(1), &uiWorkArea );

   hb_retni( uiWorkArea );
}

HB_FUNC( DBCLEARRELATION )
{
   if( s_pCurrArea )
      SELF_CLEARREL( ( AREAP ) s_pCurrArea->pArea );
}

HB_FUNC( DBSETRELATION )
{
   char * szAlias;
   DBRELINFO dbRelations;
   LPAREANODE s_pArea, pAreaNode;
   USHORT uiChildArea;

   if( s_pCurrArea )
   {
      szAlias = NULL;
      s_pArea = NULL;

      if( hb_pcount() < 2 || ( !( hb_parinfo( 1 ) & HB_IT_NUMERIC ) &&
          ( hb_parinfo( 1 ) != HB_IT_STRING ) ) ||
          !( ISNIL( 4 ) || ISLOG( 4 ) )  )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "DBSETRELATION" );
         return;
      }

      if( hb_parinfo( 1 ) & HB_IT_NUMERIC )
         uiChildArea = hb_parni( 1 );
      else
      {
         szAlias = hb_parc( 1 );
         if( ( uiChildArea = hb_rddSelect( szAlias ) ) == 0 )
         {
            hb_errRT_BASE( EG_NOALIAS, EDBCMD_NOALIAS, NULL, szAlias, 0 );
            return;
         }
      }

      pAreaNode = s_pWorkAreas;
      while( pAreaNode )
      {
         if( ( ( AREAP ) pAreaNode->pArea )->uiArea == uiChildArea )
         {
            s_pArea = pAreaNode; /* Select a valid WorkArea */
            break;
         }
         pAreaNode = pAreaNode->pNext;
      }
      if( !s_pArea )
      {
         hb_errRT_BASE( EG_NOALIAS, EDBCMD_NOALIAS, NULL, szAlias, 0 );
         return;
      }

      dbRelations.lpaChild = ( AREAP ) s_pArea->pArea;
      dbRelations.itmCobExpr = hb_itemNew( hb_param( 2, HB_IT_BLOCK ) );
      dbRelations.abKey = hb_itemNew( hb_param( 3, HB_IT_STRING ) );
      dbRelations.isScoped = ( hb_pcount() > 3 )? hb_parl( 4 ):0;
      dbRelations.lpdbriNext = NULL;

      SELF_SETREL( ( AREAP ) s_pCurrArea->pArea, &dbRelations );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBSETRELATION" );
}


HB_FUNC( __DBARRANGE )
{
   USHORT uiNewArea, uiCount;
   ULONG ulSize;
   char * szFieldLine, * szFieldName, * szPos;
   PHB_ITEM pStruct, pFields;
   DBSORTINFO dbSortInfo;
   LPAREANODE pAreaNode;

   if( s_pCurrArea )
   {
      memset( &dbSortInfo, 0, sizeof( DBSORTINFO ) );
      dbSortInfo.dbtri.uiFlags = DBTF_PUTREC;
      uiNewArea = hb_parni( 1 );

      /* Fields structure of source WorkArea */
      pStruct = hb_param( 2 , HB_IT_ARRAY );
      if( pStruct )
      {
         dbSortInfo.dbtri.uiItemCount = ( USHORT ) hb_arrayLen( pStruct );
         if( dbSortInfo.dbtri.uiItemCount > 0 )
         {
            pFields = hb_itemNew( NULL );
            dbSortInfo.dbtri.lpTransItems = ( LPDBTRANSITEM )
                                            hb_xgrab( dbSortInfo.dbtri.uiItemCount *
                                                      sizeof( DBTRANSITEM ) );
            for( uiCount = 0; uiCount < dbSortInfo.dbtri.uiItemCount; uiCount++ )
            {
               if( hb_arrayGet( pStruct, uiCount + 1, pFields ) && HB_IS_ARRAY( pFields ) &&
                   ( USHORT ) hb_arrayLen( pFields ) > 0 )
               {
                  dbSortInfo.dbtri.lpTransItems[ uiCount ].uiSource =
                                 hb_rddFieldIndex( ( AREAP ) s_pCurrArea->pArea,
                                                    hb_arrayGetCPtr( pFields, 1 ) );
                  dbSortInfo.dbtri.lpTransItems[ uiCount ].uiDest =
                                 dbSortInfo.dbtri.lpTransItems[ uiCount ].uiSource;
               }
               else
               {
                  hb_xfree( dbSortInfo.dbtri.lpTransItems );
                  dbSortInfo.dbtri.uiItemCount = 0;
                  break;
               }
            }
            hb_itemRelease( pFields );
         }
      }
      else
         return;

      /* Invalid fields structure? */
      if( dbSortInfo.dbtri.uiItemCount == 0 )
         return;

      dbSortInfo.dbtri.dbsci.itmCobFor = hb_param( 3, HB_IT_BLOCK );
      dbSortInfo.dbtri.dbsci.lpstrFor = NULL;
      dbSortInfo.dbtri.dbsci.itmCobWhile = hb_param( 4, HB_IT_BLOCK );
      dbSortInfo.dbtri.dbsci.lpstrWhile = NULL;
      dbSortInfo.dbtri.dbsci.lNext = hb_param( 5, HB_IT_NUMERIC );
      dbSortInfo.dbtri.dbsci.itmRecID = hb_param( 6, HB_IT_NUMERIC );
      dbSortInfo.dbtri.dbsci.fRest = hb_param( 7, HB_IT_LOGICAL );
      dbSortInfo.dbtri.dbsci.fIgnoreFilter = dbSortInfo.dbtri.dbsci.fLast =
      dbSortInfo.dbtri.dbsci.fIgnoreDuplicates = FALSE;
      dbSortInfo.dbtri.dbsci.fIncludeDeleted = TRUE;

      pFields = hb_param( 8, HB_IT_ARRAY );
      if( pFields )
         dbSortInfo.uiItemCount = ( USHORT ) hb_arrayLen( pFields );
      else
         dbSortInfo.uiItemCount = 0;
      if( dbSortInfo.uiItemCount > 0 )
      {
         dbSortInfo.lpdbsItem = ( LPDBSORTITEM ) hb_xgrab( dbSortInfo.uiItemCount * sizeof( DBSORTITEM ) );
         ulSize = 0;
         for( uiCount = 1; uiCount <= dbSortInfo.uiItemCount; uiCount++ )
         {
            if( hb_arrayGetCLen( pFields, uiCount ) > ulSize )
               ulSize = hb_arrayGetCLen( pFields, uiCount );
         }
         szFieldLine = ( char * ) hb_xgrab( ulSize + 1 );
         for( uiCount = 0; uiCount < dbSortInfo.uiItemCount; uiCount++ )
         {
            dbSortInfo.lpdbsItem[ uiCount ].uiFlags = 0;
            hb_strncpyUpper( szFieldLine, hb_arrayGetCPtr( pFields, uiCount + 1 ),
                             hb_arrayGetCLen( pFields, uiCount + 1 ) );
            szPos = strchr( szFieldLine, '/' );
            if( szPos )
            {
               if( * ( szPos + 1 ) == 'D' )
                  dbSortInfo.lpdbsItem[ uiCount ].uiFlags |= SF_DESCEND;
               else if( * ( szPos + 1 ) == 'C' )
               {
                  dbSortInfo.lpdbsItem[ uiCount ].uiFlags |= SF_CASE;
                  dbSortInfo.lpdbsItem[ uiCount ].uiFlags |= SF_ASCEND;
               }
               else
                  dbSortInfo.lpdbsItem[ uiCount ].uiFlags |= SF_ASCEND;
               if( * ( szPos + 1 ) != 0 && ( ( * ( szPos + 2 ) == 'C' ) ||
                   ( * ( szPos + 2 ) != 0 && * ( szPos + 3 ) == 'C' ) ) )
                  dbSortInfo.lpdbsItem[ uiCount ].uiFlags |= SF_CASE;
               * szPos = 0;
            }
            else
               dbSortInfo.lpdbsItem[ uiCount ].uiFlags |= SF_ASCEND;
            szFieldName = szFieldLine;
            while( szFieldName[ 0 ] == ' ' )
               szFieldName++;
            ulSize = strlen( szFieldName );
            while( ulSize > 1 && szFieldName[ ulSize - 1 ] == ' ' )
            {
               ulSize --;
               szFieldName[ ulSize ] = 0;
            }
            dbSortInfo.lpdbsItem[ uiCount ].uiField =
                        hb_rddFieldIndex( ( AREAP ) s_pCurrArea->pArea, szFieldName );

            /* Field not found */
            if( dbSortInfo.lpdbsItem[ uiCount ].uiField == 0 )
            {
               hb_xfree( dbSortInfo.lpdbsItem );
               dbSortInfo.lpdbsItem = NULL;
               break;
            }
         }
         hb_xfree( szFieldLine );
      }
      else
         return;

      /* Fields not found? */
      if( dbSortInfo.lpdbsItem == NULL )
         return;

      dbSortInfo.dbtri.lpaSource = ( AREAP ) s_pCurrArea->pArea;
      dbSortInfo.dbtri.lpaDest = NULL;
      pAreaNode = s_pWorkAreas;
      while( pAreaNode )
      {
         if( ( ( AREAP ) pAreaNode->pArea )->uiArea == uiNewArea )
         {
            dbSortInfo.dbtri.lpaDest = ( AREAP ) pAreaNode->pArea;
            break;
         }
         pAreaNode = pAreaNode->pNext;
      }

      SELF_SORT( ( AREAP ) s_pCurrArea->pArea, &dbSortInfo );

      /* Free items */
      if( dbSortInfo.lpdbsItem )
         hb_xfree( dbSortInfo.lpdbsItem );
      if( dbSortInfo.dbtri.uiItemCount > 0 )
         hb_xfree( dbSortInfo.dbtri.lpTransItems );
   }
}

#ifdef HB_COMPAT_C53
HB_FUNC( DBINFO )
{
   PHB_ITEM pType, pInfo;
   BOOL bDeleteItem;

   if( s_pCurrArea )
   {
      pType = hb_param( 1 , HB_IT_NUMERIC );
      if( pType )
      {
         pInfo = hb_param( 2 , HB_IT_ANY );
         if( !pInfo )
         {
            pInfo = hb_itemNew( NULL );
            bDeleteItem = TRUE;
         }
         else
            bDeleteItem = FALSE;
         SELF_INFO( ( AREAP ) s_pCurrArea->pArea, hb_itemGetNI( pType ), pInfo );
         hb_itemReturn( pInfo );
         if( bDeleteItem )
            hb_itemRelease( pInfo );
         return;
      }
      hb_errRT_DBCMD( EG_ARG, EDBCMD_DBINFOBADPARAMETER, NULL, "DBINFO" );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBINFO" );
}

HB_FUNC( DBORDERINFO )
{
   PHB_ITEM pType, pInfo;
   BOOL bDeleteItem;
   DBORDERINFO pOrderInfo;

   if( s_pCurrArea )
   {
      pType = hb_param( 1 , HB_IT_NUMERIC );
      if( pType )
      {

         pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
         /* atomBagName may be NIL */
         pOrderInfo.itmOrder = hb_param( 3, HB_IT_STRING );
         if( !pOrderInfo.itmOrder )
            pOrderInfo.itmOrder = hb_param( 3, HB_IT_NUMERIC );

         /*  TODO: 4TH parameter is not supported in current ads code or the structure */
         pInfo = hb_param( 4 , HB_IT_ANY );  /* Set new value */
         if( !pInfo )
         {
            pInfo = hb_itemNew( NULL );
            bDeleteItem = TRUE;
         }
         else
            bDeleteItem = FALSE;

         pOrderInfo.itmResult = hb_itemNew( NULL );
         SELF_ORDINFO( ( AREAP ) s_pCurrArea->pArea, hb_itemGetNI( pType ), &pOrderInfo );
         hb_itemReturn(  pOrderInfo.itmResult );
         hb_itemRelease( pOrderInfo.itmResult );

         if( bDeleteItem )
            hb_itemRelease( pInfo );
         return;
      }
      hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, "DBORDERINFO" );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBORDERINFO" );
}

HB_FUNC( DBFIELDINFO )
{
   PHB_ITEM pType, pInfo;
   USHORT uiFields, uiIndex;
   BOOL bDeleteItem;

   if( s_pCurrArea )
   {
      pType = hb_param( 1 , HB_IT_NUMERIC );
      uiIndex = hb_parni( 2 );
      if( pType &&
          SELF_FIELDCOUNT( ( AREAP ) s_pCurrArea->pArea, &uiFields ) == SUCCESS &&
          uiIndex > 0 && uiIndex <= uiFields )
      {
         pInfo = hb_param( 3 , HB_IT_ANY );
         if( !pInfo )
         {
            pInfo = hb_itemNew( NULL );
            bDeleteItem = TRUE;
         }
         else
            bDeleteItem = FALSE;
         SELF_FIELDINFO( ( AREAP ) s_pCurrArea->pArea, uiIndex, hb_itemGetNI( pType ), pInfo );
         hb_itemReturn( pInfo );
         if( bDeleteItem )
            hb_itemRelease( pInfo );
         return;
      }
      hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, "DBFIELDINFO" );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBFIELDINFO" );
}

HB_FUNC( DBRECORDINFO )
{
   PHB_ITEM pType, pRecNo, pInfo;
   BOOL bDeleteItem;

   if( s_pCurrArea )
   {
      pType = hb_param( 1 , HB_IT_NUMERIC );
      pRecNo = hb_param( 2 , HB_IT_NUMERIC );
      if( pType )
      {
         pInfo = hb_param( 3 , HB_IT_ANY );
         if( !pInfo )
         {
            pInfo = hb_itemNew( NULL );
            bDeleteItem = TRUE;
         }
         else
            bDeleteItem = FALSE;
         SELF_RECINFO( ( AREAP ) s_pCurrArea->pArea, pRecNo, hb_itemGetNI( pType ), pInfo );
         hb_itemReturn( pInfo );
         if( bDeleteItem )
            hb_itemRelease( pInfo );
         return;
      }
      hb_errRT_DBCMD( EG_ARG, EDBCMD_INFOBADPARAMETER, NULL, "DBRECORDINFO" );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBRECORDINFO" );
}

HB_FUNC( DBFILEGET )
{
   PHB_ITEM pFileName, pMode;
   char szFileName[ _POSIX_PATH_MAX + 1 ];
   USHORT uiFields, uiIndex;

   if( s_pCurrArea )
   {
      uiIndex = hb_parni( 1 );
      pFileName = hb_param( 2 , HB_IT_STRING );
      pMode = hb_param( 3 , HB_IT_NUMERIC );
      if( pFileName && pMode &&
          SELF_FIELDCOUNT( ( AREAP ) s_pCurrArea->pArea, &uiFields ) == SUCCESS &&
          uiIndex > 0 && uiIndex <= uiFields )
      {
         strncpy( szFileName, hb_itemGetCPtr( pFileName ), _POSIX_PATH_MAX );
         hb_retl( SELF_GETVALUEFILE( ( AREAP ) s_pCurrArea->pArea, uiIndex, szFileName,
                                     hb_itemGetNI( pMode ) ) );
         return;
      }
      hb_errRT_DBCMD( EG_ARG, EDBCMD_DBFILEGETBADPARAMETER, NULL, "DBFILEGET" );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBFILEGET" );

   hb_retl( FALSE );
}

HB_FUNC( DBFILEPUT )
{
   PHB_ITEM pFileName;
   char szFileName[ _POSIX_PATH_MAX + 1 ];
   USHORT uiFields, uiIndex;

   if( s_pCurrArea )
   {
      uiIndex = hb_parni( 1 );
      pFileName = hb_param( 2 , HB_IT_STRING );
      if( pFileName &&
          SELF_FIELDCOUNT( ( AREAP ) s_pCurrArea->pArea, &uiFields ) == SUCCESS &&
          uiIndex > 0 && uiIndex <= uiFields )
      {
         strncpy( szFileName, hb_itemGetCPtr( pFileName ), _POSIX_PATH_MAX );
         hb_retl( SELF_PUTVALUEFILE( ( AREAP ) s_pCurrArea->pArea, uiIndex, szFileName ) );
         return;
      }
      hb_errRT_DBCMD( EG_ARG, EDBCMD_DBFILEPUTBADPARAMETER, NULL, "DBFILEPUT" );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBFILEPUT" );

   hb_retl( FALSE );
}
#endif

/*******************************************/
/* here we have the NEW Database level functions DBDROP & DBEXISTS */
HB_FUNC( DBDROP )
{
  LPRDDNODE  pRDDNode;
  USHORT     uiRddID;
  char      *szDriver;

  if ( ISCHAR( 2 ) ) /* we have a VIA RDD parameter */
    szDriver = hb_parc( 2 );
  else
    szDriver = s_szDefDriver;
  pRDDNode = hb_rddFindNode( szDriver, &uiRddID );  // find the RDD

  if ( !pRDDNode )
  {
    hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBDROP" );
    return;
  }
  if ( SELF_DROP( pRDDNode, hb_param( 1, HB_IT_STRING )) == 0 )
   hb_retl( TRUE );
  else
   hb_retl( FALSE );
}

HB_FUNC( DBEXISTS )
{
  LPRDDNODE  pRDDNode;
  USHORT     uiRddID;
  char * szDriver;

  if ( ISCHAR( 3 ) ) /* we have a VIA RDD parameter */
    szDriver = hb_parc( 3 );
  else
    szDriver = s_szDefDriver;
  pRDDNode = hb_rddFindNode( szDriver, &uiRddID );  // find the RDD

  if ( !pRDDNode )
  {
    hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBDROP" );
    return;
  }

  if ( SELF_EXISTS( pRDDNode,
                    ISCHAR( 1 ) ? hb_param( 1, HB_IT_STRING ) : NULL,
                    ISCHAR( 2 ) ? hb_param( 2, HB_IT_STRING ) : NULL ))
    hb_retl( TRUE );
  else
    hb_retl( FALSE );
}

/*******************************************/
/* as we are in C, the code is upside down,
   find __SBAPP & __DBCOPY at the bottom
*/

// check if the field is on the Fields Array
static BOOL IsFieldIn( char * fieldName, PHB_ITEM pFields )
{
  USHORT i, uiFields = ( USHORT ) hb_arrayLen( pFields );
  for ( i=0; i<uiFields; i++ )
  {
    PHB_ITEM pField = pFields->item.asArray.value->pItems + i;
    if ( strcmp( fieldName, (char *)pField->item.asString.value ) == 0 )
      return TRUE;
  }
  return FALSE;
}

/*   create a new AREANODE and open its Area
   If the file exists it will be deleted & a new one created
*/
static LPAREANODE GetTheOtherArea( char *szDriver, char * szFileName, BOOL createIt, PHB_ITEM pFields )
{
  LPAREANODE pAreaNode;
  LPRDDNODE  pRDDNode;
  PHB_ITEM   tableItem, pFileExt;
  USHORT     uiRddID;
  PHB_FNAME  pFileName;
  DBOPENINFO pInfo;

  pRDDNode = hb_rddFindNode( szDriver, &uiRddID );  // find the RDD

  if ( !pRDDNode )
  {
    hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBAPP" );
    return NULL;
  }

/* Fill pInfo structure */
  memset( &pInfo, 0, sizeof(DBOPENINFO) );
  pInfo.uiArea = uiRddID;
  pInfo.abName = ( BYTE * )  hb_xgrab( _POSIX_PATH_MAX + 1 );
  strcpy( ( char * ) pInfo.abName, szFileName );
  pInfo.atomAlias = ( BYTE * ) "__TMPAREA";
  pInfo.fShared = !hb_set.HB_SET_EXCLUSIVE;
  pInfo.fReadonly = FALSE;

/* get the new area node */
    pAreaNode =  hb_rddNewAreaNode( pRDDNode, uiRddID );

/* check the extension */
    pFileName = hb_fsFNameSplit( szFileName );
    if( !pFileName->szExtension )
    {
       pFileExt = hb_itemPutC( NULL, "" );
       SELF_INFO( ( AREAP ) pAreaNode->pArea, DBI_TABLEEXT, pFileExt );
       strncat( (char *) pInfo.abName, hb_itemGetCPtr( pFileExt ), _POSIX_PATH_MAX -
                strlen( (char *) pInfo.abName ) );
       hb_itemRelease( pFileExt );
    }
    hb_xfree( pFileName );

  if ( createIt )
  {
    PHB_ITEM pFieldArray, pItem, pData;
    USHORT uiFields, uiCount;

/* get the table structure */
    pFieldArray = hb_itemNew( NULL );
    SELF_FIELDCOUNT( ( AREAP ) s_pCurrArea->pArea, &uiFields );
    hb_arrayNew( pFieldArray, 0 );
    pData = hb_itemNew( NULL );
    pItem = hb_itemNew( NULL );
    for( uiCount = 1; uiCount <= uiFields; uiCount++ )
    {
      if ( !pFields || IsFieldIn( (( PHB_DYNS )((( AREAP )s_pCurrArea->pArea)->lpFields + (uiCount-1))->sym )->pSymbol->szName,  pFields ))
      {
        hb_arrayNew( pItem, 4 );
        SELF_FIELDINFO( ( AREAP ) s_pCurrArea->pArea, uiCount, DBS_NAME, pData );
        hb_arraySet( pItem, 1, pData );
        SELF_FIELDINFO( ( AREAP ) s_pCurrArea->pArea, uiCount, DBS_TYPE, pData );
        hb_arraySet( pItem, 2, pData );
        SELF_FIELDINFO( ( AREAP ) s_pCurrArea->pArea, uiCount, DBS_LEN, pData );
        hb_arraySet( pItem, 3, pData );
        SELF_FIELDINFO( ( AREAP ) s_pCurrArea->pArea, uiCount, DBS_DEC, pData );
        hb_arraySet( pItem, 4, pData );
        hb_arrayAdd( pFieldArray, pItem );
      }
    }
    hb_itemRelease( pItem );
    hb_itemRelease( pData );

/* check for table existence and if true, drop it */
    tableItem = hb_itemNew( NULL );
    hb_itemPutCL( tableItem, szFileName, strlen(szFileName) );
    if( SELF_EXISTS( pRDDNode, tableItem, NULL ))
      SELF_DROP( pRDDNode, tableItem );
    hb_itemRelease( tableItem );

/* now create a new table based on the current Area's record layout */
    ( ( AREAP ) pAreaNode->pArea )->atomAlias = hb_dynsymGet( ( char * ) pInfo.atomAlias );
    if( SELF_CREATEFIELDS( ( AREAP ) pAreaNode->pArea, pFieldArray ) == FAILURE )
    {
      SELF_RELEASE( ( AREAP ) pAreaNode->pArea );
      hb_xfree( pInfo.abName );
      hb_xfree( pAreaNode );
      hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, "DBAPP" );
      return NULL;
    }

    if( SELF_CREATE( ( AREAP ) pAreaNode->pArea, &pInfo ) == FAILURE )
    {
      SELF_RELEASE( ( AREAP ) pAreaNode->pArea );
      hb_xfree( pInfo.abName );
      hb_xfree( pAreaNode );
      hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, "DBAPP" );
      return NULL;
    }
    hb_itemRelease( pFieldArray );
    SELF_CLOSE( ( AREAP ) pAreaNode->pArea );
    SELF_RELEASE( ( AREAP ) pAreaNode->pArea );
    hb_xfree( pAreaNode );

/* get a new area node for this AREA */
    pAreaNode =  hb_rddNewAreaNode( pRDDNode, uiRddID );
  }

/* open it */
  if( SELF_OPEN( ( AREAP ) pAreaNode->pArea, &pInfo ) == FAILURE )
  {
    SELF_RELEASE( ( AREAP ) pAreaNode->pArea );
    hb_xfree( pInfo.abName );
    hb_xfree( pAreaNode );
    hb_errRT_DBCMD( EG_OPEN, 0, NULL, "DBAPP" ); // Could not open it
    return NULL;
  }
  hb_xfree( pInfo.abName );
  return pAreaNode;
}

/* move the Field Data between areas by name */
static void rddMoveFields( AREAP pAreaFrom, AREAP pAreaTo, PHB_ITEM pFields, BOOL bNameMatch )
{
  USHORT   i, f=1;
  PHB_ITEM fieldValue;

  fieldValue = hb_itemNew( NULL );
  for ( i=0 ; i<pAreaFrom->uiFieldCount; i++ )
  {
    /* list or field in the list?*/
    if ( !pFields || IsFieldIn( (( PHB_DYNS )(pAreaFrom->lpFields + i)->sym )->pSymbol->szName,  pFields ))
    {
      if ( bNameMatch )
        f = hb_rddFieldIndex( pAreaTo, (( PHB_DYNS )(pAreaFrom->lpFields + i)->sym )->pSymbol->szName );
      if ( f )
      {
        SELF_GETVALUE( pAreaFrom, i+1, fieldValue );
        SELF_PUTVALUE( pAreaTo, f++, fieldValue );
      }
    }
  }
  hb_itemRelease( fieldValue );
}

/*move the records, filtering if apropiate*/
static ERRCODE rddMoveRecords( char *cAreaFrom, char *cAreaTo, PHB_ITEM pFields,
                               PHB_ITEM pFor, PHB_ITEM pWhile, LONG lNext,
                               ULONG lRec, BOOL bRest, char *cDriver )
{
  char     * szDriver;
  LONG       toGo=lNext;
  BOOL       bFor, bWhile;
  BOOL       keepGoing=TRUE;
  BOOL       bNameMatch=FALSE;
  AREAP      pAreaFrom;
  AREAP      pAreaTo;
  LPAREANODE pAreaRelease=NULL;
  LPAREANODE s_pCurrAreaSaved=s_pCurrArea;

  HB_TRACE(HB_TR_DEBUG, ("rddMoveRecords(%s, %s, %p, %p, %p, %d, %lu, %d, %s )",
         cAreaFrom, cAreaTo, pFields, pFor, pWhile, lNext, lRec, bRest, cDriver));

  if ( !s_pCurrArea )   /*We need a current Area to APPEND TO or FROM*/
  {
     hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBAPP" );
     return EG_NOTABLE;
  }

  /*get the RDD Driver to use for the "other" Area*/
  if( cDriver )
     szDriver = cDriver;
  else
     szDriver = s_szDefDriver;

  if( !cAreaFrom && ! cAreaTo )          /*File is needed*/
  {
     hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBAPP" );
     return EG_ARG;
  }

  if ( pFields && hb_arrayLen( pFields ) == 0 )  /*no field clause?*/
    pFields = NULL;

  if ( cAreaTo )  /*it's a COPY TO*/
  {
    pAreaRelease = GetTheOtherArea( szDriver, cAreaTo, TRUE, pFields );
    pAreaTo = (AREAP) pAreaRelease->pArea;
  }
  else
    pAreaTo = (AREAP) s_pCurrArea->pArea;


  if ( cAreaFrom )     /*it's an APPEND FROM*/
  {                    /*make it current*/
    bNameMatch = TRUE; /*we pass fields by name */
    pAreaRelease = s_pCurrArea = GetTheOtherArea( szDriver, cAreaFrom, FALSE, NULL );
    pAreaFrom =  (AREAP) pAreaRelease->pArea;
  }
  else
    pAreaFrom = (AREAP) s_pCurrArea->pArea;

  /* one or the other but never none*/
  if ( !pAreaRelease )   /*We need another Area to APPEND TO*/
  {
     hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBAPP" );
     return EG_NOTABLE;
  }

  if ( lRec > 0 )                      /* only one record */
    SELF_GOTO( pAreaFrom, lRec );      /* go there */
  else
  {
    if( !pWhile && !bRest && !lNext )  /* these two stay current */
       SELF_GOTOP( pAreaFrom );        /* else start from the top */
  }

  /*move those records assuming we are positioned on one.*/
  while( keepGoing )
  {
    keepGoing = FALSE;
    if( !pAreaFrom->fEof )  /*until eof or an evaluation failed*/
    {
       if( pWhile )
          bWhile = hb_itemGetL( hb_vmEvalBlock( pWhile ) );
       else
          bWhile = TRUE;

       if( pFor )
          bFor = hb_itemGetL( hb_vmEvalBlock( pFor ) );
       else
          bFor = TRUE;

      if( bWhile && (!lNext || toGo > 0 ))                /*candidate?*/
      {
         if ( bFor )
         {
            SELF_APPEND( ( AREAP ) pAreaTo, FALSE );      /*put a new one on TO Area*/
            rddMoveFields( pAreaFrom, pAreaTo, pFields, bNameMatch ); /*move the data*/
         }
         if ( lRec == 0 || pFor )  /*not only one record? Or there's a For clause?*/
            keepGoing = TRUE;
         else
            continue;
      }

      toGo--;                      /*one less to go*/
      SELF_SKIP( pAreaFrom, 1L );  /*get the next one*/
    }
  }

  s_pCurrArea = s_pCurrAreaSaved;  /*set current WorkArea to initial state*/

  /*Close the File*/
  SELF_CLOSE( ( AREAP ) pAreaRelease->pArea );
  SELF_RELEASE( ( AREAP ) pAreaRelease->pArea );
  hb_xfree( pAreaRelease );

  return SUCCESS;
}

HB_FUNC( __DBAPP )
{
  if( ISCHAR( 1 ) )
  {
    rddMoveRecords(  hb_parc( 1 ),                /* File From */
                     NULL,                        /* TO current area */
                     hb_param( 2, HB_IT_ARRAY ),  /* Fields */
                     hb_param( 3, HB_IT_BLOCK ),  /* For */
                     hb_param( 4, HB_IT_BLOCK ),  /* While */
                     hb_parnl( 5 ),               /* Next */ /* Defaults to zero on bad type */
                     hb_parnl( 6 ),               /* Record */ /* Defaults to zero on bad type */
                     hb_parl( 7 ),                /* Rest */ /* Defaults to zero on bad type */
                     ISCHAR( 8 ) ? hb_parc( 8 ) : NULL ); /* RDD */
  }
}

HB_FUNC( __DBCOPY )
{
  if( ISCHAR( 1 ) )
  {
    rddMoveRecords(  NULL,                        /* fro CURRENT Area */
                     hb_parc( 1 ),                /* To File */
                     hb_param( 2, HB_IT_ARRAY ),  /* Fields */
                     hb_param( 3, HB_IT_BLOCK ),  /* For */
                     hb_param( 4, HB_IT_BLOCK ),  /* While */
                     hb_parnl( 5 ),               /* Next */ /* Defaults to zero on bad type */
                     hb_parnl( 6 ),               /* Record */ /* Defaults to zero on bad type */
                     hb_parl( 7 ),                /* Rest */ /* Defaults to zero on bad type */
                     ISCHAR( 8 ) ? hb_parc( 8 ) : NULL ); /* RDD */
  }
}

