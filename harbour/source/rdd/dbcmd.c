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
/*
 * The following functions are added by
 *       Horacio Roldan <harbour_ar@yahoo.com.ar>
 *
 * ordKeyVal()
 * ordKeyAdd()
 * ordKeyDel()
 * hb_rddIterateWorkAreas()
 * hb_rddGetTempAlias
 * __RDDGETTEMPALIAS
 *
 */

/* JC1: optimizing stack access under MT */
#define HB_THREAD_OPTIMIZE_STACK

#include <ctype.h>
#include "hbapi.h"
#include "hbstack.h"
#include "hbvm.h"
#include "hbapifs.h"
#include "hbset.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbapiitm.h"
#include "hbrddwrk.h"
#if defined(__XHARBOUR__)
#include "hbfast.h"
#else
#  define HB_THREAD_STUB
#endif
#ifndef HB_CDP_SUPPORT_OFF
#  include "hbapicdp.h"
#endif

HB_FUNC_EXTERN( RDDSYS );

static char s_szDefDriver[HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1] = ""; /* Default RDD name */
static LPRDDNODE * s_RddList = NULL;   /* Registered RDDs */
static USHORT s_uiRddMax = 0;          /* Number of registered RDD */

static AREAP * s_WaList = NULL;        /* Allocated WorkAreas */
static USHORT s_uiWaMax = 0;           /* Number of allocated WA */
static USHORT s_uiWaSpace = 0;         /* Number of allocated WA */

static USHORT * s_WaNums = NULL;       /* Allocated WorkAreas */
static USHORT s_uiWaNumMax = 0;        /* Number of allocated WA */

static BOOL s_bNetError = FALSE;       /* Error on Networked environments */
#ifndef HB_THREAD_SUPPORT
   static USHORT s_uiCurrArea = 1;     /* Selected area */
   static AREAP  s_pCurrArea = NULL;   /* Selected area */
   #define LOCK_AREA
   #define UNLOCK_AREA
   #define LOCK_AREA_INIT
   #define LOCK_AREA_DESTROY
#else
   #define s_uiCurrArea    HB_VM_STACK.uiCurrArea
   #define s_pCurrArea     HB_VM_STACK.pCurrArea
   HB_CRITICAL_T  s_mtxWorkArea;
   #if defined (HB_OS_WIN_32) || defined(HB_OS_OS2)
      static BOOL s_fMtLockInit = FALSE;
      #define LOCK_AREA          if ( s_fMtLockInit ) HB_CRITICAL_LOCK( s_mtxWorkArea );
      #define UNLOCK_AREA        if ( s_fMtLockInit ) HB_CRITICAL_UNLOCK( s_mtxWorkArea );
      #define LOCK_AREA_INIT     if ( !s_fMtLockInit ) { HB_CRITICAL_INIT( s_mtxWorkArea ); s_fMtLockInit = TRUE; }
      #define LOCK_AREA_DESTROY  if ( s_fMtLockInit ) { HB_CRITICAL_DESTROY( s_mtxWorkArea ); s_fMtLockInit = FALSE; }
   #else
      #define LOCK_AREA HB_CRITICAL_LOCK( s_mtxWorkArea );
      #define UNLOCK_AREA HB_CRITICAL_UNLOCK( s_mtxWorkArea );
      #define LOCK_AREA_INIT
      #define LOCK_AREA_DESTROY
   #endif
#endif

#define HB_SET_WA( n )  do \
            { \
               s_uiCurrArea = n; \
               s_pCurrArea = ( ( s_uiCurrArea < s_uiWaNumMax ) ? \
                                 s_WaList[ s_WaNums[ s_uiCurrArea ] ] : \
                                 NULL ); \
            } while ( 0 );

#define HB_GET_WA( n )  ( ( (n) < s_uiWaNumMax ) ? s_WaList[ s_WaNums[ ( n ) ] ] : NULL )
//#define HB_CURRENT_WA   HB_GET_WA( s_uiCurrArea )
#define HB_CURRENT_WA   s_pCurrArea

/*
 * -- DEFAULT METHODS --
 */

#if 0
/*
 * Empty method.
 */
static ERRCODE hb_waNull( AREAP pArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_waNull(%p)", pArea));
   HB_SYMBOL_UNUSED( pArea );

   return SUCCESS;
}
#endif

/*
 * Raise a runtime error if an method is not defined.
 */
static ERRCODE hb_waUnsupported( AREAP pArea )
{
   PHB_ITEM pError;

   HB_TRACE(HB_TR_DEBUG, ("hb_waUnsupported(%p)", pArea));

   if( !pArea )
   {
      HB_FUNCNAME( RDDSYS )();
   }

   pError = hb_errNew();
   hb_errPutGenCode( pError, EG_UNSUPPORTED );
   hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_UNSUPPORTED ) );
   SELF_ERROR( pArea, pError );
   hb_itemRelease( pError );

   return FAILURE;
}

/*
 * Raise a runtime error if an method is not defined.
 */
static ERRCODE hb_waRddUnsupported( LPRDDNODE pRDD )
{
   PHB_ITEM pError;

   HB_TRACE(HB_TR_DEBUG, ("hb_waRDDUnsupported(%p)", pRDD));

   pError = hb_errNew();
   hb_errPutGenCode( pError, EG_UNSUPPORTED );
   hb_errPutDescription( pError, hb_langDGetErrorDesc( EG_UNSUPPORTED ) );

   hb_errPutSeverity( pError, ES_ERROR );
   hb_errPutSubSystem( pError, pRDD->szName );
   hb_errLaunch( pError );
   hb_itemRelease( pError );

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
                            hb_waRecId,
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
                            hb_waLocate,
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
                            hb_rddInit,
                            hb_rddExit,
                            hb_rddDrop,
                            hb_rddExists,
                            hb_rddInfo,
                            hb_waWhoCares
                           };

/*
 * Get RDD node poionter
 */
HB_EXPORT LPRDDNODE hb_rddGetNode( USHORT uiNode )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_rddGetNode(%hu)", uiNode));

   return uiNode < s_uiRddMax ? s_RddList[ uiNode ] : NULL;
}

/*
 * Find a RDD node.
 */
static LPRDDNODE hb_rddFindNode( char * szDriver, USHORT * uiIndex )
{
   USHORT uiCount;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddFindNode(%s, %p)", szDriver, uiIndex));

   for ( uiCount = 0; uiCount < s_uiRddMax; uiCount++ )
   {
      if( strcmp( s_RddList[ uiCount ]->szName, szDriver ) == 0 ) /* Matched RDD */
      {
         if( uiIndex )
            * uiIndex = uiCount;
         return s_RddList[ uiCount ];
      }
   }
   if( uiIndex )
      * uiIndex = 0;
   return NULL;
}

/*
 * Get (/set) default RDD driver
 */
static char * hb_rddDefaultDrv( char * szDriver )
{
   static BOOL fInit = FALSE;

   if( szDriver && *szDriver )
   {
      char szNewDriver[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 ];

      hb_strncpyUpper( szNewDriver, szDriver, HARBOUR_MAX_RDD_DRIVERNAME_LENGTH );
      if( !hb_rddFindNode( szNewDriver, NULL ) )
      {
         return NULL;
      }
      strcpy( s_szDefDriver, szNewDriver );
   }
   else if( !fInit && !s_szDefDriver[ 0 ] && s_uiRddMax )
   {
      char *szDrvTable[] = { "DBFNTX", "DBFCDX", "DBFFPT", "DBF", NULL };
      int i;

      for( i = 0; szDrvTable[ i ]; ++i )
      {
         if( hb_rddFindNode( szDrvTable[ i ], NULL ) )
         {
            strcpy( s_szDefDriver, szDrvTable[ i ] );
            break;
         }
      }
      fInit = TRUE;
   }

   return s_szDefDriver;
}

/*
 * Register a RDD driver.
 */
HB_EXPORT int hb_rddRegister( char * szDriver, USHORT uiType )
{
   LPRDDNODE pRddNewNode;
   PHB_DYNS pGetFuncTable;
   char * szGetFuncTable;
   USHORT uiFunctions;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddRegister(%s, %hu)", szDriver, uiType));

   if( hb_rddFindNode( szDriver, NULL ) )    /* Duplicated RDD */
   {
      return 1;
   }

   szGetFuncTable = ( char * ) hb_xgrab( strlen( szDriver ) + 14 );
   strcpy( szGetFuncTable, szDriver );
   strcat( szGetFuncTable, "_GETFUNCTABLE" );
   pGetFuncTable = hb_dynsymFindName( szGetFuncTable );
   hb_xfree( szGetFuncTable );
   if( !pGetFuncTable )
   {
      return 2;              /* Not valid RDD */
   }

   /* Create a new RDD node */
   pRddNewNode = ( LPRDDNODE ) hb_xgrab( sizeof( RDDNODE ) );
   memset( pRddNewNode, 0, sizeof( RDDNODE ) );

   /* Fill the new RDD node */
   strncat( pRddNewNode->szName, szDriver, HARBOUR_MAX_RDD_DRIVERNAME_LENGTH );
   pRddNewNode->uiType = uiType;
   pRddNewNode->rddID = s_uiRddMax;

   /* Call <szDriver>_GETFUNCTABLE() */
   hb_vmPushSymbol( hb_dynsymSymbol( pGetFuncTable ) );
   hb_vmPushNil();
   hb_vmPushPointer( ( void * ) &uiFunctions );
   hb_vmPushPointer( ( void * ) &pRddNewNode->pTable );
   hb_vmPushPointer( ( void * ) &pRddNewNode->pSuperTable );
   hb_vmPushInteger( s_uiRddMax );
   hb_vmDo( 4 );
   if( hb_parni( -1 ) != SUCCESS )
   {
      hb_xfree( pRddNewNode );         /* Delete de new RDD node */
      return 3;                        /* Invalid FUNCTABLE */
   }

   if( s_uiRddMax == 0 )                /* First RDD node */
   {
      LOCK_AREA_INIT
      s_RddList = (LPRDDNODE *) hb_xgrab( sizeof(LPRDDNODE) );
   }
   else
      s_RddList = (LPRDDNODE *) hb_xrealloc( s_RddList, sizeof(LPRDDNODE) * ( s_uiRddMax + 1 ) );

   s_RddList[ s_uiRddMax++ ] = pRddNewNode;   /* Add the new RDD node */

   if( pRddNewNode->pTable.init != NULL )
   {
      SELF_INIT( pRddNewNode );
   }

   return 0;                           /* Ok */
}

/*
 * pTable - a table in new RDDNODE that will be filled
 * pSubTable - a table with a list of supported functions
 * pSuperTable - a current table in a RDDNODE
 * szDrvName - a driver name that will be inherited
 */
HB_EXPORT ERRCODE hb_rddInherit( PRDDFUNCS pTable, PRDDFUNCS pSubTable, PRDDFUNCS pSuperTable, BYTE * szDrvName )
{
   LPRDDNODE pRddNode;
   USHORT uiCount;
   DBENTRYP_V * pFunction, * pSubFunction;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddInherit(%p, %p, %p, %s)", pTable, pSubTable, pSuperTable, szDrvName));

   if( !pTable )
   {
      return FAILURE;
   }

   /* Copy the pSuperTable into pTable */
   if( !szDrvName || ! *szDrvName )
   {
      /* no name for inherited driver - use the default one */
      memcpy( pTable, &waTable, sizeof( RDDFUNCS ) );
      memcpy( pSuperTable, &waTable, sizeof( RDDFUNCS ) );
   }
   else
   {
      char szSuperName[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 ];
      hb_strncpyUpper( szSuperName, ( char * ) szDrvName, HARBOUR_MAX_RDD_DRIVERNAME_LENGTH );
      pRddNode = hb_rddFindNode( szSuperName, NULL );

      if( !pRddNode )
      {
         return FAILURE;
      }

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
 * check if a given name can be used as alias expression
 */
static ERRCODE hb_rddVerifyAliasName( char * szAlias )
{
   char c;

   if( szAlias )
   {
      while( *szAlias == ' ' )
      {
         szAlias++;
      }
      c = *szAlias;
      if( c >= 'a' && c <= 'z' )
      {
         c -= 'a' - 'A';
      }
      if( ( c >= 'A' && c <= 'Z' ) || c == '_' )
      {
         c = *(++szAlias);
         while( c != 0 && c != ' ' )
         {
            if( c != '_' && ! ( c >= '0' && c <= '9' ) &&
                ! ( c >= 'A' && c <= 'Z' ) && ! ( c >= 'a' && c <= 'z' ) )
            {
               return FAILURE;
            }
            c = *(++szAlias);
         }
         return SUCCESS;
      }
   }
   return FAILURE;
}

/*
 * Find a WorkArea by the alias, return FAILURE if not found
 */
static ERRCODE hb_rddGetAliasNumber( char * szAlias, int * iArea )
{
   BOOL fOneLetter;
   char c;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddGetAliasNumber(%s, %p)", szAlias, iArea));

   while ( *szAlias == ' ' )
   {
      szAlias++;
   }
   c = szAlias[ 0 ];
   if ( c >= 'a' && c <= 'z' )
   {
      c -= 'a' - 'A';
   }

   fOneLetter = c && ( szAlias[ 1 ] == 0 || szAlias[ 1 ] == ' ' );

   if( c >= '0' && c <= '9' )
   {
      *iArea = atoi( szAlias );
   }
   else if ( fOneLetter && c >= 'A' && c <= 'K' )
   {
      *iArea = c - 'A' + 1;
   }
   else if ( fOneLetter && c == 'M' )
   {
      *iArea = 0;
   }
   else
   {
      PHB_DYNS pSymAlias = hb_dynsymFindName( szAlias );

      *iArea = pSymAlias ? ( int ) hb_dynsymAreaHandle( pSymAlias ) : 0;
      if( *iArea == 0 )
      {
         return FAILURE;
      }
   }

   return SUCCESS;
}

/*
 * Return the next free WorkArea for later use.
 */
static ERRCODE hb_rddSelectFirstAvailable( void )
{
   HB_THREAD_STUB
   USHORT uiArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelectFirstAvailable()"));

   LOCK_AREA
   uiArea = 1;
   while ( uiArea < s_uiWaNumMax )
   {
      if ( s_WaNums[ uiArea ] == 0 )
         break;
      uiArea++;
   }
   HB_SET_WA( uiArea );
   UNLOCK_AREA
   return SUCCESS;
}

/*
 * Prepares a new WorkArea node.
 */
static AREAP hb_rddNewAreaNode( LPRDDNODE pRddNode, USHORT uiRddID )
{
   AREAP pArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddNewAreaNode(%p %d)", pRddNode, uiRddID));

   if( pRddNode->uiAreaSize == 0 ) /* Calculate the size of WorkArea */
   {
      USHORT uiSize;

      pArea = ( AREAP ) hb_xgrab( sizeof( AREA ) );
      memset( pArea, 0, sizeof( AREA ) );
      pArea->lprfsHost = &pRddNode->pTable;

      /* Need more space? */
      SELF_STRUCTSIZE( pArea, &uiSize );
      if( uiSize > sizeof( AREA ) )   /* Size of Area changed */
      {
         pArea = ( AREAP ) hb_xrealloc( pArea, uiSize );
         memset( pArea, 0, uiSize );
         pArea->lprfsHost = &pRddNode->pTable;
      }

      pRddNode->uiAreaSize = uiSize;  /* Update the size of WorkArea */
   }
   else
   {
      pArea = ( AREAP ) hb_xgrab( pRddNode->uiAreaSize );
      memset( pArea, 0, pRddNode->uiAreaSize );
      pArea->lprfsHost = &pRddNode->pTable;
   }

   pArea->rddID = uiRddID;
   SELF_NEW( pArea );

   return pArea;
}

/*
 * Closes and releases the current WorkArea preparing it
 * to be used with a new database.
 */
HB_EXPORT void hb_rddReleaseCurrentArea( void )
{
   HB_THREAD_STUB
   USHORT uiWaPos;
   AREAP pArea = HB_CURRENT_WA;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddReleaseCurrentArea()"));

   if( !pArea )
   {
      return;
   }

   if( SELF_CLOSE( pArea ) == FAILURE )
   {
      return;
   }
   SELF_RELEASE( pArea );

   LOCK_AREA

   uiWaPos = s_WaNums[ s_uiCurrArea ];
   s_WaNums[ s_uiCurrArea ] = 0;
   s_uiWaMax--;
   if ( s_uiWaMax <= 1 )
   {
      s_uiWaSpace = s_uiWaMax = s_uiWaNumMax = 0;
      hb_xfree( s_WaList );
      hb_xfree( s_WaNums );
      s_WaList = NULL;
      s_WaNums = NULL;
   }
   else
   {
      while ( uiWaPos < s_uiWaMax )
      {
         s_WaList[ uiWaPos ] = s_WaList[ uiWaPos + 1 ];
         s_WaNums[ s_WaList[ uiWaPos ]->uiArea ] = uiWaPos;
         uiWaPos++;
      }
      s_WaList[ s_uiWaMax ] = NULL;
      if ( s_uiWaSpace - s_uiWaMax >= 256 )
      {
         s_uiWaSpace = ( ( s_uiWaMax + 256 ) >> 8 ) << 8;
         s_WaList = (AREAP *) hb_xrealloc( s_WaList, s_uiWaSpace * sizeof(AREAP) );
      }
   }
   s_pCurrArea = NULL;

   UNLOCK_AREA
}

/*
 * Closes all WorkAreas.
 */
static void hb_rddCloseAll( void )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_rddCloseAll()"));

   if ( s_uiWaMax > 0 )
   {
      BOOL isParents = TRUE, isFinish = FALSE;
      AREAP pArea;
      USHORT uiIndex;

      LOCK_AREA

      while( isParents )
      {
         isParents = FALSE;
         for ( uiIndex = 1; uiIndex < s_uiWaMax; uiIndex++ )
         {
            pArea = s_WaList[ uiIndex ];
            HB_SET_WA( pArea->uiArea );
            if ( isFinish )
            {
               SELF_RELEASE( pArea );
               s_WaNums[ s_uiCurrArea ] = 0;
               s_pCurrArea = NULL;
            }
            else if( pArea->uiParents )
            {
               isParents = TRUE;
            }
            else
            {
               SELF_CLOSE( pArea );
            }
         }
         if( !isParents && !isFinish )
         {
            isParents = isFinish = TRUE;
         }
      }

      s_uiWaSpace = s_uiWaMax = s_uiWaNumMax = 0;
      hb_xfree( s_WaList );
      hb_xfree( s_WaNums );
      s_WaList = NULL;
      s_WaNums = NULL;
      HB_SET_WA( 1 );

      UNLOCK_AREA
   }
}


/*
 * -- FUNCTIONS ACCESSED FROM VIRTUAL MACHINE --
 */

/*
 * Shutdown the RDD system.
 */
HB_EXPORT void hb_rddShutDown( void )
{
   USHORT uiCount;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddShutDown()"));

   hb_rddCloseAll();

   s_szDefDriver[ 0 ] = '\0';

   if ( s_uiRddMax > 0 )
   {
      for ( uiCount = 0; uiCount < s_uiRddMax; uiCount++ )
      {
         if ( s_RddList[ uiCount ]->pTable.exit != NULL )
         {
            SELF_EXIT( s_RddList[ uiCount ] );
         }
         hb_xfree( s_RddList[ uiCount ] );
      }
      hb_xfree( s_RddList );
      s_RddList = NULL;

      LOCK_AREA_DESTROY
   }
}

/*
 * Insert the new WorkArea node
 */
HB_EXPORT USHORT hb_rddInsertAreaNode( char *szDriver )
{
   HB_THREAD_STUB

   USHORT uiRddID, uiWaPos;
   LPRDDNODE pRddNode;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddInsertAreaNode(%s)", szDriver));

   pRddNode = hb_rddFindNode( szDriver, &uiRddID );

   if( !pRddNode )
   {
      return FALSE;
   }

   LOCK_AREA

   if ( s_uiCurrArea == 0 )
   {
      USHORT uiArea = 1;
      while ( uiArea < s_uiWaNumMax )
      {
         if ( s_WaNums[ uiArea ] == 0 )
            break;
         uiArea++;
      }
      HB_SET_WA( uiArea );
   }
   else if( s_pCurrArea )
   {
      return FALSE;
   }

   if ( s_uiCurrArea >= s_uiWaNumMax )
   {
      int iSize = ( ( s_uiCurrArea + 256 ) >> 8 ) << 8;

      if ( s_uiWaNumMax == 0 )
      {
         s_WaNums = (USHORT *) hb_xgrab( iSize * sizeof(USHORT) );
      }
      else
      {
         s_WaNums = (USHORT *) hb_xrealloc( s_WaNums, iSize * sizeof(USHORT) );
      }
      memset( &s_WaNums[ s_uiWaNumMax ], 0, ( iSize - s_uiWaNumMax ) * sizeof(USHORT) );
      s_uiWaNumMax = iSize;
   }

   if ( s_uiWaSpace == 0 )
   {
      s_uiWaSpace = 256;
      s_WaList = (AREAP *) hb_xgrab( s_uiWaSpace * sizeof(AREAP) );
      memset( &s_WaList[ 0 ], 0, s_uiWaSpace * sizeof(AREAP) );
      s_WaList[ 0 ] = NULL;
      uiWaPos = 1;
      s_uiWaMax = 2;
   }
   else
   {
      uiWaPos = s_uiWaMax++;
      if ( s_uiWaMax > s_uiWaSpace )
      {
         s_uiWaSpace = ( ( s_uiWaMax + 256 ) >> 8 ) << 8;
         s_WaList = (AREAP *) hb_xrealloc( s_WaList, s_uiWaSpace * sizeof(AREAP) );
         memset( &s_WaList[ s_uiWaMax ], 0, ( s_uiWaSpace - s_uiWaMax ) * sizeof(AREAP) );
      }
      while ( uiWaPos > 1 )
      {
         if ( s_WaList[ uiWaPos - 1 ]->uiArea < s_uiCurrArea )
            break;
         s_WaList[ uiWaPos ] = s_WaList[ uiWaPos - 1 ];
         s_WaNums[ s_WaList[ uiWaPos ]->uiArea ] = uiWaPos;
         uiWaPos--;
      }
   }
   s_WaList[ uiWaPos ] = hb_rddNewAreaNode( pRddNode, uiRddID );
   s_WaNums[ s_uiCurrArea ] = uiWaPos;
   s_WaList[ uiWaPos ]->uiArea = s_uiCurrArea;
   s_pCurrArea = s_WaList[ uiWaPos ];

   UNLOCK_AREA

   return TRUE;
}

/*
 * allocate and return atomAlias for new workarea or NULL if alias already exist
 */
HB_EXPORT void * hb_rddAllocWorkAreaAlias( char * szAlias, int iArea )
{
   PHB_DYNS pSymAlias;
   int iDummyArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddAllocWorkAreaAlias(%s, %d)", szAlias, iArea));

   /* Verify if the alias name is valid symbol */
   if( hb_rddVerifyAliasName( szAlias ) != SUCCESS )
   {
      hb_errRT_DBCMD_Ext( EG_BADALIAS, EDBCMD_BADALIAS, NULL, szAlias, EF_CANDEFAULT );
      return NULL;
   }
   /* Verify if the alias is already in use */
   if( hb_rddGetAliasNumber( szAlias, &iDummyArea ) == SUCCESS )
   {
      hb_errRT_DBCMD_Ext( EG_DUPALIAS, EDBCMD_DUPALIAS, NULL, szAlias, EF_CANDEFAULT );
      return NULL;
   }

   LOCK_AREA
   pSymAlias = hb_dynsymGet( szAlias );
   if( hb_dynsymAreaHandle( pSymAlias ) != 0 )
   {
      pSymAlias = NULL;
   }
   else
   {
      hb_dynsymSetAreaHandle( pSymAlias, iArea );
   }
   UNLOCK_AREA

   if( ! pSymAlias )
   {
      hb_errRT_DBCMD_Ext( EG_DUPALIAS, EDBCMD_DUPALIAS, NULL, szAlias, EF_CANDEFAULT );
   }

   return pSymAlias;
}

/*
 * Return the current WorkArea number.
 */
HB_EXPORT int hb_rddGetCurrentWorkAreaNumber( void )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_rddGetCurrentWorkAreaNumber()"));

   return s_uiCurrArea;
}

/*
 * Select a WorkArea by the number.
 */
HB_EXPORT ERRCODE hb_rddSelectWorkAreaNumber( int iArea )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelectWorkAreaNumber(%d)", iArea));

   LOCK_AREA
   if ( iArea < 1 || iArea > HARBOUR_MAX_RDD_AREA_NUM )
   {
      HB_SET_WA( 0 );
   }
   else
   {
      HB_SET_WA( iArea );
   }
   UNLOCK_AREA

   return ( HB_CURRENT_WA == NULL ) ? FAILURE : SUCCESS;
}

/*
 * Select a WorkArea by the symbol name.
 */
HB_EXPORT ERRCODE hb_rddSelectWorkAreaSymbol( PHB_SYMB pSymAlias )
{
   ERRCODE bResult;
   char * szName;
   int iArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelectWorkAreaSymbol(%p)", pSymAlias));

   iArea = ( int ) hb_dynsymAreaHandle( pSymAlias->pDynSym );
   if( iArea )
   {
      bResult = hb_rddSelectWorkAreaNumber( iArea );
   }
   else
   {
      szName = hb_dynsymName( pSymAlias->pDynSym );

      if( szName[ 0 ] && ! szName[ 1 ] && toupper( szName[ 0 ] ) >= 'A' && toupper( szName[ 0 ] ) <= 'K' )
      {
         bResult = hb_rddSelectWorkAreaNumber( toupper( szName[ 0 ] ) - 'A' + 1 );
      }
      else
      {
         /*
          * generate an error with retry possibility
          * (user created error handler can open a missing database)
          */
         USHORT uiAction = E_RETRY;
         HB_ITEM_PTR pError;

         pError = hb_errRT_New( ES_ERROR, NULL, EG_NOALIAS, EDBCMD_NOALIAS, NULL, pSymAlias->szName, 0, EF_CANRETRY );

         bResult = FAILURE;
         while( uiAction == E_RETRY )
         {
            uiAction = hb_errLaunch( pError );

            if( uiAction == E_RETRY )
            {
               iArea = ( int ) hb_dynsymAreaHandle( pSymAlias->pDynSym );
               if( iArea )
               {
                  bResult = hb_rddSelectWorkAreaNumber( iArea );
                  uiAction = E_DEFAULT;
               }
            }
         }

         hb_itemRelease( pError );
      }
   }

   return bResult;
}

/*
 * Select a WorkArea by the name.
 */
HB_EXPORT ERRCODE hb_rddSelectWorkAreaAlias( char * szAlias )
{
   ERRCODE bResult;
   int iArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelectWorkAreaAlias(%s)", szAlias));

   bResult = hb_rddGetAliasNumber( szAlias, &iArea );

   if ( bResult == FAILURE )
   {
      /*
       * generate an error with retry possibility
       * (user created error handler can open a missing database)
       */
      HB_ITEM_PTR pError = hb_errRT_New( ES_ERROR, NULL, EG_NOALIAS, EDBCMD_NOALIAS, NULL, szAlias, 0, EF_CANRETRY );

      do
      {
         if( hb_errLaunch( pError ) != E_RETRY )
         {
            break;
         }
         bResult = hb_rddGetAliasNumber( szAlias, &iArea );
      }
      while( bResult == FAILURE );

      hb_itemRelease( pError );
   }

   if ( bResult == SUCCESS )
   {
      if( iArea < 1 || iArea > HARBOUR_MAX_RDD_AREA_NUM )
         bResult = hb_rddSelectFirstAvailable();
      else
         bResult = hb_rddSelectWorkAreaNumber( iArea );
   }

   return bResult;
}

/*
 *  Function for getting current workarea pointer
 */
HB_EXPORT void * hb_rddGetCurrentWorkAreaPointer( void )
{
   HB_THREAD_STUB

   HB_TRACE(HB_TR_DEBUG, ("hb_rddGetCurrentWorkAreaPointer()"));

   return HB_CURRENT_WA;
}

/*
 * call a pCallBack function with all open workareas ###
 */
HB_EXPORT ERRCODE hb_rddIterateWorkAreas( WACALLBACK pCallBack, int data )
{
   USHORT uiIndex;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddIterateWorkAreas(%p)", pCallBack));

   LOCK_AREA
   for ( uiIndex = 1; uiIndex < s_uiWaMax; uiIndex++ )
   {
      if ( ! (*pCallBack)( s_WaList[ uiIndex ], data ) )
      {
         break;
      }
   }
   UNLOCK_AREA
   return SUCCESS;
}

/*
 * Find a field index by name
 */
HB_EXPORT USHORT hb_rddFieldIndex( AREAP pArea, char * szName )
{
   USHORT uiCount = 0;
   LPFIELD pField;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddFieldIndex(%p, %s)", pArea, szName));

   while( HB_ISSPACE( *szName ) )
   {
      ++szName;
   }

   if( *szName )
   {
      char szSym[ HB_SYMBOL_NAME_LEN + 1 ];
      hb_strncpyUpperTrim( szSym, szName, HB_SYMBOL_NAME_LEN );

      pField = pArea->lpFields;
      while( pField )
      {
         ++uiCount;
         if( strcmp( szSym, hb_dynsymName( ( PHB_DYNS ) pField->sym ) ) == 0 )
            return uiCount;
         pField = pField->lpfNext;
      }
   }
   return 0;
}

/*
 * find a field expression index, this function strips _FIELD->, FIELD->,
 * alias-> prefixes
 */
HB_EXPORT USHORT hb_rddFieldExpIndex( AREAP pArea, char * szField )
{
   int n;

   while( HB_ISSPACE( *szField ) )
   {
      ++szField;
   }

   if( strchr( szField, '>' ) != NULL )
   {
      char szAlias[ HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 ];
      int i, j, l;

      n = 0;
      if ( SELF_ALIAS( pArea, ( BYTE * ) szAlias ) == SUCCESS )
         l = strlen( szAlias );
      else
         l = 0;

      /*
       * strip the _FIELD-> and FIELD-> prefix, it could be nested
       * so repeat this process until all prefixes will be removed
       */
      do
      {
         j = n;
         if( hb_strnicmp( &szField[ n ], "FIELD", 5 ) == 0 )
            i = 5;
         else if( hb_strnicmp( &szField[ n ], "_FIELD", 6 ) == 0 )
            i = 6;
         else if( l > 0 && hb_strnicmp( &szField[ n ], szAlias, l ) == 0 )
            i = l;
         else
            i = 0;

         if( i > 0 )
         {
            i += n;
            while( HB_ISSPACE( szField[ i ] ) )
               i++;
            if( szField[ i ] == '-' && szField[ i + 1 ] == '>' )
            {
               n = i + 2;
               while( szField[ n ] == ' ' )
                  n++;
            }
         }
      }
      while ( n != j );
      szField = &szField[ n ];
   }
   return hb_rddFieldIndex( pArea, szField );
}

/*
 * Obtain the current value of a field.
 */
HB_EXPORT ERRCODE hb_rddFieldGet( HB_ITEM_PTR pItem, PHB_SYMB pFieldSymbol )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddFieldGet(%p, %p)", pItem, pFieldSymbol));

   if( pArea )
   {
      USHORT uiField = 1;
      LPFIELD pField = pArea->lpFields;
      PHB_DYNS pDynSym = pFieldSymbol->pDynSym;

      while( pField )
      {
         if( ( PHB_DYNS ) pField->sym == pDynSym )
         {
            return SELF_GETVALUE( pArea, uiField, pItem );
         }
         ++uiField;
         pField = pField->lpfNext;
      }
   }
   return FAILURE;
}

/*
 * Assign a value to a field.
 */
HB_EXPORT ERRCODE hb_rddFieldPut( HB_ITEM_PTR pItem, PHB_SYMB pFieldSymbol )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddFieldPut(%p, %p)", pItem, pFieldSymbol));

   if( pArea )
   {
      USHORT uiField = 1;
      LPFIELD pField = pArea->lpFields;
      PHB_DYNS pDynSym = pFieldSymbol->pDynSym;

      while( pField )
      {
         if( ( PHB_DYNS ) pField->sym == pDynSym )
         {
            return SELF_PUTVALUE( pArea, uiField, pItem );
         }
         ++uiField;
         pField = pField->lpfNext;
      }
   }
   return FAILURE;
}

/*
 * Obtain the current value of a field.
 */
HB_EXPORT ERRCODE hb_rddGetFieldValue( HB_ITEM_PTR pItem, PHB_SYMB pFieldSymbol )
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
      pError = hb_errRT_New( ES_ERROR, NULL, EG_NOVAR, EDBCMD_NOVAR, NULL, pFieldSymbol->szName, 0, EF_CANRETRY );

      while( uiAction == E_RETRY )
      {
         uiAction = hb_errLaunch( pError );
         if( uiAction == E_RETRY )
         {
            bSuccess = hb_rddFieldGet( pItem, pFieldSymbol );

            if( bSuccess == SUCCESS )
            {
               uiAction = E_DEFAULT;
            }
         }
      }
      hb_itemRelease( pError );
   }
   return bSuccess;
}

/*
 * Assign a value to a field.
 */
HB_EXPORT ERRCODE hb_rddPutFieldValue( HB_ITEM_PTR pItem, PHB_SYMB pFieldSymbol )
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
            {
               uiAction = E_DEFAULT;
            }
         }
      }
      hb_itemRelease( pError );
   }
   return bSuccess;
}

/*
 * -- END OF FUNCTIONS ACCESSED FROM VIRTUAL MACHINE --
 */

/*
 * -- BASIC RDD METHODS --
 */

/*
 * -- HARBOUR FUNCTIONS --
 */

HB_FUNC( AFIELDS )
{
   HB_THREAD_STUB
   PHB_ITEM pName, pType, pLen, pDec;
   USHORT uiFields, uiArrayLen, uiCount;
   AREAP pArea = HB_CURRENT_WA;

   if( !pArea )
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
   SELF_FIELDCOUNT( pArea, &uiFields );
   if( pName )
   {
      uiArrayLen = ( USHORT ) hb_arrayLen( pName );
      if( uiArrayLen > uiFields )
         uiArrayLen = uiFields;
      for( uiCount = 1; uiCount <= uiArrayLen; ++uiCount )
      {
         SELF_FIELDINFO( pArea, uiCount, DBS_NAME,
                         hb_arrayGetItemPtr( pName, uiCount ) );
      }
   }
   if( pType )
   {
      uiArrayLen = ( USHORT ) hb_arrayLen( pType );
      if( uiArrayLen > uiFields )
         uiArrayLen = uiFields;
      for( uiCount = 1; uiCount <= uiArrayLen; ++uiCount )
      {
         SELF_FIELDINFO( pArea, uiCount, DBS_TYPE,
                         hb_arrayGetItemPtr( pType, uiCount ) );
      }
   }
   if( pLen )
   {
      uiArrayLen = ( USHORT ) hb_arrayLen( pLen );
      if( uiArrayLen > uiFields )
         uiArrayLen = uiFields;
      for( uiCount = 1; uiCount <= uiArrayLen; ++uiCount )
      {
         SELF_FIELDINFO( pArea, uiCount, DBS_LEN,
                         hb_arrayGetItemPtr( pLen, uiCount ) );
      }
   }
   if( pDec )
   {
      uiArrayLen = ( USHORT ) hb_arrayLen( pDec );
      if( uiArrayLen > uiFields )
         uiArrayLen = uiFields;
      for( uiCount = 1; uiCount <= uiArrayLen; ++uiCount )
      {
         SELF_FIELDINFO( pArea, uiCount, DBS_DEC,
                         hb_arrayGetItemPtr( pDec, uiCount ) );
      }
   }

   hb_retni( uiArrayLen );
}

HB_FUNC( ALIAS )
{
   HB_THREAD_STUB
   USHORT uiArea;
   AREAP pArea;

   uiArea = hb_parni( 1 );
   pArea = uiArea ? HB_GET_WA( uiArea ) : HB_CURRENT_WA;
   if( pArea )
   {
      char szAlias[ HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 ];

      if ( SELF_ALIAS( pArea, ( BYTE * ) szAlias ) == SUCCESS )
      {
         hb_retc( szAlias );
         return;
      }
   }
   hb_retc( NULL );
}

HB_FUNC( DBEVAL )
{
   HB_THREAD_STUB

   DBEVALINFO pEvalInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      memset( &pEvalInfo, 0, sizeof( DBEVALINFO ) );
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

      SELF_DBEVAL( pArea, &pEvalInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBEVAL" );
}

HB_FUNC( DBF )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      char szAlias[ HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 ];

      if ( SELF_ALIAS( pArea, ( BYTE * ) szAlias ) == SUCCESS )
      {
         hb_retc( szAlias );
         return;
      }
   }
   hb_retc( NULL );
}

HB_FUNC( BOF )
{
   HB_THREAD_STUB
   BOOL bBof = TRUE;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_BOF( pArea, &bBof );

   hb_retl( bBof );
}

HB_FUNC( DBAPPEND )
{
   HB_THREAD_STUB
   BOOL bUnLockAll;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      bUnLockAll = ISLOG( 1 ) ? hb_parl( 1 ) : TRUE;
      s_bNetError = FALSE;
      if( SELF_APPEND( pArea, bUnLockAll ) == FAILURE )
      {
         s_bNetError = TRUE;           /* Temp fix! What about other types of errors? */
      }
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBAPPEND" );
}

HB_FUNC( DBCLEARFILTER )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_CLEARFILTER( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBCLEARFILTER" );
}

HB_FUNC( DBCLOSEALL )
{
   hb_rddCloseAll();
}

HB_FUNC( DBCLOSEAREA )
{
   HB_THREAD_STUB

   if( HB_CURRENT_WA )
   {
      hb_rddReleaseCurrentArea();
   }
}

HB_FUNC( DBCOMMIT )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;
   if( pArea )
      SELF_FLUSH( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBCOMMIT" );
}

HB_FUNC( DBCOMMITALL )
{
   HB_THREAD_STUB
   USHORT uiArea = hb_rddGetCurrentWorkAreaNumber(), uiIndex;

   LOCK_AREA
   for ( uiIndex = 1; uiIndex < s_uiWaMax; ++uiIndex )
   {
      hb_rddSelectWorkAreaNumber( s_WaList[ uiIndex ]->uiArea );
      SELF_FLUSH( HB_CURRENT_WA );
   }
   UNLOCK_AREA
   hb_rddSelectWorkAreaNumber( uiArea );
}

static ERRCODE hb_rddOpenTable( char * szFileName,  char * szDriver,
                                USHORT uiArea, char *szAlias,
                                BOOL fShared, BOOL fReadonly,
                                char * szCpId, ULONG ulConnection )
{
   char szDriverBuffer[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 ];
   DBOPENINFO pInfo;
   ERRCODE errCode;
   USHORT uiPrevArea;
   AREAP pArea;

   s_bNetError = FALSE;

   if( szDriver && szDriver[ 0 ] )
   {
      hb_strncpyUpper( szDriverBuffer, szDriver, HARBOUR_MAX_RDD_DRIVERNAME_LENGTH );
      szDriver = szDriverBuffer;
   }
   else
   {
      szDriver = hb_rddDefaultDrv( NULL );
   }

   uiPrevArea = hb_rddGetCurrentWorkAreaNumber();

   /*
    * 0 means chose first available in hb_rddInsertAreaNode()
    * This hack is necessary to avoid race condition in MT
    * if we don't want to lock whole RDD subsystem, Druzus
    */
   hb_rddSelectWorkAreaNumber( uiArea );
   if( uiArea )
   {
      hb_rddReleaseCurrentArea();
   }

   s_bNetError = TRUE;

   /* Create a new WorkArea node */
   if( ! hb_rddInsertAreaNode( szDriver ) )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_USE_BADPARAMETER, NULL, "DBUSEAREA" );
      return FAILURE;
   }
   pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   /* Fill pInfo structure */
   pInfo.uiArea = pArea->uiArea;
   pInfo.abName = ( BYTE * ) szFileName;
   pInfo.atomAlias = ( BYTE * ) szAlias;
   pInfo.fShared = fShared;
   pInfo.fReadonly = fReadonly;
   pInfo.cdpId = ( BYTE * ) szCpId;
   pInfo.ulConnection = ulConnection;
   pInfo.lpdbHeader = NULL;

   /* Open file */
   errCode = SELF_OPEN( pArea, &pInfo );

   if( errCode != SUCCESS )
   {
      hb_rddReleaseCurrentArea();
      hb_rddSelectWorkAreaNumber( uiPrevArea );
   }

   s_bNetError = errCode != SUCCESS;

   return errCode;
}

static ERRCODE hb_rddCreateTable( char * szFileName, PHB_ITEM pStruct,
                                  char * szDriver,
                                  BOOL fKeepOpen, USHORT uiArea, char *szAlias,
                                  char * szCpId, ULONG ulConnection )
{
   char szDriverBuffer[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 ];
   DBOPENINFO pInfo;
   ERRCODE errCode;
   USHORT uiPrevArea;
   AREAP pArea;

   if( szDriver && szDriver[ 0 ] )
   {
      hb_strncpyUpper( szDriverBuffer, szDriver, HARBOUR_MAX_RDD_DRIVERNAME_LENGTH );
      szDriver = szDriverBuffer;
   }
   else
   {
      szDriver = hb_rddDefaultDrv( NULL );
   }

   uiPrevArea = hb_rddGetCurrentWorkAreaNumber();

   /*
    * 0 means chose first available in hb_rddInsertAreaNode()
    * This hack is necessary to avoid race condition in MT
    * if we don't want to lock whole RDD subsystem, Druzus
    */
   hb_rddSelectWorkAreaNumber( uiArea );
   if( uiArea )
   {
      hb_rddReleaseCurrentArea();
   }

   s_bNetError = TRUE;

   /* Create a new WorkArea node */
   if( ! hb_rddInsertAreaNode( szDriver ) )
   {
      hb_errRT_DBCMD( EG_CREATE, EDBCMD_BADPARAMETER, NULL, "DBCREATE" );
      return FAILURE;
   }
   pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   /* Fill pInfo structure */
   pInfo.uiArea = pArea->uiArea;
   pInfo.abName = ( BYTE * ) szFileName;
   pInfo.atomAlias = ( BYTE * ) szAlias;
   pInfo.fShared = FALSE;
   pInfo.fReadonly = FALSE;
   pInfo.cdpId = ( BYTE * ) szCpId;
   pInfo.ulConnection = ulConnection;
   pInfo.lpdbHeader = NULL;

   errCode = SELF_CREATEFIELDS( pArea, pStruct );
   if( errCode == SUCCESS )
      errCode = SELF_CREATE( pArea, &pInfo );

   if( !fKeepOpen || errCode != SUCCESS )
   {
      hb_rddReleaseCurrentArea();
      hb_rddSelectWorkAreaNumber( uiPrevArea );
   }

   s_bNetError = errCode != SUCCESS;

   return errCode;
}

HB_FUNC( DBCREATE )
{
   char * szFileName;
   USHORT uiSize, uiLen;
   PHB_ITEM pStruct, pFieldDesc;
   BOOL fKeepOpen, fCurrArea;

   /*
    * NOTE: 4-th and 5-th parameters are undocumented Clipper ones
    * 4-th is boolean flag indicating if file should stay open and
    * 5-th is alias - if not given then WA is open without alias
    */

   szFileName = hb_parc( 1 );
   pStruct = hb_param( 2 , HB_IT_ARRAY );
   fKeepOpen = ISLOG( 4 );
   fCurrArea = fKeepOpen && !hb_parl( 4 );

   if( !pStruct || hb_arrayLen( pStruct ) == 0 ||
       !szFileName || !szFileName[ 0 ] )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, "DBCREATE" );
      return;
   }
   uiLen = ( USHORT ) hb_arrayLen( pStruct );

   for( uiSize = 1; uiSize <= uiLen; ++uiSize )
   {
      pFieldDesc = hb_arrayGetItemPtr( pStruct, uiSize );

      /* Validate items types of fields */
      if( hb_arrayLen( pFieldDesc ) < 4 ||
          !( hb_arrayGetType( pFieldDesc, 1 ) & HB_IT_STRING ) ||
          !( hb_arrayGetType( pFieldDesc, 2 ) & HB_IT_STRING ) ||
          !( hb_arrayGetType( pFieldDesc, 3 ) & HB_IT_NUMERIC ) ||
          !( hb_arrayGetType( pFieldDesc, 4 ) & HB_IT_NUMERIC ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, "DBCREATE" );
         return;
      }
   }

   hb_rddCreateTable( szFileName, pStruct, hb_parc( 3 ), fKeepOpen,
                      fCurrArea ? hb_rddGetCurrentWorkAreaNumber() : 0,
                      hb_parc( 5 ), hb_parc( 6 ), hb_parnl( 7 ) );
}

HB_FUNC( DBDELETE )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      SELF_DELETE( pArea );
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBDELETE" );
   }
}

HB_FUNC( DBFILTER )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      PHB_ITEM pFilter = hb_itemNew( NULL );
      hb_itemPutC( pFilter, "" );
      SELF_FILTERTEXT( pArea, pFilter );
      hb_itemReturn( pFilter );
      hb_itemRelease( pFilter );
   }
   else
      hb_retc( NULL );
}

HB_FUNC( DBGOBOTTOM )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_GOBOTTOM( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBGOBOTTOM" );
}

HB_FUNC( DBGOTO )
{
   HB_THREAD_STUB
   PHB_ITEM pItem;
   AREAP pArea = HB_CURRENT_WA;

   if( !pArea )
   {
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBGOTO" );
      return;
   }

   pItem = hb_param( 1, HB_IT_ANY );
   if( !pItem )
      hb_errRT_DBCMD( EG_ARG, EDBCMD_NOVAR, NULL, "DBGOTO" );
   else
      SELF_GOTOID( pArea, pItem );
   hb_ret();
}

HB_FUNC( DBGOTOP )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_GOTOP( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBGOTOP" );
}

HB_FUNC( __DBLOCATE )
{
   HB_THREAD_STUB
   DBSCOPEINFO dbScopeInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( !pArea )
   {
      hb_errRT_DBCMD( EG_NOTABLE, EG_NOTABLE, NULL, "__DBLOCATE" );
      return;
   }

   dbScopeInfo.itmCobFor   = hb_param( 1, HB_IT_BLOCK );
   dbScopeInfo.lpstrFor    = NULL;
   dbScopeInfo.itmCobWhile = hb_param( 2, HB_IT_BLOCK );
   dbScopeInfo.lpstrWhile  = NULL;
   dbScopeInfo.lNext       = hb_param( 3, HB_IT_NUMERIC );
   dbScopeInfo.itmRecID    = hb_param( 4, HB_IT_NUMERIC );
   dbScopeInfo.fRest       = hb_param( 5, HB_IT_LOGICAL );

   dbScopeInfo.fIgnoreFilter     = TRUE;
   dbScopeInfo.fIncludeDeleted   = TRUE;
   dbScopeInfo.fLast             = FALSE;
   dbScopeInfo.fIgnoreDuplicates = FALSE;
   dbScopeInfo.fBackword         = FALSE;

   if ( SELF_SETLOCATE( pArea, &dbScopeInfo ) == SUCCESS )
   {
      SELF_LOCATE( pArea, FALSE );
   }
}

HB_FUNC( __DBSETLOCATE )
{
   HB_THREAD_STUB
   PHB_ITEM pLocate;
   DBSCOPEINFO pScopeInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pLocate = hb_param( 1, HB_IT_BLOCK );
      if( pLocate )
      {
         memset( &pScopeInfo, 0, sizeof( DBSCOPEINFO ) );
         pScopeInfo.itmCobFor = pLocate;
         SELF_SETLOCATE( pArea, &pScopeInfo );
      }
   }
}

HB_FUNC( __DBCONTINUE )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      SELF_LOCATE( pArea, TRUE );
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBCONTINUE" );
      return;
   }
}

HB_FUNC( __DBPACK )
{
   HB_THREAD_STUB
   PHB_ITEM pBlock, pEvery;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      /*
       * Additional feature: __dbPack( [<bBlock>, [<nEvery>] )
       * Code Block to execute for every record.
       */
      pBlock = hb_param( 1, HB_IT_BLOCK );
      if( pBlock )
      {
         hb_itemRelease( pArea->valResult );
         pArea->valResult = hb_itemArrayNew( 2 );
         hb_arraySet( pArea->valResult, 1, pBlock );
         pEvery = hb_param( 2, HB_IT_ANY );
         if( pEvery && HB_IS_NUMERIC( pEvery ) )
            hb_arraySet( pArea->valResult, 2, pEvery );
      }
      else
      {
         if ( pArea->valResult )
            hb_itemClear( pArea->valResult );
         else
            pArea->valResult = hb_itemNew( NULL );
      }
      SELF_PACK( pArea );
      if( pBlock )
         hb_itemClear( pArea->valResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "__DBPACK" );
}

HB_FUNC( DBRECALL )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;
   if( pArea )
      SELF_RECALL( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBRECALL" );
}

HB_FUNC( DBRLOCK )
{
   HB_THREAD_STUB
   DBLOCKINFO dbLockInfo;
   AREAP pArea = HB_CURRENT_WA;

   dbLockInfo.fResult = FALSE;
   if( pArea )
   {
      dbLockInfo.itmRecID = hb_param( 1, HB_IT_ANY );
      if( !dbLockInfo.itmRecID || ISNIL( 1 ) )
         dbLockInfo.uiMethod = DBLM_EXCLUSIVE;
      else
         dbLockInfo.uiMethod = DBLM_MULTIPLE;
      SELF_LOCK( pArea, &dbLockInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBRLOCK" );

   hb_retl( dbLockInfo.fResult );
}

HB_FUNC( DBRLOCKLIST )
{
   HB_THREAD_STUB
   PHB_ITEM pList;
   AREAP pArea = HB_CURRENT_WA;

   pList = hb_itemNew( NULL );
   hb_arrayNew( pList, 0 );
   if( pArea )
      SELF_INFO( pArea, DBI_GETLOCKARRAY, pList );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBRLOCKLIST" );

   hb_itemReturn( pList );
   hb_itemRelease( pList );
}

HB_FUNC( DBRUNLOCK )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_UNLOCK( pArea, hb_param( 1, HB_IT_ANY ) );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBRUNLOCK" );
}

HB_FUNC( DBSEEK )
{
   HB_THREAD_STUB
   PHB_ITEM pKey;
   BOOL bSoftSeek, bFindLast, fFound;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      if( !ISNIL( 1 ) )
      {
         pKey = hb_param( 1, HB_IT_ANY );
         bSoftSeek = ISLOG( 2 ) ? hb_parl( 2 ) : hb_set.HB_SET_SOFTSEEK;
         bFindLast = ISLOG( 3 ) ? hb_parl( 3 ) : FALSE;
         if( SELF_SEEK( pArea, bSoftSeek, pKey, bFindLast ) == SUCCESS )
         {
            if( SELF_FOUND( pArea, &fFound ) != FAILURE )
            {
               hb_retl( fFound );
               return;
            }
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

   if( ISCHAR( 1 ) )
   {
      hb_rddSelectWorkAreaAlias( hb_parc( 1 ) );
      hb_rddGetCurrentWorkAreaNumber();
   }
   else
   {
      LONG lNewArea = hb_parnl( 1 );

      if( lNewArea < 1 || lNewArea > HARBOUR_MAX_RDD_AREA_NUM )
      {
         hb_rddSelectFirstAvailable();
      }
      else
      {
         hb_rddSelectWorkAreaNumber( lNewArea );
      }
   }

}

HB_FUNC( __DBSETFOUND )
{
   HB_THREAD_STUB
   PHB_ITEM pFound;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pFound = hb_param( 1, HB_IT_LOGICAL );
      if( pFound )
         pArea->fFound = hb_itemGetL( pFound );
   }
}

HB_FUNC( DBSETFILTER )
{
   HB_THREAD_STUB
   PHB_ITEM pBlock, pText;
   DBFILTERINFO pFilterInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
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
         pFilterInfo.fFilter = TRUE;
         pFilterInfo.lpvCargo = NULL;
         pFilterInfo.fOptimized = FALSE;
         SELF_SETFILTER( pArea, &pFilterInfo );
         if( !pText )
            hb_itemRelease( pFilterInfo.abFilterText );
      }
      else
      {
         SELF_CLEARFILTER( pArea );
      }
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBSETFILTER" );
}

HB_FUNC( DBSKIP )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;
   if( pArea )
      SELF_SKIP( pArea, ISNUM( 1 ) ? hb_parnl( 1 ) : 1 );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBSKIP" );
}

static void hb_fldStructure( AREAP pArea, USHORT uiField, PHB_ITEM pField )
{
   hb_arrayNew( pField, 4 );

   SELF_FIELDINFO( pArea, uiField, DBS_NAME, hb_arrayGetItemPtr( pField, 1 ) );
   SELF_FIELDINFO( pArea, uiField, DBS_TYPE, hb_arrayGetItemPtr( pField, 2 ) );
   SELF_FIELDINFO( pArea, uiField, DBS_LEN,  hb_arrayGetItemPtr( pField, 3 ) );
   SELF_FIELDINFO( pArea, uiField, DBS_DEC,  hb_arrayGetItemPtr( pField, 4 ) );
}

static void hb_tblStructure( AREAP pArea, PHB_ITEM pStruct )
{
   USHORT uiFields, uiCount;

   if( SELF_FIELDCOUNT( pArea, &uiFields ) == SUCCESS )
   {
      if( hb_arraySize( pStruct, uiFields ) )
      {
         for( uiCount = 1; uiCount <= uiFields; ++uiCount )
         {
            hb_fldStructure( pArea, uiCount,
                             hb_arrayGetItemPtr( pStruct, uiCount ) );
         }
      }
   }
}

HB_FUNC( DBSTRUCT )
{
   PHB_ITEM pStruct;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   pStruct = hb_itemNew( NULL );
   hb_arrayNew( pStruct, 0 );
   if( pArea )
   {
      hb_tblStructure( pArea, pStruct );
   }
   hb_itemReturn( pStruct );
   hb_itemRelease( pStruct );
}

HB_FUNC( DBTABLEEXT )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;
   PHB_ITEM pItem = hb_itemPutC( NULL, "" );

   if( !pArea )
   {
      LPRDDNODE pRddNode;
      USHORT uiRddID;
      pRddNode = hb_rddFindNode( hb_rddDefaultDrv( NULL ), &uiRddID );
      if( pRddNode )
      {
         pArea = hb_rddNewAreaNode( pRddNode, uiRddID );
         if ( pArea )
         {
            SELF_INFO( ( AREAP ) pArea, DBI_TABLEEXT, pItem );
            SELF_RELEASE( pArea );
         }
      }
   }
   else
   {
      SELF_INFO( pArea, DBI_TABLEEXT, pItem );
   }
   hb_itemReturnForward( pItem );
   hb_itemRelease( pItem );
}

HB_FUNC( DBUNLOCK )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_UNLOCK( pArea, NULL );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBUNLOCK" );
}

HB_FUNC( DBUNLOCKALL )
{
   HB_THREAD_STUB
   USHORT uiArea = hb_rddGetCurrentWorkAreaNumber(), uiIndex;

   LOCK_AREA
   for ( uiIndex = 1; uiIndex < s_uiWaMax; ++uiIndex )
   {
      hb_rddSelectWorkAreaNumber( s_WaList[ uiIndex ]->uiArea );
      SELF_UNLOCK( HB_CURRENT_WA, NULL );
   }
   UNLOCK_AREA
   hb_rddSelectWorkAreaNumber( uiArea );
}

HB_FUNC( DBUSEAREA )
{
   char * szFileName;

   szFileName = hb_parc( 3 );

   if( !szFileName || !szFileName[ 0 ] )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_USE_BADPARAMETER, NULL, "DBUSEAREA" );
      return;
   }

   hb_rddOpenTable( szFileName, hb_parc( 2 ),
                    hb_parl( 1 ) ? 0 : hb_rddGetCurrentWorkAreaNumber(),
                    hb_parc( 4 ),
                    ISLOG( 5 ) ? hb_parl( 5 ) : !hb_set.HB_SET_EXCLUSIVE,
                    hb_parl( 6 ), hb_parc( 7 ), hb_parnl( 8 ) );
}

HB_FUNC( __DBZAP )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_ZAP( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "__DBZAP" );
}

HB_FUNC( DELETED )
{
   HB_THREAD_STUB
   BOOL bDeleted = FALSE;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_DELETED( pArea, &bDeleted );
   hb_retl( bDeleted );
}

HB_FUNC( EOF )
{
   HB_THREAD_STUB
   BOOL bEof = TRUE;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_EOF( pArea, &bEof );
   hb_retl( bEof );
}

HB_FUNC( FCOUNT )
{
   HB_THREAD_STUB
   USHORT uiFields = 0;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_FIELDCOUNT( pArea, &uiFields );
   hb_retni( uiFields );
}

HB_FUNC( FIELDDEC )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      USHORT uiIndex;

      if( ( uiIndex = hb_parni( 1 ) ) > 0 )
      {
         PHB_ITEM pItem = hb_itemNew( NULL );

         if( SELF_FIELDINFO( pArea, uiIndex, DBS_DEC, pItem ) == SUCCESS)
         {
            hb_itemReturnForward( pItem );
            hb_itemRelease( pItem );
            return;
         }
         hb_itemRelease( pItem );
      }
   }

   hb_retni(0);
}

HB_FUNC( FIELDGET )
{
   HB_THREAD_STUB
   /* this cannot be C stack item - memos can handle nested arrays
      and IO operation may cause GC activation in MT programs what
      can be the reason of random GPFs */
   PHB_ITEM pItem = hb_itemNew( NULL );
   USHORT uiField, uiFields;
   AREAP pArea = HB_CURRENT_WA;

   uiField = hb_parni( 1 );

   if( pArea && uiField )
   {
      if( SELF_FIELDCOUNT( pArea, &uiFields ) == SUCCESS &&
          uiField > 0 && uiField <= uiFields )
         SELF_GETVALUE( pArea, uiField, pItem );
   }

   hb_itemReturnForward( pItem );
   hb_itemRelease( pItem );
}

HB_FUNC( FIELDLEN )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      USHORT uiIndex;
      if( ( uiIndex = hb_parni( 1 ) ) > 0 )
      {
         PHB_ITEM pItem = hb_itemNew( NULL );

         if( SELF_FIELDINFO( pArea, uiIndex, DBS_LEN, pItem ) == SUCCESS )
         {
            hb_itemReturnForward( pItem );
            hb_itemRelease( pItem );
            return;
         }
         hb_itemRelease( pItem );
      }
   }

   hb_retni(0);
}

HB_FUNC( FIELDNAME )
{
   HB_THREAD_STUB
   char * szName;
   USHORT uiFields, uiIndex;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      uiIndex = hb_parni( 1 );
      if( SELF_FIELDCOUNT( pArea, &uiFields ) == SUCCESS &&
          uiIndex > 0 && uiIndex <= uiFields )
      {
         szName = ( char * ) hb_xgrab( pArea->uiMaxFieldNameLength + 1 );
         SELF_FIELDNAME( pArea, hb_parni( 1 ), szName );
         hb_retcAdopt( szName );
         return;
      }
      /* This is not Clipper compatible! - David G. Holm <dholm@jsd-llc.com>
       *
      hb_errRT_DBCMD( EG_ARG, EDBCMD_FIELDNAME_BADPARAMETER, NULL, "FIELDNAME" );
      */
   }
   hb_retc( "" ); /* Was NULL, which is not Clipper compatible! - David G. Holm <dholm@jsd-llc.com> */
}

HB_FUNC( FIELDPOS )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea && hb_parclen( 1 ) > 0 )
   {
      hb_retni( hb_rddFieldIndex( pArea, hb_parc( 1 ) ) );
   }
   else
   {
      hb_retni( 0 );
   }
}

HB_FUNC( FIELDPUT )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      USHORT uiIndex = hb_parni( 1 );
      if( uiIndex )
      {
         PHB_ITEM pItem = hb_param( 2, HB_IT_ANY );
         if( pItem && !HB_IS_NIL( pItem ) )
         {
            if( SELF_PUTVALUE( pArea, uiIndex, pItem ) == SUCCESS )
            {
               hb_itemReturn( pItem );
            }
         }
      }
   }
}

HB_FUNC( FIELDTYPE )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      USHORT uiIndex;

      if( ( uiIndex = hb_parni( 1 ) ) > 0 )
      {
         PHB_ITEM pItem = hb_itemNew( NULL );

         if( SELF_FIELDINFO( pArea, uiIndex, DBS_TYPE, pItem ) == SUCCESS )
         {
            hb_itemReturnForward( pItem );
            hb_itemRelease( pItem );
            return;
         }
         hb_itemRelease( pItem );
      }
   }

   hb_retc("");
}

HB_FUNC( FLOCK )
{
   HB_THREAD_STUB
   DBLOCKINFO dbLockInfo;
   AREAP pArea = HB_CURRENT_WA;

   dbLockInfo.fResult = FALSE;
   if( pArea )
   {
      dbLockInfo.itmRecID = NULL;
      dbLockInfo.uiMethod = DBLM_FILE;
      SELF_LOCK( pArea, &dbLockInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "FLOCK" );

   hb_retl( dbLockInfo.fResult );
}

HB_FUNC( FOUND )
{
   HB_THREAD_STUB
   BOOL bFound = FALSE;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_FOUND( pArea, &bFound );
   hb_retl( bFound );
}

HB_FUNC( HEADER )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( !pArea )
      hb_retni( 0 );
   else
   {
      PHB_ITEM pItem = hb_itemNew( NULL );
      SELF_INFO( pArea, DBI_GETHEADERSIZE, pItem );
      hb_itemReturnForward( pItem );
      hb_itemRelease( pItem );
   }
}

HB_FUNC( INDEXORD )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      DBORDERINFO pInfo;
      pInfo.itmResult = hb_itemPutNI( NULL, 0 );
      pInfo.itmOrder = NULL;
      pInfo.atomBagName = NULL;
      SELF_ORDINFO( pArea, DBOI_NUMBER, &pInfo );
      hb_retni( hb_itemGetNI( pInfo.itmResult ) );
      hb_itemRelease( pInfo.itmResult );
   }
   else
      hb_retni( 0 );
}

/* Same as RECCOUNT() */
HB_FUNC( LASTREC )
{
   HB_THREAD_STUB
   ULONG ulRecCount = 0;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_RECCOUNT( pArea, &ulRecCount );

   hb_retnl( ulRecCount );
}

HB_FUNC( LOCK )
{
   HB_THREAD_STUB
   DBLOCKINFO dbLockInfo;
   AREAP pArea = HB_CURRENT_WA;

   dbLockInfo.fResult = FALSE;
   if( pArea )
   {
      dbLockInfo.itmRecID = NULL;
      dbLockInfo.uiMethod = DBLM_EXCLUSIVE;
      SELF_LOCK( pArea, &dbLockInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "LOCK" );

   hb_retl( dbLockInfo.fResult );
}

HB_FUNC( LUPDATE )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      PHB_ITEM pItem = hb_itemNew( NULL );

      SELF_INFO( pArea, DBI_LASTUPDATE, pItem );
      hb_itemReturnForward( pItem );
      hb_itemRelease( pItem );
   }
   else
      hb_retds( "" );
}

HB_FUNC( NETERR )
{
   HB_THREAD_STUB

   hb_retl( s_bNetError );

   if( ISLOG( 1 ) )
      s_bNetError = hb_parl( 1 );
}

HB_FUNC( ORDBAGEXT )
{
   HB_THREAD_STUB
   DBORDERINFO pInfo;
   AREAP pArea = HB_CURRENT_WA;

   pInfo.itmOrder = NULL;
   pInfo.atomBagName = NULL;
   pInfo.itmResult = hb_itemPutC( NULL, "" );
   if( !pArea )
   {
      LPRDDNODE pRddNode;
      USHORT uiRddID;
      pRddNode = hb_rddFindNode( hb_rddDefaultDrv( NULL ), &uiRddID );
      if( pRddNode )
      {
         pArea = hb_rddNewAreaNode( pRddNode, uiRddID );
         if ( pArea )
         {
            SELF_ORDINFO( pArea, DBOI_BAGEXT, &pInfo );
            SELF_RELEASE( pArea );
         }
      }
   }
   else
   {
      SELF_ORDINFO( pArea, DBOI_BAGEXT, &pInfo );
   }
   hb_itemReturn( pInfo.itmResult );
   hb_itemRelease( pInfo.itmResult );
}

HB_FUNC( ORDBAGNAME )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      if ( ISNUM(1) || ISNIL(1) )
      {
         if ( hb_parni(1) == 0 || ISNIL(1) )          /* if NIL or ask for 0, use current order  */
            pOrderInfo.itmOrder  = NULL;
         else
            pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      }
      else
      {
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
         if( !pOrderInfo.itmOrder )
         {
            hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "ORDBAGNAME" );
            return;
         }
      }
      pOrderInfo.atomBagName = NULL;
      pOrderInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( pArea, DBOI_BAGNAME, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDBAGNAME" );
}

HB_FUNC( ORDCONDSET )
{
   HB_THREAD_STUB
   LPDBORDERCONDINFO lpdbOrdCondInfo;
   PHB_ITEM pItem;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      lpdbOrdCondInfo = ( LPDBORDERCONDINFO ) hb_xgrab( sizeof( DBORDERCONDINFO ) );
      lpdbOrdCondInfo->abFor = hb_parclen( 1 ) > 0 ?
                               ( BYTE * ) hb_strdup( hb_parc( 1 ) ) : NULL;
      pItem = hb_param( 2, HB_IT_BLOCK );
      lpdbOrdCondInfo->itmCobFor = pItem ? hb_itemNew( pItem ) : NULL;

      lpdbOrdCondInfo->fAll = !ISLOG( 3 ) || hb_parl( 3 );

      lpdbOrdCondInfo->abWhile = hb_parclen( 17 ) > 0 ?
                                 ( BYTE * ) hb_strdup( hb_parc( 17 ) ) : NULL;
      pItem = hb_param( 4, HB_IT_BLOCK );
      lpdbOrdCondInfo->itmCobWhile = pItem ? hb_itemNew( pItem ) : NULL;

      pItem = hb_param( 5, HB_IT_BLOCK );
      lpdbOrdCondInfo->itmCobEval = pItem ? hb_itemNew( pItem ) : NULL;

      lpdbOrdCondInfo->lStep         = hb_parnl( 6 );
      lpdbOrdCondInfo->itmStartRecID = ISNIL( 7 ) ? NULL : hb_itemNew( hb_param( 7, HB_IT_ANY ) );
      lpdbOrdCondInfo->lNextCount    = hb_parnl( 8 );
      lpdbOrdCondInfo->itmRecID      = ISNIL( 9 ) ? NULL : hb_itemNew( hb_param( 9, HB_IT_ANY ) );
      lpdbOrdCondInfo->fRest         = hb_parl( 10 );
      lpdbOrdCondInfo->fDescending   = hb_parl( 11 );
      /* 12th parameter is always nil in CL5.3, in CL5.2 it's compound flag */
      lpdbOrdCondInfo->fCompound     = hb_parl( 12 );
      lpdbOrdCondInfo->fAdditive     = hb_parl( 13 );
      lpdbOrdCondInfo->fUseCurrent   = hb_parl( 14 );
      lpdbOrdCondInfo->fCustom       = hb_parl( 15 );
      lpdbOrdCondInfo->fNoOptimize   = hb_parl( 16 );
      /* 18th parameter in[x]Harbour is MEMORY flag added by Alexander for
         DBFNTX, so far it was served in hacked way inside SELF_ORDSETCOND()
         so it was working only if this method was called from ORDCONDSET()
         function. I also do not like the idea that it was called MEMORY.
         It should be RDD decision how such index will be served on low
         level and it should be IMHO called TEMPORARY - if RDD wants then
         it can make it fully in memory or in temporary file which will
         be removed on index close operation */
      lpdbOrdCondInfo->fTemporary    = hb_parl( 18 );
      /* 19th parameter is CL5.2 USEFILTER parameter which means
         that RDD should respect SET FILTER and SET DELETE flag */
      lpdbOrdCondInfo->fUseFilter    = hb_parl( 19 );
      /* 20th parameter is xHarbour extenstion and informs RDD that
         index is not shared between other clients */
      lpdbOrdCondInfo->fExclusive    = hb_parl( 20 );

      if( lpdbOrdCondInfo->itmCobWhile )
         lpdbOrdCondInfo->fRest = TRUE;
      if( lpdbOrdCondInfo->lNextCount || lpdbOrdCondInfo->itmRecID ||
               lpdbOrdCondInfo->fRest || lpdbOrdCondInfo->fUseCurrent ||
          lpdbOrdCondInfo->fUseFilter )
         lpdbOrdCondInfo->fAll = FALSE;

      lpdbOrdCondInfo->fActive = !lpdbOrdCondInfo->fAll ||
               lpdbOrdCondInfo->abFor || lpdbOrdCondInfo->itmCobFor ||
               lpdbOrdCondInfo->abWhile || lpdbOrdCondInfo->itmCobWhile ||
               lpdbOrdCondInfo->fNoOptimize || lpdbOrdCondInfo->itmCobEval ||
               lpdbOrdCondInfo->fTemporary;

      lpdbOrdCondInfo->fScoped  = !lpdbOrdCondInfo->fAll;
      lpdbOrdCondInfo->lpvCargo = NULL;

      hb_retl( SELF_ORDSETCOND( pArea, lpdbOrdCondInfo ) == SUCCESS );
   }
   else
      hb_retl( FALSE );
}

HB_FUNC( ORDCREATE )
{
   HB_THREAD_STUB
   DBORDERCREATEINFO dbOrderInfo;
   DBCONSTRAINTINFO dbConstrInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      dbOrderInfo.abBagName = ( BYTE * ) hb_parcx( 1 );
      dbOrderInfo.atomBagName = ( BYTE * ) hb_parcx( 2 );
      dbOrderInfo.abExpr = hb_param( 3, HB_IT_STRING );
      if( ( ( dbOrderInfo.abBagName == NULL || strlen( ( char * ) dbOrderInfo.abBagName ) == 0 ) &&
            ( dbOrderInfo.atomBagName == NULL || strlen( ( char * ) dbOrderInfo.atomBagName ) == 0 ) ) ||
          !dbOrderInfo.abExpr )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "ORDCREATE" );
         return;
      }
      dbOrderInfo.itmCobExpr = hb_param( 4, HB_IT_BLOCK );
      if( ISLOG( 5 ) )
      {
         dbOrderInfo.fUnique = hb_parl( 5 );
      }
      else
      {
         dbOrderInfo.fUnique = hb_set.HB_SET_UNIQUE;
      }

      dbConstrInfo.abConstrName = ( BYTE * ) hb_parc( 6 );
      dbConstrInfo.abTargetName = ( BYTE * ) hb_parc( 7 );
      dbConstrInfo.itmRelationKey = hb_param( 8, HB_IT_ARRAY );
      if( dbConstrInfo.abConstrName && dbConstrInfo.abTargetName && dbConstrInfo.itmRelationKey )
      {
         dbConstrInfo.fEnabled = hb_parl( 9 );
         dbOrderInfo.lpdbConstraintInfo = &dbConstrInfo;
      }
      else
      {
         dbOrderInfo.lpdbConstraintInfo = NULL;
      }

      SELF_ORDCREATE( pArea, &dbOrderInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDCREATE" );
}

HB_FUNC( ORDBAGCLEAR )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pOrderInfo.itmOrder = NULL;
      pOrderInfo.atomBagName = hb_param( 1, HB_IT_STRING );
      if( !pOrderInfo.atomBagName )
         pOrderInfo.atomBagName = hb_param( 1, HB_IT_NUMERIC );
      hb_retl( SELF_ORDLSTDELETE( pArea, &pOrderInfo ) == SUCCESS );
   }
   else
      hb_retl( FALSE );
}

HB_FUNC( ORDDESTROY )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      if( !pOrderInfo.itmOrder )
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      hb_retl( SELF_ORDDESTROY( pArea, &pOrderInfo ) == SUCCESS );
   }
   else
      hb_retl( FALSE );
}

HB_FUNC( ORDFOR )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      if ( ISNUM(1) || ISNIL(1) )
      {
         if ( hb_parni(1) == 0 || ISNIL(1) )          /* if NIL or ask for 0, use current order  */
            pOrderInfo.itmOrder  = NULL;
         else
            pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      }
      else
      {
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
         if( !pOrderInfo.itmOrder )
         {
            hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "ORDFOR" );
            return;
         }
      }

      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      pOrderInfo.itmNewVal = hb_param( 3, HB_IT_STRING );
      pOrderInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( pArea, DBOI_CONDITION, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDFOR" );
}

HB_FUNC( ORDKEY )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      if ( ISNUM(1) || ISNIL(1) )
      {
         if ( hb_parni(1) == 0 || ISNIL(1) )          /* if NIL or ask for 0, use current order  */
            pOrderInfo.itmOrder = NULL;
         else
            pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      }
      else
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
      SELF_ORDINFO( pArea, DBOI_EXPRESSION, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDKEY" );
}

#ifdef HB_COMPAT_C53
HB_FUNC( ORDKEYCOUNT )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      if( !pOrderInfo.itmOrder )
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      /* Either or both may be NIL */

      pOrderInfo.itmResult = hb_itemPutNL( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_KEYCOUNT, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDKEYCOUNT" );

}

HB_FUNC( ORDKEYNO )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      if( !pOrderInfo.itmOrder )
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      /* Either or both may be NIL */
      pOrderInfo.itmNewVal = NULL;
      pOrderInfo.itmResult = hb_itemPutNL( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_POSITION, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDKEYNO" );
}

HB_FUNC( ORDKEYGOTO )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pOrderInfo.itmOrder = NULL;
      pOrderInfo.atomBagName = NULL;
      pOrderInfo.itmNewVal = hb_param( 1 , HB_IT_NUMERIC );
      pOrderInfo.itmResult = hb_itemPutL( NULL, FALSE );
      SELF_ORDINFO( pArea, DBOI_POSITION, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDKEYGOTO" );
}

HB_FUNC( ORDKEYRELPOS )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pOrderInfo.itmOrder = NULL;
      pOrderInfo.atomBagName = NULL;
      pOrderInfo.itmNewVal = hb_param( 1 , HB_IT_NUMERIC );
      pOrderInfo.itmResult = hb_itemPutNI( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_RELKEYPOS, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDKEYRELPOS" );
}

HB_FUNC( ORDFINDREC )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pOrderInfo.itmOrder = NULL;
      pOrderInfo.atomBagName = NULL;
      pOrderInfo.itmNewVal = hb_param( 1 , HB_IT_NUMERIC );
      pOrderInfo.itmResult = hb_itemPutL( NULL, FALSE );
      SELF_ORDINFO( pArea, hb_parl( 2 ) ? DBOI_FINDRECCONT :
                                          DBOI_FINDREC, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDKEYGOTO" );
}

HB_FUNC( ORDSKIPRAW )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;
   if( pArea )
      SELF_SKIPRAW( pArea, ISNUM( 1 ) ? hb_parnl( 1 ) : 1 );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDSKIPRAW" );
}


HB_FUNC( ORDSKIPUNIQUE )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pOrderInfo.itmOrder = NULL;
      pOrderInfo.atomBagName = NULL;
      pOrderInfo.itmNewVal = hb_param( 1, HB_IT_ANY );
      pOrderInfo.itmResult = hb_itemPutL( NULL, FALSE );
      SELF_ORDINFO( pArea, DBOI_SKIPUNIQUE, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDSKIPUNIQUE" );
}

HB_FUNC( ORDKEYVAL )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pOrderInfo.itmOrder = NULL;
      pOrderInfo.atomBagName = NULL;
      pOrderInfo.itmResult = hb_itemNew( NULL );
      SELF_ORDINFO( pArea, DBOI_KEYVAL, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDKEYVAL" );
}

HB_FUNC( ORDKEYADD )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      if( !pOrderInfo.itmOrder )
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      /* Either or both may be NIL */
      pOrderInfo.itmNewVal = hb_param( 3 , HB_IT_ANY );
      pOrderInfo.itmResult = hb_itemPutNL( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_KEYADD, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDKEYADD" );
}

HB_FUNC( ORDKEYDEL )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      if( !pOrderInfo.itmOrder )
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      /* Either or both may be NIL */
      pOrderInfo.itmNewVal = hb_param( 3 , HB_IT_ANY );
      pOrderInfo.itmResult = hb_itemPutNL( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_KEYDELETE, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDKEYDEL" );
}

HB_FUNC( ORDDESCEND )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      if( !pOrderInfo.itmOrder )
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      /* Either or both may be NIL */
      pOrderInfo.itmNewVal = hb_param( 3 , HB_IT_LOGICAL );
      pOrderInfo.itmResult = hb_itemPutL( NULL, FALSE );
      SELF_ORDINFO( pArea, DBOI_ISDESC, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDDESCEND" );
}

HB_FUNC( ORDISUNIQUE )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      if( !pOrderInfo.itmOrder )
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      /* HARBOUR extension: NewVal to set/reset unique flag */
      pOrderInfo.itmNewVal = hb_param( 3 , HB_IT_LOGICAL );
      pOrderInfo.itmResult = hb_itemPutL( NULL, FALSE );
      SELF_ORDINFO( pArea, DBOI_UNIQUE, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDISUNIQUE" );
}

HB_FUNC( ORDCUSTOM )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      if( !pOrderInfo.itmOrder )
         pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      /* Either or both may be NIL */
      pOrderInfo.itmNewVal = hb_param( 3 , HB_IT_LOGICAL );
      pOrderInfo.itmResult = hb_itemPutL( NULL, FALSE );
      SELF_ORDINFO( pArea, DBOI_CUSTOM, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDCUSTOM" );
}

HB_FUNC( ORDCOUNT )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pOrderInfo.itmOrder = NULL;
      pOrderInfo.atomBagName = hb_param( 1, HB_IT_STRING );
      pOrderInfo.itmResult = hb_itemPutNI( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_ORDERCOUNT, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDCOUNT" );
}

#endif

#ifdef HB_COMPAT_XPP
HB_FUNC( ORDWILDSEEK )
{
   HB_THREAD_STUB
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   BOOL fFound = FALSE;

   if( pArea )
   {
      char * szPatern = hb_parc( 1 );

      if( szPatern )
      {
         BOOL fCont = hb_parl( 2 ), fBack = hb_parl( 3 );
         DBORDERINFO OrderInfo;

         memset( &OrderInfo, 0, sizeof( DBORDERINFO ) );
         OrderInfo.itmResult = hb_itemNew( NULL );

         if( !fCont )
         {
            char * szKey;

            if( fBack )
               SELF_GOBOTTOM( pArea );
            else
               SELF_GOTOP( pArea );

            SELF_ORDINFO( pArea, DBOI_KEYVAL, &OrderInfo );
            szKey = hb_itemGetCPtr( OrderInfo.itmResult );

            fFound = hb_strMatchWild( szKey, szPatern );
         }
         if( !fFound )
         {
            OrderInfo.itmNewVal = hb_param( 1, HB_IT_STRING );
            SELF_ORDINFO( pArea, fBack ? DBOI_SKIPWILDBACK : DBOI_SKIPWILD,
                          &OrderInfo );
            fFound = hb_itemGetL( OrderInfo.itmResult );
         }
         hb_itemRelease( OrderInfo.itmResult );
      }
      else
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_DBFILEPUTBADPARAMETER, NULL, "ORDWILDSEEK" );
      }
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDWILDSEEK" );
   }
   hb_retl( fFound );
}
#endif

HB_FUNC( ORDLISTADD )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      BOOL bFirst;
      /*  determine if there are existing orders; if not, this becomes the controlling order
      */
      pOrderInfo.itmOrder = NULL;
      pOrderInfo.atomBagName = NULL;
      pOrderInfo.itmResult = hb_itemPutNI( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_ORDERCOUNT, &pOrderInfo );
      bFirst = HB_IS_NUMERIC( pOrderInfo.itmResult ) &&
               hb_itemGetNI( pOrderInfo.itmResult ) == 0;

      pOrderInfo.atomBagName = hb_param( 1, HB_IT_STRING );
      pOrderInfo.itmOrder  = hb_param( 2, HB_IT_STRING );
      if( !pOrderInfo.atomBagName )
      {
         if ( hb_parinfo(1) != HB_IT_NIL )
            hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "ORDLISTADD" );
         else
            hb_itemRelease( pOrderInfo.itmResult );
         return;
      }

      if( SELF_ORDLSTADD( pArea, &pOrderInfo ) == SUCCESS )
      {
         if( bFirst )        /* set as controlling order and go top */
         {
            pOrderInfo.itmOrder = hb_itemPutNI( NULL, 1 );
            SELF_ORDLSTFOCUS( pArea, &pOrderInfo );
            hb_itemRelease( pOrderInfo.itmOrder );
            SELF_GOTOP( pArea );
         }
      }
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDLISTADD" );

}

HB_FUNC( ORDLISTCLEAR )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_ORDLSTCLEAR( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDLISTCLEAR" );
}

HB_FUNC( ORDLISTREBUILD )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_ORDLSTREBUILD( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDLISTREBUILD" );
}

HB_FUNC( ORDNAME )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      if ( ISNUM(1) || ISNIL(1) )
      {
         if ( hb_parni(1) == 0 || ISNIL(1) )          /* if NIL or ask for 0, use current order  */
            pOrderInfo.itmOrder  = NULL;
         else
            pOrderInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      }
      else
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "ORDNAME" );
         return;
      }

      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      pOrderInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDINFO( pArea, DBOI_NAME, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDNAME" );
}

HB_FUNC( ORDNUMBER )
{
   HB_THREAD_STUB
   DBORDERINFO pOrderInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      if( !pOrderInfo.itmOrder && ! ISNIL(1) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "ORDNUMBER" );
         return;
      }
      pOrderInfo.itmResult = hb_itemPutNI( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_NUMBER, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDNUMBER" );
}

HB_FUNC( ORDSETFOCUS )
{
   HB_THREAD_STUB
   DBORDERINFO pInfo;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      if( !pInfo.itmOrder )
         pInfo.itmOrder = hb_param( 1, HB_IT_NUMERIC );
      pInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      pInfo.itmResult = hb_itemPutC( NULL, "" );
      SELF_ORDLSTFOCUS( pArea, &pInfo );
      hb_itemReturn( pInfo.itmResult );
      hb_itemRelease( pInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDSETFOCUS" );
}

HB_FUNC( RDDLIST )
{
   USHORT uiType, uiCount, uiRdds = 0, uiIndex = 0;
   PHB_ITEM pRddArray = hb_itemNew( NULL );

   uiType = hb_parni( 1 );       /* 0 all types of RDD's */

   for( uiCount = 0; uiCount < s_uiRddMax; ++uiCount )
   {
      if( ( uiType == 0 ) || ( s_RddList[ uiCount ]->uiType == uiType ) )
         ++ uiRdds;
   }
   hb_arrayNew( pRddArray, uiRdds );
   for( uiCount = 0; uiRdds && uiCount < s_uiRddMax; ++uiCount )
   {
      if( ( uiType == 0 ) || ( s_RddList[ uiCount ]->uiType == uiType ) )
      {
         hb_itemPutC( hb_arrayGetItemPtr( pRddArray, ++uiIndex ), s_RddList[ uiCount ]->szName );
      }
   }
   hb_itemReturnForward( pRddArray );
   hb_itemRelease( pRddArray );
}

HB_FUNC( RDDNAME )
{
   HB_THREAD_STUB
   char * pBuffer;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      pBuffer = ( char * ) hb_xgrab( HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 );
      pBuffer[ 0 ] = '\0';
      SELF_SYSNAME( pArea, ( BYTE * ) pBuffer );
      hb_retcAdopt( pBuffer );
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "RDDNAME" );
      hb_retc( NULL );
   }
}

HB_FUNC( RDDREGISTER )
{
   USHORT uiLen;
   char szDriver[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 ];

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
      {
         hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );
      }
   }
}

/* Same as LASTREC() */
HB_FUNC( RECCOUNT )
{
   HB_FUNCNAME( LASTREC )();
}

HB_FUNC( RECNO )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;
   PHB_ITEM pRecNo = hb_itemPutNL( NULL, 0 );

   if( pArea )
   {
      SELF_RECID( pArea, pRecNo );
   }
   hb_itemReturnForward( pRecNo );
   hb_itemRelease( pRecNo );
}

HB_FUNC( RECSIZE )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      PHB_ITEM pItem = hb_itemNew( NULL );
      SELF_INFO( pArea, DBI_GETRECSIZE, pItem );
      hb_itemReturnForward( pItem );
      hb_itemRelease( pItem );
   }
   else
      hb_retni( 0 );
}

HB_FUNC( RLOCK )
{
   HB_THREAD_STUB
   DBLOCKINFO dbLockInfo;
   AREAP pArea = HB_CURRENT_WA;

   dbLockInfo.fResult = FALSE;
   if( pArea )
   {
      dbLockInfo.itmRecID = NULL;
      dbLockInfo.uiMethod = DBLM_EXCLUSIVE;
      SELF_LOCK( pArea, &dbLockInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "RLOCK" );

   hb_retl( dbLockInfo.fResult );
}

HB_FUNC( SELECT )
{
   HB_THREAD_STUB
   if( hb_parinfo( 0 ) == 0 )
   {
      hb_retni( hb_rddGetCurrentWorkAreaNumber() );
   }
   else
   {
      char * szAlias = hb_parc( 1 );
      int iArea = 0;

      if( szAlias )
      {
#ifdef HB_C52_STRICT
         /*
          * I do not like this Clipper behavior, in some constructions
          * programmer may use "<aliasNum>" in some others not. [Druzus]
          */
         if( hb_rddVerifyAliasName( szAlias ) == SUCCESS )
#endif
            hb_rddGetAliasNumber( szAlias, &iArea );
      }
      hb_retni( iArea );
   }
}

HB_FUNC( USED )
{
   HB_THREAD_STUB
   hb_retl( HB_CURRENT_WA != NULL );
}

/* NOTE: Same as dbSetDriver() and rddSetDefault(), but doesn't
         throw any error if the driver doesn't exist, this is
         required in the RDDSYS INIT function, since it's not guaranteed
         that the RDD is already registered at that point. [vszakats] */

HB_FUNC( __RDDSETDEFAULT )
{
   HB_THREAD_STUB
   USHORT uiLen;

   hb_retc( s_szDefDriver );

   uiLen = ( USHORT ) hb_parclen( 1 );

   if( uiLen > 0 )
   {
      if( uiLen > HARBOUR_MAX_RDD_DRIVERNAME_LENGTH )
      {
         uiLen = HARBOUR_MAX_RDD_DRIVERNAME_LENGTH;
      }
      hb_strncpyUpper( s_szDefDriver, hb_parc( 1 ), uiLen );
   }
}

HB_FUNC( RDDSETDEFAULT )
{
   HB_THREAD_STUB

   hb_retc( hb_rddDefaultDrv( NULL ) );

   if( hb_parclen( 1 ) > 0 )
   {
      if( ! hb_rddDefaultDrv( hb_parc( 1 ) ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, "RDDSETDEFAULT" );
         return;
      }
   }
}

HB_FUNC( DBSETDRIVER )
{
   HB_THREAD_STUB

   hb_retc( hb_rddDefaultDrv( NULL ) );

   if( hb_parclen( 1 ) > 0 )
   {
      if( ! hb_rddDefaultDrv( hb_parc( 1 ) ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, "DBSETDRIVER" );
         return;
      }
   }
}

HB_FUNC( ORDSCOPE )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if ( pArea )
   {
      DBORDERINFO pInfo;
      USHORT uiAction;
      int iScope = hb_parni( 1 );

      pInfo.itmOrder = NULL;
      pInfo.atomBagName = NULL;
      pInfo.itmResult = hb_itemNew( NULL );
      pInfo.itmNewVal = NULL;
      if ( iScope == 2 )
      {
         if ( hb_pcount() > 1 && !ISNIL( 2 ) )
         {
            uiAction = DBOI_SCOPESET;
            pInfo.itmNewVal = hb_param( 2, HB_IT_ANY);
         }
         else
            uiAction = DBOI_SCOPECLEAR;
      }
      else
      {
         uiAction = ( iScope == 0 ) ? DBOI_SCOPETOP : DBOI_SCOPEBOTTOM;
         if( hb_pcount() > 1 )
         {
            if( ISNIL( 2 ) )
               uiAction = ( iScope == 0 ) ? DBOI_SCOPETOPCLEAR : DBOI_SCOPEBOTTOMCLEAR;
            else
               pInfo.itmNewVal = hb_param( 2, HB_IT_ANY);
         }
      }
      SELF_ORDINFO( pArea, uiAction, &pInfo );
      hb_itemReturn( pInfo.itmResult );
      hb_itemRelease( pInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDSCOPE" );
}

HB_FUNC( DBRELATION )  /* (<nRelation>) --> cLinkExp */
{
   HB_THREAD_STUB
   char szExprBuff[ HARBOUR_MAX_RDD_RELTEXT_LENGTH + 1 ];
   AREAP pArea = HB_CURRENT_WA;

   szExprBuff[ 0 ] = 0;
   if( pArea )
      SELF_RELTEXT( pArea, hb_parni(1), szExprBuff ) ;

   hb_retc( szExprBuff );
}

HB_FUNC( DBRSELECT )  /* (<nRelation>) --> nWorkArea */
{
   HB_THREAD_STUB
   USHORT uiWorkArea = 0;
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_RELAREA( pArea, hb_parni(1), &uiWorkArea );

   hb_retni( uiWorkArea );
}

HB_FUNC( DBCLEARRELATION )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
      SELF_CLEARREL( pArea );
}

HB_FUNC( DBSETRELATION )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      DBRELINFO dbRelations;
      AREAP pChildArea;
      USHORT uiChildArea;
      char * szAlias = NULL;

      if( hb_pcount() < 2 || ( !( hb_parinfo( 1 ) & HB_IT_NUMERIC ) && ( hb_parinfo( 1 ) != HB_IT_STRING ) ) || !( ISNIL( 4 ) || ISLOG( 4 ) )  )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "DBSETRELATION" );
         return;
      }

      if( hb_parinfo( 1 ) & HB_IT_NUMERIC )
      {
         uiChildArea = hb_parni( 1 );
      }
      else
      {
         USHORT uiArea = hb_rddGetCurrentWorkAreaNumber();

         hb_rddSelectWorkAreaAlias( hb_parcx( 1 ) );
         if( hb_vmRequestQuery() )
         {
            return;
         }
         uiChildArea = hb_rddGetCurrentWorkAreaNumber();
         hb_rddSelectWorkAreaNumber( uiArea );
      }

      pChildArea = HB_GET_WA( uiChildArea );

      if( !pChildArea )
      {
         hb_errRT_BASE( EG_NOALIAS, EDBCMD_NOALIAS, NULL, szAlias, 0 );
         return;
      }

      dbRelations.lpaChild = pChildArea;
      dbRelations.itmCobExpr = hb_itemNew( hb_param( 2, HB_IT_BLOCK ) );
      dbRelations.abKey = hb_itemNew( hb_param( 3, HB_IT_STRING ) );
      dbRelations.isScoped = hb_parl( 4 );
      dbRelations.isOptimized = FALSE;
      dbRelations.lpdbriNext = NULL;

      SELF_SETREL( pArea, &dbRelations );
   }
   else
   {
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBSETRELATION" );
   }
}

HB_FUNC( __DBARRANGE )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      USHORT uiNewArea, uiCount, uiDest;
      ULONG ulSize;
      char * szFieldLine, * szPos;
      PHB_ITEM pStruct, pFields;
      DBSORTINFO dbSortInfo;

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
            dbSortInfo.dbtri.lpTransItems = ( LPDBTRANSITEM )
                                            hb_xgrab( dbSortInfo.dbtri.uiItemCount *
                                                      sizeof( DBTRANSITEM ) );
            for( uiCount = 0; uiCount < dbSortInfo.dbtri.uiItemCount; ++uiCount )
            {
               pFields = hb_arrayGetItemPtr( pStruct, uiCount + 1 );
               if( HB_IS_ARRAY( pFields ) && hb_arrayLen( pFields ) > 0 )
               {
                  dbSortInfo.dbtri.lpTransItems[ uiCount ].uiSource =
                  dbSortInfo.dbtri.lpTransItems[ uiCount ].uiDest =
                     hb_rddFieldIndex( pArea, hb_arrayGetCPtr( pFields, 1 ) );
               }
               else
               {
                  hb_xfree( dbSortInfo.dbtri.lpTransItems );
                  dbSortInfo.dbtri.lpTransItems = NULL;
                  dbSortInfo.dbtri.uiItemCount = 0;
                  break;
               }
            }
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
      dbSortInfo.dbtri.dbsci.itmRecID = ISNIL( 6 ) ? NULL : hb_param( 6, HB_IT_ANY );
      dbSortInfo.dbtri.dbsci.fRest = hb_param( 7, HB_IT_LOGICAL );
      dbSortInfo.dbtri.dbsci.fIgnoreFilter =
      dbSortInfo.dbtri.dbsci.fLast =
      dbSortInfo.dbtri.dbsci.fIgnoreDuplicates =
      dbSortInfo.dbtri.dbsci.fBackword = FALSE;
      dbSortInfo.dbtri.dbsci.fIncludeDeleted = TRUE;

      pFields = hb_param( 8, HB_IT_ARRAY );
      dbSortInfo.uiItemCount = pFields ? ( USHORT ) hb_arrayLen( pFields ) : 0;
      if( dbSortInfo.uiItemCount > 0 )
      {
         dbSortInfo.lpdbsItem = ( LPDBSORTITEM ) hb_xgrab( dbSortInfo.uiItemCount * sizeof( DBSORTITEM ) );
         ulSize = 0;
         for( uiCount = 1; uiCount <= dbSortInfo.uiItemCount; ++uiCount )
         {
            ULONG ulLine = hb_arrayGetCLen( pFields, uiCount );
            if( ulLine > ulSize )
               ulSize = ulLine;
         }
         szFieldLine = ( char * ) hb_xgrab( ulSize + 1 );
         for( uiCount = uiDest = 0; uiCount < dbSortInfo.uiItemCount; ++uiCount )
         {
            dbSortInfo.lpdbsItem[ uiDest ].uiFlags = 0;
            hb_strncpyUpper( szFieldLine, hb_arrayGetCPtr( pFields, uiCount + 1 ),
                             hb_arrayGetCLen( pFields, uiCount + 1 ) );
            szPos = strchr( szFieldLine, '/' );
            if( szPos )
            {
               *szPos++ = 0;
               if( strchr( szPos, 'D' ) > strchr( szPos, 'A' ) )
                  dbSortInfo.lpdbsItem[ uiDest ].uiFlags |= SF_DESCEND;
               else
                  dbSortInfo.lpdbsItem[ uiDest ].uiFlags |= SF_ASCEND;
               if( strchr( szPos, 'C' ) != NULL )
                  dbSortInfo.lpdbsItem[ uiDest ].uiFlags |= SF_CASE;
            }
            else
            {
               dbSortInfo.lpdbsItem[ uiDest ].uiFlags |= SF_ASCEND;
            }

            dbSortInfo.lpdbsItem[ uiDest ].uiField = hb_rddFieldExpIndex( pArea, szFieldLine );

            /* Field found */
            if( dbSortInfo.lpdbsItem[ uiDest ].uiField != 0 )
            {
               ++uiDest;
            }
         }
         dbSortInfo.uiItemCount = uiDest;
         hb_xfree( szFieldLine );
      }

      dbSortInfo.dbtri.lpaSource = pArea;
      dbSortInfo.dbtri.lpaDest = HB_GET_WA( uiNewArea );

      if( dbSortInfo.uiItemCount == 0 )
         SELF_TRANS( pArea, &dbSortInfo.dbtri );
      else
         SELF_SORT( pArea, &dbSortInfo );

      /* Free items */
      if( dbSortInfo.lpdbsItem )
         hb_xfree( dbSortInfo.lpdbsItem );
      if( dbSortInfo.dbtri.lpTransItems > 0 )
         hb_xfree( dbSortInfo.dbtri.lpTransItems );
   }
}

#ifdef HB_COMPAT_C53

HB_FUNC( DBINFO )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      PHB_ITEM pIndex;

      pIndex = hb_param( 1, HB_IT_NUMERIC );
      if( pIndex )
      {
         PHB_ITEM pInfo = hb_itemNew( hb_param( 2, HB_IT_ANY ) );

         SELF_INFO( pArea, hb_itemGetNI( pIndex ), pInfo );
         hb_itemReturn( pInfo );
         hb_itemRelease( pInfo );
      }
      else
         hb_errRT_DBCMD( EG_ARG, EDBCMD_DBINFOBADPARAMETER, NULL, "DBINFO" );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBINFO" );
}

HB_FUNC( DBORDERINFO )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      PHB_ITEM pType;
      DBORDERINFO pOrderInfo;

      pType = hb_param( 1 , HB_IT_NUMERIC );
      if( pType )
      {
         /* atomBagName may be NIL */
         pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
         if( !pOrderInfo.atomBagName )
            pOrderInfo.atomBagName = hb_param( 2, HB_IT_NUMERIC );

         pOrderInfo.itmOrder = hb_param( 3, HB_IT_STRING );
         if( !pOrderInfo.itmOrder )
            pOrderInfo.itmOrder = hb_param( 3, HB_IT_NUMERIC );

         pOrderInfo.itmNewVal = hb_param( 4 , HB_IT_ANY );
         pOrderInfo.itmResult = hb_itemNew( NULL );
         SELF_ORDINFO( pArea, hb_itemGetNI( pType ), &pOrderInfo );
         hb_itemReturn( pOrderInfo.itmResult );
         hb_itemRelease( pOrderInfo.itmResult );
      }
      else
         hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, "DBORDERINFO" );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBORDERINFO" );
}

HB_FUNC( DBFIELDINFO )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      USHORT uiFields, uiIndex;
      PHB_ITEM pType;

      pType = hb_param( 1 , HB_IT_NUMERIC );
      uiIndex = hb_parni( 2 );
      if( pType && SELF_FIELDCOUNT( pArea, &uiFields ) == SUCCESS &&
          uiIndex > 0 && uiIndex <= uiFields )
      {
         PHB_ITEM pInfo = hb_itemNew( hb_param( 3, HB_IT_ANY ) );

         SELF_FIELDINFO( pArea, uiIndex, hb_itemGetNI( pType ), pInfo );
         hb_itemReturn( pInfo );
         hb_itemRelease( pInfo );
      }
      else
         hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, "DBFIELDINFO" );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBFIELDINFO" );
}

HB_FUNC( DBRECORDINFO )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      PHB_ITEM pType, pRecNo;

      pType = hb_param( 1, HB_IT_NUMERIC );
      pRecNo = hb_param( 2, HB_IT_ANY );
      if( pType )
      {
         PHB_ITEM pInfo = hb_itemNew( hb_param( 3, HB_IT_ANY ) );

         SELF_RECINFO( pArea, pRecNo, hb_itemGetNI( pType ), pInfo );
         hb_itemReturn( pInfo );
         hb_itemRelease( pInfo );
      }
      else
         hb_errRT_DBCMD( EG_ARG, EDBCMD_INFOBADPARAMETER, NULL, "DBRECORDINFO" );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBRECORDINFO" );
}

HB_FUNC( DBFILEGET )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      USHORT uiFields, uiIndex;
      PHB_ITEM pMode;
      char * szField = hb_parc( 1 );

      if( szField )
         uiIndex = hb_rddFieldIndex( pArea, szField );
      else
         uiIndex = hb_parni( 1 );

      pMode = hb_param( 3, HB_IT_NUMERIC );
      if( uiIndex > 0 && pMode && hb_parclen( 2 ) > 0 &&
          SELF_FIELDCOUNT( pArea, &uiFields ) == SUCCESS &&
          uiIndex <= uiFields )
      {
         hb_retl( SELF_GETVALUEFILE( pArea, uiIndex, hb_parc( 2 ),
                                     hb_itemGetNI( pMode ) ) == SUCCESS );
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
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      USHORT uiFields, uiIndex;
      char * szField = hb_parc( 1 );

      if( szField )
         uiIndex = hb_rddFieldIndex( pArea, szField );
      else
         uiIndex = hb_parni( 1 );
      if( uiIndex > 0 && hb_parclen( 2 ) > 0 &&
          SELF_FIELDCOUNT( pArea, &uiFields ) == SUCCESS &&
          uiIndex <= uiFields )
      {
         hb_retl( SELF_PUTVALUEFILE( pArea, uiIndex, hb_parc( 2 ),
                                     hb_parni( 3 ) ) == SUCCESS );
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
/* here we have the NEW RDD level functions DBDROP, DBEXISTS, RDDINFO */
HB_FUNC( DBDROP )
{
   HB_THREAD_STUB
   LPRDDNODE  pRDDNode;
   USHORT     uiRddID;
   char      *szDriver;

   szDriver = hb_parc( 3 );
   if( !szDriver ) /* no VIA RDD parameter, use default */
   {
      szDriver = hb_rddDefaultDrv( NULL );
   }

   pRDDNode = hb_rddFindNode( szDriver, &uiRddID );  /* find the RDDNODE */

   if ( !pRDDNode )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBDROP" );
      return;
   }

   hb_retl( SELF_DROP( pRDDNode, hb_param( 1, HB_IT_STRING ),
                                 hb_param( 2, HB_IT_STRING ) ) == SUCCESS );
}

HB_FUNC( DBEXISTS )
{
   HB_THREAD_STUB
   LPRDDNODE  pRDDNode;
   USHORT     uiRddID;
   char * szDriver;

   szDriver = hb_parc( 3 );
   if( !szDriver ) /* no VIA RDD parameter, use default */
   {
      szDriver = hb_rddDefaultDrv( NULL );
   }

   pRDDNode = hb_rddFindNode( szDriver, &uiRddID );  // find the RDD

   if ( !pRDDNode )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBEXISTS" );
      return;
   }

   hb_retl( SELF_EXISTS( pRDDNode, hb_param( 1, HB_IT_STRING ),
                                   hb_param( 2, HB_IT_STRING ) ) == SUCCESS );
}

HB_FUNC( RDDINFO )
{
   LPRDDNODE  pRDDNode;
   USHORT     uiRddID;
   ULONG      ulConnection;
   PHB_ITEM   pIndex, pParam;
   char      *szDriver;

   szDriver = hb_parc( 3 );
   if( !szDriver ) /* no VIA RDD parameter, use default */
   {
      szDriver = hb_rddDefaultDrv( NULL );
   }
   ulConnection = hb_parnl( 4 );

   pRDDNode = hb_rddFindNode( szDriver, &uiRddID );  /* find the RDDNODE */
   pIndex = hb_param( 1, HB_IT_NUMERIC );
   pParam = hb_param( 2, HB_IT_ANY );

   if( pRDDNode && pIndex )
   {
      PHB_ITEM pInfo = hb_itemNew( pParam );
      SELF_RDDINFO( pRDDNode, hb_itemGetNI( pIndex ), ulConnection, pInfo );
      hb_itemReturn( pInfo );
      hb_itemRelease( pInfo );
   }
   else
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "RDDINFO" );
   }
}


static ERRCODE hb_dbTransStruct( AREAP lpaSource, AREAP lpaDest,
                                 LPDBTRANSINFO lpdbTransInfo,
                                 PHB_ITEM *pStruct, PHB_ITEM pFields )
{
   USHORT uiFields, uiSize, uiCount, uiPosSrc, uiPosDst, uiSizeSrc, uiSizeDst;
   ERRCODE errCode;
   char * szField;
   BOOL fAll;

   errCode = SELF_FIELDCOUNT( lpaSource, &uiSizeSrc );
   if( errCode != SUCCESS )
      return errCode;

   if( lpaDest )
   {
      errCode = SELF_FIELDCOUNT( lpaDest, &uiSizeDst );
      if( errCode != SUCCESS )
         return errCode;
      uiSize = HB_MIN( uiSizeDst, uiSizeSrc );
   }
   else
   {
      uiSize = uiSizeDst = uiSizeSrc;
   }

   if( !uiSize )
      return FAILURE;
   if( hb_itemType( pFields ) & HB_IT_ARRAY )
   {
      uiFields = ( USHORT ) hb_arrayLen( pFields );
      if( uiFields )
         uiSize = uiFields;
   }
   else
      uiFields = 0;

   fAll = ( uiSizeDst == uiSizeSrc );

   lpdbTransInfo->lpaSource    = lpaSource;
   lpdbTransInfo->lpaDest      = lpaDest;
   lpdbTransInfo->lpTransItems = ( LPDBTRANSITEM )
                                    hb_xgrab( uiSize * sizeof( DBTRANSITEM ) );

   if( !lpaDest )
   {
      *pStruct = hb_itemNew( NULL );
      hb_arrayNew( *pStruct, 0 );
   }

   if( uiFields == 0 )
   {
      if( lpaDest )
      {
         PHB_ITEM pItem = hb_itemNew( NULL );
         uiSize = 0;
         for( uiCount = 1; uiCount <= uiSizeSrc; ++uiCount )
         {
            SELF_FIELDINFO( lpaSource, uiCount, DBS_NAME, pItem );
            szField = hb_itemGetCPtr( pItem );
            uiPosDst = hb_rddFieldExpIndex( lpaDest, szField );
            if( uiPosDst != uiCount )
               fAll = FALSE;
            if( uiPosDst )
            {
               lpdbTransInfo->lpTransItems[ uiSize ].uiSource = uiCount;
               lpdbTransInfo->lpTransItems[ uiSize++ ].uiDest = uiPosDst;
            }
         }
         hb_itemRelease( pItem );
      }
      else
      {
         hb_tblStructure( lpaSource, *pStruct );
         uiSize = ( USHORT ) hb_arrayLen( *pStruct );
         for( uiCount = 0; uiCount < uiSize; ++uiCount )
         {
            lpdbTransInfo->lpTransItems[ uiCount ].uiSource =
            lpdbTransInfo->lpTransItems[ uiCount ].uiDest = uiCount + 1;
         }
      }
   }
   else
   {
      uiSize = 0;
      for( uiCount = 1; uiCount <= uiFields; ++uiCount )
      {
         szField = hb_arrayGetCPtr( pFields, uiCount );
         if( szField )
         {
            uiPosSrc = hb_rddFieldExpIndex( lpaSource, szField );
            if( !uiPosSrc )
               continue;
            if( lpaDest )
               uiPosDst = hb_rddFieldExpIndex( lpaDest, szField );
            else
               uiPosDst = uiSize + 1;
            if( uiPosDst )
            {
               if( uiPosSrc != uiPosDst )
                  fAll = FALSE;
               lpdbTransInfo->lpTransItems[ uiSize ].uiSource = uiPosSrc;
               lpdbTransInfo->lpTransItems[ uiSize++ ].uiDest = uiPosDst;
               if( !lpaDest )
               {
                  hb_arraySize( *pStruct, uiSize );
                  hb_fldStructure( lpaSource, uiPosSrc,
                                   hb_arrayGetItemPtr( *pStruct, uiSize ) );
               }
            }
         }
      }
   }

   if( uiSize != uiSizeSrc )
      fAll = FALSE;

   if( fAll && lpaDest )
   {
      PHB_ITEM pSrcItm = hb_itemNew( NULL ),
               pDstItm = hb_itemNew( NULL );
      /*
       * if fAll is TRUE here then it means that all fields are included
       * and they are on the same positions in both tables, so now check
       * if their types and sizes are also equal
       */
      for( uiCount = 1; uiCount <= uiSize; ++uiCount )
      {
         SELF_FIELDINFO( lpaSource, uiCount, DBS_TYPE, pSrcItm );
         SELF_FIELDINFO( lpaDest,   uiCount, DBS_TYPE, pDstItm );
         if( hb_stricmp( hb_itemGetCPtr( pSrcItm ),
                         hb_itemGetCPtr( pDstItm ) ) != 0 )
         {
            fAll = FALSE;
            break;
         }
         SELF_FIELDINFO( lpaSource, uiCount, DBS_LEN, pSrcItm );
         SELF_FIELDINFO( lpaDest,   uiCount, DBS_LEN, pDstItm );
         if( hb_itemGetNL( pSrcItm ) != hb_itemGetNL( pDstItm ) )
         {
            fAll = FALSE;
            break;
         }
         SELF_FIELDINFO( lpaSource, uiCount, DBS_DEC, pSrcItm );
         SELF_FIELDINFO( lpaDest,   uiCount, DBS_DEC, pDstItm );
         if( hb_itemGetNL( pSrcItm ) != hb_itemGetNL( pDstItm ) )
         {
            fAll = FALSE;
            break;
         }
      }
      hb_itemRelease( pSrcItm );
      hb_itemRelease( pDstItm );
   }

   lpdbTransInfo->uiFlags = fAll ? DBTF_MATCH : 0;
   lpdbTransInfo->uiItemCount = uiSize;

   return uiSize ? SUCCESS : FAILURE;
}

static ERRCODE hb_rddTransRecords( AREAP pArea,
                                   char *szFileName, char *szDriver,
                                   ULONG ulConnection,
                                   PHB_ITEM pFields, BOOL fExport,
                                   PHB_ITEM pCobFor, PHB_ITEM pStrFor,
                                   PHB_ITEM pCobWhile, PHB_ITEM pStrWhile,
                                   PHB_ITEM pNext, PHB_ITEM pRecID,
                                   PHB_ITEM pRest,
                                   char *szCpId )
{
   AREAP lpaSource, lpaDest, lpaClose = NULL;
   DBTRANSINFO dbTransInfo;
   USHORT uiPrevArea;
   ERRCODE errCode;

   memset( &dbTransInfo, 0, sizeof( DBTRANSINFO ) );
   uiPrevArea = hb_rddGetCurrentWorkAreaNumber();

   if( fExport )
   {
      PHB_ITEM pStruct = NULL;

      lpaSource = pArea;
      errCode = hb_dbTransStruct( lpaSource, NULL, &dbTransInfo,
                                  &pStruct, pFields );
      if( errCode == SUCCESS )
      {
         errCode = hb_rddCreateTable( szFileName, pStruct, szDriver,
                                      TRUE, 0, "", szCpId, ulConnection );
         if( errCode == SUCCESS )
         {
            dbTransInfo.lpaDest = lpaClose = lpaDest =
                                 ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
         }
      }
      if( pStruct )
         hb_itemRelease( pStruct );
   }
   else
   {
      lpaDest = pArea;
      errCode = hb_rddOpenTable( szFileName, szDriver, 0, "", TRUE, TRUE,
                                 szCpId, ulConnection );
      if( errCode == SUCCESS )
      {
         lpaClose = lpaSource = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
         errCode = hb_dbTransStruct( lpaSource, lpaDest, &dbTransInfo,
                                     NULL, pFields );
      }
   }

   if( errCode == SUCCESS )
   {
      hb_rddSelectWorkAreaNumber( dbTransInfo.lpaSource->uiArea );

      dbTransInfo.dbsci.itmCobFor   = pCobFor;
      dbTransInfo.dbsci.lpstrFor    = pStrFor;
      dbTransInfo.dbsci.itmCobWhile = pCobWhile;
      dbTransInfo.dbsci.lpstrWhile  = pStrWhile;
      dbTransInfo.dbsci.lNext       = pNext;
      dbTransInfo.dbsci.itmRecID    = pRecID;
      dbTransInfo.dbsci.fRest       = pRest;

      dbTransInfo.dbsci.fIgnoreFilter     = TRUE;
      dbTransInfo.dbsci.fIncludeDeleted   = TRUE;
      dbTransInfo.dbsci.fLast             = FALSE;
      dbTransInfo.dbsci.fIgnoreDuplicates = FALSE;
      dbTransInfo.dbsci.fBackword         = FALSE;

      errCode = SELF_TRANS( dbTransInfo.lpaSource, &dbTransInfo );
   }

   if( dbTransInfo.lpTransItems )
      hb_xfree( dbTransInfo.lpTransItems );
   if( lpaClose )
   {
      hb_rddSelectWorkAreaNumber( lpaClose->uiArea );
      hb_rddReleaseCurrentArea();
   }
   hb_rddSelectWorkAreaNumber( uiPrevArea );

   return errCode;
}

HB_FUNC( __DBAPP )
{
   if( ISCHAR( 1 ) )
   {
      AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
      if( pArea )
      {
         hb_rddTransRecords( pArea,
               hb_parc( 1 ),                  /* file name */
               hb_parc( 8 ),                  /* RDD */
               hb_parnl( 9 ),                 /* connection */
               hb_param( 2, HB_IT_ARRAY ),    /* Fields */
               FALSE,                         /* Export? */
               hb_param( 3, HB_IT_BLOCK ),    /* cobFor */
               NULL,                          /* lpStrFor */
               hb_param( 4, HB_IT_BLOCK ),    /* cobWhile */
               NULL,                          /* lpStrWhile */
               hb_param( 5, HB_IT_NUMERIC ),  /* Next */
               ISNIL( 6 ) ? NULL : hb_param( 6, HB_IT_ANY ),   /* RecID */
               hb_param( 7, HB_IT_LOGICAL ),  /* Rest */
               hb_parc( 10 ) );               /* Codepage */
      }
      else
      {
         hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "APPEND FROM" );
      }
   }
}

HB_FUNC( __DBCOPY )
{
   if( ISCHAR( 1 ) )
   {
      AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
      if( pArea )
      {
         hb_rddTransRecords( pArea,
               hb_parc( 1 ),                  /* file name */
               hb_parc( 8 ),                  /* RDD */
               hb_parnl( 9 ),                 /* connection */
               hb_param( 2, HB_IT_ARRAY ),    /* Fields */
               TRUE,                          /* Export? */
               hb_param( 3, HB_IT_BLOCK ),    /* cobFor */
               NULL,                          /* lpStrFor */
               hb_param( 4, HB_IT_BLOCK ),    /* cobWhile */
               NULL,                          /* lpStrWhile */
               hb_param( 5, HB_IT_NUMERIC ),  /* Next */
               ISNIL( 6 ) ? NULL : hb_param( 6, HB_IT_ANY ),   /* RecID */
               hb_param( 7, HB_IT_LOGICAL ),  /* Rest */
               hb_parc( 10 ) );               /* Codepage */
      }
      else
      {
         hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "COPY TO" );
      }
   }
}

HB_EXPORT ERRCODE hb_rddGetTempAlias( char * szAliasTmp )
{
   int i, iArea;

   for( i = 1 ; i < 1000 ; i++ )
   {
      sprintf( szAliasTmp, "__HBTMP%03i", i);

      if( hb_rddGetAliasNumber( szAliasTmp, &iArea ) != SUCCESS )
      {
         return SUCCESS;
      }
   }

   szAliasTmp[0] = '\0';
   return FAILURE;
}

HB_FUNC( __RDDGETTEMPALIAS )
{
   HB_THREAD_STUB
   char szAliasTmp[ HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 ];

   if ( hb_rddGetTempAlias( szAliasTmp ) == SUCCESS )
      hb_retc( szAliasTmp );
   else
      hb_ret();
}

#ifdef HB_COMPAT_XPP
HB_FUNC( DBSKIPPER )
{
   HB_THREAD_STUB
   AREAP pArea = HB_CURRENT_WA;

   if( pArea )
   {
      LONG nSkipped    = 0;
      LONG nRecs       = 1;
      BOOL bBEof       = TRUE;
      if( hb_pcount() > 0 )
      {
         nRecs = hb_parnl( 1 ) ;
      }

      SELF_EOF( pArea, &bBEof );
      if( nRecs == 0 )
      {
         SELF_SKIP( pArea, 0 );
      }
      else if( nRecs > 0 && !bBEof  )
      {
         while( nSkipped < nRecs )
         {
            SELF_SKIP( pArea, 1 );
            SELF_EOF( pArea, &bBEof );
            if( bBEof )
            {
               SELF_SKIP( pArea, -1 );
               nRecs = nSkipped ;
            }
            else
            {
               nSkipped++ ;
            }
         }
      }
      else if( nRecs < 0 )
      {
         while( nSkipped > nRecs )
         {
            SELF_SKIP( pArea, -1 );
            SELF_BOF( pArea, &bBEof );
            if( bBEof )
            {
               nRecs = nSkipped ;
            }
            else
            {
               nSkipped-- ;
            }
         }
      }
      hb_retnl( nSkipped );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBSKIPPER" );
}
#endif





// Escaping delimited strings. Need to be cleaned/optimized/improved
static char *hb_strescape( char *szInput, int lLen, char *cDelim )
{
   int     lCnt = 0;
   char  * szChr;
   char  * szEscape;
   char  * szReturn;

   szReturn = szEscape = ( char * ) hb_xgrab( lLen * 2 + 4 );

   while( lLen && HB_ISSPACE( szInput[ lLen - 1 ] ) )
   {
      lLen--;
   }

   szChr = szInput;

   while ( *szChr && lCnt++ < lLen )
   {
      if( *szChr == *cDelim )
      {
         *szEscape++ = '\\';
      }
      *szEscape++ = *szChr++;
   }
   *szEscape = '\0';

   return szReturn;
}

// Export field values to text file
#ifndef HB_CDP_SUPPORT_OFF
static BOOL hb_ExportVar( int handle, PHB_ITEM pValue, char *cDelim, PHB_CODEPAGE cdp )
#else
static BOOL hb_ExportVar( int handle, PHB_ITEM pValue, char *cDelim )
#endif
{
   switch( hb_itemType( pValue ) )
   {
      // a "C" field
      case HB_IT_STRING:
      {
         char *szStrEsc;
         char *szString;

         szStrEsc = hb_strescape( hb_itemGetCPtr( pValue ),
                                  hb_itemGetCLen( pValue ), cDelim );
#ifndef HB_CDP_SUPPORT_OFF
         if( cdp )
         {
            hb_cdpnTranslate( szStrEsc, hb_cdp_page, cdp, hb_itemGetCLen( pValue ) );
         }
#endif
         szString = hb_xstrcpy( NULL,cDelim,szStrEsc,cDelim,NULL);

         // FWrite( handle, szString )
         hb_fsWriteLarge( handle, (BYTE*) szString, strlen( szString ) );

         // Orphaned, get rif off it
         hb_xfree( szStrEsc );
         hb_xfree( szString );
         break;
      }
      // a "D" field
      case HB_IT_DATE:
      {
         char *szDate = (char*) hb_xgrab( 9 );

         hb_itemGetDS( pValue, szDate );
         hb_fsWriteLarge( handle, (BYTE*) szDate, strlen( szDate ) );
         hb_xfree( szDate );
         break;
      }
      // an "L" field
      case HB_IT_LOGICAL:
      {
         hb_fsWriteLarge( handle, (BYTE*) ( hb_itemGetL( pValue )  ? "T" : "F" ), 1 );
         break;
      }
      // an "N" field
      case HB_IT_INTEGER:
      case HB_IT_LONG:
      case HB_IT_DOUBLE:
      {
         char *szResult = hb_itemStr( pValue, NULL, NULL );

         if ( szResult )
         {
            ULONG ulLen = strlen( szResult );
            char * szTrimmed = hb_strLTrim( szResult, &ulLen );

            hb_fsWriteLarge( handle, (BYTE*) szTrimmed, strlen( szTrimmed ) );
            hb_xfree( szResult );
         }
         break;
      }
      // an "M" field or the other, might be a "V" in SixDriver
      default:
      // We do not want MEMO contents
         return FALSE;
   }
   return TRUE;
}

HB_FUNC( DBF2TEXT )
{
   HB_THREAD_STUB

   PHB_ITEM pWhile  = hb_param( 1, HB_IT_BLOCK );
   PHB_ITEM pFor    = hb_param( 2, HB_IT_BLOCK );
   PHB_ITEM pFields = hb_param( 3, HB_IT_ARRAY );

   char *cDelim   = hb_parc( 4 );
   FHANDLE handle = (FHANDLE) hb_parnl(5);
   BYTE *cSep     = (BYTE *) hb_parc( 6 );
   int nCount     = (int) hb_parnl( 7 );
#ifndef HB_CDP_SUPPORT_OFF
   PHB_CODEPAGE cdp = hb_cdpFind( (char *) hb_parc( 8 ) );
#endif

   AREAP pArea = HB_CURRENT_WA;

   // Export DBF content to text file

   int iSepLen;
   USHORT uiFields = 0;
   USHORT ui;
   PHB_ITEM pTmp;
   BOOL bWriteSep = FALSE;

   BOOL bEof = TRUE;
   BOOL bBof = TRUE;

   BOOL bNoFieldPassed = ( pFields == NULL || hb_arrayLen( pFields ) == 0 );


   if( ! handle )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBF2TEXT" );
      return;
   }

   if( cdp && cdp == hb_cdp_page )
   {
      cdp = NULL;
   }

   pTmp = hb_itemNew( NULL );

   if ( !cDelim )
   {
      cDelim = "\"";
   }

   if ( cSep )
   {
      iSepLen = strlen( (char*) cSep );
   }
   else
   {
      cSep = (BYTE*) ',';
      iSepLen = 1;
   }

   SELF_FIELDCOUNT( pArea, &uiFields );

   while( ( nCount == -1 || nCount > 0 ) &&
          ( !pWhile || hb_itemGetL( hb_vmEvalBlock( pWhile ) ) ) )
   {
      // While !BOF() .AND. !EOF()
      SELF_EOF( pArea, &bEof );
      SELF_BOF( pArea, &bBof );

      if( bEof || bBof )
      {
         break;
      }

      // For condition is met
      // if For is NULL, hb__Eval returns TRUE
      if( !pFor || hb_itemGetL( hb_vmEvalBlock( pFor ) ) )
      {
         // User does not request fields, copy all fields
         if( bNoFieldPassed )
         {
            for ( ui = 1; ui <= uiFields; ui ++ )
            {
               if ( bWriteSep )
               {
                  hb_fsWriteLarge( handle, cSep, iSepLen );
               }

               SELF_GETVALUE( pArea, ui, pTmp );
#ifndef HB_CDP_SUPPORT_OFF
               bWriteSep = hb_ExportVar( handle, pTmp, cDelim, cdp );
#else
               bWriteSep = hb_ExportVar( handle, pTmp, cDelim );
#endif
               hb_itemClear( pTmp );
            }
         }
         // Only requested fields are exported here
         else
         {
            USHORT uiFieldCopy = ( USHORT ) hb_arrayLen( pFields );
            USHORT uiItter;

            for ( uiItter = 1; uiItter <= uiFieldCopy; uiItter++ )
            {
               char * szFieldName = hb_arrayGetCPtr( pFields, uiItter );
               if( szFieldName )
               {
                  int iPos = hb_rddFieldIndex( pArea, szFieldName );

                  if( iPos )
                  {
                     if ( bWriteSep )
                     {
                        hb_fsWriteLarge( handle, cSep, iSepLen );
                     }
                     SELF_GETVALUE( pArea, iPos, pTmp );
#ifndef HB_CDP_SUPPORT_OFF
                     bWriteSep = hb_ExportVar( handle, pTmp, cDelim, cdp );
#else
                     bWriteSep = hb_ExportVar( handle, pTmp, cDelim );
#endif
                     hb_itemClear( pTmp );
                  }
               }
            }
         }
         hb_fsWriteLarge( handle, (BYTE*) "\r\n", 2 );
         bWriteSep = FALSE;
      }

      if ( nCount != -1 )
      {
         nCount-- ;
      }

      // DBSKIP()
      SELF_SKIP( pArea, 1 );
   }

   // Writing EOF
   hb_fsWriteLarge( handle, (BYTE*) "\x1A", 1 );
   hb_itemRelease( pTmp );
}
