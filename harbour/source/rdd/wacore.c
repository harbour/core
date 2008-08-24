/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Default RDD module
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbapi.h"
#include "hbapirdd.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbvm.h"

/* Default RDD name */

static AREAP * s_WaList = NULL;        /* Allocated WorkAreas */
static USHORT s_uiWaMax = 0;           /* Number of allocated WA */
static USHORT s_uiWaSpace = 0;         /* Number of allocated WA */

static USHORT * s_WaNums = NULL;       /* Allocated WorkAreas */
static USHORT s_uiWaNumMax = 0;        /* Number of allocated WA */

static USHORT s_uiCurrArea = 1;        /* Current WokrArea number */
static AREAP  s_pCurrArea = NULL;      /* Current WorkArea pointer */

static BOOL s_fNetError = FALSE;       /* Error on Networked environments */


#define HB_SET_WA( n )  do \
            { \
               s_uiCurrArea = n; \
               s_pCurrArea = ( ( s_uiCurrArea < s_uiWaNumMax ) ? \
                                 s_WaList[ s_WaNums[ s_uiCurrArea ] ] : \
                                 NULL ); \
            } while( 0 )


/*
 * Return the next free WorkArea for later use.
 */
HB_EXPORT ERRCODE hb_rddSelectFirstAvailable( void )
{
   USHORT uiArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelectFirstAvailable()"));

   uiArea = 1;
   while( uiArea < s_uiWaNumMax )
   {
      if( s_WaNums[ uiArea ] == 0 )
         break;
      uiArea++;
   }
   if( uiArea >= HB_RDD_MAX_AREA_NUM )
      return FAILURE;
   HB_SET_WA( uiArea );
   return SUCCESS;
}

/*
 * Insert the new WorkArea node
 */
HB_EXPORT USHORT hb_rddInsertAreaNode( const char *szDriver )
{
   USHORT uiRddID, uiWaPos;
   LPRDDNODE pRddNode;
   AREAP pArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddInsertAreaNode(%s)", szDriver));

   if( s_uiCurrArea && s_pCurrArea )
      return 0;

   pRddNode = hb_rddFindNode( szDriver, &uiRddID );
   if( !pRddNode )
      return 0;

   pArea = ( AREAP ) hb_rddNewAreaNode( pRddNode, uiRddID );
   if( !pArea )
      return 0;

   if( s_uiCurrArea == 0 )
   {
      if( hb_rddSelectFirstAvailable() != SUCCESS )
         return 0;
   }

   if( s_uiCurrArea >= s_uiWaNumMax )
   {
      int iSize = ( ( ( int ) s_uiCurrArea + 256 ) >> 8 ) << 8;

      if( iSize > HB_RDD_MAX_AREA_NUM )
         iSize = HB_RDD_MAX_AREA_NUM;

      if( s_uiWaNumMax == 0 )
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

   if( s_uiWaSpace == 0 )
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
      if( s_uiWaMax > s_uiWaSpace )
      {
         s_uiWaSpace = ( ( s_uiWaMax + 256 ) >> 8 ) << 8;
         s_WaList = (AREAP *) hb_xrealloc( s_WaList, s_uiWaSpace * sizeof(AREAP) );
         memset( &s_WaList[ s_uiWaMax ], 0, ( s_uiWaSpace - s_uiWaMax ) * sizeof(AREAP) );
      }
      while( uiWaPos > 1 )
      {
         if( s_WaList[ uiWaPos - 1 ]->uiArea < s_uiCurrArea )
            break;
         s_WaList[ uiWaPos ] = s_WaList[ uiWaPos - 1 ];
         s_WaNums[ s_WaList[ uiWaPos ]->uiArea ] = uiWaPos;
         uiWaPos--;
      }
   }
   s_WaList[ uiWaPos ] = pArea;
   s_WaNums[ s_uiCurrArea ] = uiWaPos;
   s_pCurrArea = s_WaList[ uiWaPos ];
   s_pCurrArea->uiArea = s_uiCurrArea;

   return s_uiCurrArea;
}

/*
 * Closes and releases the current WorkArea preparing it
 * to be used with a new database.
 */
HB_EXPORT void hb_rddReleaseCurrentArea( void )
{
   USHORT uiWaPos;
   AREAP pArea = s_pCurrArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddReleaseCurrentArea()"));

   if( !pArea )
      return;

   if( SELF_CLOSE( pArea ) == FAILURE )
      return;

   SELF_RELEASE( pArea );

   uiWaPos = s_WaNums[ s_uiCurrArea ];
   s_WaNums[ s_uiCurrArea ] = 0;
   s_uiWaMax--;
   if( s_uiWaMax <= 1 )
   {
      s_uiWaSpace = s_uiWaMax = s_uiWaNumMax = 0;
      hb_xfree( s_WaList );
      hb_xfree( s_WaNums );
      s_WaList = NULL;
      s_WaNums = NULL;
   }
   else
   {
      while( uiWaPos < s_uiWaMax )
      {
         s_WaList[ uiWaPos ] = s_WaList[ uiWaPos + 1 ];
         s_WaNums[ s_WaList[ uiWaPos ]->uiArea ] = uiWaPos;
         uiWaPos++;
      }
      s_WaList[ s_uiWaMax ] = NULL;
      if( s_uiWaSpace - s_uiWaMax >= 256 )
      {
         s_uiWaSpace = ( ( s_uiWaMax + 256 ) >> 8 ) << 8;
         s_WaList = ( AREAP * ) hb_xrealloc( s_WaList, s_uiWaSpace * sizeof( AREAP ) );
      }
   }
   s_pCurrArea = NULL;
}

/*
 * Closes all WorkAreas.
 */
HB_EXPORT void hb_rddCloseAll( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_rddCloseAll()"));

   if( s_uiWaMax > 0 )
   {
      BOOL isParents, isFinish = FALSE;
      AREAP pArea;
      USHORT uiIndex;

      do
      {
         isParents = FALSE;
         for( uiIndex = 1; uiIndex < s_uiWaMax; uiIndex++ )
         {
            pArea = s_WaList[ uiIndex ];
            HB_SET_WA( pArea->uiArea );
            if( isFinish )
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
      while( isParents );

      s_uiWaSpace = s_uiWaMax = s_uiWaNumMax = 0;
      hb_xfree( s_WaList );
      hb_xfree( s_WaNums );
      s_WaList = NULL;
      s_WaNums = NULL;
      HB_SET_WA( 1 );
   }
}

HB_EXPORT void hb_rddFlushAll( void )
{
   USHORT uiArea = hb_rddGetCurrentWorkAreaNumber(), uiIndex;

   for( uiIndex = 1; uiIndex < s_uiWaMax; ++uiIndex )
   {
      hb_rddSelectWorkAreaNumber( s_WaList[ uiIndex ]->uiArea );
      SELF_FLUSH( s_pCurrArea );
   }
   hb_rddSelectWorkAreaNumber( uiArea );
}

HB_EXPORT void hb_rddUnLockAll( void )
{
   USHORT uiArea = hb_rddGetCurrentWorkAreaNumber(), uiIndex;

   for( uiIndex = 1; uiIndex < s_uiWaMax; ++uiIndex )
   {
      hb_rddSelectWorkAreaNumber( s_WaList[ uiIndex ]->uiArea );
      SELF_UNLOCK( s_pCurrArea, NULL );
   }
   hb_rddSelectWorkAreaNumber( uiArea );
}

/*
 * call a pCallBack function with all open workareas ###
 */
HB_EXPORT ERRCODE hb_rddIterateWorkAreas( WACALLBACK pCallBack, void * cargo )
{
   ERRCODE errCode = SUCCESS;
   USHORT uiIndex;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddIterateWorkAreas(%p,%p)", pCallBack, cargo));

   for( uiIndex = 1; uiIndex < s_uiWaMax; uiIndex++ )
   {
      errCode = pCallBack( s_WaList[ uiIndex ], cargo );
      if( errCode != SUCCESS )
         break;
   }
   return errCode;
}

HB_EXPORT BOOL hb_rddGetNetErr( void )
{
   return s_fNetError;
}

HB_EXPORT void hb_rddSetNetErr( BOOL fNetErr )
{
   s_fNetError = fNetErr;
}

/*
 * Get (/set) default RDD driver
 */
HB_EXPORT const char * hb_rddDefaultDrv( const char * szDriver )
{
   static char s_szDefDriver[ HB_RDD_MAX_DRIVERNAME_LEN + 1 ] = "";
   static BOOL s_fInit = FALSE;

   if( szDriver && *szDriver )
   {
      char szNewDriver[ HB_RDD_MAX_DRIVERNAME_LEN + 1 ];

      hb_strncpyUpper( szNewDriver, szDriver, sizeof( szNewDriver ) - 1 );
      if( !hb_rddFindNode( szNewDriver, NULL ) )
         return NULL;
      hb_strncpy( s_szDefDriver, szNewDriver, sizeof( s_szDefDriver ) - 1 );
   }
   else if( !s_fInit && !s_szDefDriver[ 0 ] && hb_rddGetNode( 0 ) )
   {
      char *szDrvTable[] = { "DBFNTX", "DBFCDX", "DBFFPT", "DBF", NULL };
      int i;

      for( i = 0; szDrvTable[ i ]; ++i )
      {
         if( hb_rddFindNode( szDrvTable[ i ], NULL ) )
         {
            hb_strncpy( s_szDefDriver, szDrvTable[ i ], sizeof( s_szDefDriver ) - 1 );
            break;
         }
      }
      s_fInit = TRUE;
   }

   return s_szDefDriver;
}

/*
 * Function for getting given workarea pointer
 */
HB_EXPORT void * hb_rddGetWorkAreaPointer( int iArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_rddGetWorkAreaPointer(%d)", iArea));

   if( iArea == 0 )
      return s_pCurrArea;
   else if( iArea >= 1 && ( UINT ) iArea < ( UINT ) s_uiWaNumMax )
      return s_WaList[ s_WaNums[ iArea ] ];
   else
      return NULL;
}

/*
 * Function for getting current workarea pointer
 */
HB_EXPORT void * hb_rddGetCurrentWorkAreaPointer( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_rddGetCurrentWorkAreaPointer()"));

   return s_pCurrArea;
}

/*
 * Return the current WorkArea number.
 */
HB_EXPORT int hb_rddGetCurrentWorkAreaNumber( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_rddGetCurrentWorkAreaNumber()"));

   return s_uiCurrArea;
}

/*
 * Select a WorkArea by the number.
 */
HB_EXPORT ERRCODE hb_rddSelectWorkAreaNumber( int iArea )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelectWorkAreaNumber(%d)", iArea));

   if( iArea < 1 || iArea > HB_RDD_MAX_AREA_NUM )
      HB_SET_WA( 0 );
   else
      HB_SET_WA( iArea );

   return ( s_pCurrArea == NULL ) ? FAILURE : SUCCESS;
}
