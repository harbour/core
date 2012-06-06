/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Default RDD module
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
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
#include "rddsys.ch"
#include "hbset.h"

/*
 * check if a given name can be used as alias expression
 */
HB_ERRCODE hb_rddVerifyAliasName( const char * szAlias )
{
   char c;

   if( szAlias )
   {
      /* Clipper ignores only trailing spaces */
#if 0
      while( *szAlias == ' ' )
         szAlias++;
#endif

      c = *szAlias;
      if( ( c >= 'A' && c <= 'Z' ) || ( c >= 'a' && c <= 'z' ) || c == '_' )
      {
         c = *(++szAlias);
         while( c != 0 )
         {
            if( c != '_' && ! ( c >= '0' && c <= '9' ) &&
                ! ( c >= 'A' && c <= 'Z' ) && ! ( c >= 'a' && c <= 'z' ) )
            {
               if( c == ' ' )
               {
                  while( *(++szAlias) == ' ' ) { ; }
                  if( ! *szAlias )
                     break;
               }
               return HB_FAILURE;
            }
            c = *(++szAlias);
         }
         return HB_SUCCESS;
      }
   }
   return HB_FAILURE;
}

/*
 * Prepares a new WorkArea node.
 */
void * hb_rddNewAreaNode( LPRDDNODE pRddNode, HB_USHORT uiRddID )
{
   AREAP pArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddNewAreaNode(%p,%hu)", pRddNode, uiRddID));

   if( pRddNode->uiAreaSize == 0 ) /* Calculate the size of WorkArea */
   {
      HB_USHORT uiSize;

      pArea = ( AREAP ) hb_xgrab( sizeof( AREA ) );
      memset( pArea, 0, sizeof( AREA ) );
      pArea->lprfsHost = &pRddNode->pTable;
      pArea->rddID = uiRddID;

      if( SELF_STRUCTSIZE( pArea, &uiSize ) != HB_SUCCESS )
         return NULL;

      /* Need more space? */
      if( uiSize > sizeof( AREA ) )   /* Size of Area changed */
      {
         pArea = ( AREAP ) hb_xrealloc( pArea, uiSize );
         memset( pArea, 0, uiSize );
         pArea->lprfsHost = &pRddNode->pTable;
         pArea->rddID = uiRddID;
      }

      pRddNode->uiAreaSize = uiSize;  /* Update the size of WorkArea */
   }
   else
   {
      pArea = ( AREAP ) hb_xgrab( pRddNode->uiAreaSize );
      memset( pArea, 0, pRddNode->uiAreaSize );
      pArea->lprfsHost = &pRddNode->pTable;
      pArea->rddID = uiRddID;
   }

   if( SELF_NEW( pArea ) != HB_SUCCESS )
   {
      SELF_RELEASE( pArea );
      return NULL;
   }

   return ( void * ) pArea;
}

HB_ERRCODE hb_rddGetTempAlias( char * szAliasTmp )
{
   int i, iArea;

   for( i = 1; i < 1000; i++ )
   {
      hb_snprintf( szAliasTmp, 11, "__HBTMP%03i", i );
      if( hb_rddGetAliasNumber( szAliasTmp, &iArea ) != HB_SUCCESS )
         return HB_SUCCESS;
   }
   szAliasTmp[0] = '\0';
   return HB_FAILURE;
}

/*
 * allocate and return atomAlias for new workarea or NULL if alias already exist
 */
void * hb_rddAllocWorkAreaAlias( const char * szAlias, int iArea )
{
   PHB_DYNS pSymAlias;
   int iDummyArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddAllocWorkAreaAlias(%s, %d)", szAlias, iArea));

   /* Verify if the alias name is valid symbol */
   if( hb_rddVerifyAliasName( szAlias ) != HB_SUCCESS )
   {
      hb_errRT_DBCMD_Ext( EG_BADALIAS, EDBCMD_BADALIAS, NULL, szAlias, EF_CANDEFAULT );
      return NULL;
   }
   /* Verify if the alias is already in use */
   if( hb_rddGetAliasNumber( szAlias, &iDummyArea ) == HB_SUCCESS )
   {
      hb_errRT_DBCMD_Ext( EG_DUPALIAS, EDBCMD_DUPALIAS, NULL, szAlias, EF_CANDEFAULT );
      return NULL;
   }

   pSymAlias = hb_dynsymGet( szAlias );
   if( hb_dynsymAreaHandle( pSymAlias ) != 0 )
   {
      pSymAlias = NULL;
   }
   else
   {
      hb_dynsymSetAreaHandle( pSymAlias, iArea );
   }

   if( ! pSymAlias )
   {
      hb_errRT_DBCMD_Ext( EG_DUPALIAS, EDBCMD_DUPALIAS, NULL, szAlias, EF_CANDEFAULT );
   }

   return pSymAlias;
}

/*
 * Find a field index by name
 */
HB_USHORT hb_rddFieldIndex( AREAP pArea, const char * szName )
{
   HB_USHORT uiCount = 0;
   LPFIELD pField;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddFieldIndex(%p, %s)", pArea, szName));

   while( HB_ISSPACE( *szName ) )
   {
      ++szName;
   }

   if( *szName )
   {
      char szSym[ HB_SYMBOL_NAME_LEN + 1 ];
      hb_strncpyUpperTrim( szSym, szName, sizeof( szSym ) - 1 );

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
HB_USHORT hb_rddFieldExpIndex( AREAP pArea, const char * szField )
{
   int n;

   while( HB_ISSPACE( *szField ) )
      ++szField;

   if( strchr( szField, '>' ) != NULL )
   {
      char szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];
      int i, j, l;

      n = 0;
      if( SELF_ALIAS( pArea, szAlias ) == HB_SUCCESS )
         l = ( int ) strlen( szAlias );
      else
         l = 0;

      /*
       * strip the _FIELD-> and FIELD-> prefix, it could be nested
       * so repeat this process until all prefixes will be removed
       */
      do
      {
         j = n;
         i = 0;
         if( HB_ISFIRSTIDCHAR( szField[ n ] ) )
         {
            ++i;
            while( HB_ISNEXTIDCHAR( szField[ n + i ] ) )
               ++i;

            if( !( ( i == l &&
                       hb_strnicmp( &szField[ n ], szAlias, l ) == 0 ) ) &&
                !( i >=4 && i <= 5 &&
                   hb_strnicmp( &szField[ n ], "FIELD", i ) == 0 ) &&
                !( i >=4 && i <= 6 &&
                   hb_strnicmp( &szField[ n ], "_FIELD", i ) == 0 ) )
            {
               i = 0;
            }
         }

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
      while( n != j );
      szField = &szField[ n ];
   }
   return hb_rddFieldIndex( pArea, szField );
}

/*
 * Find a WorkArea by the alias, return HB_FAILURE if not found
 */
HB_ERRCODE hb_rddGetAliasNumber( const char * szAlias, int * iArea )
{
   HB_BOOL fOneLetter;
   char c;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddGetAliasNumber(%s, %p)", szAlias, iArea));

   while( *szAlias == ' ' )
      szAlias++;

   c = szAlias[ 0 ];
   if( c >= 'a' && c <= 'z' )
      c -= 'a' - 'A';

   fOneLetter = c && ( szAlias[ 1 ] == 0 || szAlias[ 1 ] == ' ' );

   if( c >= '0' && c <= '9' )
   {
      *iArea = atoi( szAlias );
   }
   else if( fOneLetter && c >= 'A' && c <= 'K' )
   {
      *iArea = c - 'A' + 1;
   }
   else if( fOneLetter && c == 'M' )
   {
      *iArea = HB_RDD_MAX_AREA_NUM;
   }
   else
   {
      PHB_DYNS pSymAlias = hb_dynsymFindName( szAlias );

      *iArea = pSymAlias ? ( int ) hb_dynsymAreaHandle( pSymAlias ) : 0;
      if( *iArea == 0 )
         return HB_FAILURE;
   }

   return HB_SUCCESS;
}

/*
 * Select a WorkArea by the symbol name.
 */
HB_ERRCODE hb_rddSelectWorkAreaSymbol( PHB_SYMB pSymAlias )
{
   PHB_ITEM pError;
   HB_ERRCODE errCode;
   const char * szName;
   int iArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelectWorkAreaSymbol(%p)", pSymAlias));

   iArea = ( int ) hb_dynsymAreaHandle( pSymAlias->pDynSym );
   if( iArea )
   {
      hb_rddSelectWorkAreaNumber( iArea );
      return HB_SUCCESS;
   }

   szName = hb_dynsymName( pSymAlias->pDynSym );

   if( szName[ 0 ] && ! szName[ 1 ] )
   {
      if( szName[ 0 ] >= 'A' && szName[ 0 ] <= 'K' )
      {
         hb_rddSelectWorkAreaNumber( szName[ 0 ] - 'A' + 1 );
         return HB_SUCCESS;
      }
      else if( szName[ 0 ] >= 'a' && szName[ 0 ] <= 'k' )
      {
         hb_rddSelectWorkAreaNumber( szName[ 0 ] - 'a' + 1 );
         return HB_SUCCESS;
      }
      else if( szName[ 0 ] == 'M' || szName[ 0 ] == 'm' )
      {
         hb_rddSelectWorkAreaNumber( HB_RDD_MAX_AREA_NUM );
         return HB_SUCCESS;
      }
   }

   /*
    * generate an error with retry possibility
    * (user created error handler can open a missing database)
    */

   pError = hb_errRT_New( ES_ERROR, NULL, EG_NOALIAS, EDBCMD_NOALIAS, NULL, pSymAlias->szName, 0, EF_CANRETRY );
   errCode = HB_FAILURE;

   do
   {
      if( hb_errLaunch( pError ) != E_RETRY )
         break;
      iArea = ( int ) hb_dynsymAreaHandle( pSymAlias->pDynSym );
      if( iArea )
      {
         hb_rddSelectWorkAreaNumber( iArea );
         errCode = HB_SUCCESS;
      }
   }
   while( errCode == HB_FAILURE );

   hb_itemRelease( pError );

   return errCode;
}

/*
 * Select a WorkArea by the name.
 */
HB_ERRCODE hb_rddSelectWorkAreaAlias( const char * szAlias )
{
   HB_ERRCODE errCode;
   int iArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelectWorkAreaAlias(%s)", szAlias));

   errCode = hb_rddGetAliasNumber( szAlias, &iArea );

   if( errCode == HB_FAILURE )
   {
      /*
       * generate an error with retry possibility
       * (user created error handler can open a missing database)
       */
      PHB_ITEM pError = hb_errRT_New( ES_ERROR, NULL, EG_NOALIAS, EDBCMD_NOALIAS, NULL, szAlias, 0, EF_CANRETRY );

      do
      {
         if( hb_errLaunch( pError ) != E_RETRY )
            break;
         errCode = hb_rddGetAliasNumber( szAlias, &iArea );
      }
      while( errCode == HB_FAILURE );

      hb_itemRelease( pError );
   }

   if( errCode == HB_SUCCESS )
   {
      if( iArea < 1 || iArea > HB_RDD_MAX_AREA_NUM )
         errCode = hb_rddSelectFirstAvailable();
      else
         errCode = hb_rddSelectWorkAreaNumber( iArea );
   }

   return errCode;
}

/*
 * Obtain the current value of a field.
 */
HB_ERRCODE hb_rddFieldGet( PHB_ITEM pItem, PHB_SYMB pFieldSymbol )
{
   AREAP pArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddFieldGet(%p, %p)", pItem, pFieldSymbol));

   pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      HB_USHORT uiField = 1;
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
   return HB_FAILURE;
}

/*
 * Assign a value to a field.
 */
HB_ERRCODE hb_rddFieldPut( PHB_ITEM pItem, PHB_SYMB pFieldSymbol )
{
   AREAP pArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddFieldPut(%p, %p)", pItem, pFieldSymbol));

   pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
   {
      HB_USHORT uiField = 1;
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
   return HB_FAILURE;
}

/*
 * Obtain the current value of a field.
 */
HB_ERRCODE hb_rddGetFieldValue( PHB_ITEM pItem, PHB_SYMB pFieldSymbol )
{
   HB_ERRCODE errCode;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddGetFieldValue(%p, %p)", pItem, pFieldSymbol));

   errCode = hb_rddFieldGet( pItem, pFieldSymbol );

   if( errCode == HB_FAILURE && hb_vmRequestQuery() == 0 )
   {
      /*
       * generate an error with retry possibility
       * (user created error handler can make this field accessible)
       */
      PHB_ITEM pError = hb_errRT_New( ES_ERROR, NULL, EG_NOVAR, EDBCMD_NOVAR,
                                      NULL, pFieldSymbol->szName, 0, EF_CANRETRY );

      while( hb_errLaunch( pError ) == E_RETRY )
      {
         errCode = hb_rddFieldGet( pItem, pFieldSymbol );

         if( errCode == HB_SUCCESS || hb_vmRequestQuery() != 0 )
            break;
      }
      hb_itemRelease( pError );
   }

   return errCode;
}

/*
 * Assign a value to a field.
 */
HB_ERRCODE hb_rddPutFieldValue( PHB_ITEM pItem, PHB_SYMB pFieldSymbol )
{
   HB_ERRCODE errCode;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddPutFieldValue(%p, %p)", pItem, pFieldSymbol));

   errCode = hb_rddFieldPut( pItem, pFieldSymbol );

   if( errCode == HB_FAILURE && hb_vmRequestQuery() == 0 )
   {
      /*
       * generate an error with retry possibility
       * (user created error handler can make this field accessible)
       */
      PHB_ITEM pError = hb_errRT_New( ES_ERROR, NULL, EG_NOVAR, EDBCMD_NOVAR,
                                      NULL, pFieldSymbol->szName, 0, EF_CANRETRY );

      while( hb_errLaunch( pError ) == E_RETRY )
      {
         errCode = hb_rddFieldPut( pItem, pFieldSymbol );

         if( errCode == HB_SUCCESS || hb_vmRequestQuery() != 0 )
            break;
      }
      hb_itemRelease( pError );
   }

   return errCode;
}

HB_ERRCODE hb_rddOpenTable( const char * szFileName, const char * szDriver,
                            HB_USHORT uiArea, const char *szAlias,
                            HB_BOOL fShared, HB_BOOL fReadonly,
                            const char * szCpId, HB_ULONG ulConnection,
                            PHB_ITEM pStruct, PHB_ITEM pDelim )
{
   char szDriverBuffer[ HB_RDD_MAX_DRIVERNAME_LEN + 1 ];
   DBOPENINFO pInfo;
   HB_ERRCODE errCode;
   AREAP pArea;

   /* uiArea = 0 in hb_rddInsertAreaNode() means chose first
    * available free area, otherwise we should close table in
    * current WA and it should be done before parameter validation
    * RT errors below. This breaks xHarbour like MT code which
    * shares WA between threads so dbUseArea() should be covered
    * by external mutex to make lNewArea MT safe, [druzus]
    */
   if( uiArea )
   {
      hb_rddSelectWorkAreaNumber( uiArea );
      hb_rddReleaseCurrentArea();
   }
   else
      hb_rddSelectFirstAvailable();

   /* Clipper clears NETERR flag before parameter validation, [druzus]
    */
   hb_rddSetNetErr( HB_FALSE );

   /* Now check parameters, first RDD name.
    * Clipper seems to make sth like:
    *    if( szDriver && strlen( szDriver ) > 1 )
    * but I do not think we should replicate it, [druzus]
    */
   if( szDriver && szDriver[ 0 ] )
   {
      hb_strncpyUpper( szDriverBuffer, szDriver, sizeof( szDriverBuffer ) - 1 );
      szDriver = szDriverBuffer;
   }
   else
      szDriver = hb_rddDefaultDrv( NULL );

   /* First try to create new are node and validate RDD name */
   if( ! hb_rddInsertAreaNode( szDriver ) )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
      return HB_FAILURE;
   }

   /* Then check if valid file name was given - Clipper allows to use empty
    * ("") file name
    */
   if( !szFileName )
   {
      hb_rddReleaseCurrentArea();
      hb_errRT_DBCMD( EG_ARG, EDBCMD_USE_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
      return HB_FAILURE;
   }

   pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   /* Fill pInfo structure */
   pInfo.uiArea = pArea->uiArea;
   pInfo.abName = szFileName;
   pInfo.atomAlias = szAlias;
   pInfo.fShared = fShared;
   pInfo.fReadonly = fReadonly;
   pInfo.cdpId = szCpId ? szCpId : hb_setGetDBCODEPAGE();
   pInfo.ulConnection = ulConnection;
   pInfo.lpdbHeader = NULL;

   errCode = pStruct ? SELF_CREATEFIELDS( pArea, pStruct ) : HB_SUCCESS;
   if( errCode == HB_SUCCESS )
   {
      if( pDelim && !HB_IS_NIL( pDelim ) )
         errCode = SELF_INFO( pArea, DBI_SETDELIMITER, pDelim );
      if( errCode == HB_SUCCESS )
         /* Open file */
         errCode = SELF_OPEN( pArea, &pInfo );
   }

   if( errCode != HB_SUCCESS )
      hb_rddReleaseCurrentArea();

   return errCode;
}

HB_ERRCODE hb_rddCreateTable( const char * szFileName, const char * szDriver,
                              HB_USHORT uiArea, const char *szAlias,
                              HB_BOOL fKeepOpen,
                              const char * szCpId, HB_ULONG ulConnection,
                              PHB_ITEM pStruct, PHB_ITEM pDelim )
{
   char szDriverBuffer[ HB_RDD_MAX_DRIVERNAME_LEN + 1 ];
   DBOPENINFO pInfo;
   HB_ERRCODE errCode;
   HB_USHORT uiPrevArea;
   AREAP pArea;

   if( !szFileName )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, HB_ERR_FUNCNAME );
      return HB_FAILURE;
   }

   if( szDriver && szDriver[ 0 ] )
   {
      hb_strncpyUpper( szDriverBuffer, szDriver, sizeof( szDriverBuffer ) - 1 );
      szDriver = szDriverBuffer;
   }
   else
      szDriver = hb_rddDefaultDrv( NULL );

   uiPrevArea = ( HB_AREANO ) hb_rddGetCurrentWorkAreaNumber();

   /* 0 means chose first available in hb_rddInsertAreaNode() */
   hb_rddSelectWorkAreaNumber( uiArea );
   if( uiArea )
      hb_rddReleaseCurrentArea();

   /* Create a new WorkArea node */
   if( ! hb_rddInsertAreaNode( szDriver ) )
   {
      hb_rddSelectWorkAreaNumber( uiPrevArea );
      hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
      return HB_FAILURE;
   }
   pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   /* Fill pInfo structure */
   pInfo.uiArea = pArea->uiArea;
   pInfo.abName = szFileName;
   pInfo.atomAlias = szAlias;
   pInfo.fShared = HB_FALSE;
   pInfo.fReadonly = HB_FALSE;
   pInfo.cdpId = szCpId ? szCpId : hb_setGetDBCODEPAGE();
   pInfo.ulConnection = ulConnection;
   pInfo.lpdbHeader = NULL;

   if( pDelim && !HB_IS_NIL( pDelim ) )
      errCode = SELF_INFO( pArea, DBI_SETDELIMITER, pDelim );
   else
      errCode = HB_SUCCESS;

   if( errCode == HB_SUCCESS )
   {
      errCode = SELF_CREATEFIELDS( pArea, pStruct );
      if( errCode == HB_SUCCESS )
         errCode = SELF_CREATE( pArea, &pInfo );
   }

   if( !fKeepOpen || errCode != HB_SUCCESS )
   {
      hb_rddReleaseCurrentArea();
      hb_rddSelectWorkAreaNumber( uiPrevArea );
   }

   return errCode;
}

HB_ERRCODE hb_rddCreateTableTemp( const char * szDriver,
                                  const char * szAlias,
                                  const char * szCpId, HB_ULONG ulConnection,
                                  PHB_ITEM pStruct )
{
   char szDriverBuffer[ HB_RDD_MAX_DRIVERNAME_LEN + 1 ];
   DBOPENINFO pInfo;
   PHB_ITEM pItem;
   HB_ERRCODE errCode;
   HB_USHORT uiPrevArea;
   AREAP pArea;

   if( szDriver && szDriver[ 0 ] )
   {
      hb_strncpyUpper( szDriverBuffer, szDriver, sizeof( szDriverBuffer ) - 1 );
      szDriver = szDriverBuffer;
   }
   else
      szDriver = hb_rddDefaultDrv( NULL );

   uiPrevArea = ( HB_AREANO ) hb_rddGetCurrentWorkAreaNumber();

   /* 0 means chose first available in hb_rddInsertAreaNode() */
   hb_rddSelectWorkAreaNumber( 0 );

   /* Create a new WorkArea node */
   if( ! hb_rddInsertAreaNode( szDriver ) )
   {
      hb_rddSelectWorkAreaNumber( uiPrevArea );
      hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
      return HB_FAILURE;
   }
   pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   /* Fill pInfo structure */
   pInfo.uiArea = pArea->uiArea;
   pInfo.abName = NULL;
   pInfo.atomAlias = szAlias;
   pInfo.fShared = HB_FALSE;
   pInfo.fReadonly = HB_FALSE;
   pInfo.cdpId = szCpId ? szCpId : hb_setGetDBCODEPAGE();
   pInfo.ulConnection = ulConnection;
   pInfo.lpdbHeader = NULL;

   pItem = hb_itemPutL( NULL, HB_TRUE );
   errCode = SELF_INFO( pArea, DBI_ISTEMPORARY, pItem );
   hb_itemRelease( pItem );

   if( errCode == HB_SUCCESS )
   {
      errCode = SELF_CREATEFIELDS( pArea, pStruct );
      if( errCode == HB_SUCCESS )
         errCode = SELF_CREATE( pArea, &pInfo );
   }

   if( errCode != HB_SUCCESS )
   {
      hb_rddReleaseCurrentArea();
      hb_rddSelectWorkAreaNumber( uiPrevArea );
   }

   return errCode;
}

static void hb_fldStructure( AREAP pArea, HB_USHORT uiField, HB_USHORT uiSize,
                             PHB_ITEM pField )
{
#ifdef DBS_FLAG
   static const HB_USHORT s_uiActions[] =
            { DBS_NAME, DBS_TYPE, DBS_LEN, DBS_DEC, DBS_FLAG };
#else
   static const HB_USHORT s_uiActions[] =
            { DBS_NAME, DBS_TYPE, DBS_LEN, DBS_DEC };
#endif
   HB_USHORT uiCount;

   if( uiSize == 0 || uiSize > HB_SIZEOFARRAY( s_uiActions ) )
      uiSize = HB_SIZEOFARRAY( s_uiActions );

   hb_arrayNew( pField, uiSize );
   for( uiCount = 0; uiCount < uiSize; ++uiCount )
   {
      SELF_FIELDINFO( pArea, uiField, s_uiActions[ uiCount ],
                      hb_arrayGetItemPtr( pField, uiCount + 1 ) );
   }
}

void hb_tblStructure( AREAP pArea, PHB_ITEM pStruct, HB_USHORT uiSize )
{
   HB_USHORT uiFields, uiCount;

   if( SELF_FIELDCOUNT( pArea, &uiFields ) == HB_SUCCESS )
   {
      if( hb_arraySize( pStruct, uiFields ) )
      {
         for( uiCount = 1; uiCount <= uiFields; ++uiCount )
            hb_fldStructure( pArea, uiCount, uiSize,
                             hb_arrayGetItemPtr( pStruct, uiCount ) );
      }
   }
}

static const char * hb_dbTransFieldPos( PHB_ITEM pFields, HB_USHORT uiField )
{
   const char * szField = NULL;
   PHB_ITEM pItem;

   pItem = hb_arrayGetItemPtr( pFields, uiField );
   if( pItem )
   {
      if( HB_IS_ARRAY( pItem ) )
         szField = hb_arrayGetCPtr( pItem, DBS_NAME );
      else
         szField = hb_itemGetCPtr( pItem );

      if( * szField == '\0' )
         szField = NULL;
   }

   return szField;
}

HB_ERRCODE hb_dbTransStruct( AREAP lpaSource, AREAP lpaDest,
                             LPDBTRANSINFO lpdbTransInfo,
                             PHB_ITEM *pStruct, PHB_ITEM pFields )
{
   HB_USHORT uiFields, uiSize, uiCount, uiPosSrc, uiPosDst, uiSizeSrc, uiSizeDst;
   HB_ERRCODE errCode;
   const char * szField;
   HB_BOOL fAll;

   errCode = SELF_FIELDCOUNT( lpaSource, &uiSizeSrc );
   if( errCode != HB_SUCCESS )
      return errCode;

   if( lpaDest )
   {
      errCode = SELF_FIELDCOUNT( lpaDest, &uiSizeDst );
      if( errCode != HB_SUCCESS )
         return errCode;
      uiSize = HB_MIN( uiSizeDst, uiSizeSrc );
   }
   else
   {
      uiSize = uiSizeDst = uiSizeSrc;
   }

   if( !uiSize )
      return HB_FAILURE;
   if( hb_itemType( pFields ) & HB_IT_ARRAY )
   {
      uiFields = ( HB_USHORT ) hb_arrayLen( pFields );
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
            if( SELF_FIELDINFO( lpaSource, uiCount, DBS_NAME, pItem ) != HB_SUCCESS )
            {
               uiSize = 0;
               break;
            }
            szField = hb_itemGetCPtr( pItem );
            uiPosDst = hb_rddFieldExpIndex( lpaDest, szField );
            if( uiPosDst != uiCount )
               fAll = HB_FALSE;
            if( uiPosDst )
            {
               HB_USHORT ui;

               /* check for replicated field names in source area */
               for( ui = 0; ui < uiSize; ++ui )
               {
                  if( lpdbTransInfo->lpTransItems[ ui ].uiDest == uiPosDst )
                     break;
               }
               if( ui == uiSize )
               {
                  lpdbTransInfo->lpTransItems[ uiSize ].uiSource = uiCount;
                  lpdbTransInfo->lpTransItems[ uiSize++ ].uiDest = uiPosDst;
               }
            }
         }
         hb_itemRelease( pItem );
      }
      else
      {
         hb_tblStructure( lpaSource, *pStruct, 0 );
         uiSize = ( HB_USHORT ) hb_arrayLen( *pStruct );
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
         szField = hb_dbTransFieldPos( pFields, uiCount );
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
                  fAll = HB_FALSE;
               lpdbTransInfo->lpTransItems[ uiSize ].uiSource = uiPosSrc;
               lpdbTransInfo->lpTransItems[ uiSize++ ].uiDest = uiPosDst;
               if( !lpaDest )
               {
                  hb_arraySize( *pStruct, uiSize );
                  hb_fldStructure( lpaSource, uiPosSrc, 0,
                                   hb_arrayGetItemPtr( *pStruct, uiSize ) );
               }
            }
         }
      }
   }

   if( uiSize != uiSizeSrc )
      fAll = HB_FALSE;

   if( fAll && lpaDest )
   {
      PHB_ITEM pSrcItm = hb_itemNew( NULL ),
               pDstItm = hb_itemNew( NULL );
      /*
       * if fAll is HB_TRUE here then it means that all fields are included
       * and they are on the same positions in both tables, so now check
       * if their types and sizes are also equal
       */
      for( uiCount = 1; uiCount <= uiSize; ++uiCount )
      {
         if( SELF_FIELDINFO( lpaSource, uiCount, DBS_TYPE, pSrcItm ) != HB_SUCCESS ||
             SELF_FIELDINFO( lpaDest,   uiCount, DBS_TYPE, pDstItm ) != HB_SUCCESS )
         {
            uiSize = 0;
            break;
         }
         if( hb_stricmp( hb_itemGetCPtr( pSrcItm ),
                         hb_itemGetCPtr( pDstItm ) ) != 0 )
         {
            fAll = HB_FALSE;
            break;
         }
         if( SELF_FIELDINFO( lpaSource, uiCount, DBS_LEN, pSrcItm ) != HB_SUCCESS ||
             SELF_FIELDINFO( lpaDest,   uiCount, DBS_LEN, pDstItm ) != HB_SUCCESS )
         {
            uiSize = 0;
            break;
         }
         if( hb_itemGetNL( pSrcItm ) != hb_itemGetNL( pDstItm ) )
         {
            fAll = HB_FALSE;
            break;
         }
         if( SELF_FIELDINFO( lpaSource, uiCount, DBS_DEC, pSrcItm ) != HB_SUCCESS ||
             SELF_FIELDINFO( lpaDest,   uiCount, DBS_DEC, pDstItm ) != HB_SUCCESS )
         {
            uiSize = 0;
            break;
         }
         if( hb_itemGetNL( pSrcItm ) != hb_itemGetNL( pDstItm ) )
         {
            fAll = HB_FALSE;
            break;
         }
#ifdef DBS_FLAG
         if( SELF_FIELDINFO( lpaSource, uiCount, DBS_FLAG, pSrcItm ) != HB_SUCCESS ||
             SELF_FIELDINFO( lpaDest,   uiCount, DBS_FLAG, pDstItm ) != HB_SUCCESS )
         {
            uiSize = 0;
            break;
         }
#endif
         if( hb_itemGetNL( pSrcItm ) != hb_itemGetNL( pDstItm ) )
         {
            fAll = HB_FALSE;
            break;
         }
      }
      hb_itemRelease( pSrcItm );
      hb_itemRelease( pDstItm );
   }

   lpdbTransInfo->uiFlags = fAll ? DBTF_MATCH : 0;
   lpdbTransInfo->uiItemCount = uiSize;

   return uiSize ? HB_SUCCESS : HB_FAILURE;
}

HB_ERRCODE hb_rddTransRecords( AREAP pArea,
                               const char *szFileName, const char *szDriver,
                               HB_ULONG ulConnection,
                               PHB_ITEM pFields, HB_BOOL fExport,
                               PHB_ITEM pCobFor, PHB_ITEM pStrFor,
                               PHB_ITEM pCobWhile, PHB_ITEM pStrWhile,
                               PHB_ITEM pNext, PHB_ITEM pRecID,
                               PHB_ITEM pRest,
                               const char *szCpId,
                               PHB_ITEM pDelim )
{
   AREAP lpaClose = NULL;
   PHB_ITEM pStruct = NULL;
   DBTRANSINFO dbTransInfo;
   HB_USHORT uiPrevArea, uiCount, uiSwap;
   HB_ERRCODE errCode;

   memset( &dbTransInfo, 0, sizeof( dbTransInfo ) );
   uiPrevArea = ( HB_AREANO ) hb_rddGetCurrentWorkAreaNumber();

   if( szDriver == NULL )
      /* szDriver = SELF_RDDNODE( pArea )->szName; */
      szDriver = hb_rddDefaultDrv( NULL );

   if( fExport )
   {
      errCode = hb_dbTransStruct( pArea, NULL, &dbTransInfo,
                                  &pStruct, pFields );
      if( errCode == HB_SUCCESS )
      {
         errCode = hb_rddCreateTable( szFileName, szDriver, 0, "",
                                      HB_TRUE,
                                      szCpId, ulConnection, pStruct, pDelim );
         if( errCode == HB_SUCCESS )
         {
            dbTransInfo.lpaDest = lpaClose =
                                 ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
         }
      }
   }
   else
   {
      LPRDDNODE pRddNode = hb_rddFindNode( szDriver, NULL );

      if( !pRddNode )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_USE_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
         return HB_FAILURE;
      }

      if( pRddNode->uiType == RDT_TRANSFER )
      {
         errCode = hb_dbTransStruct( pArea, NULL, &dbTransInfo,
                                     &pStruct, pFields );

         /* revert area and items */
         dbTransInfo.lpaDest = dbTransInfo.lpaSource;
         for( uiCount = 0; uiCount < dbTransInfo.uiItemCount; ++uiCount )
         {
            uiSwap = dbTransInfo.lpTransItems[ uiCount ].uiSource;
            dbTransInfo.lpTransItems[ uiCount ].uiSource =
                                    dbTransInfo.lpTransItems[ uiCount ].uiDest;
            dbTransInfo.lpTransItems[ uiCount ].uiDest = uiSwap;
         }

         if( errCode == HB_SUCCESS )
         {
            errCode = hb_rddOpenTable( szFileName, szDriver, 0, "", HB_TRUE, HB_TRUE,
                                       szCpId, ulConnection, pStruct, pDelim );
            if( errCode == HB_SUCCESS )
            {
               lpaClose = dbTransInfo.lpaSource =
                                 ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
            }
         }
      }
      else
      {
         errCode = hb_rddOpenTable( szFileName, szDriver, 0, "", HB_TRUE, HB_TRUE,
                                    szCpId, ulConnection, NULL, pDelim );
         if( errCode == HB_SUCCESS )
         {
            lpaClose = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
            errCode = hb_dbTransStruct( lpaClose, pArea, &dbTransInfo,
                                        NULL, pFields );
         }
      }
   }

   if( pStruct )
      hb_itemRelease( pStruct );

   if( errCode == HB_SUCCESS )
   {
      hb_rddSelectWorkAreaNumber( dbTransInfo.lpaSource->uiArea );

      dbTransInfo.dbsci.itmCobFor   = pCobFor;
      dbTransInfo.dbsci.lpstrFor    = pStrFor;
      dbTransInfo.dbsci.itmCobWhile = pCobWhile;
      dbTransInfo.dbsci.lpstrWhile  = pStrWhile;
      dbTransInfo.dbsci.lNext       = pNext;
      dbTransInfo.dbsci.itmRecID    = pRecID;
      dbTransInfo.dbsci.fRest       = pRest;

      dbTransInfo.dbsci.fIgnoreFilter     = HB_TRUE;
      dbTransInfo.dbsci.fIncludeDeleted   = HB_TRUE;
      dbTransInfo.dbsci.fLast             = HB_FALSE;
      dbTransInfo.dbsci.fIgnoreDuplicates = HB_FALSE;
      dbTransInfo.dbsci.fBackward         = HB_FALSE;

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

static HB_ERRCODE hb_rddCloseParentRel( AREAP pArea, void * pChildArea )
{
   if( pArea->lpdbRelations )
   {
      LPDBRELINFO * lpdbRelationPtr = &pArea->lpdbRelations;
      HB_USHORT uiArea = ( ( AREAP ) pChildArea )->uiArea;

      do
      {
         LPDBRELINFO lpdbRelation = *lpdbRelationPtr;

         if( lpdbRelation->lpaChild->uiArea == uiArea )
         {
            /* Clear this relation */
            hb_rddSelectWorkAreaNumber( lpdbRelation->lpaChild->uiArea );
            SELF_CHILDEND( lpdbRelation->lpaChild, lpdbRelation );
            if( lpdbRelation->itmCobExpr )
               hb_itemRelease( lpdbRelation->itmCobExpr );
            if( lpdbRelation->abKey )
               hb_itemRelease( lpdbRelation->abKey );

            *lpdbRelationPtr = lpdbRelation->lpdbriNext;
            hb_xfree( lpdbRelation );
         }
         else
            lpdbRelationPtr = &lpdbRelation->lpdbriNext;
      }
      while ( *lpdbRelationPtr );
   }
   return HB_SUCCESS;
}

/* close all parent relations */
HB_ERRCODE hb_rddCloseAllParentRelations( AREAP pArea )
{
   HB_ERRCODE errCode = HB_SUCCESS;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddCloseAllParentRelations(%p)", pArea));

   if( pArea->uiParents > 0 )
   {
      HB_USHORT uiArea = ( HB_AREANO ) hb_rddGetCurrentWorkAreaNumber();
      errCode = hb_rddIterateWorkAreas( hb_rddCloseParentRel, pArea );
      hb_rddSelectWorkAreaNumber( uiArea );
   }

   return errCode;
}

static HB_ERRCODE hb_rddEvalWABlock( AREAP pArea, void * pBlock )
{
   PHB_ITEM pItem;

   hb_rddSelectWorkAreaNumber( pArea->uiArea );
   pItem = hb_vmEvalBlockOrMacro( ( PHB_ITEM ) pBlock );

   if( hb_vmRequestQuery() != 0 ||
       ( HB_IS_LOGICAL( pItem ) && ! hb_itemGetL( pItem ) ) )
      return HB_FAILURE;
   else
      return HB_SUCCESS;
}

HB_ERRCODE hb_rddEvalWA( PHB_ITEM pBlock )
{
   HB_ERRCODE errCode;
   HB_USHORT uiArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddEvalWA(%p)", pBlock));

   uiArea = ( HB_AREANO ) hb_rddGetCurrentWorkAreaNumber();
   errCode = hb_rddIterateWorkAreas( hb_rddEvalWABlock, pBlock );
   hb_rddSelectWorkAreaNumber( uiArea );

   return errCode;
}
