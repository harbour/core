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
#include "rddsys.ch"

/*
 * check if a given name can be used as alias expression
 */
HB_EXPORT ERRCODE hb_rddVerifyAliasName( const char * szAlias )
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
 * Prepares a new WorkArea node.
 */
HB_EXPORT void * hb_rddNewAreaNode( LPRDDNODE pRddNode, USHORT uiRddID )
{
   AREAP pArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddNewAreaNode(%p,%hu)", pRddNode, uiRddID));

   if( pRddNode->uiAreaSize == 0 ) /* Calculate the size of WorkArea */
   {
      USHORT uiSize;

      pArea = ( AREAP ) hb_xgrab( sizeof( AREA ) );
      memset( pArea, 0, sizeof( AREA ) );
      pArea->lprfsHost = &pRddNode->pTable;
      pArea->rddID = uiRddID;

      if( SELF_STRUCTSIZE( pArea, &uiSize ) != SUCCESS )
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

   if( SELF_NEW( pArea ) != SUCCESS )
   {
      SELF_RELEASE( pArea );
      return NULL;
   }

   return ( void * ) pArea;
}

HB_EXPORT ERRCODE hb_rddGetTempAlias( char * szAliasTmp )
{
   int i, iArea;

   for( i = 1 ; i < 1000 ; i++ )
   {
      snprintf( szAliasTmp, 11, "__HBTMP%03i", i);
      if( hb_rddGetAliasNumber( szAliasTmp, &iArea ) != SUCCESS )
         return SUCCESS;
   }
   szAliasTmp[0] = '\0';
   return FAILURE;
}

/*
 * allocate and return atomAlias for new workarea or NULL if alias already exist
 */
HB_EXPORT void * hb_rddAllocWorkAreaAlias( const char * szAlias, int iArea )
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
HB_EXPORT USHORT hb_rddFieldIndex( AREAP pArea, const char * szName )
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
HB_EXPORT USHORT hb_rddFieldExpIndex( AREAP pArea, const char * szField )
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
      if( SELF_ALIAS( pArea, ( BYTE * ) szAlias ) == SUCCESS )
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
      while( n != j );
      szField = &szField[ n ];
   }
   return hb_rddFieldIndex( pArea, szField );
}

/*
 * Find a WorkArea by the alias, return FAILURE if not found
 */
HB_EXPORT ERRCODE hb_rddGetAliasNumber( const char * szAlias, int * iArea )
{
   BOOL fOneLetter;
   char c;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddGetAliasNumber(%s, %p)", szAlias, iArea));

   while( *szAlias == ' ' )
   {
      szAlias++;
   }
   c = szAlias[ 0 ];
   if( c >= 'a' && c <= 'z' )
   {
      c -= 'a' - 'A';
   }

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
      *iArea = HARBOUR_MAX_RDD_AREA_NUM;
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
 * Select a WorkArea by the symbol name.
 */
HB_EXPORT ERRCODE hb_rddSelectWorkAreaSymbol( PHB_SYMB pSymAlias )
{
   HB_ITEM_PTR pError;
   ERRCODE errCode;
   char * szName;
   int iArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelectWorkAreaSymbol(%p)", pSymAlias));

   iArea = ( int ) hb_dynsymAreaHandle( pSymAlias->pDynSym );
   if( iArea )
   {
      hb_rddSelectWorkAreaNumber( iArea );
      return SUCCESS;
   }

   szName = hb_dynsymName( pSymAlias->pDynSym );

   if( szName[ 0 ] && ! szName[ 1 ] )
   {
      if( szName[ 0 ] >= 'A' && szName[ 0 ] <= 'K' )
      {
         hb_rddSelectWorkAreaNumber( szName[ 0 ] - 'A' + 1 );
         return SUCCESS;
      }
      else if( szName[ 0 ] >= 'a' && szName[ 0 ] <= 'k' )
      {
         hb_rddSelectWorkAreaNumber( szName[ 0 ] - 'a' + 1 );
         return SUCCESS;
      }
      else if( szName[ 0 ] == 'M' || szName[ 0 ] == 'm' )
      {
         hb_rddSelectWorkAreaNumber( HARBOUR_MAX_RDD_AREA_NUM );
         return SUCCESS;
      }
   }

   /*
    * generate an error with retry possibility
    * (user created error handler can open a missing database)
    */

   pError = hb_errRT_New( ES_ERROR, NULL, EG_NOALIAS, EDBCMD_NOALIAS, NULL, pSymAlias->szName, 0, EF_CANRETRY );
   errCode = FAILURE;

   do
   {
      if( hb_errLaunch( pError ) != E_RETRY )
         break;
      iArea = ( int ) hb_dynsymAreaHandle( pSymAlias->pDynSym );
      if( iArea )
      {
         hb_rddSelectWorkAreaNumber( iArea );
         errCode = SUCCESS;
      }
   }
   while( errCode == FAILURE );

   hb_itemRelease( pError );

   return errCode;
}

/*
 * Select a WorkArea by the name.
 */
HB_EXPORT ERRCODE hb_rddSelectWorkAreaAlias( const char * szAlias )
{
   ERRCODE errCode;
   int iArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddSelectWorkAreaAlias(%s)", szAlias));

   errCode = hb_rddGetAliasNumber( szAlias, &iArea );

   if( errCode == FAILURE )
   {
      /*
       * generate an error with retry possibility
       * (user created error handler can open a missing database)
       */
      HB_ITEM_PTR pError = hb_errRT_New( ES_ERROR, NULL, EG_NOALIAS, EDBCMD_NOALIAS, NULL, szAlias, 0, EF_CANRETRY );

      do
      {
         if( hb_errLaunch( pError ) != E_RETRY )
            break;
         errCode = hb_rddGetAliasNumber( szAlias, &iArea );
      }
      while( errCode == FAILURE );

      hb_itemRelease( pError );
   }

   if( errCode == SUCCESS )
   {
      if( iArea < 1 || iArea > HARBOUR_MAX_RDD_AREA_NUM )
         errCode = hb_rddSelectFirstAvailable();
      else
         errCode = hb_rddSelectWorkAreaNumber( iArea );
   }

   return errCode;
}

/*
 * Obtain the current value of a field.
 */
HB_EXPORT ERRCODE hb_rddFieldGet( HB_ITEM_PTR pItem, PHB_SYMB pFieldSymbol )
{
   AREAP pArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddFieldGet(%p, %p)", pItem, pFieldSymbol));

   pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
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
   AREAP pArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddFieldPut(%p, %p)", pItem, pFieldSymbol));

   pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
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
   ERRCODE errCode;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddGetFieldValue(%p, %p)", pItem, pFieldSymbol));

   errCode = hb_rddFieldGet( pItem, pFieldSymbol );

   if( errCode == FAILURE && hb_vmRequestQuery() == 0 )
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

         if( errCode == SUCCESS || hb_vmRequestQuery() != 0 )
            break;
      }
      hb_itemRelease( pError );
   }

   return errCode;
}

/*
 * Assign a value to a field.
 */
HB_EXPORT ERRCODE hb_rddPutFieldValue( HB_ITEM_PTR pItem, PHB_SYMB pFieldSymbol )
{
   ERRCODE errCode;

   HB_TRACE(HB_TR_DEBUG, ("hb_rddPutFieldValue(%p, %p)", pItem, pFieldSymbol));

   errCode = hb_rddFieldPut( pItem, pFieldSymbol );

   if( errCode == FAILURE && hb_vmRequestQuery() == 0 )
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

         if( errCode == SUCCESS || hb_vmRequestQuery() != 0 )
            break;
      }
      hb_itemRelease( pError );
   }

   return errCode;
}

ERRCODE hb_rddOpenTable( const char * szFileName, const char * szDriver,
                         USHORT uiArea, const char *szAlias,
                         BOOL fShared, BOOL fReadonly,
                         const char * szCpId, ULONG ulConnection,
                         PHB_ITEM pStruct, PHB_ITEM pDelim )
{
   char szDriverBuffer[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 ];
   DBOPENINFO pInfo;
   ERRCODE errCode;
   //USHORT uiPrevArea;
   AREAP pArea;

   /* Clipper clears NETERR flag before RT error below */
   hb_rddSetNetErr( FALSE );

   if( !szFileName || !szFileName[ 0 ] )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_USE_BADPARAMETER, NULL, "DBUSEAREA" );
      return FAILURE;
   }

   if( szDriver && szDriver[ 0 ] )
   {
      hb_strncpyUpper( szDriverBuffer, szDriver, HARBOUR_MAX_RDD_DRIVERNAME_LENGTH );
      szDriver = szDriverBuffer;
   }
   else
   {
      szDriver = hb_rddDefaultDrv( NULL );
   }

   //uiPrevArea = hb_rddGetCurrentWorkAreaNumber();

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

   /* Create a new WorkArea node */
   if( ! hb_rddInsertAreaNode( szDriver ) )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, "DBUSEAREA" );
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

   if( pStruct )
   {
      errCode = SELF_CREATEFIELDS( pArea, pStruct );
   }
   else
   {
      errCode = SUCCESS;
   }

   if( errCode == SUCCESS )
   {
      if( pDelim && !HB_IS_NIL( pDelim ) )
         errCode = SELF_INFO( pArea, DBI_SETDELIMITER, pDelim );

      if( errCode == SUCCESS )
      {
         /* Open file */
         errCode = SELF_OPEN( pArea, &pInfo );
         /*-----------------04/05/2007 11:00-----------------
          * Clipper not restore the old workarea
          * --------------------------------------------------*/
         /*
         if( errCode != SUCCESS )
         {
            hb_rddReleaseCurrentArea();
            hb_rddSelectWorkAreaNumber( uiPrevArea );
         }*/
      }
   }

   /*
    * Warning: this is not Clipper compatible. NETERR() should be set by
    * error handler not here
    */
   if( errCode != SUCCESS )
      hb_rddSetNetErr( TRUE );

   return errCode;
}

ERRCODE hb_rddCreateTable( const char * szFileName, const char * szDriver,
                           USHORT uiArea, const char *szAlias,
                           BOOL fKeepOpen,
                           const char * szCpId, ULONG ulConnection,
                           PHB_ITEM pStruct, PHB_ITEM pDelim )
{
   char szDriverBuffer[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 ];
   DBOPENINFO pInfo;
   ERRCODE errCode;
   USHORT uiPrevArea;
   AREAP pArea;

   if( !szFileName || !szFileName[ 0 ] )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, "DBCREATE" );
      return FAILURE;
   }

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

   if( pDelim && !HB_IS_NIL( pDelim ) )
      errCode = SELF_INFO( pArea, DBI_SETDELIMITER, pDelim );
   else
      errCode = SUCCESS;

   if( errCode == SUCCESS )
   {
      errCode = SELF_CREATEFIELDS( pArea, pStruct );
      if( errCode == SUCCESS )
         errCode = SELF_CREATE( pArea, &pInfo );
   }

   if( !fKeepOpen || errCode != SUCCESS )
   {
      hb_rddReleaseCurrentArea();
      hb_rddSelectWorkAreaNumber( uiPrevArea );
   }

   return errCode;
}

static void hb_fldStructure( AREAP pArea, USHORT uiField, PHB_ITEM pField )
{
   hb_arrayNew( pField, 4 );

   SELF_FIELDINFO( pArea, uiField, DBS_NAME, hb_arrayGetItemPtr( pField, 1 ) );
   SELF_FIELDINFO( pArea, uiField, DBS_TYPE, hb_arrayGetItemPtr( pField, 2 ) );
   SELF_FIELDINFO( pArea, uiField, DBS_LEN,  hb_arrayGetItemPtr( pField, 3 ) );
   SELF_FIELDINFO( pArea, uiField, DBS_DEC,  hb_arrayGetItemPtr( pField, 4 ) );
}

void hb_tblStructure( AREAP pArea, PHB_ITEM pStruct )
{
   USHORT uiFields, uiCount;

   if( SELF_FIELDCOUNT( pArea, &uiFields ) == SUCCESS )
   {
      if( hb_arraySize( pStruct, uiFields ) )
      {
         for( uiCount = 1; uiCount <= uiFields; ++uiCount )
            hb_fldStructure( pArea, uiCount,
                             hb_arrayGetItemPtr( pStruct, uiCount ) );
      }
   }
}

static char * hb_dbTransFieldPos( PHB_ITEM pFields, USHORT uiField )
{
   char * szField = NULL;
   PHB_ITEM pItem;

   pItem = hb_arrayGetItemPtr( pFields, uiField );
   if( pItem )
   {
      HB_TYPE type = hb_itemType( pItem );

      if( type & HB_IT_ARRAY )
         szField = hb_arrayGetCPtr( pItem, DBS_NAME );
      else if( type & HB_IT_STRING )
         szField = hb_itemGetCPtr( pItem );

      if( * szField == '\0' )
         szField = NULL;
   }

   return szField;
}

ERRCODE hb_dbTransStruct( AREAP lpaSource, AREAP lpaDest,
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
            if( SELF_FIELDINFO( lpaSource, uiCount, DBS_NAME, pItem ) != SUCCESS )
            {
               uiSize = 0;
               break;
            }
            szField = hb_itemGetCPtr( pItem );
            uiPosDst = hb_rddFieldExpIndex( lpaDest, szField );
            if( uiPosDst != uiCount )
               fAll = FALSE;
            if( uiPosDst )
            {
               USHORT ui;

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
         if( SELF_FIELDINFO( lpaSource, uiCount, DBS_TYPE, pSrcItm ) != SUCCESS ||
             SELF_FIELDINFO( lpaDest,   uiCount, DBS_TYPE, pDstItm ) != SUCCESS )
         {
            uiSize = 0;
            break;
         }
         if( hb_stricmp( hb_itemGetCPtr( pSrcItm ),
                         hb_itemGetCPtr( pDstItm ) ) != 0 )
         {
            fAll = FALSE;
            break;
         }
         if( SELF_FIELDINFO( lpaSource, uiCount, DBS_LEN, pSrcItm ) != SUCCESS ||
             SELF_FIELDINFO( lpaDest,   uiCount, DBS_LEN, pDstItm ) != SUCCESS )
         {
            uiSize = 0;
            break;
         }
         if( hb_itemGetNL( pSrcItm ) != hb_itemGetNL( pDstItm ) )
         {
            fAll = FALSE;
            break;
         }
         if( SELF_FIELDINFO( lpaSource, uiCount, DBS_DEC, pSrcItm ) != SUCCESS ||
             SELF_FIELDINFO( lpaDest,   uiCount, DBS_DEC, pDstItm ) != SUCCESS )
         {
            uiSize = 0;
            break;
         }
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

ERRCODE hb_rddTransRecords( AREAP pArea,
                            const char *szFileName, const char *szDriver,
                            ULONG ulConnection,
                            PHB_ITEM pFields, BOOL fExport,
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
   USHORT uiPrevArea, uiCount, uiSwap;
   ERRCODE errCode;

   memset( &dbTransInfo, 0, sizeof( DBTRANSINFO ) );
   uiPrevArea = hb_rddGetCurrentWorkAreaNumber();

   if( szDriver == NULL )
      /* szDriver = SELF_RDDNODE( pArea )->szName; */
      szDriver = hb_rddDefaultDrv( NULL );

   if( fExport )
   {
      errCode = hb_dbTransStruct( pArea, NULL, &dbTransInfo,
                                  &pStruct, pFields );
      if( errCode == SUCCESS )
      {
         errCode = hb_rddCreateTable( szFileName, szDriver, 0, "",
                                      TRUE,
                                      szCpId, ulConnection, pStruct, pDelim );
         if( errCode == SUCCESS )
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
         hb_errRT_DBCMD( EG_ARG, EDBCMD_USE_BADPARAMETER, NULL, "DBUSEAREA" );
         return FAILURE;
      }

      if( pRddNode->uiType == RDT_TRANSFER )
      {
         errCode = hb_dbTransStruct( pArea, NULL, &dbTransInfo,
                                     &pStruct, pFields );

         /* revert area and items */
         dbTransInfo.lpaDest = dbTransInfo.lpaSource;
         for( uiCount = 0; uiCount < dbTransInfo.uiItemCount; ++uiCount )
         {
            uiSwap = dbTransInfo.lpTransItems[uiCount].uiSource;
            dbTransInfo.lpTransItems[uiCount].uiSource =
                                    dbTransInfo.lpTransItems[uiCount].uiDest;
            dbTransInfo.lpTransItems[uiCount].uiDest = uiSwap;
         }

         if( errCode == SUCCESS )
         {
            errCode = hb_rddOpenTable( szFileName, szDriver, 0, "", TRUE, TRUE,
                                       szCpId, ulConnection, pStruct, pDelim );
            if( errCode == SUCCESS )
            {
               lpaClose = dbTransInfo.lpaSource =
                                 ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
            }
         }
      }
      else
      {
         errCode = hb_rddOpenTable( szFileName, szDriver, 0, "", TRUE, TRUE,
                                    szCpId, ulConnection, NULL, pDelim );
         if( errCode == SUCCESS )
         {
            lpaClose = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
            errCode = hb_dbTransStruct( lpaClose, pArea, &dbTransInfo,
                                        NULL, pFields );
         }
      }
   }

   if( pStruct )
      hb_itemRelease( pStruct );

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
      dbTransInfo.dbsci.fBackward         = FALSE;

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
