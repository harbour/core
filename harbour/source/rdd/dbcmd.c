/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Base RDD module
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
 * Copyright 2004-2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbapi.h"
#include "hbapirdd.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbset.h"

HB_FUNC( AFIELDS )
{
   PHB_ITEM pName, pType, pLen, pDec;
   USHORT uiFields, uiCount;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

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

   if( SELF_FIELDCOUNT( pArea, &uiFields ) != SUCCESS )
      return;

   if( pName )
   {
      USHORT uiArrayLen = ( USHORT ) hb_arrayLen( pName );
      if( uiArrayLen < uiFields )
         uiFields = uiArrayLen;
   }
   if( pType )
   {
      USHORT uiArrayLen = ( USHORT ) hb_arrayLen( pType );
      if( uiArrayLen < uiFields )
         uiFields = uiArrayLen;
   }
   if( pLen )
   {
      USHORT uiArrayLen = ( USHORT ) hb_arrayLen( pLen );
      if( uiArrayLen < uiFields )
         uiFields = uiArrayLen;
   }
   if( pDec )
   {
      USHORT uiArrayLen = ( USHORT ) hb_arrayLen( pDec );
      if( uiArrayLen < uiFields )
         uiFields = uiArrayLen;
   }

   if( pName )
   {
      for( uiCount = 1; uiCount <= uiFields; ++uiCount )
      {
         if( SELF_FIELDINFO( pArea, uiCount, DBS_NAME, hb_arrayGetItemPtr( pName, uiCount ) ) != SUCCESS )
            return;
      }
   }
   if( pType )
   {
      for( uiCount = 1; uiCount <= uiFields; ++uiCount )
      {
         if( SELF_FIELDINFO( pArea, uiCount, DBS_TYPE, hb_arrayGetItemPtr( pType, uiCount ) ) != SUCCESS )
            return;
      }
   }
   if( pLen )
   {
      for( uiCount = 1; uiCount <= uiFields; ++uiCount )
      {
         if( SELF_FIELDINFO( pArea, uiCount, DBS_LEN, hb_arrayGetItemPtr( pLen, uiCount ) ) != SUCCESS )
            return;
      }
   }
   if( pDec )
   {
      for( uiCount = 1; uiCount <= uiFields; ++uiCount )
      {
         if( SELF_FIELDINFO( pArea, uiCount, DBS_DEC, hb_arrayGetItemPtr( pDec, uiCount ) ) != SUCCESS )
            return;
      }
   }

   hb_retni( uiFields );
}

HB_FUNC( ALIAS )
{
   USHORT uiArea;
   AREAP pArea;

   uiArea = hb_parni( 1 );
   pArea = ( AREAP ) hb_rddGetWorkAreaPointer( uiArea );
   if( pArea )
   {
      char szAlias[ HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 ];

      if( SELF_ALIAS( pArea, ( BYTE * ) szAlias ) == SUCCESS )
      {
         hb_retc( szAlias );
         return;
      }
   }
   hb_retc( NULL );
}

HB_FUNC( DBEVAL )
{
   DBEVALINFO pEvalInfo;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      char szAlias[ HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 ];

      if( SELF_ALIAS( pArea, ( BYTE * ) szAlias ) == SUCCESS )
      {
         hb_retc( szAlias );
         return;
      }
   }
   hb_retc( NULL );
}

HB_FUNC( BOF )
{
   BOOL bBof = TRUE;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_BOF( pArea, &bBof );

   hb_retl( bBof );
}

HB_FUNC( DBAPPEND )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      BOOL bUnLockAll = ISLOG( 1 ) ? hb_parl( 1 ) : TRUE;
      ERRCODE errCode;

      /* Clipper clears NETERR flag when table is open */
      hb_rddSetNetErr( FALSE );
      errCode = SELF_APPEND( pArea, bUnLockAll );
      hb_retl( errCode == SUCCESS );

      /*
       * Warning: this is not Clipper compatible. NETERR() should be set by
       * error handler not here
       */
      if( errCode != SUCCESS )
         hb_rddSetNetErr( TRUE );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBAPPEND" );
}

HB_FUNC( DBCLEARFILTER )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

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
   hb_rddReleaseCurrentArea();
}

HB_FUNC( DBCOMMIT )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
      SELF_FLUSH( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBCOMMIT" );
}

HB_FUNC( DBCOMMITALL )
{
   hb_rddFlushAll();
}

/*
 * In Clipper the arguments are:
 *    dbCreate( cFile, aStruct, cRDD, lKeepOpen, cAlias, cDelimArg )
 * In Harbour:
 *    dbCreate( cFile, aStruct, cRDD, lKeepOpen, cAlias, cDelimArg, cCodePage, nConnection )
 */
HB_FUNC( DBCREATE )
{
   char * szFileName, * szAlias, * szDriver, * szCpId;
   USHORT uiSize, uiLen;
   PHB_ITEM pStruct, pFieldDesc, pDelim;
   BOOL fKeepOpen, fCurrArea;
   ULONG ulConnection;

   /*
    * NOTE: 4-th, 5-th and 6-th parameters are undocumented Clipper ones
    * 4-th is boolean flag indicating if file should stay open
    * 5-th is alias - if not given then WA is open without alias
    * 6-th is optional DELIMITED value used by some RDDs like DELIM
    */

   szFileName = hb_parc( 1 );
   pStruct = hb_param( 2, HB_IT_ARRAY );
   szDriver = hb_parc( 3 );
   fKeepOpen = ISLOG( 4 );
   fCurrArea = fKeepOpen && !hb_parl( 4 );
   szAlias = hb_parc( 5 );
   pDelim = hb_param( 6, HB_IT_ANY );
   szCpId = hb_parc( 7 );
   ulConnection = hb_parnl( 8 );

   /*
    * Clipper allows to use empty struct array for RDDs which does not
    * support fields, f.e.: DBFBLOB in CL5.3
    * In CL5.3 it's also possible to create DBF file without fields.
    * if some RDD wants to block it then they should serve it in lower
    * level, [druzus]
    */
   if( !pStruct ||
#ifdef HB_C52_STRICT
       hb_arrayLen( pStruct ) == 0 ||
#endif
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

   hb_retl( hb_rddCreateTable( szFileName, szDriver,
                               fCurrArea ? hb_rddGetCurrentWorkAreaNumber() : 0,
                               szAlias, fKeepOpen,
                               szCpId, ulConnection,
                               pStruct, pDelim ) == SUCCESS );
}

/*
 * I'm not sure if lKeepOpen open works exactly like in DBCREATE, I haven't
 * tested it with Clipper yet. If it doesn't then please inform me about it
 * and I'll update the code. [druzus]
 */
/* __dbopensdf( cFile, aStruct, cRDD, lKeepOpen, cAlias, cDelimArg, cCodePage, nConnection ) */
HB_FUNC( __DBOPENSDF )
{
   char * szFileName, * szAlias, * szDriver, * szCpId;
   USHORT uiSize, uiLen;
   PHB_ITEM pStruct, pFieldDesc, pDelim;
   BOOL fKeepOpen, fCurrArea;
   ULONG ulConnection;
   ERRCODE errCode;

   /*
    * NOTE: 4-th and 5-th parameters are undocumented Clipper ones
    * 4-th is boolean flag indicating if file should stay open and
    * 5-th is alias - if not given then WA is open without alias
    */

   szFileName = hb_parc( 1 );
   pStruct = hb_param( 2, HB_IT_ARRAY );
   szDriver = hb_parc( 3 );
   fKeepOpen = ISLOG( 4 );
   fCurrArea = fKeepOpen && !hb_parl( 4 );
   szAlias = hb_parc( 5 );
   pDelim = hb_param( 6, HB_IT_ANY );
   szCpId = hb_parc( 7 );
   ulConnection = hb_parnl( 8 );

   if( !pStruct ||
       hb_arrayLen( pStruct ) == 0 ||
       !szFileName || !szFileName[ 0 ] )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, "__DBOPENSDF" );
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
         hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, "__DBOPENSDF" );
         return;
      }
   }

   errCode = hb_rddOpenTable( szFileName, szDriver,
                              fCurrArea ? hb_rddGetCurrentWorkAreaNumber() : 0,
                              szAlias, TRUE, TRUE,
                              szCpId, ulConnection,
                              pStruct, pDelim );

   if( !fKeepOpen && errCode == SUCCESS )
      hb_rddReleaseCurrentArea();

   hb_retl( errCode == SUCCESS );
}


HB_FUNC( DBDELETE )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_DELETE( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBDELETE" );
}

HB_FUNC( DBFILTER )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_GOBOTTOM( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBGOBOTTOM" );
}

HB_FUNC( DBGOTO )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PHB_ITEM pItem = hb_param( 1, HB_IT_ANY );
      if( !pItem )
         hb_errRT_DBCMD( EG_ARG, EDBCMD_NOVAR, NULL, "DBGOTO" );
      else
         SELF_GOTOID( pArea, pItem );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBGOTO" );
}

HB_FUNC( DBGOTOP )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_GOTOP( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBGOTOP" );
}

HB_FUNC( __DBLOCATE )
{
   DBSCOPEINFO dbScopeInfo;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
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
      dbScopeInfo.fBackward         = FALSE;

      if( SELF_SETLOCATE( pArea, &dbScopeInfo ) == SUCCESS )
         SELF_LOCATE( pArea, FALSE );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EG_NOTABLE, NULL, "__DBLOCATE" );
}

HB_FUNC( __DBSETLOCATE )
{
   PHB_ITEM pLocate;
   DBSCOPEINFO pScopeInfo;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_LOCATE( pArea, TRUE );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "__DBCONTINUE" );
}

HB_FUNC( __DBPACK )
{
   PHB_ITEM pBlock, pEvery;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

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
         if( pArea->valResult )
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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
      SELF_RECALL( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBRECALL" );
}

HB_FUNC( DBRLOCK )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBLOCKINFO dbLockInfo;
      dbLockInfo.fResult = FALSE;
      dbLockInfo.itmRecID = hb_param( 1, HB_IT_ANY );
      if( !dbLockInfo.itmRecID || ISNIL( 1 ) )
         dbLockInfo.uiMethod = DBLM_EXCLUSIVE;
      else
         dbLockInfo.uiMethod = DBLM_MULTIPLE;
      SELF_LOCK( pArea, &dbLockInfo );
      hb_retl( dbLockInfo.fResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBRLOCK" );
}

HB_FUNC( DBRLOCKLIST )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PHB_ITEM pList = hb_itemArrayNew( 0 );
      SELF_INFO( pArea, DBI_GETLOCKARRAY, pList );
      hb_itemRelease( hb_itemReturnForward( pList ) );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBRLOCKLIST" );

}

HB_FUNC( DBRUNLOCK )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_UNLOCK( pArea, hb_param( 1, HB_IT_ANY ) );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBRUNLOCK" );
}

HB_FUNC( DBSEEK )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      if( !ISNIL( 1 ) )
      {
         PHB_ITEM pKey = hb_param( 1, HB_IT_ANY );
         BOOL bSoftSeek = ISLOG( 2 ) ? ( BOOL ) hb_parl( 2 ) : hb_set.HB_SET_SOFTSEEK;
         BOOL bFindLast = ISLOG( 3 ) ? hb_parl( 3 ) : FALSE, fFound = FALSE;
         if( SELF_SEEK( pArea, bSoftSeek, pKey, bFindLast ) == SUCCESS )
         {
            if( SELF_FOUND( pArea, &fFound ) != SUCCESS )
               fFound = FALSE;
         }
         hb_retl( fFound );
      }
      else
         hb_errRT_DBCMD( EG_ARG, EDBCMD_SEEK_BADPARAMETER, NULL, "DBSEEK" );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBSEEK" );
}

HB_FUNC( DBSELECTAREA )
{

   if( ISCHAR( 1 ) )
   {
      hb_rddSelectWorkAreaAlias( hb_parc( 1 ) );
   }
   else
   {
      LONG lNewArea = hb_parnl( 1 );

      /*
       * NOTE: lNewArea >= HARBOUR_MAX_RDD_AREA_NUM used intentionally
       * In Clipper area 65535 is reserved for "M" alias [druzus]
       */
      if( lNewArea < 1 || lNewArea >= HARBOUR_MAX_RDD_AREA_NUM )
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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PHB_ITEM pFound = hb_param( 1, HB_IT_LOGICAL );
      if( pFound )
         pArea->fFound = hb_itemGetL( pFound );
   }
}

HB_FUNC( DBSETFILTER )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PHB_ITEM pBlock, pText;
      DBFILTERINFO pFilterInfo;
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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
      SELF_SKIP( pArea, ISNUM( 1 ) ? hb_parnl( 1 ) : 1 );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBSKIP" );
}

HB_FUNC( DBSTRUCT )
{
   PHB_ITEM pStruct = hb_itemArrayNew( 0 );
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
      hb_tblStructure( pArea, pStruct );
   hb_itemRelease( hb_itemReturn( pStruct ) );
}

HB_FUNC( DBTABLEEXT )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   PHB_ITEM pItem = hb_itemPutC( NULL, "" );

   if( !pArea )
   {
      LPRDDNODE pRddNode;
      USHORT uiRddID;
      pRddNode = hb_rddFindNode( hb_rddDefaultDrv( NULL ), &uiRddID );
      if( pRddNode )
      {
         pArea = ( AREAP ) hb_rddNewAreaNode( pRddNode, uiRddID );
         if( pArea )
         {
            SELF_INFO( pArea, DBI_TABLEEXT, pItem );
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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_UNLOCK( pArea, NULL );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBUNLOCK" );
}

HB_FUNC( DBUNLOCKALL )
{
   hb_rddUnLockAll();
}

HB_FUNC( DBUSEAREA )
{
   hb_retl( hb_rddOpenTable( hb_parc( 3 ), hb_parc( 2 ),
         hb_parl( 1 ) ? 0 : hb_rddGetCurrentWorkAreaNumber(),
         hb_parc( 4 ), ISLOG( 5 ) ? hb_parl( 5 ) : !hb_set.HB_SET_EXCLUSIVE,
         hb_parl( 6 ), hb_parc( 7 ), hb_parnl( 8 ), NULL, NULL ) == SUCCESS );
}

HB_FUNC( __DBZAP )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_ZAP( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "__DBZAP" );
}

HB_FUNC( DELETED )
{
   BOOL bDeleted = FALSE;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_DELETED( pArea, &bDeleted );
   hb_retl( bDeleted );
}

HB_FUNC( EOF )
{
   BOOL bEof = TRUE;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_EOF( pArea, &bEof );
   hb_retl( bEof );
}

HB_FUNC( FCOUNT )
{
   USHORT uiFields = 0;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_FIELDCOUNT( pArea, &uiFields );
   hb_retni( uiFields );
}

HB_FUNC( FIELDDEC )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      USHORT uiIndex;

      if( ( uiIndex = hb_parni( 1 ) ) > 0 )
      {
         PHB_ITEM pItem = hb_itemNew( NULL );

         if( SELF_FIELDINFO( pArea, uiIndex, DBS_DEC, pItem ) == SUCCESS )
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
   PHB_ITEM pItem = hb_itemNew( NULL );
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   USHORT uiField = hb_parni( 1 );

   if( pArea && uiField )
   {
      SELF_GETVALUE( pArea, uiField, pItem );
   }

   hb_itemReturnForward( pItem );
   hb_itemRelease( pItem );
}

HB_FUNC( FIELDLEN )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

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
   char * szName;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   USHORT uiFields, uiIndex = hb_parni( 1 );

   if( pArea && uiIndex )
   {
      if( SELF_FIELDCOUNT( pArea, &uiFields ) == SUCCESS &&
          uiIndex <= uiFields )
      {
         szName = ( char * ) hb_xgrab( pArea->uiMaxFieldNameLength + 1 );
         szName[ 0 ] = '\0';
         SELF_FIELDNAME( pArea, uiIndex, szName );
         hb_retcAdopt( szName );
         return;
      }
      /* This is not Clipper compatible! - David G. Holm <dholm@jsd-llc.com>
       *
      hb_errRT_DBCMD( EG_ARG, EDBCMD_FIELDNAME_BADPARAMETER, NULL, "FIELDNAME" );
       */
   }
   hb_retc( NULL );
}

HB_FUNC( FIELDPOS )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea && hb_parclen( 1 ) > 0 )
      hb_retni( hb_rddFieldIndex( pArea, hb_parc( 1 ) ) );
   else
      hb_retni( 0 );
}

HB_FUNC( FIELDPUT )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      USHORT uiIndex = hb_parni( 1 );
      if( uiIndex > 0 )
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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBLOCKINFO dbLockInfo;
      dbLockInfo.fResult = FALSE;
      dbLockInfo.itmRecID = NULL;
      dbLockInfo.uiMethod = DBLM_FILE;
      SELF_LOCK( pArea, &dbLockInfo );
      hb_retl( dbLockInfo.fResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "FLOCK" );
}

HB_FUNC( FOUND )
{
   BOOL bFound = FALSE;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_FOUND( pArea, &bFound );
   hb_retl( bFound );
}

HB_FUNC( HEADER )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pInfo;
      memset( &pInfo, 0, sizeof( DBORDERINFO ) );
      pInfo.itmResult = hb_itemPutNI( NULL, 0 );
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
   ULONG ulRecCount = 0;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_RECCOUNT( pArea, &ulRecCount );

   hb_retnl( ulRecCount );
}

HB_FUNC( LOCK )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBLOCKINFO dbLockInfo;
      dbLockInfo.fResult = FALSE;
      dbLockInfo.itmRecID = NULL;
      dbLockInfo.uiMethod = DBLM_EXCLUSIVE;
      SELF_LOCK( pArea, &dbLockInfo );
      hb_retl( dbLockInfo.fResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "LOCK" );
}

HB_FUNC( LUPDATE )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

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
   hb_retl( hb_rddGetNetErr() );

   if( ISLOG( 1 ) )
      hb_rddSetNetErr( hb_parl( 1 ) );
}

HB_FUNC( ORDBAGEXT )
{
   DBORDERINFO pInfo;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   memset( &pInfo, 0, sizeof( DBORDERINFO ) );
   pInfo.itmResult = hb_itemPutC( NULL, "" );
   if( !pArea )
   {
      LPRDDNODE pRddNode;
      USHORT uiRddID;
      pRddNode = hb_rddFindNode( hb_rddDefaultDrv( NULL ), &uiRddID );
      if( pRddNode )
      {
         pArea = ( AREAP ) hb_rddNewAreaNode( pRddNode, uiRddID );
         if( pArea )
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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );

      pOrderInfo.itmOrder = hb_param( 1, HB_IT_ANY );
      if( pOrderInfo.itmOrder && !HB_IS_STRING( pOrderInfo.itmOrder ) )
      {
         if( HB_IS_NIL( pOrderInfo.itmOrder ) )
            pOrderInfo.itmOrder = NULL;
         else if( HB_IS_NUMERIC( pOrderInfo.itmOrder ) )
         {
            if( hb_itemGetNI( pOrderInfo.itmOrder ) == 0 )
               pOrderInfo.itmOrder = NULL;
         }
         else
         {
            hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "ORDBAGNAME" );
            return;
         }
      }
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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      LPDBORDERCONDINFO lpdbOrdCondInfo;
      PHB_ITEM pItem;

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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERCREATEINFO dbOrderInfo;
      DBCONSTRAINTINFO dbConstrInfo;

      dbOrderInfo.lpdbOrdCondInfo = pArea->lpdbOrdCondInfo;
      dbOrderInfo.abBagName = ( BYTE * ) hb_parcx( 1 );
      dbOrderInfo.atomBagName = ( BYTE * ) hb_parcx( 2 );
      dbOrderInfo.itmOrder = NULL;
      dbOrderInfo.fUnique = ISLOG( 5 ) ? ( BOOL ) hb_parl( 5 ) : hb_set.HB_SET_UNIQUE;
      dbOrderInfo.abExpr = hb_param( 3, HB_IT_STRING );
      if( ( ( dbOrderInfo.abBagName == NULL || strlen( ( char * ) dbOrderInfo.abBagName ) == 0 ) &&
            ( dbOrderInfo.atomBagName == NULL || strlen( ( char * ) dbOrderInfo.atomBagName ) == 0 ) ) ||
          !dbOrderInfo.abExpr )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "ORDCREATE" );
         return;
      }
      dbOrderInfo.itmCobExpr = hb_param( 4, HB_IT_BLOCK );

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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_ANY );
      if( pOrderInfo.itmOrder && !HB_IS_STRING( pOrderInfo.itmOrder ) )
      {
         if( HB_IS_NIL( pOrderInfo.itmOrder ) )
            pOrderInfo.itmOrder = NULL;
         else if( HB_IS_NUMERIC( pOrderInfo.itmOrder ) )
         {
            if( hb_itemGetNI( pOrderInfo.itmOrder ) == 0 )
               pOrderInfo.itmOrder = NULL;
         }
         else
         {
            hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "ORDFOR" );
            return;
         }
      }
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      pOrderInfo.itmNewVal = hb_param( 3, HB_IT_STRING );
      pOrderInfo.itmResult = hb_itemPutC( NULL, "" );
      pOrderInfo.itmCobExpr = NULL;
      pOrderInfo.fAllTags = FALSE;
      SELF_ORDINFO( pArea, DBOI_CONDITION, &pOrderInfo );
      hb_itemReturn( pOrderInfo.itmResult );
      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDFOR" );
}

HB_FUNC( ORDKEY )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_ANY );
      if( pOrderInfo.itmOrder && !HB_IS_STRING( pOrderInfo.itmOrder ) )
      {
         if( HB_IS_NIL( pOrderInfo.itmOrder ) )
            pOrderInfo.itmOrder = NULL;
         else if( HB_IS_NUMERIC( pOrderInfo.itmOrder ) )
         {
            if( hb_itemGetNI( pOrderInfo.itmOrder ) == 0 )
               pOrderInfo.itmOrder = NULL;
         }
         else
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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
      SELF_SKIPRAW( pArea, ISNUM( 1 ) ? hb_parnl( 1 ) : 1 );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDSKIPRAW" );
}


HB_FUNC( ORDSKIPUNIQUE )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      char * szPatern = hb_parc( 1 );

      if( szPatern )
      {
         BOOL fCont = hb_parl( 2 ), fBack = hb_parl( 3 ), fFound = FALSE;
         DBORDERINFO OrderInfo;
         ERRCODE errCode = SUCCESS;

         memset( &OrderInfo, 0, sizeof( DBORDERINFO ) );
         OrderInfo.itmResult = hb_itemNew( NULL );

         if( !fCont )
         {
            char * szKey;

            if( fBack )
               errCode = SELF_GOBOTTOM( pArea );
            else
               errCode = SELF_GOTOP( pArea );

            if( errCode == SUCCESS )
            {
               errCode = SELF_ORDINFO( pArea, DBOI_KEYVAL, &OrderInfo );
               if( errCode == SUCCESS )
               {
                  szKey = hb_itemGetCPtr( OrderInfo.itmResult );
                  fFound = hb_strMatchWild( szKey, szPatern );
               }
            }
         }
         if( !fFound && errCode == SUCCESS )
         {
            OrderInfo.itmNewVal = hb_param( 1, HB_IT_STRING );
            if( SELF_ORDINFO( pArea, fBack ? DBOI_SKIPWILDBACK : DBOI_SKIPWILD,
                          &OrderInfo ) == SUCCESS )
               fFound = hb_itemGetL( OrderInfo.itmResult );
         }
         hb_itemRelease( OrderInfo.itmResult );
         hb_retl( fFound );
      }
      else
         hb_errRT_DBCMD( EG_ARG, EDBCMD_DBFILEPUTBADPARAMETER, NULL, "ORDWILDSEEK" );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDWILDSEEK" );
}
#endif

HB_FUNC( ORDLISTADD )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      ERRCODE errCode;

      /* Clipper clears NETERR flag when table is open */
      hb_rddSetNetErr( FALSE );

      memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
      pOrderInfo.atomBagName = hb_param( 1, HB_IT_STRING );
      pOrderInfo.itmOrder    = hb_param( 2, HB_IT_STRING );

      if( !pOrderInfo.atomBagName )
      {
         if( !ISNIL( 1 ) )
            hb_errRT_DBCMD( EG_ARG, EDBCMD_ORDLSTADD_BADPARAMETER, NULL, "ORDLISTADD" );
         return;
      }

      pOrderInfo.itmResult = hb_itemNew( NULL );

      errCode = SELF_ORDLSTADD( pArea, &pOrderInfo );

      /*
       * Warning: this is not Clipper compatible. NETERR() should be set by
       * error handler not here
       */
      if( errCode != SUCCESS )
         hb_rddSetNetErr( TRUE );

      hb_itemRelease( hb_itemReturn( pOrderInfo.itmResult ) );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDLISTADD" );
}

HB_FUNC( ORDLISTCLEAR )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_ORDLSTCLEAR( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDLISTCLEAR" );
}

HB_FUNC( ORDLISTREBUILD )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_ORDLSTREBUILD( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "ORDLISTREBUILD" );
}

HB_FUNC( ORDNAME )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_ANY );
      if( pOrderInfo.itmOrder )
      {
         if( HB_IS_NIL( pOrderInfo.itmOrder ) )
            pOrderInfo.itmOrder = NULL;
         else if( HB_IS_NUMERIC( pOrderInfo.itmOrder ) )
         {
            if( hb_itemGetNI( pOrderInfo.itmOrder ) == 0 )
               pOrderInfo.itmOrder = NULL;
         }
         else
         {
            hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, "ORDNAME" );
            return;
         }
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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( DBORDERINFO ) );
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      if( !pOrderInfo.itmOrder && ! ISNIL( 1 ) )
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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pInfo;
      memset( &pInfo, 0, sizeof( DBORDERINFO ) );
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
   hb_itemRelease( hb_itemReturnForward( hb_rddList( hb_parni( 1 ) ) ) );
}

HB_FUNC( RDDNAME )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      char pBuffer[ HARBOUR_MAX_RDD_DRIVERNAME_LENGTH + 1 ];
      pBuffer[ 0 ] = '\0';
      SELF_SYSNAME( pArea, ( BYTE * ) pBuffer );
      hb_retc( pBuffer );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "RDDNAME" );
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
   HB_FUNC_EXEC( LASTREC );
}

HB_FUNC( RECNO )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBLOCKINFO dbLockInfo;
      dbLockInfo.fResult = FALSE;
      dbLockInfo.itmRecID = NULL;
      dbLockInfo.uiMethod = DBLM_EXCLUSIVE;
      SELF_LOCK( pArea, &dbLockInfo );
      hb_retl( dbLockInfo.fResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "RLOCK" );
}

HB_FUNC( SELECT )
{
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
   hb_retl( hb_rddGetCurrentWorkAreaPointer() != NULL );
}

HB_FUNC( RDDSETDEFAULT )
{
   hb_retc( hb_rddDefaultDrv( NULL ) );

   if( hb_parclen( 1 ) > 0 )
   {
      if( ! hb_rddDefaultDrv( hb_parc( 1 ) ) )
         hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, "RDDSETDEFAULT" );
   }
}

HB_FUNC( DBSETDRIVER )
{
   hb_retc( hb_rddDefaultDrv( NULL ) );

   if( hb_parclen( 1 ) > 0 )
   {
      if( ! hb_rddDefaultDrv( hb_parc( 1 ) ) )
         hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, "DBSETDRIVER" );
   }
}

HB_FUNC( ORDSCOPE )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pInfo;
      USHORT uiAction;
      int iScope = hb_parni( 1 );

      memset( &pInfo, 0, sizeof( DBORDERINFO ) );
      pInfo.itmResult = hb_itemNew( NULL );
      if( iScope == 2 )
      {
         if( hb_pcount() > 1 && !ISNIL( 2 ) )
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
   char szExprBuff[ HARBOUR_MAX_RDD_RELTEXT_LENGTH + 1 ];
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   szExprBuff[ 0 ] = 0;
   if( pArea )
      SELF_RELTEXT( pArea, hb_parni(1), szExprBuff ) ;

   hb_retc( szExprBuff );
}

HB_FUNC( DBRSELECT )  /* (<nRelation>) --> nWorkArea */
{
   USHORT uiWorkArea = 0;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_RELAREA( pArea, hb_parni(1), &uiWorkArea );

   hb_retni( uiWorkArea );
}

HB_FUNC( DBCLEARRELATION )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_CLEARREL( pArea );
}

HB_FUNC( DBSETRELATION )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

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
            return;
         uiChildArea = hb_rddGetCurrentWorkAreaNumber();
         hb_rddSelectWorkAreaNumber( uiArea );
      }

      pChildArea = uiChildArea ? ( AREAP ) hb_rddGetWorkAreaPointer( uiChildArea ) : NULL;

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
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBSETRELATION" );
}

HB_FUNC( __DBARRANGE )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

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
      dbSortInfo.dbtri.dbsci.fBackward = FALSE;
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
      dbSortInfo.dbtri.lpaDest = ( AREAP ) hb_rddGetWorkAreaPointer( uiNewArea );
      /* TODO: check what Clipper does when lpaDest == NULL or lpaDest == lpaSource */

      if( dbSortInfo.uiItemCount == 0 )
         SELF_TRANS( pArea, &dbSortInfo.dbtri );
      else
         SELF_SORT( pArea, &dbSortInfo );

      /* Free items */
      if( dbSortInfo.lpdbsItem )
         hb_xfree( dbSortInfo.lpdbsItem );
      if( dbSortInfo.dbtri.lpTransItems )
         hb_xfree( dbSortInfo.dbtri.lpTransItems );
   }
}

#ifdef HB_COMPAT_C53

HB_FUNC( DBINFO )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PHB_ITEM pType = hb_param( 1 , HB_IT_NUMERIC );
      if( pType )
      {
         DBORDERINFO pOrderInfo;

         /* atomBagName may be NIL */
         pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
         if( !pOrderInfo.atomBagName )
            pOrderInfo.atomBagName = hb_param( 2, HB_IT_NUMERIC );

         pOrderInfo.itmOrder = hb_param( 3, HB_IT_STRING );
         if( !pOrderInfo.itmOrder )
            pOrderInfo.itmOrder = hb_param( 3, HB_IT_NUMERIC );

         pOrderInfo.itmNewVal = hb_param( 4 , HB_IT_ANY );
         pOrderInfo.itmResult = hb_itemNew( NULL );
         pOrderInfo.itmCobExpr = NULL;
         pOrderInfo.fAllTags = FALSE;
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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

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
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

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
      }
      else
         hb_errRT_DBCMD( EG_ARG, EDBCMD_DBFILEGETBADPARAMETER, NULL, "DBFILEGET" );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBFILEGET" );
}

HB_FUNC( DBFILEPUT )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

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
      }
      else
         hb_errRT_DBCMD( EG_ARG, EDBCMD_DBFILEPUTBADPARAMETER, NULL, "DBFILEPUT" );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "DBFILEPUT" );
}
#endif

/*******************************************/
/* here we have the NEW RDD level functions DBDROP, DBEXISTS, RDDINFO */
HB_FUNC( DBDROP )
{
   LPRDDNODE  pRDDNode;
   USHORT     uiRddID;
   const char * szDriver;

   szDriver = hb_parc( 3 );
   if( !szDriver ) /* no VIA RDD parameter, use default */
   {
      szDriver = hb_rddDefaultDrv( NULL );
   }

   pRDDNode = hb_rddFindNode( szDriver, &uiRddID );  /* find the RDDNODE */

   if( !pRDDNode )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, "DBDROP" );
      return;
   }

   hb_retl( SELF_DROP( pRDDNode, hb_param( 1, HB_IT_STRING ),
                                 hb_param( 2, HB_IT_STRING ) ) == SUCCESS );
}

HB_FUNC( DBEXISTS )
{
   LPRDDNODE  pRDDNode;
   USHORT     uiRddID;
   const char * szDriver;

   szDriver = hb_parc( 3 );
   if( !szDriver ) /* no VIA RDD parameter, use default */
   {
      szDriver = hb_rddDefaultDrv( NULL );
   }

   pRDDNode = hb_rddFindNode( szDriver, &uiRddID );  /* find the RDD */

   if( !pRDDNode )
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
   const char * szDriver;

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

/* __dbTrans( nDstArea, aFieldsStru, bFor, bWhile, nNext, nRecord, lRest ) */
HB_FUNC( __DBTRANS )
{
   if( ISNUM( 1 ) )
   {
      USHORT uiSrcArea, uiDstArea;
      AREAP pSrcArea, pDstArea;

      uiSrcArea = hb_rddGetCurrentWorkAreaNumber();
      pSrcArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
      uiDstArea = hb_parni( 1 );
      hb_rddSelectWorkAreaNumber( uiDstArea );
      pDstArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

      if( pSrcArea && pDstArea )
      {
         DBTRANSINFO dbTransInfo;
         PHB_ITEM pFields = hb_param( 2, HB_IT_ARRAY );
         ERRCODE errCode;

         memset( &dbTransInfo, 0, sizeof( DBTRANSINFO ) );
         errCode = hb_dbTransStruct( pSrcArea, pDstArea, &dbTransInfo,
                                     NULL, pFields );
         if( errCode == SUCCESS )
         {
            hb_rddSelectWorkAreaNumber( dbTransInfo.lpaSource->uiArea );

            dbTransInfo.dbsci.itmCobFor   = hb_param( 3, HB_IT_BLOCK );
            dbTransInfo.dbsci.lpstrFor    = NULL;
            dbTransInfo.dbsci.itmCobWhile = hb_param( 4, HB_IT_BLOCK );
            dbTransInfo.dbsci.lpstrWhile  = NULL;
            dbTransInfo.dbsci.lNext       = hb_param( 5, HB_IT_NUMERIC );
            dbTransInfo.dbsci.itmRecID    = ISNIL( 6 ) ? NULL : hb_param( 6, HB_IT_ANY );
            dbTransInfo.dbsci.fRest       = hb_param( 7, HB_IT_LOGICAL );

            dbTransInfo.dbsci.fIgnoreFilter     = TRUE;
            dbTransInfo.dbsci.fIncludeDeleted   = TRUE;
            dbTransInfo.dbsci.fLast             = FALSE;
            dbTransInfo.dbsci.fIgnoreDuplicates = FALSE;
            dbTransInfo.dbsci.fBackward         = FALSE;

            errCode = SELF_TRANS( dbTransInfo.lpaSource, &dbTransInfo );
         }

         if( dbTransInfo.lpTransItems )
            hb_xfree( dbTransInfo.lpTransItems );

         hb_retl( errCode == SUCCESS );
      }
      else
         hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "__DBTRANS" );

      hb_rddSelectWorkAreaNumber( uiSrcArea );
   }
   else
      hb_errRT_DBCMD( EG_ARG, EDBCMD_USE_BADPARAMETER, NULL, "__DBTRANS" );
}

HB_FUNC( __DBAPP )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
      hb_retl( SUCCESS == hb_rddTransRecords( pArea,
               hb_parc( 1 ),                     /* file name */
               hb_parc( 8 ),                     /* RDD */
               hb_parnl( 9 ),                    /* connection */
               hb_param( 2, HB_IT_ARRAY ),       /* Fields */
               FALSE,                            /* Export? */
               hb_param( 3, HB_IT_BLOCK ),       /* cobFor */
               NULL,                             /* lpStrFor */
               hb_param( 4, HB_IT_BLOCK ),       /* cobWhile */
               NULL,                             /* lpStrWhile */
               hb_param( 5, HB_IT_NUMERIC ),     /* Next */
               ISNIL( 6 ) ? NULL : hb_param( 6, HB_IT_ANY ),   /* RecID */
               hb_param( 7, HB_IT_LOGICAL ),     /* Rest */
               hb_parc( 10 ),                    /* Codepage */
               hb_param( 11, HB_IT_ANY ) ) );    /* Delimiter */
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "APPEND FROM" );
}

HB_FUNC( __DBCOPY )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   if( pArea )
      hb_retl( SUCCESS == hb_rddTransRecords( pArea,
               hb_parc( 1 ),                     /* file name */
               hb_parc( 8 ),                     /* RDD */
               hb_parnl( 9 ),                    /* connection */
               hb_param( 2, HB_IT_ARRAY ),       /* Fields */
               TRUE,                             /* Export? */
               hb_param( 3, HB_IT_BLOCK ),       /* cobFor */
               NULL,                             /* lpStrFor */
               hb_param( 4, HB_IT_BLOCK ),       /* cobWhile */
               NULL,                             /* lpStrWhile */
               hb_param( 5, HB_IT_NUMERIC ),     /* Next */
               ISNIL( 6 ) ? NULL : hb_param( 6, HB_IT_ANY ),   /* RecID */
               hb_param( 7, HB_IT_LOGICAL ),     /* Rest */
               hb_parc( 10 ),                    /* Codepage */
               hb_param( 11, HB_IT_ANY ) ) );    /* Delimiter */
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "COPY TO" );
}

HB_FUNC( __RDDGETTEMPALIAS )
{
   char szAliasTmp[ HARBOUR_MAX_RDD_ALIAS_LENGTH + 1 ];

   if( hb_rddGetTempAlias( szAliasTmp ) == SUCCESS )
      hb_retc( szAliasTmp );
   else
      hb_ret();
}

#ifdef HB_COMPAT_XPP
HB_FUNC( DBSKIPPER )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      LONG nSkipped    = 0;
      LONG nRecs       = 1;
      BOOL bBEof       = TRUE;
      if( hb_pcount() > 0 )
      {
         nRecs = hb_parnl( 1 ) ;
      }

      if( SELF_EOF( pArea, &bBEof ) != SUCCESS )
         return;

      if( nRecs == 0 )
      {
         if( SELF_SKIP( pArea, 0 ) != SUCCESS )
            return;
      }
      else if( nRecs > 0 && !bBEof  )
      {
         while( nSkipped < nRecs )
         {
            if( SELF_SKIP( pArea, 1 ) != SUCCESS )
               return;
            if( SELF_EOF( pArea, &bBEof ) != SUCCESS )
               return;
            if( bBEof )
            {
               if( SELF_SKIP( pArea, -1 ) != SUCCESS )
                  return;
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
            if( SELF_SKIP( pArea, -1 ) != SUCCESS )
               return;
            if( SELF_BOF( pArea, &bBEof ) != SUCCESS )
               return;
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
