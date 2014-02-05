/*
 * Harbour Project source code:
 * Base RDD module
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
 * Copyright 2004-2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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
 * hb_rddIterateWorkAreas()
 * hb_rddGetTempAlias()
 * hb_rddGetTempAlias()
 *
 */

#include "hbapi.h"
#include "hbapirdd.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbset.h"

/* The 5-th parameter is Harbour extensions */
HB_FUNC( AFIELDS )
{
   HB_USHORT uiFields, uiCount;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   PHB_ITEM pName = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pType = hb_param( 2, HB_IT_ARRAY );
   PHB_ITEM pLen = hb_param( 3, HB_IT_ARRAY );
   PHB_ITEM pDec = hb_param( 4, HB_IT_ARRAY );

#ifdef DBS_FLAG
   PHB_ITEM pFlags = hb_param( 5, HB_IT_ARRAY );
#else
   PHB_ITEM pFlags = NULL;
#endif

   if( ! pArea || ( ! pName && ! pType && ! pLen && ! pDec && ! pFlags ) )
   {
      hb_retni( 0 );
      return;
   }

   if( SELF_FIELDCOUNT( pArea, &uiFields ) != HB_SUCCESS )
      return;

   if( pName )
   {
      HB_USHORT uiArrayLen = ( HB_USHORT ) hb_arrayLen( pName );
      if( uiArrayLen < uiFields )
         uiFields = uiArrayLen;
   }
   if( pType )
   {
      HB_USHORT uiArrayLen = ( HB_USHORT ) hb_arrayLen( pType );
      if( uiArrayLen < uiFields )
         uiFields = uiArrayLen;
   }
   if( pLen )
   {
      HB_USHORT uiArrayLen = ( HB_USHORT ) hb_arrayLen( pLen );
      if( uiArrayLen < uiFields )
         uiFields = uiArrayLen;
   }
   if( pDec )
   {
      HB_USHORT uiArrayLen = ( HB_USHORT ) hb_arrayLen( pDec );
      if( uiArrayLen < uiFields )
         uiFields = uiArrayLen;
   }

   if( pFlags )
   {
      HB_USHORT uiArrayLen = ( HB_USHORT ) hb_arrayLen( pFlags );
      if( uiArrayLen < uiFields )
         uiFields = uiArrayLen;
   }

   if( pName )
   {
      for( uiCount = 1; uiCount <= uiFields; ++uiCount )
      {
         if( SELF_FIELDINFO( pArea, uiCount, DBS_NAME, hb_arrayGetItemPtr( pName, uiCount ) ) != HB_SUCCESS )
            return;
      }
   }
   if( pType )
   {
      for( uiCount = 1; uiCount <= uiFields; ++uiCount )
      {
         if( SELF_FIELDINFO( pArea, uiCount, DBS_TYPE, hb_arrayGetItemPtr( pType, uiCount ) ) != HB_SUCCESS )
            return;
      }
   }
   if( pLen )
   {
      for( uiCount = 1; uiCount <= uiFields; ++uiCount )
      {
         if( SELF_FIELDINFO( pArea, uiCount, DBS_LEN, hb_arrayGetItemPtr( pLen, uiCount ) ) != HB_SUCCESS )
            return;
      }
   }
   if( pDec )
   {
      for( uiCount = 1; uiCount <= uiFields; ++uiCount )
      {
         if( SELF_FIELDINFO( pArea, uiCount, DBS_DEC, hb_arrayGetItemPtr( pDec, uiCount ) ) != HB_SUCCESS )
            return;
      }
   }
#ifdef DBS_FLAG
   if( pFlags )
   {
      for( uiCount = 1; uiCount <= uiFields; ++uiCount )
      {
         if( SELF_FIELDINFO( pArea, uiCount, DBS_FLAG, hb_arrayGetItemPtr( pFlags, uiCount ) ) != HB_SUCCESS )
            return;
      }
   }
#endif

   hb_retni( uiFields );
}

HB_FUNC( ALIAS )
{
   int iArea;
   AREAP pArea;

   iArea = hb_parni( 1 );
   pArea = ( AREAP ) hb_rddGetWorkAreaPointer( iArea );
   if( pArea )
   {
      char szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];

      if( SELF_ALIAS( pArea, szAlias ) == HB_SUCCESS )
      {
         hb_retc( szAlias );
         return;
      }
   }
   hb_retc_null();
}

HB_FUNC( DBEVAL )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBEVALINFO pEvalInfo;
      memset( &pEvalInfo, 0, sizeof( pEvalInfo ) );
      pEvalInfo.itmBlock = hb_param( 1, HB_IT_BLOCK );
      if( ! pEvalInfo.itmBlock )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
         return;
      }

      pEvalInfo.dbsci.itmCobFor = hb_param( 2, HB_IT_BLOCK );
      if( ! pEvalInfo.dbsci.itmCobFor && ! HB_ISNIL( 2 ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
         return;
      }

      pEvalInfo.dbsci.itmCobWhile = hb_param( 3, HB_IT_BLOCK );
      if( ! pEvalInfo.dbsci.itmCobWhile && ! HB_ISNIL( 3 ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
         return;
      }

      pEvalInfo.dbsci.lNext = hb_param( 4, HB_IT_NUMERIC );
      if( ! pEvalInfo.dbsci.lNext && ! HB_ISNIL( 4 ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
         return;
      }

      pEvalInfo.dbsci.itmRecID = hb_param( 5, HB_IT_NUMERIC );
      if( ! pEvalInfo.dbsci.itmRecID && ! HB_ISNIL( 5 ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
         return;
      }

      pEvalInfo.dbsci.fRest = hb_param( 6, HB_IT_LOGICAL );
      if( ! pEvalInfo.dbsci.fRest && ! HB_ISNIL( 6 ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
         return;
      }

      SELF_DBEVAL( pArea, &pEvalInfo );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( DBF )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      char szAlias[ HB_RDD_MAX_ALIAS_LEN + 1 ];

      if( SELF_ALIAS( pArea, szAlias ) == HB_SUCCESS )
      {
         hb_retc( szAlias );
         return;
      }
   }
   hb_retc_null();
}

HB_FUNC( BOF )
{
   HB_BOOL bBof = HB_TRUE;
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
      HB_BOOL bUnLockAll = hb_parldef( 1, 1 );
      HB_ERRCODE errCode;

      /* Clipper clears NETERR flag before APPEND */
      hb_rddSetNetErr( HB_FALSE );
      errCode = SELF_APPEND( pArea, bUnLockAll );
      hb_retl( errCode == HB_SUCCESS );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( DBCLEARFILTER )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_CLEARFILTER( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
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
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( DBCOMMITALL )
{
   hb_rddFlushAll();
}

/*
 * In Clipper the arguments are:
 *    dbCreate( cFile, aStruct, cRDD, lKeepOpen, cAlias, cDelimArg )
 * In Harbour:
 *    dbCreate( cFile, aStruct, cRDD, lKeepOpen, cAlias, cDelimArg, cCodePage, nConnection ) (HB_EXTENSION)
 */
HB_FUNC( DBCREATE )
{
   const char * szFileName, * szAlias, * szDriver, * szCpId;
   HB_USHORT uiSize, uiLen;
   PHB_ITEM pStruct, pFieldDesc, pDelim;
   HB_BOOL fKeepOpen, fCurrArea;
   HB_ULONG ulConnection;

   /*
    * NOTE: 4-th, 5-th and 6-th parameters are undocumented Clipper ones
    * 4-th is boolean flag indicating if file should stay open (any boolean
    *      value will enable this behavior)
    * 5-th is alias - if not given then WA is open without alias
    * 6-th is optional DELIMITED value used by some RDDs like DELIM
    */

   szFileName = hb_parc( 1 );
   pStruct = hb_param( 2, HB_IT_ARRAY );
   szDriver = hb_parc( 3 );
   fKeepOpen = HB_ISLOG( 4 );
   fCurrArea = fKeepOpen && ! hb_parl( 4 );
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
   if( ! pStruct ||
#ifdef HB_CLP_STRICT
       hb_arrayLen( pStruct ) == 0 ||
#endif
       ! szFileName )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, HB_ERR_FUNCNAME );
      return;
   }
   uiLen = ( HB_USHORT ) hb_arrayLen( pStruct );

   for( uiSize = 1; uiSize <= uiLen; ++uiSize )
   {
      pFieldDesc = hb_arrayGetItemPtr( pStruct, uiSize );

      /* Validate items types of fields */
      if( hb_arrayLen( pFieldDesc ) < 4 ||
          ! ( hb_arrayGetType( pFieldDesc, 1 ) & HB_IT_STRING ) ||
          ! ( hb_arrayGetType( pFieldDesc, 2 ) & HB_IT_STRING ) ||
          ! ( hb_arrayGetType( pFieldDesc, 3 ) & HB_IT_NUMERIC ) ||
          ! ( hb_arrayGetType( pFieldDesc, 4 ) & HB_IT_NUMERIC ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, HB_ERR_FUNCNAME );
         return;
      }
   }

   hb_retl( hb_rddCreateTable( szFileName, szDriver,
                               fCurrArea ? ( HB_AREANO ) hb_rddGetCurrentWorkAreaNumber() : 0,
                               szAlias, fKeepOpen,
                               szCpId, ulConnection,
                               pStruct, pDelim ) == HB_SUCCESS );
}

/*
 *    hb_dbCreateTemp( <cAlias>, <aStruct>, <cRDD>, <cCodePage>, <nConnection> ) -> <lSuccess>
 */
HB_FUNC( HB_DBCREATETEMP )
{
   const char * szAlias, * szDriver, * szCpId;
   HB_USHORT uiSize, uiLen;
   PHB_ITEM pStruct, pFieldDesc;
   HB_ULONG ulConnection;

   szAlias = hb_parc( 1 );
   pStruct = hb_param( 2, HB_IT_ARRAY );
   szDriver = hb_parc( 3 );
   szCpId = hb_parc( 4 );
   ulConnection = hb_parnl( 5 );

   /*
    * Clipper allows to use empty struct array for RDDs which does not
    * support fields, f.e.: DBFBLOB in CL5.3
    * In CL5.3 it's also possible to create DBF file without fields.
    * if some RDD wants to block it then they should serve it in lower
    * level, [druzus]
    */
   if( ! szAlias || ! pStruct
#ifdef HB_CLP_STRICT
       || hb_arrayLen( pStruct ) == 0
#endif
       )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, HB_ERR_FUNCNAME );
      return;
   }
   uiLen = ( HB_USHORT ) hb_arrayLen( pStruct );

   for( uiSize = 1; uiSize <= uiLen; ++uiSize )
   {
      pFieldDesc = hb_arrayGetItemPtr( pStruct, uiSize );

      /* Validate items types of fields */
      if( hb_arrayLen( pFieldDesc ) < 4 ||
          ! ( hb_arrayGetType( pFieldDesc, 1 ) & HB_IT_STRING ) ||
          ! ( hb_arrayGetType( pFieldDesc, 2 ) & HB_IT_STRING ) ||
          ! ( hb_arrayGetType( pFieldDesc, 3 ) & HB_IT_NUMERIC ) ||
          ! ( hb_arrayGetType( pFieldDesc, 4 ) & HB_IT_NUMERIC ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, HB_ERR_FUNCNAME );
         return;
      }
   }

   hb_retl( hb_rddCreateTableTemp( szDriver, szAlias,
                                   szCpId, ulConnection,
                                   pStruct ) == HB_SUCCESS );
}

/*
 * I'm not sure if lKeepOpen open works exactly like in dbCreate(), I haven't
 * tested it with Clipper yet. If it doesn't then please inform me about it
 * and I'll update the code. [druzus]
 */

/* NOTE: The created table will be kept open if lOpenMode parameter
         is of logical type. If .T. it will be opened in a new workarea,
         if .F. it will be opened in the current one. */
/* NOTE: Has an identical parameter list with dbCreate() */

/* __dbOpenSDF( cFile, aStruct, cRDD, lKeepOpen, cAlias, cDelimArg, cCodePage, nConnection ) */
HB_FUNC( __DBOPENSDF )
{
   const char * szFileName, * szAlias, * szDriver, * szCpId;
   HB_USHORT uiSize, uiLen;
   PHB_ITEM pStruct, pFieldDesc, pDelim;
   HB_BOOL fKeepOpen, fCurrArea;
   HB_ULONG ulConnection;
   HB_ERRCODE errCode;

   /*
    * NOTE: 4-th and 5-th parameters are undocumented Clipper ones
    * 4-th is boolean flag indicating if file should stay open and
    * 5-th is alias - if not given then WA is open without alias
    */

   szFileName = hb_parc( 1 );
   pStruct = hb_param( 2, HB_IT_ARRAY );
   szDriver = hb_parc( 3 );
   fKeepOpen = HB_ISLOG( 4 );
   fCurrArea = fKeepOpen && ! hb_parl( 4 );
   szAlias = hb_parc( 5 );
   pDelim = hb_param( 6, HB_IT_ANY );
   szCpId = hb_parc( 7 );
   ulConnection = hb_parnl( 8 );

   if( ! pStruct ||
       hb_arrayLen( pStruct ) == 0 ||
       ! szFileName || ! szFileName[ 0 ] )
   {
      hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, HB_ERR_FUNCNAME );
      return;
   }
   uiLen = ( HB_USHORT ) hb_arrayLen( pStruct );

   for( uiSize = 1; uiSize <= uiLen; ++uiSize )
   {
      pFieldDesc = hb_arrayGetItemPtr( pStruct, uiSize );

      /* Validate items types of fields */
      if( hb_arrayLen( pFieldDesc ) < 4 ||
          ! ( hb_arrayGetType( pFieldDesc, 1 ) & HB_IT_STRING ) ||
          ! ( hb_arrayGetType( pFieldDesc, 2 ) & HB_IT_STRING ) ||
          ! ( hb_arrayGetType( pFieldDesc, 3 ) & HB_IT_NUMERIC ) ||
          ! ( hb_arrayGetType( pFieldDesc, 4 ) & HB_IT_NUMERIC ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, HB_ERR_FUNCNAME );
         return;
      }
   }

   errCode = hb_rddOpenTable( szFileName, szDriver,
                              fCurrArea ? ( HB_AREANO ) hb_rddGetCurrentWorkAreaNumber() : 0,
                              szAlias, HB_TRUE, HB_TRUE,
                              szCpId, ulConnection,
                              pStruct, pDelim );

   if( ! fKeepOpen && errCode == HB_SUCCESS )
      hb_rddReleaseCurrentArea();

   hb_retl( errCode == HB_SUCCESS );
}


HB_FUNC( DBDELETE )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_DELETE( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( DBFILTER )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PHB_ITEM pFilter = hb_itemPutC( NULL, NULL );
      SELF_FILTERTEXT( pArea, pFilter );
      hb_itemReturnRelease( pFilter );
   }
   else
      hb_retc_null();
}

HB_FUNC( DBGOBOTTOM )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_GOBOTTOM( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( DBGOTO )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PHB_ITEM pItem = hb_param( 1, HB_IT_ANY );
      if( ! pItem )
         hb_errRT_DBCMD( EG_ARG, EDBCMD_NOVAR, NULL, HB_ERR_FUNCNAME );
      else
         SELF_GOTOID( pArea, pItem );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( DBGOTOP )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_GOTOP( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
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

      dbScopeInfo.fIgnoreFilter     = HB_TRUE;
      dbScopeInfo.fIncludeDeleted   = HB_TRUE;
      dbScopeInfo.fLast             = HB_FALSE;
      dbScopeInfo.fIgnoreDuplicates = HB_FALSE;
      dbScopeInfo.fBackward         = HB_FALSE;

      if( SELF_SETLOCATE( pArea, &dbScopeInfo ) == HB_SUCCESS )
         SELF_LOCATE( pArea, HB_FALSE );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EG_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( __DBSETLOCATE )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PHB_ITEM pLocate = hb_param( 1, HB_IT_BLOCK );
      if( pLocate )
      {
         DBSCOPEINFO pScopeInfo;
         memset( &pScopeInfo, 0, sizeof( pScopeInfo ) );
         pScopeInfo.itmCobFor = pLocate;
         SELF_SETLOCATE( pArea, &pScopeInfo );
      }
   }
}

HB_FUNC( __DBCONTINUE )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_LOCATE( pArea, HB_TRUE );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
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
         pEvery = hb_param( 2, HB_IT_NUMERIC );
         if( pEvery )
            hb_arraySet( pArea->valResult, 2, pEvery );
         else
            hb_arraySetNI( pArea->valResult, 2, 0 );
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
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( DBRECALL )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_RECALL( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( DBRLOCK )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBLOCKINFO dbLockInfo;
      dbLockInfo.fResult = HB_FALSE;
      dbLockInfo.itmRecID = hb_param( 1, HB_IT_ANY );
      if( ! dbLockInfo.itmRecID || HB_ISNIL( 1 ) )
         dbLockInfo.uiMethod = DBLM_EXCLUSIVE;
      else
         dbLockInfo.uiMethod = DBLM_MULTIPLE;
      SELF_LOCK( pArea, &dbLockInfo );
      hb_retl( dbLockInfo.fResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( DBRLOCKLIST )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PHB_ITEM pList = hb_itemArrayNew( 0 );
      SELF_INFO( pArea, DBI_GETLOCKARRAY, pList );
      hb_itemReturnRelease( pList );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );

}

HB_FUNC( DBRUNLOCK )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_UNLOCK( pArea, hb_param( 1, HB_IT_ANY ) );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( DBSEEK )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      if( ! HB_ISNIL( 1 ) )
      {
         PHB_ITEM pKey = hb_param( 1, HB_IT_ANY );
         HB_BOOL bSoftSeek = HB_ISLOG( 2 ) ? ( HB_BOOL ) hb_parl( 2 ) : hb_setGetSoftSeek();
         HB_BOOL bFindLast = hb_parl( 3 ), fFound = HB_FALSE;
         if( SELF_SEEK( pArea, bSoftSeek, pKey, bFindLast ) == HB_SUCCESS )
         {
            if( SELF_FOUND( pArea, &fFound ) != HB_SUCCESS )
               fFound = HB_FALSE;
         }
         hb_retl( fFound );
      }
      else
         hb_errRT_DBCMD( EG_ARG, EDBCMD_SEEK_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( DBSELECTAREA )
{
   const char * szAlias = hb_parc( 1 );

   if( szAlias )
   {
      hb_rddSelectWorkAreaAlias( szAlias );
      if( hb_rddGetCurrentWorkAreaNumber() == HB_RDD_MAX_AREA_NUM )
         hb_rddSelectFirstAvailable();
   }
   else
   {
      int iNewArea = hb_parni( 1 );

      /*
       * NOTE: lNewArea >= HB_RDD_MAX_AREA_NUM used intentionally
       * In Clipper area 65535 is reserved for "M" alias [druzus]
       */
      if( iNewArea < 1 || iNewArea >= HB_RDD_MAX_AREA_NUM )
      {
         hb_rddSelectFirstAvailable();
      }
      else
      {
         hb_rddSelectWorkAreaNumber( iNewArea );
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
            pFilterInfo.abFilterText = hb_itemPutC( NULL, NULL );
         pFilterInfo.fFilter = HB_TRUE;
         pFilterInfo.lpvCargo = NULL;
         pFilterInfo.fOptimized = HB_FALSE;
         SELF_SETFILTER( pArea, &pFilterInfo );
         if( ! pText )
            hb_itemRelease( pFilterInfo.abFilterText );
      }
      else
      {
         SELF_CLEARFILTER( pArea );
      }
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( DBSKIP )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_SKIP( pArea, hb_parnldef( 1, 1 ) );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( DBSTRUCT )
{
   PHB_ITEM pStruct = hb_itemArrayNew( 0 );
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      hb_tblStructure( pArea, pStruct, DBS_ALEN );
   hb_itemReturnRelease( pStruct );
}

HB_FUNC( DBTABLEEXT )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   PHB_ITEM pItem = hb_itemPutC( NULL, NULL );

   if( ! pArea )
   {
      LPRDDNODE pRddNode;
      HB_USHORT uiRddID;
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
   hb_itemReturnRelease( pItem );
}

HB_FUNC( DBUNLOCK )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_UNLOCK( pArea, NULL );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( DBUNLOCKALL )
{
   hb_rddUnLockAll();
}

/* dbUseArea( [<lNewArea>], [<cDriver>], <cName>, [<xcAlias>], [<lShared>], [<lReadonly>], [<cCodePage>], [<nConnection>] ) -> NIL */

HB_FUNC( DBUSEAREA )
{
   hb_retl( hb_rddOpenTable( hb_parc( 3 ), hb_parc( 2 ),
         hb_parl( 1 ) ? 0 : ( HB_AREANO ) hb_rddGetCurrentWorkAreaNumber(),
         hb_parc( 4 ), HB_ISLOG( 5 ) ? hb_parl( 5 ) : ! hb_setGetExclusive(),
         hb_parl( 6 ), hb_parc( 7 ), hb_parnl( 8 ), NULL, NULL ) == HB_SUCCESS );
}

HB_FUNC( __DBZAP )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_ZAP( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( DELETED )
{
   HB_BOOL bDeleted = HB_FALSE;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_DELETED( pArea, &bDeleted );
   hb_retl( bDeleted );
}

HB_FUNC( EOF )
{
   HB_BOOL bEof = HB_TRUE;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_EOF( pArea, &bEof );
   hb_retl( bEof );
}

HB_FUNC( FCOUNT )
{
   HB_USHORT uiFields = 0;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_FIELDCOUNT( pArea, &uiFields );
   hb_retni( uiFields );
}

HB_FUNC( FIELDGET )
{
   PHB_ITEM pItem = hb_itemNew( NULL );
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_USHORT uiField = ( HB_FIELDNO ) hb_parni( 1 );

   if( pArea && uiField )
   {
      SELF_GETVALUE( pArea, uiField, pItem );
   }

   hb_itemReturnRelease( pItem );
}

HB_FUNC( FIELDNAME )
{
   char * szName;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   HB_USHORT uiFields, uiIndex = ( HB_FIELDNO ) hb_parni( 1 );

   if( pArea && uiIndex )
   {
      if( SELF_FIELDCOUNT( pArea, &uiFields ) == HB_SUCCESS &&
          uiIndex <= uiFields )
      {
         szName = ( char * ) hb_xgrab( pArea->uiMaxFieldNameLength + 1 );
         szName[ 0 ] = '\0';
         SELF_FIELDNAME( pArea, uiIndex, szName );
         hb_retc_buffer( szName );
         return;
      }
      /* This is not Clipper compatible! - David G. Holm <dholm@jsd-llc.com> */
#if 0
      hb_errRT_DBCMD( EG_ARG, EDBCMD_FIELDNAME_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
#endif
   }
   hb_retc_null();
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
      HB_USHORT uiIndex = ( HB_FIELDNO ) hb_parni( 1 );

      if( uiIndex > 0 )
      {
         PHB_ITEM pItem = hb_param( 2, HB_IT_ANY );
         if( pItem && ! HB_IS_NIL( pItem ) )
         {
            if( SELF_PUTVALUE( pArea, uiIndex, pItem ) == HB_SUCCESS )
               hb_itemReturn( pItem );
         }
      }
   }
}

HB_FUNC( FLOCK )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBLOCKINFO dbLockInfo;
      dbLockInfo.fResult  = HB_FALSE;
      dbLockInfo.itmRecID = NULL;
      dbLockInfo.uiMethod = DBLM_FILE;
      SELF_LOCK( pArea, &dbLockInfo );
      hb_retl( dbLockInfo.fResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( FOUND )
{
   HB_BOOL bFound = HB_FALSE;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_FOUND( pArea, &bFound );
   hb_retl( bFound );
}

HB_FUNC( HEADER )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( ! pArea )
      hb_retni( 0 );
   else
   {
      PHB_ITEM pItem = hb_itemNew( NULL );
      SELF_INFO( pArea, DBI_GETHEADERSIZE, pItem );
      hb_itemReturnRelease( pItem );
   }
}

HB_FUNC( INDEXORD )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pInfo;
      memset( &pInfo, 0, sizeof( pInfo ) );
      pInfo.itmResult = hb_itemPutNI( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_NUMBER, &pInfo );
      hb_retni( hb_itemGetNI( pInfo.itmResult ) );
      hb_itemRelease( pInfo.itmResult );
   }
   else
      hb_retni( 0 );
}

/* Same as RecCount() */
HB_FUNC( LASTREC )
{
   HB_ULONG ulRecCount = 0;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_RECCOUNT( pArea, &ulRecCount );

   hb_retnint( ulRecCount );
}

HB_FUNC( LOCK )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBLOCKINFO dbLockInfo;
      dbLockInfo.fResult  = HB_FALSE;
      dbLockInfo.itmRecID = NULL;
      dbLockInfo.uiMethod = DBLM_EXCLUSIVE;
      SELF_LOCK( pArea, &dbLockInfo );
      hb_retl( dbLockInfo.fResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( LUPDATE )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PHB_ITEM pItem = hb_itemNew( NULL );

      SELF_INFO( pArea, DBI_LASTUPDATE, pItem );
      hb_itemReturnRelease( pItem );
   }
   else
      hb_retds( NULL );
}

HB_FUNC( NETERR )
{
   hb_retl( hb_rddGetNetErr() );

   if( HB_ISLOG( 1 ) )
      hb_rddSetNetErr( hb_parl( 1 ) );
}

HB_FUNC( ORDBAGEXT )
{
   DBORDERINFO pInfo;
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   memset( &pInfo, 0, sizeof( pInfo ) );
   pInfo.itmResult = hb_itemPutC( NULL, NULL );
   if( ! pArea )
   {
      LPRDDNODE pRddNode;
      HB_USHORT uiRddID;
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
   hb_itemReturnRelease( pInfo.itmResult );
}

HB_FUNC( ORDBAGNAME )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );

      pOrderInfo.itmOrder = hb_param( 1, HB_IT_ANY );
      if( pOrderInfo.itmOrder && ! HB_IS_STRING( pOrderInfo.itmOrder ) )
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
            hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
            return;
         }
      }
      pOrderInfo.itmResult = hb_itemPutC( NULL, NULL );
      SELF_ORDINFO( pArea, DBOI_BAGNAME, &pOrderInfo );
      hb_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
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
                               hb_strdup( hb_parc( 1 ) ) : NULL;
      pItem = hb_param( 2, HB_IT_BLOCK );
      lpdbOrdCondInfo->itmCobFor = pItem ? hb_itemNew( pItem ) : NULL;

      lpdbOrdCondInfo->fAll = hb_parldef( 3, 1 );

      lpdbOrdCondInfo->abWhile = hb_parclen( 17 ) > 0 ?
                                 hb_strdup( hb_parc( 17 ) ) : NULL;
      pItem = hb_param( 4, HB_IT_BLOCK );
      lpdbOrdCondInfo->itmCobWhile = pItem ? hb_itemNew( pItem ) : NULL;

      pItem = hb_param( 5, HB_IT_BLOCK );
      lpdbOrdCondInfo->itmCobEval = pItem ? hb_itemNew( pItem ) : NULL;

      lpdbOrdCondInfo->lStep         = hb_parnl( 6 );
      lpdbOrdCondInfo->itmStartRecID = HB_ISNIL( 7 ) ? NULL : hb_itemNew( hb_param( 7, HB_IT_ANY ) );
      lpdbOrdCondInfo->lNextCount    = hb_parnl( 8 );
      lpdbOrdCondInfo->itmRecID      = HB_ISNIL( 9 ) ? NULL : hb_itemNew( hb_param( 9, HB_IT_ANY ) );
      lpdbOrdCondInfo->fRest         = hb_parl( 10 );
      lpdbOrdCondInfo->fDescending   = hb_parl( 11 );
      /* 12th parameter is always nil in CL5.3, in CL5.2 it's compound flag */
      lpdbOrdCondInfo->fCompound     = hb_parl( 12 );
      lpdbOrdCondInfo->fAdditive     = hb_parl( 13 );
      lpdbOrdCondInfo->fUseCurrent   = hb_parl( 14 );
      lpdbOrdCondInfo->fCustom       = hb_parl( 15 );
      lpdbOrdCondInfo->fNoOptimize   = hb_parl( 16 );
      /* 18th parameter in [x]Harbour is MEMORY flag added by Alexander for
         DBFNTX, so far it was served in hacked way inside SELF_ORDSETCOND()
         so it was working only if this method was called from ordCondSet()
         function. I also do not like the idea that it was called MEMORY.
         It should be RDD decision how such index will be served on low
         level and it should be IMHO called TEMPORARY - if RDD wants then
         it can make it fully in memory or in temporary file which will
         be removed on index close operation */
      lpdbOrdCondInfo->fTemporary    = hb_parl( 18 );
      /* 19th parameter is CL5.2 USEFILTER parameter which means
         that RDD should respect SET FILTER and SET DELETE flag */
      lpdbOrdCondInfo->fUseFilter    = hb_parl( 19 );
      /* 20th parameter is Harbour extenstion and informs RDD that
         index is not shared between other clients */
      lpdbOrdCondInfo->fExclusive    = hb_parl( 20 );

      if( lpdbOrdCondInfo->itmCobWhile )
         lpdbOrdCondInfo->fRest = HB_TRUE;
      if( lpdbOrdCondInfo->lNextCount || lpdbOrdCondInfo->itmRecID ||
               lpdbOrdCondInfo->fRest || lpdbOrdCondInfo->fUseCurrent ||
          lpdbOrdCondInfo->fUseFilter )
         lpdbOrdCondInfo->fAll = HB_FALSE;

      lpdbOrdCondInfo->fActive = ! lpdbOrdCondInfo->fAll ||
               lpdbOrdCondInfo->abFor || lpdbOrdCondInfo->itmCobFor ||
               lpdbOrdCondInfo->abWhile || lpdbOrdCondInfo->itmCobWhile ||
               lpdbOrdCondInfo->fNoOptimize || lpdbOrdCondInfo->itmCobEval ||
               lpdbOrdCondInfo->fTemporary;

      lpdbOrdCondInfo->fScoped  = ! lpdbOrdCondInfo->fAll;
      lpdbOrdCondInfo->lpvCargo = NULL;

      hb_retl( SELF_ORDSETCOND( pArea, lpdbOrdCondInfo ) == HB_SUCCESS );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( ORDCREATE )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERCREATEINFO dbOrderInfo;
      DBCONSTRAINTINFO dbConstrInfo;

      dbOrderInfo.lpdbOrdCondInfo = pArea->lpdbOrdCondInfo;
      dbOrderInfo.abBagName = hb_parcx( 1 );
      dbOrderInfo.atomBagName = hb_parcx( 2 );
      dbOrderInfo.itmOrder = NULL;
      dbOrderInfo.fUnique = HB_ISLOG( 5 ) ? ( HB_BOOL ) hb_parl( 5 ) : hb_setGetUnique();
      dbOrderInfo.abExpr = hb_param( 3, HB_IT_STRING );
      if( ( ( dbOrderInfo.abBagName == NULL || dbOrderInfo.abBagName[ 0 ] == 0 ) &&
            ( dbOrderInfo.atomBagName == NULL || dbOrderInfo.atomBagName[ 0 ] == 0 ) ) ||
          ! dbOrderInfo.abExpr )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
         return;
      }
      dbOrderInfo.itmCobExpr = hb_param( 4, HB_IT_BLOCK );

      dbConstrInfo.abConstrName = hb_parc( 6 );
      dbConstrInfo.abTargetName = hb_parc( 7 );
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
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ORDBAGCLEAR )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.atomBagName = hb_param( 1, HB_IT_STRING | HB_IT_NUMERIC );
      hb_retl( SELF_ORDLSTDELETE( pArea, &pOrderInfo ) == HB_SUCCESS );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( ORDDESTROY )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING | HB_IT_NUMERIC );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      hb_retl( SELF_ORDDESTROY( pArea, &pOrderInfo ) == HB_SUCCESS );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( ORDFOR )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_ANY );
      if( pOrderInfo.itmOrder && ! HB_IS_STRING( pOrderInfo.itmOrder ) )
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
            hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
            return;
         }
      }
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      pOrderInfo.itmNewVal = hb_param( 3, HB_IT_STRING );
      pOrderInfo.itmResult = hb_itemPutC( NULL, NULL );
      pOrderInfo.itmCobExpr = NULL;
      pOrderInfo.fAllTags = HB_FALSE;
      SELF_ORDINFO( pArea, DBOI_CONDITION, &pOrderInfo );
      hb_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ORDKEY )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_ANY );
      if( pOrderInfo.itmOrder && ! HB_IS_STRING( pOrderInfo.itmOrder ) )
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
            hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
            return;
         }
      }
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      pOrderInfo.itmResult = hb_itemPutC( NULL, NULL );
      SELF_ORDINFO( pArea, DBOI_EXPRESSION, &pOrderInfo );
      hb_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ORDLISTADD )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      HB_ERRCODE errCode;

      /* Clipper clears NETERR flag when index is open */
      hb_rddSetNetErr( HB_FALSE );

      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.atomBagName = hb_param( 1, HB_IT_STRING );
      pOrderInfo.itmOrder    = hb_param( 2, HB_IT_STRING );

      if( ! pOrderInfo.atomBagName )
      {
         if( ! HB_ISNIL( 1 ) )
            hb_errRT_DBCMD( EG_ARG, EDBCMD_ORDLSTADD_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
         return;
      }

      pOrderInfo.itmResult = hb_itemNew( NULL );

      errCode = SELF_ORDLSTADD( pArea, &pOrderInfo );

      if( ! pOrderInfo.itmResult || HB_IS_NIL( pOrderInfo.itmResult ) )
         hb_retl( errCode == HB_SUCCESS );
      else
         hb_itemReturn( pOrderInfo.itmResult );

      hb_itemRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ORDLISTCLEAR )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_ORDLSTCLEAR( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ORDLISTREBUILD )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      SELF_ORDLSTREBUILD( pArea );
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ORDNAME )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
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
            hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
            return;
         }
      }

      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      pOrderInfo.itmResult   = hb_itemPutC( NULL, NULL );
      SELF_ORDINFO( pArea, DBOI_NAME, &pOrderInfo );
      hb_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ORDNUMBER )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pOrderInfo;
      memset( &pOrderInfo, 0, sizeof( pOrderInfo ) );
      pOrderInfo.itmOrder = hb_param( 1, HB_IT_STRING );
      pOrderInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      if( ! ( pOrderInfo.itmOrder || HB_ISNIL( 1 ) ) ||
          ! ( pOrderInfo.atomBagName || HB_ISNIL( 2 ) ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
         return;
      }
      pOrderInfo.itmResult = hb_itemPutNI( NULL, 0 );
      SELF_ORDINFO( pArea, DBOI_NUMBER, &pOrderInfo );
      hb_itemReturnRelease( pOrderInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( ORDSETFOCUS )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pInfo;
      memset( &pInfo, 0, sizeof( pInfo ) );
      pInfo.itmOrder = hb_param( 1, HB_IT_STRING | HB_IT_NUMERIC );
      pInfo.atomBagName = hb_param( 2, HB_IT_STRING );
      pInfo.itmResult = hb_itemPutC( NULL, NULL );
      SELF_ORDLSTFOCUS( pArea, &pInfo );
      hb_itemReturnRelease( pInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( RDDLIST )
{
   hb_itemReturnRelease( hb_rddList( ( HB_USHORT ) hb_parni( 1 ) ) );
}

HB_FUNC( RDDNAME )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      char szRddName[ HB_RDD_MAX_DRIVERNAME_LEN + 1 ];

      SELF_SYSNAME( pArea, szRddName );
      hb_retc( szRddName );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( RDDREGISTER )
{
   HB_USHORT uiLen;
   char szDriver[ HB_RDD_MAX_DRIVERNAME_LEN + 1 ];

   uiLen = ( HB_USHORT ) hb_parclen( 1 );
   if( uiLen > 0 )
   {
      if( uiLen > HB_RDD_MAX_DRIVERNAME_LEN )
         uiLen = HB_RDD_MAX_DRIVERNAME_LEN;

      hb_strncpyUpper( szDriver, hb_parc( 1 ), uiLen );
      /*
       * hb_rddRegister returns:
       *
       * 0: Ok, RDD registered
       * 1: RDD already registerd
       * > 1: error
       */
      if( hb_rddRegister( szDriver, ( HB_USHORT ) hb_parni( 2 ) ) > 1 )
      {
         hb_errInternal( HB_EI_RDDINVALID, NULL, NULL, NULL );
      }
   }
}

/* Same as LastRec() */
HB_FUNC_TRANSLATE( RECCOUNT, LASTREC )

HB_FUNC( RECNO )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
   PHB_ITEM pRecNo = hb_itemPutNL( NULL, 0 );

   if( pArea )
   {
      SELF_RECID( pArea, pRecNo );
   }
   hb_itemReturnRelease( pRecNo );
}

HB_FUNC( RECSIZE )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PHB_ITEM pItem = hb_itemNew( NULL );
      SELF_INFO( pArea, DBI_GETRECSIZE, pItem );
      hb_itemReturnRelease( pItem );
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
      dbLockInfo.fResult = HB_FALSE;
      dbLockInfo.itmRecID = NULL;
      dbLockInfo.uiMethod = DBLM_EXCLUSIVE;
      SELF_LOCK( pArea, &dbLockInfo );
      hb_retl( dbLockInfo.fResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( SELECT )
{
   if( hb_pcount() == 0 )
   {
      hb_retni( hb_rddGetCurrentWorkAreaNumber() );
   }
   else
   {
      const char * szAlias = hb_parc( 1 );
      int iArea = 0;

      if( szAlias )
      {
#if defined( HB_CLP_STRICT ) || 1
         /*
          * I do not like this Clipper behavior, in some constructions
          * programmer may use "<aliasNum>" in some others not. [Druzus]
          */
         if( hb_rddVerifyAliasName( szAlias ) == HB_SUCCESS )
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
         hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
   }
}

HB_FUNC( DBSETDRIVER )
{
   hb_retc( hb_rddDefaultDrv( NULL ) );

   if( hb_parclen( 1 ) > 0 )
   {
      if( ! hb_rddDefaultDrv( hb_parc( 1 ) ) )
         hb_errRT_DBCMD( EG_ARG, EDBCMD_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
   }
}

HB_FUNC( ORDSCOPE )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      DBORDERINFO pInfo;
      HB_USHORT uiAction;
      int iScope = hb_parni( 1 );

      memset( &pInfo, 0, sizeof( pInfo ) );
      pInfo.itmResult = hb_itemNew( NULL );
      if( iScope == 2 )
      {
         if( hb_pcount() > 1 && ! HB_ISNIL( 2 ) )
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
            if( HB_ISNIL( 2 ) )
               uiAction = ( iScope == 0 ) ? DBOI_SCOPETOPCLEAR : DBOI_SCOPEBOTTOMCLEAR;
            else
               pInfo.itmNewVal = hb_param( 2, HB_IT_ANY );
         }
      }
      SELF_ORDINFO( pArea, uiAction, &pInfo );
      hb_itemReturnRelease( pInfo.itmResult );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( DBRELATION )  /* (<nRelation>) --> cLinkExp */
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      PHB_ITEM pRelExpr = hb_itemPutC( NULL, NULL );
      HB_USHORT uiRelNo = ( HB_USHORT ) hb_parni( 1 );
      SELF_RELTEXT( pArea, uiRelNo ? uiRelNo : 1, pRelExpr );
      hb_itemReturnRelease( pRelExpr );
   }
   else
      hb_retc_null();
}

HB_FUNC( DBRSELECT )  /* (<nRelation>) --> nWorkArea */
{
   HB_USHORT uiWorkArea = 0, uiRelation = ( HB_USHORT ) hb_parni( 1 );
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      /* undocumented CA-Cl*pper behavior:
       * When parameter is missing, wrong or 0 then 1 is used as
       * relation number [druzus]
       */
      SELF_RELAREA( pArea, uiRelation ? uiRelation : 1, &uiWorkArea );

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
      HB_AREANO uiChildArea;
      char * szAlias = NULL;

      if( hb_pcount() < 2 ||
          hb_param( 1, HB_IT_NUMERIC | HB_IT_STRING ) == NULL ||
          ! ( HB_ISNIL( 4 ) || HB_ISLOG( 4 ) ) )
      {
         hb_errRT_DBCMD( EG_ARG, EDBCMD_REL_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
         return;
      }

      if( HB_ISNUM( 1 ) )
      {
         uiChildArea = ( HB_AREANO ) hb_parni( 1 );
      }
      else
      {
         int iArea = hb_rddGetCurrentWorkAreaNumber();

         hb_rddSelectWorkAreaAlias( hb_parcx( 1 ) );
         if( hb_vmRequestQuery() )
            return;
         uiChildArea = ( HB_AREANO ) hb_rddGetCurrentWorkAreaNumber();
         hb_rddSelectWorkAreaNumber( iArea );
      }

      pChildArea = uiChildArea ? ( AREAP ) hb_rddGetWorkAreaPointer( uiChildArea ) : NULL;

      if( ! pChildArea )
      {
         hb_errRT_BASE( EG_NOALIAS, EDBCMD_NOALIAS, NULL, szAlias, 0 );
         return;
      }

      dbRelations.itmCobExpr = hb_itemNew( hb_param( 2, HB_IT_BLOCK ) );
      dbRelations.abKey = hb_itemNew( hb_param( 3, HB_IT_STRING ) );
      dbRelations.isScoped = hb_parl( 4 );
      dbRelations.isOptimized = HB_FALSE;
      dbRelations.lpaChild = pChildArea;
      dbRelations.lpaParent = pArea;
      dbRelations.lpdbriNext = NULL;

      SELF_SETREL( pArea, &dbRelations );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( __DBARRANGE )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      HB_USHORT uiCount, uiDest;
      HB_AREANO uiNewArea;
      HB_SIZE nSize;
      char * szFieldLine, * szPos;
      PHB_ITEM pStruct, pFields;
      DBSORTINFO dbSortInfo;

      memset( &dbSortInfo, 0, sizeof( dbSortInfo ) );
      dbSortInfo.dbtri.uiFlags = DBTF_PUTREC;
      uiNewArea = ( HB_AREANO ) hb_parni( 1 );

      /* Fields structure of source WorkArea */
      pStruct = hb_param( 2, HB_IT_ARRAY );
      if( pStruct )
      {
         dbSortInfo.dbtri.uiItemCount = ( HB_USHORT ) hb_arrayLen( pStruct );
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
      dbSortInfo.dbtri.dbsci.itmRecID = HB_ISNIL( 6 ) ? NULL : hb_param( 6, HB_IT_ANY );
      dbSortInfo.dbtri.dbsci.fRest = hb_param( 7, HB_IT_LOGICAL );
      dbSortInfo.dbtri.dbsci.fIgnoreFilter =
      dbSortInfo.dbtri.dbsci.fLast =
      dbSortInfo.dbtri.dbsci.fIgnoreDuplicates =
      dbSortInfo.dbtri.dbsci.fBackward =
      dbSortInfo.dbtri.dbsci.fOptimized = HB_FALSE;
      dbSortInfo.dbtri.dbsci.fIncludeDeleted = HB_TRUE;

      pFields = hb_param( 8, HB_IT_ARRAY );
      dbSortInfo.uiItemCount = pFields ? ( HB_USHORT ) hb_arrayLen( pFields ) : 0;
      if( dbSortInfo.uiItemCount > 0 )
      {
         dbSortInfo.lpdbsItem = ( LPDBSORTITEM ) hb_xgrab( dbSortInfo.uiItemCount * sizeof( DBSORTITEM ) );
         nSize = 0;
         for( uiCount = 1; uiCount <= dbSortInfo.uiItemCount; ++uiCount )
         {
            HB_SIZE nLine = hb_arrayGetCLen( pFields, uiCount );
            if( nLine > nSize )
               nSize = nLine;
         }
         szFieldLine = ( char * ) hb_xgrab( nSize + 1 );
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

/* __dbTrans( nDstArea, aFieldsStru, bFor, bWhile, nNext, nRecord, lRest ) */
HB_FUNC( __DBTRANS )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_AREANO uiSrcArea, uiDstArea;
      AREAP pSrcArea, pDstArea;

      uiSrcArea = ( HB_AREANO ) hb_rddGetCurrentWorkAreaNumber();
      pSrcArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();
      uiDstArea = ( HB_AREANO ) hb_parni( 1 );
      hb_rddSelectWorkAreaNumber( uiDstArea );
      pDstArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

      if( pSrcArea && pDstArea )
      {
         DBTRANSINFO dbTransInfo;
         PHB_ITEM pFields = hb_param( 2, HB_IT_ARRAY );
         HB_ERRCODE errCode;

         memset( &dbTransInfo, 0, sizeof( dbTransInfo ) );
         errCode = hb_dbTransStruct( pSrcArea, pDstArea, &dbTransInfo,
                                     NULL, pFields );
         if( errCode == HB_SUCCESS )
         {
            hb_rddSelectWorkAreaNumber( dbTransInfo.lpaSource->uiArea );

            dbTransInfo.dbsci.itmCobFor   = hb_param( 3, HB_IT_BLOCK );
            dbTransInfo.dbsci.lpstrFor    = NULL;
            dbTransInfo.dbsci.itmCobWhile = hb_param( 4, HB_IT_BLOCK );
            dbTransInfo.dbsci.lpstrWhile  = NULL;
            dbTransInfo.dbsci.lNext       = hb_param( 5, HB_IT_NUMERIC );
            dbTransInfo.dbsci.itmRecID    = HB_ISNIL( 6 ) ? NULL : hb_param( 6, HB_IT_ANY );
            dbTransInfo.dbsci.fRest       = hb_param( 7, HB_IT_LOGICAL );

            dbTransInfo.dbsci.fIgnoreFilter     = HB_TRUE;
            dbTransInfo.dbsci.fIncludeDeleted   = HB_TRUE;
            dbTransInfo.dbsci.fLast             = HB_FALSE;
            dbTransInfo.dbsci.fIgnoreDuplicates = HB_FALSE;
            dbTransInfo.dbsci.fBackward         = HB_FALSE;

            errCode = SELF_TRANS( dbTransInfo.lpaSource, &dbTransInfo );
         }

         if( dbTransInfo.lpTransItems )
            hb_xfree( dbTransInfo.lpTransItems );

         hb_retl( errCode == HB_SUCCESS );
      }
      else
         hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );

      hb_rddSelectWorkAreaNumber( uiSrcArea );
   }
   else
      hb_errRT_DBCMD( EG_ARG, EDBCMD_USE_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( __DBAPP )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
      hb_retl( HB_SUCCESS == hb_rddTransRecords( pArea,
               hb_parc( 1 ),                     /* file name */
               hb_parc( 8 ),                     /* RDD */
               hb_parnl( 9 ),                    /* connection */
               hb_param( 2, HB_IT_ARRAY ),       /* Fields */
               HB_FALSE,                         /* Export? */
               hb_param( 3, HB_IT_BLOCK ),       /* cobFor */
               NULL,                             /* lpStrFor */
               hb_param( 4, HB_IT_BLOCK ),       /* cobWhile */
               NULL,                             /* lpStrWhile */
               hb_param( 5, HB_IT_NUMERIC ),     /* Next */
               HB_ISNIL( 6 ) ? NULL : hb_param( 6, HB_IT_ANY ),   /* RecID */
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
      hb_retl( HB_SUCCESS == hb_rddTransRecords( pArea,
               hb_parc( 1 ),                     /* file name */
               hb_parc( 8 ),                     /* RDD */
               hb_parnl( 9 ),                    /* connection */
               hb_param( 2, HB_IT_ARRAY ),       /* Fields */
               HB_TRUE,                          /* Export? */
               hb_param( 3, HB_IT_BLOCK ),       /* cobFor */
               NULL,                             /* lpStrFor */
               hb_param( 4, HB_IT_BLOCK ),       /* cobWhile */
               NULL,                             /* lpStrWhile */
               hb_param( 5, HB_IT_NUMERIC ),     /* Next */
               HB_ISNIL( 6 ) ? NULL : hb_param( 6, HB_IT_ANY ),   /* RecID */
               hb_param( 7, HB_IT_LOGICAL ),     /* Rest */
               hb_parc( 10 ),                    /* Codepage */
               hb_param( 11, HB_IT_ANY ) ) );    /* Delimiter */
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "COPY TO" );
}

HB_FUNC( HB_RDDGETTEMPALIAS )
{
   char szAliasTmp[ HB_RDD_MAX_ALIAS_LEN + 1 ];

   if( hb_rddGetTempAlias( szAliasTmp ) == HB_SUCCESS )
      hb_retc( szAliasTmp );
}

HB_FUNC( HB_RDDINFO )
{
   LPRDDNODE  pRDDNode;
   HB_USHORT  uiRddID;
   HB_ULONG   ulConnection;
   PHB_ITEM   pIndex, pParam;
   const char * szDriver;

   szDriver = hb_parc( 3 );
   if( ! szDriver ) /* no VIA RDD parameter, use default */
      szDriver = hb_rddDefaultDrv( NULL );

   ulConnection = hb_parnl( 4 );

   pRDDNode = hb_rddFindNode( szDriver, &uiRddID );  /* find the RDDNODE */
   pIndex = hb_param( 1, HB_IT_NUMERIC );
   pParam = hb_param( 2, HB_IT_ANY );

   if( pRDDNode && pIndex )
   {
      PHB_ITEM pInfo = hb_itemNew( pParam );
      SELF_RDDINFO( pRDDNode, ( HB_USHORT ) hb_itemGetNI( pIndex ), ulConnection, pInfo );
      hb_itemReturnRelease( pInfo );
   }
   else
      hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( HB_DBDROP )
{
   LPRDDNODE  pRDDNode;
   HB_USHORT  uiRddID;
   HB_ULONG   ulConnection;
   const char * szDriver;
   PHB_ITEM   pName;

   szDriver = hb_parc( 3 );
   if( ! szDriver ) /* no VIA RDD parameter, use default */
   {
      szDriver = hb_rddDefaultDrv( NULL );
   }
   ulConnection = hb_parnl( 4 );

   pRDDNode = hb_rddFindNode( szDriver, &uiRddID );  /* find the RDDNODE */
   pName = hb_param( 1, HB_IT_STRING );

   if( pRDDNode && pName )
      hb_retl( SELF_DROP( pRDDNode, pName, hb_param( 2, HB_IT_STRING ),
                          ulConnection ) == HB_SUCCESS );
   else
      hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( HB_DBEXISTS )
{
   LPRDDNODE  pRDDNode;
   HB_USHORT  uiRddID;
   HB_ULONG   ulConnection;
   const char * szDriver;
   PHB_ITEM   pName;

   szDriver = hb_parc( 3 );
   if( ! szDriver ) /* no VIA RDD parameter, use default */
      szDriver = hb_rddDefaultDrv( NULL );

   ulConnection = hb_parnl( 4 );

   pRDDNode = hb_rddFindNode( szDriver, &uiRddID );  /* find the RDD */
   pName = hb_param( 1, HB_IT_STRING );

   if( pRDDNode && pName )
      hb_retl( SELF_EXISTS( pRDDNode, pName, hb_param( 2, HB_IT_STRING ),
                            ulConnection ) == HB_SUCCESS );
   else
      hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( HB_DBRENAME )
{
   LPRDDNODE  pRDDNode;
   HB_USHORT  uiRddID;
   HB_ULONG   ulConnection;
   const char * szDriver;
   PHB_ITEM   pTable, pIndex, pNewName;

   szDriver = hb_parc( 4 );
   if( ! szDriver ) /* no VIA RDD parameter, use default */
      szDriver = hb_rddDefaultDrv( NULL );

   ulConnection = hb_parnl( 5 );

   pRDDNode = hb_rddFindNode( szDriver, &uiRddID );  /* find the RDDNODE */
   pTable = hb_param( 1, HB_IT_STRING );
   pIndex = hb_param( 2, HB_IT_STRING );
   pNewName = hb_param( 3, HB_IT_STRING );
   if( pIndex && ! pNewName )
   {
      pNewName = pIndex;
      pIndex = NULL;
   }

   if( pRDDNode && pTable && pNewName )
      hb_retl( SELF_RENAME( pRDDNode, pTable, pIndex, pNewName,
                            ulConnection ) == HB_SUCCESS );
   else
      hb_errRT_DBCMD( EG_ARG, EDBCMD_EVAL_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
}

HB_FUNC( HB_FIELDLEN )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      HB_USHORT uiIndex;
      const char * szField = hb_parc( 1 );

      if( szField )
         uiIndex = hb_rddFieldIndex( pArea, szField );
      else
         uiIndex = ( HB_FIELDNO ) hb_parni( 1 );

      if( uiIndex > 0 )
      {
         PHB_ITEM pItem = hb_itemNew( NULL );

         if( SELF_FIELDINFO( pArea, uiIndex, DBS_LEN, pItem ) == HB_SUCCESS )
         {
            hb_itemReturnRelease( pItem );
            return;
         }
         hb_itemRelease( pItem );
      }
   }

   hb_retni( 0 );
}

HB_FUNC( HB_FIELDDEC )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      HB_USHORT uiIndex;
      const char * szField = hb_parc( 1 );

      if( szField )
         uiIndex = hb_rddFieldIndex( pArea, szField );
      else
         uiIndex = ( HB_FIELDNO ) hb_parni( 1 );

      if( uiIndex > 0 )
      {
         PHB_ITEM pItem = hb_itemNew( NULL );

         if( SELF_FIELDINFO( pArea, uiIndex, DBS_DEC, pItem ) == HB_SUCCESS )
         {
            hb_itemReturnRelease( pItem );
            return;
         }
         hb_itemRelease( pItem );
      }
   }

   hb_retni( 0 );
}

HB_FUNC( HB_FIELDTYPE )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      HB_USHORT uiIndex;
      const char * szField = hb_parc( 1 );

      if( szField )
         uiIndex = hb_rddFieldIndex( pArea, szField );
      else
         uiIndex = ( HB_FIELDNO ) hb_parni( 1 );

      if( uiIndex > 0 )
      {
         PHB_ITEM pItem = hb_itemNew( NULL );

         if( SELF_FIELDINFO( pArea, uiIndex, DBS_TYPE, pItem ) == HB_SUCCESS )
         {
            hb_itemReturnRelease( pItem );
            return;
         }
         hb_itemRelease( pItem );
      }
   }

   hb_retc_null();
}

HB_FUNC( HB_WAEVAL )
{
   PHB_ITEM pBlock = hb_param( 1, HB_IT_BLOCK );

   if( pBlock )
      hb_rddEvalWA( pBlock );
   else
      hb_errRT_DBCMD( EG_ARG, EDBCMD_USE_BADPARAMETER, NULL, HB_ERR_FUNCNAME );
}

#ifndef HB_CLP_STRICT

HB_FUNC( __DBSKIPPER )
{
   AREAP pArea = ( AREAP ) hb_rddGetCurrentWorkAreaPointer();

   if( pArea )
   {
      HB_LONG lSkipped = 0;
      HB_LONG lRecs = 1;
      HB_BOOL fBEof;
      HB_ULONG ulRecords = 0;

      if( SELF_RECCOUNT( pArea, &ulRecords ) == HB_SUCCESS && ulRecords > 0 )
      {
         if( HB_ISNUM( 1 ) )
            lRecs = hb_parnl( 1 );

         if( lRecs == 0 )
            SELF_SKIP( pArea, 0 );
         else if( lRecs > 0 )
         {
            /* the condition below is exact Clipper behavior anyhow
             * we cannot replicate it without introducing serious problem:
             * some RDDs use non continuous record numbers (i.e. ADT) and
             * the condition: ulRecNo != ulRecords + 1 can be true also for
             * normal records not only for the phantom EOF record. [druzus]
             */
#if 0
            HB_ULONG ulRecNo = 0;
            if( SELF_RECNO( pArea, &ulRecNo ) == HB_SUCCESS &&
                ulRecNo != ulRecords + 1 )
#endif
            {
               while( lSkipped < lRecs )
               {
                  if( SELF_SKIP( pArea, 1 ) != HB_SUCCESS )
                     break;
                  if( SELF_EOF( pArea, &fBEof ) != HB_SUCCESS )
                     break;
                  if( fBEof )
                  {
                     SELF_SKIP( pArea, -1 );
                     break;
                  }
                  lSkipped++;
               }
            }
         }
         else /* if( lRecs < 0 ) */
         {
            while( lSkipped > lRecs )
            {
               if( SELF_SKIP( pArea, -1 ) != HB_SUCCESS )
                  break;
               if( SELF_BOF( pArea, &fBEof ) != HB_SUCCESS )
                  break;
               if( fBEof )
                  break;
               lSkipped--;
            }
         }
      }
      hb_retnl( lSkipped );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, HB_ERR_FUNCNAME );
}

#endif
