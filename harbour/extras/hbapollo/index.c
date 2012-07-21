/*
 * $Id$
 */

/*
 * SixAPI Project source code:
 *
 * Copyright 2010 Andi Jahja <xharbour@telkom.net.id>
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
 */
#include "sxapi.h"

static int _sx_CheckIndexMode( char * szIndexMode )
{
   char *   sxIndexMode[] = { "UNIQUE", "RYO" };
   int      ui;
   int      iIndexMode    = 0;

   HB_ISIZ  iLen          = strlen( szIndexMode ) + 1;
   char *   szTmp         = ( char * ) hb_xgrab( iLen );

   hb_snprintf( szTmp, iLen, "%s", szIndexMode );
   szTmp = _sx_upper( szTmp );

   for( ui = 0; ui < 2; ui++ )
   {
      if( strcmp( sxIndexMode[ ui ], szTmp ) == 0 )
      {
         iIndexMode = ui;
         break;
      }
   }

   hb_xfree( szTmp );
   return iIndexMode;
}

HB_FUNC( SX_SETORDER )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_SETORDER" );

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   if( HB_ISNUM( 1 ) )
   {
      SHORT iIndex = ( SHORT ) hb_parni( 1 );
      if( iIndex < 0 )
         hb_retni( sx_SetOrder( 0 ) );
      else
         hb_retni( sx_SetOrder( iIndex ) );
   }
   else if( HB_ISCHAR( 1 ) )
      hb_retni( sx_SetOrder( sx_TagArea( ( PBYTE ) hb_parc( 1 ) ) ) );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_ORDSETFOCUS )
{
   HB_FUNCNAME( SX_SETORDER ) ();
}

HB_FUNC( SX_CLOSEINDEXES )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_CLOSEINDEXES" );

   if( ! HB_ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   sx_CloseIndexes();

   if( ! ( iWorkArea == SX_DUMMY_NUMBER ) )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_INDEX )
{
   WORD  iOption       = 0;
   PBYTE cpCondition   = ( PBYTE ) 0;
   WORD  iWorkArea     = SX_DUMMY_NUMBER;

   /*
      Any Table Opened ?
    */
   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_INDEX" );

   /*
      Index Filename and index expression passed correctly
    */
   if( ! HB_ISCHAR( 1 ) && ! HB_ISCHAR( 2 ) )
   {
      hb_retni( -1 );
      return;
   }

   /*
      Workarea
    */
   if( ! HB_ISNIL( 6 ) )
      iWorkArea = _sx_select( hb_param( 6, HB_IT_ANY ) );

   /*
      Indexing Option
    */
   if( HB_ISCHAR( 3 ) )
      iOption = ( WORD ) _sx_CheckIndexMode( ( char * ) hb_parc( 3 ) );

   /*
      Condition
    */
   if( HB_ISCHAR( 5 ) )
      cpCondition = ( PBYTE ) hb_parc( 5 );

   hb_retni( sx_Index( ( PBYTE ) hb_parc( 1 ),  /* cpFileName */
                       ( PBYTE ) hb_parc( 2 ),  /* cpExpr*/
                       iOption,                 /* iOption: IDX_NONE=0 IDX_UNIQUE=1 IDX_EMPTY=2  */
                       hb_parl( 4 ),            /* bDescend */
                       cpCondition              /* cpCondition */
                       ) );

   /*
      Go to previous area
    */
   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_REINDEX )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_REINDEX" );

   if( ! HB_ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   sx_Reindex();

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_INDEXCLOSE )
{
/*
   sx_IndexClose() applied only to SDENSXDBT !
 */
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_INDEXCLOSE" );

   if( ! HB_ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   sx_IndexClose();

   if( ! ( iWorkArea == SX_DUMMY_NUMBER ) )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_INDEXCONDITION )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_INDEXCONDITION" );

   if( ! HB_ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   hb_retc( ( char * ) sx_IndexCondition() );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_INDEXFLIP )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_INDEXFLIP" );

   if( ! HB_ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   hb_retl( sx_IndexFlip() );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_INDEXKEY )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_INDEXKEY" );

   if( ! HB_ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   hb_retc( ( char * ) sx_IndexKey() );

   if( ! ( iWorkArea == SX_DUMMY_NUMBER ) )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_ORDKEY )
{
   HB_FUNCNAME( SX_INDEXKEY ) ();
}

HB_FUNC( SX_INDEXKEYFIELD )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_INDEXKEYFIELD" );

   if( ! HB_ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   hb_retc( ( char * ) sx_IndexKeyField() );

   if( ! ( iWorkArea == SX_DUMMY_NUMBER ) )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_INDEXNAME )
{
   WORD  iIndex;
   WORD  iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_INDEXNAME" );

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   if( HB_ISNUM( 1 ) )
   {
      iIndex = ( WORD ) hb_parni( 1 );
      hb_retc( ( char * ) sx_IndexName( iIndex ) );
   }
   else
      hb_retc_null();

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_INDEXOPEN )
{
   if( _sx_Used() )
   {
      if( HB_ISCHAR( 1 ) )
      {
         HB_ISIZ  iLen    = hb_parclen( 1 ) + 1;
         char *   szTmp   = ( char * ) hb_xgrab( iLen );

         hb_snprintf( szTmp, iLen, "%s", ( char * ) hb_parc( 1 ) );
         if( hb_fsFileExists( ( const char * ) szTmp ) )
         {
            WORD iWorkArea = SX_DUMMY_NUMBER;

            if( ! HB_ISNIL( 2 ) )
               iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

            hb_retni( sx_IndexOpen( ( PBYTE ) szTmp ) );

            hb_xfree( szTmp );

            if( iWorkArea != SX_DUMMY_NUMBER )
               sx_Select( iWorkArea );
         }
         else
         {
            hb_xfree( szTmp );
            hb_errRT_BASE( EG_OPEN, 2020, NULL, "SX_INDEXOPEN", 1,
                           hb_paramError( 1 ) );
         }
      }
      else
         hb_errRT_DBCMD( EG_ARG, EDBCMD_DBCMDBADPARAMETER, NULL, "SX_INDEXOPEN" );
   }
   else
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_INDEXOPEN" );
}

HB_FUNC( SX_INDEXORD )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_INDEXORD" );

   if( ! HB_ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   hb_retni( sx_IndexOrd() );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_ORDNUMBER )
{
   HB_FUNCNAME( SX_INDEXORD ) ();
}

HB_FUNC( SX_INDEXTAG )
{
   WORD  iWorkArea = SX_DUMMY_NUMBER;
   WORD  iIndexType;
   PBYTE cpFileName,
         cpCondition,
         cpTagName;
   HB_BOOL  bDescend;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_INDEXTAG" );

   if( ! HB_ISCHAR( 2 ) && ! HB_ISCHAR( 3 ) )
   {
      hb_retni( -1 );
      return;
   }

   if( ! HB_ISNIL( 7 ) )
      iWorkArea = _sx_select( hb_param( 7, HB_IT_ANY ) );

   cpFileName = HB_ISCHAR( 1 ) ? ( PBYTE ) hb_parc( 1 ) : ( PBYTE ) 0;
   cpTagName  = ( PBYTE ) hb_parc( 2 );
   iIndexType = HB_ISNUM( 4 ) ? ( WORD ) hb_parni( 4 ) : ( WORD ) 0;

   if( iIndexType > 2 )
      iIndexType = 0;

   bDescend      = HB_ISLOG( 5 ) ? hb_parl( 5 ) : HB_FALSE;
   cpCondition   = HB_ISCHAR( 6 ) ? ( PBYTE ) hb_parc( 6 ) : ( PBYTE ) 0;

   hb_retni( sx_IndexTag( cpFileName, cpTagName, ( PBYTE ) hb_parc( 3 ) /* cpExpr */,
                          iIndexType, bDescend, cpCondition ) );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_INDEXTYPE )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_INDEXTYPE" );

   if( ! HB_ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   hb_retni( sx_IndexType() );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_KEYADD )
{
   WORD  iWorkArea = SX_DUMMY_NUMBER;
   PBYTE cpTagName;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_KEYADD" );

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   cpTagName = HB_ISCHAR( 1 ) ? ( PBYTE ) hb_parc( 1 ) : ( PBYTE ) 0;

   hb_retl( sx_KeyAdd( cpTagName ) );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_KEYDROP )
{
   WORD  iWorkArea = SX_DUMMY_NUMBER;
   PBYTE cpTagname;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_KEYDROP" );

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   cpTagname = HB_ISCHAR( 1 ) ? ( PBYTE ) hb_parc( 1 ) : ( PBYTE ) NULL;

   hb_retl( sx_KeyDrop( cpTagname ) );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_KEYDATA )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_KEYDATA" );

   if( ! HB_ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   hb_retc( ( char * ) sx_KeyData() );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_ORDERPOSGET )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! HB_ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   hb_retni( ( int ) sx_OrderPosGet() );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_ORDERPOSSET )
{
   double   iPos;
   WORD     iWorkArea = SX_DUMMY_NUMBER;

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   iPos = hb_parnd( 1 );

   sx_OrderPosSet( iPos );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_ORDERRECNO )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_ORDERRECNO" );

   if( ! HB_ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   hb_retnl( sx_OrderRecNo() );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_TAGAREA )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_TAGAREA" );

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   hb_retni( sx_TagArea( ( PBYTE ) hb_parc( 1 ) /* cpTagName */ ) );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_TAGCOUNT )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_TAGCOUNT" );

   if( ! HB_ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   hb_retni( sx_SysProp( SDE_SP_GETINDEXCOUNT, ( PVOID ) NULL ) );

   if( ! ( iWorkArea == SX_DUMMY_NUMBER ) )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_TAGDELETE )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_TAGDELETE" );

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   hb_retl( sx_TagDelete( ( PBYTE ) hb_parc( 1 ) ) );

   if( ! ( iWorkArea == SX_DUMMY_NUMBER ) )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_TAGINFO )
{
   PHB_ITEM pItem;
   PHB_ITEM pData;
   WORD     uiCount,
            nTagCount,
            nOldOrd;

   /* char *cFieldName; */
   /* HB_USHORT nIndexArea = -1; */
   WORD     uiIndex    = 1;
   PHB_ITEM pTagInfo   = hb_itemNew( NULL );

   hb_arrayNew( pTagInfo, 0 );

   if( _sx_Used() )
   {
      nOldOrd    = sx_IndexOrd();
      nTagCount  = ( HB_USHORT ) sx_SysProp( SDE_SP_GETINDEXCOUNT, ( PVOID ) NULL );
      pData      = hb_itemNew( NULL );
      pItem      = hb_itemNew( NULL );

      if( HB_ISNUM( 1 ) )
      {
         uiIndex    = ( WORD ) hb_parni( 1 );
         nTagCount  = uiIndex;
      }
      else if( HB_ISCHAR( 1 ) )
      {
         uiIndex    = sx_TagArea( ( PBYTE ) hb_parc( 1 ) );
         nTagCount  = uiIndex;
      }

      for( uiCount = uiIndex; uiCount <= nTagCount; uiCount++ )
      {
         sx_SetOrder( uiCount );
         hb_arrayNew( pItem, 8 );
         hb_arraySet( pItem, 1, hb_itemPutC( pData, ( char * ) sx_TagName( uiCount ) ) );
         hb_arraySet( pItem, 2, hb_itemPutC( pData, ( char * ) sx_IndexKey() ) );
         hb_arraySet( pItem, 3, hb_itemPutC( pData, ( char * ) sx_IndexCondition() ) );
         hb_arraySet( pItem, 4, hb_itemPutNI( pData, sx_IndexType() ) );
         hb_arraySet( pItem, 5, hb_itemPutL( pData,
                                             ( HB_BOOL ) sx_SysProp( SDE_SP_GETDESCENDING, ( PVOID ) &uiCount )
                                             ) );
         hb_arraySet( pItem, 6, hb_itemPutL( pData, ( HB_BOOL ) ( ( int ) sx_SysProp(
                                                                  SDE_SP_GETEMPTY, ( PVOID ) &uiCount ) == 2 ) ) );
         hb_arraySet( pItem, 7, hb_itemPutC( pData, ( char * ) sx_IndexName( uiCount ) ) );
         hb_arraySet( pItem, 8, hb_itemPutC( pData, ( char * ) sx_IndexKeyField() ) );
         hb_arrayAdd( pTagInfo, pItem );
      }

      hb_itemRelease( pItem );
      hb_itemRelease( pData );
      sx_SetOrder( nOldOrd );
   }

   hb_itemCopy( hb_stackReturnItem(), pTagInfo );
   hb_itemRelease( pTagInfo );
}

HB_FUNC( SX_TAGNAME )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_TAGNAME" );

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   hb_retc( ( char * ) sx_TagName( ( WORD ) hb_parni( 1 ) /* iTagArea */ ) );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}
