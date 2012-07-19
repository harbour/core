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

HB_FUNC( SX_COPYSTRUCTURE )
{
   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_COPYSTRUCTURE" );

   if( HB_ISCHAR( 1 ) )
   {
#if 0
      HB_BOOL     bAllocated    = HB_FALSE;
#endif
      PHB_ITEM paFields      = hb_param( 2, HB_IT_ARRAY );
      HB_ISIZ  uilenpArray   = 0;
      WORD     iWorkArea     = SX_DUMMY_NUMBER;
      HB_ISIZ  iLen          = hb_parclen( 1 ) + 1;
      char *   szTmp         = ( char * ) hb_xgrab( iLen );

      hb_snprintf( szTmp, iLen, "%s", hb_parc( 1 ) );

      if( ! HB_ISNIL( 3 ) )
         iWorkArea = _sx_select( hb_param( 3, HB_IT_ANY ) );

      if( paFields )
         uilenpArray = hb_arrayLen( paFields );

      if( uilenpArray == 0 )
      {
#if 0
         paFields   = _sx_FieldNames();
         bAllocated = HB_TRUE;
#endif
         hb_retl( sx_CopyStructure( ( PBYTE ) szTmp, ( PBYTE ) "__TEMP" ) );
      }
      else
      {
         hb_retl( _sx_CopyStructure( ( PBYTE ) szTmp, paFields ) );
      }

      hb_xfree( szTmp );

#if 0
      if( bAllocated )
         hb_itemRelease( paFields );
#endif
      if( ! ( iWorkArea == SX_DUMMY_NUMBER ) )
         sx_Select( iWorkArea );
   }
   else
      hb_retl( HB_FALSE );
}

HB_BOOL _sx_CopyStructure( PBYTE cpFileName, PHB_ITEM paFields )
{
   PHB_ITEM pStruct       = hb_itemNew( NULL );
   PHB_ITEM pData         = hb_itemNew( NULL );
   PHB_ITEM pItem         = hb_itemNew( NULL );
   PHB_ITEM pFieldDesc,
            pTemp         = NULL;

   int      iLenSource    = sx_FieldCount();
   HB_ISIZ  i,
            ui,
            uilenpArray   = 0;

   char *   cpFieldName,
   * cFieldToCopy;

   PBYTE    cFieldName;
   PBYTE    cFieldType;
   SHORT    iFieldLen;
   SHORT    iDec;
   HB_BOOL     bSuccess = HB_FALSE;

   if( paFields )
      uilenpArray = hb_arrayLen( paFields );

   if( uilenpArray )
   {
      hb_arrayNew( pStruct, 0 );

      for( i = 0; i < iLenSource; i++ )
      {
         cpFieldName = ( char * ) sx_FieldName( ( WORD ) ( i + 1 ) );
         for( ui = 0; ui < uilenpArray; ui++ )
         {
            cFieldToCopy = ( char * ) hb_arrayGetCPtr( paFields, ui + 1 );
            if( strcmp( cpFieldName, cFieldToCopy ) == 0 )
            {
               hb_arrayNew( pItem, 4 );
               pTemp   = hb_itemPutC( NULL, cpFieldName );
               hb_arraySet( pItem, 1, pTemp );
               hb_itemRelease( pTemp );
               pTemp   = hb_itemPutC( NULL, ( char * ) sx_FieldType( ( PBYTE ) cpFieldName ) );
               hb_arraySet( pItem, 2, pTemp );
               hb_itemRelease( pTemp );
               hb_arraySet( pItem, 3, hb_itemPutNI( pData, sx_FieldWidth( ( PBYTE ) cpFieldName ) ) );
               hb_arraySet( pItem, 4, hb_itemPutNI( pData, sx_FieldDecimals( ( PBYTE ) cpFieldName ) ) );
               hb_arrayAdd( pStruct, pItem );
            }
         }
      }

      hb_itemRelease( pItem );
      hb_itemRelease( pData );

      if( pTemp )
         hb_itemClear( pTemp );
   }

   /* Reuse variable for new array */
   uilenpArray = hb_arrayLen( pStruct );

   /* OK Now we Get new array in &hb_stack.Return */
   /* Will create DBF as per new array */
   if( uilenpArray )
   {
      #if 0
      printf( "_sx_CopyStructure... cpFileName=>>%s<<\n", cpFileName );
      printf( "_sx_CopyStructure... i_sxApi_RDD_Default=>>%i<<\n", i_sxApi_RDD_Default );
      printf( "_sx_CopyStructure... uilenpArray=>>%i<<\n", uilenpArray );
      sx_CreateNew( ( PBYTE ) cpFileName, ( PBYTE ) "TEMPORARY_STRUCT", i_sxApi_RDD_Default, uilenpArray );
      #endif
      sx_CreateNew( ( PBYTE ) cpFileName, ( PBYTE ) "__TEMP", ( WORD ) i_sxApi_RDD_Default,
                    ( WORD ) uilenpArray );

      /* printf( "_sx_CopyStructure...2\n"); */
      for( ui = 0; ui < uilenpArray; ui++ )
      {
         pFieldDesc = hb_arrayGetItemPtr( pStruct, ui + 1 );
         cFieldName = ( PBYTE ) hb_arrayGetC( pFieldDesc, 1 );
         cFieldType = ( PBYTE ) hb_arrayGetC( pFieldDesc, 2 );
         iFieldLen  = ( SHORT ) hb_arrayGetNI( pFieldDesc, 3 );
         iDec       = ( SHORT ) hb_arrayGetNI( pFieldDesc, 4 );

         /* printf("Creating %s\n", cFieldName ); */
         sx_CreateField( cFieldName, cFieldType, iFieldLen, iDec );
         hb_xfree( cFieldName );
         hb_xfree( cFieldType );
      }

      hb_itemRelease( pStruct );
      bSuccess = sx_CreateExec();
      if( bSuccess )
         sx_Close();
   }

   return bSuccess;
}

HB_FUNC( SX_COPYSTRUCTUREEXTENDED )
{
   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL,
                      "SX_COPYSTRUCTUREEXTENDED" );

   if( HB_ISCHAR( 1 ) )
   {
      WORD iWorkArea = SX_DUMMY_NUMBER;

      if( ! HB_ISNIL( 2 ) )
         iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

      hb_retl( sx_CopyStructureExtended( ( PBYTE ) hb_parc( 1 ) ) );

      if( ! ( iWorkArea == SX_DUMMY_NUMBER ) )
         sx_Select( iWorkArea );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( SX_DBSTRUCT )
{
   WORD     iWorkArea = SX_DUMMY_NUMBER;
   PHB_ITEM pStruct;

   if( ! _sx_Used() )
      return;
   if( ! HB_ISNIL( 1 ) )
      iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

   pStruct = _sx_DbStruct();
   hb_itemReturnRelease( pStruct );

   if( ! ( iWorkArea == SX_DUMMY_NUMBER ) )
      sx_Select( iWorkArea );
}

PHB_ITEM _sx_DbStruct()
{
   PHB_ITEM pStruct    = hb_itemNew( NULL );
   PHB_ITEM pData      = hb_itemNew( NULL );
   PHB_ITEM pItem      = hb_itemNew( NULL );
   HB_USHORT   uiFields   = sx_FieldCount(),
            uiCount;

   hb_arrayNew( pStruct, 0 );

   for( uiCount = 1; uiCount <= uiFields; uiCount++ )
   {
      int   iWidth;
      int   iDec;
      PBYTE cFieldName = ( PBYTE ) sx_FieldName( ( WORD ) uiCount );

      iWidth  = sx_FieldWidth( cFieldName );
      iDec    = sx_FieldDecimals( cFieldName );

      /*
         iWidth and iDec MUST beinitialized as above else int returns 65535
         we cannot directly called:
         hb_arraySet( pItem, 3, hb_itemPutNI( pData, sx_FieldWidth( cFieldName )     ) );
         hb_arraySet( pItem, 4, hb_itemPutNI( pData, sx_FieldDecimals( cFieldName )  ) );
       */
      hb_arrayNew( pItem, 4 );
      hb_arraySet( pItem, 1, hb_itemPutC( pData, ( char * ) cFieldName ) );
      hb_arraySet( pItem, 2, hb_itemPutC( pData, ( char * ) sx_FieldType( cFieldName ) ) );
      hb_arraySet( pItem, 3, hb_itemPutNI( pData, iWidth ) );
      hb_arraySet( pItem, 4, hb_itemPutNI( pData, iDec ) );
      hb_arrayAdd( pStruct, pItem );
   }

   hb_itemRelease( pItem );
   hb_itemRelease( pData );

   return pStruct;
}

PHB_ITEM _sx_FieldNames( void )
{
   PHB_ITEM pStruct = hb_itemNew( NULL );
   HB_USHORT   uiFields,
            uiCount;
   char *   cFieldName;
   PHB_ITEM pItem;

   hb_arrayNew( pStruct, 0 );

   uiFields = sx_FieldCount();

   for( uiCount = 1; uiCount <= uiFields; uiCount++ )
   {
      cFieldName = ( char * ) sx_FieldName( uiCount );
      pItem      = hb_itemPutC( NULL, cFieldName );
      hb_arrayAdd( pStruct, pItem );
      hb_itemRelease( pItem );
   }

   return pStruct;
}
