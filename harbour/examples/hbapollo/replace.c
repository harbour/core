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

HB_FUNC_EXTERN( SX_REPLACEARRAY );

HB_FUNC( SX_REPLACE )  /* ( cpFieldName, xData, cArea ) */
{
   PHB_ITEM pItem = hb_param( 2, HB_IT_ANY );

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_REPLACE" );

   if( HB_ISCHAR( 1 ) )
   {
      WORD  wPreviousArea = SX_DUMMY_NUMBER;
      PBYTE cFieldName    = ( PBYTE ) hb_parc( 1 );

      if( ! HB_ISNIL( 3 ) )
         wPreviousArea = _sx_select( hb_param( 3, HB_IT_ANY ) );

      switch( hb_itemType( pItem ) )
      {
         case HB_IT_ARRAY:
         {
            PHB_ITEM pAlias = hb_itemNew( NULL );
            PHB_ITEM pFieldName = hb_itemNew( NULL );
            PHB_ITEM pResult;

            hb_itemPutC( pAlias, (const char*) sx_Alias( 0 ) );
            hb_itemPutC( pFieldName, (const char*) cFieldName );
            pResult = hb_itemDoC( "SX_REPLACEARRAY", 3, pFieldName, hb_param( 2, HB_IT_ARRAY ), pAlias );
            hb_itemRelease( pAlias );
            hb_itemRelease( pFieldName );
            hb_itemRelease( pResult );
            break;
         }

         case HB_IT_STRING:
         {
            PVOID    pValue     = ( PVOID ) hb_parc( 2 );
            char *   cFieldType = ( char * ) sx_FieldType( cFieldName );
            switch( *cFieldType )
            {
               case 'C':
                  sx_Replace( cFieldName, R_CHAR, pValue );
                  break;

               case 'D':
                  sx_Replace( cFieldName, R_DATESTR, pValue );
                  break;

               case 'M':
                  sx_Replace( cFieldName, R_MEMO, pValue );
                  break;
            }

            hb_retc( ( char * ) pValue );
            break;
         }     /* enf of if ( HB_IS_STRING( vData ) ) */

         case HB_IT_DOUBLE:
         {
            double pValue = hb_parnd( 2 );
            sx_Replace( cFieldName, R_DOUBLE, ( PVOID ) &pValue );
            hb_retnd( pValue );
            break;
         }

         case HB_IT_LONG:
         {
            long pValue = hb_parnl( 2 );
            sx_Replace( cFieldName, R_LONG, ( PVOID ) &pValue );
            hb_retnl( pValue );
            break;
         }

         case HB_IT_INTEGER:
         {
            int pValue = hb_parni( 2 );
            sx_Replace( cFieldName, R_INTEGER, ( PVOID ) &pValue );
            hb_retni( pValue );
            break;
         }

         case HB_IT_LOGICAL:
         {
            HB_BOOL pValue = hb_parl( 2 );
            sx_Replace( cFieldName, R_LOGICAL, ( PVOID ) &pValue );
            hb_retl( pValue );
            break;
         }     /* end of else if ( HB_IS_LOGICAL( vData ) ) */

         case HB_IT_DATE:
         {
            long lJulian = hb_itemGetDL( pItem );
            sx_Replace( cFieldName, R_JULIAN, ( PVOID ) &lJulian );
            break;
         }     /* end of else if ( HB_IS_DATE( vData ) ) */
      }

      /* Back to Previous Area */
      if( ! ( wPreviousArea == SX_DUMMY_NUMBER ) )
         sx_Select( wPreviousArea );
   }
}

HB_FUNC( SX_FIELDPUT )
{
   HB_FUNCNAME( SX_REPLACE ) ();
}

HB_FUNC( SX_REPLACEBITMAP )  /* ( cpFieldName, xData, cArea ) */
{
   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_REPLACEBITMAP" );

   if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
   {
      WORD  wPreviousArea = SX_DUMMY_NUMBER;
      PVOID pValue;
      PBYTE cFieldName    = ( PBYTE ) hb_parc( 1 );

      if( ! HB_ISNIL( 3 ) )
         wPreviousArea = _sx_select( hb_param( 3, HB_IT_ANY ) );

      pValue = ( PVOID ) hb_parc( 2 );
      sx_Replace( cFieldName, R_BITMAP, pValue );

      /* Back to Previous Area */
      if( ! ( wPreviousArea == SX_DUMMY_NUMBER ) )
         sx_Select( wPreviousArea );
   }
}

HB_FUNC( __SX_FORCELINK_SX_REPLACEARRAY )
{
   HB_FUNC_EXEC( SX_REPLACEARRAY );
}

HB_FUNC( SX_REPLACEBLOB )    /* ( cpFieldName, xData, cArea ) */
{
   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_REPLACEBLOB" );

   if( HB_ISCHAR( 1 ) )
   {
      WORD  wPreviousArea = SX_DUMMY_NUMBER;
      PVOID pValue;
      PBYTE cFieldName    = ( PBYTE ) hb_parc( 1 );
      long  lResult       = 0;

      if( ! HB_ISNIL( 3 ) )
         wPreviousArea = _sx_select( hb_param( 3, HB_IT_ANY ) );

      if( HB_ISCHAR( 2 ) )
      {
         pValue = ( PVOID ) hb_parc( 2 );
         sx_Replace( cFieldName, R_BLOBFILE, pValue );
      }
      else if( HB_ISARRAY( 2 ) )
      {
         PHB_ITEM pAlias = hb_itemNew( NULL );
         PHB_ITEM pFieldName = hb_itemNew( NULL );
         PHB_ITEM pResult;

         hb_itemPutC( pAlias, (const char*) sx_Alias( 0 ) );
         hb_itemPutC( pFieldName, (const char*) cFieldName );
         pResult = hb_itemDoC( "SX_REPLACEARRAY", 3, pFieldName, hb_param( 2, HB_IT_ARRAY ), pAlias );
         lResult = hb_itemGetL( pResult );
         hb_itemRelease( pAlias );
         hb_itemRelease( pFieldName );
         hb_itemRelease( pResult );
      }

      /* Back to Previous Area */
      if( ! ( wPreviousArea == SX_DUMMY_NUMBER ) )
         sx_Select( wPreviousArea );

      hb_retnl( lResult );
   }
}

HB_FUNC( SX_REPLACEEX )      /* ( aReplace, cArea ) */
{
   WORD wPreviousArea = SX_DUMMY_NUMBER;

   if( ! _sx_Used() )
      hb_errRT_DBCMD( EG_NOTABLE, EDBCMD_NOTABLE, NULL, "SX_REPLACEEX" );

   if( HB_ISARRAY( 1 ) )
   {
      PHB_ITEM pReplace   = hb_param( 1, HB_IT_ARRAY );
      PHB_ITEM pInfo;
      HB_USHORT   uiLen      = ( HB_USHORT ) hb_arrayLen( pReplace );
      HB_USHORT   uiSize;
      PBYTE    cFieldName;
      WORD     uVarType;

      if( ! HB_ISNIL( 2 ) )
         wPreviousArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

      for( uiSize = 0; uiSize < uiLen; uiSize++ )
      {
         pInfo = hb_arrayGetItemPtr( pReplace, uiSize + 1 );

         /* Give Protection of NOT an array */
         if( HB_IS_ARRAY( pInfo ) && hb_arrayLen( pInfo ) >= 2 )
         {
            /* if ( pInfo->item.asArray.value->ulLen >= 2 ) */
            cFieldName = ( PBYTE ) hb_arrayGetC( pInfo, 1 );
            uVarType   = ( WORD ) hb_arrayGetType( pInfo, 2 );
            switch( uVarType )
            {
               case HB_IT_DOUBLE:
               case HB_IT_LONG:
               case HB_IT_INTEGER:
               {
                  double pDouble = hb_arrayGetND( pInfo, 2 );
                  sx_Replace( cFieldName, R_DOUBLE, ( PVOID ) &pDouble );
               }

                  hb_xfree( cFieldName );
                  break;

               case HB_IT_DATE:
               {
                  long lJulian = hb_arrayGetDL( pInfo, 2 );
                  sx_Replace( cFieldName, R_JULIAN, ( PVOID ) &lJulian );
               }

                  hb_xfree( cFieldName );
                  break;

               case HB_IT_LOGICAL:
               {
                  HB_BOOL pValue = hb_arrayGetL( pInfo, 2 );
                  sx_Replace( cFieldName, R_LOGICAL, ( PVOID ) &pValue );
                  hb_xfree( cFieldName );
               }
               break;

               case HB_IT_STRING:
               {
                  PBYTE    cVar       = ( PBYTE ) hb_arrayGetC( pInfo, 2 );
                  char *   cFieldType = ( char * ) sx_FieldType( cFieldName );
                  switch( *cFieldType )
                  {
                     case 'C':
                        sx_Replace( cFieldName, R_CHAR, cVar );
                        break;

                     case 'D':
                        sx_Replace( cFieldName, R_DATESTR, cVar );
                        break;

                     case 'M':
                        sx_Replace( cFieldName, R_MEMO, cVar );
                        break;
                  }

                  hb_xfree( cVar );
                  hb_xfree( cFieldName );
               }
               break;
            }
         }
      }

      if( ! ( wPreviousArea == SX_DUMMY_NUMBER ) )
         sx_Select( wPreviousArea );
   }
}

HB_FUNC( SX_BLOBTOFILE )
{
   if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
   {
      WORD     iWorkArea = SX_DUMMY_NUMBER;
      PBYTE    cpFieldName;
      char *   pFieldType;

      cpFieldName = ( PBYTE ) hb_parc( 1 );

      /* Work area passed */
      if( ! HB_ISNIL( 3 ) )
         iWorkArea = _sx_select( hb_param( 1, HB_IT_ANY ) );

      /* Check if the field is of MEMO type */
      pFieldType = ( char * ) sx_FieldType( cpFieldName );
      switch( *pFieldType )
      {
         case 'P':
         case 'B':
            hb_retl( sx_BlobToFile( cpFieldName, ( PBYTE ) hb_parc( 2 ) ) );
            break;

         default:
            hb_retl( HB_FALSE );
      }

      if( ! ( iWorkArea == SX_DUMMY_NUMBER ) )
         sx_Select( iWorkArea );
   }
   else
      hb_retl( HB_FALSE );
}
