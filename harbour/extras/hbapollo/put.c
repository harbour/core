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

HB_FUNC( SX_PUTBLOB )  /* The number of bytes written */
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! HB_ISNIL( 4 ) )
      iWorkArea = _sx_select( hb_param( 4, HB_IT_ANY ) );

   hb_retnl( sx_PutBlob( ( PBYTE ) hb_parc( 1 ) /* cpFieldName */,
                         ( PVOID ) hb_parc( 2 ) /* vpVar */, hb_parnl( 3 ) /* lSize */
                         ) );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_PUTRECORD )
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   sx_PutRecord( ( PBYTE ) hb_parc( 1 ) /* cpRecord */ );

   if( iWorkArea != SX_DUMMY_NUMBER )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_PUTVALUEEX )  /* ( aValues, nArea ) */
{
   WORD iWorkArea = SX_DUMMY_NUMBER;

   if( ! HB_ISNIL( 2 ) )
      iWorkArea = _sx_select( hb_param( 2, HB_IT_ANY ) );

   if( HB_ISARRAY( 1 ) )
   {
      PHB_ITEM pValue  = hb_param( 1, HB_IT_ARRAY );
      HB_USHORT   uiLen   = ( HB_USHORT ) hb_arrayLen( pValue );
      HB_USHORT   uiSize;
      WORD     uVarType;
      PBYTE    cFieldName;

      for( uiSize = 0; uiSize < uiLen; uiSize++ )
      {
         uVarType   = ( WORD ) hb_arrayGetType( pValue, uiSize + 1 );
         cFieldName = ( PBYTE ) sx_FieldName( ( WORD ) ( uiSize + 1 ) );
         switch( uVarType )
         {
            case HB_IT_DOUBLE:
            case HB_IT_LONG:
            case HB_IT_INTEGER:
            {
               double pDouble = hb_arrayGetND( pValue, uiSize + 1 );
               sx_Replace( cFieldName, R_DOUBLE, ( PVOID ) &pDouble );
            }
            break;

            case HB_IT_DATE:
            {
               char     szDate[ 9 ];
               char *   buffer;
               hb_dateDecStr( szDate, hb_arrayGetDL( pValue, uiSize + 1 ) );
               buffer = ( char * ) hb_xgrab( 11 );
   #if 0
               hb_dateFormat( szDate, buffer, hb_set.HB_SET_DATEFORMAT );
   #else
               hb_dateFormat( szDate, buffer, hb_setGetDateFormat() );
   #endif
               sx_Replace( cFieldName, R_DATESTR, ( PVOID ) buffer );
               hb_xfree( buffer );
            }
            break;

            case HB_IT_LOGICAL:
            {
               HB_BOOL bValue = hb_arrayGetL( pValue, uiSize + 1 );
               sx_Replace( cFieldName, R_LOGICAL, ( PVOID ) &bValue );
            }
            break;

            case HB_IT_STRING:
            {
               PBYTE    cVar       = ( PBYTE ) hb_arrayGetC( pValue, uiSize + 1 );
               char *   cFieldType = ( char * ) sx_FieldType( cFieldName );
               switch( *cFieldType )
               {
                  case 'C':
                     sx_Replace( cFieldName, R_CHAR, cVar );
                     hb_xfree( cVar );
                     break;

                  case 'D':
                     sx_Replace( cFieldName, R_DATESTR, cVar );
                     hb_xfree( cVar );
                     break;

                  case 'M':
                     sx_Replace( cFieldName, R_MEMO, cVar );
                     hb_xfree( cVar );
                     break;
               }
            }
            break;
         }  /* end switch( uVarType ) */
      }     /* end for( uiSize = 0; uiSize < uiLen; uiSize++ ) */
   }

   if( ! ( iWorkArea == SX_DUMMY_NUMBER ) )
      sx_Select( iWorkArea );
}

HB_FUNC( SX_PUTVARIANT )
{
/*
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   lpVariant: A variant containing the replacement data.
   Logical field values should be passed as either zero (HB_FALSE) or -1 (HB_TRUE).
   Date strings must be formatted according to the setting of sx_SetDateFormat
   and sx_SetCentury.
 */
   sx_PutVariant( ( PBYTE ) hb_parc( 1 ), ( PVOID ) hb_parc( 2 ) );
}
