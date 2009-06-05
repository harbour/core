/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * hbmake C support code
 *
 * Copyright 2000 Jose Lalin <dezac@corevia.com> [GAUGE*()]
 * Copyright 2000,2001 Luiz Rafael Culik <culik@sl.conex.net> [GETUSERLANG()]
 * Copyright 2003 Marcelo Lombardo - lombardo@uol.com.br [HB_FREADLINE()]
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

#define HB_OS_WIN_USED

#include "hbapi.h"
#include "hbapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbset.h"

/* Box array definitions */
#define B_TOP           1
#define B_LEFT          2
#define B_BOTTOM        3
#define B_RIGHT         4
#define B_BACKCOLOR     5
#define B_BARCOLOR      6
#define B_DISPLAYNUM    8
#define B_BARCHAR       7
#define B_PERCENT       9
#define B_LEN           B_PERCENT

#define B_BOXLINES      "ÚÄ¿³ÙÄÀ³"

static void hb_gaugeUpdate( PHB_ITEM pArray, float fPercent )
{
   SHORT iCenter = ( SHORT ) ( ( ( hb_arrayGetNI( pArray, B_RIGHT ) - hb_arrayGetNI( pArray, B_LEFT ) ) / 2 ) + 1 );
   SHORT iRatio = ( SHORT ) ( hb_arrayGetNI( pArray, B_RIGHT ) - hb_arrayGetNI( pArray, B_LEFT ) - 1 );
   SHORT iRow;
   SHORT iCols;
   SHORT iMax;
   char szOldColor[ HB_CLRSTR_LEN ];
   const char * szStr = "        ";
   char szPct[ 5 ];

   hb_gtGetColorStr( szOldColor );
   hb_gtSetColorStr( hb_arrayGetCPtr( pArray, B_BARCOLOR ) );

   fPercent = ( fPercent < 0 ? 0 : ( fPercent > 1 ? 1 : fPercent ) );
   iCols    = ( SHORT ) ( fPercent * iRatio );

   if( hb_arrayGetL( pArray, B_DISPLAYNUM ) )
   {
      hb_snprintf( szPct, sizeof( szPct ), "%3.0f%%", fPercent * 100 );
      hb_gtWriteAt( ( SHORT ) hb_arrayGetNI( pArray, B_TOP ),
                    iCenter + 2,
                    ( BYTE * ) szPct, 4 );
   }

   hb_gtBox( ( SHORT ) hb_arrayGetNI( pArray, B_TOP ) + 1,
             ( SHORT ) hb_arrayGetNI( pArray, B_LEFT ) + 1,
             ( SHORT ) hb_arrayGetNI( pArray, B_BOTTOM ) - 1,
             ( SHORT ) hb_arrayGetNI( pArray, B_RIGHT ) - 1,
             ( BYTE * ) szStr );

   iMax = ( SHORT ) ( hb_arrayGetNI( pArray, B_BOTTOM ) - hb_arrayGetNI( pArray, B_TOP ) - 1 );
   for( iRow = 1; iRow <= iMax; iRow++ )
   {
      hb_gtRepChar( ( SHORT ) ( hb_arrayGetNI( pArray, B_TOP ) + iRow ),
                    ( SHORT ) ( hb_arrayGetNI( pArray, B_LEFT ) + 1 ),
                    ( BYTE ) * hb_arrayGetCPtr( pArray, B_BARCHAR ),
                    iCols );
   }

   hb_gtSetColorStr( szOldColor );
}

/* GaugeNew( <nRowTop>, <nColumnTop>, <nRowBottom>, <nColumnBottom>,
      [<cBackgroundColor>],
      [<cGaugeColor>],
      [<cGaugeCharacter>] ) --> aGauge
*/
HB_FUNC( GAUGENEW )
{
   PHB_ITEM pReturn = hb_itemArrayNew( B_LEN );   /* Create array */

   hb_arraySetNL( pReturn, B_TOP, hb_parni( B_TOP ) );
   hb_arraySetNL( pReturn, B_LEFT, hb_parni( B_LEFT ) );
   hb_arraySetNL( pReturn, B_BOTTOM,
              ISNUM( B_BOTTOM ) ?
               ( hb_parni( B_BOTTOM ) < hb_parni( B_TOP ) + 2 ?
                   hb_parni( B_TOP ) + 2 : hb_parni( B_BOTTOM ) ) : 0 );
   hb_arraySetNL( pReturn, B_RIGHT,
              ISNUM( B_RIGHT ) ?
               ( hb_parni( B_RIGHT ) < hb_parni( B_LEFT ) + 4 ?
                  hb_parni( B_LEFT ) + 4 : hb_parni( B_RIGHT ) ) : 0 );
   hb_arraySetC( pReturn, B_BACKCOLOR, ISCHAR( B_BACKCOLOR ) ? hb_parc( B_BACKCOLOR ) : "W/N" );
   hb_arraySetC( pReturn, B_BARCOLOR, ISCHAR( B_BARCOLOR ) ? hb_parc( B_BARCOLOR ) : "W+/N" );
   hb_arraySetL( pReturn, B_DISPLAYNUM,
              !( ISNUM( B_RIGHT ) &&
                 ISNUM( B_LEFT ) &&
                 ( hb_parni( B_RIGHT ) < hb_parni( B_LEFT ) + 9 ) ) );
   hb_arraySetC( pReturn, B_BARCHAR, ISCHAR( B_BARCHAR ) ? hb_parc( B_BARCHAR ) : "\xdb" );
   hb_arraySetNL( pReturn, B_PERCENT, 0 );

   hb_itemReturnRelease( pReturn );
}

/* GaugeDisplay( aGauge ) --> aGauge
*/
HB_FUNC( GAUGEDISPLAY )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
   {
      SHORT iCenter = ( SHORT ) ( ( ( hb_arrayGetNI( pArray, B_RIGHT ) - hb_arrayGetNI( pArray, B_LEFT ) ) / 2 ) + 1 );
      char szOldColor[ HB_CLRSTR_LEN ];
      const char * szStr = "        ";

      hb_gtGetColorStr( szOldColor );
      hb_gtSetColorStr( hb_arrayGetCPtr( pArray, B_BACKCOLOR ) );

      hb_gtBox( ( SHORT) hb_arrayGetNI( pArray, B_TOP ),
                ( SHORT) hb_arrayGetNI( pArray, B_LEFT ),
                ( SHORT) hb_arrayGetNI( pArray, B_BOTTOM ),
                ( SHORT) hb_arrayGetNI( pArray, B_RIGHT ),
                ( BYTE * ) szStr );

      hb_gtBox( ( SHORT ) hb_arrayGetNI( pArray, B_TOP ),
                ( SHORT ) hb_arrayGetNI( pArray, B_LEFT ),
                ( SHORT ) hb_arrayGetNI( pArray, B_BOTTOM ),
                ( SHORT ) hb_arrayGetNI( pArray, B_RIGHT ),
                ( BYTE * ) B_BOXLINES );

      if( hb_arrayGetL( pArray, B_DISPLAYNUM ) )
         hb_gtWriteAt( ( SHORT ) hb_arrayGetNI( pArray, B_TOP ),
                       iCenter,
                       ( BYTE * ) "[      ]", 8 );

      hb_gtSetColorStr( szOldColor );

      hb_gaugeUpdate( pArray, ( float ) hb_arrayGetND( pArray, B_PERCENT ) );

      hb_itemReturn( pArray );
   }
}

/* GaugeUpdate( aGauge, nPercent ) --> aGauge
*/
HB_FUNC( GAUGEUPDATE )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
   {
      hb_gaugeUpdate( pArray, ISNUM( 2 ) ? ( float ) hb_parnd( 2 ) : 0 );

      hb_itemReturn( pArray );
   }
}

#define READING_BLOCK      4096

BYTE * hb_fsReadLine( HB_FHANDLE hFileHandle, LONG * plBuffLen, char ** Term, int * iTermSizes, USHORT iTerms, BOOL * bFound, BOOL * bEOF )
{
   USHORT uiPosTerm = 0, iPos, uiPosition;
   USHORT nTries;
   LONG lRead = 0, lOffset, lSize;
   BYTE * pBuff;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsReadLine(%p, %ld, %p, %p, %hu, %i, %i)", ( void * ) ( HB_PTRDIFF ) hFileHandle, *plBuffLen, Term, iTermSizes, iTerms, *bFound, *bEOF ));

   *bFound  = FALSE;
   *bEOF    = FALSE;
   nTries   = 0;
   lOffset  = 0;
   lSize    = *plBuffLen;

   if( *plBuffLen < 10 )
      *plBuffLen = READING_BLOCK;

   pBuff = ( BYTE * ) hb_xgrab( *plBuffLen );

   do
   {
      if( nTries > 0 )
      {
         /* pBuff can be enlarged to hold the line as needed.. */
         lSize = ( *plBuffLen * ( nTries + 1 ) ) + 1;
         pBuff = ( BYTE * ) hb_xrealloc( pBuff, lSize );
         lOffset += lRead;
      }

      /* read from file */
      lRead = hb_fsReadLarge( hFileHandle, pBuff + lOffset, lSize - lOffset );

      /* scan the read buffer */

      if( lRead > 0 )
      {
         for( iPos = 0; iPos < lRead; iPos++ )
         {
            for( uiPosTerm = 0; uiPosTerm < iTerms; uiPosTerm++ )
            {
               /* Compare with the LAST terminator byte */
               if( pBuff[lOffset+iPos] == Term[uiPosTerm][iTermSizes[uiPosTerm]-1] && (iTermSizes[uiPosTerm]-1) <= (iPos+lOffset) )
               {
                  *bFound = TRUE;

                  for(uiPosition=0; uiPosition < (iTermSizes[uiPosTerm]-1); uiPosition++)
                  {
                     if(Term[uiPosTerm][uiPosition] != pBuff[ lOffset+(iPos-iTermSizes[uiPosTerm])+uiPosition+1 ])
                     {
                        *bFound = FALSE;
                        break;
                     }
                  }

                  if( *bFound )
                     break;
               }
            }

            if( *bFound )
               break;
         }

         if( *bFound )
         {
            *plBuffLen = lOffset + iPos - iTermSizes[ uiPosTerm ] + 1;

            pBuff[ *plBuffLen ] = '\0';

            /* Set handle pointer in the end of the line */
            hb_fsSeek( hFileHandle, (((lRead-((LONG)iPos)))*-1)+1, FS_RELATIVE );

            return( pBuff );
         }
      }
      else
      {
         if( ! *bFound )
         {
            if( nTries == 0 )
            {
               pBuff[ 0 ] = '\0';
               *plBuffLen = 0;
            }
            else
            {
               pBuff[ lOffset + lRead ] = '\0';
               *plBuffLen = lOffset + lRead;
            }

            *bEOF = TRUE;
         }
      }

      nTries++;
   }
   while( ( ! *bFound ) && lRead > 0 );

   return( pBuff );
}

/* PRG level fReadLine( <Handle>, <@buffer>, [<aTerminators | cTerminator>], [<nReadingBlock>] ) */

HB_FUNC( HB_FREADLINE )
{
   PHB_ITEM pTerm1;
   HB_FHANDLE hFileHandle  = ( HB_FHANDLE ) hb_parnl( 1 );
   char ** Term;
   BYTE * pBuffer;
   int * iTermSizes;
   LONG lSize = hb_parnl( 4 );
   USHORT i, iTerms;
   BOOL bFound, bEOF;

   if( ( !ISBYREF( 2 ) ) || ( !ISNUM( 1 ) ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, 4,
         hb_paramError( 1 ),
         hb_paramError( 2 ),
         hb_paramError( 3 ),
         hb_paramError( 4 ) );
      return;
   }

   if( ISARRAY( 3 ) || ISCHAR( 3 ) )
   {
      if( ISARRAY( 3 ) )
      {
         pTerm1 = hb_param( 3, HB_IT_ARRAY );
         iTerms = ( USHORT ) hb_arrayLen( pTerm1 );

         if( iTerms <= 0 )
         {
            hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, 4,
               hb_paramError( 1 ),
               hb_paramError( 2 ),
               hb_paramError( 3 ),
               hb_paramError( 4 ) );
            return;
         }

         Term = ( char ** ) hb_xgrab( sizeof( char * ) * iTerms );
         iTermSizes = ( int * ) hb_xgrab( sizeof( int ) * iTerms );

         for( i = 0; i < iTerms; i++ )
         {
            Term[ i ]       = hb_arrayGetCPtr( pTerm1, i + 1 );
            iTermSizes[ i ] = hb_arrayGetCLen( pTerm1, i + 1 );
         }
      }
      else
      {
         pTerm1          = hb_param( 3, HB_IT_STRING );
         Term            = ( char ** ) hb_xgrab( sizeof( char * ) );
         iTermSizes      = ( int * ) hb_xgrab( sizeof( int ) );
         Term[ 0 ]       = ( char * ) hb_itemGetCPtr( pTerm1 );
         iTermSizes[ 0 ] = hb_itemGetCLen( pTerm1 );
         iTerms          = 1;
      }
   }
   else
   {
      Term            = ( char ** ) hb_xgrab( sizeof( char * ) );
      iTermSizes      = ( int * ) hb_xgrab( sizeof( int ) );
      Term[ 0 ]       = ( char * ) "\r\n";    /* Should be preplaced with the default EOL sequence */
      iTerms          = 1;
      iTermSizes[ 0 ] = 2;
   }

   if( lSize == 0 )
      lSize = READING_BLOCK;

   pBuffer = hb_fsReadLine( hFileHandle, &lSize, Term, iTermSizes, iTerms, &bFound, &bEOF );

   if( ! hb_storclen_buffer( ( char * ) pBuffer, lSize, 2 ) )
      hb_xfree( pBuffer );
   hb_retnl( bEOF ? -1 : 0 );
   hb_xfree( Term );
   hb_xfree( iTermSizes );
}

HB_FUNC( GETUSERLANG )
{
   long lRet;

#if defined(HB_OS_WIN) && (!defined(__RSXNT__)) && (!defined(__CYGWIN__))

   switch( GetSystemDefaultLangID() )
   {
   case 0x0416:
   case 0x0816:
      lRet = 1;
      break;

   case 0x0409:
   case 0x0809:
   case 0x0C09:
   case 0x1009:
   case 0x1409:
   case 0x1809:
   case 0x1C09:
   case 0x2009:
   case 0x2409:
   case 0x2809:
   case 0x2C09:
      lRet = 2;
      break;

   case 0x040A:
   case 0x080A:
   case 0x0C0A:
   case 0x100A:
   case 0x140A:
   case 0x180A:
   case 0x1C0A:
   case 0x200A:
   case 0x240A:
   case 0x280A:
   case 0x2C0A:
   case 0x300A:
   case 0x340A:
   case 0x380A:
   case 0x3C0A:
   case 0x400A:
   case 0x440A:
   case 0x480A:
   case 0x4C0A:
   case 0x500A:
      lRet = 3;
      break;

   default:
      lRet = 2;
      break;
   }
#else
   lRet = 2;
#endif
   hb_retnl( lRet );
}
