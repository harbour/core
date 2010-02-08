/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * Text file reading functions
 *
 * Copyright 2003 Marcelo Lombardo - lombardo@uol.com.br
 * http://www.xharbour.org
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
#include "hbapifs.h"
#include "hbset.h"
#include "hbapiitm.h"
#include "hbapierr.h"

#define READING_BLOCK      4096

char * hb_fsReadLine( HB_FHANDLE hFileHandle, HB_ISIZ * plBuffLen, const char ** Term, HB_ISIZ * iTermSizes, HB_ISIZ iTerms, HB_BOOL * bFound, HB_BOOL * bEOF )
{
   HB_ISIZ iPosTerm = 0, iPos, iPosition;
   int nTries;
   HB_ISIZ lRead = 0, lOffset, lSize;
   char * pBuff;

   HB_TRACE(HB_TR_DEBUG, ("hb_fsReadLine(%p, %ld, %p, %p, %i, %i, %i)", ( void * ) ( HB_PTRDIFF ) hFileHandle, *plBuffLen, Term, iTermSizes, iTerms, *bFound, *bEOF ));

   *bFound  = HB_FALSE;
   *bEOF    = HB_FALSE;
   nTries   = 0;
   lOffset  = 0;
   lSize    = *plBuffLen;

   if( *plBuffLen < 10 )
      *plBuffLen = READING_BLOCK;

   pBuff = ( char * ) hb_xgrab( *plBuffLen + 1 );

   do
   {
      if( nTries > 0 )
      {
         /* pBuff can be enlarged to hold the line as needed.. */
         lSize = ( *plBuffLen * ( nTries + 1 ) ) + 1;
         pBuff = ( char * ) hb_xrealloc( pBuff, lSize );
         lOffset += lRead;
      }

      /* read from file */
      lRead = hb_fsReadLarge( hFileHandle, pBuff + lOffset, lSize - lOffset );

      /* scan the read buffer */

      if( lRead > 0 )
      {
         for( iPos = 0; iPos < lRead; iPos++ )
         {
            for( iPosTerm = 0; iPosTerm < iTerms; iPosTerm++ )
            {
               /* Compare with the LAST terminator byte */
               if( pBuff[lOffset+iPos] == Term[iPosTerm][iTermSizes[iPosTerm]-1] && (iTermSizes[iPosTerm]-1) <= (iPos+lOffset) )
               {
                  *bFound = HB_TRUE;

                  for(iPosition=0; iPosition < (iTermSizes[iPosTerm]-1); iPosition++)
                  {
                     if(Term[iPosTerm][iPosition] != pBuff[ lOffset+(iPos-iTermSizes[iPosTerm])+iPosition+1 ])
                     {
                        *bFound = HB_FALSE;
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
            *plBuffLen = lOffset + iPos - iTermSizes[ iPosTerm ] + 1;

            pBuff[ *plBuffLen ] = '\0';

            /* Set handle pointer in the end of the line */
            hb_fsSeek( hFileHandle, (((lRead-(( LONG ) iPos )))*-1)+1, FS_RELATIVE );

            return pBuff;
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

            *bEOF = HB_TRUE;
         }
      }

      nTries++;
   }
   while( ( ! *bFound ) && lRead > 0 );

   return pBuff;
}

/* PRG level fReadLine( <Handle>, <@buffer>, [<aTerminators | cTerminator>], [<nReadingBlock>] ) */

HB_FUNC( HB_FREADLINE )
{
   PHB_ITEM pTerm1;
   HB_FHANDLE hFileHandle  = ( HB_FHANDLE ) hb_parnint( 1 );
   const char ** Term;
   char * pBuffer;
   HB_ISIZ * iTermSizes;
   HB_ISIZ lSize = hb_parnl( 4 );
   HB_ISIZ i, iTerms;
   HB_BOOL bFound, bEOF;

   if( ( !HB_ISBYREF( 2 ) ) || ( !HB_ISNUM( 1 ) ) )
   {
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, 4,
         hb_paramError( 1 ),
         hb_paramError( 2 ),
         hb_paramError( 3 ),
         hb_paramError( 4 ) );
      return;
   }

   if( HB_ISARRAY( 3 ) || HB_ISCHAR( 3 ) )
   {
      if( HB_ISARRAY( 3 ) )
      {
         pTerm1 = hb_param( 3, HB_IT_ARRAY );
         iTerms = hb_arrayLen( pTerm1 );

         if( iTerms <= 0 )
         {
            hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, 4,
               hb_paramError( 1 ),
               hb_paramError( 2 ),
               hb_paramError( 3 ),
               hb_paramError( 4 ) );
            return;
         }

         Term = ( const char ** ) hb_xgrab( sizeof( char * ) * iTerms );
         iTermSizes = ( HB_ISIZ * ) hb_xgrab( sizeof( HB_ISIZ ) * iTerms );

         for( i = 0; i < iTerms; i++ )
         {
            Term[ i ]       = hb_arrayGetCPtr( pTerm1, i + 1 );
            iTermSizes[ i ] = hb_arrayGetCLen( pTerm1, i + 1 );
         }
      }
      else
      {
         pTerm1          = hb_param( 3, HB_IT_STRING );
         Term            = ( const char ** ) hb_xgrab( sizeof( char * ) );
         iTermSizes      = ( HB_ISIZ * ) hb_xgrab( sizeof( HB_ISIZ ) );
         Term[ 0 ]       = hb_itemGetCPtr( pTerm1 );
         iTermSizes[ 0 ] = hb_itemGetCLen( pTerm1 );
         iTerms          = 1;
      }
   }
   else
   {
      Term            = ( const char ** ) hb_xgrab( sizeof( char * ) );
      iTermSizes      = ( HB_ISIZ * ) hb_xgrab( sizeof( HB_ISIZ ) );
      Term[ 0 ]       = "\r\n";    /* Should be preplaced with the default EOL sequence */
      iTerms          = 1;
      iTermSizes[ 0 ] = 2;
   }

   if( lSize == 0 )
      lSize = READING_BLOCK;

   pBuffer = hb_fsReadLine( hFileHandle, &lSize, Term, iTermSizes, iTerms, &bFound, &bEOF );

   if( ! hb_storclen_buffer( pBuffer, lSize, 2 ) )
      hb_xfree( pBuffer );
   hb_retnl( bEOF ? -1 : 0 );
   hb_xfree( ( void * ) Term );
   hb_xfree( iTermSizes );
}
