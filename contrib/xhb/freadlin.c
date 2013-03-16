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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#define READING_BLOCK  4096

static char * hb_fsReadLine( HB_FHANDLE hFileHandle, HB_ISIZ * plBuffLen, const char ** pTerm, HB_ISIZ * pnTermSizes, HB_ISIZ nTerms, HB_BOOL * pbFound, HB_BOOL * pbEOF )
{
   HB_ISIZ nPosTerm = 0, nPos, nPosition;
   int     nTries;
   HB_ISIZ nRead = 0, nOffset, nSize;
   char *  pBuff;

   HB_TRACE( HB_TR_DEBUG, ( "hb_fsReadLine(%p, %" HB_PFS "d, %p, %p, %" HB_PFS "d, %p, %p)", ( void * ) ( HB_PTRDIFF ) hFileHandle, *plBuffLen, pTerm, pnTermSizes, nTerms, pbFound, pbEOF ) );

   *pbFound = HB_FALSE;
   *pbEOF   = HB_FALSE;
   nTries   = 0;
   nOffset  = 0;
   nSize    = *plBuffLen;

   if( *plBuffLen < 10 )
      *plBuffLen = READING_BLOCK;

   pBuff = ( char * ) hb_xgrab( *plBuffLen + 1 );

   do
   {
      if( nTries > 0 )
      {
         /* pBuff can be enlarged to hold the line as needed.. */
         nSize    = ( *plBuffLen * ( nTries + 1 ) ) + 1;
         pBuff    = ( char * ) hb_xrealloc( pBuff, nSize );
         nOffset += nRead;
      }

      /* read from file */
      nRead = hb_fsReadLarge( hFileHandle, pBuff + nOffset, nSize - nOffset );

      /* scan the read buffer */

      if( nRead > 0 )
      {
         for( nPos = 0; nPos < nRead; nPos++ )
         {
            for( nPosTerm = 0; nPosTerm < nTerms; nPosTerm++ )
            {
               /* Compare with the LAST terminator byte */
               if( pBuff[ nOffset + nPos ] == pTerm[ nPosTerm ][ pnTermSizes[ nPosTerm ] - 1 ] && ( pnTermSizes[ nPosTerm ] - 1 ) <= ( nPos + nOffset ) )
               {
                  *pbFound = HB_TRUE;

                  for( nPosition = 0; nPosition < ( pnTermSizes[ nPosTerm ] - 1 ); nPosition++ )
                  {
                     if( pTerm[ nPosTerm ][ nPosition ] != pBuff[ nOffset + ( nPos - pnTermSizes[ nPosTerm ] ) + nPosition + 1 ] )
                     {
                        *pbFound = HB_FALSE;
                        break;
                     }
                  }

                  if( *pbFound )
                     break;
               }
            }

            if( *pbFound )
               break;
         }

         if( *pbFound )
         {
            *plBuffLen = nOffset + nPos - pnTermSizes[ nPosTerm ] + 1;

            pBuff[ *plBuffLen ] = '\0';

            /* Set handle pointer in the end of the line */
            hb_fsSeekLarge( hFileHandle, ( ( nRead - nPos ) * -1 ) + 1, FS_RELATIVE );

            return pBuff;
         }
      }
      else
      {
         if( ! *pbFound )
         {
            if( nTries == 0 )
            {
               pBuff[ 0 ] = '\0';
               *plBuffLen = 0;
            }
            else
            {
               pBuff[ nOffset + nRead ] = '\0';
               *plBuffLen = nOffset + nRead;
            }

            *pbEOF = HB_TRUE;
         }
      }

      nTries++;
   }
   while( ! *pbFound && nRead > 0 );

   return pBuff;
}

/* PRG level fReadLine( <Handle>, <@buffer>, [<aTerminators | cTerminator>], [<nReadingBlock>] ) */

HB_FUNC( HB_FREADLINE )
{
   PHB_ITEM      pTerm1;
   HB_FHANDLE    hFileHandle = ( HB_FHANDLE ) hb_parnint( 1 );
   const char ** Term;
   char *        pBuffer;
   HB_ISIZ *     pnTermSizes;
   HB_ISIZ       nSize = hb_parns( 4 );
   HB_ISIZ       i, nTerms;
   HB_BOOL       bFound, bEOF;

   if( ( ! HB_ISBYREF( 2 ) ) || ( ! HB_ISNUM( 1 ) ) )
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
         nTerms = hb_arrayLen( pTerm1 );

         if( nTerms <= 0 )
         {
            hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, 4,
                                  hb_paramError( 1 ),
                                  hb_paramError( 2 ),
                                  hb_paramError( 3 ),
                                  hb_paramError( 4 ) );
            return;
         }

         Term        = ( const char ** ) hb_xgrab( sizeof( char * ) * nTerms );
         pnTermSizes = ( HB_ISIZ * ) hb_xgrab( sizeof( HB_ISIZ ) * nTerms );

         for( i = 0; i < nTerms; i++ )
         {
            Term[ i ]        = hb_arrayGetCPtr( pTerm1, i + 1 );
            pnTermSizes[ i ] = hb_arrayGetCLen( pTerm1, i + 1 );
         }
      }
      else
      {
         pTerm1           = hb_param( 3, HB_IT_STRING );
         Term             = ( const char ** ) hb_xgrab( sizeof( char * ) );
         pnTermSizes      = ( HB_ISIZ * ) hb_xgrab( sizeof( HB_ISIZ ) );
         Term[ 0 ]        = hb_itemGetCPtr( pTerm1 );
         pnTermSizes[ 0 ] = hb_itemGetCLen( pTerm1 );
         nTerms           = 1;
      }
   }
   else
   {
      Term             = ( const char ** ) hb_xgrab( sizeof( char * ) );
      pnTermSizes      = ( HB_ISIZ * ) hb_xgrab( sizeof( HB_ISIZ ) );
      Term[ 0 ]        = "\r\n";    /* Should be preplaced with the default EOL sequence */
      nTerms           = 1;
      pnTermSizes[ 0 ] = 2;
   }

   if( nSize == 0 )
      nSize = READING_BLOCK;

   pBuffer = hb_fsReadLine( hFileHandle, &nSize, Term, pnTermSizes, nTerms, &bFound, &bEOF );

   if( ! hb_storclen_buffer( pBuffer, nSize, 2 ) )
      hb_xfree( pBuffer );
   hb_retns( bEOF ? -1 : 0 );
   hb_xfree( ( void * ) Term );
   hb_xfree( pnTermSizes );
}
