/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour Graphic Terminal low level code
 *
 * Copyright 2006 Przemyslaw Czerpak < druzus /at/ priv.onet.pl >
 * www - http://harbour-project.org
 *
 * part of the code in hb_gt_def_* functions is based on the code
 * from old hbapi.c copyrighted by:
 * Copyright 1999 Bil Simser <bsimser@home.com>
 * Copyright 1999 Paul Tucker <ptucker@sympatico.ca>
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.hu)
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

#define HB_GT_NAME      NUL

#include "hbgtcore.h"
#include "hbapiitm.h"
#include "hbapifs.h"
#include "hbapierr.h"
#include "hbapicdp.h"
#include "hbdate.h"
#include "hbset.h"
#include "hbvm.h"
#include "hbthread.h"
#include "hbstack.h"

PHB_GT hb_gt_Base( void )
{
   PHB_GT pGT = ( PHB_GT ) hb_stackGetGT();

   if( pGT && HB_GTSELF_LOCK( pGT ) )
      return pGT;
   else
      return NULL;
}

void hb_gt_BaseFree( PHB_GT pGT )
{
   if( pGT )
      HB_GTSELF_UNLOCK( pGT );
}

/* helper internal function */
static void hb_gt_def_BaseInit( PHB_GT_BASE pGT )
{
   pGT->fVgaCell     = HB_TRUE;
   pGT->fIsColor     = HB_TRUE;
   pGT->fBlinking    = HB_TRUE;
   pGT->fStdOutCon   = HB_FALSE;
   pGT->fStdErrCon   = HB_FALSE;
   pGT->iCursorShape = SC_NORMAL;
   pGT->iDispCount   = 0;
   pGT->iExtCount    = 0;
   pGT->usClearChar  = ' ';
   pGT->iClearColor  = 0x07;
   pGT->iHeight      = 24;
   pGT->iWidth       = 80;
   pGT->hStdIn       = HB_STDIN_HANDLE;
   pGT->hStdOut      = HB_STDOUT_HANDLE;
   pGT->hStdErr      = HB_STDERR_HANDLE;

   pGT->iDoubleClickSpeed = 168; /* In milliseconds */

   pGT->inkeyBuffer     = pGT->defaultKeyBuffer;
   pGT->inkeyBufferSize = HB_DEFAULT_INKEY_BUFSIZE;

   pGT->pMutex       = hb_threadMutexCreate();
   if( pGT->pMutex )
      hb_gcUnlock( pGT->pMutex );
}

static void * hb_gt_def_New( PHB_GT pGT )
{
   HB_SIZE nSize, nIndex;
   HB_USHORT usChar;
   int iColor;
   HB_BYTE bAttr;
   int i;

   hb_gt_def_BaseInit( pGT );

   HB_GTSELF_GETSIZE( pGT, &pGT->iHeight, &pGT->iWidth );
   nSize = ( HB_SIZE ) pGT->iHeight * pGT->iWidth;

   pGT->screenBuffer =
            ( PHB_SCREENCELL ) hb_xgrab( sizeof( HB_SCREENCELL ) * nSize );
   pGT->prevBuffer =
            ( PHB_SCREENCELL ) hb_xgrab( sizeof( HB_SCREENCELL ) * nSize );
   pGT->pLines = ( HB_BOOL * ) hb_xgrab( sizeof( HB_BOOL ) * pGT->iHeight );

   memset( pGT->prevBuffer, 0, sizeof( HB_SCREENCELL ) * nSize );
   for( i = 0; i < pGT->iHeight; ++i )
      pGT->pLines[ i ] = HB_TRUE;

   usChar = HB_GTSELF_GETCLEARCHAR( pGT );
   iColor = HB_GTSELF_GETCLEARCOLOR( pGT );
   bAttr  = 0;
   for( nIndex = 0; nIndex < nSize; ++nIndex )
   {
      pGT->screenBuffer[ nIndex ].c.usChar = usChar;
      pGT->screenBuffer[ nIndex ].c.bColor = ( HB_BYTE ) iColor;
      pGT->screenBuffer[ nIndex ].c.bAttr = bAttr;
      pGT->prevBuffer[ nIndex ].c.bAttr = HB_GT_ATTR_REFRESH;
   }

   return pGT;
}

static void hb_gt_def_Free( PHB_GT pGT )
{
   if( pGT == ( PHB_GT ) hb_stackGetGT() )
      hb_stackSetGT( NULL );

   if( pGT->screenBuffer )
      hb_xfree( pGT->screenBuffer );
   if( pGT->prevBuffer )
      hb_xfree( pGT->prevBuffer );
   if( pGT->pLines )
      hb_xfree( pGT->pLines );
   if( pGT->iColorCount > 0 )
      hb_xfree( pGT->pColor );

   if( pGT->pNotifierBlock )
      hb_itemRelease( pGT->pNotifierBlock );
   if( pGT->pInkeyFilterBlock )
      hb_itemRelease( pGT->pInkeyFilterBlock );
   if( pGT->pInkeyReadBlock )
      hb_itemRelease( pGT->pInkeyReadBlock );
   if( pGT->pCargo )
      hb_itemRelease( pGT->pCargo );

   if( pGT->pMutex )
      hb_itemRelease( pGT->pMutex );

   if( pGT->pFuncTable )
      hb_xfree( pGT->pFuncTable );

   hb_xfree( pGT );
}

static void hb_gt_def_Mark( PHB_GT pGT )
{
   if( pGT->pNotifierBlock )
      hb_gcMark( pGT->pNotifierBlock );
   if( pGT->pInkeyFilterBlock )
      hb_gcMark( pGT->pInkeyFilterBlock );
   if( pGT->pInkeyReadBlock )
      hb_gcMark( pGT->pInkeyReadBlock );
   if( pGT->pCargo )
      hb_gcMark( pGT->pCargo );
   if( pGT->pMutex )
      hb_gcMark( pGT->pMutex );
}

static HB_BOOL hb_gt_def_Lock( PHB_GT pGT )
{
   return !pGT->pMutex || hb_threadMutexLock( pGT->pMutex );
}

static void hb_gt_def_Unlock( PHB_GT pGT )
{
   if( pGT->pMutex )
      hb_threadMutexUnlock( pGT->pMutex );
}

static void hb_gt_def_Init( PHB_GT pGT, HB_FHANDLE hStdIn, HB_FHANDLE hStdOut, HB_FHANDLE hStdErr )
{
   HB_GTSELF_NEW( pGT );

   pGT->hStdIn  = hStdIn;
   pGT->hStdOut = hStdOut;
   pGT->hStdErr = hStdErr;

   HB_GTSELF_RESIZE( pGT, pGT->iHeight, pGT->iWidth );
   HB_GTSELF_MOUSEINIT( pGT );
   HB_GTSELF_MOUSEGETPOS( pGT, &pGT->iMouseLastRow, &pGT->iMouseLastCol );
}

static void hb_gt_def_Exit( PHB_GT pGT )
{
   HB_GTSELF_MOUSEEXIT( pGT );
   HB_GTSELF_INKEYEXIT( pGT );

   HB_GTSELF_FREE( pGT );
}

static HB_BOOL hb_gt_def_CheckPos( PHB_GT pGT, int iRow, int iCol, long *plIndex )
{
   if( iRow >= 0 && iCol >= 0 )
   {
      int iHeight, iWidth;

      HB_GTSELF_GETSIZE( pGT, &iHeight, &iWidth );
      if( iRow < iHeight && iCol < iWidth )
      {
         if( plIndex )
            *plIndex = ( long ) iRow * iWidth + iCol;
         return HB_TRUE;
      }
   }
   return HB_FALSE;
}

static void hb_gt_def_GetPos( PHB_GT pGT, int * piRow, int * piCol )
{
   * piRow = pGT->iRow;
   * piCol = pGT->iCol;
}

static void hb_gt_def_SetPos( PHB_GT pGT, int iRow, int iCol )
{
   pGT->iRow = iRow;
   pGT->iCol = iCol;
}

static int hb_gt_def_MaxCol( PHB_GT pGT )
{
   return pGT->iWidth - 1;
}

static int hb_gt_def_MaxRow( PHB_GT pGT )
{
   return pGT->iHeight - 1;
}

static HB_BOOL hb_gt_def_IsColor( PHB_GT pGT )
{
   return pGT->fIsColor;
}

/* NOTE: szColorString must be at least HB_CLRSTR_LEN wide by the NG. It seems
         that CA-Cl*pper SETCOLOR() will return string lengths up to 131+EOF.
         That seems like a 127+1 buffer size, plus lazy overflow checking.
         [vszakats] */
static void hb_gt_def_GetColorStr( PHB_GT pGT, char * pszColorString )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_GetColorStr(%p,%s)", pGT, pszColorString));

   HB_GTSELF_COLORSTOSTRING( pGT, pGT->pColor, pGT->iColorCount,
                             pszColorString, HB_CLRSTR_LEN );
}

static void hb_gt_def_SetColorStr( PHB_GT pGT, const char * szColorString )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_SetColorStr(%p,%s)", pGT, szColorString));

   HB_GTSELF_STRINGTOCOLORS( pGT, szColorString, &pGT->pColor, &pGT->iColorCount );
   pGT->iColorIndex = HB_CLR_STANDARD; /* HB_GTSELF_COLORSELECT( pGT, HB_CLR_STANDARD ); */
}

static void hb_gt_def_ColorSelect( PHB_GT pGT, int iColorIndex )
{
   if( iColorIndex >= 0 && iColorIndex < pGT->iColorCount )
      pGT->iColorIndex = iColorIndex;
}

static int  hb_gt_def_GetColor( PHB_GT pGT )
{
   if( pGT->iColorCount )
      return pGT->pColor[ pGT->iColorIndex ];
   else
      return HB_GTSELF_GETCLEARCOLOR( pGT );
}

static void hb_gt_def_GetColorData( PHB_GT pGT, int ** pColorsPtr, int * piColorCount, int * piColorIndex )
{
   if( pGT->iColorCount )
   {
      *pColorsPtr = ( int * ) hb_xgrab( pGT->iColorCount * sizeof( int ) );
      memcpy( *pColorsPtr, pGT->pColor, pGT->iColorCount * sizeof( int ) );
      *piColorCount = pGT->iColorCount;
      *piColorIndex = pGT->iColorIndex;
   }
   else
   {
      *pColorsPtr = ( int * ) hb_xgrab( sizeof( int ) );
      *pColorsPtr[ 0 ] = 0;
      *piColorCount = 1;
      *piColorIndex = 0;
   }
}

static int  hb_gt_def_GetClearColor( PHB_GT pGT )
{
   return pGT->iClearColor;
}

static void hb_gt_def_SetClearColor( PHB_GT pGT, int iColor )
{
   pGT->iClearColor = ( iColor & 0xFF );
}

static HB_USHORT hb_gt_def_GetClearChar( PHB_GT pGT )
{
   return pGT->usClearChar;
}

static void hb_gt_def_SetClearChar( PHB_GT pGT, HB_USHORT usChar )
{
   pGT->usClearChar = usChar;
}

/* helper internal function */
/* masks: 0x0007     Foreground
          0x0070     Background
          0x0008     Bright
          0x0080     Blink
          0x0800     Underline foreground
          0x8000     Underline background
 */
static const char * hb_gt_def_ColorDecode( const char * szColorString, int * piColor )
{
   char c;
   int nColor = 0, iCount = 0;
   HB_BOOL bFore = HB_TRUE;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_ColorDecode(%s,%p)", szColorString, piColor));

   while( ( c = *szColorString++ ) != 0 )
   {
      switch( c )
      {
         case '*':
            nColor |= 0x80;
            break;

         case '+':
            nColor |= 0x08;
            break;

         case '/':
            if( !bFore )
               nColor = ( ( nColor >> 4 ) & 0x0F07 ) | ( nColor & 0x88 );
            else
               bFore = HB_FALSE;
            break;

         case 'b':
         case 'B':
            nColor |= bFore ? 0x01: 0x10;
            break;

         case 'g':
         case 'G':
            nColor |= bFore ? 0x02: 0x20;
            break;

         case 'r':
         case 'R':
            nColor |= bFore ? 0x04: 0x40;
            break;

         case 'w':
         case 'W':
            nColor |= bFore ? 0x07: 0x70;
            break;

         case 'n':
         case 'N':
            nColor &= bFore ? 0xFFF8: 0xFF8F;
            break;

         case 'i':
         case 'I':
            bFore = HB_FALSE;
            nColor &= 0x88;
            nColor |= 0x70;
            break;

         case 'x':
         case 'X':
            nColor &= 0x88;
            break;

         case 'u':
         case 'U':
            if( bFore )
               nColor = ( nColor & 0xF0F8 ) | 0x0801;
            else
               nColor = ( nColor & 0x0F8F ) | 0x8010;
            break;

         case ',':
            * piColor = iCount == 0 ? -1 : nColor;
            return szColorString;

         default:
            if( c >= '0' && c <= '9' )
            {
               int iColor = c - '0';
               while( *szColorString >= '0' && *szColorString <= '9' )
                  iColor = iColor * 10 + ( *szColorString++ - '0' );
               iColor &= 0x0f;
               if( bFore )
                  nColor = ( nColor & 0xF0F8 ) | iColor;
               else
                  nColor = ( nColor & 0x0F8F ) | ( iColor << 4 );
            }
            else
               --iCount;
      }
      ++iCount;
   }

   * piColor = iCount == 0 ? -1 : nColor;
   return NULL;
}

static int  hb_gt_def_ColorNum( PHB_GT pGT, const char * szColorString )
{
   int nColor;

   HB_SYMBOL_UNUSED( pGT );
   hb_gt_def_ColorDecode( szColorString, &nColor );

   return nColor;
}

static void hb_gt_def_StringToColors( PHB_GT pGT, const char * szColorString, int ** pColorsPtr, int * piColorCount )
{
   int * pColors;
   int nPos = 0;
   int nColor;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_StringToColors(%p,%s,%p,%p)", pGT, szColorString, pColorsPtr, piColorCount));

   HB_SYMBOL_UNUSED( pGT );

   if( *piColorCount == 0 )
   {
      *piColorCount = HB_CLR_MAX_ + 1;
      *pColorsPtr = ( int * ) hb_xgrab( *piColorCount * sizeof( int ) );
      memset( *pColorsPtr, 0, *piColorCount * sizeof( int ) );
   }

   pColors = *pColorsPtr;

   if( !szColorString || !*szColorString )
   {
      pColors[ HB_CLR_STANDARD   ] = 0x07;
      pColors[ HB_CLR_ENHANCED   ] = 0x70;
      pColors[ HB_CLR_BORDER     ] = 0;
      pColors[ HB_CLR_BACKGROUND ] = 0;
      pColors[ HB_CLR_UNSELECTED ] = 0x70;
   }
   else do
   {
      szColorString = hb_gt_def_ColorDecode( szColorString, &nColor );

      if( nPos == *piColorCount )
      {
         ++*piColorCount;
         pColors = *pColorsPtr = ( int * ) hb_xrealloc( pColors, *piColorCount * sizeof( int ) );
         pColors[ nPos ] = 0;
      }
      if( nColor != -1 )
      {
         pColors[ nPos ] = nColor;
         if( nPos == HB_CLR_ENHANCED && *piColorCount > HB_CLR_UNSELECTED )
            pColors[ HB_CLR_UNSELECTED ] = nColor;
      }
      ++nPos;
   }
   while( szColorString );
}

static void hb_gt_def_ColorsToString( PHB_GT pGT, int * pColors, int iColorCount, char * pszColorString, int iBufSize )
{
   int iColorIndex, iPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_ColorsToString(%p,%p,%d,%p,%d)", pGT, pColors, iColorCount, pszColorString, iBufSize));

   HB_SYMBOL_UNUSED( pGT );

   /* Go on if there's space left for the largest color string plus EOF */
   for( iColorIndex = iPos = 0; iColorIndex < iColorCount && iPos < iBufSize - 8; ++iColorIndex )
   {
      int nColor = pColors[ iColorIndex ] & 7;
      int j;

      if( iColorIndex > 0 )
         pszColorString[ iPos++ ] = ',';

      for( j = 0; j <= 1; j++ )
      {
         if( ( pColors[ iColorIndex ] & ( j ? 0x8000 : 0x0800 ) ) == 0 )
         {
            if( nColor == 7 )
               pszColorString[ iPos++ ] = 'W';
            else if( nColor == 0 )
               pszColorString[ iPos++ ] = 'N';
            else
            {
               if( ( nColor & 1 ) != 0 )
                  pszColorString[ iPos++ ] = 'B';

               if( ( nColor & 2 ) != 0 )
                  pszColorString[ iPos++ ] = 'G';

               if( ( nColor & 4 ) != 0 )
                  pszColorString[ iPos++ ] = 'R';
            }
         }
         else
            pszColorString[ iPos++ ] = 'U';

         if( j == 0 )
         {
            /* NOTE: When STRICT is on, Harbour will put both the "*" and "+"
                     chars to the first half of the colorspec (like "W*+/B"),
                     which is quite ugly, otherwise it will put the "+" to the
                     first half and the "*" to the second (like "W+/B*"), which
                     is how it should be done. [vszakats] */
#ifdef HB_CLP_STRICT
            if( ( pColors[ iColorIndex ] & 0x80 ) != 0 )
               pszColorString[ iPos++ ] = '*';
#endif

            if( ( pColors[ iColorIndex ] & 0x08 ) != 0 )
               pszColorString[ iPos++ ] = '+';

            pszColorString[ iPos++ ] = '/';
         }
#ifndef HB_CLP_STRICT
         else
         {
            if( ( pColors[ iColorIndex ] & 0x80 ) != 0 )
               pszColorString[ iPos++ ] = '*';
         }
#endif
         nColor = ( pColors[ iColorIndex ] >> 4 ) & 7;
      }
   }

   pszColorString[ iPos ] = '\0';
}


static int  hb_gt_def_GetCursorStyle( PHB_GT pGT )
{
   return pGT->iCursorShape;
}

static void hb_gt_def_SetCursorStyle( PHB_GT pGT, int iStyle )
{
   switch( iStyle )
   {
      case SC_NONE:
      case SC_NORMAL:
      case SC_INSERT:
      case SC_SPECIAL1:
      case SC_SPECIAL2:
         pGT->iCursorShape = iStyle;
         break;
      default:
         pGT->iCursorShape = SC_NORMAL;
         break;
   }
}

static void hb_gt_def_GetScrCursor( PHB_GT pGT, int * piRow, int * piCol, int * piStyle )
{
   HB_GTSELF_GETPOS( pGT, piRow, piCol );
   if( *piRow < 0 || *piCol < 0 ||
       *piRow > HB_GTSELF_MAXROW( pGT ) || *piCol > HB_GTSELF_MAXCOL( pGT ) )
      *piStyle = SC_NONE;
   else
      *piStyle = HB_GTSELF_GETCURSORSTYLE( pGT );
}

static HB_BOOL hb_gt_def_GetBlink( PHB_GT pGT )
{
   return pGT->fBlinking;
}

static void hb_gt_def_SetBlink( PHB_GT pGT, HB_BOOL fBlink )
{
   pGT->fBlinking = fBlink;
}

static void hb_gt_def_SetSnowFlag( PHB_GT pGT, HB_BOOL fNoSnow )
{
   /*
    * NOTE: This is a compatibility function which have to be implemented
    *       in low level GT driver.
    *       If you're running on a CGA and snow is a problem speak up!
    */

   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( fNoSnow );
}

static void hb_gt_def_DispBegin( PHB_GT pGT )
{
   pGT->iDispCount++;
}

static void hb_gt_def_DispEnd( PHB_GT pGT )
{
   if( pGT->iDispCount > 0 )
      pGT->iDispCount--;
}

static int hb_gt_def_DispCount( PHB_GT pGT )
{
   return pGT->iDispCount;
}

static HB_BOOL hb_gt_def_PreExt( PHB_GT pGT )
{
   if( pGT->iExtCount == 0 )
      HB_GTSELF_REFRESH( pGT );
   pGT->iExtCount++;

   return HB_TRUE;
}

static HB_BOOL hb_gt_def_PostExt( PHB_GT pGT )
{
   if( pGT->iExtCount )
      pGT->iExtCount--;

   return HB_TRUE;
}

static HB_BOOL hb_gt_def_Suspend( PHB_GT pGT )
{
   return HB_GTSELF_PREEXT( pGT );
}

static HB_BOOL hb_gt_def_Resume( PHB_GT pGT )
{
   return HB_GTSELF_POSTEXT( pGT );
}

static void hb_gt_def_OutStd( PHB_GT pGT, const char * szStr, HB_SIZE nLen )
{
   if( nLen )
   {
      if( pGT->fStdOutCon )
         HB_GTSELF_WRITECON( pGT, szStr, nLen );
      else
      {
         HB_GTSELF_PREEXT( pGT );
         if( pGT->fDispTrans )
         {
            char * szStrBuff = hb_cdpnDup( szStr, &nLen,
                                           pGT->cdpHost, pGT->cdpTerm );
            hb_fsWriteLarge( pGT->hStdErr, szStrBuff, nLen );
            hb_xfree( szStrBuff );
         }
         else
            hb_fsWriteLarge( pGT->hStdOut, szStr, nLen );
         HB_GTSELF_POSTEXT( pGT );
      }
   }
}

static void hb_gt_def_OutErr( PHB_GT pGT, const char * szStr, HB_SIZE nLen )
{
   if( nLen )
   {
      if( pGT->fStdErrCon )
         HB_GTSELF_WRITECON( pGT, szStr, nLen );
      else
      {
         HB_GTSELF_PREEXT( pGT );
         if( pGT->fDispTrans )
         {
            char * szStrBuff = hb_cdpnDup( szStr, &nLen,
                                           pGT->cdpHost, pGT->cdpTerm );
            hb_fsWriteLarge( pGT->hStdErr, szStrBuff, nLen );
            hb_xfree( szStrBuff );
         }
         else
            hb_fsWriteLarge( pGT->hStdErr, szStr, nLen );
         HB_GTSELF_POSTEXT( pGT );
      }
   }
}

static void hb_gt_def_Tone( PHB_GT pGT, double dFrequency, double dDuration )
{
   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( dFrequency );

   /* convert Clipper (DOS) timer tick units to seconds ( x / 18.2 ) */
   hb_idleSleep( dDuration / 18.2 );
}

static void hb_gt_def_Bell( PHB_GT pGT )
{
   HB_GTSELF_TONE( pGT, 700.0, 3.0 );
}

static const char * hb_gt_def_Version( PHB_GT pGT, int iType )
{
   HB_SYMBOL_UNUSED( pGT );

   if( iType == 0 )
      return "NUL";

   return "Harbour Terminal: NULL";
}

static HB_BOOL hb_gt_def_GetChar( PHB_GT pGT, int iRow, int iCol,
                                  int * piColor, HB_BYTE * pbAttr, HB_USHORT * pusChar )
{
   long lIndex;

   if( HB_GTSELF_CHECKPOS( pGT, iRow, iCol, &lIndex ) )
   {
      *pusChar = pGT->screenBuffer[ lIndex ].c.usChar;
      *piColor = pGT->screenBuffer[ lIndex ].c.bColor;
      *pbAttr  = pGT->screenBuffer[ lIndex ].c.bAttr;
      return HB_TRUE;
   }
   return HB_FALSE;
}

static HB_BOOL hb_gt_def_PutChar( PHB_GT pGT, int iRow, int iCol,
                                  int iColor, HB_BYTE bAttr, HB_USHORT usChar )
{
   long lIndex;

   if( HB_GTSELF_CHECKPOS( pGT, iRow, iCol, &lIndex ) )
   {
      pGT->screenBuffer[ lIndex ].c.usChar = usChar;
      pGT->screenBuffer[ lIndex ].c.bColor = ( HB_BYTE ) iColor;
      pGT->screenBuffer[ lIndex ].c.bAttr  = bAttr;
      pGT->pLines[ iRow ] = HB_TRUE;
      pGT->fRefresh = HB_TRUE;
      return HB_TRUE;
   }
   return HB_FALSE;
}

static void hb_gt_def_PutText( PHB_GT pGT, int iRow, int iCol, int iColor, const char * szText, HB_SIZE nLen )
{
   while( nLen-- )
   {
      if( !HB_GTSELF_PUTCHAR( pGT, iRow, iCol, iColor, 0, *szText++ ) )
         break;
      ++iCol;
   }
}

static void hb_gt_def_Replicate( PHB_GT pGT, int iRow, int iCol, int iColor,
                                 HB_BYTE bAttr, HB_USHORT usChar, HB_SIZE nLen )
{
   if( iCol < 0 )
   {
      if( nLen < ( HB_SIZE ) -iCol )
         nLen = 0;
      else
         nLen += iCol;
      iCol = 0;
   }
   while( nLen-- )
   {
      if( !HB_GTSELF_PUTCHAR( pGT, iRow, iCol, iColor, bAttr, usChar ) )
         break;
      ++iCol;
   }
}

static void hb_gt_def_WriteAt( PHB_GT pGT, int iRow, int iCol, const char * szText, HB_SIZE nLength )
{
   int iMaxCol = HB_GTSELF_MAXCOL( pGT );

   /* Truncate the text if the cursor will end up off the right edge */
   HB_GTSELF_PUTTEXT( pGT, iRow, iCol, HB_GTSELF_GETCOLOR( pGT ), szText,
                      HB_MIN( nLength, ( HB_SIZE ) ( iMaxCol - iCol + 1 ) ) );

   /* Finally, save the new cursor position, even if off-screen */
   HB_GTSELF_SETPOS( pGT, iRow, iCol + ( int ) nLength );
}

static void hb_gt_def_Write( PHB_GT pGT, const char * szText, HB_SIZE nLength )
{
   int iRow, iCol;

   HB_GTSELF_GETPOS( pGT, &iRow, &iCol );
   HB_GTSELF_WRITEAT( pGT, iRow, iCol, szText, nLength );
}

#define WRITECON_BUFFER_SIZE 512

static void hb_gt_def_WriteCon( PHB_GT pGT, const char * szText, HB_SIZE nLength )
{
   int iLen = 0;
   HB_BOOL bDisp = HB_FALSE;
   HB_BOOL bBell = HB_FALSE;
   HB_BOOL bNewLine = HB_FALSE;
   int iRow, iCol, iMaxRow, iMaxCol;
   char szString[ WRITECON_BUFFER_SIZE ];

   iMaxRow = HB_GTSELF_MAXROW( pGT );
   iMaxCol = HB_GTSELF_MAXCOL( pGT );

   HB_GTSELF_GETPOS( pGT, &iRow, &iCol );

   /* Limit the starting cursor position to maxrow(),maxcol()
      on the high end, but don't limit it on the low end. */

   if( iRow > iMaxRow || iCol > iMaxCol )
   {
      if( iRow > iMaxRow )
         iRow = iMaxRow;
      if( iCol > iMaxCol )
         iCol = iMaxCol;
      HB_GTSELF_SETPOS( pGT, iRow, iCol );
   }

   while( nLength-- )
   {
      char ch = *szText++;

      switch( ch )
      {
         case HB_CHAR_BEL:
            bDisp = bBell = HB_TRUE;
            break;

         case HB_CHAR_BS:
            if( iCol > 0 )
            {
               --iCol;
               bDisp = HB_TRUE;
            }
            else if( iCol == 0 && iRow > 0 )
            {
               iCol = iMaxCol;
               --iRow;
               bDisp = HB_TRUE;
            }
            if( bDisp )
            {
               if( iLen )
                  szString[ iLen - 1 ] = ' ';
               else
               {
                  HB_GTSELF_SETPOS( pGT, iRow, iCol );
                  szString[ iLen++ ] = ' ';
               }
            }
            break;

         case HB_CHAR_LF:
            iCol = 0;
            if( iRow >= 0 ) ++iRow;
            bDisp = HB_TRUE;
            bNewLine = HB_TRUE;
            break;

         case HB_CHAR_CR:
            iCol = 0;
            if( *szText == HB_CHAR_LF )
            {
               if( iRow >= 0 ) ++iRow;
               bNewLine = HB_TRUE;
               ++szText;
               --nLength;
            }
            bDisp = HB_TRUE;
            break;

         default:
            ++iCol;
            if( iCol > iMaxCol || iCol <= 0 )
            {
               /* If the cursor position started off the left edge,
                  don't display the first character of the string */
               if( iCol > 0 ) szString[ iLen++ ] = ch;
               /* Always advance to the first column of the next row
                  when the right edge is reached or when the cursor
                  started off the left edge, unless the cursor is off
                  the top edge, in which case only change the column */
               iCol = 0;
               if( iRow >= 0 ) ++iRow;
               bDisp = HB_TRUE;
               bNewLine = HB_TRUE;
            }
            else
               szString[ iLen++ ] = ch;

            /* Special handling for a really wide screen or device */
            if( iLen >= WRITECON_BUFFER_SIZE ) bDisp = HB_TRUE;
      }

      if( bDisp || nLength == 0 )
      {
         if( iLen )
            HB_GTSELF_WRITE( pGT, szString, iLen );

         iLen = 0;
         if( iRow > iMaxRow )
         {
            /* Normal scroll */
            HB_GTSELF_SCROLL( pGT, 0, 0, iMaxRow, iMaxCol, HB_GTSELF_GETCOLOR( pGT ),
                              HB_GTSELF_GETCLEARCHAR( pGT ), iRow - iMaxRow, 0 );
            iRow = iMaxRow;
            iCol = 0;
         }
         else if( iRow < 0 && bNewLine )
         {
            /* Special case scroll when newline
               and cursor off top edge of display */
            HB_GTSELF_SCROLL( pGT, 0, 0, iMaxRow, iMaxCol, HB_GTSELF_GETCOLOR( pGT ),
                              HB_GTSELF_GETCLEARCHAR( pGT ), 1, 0 );
         }
         HB_GTSELF_SETPOS( pGT, iRow, iCol );
         bDisp = HB_FALSE;
         bNewLine = HB_FALSE;

         /* To emulate scrolling */
         HB_GTSELF_FLUSH( pGT );

         if( bBell )
         {
            HB_GTSELF_BELL( pGT );
            bBell = HB_FALSE;
         }
      }
   }
}

static long hb_gt_def_RectSize( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight )
{
   int iRows, iCols;

   iRows = iBottom - iTop + 1;
   iCols = iRight - iLeft + 1;

   if( iCols <= 0 || iRows <= 0 )
      return 0;
   else
      return ( ( long ) iRows * iCols ) << ( pGT->fVgaCell ? 1 : 2 );
}

static void hb_gt_def_Save( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                            void * pBuffer )
{
   HB_BYTE * pbyBuffer = ( HB_BYTE * ) pBuffer;

   while( iTop <= iBottom )
   {
      int iColor;
      HB_BYTE bAttr;
      HB_USHORT usChar;
      int iCol;

      for( iCol = iLeft; iCol <= iRight; ++iCol )
      {
         if( !HB_GTSELF_GETCHAR( pGT, iTop, iCol, &iColor, &bAttr, &usChar ) )
         {
            usChar = HB_GTSELF_GETCLEARCHAR( pGT );
            iColor = HB_GTSELF_GETCLEARCOLOR( pGT );
            bAttr  = 0x00;
         }
         if( pGT->fVgaCell )
         {
            *pbyBuffer++ = ( HB_BYTE ) usChar;
            *pbyBuffer++ = ( HB_BYTE ) iColor;
         }
         else
         {
            HB_PUT_LE_UINT16( pbyBuffer, usChar );
            pbyBuffer += 2;
            *pbyBuffer++ = ( HB_BYTE ) iColor;
            *pbyBuffer++ = bAttr;
         }
      }
      ++iTop;
   }
}

static void hb_gt_def_Rest( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                            const void * pBuffer )
{
   const HB_BYTE * pbyBuffer = ( const HB_BYTE * ) pBuffer;

   while( iTop <= iBottom )
   {
      int iColor;
      HB_BYTE bAttr;
      HB_USHORT usChar;
      int iCol;

      for( iCol = iLeft; iCol <= iRight; ++iCol )
      {
         if( pGT->fVgaCell )
         {
            usChar = *pbyBuffer++;
            iColor = *pbyBuffer++;
            bAttr  = 0;
         }
         else
         {
            usChar = HB_GET_LE_UINT16( pbyBuffer );
            pbyBuffer += 2;
            iColor = *pbyBuffer++;
            bAttr  = *pbyBuffer++;
         }
         HB_GTSELF_PUTCHAR( pGT, iTop, iCol, iColor, bAttr, usChar );
      }
      ++iTop;
   }
}

static void hb_gt_def_SetAttribute( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                                    int iColor )
{
   while( iTop <= iBottom )
   {
      int iColorOld;
      HB_BYTE bAttr;
      HB_USHORT usChar;
      int iCol;

      for( iCol = iLeft; iCol <= iRight; ++iCol )
      {
         if( !HB_GTSELF_GETCHAR( pGT, iTop, iCol, &iColorOld, &bAttr, &usChar ) )
            break;
         if( !HB_GTSELF_PUTCHAR( pGT, iTop, iCol, iColor, bAttr, usChar ) )
            break;
      }
      ++iTop;
   }
}

static void hb_gt_def_DrawShadow( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                                  int iColor )
{
   int iMaxRow, iMaxCol, i;

   if( iTop > iBottom )
   {
      i = iTop;
      iTop = iBottom;
      iBottom = i;
   }
   if( iLeft > iRight )
   {
      i = iLeft;
      iLeft = iRight;
      iRight = i;
   }

   iLeft += 2;
   ++iBottom;

   iMaxRow = HB_GTSELF_MAXROW( pGT );
   iMaxCol = HB_GTSELF_MAXCOL( pGT );

   /* Draw the bottom edge */
   if( iBottom <= iMaxRow && iLeft <= iMaxCol )
      HB_GTSELF_SETATTRIBUTE( pGT, iBottom, iLeft, iBottom, HB_MIN( iRight, iMaxCol ), iColor );

   ++iRight;
   ++iTop;

   /* Draw the right edge */
   if( iTop <= iMaxRow && iRight <= iMaxCol )
      HB_GTSELF_SETATTRIBUTE( pGT, iTop, iRight, iBottom, HB_MIN( iRight + 1, iMaxCol ), iColor );
}

static void hb_gt_def_Scroll( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                              int iColor, HB_USHORT usChar, int iRows, int iCols )
{
   int iColOld, iColNew, iColSize, iColClear, iClrs, iLength;

   iColSize = iRight - iLeft;
   iLength = iColSize + 1;
   iColOld = iColNew = iLeft;
   if( iCols >= 0 )
   {
      iColOld += iCols;
      iColSize -= iCols;
      iColClear = iColNew + iColSize + 1;
      iClrs = iCols;
   }
   else
   {
      iColNew -= iCols;
      iColSize += iCols;
      iColClear = iColOld;
      iClrs = -iCols;
   }

   if( iLength > 0 && iTop <= iBottom )
   {
      void * pBuffer = NULL;

      if( ( iRows || iCols ) && iColSize >= 0 && ( iBottom - iTop >= iRows ) )
      {
         HB_SIZE nSize = HB_GTSELF_RECTSIZE( pGT, iTop, iColOld, iTop, iColOld + iColSize );

         if( nSize )
            pBuffer = hb_xgrab( nSize );
      }

      while( iTop <= iBottom )
      {
         int iRowPos;

         if( iRows >= 0 )
            iRowPos = iTop++;
         else
            iRowPos = iBottom--;

         if( pBuffer && ( iRows == 0 ||
             ( iRowPos + iRows >= iTop && iRowPos + iRows <= iBottom ) ) )
         {
            HB_GTSELF_SAVE( pGT, iRowPos + iRows, iColOld, iRowPos + iRows, iColOld + iColSize, pBuffer );
            HB_GTSELF_REST( pGT, iRowPos, iColNew, iRowPos, iColNew + iColSize, pBuffer );
            if( iClrs )
               HB_GTSELF_REPLICATE( pGT, iRowPos, iColClear, iColor, 0, usChar, iClrs );
         }
         else
            HB_GTSELF_REPLICATE( pGT, iRowPos, iLeft, iColor, 0, usChar, iLength );
      }

      if( pBuffer )
         hb_xfree( pBuffer );
   }
}

static void hb_gt_def_ScrollArea( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                                  int iColor, HB_USHORT usChar, int iRows, int iCols )
{
   if( iRows || iCols )
   {
      int iColNew, iColSize, iColClear, iClrs, iLength, iHeight, iWidth;

      HB_GTSELF_GETSIZE( pGT, &iHeight, &iWidth );
      if( iTop < 0 )
         iTop = 0;
      if( iLeft < 0 )
         iLeft = 0;
      if( iBottom >= iHeight )
         iBottom = iHeight -1;
      if( iRight >= iWidth )
         iRight = iWidth -1;

      iColSize = iRight - iLeft;
      iLength = iColSize + 1;
      iColNew = iLeft;

      if( iCols >= 0 )
      {
         iColSize -= iCols;
         iColClear = iColNew + iColSize + 1;
         iClrs = iCols;
      }
      else
      {
         iColClear = iColNew;
         iColNew -= iCols;
         iColSize += iCols;
         iClrs = -iCols;
      }

      if( iLength > 0 )
      {
         long lIndex, lOffset = ( long ) iRows * iWidth + iCols;
         HB_BOOL fMove = ( iRows || iCols ) && iColSize >= 0 &&
                         ( iBottom - iTop >= iRows );

         while( iTop <= iBottom )
         {
            int iRowPos, i;

            if( iRows >= 0 )
               iRowPos = iTop++;
            else
               iRowPos = iBottom--;

            if( fMove && ( iRows == 0 ||
                ( iRowPos + iRows >= iTop && iRowPos + iRows <= iBottom ) ) )
            {
               lIndex = ( long ) iRowPos * iWidth + iColNew;
               if( lOffset < 0 )
               {
                  for( i = 0; i <= iColSize; ++i, ++lIndex )
                  {
                     pGT->screenBuffer[ lIndex ].uiValue =
                        pGT->screenBuffer[ lIndex + lOffset ].uiValue;
                     pGT->prevBuffer[ lIndex ].uiValue =
                        pGT->prevBuffer[ lIndex + lOffset ].uiValue;
                  }
               }
               else
               {
                  for( i = iColSize, lIndex += iColSize; i >= 0; --i, --lIndex )
                  {
                     pGT->screenBuffer[ lIndex ].uiValue =
                        pGT->screenBuffer[ lIndex + lOffset ].uiValue;
                     pGT->prevBuffer[ lIndex ].uiValue =
                        pGT->prevBuffer[ lIndex + lOffset ].uiValue;
                  }
               }
               if( iClrs )
                  HB_GTSELF_REPLICATE( pGT, iRowPos, iColClear, iColor, 0, usChar, iClrs );
            }
            else
               HB_GTSELF_REPLICATE( pGT, iRowPos, iLeft, iColor, 0, usChar, iLength );
         }
      }
   }
}

static void hb_gt_def_ScrollUp( PHB_GT pGT, int iRows, int iColor, HB_USHORT usChar )
{
   if( iRows > 0 )
   {
      int i, j, iHeight, iWidth;
      long lIndex = 0, lOffset;
      HB_BYTE bAttr = 0;

      HB_GTSELF_GETSIZE( pGT, &iHeight, &iWidth );
      lOffset = ( long ) iRows * iWidth;
      for( i = iRows; i < iHeight; ++i )
      {
         pGT->pLines[ i - iRows ] = pGT->pLines[ i ];
         for( j = 0; j < iWidth; ++j )
         {
            pGT->screenBuffer[ lIndex ].uiValue =
               pGT->screenBuffer[ lIndex + lOffset ].uiValue;
            pGT->prevBuffer[ lIndex ].uiValue =
               pGT->prevBuffer[ lIndex + lOffset ].uiValue;
            ++lIndex;
         }
      }
      for( i = HB_MAX( 0, iHeight - iRows ); i < iHeight; ++i )
      {
         for( j = 0; j < iWidth; ++j )
         {
            pGT->screenBuffer[ lIndex ].c.usChar = usChar;
            pGT->screenBuffer[ lIndex ].c.bColor = ( HB_BYTE ) iColor;
            pGT->screenBuffer[ lIndex ].c.bAttr  = bAttr;
            ++lIndex;
         }
         pGT->pLines[ i ] = HB_TRUE;
      }
      pGT->fRefresh = HB_TRUE;
   }
}

static void hb_gt_def_Box( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                           const char * szFrame, int iColor )
{
   int iMaxRow, iMaxCol, iRows, iCols, iFirst, i;

   if( iTop > iBottom )
   {
      i = iTop;
      iTop = iBottom;
      iBottom = i;
   }
   if( iLeft > iRight )
   {
      i = iLeft;
      iLeft = iRight;
      iRight = i;
   }
   iMaxRow = HB_GTSELF_MAXROW( pGT );
   iMaxCol = HB_GTSELF_MAXCOL( pGT );

   if( iTop <= iMaxRow && iLeft <= iMaxCol && iBottom >= 0 && iRight >= 0 )
   {
      char szBox[ 10 ];
      char cPadCh = ( char ) HB_GTSELF_GETCLEARCHAR( pGT );

      if( szFrame )
      {
         for( i = 0; *szFrame && i < 9; ++i )
            cPadCh = szBox[ i ] = *szFrame++;
      }
      else
         i = 0;

      while( i < 8 )
         szBox[ i++ ] = cPadCh;
      szBox[ i ] = '\0';

      if( iTop == iBottom )
         HB_GTSELF_HORIZLINE( pGT, iTop, iLeft, iRight, szBox[ 1 ], iColor );
      else if( iLeft == iRight )
         HB_GTSELF_VERTLINE( pGT, iLeft, iTop, iBottom, szBox[ 3 ], iColor );
      else
      {
         HB_BYTE bAttr = HB_GT_ATTR_BOX;
         iRows = ( iBottom > iMaxRow ? iMaxRow + 1 : iBottom ) -
                 ( iTop < 0 ? -1 : iTop ) - 1;
         iCols = ( iRight > iMaxCol ? iMaxCol + 1 : iRight ) -
                 ( iLeft < 0 ? -1 : iLeft ) - 1;
         iFirst = iLeft < 0 ? 0 : iLeft + 1;

         if( iTop >= 0 )
         {
            if( iLeft >= 0 )
               HB_GTSELF_PUTCHAR( pGT, iTop, iLeft, iColor, bAttr, szBox[ 0 ] );
            if( iCols )
               HB_GTSELF_REPLICATE( pGT, iTop, iFirst, iColor, bAttr, szBox[ 1 ], iCols );
            if( iRight <= iMaxCol )
               HB_GTSELF_PUTCHAR( pGT, iTop, iFirst + iCols, iColor, bAttr, szBox[ 2 ] );
            iTop++;
         }
         else
            iTop = 0;
         for( i = 0; i < iRows; ++i )
         {
            if( iLeft >= 0 )
               HB_GTSELF_PUTCHAR( pGT, iTop + i, iLeft, iColor, bAttr, szBox[ 7 ] );
            if( iCols && szBox[ 8 ] )
               HB_GTSELF_REPLICATE( pGT, iTop + i, iFirst, iColor, bAttr, szBox[ 8 ], iCols );
            if( iRight <= iMaxCol )
               HB_GTSELF_PUTCHAR( pGT, iTop + i, iFirst + iCols, iColor, bAttr, szBox[ 3 ] );
         }
         if( iBottom <= iMaxRow )
         {
            if( iLeft >= 0 )
               HB_GTSELF_PUTCHAR( pGT, iBottom, iLeft, iColor, bAttr, szBox[ 6 ] );
            if( iCols )
               HB_GTSELF_REPLICATE( pGT, iBottom, iFirst, iColor, bAttr, szBox[ 5 ], iCols );
            if( iRight <= iMaxCol )
               HB_GTSELF_PUTCHAR( pGT, iBottom, iFirst + iCols, iColor, bAttr, szBox[ 4 ] );
         }
      }
   }
}

static void hb_gt_def_BoxS( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                            const char * szFrame, int iColor )
{
   HB_GTSELF_BOX( pGT, iTop, iLeft, iBottom, iRight,
                  szFrame ? szFrame : _B_SINGLE, iColor );
}

static void hb_gt_def_BoxD( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight,
                            const char * szFrame, int iColor )
{
   HB_GTSELF_BOX( pGT, iTop, iLeft, iBottom, iRight,
                  szFrame ? szFrame : _B_DOUBLE, iColor );
}

static void hb_gt_def_HorizLine( PHB_GT pGT, int iRow, int iLeft, int iRight,
                                 HB_USHORT usChar, int iColor )
{
   int iLength, iCol;

   if( iLeft <= iRight )
   {
      iLength = iRight - iLeft + 1;
      iCol = iLeft;
   }
   else
   {
      iLength = iLeft - iRight + 1;
      iCol = iRight;
   }

   HB_GTSELF_REPLICATE( pGT, iRow, iCol, iColor, HB_GT_ATTR_BOX, usChar, iLength );
}

static void hb_gt_def_VertLine( PHB_GT pGT, int iCol, int iTop, int iBottom,
                                HB_USHORT usChar, int iColor )
{
   int iLength, iRow;

   if( iTop <= iBottom )
   {
      iLength = iBottom - iTop + 1;
      iRow = iTop;
   }
   else
   {
      iLength = iTop - iBottom + 1;
      iRow = iBottom;
   }

   if( iRow < 0 )
   {
      iLength += iRow;
      iRow = 0;
   }

   while( --iLength >= 0 )
   {
      if( !HB_GTSELF_PUTCHAR( pGT, iRow, iCol, iColor, HB_GT_ATTR_BOX, usChar ) )
         break;
      ++iRow;
   }
}

static HB_BOOL hb_gt_def_SetDispCP( PHB_GT pGT, const char * pszTermCDP, const char * pszHostCDP, HB_BOOL fBox )
{
   if( !pszHostCDP )
      pszHostCDP = hb_cdpID();
   if( !pszTermCDP )
      pszTermCDP = pszHostCDP;

   if( pszTermCDP && pszHostCDP )
   {
      pGT->cdpTerm = hb_cdpFindExt( pszTermCDP );
      pGT->cdpHost = hb_cdpFindExt( pszHostCDP );
      pGT->fDispTrans = pGT->cdpTerm && pGT->cdpHost &&
                        pGT->cdpTerm != pGT->cdpHost;
      return HB_TRUE;
   }
   HB_SYMBOL_UNUSED( fBox );

   return HB_FALSE;
}

static HB_BOOL hb_gt_def_SetKeyCP( PHB_GT pGT, const char * pszTermCDP, const char * pszHostCDP )
{
   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( pszTermCDP );
   HB_SYMBOL_UNUSED( pszHostCDP );

   return HB_FALSE;
}

static void hb_gt_def_SetBlock( PHB_ITEM * pItemPtr, PHB_GT_INFO pInfo )
{
   if( *pItemPtr )
   {
      if( pInfo->pResult )
         hb_itemCopy( pInfo->pResult, *pItemPtr );
      else
         pInfo->pResult = hb_itemNew( *pItemPtr );
   }
   if( pInfo->pNewVal )
   {
      if( *pItemPtr )
      {
         hb_itemRelease( *pItemPtr );
         *pItemPtr = NULL;
      }
      if( hb_itemType( pInfo->pNewVal ) & HB_IT_BLOCK )
      {
         *pItemPtr = hb_itemNew( pInfo->pNewVal );
         hb_gcUnlock( *pItemPtr );
      }
   }
}

static HB_BOOL hb_gt_def_Info( PHB_GT pGT, int iType, PHB_GT_INFO pInfo )
{
   switch( iType )
   {
      case HB_GTI_ALTENTER:
      case HB_GTI_ISFULLSCREEN:
      case HB_GTI_ISGRAPHIC:
      case HB_GTI_ISSCREENPOS:
      case HB_GTI_KBDSUPPORT:
      case HB_GTI_ISCTWIN:
      case HB_GTI_ISMULTIWIN:
      case HB_GTI_ISUNICODE:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, HB_FALSE );
         break;

      case HB_GTI_KBDSHIFTS:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, 0 );
         break;

      case HB_GTI_INPUTFD:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, ( HB_NHANDLE ) pGT->hStdIn );
         break;

      case HB_GTI_OUTPUTFD:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, ( HB_NHANDLE ) pGT->hStdOut );
         break;

      case HB_GTI_ERRORFD:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, ( HB_NHANDLE ) pGT->hStdErr );
         break;

      case HB_GTI_COMPATBUFFER:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, pGT->fVgaCell );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_LOGICAL )
            pGT->fVgaCell = hb_itemGetL( pInfo->pNewVal );
         break;

      case HB_GTI_VIEWMAXWIDTH:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, HB_GTSELF_MAXCOL( pGT ) );
         break;

      case HB_GTI_VIEWMAXHEIGHT:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, HB_GTSELF_MAXROW( pGT ) );
         break;

      case HB_GTI_NEWWIN:  /* clear screen area, set default cursor shape and position */
      {
         /* Clear screen */
         HB_GTSELF_DISPBEGIN( pGT );
         HB_GTSELF_SCROLL( pGT, 0, 0, HB_GTSELF_MAXROW( pGT ), HB_GTSELF_MAXCOL( pGT ),
                           HB_GTSELF_GETCOLOR( pGT ), HB_GTSELF_GETCLEARCHAR( pGT ), 0, 0 );
         HB_GTSELF_SETPOS( pGT, 0, 0 );
         HB_GTSELF_SETCURSORSTYLE( pGT, SC_NORMAL );
         HB_GTSELF_DISPEND( pGT );
         HB_GTSELF_FLUSH( pGT );
         /* no break; */
      }
      case HB_GTI_GETWIN:  /* save screen buffer, cursor shape and position */
      {
         int iRow, iCol;
         HB_SIZE nSize;

         if( !pInfo->pResult )
            pInfo->pResult = hb_itemNew( NULL );
         hb_arrayNew( pInfo->pResult, 8 );
         HB_GTSELF_GETPOS( pGT, &iRow, &iCol );
         hb_arraySetNI( pInfo->pResult, 1, iRow );
         hb_arraySetNI( pInfo->pResult, 2, iCol );
         hb_arraySetNI( pInfo->pResult, 3, HB_GTSELF_GETCURSORSTYLE( pGT ) );
         hb_arraySetC ( pInfo->pResult, 4, hb_conSetColor( NULL ) );

         iRow = HB_GTSELF_MAXROW( pGT );
         iCol = HB_GTSELF_MAXCOL( pGT );
         hb_arraySetNI( pInfo->pResult, 5, iRow );
         hb_arraySetNI( pInfo->pResult, 6, iCol );

         nSize = HB_GTSELF_RECTSIZE( pGT, 0, 0, iRow, iCol );
         if( nSize )
         {
            void * pBuffer = hb_xgrab( nSize + 1 );
            HB_GTSELF_SAVE( pGT, 0, 0, iRow, iCol, pBuffer );
            hb_arraySetCLPtr( pInfo->pResult, 7, ( char * ) pBuffer, nSize );
         }
         break;
      }
      case HB_GTI_SETWIN:  /* restore screen buffer, cursor shape and possition */
         if( ( hb_itemType( pInfo->pNewVal ) & HB_IT_ARRAY ) &&
             hb_arrayLen( pInfo->pNewVal ) == 8 )
         {
            HB_GTSELF_DISPBEGIN( pGT );
            if( hb_arrayGetCLen( pInfo->pNewVal, 7 ) > 0 )
            {
               HB_GTSELF_REST( pGT, 0, 0, hb_arrayGetNI( pInfo->pNewVal, 5 ),
                               hb_arrayGetNI( pInfo->pNewVal, 6 ),
                               hb_arrayGetCPtr( pInfo->pNewVal, 7 ) );
            }
            HB_GTSELF_SETPOS( pGT, hb_arrayGetNI( pInfo->pNewVal, 1 ),
                                   hb_arrayGetNI( pInfo->pNewVal, 2 ) );
            HB_GTSELF_SETCURSORSTYLE( pGT, hb_arrayGetNI( pInfo->pNewVal, 3 ) );
            hb_conSetColor( hb_arrayGetCPtr( pInfo->pNewVal, 4 ) );
            HB_GTSELF_DISPEND( pGT );
            HB_GTSELF_FLUSH( pGT );
         }
         break;

      case HB_GTI_CLIPBOARDDATA:
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
         {
            /* set new Clipboard value */
            hb_gt_setClipboard( hb_itemGetCPtr( pInfo->pNewVal ),
                                hb_itemGetCLen( pInfo->pNewVal ) );
         }
         else
         {
            /* get Clipboard value */
            char * pszClipData;
            HB_SIZE nLen;

            if( hb_gt_getClipboard( &pszClipData, &nLen ) )
               pInfo->pResult = hb_itemPutCLPtr( pInfo->pResult, pszClipData, nLen );
            else
               pInfo->pResult = hb_itemPutC( pInfo->pResult, NULL );
         }
         break;

      case HB_GTI_CLIPBOARDPASTE:
         if( HB_GTSELF_INFO( pGT, HB_GTI_CLIPBOARDDATA, pInfo ) )
            HB_GTSELF_INKEYSETTEXT( pGT, hb_itemGetCPtr( pInfo->pResult ),
                                         hb_itemGetCLen( pInfo->pResult ) );
         break;

      case HB_GTI_NOTIFIERBLOCK:
         hb_gt_def_SetBlock( &pGT->pNotifierBlock, pInfo );
         break;

      case HB_GTI_INKEYFILTER:
         hb_gt_def_SetBlock( &pGT->pInkeyFilterBlock, pInfo );
         break;

      case HB_GTI_INKEYREAD:
         hb_gt_def_SetBlock( &pGT->pInkeyReadBlock, pInfo );
         break;

      case HB_GTI_CARGO:
         if( pGT->pCargo )
         {
            if( pInfo->pResult )
               hb_itemCopy( pInfo->pResult, pGT->pCargo );
            else
               pInfo->pResult = hb_itemNew( pGT->pCargo );
         }
         if( pInfo->pNewVal )
         {
            if( pGT->pCargo )
            {
               hb_itemRelease( pGT->pCargo );
               pGT->pCargo = NULL;
            }
            pGT->pCargo = hb_itemNew( pInfo->pNewVal );
            hb_gcUnlock( pGT->pCargo );
         }
         break;

      case HB_GTI_RESIZEMODE:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, HB_GTI_RESIZEMODE_FONT );
         break;

      case HB_GTI_VERSION:
         pInfo->pResult = hb_itemPutC( pInfo->pResult,
                  HB_GTSELF_VERSION( pGT, hb_itemGetNI( pInfo->pNewVal ) ) );

      default:
         return HB_FALSE;
   }

   return HB_TRUE;
}

static int hb_gt_def_Alert( PHB_GT pGT, PHB_ITEM pMessage, PHB_ITEM pOptions,
                            int iClrNorm, int iClrHigh, double dDelay )
{
   int iRet = 0, iOptions;

   if( pMessage && HB_IS_STRING( pMessage ) &&
       pOptions && ( iOptions = ( int ) hb_arrayLen( pOptions ) ) > 0 )
   {
      const char * szMessage = hb_itemGetCPtr( pMessage );
      HB_UINT nLen = ( HB_UINT ) hb_itemGetCLen( pMessage );
      HB_BOOL fScreen = HB_FALSE, fKeyBoard = HB_FALSE;
      int iKey = 0, i, iDspCount, iStyle, iRows, iCols,
          iRow, iCol, iTop, iLeft, iBottom, iRight, iMnuCol, iPos, iClr;
      void * pBuffer = NULL;
      HB_GT_INFO gtInfo;

      memset( &gtInfo, 0, sizeof( gtInfo ) );

      HB_GTSELF_INFO( pGT, HB_GTI_ISSCREENPOS, &gtInfo );
      if( gtInfo.pResult )
      {
         fScreen = hb_itemGetL( gtInfo.pResult );
      }
      HB_GTSELF_INFO( pGT, HB_GTI_KBDSUPPORT, &gtInfo );
      if( gtInfo.pResult )
      {
         fKeyBoard = hb_itemGetL( gtInfo.pResult );
         hb_itemRelease( gtInfo.pResult );
      }
      HB_GTSELF_GETSIZE( pGT, &iRows, &iCols );
      if( iCols <= 4 || iRows <= 4 )
         fScreen = HB_FALSE;

      if( fScreen )
      {
         HB_UINT ulLines = 0, ulWidth = 0, ulCurrWidth = 0, ul = 0, ulDst = 0,
               ulLast = 0, ulSpace1 = 0, ulSpace2 = 0, ulDefWidth, ulMaxWidth;
         char * szMsgDsp;

         ulMaxWidth = iCols - 4;
         ulDefWidth = ( ulMaxWidth * 3 ) >> 2;
         if( ulDefWidth == 0 )
            ulDefWidth = 1;
         szMsgDsp = ( char * ) hb_xgrab( nLen + ( nLen / ulDefWidth ) + 1 );

         while( ul < nLen )
         {
            if( szMessage[ ul ] == '\n' )
            {
               if( ulCurrWidth > ulMaxWidth )
               {
                  ulDst = ulLast;
               }
               else
               {
                  ++ulLines;
                  if( ulCurrWidth > ulWidth )
                     ulWidth = ulCurrWidth;
                  ulCurrWidth = ulSpace1 = ulSpace2 = 0;
                  szMsgDsp[ ulDst++ ] = '\n';
                  ulLast = ulDst;
               }
            }
            else
            {
               if( szMessage[ ul ] == ' ' )
               {
                  if( ulCurrWidth <= ulDefWidth )
                     ulSpace1 = ul;
                  else if( ulCurrWidth <= ulMaxWidth && !ulSpace2 )
                     ulSpace2 = ul;
               }
               szMsgDsp[ ulDst++ ] = szMessage[ ul ];
               ++ulCurrWidth;
               if( ulCurrWidth > ulDefWidth && ulSpace1 )
               {
                  ulCurrWidth -= ul - ulSpace1 + 1;
                  ulDst -= ul - ulSpace1 + 1;
                  ul = ulSpace1;
                  ++ulLines;
                  if( ulCurrWidth > ulWidth )
                     ulWidth = ulCurrWidth;
                  ulCurrWidth = ulSpace1 = ulSpace2 = 0;
                  szMsgDsp[ ulDst++ ] = '\n';
                  ulLast = ulDst;
               }
               else if( ulCurrWidth > ulMaxWidth )
               {
                  if( ulSpace2 )
                  {
                     ulCurrWidth -= ul - ulSpace2 + 1;
                     ulDst -= ul - ulSpace2 + 1;
                     ul = ulSpace2;
                     ++ulLines;
                     if( ulCurrWidth > ulWidth )
                        ulWidth = ulCurrWidth;
                     ulCurrWidth = ulSpace1 = ulSpace2 = 0;
                     szMsgDsp[ ulDst++ ] = '\n';
                     ulLast = ulDst;
                  }
#ifndef HB_CLP_STRICT
                  else
                  {
                     ulCurrWidth--;
                     ulDst--;
                     ul--;
                     szMsgDsp[ ulDst++ ] = '\n';
                     ulLast = ulDst;
                     ++ulLines;
                     if( ulCurrWidth > ulWidth )
                        ulWidth = ulCurrWidth;
                     ulCurrWidth = ulSpace1 = ulSpace2 = 0;
                  }
#endif
               }
            }
            ++ul;
         }
         ulLines++;
         if( ulCurrWidth > ulWidth )
            ulWidth = ulCurrWidth;
         if( ulLines == 1 && ulWidth < ulDefWidth )
            ulWidth += HB_MIN( 4, ulDefWidth - ulWidth );

         ulCurrWidth = 0;
         for( i = 1; i <= iOptions; ++i )
         {
            ulCurrWidth += ( HB_UINT ) hb_arrayGetCLen( pOptions, i ) + 4;
         }
         if( ulCurrWidth > ulMaxWidth )
            ulCurrWidth = ulMaxWidth;
         if( ulCurrWidth > ulWidth )
            ulWidth = ulCurrWidth;
         if( ( HB_SIZE ) iRows < ulLines + 4 )
            ulLines = iRows - 4;
         iTop = ( iRows - ulLines - 4 ) >> 1;
         iLeft = ( iCols - ulWidth - 4 ) >> 1;
         iBottom = iTop + ulLines + 3;
         iRight = iLeft + ulWidth + 3;

         if( iClrNorm == 0 )
            iClrNorm = 0x4F;
         if( iClrHigh == 0 )
            iClrHigh = 0x1F;
         iDspCount = HB_GTSELF_DISPCOUNT( pGT );
         if( iDspCount == 0 )
            HB_GTSELF_DISPBEGIN( pGT );
         HB_GTSELF_GETPOS( pGT, &iRow, &iCol );
         iStyle = HB_GTSELF_GETCURSORSTYLE( pGT );
         HB_GTSELF_SETCURSORSTYLE( pGT, SC_NONE );
         nLen = HB_GTSELF_RECTSIZE( pGT, iTop, iLeft, iBottom, iRight );
         if( nLen )
         {
            pBuffer = hb_xgrab( nLen );
            HB_GTSELF_SAVE( pGT, iTop, iLeft, iBottom, iRight, pBuffer );
         }
         HB_GTSELF_BOXS( pGT, iTop, iLeft, iBottom, iRight, NULL, iClrNorm );
         HB_GTSELF_BOXS( pGT, iTop + 1, iLeft + 1, iBottom - 1, iRight - 1, "         ", iClrNorm );
         ulLast = 0;
         i = iTop + 1;
         for( ul = 0; ul < ulDst; ++ul )
         {
            if( szMsgDsp[ ul ] == '\n' )
            {
               if( ul > ulLast )
               {
                  nLen = ul - ulLast;
                  if( nLen > ulWidth )
                     nLen = ulWidth;
                  HB_GTSELF_PUTTEXT( pGT, i, iLeft + ( ( ulWidth - nLen + 1 ) >> 1 ) + 2,
                                     iClrNorm, szMsgDsp + ulLast, nLen );
               }
               ulLast = ul + 1;
               if( ++i >= iBottom - 1 )
                  break;
            }
         }
         if( ul > ulLast && i < iBottom - 1 )
         {
            nLen = ul - ulLast;
            if( nLen > ulWidth )
               nLen = ulWidth;
            HB_GTSELF_PUTTEXT( pGT, i, iLeft + ( ( ulWidth - nLen + 1 ) >> 1 ) + 2,
                               iClrNorm, szMsgDsp + ulLast, nLen );
         }

         iPos = 1;
         while( iRet == 0 )
         {
            HB_GTSELF_DISPBEGIN( pGT );
            iMnuCol = iLeft + ( ( ulWidth - ulCurrWidth ) >> 1 ) + 3;
            for( i = 1; i <= iOptions; ++i )
            {
               iClr = i == iPos ? iClrHigh : iClrNorm;
               nLen = ( HB_UINT ) hb_arrayGetCLen( pOptions, i );
               HB_GTSELF_PUTTEXT( pGT, iBottom - 1, iMnuCol, iClr, " ", 1 );
               HB_GTSELF_PUTTEXT( pGT, iBottom - 1, iMnuCol + 1, iClr,
                                  hb_arrayGetCPtr( pOptions, i ), nLen );
               HB_GTSELF_PUTTEXT( pGT, iBottom - 1, iMnuCol + 1 + nLen, iClr, " ", 1 );
               iMnuCol += nLen + 4;
            }
            while( HB_GTSELF_DISPCOUNT( pGT ) )
               HB_GTSELF_DISPEND( pGT );
            HB_GTSELF_REFRESH( pGT );

            iKey = fKeyBoard ? HB_GTSELF_INKEYGET( pGT, HB_TRUE, dDelay, INKEY_ALL ) : 0;

            if( iKey == K_ESC )
               break;
            else if( iKey == K_ENTER || iKey == K_SPACE || iKey == 0 )
            {
               iRet = iPos;
            }
            else if( iKey == K_LEFT || iKey == K_SH_TAB )
            {
               if( --iPos == 0 )
                  iPos = iOptions;
               dDelay = 0.0;
            }
            else if( iKey == K_RIGHT || iKey == K_TAB )
            {
               if( ++iPos > iOptions )
                  iPos = 1;
               dDelay = 0.0;
            }
#ifdef HB_COMPAT_C53
            else if( iKey == K_LBUTTONDOWN )
            {
               int iMRow, iMCol;
               HB_GTSELF_MOUSEGETPOS( pGT, &iMRow, &iMCol );
               if( iMRow == iBottom - 1 )
               {
                  iMnuCol = iLeft + ( ( ulWidth - ulCurrWidth ) >> 1 ) + 4;
                  for( i = 1; i <= iOptions; ++i )
                  {
                     nLen = ( HB_UINT ) hb_arrayGetCLen( pOptions, i );
                     if( iMCol >= iMnuCol && iMCol < iMnuCol + ( int ) nLen )
                     {
                        iRet = i;
                        break;
                     }
                     iMnuCol += nLen + 4;
                  }
               }
            }
#endif
            else if( iKey >= 32 && iKey <= 255 )
            {
               int iUp = hb_charUpper( iKey );
               for( i = 1; i <= iOptions; ++i )
               {
                  const char *szValue = hb_arrayGetCPtr( pOptions, i );
                  if( szValue && iUp == hb_charUpper( *szValue ) )
                  {
                     iRet = i;
                     break;
                  }
               }
            }
         }

         hb_xfree( szMsgDsp );
         if( pBuffer )
         {
            HB_GTSELF_REST( pGT, iTop, iLeft, iBottom, iRight, pBuffer );
            hb_xfree( pBuffer );
         }
         HB_GTSELF_SETPOS( pGT, iRow, iCol );
         HB_GTSELF_SETCURSORSTYLE( pGT, iStyle );
         HB_GTSELF_REFRESH( pGT );
         while( HB_GTSELF_DISPCOUNT( pGT ) < iDspCount )
            HB_GTSELF_DISPBEGIN( pGT );
      }
      else
      {
         HB_SIZE ul, nStart = 0;
         const char *szEol = hb_conNewLine();

         for( ul = 0; ul < nLen; ++ul )
         {
            if( szMessage[ ul ] == '\n' )
            {
               if( ul > nStart )
                  HB_GTSELF_WRITECON( pGT, szMessage + nStart, ul - nStart );
               HB_GTSELF_WRITECON( pGT, szEol, strlen( szEol ) );
               nStart = ul + 1;
            }
         }
         if( ul > nStart )
            HB_GTSELF_WRITECON( pGT, szMessage + nStart, ul - nStart );
         HB_GTSELF_WRITECON( pGT, " (", 2 );
         for( i = 1; i <= iOptions; ++i )
         {
            if( i > 1 )
               HB_GTSELF_WRITECON( pGT, ", ", 2 );
            HB_GTSELF_WRITECON( pGT, hb_arrayGetCPtr( pOptions, i ),
                                     hb_arrayGetCLen( pOptions, i ) );
         }
         HB_GTSELF_WRITECON( pGT, ") ", 2 );
         while( iRet == 0 )
         {
            iKey = fKeyBoard ? HB_GTSELF_INKEYGET( pGT, HB_TRUE, dDelay, INKEY_ALL ) : 0;
            if( iKey == 0 )
               iRet = 1;
            else if( iKey == K_ESC )
               break;
            else if( iKey >= 32 && iKey <= 255 )
            {
               int iUp = hb_charUpper( iKey );
               for( i = 1; i <= iOptions; ++i )
               {
                  const char *szValue = hb_arrayGetCPtr( pOptions, i );
                  if( szValue && iUp == hb_charUpper( *szValue ) )
                  {
                     iRet = i;
                     break;
                  }
               }
            }
         }
         if( iKey >= 32 && iKey <= 255 )
         {
            char szVal[2];
            szVal[ 0 ] = ( char ) iKey;
            szVal[ 1 ] = '\0';
            HB_GTSELF_WRITECON( pGT, szVal, 1 );
         }
      }
   }

   return iRet;
}

static int hb_gt_def_SetFlag( PHB_GT pGT, int iType, int iNewValue )
{
   int iPrevValue = 0;

   switch( iType )
   {
      case HB_GTI_COMPATBUFFER:
         iPrevValue = pGT->fVgaCell;
         pGT->fVgaCell = iNewValue != 0;
         break;

      case HB_GTI_STDOUTCON:
         iPrevValue = pGT->fStdOutCon;
         pGT->fStdOutCon = iNewValue != 0;
         break;

      case HB_GTI_STDERRCON:
         iPrevValue = pGT->fStdErrCon;
         pGT->fStdErrCon = iNewValue != 0;
         break;
   }

   return iPrevValue;
}

static HB_BOOL hb_gt_def_SetMode( PHB_GT pGT, int iRows, int iCols )
{
   return HB_GTSELF_RESIZE( pGT, iRows, iCols );
}

static HB_BOOL hb_gt_def_Resize( PHB_GT pGT, int iRows, int iCols )
{
   if( iRows > 0 && iCols > 0 && pGT->screenBuffer )
   {
      if( pGT->iHeight != iRows || pGT->iWidth != iCols )
      {
         void * pBuffer = NULL;
         HB_SIZE nLen = ( HB_SIZE ) iRows * iCols, nIndex;
         HB_SIZE nSize;
         int i;

         nSize = HB_GTSELF_RECTSIZE( pGT, 0, 0, iRows - 1, iCols - 1 );
         if( nSize )
         {
            pBuffer = hb_xgrab( nSize );
            HB_GTSELF_SAVE( pGT, 0, 0, iRows - 1, iCols - 1, pBuffer );
         }

         pGT->screenBuffer =
               ( PHB_SCREENCELL ) hb_xrealloc( pGT->screenBuffer,
                                             sizeof( HB_SCREENCELL ) * nLen );
         pGT->prevBuffer =
               ( PHB_SCREENCELL ) hb_xrealloc( pGT->prevBuffer,
                                             sizeof( HB_SCREENCELL ) * nLen );
         pGT->pLines =
               ( HB_BOOL * ) hb_xrealloc( pGT->pLines,
                                             sizeof( HB_BOOL ) * iRows );

         memset( pGT->screenBuffer, 0, sizeof( HB_SCREENCELL ) * nLen );
         memset( pGT->prevBuffer, 0, sizeof( HB_SCREENCELL ) * nLen );
         for( i = 0; i < iRows; ++i )
            pGT->pLines[ i ] = HB_TRUE;
         for( nIndex = 0; nIndex < nLen; ++nIndex )
         {
            pGT->screenBuffer[ nIndex ].c.usChar = HB_GTSELF_GETCLEARCHAR( pGT );
            pGT->screenBuffer[ nIndex ].c.bColor = ( HB_BYTE ) HB_GTSELF_GETCLEARCOLOR( pGT );
            pGT->screenBuffer[ nIndex ].c.bAttr  = 0x00;
            pGT->prevBuffer[ nIndex ].c.bAttr = HB_GT_ATTR_REFRESH;
         }

         pGT->iHeight = iRows;
         pGT->iWidth = iCols;

         if( pGT->iRow >= pGT->iHeight )
            pGT->iRow = pGT->iHeight - 1;
         if( pGT->iCol >= pGT->iWidth )
            pGT->iCol = pGT->iWidth - 1;

         pGT->fRefresh = HB_TRUE;

         if( nSize )
         {
            HB_GTSELF_REST( pGT, 0, 0, iRows - 1, iCols - 1, pBuffer );
            hb_xfree( pBuffer );
         }
      }

      return HB_TRUE;
   }

   return HB_FALSE;
}

static void hb_gt_def_GetSize( PHB_GT pGT, int * piRows, int  * piCols )
{
   *piRows = pGT->iHeight;
   *piCols = pGT->iWidth;
}

static void hb_gt_def_SemiCold( PHB_GT pGT )
{
   int i;
   for( i = 0; i < pGT->iHeight; ++i )
      pGT->pLines[ i ] = HB_FALSE;
   pGT->fRefresh = HB_FALSE;
}

static void hb_gt_def_ColdArea( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight )
{
   long lIndex;
   int i;

   if( iTop > iBottom )
   {
      i = iTop;
      iTop = iBottom;
      iBottom = i;
   }
   if( iLeft > iRight )
   {
      i = iLeft;
      iLeft = iRight;
      iRight = i;
   }
   while( iTop <= iBottom )
   {
      for( i = iLeft; i <= iRight; ++i )
      {
         if( HB_GTSELF_CHECKPOS( pGT, iTop, i, &lIndex ) )
         {
            pGT->screenBuffer[ lIndex ].c.bAttr &= ~HB_GT_ATTR_REFRESH;
            pGT->prevBuffer[ lIndex ].uiValue = pGT->screenBuffer[ lIndex ].uiValue;
         }
      }
      if( iLeft == 0 && iRight == pGT->iWidth - 1 )
         pGT->pLines[ iTop ] = HB_FALSE;
      ++iTop;
   }
}

static void hb_gt_def_ExposeArea( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight )
{
   long lIndex;
   int i;

   if( iTop > iBottom )
   {
      i = iTop;
      iTop = iBottom;
      iBottom = i;
   }
   if( iLeft > iRight )
   {
      i = iLeft;
      iLeft = iRight;
      iRight = i;
   }
   while( iTop <= iBottom )
   {
      for( i = iLeft; i <= iRight; ++i )
      {
         if( HB_GTSELF_CHECKPOS( pGT, iTop, i, &lIndex ) )
         {
            pGT->prevBuffer[ lIndex ].c.bAttr = HB_GT_ATTR_REFRESH;
            pGT->pLines[ iTop ] = HB_TRUE;
            pGT->fRefresh = HB_TRUE;
         }
      }
      ++iTop;
   }
}

static void hb_gt_def_TouchLine( PHB_GT pGT, int iRow )
{
   if( iRow >= 0 && iRow < pGT->iHeight )
   {
      pGT->pLines[ iRow ] = HB_TRUE;
      pGT->fRefresh = HB_TRUE;
   }
}

static void hb_gt_def_TouchCell( PHB_GT pGT, int iRow, int iCol )
{
   long lIndex;

   if( HB_GTSELF_CHECKPOS( pGT, iRow, iCol, &lIndex ) )
   {
      pGT->prevBuffer[ lIndex ].c.bAttr = HB_GT_ATTR_REFRESH;
      pGT->pLines[ iRow ] = HB_TRUE;
      pGT->fRefresh = HB_TRUE;
   }
}

static void hb_gt_def_Redraw( PHB_GT pGT, int iRow, int iCol, int iSize )
{
   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( iRow );
   HB_SYMBOL_UNUSED( iCol );
   HB_SYMBOL_UNUSED( iSize );
}

static void hb_gt_def_RedrawDiff( PHB_GT pGT )
{
   if( pGT->fRefresh )
   {
      int i, l, r;
      long lIndex;

      for( i = 0; i < pGT->iHeight; ++i )
      {
         if( pGT->pLines[ i ] )
         {
            lIndex = ( long ) i * pGT->iWidth;
            for( l = 0; l < pGT->iWidth; ++l, ++lIndex )
            {
               if( pGT->prevBuffer[ lIndex ].uiValue !=
                   pGT->screenBuffer[ lIndex ].uiValue )
                  break;
            }
            if( l < pGT->iWidth )
            {
               lIndex = ( long ) ( i + 1 ) * pGT->iWidth - 1;
               for( r = pGT->iWidth - 1; r > l; --r, --lIndex )
               {
                  if( pGT->prevBuffer[ lIndex ].uiValue !=
                      pGT->screenBuffer[ lIndex ].uiValue )
                     break;
               }
               HB_GTSELF_REDRAW( pGT, i, l, r - l + 1 );
               lIndex = ( long ) i * pGT->iWidth + l;
               do
               {
                  pGT->prevBuffer[ lIndex ].uiValue =
                     pGT->screenBuffer[ lIndex ].uiValue;
                  ++lIndex;
               }
               while( ++l <= r );
            }
            pGT->pLines[ i ] = HB_FALSE;
         }
      }
      pGT->fRefresh = HB_FALSE;
   }
}

static void hb_gt_def_Refresh( PHB_GT pGT )
{
   HB_GTSELF_REDRAWDIFF( pGT );
}

static void hb_gt_def_Flush( PHB_GT pGT )
{
   if( HB_GTSELF_DISPCOUNT( pGT ) == 0 )
      HB_GTSELF_REFRESH( pGT );
}

static int hb_gt_def_ReadKey( PHB_GT pGT, int iEventMask )
{
   return HB_GTSELF_MOUSEREADKEY( pGT, iEventMask );
}

/* helper internal function */
static int hb_gt_def_InkeyFilter( PHB_GT pGT, int iKey, int iEventMask )
{
   int iMask;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_InkeyFilter(%p,%d,%d)", pGT, iKey, iEventMask));

   HB_SYMBOL_UNUSED( pGT );

   switch( iKey )
   {
      case K_MOUSEMOVE:
      case K_MMLEFTDOWN:
      case K_MMRIGHTDOWN:
      case K_MMMIDDLEDOWN:
      case K_NCMOUSEMOVE:
         iMask = INKEY_MOVE;
         break;
      case K_LBUTTONDOWN:
      case K_LDBLCLK:
         iMask = INKEY_LDOWN;
         break;
      case K_LBUTTONUP:
         iMask = INKEY_LUP;
         break;
      case K_RBUTTONDOWN:
      case K_RDBLCLK:
         iMask = INKEY_RDOWN;
         break;
      case K_RBUTTONUP:
         iMask = INKEY_RUP;
         break;
      case K_MBUTTONDOWN:
      case K_MBUTTONUP:
      case K_MDBLCLK:
         iMask = INKEY_MMIDDLE;
         break;
      case K_MWFORWARD:
      case K_MWBACKWARD:
         iMask = INKEY_MWHEEL;
         break;
      case HB_K_RESIZE:
      case HB_K_CLOSE:
      case HB_K_GOTFOCUS:
      case HB_K_LOSTFOCUS:
      case HB_K_CONNECT:
      case HB_K_DISCONNECT:
         iMask = HB_INKEY_GTEVENT;
         break;
      default:
         iMask = INKEY_KEYBOARD;
         break;
   }

   if( ( iMask & iEventMask ) == 0 )
      return 0;

   return iKey;
}

/* helper internal function: drop the next key in keyboard buffer */
static void hb_gt_def_InkeyPop( PHB_GT pGT )
{
   if( pGT->StrBuffer )
   {
      if( ++pGT->StrBufferPos >= pGT->StrBufferSize )
      {
         hb_xfree( pGT->StrBuffer );
         pGT->StrBuffer = NULL;
      }
   }
   else if( pGT->inkeyHead != pGT->inkeyTail )
   {
      if( ++pGT->inkeyTail >= pGT->inkeyBufferSize )
         pGT->inkeyTail = 0;
   }
}

/* Put the key into keyboard buffer */
static void hb_gt_def_InkeyPut( PHB_GT pGT, int iKey )
{
   int iHead;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_InkeyPut(%p,%d)", pGT, iKey));

   iHead = pGT->inkeyHead;

   if( iKey == K_MOUSEMOVE )
   {
      /*
       * Clipper does not store in buffer repeated mouse movement
       * IMHO it's good idea to reduce unnecessary inkey buffer
       * overloading so I also implemented it, [druzus]
       */
      if( pGT->iLastPut == iKey && pGT->inkeyHead != pGT->inkeyTail )
         return;
   }

   /*
    * When the buffer is full new event overwrite the last one
    * in the buffer - it's Clipper behavior, [druzus]
    */
   pGT->inkeyBuffer[ iHead++ ] = pGT->iLastPut = iKey;
   if( iHead >= pGT->inkeyBufferSize )
      iHead = 0;

   if( iHead != pGT->inkeyTail )
      pGT->inkeyHead = iHead;
}

/* Inset the key into head of keyboard buffer */
static void hb_gt_def_InkeyIns( PHB_GT pGT, int iKey )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_InkeyIns(%p,%d)", pGT, iKey));

   if( --pGT->inkeyTail < 0 )
      pGT->inkeyTail = pGT->inkeyBufferSize -1;

   pGT->inkeyBuffer[ pGT->inkeyTail ] = iKey;

   /* When the buffer is full new event overwrite the last one
    * in the buffer. [druzus]
    */
   if( pGT->inkeyHead == pGT->inkeyTail )
   {
      if( --pGT->inkeyHead < 0 )
         pGT->inkeyHead = pGT->inkeyBufferSize -1;
   }
}

/* helper internal function */
static HB_BOOL hb_gt_def_InkeyNextCheck( PHB_GT pGT, int iEventMask, int * iKey )
{
   HB_TRACE( HB_TR_DEBUG, ("hb_gt_def_InkeyNextCheck(%p,%p)", pGT, iKey) );

   if( pGT->StrBuffer )
   {
      *iKey = pGT->StrBuffer[ pGT->StrBufferPos ];
   }
   else if( pGT->inkeyHead != pGT->inkeyTail )
   {
      *iKey = hb_gt_def_InkeyFilter( pGT, pGT->inkeyBuffer[ pGT->inkeyTail ], iEventMask );
   }
   else
   {
      return HB_FALSE;
   }

   if( *iKey == 0 )
   {
      hb_gt_def_InkeyPop( pGT );
      return HB_FALSE;
   }

   return HB_TRUE;
}

/* helper internal function */
static void hb_gt_def_InkeyPollDo( PHB_GT pGT )
{
   int iKey;

   HB_TRACE( HB_TR_DEBUG, ("hb_gt_def_InkeyPollDo(%p)", pGT) );

   iKey = HB_GTSELF_READKEY( pGT, INKEY_ALL );

   if( iKey )
   {
      switch( iKey )
      {
         case HB_BREAK_FLAG:           /* Check for Ctrl+Break */
         case K_ALT_C:                 /* Check for normal Alt+C */
            if( hb_setGetCancel() )
            {
               hb_vmRequestCancel();   /* Request cancellation */
               return;
            }
            break;
         case K_ALT_D:                 /* Check for Alt+D */
            if( hb_setGetDebug() )
            {
               hb_vmRequestDebug();    /* Request the debugger */
               return;
            }
      }
      HB_GTSELF_INKEYPUT( pGT, iKey );
   }
}

/* Poll the console keyboard to stuff the Harbour buffer */
static void hb_gt_def_InkeyPoll( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ("hb_gt_def_InkeyPoll(%p)", pGT) );

   /*
    * Clipper 5.3 always poll events without respecting
    * _SET_TYPEAHEAD when CL5.2 only when it's non zero.
    * IMHO keeping CL5.2 behavior will be more accurate for xharbour
    * because it allow to control it by user what some times could be
    * necessary due to different low level GT behavior on some platforms
    */
   if( hb_setGetTypeAhead() )
      hb_gt_def_InkeyPollDo( pGT );
}

/* Return the next key without extracting it */
static int hb_gt_def_InkeyNext( PHB_GT pGT, int iEventMask )
{
   int iKey = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_InkeyNext(%p,%d)", pGT, iEventMask));

   HB_GTSELF_INKEYPOLL( pGT );
   hb_gt_def_InkeyNextCheck( pGT, iEventMask, &iKey );

   return iKey;
}

/* Wait for keyboard input */
static int hb_gt_def_InkeyGet( PHB_GT pGT, HB_BOOL fWait, double dSeconds, int iEventMask )
{
   HB_MAXUINT end_timer;
   PHB_ITEM pKey;
   HB_BOOL fPop;
   int iKey;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_InkeyGet(%p,%d,%f,%d)", pGT, (int) fWait, dSeconds, iEventMask));

   pKey = NULL;

   if( pGT->pInkeyReadBlock )
   {
      HB_GTSELF_UNLOCK( pGT );
      iKey = hb_itemGetNI( hb_vmEvalBlock( pGT->pInkeyReadBlock ) );
      HB_GTSELF_LOCK( pGT );
      if( iKey != 0 )
         return iKey;
   }

   /* Wait forever ?, Use fixed value 100 for strict Clipper compatibility */
   if( fWait && dSeconds * 100 >= 1 )
      end_timer = hb_dateMilliSeconds() + ( HB_MAXUINT ) ( dSeconds * 1000 );
   else
      end_timer = 0;

   do
   {
      hb_gt_def_InkeyPollDo( pGT );
      fPop = hb_gt_def_InkeyNextCheck( pGT, iEventMask, &pGT->inkeyLast );

      if( fPop )
      {
         hb_gt_def_InkeyPop( pGT );
         if( !pGT->pInkeyFilterBlock )
            break;
         pKey = hb_itemPutNI( pKey, pGT->inkeyLast );
         HB_GTSELF_UNLOCK( pGT );
         pGT->inkeyLast = hb_itemGetNI( hb_vmEvalBlockV( pGT->pInkeyFilterBlock, 1, pKey ) );
         HB_GTSELF_LOCK( pGT );
         if( pGT->inkeyLast != 0 )
            break;
      }

      /* immediately break if a VM request is pending. */
      if( !fWait || hb_vmRequestQuery() != 0 )
         break;

      HB_GTSELF_UNLOCK( pGT );
      hb_idleState();
      HB_GTSELF_LOCK( pGT );
   }
   while( end_timer == 0 || end_timer > hb_dateMilliSeconds() );

   if( pKey )
      hb_itemRelease( pKey );

   hb_idleReset();

   return fPop ? pGT->inkeyLast : 0;
}

/* Return the value of the last key that was extracted */
static int hb_gt_def_InkeyLast( PHB_GT pGT, int iEventMask )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_InkeyLast(%p,%d)", pGT, iEventMask));

   HB_GTSELF_INKEYPOLL( pGT );

   return hb_gt_def_InkeyFilter( pGT, pGT->inkeyLast, iEventMask );
}

/* Set LastKey() value and return previous value */
static int hb_gt_def_InkeySetLast( PHB_GT pGT, int iKey )
{
   int iLast;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_InkeySetLast(%p,%d)", pGT, iKey));

   iLast = pGT->inkeyLast;
   pGT->inkeyLast = iKey;

   return iLast;
}

/* Set text into inkey buffer */
static void hb_gt_def_InkeySetText( PHB_GT pGT, const char * szText, HB_SIZE nLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_InkeySetText(%p,%s,%" HB_PFS "u)", pGT, szText, nLen));

   if( pGT->StrBuffer )
   {
      hb_xfree( pGT->StrBuffer );
      pGT->StrBuffer = NULL;
   }

   if( szText && nLen )
   {
      pGT->StrBuffer = ( HB_BYTE * ) hb_xgrab( nLen );
      memcpy( pGT->StrBuffer, szText, nLen );
      pGT->StrBufferSize = nLen;
      pGT->StrBufferPos = 0;
      do
      {
         if( pGT->StrBuffer[ --nLen ] == ';' )
            pGT->StrBuffer[ nLen ] = HB_CHAR_CR;
      }
      while( nLen );
   }
}

/* Reset the keyboard buffer */
static void hb_gt_def_InkeyReset( PHB_GT pGT )
{
   int iTypeAhead;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_InkeyReset(%p)", pGT));

   if( pGT->StrBuffer )
   {
      hb_xfree( pGT->StrBuffer );
      pGT->StrBuffer = NULL;
   }

   pGT->inkeyHead = 0;
   pGT->inkeyTail = 0;

   iTypeAhead = hb_setGetTypeAhead();

   if( iTypeAhead != pGT->inkeyBufferSize )
   {
      if( pGT->inkeyBufferSize > HB_DEFAULT_INKEY_BUFSIZE )
         hb_xfree( pGT->inkeyBuffer );

      if( iTypeAhead > HB_DEFAULT_INKEY_BUFSIZE )
      {
         pGT->inkeyBufferSize = iTypeAhead;
         pGT->inkeyBuffer = ( int * ) hb_xgrab( pGT->inkeyBufferSize * sizeof( int ) );
      }
      else
      {
         pGT->inkeyBufferSize = HB_DEFAULT_INKEY_BUFSIZE;
         pGT->inkeyBuffer = pGT->defaultKeyBuffer;
      }
   }
}

/* reset inkey pool to default state and free any allocated resources */
static void hb_gt_def_InkeyExit( PHB_GT pGT )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_InkeyExit(%p)", pGT));

   if( pGT->StrBuffer )
   {
      hb_xfree( pGT->StrBuffer );
      pGT->StrBuffer = NULL;
   }

   if( pGT->inkeyBufferSize > HB_DEFAULT_INKEY_BUFSIZE )
   {
      hb_xfree( pGT->inkeyBuffer );
      pGT->inkeyBufferSize = HB_DEFAULT_INKEY_BUFSIZE;
      pGT->inkeyBuffer = pGT->defaultKeyBuffer;
   }
}

static void hb_gt_def_MouseInit( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );
}

static void hb_gt_def_MouseExit( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );
}

static HB_BOOL hb_gt_def_MouseIsPresent( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );
   return HB_FALSE;
}

static void hb_gt_def_MouseShow( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );
}

static void hb_gt_def_MouseHide( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );
}

static HB_BOOL hb_gt_def_MouseGetCursor( PHB_GT pGT )
{
   return pGT->fMouseVisible;
}

static void hb_gt_def_MouseSetCursor( PHB_GT pGT, HB_BOOL fVisible )
{
   if( fVisible )
   {
      HB_GTSELF_MOUSESHOW( pGT );
      pGT->fMouseVisible = HB_TRUE;
   }
   else if( pGT->fMouseVisible )
   {
      /*
       * mouse drivers use hide counters, so repeated calls to
       * HB_GTSELF_MOUSEHIDE( pGT ) will need at least the same number of
       * calls to HB_GTSELF_MOUSESHOW() to make mouse cursor visible. This
       * behavior is not compatible with Clipper so call to
       * HB_GTSELF_MOUSEHIDE( pGT ) is guarded by pGT->fMouseVisible.
       * The counter is not updated when mouse cursor is visible and
       * HB_GTSELF_MOUSESHOW() is called so this behavior is enough.
       * If some platform works in differ way then and this behavior
       * will create problems GT driver should overload
       * HB_GTSELF_MOUSESETCURSOR()/HB_GTSELF_MOUSEGETCURSOR() methods.
       * [druzus]
       */
      HB_GTSELF_MOUSEHIDE( pGT );
      pGT->fMouseVisible = HB_FALSE;
   }
}

static int hb_gt_def_MouseRow( PHB_GT pGT )
{
   int iRow, iCol;

   HB_GTSELF_MOUSEGETPOS( pGT, &iRow, &iCol );
   return iRow;
}

static int hb_gt_def_MouseCol( PHB_GT pGT )
{
   int iRow, iCol;

   HB_GTSELF_MOUSEGETPOS( pGT, &iRow, &iCol );
   return iCol;
}

static void hb_gt_def_MouseGetPos( PHB_GT pGT, int * piRow, int * piCol )
{
   HB_SYMBOL_UNUSED( pGT );

   *piRow = *piCol = 0;
}

static void hb_gt_def_MouseSetPos( PHB_GT pGT, int iRow, int iCol )
{
   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( iRow );
   HB_SYMBOL_UNUSED( iCol );
}

static void hb_gt_def_MouseSetBounds( PHB_GT pGT, int iTop, int iLeft, int iBottom, int iRight )
{
   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( iTop );
   HB_SYMBOL_UNUSED( iLeft );
   HB_SYMBOL_UNUSED( iBottom );
   HB_SYMBOL_UNUSED( iRight );
}

static void hb_gt_def_MouseGetBounds( PHB_GT pGT, int * piTop, int * piLeft, int * piBottom, int * piRight )
{
   HB_SYMBOL_UNUSED( pGT );

   *piTop = *piLeft = 0;
   HB_GTSELF_GETSIZE( pGT, piBottom, piRight );
   --(*piBottom);
   --(*piRight);
}

typedef struct
{
   int   iRow;
   int   iCol;
   int   fVisible;
   int   iTop;
   int   iLeft;
   int   iBottom;
   int   iRight;
} _HB_MOUSE_STORAGE;

static int  hb_gt_def_mouseStorageSize( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   return sizeof( _HB_MOUSE_STORAGE );
}

static void hb_gt_def_mouseSaveState( PHB_GT pGT, void * pBuffer )
{
   _HB_MOUSE_STORAGE * pStore = ( _HB_MOUSE_STORAGE * ) pBuffer;
   int iRow, iCol, iTop, iLeft, iBottom, iRight;

   HB_GTSELF_MOUSEGETPOS( pGT, &iRow, &iCol );
   HB_GTSELF_MOUSEGETBOUNDS( pGT, &iTop, &iLeft, &iBottom, &iRight );

   pStore->iRow      = iRow;
   pStore->iCol      = iCol;
   pStore->fVisible  = HB_GTSELF_MOUSEGETCURSOR( pGT );
   pStore->iTop      = iTop;
   pStore->iLeft     = iLeft;
   pStore->iBottom   = iBottom;
   pStore->iRight    = iRight;
}

static void hb_gt_def_mouseRestoreState( PHB_GT pGT, const void * pBuffer )
{
   const _HB_MOUSE_STORAGE * pStore = ( const _HB_MOUSE_STORAGE * ) pBuffer;

   HB_GTSELF_MOUSESETBOUNDS( pGT, pStore->iTop, pStore->iLeft, pStore->iBottom, pStore->iRight );
   HB_GTSELF_MOUSESETPOS( pGT, pStore->iRow, pStore->iCol );
   HB_GTSELF_MOUSESETCURSOR( pGT, pStore->fVisible );
}

static int  hb_gt_def_mouseGetDoubleClickSpeed( PHB_GT pGT )
{
   return pGT->iDoubleClickSpeed;
}

static void hb_gt_def_mouseSetDoubleClickSpeed( PHB_GT pGT, int iSpeed )
{
   if( iSpeed > 0 )
      pGT->iDoubleClickSpeed = iSpeed;
}

static int hb_gt_def_MouseCountButton( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   return 0;
}

static HB_BOOL hb_gt_def_MouseButtonState( PHB_GT pGT, int iButton )
{
   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( iButton );

   return HB_FALSE;
}

static HB_BOOL hb_gt_def_MouseButtonPressed( PHB_GT pGT, int iButton, int * piRow, int * piCol )
{
   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( iButton );
   HB_SYMBOL_UNUSED( piRow );
   HB_SYMBOL_UNUSED( piCol );

   return HB_FALSE;
}

static HB_BOOL hb_gt_def_MouseButtonReleased( PHB_GT pGT, int iButton, int * piRow, int * piCol )
{
   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( iButton );
   HB_SYMBOL_UNUSED( piRow );
   HB_SYMBOL_UNUSED( piCol );

   return HB_FALSE;
}

static int hb_gt_def_MouseReadKey( PHB_GT pGT, int iEventMask )
{
   int iKey = 0, iRow, iCol;

   if( HB_GTSELF_MOUSEISPRESENT( pGT ) )
   {
      if( iEventMask & INKEY_LDOWN && HB_GTSELF_MOUSEBUTTONPRESSED( pGT, 0, &iRow, &iCol ) )
      {
         HB_MAXUINT timer = hb_dateMilliSeconds();
         if( timer - pGT->nMouseLeftTimer <= ( HB_MAXUINT ) HB_GTSELF_MOUSEGETDOUBLECLICKSPEED( pGT ) )
            iKey = K_LDBLCLK;
         else
            iKey = K_LBUTTONDOWN;
         pGT->nMouseLeftTimer = timer;
      }
      else if( iEventMask & INKEY_LUP && HB_GTSELF_MOUSEBUTTONRELEASED( pGT, 0, &iRow, &iCol ) )
      {
         iKey = K_LBUTTONUP;
      }
      else if( iEventMask & INKEY_RDOWN && HB_GTSELF_MOUSEBUTTONPRESSED( pGT, 1, &iRow, &iCol ) )
      {
         HB_MAXUINT timer = hb_dateMilliSeconds();
         if( timer - pGT->nMouseRightTimer <= ( HB_MAXUINT ) HB_GTSELF_MOUSEGETDOUBLECLICKSPEED( pGT ) )
            iKey = K_RDBLCLK;
         else
            iKey = K_RBUTTONDOWN;
         pGT->nMouseRightTimer = timer;
      }
      else if( iEventMask & INKEY_RUP && HB_GTSELF_MOUSEBUTTONRELEASED( pGT, 1, &iRow, &iCol ) )
      {
         iKey = K_RBUTTONUP;
      }
      else if( iEventMask & INKEY_MMIDDLE && HB_GTSELF_MOUSEBUTTONPRESSED( pGT, 2, &iRow, &iCol ) )
      {
         HB_MAXUINT timer = hb_dateMilliSeconds();
         if( timer - pGT->nMouseMiddleTimer <= ( HB_MAXUINT ) HB_GTSELF_MOUSEGETDOUBLECLICKSPEED( pGT ) )
            iKey = K_MDBLCLK;
         else
            iKey = K_MBUTTONDOWN;
         pGT->nMouseMiddleTimer = timer;
      }
      else if( iEventMask & INKEY_MMIDDLE && HB_GTSELF_MOUSEBUTTONRELEASED( pGT, 2, &iRow, &iCol ) )
      {
         iKey = K_MBUTTONUP;
      }
      else if( iEventMask & INKEY_MOVE )
      {
         HB_GTSELF_MOUSEGETPOS( pGT, &iRow, &iCol );
         if( iRow != pGT->iMouseLastRow || iCol != pGT->iMouseLastCol )
         {
            pGT->iMouseLastRow = iRow;
            pGT->iMouseLastCol = iCol;
            iKey = K_MOUSEMOVE;
         }
      }
   }
   return iKey;
}

static int hb_gt_def_GfxPrimitive( PHB_GT pGT, int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor )
{
   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( iType );
   HB_SYMBOL_UNUSED( iTop );
   HB_SYMBOL_UNUSED( iLeft );
   HB_SYMBOL_UNUSED( iBottom );
   HB_SYMBOL_UNUSED( iRight );
   HB_SYMBOL_UNUSED( iColor );

   return 0;
}

static void hb_gt_def_GfxText( PHB_GT pGT, int iTop, int iLeft, const char * szText, int iColor, int iSize, int iWidth )
{
   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( iTop );
   HB_SYMBOL_UNUSED( iLeft );
   HB_SYMBOL_UNUSED( szText );
   HB_SYMBOL_UNUSED( iColor );
   HB_SYMBOL_UNUSED( iSize );
   HB_SYMBOL_UNUSED( iWidth );
}

static void hb_gt_def_WhoCares( PHB_GT pGT, void * pCargo )
{
   HB_SYMBOL_UNUSED( pGT );
   HB_SYMBOL_UNUSED( pCargo );
}

/* ************************************************************************* */

#if defined( __GNUC__ ) && 0
static const HB_GT_FUNCS s_gtCoreFunc =
{
   Lock                       : hb_gt_def_Lock                          ,
   Unlock                     : hb_gt_def_Unlock                        ,
   Init                       : hb_gt_def_Init                          ,
   Exit                       : hb_gt_def_Exit                          ,
   New                        : hb_gt_def_New                           ,
   Free                       : hb_gt_def_Free                          ,
   Mark                       : hb_gt_def_Mark                          ,
   Resize                     : hb_gt_def_Resize                        ,
   SetMode                    : hb_gt_def_SetMode                       ,
   GetSize                    : hb_gt_def_GetSize                       ,
   SemiCold                   : hb_gt_def_SemiCold                      ,
   ColdArea                   : hb_gt_def_ColdArea                      ,
   ExposeArea                 : hb_gt_def_ExposeArea                    ,
   ScrollArea                 : hb_gt_def_ScrollArea                    ,
   TouchLine                  : hb_gt_def_TouchLine                     ,
   TouchCell                  : hb_gt_def_TouchCell                     ,
   Redraw                     : hb_gt_def_Redraw                        ,
   RedrawDiff                 : hb_gt_def_RedrawDiff                    ,
   Refresh                    : hb_gt_def_Refresh                       ,
   Flush                      : hb_gt_def_Flush                         ,
   MaxCol                     : hb_gt_def_MaxCol                        ,
   MaxRow                     : hb_gt_def_MaxRow                        ,
   CheckPos                   : hb_gt_def_CheckPos                      ,
   SetPos                     : hb_gt_def_SetPos                        ,
   GetPos                     : hb_gt_def_GetPos                        ,
   IsColor                    : hb_gt_def_IsColor                       ,
   GetColorStr                : hb_gt_def_GetColorStr                   ,
   SetColorStr                : hb_gt_def_SetColorStr                   ,
   ColorSelect                : hb_gt_def_ColorSelect                   ,
   GetColor                   : hb_gt_def_GetColor                      ,
   ColorNum                   : hb_gt_def_ColorNum                      ,
   ColorsToString             : hb_gt_def_ColorsToString                ,
   StringToColors             : hb_gt_def_StringToColors                ,
   GetColorData               : hb_gt_def_GetColorData                  ,
   GetClearColor              : hb_gt_def_GetClearColor                 ,
   SetClearColor              : hb_gt_def_SetClearColor                 ,
   GetClearChar               : hb_gt_def_GetClearChar                  ,
   SetClearChar               : hb_gt_def_SetClearChar                  ,
   GetCursorStyle             : hb_gt_def_GetCursorStyle                ,
   SetCursorStyle             : hb_gt_def_SetCursorStyle                ,
   GetScrCursor               : hb_gt_def_GetScrCursor                  ,
   GetScrChar                 : hb_gt_def_GetChar                       ,
   PutScrChar                 : hb_gt_def_PutChar                       ,
   DispBegin                  : hb_gt_def_DispBegin                     ,
   DispEnd                    : hb_gt_def_DispEnd                       ,
   DispCount                  : hb_gt_def_DispCount                     ,
   GetChar                    : hb_gt_def_GetChar                       ,
   PutChar                    : hb_gt_def_PutChar                       ,
   RectSize                   : hb_gt_def_RectSize                      ,
   Save                       : hb_gt_def_Save                          ,
   Rest                       : hb_gt_def_Rest                          ,
   PutText                    : hb_gt_def_PutText                       ,
   Replicate                  : hb_gt_def_Replicate                     ,
   WriteAt                    : hb_gt_def_WriteAt                       ,
   Write                      : hb_gt_def_Write                         ,
   WriteCon                   : hb_gt_def_WriteCon                      ,
   SetAttribute               : hb_gt_def_SetAttribute                  ,
   DrawShadow                 : hb_gt_def_DrawShadow                    ,
   Scroll                     : hb_gt_def_Scroll                        ,
   ScrollUp                   : hb_gt_def_ScrollUp                      ,
   Box                        : hb_gt_def_Box                           ,
   BoxD                       : hb_gt_def_BoxD                          ,
   BoxS                       : hb_gt_def_BoxS                          ,
   HorizLine                  : hb_gt_def_HorizLine                     ,
   VertLine                   : hb_gt_def_VertLine                      ,
   GetBlink                   : hb_gt_def_GetBlink                      ,
   SetBlink                   : hb_gt_def_SetBlink                      ,
   SetSnowFlag                : hb_gt_def_SetSnowFlag                   ,
   Version                    : hb_gt_def_Version                       ,
   Suspend                    : hb_gt_def_Suspend                       ,
   Resume                     : hb_gt_def_Resume                        ,
   PreExt                     : hb_gt_def_PreExt                        ,
   PostExt                    : hb_gt_def_PostExt                       ,
   OutStd                     : hb_gt_def_OutStd                        ,
   OutErr                     : hb_gt_def_OutErr                        ,
   Tone                       : hb_gt_def_Tone                          ,
   Bell                       : hb_gt_def_Bell                          ,
   Info                       : hb_gt_def_Info                          ,
   Alert                      : hb_gt_def_Alert                         ,
   SetFlag                    : hb_gt_def_SetFlag                       ,
   SetDispCP                  : hb_gt_def_SetDispCP                     ,
   SetKeyCP                   : hb_gt_def_SetKeyCP                      ,
   ReadKey                    : hb_gt_def_ReadKey                       ,
   InkeyGet                   : hb_gt_def_InkeyGet                      ,
   InkeyPut                   : hb_gt_def_InkeyPut                      ,
   InkeyIns                   : hb_gt_def_InkeyIns                      ,
   InkeyLast                  : hb_gt_def_InkeyLast                     ,
   InkeyNext                  : hb_gt_def_InkeyNext                     ,
   InkeyPoll                  : hb_gt_def_InkeyPoll                     ,
   InkeySetText               : hb_gt_def_InkeySetText                  ,
   InkeySetLast               : hb_gt_def_InkeySetLast                  ,
   InkeyReset                 : hb_gt_def_InkeyReset                    ,
   InkeyExit                  : hb_gt_def_InkeyExit                     ,
   MouseInit                  : hb_gt_def_MouseInit                     ,
   MouseExit                  : hb_gt_def_MouseExit                     ,
   MouseIsPresent             : hb_gt_def_MouseIsPresent                ,
   MouseShow                  : hb_gt_def_MouseShow                     ,
   MouseHide                  : hb_gt_def_MouseHide                     ,
   MouseGetCursor             : hb_gt_def_MouseGetCursor                ,
   MouseSetCursor             : hb_gt_def_MouseSetCursor                ,
   MouseCol                   : hb_gt_def_MouseCol                      ,
   MouseRow                   : hb_gt_def_MouseRow                      ,
   MouseGetPos                : hb_gt_def_MouseGetPos                   ,
   MouseSetPos                : hb_gt_def_MouseSetPos                   ,
   MouseSetBounds             : hb_gt_def_MouseSetBounds                ,
   MouseGetBounds             : hb_gt_def_MouseGetBounds                ,
   MouseStorageSize           : hb_gt_def_mouseStorageSize              ,
   MouseSaveState             : hb_gt_def_mouseSaveState                ,
   MouseRestoreState          : hb_gt_def_mouseRestoreState             ,
   MouseGetDoubleClickSpeed   : hb_gt_def_mouseGetDoubleClickSpeed      ,
   MouseSetDoubleClickSpeed   : hb_gt_def_mouseSetDoubleClickSpeed      ,
   MouseCountButton           : hb_gt_def_MouseCountButton              ,
   MouseButtonState           : hb_gt_def_MouseButtonState              ,
   MouseButtonPressed         : hb_gt_def_MouseButtonPressed            ,
   MouseButtonReleased        : hb_gt_def_MouseButtonReleased           ,
   MouseReadKey               : hb_gt_def_MouseReadKey                  ,
   GfxPrimitive               : hb_gt_def_GfxPrimitive                  ,
   GfxText                    : hb_gt_def_GfxText                       ,
   WhoCares                   : hb_gt_def_WhoCares
};
#else
static const HB_GT_FUNCS s_gtCoreFunc =
{
   hb_gt_def_Lock                         ,
   hb_gt_def_Unlock                       ,
   hb_gt_def_Init                         ,
   hb_gt_def_Exit                         ,
   hb_gt_def_New                          ,
   hb_gt_def_Free                         ,
   hb_gt_def_Mark                         ,
   hb_gt_def_Resize                       ,
   hb_gt_def_SetMode                      ,
   hb_gt_def_GetSize                      ,
   hb_gt_def_SemiCold                     ,
   hb_gt_def_ColdArea                     ,
   hb_gt_def_ExposeArea                   ,
   hb_gt_def_ScrollArea                   ,
   hb_gt_def_TouchLine                    ,
   hb_gt_def_TouchCell                    ,
   hb_gt_def_Redraw                       ,
   hb_gt_def_RedrawDiff                   ,
   hb_gt_def_Refresh                      ,
   hb_gt_def_Flush                        ,
   hb_gt_def_MaxCol                       ,
   hb_gt_def_MaxRow                       ,
   hb_gt_def_CheckPos                     ,
   hb_gt_def_SetPos                       ,
   hb_gt_def_GetPos                       ,
   hb_gt_def_IsColor                      ,
   hb_gt_def_GetColorStr                  ,
   hb_gt_def_SetColorStr                  ,
   hb_gt_def_ColorSelect                  ,
   hb_gt_def_GetColor                     ,
   hb_gt_def_ColorNum                     ,
   hb_gt_def_ColorsToString               ,
   hb_gt_def_StringToColors               ,
   hb_gt_def_GetColorData                 ,
   hb_gt_def_GetClearColor                ,
   hb_gt_def_SetClearColor                ,
   hb_gt_def_GetClearChar                 ,
   hb_gt_def_SetClearChar                 ,
   hb_gt_def_GetCursorStyle               ,
   hb_gt_def_SetCursorStyle               ,
   hb_gt_def_GetScrCursor                 ,
   hb_gt_def_GetChar                      , /* intentionally mapped to GetScrChar */
   hb_gt_def_PutChar                      , /* intentionally mapped to PutScrChar */
   hb_gt_def_DispBegin                    ,
   hb_gt_def_DispEnd                      ,
   hb_gt_def_DispCount                    ,
   hb_gt_def_GetChar                      ,
   hb_gt_def_PutChar                      ,
   hb_gt_def_RectSize                     ,
   hb_gt_def_Save                         ,
   hb_gt_def_Rest                         ,
   hb_gt_def_PutText                      ,
   hb_gt_def_Replicate                    ,
   hb_gt_def_WriteAt                      ,
   hb_gt_def_Write                        ,
   hb_gt_def_WriteCon                     ,
   hb_gt_def_SetAttribute                 ,
   hb_gt_def_DrawShadow                   ,
   hb_gt_def_Scroll                       ,
   hb_gt_def_ScrollUp                     ,
   hb_gt_def_Box                          ,
   hb_gt_def_BoxD                         ,
   hb_gt_def_BoxS                         ,
   hb_gt_def_HorizLine                    ,
   hb_gt_def_VertLine                     ,
   hb_gt_def_GetBlink                     ,
   hb_gt_def_SetBlink                     ,
   hb_gt_def_SetSnowFlag                  ,
   hb_gt_def_Version                      ,
   hb_gt_def_Suspend                      ,
   hb_gt_def_Resume                       ,
   hb_gt_def_PreExt                       ,
   hb_gt_def_PostExt                      ,
   hb_gt_def_OutStd                       ,
   hb_gt_def_OutErr                       ,
   hb_gt_def_Tone                         ,
   hb_gt_def_Bell                         ,
   hb_gt_def_Info                         ,
   hb_gt_def_Alert                        ,
   hb_gt_def_SetFlag                      ,
   hb_gt_def_SetDispCP                    ,
   hb_gt_def_SetKeyCP                     ,
   hb_gt_def_ReadKey                      ,
   hb_gt_def_InkeyGet                     ,
   hb_gt_def_InkeyPut                     ,
   hb_gt_def_InkeyIns                     ,
   hb_gt_def_InkeyLast                    ,
   hb_gt_def_InkeyNext                    ,
   hb_gt_def_InkeyPoll                    ,
   hb_gt_def_InkeySetText                 ,
   hb_gt_def_InkeySetLast                 ,
   hb_gt_def_InkeyReset                   ,
   hb_gt_def_InkeyExit                    ,
   hb_gt_def_MouseInit                    ,
   hb_gt_def_MouseExit                    ,
   hb_gt_def_MouseIsPresent               ,
   hb_gt_def_MouseShow                    ,
   hb_gt_def_MouseHide                    ,
   hb_gt_def_MouseGetCursor               ,
   hb_gt_def_MouseSetCursor               ,
   hb_gt_def_MouseCol                     ,
   hb_gt_def_MouseRow                     ,
   hb_gt_def_MouseGetPos                  ,
   hb_gt_def_MouseSetPos                  ,
   hb_gt_def_MouseSetBounds               ,
   hb_gt_def_MouseGetBounds               ,
   hb_gt_def_mouseStorageSize             ,
   hb_gt_def_mouseSaveState               ,
   hb_gt_def_mouseRestoreState            ,
   hb_gt_def_mouseGetDoubleClickSpeed     ,
   hb_gt_def_mouseSetDoubleClickSpeed     ,
   hb_gt_def_MouseCountButton             ,
   hb_gt_def_MouseButtonState             ,
   hb_gt_def_MouseButtonPressed           ,
   hb_gt_def_MouseButtonReleased          ,
   hb_gt_def_MouseReadKey                 ,
   hb_gt_def_GfxPrimitive                 ,
   hb_gt_def_GfxText                      ,
   hb_gt_def_WhoCares
};
#endif

/* ************************************************************************* */

static char s_gtNameBuf[ HB_GT_NAME_MAX_ + 1 ];

/* NOTE: Must be in sync with gtsys.c */
#if defined( HB_GT_LIB )
   const char * hb_gt_szNameDefault = HB_GT_DRVNAME( HB_GT_LIB );
#elif defined( HB_OS_WIN_CE )
   const char * hb_gt_szNameDefault = "wvt";
#elif defined( HB_OS_WIN )
   const char * hb_gt_szNameDefault = "win";
#elif defined( HB_OS_DOS )
   const char * hb_gt_szNameDefault = "dos";
#elif defined( HB_OS_OS2 )
   const char * hb_gt_szNameDefault = "os2";
#elif defined( HB_OS_VXWORKS ) || defined( HB_OS_SYMBIAN )
   const char * hb_gt_szNameDefault = "std";
#elif defined( HB_OS_UNIX )
   const char * hb_gt_szNameDefault = "trm";
#else
   const char * hb_gt_szNameDefault = "std";
#endif

static const HB_GT_INIT * s_gtInit[ HB_GT_MAX_ ];
static int s_iGtCount = 0;

HB_FUNC_EXTERN( HB_GTSYS );

static const char * hb_gt_FindDefault( void )
{
   char szFuncName[ 15 + HB_GT_NAME_MAX_ ];
   int iPos;

   for( iPos = 0; iPos < s_iGtCount; iPos++ )
   {
      hb_snprintf( szFuncName, sizeof( szFuncName ),
                   "HB_GT_%s_DEFAULT", s_gtInit[ iPos ]->id );
      if( hb_dynsymFind( szFuncName ) )
         return s_gtInit[ iPos ]->id;
   }

   if( hb_dynsymFind( "HB_GT_NUL_DEFAULT" ) )
      return "NUL";
   else
      return NULL;
}

static int hb_gt_FindEntry( const char * pszID )
{
   int iPos;

   for( iPos = 0; iPos < s_iGtCount; iPos++ )
   {
      if( hb_stricmp( s_gtInit[ iPos ]->id, pszID ) == 0 ||
          ( hb_strnicmp( pszID, "gt", 2 ) == 0 &&
            hb_stricmp( s_gtInit[ iPos ]->id, pszID + 2 ) == 0 ) )
         return iPos;
   }

   return -1;
}

void hb_gtSetDefault( const char * szGtName )
{
   hb_strncpy( s_gtNameBuf, szGtName, sizeof( s_gtNameBuf ) - 1 );
   hb_gt_szNameDefault = s_gtNameBuf;
}

HB_BOOL hb_gtRegister( const HB_GT_INIT * gtInit )
{
   if( s_iGtCount < HB_GT_MAX_ && hb_gt_FindEntry( gtInit->id ) == -1 )
   {
      if( gtInit->pGtId )
         *gtInit->pGtId = s_iGtCount;
      s_gtInit[ s_iGtCount++ ] = gtInit;
      return HB_TRUE;
   }
   return HB_FALSE;
}

PHB_GT hb_gtLoad( const char * szGtName, PHB_GT pGT, PHB_GT_FUNCS pSuperTable )
{
   int iPos;

   if( szGtName )
   {
      if( hb_stricmp( szGtName, "nul" ) == 0 || hb_stricmp( szGtName, "null" ) == 0 )
      {
         if( pGT || pSuperTable )
            hb_errInternal( 9996, "Harbour terminal (GT) initialization failure", NULL, NULL );

         pGT = ( PHB_GT_BASE ) hb_xgrab( sizeof( HB_GT_BASE ) );
         memset( pGT, 0, sizeof( HB_GT_BASE ) );
         pGT->pFuncTable = ( PHB_GT_FUNCS ) hb_xgrab( sizeof( HB_GT_FUNCS ) );
         memcpy( pGT->pFuncTable, &s_gtCoreFunc, sizeof( HB_GT_FUNCS ) );
         pGT->iUsed++;
         return pGT;
      }

      iPos = hb_gt_FindEntry( szGtName );

      if( iPos != -1 )
      {
         HB_BOOL fNew = pGT == NULL;

         if( fNew )
         {
            pGT = ( PHB_GT_BASE ) hb_xgrab( sizeof( HB_GT_BASE ) );
            memset( pGT, 0, sizeof( HB_GT_BASE ) );
            pGT->pFuncTable = ( PHB_GT_FUNCS ) hb_xgrab( sizeof( HB_GT_FUNCS ) );
            memcpy( pGT->pFuncTable, &s_gtCoreFunc, sizeof( HB_GT_FUNCS ) );
            pGT->iUsed++;
         }

         if( pSuperTable == NULL )
            pSuperTable = s_gtInit[ iPos ]->pSuperTable;
         if( pSuperTable != NULL )
            memcpy( pSuperTable, pGT->pFuncTable, sizeof( HB_GT_FUNCS ) );

         if( s_gtInit[ iPos ]->init( pGT->pFuncTable ) )
            return pGT;
         else if( fNew )
         {
            hb_xfree( pGT->pFuncTable );
            hb_xfree( pGT );
         }
      }
   }
   return NULL;
}

void hb_gtIsGtRef( void * hGT )
{
   PHB_GT pGT = ( PHB_GT ) hGT;

   if( pGT )
      HB_GTSELF_MARK( pGT );
}

void * hb_gtAlloc( void * hGT )
{
   PHB_GT pGT;

   if( hGT )
   {
      pGT = ( PHB_GT ) hGT;
      if( !HB_GTSELF_LOCK( pGT ) )
         pGT = NULL;
   }
   else
      pGT = hb_gt_Base();

   if( pGT )
   {
      pGT->iUsed++;
      hb_gt_BaseFree( pGT );
   }

   return ( void * ) pGT;
}

void hb_gtRelease( void * hGT )
{
   PHB_GT pGT;

   if( hGT )
   {
      pGT = ( PHB_GT ) hGT;
      if( !HB_GTSELF_LOCK( pGT ) )
         pGT = NULL;
   }
   else
      pGT = hb_gt_Base();

   if( pGT )
   {
      if( --pGT->iUsed == 0 )
      {
         while( HB_GTSELF_DISPCOUNT( pGT ) )
            HB_GTSELF_DISPEND( pGT );
         HB_GTSELF_FLUSH( pGT );
         HB_GTSELF_EXIT( pGT );
      }
      else
         hb_gt_BaseFree( pGT );
   }
}

void hb_gtAttach( void * hGT )
{
   if( hGT && hGT != hb_stackGetGT() )
   {
      hb_gtRelease( NULL );
      hb_stackSetGT( hGT );
   }
}

void * hb_gtSwap( void * hGT )
{
   void * hCurrGT = hb_stackGetGT();

   hb_stackSetGT( hGT );

   return hCurrGT;
}

HB_BOOL hb_gtReload( const char * szGtName,
                     HB_FHANDLE hFilenoStdin,
                     HB_FHANDLE hFilenoStdout,
                     HB_FHANDLE hFilenoStderr )
{
   HB_BOOL fResult = HB_FALSE;
   if( szGtName && hb_gt_FindEntry( szGtName ) != -1 )
   {
      hb_gtRelease( NULL );
      hb_stackSetGT( hb_gtLoad( szGtName, NULL, NULL ) );
      fResult = hb_stackGetGT() != NULL;
      hb_gtInit( hFilenoStdin, hFilenoStdout, hFilenoStderr );
   }
   return fResult;
}

void * hb_gtCreate( const char * szGtName,
                    HB_FHANDLE hFilenoStdin,
                    HB_FHANDLE hFilenoStdout,
                    HB_FHANDLE hFilenoStderr )
{
   void * hCurrGT = hb_gtSwap( NULL );

   if( szGtName && hb_gt_FindEntry( szGtName ) != -1 )
   {
      PHB_GT pGT = hb_gtLoad( szGtName, NULL, NULL );
      if( pGT )
      {
         hb_stackSetGT( pGT );
         hb_gtInit( hFilenoStdin, hFilenoStdout, hFilenoStderr );
      }
   }
   return hb_gtSwap( hCurrGT );
}

static HB_BOOL hb_gtTryInit( const char * szGtName, HB_BOOL fFree )
{
   if( szGtName )
   {
      if( hb_stackGetGT() == NULL )
         hb_stackSetGT( hb_gtLoad( szGtName, NULL, NULL ) );

      if( fFree )
         hb_xfree( ( void * ) szGtName );
   }

   return hb_stackGetGT() != NULL;
}

void hb_gtStartupInit( void )
{
   if( hb_gtTryInit( hb_cmdargString( "GT" ), HB_TRUE ) )
      return;
   if( hb_gtTryInit( hb_getenv( "HB_GT" ), HB_TRUE ) )
      return;
   if( hb_gtTryInit( hb_gt_FindDefault(), HB_FALSE ) )
      return;
   if( hb_gtTryInit( hb_gt_szNameDefault, HB_FALSE ) )
      return;

   if( hb_dynsymFind( "HB_GT_NUL" ) ) /* GTNUL was explicitly REQUESTed */
   {
      if( hb_gtTryInit( "NUL", HB_FALSE ) )
         return;
   }

   hb_errInternal( 9998, "Harbour terminal (GT) initialization failure", NULL, NULL );

   /* not executed, only to force linking HB_GTSYS() */
   HB_FUNC_EXEC( HB_GTSYS );
}

HB_GT_ANNOUNCE( HB_GT_NAME )

static HB_GARBAGE_FUNC( hb_gt_Destructor )
{
   void ** gtHolder = ( void ** ) Cargo;

   if( *gtHolder )
   {
      hb_gtRelease( *gtHolder );
      *gtHolder = NULL;
   }
}

static HB_GARBAGE_FUNC( hb_gt_Mark )
{
   void ** gtHolder = ( void ** ) Cargo;

   if( *gtHolder )
      HB_GTSELF_MARK( ( PHB_GT ) *gtHolder );
}

static const HB_GC_FUNCS s_gcGTFuncs =
{
   hb_gt_Destructor,
   hb_gt_Mark
};

static void * hb_gtParam( int iParam )
{
   void ** gtHolder = ( void ** ) hb_parptrGC( &s_gcGTFuncs, iParam );

   if( gtHolder && *gtHolder )
      return *gtHolder;

   hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   return NULL;
}

PHB_GT hb_gt_ItemBase( PHB_ITEM pItemGT )
{
   void ** gtHolder = ( void ** ) hb_itemGetPtrGC( pItemGT, &s_gcGTFuncs );
   if( gtHolder && *gtHolder )
   {
      PHB_GT pGT = ( PHB_GT ) *gtHolder;
      if( HB_GTSELF_LOCK( pGT ) )
         return pGT;
   }
   return NULL;
}

HB_FUNC( HB_GTRELOAD )
{
   hb_retl( hb_gtReload( hb_parc( 1 ),
            HB_ISNUM( 2 ) ? hb_numToHandle( hb_parnint( 1 ) ) : HB_STDIN_HANDLE,
            HB_ISNUM( 3 ) ? hb_numToHandle( hb_parnint( 2 ) ) : HB_STDOUT_HANDLE,
            HB_ISNUM( 4 ) ? hb_numToHandle( hb_parnint( 3 ) ) : HB_STDERR_HANDLE ) );
}

HB_FUNC( HB_GTCREATE )
{
   void * hGT;

   hGT = hb_gtCreate( hb_parc( 1 ),
            HB_ISNUM( 2 ) ? hb_numToHandle( hb_parnint( 1 ) ) : HB_STDIN_HANDLE,
            HB_ISNUM( 3 ) ? hb_numToHandle( hb_parnint( 2 ) ) : HB_STDOUT_HANDLE,
            HB_ISNUM( 4 ) ? hb_numToHandle( hb_parnint( 3 ) ) : HB_STDERR_HANDLE );

   if( hGT )
   {
      void ** gtHolder = ( void ** ) hb_gcAllocate( sizeof( void * ), &s_gcGTFuncs );
      *gtHolder = hGT;
      hb_retptrGC( gtHolder );
   }
}

HB_FUNC( HB_GTSELECT )
{
   void * hGT;

   if( hb_pcount() > 0 )
   {
      hGT = hb_gtParam( 1 );
      if( hGT )
      {
         hGT = hb_gtAlloc( hGT );
         if( hGT )
            hGT = hb_gtSwap( hGT );
      }
   }
   else
      hGT = hb_gtAlloc( NULL );

   if( hGT )
   {
      void ** gtHolder = ( void ** ) hb_gcAllocate( sizeof( void * ), &s_gcGTFuncs );
      *gtHolder = hGT;
      hb_retptrGC( gtHolder );
   }
}
