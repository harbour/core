/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour Graphic Terminal low level code
 *
 * Copyright 2006 Przemyslaw Czerpak < druzus /at/ priv.onet.pl >
 * www - http://www.harbour-project.org
 *
 * part of the code in hb_gt_def_* functions is based on the code
 * from old hbapi.c copyrighted by:
 * Copyright 1999 Bil Simser <bsimser@home.com>
 * Copyright 1999 Paul Tucker <ptucker@sympatico.ca>
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
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

static PHB_GT_BASE   s_curGT = NULL;
static BOOL          s_fVgaCell   = TRUE;
static BOOL          s_fIsColor   = TRUE;
static BOOL          s_fBlinking  = TRUE;
static BOOL          s_fStdOutCon = FALSE;
static BOOL          s_fStdErrCon = FALSE;
static int           s_iCursorShape = SC_NORMAL;
static USHORT        s_uiDispCount = 0;
static USHORT        s_uiExtCount = 0;
static USHORT        s_uiClearChar = ' ';
static BYTE          s_bClearColor = 0x07;
static FHANDLE       s_hStdIn = 0, s_hStdOut = 1, s_hStdErr = 2;

static BOOL          s_fDispTrans = FALSE;
static PHB_CODEPAGE  s_cdpTerm = NULL;
static PHB_CODEPAGE  s_cdpHost = NULL;

static int           s_iColorIndex;
static int           s_iColorCount;
static int *         s_pColor;

/* masks: 0x0007     Foreground
          0x0070     Background
          0x0008     Bright
          0x0080     Blink
          0x0800     Underline foreground
          0x8000     Underline background
 */

static int           s_iDoubleClickSpeed = 168; /* In milliseconds */
static BOOL          s_fMouseVisible = FALSE;
static int           s_iMouseLastRow;
static int           s_iMouseLastCol;
static HB_LONG       s_iMouseLeftTimer;
static HB_LONG       s_iMouseRightTimer;
static HB_LONG       s_iMouseMiddleTimer;

static void * hb_gt_def_New( void )
{
   PHB_GT_BASE pGT;
   ULONG ulSize, ulIndex;
   USHORT usChar;
   BYTE bColor, bAttr;
   int i;

   pGT = ( PHB_GT_BASE ) hb_xgrab( sizeof( HB_GT_BASE ) );
   memset( pGT, 0, sizeof( HB_GT_BASE ) );

   hb_gt_GetSize( &pGT->iHeight, &pGT->iWidth );
   ulSize = ( ULONG ) pGT->iHeight * pGT->iWidth;

   pGT->screenBuffer =
            ( PHB_SCREENCELL ) hb_xgrab( sizeof( HB_SCREENCELL ) * ulSize );
   pGT->prevBuffer =
            ( PHB_SCREENCELL ) hb_xgrab( sizeof( HB_SCREENCELL ) * ulSize );
   pGT->pLines = ( BOOL * ) hb_xgrab( sizeof( BOOL ) * pGT->iHeight );

   memset( pGT->prevBuffer, 0, sizeof( HB_SCREENCELL ) * ulSize );
   for( i = 0; i < pGT->iHeight; ++i )
      pGT->pLines[ i ] = TRUE;

   usChar = hb_gt_GetClearChar();
   bColor = hb_gt_GetClearColor();
   bAttr  = 0;
   for( ulIndex = 0; ulIndex < ulSize; ++ulIndex )
   {
      pGT->screenBuffer[ ulIndex ].c.usChar = usChar;
      pGT->screenBuffer[ ulIndex ].c.bColor = bColor;
      pGT->screenBuffer[ ulIndex ].c.bAttr = bAttr;
      pGT->prevBuffer[ ulIndex ].c.bAttr = HB_GT_ATTR_REFRESH;
   }

   return pGT;
}

static void hb_gt_def_Free( void * pGtPtr )
{
   PHB_GT_BASE pGT = ( PHB_GT_BASE ) pGtPtr;

   if( pGT->screenBuffer )
      hb_xfree( pGT->screenBuffer );
   if( pGT->prevBuffer )
      hb_xfree( pGT->prevBuffer );
   if( pGT->pLines )
      hb_xfree( pGT->pLines );
   hb_xfree( pGT );
}

static void hb_gt_def_Init( FHANDLE hStdIn, FHANDLE hStdOut, FHANDLE hStdErr )
{
   s_hStdIn  = hStdIn;
   s_hStdOut = hStdOut;
   s_hStdErr = hStdErr;

   s_curGT = ( PHB_GT_BASE ) hb_gt_New();
   hb_gt_Resize( s_curGT->iHeight, s_curGT->iWidth );
   hb_mouse_Init();
   hb_mouse_GetPos( &s_iMouseLastRow, &s_iMouseLastCol );
}

static void hb_gt_def_Exit( void )
{
   hb_mouse_Exit();

   if( s_curGT )
   {
      hb_gt_Free( s_curGT );
      s_curGT = NULL;
   }
   if( s_iColorCount > 0 )
   {
      hb_xfree( s_pColor );
      s_iColorCount = 0;
   }
}

static BOOL hb_gt_def_CheckPos( int iRow, int iCol, long *plIndex )
{
   if( iRow >= 0 && iCol >= 0 )
   {
      int iHeight, iWidth;

      hb_gt_GetSize( &iHeight, &iWidth );
      if( iRow < iHeight && iCol < iWidth )
      {
         if( plIndex )
            *plIndex = ( long ) iRow * iWidth + iCol;
         return TRUE;
      }
   }
   return FALSE;
}

static void hb_gt_def_GetPos( int * piRow, int * piCol )
{
   if( s_curGT )
   {
      * piRow = s_curGT->iRow;
      * piCol = s_curGT->iCol;
   }
   else
      * piRow = * piCol = 0;
}

static void hb_gt_def_SetPos( int iRow, int iCol )
{
   if( s_curGT )
   {
      s_curGT->iRow = iRow;
      s_curGT->iCol = iCol;
   }
}

static int hb_gt_def_MaxCol( void )
{
   if( s_curGT )
      return s_curGT->iWidth - 1;
   else
      return 79;
}

static int hb_gt_def_MaxRow( void )
{
   if( s_curGT )
      return s_curGT->iHeight - 1;
   else
      return 24;
}

static BOOL hb_gt_def_IsColor( void )
{
   return s_fIsColor;
}

/* NOTE: szColorString must be at least CLR_STRLEN wide by the NG. It seems
         that CA-Cl*pper SETCOLOR() will return string lengths up to 131+EOF.
         That seems like a 127+1 buffer size, plus lazy overflow checking.
         [vszakats] */
static void hb_gt_def_GetColorStr( char * pszColorString )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_GetColorStr(%s)", pszColorString));

   hb_gt_ColorsToString( s_pColor, s_iColorCount, pszColorString, CLR_STRLEN );
}

static void hb_gt_def_SetColorStr( const char * szColorString )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_SetColorStr(%s)", szColorString));

   hb_gt_StringToColors( szColorString, &s_pColor, &s_iColorCount );

   s_iColorIndex = HB_CLR_STANDARD; /* hb_gtColorSelect( HB_CLR_STANDARD ); */
}

static void hb_gt_def_ColorSelect( int iColorIndex )
{
   if( iColorIndex >= 0 && iColorIndex < ( int ) s_iColorCount )
      s_iColorIndex = iColorIndex;
}

static int  hb_gt_def_GetColor( void )
{
   if( s_iColorCount )
      return s_pColor[ s_iColorIndex ];
   else
      return hb_gt_GetClearChar();
}

static void hb_gt_def_GetColorData( int ** pColorsPtr, int * piColorCount, int * piColorIndex )
{
   if( s_iColorCount )
   {
      *pColorsPtr = ( int * ) hb_xgrab( s_iColorCount * sizeof( int ) );
      memcpy( *pColorsPtr, s_pColor, s_iColorCount * sizeof( int ) );
      *piColorCount = s_iColorCount;
      *piColorIndex = s_iColorIndex;
   }
   else
   {
      *pColorsPtr = ( int * ) hb_xgrab( sizeof( int ) );
      *pColorsPtr[ 0 ] = 0;
      *piColorCount = 1;
      *piColorIndex = 0;
   }
}

static int  hb_gt_def_GetClearColor( void )
{
   return s_bClearColor;
}

static void hb_gt_def_SetClearColor( int iColor )
{
   s_bClearColor = iColor ;
}

static int  hb_gt_def_GetClearChar( void )
{
   return s_uiClearChar;
}

static void hb_gt_def_SetClearChar( int iChar )
{
   s_uiClearChar = iChar;
}

static const char * hb_gt_def_ColorDecode( const char * szColorString, int * piColor )
{
   char c;
   int nColor = 0;
   BOOL bFore = TRUE;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_ColorDecode(%p)", szColorString, piColor));

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
               bFore = FALSE;
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
            bFore = FALSE;
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
            * piColor = nColor;
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
      }
   }

   * piColor = nColor;
   return NULL;
}

static int  hb_gt_def_ColorNum( const char * szColorString )
{
   int nColor;

   hb_gt_def_ColorDecode( szColorString, &nColor );

   return nColor;
}

static void hb_gt_def_StringToColors( const char * szColorString, int ** pColorsPtr, int * piColorCount )
{
   int * pColors;
   int nPos = 0;
   int nColor;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_StringToColors(%s,%p,%p)", szColorString, pColorsPtr, piColorCount));

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
      }
      pColors[ nPos++ ] = nColor;

   }
   while( szColorString );

   if( nPos >= HB_CLR_ENHANCED && nPos < HB_CLR_UNSELECTED &&
       *piColorCount > HB_CLR_UNSELECTED )
      pColors[ HB_CLR_UNSELECTED ] = pColors[ HB_CLR_ENHANCED ];
}

static void hb_gt_def_ColorsToString( int * pColors, int iColorCount, char * pszColorString, int iBufSize )
{
   int iColorIndex, iPos = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_def_ColorsToString(%p,%d,%s,%d)", pColors, iColorCount, pszColorString, iBufSize));

   /* Go on if there's space left for the largest color string plus EOF */
   for( iColorIndex = 0; iColorIndex < iColorCount && iPos < iBufSize - 8; ++iColorIndex )
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
#ifdef HB_C52_STRICT
            if( ( pColors[ iColorIndex ] & 0x80 ) != 0 )
               pszColorString[ iPos++ ] = '*';
#endif

            if( ( pColors[ iColorIndex ] & 0x08 ) != 0 )
               pszColorString[ iPos++ ] = '+';

            pszColorString[ iPos++ ] = '/';
         }
#ifndef HB_C52_STRICT
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


static int  hb_gt_def_GetCursorStyle( void )
{
   return s_iCursorShape;
}

static void hb_gt_def_SetCursorStyle( int iStyle )
{
   switch( iStyle )
   {
      case SC_NONE:
      case SC_NORMAL:
      case SC_INSERT:
      case SC_SPECIAL1:
      case SC_SPECIAL2:
         s_iCursorShape = iStyle;
         break;
      default:
         s_iCursorShape = SC_NORMAL;
         break;
   }
}

static void hb_gt_def_GetScrCursor( int * piRow, int * piCol, int * piStyle )
{
   hb_gt_GetPos( piRow, piCol );
   if( *piRow < 0 || *piCol < 0 || *piRow > hb_gt_MaxRow() || *piCol > hb_gt_MaxCol() )
      *piStyle = SC_NONE;
   else
      *piStyle = hb_gt_GetCursorStyle();
}

static BOOL hb_gt_def_GetBlink( void )
{
   return s_fBlinking;
}

static void hb_gt_def_SetBlink( BOOL fBlink )
{
   s_fBlinking = fBlink;
}

static void hb_gt_def_SetSnowFlag( BOOL fNoSnow )
{
   /*
    * NOTE: This is a compatibility function which have to be implemented
    *       in low level GT driver.
    *       If you're running on a CGA and snow is a problem speak up!
    */

   HB_SYMBOL_UNUSED( fNoSnow );
}

static void hb_gt_def_DispBegin( void )
{
   ++s_uiDispCount;
}

static void hb_gt_def_DispEnd( void )
{
   if( s_uiDispCount > 0 )
      --s_uiDispCount;
}

static int hb_gt_def_DispCount( void )
{
   return s_uiDispCount;
}

static BOOL hb_gt_def_PreExt( void )
{
   if( s_uiExtCount == 0 )
   {
      hb_gt_Refresh();
   }
   s_uiExtCount++;

   return TRUE;
}

static BOOL hb_gt_def_PostExt( void )
{
   if( s_uiExtCount )
      --s_uiExtCount;

   return TRUE;
}

static BOOL hb_gt_def_Suspend( void )
{
   return hb_gt_PreExt();
}

static BOOL hb_gt_def_Resume( void )
{
   return hb_gt_PostExt();
}

static void hb_gt_def_OutFile( FHANDLE hFile, BYTE * pbyStr, ULONG ulLen )
{
   hb_fsWriteLarge( hFile, pbyStr, ulLen );
}

static void hb_gt_def_OutStd( BYTE * pbyStr, ULONG ulLen )
{
   if( ulLen )
   {
      if( s_curGT )
      {
         if( s_fStdOutCon )
            hb_gt_WriteCon( pbyStr, ulLen );
         else
         {
            hb_gt_PreExt();
            if( s_fDispTrans )
            {
               BYTE * pbyStrBuff = ( BYTE * ) hb_xgrab( ulLen );
               memcpy( pbyStrBuff, pbyStr, ulLen );
               hb_cdpnTranslate( ( char * ) pbyStrBuff, s_cdpHost, s_cdpTerm, ulLen );
               hb_gt_def_OutFile( s_hStdOut, pbyStrBuff, ulLen );
               hb_xfree( pbyStrBuff );
            }
            else
               hb_gt_def_OutFile( s_hStdOut, pbyStr, ulLen );
            hb_gt_PostExt();
         }
      }
      else
         hb_gt_def_OutFile( s_hStdOut, pbyStr, ulLen );
   }
}

static void hb_gt_def_OutErr( BYTE * pbyStr, ULONG ulLen )
{
   if( ulLen )
   {
      if( s_curGT )
      {
         if( s_fStdErrCon )
            hb_gt_WriteCon( pbyStr, ulLen );
         else
         {
            hb_gt_PreExt();
            if( s_fDispTrans )
            {
               BYTE * pbyStrBuff = ( BYTE * ) hb_xgrab( ulLen );
               memcpy( pbyStrBuff, pbyStr, ulLen );
               hb_cdpnTranslate( ( char * ) pbyStrBuff, s_cdpHost, s_cdpTerm, ulLen );
               hb_gt_def_OutFile( s_hStdErr, pbyStrBuff, ulLen );
               hb_xfree( pbyStrBuff );
            }
            else
               hb_gt_def_OutFile( s_hStdErr, pbyStr, ulLen );
            hb_gt_PostExt();
         }
      }
      else
         hb_gt_def_OutFile( s_hStdErr, pbyStr, ulLen );
   }
}

static void hb_gt_def_Tone( double dFrequency, double dDuration )
{
   HB_SYMBOL_UNUSED( dFrequency );

   /* convert Clipper (DOS) timer tick units to seconds ( x / 18.2 ) */
   hb_idleSleep( dDuration / 18.2 );
}

static void hb_gt_def_Bell( void )
{
   hb_gt_Tone( 700.0, 3.0 );
}

static char * hb_gt_def_Version( int iType )
{
   if( iType == 0 )
      return "NUL";

   return "Harbour Terminal: NULL";
}

static BOOL hb_gt_def_GetChar( int iRow, int iCol,
                               BYTE * pbColor, BYTE * pbAttr, USHORT * pusChar )
{
   if( s_curGT )
   {
      long lIndex;

      if( hb_gt_CheckPos( iRow, iCol, &lIndex ) )
      {
         *pusChar = s_curGT->screenBuffer[ lIndex ].c.usChar;
         *pbColor = s_curGT->screenBuffer[ lIndex ].c.bColor;
         *pbAttr  = s_curGT->screenBuffer[ lIndex ].c.bAttr;
         return TRUE;
      }
   }
   return FALSE;
}

static BOOL hb_gt_def_PutChar( int iRow, int iCol,
                               BYTE bColor, BYTE bAttr, USHORT usChar )
{
   if( s_curGT )
   {
      long lIndex;

      if( hb_gt_CheckPos( iRow, iCol, &lIndex ) )
      {
         s_curGT->screenBuffer[ lIndex ].c.usChar = usChar;
         s_curGT->screenBuffer[ lIndex ].c.bColor = bColor;
         s_curGT->screenBuffer[ lIndex ].c.bAttr  = bAttr;
         s_curGT->pLines[ iRow ] = TRUE;
         s_curGT->fRefresh = TRUE;
         return TRUE;
      }
   }
   return FALSE;
}

static void hb_gt_def_PutText( int iRow, int iCol, BYTE bColor, BYTE * pText, ULONG ulLen )
{
   while( ulLen-- )
   {
      if( !hb_gt_PutChar( iRow, iCol, bColor, 0, *pText++ ) )
         break;
      ++iCol;
   }
}

static void hb_gt_def_Replicate( int iRow, int iCol, BYTE bColor, BYTE bAttr,
                                 USHORT usChar, ULONG ulLen )
{
   if( iCol < 0 )
   {
      if( ulLen < ( ULONG ) -iCol )
         ulLen = 0;
      else
         ulLen += iCol;
      iCol = 0;
   }
   while( ulLen-- )
   {
      if( !hb_gt_PutChar( iRow, iCol, bColor, bAttr, usChar ) )
         break;
      ++iCol;
   }
}

static void hb_gt_def_WriteAt( int iRow, int iCol, BYTE * pText, ULONG ulLength )
{
   int iMaxCol = hb_gt_MaxCol();

   /* Truncate the text if the cursor will end up off the right edge */
   hb_gt_PutText( iRow, iCol, ( BYTE ) hb_gt_GetColor(), pText,
                  HB_MIN( ulLength, ( ULONG ) ( iMaxCol - iCol + 1 ) ) );

   /* Finally, save the new cursor position, even if off-screen */
   hb_gt_SetPos( iRow, iCol + ( int ) ulLength );
}

static void hb_gt_def_Write( BYTE * pText, ULONG ulLength )
{
   int iRow, iCol;

   hb_gt_GetPos( &iRow, &iCol );
   hb_gt_WriteAt( iRow, iCol, pText, ulLength );
}

#define WRITECON_BUFFER_SIZE 512

static void hb_gt_def_WriteCon( BYTE * pText, ULONG ulLength )
{
   int iLen = 0;
   BOOL bDisp = FALSE;
   BOOL bBell = FALSE;
   BOOL bNewLine = FALSE;
   int iRow, iCol, iMaxRow, iMaxCol;
   BYTE szString[ WRITECON_BUFFER_SIZE ];

   iMaxRow = hb_gt_MaxRow();
   iMaxCol = hb_gt_MaxCol();

   hb_gt_GetPos( &iRow, &iCol );

   /* Limit the starting cursor position to maxrow(),maxcol()
      on the high end, but don't limit it on the low end. */

   if( iRow > iMaxRow || iCol > iMaxCol )
   {
      if( iRow > iMaxRow )
         iRow = iMaxRow;
      if( iCol > iMaxCol )
         iCol = iMaxCol;
      hb_gt_SetPos( iRow, iCol );
   }

   while( ulLength-- )
   {
      BYTE ch = *pText++;

      switch( ch )
      {
         case HB_CHAR_BEL:
            bDisp = bBell = TRUE;
            break;

         case HB_CHAR_BS:
            if( iCol > 0 )
            {
               --iCol;
               bDisp = TRUE;
            }
            else if( iCol == 0 && iRow > 0 )
            {
               iCol = iMaxCol;
               --iRow;
               bDisp = TRUE;
            }
            if( bDisp )
            {
               if( iLen )
                  szString[ iLen - 1 ] = ' ';
               else
               {
                  hb_gt_SetPos( iRow, iCol );
                  szString[ iLen++ ] = ' ';
               }
            }
            break;

         case HB_CHAR_LF:
            iCol = 0;
            if( iRow >= 0 ) ++iRow;
            bDisp = TRUE;
            bNewLine = TRUE;
            break;

         case HB_CHAR_CR:
            iCol = 0;
            if( *pText == HB_CHAR_LF )
            {
               if( iRow >= 0 ) ++iRow;
               bNewLine = TRUE;
               ++pText;
               --ulLength;
            }
            bDisp = TRUE;
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
               bDisp = TRUE;
               bNewLine = TRUE;
            }
            else
               szString[ iLen++ ] = ch;

            /* Special handling for a really wide screen or device */
            if( iLen >= WRITECON_BUFFER_SIZE ) bDisp = TRUE;
      }

      if( bDisp || ulLength == 0 )
      {
         if( iLen )
            hb_gt_Write( szString, iLen );

         iLen = 0;
         if( iRow > iMaxRow )
         {
            /* Normal scroll */
            hb_gt_Scroll( 0, 0, iMaxRow, iMaxCol, ( BYTE ) hb_gt_GetColor(), hb_gt_GetClearChar(), iRow - iMaxRow, 0 );
            iRow = iMaxRow;
            iCol = 0;
         }
         else if( iRow < 0 && bNewLine )
         {
            /* Special case scroll when newline
               and cursor off top edge of display */
            hb_gt_Scroll( 0, 0, iMaxRow, iMaxCol, ( BYTE ) hb_gt_GetColor(), hb_gt_GetClearChar(), 1, 0 );
         }
         hb_gt_SetPos( iRow, iCol );
         bDisp = FALSE;
         bNewLine = FALSE;

         /* To emulate scrolling */
         hb_gt_Flush();

         if( bBell )
         {
            hb_gt_Bell();
            bBell = FALSE;
         }
      }
   }
}

static long hb_gt_def_RectSize( int iTop, int iLeft, int iBottom, int iRight )
{
   int iRows, iCols;

   iRows = iBottom - iTop + 1;
   iCols = iRight - iLeft + 1;

   if( iCols <= 0 || iRows <= 0 )
      return 0;
   else
      return ( ( long ) iRows * iCols ) << ( s_fVgaCell ? 1 : 2 );
}

static void hb_gt_def_Save( int iTop, int iLeft, int iBottom, int iRight,
                            BYTE * pBuffer )
{
   while( iTop <= iBottom )
   {
      BYTE bColor, bAttr;
      USHORT usChar;
      int iCol;

      for( iCol = iLeft; iCol <= iRight; ++iCol )
      {
         if( !hb_gt_GetChar( iTop, iCol, &bColor, &bAttr, &usChar ) )
         {
            usChar = hb_gt_GetClearChar();
            bColor = hb_gt_GetClearColor();
            bAttr  = 0x00;
         }
         if( s_fVgaCell )
         {
            *pBuffer++ = ( BYTE ) usChar;
            *pBuffer++ = bColor;
         }
         else
         {
            HB_PUT_LE_UINT16( pBuffer, usChar );
            pBuffer += 2;
            *pBuffer++ = bColor;
            *pBuffer++ = bAttr;
         }
      }
      ++iTop;
   }
}

static void hb_gt_def_Rest( int iTop, int iLeft, int iBottom, int iRight,
                            BYTE * pBuffer )
{
   while( iTop <= iBottom )
   {
      BYTE bColor, bAttr;
      USHORT usChar;
      int iCol;

      for( iCol = iLeft; iCol <= iRight; ++iCol )
      {
         if( s_fVgaCell )
         {
            usChar = *pBuffer++;
            bColor = *pBuffer++;
            bAttr  = 0;
         }
         else
         {
            usChar = HB_GET_LE_UINT16( pBuffer );
            pBuffer += 2;
            bColor = *pBuffer++;
            bAttr  = *pBuffer++;
         }
         hb_gt_PutChar( iTop, iCol, bColor, bAttr, usChar );
      }
      ++iTop;
   }
}

static void hb_gt_def_SetAttribute( int iTop, int iLeft, int iBottom, int iRight,
                                    BYTE bColor )
{
   while( iTop <= iBottom )
   {
      BYTE bColorOld, bAttr;
      USHORT usChar;
      int iCol;

      for( iCol = iLeft; iCol <= iRight; ++iCol )
      {
         if( !hb_gt_GetChar( iTop, iCol, &bColorOld, &bAttr, &usChar ) )
            break;
         if( !hb_gt_PutChar( iTop, iCol, bColor, bAttr, usChar ) )
            break;
      }
      ++iTop;
   }
}

static void hb_gt_def_DrawShadow( int iTop, int iLeft, int iBottom, int iRight,
                                  BYTE bColor )
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

   iMaxRow = hb_gt_MaxRow();
   iMaxCol = hb_gt_MaxCol();

   /* Draw the bottom edge */
   if( iBottom <= iMaxRow && iLeft <= iMaxCol )
      hb_gt_SetAttribute( iBottom, iLeft, iBottom, HB_MIN( iRight, iMaxCol ), bColor );

   ++iRight;
   ++iTop;

   /* Draw the right edge */
   if( iTop <= iMaxRow && iRight <= iMaxCol )
      hb_gt_SetAttribute( iTop, iRight, iBottom, HB_MIN( iRight + 1, iMaxCol ), bColor );
}

static void hb_gt_def_Scroll( int iTop, int iLeft, int iBottom, int iRight,
                              BYTE bColor, BYTE bChar, int iRows, int iCols )
{
   int iColOld, iColNew, iColSize, iColClear, iClrs, iLength;

   iColSize = iRight - iLeft;
   iLength = iColSize + 1;
   iColOld = iColNew = iLeft;
   if ( iCols >= 0 )
   {
      iColOld += iCols;
      iColSize -= iCols;
      iColClear = iColOld + iColSize + 1;
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
      BYTE * pBuffer = NULL;

      if( ( iRows || iCols ) && iColSize >= 0 && ( iBottom - iTop >= iRows ) )
      {
         ULONG ulSize = hb_gt_RectSize( iTop, iColOld, iTop, iColOld + iColSize );

         if( ulSize )
            pBuffer = ( BYTE * ) hb_xgrab( ulSize );
      }

      while( iTop <= iBottom )
      {
         int iRowPos;

         if( iRows >= 0 )
            iRowPos = iTop++;
         else
            iRowPos = iBottom--;

         if( pBuffer && iRowPos + iRows >= iTop && iRowPos + iRows <= iBottom )
         {
            hb_gt_Save( iRowPos + iRows, iColOld, iRowPos + iRows, iColOld + iColSize, pBuffer );
            hb_gt_Rest( iRowPos, iColNew, iRowPos, iColNew + iColSize, pBuffer );
            if( iClrs )
               hb_gt_Replicate( iRowPos, iColClear, bColor, 0, bChar, iClrs );
         }
         else
            hb_gt_Replicate( iRowPos, iLeft, bColor, 0, bChar, iLength );
      }

      if( pBuffer )
         hb_xfree( pBuffer );
   }
}

static void hb_gt_def_ScrollArea( int iTop, int iLeft, int iBottom, int iRight,
                                  BYTE bColor, BYTE bChar, int iRows, int iCols )
{
   if( s_curGT && ( iRows || iCols ) )
   {
      int iColOld, iColNew, iColSize, iColClear, iClrs, iLength, iHeight, iWidth;

      hb_gt_GetSize( &iHeight, &iWidth );
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
      iColOld = iColNew = iLeft;

      if ( iCols >= 0 )
      {
         iColOld += iCols;
         iColSize -= iCols;
         iColClear = iColOld + iColSize + 1;
         iClrs = iCols;
      }
      else
      {
         iColNew -= iCols;
         iColSize += iCols;
         iColClear = iColOld;
         iClrs = -iCols;
      }

      if( iLength > 0 )
      {
         long lIndex, lOffset = ( long ) iRows * iWidth + iCols;
         BOOL fMove = ( iRows || iCols ) && iColSize >= 0 &&
                      ( iBottom - iTop >= iRows );

         while( iTop <= iBottom )
         {
            int iRowPos, i;

            if( iRows >= 0 )
               iRowPos = iTop++;
            else
               iRowPos = iBottom--;

            if( fMove && iRowPos + iRows >= iTop && iRowPos + iRows <= iBottom )
            {
               lIndex = ( long ) iRowPos * iWidth + iColNew;
               if( lOffset < 0 )
               {
                  for( i = 0; i <= iColSize; ++i, ++lIndex )
                  {
                     s_curGT->screenBuffer[ lIndex ].uiValue =
                        s_curGT->screenBuffer[ lIndex + lOffset ].uiValue;
                     s_curGT->prevBuffer[ lIndex ].uiValue =
                        s_curGT->prevBuffer[ lIndex + lOffset ].uiValue;
                  }
               }
               else
               {
                  for( i = iColSize, lIndex += iColSize; i >= 0; --i, --lIndex )
                  {
                     s_curGT->screenBuffer[ lIndex ].uiValue =
                        s_curGT->screenBuffer[ lIndex + lOffset ].uiValue;
                     s_curGT->prevBuffer[ lIndex ].uiValue =
                        s_curGT->prevBuffer[ lIndex + lOffset ].uiValue;
                  }
               }
               if( iClrs )
                  hb_gt_Replicate( iRowPos, iColClear, bColor, 0, bChar, iClrs );
            }
            else
               hb_gt_Replicate( iRowPos, iLeft, bColor, 0, bChar, iLength );
         }
      }
   }
}

static void hb_gt_def_ScrollUp( int iRows, BYTE bColor, BYTE bChar )
{
   if( s_curGT && iRows > 0 )
   {
      int i, j, iHeight, iWidth;
      long lIndex = 0, lOffset;
      BYTE bAttr = 0;

      hb_gt_GetSize( &iHeight, &iWidth );
      lOffset = ( long ) iRows * iWidth;
      for( i = iRows; i < iHeight; ++i )
      {
         s_curGT->pLines[ i - iRows ] = s_curGT->pLines[ i ];
         for( j = 0; j < iWidth; ++j )
         {
            s_curGT->screenBuffer[ lIndex ].uiValue =
               s_curGT->screenBuffer[ lIndex + lOffset ].uiValue;
            s_curGT->prevBuffer[ lIndex ].uiValue =
               s_curGT->prevBuffer[ lIndex + lOffset ].uiValue;
            ++lIndex;
         }
      }
      for( i = HB_MAX( 0, iHeight - iRows ); i < iHeight; ++i )
      {
         for( j = 0; j < iWidth; ++j )
         {
            s_curGT->screenBuffer[ lIndex ].c.usChar = bChar;
            s_curGT->screenBuffer[ lIndex ].c.bColor = bColor;
            s_curGT->screenBuffer[ lIndex ].c.bAttr  = bAttr;
            ++lIndex;
         }
         s_curGT->pLines[ i ] = TRUE;
      }
      s_curGT->fRefresh = TRUE;
   }
}

static void hb_gt_def_Box( int iTop, int iLeft, int iBottom, int iRight,
                           BYTE * pbyFrame, BYTE bColor )
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
   iMaxRow = hb_gt_MaxRow(), iMaxCol = hb_gt_MaxCol();

   if( iTop <= iMaxRow && iLeft <= iMaxCol && iBottom >= 0 && iRight >= 0 )
   {
      BYTE szBox[ 10 ];
      BYTE bPadCh = hb_gt_GetClearChar();

      if( pbyFrame )
      {
         for( i = 0; *pbyFrame && i < 9; ++i )
            bPadCh = szBox[ i ] = *pbyFrame++;
      }
      else
         i = 0;

      while( i < 8 )
         szBox[ i++ ] = bPadCh;
      szBox[ i ] = '\0';

      if( iTop == iBottom )
         hb_gt_HorizLine( iTop, iLeft, iRight, szBox[ 1 ], bColor );
      else if( iLeft == iRight )
         hb_gt_VertLine( iLeft, iTop, iBottom, szBox[ 3 ], bColor );
      else
      {
         BYTE bAttr = HB_GT_ATTR_BOX;
         iRows = ( iBottom > iMaxRow ? iMaxRow + 1 : iBottom ) -
                 ( iTop < 0 ? -1 : iTop ) - 1;
         iCols = ( iRight > iMaxCol ? iMaxCol + 1 : iRight ) -
                 ( iLeft < 0 ? -1 : iLeft ) - 1;
         iFirst = iLeft < 0 ? 0 : iLeft + 1;

         if( iTop >= 0 )
         {
            if( iLeft >= 0 )
               hb_gt_PutChar( iTop, iLeft, bColor, bAttr, szBox[ 0 ] );
            if( iCols )
               hb_gt_Replicate( iTop, iFirst, bColor, bAttr, szBox[ 1 ], iCols );
            if( iRight <= iMaxCol )
               hb_gt_PutChar( iTop, iFirst + iCols, bColor, bAttr, szBox[ 2 ] );
            iTop++;
         }
         else
            iTop = 0;
         for( i = 0; i < iRows; ++i )
         {
            if( iLeft >= 0 )
               hb_gt_PutChar( iTop + i, iLeft, bColor, bAttr, szBox[ 7 ] );
            if( iCols && szBox[ 8 ] )
               hb_gt_Replicate( iTop + i, iFirst, bColor, bAttr, szBox[ 8 ], iCols );
            if( iRight <= iMaxCol )
               hb_gt_PutChar( iTop + i, iFirst + iCols, bColor, bAttr, szBox[ 3 ] );
         }
         if( iBottom <= iMaxRow )
         {
            if( iLeft >= 0 )
               hb_gt_PutChar( iBottom, iLeft, bColor, bAttr, szBox[ 6 ] );
            if( iCols )
               hb_gt_Replicate( iBottom, iFirst, bColor, bAttr, szBox[ 5 ], iCols );
            if( iRight <= iMaxCol )
               hb_gt_PutChar( iBottom, iFirst + iCols, bColor, bAttr, szBox[ 4 ] );
         }
      }
   }
}

static void hb_gt_def_BoxS( int iTop, int iLeft, int iBottom, int iRight,
                            BYTE * pbyFrame, BYTE bColor )
{
   hb_gt_Box( iTop, iLeft, iBottom, iRight,
              pbyFrame ? pbyFrame : ( BYTE * ) _B_SINGLE, bColor );
}

static void hb_gt_def_BoxD( int iTop, int iLeft, int iBottom, int iRight,
                            BYTE * pbyFrame, BYTE bColor )
{
   hb_gt_Box( iTop, iLeft, iBottom, iRight,
              pbyFrame ? pbyFrame : ( BYTE * ) _B_DOUBLE, bColor );
}

static void hb_gt_def_HorizLine( int iRow, int iLeft, int iRight,
                                 BYTE bChar, BYTE bColor )
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

   hb_gt_Replicate( iRow, iCol, bColor, HB_GT_ATTR_BOX, bChar, iLength );
}

static void hb_gt_def_VertLine( int iCol, int iTop, int iBottom,
                                BYTE bChar, BYTE bColor )
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
      if( !hb_gt_PutChar( iRow, iCol, bColor, HB_GT_ATTR_BOX, bChar ) )
         break;
      ++iRow;
   }
}

static BOOL hb_gt_def_SetDispCP( char * pszTermCDP, char * pszHostCDP, BOOL fBox )
{
#ifndef HB_CDP_SUPPORT_OFF
   if( !pszHostCDP )
      pszHostCDP = hb_cdp_page->id;
   if( !pszTermCDP )
      pszTermCDP = pszHostCDP;

   if( pszTermCDP && pszHostCDP )
   {
      s_cdpTerm = hb_cdpFind( pszTermCDP );
      s_cdpHost = hb_cdpFind( pszHostCDP );
      s_fDispTrans = s_cdpTerm && s_cdpHost && s_cdpTerm != s_cdpHost;
      return TRUE;
   }
#else
   HB_SYMBOL_UNUSED( pszTermCDP );
   HB_SYMBOL_UNUSED( pszHostCDP );
#endif
   HB_SYMBOL_UNUSED( fBox );

   return FALSE;
}

static BOOL hb_gt_def_SetKeyCP( char * pszTermCDP, char * pszHostCDP )
{
   HB_SYMBOL_UNUSED( pszTermCDP );
   HB_SYMBOL_UNUSED( pszHostCDP );

   return FALSE;
}

static BOOL hb_gt_def_Info( int iType, PHB_GT_INFO pInfo )
{
   switch ( iType )
   {
      case GTI_ISGRAPHIC:
      case GTI_FULLSCREEN:
      case GTI_KBDSUPPORT:
      case GTI_ISCTWIN:
      case GTI_ISMULTIWIN:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, FALSE );
         break;

      case GTI_INPUTFD:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, s_hStdIn );
         break;

      case GTI_OUTPUTFD:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, s_hStdOut );
         break;

      case GTI_ERRORFD:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, s_hStdErr );
         break;

      case GTI_COMPATBUFFER:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, s_fVgaCell );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_LOGICAL )
            s_fVgaCell = hb_itemGetL( pInfo->pNewVal );
         break;

      case GTI_VIEWMAXWIDTH:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, hb_gt_MaxCol() );
         break;

      case GTI_VIEWMAXHEIGHT:
         pInfo->pResult = hb_itemPutNInt( pInfo->pResult, hb_gt_MaxRow() );
         break;

      case GTI_NEWWIN:  /* clear screen area, set default cursor shape and position */
      {
         /* Clear screen */
         hb_gt_DispBegin();
         hb_gt_Scroll( 0, 0, hb_gt_MaxRow(), hb_gt_MaxCol(), ( BYTE ) hb_gt_GetColor(), hb_gt_GetClearChar(), 0, 0 );
         hb_gt_SetPos( 0, 0 );
         hb_gt_SetCursorStyle( SC_NORMAL );
         hb_gt_DispEnd();
         hb_gt_Flush();
         /* no break; */
      }
      case GTI_GETWIN:  /* save screen buffer, cursor shape and possition */
      {
         int iRow, iCol;
         ULONG ulSize;

         if( !pInfo->pResult )
            pInfo->pResult = hb_itemNew( NULL );
         hb_arrayNew( pInfo->pResult, 8 );
         hb_gt_GetPos( &iRow, &iCol );
         hb_itemPutNI( hb_arrayGetItemPtr( pInfo->pResult, 1 ), iRow );
         hb_itemPutNI( hb_arrayGetItemPtr( pInfo->pResult, 2 ), iCol );
         hb_itemPutNI( hb_arrayGetItemPtr( pInfo->pResult, 3 ), hb_gt_GetCursorStyle() );
         hb_itemPutC( hb_arrayGetItemPtr( pInfo->pResult, 4 ), hb_conSetColor( NULL ) );

         iRow = hb_gt_MaxRow();
         iCol = hb_gt_MaxCol();
         hb_itemPutNI( hb_arrayGetItemPtr( pInfo->pResult, 5 ), iRow );
         hb_itemPutNI( hb_arrayGetItemPtr( pInfo->pResult, 6 ), iCol );

         ulSize = hb_gt_RectSize( 0, 0, iRow, iCol );
         if( ulSize )
         {
            BYTE * pBuffer = ( BYTE * ) hb_xgrab( ulSize + 1 );
            hb_gt_Save( 0, 0, iRow, iCol, pBuffer );
            hb_itemPutCPtr( hb_arrayGetItemPtr( pInfo->pResult, 7 ),
                            ( char * ) pBuffer, ulSize );
         }
         break;
      }
      case GTI_SETWIN:  /* restore screen buffer, cursor shape and possition */
         if( hb_arrayLen( pInfo->pNewVal ) == 8 )
         {
            hb_gt_DispBegin();
            if( hb_arrayGetCLen( pInfo->pNewVal, 7 ) > 0 )
            {
               hb_gt_Rest( 0, 0, hb_arrayGetNI( pInfo->pNewVal, 5 ),
                           hb_arrayGetNI( pInfo->pNewVal, 6 ),
                           ( BYTE * ) hb_arrayGetCPtr( pInfo->pNewVal, 7 ) );
            }
            hb_gt_SetPos( hb_arrayGetNI( pInfo->pNewVal, 1 ),
                          hb_arrayGetNI( pInfo->pNewVal, 2 ) );
            hb_gt_SetCursorStyle( hb_arrayGetNI( pInfo->pNewVal, 3 ) );
            hb_conSetColor( hb_arrayGetCPtr( pInfo->pNewVal, 4 ) );
            hb_gt_DispEnd();
            hb_gt_Flush();
         }
         break;

      default:
         return FALSE;
   }

   return TRUE;
}

static int hb_gt_def_Alert( PHB_ITEM pMessage, PHB_ITEM pOptions,
                            int iClrNorm, int iClrHigh, double dDelay )
{
   int iOptions = ( int ) hb_arrayLen( pOptions );
   int iRet = 0;

   if( HB_IS_STRING( pMessage ) && iOptions > 0 )
   {
      char * szMessage = hb_itemGetCPtr( pMessage );
      ULONG ulLen = hb_itemGetCLen( pMessage );
      BOOL fScreen = FALSE, fKeyBoard = FALSE;
      int iKey = 0, i, iDspCount, iStyle, iRows, iCols,
          iRow, iCol, iTop, iLeft, iBottom, iRight, iMnuCol, iPos, iClr;
      BYTE * pBuffer = NULL;
      HB_GT_INFO gtInfo;

      gtInfo.pNewVal = gtInfo.pResult = NULL;

      hb_gt_Info( GTI_FULLSCREEN, &gtInfo );
      if( gtInfo.pResult )
      {
         fScreen = hb_itemGetL( gtInfo.pResult );
      }
      hb_gt_Info( GTI_KBDSUPPORT, &gtInfo );
      if( gtInfo.pResult )
      {
         fKeyBoard = hb_itemGetL( gtInfo.pResult );
         hb_itemRelease( gtInfo.pResult );
      }
      hb_gt_GetSize( &iRows, &iCols );
      if( iCols <= 4 || iRows <= 4 )
         fScreen = FALSE;

      if( fScreen )
      {
         ULONG ulLines = 0, ulWidth = 0, ulCurrWidth = 0, ul = 0, ulDst = 0,
               ulLast = 0, ulSpace1 = 0, ulSpace2 = 0, ulDefWidth, ulMaxWidth;
         char * szMsgDsp;

         ulMaxWidth = iCols - 4;
         ulDefWidth = ( ulMaxWidth * 3 ) >> 2;
         if( ulDefWidth == 0 )
            ulDefWidth = 1;
         szMsgDsp = ( char * ) hb_xgrab( ulLen + ( ulLen / ulDefWidth ) + 1 );

         while( ul < ulLen )
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
#ifndef HB_C52_STRICT
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
            ulCurrWidth += hb_arrayGetCLen( pOptions, i ) + 4;
         }
         if( ulCurrWidth > ulMaxWidth )
            ulCurrWidth = ulMaxWidth;
         if( ulCurrWidth > ulWidth )
            ulWidth = ulCurrWidth;
         if( ( ULONG ) iRows < ulLines + 4 )
            ulLines = iRows - 4;
         iTop = ( iRows - ulLines - 4 ) >> 1;
         iLeft = ( iCols - ulWidth - 4 ) >> 1;
         iBottom = iTop + ulLines + 3;
         iRight = iLeft + ulWidth + 3;

         if( iClrNorm == 0 )
            iClrNorm = 79;
         if( iClrHigh == 0 )
            iClrHigh = 31;
         iDspCount = hb_gt_DispCount();
         if( iDspCount == 0 )
            hb_gt_DispBegin();
         hb_gt_GetPos( &iRow, &iCol );
         iStyle = hb_gt_GetCursorStyle();
         hb_gt_SetCursorStyle( SC_NONE );
         ulLen = hb_gt_RectSize( iTop, iLeft, iBottom, iRight );
         if( ulLen )
         {
            pBuffer = ( BYTE * ) hb_xgrab( ulLen );
            hb_gt_Save( iTop, iLeft, iBottom, iRight, pBuffer );
         }
         hb_gt_BoxS( iTop, iLeft, iBottom, iRight, NULL, iClrNorm );
         hb_gt_Box( iTop + 1, iLeft + 1, iBottom - 1, iRight - 1, ( BYTE * ) "         ", iClrNorm );
         ulLast = 0;
         i = iTop + 1;
         for( ul = 0; ul < ulDst; ++ul )
         {
            if( szMsgDsp[ ul ] == '\n' )
            {
               if( ul > ulLast )
               {
                  ulLen = ul - ulLast;
                  if( ulLen > ulWidth )
                     ulLen = ulWidth;
                  hb_gt_PutText( i, iLeft + ( ( ulWidth - ulLen + 1 ) >> 1 ) + 2,
                                 iClrNorm, ( BYTE * ) szMsgDsp + ulLast, ulLen );
               }
               ulLast = ul + 1;
               if( ++i >= iBottom - 1 )
                  break;
            }
         }
         if( ul > ulLast && i < iBottom - 1 )
         {
            ulLen = ul - ulLast;
            if( ulLen > ulWidth )
               ulLen = ulWidth;
            hb_gt_PutText( i, iLeft + ( ( ulWidth - ulLen + 1 ) >> 1 ) + 2,
                           iClrNorm, ( BYTE * ) szMsgDsp + ulLast, ulLen );
         }

         iPos = 1;
         while( iRet == 0 )
         {
            hb_gt_DispBegin();
            iMnuCol = iLeft + ( ( ulWidth - ulCurrWidth ) >> 1 ) + 3;
            for( i = 1; i <= iOptions; ++i )
            {
               iClr = i == iPos ? iClrHigh : iClrNorm;
               ulLen = hb_arrayGetCLen( pOptions, i );
               hb_gt_PutText( iBottom - 1, iMnuCol, iClr, ( BYTE * ) " ", 1 );
               hb_gt_PutText( iBottom - 1, iMnuCol + 1, iClr,
                              ( BYTE * ) hb_arrayGetCPtr( pOptions, i ), ulLen );
               hb_gt_PutText( iBottom - 1, iMnuCol + 1 + ulLen, iClr, ( BYTE * ) " ", 1 );
               iMnuCol += ulLen + 4;
            }
            while( hb_gt_DispCount() )
               hb_gt_DispEnd();
            hb_gt_Refresh();

            iKey = fKeyBoard ? hb_inkey( TRUE, dDelay, INKEY_ALL ) : 0;

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
               hb_mouse_GetPos( &iMRow, &iMCol );
               if( iMRow == iBottom - 1 )
               {
                  iMnuCol = iLeft + ( ( ulWidth - ulCurrWidth ) >> 1 ) + 4;
                  for( i = 1; i <= iOptions; ++i )
                  {
                     ulLen = hb_arrayGetCLen( pOptions, i );
                     if( iMCol >= iMnuCol && iMCol < iMnuCol + ( int ) ulLen )
                     {
                        iRet = i;
                        break;
                     }
                     iMnuCol += ulLen + 4;
                  }
               }
            }
#endif
            else if( iKey >= 32 && iKey <= 255 )
            {
               int iUp = hb_charUpper( iKey );
               for( i = 1; i <= iOptions; ++i )
               {
                  char *szValue = hb_arrayGetCPtr( pOptions, i );
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
            hb_gt_Rest( iTop, iLeft, iBottom, iRight, pBuffer );
            hb_xfree( pBuffer );
         }
         hb_gt_SetPos( iRow, iCol );
         hb_gt_SetCursorStyle( iStyle );
         hb_gt_Refresh();
         while( hb_gt_DispCount() < iDspCount )
            hb_gt_DispBegin();
      }
      else
      {
         ULONG ul, ulStart = 0;
         char *szEol = hb_conNewLine();

         for( ul = 0; ul < ulLen; ++ul )
         {
            if( szMessage[ ul ] == '\n' )
            {
               if( ul > ulStart )
                  hb_gt_WriteCon( ( BYTE * ) szMessage + ulStart, ul - ulStart );
               hb_gt_WriteCon( ( BYTE * ) szEol, strlen( szEol ) );
               ulStart = ul + 1;
            }
         }
         if( ul > ulStart )
            hb_gt_WriteCon( ( BYTE * ) szMessage + ulStart, ul - ulStart );
         hb_gt_WriteCon( ( BYTE * ) " (", 2 );
         for( i = 1; i <= iOptions; ++i )
         {
            if( i > 1 )
               hb_gt_WriteCon( ( BYTE * ) ", ", 2 );
            hb_gt_WriteCon( ( BYTE * ) hb_arrayGetCPtr( pOptions, i ),
                            hb_arrayGetCLen( pOptions, i ) );
         }
         hb_gt_WriteCon( ( BYTE * ) ") ", 2 );
         while( iRet == 0 )
         {
            iKey = fKeyBoard ? hb_inkey( TRUE, dDelay, INKEY_ALL ) : 0;
            if( iKey == 0 )
               iRet = 1;
            else if( iKey == K_ESC )
               break;
            else if( iKey >= 32 && iKey <= 255 )
            {
               int iUp = hb_charUpper( iKey );
               for( i = 1; i <= iOptions; ++i )
               {
                  char *szValue = hb_arrayGetCPtr( pOptions, i );
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
            hb_gt_WriteCon( ( BYTE * ) szVal, 1 );
         }
      }
   }

   return iRet;
}

static int hb_gt_def_SetFlag( int iType, int iNewValue )
{
   int iPrevValue = 0;

   switch ( iType )
   {
      case GTI_COMPATBUFFER:
         iPrevValue = s_fVgaCell;
         s_fVgaCell = iNewValue != 0;
         break;

      case GTI_STDOUTCON:
         iPrevValue = s_fStdOutCon;
         s_fStdOutCon = iNewValue != 0;
         break;

      case GTI_STDERRCON:
         iPrevValue = s_fStdErrCon;
         s_fStdErrCon = iNewValue != 0;
         break;
   }

   return iPrevValue;
}

static BOOL hb_gt_def_SetMode( int iRows, int iCols )
{
   return hb_gt_Resize( iRows, iCols );
}

static BOOL hb_gt_def_Resize( int iRows, int iCols )
{
   if( iRows > 0 && iCols > 0 && s_curGT )
   {
      if( s_curGT->iHeight != iRows || s_curGT->iWidth != iCols )
      {
         BYTE * pBuffer = NULL;
         ULONG ulLen = ( ULONG ) iRows * iCols, ulIndex;
         ULONG ulSize;
         int i;

         ulSize = hb_gt_RectSize( 0, 0, iRows - 1, iCols - 1 );
         if( ulSize )
         {
            pBuffer = ( BYTE * ) hb_xgrab( ulSize );
            hb_gt_Save( 0, 0, iRows - 1, iCols - 1, pBuffer );
         }

         s_curGT->screenBuffer =
               ( PHB_SCREENCELL ) hb_xrealloc( s_curGT->screenBuffer,
                                             sizeof( HB_SCREENCELL ) * ulLen );
         s_curGT->prevBuffer =
               ( PHB_SCREENCELL ) hb_xrealloc( s_curGT->prevBuffer,
                                             sizeof( HB_SCREENCELL ) * ulLen );
         s_curGT->pLines = ( BOOL * ) hb_xrealloc( s_curGT->pLines,
                                             sizeof( BOOL ) * iRows );

         memset( s_curGT->screenBuffer, 0, sizeof( HB_SCREENCELL ) * ulLen );
         memset( s_curGT->prevBuffer, 0, sizeof( HB_SCREENCELL ) * ulLen );
         for( i = 0; i < iRows; ++i )
            s_curGT->pLines[ i ] = TRUE;
         for( ulIndex = 0; ulIndex < ulLen; ++ulIndex )
         {
            s_curGT->screenBuffer[ ulIndex ].c.usChar = hb_gt_GetClearChar();
            s_curGT->screenBuffer[ ulIndex ].c.bColor = hb_gt_GetClearColor();
            s_curGT->screenBuffer[ ulIndex ].c.bAttr  = 0x00;
            s_curGT->prevBuffer[ ulIndex ].c.bAttr = HB_GT_ATTR_REFRESH;
         }

         s_curGT->iHeight = iRows;
         s_curGT->iWidth = iCols;

         if( s_curGT->iRow >= s_curGT->iHeight )
            s_curGT->iRow = s_curGT->iHeight - 1;
         if( s_curGT->iCol >= s_curGT->iWidth )
            s_curGT->iCol = s_curGT->iWidth - 1;

         s_curGT->fRefresh = TRUE;

         if( ulSize )
         {
            hb_gt_Rest( 0, 0, iRows - 1, iCols - 1, pBuffer );
            hb_xfree( pBuffer );
         }
      }

      return TRUE;
   }

   return FALSE;
}

static void hb_gt_def_GetSize( int * piRows, int  * piCols )
{
   if( s_curGT )
   {
      *piRows = s_curGT->iHeight;
      *piCols = s_curGT->iWidth;
   }
   else
   {
      *piRows = 25;
      *piCols = 80;
   }
}

void hb_gt_def_SemiCold( void )
{
   if( s_curGT )
   {
      int i;
      for( i = 0; i < s_curGT->iHeight; ++i )
         s_curGT->pLines[ i ]  = FALSE;
      s_curGT->fRefresh = FALSE;
   }
}

static void hb_gt_def_ColdArea( int iTop, int iLeft, int iBottom, int iRight )
{
   if( s_curGT )
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
            if( hb_gt_CheckPos( iTop, i, &lIndex ) )
            {
               s_curGT->prevBuffer[ lIndex ].uiValue =
                  ( s_curGT->screenBuffer[ lIndex ].uiValue &=
                                                   ~HB_GT_ATTR_REFRESH );
            }
         }
         if( iLeft == 0 && iRight == s_curGT->iWidth - 1 )
            s_curGT->pLines[ iTop ] = FALSE;
         ++iTop;
      }
   }
}

static void hb_gt_def_ExposeArea( int iTop, int iLeft, int iBottom, int iRight )
{
   if( s_curGT )
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
            if( hb_gt_CheckPos( iTop, i, &lIndex ) )
            {
               s_curGT->prevBuffer[ lIndex ].c.bAttr = HB_GT_ATTR_REFRESH;
               s_curGT->pLines[ iTop ] = TRUE;
               s_curGT->fRefresh = TRUE;
            }
         }
         ++iTop;
      }
   }
}

static void hb_gt_def_TouchCell( int iRow, int iCol )
{
   if( s_curGT )
   {
      long lIndex;

      if( hb_gt_CheckPos( iRow, iCol, &lIndex ) )
      {
         s_curGT->prevBuffer[ lIndex ].c.bAttr = HB_GT_ATTR_REFRESH;
         s_curGT->pLines[ iRow ] = TRUE;
         s_curGT->fRefresh = TRUE;
      }
   }
}

static void hb_gt_def_Redraw( int iRow, int iCol, int iSize )
{
   HB_SYMBOL_UNUSED( iRow );
   HB_SYMBOL_UNUSED( iCol );
   HB_SYMBOL_UNUSED( iSize );
}

static void hb_gt_def_Refresh( void )
{
   if( s_curGT && s_curGT->fRefresh )
   {
      int i, l, r;
      long lIndex;

      for( i = 0; i < s_curGT->iHeight; ++i )
      {
         if( s_curGT->pLines[ i ] )
         {
            lIndex = ( long ) i * s_curGT->iWidth;
            for( l = 0; l < s_curGT->iWidth; ++l, ++lIndex )
            {
               if( s_curGT->prevBuffer[ lIndex ].uiValue !=
                   s_curGT->screenBuffer[ lIndex ].uiValue )
                  break;
            }
            if( l < s_curGT->iWidth )
            {
               lIndex = ( long ) ( i + 1 ) * s_curGT->iWidth - 1;
               for( r = s_curGT->iWidth - 1; r > l; --r, --lIndex )
               {
                  if( s_curGT->prevBuffer[ lIndex ].uiValue !=
                      s_curGT->screenBuffer[ lIndex ].uiValue )
                     break;
               }
               hb_gt_Redraw( i, l, r - l + 1 );
               lIndex = ( long ) i * s_curGT->iWidth + l;
               do
               {
                  s_curGT->prevBuffer[ lIndex ].uiValue =
                     s_curGT->screenBuffer[ lIndex ].uiValue;
                  ++lIndex;
               }
               while( ++l <= r );
            }
            s_curGT->pLines[ i ] = FALSE;
         }
      }
      s_curGT->fRefresh = FALSE;
   }
}

static void hb_gt_def_Flush( void )
{
   if( hb_gt_DispCount() == 0 )
      hb_gt_Refresh();
}

static int hb_gt_def_ReadKey( int iEventMask )
{
   return hb_mouse_ReadKey( iEventMask );
}

static void hb_gt_def_MouseInit( void )
{
   ;
}

static void hb_gt_def_MouseExit( void )
{
   ;
}

static BOOL hb_gt_def_MouseIsPresent( void )
{
   return FALSE;
}

static void hb_gt_def_MouseShow( void )
{
   ;
}

static void hb_gt_def_MouseHide( void )
{
   ;
}

static BOOL hb_gt_def_MouseGetCursor( void )
{
   return s_fMouseVisible;
}

static void hb_gt_def_MouseSetCursor( BOOL fVisible )
{
   if( fVisible )
   {
      hb_mouse_Show();
      s_fMouseVisible = TRUE;
   }
   else if( s_fMouseVisible )
   {
      /*
       * mouse drivers use hide counters, so repeated calls to
       * hb_mouse_Hide() will need at least the same number of calls to
       * hb_mouse_Show() to make mouse cursor visible. This behavior
       * is not compatible with Clipper so call to hb_mouse_Hide() is
       * guarded by s_fMouseVisible. The counter is not updated when
       * mouse cursor is visible and hb_mouse_Show() is called so this
       * behavior is enough. If some platform works in differ way then
       * and this behavior will create problems GT driver should overload
       * hb_mouse_SetCursor()/hb_mouse_GetCursor() methods. [druzus]
       */
      hb_mouse_Hide();
      s_fMouseVisible = FALSE;
   }
}

static int hb_gt_def_MouseRow( void )
{
   int iRow, iCol;

   hb_mouse_GetPos( &iRow, &iCol );
   return iRow;
}

static int hb_gt_def_MouseCol( void )
{
   int iRow, iCol;

   hb_mouse_GetPos( &iRow, &iCol );
   return iCol;
}

static void hb_gt_def_MouseGetPos( int * piRow, int * piCol )
{
   *piRow = *piCol = 0;
}

static void hb_gt_def_MouseSetPos( int iRow, int iCol )
{
   HB_SYMBOL_UNUSED( iRow );
   HB_SYMBOL_UNUSED( iCol );
}

static void hb_gt_def_MouseSetBounds( int iTop, int iLeft, int iBottom, int iRight )
{
   HB_SYMBOL_UNUSED( iTop );
   HB_SYMBOL_UNUSED( iLeft );
   HB_SYMBOL_UNUSED( iBottom );
   HB_SYMBOL_UNUSED( iRight );
}

static void hb_gt_def_MouseGetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight )
{
   *piTop = *piLeft = 0;
   hb_gt_GetSize( piBottom, piRight );
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

static int  hb_gt_def_mouseStorageSize( void )
{
   return sizeof( _HB_MOUSE_STORAGE );
}

static void hb_gt_def_mouseSaveState( BYTE * pBuffer )
{
   _HB_MOUSE_STORAGE * pStore = ( _HB_MOUSE_STORAGE * ) pBuffer;
   int iRow, iCol, iTop, iLeft, iBottom, iRight;

   hb_mouse_GetPos( &iRow, &iCol );
   hb_mouse_GetBounds( &iTop, &iLeft, &iBottom, &iRight );

   pStore->iRow      = iRow;
   pStore->iCol      = iCol;
   pStore->fVisible  = hb_mouse_GetCursor();
   pStore->iTop      = iTop;
   pStore->iLeft     = iLeft;
   pStore->iBottom   = iBottom;
   pStore->iRight    = iRight;
}

static void hb_gt_def_mouseRestoreState( BYTE * pBuffer )
{
   _HB_MOUSE_STORAGE * pStore = ( _HB_MOUSE_STORAGE * ) pBuffer;

   hb_mouse_SetBounds( pStore->iTop, pStore->iLeft, pStore->iBottom, pStore->iRight );
   hb_mouse_SetPos( pStore->iRow, pStore->iCol );
   hb_mouse_SetCursor( pStore->fVisible );
}

static int  hb_gt_def_mouseGetDoubleClickSpeed( void )
{
   return s_iDoubleClickSpeed;
}

static void hb_gt_def_mouseSetDoubleClickSpeed( int iSpeed )
{
   if( iSpeed > 0 )
      s_iDoubleClickSpeed = iSpeed;
}

static int hb_gt_def_MouseCountButton( void )
{
   return 0;
}

static BOOL hb_gt_def_MouseButtonState( int iButton )
{
   HB_SYMBOL_UNUSED( iButton );

   return FALSE;
}

static BOOL hb_gt_def_MouseButtonPressed( int iButton, int * piRow, int * piCol )
{
   HB_SYMBOL_UNUSED( iButton );
   HB_SYMBOL_UNUSED( piRow );
   HB_SYMBOL_UNUSED( piCol );

   return FALSE;
}

static BOOL hb_gt_def_MouseButtonReleased( int iButton, int * piRow, int * piCol )
{
   HB_SYMBOL_UNUSED( iButton );
   HB_SYMBOL_UNUSED( piRow );
   HB_SYMBOL_UNUSED( piCol );

   return FALSE;
}

static int hb_gt_def_MouseReadKey( int iEventMask )
{
   int iKey = 0, iRow, iCol;

   if( hb_mouse_IsPresent() )
   {
      if( iEventMask & INKEY_LDOWN && hb_mouse_ButtonPressed( 0, &iRow, &iCol ) )
      {
         HB_ULONG timer = hb_dateMilliSeconds();
         if( timer - s_iMouseLeftTimer <= ( HB_ULONG ) hb_mouse_GetDoubleClickSpeed() )
            iKey = K_LDBLCLK;
         else
            iKey = K_LBUTTONDOWN;
         s_iMouseLeftTimer = timer;
      }
      else if( iEventMask & INKEY_LUP && hb_mouse_ButtonReleased( 0, &iRow, &iCol ) )
      {
         iKey = K_LBUTTONUP;
      }
      else if( iEventMask & INKEY_RDOWN && hb_mouse_ButtonPressed( 1, &iRow, &iCol ) )
      {
         HB_ULONG timer = hb_dateMilliSeconds();
         if( timer - s_iMouseRightTimer <= ( HB_ULONG ) hb_mouse_GetDoubleClickSpeed() )
            iKey = K_RDBLCLK;
         else
            iKey = K_RBUTTONDOWN;
         s_iMouseRightTimer = timer;
      }
      else if( iEventMask & INKEY_RUP && hb_mouse_ButtonReleased( 1, &iRow, &iCol ) )
      {
         iKey = K_RBUTTONUP;
      }
      else if( iEventMask & INKEY_MMIDDLE && hb_mouse_ButtonPressed( 2, &iRow, &iCol ) )
      {
         HB_ULONG timer = hb_dateMilliSeconds();
         if( timer - s_iMouseMiddleTimer <= ( HB_ULONG ) hb_mouse_GetDoubleClickSpeed() )
            iKey = K_MDBLCLK;
         else
            iKey = K_MBUTTONDOWN;
         s_iMouseMiddleTimer = timer;
      }
      else if( iEventMask & INKEY_MMIDDLE && hb_mouse_ButtonReleased( 2, &iRow, &iCol ) )
      {
         iKey = K_MBUTTONUP;
      }
      else if( iEventMask & INKEY_MOVE )
      {
         hb_mouse_GetPos( &iRow, &iCol );
         if( iRow != s_iMouseLastRow || iCol != s_iMouseLastCol )
         {
            s_iMouseLastRow = iRow;
            s_iMouseLastCol = iCol;
            iKey = K_MOUSEMOVE;
         }
      }
   }
   return iKey;
}

static int hb_gt_def_GfxPrimitive( int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor )
{
   HB_SYMBOL_UNUSED( iType );
   HB_SYMBOL_UNUSED( iTop );
   HB_SYMBOL_UNUSED( iLeft );
   HB_SYMBOL_UNUSED( iBottom );
   HB_SYMBOL_UNUSED( iRight );
   HB_SYMBOL_UNUSED( iColor );

   return 0;
}

static void hb_gt_def_GfxText( int iTop, int iLeft, char * szText, int iColor, int iSize, int iWidth )
{
   HB_SYMBOL_UNUSED( iTop );
   HB_SYMBOL_UNUSED( iLeft );
   HB_SYMBOL_UNUSED( szText );
   HB_SYMBOL_UNUSED( iColor );
   HB_SYMBOL_UNUSED( iSize );
   HB_SYMBOL_UNUSED( iWidth );
}

static void hb_gt_def_WhoCares( void * pCargo )
{
   HB_SYMBOL_UNUSED( pCargo );
}

/* ************************************************************************* */

#if defined( __GNUC__ ) && 0
static HB_GT_FUNCS gtCoreFunc =
{
   Init                       : hb_gt_def_Init                          ,
   Exit                       : hb_gt_def_Exit                          ,
   New                        : hb_gt_def_New                           ,
   Free                       : hb_gt_def_Free                          ,
   Resize                     : hb_gt_def_Resize                        ,
   SetMode                    : hb_gt_def_SetMode                       ,
   GetSize                    : hb_gt_def_GetSize                       ,
   SemiCold                   : hb_gt_def_SemiCold                      ,
   ColdArea                   : hb_gt_def_ColdArea                      ,
   ExposeArea                 : hb_gt_def_ExposeArea                    ,
   ScrollArea                 : hb_gt_def_ScrollArea                    ,
   TouchCell                  : hb_gt_def_TouchCell                     ,
   Redraw                     : hb_gt_def_Redraw                        ,
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
static HB_GT_FUNCS gtCoreFunc =
{
   hb_gt_def_Init                         ,
   hb_gt_def_Exit                         ,
   hb_gt_def_New                          ,
   hb_gt_def_Free                         ,
   hb_gt_def_Resize                       ,
   hb_gt_def_SetMode                      ,
   hb_gt_def_GetSize                      ,
   hb_gt_def_SemiCold                     ,
   hb_gt_def_ColdArea                     ,
   hb_gt_def_ExposeArea                   ,
   hb_gt_def_ScrollArea                   ,
   hb_gt_def_TouchCell                    ,
   hb_gt_def_Redraw                       ,
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

void * hb_gt_New( void )
{
   return gtCoreFunc.New();
}

void   hb_gt_Free( void * pGtPtr )
{
   gtCoreFunc.Free( pGtPtr );
}

void   hb_gt_Init( FHANDLE hStdIn, FHANDLE hStdOut, FHANDLE hStdErr )
{
   gtCoreFunc.Init( hStdIn, hStdOut, hStdErr );
}

void   hb_gt_Exit( void )
{
   gtCoreFunc.Exit();
}

BOOL   hb_gt_CheckPos( int iRow, int iCol, long *plIndex )
{
   return gtCoreFunc.CheckPos( iRow, iCol, plIndex );
}

void   hb_gt_GetPos( int * piRow, int * piCol )
{
   gtCoreFunc.GetPos( piRow, piCol );
}

void   hb_gt_SetPos( int iRow, int iCol )
{
   gtCoreFunc.SetPos( iRow, iCol );
}

int    hb_gt_MaxCol( void )
{
   return gtCoreFunc.MaxCol();
}

int    hb_gt_MaxRow( void )
{
   return gtCoreFunc.MaxRow();
}

BOOL   hb_gt_IsColor( void )
{
   return gtCoreFunc.IsColor();
}

void   hb_gt_GetColorStr( char * pszColorString )
{
   gtCoreFunc.GetColorStr( pszColorString );
}

void   hb_gt_SetColorStr( const char * pszColorString )
{
   gtCoreFunc.SetColorStr( pszColorString );
}

void   hb_gt_ColorSelect( int iColorInddex )
{
   gtCoreFunc.ColorSelect( iColorInddex );
}

int    hb_gt_GetColor( void )
{
   return gtCoreFunc.GetColor();
}

int    hb_gt_ColorNum( const char * pszColorString )
{
   return gtCoreFunc.ColorNum( pszColorString );
}

void   hb_gt_ColorsToString( int * pColors, int iColorCount, char * pszColorString, int iBufSize )
{
   gtCoreFunc.ColorsToString( pColors, iColorCount, pszColorString, iBufSize );
}

void   hb_gt_StringToColors( const char * pszColorString, int ** pColorsPtr, int * piColorCount )
{
   gtCoreFunc.StringToColors( pszColorString, pColorsPtr, piColorCount );
}

void   hb_gt_GetColorData( int ** pColorsPtr, int * piColorCount, int * piColorIndex )
{
   gtCoreFunc.GetColorData( pColorsPtr, piColorCount, piColorIndex );
}

int    hb_gt_GetClearColor( void )
{
   return gtCoreFunc.GetClearColor();
}

void   hb_gt_SetClearColor( int iColor )
{
   gtCoreFunc.SetClearColor( iColor );
}

int    hb_gt_GetClearChar( void )
{
   return gtCoreFunc.GetClearChar();
}

void   hb_gt_SetClearChar( int iChar )
{
   gtCoreFunc.SetClearChar( iChar );
}

int    hb_gt_GetCursorStyle( void )
{
   return gtCoreFunc.GetCursorStyle();
}

void   hb_gt_SetCursorStyle( int iStyle )
{
   gtCoreFunc.SetCursorStyle( iStyle );
}

void   hb_gt_GetScrCursor( int * piRow, int * piCol, int * piStyle )
{
   gtCoreFunc.GetScrCursor( piRow, piCol, piStyle );
}

BOOL   hb_gt_GetBlink( void )
{
   return gtCoreFunc.GetBlink();
}

void   hb_gt_SetBlink( BOOL fBlink )
{
   gtCoreFunc.SetBlink( fBlink );
}

void   hb_gt_SetSnowFlag( BOOL fNoSnow )
{
   gtCoreFunc.SetSnowFlag( fNoSnow );
}

void   hb_gt_DispBegin( void )
{
   gtCoreFunc.DispBegin();
}

void   hb_gt_DispEnd( void )
{
   gtCoreFunc.DispEnd();
}

int    hb_gt_DispCount( void )
{
   return gtCoreFunc.DispCount();
}

BOOL   hb_gt_PreExt()
{
   return gtCoreFunc.PreExt();
}

BOOL   hb_gt_PostExt()
{
   return gtCoreFunc.PostExt();
}

BOOL   hb_gt_Suspend()
{
   return gtCoreFunc.Suspend();
}

BOOL   hb_gt_Resume()
{
   return gtCoreFunc.Resume();
}

char * hb_gt_Version( int iType )
{
   return gtCoreFunc.Version( iType );
}

BOOL   hb_gt_GetScrChar( int iRow, int iCol, BYTE * pbColor, BYTE * pbAttr, USHORT * pusChar )
{
   return gtCoreFunc.GetScrChar( iRow, iCol, pbColor, pbAttr, pusChar );
}

BOOL   hb_gt_GetChar( int iRow, int iCol, BYTE * pbColor, BYTE * pbAttr, USHORT * pusChar )
{
   return gtCoreFunc.GetChar( iRow, iCol, pbColor, pbAttr, pusChar );
}

BOOL   hb_gt_PutScrChar( int iRow, int iCol, BYTE bColor, BYTE bAttr, USHORT usChar )
{
   return gtCoreFunc.PutScrChar( iRow, iCol, bColor, bAttr, usChar );
}

BOOL   hb_gt_PutChar( int iRow, int iCol, BYTE bColor, BYTE bAttr, USHORT usChar )
{
   return gtCoreFunc.PutChar( iRow, iCol, bColor, bAttr, usChar );
}

void   hb_gt_PutText( int iRow, int iCol, BYTE bColor, BYTE * pText, ULONG ulLen )
{
   gtCoreFunc.PutText( iRow, iCol, bColor, pText, ulLen );
}

void   hb_gt_Replicate( int iRow, int iCol, BYTE bColor, BYTE bAttr, USHORT usChar, ULONG ulLen )
{
   gtCoreFunc.Replicate( iRow, iCol, bColor, bAttr, usChar, ulLen );
}

void   hb_gt_WriteAt( int iRow, int iCol, BYTE * pText, ULONG ulLength )
{
   gtCoreFunc.WriteAt( iRow, iCol, pText, ulLength );
}

void   hb_gt_Write( BYTE * pText, ULONG ulLength )
{
   gtCoreFunc.Write( pText, ulLength );
}

void   hb_gt_WriteCon( BYTE * pText, ULONG ulLength )
{
   gtCoreFunc.WriteCon( pText, ulLength );
}

long   hb_gt_RectSize( int iTop, int iLeft, int iBottom, int iRight )
{
   return gtCoreFunc.RectSize( iTop, iLeft, iBottom, iRight );
}

void   hb_gt_Save( int iTop, int iLeft, int iBottom, int iRight, BYTE * pBuffer )
{
   gtCoreFunc.Save( iTop, iLeft, iBottom, iRight, pBuffer );
}

void   hb_gt_Rest( int iTop, int iLeft, int iBottom, int iRight, BYTE * pBuffer )
{
   gtCoreFunc.Rest( iTop, iLeft, iBottom, iRight, pBuffer );
}

void   hb_gt_SetAttribute( int iTop, int iLeft, int iBottom, int iRight, BYTE bColor )
{
   gtCoreFunc.SetAttribute( iTop, iLeft, iBottom, iRight, bColor );
}

void   hb_gt_DrawShadow( int iTop, int iLeft, int iBottom, int iRight, BYTE bColor )
{
   gtCoreFunc.DrawShadow( iTop, iLeft, iBottom, iRight, bColor );
}

void   hb_gt_Scroll( int iTop, int iLeft, int iBottom, int iRight, BYTE bColor, BYTE bChar, int iRows, int iCols )
{
   gtCoreFunc.Scroll( iTop, iLeft, iBottom, iRight, bColor, bChar, iRows, iCols );
}

void   hb_gt_ScrollUp( int iRows, BYTE bColor, BYTE bChar )
{
   gtCoreFunc.ScrollUp( iRows, bColor, bChar );
}

void   hb_gt_Box( int iTop, int iLeft, int iBottom, int iRight, BYTE * pbyFrame, BYTE bColor )
{
   gtCoreFunc.Box( iTop, iLeft, iBottom, iRight, pbyFrame, bColor );
}

void   hb_gt_BoxS( int iTop, int iLeft, int iBottom, int iRight, BYTE * pbyFrame, BYTE bColor )
{
   gtCoreFunc.BoxS( iTop, iLeft, iBottom, iRight, pbyFrame, bColor );
}

void   hb_gt_BoxD( int iTop, int iLeft, int iBottom, int iRight, BYTE * pbyFrame, BYTE bColor )
{
   gtCoreFunc.BoxD( iTop, iLeft, iBottom, iRight, pbyFrame, bColor );
}

void   hb_gt_HorizLine( int iRow, int iLeft, int iRight, BYTE bChar, BYTE bColor )
{
   gtCoreFunc.HorizLine( iRow, iLeft, iRight, bChar, bColor );
}

void   hb_gt_VertLine( int iCol, int iTop, int iBottom, BYTE bChar, BYTE bColor )
{
   gtCoreFunc.VertLine( iCol, iTop, iBottom, bChar, bColor );
}

BOOL   hb_gt_SetMode( int iRows, int iCols )
{
   return gtCoreFunc.SetMode( iRows, iCols );
}

BOOL   hb_gt_Resize( int iRows, int iCols )
{
   return gtCoreFunc.Resize( iRows, iCols );
}

void   hb_gt_GetSize( int * piRows, int * piCols )
{
   gtCoreFunc.GetSize( piRows, piCols );
}

void   hb_gt_SemiCold( void )
{
   gtCoreFunc.SemiCold();
}

void   hb_gt_ColdArea( int iTop, int iLeft, int iBottom, int iRight )
{
   gtCoreFunc.ColdArea( iTop, iLeft, iBottom, iRight );
}

void   hb_gt_ExposeArea( int iTop, int iLeft, int iBottom, int iRight )
{
   gtCoreFunc.ExposeArea( iTop, iLeft, iBottom, iRight );
}

void   hb_gt_ScrollArea( int iTop, int iLeft, int iBottom, int iRight, BYTE bColor, BYTE bChar, int iRows, int iCols )
{
   gtCoreFunc.ScrollArea( iTop, iLeft, iBottom, iRight, bColor, bChar, iRows, iCols );
}

void   hb_gt_TouchCell( int iRow, int iCol )
{
   gtCoreFunc.TouchCell( iRow, iCol );
}

void   hb_gt_Redraw( int iRow, int iCol, int iSize )
{
   gtCoreFunc.Redraw( iRow, iCol, iSize );
}

void   hb_gt_Refresh( void )
{
   gtCoreFunc.Refresh();
}

void   hb_gt_Flush( void )
{
   gtCoreFunc.Flush();
}

void   hb_gt_Tone( double dFrequency, double dDuration )
{
   gtCoreFunc.Tone( dFrequency, dDuration );
}

void   hb_gt_Bell( void )
{
   gtCoreFunc.Bell();
}

void   hb_gt_OutStd( BYTE * pbyStr, ULONG ulLen )
{
    gtCoreFunc.OutStd( pbyStr, ulLen );
}

void   hb_gt_OutErr( BYTE * pbyStr, ULONG ulLen )
{
    gtCoreFunc.OutErr( pbyStr, ulLen );
}

BOOL   hb_gt_SetDispCP( char * pszTermCDP, char * pszHostCDP, BOOL fBox )
{
   return gtCoreFunc.SetDispCP( pszTermCDP, pszHostCDP, fBox );
}

BOOL   hb_gt_SetKeyCP( char * pszTermCDP, char * pszHostCDP )
{
   return gtCoreFunc.SetKeyCP( pszTermCDP, pszHostCDP );
}

BOOL   hb_gt_Info( int iType, PHB_GT_INFO pInfo )
{
   return gtCoreFunc.Info( iType, pInfo );
}

int    hb_gt_Alert( PHB_ITEM pMessage, PHB_ITEM pOptions,
                    int iClrNorm, int iClrHigh, double dDelay )
{
   return gtCoreFunc.Alert( pMessage, pOptions, iClrNorm, iClrHigh, dDelay );
}

int    hb_gt_SetFlag( int iType, int iNewValue )
{
   return gtCoreFunc.SetFlag( iType, iNewValue );
}

int    hb_gt_ReadKey( int iEventMask )
{
   return gtCoreFunc.ReadKey( iEventMask );
}

void   hb_mouse_Init( void )
{
   gtCoreFunc.MouseInit();
}

void   hb_mouse_Exit( void )
{
   gtCoreFunc.MouseExit();
}

BOOL   hb_mouse_IsPresent( void )
{
   return gtCoreFunc.MouseIsPresent();
}

void   hb_mouse_Show( void )
{
   gtCoreFunc.MouseShow();
}

void   hb_mouse_Hide( void )
{
   gtCoreFunc.MouseHide();
}

BOOL   hb_mouse_GetCursor( void )
{
   return gtCoreFunc.MouseGetCursor();
}

void   hb_mouse_SetCursor( BOOL fVisible )
{
   gtCoreFunc.MouseSetCursor( fVisible );
}

int    hb_mouse_Col( void )
{
   return gtCoreFunc.MouseCol();
}

int    hb_mouse_Row( void )
{
   return gtCoreFunc.MouseRow();
}

void   hb_mouse_GetPos( int * piRow, int * piCol )
{
   gtCoreFunc.MouseGetPos( piRow, piCol );
}

void   hb_mouse_SetPos( int iRow, int iCol )
{
   gtCoreFunc.MouseSetPos( iRow, iCol );
}

void   hb_mouse_SetBounds( int iTop, int iLeft, int iBottom, int iRight )
{
   gtCoreFunc.MouseSetBounds( iTop, iLeft, iBottom, iRight );
}

void   hb_mouse_GetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight )
{
   gtCoreFunc.MouseGetBounds( piTop, piLeft, piBottom, piRight );
}

int    hb_mouse_StorageSize( void )
{
   return gtCoreFunc.MouseStorageSize();
}

void   hb_mouse_SaveState( BYTE * pBuffer )
{
   gtCoreFunc.MouseSaveState( pBuffer );
}

void   hb_mouse_RestoreState( BYTE * pBuffer )
{
   gtCoreFunc.MouseRestoreState( pBuffer );
}

int    hb_mouse_GetDoubleClickSpeed( void )
{
   return gtCoreFunc.MouseGetDoubleClickSpeed();
}

void   hb_mouse_SetDoubleClickSpeed( int iSpeed )
{
   gtCoreFunc.MouseSetDoubleClickSpeed( iSpeed );
}

int    hb_mouse_CountButton( void )
{
   return gtCoreFunc.MouseCountButton();
}

BOOL   hb_mouse_ButtonState( int iButton )
{
   return gtCoreFunc.MouseButtonState( iButton );
}

BOOL   hb_mouse_ButtonPressed( int iButton, int * piRow, int * piCol )
{
   return gtCoreFunc.MouseButtonPressed( iButton, piRow, piCol );
}

BOOL   hb_mouse_ButtonReleased( int iButton, int * piRow, int * piCol )
{
   return gtCoreFunc.MouseButtonReleased( iButton, piRow, piCol );
}

int    hb_mouse_ReadKey( int iEventMask )
{
   return gtCoreFunc.MouseReadKey( iEventMask );
}

int    hb_gt_GfxPrimitive( int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor )
{
   return gtCoreFunc.GfxPrimitive( iType, iTop, iLeft, iBottom, iRight, iColor );
}

void   hb_gt_GfxText( int iTop, int iLeft, char * szText, int iColor, int iSize, int iWidth )
{
   gtCoreFunc.GfxText( iTop, iLeft, szText, iColor, iSize, iWidth );
}

void   hb_gt_WhoCares( void * pCargo )
{
   gtCoreFunc.WhoCares( pCargo );
}

/* ************************************************************************* */

static char s_gtNameBuf[ HB_GT_NAME_MAX_ + 1 ];

#if defined(HB_GT_DEFAULT)
   char * s_defaultGT = HB_GT_DRVNAME( HB_GT_DEFAULT );
#elif defined(HB_GT_LIB)
   char * s_defaultGT = HB_GT_DRVNAME( HB_GT_LIB );
#elif defined(HB_OS_LINUX)
   char * s_defaultGT = "crs";
#elif defined(HB_OS_WIN_32)
   char * s_defaultGT = "win";
#elif defined(HB_OS_DOS)
   char * s_defaultGT = "dos";
#elif defined(HB_OS_OS2)
   char * s_defaultGT = "os2";
#else
   char * s_defaultGT = "std";
#endif

static PHB_GT_INIT s_gtInit[ HB_GT_MAX_ ];
static int s_gtLinkOrder[ HB_GT_MAX_ ];
static int s_iGtLinkCount = 0;
static int s_iGtCount = 0;

HB_FUNC_EXTERN( HB_GTSYS );

static char * hb_gtFindDefault( void )
{
   char szFuncName[ 15 + HB_GT_NAME_MAX_ ];
   int iPos;

   for( iPos = 0; iPos < s_iGtCount; iPos++ )
   {
      snprintf( szFuncName, sizeof( szFuncName ),
                "HB_GT_%s_DEFAULT", s_gtInit[ iPos ]->id );
      if( hb_dynsymFind( szFuncName ) )
         return s_gtInit[ iPos ]->id;
   }

   if( hb_dynsymFind( "HB_GT_NUL_DEFAULT" ) )
      return "NUL";
   else
      return NULL;
}

static int hb_gtFindEntry( const char * pszID )
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

HB_EXPORT void hb_gtSetDefault( const char * szGtName )
{
   hb_strncpy( s_gtNameBuf, szGtName, HB_GT_NAME_MAX_ );
   s_defaultGT = s_gtNameBuf;
}

HB_EXPORT BOOL hb_gtRegister( PHB_GT_INIT gtInit )
{
   if( hb_gtFindEntry( gtInit->id ) == -1 )
   {
      s_gtInit[ s_iGtCount++ ] = gtInit;
      return TRUE;
   }
   return FALSE;
}

HB_EXPORT BOOL hb_gtLoad( const char * szGtName, PHB_GT_FUNCS pFuncTable )
{
   int iPos;

   if( szGtName )
   {
      if( hb_stricmp( szGtName, "nul" ) == 0 || hb_stricmp( szGtName, "null" ) == 0 )
         return TRUE;

      iPos = hb_gtFindEntry( szGtName );

      if( iPos != -1 )
      {
         if( pFuncTable == NULL )
            pFuncTable = &gtCoreFunc;
         memcpy( s_gtInit[ iPos ]->pSuperTable, pFuncTable, sizeof( HB_GT_FUNCS ) );
         if( !s_gtInit[ iPos ]->init( pFuncTable ) )
         {
            hb_errInternal( 9999, "Internal error: screen driver initialization failure", NULL, NULL );
         }
         s_gtLinkOrder[ s_iGtLinkCount++ ] = iPos;
         return TRUE;
      }
   }
   return FALSE;
}

HB_EXPORT BOOL hb_gtUnLoad( void )
{
   while( s_iGtLinkCount > 0 )
   {
      if( --s_iGtLinkCount == 0 )
         memcpy( &gtCoreFunc,
                 s_gtInit[ s_gtLinkOrder[ s_iGtLinkCount ] ]->pSuperTable,
                 sizeof( HB_GT_FUNCS ) );
   }

   return TRUE;
}

HB_EXPORT void hb_gtStartupInit( void )
{
   char * szGtName;
   BOOL fInit;

   szGtName = hb_cmdargString( "GT" );
   if( szGtName )
   {
      fInit = hb_gtLoad( szGtName, &gtCoreFunc );
      hb_xfree( szGtName );
      if( fInit )
         return;
   }
   szGtName = hb_getenv( "HB_GT" );
   if( szGtName )
   {
      fInit = hb_gtLoad( szGtName, &gtCoreFunc );
      hb_xfree( szGtName );
      if( fInit )
         return;
   }
   if( hb_gtLoad( hb_gtFindDefault(), &gtCoreFunc ) )
      return;
   if( hb_gtLoad( s_defaultGT, &gtCoreFunc ) )
      return;

   if( hb_dynsymFind( "HB_GT_NUL" ) ) /* GTNUL was explicitly requsted */
   {
      if( hb_gtLoad( "NUL", &gtCoreFunc ) )
         return;
   }

   hb_errInternal( 9998, "Internal error: screen driver initialization failure", NULL, NULL );

   /* force linking HB_GTSYS() */
   HB_FUNC_EXEC( HB_GTSYS );
}

HB_GT_ANNOUNCE( HB_GT_NAME )
