/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Terminal API
 *
 * Copyright 1999 Bil Simser <bsimser@home.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Paul Tucker <ptucker@sympatico.ca>
 *    hb_gtInit()
 *    hb_gtExit()
 *    hb_gtDispBegin()
 *    hb_gtDispEnd()
 *    hb_gtPreExt()
 *    hb_gtPostExt()
 *    hb_gtGetColorStr()
 *    hb_gtSetColorStr()
 *    hb_gtSetMode()
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 *    hb_gtDrawShadow()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#if defined(__GNUC__) && ! defined(__MINGW32__)
   #include <unistd.h>
   #if defined(__DJGPP__) || defined(__CYGWIN__) || defined(__EMX__)
      #include <io.h>
   #endif
#else
   #include <io.h>
#endif

#include <ctype.h>

#include "hbapigt.h"
#include "hbset.h"

static BOOL   s_bInit = FALSE;

static SHORT  s_iRow;
static SHORT  s_iCol;
static USHORT s_uiPreCount;
static USHORT s_uiPreCNest;
static USHORT s_uiCursorStyle;

static USHORT s_uiColorIndex;
static USHORT s_uiColorCount;
static int *  s_pColor;

/* masks: 0x0007     Foreground
          0x0070     Background
          0x0008     Bright
          0x0080     Blink
          0x0800     Underline foreground
          0x8000     Underline background
 */

/* gt API functions */

#define COLOR_COUNT_DEF 5

void hb_gtInit( int s_iFilenoStdin, int s_iFilenoStdout, int s_iFilenoStderr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtInit()"));

   s_pColor = ( int * ) hb_xgrab( COLOR_COUNT_DEF * sizeof( int ) );
   s_uiColorCount = COLOR_COUNT_DEF;

   hb_gt_Init( s_iFilenoStdin, s_iFilenoStdout, s_iFilenoStderr );

   hb_gtSetColorStr( hb_set.HB_SET_COLOR );

   s_iRow = hb_gt_Row();
   s_iCol = hb_gt_Col();
   s_uiPreCount = 0;
   s_uiPreCNest = 0;

   /* This should be called after s_iRow/s_iCol initialization. */
   hb_gtSetCursor( SC_NORMAL );

   s_bInit = TRUE;

   if( hb_cmdargCheck( "INFO" ) )
   {
      hb_conOutErr( hb_gt_Version(), 0 );
      hb_conOutErr( hb_conNewLine(), 0 );
   }
}

void hb_gtExit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtExit()"));

   s_bInit = FALSE;

   while( hb_gt_DispCount() )
      hb_gt_DispEnd();

   hb_gt_Exit();

   hb_xfree( s_pColor );
}

int hb_gtReadKey( HB_inkey_enum eventmask )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtReadKey(%d)", (int) eventmask));

   return hb_gt_ReadKey( eventmask );
}

void hb_gtAdjustPos( int iHandle, char * pStr, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtAdjustPos()"));

   if( isatty( iHandle ) && hb_gt_AdjustPos( ( BYTE * ) pStr, ulLen ) )
   {
      /* Adjust the console cursor position to match the device driver */
      s_iRow = hb_gt_Row();
      s_iCol = hb_gt_Col();
   }
}

USHORT hb_gtBox( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbyFrame )
{
   USHORT uiMaxRow;
   USHORT uiMaxCol;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtBox(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pbyFrame));

   uiMaxRow = hb_gt_GetScreenHeight();
   uiMaxCol = hb_gt_GetScreenWidth();

   /* TODO: Would be better to support these cases, Clipper implementation was 
            quite messy, which can be considered as a bug there. [vszakats] */

   if( uiTop  < uiMaxRow && uiBottom < uiMaxRow &&
       uiLeft < uiMaxCol && uiRight  < uiMaxCol )
   {
      BYTE szBox[ 10 ];
      BYTE cPadChar;

      USHORT uiRow;
      USHORT uiCol;
      USHORT uiHeight;
      USHORT uiWidth;

      USHORT uiTopBak = uiTop;
      USHORT uiLeftBak = uiLeft;
   
      /* NOTE: For full compatibility, pad box string with last char if too 
               short [vszakats] */
      
      {
         USHORT tmp;

         cPadChar = ' ';
         for( tmp = 0; *pbyFrame && tmp < 9; tmp++ )
            cPadChar = szBox[ tmp ] = *pbyFrame++;
         while( tmp < 8 )
            szBox[ tmp++ ] = cPadChar;
         szBox[ tmp ] = '\0';
      }
      
      /* Ensure that box is drawn from top left to bottom right. */
      if( uiTop > uiBottom )
      {
         USHORT tmp = uiTop;
         uiTop = uiBottom;
         uiBottom = tmp;
      }
      if( uiLeft > uiRight )
      {
         USHORT tmp = uiLeft;
         uiLeft = uiRight;
         uiRight = tmp;
      }
      
      uiRow = uiTop;
      uiCol = uiLeft;
      
      /* Draw the box or line as specified */
      uiHeight = uiBottom - uiTop + 1;
      uiWidth  = uiRight - uiLeft + 1;
      
      hb_gtDispBegin();
      
      if( uiHeight > 1 && uiWidth > 1 )
         hb_gtWriteAt( uiRow, uiCol, szBox + 0, sizeof( BYTE ) ); /* Upper left corner */
      
      uiCol = ( uiHeight > 1 ? uiLeft + 1 : uiLeft );
      
      if( uiCol <= uiRight )
         hb_gtRepChar( uiRow, uiCol, szBox[ 1 ], uiRight - uiLeft + ( uiHeight > 1 ? -1 : 1 ) ); /* Top line */
      
      if( uiHeight > 1 && uiWidth > 1 )
         hb_gtWriteAt( uiRow, uiRight, szBox + 2, sizeof( BYTE ) ); /* Upper right corner */
      
      if( szBox[ 8 ] && uiHeight > 2 && uiWidth > 2 )
      {
         for( uiRow = uiTop + 1; uiRow < uiBottom; uiRow++ )
         {
            uiCol = uiLeft;
            hb_gtWriteAt( uiRow, uiCol++, szBox + 7, sizeof( BYTE ) ); /* Left side */
            hb_gtRepChar( uiRow, uiCol  , szBox[ 8 ], uiRight - uiLeft - 1 ); /* Fill */
            hb_gtWriteAt( uiRow, uiRight, szBox + 3, sizeof( BYTE ) ); /* Right side */
         }
      }
      else
      {
         for( uiRow = ( uiWidth > 1 ? uiTop + 1 : uiTop ); uiRow < ( uiWidth > 1 ? uiBottom : uiBottom + 1 ); uiRow++ )
         {
            hb_gtWriteAt( uiRow, uiLeft, szBox + 7, sizeof( BYTE ) ); /* Left side */
            if( uiWidth > 1 )
               hb_gtWriteAt( uiRow, uiRight, szBox + 3, sizeof( BYTE ) ); /* Right side */
         }
      }
      
      if( uiHeight > 1 && uiWidth > 1 )
      {
         hb_gtWriteAt( uiBottom, uiLeft, szBox + 6, sizeof( BYTE ) ); /* Bottom left corner */
      
         uiCol = ( uiHeight > 1 ? uiLeft + 1 : uiLeft );
      
         if( uiCol <= uiRight && uiHeight > 1 )
            hb_gtRepChar( uiBottom, uiCol, szBox[ 5 ], uiRight - uiLeft + ( uiHeight > 1 ? -1 : 1 ) ); /* Bottom line */
      
         hb_gtWriteAt( uiBottom, uiRight, szBox + 4, sizeof( BYTE ) ); /* Bottom right corner */
      }
      
      hb_gtDispEnd();
      
      hb_gtSetPos( uiTopBak + 1, uiLeftBak + 1 );
      
      return 0;
   }
   else
      return 1;
}

USHORT hb_gtBoxD( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight )
{
   return hb_gtBox( uiTop, uiLeft, uiBottom, uiRight, ( BYTE * ) _B_DOUBLE );
}

USHORT hb_gtBoxS( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight )
{
   return hb_gtBox( uiTop, uiLeft, uiBottom, uiRight, ( BYTE * ) _B_SINGLE );
}

USHORT hb_gtColorSelect( USHORT uiColorIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtColorSelect(%hu)", uiColorIndex));

   if( uiColorIndex <= s_uiColorCount )
   {
      s_uiColorIndex = uiColorIndex;
      return 0;
   }
   else
      return 1;
}

USHORT hb_gtDispBegin( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtDispBegin()"));

   if( s_uiPreCount == 0 )
      hb_gt_DispBegin();
   else
      ++s_uiPreCount;

   return 0;
}

USHORT hb_gtDispCount( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtDispCount()"));

   return hb_gt_DispCount();
}

USHORT hb_gtDispEnd( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtDispEnd()"));

   if( s_uiPreCount == 0 )
      hb_gt_DispEnd();
   else
      --s_uiPreCount;

   return 0;
}

USHORT hb_gtPreExt( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtPreExt()"));

   /* an external (printf...) write is about to take place */

   if( s_uiPreCNest == 0 )
   {
      if( s_uiPreCount == 0 )
      {
         USHORT uidc;

         uidc = s_uiPreCount = hb_gt_DispCount();

         while( uidc-- )
            hb_gt_DispEnd();
      }

      s_uiPreCNest = 1;
   }
   else
      ++s_uiPreCNest;

   return 0;
}

USHORT hb_gtPostExt( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtPostExt()"));

   if( s_uiPreCNest == 1 )
   {
      while( s_uiPreCount-- )
         hb_gt_DispBegin();

      s_uiPreCount = 0;
      s_uiPreCNest = 0;
   }
   else
      --s_uiPreCNest;

   return 0;
}

/* NOTE: szColorString must be at least CLR_STRLEN wide by the NG. It seems 
         that CA-Cl*pper SETCOLOR() will return string lengths up to 131+EOF. 
         That seems like a 127+1 buffer size, plus lazy overflow checking.
         [vszakats] */

USHORT hb_gtGetColorStr( char * pszColorString )
{
   USHORT uiColorIndex;
   int iPos = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetColorStr(%s)", pszColorString));

   /* Go on if there's space left for the largest color string plus EOF */
   for( uiColorIndex = 0; uiColorIndex < s_uiColorCount && iPos < ( CLR_STRLEN - 8 ); uiColorIndex++ )
   {
      int nColor = s_pColor[ uiColorIndex ] & 7;
      int j;

      if( uiColorIndex > 0 )
         pszColorString[ iPos++ ] = ',';

      for( j = 0; j <= 1; j++ )
      {
         if( ( s_pColor[ uiColorIndex ] & ( j ? 0x8000 : 0x0800 ) ) == 0 )
         {
            if( nColor == 7 )
                pszColorString[ iPos++ ] = 'W';
            else
            {
               if( nColor == 0 )
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
            if( ( s_pColor[ uiColorIndex ] & 0x80 ) != 0 )
               pszColorString[ iPos++ ] = '*';
#endif

            if( ( s_pColor[ uiColorIndex ] & 0x08 ) != 0 )
               pszColorString[ iPos++ ] = '+';

            pszColorString[ iPos++ ] = '/';
         }
#ifndef HB_C52_STRICT
         else
         {
            if( ( s_pColor[ uiColorIndex ] & 0x80 ) != 0 )
               pszColorString[ iPos++ ] = '*';
         }
#endif

         nColor = ( s_pColor[ uiColorIndex ] >> 4 ) & 7;
      }
   }

   pszColorString[ iPos ] = '\0';

   return 0;
}

USHORT hb_gtSetColorStr( char * szColorString )
{
   char c;
   char buff[ 6 ];
   BOOL bHasI = FALSE;
   BOOL bHasU = FALSE;
   BOOL bHasX = FALSE;
   BOOL bSlash = FALSE;
   int nPos = 0;
   int nFore = 0;
   int nColor = 0;
   int nCount = -1, i = 0, y;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetColorStr(%s)", szColorString));

   if( szColorString == ( char * ) NULL )
      return 1;

   if( *szColorString == '\0' )
   {
      s_pColor[ 0 ] = 0x07;
      s_pColor[ 1 ] = 0x70;
      s_pColor[ 2 ] = 0;
      s_pColor[ 3 ] = 0;
      s_pColor[ 4 ] = 0x07;
   }

   do
   {
      c = *szColorString++;
      c = toupper( c );

      while( c <= '9' && c >= '0' && i < 6 )
      {
         if( i == 0 )
            memset( buff, '\0', 6 );

         buff[ i++ ] = c;
         c = *szColorString++;
      }

      if( i > 0 )
      {
         --i;
         nColor = 0;
         /* TODO: this can probably be replaced with atoi() */
         /* ie: nColor = atoi( buff ); */
         for( y = 1; i + 1; y *= 10, i-- )
         {
            if( buff[ i ] != '\0')
               nColor += ( ( buff[ i ] - '0' ) * y );
         }
         nColor &= 0x0F;
         i = 0;
         ++nCount;
      }

      ++nCount;

      switch( c )
      {
         case 'B':
            nColor |= 1;
            break;
         case 'G':
            nColor |= 2;
            break;
         case 'I':
            bHasI   = TRUE;
            break;
         case 'N':
            nColor  = 0;
            break;
         case 'R':
            nColor |= 4;
            break;
         case 'U':
            bHasU   = TRUE;
            break;
         case 'W':
            nColor  = 7;
            break;
         case 'X':                   /* always sets forground to 'N' */
            bHasX   = TRUE;
            break;
         case '*':
            nFore  |= 128;
            break;
         case '+':
            nFore  |= 8;
            break;
         case '/':
            if( bHasU )
            {
               bHasU = FALSE;
               nFore |= 0x0800;  /* foreground underline bit */
            }
            else if( bHasX )
            {
               nColor = 0;
               bHasX = FALSE;
            }
            else if( bHasI )
            {
               nColor = 7;
               bHasI = FALSE;
            }

            nFore |= nColor;
            nColor = 0;
            bSlash = TRUE;
            break;
         case ',':
         case '\0':
            if( ! nCount )
               nFore = s_pColor[ nPos ];
            nCount = -1;
            if( nPos == s_uiColorCount )
            {
               s_pColor = ( int * ) hb_xrealloc( s_pColor, sizeof( int ) * ( nPos + 1 ) );
               ++s_uiColorCount;
            }
            if( bHasX )
               nFore &= 0x88F8;

            if( bHasU ) /* background if slash, else foreground */
               nColor |= 0x0800;

            if( bHasI )
            {
               if( bSlash )
               {
                  nColor &= 0x088F;
                  nColor |= 0x0007;
                  nFore &= 0x88F8;
               }
               else
               {
                  nColor &= 0x08F8;
                  nColor |= 0x0070;
                  nFore &= 0x888F;
               }
            }
            if( ( nFore & 0x8800 ) != 0 && ( ( nFore | nColor ) & 0x0077 ) == 0)
               nFore |= 1;

            if( bSlash )
               s_pColor[ nPos++ ] = ( nColor << 4 ) | nFore;
            else
               s_pColor[ nPos++ ] = nColor | nFore;

            nColor = nFore = 0;
            bSlash = bHasX = bHasU = bHasI = FALSE;
      }
   }
   while( c );

   if( nPos > 0 && nPos < 4 )
      s_pColor[ 4 ] = s_pColor[ 1 ];

   s_uiColorIndex = CLR_STANDARD; /* hb_gtColorSelect( CLR_STANDARD ); */

   return 0;
}

USHORT hb_gtGetCursor( USHORT * uipCursorStyle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetCursor(%p)", uipCursorStyle));

   *uipCursorStyle = hb_gt_GetCursorStyle();

   return 0;
}

USHORT hb_gtSetCursor( USHORT uiCursorStyle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetCursor(%hu)", uiCursorStyle));

   if( uiCursorStyle <= SC_SPECIAL2 )
   {
      /* Set the cursor only when, it's in bounds. */
      if( s_iRow >= 0 && s_iRow < hb_gt_GetScreenHeight() && 
          s_iCol >= 0 && s_iCol < hb_gt_GetScreenWidth() )
         hb_gt_SetCursorStyle( uiCursorStyle );

      s_uiCursorStyle = uiCursorStyle;

      return 0;
   }
   else
      return 1;
}

USHORT hb_gtGetPos( SHORT * piRow, SHORT * piCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetPos(%p, %p)", piRow, piCol));

   if( s_iRow >= 0 && s_iRow < hb_gt_GetScreenHeight() &&
       s_iCol >= 0 && s_iCol < hb_gt_GetScreenWidth() )
   {
      /* Only return the actual cursor position if the current
         cursor position was not previously set out of bounds. */
      s_iRow = hb_gt_Row();
      s_iCol = hb_gt_Col();
   }

   *piRow = s_iRow;
   *piCol = s_iCol;

   return 0;
}

USHORT hb_gtSetPos( SHORT iRow, SHORT iCol )
{
   USHORT uiMaxRow;
   USHORT uiMaxCol;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetPos(%hd, %hd)", iRow, iCol));

   uiMaxRow = hb_gt_GetScreenHeight();
   uiMaxCol = hb_gt_GetScreenWidth();

   /* Validate the new cursor position */
   if( iRow >= 0 && iRow < uiMaxRow &&
       iCol >= 0 && iCol < uiMaxCol )
   {
      hb_gt_SetPos( iRow, iCol );

      /* If cursor was out bounds, now enable it */
      if( s_iRow < 0 || s_iRow >= uiMaxRow || 
          s_iCol < 0 || s_iCol >= uiMaxCol )
         hb_gt_SetCursorStyle( s_uiCursorStyle );
   }
   else
      hb_gt_SetCursorStyle( SC_NONE ); /* Disable cursor if out of bounds */

   s_iRow = iRow;
   s_iCol = iCol;

   return 0;
}

BOOL hb_gtIsColor( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtIsColor()"));

   return hb_gt_IsColor();
}

USHORT hb_gtMaxCol( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtMaxCol()"));

   return hb_gt_GetScreenWidth() - 1;
}

USHORT hb_gtMaxRow( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtMaxRow()"));

   return hb_gt_GetScreenHeight() - 1;
}

USHORT hb_gtRectSize( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, USHORT * uipBuffSize )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtRectSize(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, uipBuffSize));

   *uipBuffSize = ( uiBottom - uiTop + 1 ) * ( uiRight - uiLeft + 1 ) * 2;

   return 0;
}

/* NOTE: Above this buffer size the function will allocate dynamic memory and 
         will be slower. [vszakats] */

#define REPCHAR_BUFFER_SIZE 255

USHORT hb_gtRepChar( USHORT uiRow, USHORT uiCol, BYTE byChar, USHORT uiCount )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtRepChar(%hu, %hu, %d, %hu)", uiRow, uiCol, (int) byChar, uiCount));

   if( uiCount <= REPCHAR_BUFFER_SIZE )
   {
      BYTE buffer[ REPCHAR_BUFFER_SIZE ];

      memset( buffer, byChar, uiCount );
      hb_gtWriteAt( uiRow, uiCol, buffer, uiCount );
   }
   else
   {
      BYTE * buffer = ( BYTE * ) hb_xgrab( uiCount );

      memset( buffer, byChar, uiCount );
      hb_gtWriteAt( uiRow, uiCol, buffer, uiCount );

      hb_xfree( buffer );
   }

   return 0;
}

USHORT hb_gtRest( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, void * pScrBuff )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtRest(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pScrBuff));

   hb_gt_PutText( uiTop, uiLeft, uiBottom, uiRight, ( BYTE * ) pScrBuff );

   return 0;
}

USHORT hb_gtSave( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, void * pScrBuff )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSave(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pScrBuff));

   hb_gt_GetText( uiTop, uiLeft, uiBottom, uiRight, ( BYTE * ) pScrBuff );

   return 0;
}

USHORT hb_gtScrDim( USHORT * uipHeight, USHORT * uipWidth )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtScrDim(%p, %p)", uipHeight, uipWidth));

   *uipHeight = hb_gt_GetScreenHeight() - 1;
   *uipWidth = hb_gt_GetScreenWidth() - 1;

   return 0;
}

USHORT hb_gtGetBlink( BOOL * bpBlink )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetBlink(%p)", bpBlink));

   *bpBlink = hb_gt_GetBlink();

   return 0;
}

USHORT hb_gtSetBlink( BOOL bBlink )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetBlink(%d)", (int) bBlink));

   hb_gt_SetBlink( bBlink );

   return 0;
}

USHORT hb_gtSetMode( USHORT uiRows, USHORT uiCols )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetMode(%hu, %hu)", uiRows, uiCols));

   return hb_gt_SetMode( uiRows, uiCols ) ? 0 : 1;
}

/* NOTE: This is a compatibility function.
         If you're running on a CGA and snow is a problem speak up! */

USHORT hb_gtSetSnowFlag( BOOL bNoSnow )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetSnowFlag(%d)", (int) bNoSnow));

   HB_SYMBOL_UNUSED( bNoSnow );

   return 0;
}

USHORT hb_gtWrite( BYTE * pStr, ULONG ulLength )
{
   SHORT iMaxCol;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtWrite(%p, %lu)", pStr, ulLength));

   /* Optimize access to max col position */
   iMaxCol = hb_gt_GetScreenWidth();

   /* Display the text if the cursor is on screen */
   if( s_iCol >= 0 && s_iCol < iMaxCol &&
       s_iRow >= 0 && s_iRow < hb_gt_GetScreenHeight() )
   {
      /* Truncate the text if the cursor will end up off the right edge */
      hb_gt_Puts( s_iRow, s_iCol, ( BYTE ) s_pColor[ s_uiColorIndex ], pStr,
         HB_MIN( ulLength, ( ULONG ) ( iMaxCol - s_iCol ) ) );
   }

   /* Finally, save the new cursor position, even if off-screen */
   hb_gtSetPos( s_iRow, s_iCol + ( SHORT ) ulLength );

   return 0;
}

USHORT hb_gtWriteAt( USHORT uiRow, USHORT uiCol, BYTE * pStr, ULONG ulLength )
{
   USHORT uiMaxCol;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtWriteAt(%hu, %hu, %p, %lu)", uiRow, uiCol, pStr, ulLength));

   /* Optimize access to max col position */
   uiMaxCol = hb_gt_GetScreenWidth();

   /* Display the text if the cursor is on screen */
   if( uiCol < uiMaxCol && uiRow < hb_gt_GetScreenHeight() )
   {
      /* Truncate the text if the cursor will end up off the right edge */
      hb_gt_Puts( uiRow, uiCol, ( BYTE ) s_pColor[ s_uiColorIndex ], pStr,
         HB_MIN( ulLength, ( ULONG ) ( uiMaxCol - uiCol ) ) );
   }

   /* Finally, save the new cursor position, even if off-screen */
   hb_gtSetPos( uiRow, uiCol + ( SHORT ) ulLength );

   return 0;
}

#define WRITECON_BUFFER_SIZE 500

USHORT hb_gtWriteCon( BYTE * pStr, ULONG ulLength )
{
   int iLen = 0;
   BOOL bDisp = FALSE;
   BOOL bNewLine = FALSE;
   SHORT iRow;
   SHORT iCol;
   SHORT iMaxRow;
   SHORT iMaxCol;
   BYTE szString[ WRITECON_BUFFER_SIZE ];

   HB_TRACE(HB_TR_DEBUG, ("hb_gtWriteCon(%p, %lu)", pStr, ulLength));

   iMaxRow = hb_gt_GetScreenHeight() - 1;
   iMaxCol = hb_gt_GetScreenWidth() - 1;

   /* Limit the starting cursor position to maxrow(),maxcol()
      on the high end, but don't limit it on the low end. */

   iRow = ( s_iRow <= iMaxRow ) ? s_iRow : iMaxRow;
   iCol = ( s_iCol <= iMaxCol ) ? s_iCol : iMaxCol;

   if( iRow != s_iRow || iCol != s_iCol )
      hb_gtSetPos( iRow, iCol );

   while( ulLength-- )
   {
      BYTE ch = *pStr++;

      switch( ch )
      {
         case HB_CHAR_BEL:
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
            break;

         case HB_CHAR_LF:
            iCol = 0;
            if( iRow >= 0 ) ++iRow;
            bDisp = TRUE;
            bNewLine = TRUE;
            break;

         case HB_CHAR_CR:
            iCol = 0;
            if( *pStr != HB_CHAR_LF )
               bDisp = TRUE;
            else
            {
               if( iRow >= 0 ) ++iRow;
               bDisp = TRUE;
               bNewLine = TRUE;
               ++pStr;
            }
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
         if( iLen && s_iRow >= 0 )
            hb_gtWrite( szString, iLen );

         iLen = 0;
         if( iRow > iMaxRow )
         {
            /* Normal scroll */
            hb_gtScroll( 0, 0, iMaxRow, iMaxCol, iRow - iMaxRow, 0 );
            iRow = iMaxRow;
            iCol = 0;
         }
         else if( iRow < 0 && bNewLine )
         {
            /* Special case scroll when newline
               and cursor off top edge of display */
            hb_gtScroll( 0, 0, iMaxRow, iMaxCol, 1, 0 );
         }
         hb_gtSetPos( iRow, iCol );
         bDisp = FALSE;
         bNewLine = FALSE;
      }
   }

   return 0;
}

USHORT hb_gtScroll( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, SHORT iRows, SHORT iCols )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtScroll(%hu, %hu, %hu, %hu, %hd, %hd)", uiTop, uiLeft, uiBottom, uiRight, iRows, iCols));

   hb_gt_Scroll( uiTop, uiLeft, uiBottom, uiRight, ( BYTE ) s_pColor[ s_uiColorIndex ], iRows, iCols );

   return 0;
}

/* NOTE: It would be better if the clipping was done by the low level API */

USHORT hb_gtDrawShadow( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr )
{
   USHORT uiMaxRow;
   USHORT uiMaxCol;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtDrawShadow(%hu, %hu, %hu, %hu, %d)", uiTop, uiLeft, uiBottom, uiRight, (int) byAttr));

   uiMaxRow = hb_gt_GetScreenHeight() - 1;
   uiMaxCol = hb_gt_GetScreenWidth() - 1;

   uiLeft += 2;
   ++uiBottom;

   /* Draw the bottom edge */

   if( uiBottom <= uiMaxRow && uiLeft <= uiMaxCol )
      hb_gt_SetAttribute( uiBottom, uiLeft, uiBottom, HB_MIN( uiRight, uiMaxCol ), byAttr );

   ++uiRight;
   ++uiTop;

   /* Draw the right edge */

   if( uiTop <= uiMaxRow && uiRight <= uiMaxCol )
      hb_gt_SetAttribute( uiTop, uiRight, uiBottom, HB_MIN( uiRight + 1, uiMaxCol ), byAttr );

   return 0;
}

void hb_gtTone( double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtTone(%lf, %lf)", dFrequency, dDuration));

   hb_gt_Tone( dFrequency, dDuration );
}

char * hb_gtVersion( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtVersion()"));

   return hb_gt_Version();
}

