/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Terminal API
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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
   #if defined(__DJGPP__) || defined(__CYGWIN__) || defined(HARBOUR_GCC_OS2)
      #include <io.h>
   #endif
#else
   #include <io.h>
#endif

#include <ctype.h>

#include "hbapigt.h"
#include "hbset.h"

static BOOL   s_bInit = FALSE;
static SHORT  s_iCurrentRow;
static SHORT  s_iCurrentCol;
static USHORT s_uiPreCount;
static USHORT s_uiPreCNest;
static USHORT s_uiColorIndex;
static USHORT s_uiCursorShape;

/* masks: 0x0007     Foreground              
          0x0070     Background              
          0x0008     Bright                  
          0x0080     Blink                   
          0x0800     Underline foreground    
          0x8000     Underline background    
 */                                          

static int *  s_Color; 
static int    s_ColorCount;

/* gt API functions */

void hb_gtInit( int s_iFilenoStdin, int s_iFilenoStdout, int s_iFilenoStderr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtInit()"));

   s_Color = ( int * ) hb_xgrab( 5 * sizeof( int ) );
   s_ColorCount = 5;

   hb_gt_Init( s_iFilenoStdin, s_iFilenoStdout, s_iFilenoStderr );
   hb_gtSetColorStr( hb_set.HB_SET_COLOR );
   hb_gtSetCursor( SC_NORMAL );

   s_iCurrentRow = hb_gt_Row();
   s_iCurrentCol = hb_gt_Col();
   s_uiPreCount = 0;
   s_uiPreCNest = 0;
   s_uiColorIndex = 0;
   s_uiCursorShape = 0;

   s_bInit = TRUE;

   if( hb_cmdargCheck( "INFO" ) )
   {
      hb_outerr( hb_gtVersion(), 0 );
      hb_outerr( hb_consoleGetNewLine(), 0 );
   }
}

void hb_gtExit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtExit()"));

   s_bInit = FALSE;

   while( hb_gt_DispCount() )
      hb_gt_DispEnd();

   hb_gt_Done();
   hb_xfree( s_Color );
}

int hb_gtReadKey( HB_inkey_enum eventmask )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtReadKey(%d)", (int) event_mask));

   return hb_gt_ReadKey( eventmask );
}

void hb_gtAdjustPos( int iHandle, char * pStr, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtAdjustPos()"));

   if( isatty( iHandle ) && hb_gt_AdjustPos( ( BYTE * ) pStr, ulLen ) )
   {
      /* Adjust the console cursor position to match the device driver */
      s_iCurrentRow = hb_gt_Row();
      s_iCurrentCol = hb_gt_Col();
   }
}

USHORT hb_gtBox( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbyFrame )
{
   BYTE pszBox[ 10 ];
   BYTE cPadChar;

   USHORT uiRow;
   USHORT uiCol;
   USHORT height, width, tmp;

   USHORT uiTopBak;
   USHORT uiLeftBak;

   USHORT uiMaxRow;
   USHORT uiMaxCol;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtBox(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pbyFrame));

   uiTopBak = uiTop;
   uiLeftBak = uiLeft;

   uiMaxRow = hb_gtMaxRow();
   uiMaxCol = hb_gtMaxCol();

   /* TODO: Would be better to support these cases, Clipper implementation */
   /*       was quite messy for these cases, which can be considered as */
   /*       a bug there. */

   if( uiTop  > uiMaxRow || uiBottom > uiMaxRow ||
       uiLeft > uiMaxCol || uiRight  > uiMaxCol )
   {
      return 1;
   }

   /* For full compatibility, pad box string with last char if too short */

   cPadChar = ' ';
   for( tmp = 0; *pbyFrame && tmp < 9; tmp++ )
      cPadChar = pszBox[ tmp ] = *pbyFrame++;
   while( tmp < 8 )
      pszBox[ tmp++ ] = cPadChar;
   pszBox[ tmp ] = '\0';

   /* Ensure that box is drawn from top left to bottom right. */
   if( uiTop > uiBottom )
   {
      tmp = uiTop;
      uiTop = uiBottom;
      uiBottom = tmp;
   }
   if( uiLeft > uiRight )
   {
      tmp = uiLeft;
      uiLeft = uiRight;
      uiRight = tmp;
   }

   uiRow = uiTop;
   uiCol = uiLeft;

   /* Draw the box or line as specified */
   height = uiBottom - uiTop + 1;
   width  = uiRight - uiLeft + 1;

   hb_gtDispBegin();

   if( height > 1 && width > 1 )
      hb_gtWriteAt( uiRow, uiCol, pszBox + 0, sizeof( BYTE ) ); /* Upper left corner */

   uiCol = ( height > 1 ? uiLeft + 1 : uiLeft );

   if( uiCol <= uiRight )
      hb_gtRepChar( uiRow, uiCol, pszBox[ 1 ], uiRight - uiLeft + ( height > 1 ? -1 : 1 ) ); /* Top line */
   
   if( height > 1 && width > 1 )
      hb_gtWriteAt( uiRow, uiRight, pszBox + 2, sizeof( BYTE ) ); /* Upper right corner */

   if( pszBox[ 8 ] && height > 2 && width > 2 )
   {
      for( uiRow = uiTop + 1; uiRow < uiBottom; uiRow++ )
      {
         uiCol = uiLeft;
         hb_gtWriteAt( uiRow, uiCol++, pszBox + 7, sizeof( BYTE ) ); /* Left side */
         hb_gtRepChar( uiRow, uiCol  , pszBox[ 8 ], uiRight - uiLeft - 1 ); /* Fill */
         hb_gtWriteAt( uiRow, uiRight, pszBox + 3, sizeof( BYTE ) ); /* Right side */
      }
   }
   else
   {
      for( uiRow = ( width > 1 ? uiTop + 1 : uiTop ); uiRow < ( width > 1 ? uiBottom : uiBottom + 1 ); uiRow++ )
      {
         hb_gtWriteAt( uiRow, uiLeft, pszBox + 7, sizeof( BYTE ) ); /* Left side */
         if( width > 1 )
            hb_gtWriteAt( uiRow, uiRight, pszBox + 3, sizeof( BYTE ) ); /* Right side */
      }
   }

   if( height > 1 && width > 1 )
   {
      hb_gtWriteAt( uiBottom, uiLeft, pszBox + 6, sizeof( BYTE ) ); /* Bottom left corner */

      uiCol = ( height > 1 ? uiLeft + 1 : uiLeft );

      if( uiCol <= uiRight && height > 1 )
         hb_gtRepChar( uiBottom, uiCol, pszBox[ 5 ], uiRight - uiLeft + ( height > 1 ? -1 : 1 ) ); /* Bottom line */

      hb_gtWriteAt( uiBottom, uiRight, pszBox + 4, sizeof( BYTE ) ); /* Bottom right corner */
   }

   hb_gtDispEnd();

   hb_gtSetPos( uiTopBak + 1, uiLeftBak + 1 );

   return 0;
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

   if( uiColorIndex > s_ColorCount )
      return 1;
   else
      s_uiColorIndex = uiColorIndex;

   return 0;
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
      USHORT uidc = s_uiPreCount;

      while( uidc-- )
         hb_gt_DispBegin();

      s_uiPreCount = 0;
      s_uiPreCNest = 0;
   }
   else
      --s_uiPreCNest;

   return 0;
}

USHORT hb_gtGetColorStr( char * fpColorString )
{
   char * sColors;
   int i, k = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetColorStr(%s)", fpColorString));

   sColors = ( char * ) hb_xgrab( s_ColorCount * 8 + 1 ); /* max possible */

   for( i = 0; i < s_ColorCount; i++ )
   {
      int j = 0;
      int nColor = s_Color[ i ] & 7;

      do
      {
         if( ( s_Color[ i ] & ( j ? 0x8000 : 0x0800 ) ) != 0 )
            sColors[ k++ ] = 'U';
         else
         {
            if( nColor == 7 )
                sColors[ k++ ] = 'W';
            else
            {
               if( nColor == 0 )
                  sColors[ k++ ] = 'N';
               else
               {
                  if( ( nColor & 1 ) != 0 )
                     sColors[ k++ ] = 'B';

                  if( ( nColor & 2 ) != 0 )
                     sColors[ k++ ] = 'G';

                  if( ( nColor & 4 ) != 0 )
                     sColors[ k++ ] = 'R';
               }
            }
         }
         if( j == 0 )
         {
            if( ( s_Color[ i ] & 8 ) != 0 )
               sColors[ k++ ] = '+';
            sColors[ k++ ] = '/';
         }
         else
            if( ( s_Color[ i ] & 128 ) != 0 )
               sColors[ k++ ] = '*';

         nColor = ( s_Color[ i ] >> 4 ) & 7;
      }
      while( ++j < 2 );

      if( i + 1 < s_ColorCount )
         sColors[ k++ ] = ',';
   }

   sColors[ k ] = '\0';

   strcpy( fpColorString, sColors );
   hb_xfree( sColors );

   return 0;
}

USHORT hb_gtSetColorStr( char * fpColorString )
{
   char c, buff[ 6 ];
   BOOL bHasI = FALSE;
   BOOL bHasU = FALSE;
   BOOL bHasX = FALSE;
   BOOL bSlash = FALSE;
   int nPos = 0;
   int nFore = 0;
   int nColor = 0;
   int nCount = -1, i = 0, y;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetColorStr(%s)", fpColorString));

   if( !fpColorString )
      return 1;
   if( ! *fpColorString )
   {
      s_Color[ 0 ] = 0x07;
      s_Color[ 1 ] = 0x70;
      s_Color[ 2 ] = 0;
      s_Color[ 3 ] = 0;
      s_Color[ 4 ] = 0x07;
   }

   do
   {
      c = *fpColorString++;
      c = toupper( c );

      while( c <= '9' && c >= '0' && i < 6 )
      {
         if( i == 0 )
            memset( buff, '\0', 6 );

         buff[ i++ ] = c;
         c = *fpColorString++;
      }
      if( i > 0 )
      {
         i--;
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
      switch ( c )
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
               nFore = s_Color[ nPos ];
            nCount = -1;
            if( nPos == s_ColorCount )
            {
               s_Color = ( int * ) hb_xrealloc( s_Color, sizeof( int ) * ( nPos + 1 ) );
               ++s_ColorCount;
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
               s_Color[ nPos++ ] = ( nColor << 4 ) | nFore;
            else
               s_Color[ nPos++ ] = nColor | nFore;

            nColor = nFore = 0;
            bSlash = bHasX = bHasU = bHasI = FALSE;
      }
   }
   while( c );

   if( nPos > 0 && nPos < 4 )
      s_Color[ 4 ] = s_Color[ 1 ];

   hb_gtColorSelect( CLR_STANDARD );

   return 0;
}

USHORT hb_gtGetCursor( USHORT * uipCursorShape )
{
   int i;
   int rc = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetCursor(%p)", uipCursorShape));

   i = s_uiCursorShape = hb_gt_GetCursorStyle();

   if( i <= SC_SPECIAL2 )
   {
      *uipCursorShape = i;
   }
   else
   {
      rc = i;
   }

   return rc;
}

USHORT hb_gtSetCursor( USHORT uiCursorShape )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetCursor(%hu)", uiCursorShape));

   hb_gt_SetCursorStyle( uiCursorShape );
   s_uiCursorShape = uiCursorShape;

   return 0;
}

USHORT hb_gtGetPos( SHORT * piRow, SHORT * piCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetPos(%p, %p)", piRow, piCol));

   if( s_iCurrentRow >= 0 && s_iCurrentRow <=  hb_gtMaxRow()
      && s_iCurrentCol >= 0 && s_iCurrentCol <= hb_gtMaxCol() )
   {
      /* Only return the actual cursor position if the current
         cursor position was not previously set out of bounds. */
      s_iCurrentRow = hb_gt_Row();
      s_iCurrentCol = hb_gt_Col();
   }
   *piRow = s_iCurrentRow;
   *piCol = s_iCurrentCol;

   return 0;
}

USHORT hb_gtSetPos( SHORT iRow, SHORT iCol )
{
   BOOL set_cursor;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetPos(%hd, %hd)", iRow, iCol));

   /* Validate the new cursor position */
   if( iRow < 0 || iCol < 0 || iRow > hb_gtMaxRow() || iCol > hb_gtMaxCol() )
   {
      /* Disable cursor if out of bounds */
      hb_gt_SetCursorStyle( SC_NONE );
      set_cursor = FALSE;
   }
   else
      set_cursor = TRUE;

   if( set_cursor ) hb_gt_SetPos( iRow, iCol );

   /* Check the old cursor position */
   if( s_iCurrentRow < 0 || s_iCurrentCol < 0 || s_iCurrentRow > hb_gtMaxRow()
      || s_iCurrentCol > hb_gtMaxCol() )
   {
      /* If back in bounds, enable the cursor */
      if( set_cursor) hb_gt_SetCursorStyle( s_uiCursorShape );
   }

   s_iCurrentRow = iRow;
   s_iCurrentCol = iCol;

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

USHORT hb_gtRepChar( USHORT uiRow, USHORT uiCol, BYTE byChar, USHORT uiCount )
{
   int rc;
   BYTE buff[ 255 ];

   HB_TRACE(HB_TR_DEBUG, ("hb_gtRepChar(%hu, %hu, %d, %hu)", uiRow, uiCol, (int) byChar, uiCount));

   if( uiCount > sizeof( buff ) )
      return 1;

   memset( buff, byChar, uiCount );
   buff[ uiCount ] = '\0';
   rc = hb_gtSetPos( uiRow, uiCol );
   if( rc != 0 )
      return rc;
   rc = hb_gtWrite( buff, uiCount );

   return rc;
}

USHORT hb_gtRest( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, void * vlpScrBuff )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtRest(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, vlpScrBuff));

   hb_gt_PutText( uiTop, uiLeft, uiBottom, uiRight, ( BYTE * ) vlpScrBuff );

   return 0;
}

USHORT hb_gtSave( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, void * vlpScrBuff )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSave(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, vlpScrBuff));

   hb_gt_GetText( uiTop, uiLeft, uiBottom, uiRight, ( BYTE * ) vlpScrBuff );

   return 0;
}

USHORT hb_gtScrDim( USHORT * uipHeight, USHORT * uipWidth )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtScrDim(%p, %p)", uipHeight, uipWidth));

   *uipHeight = hb_gtMaxRow();
   *uipWidth = hb_gtMaxCol();

   return 0;
}

USHORT hb_gtGetBlink( BOOL * bBlink )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtGetBlink(%p)", bBlink));

   *bBlink = hb_gt_GetBlink();

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

USHORT hb_gtSetSnowFlag( BOOL bNoSnow )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtSetSnowFlag(%d)", (int) bNoSnow));

   /* NOTE: This is a compatibility function.
      If you're running on a CGA and snow is a problem
      speak up!
   */
   HB_SYMBOL_UNUSED( bNoSnow );

   return 0;
}

USHORT hb_gtWrite( BYTE * fpStr, ULONG length )
{
   SHORT iMaxCol, iMaxRow;
   ULONG size = length;
   BYTE attr = s_Color[ s_uiColorIndex ] & 0xFF;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtWrite(%p, %lu)", fpStr, length));

   /* Optimize access to max row and col positions */
   iMaxRow = hb_gtMaxRow();
   iMaxCol = hb_gtMaxCol();

   /* Display the text if the cursor is on screen */
   if( s_iCurrentCol >= 0 && s_iCurrentCol <= iMaxCol
      && s_iCurrentRow >= 0 && s_iCurrentRow <= iMaxRow )
   {
      /* Truncate the text if the cursor will end up off the right edge */
      if( s_iCurrentCol + ( SHORT ) size > iMaxCol + 1 ) size = iMaxCol - s_iCurrentCol + 1;
      hb_gt_Puts( s_iCurrentRow, s_iCurrentCol, attr, fpStr, size );
   }
   /* Finally, save the new cursor position, even if off-screen */
   hb_gtSetPos( s_iCurrentRow, s_iCurrentCol + (SHORT) length );

   return 0;
}

USHORT hb_gtWriteAt( USHORT uiRow, USHORT uiCol, BYTE * fpStr, ULONG length )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtWriteAt(%hu, %hu, %p, %lu)", uiRow, uiCol, fpStr, length));

   hb_gtSetPos( uiRow, uiCol );
   return hb_gtWrite( fpStr, length );
}

USHORT hb_gtWriteCon( BYTE * fpStr, ULONG length )
{
   int rc = 0, nLen = 0;
   BOOL ldisp = FALSE;
   BOOL lnewline = FALSE;
   SHORT iRow, iCol;
   SHORT iMaxRow, iMaxCol;
   BYTE ch;
   BYTE * fpPtr = fpStr;
   #define STRNG_SIZE 500
   BYTE strng[ STRNG_SIZE ];

   HB_TRACE(HB_TR_DEBUG, ("hb_gtWriteCon(%p, %lu)", fpStr, length));

   iRow = s_iCurrentRow;
   iCol = s_iCurrentCol;
   iMaxRow = hb_gtMaxRow();
   iMaxCol = hb_gtMaxCol();

   /* Limit the starting cursor position to maxrow(),maxcol()
      on the high end, but don't limit it on the low end. */
   if( iRow > iMaxRow ) iRow = iMaxRow;
   if( iCol > iMaxCol ) iCol = iMaxCol;
   if( iRow != s_iCurrentRow || iCol != s_iCurrentCol)
      hb_gtSetPos( iRow, iCol );

   while( length-- )
   {
      ch = *fpPtr++;
      switch( ch )
      {
         case HB_CHAR_BEL:
            break;

         case HB_CHAR_BS:
            if( iCol > 0 )
            {
               --iCol;
               ldisp = TRUE;
            }
            else if( iCol == 0 && iRow > 0 )
            {
               iCol = iMaxCol;
               --iRow;
               ldisp = TRUE;
            }

            break;

         case HB_CHAR_LF:
            if( iRow >= 0 ) ++iRow;
            ldisp = TRUE;
            lnewline = TRUE;
            break;

         case HB_CHAR_CR:
            iCol = 0;
            if( *fpPtr != HB_CHAR_LF ) ldisp = TRUE;
            break;

         default:
            iCol++;
            if( iCol > iMaxCol || iCol <= 0 )
            {
               /* If the cursor position started off the left edge,
                  don't display the first character of the string */
               if( iCol > 0 ) strng[ nLen++ ] = ch;
               /* Always advance to the first column of the next row
                  when the right edge is reached or when the cursor
                  started off the left edge, unless the cursor is off
                  the top edge, in which case only change the column */
               iCol = 0;
               if( iRow >= 0 ) ++iRow;
               ldisp = TRUE;
               lnewline = TRUE;
            }
            else strng[ nLen++ ] = ch;

            /* Special handling for a really wide screen or device */
            if( nLen >= STRNG_SIZE ) ldisp = TRUE;
      }
      if( ldisp || ! length )
      {
         if( nLen && s_iCurrentRow >= 0 )
            rc = hb_gtWrite( strng, nLen );
         nLen = 0;
         if( iRow > iMaxRow )
         {
            /* Normal scroll */
            hb_gtScroll( 0, 0, iMaxRow, iMaxCol, iRow - iMaxRow, 0 );
            iRow = iMaxRow;
            iCol = 0;
         }
         else if( iRow < 0 && lnewline )
         {
            /* Special case scroll when newline
               and cursor off top edge of display */
            hb_gtScroll( 0, 0, iMaxRow, iMaxCol, 1, 0 );
         }
         hb_gtSetPos( iRow, iCol );
         ldisp = FALSE;
         lnewline = FALSE;
      }
      if( rc )
         break;
   }

   return rc;
}

USHORT hb_gtScroll( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, SHORT iRows, SHORT iCols )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gtScroll(%hu, %hu, %hu, %hu, %hd, %hd)", uiTop, uiLeft, uiBottom, uiRight, iRows, iCols));

   hb_gt_Scroll( uiTop, uiLeft, uiBottom, uiRight, s_Color[ s_uiColorIndex ], iRows, iCols );
   return 0;
}

/* NOTE: It would be better if the clipping was done by the low level API */

USHORT hb_gtDrawShadow( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr )
{
   USHORT uiMaxRow;
   USHORT uiMaxCol;

   HB_TRACE(HB_TR_DEBUG, ("hb_gtDrawShadow(%hu, %hu, %hu, %hu, %d)", uiTop, uiLeft, uiBottom, uiRight, (int) byAttr));

   uiMaxRow = hb_gtMaxRow();
   uiMaxCol = hb_gtMaxCol();

   uiLeft += 2;
   uiBottom++;

   /* Draw the bottom edge */

   if( uiBottom <= uiMaxRow && uiLeft <= uiMaxCol )
      hb_gt_SetAttribute( uiBottom, uiLeft, uiBottom, HB_MIN( uiRight, uiMaxCol ), byAttr );

   uiRight++;
   uiTop++;

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
 
#ifdef TEST
void main( void )
{
   BYTE * test = "Testing GT API Functions";
   BYTE * test2 = "This message wraps!";
   int iRow, iCol;

   /* NOTE: always have to initialze video subsystem */
   hb_gtInit();

   /* save screen (doesn't work under DOS) */
   /*
   BYTE * scr;
   USHORT size;

   hb_gtRectSize( 1, 1, hb_gtMaxRow(), hb_gtMaxCol(), &size );
   scr = ( BYTE * ) hb_xgrab( size );
   hb_gtSave( 1, 1, hb_gtMaxRow() - 1, hb_gtMaxCol() - 1, scr );
   */

   /* writing text */
   hb_gtSetPos( 3, 3 );
   hb_gtWrite( test, strlen( test ) );
   hb_gtSetPos( 12, 42 );
   hb_gtWrite( test, strlen( test ) );

   /* wrapping text */
   hb_gtSetPos( 7, 70 );
   hb_gtWrite( test2, strlen( test2 ) );

   /* writing color text */
   hb_gtSetColorStr( "W+/B, B/W" );
   hb_gtColorSelect( _CLR_STANDARD );
   hb_gtWrite( "Enhanced color (B/W)", 20 );
   hb_gtSetPos( 22, 62 );
   hb_gtColorSelect( _CLR_ENHANCED );
   hb_gtWrite( "Standard Color (W+/B)", 21 );

   /* boxes */
   hb_gtBoxS( 10, 10, 20, 20 );
   hb_gtBoxD( 10, 40, 15, 45 );

   /* cursor functions */
   hb_gtSetPos( 12, 1 );

   /* none */
   hb_gtSetCursor( _SC_NONE );
   getch();

   /* underline */
   hb_gtSetCursor( _SC_NORMAL );
   getch();

   /* lower half block */
   hb_gtSetCursor( _SC_INSERT );
   getch();

   /* full block */
   hb_gtSetCursor( _SC_SPECIAL1 );
   getch();

   /* upper half block */
   hb_gtSetCursor( _SC_SPECIAL2 );
   getch();

   /* restore screen (doesn't work under DOS) */
   /*
   hb_gtRest( 1, 1, hb_gtMaxRow() - 1, hb_gtMaxCol() - 1, scr );
   hb_xfree( scr );
   */
}
#endif
