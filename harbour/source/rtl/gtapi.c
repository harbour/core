/*
 * $Id$
 */

/*
 *  GTAPI.C: Generic Terminal for Harbour
 *
 * Latest mods:
 * 1.66   19990830   vszel     Reformatted using Harbour standard. Some
 *                             small cleanups.
 * 1.65   19990830   ptucker   Handle nesting of gtPre/PostExt - corrected
 *                             s_uiPreCount handling in gtDispEnd
 * 1.63   19990811   ptucker   Implimented gtPre and PostExt to be used when
 *                             writing to screen via printf.
 * 1.58   19990811   ptucker   changes to gtWriteCon and gtWrite to improve
 *                             speed.
 * 1.56   19990811   ptucker   Corrected initial MaxRow/col
 * 1.53   19990807   ptucker   Modified Dispbegin/end support
 * 1.47   19990802   ptucker   DispBegin/End and SetMode for gtWin
 * 1.46   19990801   ptucker   simplified hb_gtScroll if gtWin
 * 1.44   19990730   ptucker   simplified gtputs and gtSetAttribute
 *                             corrected gtGetCursorStyle for !cci.bVisible
 *                             return SC_NONE - other cases should be handled
 *                             by the switch that follows.
 *                             changed 'case 8:' in gtWriteCon to check
 *                             against uiCol instead of uiRow
 * 1.43   19990729   ptucker   Corrected a number of calls so params are
 *                             in top,left,bottom,right or row,col order.
 *                             removed call to gtrectsize in gtputtext.
 *                             This should be handled by the caller.
 * 1.41   19990728   ptucker   Minor correction for inverted coords
 * 1.40   19990726   vszel     Allowing Top > Bottom and Right > Left
 *                             cases again. Clipper allows these, too.
 *                             Cursor positioning fixed to support these cases.
 *                             uMRow renamed to uiMRow
 *                             uMCol renamed to uiMCol
 * 1.39   19990726   ptucker   Position cursor inside top-left corner
 *                             after drawing box - compatibility
 * 1.35   19990726   ptucker   Much improved box drawing speed
 *                             Modifed some if statments to test for != 0
 * 1.34   19990721   ptucker   Corrected _Color mask descriptions
 * 1.33   19990721   ptucker   Improved Clipper color compatibility
 * 1.31   19990720   ptucker   Implimented color selection in gtWrite and
 *                             gtScroll
 * 1.30   19990719   ptucker   Removed temp init hack
 *                             call gtDone from hb_gtExit
 * 1.29   19990719   ptucker   Minor change to catch last color parameter
 *                             that may be empty
 * 1.28   19990719   ptucker   Added support for numeric color strings
 *                             like "1/7,8/15"
 * 1.26   19990719   ptucker   Changed call in hb_gtinit() to pass the
 *                             literal initial color setting in case
 *                             the GT system is initialised prior to Set.
 *                             Skipped color params in a string now keep
 *                             their previous value.  ie ",,,r/b"
 * 1.25   19990718   dholm     Moved calls to various gtFunctions out of
 *                             InitializeConsole() in console.c and put
 *                             them in hb_gtInit() in this module. Use
 *                             hb_set.HB_SET_COLOR to initialize the GT
 *                             API color string. Converted // comments.
 * 1.24   19990718   ptucker   corrected returned color strings so ordering
 *                             is the same as clipper.
 * 1.23   19990718   ptucker   implimented surface for gtGet/SetColorStr()
 *                             changed to allow unlimited color pairs.
 */

#include <ctype.h>
#include "set.h"
#include "gtapi.h"

static USHORT s_uiCurrentRow = 0;
static USHORT s_uiCurrentCol = 0;
static USHORT s_uiDispCount  = 0;
static USHORT s_uiPreCount   = 0;
static USHORT s_uiPreCNest   = 0;
static USHORT s_uiColorIndex = 0;
static USHORT s_uiMaxCol;
static USHORT s_uiMaxRow;

static int *  s_Color;  /* masks: 0x0007     Foreground
                                  0x0070     Background
                                  0x0008     Bright
                                  0x0080     Blink
                                  0x0800     Underline foreground
                                  0x8000     Underline background
                         */
static int    s_ColorCount;

/* gt API functions */

void hb_gtInit( void )
{
/* ptucker */

   s_Color      = ( int * ) hb_xgrab( 5 * sizeof( int ) );
   s_ColorCount = 5;
   hb_gt_Init();
   hb_gtSetColorStr( hb_set.HB_SET_COLOR );
   hb_gtMaxRow();
   hb_gtMaxCol();
}

void hb_gtExit( void )
{
/* ptucker */

   while( s_uiDispCount )
      hb_gtDispEnd();

   hb_gt_Done();
   hb_xfree( s_Color );
}

int hb_gtBox( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, char * pbyFrame )
{
   char pszBox[ 10 ];
   char cPadChar;

   USHORT uiRow;
   USHORT uiCol;
   USHORT height, width, tmp;

   USHORT uiTopBak = uiTop;
   USHORT uiLeftBak = uiLeft;

   USHORT uiMRow = s_uiMaxRow;
   USHORT uiMCol = s_uiMaxCol;

   /* TODO: Would be better to support these cases, Clipper implementation */
   /*       was quite messy for these cases, which can be considered as */
   /*       a bug there. */

   if( uiTop  > uiMRow || uiBottom > uiMRow ||
       uiLeft > uiMCol || uiRight  > uiMCol )
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
/* hb_gtDispBegin(); */

   if( height > 1 && width > 1 )
   {
      hb_gtWriteAt( uiRow   , uiCol  , pszBox + 0, sizeof( BYTE ) );
      hb_gtWriteAt( uiRow   , uiRight, pszBox + 2, sizeof( BYTE ) );
      hb_gtWriteAt( uiBottom, uiCol  , pszBox + 6, sizeof( BYTE ) );
      hb_gtWriteAt( uiBottom, uiRight, pszBox + 4, sizeof( BYTE ) );
   }

   uiCol = ( height > 1 ? uiLeft + 1 : uiLeft );

   if( uiCol <= uiRight )
   {
      hb_gtRepChar( uiRow, uiCol, pszBox[ 1 ], uiRight - uiLeft + ( height > 1 ? -1 : 1 ) );
      if( height > 1 )
         hb_gtRepChar( uiBottom, uiCol, pszBox[ 5 ], uiRight - uiLeft + ( height > 1 ? -1 : 1 ) );
   }

   if( pszBox[ 8 ] && height > 2 && width > 2 )
   {
      for( uiRow = uiTop + 1; uiRow < uiBottom; uiRow++ )
      {
         uiCol = uiLeft;
         hb_gtWriteAt( uiRow, uiCol++, pszBox + 7, sizeof( BYTE ) );
         hb_gtRepChar( uiRow, uiCol  , pszBox[ 8 ], uiRight - uiLeft - 1 );
         hb_gtWriteAt( uiRow, uiRight, pszBox + 3, sizeof( BYTE ) );
      }
   }
   else
   {
      for( uiRow = ( width > 1 ? uiTop + 1 : uiTop ); uiRow < ( width > 1 ? uiBottom : uiBottom + 1 ); uiRow++ )
      {
         hb_gtWriteAt( uiRow, uiLeft, pszBox + 7, sizeof( BYTE ) );
         if( width > 1 )
            hb_gtWriteAt( uiRow, uiRight, pszBox + 3, sizeof( BYTE ) );
      }
   }

/* speed issue for now */
/* hb_gtDispEnd(); */

   hb_gtSetPos( uiTopBak + 1, uiLeftBak + 1 );

   return 0;
}

int hb_gtBoxD( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight )
{
   return hb_gtBox( uiTop, uiLeft, uiBottom, uiRight, B_DOUBLE );
}

int hb_gtBoxS( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight )
{
   return hb_gtBox( uiTop, uiLeft, uiBottom, uiRight, B_SINGLE );
}

int hb_gtColorSelect( USHORT uiColorIndex )
{
   if( uiColorIndex > s_ColorCount )
      return 1;
   else
      s_uiColorIndex = uiColorIndex;

   return 0;
}

int hb_gtDispBegin( void )
{
/* ptucker */

   if( s_uiPreCount == 0 )
   {
      ++s_uiDispCount;
      hb_gt_DispBegin();
   }
   else
      ++s_uiPreCount;

   return 0;
}

USHORT hb_gtDispCount( void )
{
   return s_uiDispCount;
}

int hb_gtDispEnd( void )
{
/* ptucker */

   if( s_uiPreCount == 0 )
   {
      hb_gt_DispEnd();
      --s_uiDispCount;
   }
   else
      --s_uiPreCount;

   return 0;
}

int hb_gtPreExt( void )
{
/* ptucker */

   /* an external (printf...) write is about to take place */

   if( s_uiPreCNest == 0 )
   {
      if( s_uiPreCount == 0 )
      {
         USHORT uidc;

         uidc = s_uiPreCount = s_uiDispCount;

         while( uidc-- )
         {
            hb_gt_DispEnd();
            --s_uiDispCount;
         }
      }
      s_uiPreCNest = 1;
   }
   else
      ++s_uiPreCNest;

   return 0;

}

int hb_gtPostExt( void )
{
/* ptucker */

   if( s_uiPreCNest == 1 )
   {
      USHORT uidc = s_uiPreCount;

      while( uidc-- )
      {
         ++s_uiDispCount;
         hb_gt_DispBegin();
      }
      s_uiPreCount = 0;
      s_uiPreCNest = 0;
   }
   else
      --s_uiPreCNest;

   return 0;
}

int hb_gtGetColorStr( char * fpColorString )
{
/* ptucker */
   char * sColors;
   int i, k = 0;

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
   sColors[ k++ ] = '\0';

   strcpy( fpColorString, sColors );
   hb_xfree( sColors );

   return 0;
}

int hb_gtSetColorStr( char * fpColorString )
{
/* ptucker */
   char c, buff[ 6 ];
   BOOL bHasI = FALSE;
   BOOL bHasU = FALSE;
   BOOL bHasX = FALSE;
   BOOL bSlash = FALSE;
   int nPos = 0;
   int nFore = 0;
   int nColor = 0;
   int nCount = -1, i = 0, y;

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

   return 0;
}

int hb_gtGetCursor( USHORT * uipCursorShape )
{
   int i = hb_gt_GetCursorStyle();
   int rc = 0;

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

int hb_gtSetCursor( USHORT uiCursorShape )
{
   hb_gt_SetCursorStyle( uiCursorShape );

   return 0;
}

int hb_gtGetPos( USHORT * uipRow, USHORT * uipCol )
{
   *uipRow = s_uiCurrentRow = hb_gt_Row();
   *uipCol = s_uiCurrentCol = hb_gt_Col();

   return 0;
}

int hb_gtSetPos( USHORT uiRow, USHORT uiCol )
{
   /* TODO: in this situation Clipper just turns off the cursor */
   /* any further writes would be accounted for by clipping */
   if( uiRow > s_uiMaxRow || uiCol > s_uiMaxCol )
      return 1;

   s_uiCurrentRow = uiRow;
   s_uiCurrentCol = uiCol;

   hb_gt_SetPos( uiRow, uiCol );

   return 0;
}

BOOL hb_gtIsColor( void )
{
   return hb_gt_IsColor();
}

USHORT hb_gtMaxCol( void )
{
   return s_uiMaxCol = hb_gt_GetScreenWidth() - 1;
}

USHORT hb_gtMaxRow( void )
{
   return s_uiMaxRow = hb_gt_GetScreenHeight() - 1;
}

int hb_gtRectSize( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, USHORT * uipBuffSize )
{
   USHORT uiMRow = s_uiMaxRow;
   USHORT uiMCol = s_uiMaxCol;

   if( uiBottom > uiMRow )
      uiBottom = uiMRow;
   if( uiRight > uiMCol )
      uiRight = uiMCol;

   if( uiTop  > uiMRow   || uiBottom > uiMRow ||
       uiLeft > uiMCol   || uiRight  > uiMCol ||
       uiTop  > uiBottom || uiLeft   > uiRight )
   {
      return 1;
   }

   *uipBuffSize = ( uiBottom - uiTop + 1 ) * ( uiRight - uiLeft + 1 ) * 2;

   return 0;
}

int hb_gtRepChar( USHORT uiRow, USHORT uiCol, USHORT uiChar, USHORT uiCount )
{
   int rc;
   char buff[ 255 ];

   memset( buff, uiChar, uiCount );
   buff[ uiCount ] = 0;
   rc = hb_gtSetPos( uiRow, uiCol );
   if( rc != 0 )
      return rc;
   rc = hb_gtWrite( buff, uiCount );

   return rc;
}

int hb_gtRest( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, char * vlpScrBuff )
{
   hb_gt_PutText( uiTop, uiLeft, uiBottom, uiRight, vlpScrBuff );

   return 0;
}

int hb_gtSave( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, char * vlpScrBuff )
{
   hb_gt_GetText( uiTop, uiLeft, uiBottom, uiRight, vlpScrBuff );

   return 0;
}

int hb_gtScrDim( USHORT * uipHeight, USHORT * uipWidth )
{
   *uipHeight = s_uiMaxRow;
   *uipWidth = s_uiMaxCol;

   return 0;
}

int hb_gtGetBlink( BOOL * bBlink )
{
   *bBlink = hb_gt_GetBlink();

   return 0;
}

int hb_gtSetBlink( BOOL bBlink )
{
   hb_gt_SetBlink( bBlink );

   return 0;
}

int hb_gtSetMode( USHORT uiRows, USHORT uiCols )
{
/* ptucker */
   hb_gt_SetMode( uiRows, uiCols );
   s_uiMaxRow = hb_gtMaxRow();
   s_uiMaxCol = hb_gtMaxCol();

   return 0;
}

int hb_gtSetSnowFlag( BOOL bNoSnow )
{
   /* COMMENT: This is a compatibility function.
      If you're running on a CGA and snow is a problem
      speak up!
   */
   HB_SYMBOL_UNUSED( bNoSnow );

   return 0;
}

int hb_gtWrite( char * fpStr, ULONG length )
{
   int iRow, iCol, iMaxCol, iMaxRow;
   ULONG size = length;
   char attr = s_Color[ s_uiColorIndex ] & 0xFF;
   char *fpPointer = fpStr;

  /* TODO: this is doing more work than needed */

   /* Determine where the cursor is going to end up */
   iRow = s_uiCurrentRow;
   iCol = s_uiCurrentCol;
   iMaxRow = s_uiMaxRow;
   iMaxCol = s_uiMaxCol;

   length = ( length < iMaxCol-iCol+1 ) ? length : iMaxCol - iCol + 1;

   size = length;

   if( iCol + size > iMaxCol )
   {
      /* Calculate eventual row position and the remainder size for the column adjust */
      iRow += ( size / ( iMaxCol + 1 ) );
      size = size % ( iMaxCol + 1 );
   }
   iCol += size;
   if( iCol > iMaxCol )
   {
      /* Column movement overflows onto next row */
      iRow++;
      iCol -= ( iMaxCol + 1 );
   }
   /* If needed, prescroll the display to the new position and adjust the current row
      position to account for the prescroll */
   if( iRow > iMaxRow )
   {
      int iTemp;

      hb_gtScroll( 0, 0, iMaxRow, iMaxCol, iRow - iMaxRow, 0 );
      iTemp = s_uiCurrentRow - ( iRow - iMaxRow );
      if( iTemp < 0 )
      {
         /* The string is too long to fit on the screen. Only display part of it. */
         fpPointer += iMaxCol * abs( iTemp );
         iTemp = 0;
         if( s_uiCurrentCol > 0 )
         {
              /* Ensure that the truncated text will fill the screen */
            fpPointer -= s_uiCurrentCol;
            s_uiCurrentCol = 0;
         }
      }
      else size = length;

      /* Save the new starting row and the new ending row */
      s_uiCurrentRow = iTemp;
      iRow = iMaxRow;
   }
   else size = length;

   /* Now the text string can be displayed */
   hb_gt_Puts( s_uiCurrentRow, s_uiCurrentCol, attr, fpPointer, size );

   /* Finally, save the new cursor position */
   hb_gtSetPos( iRow, iCol );

   return 0;
}

int hb_gtWriteAt( USHORT uiRow, USHORT uiCol, char * fpStr, ULONG length )
{
   int rc;

   if( ( rc = hb_gtSetPos( uiRow, uiCol ) ) != 0 )
      return rc;

   return hb_gtWrite( fpStr, length );
}

int hb_gtWriteCon( char * fpStr, ULONG length )
{
   int rc = 0, nLen = 0;
   BOOL ldisp = FALSE;
   USHORT uiRow = s_uiCurrentRow, uiCol = s_uiCurrentCol;
   USHORT tmpRow = s_uiCurrentRow, tmpCol = s_uiCurrentCol;
   int ch;
   char * fpPtr = fpStr;
   #define STRNG_SIZE 500
   char strng[ STRNG_SIZE ];

   while( length-- )
   {
      ch = *fpPtr++;
      switch( ch )
      {
         case 7:
            break;
         case 8:
/*
            COMMENT: Clipper does not scroll backwards up the screen!
            if( uiCol > 0 ) uiCol--;
            else if( uiRow > 0 )
            {
               uiRow--;
               uiCol = s_uiMaxCol;
            }
            else
            {
               hb_gtScroll( 0, 0, s_uiMaxRow, s_uiMaxCol, -1, 0 );
               uiCol = s_uiMaxCol;
            }
*/
            if( nLen > 0 )
                --nLen;
            if( uiCol > 0 )
                --uiCol;

            break;
         case 10:
/*
            if( uiRow < s_uiMaxRow ) uiRow++;
            else
               hb_gtScroll( 0, 0, s_uiMaxRow, s_uiMaxCol, 1, 0 );

            hb_gtSetPos( uiRow, uiCol );
*/
            ++uiRow;
            ldisp = TRUE;
            break;

         case 13:
            uiCol = 0;
            if( *fpPtr != '\n') ldisp = TRUE;
            break;

         default:
            if( ++uiCol > s_uiMaxCol )
            {
               uiCol = 0;
               ++uiRow;
               ldisp = TRUE;
            }
            strng[ nLen++ ] = ch;
            if( nLen >= STRNG_SIZE ) ldisp = TRUE;
      }
      if( ldisp || ! length )
      {
         if( nLen )
            rc = hb_gtWrite( strng, nLen );
         nLen = 0;
         if( uiRow > s_uiMaxRow )
         {
            hb_gtScroll( 0, 0, s_uiMaxRow, s_uiMaxCol, uiRow - s_uiMaxRow, 0 );
            uiRow = s_uiMaxRow;
            uiCol = 0;
         }
         tmpRow = uiRow; tmpCol = uiCol;
         hb_gtSetPos( uiRow, uiCol );
         ldisp = FALSE;
      }
      if( rc )
         break;
   }

   return rc;
}

int hb_gtScroll( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, SHORT iRows, SHORT iCols )
{
   hb_gt_Scroll( uiTop, uiLeft, uiBottom, uiRight, s_Color[ s_uiColorIndex ], iRows, iCols );
   return 0;
}

#ifdef TEST
void main( void )
{
   char * test = "Testing GT API Functions";
   char * test2 = "This message wraps!";
   int iRow, iCol;

   /* NOTE: always have to initialze video subsystem */
   hb_gtInit();

   /* save screen (doesn't work under DOS) */
   /*
   char * scr;
   USHORT size;

   hb_gtRectSize( 1, 1, hb_gtMaxRow(), hb_gtMaxCol(), &size );
   scr = ( char * ) hb_xgrab( size );
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
