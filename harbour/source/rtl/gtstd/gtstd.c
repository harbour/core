/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem for plain ANSI C stream IO
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
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

/* NOTE: User programs should never call this layer directly! */

/* TODO: include any standard headers here */

#include "hbapifs.h"
#include "hbapigt.h"

#if defined( OS_UNIX_COMPATIBLE )
   #include <unistd.h>  /* read() function requires it */
   #include <termios.h>
#endif

static SHORT  s_iRow;
static SHORT  s_iCol;
static USHORT s_uiMaxRow;
static USHORT s_uiMaxCol;
static USHORT s_uiCursorStyle;
static BOOL   s_bBlink;
static int    s_iFilenoStdout;
static USHORT s_uiDispCount;

#if defined( OS_UNIX_COMPATIBLE )
static struct termios startup_attributes;
#endif

void hb_gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init()"));

   HB_SYMBOL_UNUSED( iFilenoStdin );
   HB_SYMBOL_UNUSED( iFilenoStderr );

#if defined( OS_UNIX_COMPATIBLE )
   {
      struct termios ta;
      
      tcgetattr( STDIN_FILENO, &startup_attributes );
//      atexit( restore_input_mode );
      
      tcgetattr( STDIN_FILENO, &ta );
      ta.c_lflag &= ~( ICANON | ECHO );
      ta.c_iflag &= ~ICRNL;
      ta.c_cc[ VMIN ] = 0;
      ta.c_cc[ VTIME ] = 0;
      tcsetattr( STDIN_FILENO, TCSAFLUSH, &ta );
   }
#endif

   s_uiDispCount = 0;

   s_iRow = 0;
   s_iCol = 0;
#if defined(OS_UNIX_COMPATIBLE)
   s_uiMaxRow = 24;
#else
   s_uiMaxRow = 25;
#endif
   s_uiMaxCol = 80;
   s_uiCursorStyle = SC_NORMAL;
   s_bBlink = FALSE;
   s_iFilenoStdout = iFilenoStdout;
   hb_fsSetDevMode( s_iFilenoStdout, FD_BINARY );
}

void hb_gt_Exit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Exit()"));

#if defined( OS_UNIX_COMPATIBLE )
   tcsetattr( STDIN_FILENO, TCSANOW, &startup_attributes );
#endif
}

int hb_gt_ReadKey( HB_inkey_enum eventmask )
{
   int ch;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey(%d)", (int) eventmask));

   HB_SYMBOL_UNUSED( eventmask );

#if defined(OS_UNIX_COMPATIBLE)
   if( ! read( STDIN_FILENO, &ch, 1 ) )
      ch = 0;
#else
   ch = 0;
#endif

   /* TODO: */

   return ch;
}

BOOL hb_gt_AdjustPos( BYTE * pStr, ULONG ulLen )
{
   USHORT row = s_iRow;
   USHORT col = s_iCol;
   ULONG ulCount;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_AdjustPos(%s, %lu)", pStr, ulLen ));

   for( ulCount = 0; ulCount < ulLen; ulCount++ )
   {
      switch( *pStr++  )
      {
         case HB_CHAR_BEL:
            break;

         case HB_CHAR_BS:
            if( col )
               col--;
            else
            {
               col = s_uiMaxCol;
               if( row )
                  row--;
            }
            break;

         case HB_CHAR_LF:
            if( row < s_uiMaxRow )
               row++;
            break;

         case HB_CHAR_CR:
            col = 0;
            break;

         default:
            if( col < s_uiMaxCol )
               col++;
            else
            {
               col = 0;
               if( row < s_uiMaxRow )
                  row++;
            }
      }
   }
   hb_gt_SetPos( row, col );
   return TRUE;
}

BOOL hb_gt_IsColor( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_IsColor()"));

   return FALSE;
}

USHORT hb_gt_GetScreenWidth( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth()"));

   return s_uiMaxCol;
}

USHORT hb_gt_GetScreenHeight( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeight()"));

   return s_uiMaxRow;
}

void hb_gt_SetPos( SHORT iRow, SHORT iCol )
{
   SHORT iCount;
   SHORT iDevRow = s_iRow;
   SHORT iDevCol = s_iCol;

   char * szCrLf = hb_conNewLine();

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd)", iRow, iCol));

   if( iRow < iDevRow || iCol < iDevCol )
   {
      fputs( szCrLf, stdout );
      iDevCol = 0;
      iDevRow++;
   }
   else if( iRow > iDevRow )
      iDevCol = 0;

   for( iCount = iDevRow; iCount < iRow; iCount++ )
      fputs( szCrLf, stdout );
   for( iCount = iDevCol; iCount < iCol; iCount++ )
      fputc( ' ', stdout );

   fflush( stdout );

   s_iRow = iRow;
   s_iCol = iCol;
}

SHORT hb_gt_Col( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));

   return s_iCol;
}

SHORT hb_gt_Row( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));

   return s_iRow;
}

USHORT hb_gt_GetCursorStyle( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorStyle()"));

   return s_uiCursorStyle;
}

void hb_gt_SetCursorStyle( USHORT uiCursorStyle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorStyle(%hu)", uiCursorStyle));

   s_uiCursorStyle = uiCursorStyle;
}

static void hb_gt_xPutch( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE byChar )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_xPutch(%hu, %hu, %d, %i)", uiRow, uiCol, (int) byAttr, byAttr));

   /* TODO: */

   HB_SYMBOL_UNUSED( uiRow );
   HB_SYMBOL_UNUSED( uiCol );
   HB_SYMBOL_UNUSED( byAttr );
   HB_SYMBOL_UNUSED( byChar );
}

void hb_gt_Puts( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE * pbyStr, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu)", uiRow, uiCol, (int) byAttr, pbyStr, ulLen));

   /* TODO: */

   HB_SYMBOL_UNUSED( uiRow );
   HB_SYMBOL_UNUSED( uiCol );
   HB_SYMBOL_UNUSED( byAttr );
   HB_SYMBOL_UNUSED( pbyStr );
   HB_SYMBOL_UNUSED( ulLen );
}

int hb_gt_RectSize( USHORT rows, USHORT cols )
{
   return rows * cols * 2;
}

void hb_gt_GetText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbyDst )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetText(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pbyDst));

   HB_SYMBOL_UNUSED( uiTop );
   HB_SYMBOL_UNUSED( uiLeft );
   HB_SYMBOL_UNUSED( uiBottom );
   HB_SYMBOL_UNUSED( uiRight );
   HB_SYMBOL_UNUSED( pbyDst );
}

void hb_gt_PutText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbySrc )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pbySrc));

   /* TODO: */

   HB_SYMBOL_UNUSED( uiTop );
   HB_SYMBOL_UNUSED( uiLeft );
   HB_SYMBOL_UNUSED( uiBottom );
   HB_SYMBOL_UNUSED( uiRight );
   HB_SYMBOL_UNUSED( pbySrc );
}

void hb_gt_SetAttribute( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %d)", uiTop, uiLeft, uiBottom, uiRight, (int) byAttr));

   HB_SYMBOL_UNUSED( uiTop );
   HB_SYMBOL_UNUSED( uiLeft );
   HB_SYMBOL_UNUSED( uiBottom );
   HB_SYMBOL_UNUSED( uiRight );
   HB_SYMBOL_UNUSED( byAttr );
}

void hb_gt_Scroll( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr, SHORT iRows, SHORT iCols )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hu, %hu)", uiTop, uiLeft, uiBottom, uiRight, (int) byAttr, iRows, iCols));

   HB_SYMBOL_UNUSED( byAttr );

   /* TODO: */

   if( uiTop == 0 &&
       uiBottom == s_uiMaxRow &&
       uiLeft == 0 &&
       uiRight == s_uiMaxCol &&
       iRows == 0 &&
       iCols == 0 )
   {
      for( ; uiBottom; uiBottom-- )
        fputs( hb_conNewLine(), stdout );

      fflush( stdout );

      s_iRow = 0;
      s_iCol = 0;
   }
}

void hb_gt_DispBegin( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispBegin()"));

   ++s_uiDispCount;
   /* Do nothing else */
}

void hb_gt_DispEnd()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispEnd()"));

   --s_uiDispCount;
   /* Do nothing else */
}

BOOL hb_gt_SetMode( USHORT uiMaxRow, USHORT uiMaxCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", uiMaxRow, uiMaxCol));

   s_uiMaxRow = uiMaxRow;
   s_uiMaxCol = uiMaxCol;

   return FALSE;
}

BOOL hb_gt_GetBlink()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetBlink()"));

   return s_bBlink;
}

void hb_gt_SetBlink( BOOL bBlink )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetBlink(%d)", (int) bBlink));

   s_bBlink = bBlink;
}

void hb_gt_Tone( double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Tone(%lf, %lf)", dFrequency, dDuration));

   /* TODO: Implement this */

   HB_SYMBOL_UNUSED( dFrequency );
   HB_SYMBOL_UNUSED( dDuration );
}

char * hb_gt_Version( void )
{
   return "Harbour Terminal: Standard stream console";
}

USHORT hb_gt_DispCount()
{
   return s_uiDispCount;
}

void hb_gt_Replicate( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE byChar, ULONG nLength )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Replicate(%hu, %hu, %i, %i, %lu)", uiRow, uiCol, byAttr, byChar, nLength));

   while( nLength-- )
      hb_gt_xPutch( uiRow, uiCol++, byAttr, byChar );
}

USHORT hb_gt_Box( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight,
                  BYTE *szBox, BYTE byAttr )
{
   USHORT uiRow;
   USHORT uiCol;
   USHORT uiHeight;
   USHORT uiWidth;

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

   hb_gt_DispBegin();

   if( uiHeight > 1 && uiWidth > 1 )
      hb_gt_xPutch( uiRow, uiCol, byAttr, szBox[ 0 ] ); /* Upper left corner */

   uiCol = ( uiHeight > 1 ? uiLeft + 1 : uiLeft );

   if( uiCol <= uiRight )
      hb_gt_Replicate( uiRow, uiCol, byAttr, szBox[ 1 ], uiRight - uiLeft + ( uiHeight > 1 ? -1 : 1 ) ); /* Top line */

   if( uiHeight > 1 && uiWidth > 1 )
      hb_gt_xPutch( uiRow, uiRight, byAttr, szBox[ 2 ] ); /* Upper right corner */

   if( szBox[ 8 ] && uiHeight > 2 && uiWidth > 2 )
   {
      for( uiRow = uiTop + 1; uiRow < uiBottom; uiRow++ )
      {
         uiCol = uiLeft;
         hb_gt_xPutch( uiRow, uiCol++, byAttr, szBox[ 7 ] ); /* Left side */
         hb_gt_Replicate( uiRow, uiCol, byAttr, szBox[ 8 ], uiRight - uiLeft - 1 ); /* Fill */
         hb_gt_xPutch( uiRow, uiRight, byAttr, szBox[ 3 ] ); /* Right side */
      }
   }
   else
   {
      for( uiRow = ( uiWidth > 1 ? uiTop + 1 : uiTop ); uiRow < ( uiWidth > 1 ? uiBottom : uiBottom + 1 ); uiRow++ )
      {
         hb_gt_xPutch( uiRow, uiLeft, byAttr, szBox[ 7 ] ); /* Left side */
         if( uiWidth > 1 )
            hb_gt_xPutch( uiRow, uiRight, byAttr, szBox[ 3 ] ); /* Right side */
      }
   }

   if( uiHeight > 1 && uiWidth > 1 )
   {
      hb_gt_xPutch( uiBottom, uiLeft, byAttr, szBox[ 6 ] ); /* Bottom left corner */

      uiCol = ( uiHeight > 1 ? uiLeft + 1 : uiLeft );

      if( uiCol <= uiRight && uiHeight > 1 )
         hb_gt_Replicate( uiBottom, uiCol, byAttr, szBox[ 5 ], uiRight - uiLeft + ( uiHeight > 1 ? -1 : 1 ) ); /* Bottom line */

      hb_gt_xPutch( uiBottom, uiRight, byAttr, szBox[ 4 ] ); /* Bottom right corner */
   }

   hb_gt_DispEnd();

   return 0;
}

USHORT hb_gt_BoxD( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbyFrame, BYTE byAttr )
{
   return hb_gt_Box( uiTop, uiLeft, uiBottom, uiRight, pbyFrame, byAttr );
}

USHORT hb_gt_BoxS( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbyFrame, BYTE byAttr )
{
   return hb_gt_Box( uiTop, uiLeft, uiBottom, uiRight, pbyFrame, byAttr );
}

USHORT hb_gt_HorizLine( USHORT uiRow, USHORT uiLeft, USHORT uiRight, BYTE byChar, BYTE byAttr )
{
   if( uiLeft < uiRight )
      hb_gt_Replicate( uiRow, uiLeft, byAttr, byChar, uiRight - uiLeft + 1 );
   else
      hb_gt_Replicate( uiRow, uiRight, byAttr, byChar, uiLeft - uiRight + 1 );

   return 0;
}

USHORT hb_gt_VertLine( USHORT uiCol, USHORT uiTop, USHORT uiBottom, BYTE byChar, BYTE byAttr )
{
   USHORT uRow;

   if( uiTop <= uiBottom )
      uRow = uiTop;
   else
   {
      uRow = uiBottom;
      uiBottom = uiTop;
   }

   while( uRow <= uiBottom )
      hb_gt_xPutch( uRow++, uiCol, byAttr, byChar );

   return 0;
}

BOOL hb_gt_PreExt()
{
   return TRUE;
}

BOOL hb_gt_PostExt()
{
   return TRUE;
}
