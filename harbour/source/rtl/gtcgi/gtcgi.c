/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem for plain ANSI C stream IO
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
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

/* NOTE: User programs should never call this layer directly! */

/* TODO: include any standard headers here */

#include "hbapifs.h"
#include "hbapigt.h"

static SHORT  s_iRow;
static SHORT  s_iCol;
static USHORT s_uiMaxRow;
static USHORT s_uiMaxCol;
static USHORT s_uiCursorStyle;
static BOOL   s_bBlink;
static int    s_iFilenoStdout;
static USHORT s_uiDispCount;
static char * s_szCrLf;
static ULONG  s_ulCrLf;

void hb_gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init()"));

   HB_SYMBOL_UNUSED( iFilenoStdin );
   HB_SYMBOL_UNUSED( iFilenoStderr );

   s_uiDispCount = 0;

   s_iRow = 0;
   s_iCol = 0;
   s_uiMaxRow = 32767;
   s_uiMaxCol = 32767;
   s_uiCursorStyle = SC_NORMAL;
   s_bBlink = FALSE;
   s_iFilenoStdout = iFilenoStdout;
   hb_fsSetDevMode( s_iFilenoStdout, FD_BINARY );

   s_szCrLf = hb_conNewLine();
   s_ulCrLf = strlen( s_szCrLf );
   
   hb_mouse_Init();
}

void hb_gt_Exit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Exit()"));

   hb_mouse_Exit();
}

static void out_stdout( char * pStr, ULONG ulLen )
{
   unsigned uiErrorOld = hb_fsError(); /* Save current user file error code */
   hb_fsWriteLarge( s_iFilenoStdout, ( BYTE * ) pStr, ulLen );
   hb_fsSetError( uiErrorOld );        /* Restore last user file error code */
}

static void out_newline( void )
{
   out_stdout( s_szCrLf, s_ulCrLf );
}

int hb_gt_ExtendedKeySupport()
{
   return 0;
}
int hb_gt_ReadKey( HB_inkey_enum eventmask )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey(%d)", (int) eventmask));

   HB_SYMBOL_UNUSED( eventmask );

   return 13;
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
   hb_gt_SetPos( row, col, HB_GT_SET_POS_AFTER );
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

void hb_gt_SetPos( SHORT iRow, SHORT iCol, SHORT iMethod )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd, %hd)", iRow, iCol, iMethod));

   if( iMethod == HB_GT_SET_POS_BEFORE )
   {
      /* Only set the screen position when the cursor
         position is changed BEFORE text is displayed.
      */
      if( iRow != s_iRow )
      {
         out_newline();
         s_iCol = 0;
      }
      if( s_iCol < iCol ) while( ++s_iCol < iCol ) out_stdout( " ", 1 );
   }

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
   char szBuffer[ 2 ];

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_xPutch(%hu, %hu, %d, %i)", uiRow, uiCol, (int) byAttr, byAttr));

   HB_SYMBOL_UNUSED( byAttr );

   hb_gt_SetPos( uiRow, uiCol, HB_GT_SET_POS_BEFORE );

   szBuffer[ 0 ] = byChar;
   szBuffer[ 1 ] = '\0';
   out_stdout( szBuffer, 1 );

   hb_gt_AdjustPos( ( BYTE * ) szBuffer, 1 );
}

void hb_gt_Puts( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE * pbyStr, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu)", uiRow, uiCol, (int) byAttr, pbyStr, ulLen));

   HB_SYMBOL_UNUSED( byAttr );

   hb_gt_SetPos( uiRow, uiCol, HB_GT_SET_POS_BEFORE );

   out_stdout( ( char * ) pbyStr, ulLen );

   hb_gt_AdjustPos( pbyStr, ulLen );
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

   HB_SYMBOL_UNUSED( uiTop );
   HB_SYMBOL_UNUSED( uiLeft );
   HB_SYMBOL_UNUSED( uiBottom );
   HB_SYMBOL_UNUSED( uiRight );
   HB_SYMBOL_UNUSED( byAttr );
   HB_SYMBOL_UNUSED( iRows );
   HB_SYMBOL_UNUSED( iCols );

   out_newline();

   s_iRow = 0;
   s_iRow = 0;
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
   USHORT  uiRow;
   USHORT  uiCol;
   USHORT  uiHeight;
   USHORT  uiWidth;

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
   {
      hb_gt_xPutch( uiRow, uiRight, byAttr, szBox[ 2 ] ); /* Upper right corner */
   }

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
         {
            hb_gt_Replicate( uiRow, uiCol, byAttr, ' ', uiWidth - 2 );
            hb_gt_xPutch( uiRow, uiRight, byAttr, szBox[ 3 ] ); /* Right side */
         }
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

BOOL hb_gt_Suspend()
{
   return TRUE;
}

BOOL hb_gt_Resume()
{
   return TRUE;
}
