/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem based on Slang screen library.
 *
 * Copyright 2000 Marek Paliwoda <paliwoda@inetia.pl>
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

#ifdef __linux__
   #include <slang/slang.h>
#else
   #include <slang.h>
#endif
/* missing defines in previous versions of Slang - this was not TESTED !! */
#if SLANG_VERSION < 10400
   typedef unsigned short SLsmg_Char_Type;
   #define SLSMG_EXTRACT_CHAR( x ) ( ( x ) & 0xFF )
   #define SLSMG_EXTRACT_COLOR( x ) ( ( ( x ) >> 8 ) & 0xFF )
   #define SLSMG_BUILD_CHAR( ch, color ) ( ( ( SLsmg_Char_Type ) ( unsigned char )( ch ) ) | ( ( color ) << 8 ) )
   #define SLSMG_BOARD_CHAR   'h'
   #define SLSMG_BLOCK_CHAR   '0'
#endif

#include <unistd.h>
#include <signal.h>

#include "hbapigt.h"
#include "inkey.ch"

/* if we can not manipulate cursor state */
#define SC_UNAVAIL -1

/* Slang reserves color 0 and 1 as a normal
   and reverse color so we can't use them
*/
#define SLANG_RESERVED_COLORS 2

#ifdef IBMPC_SYSTEM
   int SLtt_Has_Alt_Charset = 1;
   char * SLtt_Graphics_Char_Pairs = "";
#endif

extern int hb_gt_Init_Terminal( int phase );
static void hb_gt_build_conv_tabs();

static USHORT s_uiDispCount;
static SHORT s_sCursorStyle = SC_NORMAL;
static BOOL s_linuxConsole = FALSE;
static BOOL s_underXTerm = FALSE;
/* indicate if we are currently running a command from system */
static BOOL s_bSuspended = FALSE;

/* to convert high characters (mostly graphics and control chars) */
static unsigned char s_convHighChars[ 256 ];
/* to convert colors to Clipper mode */
static char * s_colorNames[] =
{
   "black"        ,
   "blue"         ,
   "green"        ,
   "cyan"         ,
   "red"          ,
   "magenta"      ,
   "brown"        ,
   "lightgray"    ,
   "gray"         ,
   "brightblue"   ,
   "brightgreen"  ,
   "brightcyan"   ,
   "brightred"    ,
   "brightmagenta",
   "yellow"       ,
   "white"
};

volatile BOOL hb_gt_sln_bScreen_Size_Changed = FALSE;
#ifndef IBMPC_SYSTEM
/* window's resize handler */
static void sigwinch_handler( int sig )
{
   hb_gt_sln_bScreen_Size_Changed = TRUE;
   SLsignal( SIGWINCH, sigwinch_handler );
}
#endif

/* I think this function should not be void. It should be BOOL */
void hb_gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr )
{
   BOOL gt_Inited = FALSE;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init()"));

   s_uiDispCount = 0;

   /* read a terminal descripion from a terminfo database */
   SLtt_get_terminfo();
   /* initialize higher-level Slang routines */
   if( SLkp_init() != -1 )
   {
      /* initialize a terminal stuff and a Slang
         keyboard subsystem for the first time
      */
      if( hb_gt_Init_Terminal( 0 ) )
      {
         /* initialize a screen handling subsytem */
         if( SLsmg_init_smg() != -1 )
         {
#ifndef IBMPC_SYSTEM
            /* install window resize handler */
            SLsignal( SIGWINCH, sigwinch_handler );
#endif

            /* do not indicate USER_BREAK in SLang_Error - ??? */
            SLang_Ignore_User_Abort = 1;

            /* default abort procesing */
            SLang_set_abort_signal( NULL );

            /* NOTE: this is incompatible with CLIPPER
               but under Unix we should assume cursor is
               visible on startup because we cannot figure
               out a current cursor state
            */
            /* turn on a cursor visibility */
            if( SLtt_set_cursor_visibility( 1 ) == -1 )
               s_sCursorStyle = SC_UNAVAIL;

            /* an uncertain way to check if we run under linux console */
            s_linuxConsole = ( strncmp( getenv( "TERM" ), "linux", 5 ) == 0 );
            /* an uncertain way to check if we run under xterm */
            s_underXTerm = ( strstr( getenv( "TERM" ), "xterm" ) != NULL );

            /* NOTE: this driver is implemented in a way that it is
               imposible to get intensity/blinking background mode
               under Slang, due to a way Slang is written. This is
               incompatible with Clipper.
            */
#ifndef IBMPC_SYSTEM
            SLtt_Blink_Mode = 0;
            SLtt_Use_Blink_For_ACS = 0;
#endif

            /* initialize conversion tables */
            hb_gt_build_conv_tabs();

            /* NOTE: due to a work of a Slang library which does not
               prepare its internal screen buffer properly, a screen
               must be cleared before normal work. This is not
               compatible with Clipper
            */
#ifndef IBMPC_SYSTEM
            /* ensure we are in a normal chars set */
            SLtt_set_alt_char_set( 0 );
#endif
            SLsmg_cls();
            /* SLsmg_set_color( 7 ); */
            SLsmg_gotorc( 0, 0 );
            SLsmg_refresh();
            gt_Inited = TRUE;
         }
      }
   }

   hb_mouse_Init();

   if( ! gt_Inited )
   {
      char * errmsg = '\r'+'\n'+"Internal error : screen driver initialization failure"+'\r'+'\n'+( char )0;
      /* something went wrong - restore default settings */
      SLang_reset_tty();
      /* NOTE: a standard Harbour error should be generated here ! */
      write( iFilenoStderr, errmsg , strlen( errmsg ) );
      exit( 20 );
   }
}

void hb_gt_Exit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Exit()"));

/*
   NOTE: This is incompatible with Clipper
   - on exit leave a cursor visible
*/

   hb_mouse_Exit();

   if( s_sCursorStyle != SC_UNAVAIL )
      hb_gt_SetCursorStyle( SC_NORMAL );

   SLsmg_refresh();
   SLsmg_reset_smg();
   SLang_reset_tty();
}

BOOL hb_gt_AdjustPos( BYTE * pStr, ULONG ulLen )
{
   ULONG ulCount;
   USHORT row = SLsmg_get_row();
   USHORT col = SLsmg_get_column();

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_AdjustPos(%s, %lu)", pStr, ulLen ));

   for( ulCount = 0; ulCount < ulLen; ulCount++ )
   {
      switch( *pStr++ )
      {
         case HB_CHAR_BEL:
            break;

         case HB_CHAR_BS:
            if( col )
               col--;
            else
            {
               col = SLtt_Screen_Cols - 1;
               if( row )
                  row--;
            }
            break;

         case HB_CHAR_LF:
            col = 0;
            /* This is a hack. OutStd() is done outside Slang and it
               can't be tracked currently by Slang. This should be
               changed in console.c
            */
            SLtt_write_string( "\r" );
            if( row < SLtt_Screen_Rows - 1 )
               row++;
            break;

         case HB_CHAR_CR:
            col = 0;
            break;

         default:
            if( col < SLtt_Screen_Cols - 1 )
               col++;
            else
            {
               col = 0;
               if( row < SLtt_Screen_Rows - 1 )
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

   return SLtt_Use_Ansi_Colors;
}

USHORT hb_gt_GetScreenWidth( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth()"));

   return SLtt_Screen_Cols;
}

USHORT hb_gt_GetScreenHeight( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeight()"));

   return SLtt_Screen_Rows;
}

void hb_gt_SetPos( SHORT iRow, SHORT iCol, SHORT iMethod )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd, %hd)", iRow, iCol, iMethod));

   HB_SYMBOL_UNUSED( iMethod );

   SLsmg_gotorc( iRow, iCol );
   /* SLtt_goto_rc( iRow, iCol ); */ /* ??? */

   if( s_uiDispCount == 0 )
      SLsmg_refresh();
}

SHORT hb_gt_Col( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));

   return SLsmg_get_column();
}

SHORT hb_gt_Row( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));

   return SLsmg_get_row();
}

USHORT hb_gt_GetCursorStyle( void )
{
   /* TODO: What shape is the cursor? */

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorStyle()"));

   /* if we don't know a cursor state - assume visible */
   if( s_sCursorStyle == SC_UNAVAIL )
      return( SC_NORMAL );

   return( s_sCursorStyle );
}

void hb_gt_SetCursorStyle( USHORT uiStyle )
{
   /* keyseq to define cursor shape under linux console */
   static char cursDefseq[] = { 27, '[', '?', '1', 'c', 0 };

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorStyle(%hu)", uiStyle));

   /* TODO: How to set the shape of the cursor? */
   /* see ..\..\..\tests\working\cursrtst.prg for an explanation */
   if( s_sCursorStyle == SC_UNAVAIL )
      return;

   if( ( s_sCursorStyle >= SC_NONE ) && ( s_sCursorStyle <= SC_SPECIAL2 ) )
   {
      s_sCursorStyle = uiStyle;
      SLtt_set_cursor_visibility( s_sCursorStyle != SC_NONE );

      if( s_linuxConsole )
      {
         switch( uiStyle )
         {
         case SC_NONE:
            cursDefseq[ 3 ] = '1';
            break;

         case SC_NORMAL:
            cursDefseq[ 3 ] = '2';
            break;

         case SC_INSERT:
            cursDefseq[ 3 ] = '4';
            break;

         case SC_SPECIAL1:
            cursDefseq[ 3 ] = '8';
            break;

         case SC_SPECIAL2:
            /* TODO: find a proper sequqnce to set a cursor
               to SC_SPECIAL2 under Linux console
            */
            cursDefseq[ 3 ] = '4';
            break;
         }

         SLtt_write_string( cursDefseq );

         if( s_uiDispCount == 0 )
            SLsmg_refresh();
      }
   }
}

static void hb_gt_xPutch( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE byChar )
{
   SLsmg_Char_Type SLchar;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_xPutch(%hu, %hu, %d, %i)", uiRow, uiCol, (int) byAttr, byChar));

   /* build a Slang converted char - note we are clearing a high bit of color */
   SLchar = SLSMG_BUILD_CHAR( s_convHighChars[ byChar ],
               ( (int)byAttr + SLANG_RESERVED_COLORS ) & 0x7F );

   /* alternate char set */
   if( byChar > 127 )
      SLchar |= 0x8000;

   SLsmg_gotorc( uiRow, uiCol );
   SLsmg_write_raw( &SLchar, 1 );
}

void hb_gt_Puts( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE * pbyStr, ULONG ulLen )
{
   ULONG i;
   BYTE byChar;
   SLsmg_Char_Type SLchar, * pScr;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu)", uiRow, uiCol, (int) byAttr, pbyStr, ulLen));

   pScr = ( SLsmg_Char_Type * ) hb_xgrab( ( ulLen + 1 ) * sizeof( SLsmg_Char_Type ) );

   for( i = 0; i < ulLen; i++ )
   {
      /* next char to process */
      byChar = *pbyStr++;

      /* build a Slang converted char - note we are clearing a high bit of color */
      SLchar = SLSMG_BUILD_CHAR( s_convHighChars[ byChar ],
                  ( (int)byAttr + SLANG_RESERVED_COLORS ) & 0x7F );

      /* alternate char set */
      if( byChar > 127 )
         SLchar |= 0x8000;

      *( pScr + i ) = SLchar;
   }

   SLsmg_gotorc( uiRow, uiCol );

   if( ulLen > 0 )
      SLsmg_write_raw( pScr, ulLen );
/*
   SLsmg_gotorc( uiRow, uiCol + ulLen );

   if( s_uiDispCount == 0 )
      SLsmg_refresh();
*/
   hb_gt_SetPos( uiRow, uiCol + ulLen, HB_GT_SET_POS_AFTER );

   hb_xfree( ( BYTE * )pScr );
}

int hb_gt_RectSize( USHORT rows, USHORT cols )
{
   return rows * cols * sizeof( SLsmg_Char_Type );
}

void hb_gt_GetText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbyDst )
{
   int Cols;
   USHORT usSavRow = SLsmg_get_row();
   USHORT usSavCol = SLsmg_get_column();
   SLsmg_Char_Type * pBuf = ( SLsmg_Char_Type * ) pbyDst;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetText(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pbyDst));

   Cols = uiRight - uiLeft + 1;
   while( uiTop <= uiBottom )
   {
      SLsmg_gotorc( uiTop, uiLeft );
      SLsmg_read_raw( pBuf, Cols );
      pBuf += Cols;
      ++uiTop;
   }
   SLsmg_gotorc( usSavRow, usSavCol );
}

void hb_gt_PutText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * pbySrc )
{
   int Cols;
   USHORT usSavRow = SLsmg_get_row();
   USHORT usSavCol = SLsmg_get_column();
   SLsmg_Char_Type * pBuf = ( SLsmg_Char_Type * ) pbySrc;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pbySrc));

   Cols = uiRight - uiLeft + 1;
   while( uiTop <= uiBottom )
   {
      SLsmg_gotorc( uiTop, uiLeft );
      SLsmg_write_raw( pBuf, Cols );
      pBuf += Cols;
      ++uiTop;
   }
/*
   SLsmg_gotorc( usSavRow, usSavCol );

   if( s_uiDispCount == 0 )
      SLsmg_refresh();
*/
   hb_gt_SetPos( usSavRow, usSavCol, HB_GT_SET_POS_AFTER );
}

void hb_gt_SetAttribute( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr )
{
   int Rows, Cols;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetAttribute(%hu, %hu, %hu, %hu, %d)", uiTop, uiLeft, uiBottom, uiRight, (int) byAttr));

   Rows = uiBottom - uiTop + 1;
   Cols = uiRight - uiLeft + 1;

   /* note: we are clearing a high bit of color */
   SLsmg_set_color_in_region( ( (int)byAttr + SLANG_RESERVED_COLORS ) & 0x7F,
      uiTop, uiLeft, Rows, Cols );

   if( s_uiDispCount == 0 )
      SLsmg_refresh();
}

void hb_gt_Scroll( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE byAttr, SHORT iRows, SHORT iCols )
{
   /* this work is based on gtdos.c, but changed to get scroll
      worked well when scrolling horizontally. Clipper behaves
      strange here.
   */
   SHORT usSaveRow, usSaveCol;
   USHORT uiSize;

   int iLength = ( usRight - usLeft ) + 1;
   int iCount, iColOld, iColNew, iColSize;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hd, %hd)", uiTop, uiLeft, uiBottom, uiRight, (int) byAttr, iRows, iCols));

   if( hb_gtRectSize( usTop, usLeft, usBottom, usRight, &uiSize ) == 0 )
   {
      unsigned char * fpBlank = ( unsigned char * ) hb_xgrab( iLength );
      unsigned char * fpBuff = ( unsigned char * ) hb_xgrab( iLength * sizeof( SLsmg_Char_Type ) );

      memset( fpBlank, ' ', iLength );

      iColOld = iColNew = usLeft;
      if( iCols >= 0 )
      {
         iColOld += iCols;
         iColSize = ( int ) ( usRight - usLeft );
         iColSize -= iCols;
      }
      else
      {
         iColNew -= iCols;
         iColSize = ( int ) ( usRight - usLeft );
         iColSize += iCols;
      }

      hb_gtGetPos( &usSaveRow, &usSaveCol );

      for( iCount = ( iRows >= 0 ? usTop : usBottom );
           ( iRows >= 0 ? iCount <= usBottom : iCount >= usTop );
           ( iRows >= 0 ? iCount++ : iCount-- ) )
      {
         int iRowPos = iCount + iRows;

         /* Read the text to be scrolled into the current row */
         if( ( iRows || iCols ) && iRowPos <= usBottom && iRowPos >= usTop )
            hb_gt_GetText( iRowPos, iColOld, iRowPos, iColOld + iColSize, fpBuff );

         /* Blank the scroll region in the current row */
         hb_gt_Puts( iCount, usLeft, byAttr, fpBlank, iLength );

         /* Write the scrolled text to the current row */
         if( ( iRows || iCols ) && iRowPos <= usBottom && iRowPos >= usTop )
            hb_gt_PutText( iCount, iColNew, iCount, iColNew + iColSize, fpBuff );
      }

      hb_xfree( fpBlank );
      hb_xfree( fpBuff );

      hb_gtSetPos( usSaveRow, usSaveCol );
/*
      if( s_uiDispCount == 0 )
         SLsmg_refresh();
*/
   }
}

void hb_gt_DispBegin( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispBegin()"));

   ++s_uiDispCount;
}

void hb_gt_DispEnd()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispEnd()"));

   if( --s_uiDispCount == 0 )
      SLsmg_refresh();
}

BOOL hb_gt_SetMode( USHORT uiRows, USHORT uiCols )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", uiRows, uiCols));

   HB_SYMBOL_UNUSED( uiRows );
   HB_SYMBOL_UNUSED( uiCols );

   /* TODO: How to change the size of the screen? */
   return FALSE;
}

BOOL hb_gt_GetBlink()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetBlink()"));

   /* TODO: current implementation disables blinking/intensity */
   return FALSE;
}

void hb_gt_SetBlink( BOOL bBlink )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetBlink(%d)", (int) bBlink));

   /* TODO: current implementation disables blinking/intensity */
   HB_SYMBOL_UNUSED( bBlink );
}

void hb_gt_Tone( double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Tone(%lf, %lf)", dFrequency, dDuration));

   /* TODO: Implement this */

   HB_SYMBOL_UNUSED( dFrequency );
   HB_SYMBOL_UNUSED( dDuration );
   SLtt_beep();
}

char * hb_gt_Version( void )
{
   return "Harbour Terminal: Slang";
}

USHORT hb_gt_DispCount()
{
   return s_uiDispCount;
}

void hb_gt_Replicate( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE byChar, ULONG ulLen )
{
   ULONG i;
   SLsmg_Char_Type SLchar, * pScr;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Replicate(%hu, %hu, %i, %i, %lu)", uiRow, uiCol, byAttr, byChar, nLength));

   pScr = ( SLsmg_Char_Type * ) hb_xgrab( ( ulLen + 1 ) * sizeof( SLsmg_Char_Type ) );

   for( i = 0; i < ulLen; i++ )
   {
      /* build a Slang converted char - note we are clearing a high bit of color */
      SLchar = SLSMG_BUILD_CHAR( s_convHighChars[ byChar ],
                  ( (int)byAttr + SLANG_RESERVED_COLORS ) & 0x7F );

      /* alternate char set */
      if( byChar > 127 )
         SLchar |= 0x8000;

      *( pScr + i ) = SLchar;
   }

   SLsmg_gotorc( uiRow, uiCol );

   if( ulLen > 0 )
   {
      SLsmg_write_raw( pScr, ulLen );

      /* this should not be needed here.
         hb_gtRepChar() should set this for us
      */
/*
      SLsmg_gotorc( uiRow, uiCol + ulLen );

      if( s_uiDispCount == 0 )
         SLsmg_refresh();
*/
      hb_gt_SetPos( uiRow, uiCol + ulLen, HB_GT_SET_POS_AFTER );
   }

   hb_xfree( ( BYTE * ) pScr );
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

   SLsmg_gotorc( uiTop + 1, uiLeft + 1 );

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
/*
   SLsmg_gotorc( uiBottom + 1, uiCol );

   if( s_uiDispCount == 0 )
      SLsmg_refresh();
*/
   hb_gt_SetPos( uiBottom + 1, uiCol, HB_GT_SET_POS_AFTER );

   return 0;
}

/* ------------------------------------------------------ */

/* NOTE: these two are for prepare Slang to temporary
   finish its work. They should be called from run.c.
   They are not re-enrant ???.
*/
BOOL hb_gt_Suspend()
{
   if( ! s_bSuspended )
   {
      if( SLsmg_suspend_smg() != -1 )
      {
         SLang_reset_tty();
         s_bSuspended = TRUE;
      }
   }

   return s_bSuspended;
}

BOOL hb_gt_Resume()
{
   if( s_bSuspended &&
       SLsmg_resume_smg() != -1 &&
       hb_gt_Init_Terminal( 1 ) != -1 ) /* reinitialize a terminal */
       SLsmg_refresh();
   {
      s_bSuspended = FALSE;
   }

   return( !s_bSuspended );
}

BOOL hb_gt_PreExt()
{
   SLsmg_refresh();
   return TRUE;
}

BOOL hb_gt_PostExt()
{
   return TRUE;
}

/* ------------------------------------------------------ */

static void hb_gt_build_conv_tabs()
{
   int i, fg, bg;
#ifndef IBMPC_SYSTEM
   int len;
   char *p, ch;
#endif

   /* COMPATIBILITY: Slang uses bit 0x8000 as an alternate
      char mask so it leaves us only 128 possible fgbg colors.
      (see Notes in Slang sources). This is incompatible with
      Clipper. Slang uses color 0 as a normal color and a
      color 1 as a reverse one, leaving us only 126 fgbg.
   */
   /* init colors - color 0 and 1 are normal and
      reverse color in Slang. We can't use them
   */
   for( i = 0; i < 256 - SLANG_RESERVED_COLORS; i++ )
   {
      fg = ( i & 0x0F );
      bg = ( i >> 4 ) & 0x07; /* bit 7 is a blinking attribute - not used here */

      /* leave 0 and 1 for Slang library. Shift color number by 2 */
      SLtt_set_color( i + SLANG_RESERVED_COLORS, ( char * ) NULL, s_colorNames[ fg ], s_colorNames[ bg ] );
   }
   /* Slang normal and reverse color */
   SLtt_set_color( 0, ( char * ) NULL, s_colorNames[ 7 ], s_colorNames[ 0 ] );
   SLtt_set_color( 1, ( char * ) NULL, s_colorNames[ 0 ], s_colorNames[ 7 ] );

   /* build an alternate chars table */
   for( i = 0; i < 32; i++ )
      /* under Unix control-chars are not visible in a general meaning */
      s_convHighChars[ i ] = '.';
   for( i = 32; i < 256; i++ )
      s_convHighChars[ i ] = i;

#ifndef IBMPC_SYSTEM
   /* init an alternate chars table */
   if( ( p = SLtt_Graphics_Char_Pairs ) )
   {
      len = strlen( p );
      for( i = 0; i < len; i++ )
      {
         ch = *p++;
         ch &= 0x7F;

         switch( ch )
         {
            case SLSMG_HLINE_CHAR   :  s_convHighChars[ 196 ]  = *p; break;
            case SLSMG_VLINE_CHAR   :  s_convHighChars[ 179 ]  = *p; break;
            case SLSMG_ULCORN_CHAR  :  s_convHighChars[ 218 ]  = *p; break;
            case SLSMG_URCORN_CHAR  :  s_convHighChars[ 191 ]  = *p; break;
            case SLSMG_LLCORN_CHAR  :  s_convHighChars[ 192 ]  = *p; break;
            case SLSMG_LRCORN_CHAR  :  s_convHighChars[ 217 ]  = *p; break;
            case SLSMG_CKBRD_CHAR   :  s_convHighChars[ 176 ]  = *p; break;
            case SLSMG_RTEE_CHAR    :  s_convHighChars[ 180 ]  = *p; break;
            case SLSMG_LTEE_CHAR    :  s_convHighChars[ 195 ]  = *p; break;
            case SLSMG_UTEE_CHAR    :  s_convHighChars[ 194 ]  = *p; break;
            case SLSMG_DTEE_CHAR    :  s_convHighChars[ 193 ]  = *p; break;
            case SLSMG_PLUS_CHAR    :  s_convHighChars[ 197 ]  = *p; break;

            /* TODO: need some smart here */
/*
            case SLSMG_DIAMOND_CHAR :  s_convHighChars[  ] =  *p;  break;
            case SLSMG_DEGREE_CHAR; :  s_convHighChars[  ] =  *p;  break;
            case SLSMG_PLMINUS_CHAR :  s_convHighChars[  ] =  *p;  break;
            case SLSMG_BULLET_CHAR  :  s_convHighChars[  ] =  *p;  break;
            case SLSMG_LARROW_CHAR  :  s_convHighChars[  ] =  *p;  break;
            case SLSMG_RARROW_CHAR  :  s_convHighChars[  ] =  *p;  break;
            case SLSMG_DARROW_CHAR  :  s_convHighChars[  ] =  *p;  break;
            case SLSMG_UARROW_CHAR  :  s_convHighChars[  ] =  *p;  break;
*/
            case SLSMG_BOARD_CHAR   :  s_convHighChars[ 178 ] =  *p; break;
            case SLSMG_BLOCK_CHAR   :  s_convHighChars[ 219 ] =  *p; break;
         }

         p++;
      }
   }

   /* QUESTION: do we have double, single-double, ... frames under xterm ? */
   if( s_underXTerm )
   {
      /* frames of all Clipper type are _B_SINBLE under xterm */
      s_convHighChars[ 205 ] = s_convHighChars[ 196 ];
      s_convHighChars[ 186 ] = s_convHighChars[ 179 ];
      s_convHighChars[ 201 ] = s_convHighChars[ 218 ];
      s_convHighChars[ 214 ] = s_convHighChars[ 218 ];
      s_convHighChars[ 213 ] = s_convHighChars[ 218 ];
      s_convHighChars[ 187 ] = s_convHighChars[ 191 ];
      s_convHighChars[ 183 ] = s_convHighChars[ 191 ];
      s_convHighChars[ 184 ] = s_convHighChars[ 191 ];
      s_convHighChars[ 200 ] = s_convHighChars[ 192 ];
      s_convHighChars[ 211 ] = s_convHighChars[ 192 ];
      s_convHighChars[ 212 ] = s_convHighChars[ 192 ];
      s_convHighChars[ 188 ] = s_convHighChars[ 217 ];
      s_convHighChars[ 189 ] = s_convHighChars[ 217 ];
      s_convHighChars[ 190 ] = s_convHighChars[ 217 ];
      s_convHighChars[ 185 ] = s_convHighChars[ 118 ];
      s_convHighChars[ 204 ] = s_convHighChars[ 195 ];
      s_convHighChars[ 203 ] = s_convHighChars[ 194 ];
      s_convHighChars[ 202 ] = s_convHighChars[ 193 ];
      s_convHighChars[ 206 ] = s_convHighChars[ 197 ];
   }
#endif
}
