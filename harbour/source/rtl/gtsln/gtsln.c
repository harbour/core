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

/* *********************************************************************** */

#include <slang.h>

/* missing defines in previous versions of Slang - this can not work ! */
#if SLANG_VERSION < 10400
   typedef unsigned short SLsmg_Char_Type;
   #define SLSMG_EXTRACT_CHAR( x ) ( ( x ) & 0xFF )
   #define SLSMG_EXTRACT_COLOR( x ) ( ( ( x ) >> 8 ) & 0xFF )
   #define SLSMG_BUILD_CHAR( ch, color ) ( ( ( SLsmg_Char_Type ) ( unsigned char )( ch ) ) | ( ( color ) << 8 ) )

#if SLANG_VERSION < 10308
   #define SLSMG_DIAMOND_CHAR	0x04
   #define SLSMG_DEGREE_CHAR	0xF8
   #define SLSMG_PLMINUS_CHAR	0xF1
   #define SLSMG_BULLET_CHAR	0xF9
   #define SLSMG_LARROW_CHAR	0x1B
   #define SLSMG_RARROW_CHAR	0x1A
   #define SLSMG_DARROW_CHAR	0x19
   #define SLSMG_UARROW_CHAR	0x18
   #define SLSMG_BOARD_CHAR	0xB2
   #define SLSMG_BLOCK_CHAR	0xDB
   /*
   #define SLSMG_BOARD_CHAR     'h'
   #define SLSMG_BLOCK_CHAR     '0'
   */
#endif
#endif

#include <unistd.h>
#include <signal.h>
#include <time.h>

#include "hbapi.h"
#include "hbapigt.h"
#include "inkey.ch"

/* *********************************************************************** */

/* if we can not manipulate cursor state */
#define SC_UNAVAIL -1

/* to convert DeadKey+letter to national character */
extern unsigned char s_convKDeadKeys[];
extern int hb_gt_Init_Terminal( int phase );

/* to convert characters displayed */
static void hb_gt_build_conv_tabs();

/* extern int _SLsnprintf (char *, unsigned int, char *, ...); */

/* the name of an environmet variable containig a definition of nation chars.*/
/* A definition is a list of pairs of chars. The first char in each pair is  */
/* an ASCII key, which should be pressed *after* a "DeadKey" was pressed to  */
/* get the nation char, a second in that pair is a corresponding nation char */
unsigned char *hb_NationCharsEnvName = "HRBNATIONCHARS";

/* *********************************************************************** */

static USHORT s_uiDispCount = 0;
static SHORT s_sCursorStyle = SC_NORMAL;
static BOOL s_linuxConsole = FALSE;
static BOOL s_underXTerm = FALSE;

/* indicate if we are currently running a command from system */
static BOOL s_bSuspended = FALSE;

/* standard output */
static int s_iFilenoStdout;

/* to convert high characters (mostly graphics, nation and control chars) */
static SLsmg_Char_Type s_convHighChars[ 256 ];

/* bit indication if char is a nation char - assumes char is 8-bit */
static unsigned char s_IsNationChar[ 128 / 8 ];

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

/* a box drawing hack when nation chars are used */
static BOOL s_bUse_Alt_Char_Hack = FALSE;

/* *********************************************************************** */

volatile BOOL hb_gt_sln_bScreen_Size_Changed = FALSE;

/* window's resize handler */
static void sigwinch_handler( int sig )
{
   hb_gt_sln_bScreen_Size_Changed = TRUE;
   SLsignal( SIGWINCH, sigwinch_handler );
}

/* *********************************************************************** */

/* I think this function should not be void. It should be BOOL */
void hb_gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr )
{
   BOOL gt_Inited = FALSE;
   char * tmp;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init()"));

   s_iFilenoStdout = iFilenoStdout;
   s_uiDispCount = 0;

   /* read a terminal descripion from a terminfo database */
   SLtt_get_terminfo();

   /* initialize higher-level Slang routines */
   if( SLkp_init() != -1 )
   {
      /* initialize a terminal stuff and a Slang
         keyboard subsystem for the first time */
      if( hb_gt_Init_Terminal( 0 ) )
      {
         /* initialize a screen handling subsytem */
         if( SLsmg_init_smg() != -1 )
         {
            /* install window resize handler */
            SLsignal( SIGWINCH, sigwinch_handler );

            /* do not indicate USER_BREAK in SLang_Error - ??? */
            SLang_Ignore_User_Abort = 1;

            /* no default abort procesing */
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
            s_linuxConsole = ( strncmp( ( tmp = hb_getenv( "TERM" ) ), "linux", 5 ) == 0 );
            hb_xfree( ( void * ) tmp );

            /* an uncertain way to check if we run under xterm */
            s_underXTerm = ( strstr( ( tmp = hb_getenv( "TERM" ) ), "xterm" ) != NULL );
            hb_xfree( ( void * ) tmp );

            /* NOTE: this driver is implemented in a way that it is
               imposible to get intensity/blinking background mode.
               The reason is the way Slang is written.
               This is incompatible with Clipper. */
            SLtt_Blink_Mode = 0;
            SLtt_Use_Blink_For_ACS = 0;
            SLsmg_Display_Eight_Bit = 160;

            /* initialize conversion tables */
            hb_gt_build_conv_tabs();

            /* ensure we are in a normal chars set */
            SLtt_set_alt_char_set( 0 );

            /* NOTE: due to a work of a Slang library which does not
               prepare its internal screen buffer properly, a screen
               must be cleared before normal work. This is not
               compatible with Clipper */
            SLsmg_cls();
            SLsmg_gotorc( 0, 0 );
            SLsmg_refresh();

            gt_Inited = TRUE;
         }
      }
   }

   hb_mouse_Init();

   if( ! gt_Inited )
   {
      char *errmsg = '\r'+'\n'+"Internal error : screen driver initialization failure"+'\r'+'\n'+( char )0;

      /* something went wrong - restore default settings */
      SLang_reset_tty();

      /* TODO: a standard Harbour error should be generated here ! */
      write( iFilenoStderr, errmsg , strlen( errmsg ) );
      exit( 20 );
   }
}

/* *********************************************************************** */

void hb_gt_Exit( void )
{
   char *escstr;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Exit()"));

   hb_mouse_Exit();

   /* NOTE: This is incompatible with Clipper - on exit leave a cursor visible */
   if( s_sCursorStyle != SC_UNAVAIL )
      hb_gt_SetCursorStyle( SC_NORMAL );

   SLsmg_refresh();
   SLsmg_reset_smg();
   SLang_reset_tty();

   /* restore a standard bell frequency and duration */
   if( s_linuxConsole )
   {
      escstr = "\033[10]";
      write( s_iFilenoStdout, escstr, strlen( escstr ) );
      escstr = "\033[11]";
      write( s_iFilenoStdout, escstr, strlen( escstr ) );
   }
}

/* *********************************************************************** */

USHORT hb_gt_GetScreenWidth( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth()"));

   return SLtt_Screen_Cols;
}

/* *********************************************************************** */

USHORT hb_gt_GetScreenHeight( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeight()"));

   return SLtt_Screen_Rows;
}

/* *********************************************************************** */

SHORT hb_gt_Col( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));

   return SLsmg_get_column();
}

/* *********************************************************************** */

SHORT hb_gt_Row( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));

   return SLsmg_get_row();
}

/* *********************************************************************** */

void hb_gt_SetPos( SHORT iRow, SHORT iCol, SHORT iMethod )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd, %hd)", iRow, iCol, iMethod));

   HB_SYMBOL_UNUSED( iMethod );

   SLsmg_gotorc( iRow, iCol );
   /* SLtt_goto_rc( iRow, iCol ); */

   if( s_uiDispCount == 0 )
      SLsmg_refresh();
}

/* *********************************************************************** */

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
            /* This is a hack. Out<xxx>() is done outside Slang and
               it can't be tracked currently by Slang. This should
               be changed in console.c */
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

/* *********************************************************************** */

USHORT hb_gt_GetCursorStyle( void )
{
   /* TODO: What shape is the cursor? */

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorStyle()"));

   /* if we don't know a cursor state - assume visible */
   if( s_sCursorStyle == SC_UNAVAIL )
      return( SC_NORMAL );

   return( s_sCursorStyle );
}

/* *********************************************************************** */

void hb_gt_SetCursorStyle( USHORT uiStyle )
{
   /* keyseq to define cursor shape under linux console */
   static char cursDefseq[] = { 27, '[', '?', '1', 'c', 0 };

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorStyle(%hu)", uiStyle));

   /* TODO: How to set the shape of the cursor on terminals ? */

   if( s_sCursorStyle == SC_UNAVAIL )
      return;

   if( ( s_sCursorStyle >= SC_NONE ) && ( s_sCursorStyle <= SC_SPECIAL2 ) )
   {
      s_sCursorStyle = uiStyle;
      SLtt_set_cursor_visibility( s_sCursorStyle != SC_NONE );

#ifdef __linux__
      /* NOTE: cursor apearence works only under linux console */
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
               to SC_SPECIAL2 under Linux console  */
            cursDefseq[ 3 ] = '4';
            break;
         }

         SLtt_write_string( cursDefseq );
      }
#endif

      if( s_uiDispCount == 0 )
         /* SLsmg_refresh(); */
         SLtt_flush_output();
   }
}

/* *********************************************************************** */

BOOL hb_gt_IsColor( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_IsColor()"));

   return SLtt_Use_Ansi_Colors;
}

/* *********************************************************************** */

static void hb_gt_xPutch( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE byChar )
{
   unsigned char Pos, Msk;
   SLsmg_Char_Type SLchar, SLattr;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_xPutch(%hu, %hu, %d, %i)", uiRow, uiCol, (int) byAttr, byChar));

   Pos = ( unsigned char ) ( ( byChar & 0x7F ) >> 3 );
   Msk = ( unsigned char ) ( 1 << ( byChar & 0x07 ) );

   /* build a Slang converted attribute - note we are clearing a high bit of color */
   SLattr = ( SLsmg_Char_Type )( ( ( byAttr - 7 ) & 0x7F ) << 8 );

   /* this hack turns on Normal Char Set when we should draw a nation char. */
   if( s_bUse_Alt_Char_Hack || !( s_IsNationChar[ Pos ] & Msk ) )
      SLchar = s_convHighChars[ byChar ] | SLattr; /* build a Slang char  */
   else
      SLchar = ( SLsmg_Char_Type )byChar | SLattr; /* build a Slang char  */

   SLsmg_gotorc( uiRow, uiCol );
   SLsmg_write_raw( &SLchar, 1 );
}

/* *********************************************************************** */

void hb_gt_Puts( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE * pbyStr, ULONG ulLen )
{
   ULONG i;
   BYTE byChar;
   unsigned char Pos, Msk;
   SLsmg_Char_Type SLchar, SLattr, *pScr;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu)", uiRow, uiCol, (int) byAttr, pbyStr, ulLen));

   pScr = ( SLsmg_Char_Type * ) hb_xgrab( ( ulLen + 1 ) * sizeof( SLsmg_Char_Type ) );

   /* build a Slang converted attribute - note we are clearing a high bit of color */
   SLattr = ( SLsmg_Char_Type )( ( ( byAttr - 7 ) & 0x7F ) << 8 );

   for( i = 0; i < ulLen; i++ )
   {
      /* a next char to process */
      byChar = *pbyStr++;

      Pos = ( unsigned char ) ( ( byChar & 0x7F ) >> 3 );
      Msk = ( unsigned char ) ( 1 << ( byChar & 0x07 ) );

      /* this hack turns on Normal Char Set when we should draw a nation char. */
      if( s_bUse_Alt_Char_Hack || !( s_IsNationChar[ Pos ] & Msk ) )
         SLchar = s_convHighChars[ byChar ] | SLattr; /* build a Slang char */
      else
         SLchar = ( SLsmg_Char_Type )byChar | SLattr; /* build a Slang char */

      *( pScr + i ) = SLchar;
   }

   SLsmg_gotorc( uiRow, uiCol );

   if( ulLen > 0 )
      SLsmg_write_raw( pScr, ulLen );

   /* NOTE : enable this if problems with cursor positioning occur */
   /* hb_gt_SetPos( uiRow, uiCol + ulLen, HB_GT_SET_POS_AFTER ); */

   hb_xfree( ( BYTE * )pScr );
}

/* *********************************************************************** */

int hb_gt_RectSize( USHORT rows, USHORT cols )
{
   return rows * cols * sizeof( SLsmg_Char_Type );
}

/* *********************************************************************** */

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

/* *********************************************************************** */

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

   hb_gt_SetPos( usSavRow, usSavCol, HB_GT_SET_POS_AFTER );
}

/* *********************************************************************** */

void hb_gt_SetAttribute( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE byAttr )
{
   int Rows, Cols;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetAttribute(%hu, %hu, %hu, %hu, %d)", uiTop, uiLeft, uiBottom, uiRight, (int) byAttr));

   Rows = uiBottom - uiTop + 1;
   Cols = uiRight - uiLeft + 1;

   /* note: we are clearing a high bit of color */
   SLsmg_set_color_in_region( ( byAttr - 7 ) & 0x7F, uiTop, uiLeft, Rows, Cols );

   if( s_uiDispCount == 0 )
      SLsmg_refresh();
}

/* *********************************************************************** */

/* NOTE : hb_gt_Scroll is based on gtdos.c, but changed to get scroll
  worked well when scrolling horizontally. Clipper behaves
  strange here. */

void hb_gt_Scroll( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE byAttr, SHORT iRows, SHORT iCols )
{
   SHORT usSaveRow, usSaveCol;
   USHORT uiSize;

   int iLength = ( usRight - usLeft ) + 1;
   int iCount, iColOld, iColNew, iColSize;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hd, %hd)", usTop, usLeft, usBottom, usRight, (int) byAttr, iRows, iCols));

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

      /* this is probably not compatible with Clipper */
      hb_gt_DispBegin();

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

      /* hb_gtSetPos( usSaveRow, usSaveCol ); */
      SLsmg_gotorc( usSaveRow, usSaveCol );

      /* this is probably not compatible with Clipper */
      hb_gt_DispEnd();
   }
}

/* *********************************************************************** */

void hb_gt_DispBegin( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispBegin()"));

   ++s_uiDispCount;
}

/* *********************************************************************** */

void hb_gt_DispEnd()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispEnd()"));

   /* is this compatible with Clipper ? */
   if( s_uiDispCount > 0 )
      --s_uiDispCount;

   if( s_uiDispCount == 0 )
      SLsmg_refresh();
}

/* *********************************************************************** */

BOOL hb_gt_SetMode( USHORT uiRows, USHORT uiCols )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", uiRows, uiCols));

   HB_SYMBOL_UNUSED( uiRows );
   HB_SYMBOL_UNUSED( uiCols );

   /* TODO: How to change the size of the screen? */
   return FALSE;
}

/* *********************************************************************** */

BOOL hb_gt_GetBlink()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetBlink()"));

   /* TODO: current implementation disables blinking/intensity */
   return FALSE;
}

/* *********************************************************************** */

void hb_gt_SetBlink( BOOL bBlink )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetBlink(%d)", (int) bBlink));

   /* TODO: current implementation disables blinking/intensity */
   HB_SYMBOL_UNUSED( bBlink );
}

/* *********************************************************************** */

void hb_gt_Tone( double dFrequency, double dDuration )
{
   char escstr[ 64 ];
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Tone(%lf, %lf)", dFrequency, dDuration));

   /* TODO: Implement this for other consoles than linux ? */

   dFrequency = HB_MIN( HB_MAX( 0.0, dFrequency ), 32767.0 );
   /* dDuration = dDuration * 1000.0 / 18.2; */ /* clocks */

   if( s_linuxConsole )
   {
      snprintf( escstr, 63, "\033[10;%hd]", ( int )dFrequency );
      SLtt_write_string( escstr );
      snprintf( escstr, 63, "\033[11;%hd]", ( int )( dDuration * 1000.0 / 18.2 ) );
      SLtt_write_string( escstr );
      SLtt_flush_output();
   }

   SLtt_beep();

   if( s_linuxConsole )
   {
      /* NOTE : the code below is adapted from gtdos.c/hb_gt_Tone() */

      dDuration *= 1800;
      while( dDuration > 0.0 )
      {
         USHORT temp = ( USHORT ) HB_MIN( HB_MAX( 0, dDuration ), 1000 );
         static struct timespec nanosecs;

         dDuration -= temp;
         if( temp <= 0 )
            /* Ensure that the loop gets terminated when
               only a fraction of the delay time remains. */
            dDuration = -1.0;
         else
         {
            hb_idleState();
            nanosecs.tv_sec  = 0;
            nanosecs.tv_nsec = temp * 10;
            nanosleep( &nanosecs, NULL );
         }
      }
   }
}

/* *********************************************************************** */

char * hb_gt_Version( void )
{
   return "Harbour Terminal: Slang";
}

/* *********************************************************************** */

USHORT hb_gt_DispCount()
{
   return s_uiDispCount;
}

/* *********************************************************************** */

void hb_gt_Replicate( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE byChar, ULONG ulLen )
{
   ULONG i;
   unsigned char Pos, Msk;
   SLsmg_Char_Type SLchar, SLattr, * pScr;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Replicate(%hu, %hu, %i, %i, %lu)", uiRow, uiCol, byAttr, byChar, ulLen));

   pScr = ( SLsmg_Char_Type * ) hb_xgrab( ( ulLen + 1 ) * sizeof( SLsmg_Char_Type ) );

   Pos = ( unsigned char ) ( ( byChar & 0x7F ) >> 3 );
   Msk = ( unsigned char ) ( 1 << ( byChar & 0x07 ) );

   /* build a Slang converted attribute - note we are clearing a high bit of color */
   SLattr = ( SLsmg_Char_Type )( ( ( byAttr - 7 ) & 0x7F ) << 8 );

   /* this hack turns on Normal Char Set when we should draw a nation char. */
   if( s_bUse_Alt_Char_Hack || !( s_IsNationChar[ Pos ] & Msk ) )
      SLchar = s_convHighChars[ byChar ] | SLattr; /* build a Slang char */
   else
      SLchar = ( SLsmg_Char_Type )byChar | SLattr; /* build a Slang char */

   for( i = 0; i < ulLen; i++ )
      *( pScr + i ) = SLchar;

   SLsmg_gotorc( uiRow, uiCol );

   if( ulLen > 0 )
   {
      SLsmg_write_raw( pScr, ulLen );

      /* this should not be needed here. hb_gtRepChar() should set this for us */
      hb_gt_SetPos( uiRow, uiCol + ulLen, HB_GT_SET_POS_AFTER );
   }

   hb_xfree( ( BYTE * ) pScr );
}

/* *********************************************************************** */

USHORT hb_gt_Box( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right,
                  BYTE * szBox, BYTE byAttr )
{
   USHORT ret = 1;
   SHORT Row;
   SHORT Col;
   SHORT Height;
   SHORT Width;
   /* a box drawing hack */
   BOOL SaveUseAltChar = s_bUse_Alt_Char_Hack;

   /* a box drawing hack */
   s_bUse_Alt_Char_Hack = TRUE;

   if( Left >= 0 || Left < hb_gt_GetScreenWidth()
   || Right >= 0 || Right < hb_gt_GetScreenWidth()
   || Top >= 0 || Top < hb_gt_GetScreenHeight()
   || Bottom >= 0 || Bottom < hb_gt_GetScreenHeight() )
   {

      /* Ensure that box is drawn from top left to bottom right. */
      if( Top > Bottom )
      {
         SHORT tmp = Top;
         Top = Bottom;
         Bottom = tmp;
      }
      if( Left > Right )
      {
         SHORT tmp = Left;
         Left = Right;
         Right = tmp;
      }

      /* Draw the box or line as specified */
      Height = Bottom - Top + 1;
      Width  = Right - Left + 1;

      hb_gt_DispBegin();

      if( Height > 1 && Width > 1 && Top >= 0 && Top < hb_gt_GetScreenHeight() && Left >= 0 && Left < hb_gt_GetScreenWidth() )
         hb_gt_xPutch( Top, Left, byAttr, szBox[ 0 ] ); /* Upper left corner */

      Col = ( Height > 1 ? Left + 1 : Left );
      if(Col < 0 )
      {
         Width += Col;
         Col = 0;
      }
      if( Right >= hb_gt_GetScreenWidth() )
      {
         Width -= Right - hb_gt_GetScreenWidth();
      }

      if( Col <= Right && Col < hb_gt_GetScreenWidth() && Top >= 0 && Top < hb_gt_GetScreenHeight() )
         hb_gt_Replicate( Top, Col, byAttr, szBox[ 1 ], Width + ( (Right - Left) > 1 ? -2 : 0 ) ); /* Top line */

      if( Height > 1 && (Right - Left) > 1 && Right < hb_gt_GetScreenWidth() && Top >= 0 && Top < hb_gt_GetScreenHeight() )
         hb_gt_xPutch( Top, Right, byAttr, szBox[ 2 ] ); /* Upper right corner */

      if( szBox[ 8 ] && Height > 2 && Width > 2 )
      {
         for( Row = Top + 1; Row < Bottom; Row++ )
         {
            if( Row >= 0 && Row < hb_gt_GetScreenHeight() )
            {
               Col = Left;
               if( Col < 0 )
                  Col = 0; /* The width was corrected earlier. */
               else
                  hb_gt_xPutch( Row, Col++, byAttr, szBox[ 7 ] ); /* Left side */
               hb_gt_Replicate( Row, Col, byAttr, szBox[ 8 ], Width - 2 ); /* Fill */
               if( Right < hb_gt_GetScreenWidth() )
                  hb_gt_xPutch( Row, Right, byAttr, szBox[ 3 ] ); /* Right side */
            }
         }
      }
      else
      {
         for( Row = ( Width > 1 ? Top + 1 : Top ); Row < ( (Right - Left ) > 1 ? Bottom : Bottom + 1 ); Row++ )
         {
            if( Row >= 0 && Row < hb_gt_GetScreenHeight() )
            {
               if( Left >= 0 && Left < hb_gt_GetScreenWidth() )
                  hb_gt_xPutch( Row, Left, byAttr, szBox[ 7 ] ); /* Left side */
               if( ( Width > 1 || Left < 0 ) && Right < hb_gt_GetScreenWidth() )
                  hb_gt_xPutch( Row, Right, byAttr, szBox[ 3 ] ); /* Right side */
            }
         }
      }

      if( Height > 1 && Width > 1 )
      {
         if( Left >= 0 && Bottom < hb_gt_GetScreenHeight() )
            hb_gt_xPutch( Bottom, Left, byAttr, szBox[ 6 ] ); /* Bottom left corner */

         Col = Left + 1;
         if( Col < 0 )
            Col = 0; /* The width was corrected earlier. */

         if( Col <= Right && Bottom < hb_gt_GetScreenHeight() )
            hb_gt_Replicate( Bottom, Col, byAttr, szBox[ 5 ], Width - 2 ); /* Bottom line */

         if( Right < hb_gt_GetScreenWidth() && Bottom < hb_gt_GetScreenHeight() )
            hb_gt_xPutch( Bottom, Right, byAttr, szBox[ 4 ] ); /* Bottom right corner */
      }
      hb_gt_DispEnd();

      /* NOTE : enable this if problems with cursor positioning occur */
      /* SLsmg_gotorc( uiTop + 1, uiLeft + 1 ); */
      hb_gt_DispEnd();

      /* a box drawing hack */
      s_bUse_Alt_Char_Hack = SaveUseAltChar;

      ret = 0;
   }

   return ret;
}

/* *********************************************************************** */

USHORT hb_gt_BoxD( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame, BYTE byAttr )
{
   return hb_gt_Box( Top, Left, Bottom, Right, pbyFrame, byAttr );
}

/* *********************************************************************** */

USHORT hb_gt_BoxS( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame, BYTE byAttr )
{
   return hb_gt_Box( Top, Left, Bottom, Right, pbyFrame, byAttr );
}

/* *********************************************************************** */

USHORT hb_gt_HorizLine( SHORT Row, SHORT Left, SHORT Right, BYTE byChar, BYTE byAttr )
{
   USHORT ret = 1;
   BOOL SaveUseAltChar = s_bUse_Alt_Char_Hack;
   if( Row >= 0 && Row < hb_gt_GetScreenHeight() )
   {

      /* a box drawing hack */
      s_bUse_Alt_Char_Hack = TRUE;

      if( Left < 0 )
         Left = 0;
      else if( Left >= hb_gt_GetScreenWidth() )
         Left = hb_gt_GetScreenWidth() - 1;

      if( Right < 0 )
         Right = 0;
      else if( Right >= hb_gt_GetScreenWidth() )
         Right = hb_gt_GetScreenWidth() - 1;

      if( Left < Right )
         hb_gt_Replicate( Row, Left, byAttr, byChar, Right - Left + 1 );
      else
         hb_gt_Replicate( Row, Right, byAttr, byChar, Left - Right + 1 );

      /* a box drawing hack */
      s_bUse_Alt_Char_Hack = SaveUseAltChar;

      ret = 0;
   }
   return ret;
}

/* *********************************************************************** */

USHORT hb_gt_VertLine( SHORT Col, SHORT Top, SHORT Bottom, BYTE byChar, BYTE byAttr )
{
   USHORT ret = 1;
   USHORT uRow;
   BOOL SaveUseAltChar = s_bUse_Alt_Char_Hack;

   if( Col >= 0 && Col < hb_gt_GetScreenWidth() )
   {
      /* a box drawing hack */
      s_bUse_Alt_Char_Hack = TRUE;

      if( Top < 0 )
         Top = 0;
      else if( Top >= hb_gt_GetScreenHeight() )
         Top = hb_gt_GetScreenHeight() - 1;

      if( Bottom < 0 )
         Bottom = 0;
      else if( Bottom >= hb_gt_GetScreenHeight() )
         Bottom = hb_gt_GetScreenHeight() - 1;

      if( Top <= Bottom )
         uRow = Top;
      else
      {
         uRow = Bottom;
         Bottom = Top;
      }
      while( uRow <= Bottom )
         hb_gt_xPutch( uRow++, Col, byAttr, byChar );

      /* a box drawing hack */
      s_bUse_Alt_Char_Hack = SaveUseAltChar;

      hb_gt_SetPos( Bottom + 1, Col, HB_GT_SET_POS_AFTER );
      ret = 0;
   }
   return ret;
}

/* *********************************************************************** */

/* NOTE: these two are for prepare Slang to temporary
   finish its work. They should be called from run.c. */

/* *********************************************************************** */

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

/* *********************************************************************** */

BOOL hb_gt_Resume()
{
   if( s_bSuspended &&
       SLsmg_resume_smg() != -1 &&
       hb_gt_Init_Terminal( 1 ) != -1
     )
   {
       SLsmg_refresh(); /* reinitialize a terminal */
       s_bSuspended = FALSE;
   }

   return( !s_bSuspended );
}

/* *********************************************************************** */

BOOL hb_gt_PreExt()
{
   SLsmg_refresh();
   return TRUE;
}

/* *********************************************************************** */

BOOL hb_gt_PostExt()
{
   return TRUE;
}

/* *********************************************************************** */

static void hb_gt_build_conv_tabs()
{
   int i, fg, bg, len;
   unsigned char *p, ch;
   SLsmg_Char_Type SLch;

   /* COMPATIBILITY: Slang uses bit 0x8000 as an alternate
      char mask so it leaves us only 128 possible fgbg colors.
      (see Notes in Slang sources). This is incompatible with
      Clipper. Slang uses color 0 as a default color so we
      rotate color table by 7 to set color 0 to be color 7 */
   for( i = 7; i < 256 + 7; i++ )
   {
      fg = ( i & 0x0F );
      bg = ( i >> 4 ) & 0x07; /* bit 7 is a blinking attribute - not used here */

      /* rotate by 7 : 0->249, 1->250,...6->255, 7->0, ... 255->248 */
      ch = ( unsigned char ) ( i - 7 );

      SLtt_set_color( ( int )ch, ( char * ) NULL, s_colorNames[ fg ], s_colorNames[ bg ] );
      /*
      fprintf( stderr, "%3d %3d %3d %8lx %s %s\n", ch, i+7, i,
         SLtt_get_color_object( ( int )ch ), s_colorNames[ fg ],
         s_colorNames[ bg ] );
      */
   }

   /* build a conversion chars table */
   for( i = 0; i < 32; i++ )
      /* under Unix control-chars are not visible in a general meaning */
      s_convHighChars[ i ] = ( SLsmg_Char_Type ) '.';
      /* s_convHighChars[ i ] = ( ( SLsmg_Char_Type ) i ) | 0x8000; */
   for( i = 32; i < 128; i++ )
      /* lower 128-32 chars are from normal char set */
      s_convHighChars[ i ] = ( SLsmg_Char_Type ) i;
   for( i = 128; i < 256; i++ )
      /* upper 128 chars are from alternate char set */
      s_convHighChars[ i ] = ( ( SLsmg_Char_Type ) i ) | 0x8000;

   /* init an alternate chars table */
   if( ( p = SLtt_Graphics_Char_Pairs ) )
   {
      len = strlen( p );

      /* alternate char set should be even */
      if( ( len != ( ( len / 2 ) * 2 ) ) && ( len > 0 ) )
         --len;

      for( i = 0; i < len / 2; i++ )
      {
         ch = *p++;
         ch &= 0x7F;   /* is this really nessecary ? */
         SLch = ( ( SLsmg_Char_Type )( *p ) ) | 0x8000;

         switch( ch )
         {
            case SLSMG_HLINE_CHAR   :  s_convHighChars[ 196 ] = SLch; break;
            case SLSMG_VLINE_CHAR   :  s_convHighChars[ 179 ] = SLch; break;
            case SLSMG_ULCORN_CHAR  :  s_convHighChars[ 218 ] = SLch; break;
            case SLSMG_URCORN_CHAR  :  s_convHighChars[ 191 ] = SLch; break;
            case SLSMG_LLCORN_CHAR  :  s_convHighChars[ 192 ] = SLch; break;
            case SLSMG_LRCORN_CHAR  :  s_convHighChars[ 217 ] = SLch; break;
            case SLSMG_CKBRD_CHAR   :  s_convHighChars[ 176 ] = SLch; break;
            case SLSMG_RTEE_CHAR    :  s_convHighChars[ 180 ] = SLch; break;
            case SLSMG_LTEE_CHAR    :  s_convHighChars[ 195 ] = SLch; break;
            case SLSMG_UTEE_CHAR    :  s_convHighChars[ 194 ] = SLch; break;
            case SLSMG_DTEE_CHAR    :  s_convHighChars[ 193 ] = SLch; break;
            case SLSMG_PLUS_CHAR    :  s_convHighChars[ 197 ] = SLch; break;

/*
            case SLSMG_DEGREE_CHAR; :  s_convHighChars[  ] = SLch; break;
            case SLSMG_PLMINUS_CHAR :  s_convHighChars[  ] = SLch; break;
            case SLSMG_BULLET_CHAR  :  s_convHighChars[  ] = SLch; break;
*/
            case SLSMG_DIAMOND_CHAR :  s_convHighChars[ 04 ] = SLch;
                                       break;
            case SLSMG_LARROW_CHAR  :  s_convHighChars[ 17 ] = SLch;
                                       s_convHighChars[ 27 ] = SLch;
                                       break;
            case SLSMG_RARROW_CHAR  :  s_convHighChars[ 16 ] = SLch;
                                       s_convHighChars[ 26 ] = SLch;
                                       break;
            case SLSMG_DARROW_CHAR  :  s_convHighChars[ 25 ] = SLch;
                                       s_convHighChars[ 31 ] = SLch;
                                       break;
            case SLSMG_UARROW_CHAR  :  s_convHighChars[ 24 ] = SLch;
                                       s_convHighChars[ 30 ] = SLch;
                                       break;
            case SLSMG_BOARD_CHAR   :  s_convHighChars[ 178 ] = SLch;
                                       break;
            case SLSMG_BLOCK_CHAR   :  s_convHighChars[ 219 ] = SLch;
                                       break;
         }

         ++p;
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

   /* init national chars */
   if( ( p = hb_getenv( hb_NationCharsEnvName ) && p[ 0 ] != '\0' ) )
   {
      unsigned char Pos, Msk;

      len = strlen( p );

      /* a len of definition of National chars should be even */
      if( ( len != ( ( len / 2 ) * 2 ) ) && ( len > 0 ) )
         --len;

      /* no more than 128 National chars are allowed */
      if( len > 256 ) len = 256;

      /* the first element contains a number of Dead keys defined in an ENVAR */
      s_convKDeadKeys[ 0 ] = ( unsigned char ) ( len / 2 );

      for( i = 0; i < len / 2; i++ )
      {
         ch = *p++;

         s_convKDeadKeys[ 2 * i + 1 ] = ch;
         s_convKDeadKeys[ 2 * i + 2 ] = *p;

         Pos = ( unsigned char ) ( ( *p & 0x7F ) >> 3 );
         Msk = ( unsigned char ) ( 1 << ( *p & 0x07 ) );
         s_IsNationChar[ Pos ] |= Msk;
         ++p;
      }
/*
      for( i=0; i <= ( ( int ) s_convKDeadKeys[ 0 ] ) * 2; i++ )
         fprintf( stderr, "%3d %c\r\n", i, s_convKDeadKeys[ i ] );
      ch=getc( stdin );
*/
   }
   hb_xfree( ( void * ) p );
}

/* *********************************************************************** */