/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem based on ncurses.
 *
 * Copyright 1999 Gonzalo Diethelm <gonzalo.diethelm@iname.com>
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

#include <curses.h>
#include <unistd.h>

#include "hbapigt.h"
#include "hbinit.h"

/* static data
 */
static USHORT s_uiDispCount;

static void gt_GetMaxRC( int * r, int * c );
static void gt_GetRC( int * r, int * c );
static void gt_SetRC( int r, int c );

static unsigned s_attribmap_table[ 256 ]; /* mapping from DOS style attributes */
static BOOL s_under_xterm;
static int s_alternate_char_set;
static char s_xTermBox[ 10 ] = "lqkxjqmx ";

extern void hb_gt_keyboard_Init( void );
extern void hb_gt_keyboard_Exit( void );

static void hb_gt_terminal_Init( void )
{
   initscr();
   if( has_colors() )
   {
      int i;
      int backg, foreg;
      /* NOTE: color order=
          DOS style    -> ncurses style
          --------------------------------
          0 black         0-> COLOR_BLACK
          1 blue          4-> COLOR_RED
          2 green         2-> COLOR_GREEN
          3 cyan          6-> COLOR_YELLOW
          4 red           1-> COLOR_BLUE
          5 magenta       5-> COLOR_MAGENTA
          6 yellow        3-> COLOR_CYAN
          7 light gray    7-> COLOR_WHITE
          8 gray          0-> BOLD BLACK
          9 light blue    4-> BOLD RED
         10 light green   2-> BOLD GREEN
         11 light cyan    6-> BOLD YELLOW
         12 light red     1-> BOLD BLUE
         13 light magenta 5-> BOLD MAGENTA
         14 light yellow  3-> BOLD CYAN
         15 white         7-> BOLD WHITE
      */
      static char color_map[] = { 0, 4, 2, 6, 1, 5, 3, 7 };

      start_color();
      for( backg = 0; backg < COLORS; backg++ )
      {
         for( foreg = 0; foreg < COLORS; foreg++ )
            init_pair( backg * COLORS + foreg, color_map[ foreg ], color_map[ backg ] );
      }

      for( i = 0; i < 256; i++  )
      {
         backg = ( i >> 4 ) & 0x07;    /* bits 4-6, bit 7 is blinking attribute */
         foreg = ( i & 0x07 );
         s_attribmap_table[ i ] = COLOR_PAIR( backg * COLORS + foreg );
         if( i & 0x08 )
            s_attribmap_table[ i ] |= A_BOLD;  /* 4-th bit is an intensity bit */
         if( i & 0x80 )
            s_attribmap_table[ i ] |= A_BLINK;  /* 7-th bit is blinking bit */
      }
   }

   noecho();
   scrollok( stdscr, FALSE );
   raw();
   nodelay( stdscr, TRUE );
   keypad( stdscr, FALSE );

   s_under_xterm = ( strncmp( getenv( "TERM" ), "xterm", 5 ) == 0 );
   if( s_under_xterm )
   {
     /* Alternate characters set will be enabled only by request because
      * it changes character mapping under xterm
      */
      s_alternate_char_set = 0;
   }
   else
   {
     /* If running under Linux console enable alternate character set
      * by default
      */
      s_alternate_char_set = A_ALTCHARSET;
   }
   bkgdset( ' ' );
   ripoffline( 0, NULL );
}

static void hb_gt_terminal_Exit( void )
{
   noraw();
   refresh();
   endwin();
}

void hb_gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init()"));

   s_uiDispCount = 0;

   hb_gt_terminal_Init();
   /* Mouse sub-sytem have to be initialized after ncurses initialization */
   hb_mouse_Init();
   hb_gt_keyboard_Init();
}

void hb_gt_Exit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Exit()"));

   hb_gt_keyboard_Exit();
   hb_mouse_Exit();
   hb_gt_terminal_Exit();
}

BOOL hb_gt_AdjustPos( BYTE * pStr, ULONG ulLen )
{
   int row, col, max_row, max_col;
   ULONG ulCount;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_AdjustPos(%s, %lu)", pStr, ulLen ));

   gt_GetRC( &row, &col );
   gt_GetMaxRC( &max_row, &max_col );
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
               col = max_col;
               if( row )
                  row--;
            }
            break;

         case HB_CHAR_LF:
            col = 0;
            if( row < max_row )
               row++;
            break;

         case HB_CHAR_CR:
            col = 0;
            break;

         default:
            if( col < max_col )
               col++;
            else
            {
               col = 0;
               if( row < max_row )
                  row++;
            }
      }
   }
   gt_SetRC( row, col );
   return TRUE;
}

BOOL hb_gt_IsColor( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_IsColor()"));

   return has_colors();  /* returns TRUE or FALSE */
}

USHORT hb_gt_GetScreenWidth( void )
{
   int r, c;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth()"));

   gt_GetMaxRC( &r, &c );
   return c;
}

USHORT hb_gt_GetScreenHeight( void )
{
   int r, c;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeight()"));

   gt_GetMaxRC( &r, &c );
   return r;
}

void hb_gt_SetPos( SHORT iRow, SHORT iCol, SHORT iMethod )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd, %hd)", iRow, iCol, iMethod));

   HB_SYMBOL_UNUSED( iMethod );

   gt_SetRC( iRow, iCol );
}

SHORT hb_gt_Col( void )
{
   int r, c;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));

   gt_GetRC( &r, &c );
   return c;
}

SHORT hb_gt_Row( void )
{
   int r, c;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));

   gt_GetRC( &r, &c );
   return r;
}

USHORT hb_gt_GetCursorStyle( void )
{
   USHORT usOldCursor = curs_set( 0 );

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorStyle()"));

   curs_set( usOldCursor );

   return usOldCursor ? SC_INSERT : SC_NONE;
}

void hb_gt_SetCursorStyle( USHORT uiStyle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorStyle(%hu)", uiStyle));

   /* TODO: How to set the cursor shape? */
   if( uiStyle == SC_NONE )
      curs_set( 0 );
   else
      curs_set( 1 );
}

static void hb_gt_xPutch( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE byChar )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_xPutch(%hu, %hu, %d, %i)", uiRow, uiCol, (int) byAttr, byChar));

   move( uiRow, uiCol );

   addch( byChar | s_alternate_char_set | s_attribmap_table[ byAttr ] );

   if( s_uiDispCount == 0 )
      refresh();
}

void hb_gt_Puts( USHORT uiRow,
                 USHORT uiCol,
                 BYTE byAttr,
                 BYTE * pbyStr,
                 ULONG ulLen )
{
   ULONG i;
   int attr;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu)", uiRow, uiCol, (int) byAttr, pbyStr, ulLen));

   attr = s_alternate_char_set | s_attribmap_table[ byAttr ];
   move( uiRow, uiCol );

   for( i = 0; i < ulLen; ++i )
      addch( pbyStr[ i ] | attr );

   if( s_uiDispCount == 0 )
      refresh();
}

int hb_gt_RectSize( USHORT rows, USHORT cols )
{
   return rows * cols * sizeof( chtype );
}

void hb_gt_GetText( USHORT uiTop,
                    USHORT uiLeft,
                    USHORT uiBottom,
                    USHORT uiRight,
                    BYTE * pbyDst )
{
   int i;
   chtype * pBuffer = ( chtype * ) pbyDst;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetText(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pbyDst));

   if( s_uiDispCount == 0 )
      refresh();

   while( uiTop <= uiBottom )
   {
      for( i = uiLeft; i <= uiRight; i++, pBuffer++ )
        *pBuffer = mvinch( uiTop, i );
      ++uiTop;
   }
}

void hb_gt_PutText( USHORT uiTop,
                    USHORT uiLeft,
                    USHORT uiBottom,
                    USHORT uiRight,
                    BYTE * pbySrc )
{
   int Cols;
   chtype * pBuffer = ( chtype * ) pbySrc;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pbySrc));

   Cols = uiRight - uiLeft + 1;
   while( uiTop <= uiBottom )
   {
      mvaddchnstr( uiTop, uiLeft, pBuffer, Cols );
      pBuffer += Cols;
      ++uiTop;
   }

   if( s_uiDispCount == 0 )
      refresh();
}

void hb_gt_SetAttribute( USHORT uiTop,
                         USHORT uiLeft,
                         USHORT uiBottom,
                         USHORT uiRight,
                         BYTE byAttr )
{
   int newAttr = s_attribmap_table[ byAttr ];
   int dx;
   chtype c;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetAttribute(%hu, %hu, %hu, %hu, %d)", uiTop, uiLeft, uiBottom, uiRight, (int) byAttr));

   newAttr &= A_ATTRIBUTES;     /* extract attributes only */

   while( uiTop <= uiBottom )
   {
      for( dx = uiLeft; dx <= uiRight; dx++ )
      {
         c = mvinch( uiTop, dx );
         /* extract character only (remember about alternate chars) */
         c &= ( A_CHARTEXT | A_ALTCHARSET );
         /* set new attribute */
         c |= newAttr;
         if( addch( c ) == ERR )  /* Stop on error */
            return;
      }
      uiTop++;
   }

   if( s_uiDispCount == 0 )
      refresh();
}

void hb_gt_Scroll( USHORT uiTop,
                   USHORT uiLeft,
                   USHORT uiBottom,
                   USHORT uiRight,
                   BYTE byAttr,
                   SHORT iRows,
                   SHORT iCols )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hd, %hd)", uiTop, uiLeft, uiBottom, uiRight, (int) byAttr, iRows, iCols));

   if( iRows == 0 && iCols == 0 )
   {
      /* Clear the specified rectangle */
      WINDOW * subw;

      subw = subwin( stdscr, uiBottom - uiTop + 1, uiRight - uiLeft + 1, uiTop, uiLeft );
      wbkgdset( subw, ' ' | s_attribmap_table[ byAttr ] );
      wclear( subw );
      touchwin( stdscr );
      wrefresh( subw );
      delwin( subw );
   }
   else
   {
      if( iRows != 0 )
      {
         WINDOW * subw;

         subw = subwin( stdscr, uiBottom - uiTop + 1, uiRight - uiLeft + 1, uiTop, uiLeft );
         wbkgdset( subw, ' ' | s_attribmap_table[ byAttr ] );
         scrollok( subw, TRUE );
         wscrl( subw, iRows );
         delwin( subw );
      }

      if( iCols != 0 )
      {
         chtype * pScreen, * pTmp;
         int memsize;
         int RowCount, ColCount;
         int i, j;
         int newAttr;

         refresh();

         RowCount = uiBottom - uiTop + 1;
         ColCount = uiRight - uiLeft + 1;
         newAttr  = ' ' | s_attribmap_table[ byAttr ];

         memsize = hb_gt_RectSize( RowCount, ColCount );
         pScreen = ( chtype * ) hb_xgrab( memsize );
         hb_gt_GetText( uiTop, uiLeft, uiBottom, uiRight, ( BYTE * ) pScreen );

         if( iCols > 0 )
         {
            pTmp = pScreen;
            for( i = 0; i < RowCount; i++ )
            {
               for( j = ColCount - 1; j >= iCols; j-- )
                  pTmp[ j ] = pTmp[ j - 1 ];
               for( j = 0; j < iCols; j++ )
                  pTmp[ j ] = newAttr;
               pTmp += ColCount;
            }
         }
         else
         {
            int ColMove = ColCount + iCols;

            pTmp = pScreen;
            for( i = 0; i < RowCount; i++ )
            {
               for( j = 0; j < ColMove; j++ )
                  pTmp[ j ] = pTmp[ j - iCols ];
               for( j = ColMove; j < ColCount; j++ )
                  pTmp[ j ] = newAttr;
               pTmp += ColCount;
            }
         }
         hb_gt_PutText( uiTop, uiLeft, uiBottom, uiRight, ( BYTE * ) pScreen );
         hb_xfree( ( BYTE * ) pScreen );
      }
   }
   if( s_uiDispCount == 0 )
      refresh();
}

void hb_gt_DispBegin( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispBegin()"));

   ++s_uiDispCount;
}

void hb_gt_DispEnd()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispEnd()"));

   if( --s_uiDispCount == 0 );
      refresh();
}

BOOL hb_gt_SetMode( USHORT uiRows, USHORT uiCols )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", uiRows, uiCols));

   /* NOTE: Not tested!!!
      Use it on your own risk!
   */
#if defined(NCURSES_VERSION)
   {
      BOOL success;
      hb_gt_terminal_Exit();
      success = ( resizeterm( uiRows, uiCols ) == OK );
      hb_gt_terminal_Init();
      return success;
   }
#else
   return 0;
#endif
}

BOOL hb_gt_GetBlink()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetBlink()"));

   /* TODO: under dos, the background 'intensity' bit can be switched
      from intensity to 'blinking'
      does this work under your platform?
   */
   return FALSE;
}

void hb_gt_SetBlink( BOOL bBlink )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetBlink(%d)", (int) bBlink));

   if( bBlink )
      attron( A_BLINK );
   else
      attroff( A_BLINK );
}

void hb_gt_Tone( double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Tone(%lf, %lf)", dFrequency, dDuration));

   /* TODO: Implement this */

   HB_SYMBOL_UNUSED( dFrequency );
   HB_SYMBOL_UNUSED( dDuration );
   beep();
}

static void gt_GetMaxRC(int* r, int* c)
{
   int y, x;
   getmaxyx( stdscr, y, x );
   *r = y;
   *c = x;
}

static void gt_GetRC(int* r, int* c)
{
   int y, x;
   getyx( stdscr, y, x );
   *r = y;
   *c = x;
}

static void gt_SetRC(int r, int c)
{
   move( r, c );
   if( s_uiDispCount == 0 )
      refresh();
}

char * hb_gt_Version( void )
{
   return "Harbour Terminal: ncurses";
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

USHORT hb_gt_Box( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right,
                  BYTE * szBox, BYTE byAttr )
{
   USHORT ret = 1;
   SHORT Row;
   SHORT Col;
   SHORT Height;
   SHORT Width;

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
         Width -= Right - hb_gt_GetScreenWidth() + 1;
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
                  Col = 0; // The width was corrected earlier.
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
            Col = 0; // And use the width that was calculated earlier.

         if( Col <= Right && Bottom < hb_gt_GetScreenHeight() )
            hb_gt_Replicate( Bottom, Col, byAttr, szBox[ 5 ], Width - 2 ); /* Bottom line */

         if( Right < hb_gt_GetScreenWidth() && Bottom < hb_gt_GetScreenHeight() )
            hb_gt_xPutch( Bottom, Right, byAttr, szBox[ 4 ] ); /* Bottom right corner */
      }
      hb_gt_DispEnd();
      ret = 0;
   }

   return ret;
}

USHORT hb_gt_BoxD( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame, BYTE byAttr )
{
   USHORT ret;
   if( s_under_xterm )
   {
      /* Under xterm use hard-coded box drawing characters */
      pbyFrame = s_xTermBox;
      s_alternate_char_set = A_ALTCHARSET;
   }

   ret = hb_gt_Box( Top, Left, Bottom, Right, pbyFrame, byAttr );

   if( s_under_xterm )
      s_alternate_char_set = 0;   /* restore default setting */

   return ret;
}

USHORT hb_gt_BoxS( SHORT Top, SHORT Left, SHORT Bottom, SHORT Right, BYTE * pbyFrame, BYTE byAttr )
{
   USHORT ret;
   if( s_under_xterm )
   {
      /* Under xterm use hard-coded box drawing characters */
      pbyFrame = s_xTermBox;
      s_alternate_char_set = A_ALTCHARSET;
   }

   ret = hb_gt_Box( Top, Left, Bottom, Right, pbyFrame, byAttr );

   if( s_under_xterm )
      s_alternate_char_set = 0;   /* restore default setting */

   return ret;
}

USHORT hb_gt_HorizLine( SHORT Row, SHORT Left, SHORT Right, BYTE byChar, BYTE byAttr )
{
   if( s_under_xterm )
      byChar = ACS_HLINE;

   if( Left < Right )
      mvhline( Row, Left, byChar | A_ALTCHARSET | s_attribmap_table[ byAttr ],
               Right - Left + 1 );
   else
      mvhline( Row, Right, byChar | A_ALTCHARSET | s_attribmap_table[ byAttr ],
               Left - Right + 1 );

   return 0;
}

USHORT hb_gt_VertLine( SHORT Col, SHORT Top, SHORT Bottom, BYTE byChar, BYTE byAttr )
{
   SHORT Row;

   if( Top <= Bottom )
      Row = Top;
   else
   {
      uRow = Bottom;
      Bottom = Top;
   }

   if( s_under_xterm )
      byChar = ACS_VLINE;

   mvvline( uRow, Col, byChar | A_ALTCHARSET | s_attribmap_table[ byAttr ],
            Bottom - Row + 1 );

   return 0;
}

BOOL hb_gt_Suspend()
{
   /* TODO: save all settings */
   hb_gt_keyboard_Exit();
   hb_mouse_Exit();
   hb_gt_terminal_Exit();

   return TRUE;
}

BOOL hb_gt_Resume()
{
   /* TODO: restore settings */
   hb_gt_terminal_Init();
   hb_mouse_Init();
   hb_gt_keyboard_Init();
   
   return TRUE;
}

BOOL hb_gt_PreExt()
{
   refresh();

   return TRUE;
}

BOOL hb_gt_PostExt()
{
   return TRUE;
}
