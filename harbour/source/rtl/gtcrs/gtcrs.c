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

#include <curses.h>
#include <unistd.h>

#include "hbapigt.h"
#include "hbinit.h"

#include "inkey.ch"

static USHORT s_uiDispCount;

static void gt_GetMaxRC(int* r, int* c);
static void gt_GetRC(int* r, int* c);
static void gt_SetRC(int r, int c);

static void hb_gt_Add_terminfo_keymap( int, char * );
static void hb_gt_Add_keymap( int, char * );

/* max number of characters in a keymapped string */
#define HB_MAX_KEYMAP_CHARS	8

struct key_map_struc
{
    int inkey_code;
    int length;
    char *key_string;
    struct key_map_struc *Next;
};

static struct key_map_struc *s_keymap_table = NULL;
static unsigned s_attribmap_table[ 256 ]; /* mapping from DOS style attributes */
static BOOL s_under_buggy_xterm;
static int s_alternate_char_set;

static void hb_gt_Initialize_Terminal( void )
{
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
      for( backg=0; backg<COLORS; backg++ )
         for( foreg=0; foreg<COLORS; foreg++ )
	       init_pair( backg*COLORS+foreg, color_map[foreg], color_map[backg] );
	    
      for( i=0; i<256; i++  )
      {
         backg = ( i >> 4 ) & 0x07;    /* bits 4-6, bit 7 is blinking attribute */
	 foreg = ( i & 0x07 );  
	 s_attribmap_table[ i ] = COLOR_PAIR( backg*COLORS + foreg );
	 if( i & 0x08 )
	     s_attribmap_table[ i ] |= A_BOLD;  /* 4-th bit is an intensity bit */
	 if( i & 0x80 )
	     s_attribmap_table[ i ] |= A_BLINK;  /* 7-th bit is blinking bit */
      }
   }

   cbreak();
   noecho();
   nodelay( stdscr, 1 );
   scrollok( stdscr, TRUE );
   raw();
   keypad( stdscr, FALSE );
   
   s_under_buggy_xterm = !strncmp( getenv("TERM"), "xterm", 5 );
   /* NOTE: using A_ALTCHARSET attribute is causing that small letters
     a-z are not printed under xterm (at least xterm from Linux 
     RedHat 6.1 distribution)
   */
   if( s_under_buggy_xterm )
   {
/*
      char * str;
      
      str = tigetstr( "enacs" );   / * enable alt character set * /
      if( (str != NULL) && (str != (char *)-1) )
         write( fileno(stdout), str, strlen(str) );

      str = tigetstr( "smacs" );   / * start alt characters set * /
      if( (str != NULL) && (str != (char *)-1) )
         write( fileno(stdout), str, strlen(str) );
*/
      s_alternate_char_set = 0;
   }
   else
      s_alternate_char_set = A_ALTCHARSET;
   
   bkgdset( ' ' );
   ripoffline( 0, NULL );
   
}

void hb_gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init()"));

   s_uiDispCount = 0;

   initscr();
   hb_gt_Initialize_Terminal();

    hb_gt_Add_terminfo_keymap( K_ENTER, "kent" );
    hb_gt_Add_terminfo_keymap( K_ENTER, "ind" );
    hb_gt_Add_terminfo_keymap( K_TAB, "ht" );
    hb_gt_Add_terminfo_keymap( K_DOWN, "kcud1" );
    hb_gt_Add_terminfo_keymap( K_UP, "kcuu1" );
    hb_gt_Add_terminfo_keymap( K_LEFT, "kcub1" );
    hb_gt_Add_terminfo_keymap( K_RIGHT, "kcuf1" );
    hb_gt_Add_terminfo_keymap( K_HOME, "khome" );
    hb_gt_Add_terminfo_keymap( K_HOME, "kfnd" ); /* xterm */
    hb_gt_Add_terminfo_keymap( K_END, "kend" );
    hb_gt_Add_terminfo_keymap( K_END, "kslt" ); /* xterm */
    hb_gt_Add_terminfo_keymap( K_BS, "kbs" );
    hb_gt_Add_terminfo_keymap( K_BS, "kcbt" );
    hb_gt_Add_terminfo_keymap( K_INS, "kich1" );
    hb_gt_Add_terminfo_keymap( K_DEL, "kdch1" );
    hb_gt_Add_terminfo_keymap( K_PGDN, "knp" );
    hb_gt_Add_terminfo_keymap( K_PGUP, "kpp" );
    hb_gt_Add_terminfo_keymap( K_F1, "kf1" );
    hb_gt_Add_terminfo_keymap( K_F2, "kf2" );
    hb_gt_Add_terminfo_keymap( K_F3, "kf3" );
    hb_gt_Add_terminfo_keymap( K_F4, "kf4" );
    hb_gt_Add_terminfo_keymap( K_F5, "kf5" );
    hb_gt_Add_terminfo_keymap( K_F6, "kf6" );
    hb_gt_Add_terminfo_keymap( K_F7, "kf7" );
    hb_gt_Add_terminfo_keymap( K_F8, "kf8" );
    hb_gt_Add_terminfo_keymap( K_F9, "kf9" );
    hb_gt_Add_terminfo_keymap( K_F10, "kf10" );
    hb_gt_Add_terminfo_keymap( K_F11, "kf11" );
    hb_gt_Add_terminfo_keymap( K_F12, "kf12" );
    hb_gt_Add_terminfo_keymap( K_SH_F1, "kf13"  );
    hb_gt_Add_terminfo_keymap( K_SH_F2, "kf14"  );
    hb_gt_Add_terminfo_keymap( K_SH_F3, "kf15"  );
    hb_gt_Add_terminfo_keymap( K_SH_F4, "kf16"  );
    hb_gt_Add_terminfo_keymap( K_SH_F5, "kf17"  );
    hb_gt_Add_terminfo_keymap( K_SH_F6, "kf18"  );
    hb_gt_Add_terminfo_keymap( K_SH_F7, "kf19"  );
    hb_gt_Add_terminfo_keymap( K_SH_F8, "kf20"  );
    hb_gt_Add_terminfo_keymap( K_SH_F9, "kf21"  );
    hb_gt_Add_terminfo_keymap( K_SH_F10, "kf22"  );
    hb_gt_Add_terminfo_keymap( K_SH_F11, "kf23" );
    hb_gt_Add_terminfo_keymap( K_SH_F12, "kf24" );
    hb_gt_Add_terminfo_keymap( K_ALT_TAB, "kcbt" );
    hb_gt_Add_terminfo_keymap( K_HOME, "ka1" );
    hb_gt_Add_terminfo_keymap( K_PGUP, "ka3" );
    hb_gt_Add_terminfo_keymap( K_END, "kc1" );
    hb_gt_Add_terminfo_keymap( K_PGDN, "kc3" );
    hb_gt_Add_keymap( K_ALT_A, "\033a" );
    hb_gt_Add_keymap( K_ALT_A, "\033A" );
    hb_gt_Add_keymap( K_ALT_B, "\033b" );
    hb_gt_Add_keymap( K_ALT_B, "\033B" );
    hb_gt_Add_keymap( K_ALT_C, "\033c" );
    hb_gt_Add_keymap( K_ALT_C, "\033C" );
    hb_gt_Add_keymap( K_ALT_D, "\033d" );
    hb_gt_Add_keymap( K_ALT_D, "\033D" );    
    hb_gt_Add_keymap( K_ALT_E, "\033e" );
    hb_gt_Add_keymap( K_ALT_E, "\033E" );    
    hb_gt_Add_keymap( K_ALT_F, "\033f" );
    hb_gt_Add_keymap( K_ALT_F, "\033F" );    
    hb_gt_Add_keymap( K_ALT_G, "\033g" );
    hb_gt_Add_keymap( K_ALT_G, "\033G" );
    hb_gt_Add_keymap( K_ALT_H, "\033h" );
    hb_gt_Add_keymap( K_ALT_H, "\033H" );    
    hb_gt_Add_keymap( K_ALT_I, "\033i" );
    hb_gt_Add_keymap( K_ALT_I, "\033I" );    
    hb_gt_Add_keymap( K_ALT_J, "\033j" );
    hb_gt_Add_keymap( K_ALT_J, "\033J" );    
    hb_gt_Add_keymap( K_ALT_K, "\033k" );
    hb_gt_Add_keymap( K_ALT_K, "\033K" );
    hb_gt_Add_keymap( K_ALT_L, "\033l" );
    hb_gt_Add_keymap( K_ALT_L, "\033L" );    
    hb_gt_Add_keymap( K_ALT_M, "\033m" );
    hb_gt_Add_keymap( K_ALT_M, "\033M" );
    hb_gt_Add_keymap( K_ALT_N, "\033n" );
    hb_gt_Add_keymap( K_ALT_N, "\033N" );    
    hb_gt_Add_keymap( K_ALT_O, "\033o" );
    hb_gt_Add_keymap( K_ALT_O, "\033O" );
    hb_gt_Add_keymap( K_ALT_P, "\033p" );
    hb_gt_Add_keymap( K_ALT_P, "\033P" );    
    hb_gt_Add_keymap( K_ALT_Q, "\033q" );
    hb_gt_Add_keymap( K_ALT_Q, "\033Q" );    
    hb_gt_Add_keymap( K_ALT_R, "\033r" );
    hb_gt_Add_keymap( K_ALT_R, "\033R" );    
    hb_gt_Add_keymap( K_ALT_S, "\033s" );
    hb_gt_Add_keymap( K_ALT_S, "\033S" );    
    hb_gt_Add_keymap( K_ALT_T, "\033t" );
    hb_gt_Add_keymap( K_ALT_T, "\033T" );    
    hb_gt_Add_keymap( K_ALT_U, "\033u" );
    hb_gt_Add_keymap( K_ALT_U, "\033U" );    
    hb_gt_Add_keymap( K_ALT_V, "\033v" );
    hb_gt_Add_keymap( K_ALT_V, "\033V" );    
    hb_gt_Add_keymap( K_ALT_W, "\033w" );
    hb_gt_Add_keymap( K_ALT_W, "\033W" );
    hb_gt_Add_keymap( K_ALT_X, "\033x" );
    hb_gt_Add_keymap( K_ALT_X, "\033X" );
    hb_gt_Add_keymap( K_ALT_Y, "\033y" );
    hb_gt_Add_keymap( K_ALT_Y, "\033Y" );
    hb_gt_Add_keymap( K_ALT_Z, "\033z" );
    hb_gt_Add_keymap( K_ALT_Z, "\033Z" );
    hb_gt_Add_keymap( K_ALT_1, "\0331" );
    hb_gt_Add_keymap( K_ALT_2, "\0332" );
    hb_gt_Add_keymap( K_ALT_3, "\0333" );
    hb_gt_Add_keymap( K_ALT_4, "\0334" );
    hb_gt_Add_keymap( K_ALT_5, "\0335" );
    hb_gt_Add_keymap( K_ALT_6, "\0336" );
    hb_gt_Add_keymap( K_ALT_7, "\0337" );
    hb_gt_Add_keymap( K_ALT_8, "\0338" );
    hb_gt_Add_keymap( K_ALT_9, "\0339" );
    hb_gt_Add_keymap( K_ALT_0, "\0330" );
    hb_gt_Add_keymap( K_ALT_ENTER, "\033\n" );
    hb_gt_Add_keymap( K_ALT_EQUALS, "\033=" );
    
}

void hb_gt_Exit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Exit()"));

   noraw();
   refresh();
   endwin();
   
   if( s_keymap_table )
   {
      struct key_map_struc *tmp = s_keymap_table;
      while( tmp )
      {
         s_keymap_table = s_keymap_table->Next;
         hb_xfree( tmp );
	      tmp = s_keymap_table;
      }
   }
}

int hb_gt_ReadKey( HB_inkey_enum eventmask )
{
   static char key_codes[ HB_MAX_KEYMAP_CHARS+1 ]; /* buffer for multi-characters keycodes */
   static int key_waiting = -1; /* position of next character from buffer if > 0 */
   int ch;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey(%d)", (int) eventmask));

   HB_SYMBOL_UNUSED( eventmask );

   if( key_waiting >= 0 )
   {
       /* return next character from the buffer */
       ch = key_codes[ key_waiting++ ];
       if( key_codes[ key_waiting ] == 0 )
           key_waiting = -1; /* the last character was retrieved */
       return ch;
   }
       
   ch = getch();
   if( ch == ERR )
      ch = 0;
   else
   {
      if( ch == 3 )
      {
         /* Ctrl-C was pressed */
	       ch = HB_BREAK_FLAG;
      }
      else if( s_keymap_table )
      {
         struct key_map_struc *tmp = s_keymap_table;
	 int i = 0;
	 
	 key_codes[ 0 ] = ch;
	 while( ( ch = getch() ) != ERR && i <= HB_MAX_KEYMAP_CHARS )
	     key_codes[ ++i ] = ch;
	 key_codes[ ++i ] = 0;

         ch = 0;
	 while( tmp )
	 {
	     if( (i == tmp->length) && (strcmp( tmp->key_string, key_codes ) == 0 ) )
	     {
	         ch = tmp->inkey_code;
		 tmp = NULL;
	    }
	    else
		tmp = tmp->Next;		         
         }
	 
	 if( ch == 0 )
	 {
	     /* keymap not found */
            if( i == 1 )
	        ch = key_codes[ 0 ];
	    else
	    {
	       key_waiting = 0;	/* return raw key sequence */
	       ch = K_HB_KEYCODES;
	    }
	}
      }
   }
   return ch;
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

   gt_GetMaxRC(&r, &c);
   return c;
}

USHORT hb_gt_GetScreenHeight( void )
{
   int r, c;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeight()"));

   gt_GetMaxRC(&r, &c);
   return r;
}

void hb_gt_SetPos( SHORT iRow, SHORT iCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd)", iRow, iCol));

   gt_SetRC(iRow, iCol);
}

SHORT hb_gt_Col( void )
{
   int r, c;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));

   gt_GetRC(&r, &c);
   return c;
}

SHORT hb_gt_Row( void )
{
   int r, c;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));

   gt_GetRC(&r, &c);
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
   chtype *pBuffer = (chtype *)pbyDst;
      
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetText(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pbyDst));

   if( s_uiDispCount == 0 )
      refresh();

   while( uiTop <= uiBottom )
   {
    for( i=uiLeft; i<=uiRight; i++, pBuffer++ )
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
   chtype *pBuffer = (chtype *)pbySrc;
   
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, pbySrc));

   Cols = uiRight - uiLeft + 1;
   while( uiTop <= uiBottom )
   {
      mvaddchnstr( uiTop, uiLeft, pBuffer, Cols );
      pBuffer +=Cols;
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
   int Count = uiRight - uiLeft + 1;
   int newAttr = s_attribmap_table[ byAttr ];
   short newColor;
   
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetAttribute(%hu, %hu, %hu, %hu, %d)", uiTop, uiLeft, uiBottom, uiRight, (int) byAttr));

   newColor = PAIR_NUMBER( newAttr );
   newAttr &= A_ATTRIBUTES;	/* extract attributes only */
   while( uiTop <= uiBottom )
      mvchgat( uiTop++, uiLeft, Count, newAttr, newColor, NULL );
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
      WINDOW *subw;

      subw = subwin( stdscr, uiBottom-uiTop+1, uiRight-uiLeft+1, uiTop, uiLeft );
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
         WINDOW *subw;
      
         subw = subwin( stdscr, uiBottom-uiTop+1, uiRight-uiLeft+1, uiTop, uiLeft );
         wbkgdset( subw, ' ' | s_attribmap_table[ byAttr ] );
         scrollok( subw, TRUE );
         wscrl( subw, iRows );
         delwin( subw );
      }
   
      if( iCols != 0 )
      {
         chtype *pScreen, *pTmp;
         int memsize;
         int RowCount, ColCount;
         int i, j;
         int newAttr;

         refresh();
         
         RowCount = uiBottom - uiTop + 1;
         ColCount = uiRight - uiLeft + 1;
         newAttr  = ' ' | s_attribmap_table[ byAttr ];
      
         memsize = hb_gt_RectSize( RowCount, ColCount );
         pScreen = (chtype *) hb_xgrab( memsize );
         hb_gt_GetText( uiTop, uiLeft, uiBottom, uiRight, (BYTE *)pScreen );

         if( iCols > 0 )
         {
            pTmp = pScreen;
            for( i=0; i<RowCount; i++ )      
            {
               for( j=ColCount - 1; j>=iCols; j-- )
                  pTmp[ j ] = pTmp[ j-1 ];
               for( j=0; j<iCols; j++ )
                  pTmp[ j ] = newAttr;
               pTmp += ColCount;
            }
         }
         else
         {
            int ColMove  = ColCount + iCols;
         
            pTmp = pScreen;
            for( i=0; i<RowCount; i++ )      
            {
               for( j=0; j<ColMove; j++ )
                  pTmp[ j ] = pTmp[ j-iCols ];
               for( j=ColMove; j<ColCount; j++ )
                  pTmp[ j ] = newAttr;
               pTmp += ColCount;
            }
         }
         hb_gt_PutText( uiTop, uiLeft, uiBottom, uiRight, (BYTE *)pScreen );
         hb_xfree( (BYTE *)pScreen );
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
   BOOL success;
   
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", uiRows, uiCols));

   /* NOTE: Not tested!!!
      Use it on your own risk!
   */
   endwin();
   success = ( ( resizeterm( uiRows, uiCols) == OK) ? TRUE : FALSE );
   initscr();
   hb_gt_Initialize_Terminal();
   
   return success;
}

void hb_gt_Replicate( BYTE byChar, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Replicate(%d, %lu)", (int) byChar, ulLen));

   /* TODO: this will write character c nlength times to the screen.
      Note that it is not used yet
      If there is no native function that supports this, it is
      already handled in a generic way by higher level functions.
   */

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
   getmaxyx(stdscr, y, x);
   *r = y;
   *c = x;
}

static void gt_GetRC(int* r, int* c)
{
   int y, x;
   getyx(stdscr, y, x);
   *r = y;
   *c = x;
}

static void gt_SetRC(int r, int c)
{
   move(r, c);
   if( s_uiDispCount == 0 )
      refresh();
}

char * hb_gt_Version( void )
{
   return "Harbour Terminal: ncurses (Linux console)";
}

USHORT hb_gt_DispCount()
{
   return s_uiDispCount;
}

static void hb_gt_Add_keymap( int InkeyCode, char *key_string )
{
   struct key_map_struc *keymap;
   int iLength = strlen( key_string );
   
      if( iLength && iLength <= HB_MAX_KEYMAP_CHARS )
      {
         keymap = hb_xgrab( sizeof( struct key_map_struc ) );
         keymap->inkey_code = InkeyCode;
         keymap->key_string = key_string;
         keymap->length = iLength;
         keymap->Next = NULL;
      
         if( s_keymap_table )
         {
             struct key_map_struc *tmp = s_keymap_table;
             while( tmp->Next )
                tmp =tmp->Next;
             tmp->Next = keymap;
         }
         else
            s_keymap_table = keymap;
     }    
}

static void hb_gt_Add_terminfo_keymap( int InkeyCode, char *capname )
{
   char * code;
   
   code = tigetstr( capname );
   if( (code != NULL) && (code != (char *)-1) )
   {
       hb_gt_Add_keymap( InkeyCode, code );
   }
}

