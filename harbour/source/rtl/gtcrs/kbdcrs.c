/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Keyboard subsystem based on ncurses.
 *
 * Copyright 2000 Ryszard Glab <rglab@imid.med.pl>
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

/* functions defined in mousecrs.c
 */
extern int hb_mouse_xevent( char *, HB_inkey_enum );
extern int hb_mouse_key( void );

static void hb_gt_Add_terminfo_keymap( int, char * );
static void hb_gt_Add_keymap( int, char * );

/* max number of characters in a keymapped string */
#define HB_MAX_KEYMAP_CHARS     16

struct key_map_struc
{
   int inkey_code;
   int length;
   char * key_string;
   struct key_map_struc * Next;
};

#define HB_HASH_KEY   128
static struct key_map_struc * s_keymap_table[ HB_HASH_KEY ];
static BOOL s_under_xterm;
static char * s_mouse_event_seq;
static int s_mouse_event_len;

void hb_gt_keyboard_Init( void )
{
   int i;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Initialize_Keyboard()"));

   /* Initialize keycode table */
   for( i = 0; i < HB_HASH_KEY; i++ )
      s_keymap_table[ i ] = NULL;

   s_under_xterm = ( strncmp( getenv("TERM"), "xterm", 5 ) == 0 );
   if( s_under_xterm )
   {
      /* NOTE: under xterm \E[M is used as a leading code for a mouse event
         Events are as follows:
         \E[M   - prefix
         b0     - a byte with buttons state
         b1     - column position of a mouse pointer
         b2     - row position of a mouse pointer
      */
      s_mouse_event_seq = tigetstr( "kmous" );
      if( s_mouse_event_seq == NULL || s_mouse_event_seq == ( char * ) -1 )
         s_mouse_event_len = 0;
      else
         s_mouse_event_len = strlen( s_mouse_event_seq );
      hb_gt_Add_terminfo_keymap( K_HOME, "kfnd" );
      hb_gt_Add_terminfo_keymap( K_END, "kslt" );
      /* workaraound for xterm bug */
      hb_gt_Add_terminfo_keymap( K_UP, "cuu1" );
      hb_gt_Add_terminfo_keymap( K_RIGHT, "cuf1" );
      hb_gt_Add_keymap( K_LEFT, "\033[D" );
      hb_gt_Add_keymap( K_DOWN, "\033[B" );
   }
   else
   {
      /* NOTE: ncurses doesn't report any mouse events when run under GPM
      */
      s_mouse_event_len = 0;
   }

   hb_gt_Add_terminfo_keymap( K_ENTER, "kent" );
   hb_gt_Add_terminfo_keymap( K_ENTER, "ind" );
   hb_gt_Add_terminfo_keymap( K_TAB, "ht" );
   hb_gt_Add_terminfo_keymap( K_DOWN, "kcud1" );
   hb_gt_Add_terminfo_keymap( K_UP, "kcuu1" );
   hb_gt_Add_terminfo_keymap( K_LEFT, "kcub1" );
   hb_gt_Add_terminfo_keymap( K_RIGHT, "kcuf1" );
   hb_gt_Add_terminfo_keymap( K_HOME, "khome" );
   hb_gt_Add_terminfo_keymap( K_END, "kend" );
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

void hb_gt_keyboard_Exit( void )
{
   int i, k;
   struct key_map_struc *tmp;

   HB_TRACE(HB_TR_DEBUG, ("hb_kbd_Exit()"));

   for( i = 0; i < HB_HASH_KEY; i++ )
   {
      tmp = s_keymap_table[ i ];
      k = 0;
      while( tmp )
      {
         s_keymap_table[ i ] = tmp->Next;
         hb_xfree( tmp );
         tmp = s_keymap_table[ i ];
         k++;
      }
   }
}

int hb_gt_ExtendedKeySupport()
{
   return 0;
}
int hb_gt_ReadKey( HB_inkey_enum eventmask )
{
   static char key_codes[ HB_MAX_KEYMAP_CHARS + 1 ]; /* buffer for multi-characters keycodes */
   static int key_waiting = -1; /* position of next character from buffer if > 0 */
   int ch;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey(%d)", (int) eventmask));

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
      ch = hb_mouse_key();
   else
   {
      if( ch == 3 )
      {
         /* Ctrl-C was pressed */
         ch = HB_BREAK_FLAG;
      }
      else
      {
         int i = 0;
         BYTE sum;

         key_codes[ 0 ] = sum = ch;
         while( ( ch = getch() ) != ERR && i <= HB_MAX_KEYMAP_CHARS )
         {
            key_codes[ ++i ] = ch;
/*fprintf( stderr, "key%i=%i(%c)\n", i, ch, ch );
fflush( stderr );
*/
            sum += ch;
         }
         key_codes[ ++i ] = 0;
         sum &= HB_HASH_KEY - 1;

         ch = 0;
         if( s_keymap_table[ sum ] )
         {
            /* there is an entry in the hash table */
            struct key_map_struc * tmp = s_keymap_table[ sum ];

            while( ( ch == 0 ) && tmp )
            {
               /* now look for exact match */
               if( ( i == tmp->length ) && ( memcmp( tmp->key_string, key_codes, i ) == 0 ) )
               {
                  ch = tmp->inkey_code;   /* keycode found */
                  tmp = NULL; /* NOTE: tmp->inkey_code can be set to 0 */
               }
               else
                  tmp = tmp->Next;
            }

         }

         if( ch == 0 )
         {
            if( s_mouse_event_len )
            {
               /* check for mouse event */
               if( memcmp( s_mouse_event_seq, key_codes, s_mouse_event_len ) == 0 )
               {
                  /* Convert the mouse event into INKEY keycodes */
                  return hb_mouse_xevent( key_codes+s_mouse_event_len, eventmask );
               }
            }
            /* keymap not found */
            if( i == 1 )
               ch = key_codes[ 0 ];
            else
            {
               key_waiting = 0; /* return raw key sequence */
               ch = HB_K_MULTICODE;
            }
         }
      }
   }
   return ch;
}


static void hb_gt_Add_keymap( int InkeyCode, char *key_string )
{
   struct key_map_struc * keymap;
   int iLength = strlen( key_string );
   int i = 0;
   BYTE sum = 0;

   if( iLength && iLength <= HB_MAX_KEYMAP_CHARS )
   {
      while( i < iLength )
         sum += key_string[ i++ ];
      sum &= HB_HASH_KEY - 1;

      keymap = hb_xgrab( sizeof( struct key_map_struc ) );
      keymap->inkey_code = InkeyCode;
      keymap->key_string = key_string;
      keymap->length = iLength;
      keymap->Next = NULL;

      if( s_keymap_table[ sum ] )
      {
         struct key_map_struc * tmp = s_keymap_table[ sum ];
         while( tmp->Next )
            tmp = tmp->Next;
         tmp->Next = keymap;
      }
      else
         s_keymap_table[ sum ] = keymap;
   }
}

static void hb_gt_Add_terminfo_keymap( int InkeyCode, char * capname )
{
   char * code;

   code = tigetstr( capname );
   if( ( code != NULL ) && ( code != ( char * ) -1 ) )
      hb_gt_Add_keymap( InkeyCode, code );
}

