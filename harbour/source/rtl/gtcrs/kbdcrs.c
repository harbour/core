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

#include "inkey.ch"

/* functions defined in mousecrs.c
 */
extern int hb_mouse_xevent( char *, HB_inkey_enum );
extern int hb_mouse_key( void );

static void hb_gt_Add_terminfo_keymap( int, char * );
static void hb_gt_Add_alt_terminfo_keymap( int, char * );
static void hb_gt_Add_keymap( int, char *, BOOL );
static void hb_gt_ClearKeymap( void );

/* max number of characters in a keymapped string */
#define HB_MAX_KEYMAP_CHARS     16

struct key_map_struc
{
   int inkey_code;
   int length;
	BOOL bFreemem;
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

   {
      char * tmp = hb_getenv( "TERM" );
		tmp = hb_strupr( tmp );
      s_under_xterm = tmp && tmp[ 0 ] != '\0' && ( strstr( tmp, "TERM" ) != NULL );
      if( tmp )
         hb_xfree( ( void * ) tmp );
   }

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
      hb_gt_Add_keymap( K_LEFT, "\033[D", FALSE );
      hb_gt_Add_keymap( K_DOWN, "\033[B", FALSE );
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
   hb_gt_Add_keymap( K_ALT_A, "\033a", FALSE );
   hb_gt_Add_keymap( K_ALT_A, "\033A", FALSE );
   hb_gt_Add_keymap( K_ALT_B, "\033b", FALSE );
   hb_gt_Add_keymap( K_ALT_B, "\033B", FALSE );
   hb_gt_Add_keymap( K_ALT_C, "\033c", FALSE );
   hb_gt_Add_keymap( K_ALT_C, "\033C", FALSE );
   hb_gt_Add_keymap( K_ALT_D, "\033d", FALSE );
   hb_gt_Add_keymap( K_ALT_D, "\033D", FALSE );
   hb_gt_Add_keymap( K_ALT_E, "\033e", FALSE );
   hb_gt_Add_keymap( K_ALT_E, "\033E", FALSE );
   hb_gt_Add_keymap( K_ALT_F, "\033f", FALSE );
   hb_gt_Add_keymap( K_ALT_F, "\033F", FALSE );
   hb_gt_Add_keymap( K_ALT_G, "\033g", FALSE );
   hb_gt_Add_keymap( K_ALT_G, "\033G", FALSE );
   hb_gt_Add_keymap( K_ALT_H, "\033h", FALSE );
   hb_gt_Add_keymap( K_ALT_H, "\033H", FALSE );
   hb_gt_Add_keymap( K_ALT_I, "\033i", FALSE );
   hb_gt_Add_keymap( K_ALT_I, "\033I", FALSE );
   hb_gt_Add_keymap( K_ALT_J, "\033j", FALSE );
   hb_gt_Add_keymap( K_ALT_J, "\033J", FALSE );
   hb_gt_Add_keymap( K_ALT_K, "\033k", FALSE );
   hb_gt_Add_keymap( K_ALT_K, "\033K", FALSE );
   hb_gt_Add_keymap( K_ALT_L, "\033l", FALSE );
   hb_gt_Add_keymap( K_ALT_L, "\033L", FALSE );
   hb_gt_Add_keymap( K_ALT_M, "\033m", FALSE );
   hb_gt_Add_keymap( K_ALT_M, "\033M", FALSE );
   hb_gt_Add_keymap( K_ALT_N, "\033n", FALSE );
   hb_gt_Add_keymap( K_ALT_N, "\033N", FALSE );
   hb_gt_Add_keymap( K_ALT_O, "\033o", FALSE );
   hb_gt_Add_keymap( K_ALT_O, "\033O", FALSE );
   hb_gt_Add_keymap( K_ALT_P, "\033p", FALSE );
   hb_gt_Add_keymap( K_ALT_P, "\033P", FALSE );
   hb_gt_Add_keymap( K_ALT_Q, "\033q", FALSE );
   hb_gt_Add_keymap( K_ALT_Q, "\033Q", FALSE );
   hb_gt_Add_keymap( K_ALT_R, "\033r", FALSE );
   hb_gt_Add_keymap( K_ALT_R, "\033R", FALSE );
   hb_gt_Add_keymap( K_ALT_S, "\033s", FALSE );
   hb_gt_Add_keymap( K_ALT_S, "\033S", FALSE );
   hb_gt_Add_keymap( K_ALT_T, "\033t", FALSE );
   hb_gt_Add_keymap( K_ALT_T, "\033T", FALSE );
   hb_gt_Add_keymap( K_ALT_U, "\033u", FALSE );
   hb_gt_Add_keymap( K_ALT_U, "\033U", FALSE );
   hb_gt_Add_keymap( K_ALT_V, "\033v", FALSE );
   hb_gt_Add_keymap( K_ALT_V, "\033V", FALSE );
   hb_gt_Add_keymap( K_ALT_W, "\033w", FALSE );
   hb_gt_Add_keymap( K_ALT_W, "\033W", FALSE );
   hb_gt_Add_keymap( K_ALT_X, "\033x", FALSE );
   hb_gt_Add_keymap( K_ALT_X, "\033X", FALSE );
   hb_gt_Add_keymap( K_ALT_Y, "\033y", FALSE );
   hb_gt_Add_keymap( K_ALT_Y, "\033Y", FALSE );
   hb_gt_Add_keymap( K_ALT_Z, "\033z", FALSE );
   hb_gt_Add_keymap( K_ALT_Z, "\033Z", FALSE );
   hb_gt_Add_keymap( K_ALT_1, "\0331", FALSE );
   hb_gt_Add_keymap( K_ALT_2, "\0332", FALSE );
   hb_gt_Add_keymap( K_ALT_3, "\0333", FALSE );
   hb_gt_Add_keymap( K_ALT_4, "\0334", FALSE );
   hb_gt_Add_keymap( K_ALT_5, "\0335", FALSE );
   hb_gt_Add_keymap( K_ALT_6, "\0336", FALSE );
   hb_gt_Add_keymap( K_ALT_7, "\0337", FALSE );
   hb_gt_Add_keymap( K_ALT_8, "\0338", FALSE );
   hb_gt_Add_keymap( K_ALT_9, "\0339", FALSE );
   hb_gt_Add_keymap( K_ALT_0, "\0330", FALSE );
   hb_gt_Add_keymap( K_ALT_ENTER, "\033\n", FALSE );
   hb_gt_Add_keymap( K_ALT_EQUALS, "\033=", FALSE );
   hb_gt_Add_keymap( KP_ALT_SLASH, "\033/", FALSE );
   hb_gt_Add_keymap( KP_ALT_MINUS, "\033-", FALSE );
	/* 
	*/
	hb_gt_Add_alt_terminfo_keymap( K_ALT_PGUP, "kpp" );
	hb_gt_Add_alt_terminfo_keymap( K_ALT_PGDN, "knp" );
   hb_gt_Add_alt_terminfo_keymap( K_ALT_F1, "kf1" );
   hb_gt_Add_alt_terminfo_keymap( K_ALT_F2, "kf2" );
   hb_gt_Add_alt_terminfo_keymap( K_ALT_F3, "kf3" );
   hb_gt_Add_alt_terminfo_keymap( K_ALT_F4, "kf4" );
   hb_gt_Add_alt_terminfo_keymap( K_ALT_F5, "kf5" );
   hb_gt_Add_alt_terminfo_keymap( K_ALT_F6, "kf6" );
   hb_gt_Add_alt_terminfo_keymap( K_ALT_F7, "kf7" );
   hb_gt_Add_alt_terminfo_keymap( K_ALT_F8, "kf8" );
   hb_gt_Add_alt_terminfo_keymap( K_ALT_F9, "kf9" );
   hb_gt_Add_alt_terminfo_keymap( K_ALT_F10, "kf10" );
   hb_gt_Add_alt_terminfo_keymap( K_ALT_F11, "kf11" );
   hb_gt_Add_alt_terminfo_keymap( K_ALT_F12, "kf12" );
	
}

void hb_gt_keyboard_Exit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_kbd_Exit()"));

	hb_gt_ClearKeymap();
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

         key_codes[ 0 ] = ch;
			sum = ch;
         while( ( ch = getch() ) != ERR && i <= HB_MAX_KEYMAP_CHARS )
         {
            key_codes[ ++i ] = ch;
/*
fprintf( stderr, "key%i=%i(%c)\n", i, ch, ch );
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


static void hb_gt_Add_keymap( int InkeyCode, char *key_string, BOOL bxfree )
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
		keymap->bFreemem = bxfree;

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
      hb_gt_Add_keymap( InkeyCode, code, FALSE );
}

static void hb_gt_Add_alt_terminfo_keymap( int InkeyCode, char * capname )
{
   char * code;

   code = tigetstr( capname );
   if( ( code != NULL ) && ( code != ( char * ) -1 ) )
	{
   	int iLen = strlen( code );
		char *acode;
		
		acode = (char *)hb_xgrab( iLen+2 );
		acode[0] = '\033';
		memcpy( acode+1, code, iLen+1 );
      hb_gt_Add_keymap( InkeyCode, acode, TRUE );
	}
}

static void hb_gt_ClearKeymap( void )
{
   int i, k;
   struct key_map_struc *tmp;

   for( i = 0; i < HB_HASH_KEY; i++ )
   {
      tmp = s_keymap_table[ i ];
      k = 0;
      while( tmp )
      {
         s_keymap_table[ i ] = tmp->Next;
			if( tmp->bFreemem )
				hb_xfree( tmp->key_string );
         hb_xfree( tmp );
         tmp = s_keymap_table[ i ];
         k++;
      }
		s_keymap_table[ i ] = NULL;
   }
}

/*
	Add definition of nonstandard keycode mapping
	for example:
	HB_GT_ADDKEYMAP( 0 )            //Clear all default keymaps
	HB_GT_ADDKEYMAP( 30,"\033[6^" ) //Ctrl+PgDn
*/
HB_FUNC( HB_GT_ADDKEYMAP )
{
   if( ISNUM( 1 ) && ISCHAR( 2 ) )
	{
		char * code;
		int len = hb_parclen(2);
		
		code = hb_xgrab( len + 1 );
		memcpy( code, hb_parc(2), len );
		code[ len ] = '\0';
		hb_gt_Add_keymap( hb_parni( 1 ), code, TRUE );
	}
	else if( ISNUM( 1 ) )
	{
		if( hb_parni(1) == 0 )
			hb_gt_ClearKeymap();
	}
}
