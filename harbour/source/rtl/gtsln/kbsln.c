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

#include <sys/ioctl.h>

#ifdef M_UNIX
   #include <sys/termio.h>
   #include <sys/vtkd.h>
#endif

#ifndef IBMPC_SYSTEM
   #include <termios.h> /* we're assuming target has termios - should be better done */
#endif

#include "hbapigt.h"
#include "inkey.ch"

/* ----------------------------------------------- */

/* abort key is Ctrl+C on Unix (arbitrary chosen), on Dos it works as Alt+C (strange !) */
#ifdef OS_UNIX_COMPATIBLE
   #define CTRL_C 3
#else
   #define CTRL_C 46
#endif
#define HB_GT_ABORT_KEY CTRL_C

/* 2/10 of a second waiting for the next char after ESC */
#define ESC_TIMEOUT 2

/* keyboard states - these should be taken
   from system includes, not be hard coded
*/
#if defined(__linux__)
   #define SHIFT_PRESSED        1
   #define ALTR_PRESSED         2
   #define CONTROL_PRESSED      4
   #define ALTL_PRESSED         8
   #define ALT_PRESSED (ALTL_PRESSED || ALTR_PRESSED)
#elif defined(M_UNIX)         /* SCO */
   #define SHIFT_PRESSED        1
   #define ALTR_PRESSED         8
   #define CONTROL_PRESSED      2
   #define ALTL_PRESSED         4
   #define ALT_PRESSED (ALTL_PRESSED || ALTR_PRESSED)
#else /* we don't know how to do this */
   #define SHIFT_PRESSED        0
   #define ALTR_PRESSED         0
   #define CONTROL_PRESSED      0
   #define ALTL_PRESSED         0
   #define ALT_PRESSED (ALTL_PRESSED || ALTR_PRESSED)
#endif

/* extra keysyms definitions */
#define SL_KEY_MAX  1000
#define SL_KEY_ESC  SL_KEY_MAX + 1
#define SL_KEY_ALT_L( ch )  ( SL_KEY_MAX + ( ( unsigned int )ch ) )

/* indicates that screen size has changed */
extern BOOL hb_gt_sln_bScreen_Size_Changed;

static BOOL s_linuxConsole = FALSE;
static BOOL s_underXTerm = FALSE;

int hb_gt_Kbd_State();

/* key translations tables - notice problems with compilation after changes */
#include "keytrans.c"

static void hb_gt_Init_KeyTranslat()
{
   char ch, keyname[ SLANG_MAX_KEYMAP_KEY_SEQ + 1 ];
   int keynum, i;

#ifdef IBMPC_SYSTEM
   keynum = 11;
   keyname[ 0 ] = '^';
   keyname[ 1 ] = '@';
   keyname[ 3 ] = 0;

    /* define Shft/Ctrl/Alt+Fn on DOS - these are hard coded in Slang */
   for( ch = 0x54; ch <= 0x71; ch++ )
   {
      keyname[ 2 ] = ch;
      SLkp_define_keysym( keyname, SL_KEY_F(keynum) );
      keynum++;
   }

    /* define Alt+Key on DOS - these are hard coded in Slang */
   i = 0;
   while( i < sizeof( transDosScanCodeTab ) )
   {
      i++;
      if( transDosScanCodeTab[i] < 32 )
      {
         keyname[ 2 ] = '^';
         keyname[ 3 ] = transDosScanCodeTab[i]+'A'-1;
         keyname[ 4 ] = 0;
      }
/*
      else if( transDosScanCodeTab[i] >= 127 )
      {
         sprintf( &keyname[ 2 ], "\\x%02X", transDosScanCodeTab[i] );
         keyname[ 6 ] = 0;
      }
*/
      else
      {
         keyname[ 2 ] = transDosScanCodeTab[i];
         keyname[ 3 ] = 0;
      }
      SLkp_define_keysym( keyname, SL_KEY_ALT_L( transDosScanCodeTab[i-1] ) );
      i++;
   }

#else

    char *keyseq;

   /* on Unix systems ESC is a special key so let
      assume ESC is also a doble pressed ESCkey
   */
   SLkp_define_keysym( "^[^[", SL_KEY_ESC );

   /* try to define Shft-Fn and Ctrl-Fn keys.
      Because we assume terminal has only 10 Fkeys
      so F11-F30 is generated with Shift & Ctrl.
      This is not guaranteed to work in all cases
   */
   keynum = 11;
   keyname[ 0 ] = 'F';
   keyname[ 2 ] = 0;

   /* Shft & Ctrl FKeys definition takes place in two
      phases : from '1' to '9' and from 'A' to 'K'
   */
   for( i=1; i<=2; i++ )
   {
      for( ch = ( i==1 ? '1' : 'A' ); ch <= ( i==1 ? '9' : 'K' ); ch++ )
      {
         keyname[ 1 ] = ch;
         keyseq = SLtt_tgetstr( keyname );
         if( (keyseq != NULL) && (keyseq[0] != 0) )
         {
            if( (keyseq != NULL) && (keyseq[0] != 0) )
               SLkp_define_keysym( keyseq, SL_KEY_F(keynum) );
         }
         keynum++;
      }
   }

   /* if we are on linux console pressing Alt generates ^[ before sequence */
   if( s_linuxConsole || s_underXTerm )
   {
      keyname[ 0 ] = 033;
      keyname[ 2 ] = 0;

      /* Alt+Letter & Alt+digit definition takes place in
         two phases : from '0' to '9' and from 'A' to 'Z'
      */
      for( i=1; i<=2; i++ )
      {
         for( ch = ( i==1 ? '0' : 'A' ); ch <= ( i==1 ? '9' : 'Z' ); ch++ )
         {
            keyname[ 1 ] = ch;
            /* QUESTION: why Slang reports error for defining Alt+O ???.
                         Have I any error in key definitiions ???
            */
            if( ch != 'O' )
               SLkp_define_keysym( keyname, SL_KEY_ALT_L( ch ) );

            keyname[ 1 ] = (ch+' ');
            SLkp_define_keysym( keyname, SL_KEY_ALT_L( ch+' ' ) );
         }
      }
   }
#endif
}

int hb_gt_Init_Terminal(int phase)
{
#ifndef IBMPC_SYSTEM
   struct termios newTTY;
#endif
   int ret = 0;

   /* Ctrl-C to abort, no flow-control, no output processing */
   if( SLang_init_tty(HB_GT_ABORT_KEY, 0, 0) != (-1) )
   {
#ifndef IBMPC_SYSTEM
      /* do missing disable of start/stop processing */
      if( tcgetattr( SLang_TT_Read_FD, &newTTY ) == 0 )
      {
         newTTY.c_cc[VSTOP]  = 255;  /* disable ^S start/stop processing */
         newTTY.c_cc[VSTART] = 255;  /* disable ^Q start/stop processing */
         newTTY.c_cc[VSUSP]  = 255;  /* disable ^Z suspend processing */

         if( tcsetattr(  SLang_TT_Read_FD, TCSADRAIN, &newTTY ) == 0 )
            /* everything looks ok so far */
#endif
            ret = 1;
#ifndef IBMPC_SYSTEM
      }
#endif
   }

   /* first time init phase - we don't want this
      after return from system command ( see run.c )
   */
   if( ret && (phase == 0) )
   {
      /* an uncertain way to check if we run under linux console */
      s_linuxConsole = ( !strncmp ( getenv("TERM"), "linux", 5 ) );
      /* an uncertain way to check if we run under xterm */
      s_underXTerm = ( strstr( getenv("TERM"), "xterm" ) != NULL );

      /* define keyboard translations */
      hb_gt_Init_KeyTranslat();
   }

   return ret;
}

#undef DO_LOCAL_DEBUG

int hb_gt_ReadKey( HB_inkey_enum eventmask )
{
   unsigned int ch, kbdflags;
#ifdef DO_LOCAL_DEBUG
   USHORT savy, savx;
#endif

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey(%d)", (int) eventmask));

   HB_SYMBOL_UNUSED( eventmask );

   /* user AbortKey break */
   if( SLKeyBoard_Quit == 1 )
      return HB_BREAK_FLAG;

   /* has screen size changed ? */
   if( hb_gt_sln_bScreen_Size_Changed )
   {
      hb_gt_sln_bScreen_Size_Changed = FALSE;
      SLtt_get_screen_size();
#if SLANG_VERSION > 10202
      SLsmg_reinit_smg();
#endif
      /* TODO: we need here some kind of screen redrawing */
      /*SLsmg_refresh ();*/
   }

   if( SLang_input_pending( 0 ) > 0 )
   {
      kbdflags = hb_gt_Kbd_State();

/* ------- one key ESC key handling ----------------- */

      /* NOTE: This will probably not work on slow terminals
               or on a very busy lines (i.e. modem lines )
      */
      ch = SLang_getkey ();

      if( ch == 033 )   /* escape */
      {
         if( 0 == SLang_input_pending (ESC_TIMEOUT) )
            return 033;
      }

      SLang_ungetkey (ch);

/* ------------------------------------------------- */

      ch = SLkp_getkey();

      if( ch != SL_KEY_ERR )
      {
         int i;

#ifdef DO_LOCAL_DEBUG
         hb_gtGetPos( &savy, &savx );
         SLsmg_gotorc( 23, 0 );
         SLsmg_printf( "%d %d  ", ch, kbdflags );
         SLsmg_gotorc( savy, savx );
#endif

         /* user AbortKey break */
         if( ch == HB_GT_ABORT_KEY )
            return HB_BREAK_FLAG;

         if( ch < 32 )  /* control characters - simply return */
            return ch;

         else if( ( ch >= 32 ) && ( ch < 256 ) )/* normal characters ? */
         {
            if( kbdflags & ALT_PRESSED )
            {
               /* lower to upper case */
               if( (ch >= 'a') && (ch <= 'z') )
                  ch -= ' ';

               /* alt + letter */
               if( (ch >= 'A') && (ch <= 'Z') )
                  /* returned value is next in a table */
                  return transAltKeyLetterTab[ (ch-'A')*2+1 ];

               /* alt + digit */
               if  ( (ch >= '0') && (ch <= '9') )
                  /* returned value is next in a table */
                  return transAltKeyDigitTab[ (ch-'0')*2+1 ];
            }

            return ch;
         }

         /* standard Slang keys */
         else if( ( ch >= 256 ) && ( ch <= SL_KEY_ESC ) )
         {
            for( i = 0; i < ( sizeof( transKeyFunTab ) / sizeof( int ) ); i++ )
            {
               if( transKeyFunTab[ i++ ] == ch )
                  return transKeyFunTab[ i ];
            }
         }

         /* Linux/Dos Alt+A-Z keys */
         else if( ( ch >= SL_KEY_ALT_L( 'A' ) ) && ( ch <= SL_KEY_ALT_L( 'Z' ) ) )
            /* returned value is next in a table */
            return transAltKeyLetterTab[ (ch-SL_KEY_MAX-'A')*2+1 ];

         /* Linux     Alt+a-z keys - lower to upper conersion */
         else if( ( ch >= SL_KEY_ALT_L( 'a' ) ) && ( ch <= SL_KEY_ALT_L( 'z' ) ) )
            /* returned value is next in a table */
            return transAltKeyLetterTab[ (ch-SL_KEY_MAX-(' ')-('A'))*2+1 ];

         /* Linux/Dos Alt+0-9 keys */
         else if( ( ch >= SL_KEY_ALT_L( '0' ) ) && ( ch <= SL_KEY_ALT_L( '9' ) ) )
            /* returned value is next in a table */
            return transAltKeyDigitTab[ (ch-SL_KEY_MAX-'0')*2+1 ];

         return ch;
      }
   }

   return 0;
}

static int hb_gt_try_get_Kbd_State()
{
#if defined(__linux__)

   unsigned char modifiers = 6;

   if( ioctl (0, TIOCLINUX, &modifiers) < 0 )
      return 0;

   return ( int ) modifiers;

#elif defined(M_UNIX)

   int modifiers = 0;
   int IOcommand = 0;

   if( ioctl(0, TCGETSC, &modifiers) >= 0 )
   {
      if( modifiers == KB_XSCANCODE )
      {
         IOcommand = KB_ISSCANCODE;
         if( ioctl(0, TCSETSC, &IOcommand) >= 0 )
         {
            if( ioctl(0, KDGKBSTATE, &modifiers) < 0 )
               modifiers = 0;
         }
         else modifiers = 0;

         IOcommand = KB_XSCANCODE;
         if( ioctl(0, TCSETSC, &IOcommand) < 0 )
            modifiers = 0;
      }
      else if( ioctl(0, KDGKBSTATE, &modifiers) < 0 )
         modifiers = 0;

      return modifiers;
   }
#endif

   return 0;
}

int hb_gt_Shft_Pressed()
{
   return (hb_gt_try_get_Kbd_State() & SHIFT_PRESSED) != 0;
}

int hb_gt_Ctrl_Pressed()
{
   return (hb_gt_try_get_Kbd_State() & CONTROL_PRESSED) != 0;
}

int hb_gt_Alt_Pressed()
{
   return (hb_gt_try_get_Kbd_State() & ALT_PRESSED) != 0;
}

int hb_gt_Kbd_State()
{
   return hb_gt_try_get_Kbd_State();
}

