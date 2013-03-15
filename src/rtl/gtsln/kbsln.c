/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Keyboard subsystem based on Slang screen library.
 *
 * Copyright 2000 Marek Paliwoda <paliwoda@inetia.pl>
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#include "gtsln.h"
#include <sys/ioctl.h>

#ifdef M_UNIX
   #include <sys/termio.h>
   #include <sys/vtkd.h>
#endif

/* we're assuming target has termios - should be better done */
#include <termios.h>

/* *********************************************************************** */

/* keyboard states - these should be taken
   from system includes, not be hard coded */
#if defined( __linux__ )
   #define SHIFT_PRESSED    1
   #define ALTR_PRESSED     2
   #define CONTROL_PRESSED  4
   #define ALTL_PRESSED     8
   #define ALT_PRESSED      ALTL_PRESSED
#elif defined( M_UNIX )         /* SCO */
   #define SHIFT_PRESSED    1
   #define ALTR_PRESSED     8
   #define CONTROL_PRESSED  2
   #define ALTL_PRESSED     4
   #define ALT_PRESSED      ALTL_PRESSED
#else /* we don't know how to do this */
   #define SHIFT_PRESSED    0
   #define ALTR_PRESSED     0
   #define CONTROL_PRESSED  0
   #define ALTL_PRESSED     0
   #define ALT_PRESSED      ALTL_PRESSED
#endif
#define HB_GT_KBD_MODIF_MASK \
   ( ( SHIFT_PRESSED | CONTROL_PRESSED | ALTL_PRESSED ) << 16 )

#define MOUSE_ALL_EVENTS_MASK \
   ( INKEY_MOVE | INKEY_LDOWN | INKEY_LUP | INKEY_RDOWN | INKEY_RUP )

/* extra keysyms definitions */
#define SL_KEY_NUM_5        SL_KEY_B2     /* this is checked explicitly */
#define SL_KEY_MAX          ( ( unsigned int ) 0x2000 )
#define SL_KEY_ESC          ( SL_KEY_MAX + 1 )
#define SL_KEY_MOU          ( SL_KEY_ESC + 1 )
#define SL_KEY_ALT( ch )  ( SL_KEY_MAX + ( ( unsigned int ) ch ) )

/* we choose Ctrl+\ as an abort key on Unixes where it is a SIGQUIT key by default */
/* abort key is Ctrl+\ on Unix ( but Ctrl+@ on Linux console ) */
static int s_hb_sln_Abort_key = 28;

/* *********************************************************************** */

/* DeadKey definition's ENVVAR name. This EnvVar contains */
/* an ASCII value of a key, which serves as a DeadKey */
static const char * s_DeadKeyEnvName = "HB_GTSLN_NATIONDEADKEY";

/* a table for Keys work with a Dead key. The first
   element contains a number of defined keys */
unsigned char hb_sln_convKDeadKeys[ 257 ];  /* it should be allocated by hb_xalloc() */

/* contains an integer value of a DeadKey or -1 */
static int s_iDeadKey = -1;

/* escape key delay */
#ifdef HB_SLANG_ONE_ESC
   int hb_sln_escDelay = 250;
#else
   int hb_sln_escDelay = 0;
#endif

HB_BOOL hb_sln_UnderLinuxConsole = HB_FALSE;
HB_BOOL hb_sln_UnderXterm        = HB_FALSE;

static int hb_sln_try_get_Kbd_State( void );

/* key translations tables - notice problems with compilation after changes */
#include "keytrans.c"

/* *********************************************************************** */

static void hb_sln_Init_TermType( void )
{
   const char * Env;

   /* an uncertain way to check if we run under linux console */
   Env = getenv( "TERM" );

   hb_sln_UnderLinuxConsole = Env && strncmp( Env, "linux", 5 ) == 0;

   /* an uncertain way to check if we run under xterm */
   hb_sln_UnderXterm = Env && ( strstr( Env, "xterm" ) != NULL ||
                                strncmp( Env, "rxvt", 4 ) == 0 );
}

/* *********************************************************************** */

static void hb_sln_Init_KeyTranslations( void )
{
   char ch, keyname[ SLANG_MAX_KEYMAP_KEY_SEQ + 1 ];
   int  keynum, i;
   char * keyseq;

   /* for defining ^[<Key> sequences - this simulates Alt+Keys */
   char AltChars[][ 2 ] =
   {
      { '0', '9' },
      { 'A', 'Z' },
      { 'a', 'z' }
   };

   /* on Unix systems ESC is a special key so let
      assume ESC is a doble pressed ESC key    */
   SLkp_define_keysym( ( char * ) "^[^[", SL_KEY_ESC );

   /* try to define Shft-Fn and Ctrl-Fn keys.
      Because we assume terminal has only 10 Fkeys
      so F11-F30 is generated with Shift & Ctrl.
      This is not guaranteed to work in all cases */
   keynum = 11;
   keyname[ 0 ] = 'F';
   keyname[ 2 ] = 0;

   /* Shft & Ctrl FKeys definition takes place in two
      phases : from '1' to '9' and from 'A' to 'K' */
   for( i = 1; i <= 2; i++ )
   {
      for( ch = ( i == 1 ? '1' : 'A' ); ch <= ( i == 1 ? '9' : 'K' ); ch++ )
      {
         keyname[ 1 ] = ch;
         keyseq = SLtt_tgetstr( keyname );
         if( keyseq != NULL && keyseq[ 0 ] != 0 )
            SLkp_define_keysym( keyseq, SL_KEY_F( keynum ) );
         keynum++;
      }
   }

   /* We assume Esc key is a Meta key which is treated as an Alt key.
      Also pressing Alt+Key on linux console and linux xterm gives the
      same key sequences as with Meta key so we are happy */

   keyname[ 0 ] = 033;
   keyname[ 2 ] = 0;

   /* Alt+Letter & Alt+digit definition takes place in three phases :
      from '0' to '9', from 'A' to 'Z' and from 'a' to 'z' */
   for( i = 0; i < 3; i++ )
   {
      for( ch = AltChars[ i ][ 0 ]; ch <= AltChars[ i ][ 1 ]; ch++ )
      {
         /* fprintf( stderr, "%d %c\n", i, ch ); */
         keyname[ 1 ] = ch;

         /* QUESTION: why Slang reports error for defining Alt+O ???.
                      Have I any hidden error in key definitiions ??? */
         if( ch != 'O' )
            SLkp_define_keysym( keyname, SL_KEY_ALT( ch ) );
      }
   }

   /* mouse events under xterm */
   if( hb_sln_UnderXterm )
   {
      keyseq = SLtt_tgetstr( ( char * ) "Km" );
      if( ( keyseq != NULL ) && ( keyseq[ 0 ] != 0 ) )
      {
         /* fprintf( stderr, "%s\r\n", keyseq ); */
         SLkp_define_keysym( keyseq, SL_KEY_MOU );
      }
   }

   /* five on numeric console */
   /* SLkp_define_keysym( "^[[G", SL_KEY_NUM_5 ); */
}

/* *********************************************************************** */

int hb_sln_Init_Terminal( int phase )
{
   struct termios newTTY;
   int ret = 0;

   /* first time init phase - we don't want this after
      return from system command ( see run.c )      */
   if( phase == 0 )
   {
      unsigned const char * p;

      /* check if we run under linux console or under xterm */
      hb_sln_Init_TermType();

#ifdef HB_OS_LINUX
      /* for Linux console */
      if( hb_sln_UnderLinuxConsole )
         s_hb_sln_Abort_key = 0;
#endif

      /* get Dead key definition */
      p = ( unsigned const char * ) getenv( s_DeadKeyEnvName );
      if( p && *p )
         s_iDeadKey = ( int ) *p;

      /* number of keys dealing with a Dead key */
      hb_sln_convKDeadKeys[ 0 ] = 0;
   }

   /* Ctrl+\ to abort, no flow-control, no output processing */
   if( SLang_init_tty( s_hb_sln_Abort_key, 0, 0 ) != -1 )
   {
      /* do missing disable of start/stop processing */
      if( tcgetattr( SLang_TT_Read_FD, &newTTY ) == 0 )
      {
         newTTY.c_cc[ VSTOP ]  = 255;  /* disable ^S start/stop processing */
         newTTY.c_cc[ VSTART ] = 255;  /* disable ^Q start/stop processing */
         newTTY.c_cc[ VSUSP ]  = 255;  /* disable ^Z suspend processing */
         /* already done in Slang library */
         /* newTTY.c_cc[ VDSUSP ] = 255; */  /* disable ^Y delayed suspend processing */

         if( tcsetattr( SLang_TT_Read_FD, TCSADRAIN, &newTTY ) == 0 )
            /* everything looks ok so far */
            ret = 1;
      }
   }

   /* first time init phase - we don't want this after
      return from system command ( see run.c )      */
   if( ret && ( phase == 0 ) )
   {
      /* define keyboard translations */
      hb_sln_Init_KeyTranslations();

      /* for binary search of key translations */
      hb_sln_SortKeyTranslationTable();
   }

   return ret;
}

/* *********************************************************************** */

int hb_gt_sln_ReadKey( PHB_GT pGT, int iEventMask )
{
   static int InDeadState = HB_FALSE;
   unsigned int ch, tmp, kbdflags;
   HB_BOOL fInput;
   int iKey;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_sln_ReadKey(%p,%d)", pGT, ( int ) iEventMask ) );

   /* user AbortKey break */
   if( SLKeyBoard_Quit == 1 )
      return HB_BREAK_FLAG;

   /* has screen size changed ? */
   if( hb_sln_bScreen_Size_Changed )
   {
      hb_sln_bScreen_Size_Changed = HB_FALSE;
      SLtt_get_screen_size();
#if SLANG_VERSION > 10202
      SLsmg_reinit_smg();
#endif

      /* TODO: we need here some kind of screen redrawing */
      /*SLsmg_refresh();*/
      HB_GTSELF_RESIZE( pGT, SLtt_Screen_Rows, SLtt_Screen_Cols );
      return HB_K_RESIZE;
   }

   fInput = SLang_input_pending( 0 ) != 0;
   iKey = hb_gt_sln_mouse_Inkey( iEventMask, ! fInput );
   if( ! fInput || iKey != 0 )
      return iKey;

#if HB_GT_KBD_MODIF_MASK
   kbdflags = hb_sln_try_get_Kbd_State();
#else
   kbdflags = 0;
#endif

/* ------- one key ESC handling ----------------- */
   /* NOTE: This will probably not work on slow terminals
      or on very busy lines (i.e. modem lines ) */
   ch = SLang_getkey();
   if( ch == 033 )   /* escape char received, check for any pending chars */
   {
      if( hb_sln_escDelay == 0 )
      {
         /* standard acction, wait a 1 second for next char and if not then exit */
         if( 0 == SLang_input_pending( 10 ) )
            return 0;
      }
      else
      {
         /* wait hb_sln_escDelay milisec for next char and in not return ESC keycode */
         if( 0 == SLang_input_pending( -HB_MAX( hb_sln_escDelay, 0 ) ) )
            return 033;
      }
   }

   /* user AbortKey break */
   if( ( int ) ch == s_hb_sln_Abort_key )
      return HB_BREAK_FLAG;

   SLang_ungetkey( ch );
/* ------------------------------------------------- */

   ch = SLkp_getkey();

   /* unrecognized character */
   if( ch == SL_KEY_ERR )
      return 0;

   /* Dead key handling */
   if( InDeadState )
   {
      InDeadState = HB_FALSE;
      if( ( int ) ch == s_iDeadKey ) /* double press Dead key */
         return ch;
      if( ch < 256 )  /* is this needed ??? */
      {
         int i;
         for( i = 0; i < ( int ) hb_sln_convKDeadKeys[ 0 ]; i++ )
            if( ( int ) hb_sln_convKDeadKeys[ 2 * i + 1 ] == ( int ) ch )
               return ( int ) hb_sln_convKDeadKeys[ 2 * i + 2 ];
      }
      return 0;
   }
   else if( ( int ) ch == s_iDeadKey )
   {
      /* entering Dead key state */
      InDeadState = HB_TRUE;
      return 0;
   }

   /* any special key ? */
   if( ( tmp = ( ch | ( kbdflags << 16 ) ) ) > 256 )
   {
      if( tmp == SL_KEY_MOU )
      {
         hb_gt_sln_mouse_ProcessTerminalEvent();
         return hb_gt_sln_mouse_Inkey( iEventMask, HB_FALSE );
      }

      if( ( iEventMask & HB_INKEY_RAW ) != 0 )
         return tmp;

      tmp = hb_sln_FindKeyTranslation( tmp );
      if( tmp != 0 )
         return tmp;

      /* TOFIX: this code is broken - needs a diffrent aproach */
      tmp = hb_sln_FindKeyTranslation( ch );
      if( tmp != 0 || ch > 256 )
         return tmp;
   }

   if( ! hb_sln_Is_Unicode )
   {
      /* standard ASCII key */
      if( ch && ch < 256 && hb_sln_inputTab[ ch ] )
         ch = hb_sln_inputTab[ ch ];
   }
#if ( defined( HB_SLN_UTF8 ) || defined( HB_SLN_UNICODE ) )
   else if( ch >= 32 && ch <= 255 )
   {
      HB_WCHAR wc = 0;
      int n = 0;

      if( hb_cdpUTF8ToU16NextChar( ( HB_UCHAR ) ch, &n, &wc ) )
      {
         unsigned int buf[ 10 ], i = 0;

         while( n > 0 )
         {
            if( SLang_input_pending( hb_sln_escDelay == 0 ? -100 :
                                     -HB_MAX( hb_sln_escDelay, 0 ) ) == 0 )
               break;
            buf[ i++ ] = SLang_getkey();
            if( ! hb_cdpUTF8ToU16NextChar( ( HB_UCHAR ) buf[ i - 1 ], &n, &wc ) )
               n = -1;
         }
         if( n == 0 )
            return HB_INKEY_NEW_UNICODE( wc );
         else
            while( i > 0 )
               SLang_ungetkey( buf[ --i ] );
      }
   }
#endif

   return ch;
}

/* *********************************************************************** */

static int hb_sln_try_get_Kbd_State( void )
{
#if defined( __linux__ )
   unsigned char modifiers = 6;

   if( ioctl( 0, TIOCLINUX, &modifiers ) < 0 )
      return 0;

   return ( int ) modifiers;

#elif defined( M_UNIX )

   int modifiers = 0;
   int IOcommand = 0;

   if( ioctl( 0, TCGETSC, &IOcommand ) >= 0 )
   {
      /* if keyboard is not in SCANCODE mode */
      if( IOcommand == KB_XSCANCODE )
      {
         /* try to set it to SCANCODE mode */
         IOcommand = KB_ISSCANCODE;
         if( ioctl( 0, TCSETSC, &IOcommand ) >= 0 )
         {
            /* if SCANCODE mode is set corectly try get KBD state */
            if( ioctl( 0, KDGKBSTATE, &modifiers ) < 0 )
               modifiers = 0;

            /* turn a keyboard to a normal mode ( translation mode ) */
            IOcommand = KB_XSCANCODE;
            ( void ) ioctl( 0, TCSETSC, &IOcommand )
         }
      }
      /* keyboard is already in SCANCODE mode */
      else if( ioctl( 0, KDGKBSTATE, &modifiers ) < 0 )
         modifiers = 0;

      return modifiers;
   }
   else
      return 0;

#else

   return 0;

#endif
}

/* *********************************************************************** */

int hb_sln_Shft_Pressed( void )
{
   return ( hb_sln_try_get_Kbd_State() & SHIFT_PRESSED ) != 0;
}

/* *********************************************************************** */

int hb_sln_Ctrl_Pressed( void )
{
   return ( hb_sln_try_get_Kbd_State() & CONTROL_PRESSED ) != 0;
}

/* *********************************************************************** */

int hb_sln_Alt_Pressed( void )
{
   return ( hb_sln_try_get_Kbd_State() & ALT_PRESSED ) != 0;
}

/* *********************************************************************** */

int hb_sln_Kbd_State( void )
{
   return hb_sln_try_get_Kbd_State();
}

/* *********************************************************************** */
