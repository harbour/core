/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Keyboard API
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
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

/*
 * ChangeLog:
 *
 * V 1.63   David G. Holm               Enable Unix-compatible keyboard input
 *                                      even when not using ncurses or slang.
 * V 1.55   David G. Holm               Added _SET_CANCEL support for
 *                                      Ctrl+Break, returning no key code
 *                                      when _SET_CANCEL is off.
 * V 1.45   David G. Holm               Removed Borland Windows support.
 *                                      Removed DOS-like Windows support.
 *                                      Removed Cygwin from Unix-like support.
 *                                      Added Console Mode Windows support.
 * V 1.39   David G. Holm               Added Borland Windows support.
 *                                      Restored Unix support to what
 *                                      it was in version 1.34.
 *                                      Added separate Cygwin support and
 *                                      set it up to cooperate between the
 *                                      hb_inkeyGet() and hb_inkeyNext()
 *                                      functions (but it still blocks).
 * V 1.36   David G. Holm               Added __MINGW32__ support
 * V 1.35   David G. Holm               Changed the __CYGWIN__ build to use
 *                                      the Unix keyboard input method and
 *                                      modified it to not block the VM.
 * V 1.21   David G. Holm               Added OS/2 DosSleep()
 * V 1.15   David G. Holm               Tested Borland 3.1 hb_releaseCPU()
 * V 1.5    Paul Tucker                 ReleaseCPU comments
 * V 1.4    Victor Szel
 * V 1.3    Victor Szel                 #include <x> changed to #include "x".
 * V 1.2    Gonzalo Diethelm            ?
 * V 1.1    David G. Holm               Committed to CVS.
 * V 1.0    David G. Holm               Initial version.
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Victor Szel <info@szelvesz.hu>
 *    HB___KEYPUT()
 *
 * See doc/license.txt for licensing terms.
 *
 */

/* Note: The following #ifdef block for __IBMCPP__ must be ahead
         of any and all #include statements and requires that
         Harbour includes are ahead of platform includes.
*/
#ifdef __IBMCPP__
   #define INCL_DOSPROCESS
   #define INCL_NOPMAPI
#endif

/* NOTE: The following #include "hbwinapi.h" must
         be ahead of any other #include statements! */
#include "hbwinapi.h"

#if defined(_WINDOWS_) || defined(WINNT)
#if ! defined(HARBOUR_USE_CRS_GTAPI) && ! defined(HARBOUR_USE_SLN_GTAPI)
   #define INPUT_BUFFER_LEN 128
   extern BOOL   hb_gtBreak;  /* This variable is located in source/rtl/gt/gtwin.c */
   extern HANDLE hb_gtHInput; /* This variable is located in source/rtl/gt/gtwin.c */
   static DWORD s_cNumRead = 0;   /* Ok to use DWORD here, because this is specific... */
   static DWORD s_cNumIndex = 0;  /* ...to the Windows API, which defines DWORD, etc.  */
   static INPUT_RECORD s_irInBuf[ INPUT_BUFFER_LEN ];
   static SHORT s_iMouseCol = 0, s_iMouseRow = 0;
#endif
#endif

#define HB_BREAK_FLAG 256 /* 256, because that's what DJGPP returns Ctrl+Break as.
                             Clipper has no key code 256, so it may as well be
                             used for all the Harbour builds that need it */
#include "extend.h"
#include "ctoharb.h"
#include "errorapi.h"
#include "itemapi.h"
#include "set.h"
#include "inkey.h"
#include "inkey.ch"
#include "init.h"

#if defined(__TURBOC__) || defined(__BORLANDC__) || defined(_MSC_VER) || defined(__MINGW32__)
   #include <conio.h>
   #include <dos.h>
#elif  defined(__DJGPP__)
   #include <pc.h>
   #include <dos.h>
   #include <sys\exceptn.h>
#elif defined(HARBOUR_GCC_OS2)
   #include <stdlib.h>
#elif defined(__IBMCPP__)
   #include <bsedos.h>
   #include <conio.h>
#endif
#include <time.h>

#ifdef __WATCOMC__
   #include <conio.h>
   #include <i86.h>
   #if defined(__386__) && !defined(__WINDOWS__386__)
      #define INT_86 int386
      #define DOS_REGS REGS
   #else
      #define INT_86 int86
      #define DOS_REGS REGS
   #endif
#elif defined(__EMX__)
   #define INT_86 _int86
   #define DOS_REGS REGS
#elif defined(_MSC_VER)
   #define INT_86 _int86
   #define DOS_REGS _REGS
#else
   #define INT_86 int86
   #define DOS_REGS REGS
#endif

#if defined(HARBOUR_GCC_OS2)
   ULONG DosSleep( ULONG ulMilliseconds );
#endif

#if defined(OS_UNIX_COMPATIBLE)
#include <unistd.h>
#include <termios.h>

static struct termios startup_attributes;

static void restore_input_mode( void )
{
   HB_TRACE(HB_TR_DEBUG, ("restore_input_mode()"));

   tcsetattr( STDIN_FILENO, TCSANOW, &startup_attributes );
}

HB_CALL_ON_STARTUP_BEGIN( init_input_mode )
  struct termios ta;

  tcgetattr( STDIN_FILENO, &startup_attributes );
  atexit( restore_input_mode );

  tcgetattr( STDIN_FILENO, &ta );
  ta.c_lflag &= ~( ICANON | ECHO );
  ta.c_iflag &= ~ICRNL;
  ta.c_cc[ VMIN ] = 0;
  ta.c_cc[ VTIME ] = 0;
  tcsetattr( STDIN_FILENO, TCSAFLUSH, &ta );
HB_CALL_ON_STARTUP_END( init_input_mode )

#elif defined(HARBOUR_USE_DOS_GTAPI) && ! defined(__DJGPP__)
   extern BOOL hb_gtBreak;  /* This variable is located in source/rtl/gt/gtdos.c */
#endif

static int * s_inkeyBuffer = 0; /* Harbour keyboard buffer (empty if head == tail)     */
static int   s_inkeyHead;       /* Harbour keyboard buffer head pointer (next insert)  */
static int   s_inkeyTail;       /* Harbour keyboard buffer tail pointer (next extract) */
static int   s_inkeyLast;       /* Last key extracted from Harbour keyboard buffer     */
static BOOL  s_inkeyPoll;       /* Flag to override no polling when TYPEAHEAD is 0     */
static int   s_inkeyForce;      /* Variable to hold keyboard input when TYPEAHEAD is 0 */
static HB_inkey_enum s_eventmask;

void hb_releaseCPU( void )
{
/* TODO: Add code to release time slices on all platforms */
#if defined(_WINDOWS_) || defined(__MINGW32__)
   /* according to ms docs, you should not do this in a Win app. dos only */
#elif defined(OS2)
   DosSleep( 25 ); /* Duration is in milliseconds */
#elif defined(DOS)
/* NOTE: there is a bug under NT 4 (2000 unknown) -  if the app is running
   in protected mode, time slices will _not_ be released - you must switch
   to real mode first, execute the following, and switch back.

   It just occurred to me that this is actually by design.  Since MS doesn't
   want you to do this from a console app, their solution was to not allow
   the call to work in protected mode - screw the rest of the planet <g>.

   returns zero on failure. (means not supported)
 */
   #if defined(__TURBOC__)
      _AX = 0x1680;
      geninterrupt( 0x2f );
      _AH = 0;
      _AL ^= 0x80;
   #elif ! defined(__DJGPP__)
      union REGS regs;
      regs.h.ah = 0x16;
      regs.h.al = 0x80;
      #if defined(__WATCOMC__) && defined(__386__)
         int386( 0x2f, &regs, &regs );
      #else
         int86( 0x2f, &regs, &regs );
      #endif
      regs.h.ah  = 0;
      regs.h.al ^= 0x80;
   #endif
#elif defined(OS_UNIX_COMPATIBLE)
#else
#endif
   HB_TRACE(HB_TR_DEBUG, ("releaseCPU()"));
}

int hb_inkey( double seconds, HB_inkey_enum event_mask, BOOL wait, BOOL forever )
{
   int key;
   clock_t end_clock;

   HB_TRACE(HB_TR_DEBUG, ("hb_inkey(%lf, %d, %d, %d)", seconds, (int) event_mask, (int) wait, (int) forever));

   s_eventmask = event_mask;                   /* Set current input event mask */
   /* Check or wait for input events */
   if( wait ) end_clock = clock() + seconds * CLOCKS_PER_SEC;
   s_inkeyPoll = TRUE;                         /* Force polling */

   while( wait && hb_inkeyNext() == 0 )
   {
      /* Release the CPU between checks */
      hb_releaseCPU();

      /* Check for timeout */
      if( !forever && clock() >= end_clock ) wait = FALSE;
   }
   /* Get the current input event or 0 */
   key = hb_inkeyGet();
   s_inkeyPoll = FALSE;                        /* Stop forced polling */
   s_eventmask = hb_set.HB_SET_EVENTMASK;      /* Restore original input event mask */
   return key;
}

int hb_inkeyGet( void )       /* Extract the next key from the keyboard buffer */
{
   int key;

   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyGet()"));

   hb_inkeyPoll();
   if( hb_set.HB_SET_TYPEAHEAD )
   {
      /* Proper typeahead support is set */
      if( s_inkeyHead == s_inkeyTail ) key = 0;    /* Keyboard buffer is empty */
      else
      {                                            /* Keyboard buffer is not empty */
         s_inkeyLast = s_inkeyBuffer[ s_inkeyTail++ ];
         if( s_inkeyTail >= hb_set.HB_SET_TYPEAHEAD )
         {                                         /* Limit keyboard buffer to set size */
            s_inkeyTail = 0;
         }
         key = s_inkeyLast;
      }
   }
   else key = s_inkeyLast = s_inkeyForce; /* Typeahead support is disabled */
   s_inkeyForce = 0;
   return key;
}

int hb_inkeyLast( void )      /* Return the value of the last key that was extracted */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyLast()"));

   hb_inkeyPoll();
   return s_inkeyLast;
}

int hb_inkeyNext( void )      /* Return the next key without extracting it */
{
   int key;

   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyNext()"));

   hb_inkeyPoll();
   if( hb_set.HB_SET_TYPEAHEAD )
   {
      /* Proper typeahead support is enabled */
      if( s_inkeyHead == s_inkeyTail ) key = 0;   /* No key */
      else key = s_inkeyBuffer[ s_inkeyTail ];    /* Next key */
   }
   else key = s_inkeyForce; /* Typeahead support is disabled */
   return key;
}

void hb_inkeyPoll( void )     /* Poll the console keyboard to stuff the Harbour buffer */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyPoll()"));

   /* TODO: Add mouse support */
   if( hb_set.HB_SET_TYPEAHEAD || s_inkeyPoll )
   {
      int ch = 0;
#if defined(HARBOUR_USE_CRS_GTAPI) || defined(HARBOUR_USE_SLN_GTAPI)
      ch = hb_gtReadKey();
#elif defined(OS_UNIX_COMPATIBLE)
      if( ! read( STDIN_FILENO, &ch, 1 ) )
         ch = 0;
#elif defined(_WINDOWS_) || defined(WINNT)
      /* First check for Ctrl+Break, which is handled by gt/gtwin.c */
      if( hb_gtBreak )
      {
         /* Reset the global Ctrl+Break flag */
         hb_gtBreak = FALSE;
         ch = HB_BREAK_FLAG; /* Indicate that Ctrl+Break was pressed */
      }
      /* Check for events only when the event buffer is exhausted. */
      else if( s_cNumRead <= s_cNumIndex )
      {
         /* Check for keyboard input */
         s_cNumRead = 0;
         GetNumberOfConsoleInputEvents( hb_gtHInput, &s_cNumRead );
         if( s_cNumRead )
         {
            /* Read keyboard input */
            ReadConsoleInput(
               hb_gtHInput,      /* input buffer handle    */
               s_irInBuf,        /* buffer to read into    */
               INPUT_BUFFER_LEN, /* size of read buffer    */
               &s_cNumRead);     /* number of records read */
            /* Set up to process the first input event */
            s_cNumIndex = 0;
         }
      }
      /* Only process one keyboard event at a time. */
      if( s_cNumRead > s_cNumIndex )
      {
         if( s_irInBuf[ s_cNumIndex ].EventType == KEY_EVENT )
         {
            /* Only process key down events */
            if( s_irInBuf[ s_cNumIndex ].Event.KeyEvent.bKeyDown )
            {
               /* Save the keyboard state and ASCII key code */
               DWORD dwState = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.dwControlKeyState;
               ch = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.uChar.AsciiChar;
               if( ch == 0 || ( dwState & ( ENHANCED_KEY | LEFT_ALT_PRESSED | LEFT_CTRL_PRESSED | RIGHT_ALT_PRESSED | RIGHT_CTRL_PRESSED | SHIFT_PRESSED ) ) )
               {
                  /* Process non-ASCII key codes */
                  WORD wKey;
                  if( s_eventmask & INKEY_EXTENDED )
                     wKey = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wVirtualKeyCode;
                  else
                     wKey = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wVirtualScanCode;
                  /* Discard standalone state key presses for normal mode only */
                  if( ( s_eventmask & INKEY_EXTENDED ) == 0 ) switch( wKey )
                  {
                     /* Virtual scan codes to ignore */
                     case 29: /* Ctrl */
                     case 42: /* Left Shift */
                     case 54: /* Right Shift */
                     case 56: /* Alt */
                     case 69: /* Num Lock */
                     case 70: /* Pause or Scroll Lock */
                        wKey = 0;
                  }
                  if( wKey == 0 ) ch = 0;
                  else
                  {
                     if( s_eventmask & INKEY_EXTENDED )
                     {
                        /* Pass along all virtual key codes with all
                           enhanced and state indicators accounted for */
                        wKey += 256;
                        if( dwState & ENHANCED_KEY ) wKey += 512;
                        if( dwState & SHIFT_PRESSED ) wKey += 1024;
                        if( dwState & LEFT_CTRL_PRESSED ) wKey += 2048;
                        if( dwState & RIGHT_CTRL_PRESSED ) wKey += 4096;
                        if( dwState & LEFT_ALT_PRESSED ) wKey += 8192;
                        if( dwState & RIGHT_ALT_PRESSED ) wKey += 16384;
                        ch = wKey;
                     }
                     else
                     {
                        /* Translate virtual scan codes to Clipper codes */
                        BOOL bAlt = dwState & ( LEFT_ALT_PRESSED | RIGHT_ALT_PRESSED );
                        BOOL bCtrl = dwState & ( LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED );
                        BOOL bShift = dwState & SHIFT_PRESSED;
                        BOOL bEnhanced = dwState & ENHANCED_KEY;

			HB_TRACE(HB_TR_INFO, ("hb_inkeyPoll: wKey is %d, dwState is %d, ch is %d", wKey, dwState, ch));

                        if( bAlt )
                        {
                           /* Alt key held */
                           if( wKey == 1 ) ch = K_ALT_ESC; /* Esc */
                           else if( wKey == 15 ) ch = K_ALT_TAB; /* Tab */
                           else if( wKey <= 12 ) ch = wKey + 374; /* Numeric row */
                           else if( wKey == 28 ) ch = KP_ALT_ENTER; /* Num Pad Enter */
                           else if( wKey <= 52 ) ch = wKey + 256; /* Alpha rows */
                           else if( wKey == 53 && bEnhanced ) ch = KP_ALT_SLASH; /* Num Pad / */
                           else if( wKey == 55 ) ch = KP_ALT_ASTERISK; /* Num Pad * */
                           else if( wKey <= 58 ) ch = wKey + 367; /* ? */
                           else if( wKey <= 68 ) ch = 29 - wKey; /* F1 - F10 */
                           else if( wKey == 74 ) ch = KP_ALT_MINUS; /* Num Pad - */
                           else if( wKey == 76 ) ch = KP_ALT_5; /* Num Pad 5 */
                           else if( wKey == 78 ) ch = KP_ALT_PLUS; /* Num Pad + */
                           else if( wKey <= 86 ) ch = wKey + 336; /* Cursor */
                           else if( wKey <= 88 ) ch = 41 - wKey; /* F11, F12 */
                           else ch = wKey + 384;
                        }
                        else if( bCtrl )
                        {
                           /* Ctrl key held */
                           if( wKey == 53 && bEnhanced ) ch = KP_CTRL_SLASH; /* Num Pad / */
                           else if( wKey >= 59 && wKey <= 68 ) ch = 39 - wKey; /* F1 - F10 */
                           else switch( wKey )
                           {
                              case 1: /* Esc */
                                 ch = K_ESC;
                                 break;
                              case 3: /* 2 */
                                 ch = 259;
                                 break;
                              case 7: /* 6 */
                                 ch = K_CTRL_PGDN;
                                 break;
                              case 12: /* - */
                                 ch = K_CTRL_PGUP;
                                 break;
                              case 14: /* Backspace */
                                 ch = K_CTRL_BS;
                                 break;
                              case 15: /* Tab */
                                 ch = K_CTRL_TAB;
                                 break;
                              case 26: /* [ */
                                 ch = K_ESC;
                                 break;
                              case 27: /* ] */
                                 ch = K_CTRL_HOME;
                                 break;
                              case 28: /* Num Pad Enter */
                                 ch = K_CTRL_ENTER;
                                 break;
                              case 43: /* \ */
                                 ch = K_F1;
                                 break;
                              case 55: /* Num Pad * */
                                 ch = KP_CTRL_ASTERISK;
                                 break;
                              case 71: /* Home */
                                 ch = K_CTRL_HOME;
                                 break;
                              case 72: /* Up */
                                 ch = K_CTRL_UP;
                                 break;
                              case 73: /* PgUp */
                                 ch = K_CTRL_PGUP;
                                 break;
                              case 74: /* Num Pad - */
                                 ch = KP_CTRL_MINUS;
                                 break;
                              case 75: /* Left */
                                 ch = K_CTRL_LEFT;
                                 break;
                              case 76: /* Num Pad 5 */
                                 ch = KP_CTRL_5;
                                 break;
                              case 77: /* Right */
                                 ch = K_CTRL_RIGHT;
                                 break;
                              case 78: /* Num Pad + */
                                 ch = KP_CTRL_PLUS;
                                 break;
                              case 79: /* End */
                                 ch = K_CTRL_END;
                                 break;
                              case 80: /* Down */
                                 ch = K_CTRL_DOWN;
                                 break;
                              case 81: /* PgDn */
                                 ch = K_CTRL_PGDN;
                                 break;
                              case 82: /* Ins */
                                 ch = K_CTRL_INS;
                                 break;
                              case 83: /* Del */
                                 ch = K_CTRL_DEL;
                                 break;
                              case 87: /* F11 */
                              case 88: /* F12 */
                                 ch = 43 - wKey;
                                 break;
                              default:
                                 /* Keep Ctrl+Alpha, but scrap everything
                                    else that hasn't been translated yet. */
                                 if( ch < 1 || ch > 26 )
                                    ch = 0;
                           }
                        }
                        else if( bShift )
                        {
                           /* Shift key held */
                           if( wKey == 53 && bEnhanced ) ch = '/'; /* Num Pad / */
                           else if( wKey >= 59 && wKey <= 68 ) ch = 49 - wKey; /* F1 - F10 */
                           else switch( wKey )
                           {
                              case 1: /* Esc */
                                 ch = K_ESC;
                                 break;
                              case 15: /* Tab */
                                 ch = K_SH_TAB;
                                 break;
                              case 28: /* Num Pad Enter */
                                 ch = K_ENTER;
                                 break;
                              case 76: /* Num Pad 5 */
                                 ch = '5';
                                 break;
                              case 87: /* F11 */
                              case 88: /* F12 */
                                 ch = 45 - wKey;
                                 break;
                              case 82: /* Ins */
                                 ch = K_INS;
                                 break;
                              case 83: /* Del */
                                 ch = K_DEL;
                                 break;
                              case 71: /* Home */
                                 ch = K_HOME;
                                 break;
                              case 79: /* End */
                                 ch = K_END;
                                 break;
                              case 73: /* Page Up */
                                 ch = K_PGUP;
                                 break;
                              case 81: /* Page Down */
                                 ch = K_PGDN;
                                 break;
                              case 72: /* Up */
                                 ch = K_UP;
                                 break;
                              case 80: /* Down */
                                 ch = K_DOWN;
                                 break;
                              case 77: /* Right */
                                 ch = K_RIGHT;
                                 break;
                              case 75: /* Left */
                                 ch = K_LEFT;
                                 break;
                              default: /* Any thing not explicitly translated */
                                 if( ch == 0 )
                                    /* Only provide a translation for those key
                                       codes that don't have a default one. */
                                    ch = wKey + 128;
                           }
                        }
                        else
                        {
                           /* Normal key */
                           if( wKey == 28 ) ch = K_ENTER; /* Num Pad Enter */
                           else if( wKey == 53 && bEnhanced ) ch = '/'; /* Num Pad / */
                           else if( wKey == 59 ) ch = K_F1; /* F1 */
                           else if( wKey > 59 && wKey <= 68 ) ch = 59 - wKey; /* F2 - F10 */
                           else if( wKey == 76 ) ch = 332; /* Num Pad 5 */
                           else if( wKey == 87 || wKey == 88 ) ch = 47 - wKey; /* F11, F12 */
                           else switch( wKey )
                           {
                              case 82: /* Ins */
                                 ch = K_INS;
                                 break;
                              case 83: /* Del */
                                 ch = K_DEL;
                                 break;
                              case 71: /* Home */
                                 ch = K_HOME;
                                 break;
                              case 79: /* End */
                                 ch = K_END;
                                 break;
                              case 73: /* Page Up */
                                 ch = K_PGUP;
                                 break;
                              case 81: /* Page Down */
                                 ch = K_PGDN;
                                 break;
                              case 72: /* Up */
                                 ch = K_UP;
                                 break;
                              case 80: /* Down */
                                 ch = K_DOWN;
                                 break;
                              case 77: /* Right */
                                 ch = K_RIGHT;
                                 break;
                              case 75: /* Left */
                                 ch = K_LEFT;
                                 break;
                              case 76: /* Num Pad 5 */
                                 ch = 332;
                                 break;
                              case 28: /* Num pad Enter */
                                 ch = K_ENTER;
                                 break;
                              default:
                                 ch = wKey + 128;
                           }
                        }
                     }
                  }
               }

#if 0
	       /* Debug code: */
               else
		 {

		   WORD wKey;
		   if( s_eventmask & INKEY_EXTENDED )
		     wKey = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wVirtualKeyCode;
		   else
		     wKey = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wVirtualScanCode;
		   HB_TRACE(HB_TR_INFO, ("hb_inkeyPoll: wKey is %d", wKey));
		 }
#endif
            }
         }
         else if( s_irInBuf[ s_cNumIndex ].EventType == MOUSE_EVENT )
         {
            s_iMouseCol = s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwMousePosition.X;
            s_iMouseRow = s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwMousePosition.Y;

            if( s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwEventFlags == MOUSE_MOVED )
               ch = K_MOUSEMOVE;

            else if( s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwButtonState &
                     FROM_LEFT_1ST_BUTTON_PRESSED )
               ch = K_LBUTTONDOWN;

            else if( s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwButtonState &
                     RIGHTMOST_BUTTON_PRESSED )
               ch = K_RBUTTONDOWN;
         }
         /* Set up to process the next input event (if any) */
         s_cNumIndex++;
      }
#elif defined(OS_DOS_COMPATIBLE) || defined(HARBOUR_GCC_OS2) || defined(__IBMCPP__)
   /* The reason for including _WINDOWS_ here is that kbhit() and getch() appear
     to work properly in console mode. For true Windows mode, changes are needed. */
   #if defined(HARBOUR_GCC_OS2)
      /* Read from the keyboard with no echo, no wait, and no SIGSEV on Ctrl-C */
      ch = _read_kbd( 0, 0, 0 );
      if( ch == 0 )
      {
         /* It's a function key lead-in, so read the function key scan code */
         ch = _read_kbd( 0, 0, 0 );
         if( ch != -1 ) ch += 256;      /* If it's really a scan code, offset it */
      }
      /* _read_kbd() returns -1 for no key, the switch statement will handle
         this. */
   #else
     #if defined(HARBOUR_USE_DOS_GTAPI)
      #if defined(__DJGPP__)
         /* Check to see if Ctrl+Break has been detected */
         if( __djgpp_cbrk_count )
         {
            __djgpp_cbrk_count = 0; /* Indicate that Ctrl+Break has been handled */
            ch = HB_BREAK_FLAG; /* Note that Ctrl+Break was pressed */
         }
      #else
         /* First check for Ctrl+Break, which is handled by gt/gtdos.c,
            with the exception of the DJGPP compiler */
         if( hb_gtBreak )
         {
            hb_gtBreak = FALSE; /* Indicate that Ctrl+Break has been handled */
            ch = HB_BREAK_FLAG; /* Note that Ctrl+Break was pressed */
         }
      #endif
      else
     #endif
      if( kbhit() )
      {
         /* A key code is available in the BIOS keyboard buffer, so read it */
      #if defined(__DJGPP__)
         if( s_eventmask & INKEY_EXTENDED ) ch = getxkey();
         else ch = getkey();
         if( ch == 256 )
            /* Ignore Ctrl+Break, because it is being handled as soon as it
               happens (see above) rather than waiting for it to show up in
               the keyboard input queue */
            ch = -1;
      #else
         /* A key code is available in the BIOS keyboard buffer */
         ch = getch();                  /* Get the key code */
         if( ch == 0 && kbhit() )
         {
            /* It was a function key lead-in code, so read the actual
               function key and then offset it by 256 */
            ch = getch() + 256;
         }
         else if( ch == 224 && kbhit() )
         {
            /* It was an extended function key lead-in code, so read
               the actual function key and then offset it by 256,
               unless extended keyboard events are allowed, in which
               case offset it by 512 */
            if( s_eventmask & INKEY_EXTENDED ) ch = getch() + 512;
            else ch = getch() + 256;
         }
      #endif
      }
   #endif
      /* Perform key translations */
      switch( ch )
      {
         case -1:  /* No key available */
            return;
         case 328:  /* Up arrow */
            ch = K_UP;
            break;
         case 336:  /* Down arrow */
            ch = K_DOWN;
            break;
         case 331:  /* Left arrow */
            ch = K_LEFT;
            break;
         case 333:  /* Right arrow */
            ch = K_RIGHT;
            break;
         case 327:  /* Home */
            ch = K_HOME;
            break;
         case 335:  /* End */
            ch = K_END;
            break;
         case 329:  /* Page Up */
            ch = K_PGUP;
            break;
         case 337:  /* Page Down */
            ch = K_PGDN;
            break;
         case 371:  /*  Ctrl + Left arrow */
            ch = K_CTRL_LEFT;
            break;
         case 372:  /* Ctrl + Right arrow */
            ch = K_CTRL_RIGHT;
            break;
         case 375:  /* Ctrl + Home */
            ch = K_CTRL_HOME;
            break;
         case 373:  /* Ctrl + End */
            ch = K_CTRL_END;
            break;
         case 388:  /* Ctrl + Page Up */
            ch = K_CTRL_PGUP;
            break;
         case 374:  /* Ctrl + Page Down */
            ch = K_CTRL_PGDN;
            break;
         case 338:  /* Insert */
            ch = K_INS;
            break;
         case 339:  /* Delete */
            ch = K_DEL;
            break;
         case 315:  /* F1 */
            ch = K_F1;
            break;
         case 316:  /* F2 */
         case 317:  /* F3 */
         case 318:  /* F4 */
         case 319:  /* F5 */
         case 320:  /* F6 */
         case 321:  /* F7 */
         case 322:  /* F8 */
         case 323:  /* F9 */
         case 324:  /* F10 */
            ch = 315 - ch;
            break;
         case 340:  /* Shift + F1 */
         case 341:  /* Shift + F2 */
         case 342:  /* Shift + F3 */
         case 343:  /* Shift + F4 */
         case 344:  /* Shift + F5 */
         case 345:  /* Shift + F6 */
         case 346:  /* Shift + F7 */
         case 347:  /* Shift + F8 */
         case 348:  /* Shift + F9 */
         case 349:  /* Shift + F10 */
         case 350:  /* Ctrl + F1 */
         case 351:  /* Ctrl + F2 */
         case 352:  /* Ctrl + F3 */
         case 353:  /* Ctrl + F4 */
         case 354:  /* Ctrl + F5 */
         case 355:  /* Ctrl + F6 */
         case 356:  /* Ctrl + F7 */
         case 357:  /* Ctrl + F8 */
         case 358:  /* Ctrl + F9 */
         case 359:  /* Ctrl + F10 */
         case 360:  /* Alt + F1 */
         case 361:  /* Alt + F2 */
         case 362:  /* Alt + F3 */
         case 363:  /* Alt + F4 */
         case 364:  /* Alt + F5 */
         case 365:  /* Alt + F6 */
         case 366:  /* Alt + F7 */
         case 367:  /* Alt + F8 */
         case 368:  /* Alt + F9 */
         case 369:  /* Alt + F10 */
            ch = 330 - ch;
            break;
         case 389:  /* F11 */
         case 390:  /* F12 */
         case 391:  /* Shift + F11 */
         case 392:  /* Shift + F12 */
         case 393:  /* Ctrl + F11 */
         case 394:  /* Ctrl + F12 */
         case 395:  /* Alt + F11 */
         case 396:  /* Alt + F12 */
            ch = 349 - ch;
      }
#else
      /* TODO: Support for other platforms, such as Mac */
#endif
      switch( ch )
      {
         case HB_BREAK_FLAG:        /* Check for Ctrl+Break */
            if( ! hb_set.HB_SET_CANCEL ) /* If cancel is disabled, */
               ch = 0;                   /* then ignore the keystroke */
                                    /* In either case, handle like Alt+C */
         case K_ALT_C:              /* Alt+C was pressed */
            hb_vmRequestCancel();
      }
      hb_inkeyPut( ch );
   }
}

void hb_inkeyReset( BOOL allocate )     /* Reset the keyboard buffer */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyReset(%d)", (int) allocate));

   /* Reset the buffer head and tail pointers, the last key value,
      and the polling override flag */
   s_inkeyHead = 0;
   s_inkeyTail = 0;
   s_inkeyLast = 0;
   s_inkeyPoll = FALSE;
   s_inkeyForce = 0;
   /* The allocate flag allows the same function to be used to reset the
      buffer or to reset and allocate, reallocate, or free the buffer */
   if( allocate )
   {
      /* If the buffer already exists, free it */
      if( s_inkeyBuffer ) hb_xfree( s_inkeyBuffer );
      /* Always allocate a new buffer, unless it's being freed from hb_setRelease() */
      if( hb_set.HB_SET_TYPEAHEAD > -1 )
      {
         /* The buffer min and max are determined by SET(HB_SET_TYPEAHEAD, but it
            can also set the typeahead to 0 to disable polling, in which case the
            minimum buffer size (which is 16) must still be allocated, because
            even when polling is disabled, calling INKEY() or NEXTKEY() will
            temporarily re-enable polling */
         s_inkeyBuffer = ( int * ) hb_xgrab( sizeof( int )
         * ( hb_set.HB_SET_TYPEAHEAD == 0 ? 16 : hb_set.HB_SET_TYPEAHEAD ) );
      }
   }
}

/*  $DOC$
 *  $FUNCNAME$
 *      INKEY()
 *  $CATEGORY$
 *      Console input
 *  $ONELINER$
 *      Extracts the next key code from the Harbour keyboard buffer
 *  $SYNTAX$
 *      INKEY( [<nTimeout>] [,<nEvents>] ) --> nKey
 *  $ARGUMENTS$
 *      <nTimeout> is an optional timeout value in seconds, with a granularity
 *      of 1/10th of a second. If omitted, INKEY() returns immediately. If set
 *      to 0, INKEY() waits until an input event occurs. If set to any other
 *      value, INKEY() will return either when an input event occurs or when
 *      the timeout period has elapsed. If only this parameter is specified
 *      and it is not numeric, it will be treated as if it were 0. But if both
 *      parameters are specified and this parameter is not numeric, it will be
 *      treated as if it were not present.
 *
 *      <nEvents> is an optional mask of input events that are to be enabled.
 *      If omitted, defaults to hb_set.HB_SET_EVENTMASK. Valid input maks are
 *      in inkey.ch and are explained below. It is recommended that the mask
 *      names be used rather than their numeric values, in case the numeric
 *      values change in future releases of Harbour. To allow more than one
 *      type of input event, simply add the various mask names together.
 *        INKEY_MOVE     = Mouse motion events are allowed
 *        INKEY_LDOWN    = The mouse left click down event is allowed
 *        INKEY_LUP      = The mouse left click up event is allowed
 *        INKEY_RDOWN    = The mouse right click down event is allowed
 *        INKEY_RUP      = The mouse right click up event is allowed
 *        INKEY_KEYBOARD = All keyboard events are allowed
 *        INKEY_ALL      = All mouse and keyboard events are allowed
 *      If the parameter is not numeric, it will be treated as if it were set
 *      to hb_set.HB_SET_EVENTMASK.
 *  $RETURNS$
 *      0 in case of timeout with no input event, otherwise returns a value
 *      in the range -39 to 386 for keyboard events or the range 1001 to 1007
 *      for mouse events. Mouse events and non-printable keyboard events are
 *      represented by the K_<event> values listed in inkey.ch. Keyboard
 *      event return codes in the range 32 through 127 are equivalent to the
 *      printable ASCII character set. Keyboard event return codes in the
 *      range 128 through 255 are assumed to be printable, but results may
 *      vary based on hardware and nationality.
 *  $DESCRIPTION$
 *      INKEY() can be used to detect input events, such as keypress, mouse
 *      movement, or mouse key clicks (up and/or down).
 *  $EXAMPLES$
 *      // Wait for the user to press the Esc key
 *      ? "Please press the ESC key."
 *      WHILE INKEY( 0.1 ) != K_ESC
 *      END
 *  $TESTS$
 *      KEYBOARD "AB"; ? INKEY(), INKEY() ==>   65   66
 *  $STATUS$
 *      S
 *  $COMPLIANCE$
 *      INKEY() is compliant with the Clipper 5.3 INKEY() function with one
 *      exceptions: The Harbour INKEY() function will raise an argument error
 *      if the first parameter is less than or equal to 0 and the second
 *      parameter (or the default mask) is not valid, because otherwise INKEY
 *      would never return, because it was, in effect, asked to wait forever
 *      for no events (Note: In Clipper, this also blocks SET KEY events).
 *  $SEEALSO$
 *      inkey.ch
 *  $END$
 */

HARBOUR HB_INKEY( void )
{
   int args = hb_pcount();
   int key = 0;
   BOOL wait = FALSE, forever = FALSE;
   double seconds = 0.0;
   HB_inkey_enum event_mask = hb_set.HB_SET_EVENTMASK; /* Default to the SET input event mask */

   if( args == 1 || ( args > 1 && hb_param( 1, IT_NUMERIC ) ) )
   {
      /* If only one parameter or if 1st parameter is numeric, then use it
         as the number of seconds to wait for an input event, in seconds. */
      seconds = hb_parnd( 1 );
      wait = TRUE;
      if( seconds * CLOCKS_PER_SEC < 1 ) forever = TRUE;
   #ifndef HARBOUR_USE_GTAPI
      /* When not using the GT API, flush both stdout and stderr,
         because we are waiting for input and want to ensure that
         any user prompts are visible. */
/*    fflush( stdout ); */
/*    fflush( stderr ); */
   #endif
   }

   if( args > 1 && hb_param( 2, IT_NUMERIC ) )
   {
      /* If 2nd parameter is numeric, then use it as the input mask */
      event_mask = ( HB_inkey_enum )hb_parni( 2 );
   }

   if( wait && forever && ( event_mask & ( INKEY_ALL + INKEY_EXTENDED ) ) == 0 )
   {
      /* There is no point in waiting forever for no input events! */
      hb_errRT_BASE( EG_ARG, 3007, NULL, "INKEY" );
   }
   else
   {
      /* Call the low-level hb_inkey() function. */
      key = hb_inkey( seconds, event_mask, wait, forever );
   }
   hb_retni( key );
}

/*  $DOC$
 *  $FUNCNAME$
 *      __KEYBOARD()
 *  $CATEGORY$
 *      Console input
 *  $ONELINER$
 *      DO NOT CALL THIS FUNCTION DIRECTLY!
 *  $SYNTAX$
 *      KEYBOARD <cString>
 *      CLEAR TYPEAHEAD
 *  $ARGUMENTS$
 *      <cString> is the optional string to stuff into the Harbour keyboard
 *      buffer after clearing it first.
 *  $RETURNS$
 *      There is no return value
 *  $DESCRIPTION$
 *      Clears the Harbour keyboard typeahead buffer and then inserts an
 *      optional string into it.
 *  $EXAMPLES$
 *      // Stuff an Enter key into the keyboard buffer
 *      KEYBOARD CHR(13)
 *      // Clear the keyboard buffer
 *      CLEAR TYPEAHEAD
 *  $TESTS$
 *      KEYBOARD CHR(13); ? INKEY() ==> 13
 *      KEYBOARD "HELLO"; CLEAR TYPEAHEAD; ? INKEY() ==> 0
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      __KEYBOARD() is compliant with CA-Clipper 5.3
 *  $SEEALSO$
 *    CLEAR TYPEAHEAD, KEYBOARD
 *  $END$
 */

HARBOUR HB___KEYBOARD( void )
{
   /* Clear the typeahead buffer without reallocating the keyboard buffer */
   hb_inkeyReset( FALSE );

   if( ISCHAR( 1 ) )
   {
      long size = hb_parclen( 1 );

      if( size != 0 )
      {
         /* Stuff the string */
         char *fPtr = hb_parc( 1 );

         if( size >= hb_set.HB_SET_TYPEAHEAD )
         {
            /* Have to allow for a zero size typehead buffer */
            if( hb_set.HB_SET_TYPEAHEAD ) size = hb_set.HB_SET_TYPEAHEAD - 1;
            else size = 0;
         }

         while( size-- )
            hb_inkeyPut( *fPtr++ );
      }
   }
}

void hb_inkeyPut( int ch )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_inkeyPut(%d)", ch));

   if( ch )
   {
      if( hb_set.HB_SET_TYPEAHEAD )
      {
         /* Proper typeahead support is set */
         int head = s_inkeyHead;
         s_inkeyBuffer[ head++ ] = ch;
         if( head >= hb_set.HB_SET_TYPEAHEAD ) head = 0;
         if( head != s_inkeyTail ) s_inkeyHead = head;
         else /* TODO: Add error sound */ ;
      }
      else
         s_inkeyForce = ch; /* Typeahead support is disabled */
   }
}

/*  $DOC$
 *  $FUNCNAME$
 *      __KEYPUT()
 *  $CATEGORY$
 *      Console input
 *  $ONELINER$
 *      Put an inkey code to the keyboard buffer
 *  $SYNTAX$
 *      __keyPut( <nInkeyCode> )
 *  $ARGUMENTS$
 *      <nInkeyCode> is the inkey code, which should be inserted into the
 *      keyboard buffer.
 *  $RETURNS$
 *      There is no return value
 *  $DESCRIPTION$
 *      Inserts an inkey code to the string buffer. The buffer is *not*
 *      cleared in this operation. This function allows to insert such
 *      inkey codes which are not in the range of 0 to 255. To insert more
 *      than one code, call the function repeatedly. The zero code cannot
 *      be inserted.
 *  $EXAMPLES$
 *      // Stuff an Alt+PgDn key into the keyboard buffer
 *      __keyPut( K_ALT_PGDN )
 *  $TESTS$
 *      __keyPut( K_ALT_PGDN ) ; ? INKEY() ==> 417
 *      __keyPut( K_F11 ) ; ? INKEY() ==> -40
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      Was not part of Clipper
 *  $SEEALSO$
 *      KEYBOARD,CLEAR TYPEAHEAD,INKEY()
 *  $END$
 */

HARBOUR HB___KEYPUT( void )
{
   if( ISNUM( 1 ) )
      hb_inkeyPut( hb_parni( 1 ) );
}

/*  $DOC$
 *  $FUNCNAME$
 *      MCOL()
 *  $CATEGORY$
 *      Console input
 *  $ONELINER$
 *       Returns the mouse cursor column position
 *  $SYNTAX$
 *      MCol() --> nMouseColumn
 *  $ARGUMENTS$
 *      None
 *  $RETURNS$
 *      The mouse cursor column position
 *  $DESCRIPTION$
 *  $EXAMPLES$
 *     local nKey
 *     nKey = InKey( 0 )
 *     do case
 *        case nKey ==
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      This function is a Harbour Specific
 *  $PLATFORMS$
 *      This function is Windows Specific
 *  $SEEALSO$
 *      MROW()
 *  $END$
 */


HARBOUR HB_MCOL( void )
{
#if defined(_WINDOWS_) || defined(WINNT)
  #if ! defined(HARBOUR_USE_CRS_GTAPI) && ! defined(HARBOUR_USE_SLN_GTAPI)
    hb_retnl( s_iMouseCol );
  #else
    hb_retnl( 0 );
  #endif
#else
    hb_retnl( 0 );
#endif
}

/*  $DOC$
 *  $FUNCNAME$
 *      MROW()
 *  $CATEGORY$
 *      Console input
 *  $ONELINER$
 *       Returns the mouse cursor row position
 *  $SYNTAX$
 *      MRow() --> nMouseColumn
 *  $ARGUMENTS$
 *      None
 *  $RETURNS$
 *      The mouse cursor column position
 *  $DESCRIPTION$
 *  $EXAMPLES$
 *     local nKey
 *     nKey = InKey( 0 )
 *     do case
 *        case nKey ==
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      This function is a Harbour Specific
 *  $PLATFORMS$
 *      This function is Windows Specific
 *  $SEEALSO$
 *      MCOL()
 *  $END$
 */

HARBOUR HB_MROW( void )
{
#if defined(_WINDOWS_) || defined(WINNT)
  #if ! defined(HARBOUR_USE_CRS_GTAPI) && ! defined(HARBOUR_USE_SLN_GTAPI)
    hb_retnl( s_iMouseRow );
  #else
    hb_retnl( 0 );
  #endif
#else
    hb_retnl( 0 );
#endif
}

/*  $DOC$
 *  $FUNCNAME$
 *      NEXTKEY()
 *  $CATEGORY$
 *      Console input
 *  $ONELINER$
 *      Returns the value of the next key in the Harbour keyboard buffer
 *  $SYNTAX$
 *      NEXTKEY() --> nKey
 *  $ARGUMENTS$
 *      None
 *  $RETURNS$
 *      There is no return value
 *  $DESCRIPTION$
 *      Returns the value of the next key in the Harbour keyboard buffer
 *      without extracting it.
 *  $EXAMPLES$
 *      // Use NEXTKEY() with INKEY() to change display character or by
 *      // itself to exit the loop, so that the caller can detect the Esc.
 *      LOCAL nKey, cChar := "+"
 *      WHILE TRUE
 *         ?? cChar
 *         nKey := NEXTKEY()
 *         IF nKey == K_ESC
 *            EXIT
 *         ELSE
 *            IF nKey != 0
 *               cChar := CHR( nKey )
 *            END IF
 *         END IF
 *      END WHILE
 *  $TESTS$
 *      KEYBOARD "AB"; ? NEXTKEY(), NEXTKEY() ==>   65   65
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      NEXTKEY() is compliant with CA-Clipper 5.3
 *  $SEEALSO$
 *      INKEY(),LASTKEY()
 *  $END$
 */

HARBOUR HB_NEXTKEY( void )
{
   hb_retni( hb_inkeyNext() );
}

/*  $DOC$
 *  $FUNCNAME$
 *      LASTKEY()
 *  $CATEGORY$
 *      Console input
 *  $ONELINER$
 *      Returns the last key exttracted from the Harbour keyboard buffer
 *  $SYNTAX$
 *      LASTKEY() --> nKey
 *  $ARGUMENTS$
 *      None
 *  $RETURNS$
 *      There is no return value
 *  $DESCRIPTION$
 *      Returns the value of the last key exttracted from the Harbour
 *      keyboard buffer
 *  $EXAMPLES$
 *      // Continue looping unless the ESC key was pressed in MainFunc()
 *      WHILE TRUE
 *         MainFunc()
 *         IF LASTKEY() == K_ESC
 *            EXIT
 *         END IF
 *      END WHILE
 *  $TESTS$
 *      KEYBOARD "AB"; ? INKEY(), LASTKEY() ==>   65   65
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      LASTKEY() is compliant with CA-Clipper 5.3
 *  $SEEALSO$
 *      INKEY(),LASTKEY()
 *  $END$
 */

HARBOUR HB_LASTKEY( void )
{
   hb_retni( s_inkeyLast );
}

/* Dumb function to maintain dBase III+ and CA-Cl*pper compatibility */

HARBOUR HB_FKLABEL( void )
{
   PHB_ITEM pPar1 = hb_param( 1, IT_NUMERIC );

   if( pPar1 != NULL )
   {
      USHORT uiFKey = hb_itemGetNI( pPar1 );

      if( uiFKey > 0 && uiFKey <= 40 )
      {
         char szName[ 4 ];

         sprintf( szName, "F%i", uiFKey );

         hb_retc( szName );
      }
      else
         hb_retc( "" );
   }
   else
      hb_retc( "" );
}

/* Dumb function to maintain dBase III+ and CA-Cl*pper compatibility */

HARBOUR HB_FKMAX( void )
{
   hb_retni( 40 ); /* IBM specific */
}

/*  $DOC$
 *  $FUNCNAME$
 *      KEYBOARD
 *  $CATEGORY$
 *      Command
 *  $ONELINER$
 *      Stuffs the keyboard with a string
 *  $SYNTAX$
 *      KEYBOARD <cString>
 *  $ARGUMENTS$
 *      <cString> String to be processed, one character at a time,
 *      by the Harbour keyboard processor
 *  $RETURNS$
 *
 *  $DESCRIPTION$
 *      This command stuff the input buffer with <cString>. The
 *      number of character that can be stuffed into the keyboard
 *      buffer is controled by SET TYPEAHEAD command and may range
 *      from 0 to 32,622, with each character appearing in the ASCII
 *      range of 0 to 255. None of the extended keys may be stuffed
 *      in the keyboard buffer.
 *      Issuing a KEYBOARD " " will clear the keyboard buffer.
 *  $EXAMPLES$
 *      // Stuff an Enter key into the keyboard buffer
 *      KEYBOARD CHR(13)
 *      // Clear the keyboard buffer
 *      CLEAR TYPEAHEAD
 *  $TESTS$
 *      KEYBOARD CHR(13); ? INKEY() ==> 13
 *      KEYBOARD "HELLO"; CLEAR TYPEAHEAD; ? INKEY() ==> 0
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      __KEYBOARD() is compliant with CA-Clipper 5.3
 *  $SEEALSO$
 *       CLEAR TYPEAHEAD,__KEYBOARD()
 *  $END$
 */

