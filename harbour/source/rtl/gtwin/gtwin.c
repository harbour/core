/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem for Win32 compilers
 *
 * Copyright 1999-2000 Paul Tucker <ptucker@sympatico.ca>
 * (for functions marked ptucker)
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
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 *    hb_gt_CtrlHandler()
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_gt_Tone()
 *    hb_gt_ReadKey()
 *
 * See doc/license.txt for licensing terms.
 *
 */

/*
 *  Portions of this module are based (somewhat) on VIDMGR by
 *   Andrew Clarke and modified for the Harbour project
 */

/* NOTE: User programs should never call this layer directly! */

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define HB_OS_WIN_32_USED

#include "hbapigt.h"
#include "hbset.h" /* For Ctrl+Break handling */
#include "hbvm.h" /* For Ctrl+Break handling */
#include "inkey.ch"

#if defined(__IBMCPP__)
   #undef WORD                            /* 2 bytes unsigned */
   typedef unsigned short int WORD;
#else
   #if ! defined(HB_DONT_DEFINE_BASIC_TYPES)
      #undef WORD                            /* 2 bytes unsigned */
      typedef unsigned short int WORD;

      #undef DWORD                           /* 4 bytes unsigned */
      typedef unsigned long DWORD;
   #endif
#endif

#if ! defined(__GNUC__) && defined(__CYGWIN__)
   typedef WORD far * LPWORD;
#endif

#if defined(__RSXNT__)
   #ifndef FROM_LEFT_1ST_BUTTON_PRESSED
      #define FROM_LEFT_1ST_BUTTON_PRESSED    0x0001
   #endif
   #ifndef RIGHTMOST_BUTTON_PRESSED
      #define RIGHTMOST_BUTTON_PRESSED        0x0002
   #endif
   #ifndef MOUSE_MOVED
      #define MOUSE_MOVED                     0x0001
   #endif
   #ifndef DOUBLE_CLICK
      #define DOUBLE_CLICK                    0x0002
   #endif
#endif

#if 0
static HANDLE s_HOsave;      /* work in progress */
static HANDLE s_HDOutput;
#endif
static HANDLE s_HOriginal;
static HANDLE s_HOutput;
static HANDLE s_HActive;
static HANDLE s_HInactive;
static HANDLE s_HInput;
static BOOL   s_bOldCursor;
static BOOL   s_bBreak;

static USHORT s_uiDispCount;
static CONSOLE_SCREEN_BUFFER_INFO s_csbi; /* to restore screen mode on exit */

#define INPUT_BUFFER_LEN 128

static DWORD        s_cNumRead;   /* Ok to use DWORD here, because this is specific... */
static DWORD        s_cNumIndex;  /* ...to the Windows API, which defines DWORD, etc.  */
static INPUT_RECORD s_irInBuf[ INPUT_BUFFER_LEN ];
static int          s_mouseLast;  /* Last mouse button to be pressed                     */

extern int hb_mouse_iCol;
extern int hb_mouse_iRow;

static BOOL WINAPI hb_gt_CtrlHandler( DWORD dwCtrlType )
{
   BOOL bHandled;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_CtrlHandler(%lu)", (unsigned long) dwCtrlType));

   switch( dwCtrlType )
   {
   case CTRL_C_EVENT:
      bHandled = FALSE;
      break;

   case CTRL_BREAK_EVENT:
      s_bBreak = TRUE;
      bHandled = TRUE;
      break;

   case CTRL_CLOSE_EVENT:
   case CTRL_LOGOFF_EVENT:
   case CTRL_SHUTDOWN_EVENT:
   default:
      bHandled = FALSE;
   }

   return bHandled;
}

void hb_gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init(): %d, %d, %d", iFilenoStdin, iFilenoStdout, iFilenoStderr));

   HB_SYMBOL_UNUSED( iFilenoStdin );
   HB_SYMBOL_UNUSED( iFilenoStdout );
   HB_SYMBOL_UNUSED( iFilenoStderr );

#if 0
   s_HOsave     =
   s_HDOutput   = INVALID_HANDLE_VALUE;
#endif

   s_cNumRead = 0;
   s_cNumIndex = 0;
   s_uiDispCount = 0;

   s_bOldCursor = TRUE;
   s_bBreak = FALSE;

   /* Add Ctrl+Break handler [vszakats] */
   SetConsoleCtrlHandler( hb_gt_CtrlHandler, TRUE );

   if( ( s_HInput = GetStdHandle( STD_INPUT_HANDLE ) ) == INVALID_HANDLE_VALUE )
   {
      if( hb_dynsymFindName( "__DBGENTRY" ) ) /* the debugger is linked */
      {
         AllocConsole(); /* It is a Windows app without a console, so we create one */
         s_HInput = GetStdHandle( STD_INPUT_HANDLE );
      }
   }

   if( s_HInput != INVALID_HANDLE_VALUE )
      SetConsoleMode( s_HInput, ENABLE_MOUSE_INPUT );

   /* ptucker */
   s_HOriginal = CreateFile( "CONOUT$",     /* filename    */
                       GENERIC_READ    | GENERIC_WRITE,       /* Access flag */
                       FILE_SHARE_READ | FILE_SHARE_WRITE,    /* share mode  */
                       NULL,                                  /* security attributes */
                       OPEN_EXISTING,                         /* create mode */
                       0, 0 );

   s_HOutput = s_HOriginal;
   s_HActive = s_HOutput;

   {
      CONSOLE_SCREEN_BUFFER_INFO csbi;

      GetConsoleScreenBufferInfo( s_HOriginal, &csbi );

      /* save screen info to restore on exit */
      memcpy( &s_csbi, &csbi, sizeof( csbi ) );

      csbi.dwSize.X = HB_MIN( csbi.dwSize.X, 80 );
      csbi.dwSize.Y = HB_MIN( csbi.dwSize.Y, 50 );

      csbi.srWindow.Right = HB_MIN( csbi.srWindow.Right, 79 );
      csbi.srWindow.Bottom = HB_MIN( csbi.srWindow.Bottom, 49 );
      csbi.srWindow.Top = csbi.srWindow.Left = 0;

      SetConsoleWindowInfo( s_HOriginal, TRUE,  &csbi.srWindow );
      SetConsoleScreenBufferSize( s_HOriginal, csbi.dwSize );

      s_HInactive = CreateConsoleScreenBuffer(
                 GENERIC_READ    | GENERIC_WRITE,    /* Access flag        */
                 FILE_SHARE_READ | FILE_SHARE_WRITE, /* Buffer share mode  */
                 NULL,                               /* Security attribute */
                 CONSOLE_TEXTMODE_BUFFER,            /* Type of buffer     */
                 NULL );                             /* reserved           */


      SetConsoleWindowInfo( s_HInactive, TRUE,  &csbi.srWindow );
      SetConsoleScreenBufferSize( s_HInactive, csbi.dwSize );

   }

/*
   SetConsoleActiveScreenBuffer( s_HActive );
*/
}

void hb_gt_Exit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Exit()"));

   if( s_HOutput != s_HOriginal )
   {
      /* ptucker */
      /* because the current screen may not be the one that was active
         when the app started, we need to restore that screen and update
         it with the current image before quitting.
       */
      /* easy fix ;-) */

      hb_gt_DispBegin();
      hb_gt_DispEnd();
   }
/* NOTE: There's no need to close these explicitly, moreover if we close them
         functions using stdout will not show anything.
   CloseHandle( s_HInput );
   s_HInput = INVALID_HANDLE_VALUE;
   CloseHandle( s_HOutput );
   s_HOutput = INVALID_HANDLE_VALUE;
*/
   SetConsoleScreenBufferSize( s_HOriginal, s_csbi.dwSize );
   SetConsoleWindowInfo( s_HOriginal, FALSE, &s_csbi.srWindow );

   /* detected using NuMega BoundsChecker */
   CloseHandle( s_HOriginal );
   s_HOriginal = INVALID_HANDLE_VALUE;

   CloseHandle( s_HInactive );
   s_HInactive = INVALID_HANDLE_VALUE;

   /* Remove Ctrl+Break handler [vszakats] */
   SetConsoleCtrlHandler( hb_gt_CtrlHandler, FALSE );

}

int hb_gt_ReadKey( HB_inkey_enum eventmask )
{
   int ch = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey(%d)", (int) eventmask));

   /* First check for Ctrl+Break, which is handled by gt/gtwin.c */
   if( s_bBreak )
   {
      /* Reset the global Ctrl+Break flag */
      s_bBreak = FALSE;
      ch = HB_BREAK_FLAG; /* Indicate that Ctrl+Break was pressed */
   }
   /* Check for events only when the event buffer is exhausted. */
   else if( s_cNumRead <= s_cNumIndex )
   {
      /* Check for keyboard input */
      s_cNumRead = 0;
      GetNumberOfConsoleInputEvents( s_HInput, &s_cNumRead );
      if( s_cNumRead )
      {
         /* Read keyboard input */
         ReadConsoleInput(
            s_HInput,         /* input buffer handle    */
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
            if( ch == 224 )
            {
               /* Strip extended key lead-in codes */
               ch = 0;
            }
            else if( ch < 0 )
            {
               /* Process international key codes */
               ch += 256;
            }
            else if( ch == 0 || ( dwState & ( ENHANCED_KEY | LEFT_ALT_PRESSED | LEFT_CTRL_PRESSED | RIGHT_ALT_PRESSED | RIGHT_CTRL_PRESSED | SHIFT_PRESSED ) ) )
            {
               /* Process non-ASCII key codes */
               WORD wChar = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wVirtualKeyCode;
               WORD wKey = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wVirtualScanCode;
               if( eventmask & INKEY_RAW ) wKey = wChar;
               /* Discard standalone state key presses for normal mode only */
               if( ( eventmask & INKEY_RAW ) == 0 ) switch( wKey )
               {
                  /* Virtual scan codes to ignore */
                  case 29: /* Ctrl */
                  case 40: /* Circle Accent */
                  case 41: /* Tick Accent */
                  case 42: /* Left Shift */
                  case 43: /* Reverse Tick Accent */
                  case 54: /* Right Shift */
                  case 56: /* Alt */
                  case 58: /* Caps Lock */
                  case 69: /* Num Lock */
                  case 70: /* Pause or Scroll Lock */
                     wKey = 0;
               }
               if( wKey == 0 ) ch = 0;
               else
               {
                  if( eventmask & INKEY_RAW )
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

                     HB_TRACE(HB_TR_INFO, ("hb_gt_ReadKey(): wKey is %d, dwState is %d, ch is %d", wKey, dwState, ch));

                     if( bAlt )
                     {
                        /* Alt key held */
                        if( ch == 0 || ch == wChar || tolower( ch ) == tolower( wChar ) )
                        {
                           /* Only translate if not AltGr */
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
                           default:
                              /* Only provide a translation for those key
                                 codes that don't have a character code. */
                              if( ch == 0 )
                                 ch = wKey + 128;
                        }
                     }
                     else
                     {
                        /* Normal key */
                        if( wKey == 53 && bEnhanced ) ch = '/'; /* Num Pad / */
                        else if( wKey > 59 && wKey <= 68 ) ch = 59 - wKey; /* F2 - F10 */
                        else if( wKey == 87 || wKey == 88 ) ch = 47 - wKey; /* F11, F12 */
                        else switch( wKey )
                        {
                           case 1: /* Esc */
                              ch = K_ESC;
                              break;
                           case 28: /* Num Pad Enter */
                              ch = K_ENTER;
                              break;
                           case 59: /* F1 */
                              ch = K_F1;
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
                           case 76: /* Num Pad 5 */
                              ch = 332;
                              break;
                           default:
                                 ch = wKey;
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
               if( eventmask & INKEY_RAW )
                  wKey = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wVirtualKeyCode;
               else
                  wKey = s_irInBuf[ s_cNumIndex ].Event.KeyEvent.wVirtualScanCode;
               HB_TRACE(HB_TR_INFO, ("hb_gt_ReadKey(): wKey is %d", wKey));
            }
#endif
         }
      }
      else if( eventmask & ~( INKEY_KEYBOARD | INKEY_RAW )
                           && s_irInBuf[ s_cNumIndex ].EventType == MOUSE_EVENT )
      {

         hb_mouse_iCol = s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwMousePosition.X;
         hb_mouse_iRow = s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwMousePosition.Y;

         if( eventmask & INKEY_MOVE && s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwEventFlags == MOUSE_MOVED )
            ch = K_MOUSEMOVE;

         else if( eventmask & INKEY_LDOWN && s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwButtonState &
                  FROM_LEFT_1ST_BUTTON_PRESSED )
         {
            if( s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwEventFlags == DOUBLE_CLICK )
               ch = K_LDBLCLK;
            else
               ch = K_LBUTTONDOWN;

            s_mouseLast = K_LBUTTONDOWN;
         }
         else if( eventmask & INKEY_RDOWN && s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwButtonState &
                  RIGHTMOST_BUTTON_PRESSED )
         {
            if( s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwEventFlags == DOUBLE_CLICK )
               ch = K_RDBLCLK;
            else
               ch = K_RBUTTONDOWN;

            s_mouseLast = K_RBUTTONDOWN;
         }
         else if( s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwEventFlags == 0 &&
                  s_irInBuf[ s_cNumIndex ].Event.MouseEvent.dwButtonState == 0 )
         {
            if( eventmask & INKEY_LUP && s_mouseLast == K_LBUTTONDOWN )
               ch = K_LBUTTONUP;
            else if( eventmask & INKEY_RUP && s_mouseLast == K_RBUTTONDOWN )
               ch = K_RBUTTONUP;
         }
      }
      /* Set up to process the next input event (if any) */
      s_cNumIndex++;
   }

   return ch;
}

BOOL hb_gt_AdjustPos( BYTE * pStr, ULONG ulLen )
{
   CONSOLE_SCREEN_BUFFER_INFO csbi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_AdjustPos(%s, %lu)", pStr, ulLen ));

   HB_SYMBOL_UNUSED( pStr );
   HB_SYMBOL_UNUSED( ulLen );

   GetConsoleScreenBufferInfo( s_HActive, &csbi );

   hb_gtSetPos( csbi.dwCursorPosition.Y, csbi.dwCursorPosition.X );

   return TRUE;
}

BOOL hb_gt_IsColor( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_IsColor()"));

   /* TODO: need to call something to do this instead of returning TRUE */
   return TRUE;
}

USHORT hb_gt_GetScreenWidth( void )
{
   CONSOLE_SCREEN_BUFFER_INFO csbi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth()"));

   GetConsoleScreenBufferInfo( s_HOutput, &csbi );
/* return csbi.dwMaximumWindowSize.X; */
/* return HB_MAX( csbi.srWindow.Right - csbi.srWindow.Left + 1, 40 ); */
   return HB_MAX( csbi.dwSize.X, 40 );
}

USHORT hb_gt_GetScreenHeight( void )
{
   CONSOLE_SCREEN_BUFFER_INFO csbi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeight()"));

   GetConsoleScreenBufferInfo( s_HOutput, &csbi );
/* return csbi.dwMaximumWindowSize.Y; */
/* return HB_MAX( csbi.srWindow.Bottom - csbi.srWindow.Top + 1, 25 ); */
   return HB_MAX( csbi.dwSize.Y, 25 );
}

void hb_gt_SetPos( SHORT iRow, SHORT iCol )
{
   COORD dwCursorPosition;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd)", iRow, iCol));

   dwCursorPosition.X = iCol;
   dwCursorPosition.Y = iRow;

   SetConsoleCursorPosition( s_HActive, dwCursorPosition );
}

USHORT hb_gt_GetCursorStyle( void )
{
   CONSOLE_CURSOR_INFO cci;
   USHORT uiCursorShape;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorStyle()"));

   GetConsoleCursorInfo( s_HActive, &cci );

   if( ! cci.bVisible )
   {
      uiCursorShape = SC_NONE;
   }
   else
   {
      switch( cci.dwSize )
      {
         case 50:
            uiCursorShape = SC_INSERT;   /* half block in clipper */
            break;

         case 99:
            uiCursorShape = SC_SPECIAL1; /* full block in clipper */
            break;

         case 66:
            uiCursorShape = SC_SPECIAL2; /* upper half block in clipper */
            break;
            /* TODO: cannot tell if the block is upper or lower for cursor */
            /* Answer: Supposed to be upper third, but ms don't support it. */

         default:
            uiCursorShape = SC_NORMAL;  /* anything else, we'll call it normal */
            break;
      }
   }

   return uiCursorShape;
}

void hb_gt_SetCursorStyle( USHORT style )
{
   CONSOLE_CURSOR_INFO cci;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorStyle(%hu)", style));

   GetConsoleCursorInfo( s_HActive, &cci );

   switch( style )
   {
      case SC_NONE:
         cci.bVisible = FALSE;
         break;

      case SC_INSERT:
         cci.bVisible = TRUE;
         cci.dwSize = 50;
         break;

      case SC_SPECIAL1:
         cci.bVisible = TRUE;
         cci.dwSize = 99;
         break;

      case SC_SPECIAL2:
         cci.bVisible = TRUE;
         cci.dwSize = 66;
         /* In their infinite wisdom, MS doesn't support cursors that
            don't start at the bottom of the cell */
         break;

      case SC_NORMAL:
      default:            /* traps for invalid values */
         cci.bVisible = TRUE;
         cci.dwSize = 25; /* this was 12, but when used in full screen dos window
                             cursor state is erratic  - doesn't turn off, etc. */
         break;
   }

   s_bOldCursor = cci.bVisible;

   SetConsoleCursorInfo( s_HActive, &cci );
}

static void hb_gt_xPutch( USHORT uiRow, USHORT uiCol, BYTE attr, BYTE byChar )
{
   DWORD dwWritten;
   COORD coord;
   char tmp[ 2 ];

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_xPutch(%hu, %hu, %d, %i)", uiRow, uiCol, (int) attr, byChar));

   /* TOFIX: add correct support for a single byte instead of a string
    */
   tmp[ 0 ] = byChar;
   tmp[ 1 ] = '\0';

   coord.X = ( SHORT ) uiCol;
   coord.Y = ( SHORT ) uiRow;

   FillConsoleOutputAttribute( s_HOutput, ( WORD )( attr & 0xFF ), ( DWORD ) 1, coord, &dwWritten );
   WriteConsoleOutputCharacterA( s_HOutput, tmp, 1, coord, &dwWritten );
}

void hb_gt_Puts( USHORT uiRow, USHORT uiCol, BYTE attr, BYTE * str, ULONG len )
{
   DWORD dwWritten;
   COORD coord;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu)", uiRow, uiCol, (int) attr, str, len));

   coord.X = ( SHORT ) uiCol;
   coord.Y = ( SHORT ) uiRow;

   FillConsoleOutputAttribute( s_HOutput, ( WORD )( attr & 0xFF ), ( DWORD ) len, coord, &dwWritten );
   WriteConsoleOutputCharacterA( s_HOutput, ( char * ) str, ( DWORD ) len, coord, &dwWritten );
}

int hb_gt_RectSize( USHORT rows, USHORT cols )
{
   return rows * cols * 2;
}

void hb_gt_GetText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * dest )
{
   LPWORD pwattr;
   BYTE * pstr;
   USHORT width;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetText(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, dest));

   width = ( uiRight - uiLeft + 1 );
   pwattr = ( LPWORD ) hb_xgrab( width * sizeof( *pwattr ) );
   pstr = ( BYTE * ) hb_xgrab( width );

   for( ; uiTop <= uiBottom; uiTop++ )
   {
      COORD coord;
      USHORT i;
      DWORD dwWritten;

      coord.X = ( SHORT ) uiLeft;
      coord.Y = ( SHORT ) uiTop;
      ReadConsoleOutputCharacterA( s_HOutput, ( char * ) pstr, width, coord, &dwWritten );
      ReadConsoleOutputAttribute( s_HOutput, pwattr, width, coord, &dwWritten );
      for( i = 0; i < width; i++ )
      {
         *dest = *( pstr + i );
         dest++;
         *dest = ( BYTE ) *( pwattr + i ) & 0xFF;
         dest++;
      }
   }

   hb_xfree( pstr );
   hb_xfree( pwattr );
}

void hb_gt_PutText( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE * srce )
{
   LPWORD pwattr;
   BYTE * pstr;
   USHORT width;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p)", uiTop, uiLeft, uiBottom, uiRight, srce));

   width = ( uiRight - uiLeft + 1 );
   pwattr = ( LPWORD ) hb_xgrab( width * sizeof( *pwattr ) );
   pstr = ( BYTE * ) hb_xgrab( width );

   for( ; uiTop <= uiBottom; uiTop++ )
   {
      COORD coord;
      USHORT i;
      DWORD dwWritten;

      for( i = 0; i < width; i++ )
      {
         *( pstr + i ) = *srce;
         srce++;
         *( pwattr + i ) = ( ( WORD )( ( BYTE ) *srce ) & 0xFF );
         srce++;
      }
      coord.X = ( SHORT ) uiLeft;
      coord.Y = ( SHORT ) uiTop;
      WriteConsoleOutputAttribute( s_HOutput, pwattr, width, coord, &dwWritten );
      WriteConsoleOutputCharacterA( s_HOutput, ( char * ) pstr, width, coord, &dwWritten );
   }

   hb_xfree( pstr );
   hb_xfree( pwattr );
}

void hb_gt_SetAttribute( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE attr )
{
/* ptucker */

   COORD coord;
   USHORT width;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetAttribute(%hu, %hu, %hu, %hu, %d)", uiTop, uiLeft, uiBottom, uiRight, (int) attr));

   width = uiRight - uiLeft + 1;

   coord.X = ( SHORT ) uiLeft;

   for( ; uiTop <= uiBottom; uiTop++ )
   {
      DWORD dwWritten;

      coord.Y = uiTop;
      FillConsoleOutputAttribute( s_HOutput, ( WORD )( attr & 0xFF ), width, coord, &dwWritten );
   }
}

SHORT hb_gt_Col( void )
{
   CONSOLE_SCREEN_BUFFER_INFO csbi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));

   GetConsoleScreenBufferInfo( s_HActive, &csbi );

   return csbi.dwCursorPosition.X;
}

SHORT hb_gt_Row( void )
{
   CONSOLE_SCREEN_BUFFER_INFO csbi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));

   GetConsoleScreenBufferInfo( s_HActive, &csbi );

   return csbi.dwCursorPosition.Y;
}

void hb_gt_Scroll( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight, BYTE attr, SHORT iVert, SHORT iHoriz )
{
/* ptucker */

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hd, %hd)", uiTop, uiLeft, uiBottom, uiRight, (int) attr, iVert, iHoriz));

   if( ( iHoriz | iVert ) == 0 ) /* both zero? */
   {
      COORD coord;
      USHORT width = uiRight - uiLeft + 1;

      coord.X = ( SHORT ) uiLeft;

      for( ; uiTop <= uiBottom; uiTop++ )
      {
         DWORD dwWritten;

         coord.Y = uiTop;
         FillConsoleOutputAttribute( s_HOutput, ( WORD )( attr & 0xFF ), width, coord, &dwWritten );
         FillConsoleOutputCharacter( s_HOutput, ' ', width, coord, &dwWritten );
      }
   }
   else
   {
      SMALL_RECT Source, Clip;
      COORD      Target;
      CHAR_INFO  FillChar;

      Source.Top    = uiTop;
      Source.Left   = uiLeft;
      Source.Bottom = uiBottom;
      Source.Right  = uiRight;

      memcpy( &Clip, &Source, sizeof( SMALL_RECT ) );

      Target.Y = uiTop - iVert;
      Target.X = uiLeft - iHoriz;

      FillChar.Char.AsciiChar = ' ';
      FillChar.Attributes = ( WORD )( attr & 0xFF );

      ScrollConsoleScreenBuffer( s_HOutput, &Source, &Clip, Target, &FillChar );
   }
}

void hb_gt_DispBegin( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispBegin()"));

/* ptucker */

   if( ++s_uiDispCount == 1 )
   {
      COORD coDest = { 0, 0 };
      COORD coBuf;                    /* the size of the buffer to read into */
      CHAR_INFO * pCharInfo;  /* buffer to store info from ReadConsoleOutput */
      SMALL_RECT srWin;                     /* source rectangle to read from */
      CONSOLE_SCREEN_BUFFER_INFO csbi;

      GetConsoleScreenBufferInfo( s_HOutput, &csbi );
      srWin.Top    = srWin.Left = 0;
      srWin.Bottom = ( coBuf.Y = csbi.dwSize.Y ) - 1;
      srWin.Right  = ( coBuf.X = csbi.dwSize.X ) - 1;

      /* allocate a buffer for the screen rectangle */
      pCharInfo = ( CHAR_INFO * ) hb_xgrab( coBuf.Y * coBuf.X * sizeof( CHAR_INFO ) );

      /* read the screen rectangle into the buffer */
      ReadConsoleOutput( s_HOutput,    /* current screen handle  */
                   pCharInfo,          /* transfer area          */
                   coBuf,              /* size of destination buffer */
                   coDest,             /* upper-left cell to write data to   */
                   &srWin );           /* screen buffer rectangle to read from */

      WriteConsoleOutput( s_HInactive, /* output handle */
                   pCharInfo,          /* data to write */
                   coBuf,              /* col/row size of source buffer */
                   coDest,             /* upper-left cell to write data from in src */
                   &srWin );           /* screen buffer rect to write data to */

      s_HOutput = s_HInactive;

      {
         CONSOLE_CURSOR_INFO cci;

         GetConsoleCursorInfo( s_HActive, &cci );
         s_bOldCursor = cci.bVisible;
         cci.bVisible = FALSE;
         SetConsoleCursorInfo( s_HActive, &cci );
      }

      hb_xfree( pCharInfo );
   }
}

void hb_gt_DispEnd( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispEnd()"));

/* ptucker */

   if( --s_uiDispCount == 0 )
   {
      s_HInactive = s_HActive;
      s_HActive = s_HOutput;
      SetConsoleActiveScreenBuffer( s_HActive );

      {
         CONSOLE_CURSOR_INFO cci;

         GetConsoleCursorInfo( s_HActive, &cci );
         cci.bVisible = s_bOldCursor;
         SetConsoleCursorInfo( s_HActive, &cci );
      }
   }
}

BOOL hb_gt_SetMode( USHORT uiRows, USHORT uiCols )
{
/* ptucker */
   BOOL bRetVal = TRUE;
   CONSOLE_SCREEN_BUFFER_INFO csbi;
   SMALL_RECT srWin;
   COORD coBuf;
   USHORT uiDispCount = s_uiDispCount;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", uiRows, uiCols));

   while( s_uiDispCount )
      hb_gt_DispEnd();

   GetConsoleScreenBufferInfo( s_HOutput, &csbi );
   coBuf = GetLargestConsoleWindowSize( s_HOutput );

   /* new console window size and scroll position */
   srWin.Top    = srWin.Left = 0;
   srWin.Bottom = ( SHORT ) ( HB_MIN( uiRows, coBuf.Y ) - 1 );
   srWin.Right  = ( SHORT ) ( HB_MIN( uiCols, coBuf.X ) - 1 );

   /* new console buffer size */
   coBuf.Y = uiRows;
   coBuf.X = uiCols;

   /* if the current buffer is larger than what we want, resize the */
   /* console window first, then the buffer */
   if( ( DWORD ) csbi.dwSize.X * csbi.dwSize.Y > ( DWORD ) uiCols * uiRows )
   {
      /* TODO: these calls are a temporary solution */
      SetConsoleWindowInfo( s_HActive, TRUE, &srWin );
      SetConsoleScreenBufferSize( s_HActive, coBuf );
      SetConsoleWindowInfo( s_HInactive, TRUE, &srWin );
      SetConsoleScreenBufferSize( s_HInactive, coBuf );
      if( !SetConsoleWindowInfo( s_HOutput, TRUE, &srWin ) ||
          !SetConsoleScreenBufferSize( s_HOutput, coBuf ) )
         bRetVal = FALSE;
   }
   else if( ( DWORD ) csbi.dwSize.X * csbi.dwSize.Y < ( DWORD ) uiCols * uiRows )
   {
      /* TODO: these calls are a temporary solution */
      SetConsoleScreenBufferSize( s_HActive, coBuf );
      SetConsoleWindowInfo( s_HActive, TRUE, &srWin );
      SetConsoleScreenBufferSize( s_HInactive, coBuf );
      SetConsoleWindowInfo( s_HInactive, TRUE, &srWin );
      if( !SetConsoleScreenBufferSize( s_HOutput, coBuf ) ||
          !SetConsoleWindowInfo( s_HOutput, TRUE, &srWin ) )
         bRetVal = FALSE;
   }

   while( s_uiDispCount < uiDispCount )
      hb_gt_DispBegin();

   return bRetVal;
}

void hb_gt_Replicate( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE byChar, ULONG ulLength )
{

/* ptucker */
   COORD coBuf;
   DWORD dwWritten;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Replicate(%hu, %hu, %i, %i, %lu)", uiRow, uiCol, byAttr, byChar, ulLength));

   coBuf.Y = uiRow;
   coBuf.X = uiCol;

   FillConsoleOutputAttribute( s_HOutput, ( WORD )( byAttr & 0xFF ), ( DWORD )ulLength, coBuf, &dwWritten );
   FillConsoleOutputCharacter(
           s_HOutput,                    /* handle to screen buffer        */
           byChar,                       /* character to write             */
           ( DWORD ) ulLength,           /* number of cells to write       */
           coBuf,                        /* coordinates of first cell      */
           &dwWritten                    /* receives actual number written */
           );

}

BOOL hb_gt_GetBlink()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetBlink()"));

   /* TODO */
   return FALSE;
}

void hb_gt_SetBlink( BOOL bBlink )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetBlink(%d)", (int) bBlink));

   /* TODO: set the bit if it's supported */
   HB_SYMBOL_UNUSED( bBlink );
}

#if 0

static void hb_gt_DebugScreen( BOOL bActivate )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DebugScreen(%d)", (int) bActivate));

   /* ptucker */
   /* TODO: This is not used and is still a work in progress */
   if( bActivate )
   {
      if( s_HDOutput == INVALID_HANDLE_VALUE )
      {
         CONSOLE_SCREEN_BUFFER_INFO csbi;
         SMALL_RECT srWin;

         s_HDOutput = CreateConsoleScreenBuffer(
                    GENERIC_READ    | GENERIC_WRITE,    /* Access flag        */
                    FILE_SHARE_READ | FILE_SHARE_WRITE, /* Buffer share mode  */
                    NULL,                               /* Security attribute */
                    CONSOLE_TEXTMODE_BUFFER,            /* Type of buffer     */
                    NULL );                             /* reserved           */


         GetConsoleScreenBufferInfo( s_HOutput, &csbi );

         /* new console window size and scroll position */
         srWin.Top    = srWin.Left = 0;
         srWin.Bottom = csbi.dwSize.Y - 1;
         srWin.Right  = csbi.dwSize.X - 1;

         SetConsoleScreenBufferSize( s_HDOutput, csbi.dwSize );
         SetConsoleWindowInfo( s_HDOutput, TRUE,  &csbi.srWindow );
         SetConsoleWindowInfo( s_HDOutput, FALSE, &srWin );
      }
      s_HOsave = s_HOutput;
      s_HOutput = s_HActive = s_HDOutput;

      hb_gt_DispBegin();
      hb_gt_DispEnd();
   }
   else
   {
      s_HOutput = s_HOsave;
      s_HActive = s_HOriginal;
   }
   SetConsoleActiveScreenBuffer( s_HOutput );
}

#endif

void hb_gt_Tone( double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Tone(%lf, %lf)", dFrequency, dDuration));

   /* The conversion from Clipper timer tick units to
      milliseconds is * 1000.0 / 18.2. */

   dDuration = dDuration * 1000.0 / 18.2; /* milliseconds */
   dDuration = HB_MIN( HB_MAX( 0, dDuration ), ULONG_MAX );

   if( dDuration > 0.0 )
      Beep( ( ULONG ) HB_MIN( HB_MAX( 0.0, dFrequency ), 32767.0 ),
            ( ULONG ) dDuration );
}

char * hb_gt_Version( void )
{
   return "Harbour Terminal: Win32 console";
}

USHORT hb_gt_DispCount()
{
   return s_uiDispCount;
}

USHORT hb_gt_Box( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight,
                  BYTE * szBox, BYTE byAttr )
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
