/* $Id$

   Harbour Project source code

   This module contains the Harbour functions for INKEY management.

   Copyright 1999 David G. Holm <dholm@jsd-llc.com>
   www - http://www.harbour-project.org

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version, with one exception:

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).

   V 1.21   David G. Holm               Added OS/2 DosSleep()
   V 1.15   David G. Holm               Tested Borland 3.1 hb_releaseCPU()
   V 1.5    Paul Tucker                 ReleaseCPU comments
   V 1.4    Victor Szel
   V 1.3    Victor Szel                 #include <x> changed to #include "x".
   V 1.2    Gonzalo Diethelm            ?
   V 1.1    David G. Holm               Committed to CVS.
   V 1.0    David G. Holm               Initial version.
*/
/* Note: The following #ifdef block for __IBMCPP__ must be ahead
         of any and all #include statements and requires that
         Harbour includes are ahead of platform includes.
*/
#ifdef __IBMCPP__
  #define INCL_DOSPROCESS
#endif

#include "extend.h"
#include "errorapi.h"
#include "inkey.h"
#include "init.h"

#if defined(__TURBOC__) || defined(__BORLANDC__) || defined(__MSC__) || defined(_MSC_VER)
  #include <conio.h>
  #include <dos.h>
#elif  defined(__DJGPP__)
  #include <pc.h>
  #include <dos.h>
#elif defined(HARBOUR_GCC_OS2)
  #include <stdlib.h>
#elif defined(__IBMCPP__)
  #define INCL_DOSPROCESS
  #include <bsedos.h>
  #include <conio.h>
#elif defined(__CYGWIN__)
  #include <mingw32/conio.h>
#endif
#include <time.h>

#ifdef __WATCOMC__
  #include <conio.h>
  #include <i86.h>
  #if defined(__386__) && !defined(__WINDOWS_386__)
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
  ULONG DosSleep (ULONG ulMilliseconds);
#endif

#if defined(OS_UNIX_COMPATIBLE)
#include <unistd.h>
#include <termios.h>

static struct termios startup_attributes;

static void restore_input_mode( void )
{
   tcsetattr( STDIN_FILENO, TCSANOW, &startup_attributes );
}

HB_CALL_ON_STARTUP_BEGIN( init_input_mode )
  struct termios ta;

  tcgetattr( STDIN_FILENO, &startup_attributes );
  atexit( restore_input_mode );

  tcgetattr( STDIN_FILENO, &ta );
  ta.c_lflag &= ~(ICANON | ECHO);
  ta.c_cc[ VMIN ] =0;
  ta.c_cc[ VTIME ] =0;
  tcsetattr( STDIN_FILENO, TCSAFLUSH, &ta );
HB_CALL_ON_STARTUP_END( init_input_mode )
#endif

static int *s_inkeyBuffer=0;  /* Harbour keyboard buffer (empty if head == tail)     */
static int  s_inkeyHead;      /* Harbour keyboard buffer head pointer (next insert)  */
static int  s_inkeyTail;      /* Harbour keyboard buffer tail pointer (next extract) */
static int  s_inkeyLast;      /* Last key extracted from Harbour keyboard buffer     */
static BOOL s_inkeyPoll;      /* Flag to override no polling when TYPEAHEAD is 0     */
static int  s_inkeyForce;     /* Variable to hold keyboard input when TYPEAHEAD is 0 */
static HB_inkey_enum s_eventmask;

void hb_releaseCPU( void )
{
/* TODO: Add code to release time slices on all platforms */
#if defined(_Windows)
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
    geninterrupt(0x2f);
   _AH = 0;
   _AL ^= 0x80;
  #else
    union REGS regs;
    regs.h.ah = 0x16;
    regs.h.al = 0x80;
  #if defined(__WATCOMC__) && defined(__386__)
    int386(0x2f, &regs, &regs);
  #else
    int86(0x2f, &regs, &regs);
  #endif
    regs.h.ah  = 0;
    regs.h.al ^= 0x80;
  #endif
#elif defined(OS_UNIX_COMPATIBLE)
#else
#endif
}

int hb_inkey ( double seconds, HB_inkey_enum event_mask, BOOL wait, BOOL forever )
{
   int key;
   clock_t end_clock;
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
   hb_inkeyPoll();
   return s_inkeyLast;
}

int hb_inkeyNext( void )      /* Return the next key without extracting it */
{
   int key = 0;
   hb_inkeyPoll();
   if( hb_set.HB_SET_TYPEAHEAD )
   {
      /* Proper typeahead support is enabled */
      if( s_inkeyHead == s_inkeyTail ) key = 0;
      else key = s_inkeyBuffer[ s_inkeyTail ];
   }
   else key = s_inkeyForce; /* Typeahead support is disabled */
   return key;
}

void hb_inkeyPoll( void )     /* Poll the console keyboard to stuff the Harbour buffer */
{
   /* TODO: Add mouse support */
   if( hb_set.HB_SET_TYPEAHEAD || s_inkeyPoll )
   {
      int ch = 0;
#if defined(OS_DOS_COMPATIBLE) || defined(HARBOUR_GCC_OS2) || defined(__IBMCPP__) || defined(_Windows)
   /* The reason for including _Windows here is that kbhit() and getch() appear
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
      /* _read_kbd() returns -1 for no key, but Harbour expects 0 */
      if( ch == -1 ) ch = 0;
   #else
      if( kbhit() )
      {
         /* A key code is available in the BIOS keyboard buffer, so read it */
      #if defined(__DJGPP__)
         if( s_eventmask & INKEY_EXTENDED ) ch = getxkey();
         else ch = getkey();
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
         case - 1:  /* No key available */
            ch = 0;
            break;
         case 328:  /* Up arrow */
            ch = 5;
            break;
         case 336:  /* Down arrow */
            ch = 24;
            break;
         case 331:  /* Left arrow */
            ch = 19;
            break;
         case 333:  /* Right arrow */
            ch = 4;
            break;
         case 327:  /* Home */
            ch = 1;
            break;
         case 335:  /* End */
            ch = 6;
            break;
         case 329:  /* Page Up */
            ch = 18;
            break;
         case 337:  /* Page Down */
            ch = 3;
            break;
         case 371:  /*  Ctrl + Left arrow */
            ch = 26;
            break;
         case 372:  /* Ctrl + Right arrow */
            ch = 2;
            break;
         case 375:  /* Ctrl + Home */
            ch = 29;
            break;
         case 373:  /* Ctrl + End */
            ch = 23;
            break;
         case 388:  /* Ctrl + Page Up */
            ch = 31;
            break;
         case 374:  /* Ctrl + Page Down */
            ch = 30;
            break;
         case 338:  /* Insert */
            ch = 22;
            break;
         case 339:  /* Delete */
            ch = 7;
            break;
         case 315:  /* F1 */
            ch = 28;
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
#elif defined(OS_UNIX_COMPATIBLE)
      /* TODO: */
      if( ! read( STDIN_FILENO, &ch, 1 ) )
         ch =0;
#else
      /* TODO: Support for other platforms, such as Mac */
#endif

      hb_inkeyPut( ch );
   }
}

void hb_inkeyReset( BOOL allocate)      /* Reset the keyboard buffer */
{
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
         s_inkeyBuffer = (int *)hb_xgrab( sizeof( int )
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
   double seconds;
   HB_inkey_enum event_mask = hb_set.HB_SET_EVENTMASK; /* Default to the SET input event mask */

   if( args > 2 )
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "INKEY" ); /* NOTE: Clipper catches this at compile time! */

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
//      fflush( stdout );
//      fflush( stderr );
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
      hb_errRT_BASE(EG_ARG, 3007, NULL, "INKEY");
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
 *      __KEYBOARD( [<cString>] )
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
 *      C
 *  $COMPLIANCE$
 *      NEXTKEY() is compliant with CA-Clipper 5.3
 *  $SEEALSO$
 *      CLEAR TYPEAHEAD, KEYBOARD
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
      else s_inkeyForce = ch; /* Typeahead support is disabled */
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
 *      C
 *  $COMPLIANCE$
 *      Was not part of Clipper
 *  $SEEALSO$
 *      KEYBOARD, CLEAR TYPEAHEAD, INKEY
 *  $END$
 */

HARBOUR HB___KEYPUT( void )
{
   if( ISNUM( 1 ) )
      hb_inkeyPut( hb_parni( 1 ) );
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
 *      C
 *  $COMPLIANCE$
 *      __KEYBOARD is compliant with CA-Clipper 5.3
 *  $SEEALSO$
 *      INKEY(), LASTKEY()
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
 *      C
 *  $COMPLIANCE$
 *      __KEYBOARD is compliant with CA-Clipper 5.3
 *  $SEEALSO$
 *      INKEY(), LASTKEY()
 *  $END$
 */

HARBOUR HB_LASTKEY( void )
{
   hb_retni( s_inkeyLast );
}
