/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem for OS/2 compilers
 *
 * Copyright 1999 - 2001 Harbour Project
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_gt_ReadKey()
 *
 * Copyright 1999 Chen Kedem <niki@actcom.co.il>
 *    hb_gt_Tone()
 *    hb_gt_IsColor()
 *    hb_gt_Scroll()
 *    hb_gt_SetCursorSize()
 *    hb_gt_GetCellSize()
 *    hb_gt_GetCursorStyle()
 *    hb_gt_SetCursorStyle()
 *    hb_gt_SetAttribute()
 *    hb_gt_GetBlink()
 *    hb_gt_SetBlink()
 *
 * Copyright 2000 - 2001 Maurilio Longo <maurilio.longo@libero.it>
 *    hb_gt_DispBegin() / hb_gt_DispEnd()
 *    hb_gt_ScreenPtr() and hb_gt_xYYYY() functions and virtual screen support inside hb_gt_XXXX()s
 *    16 bit KBD subsystem use inside hb_gt_ReadKey()
 *
 * See doc/license.txt for licensing terms.
 *
 */

/*
 *  This module is partially based on VIDMGR by Andrew Clarke and modified
 *  for the Harbour project
 */

/* NOTE: User programs should never call this layer directly! */

#define INCL_BASE
#define INCL_VIO
#define INCL_KBD
#define INCL_MOU
#define INCL_DOSMEMMGR
#define INCL_DOSPROCESS
#define INCL_NOPMAPI

#define HB_GT_NAME	OS2

#include "hbgtcore.h"
#include "hbinit.h"
#include "hbapierr.h"
#include "inkey.ch"

#ifdef _HB_OS2_H
#include "hbos2.h"
#endif

/* convert 16:16 address to 0:32 */
#define SELTOFLAT(ptr) (void *)(((((ULONG)(ptr))>>19)<<16)|(0xFFFF&((ULONG)(ptr))))

#if defined(HARBOUR_GCC_OS2)
   /* 25/03/2000 - maurilio.longo@libero.it
   OS/2 GCC hasn't got ToolKit headers available */
   #include <stdlib.h>
#else
   #include <bsedos.h>
   #ifndef KBDTRF_EXTENDED_CODE
      #define KBDTRF_EXTENDED_CODE 0x02
   #endif
#endif
#include <conio.h>

static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER (&SuperTable)

static int  s_iCurRow;
static int  s_iCurCol;
static int  s_iCursorStyle;

/* pointer to offscreen video buffer */
static ULONG s_ulLVBptr;
/* length of video buffer */
static USHORT s_usLVBlength;


/* Code page ID of active codepage at the time harbour program was start */
static USHORT s_usOldCodePage;

/* Instead of calling VioGetMode() every time I need MaxRow() or MaxCol() I
   use this static which contains active mode info */
static VIOMODEINFO s_vi;

/* keyboard event record */
static PKBDKEYINFO s_key;
/* keyboard handle, 0 == default */
static PHKBD s_hk;

/* mouse logical handle */
static HMOU s_uMouHandle;

static void hb_gt_os2_mouse_Init( void )
{
   USHORT fsEvents = MOUSE_MOTION_WITH_BN1_DOWN | MOUSE_BN1_DOWN |
                     MOUSE_MOTION_WITH_BN2_DOWN | MOUSE_BN2_DOWN |
                     MOUSE_MOTION_WITH_BN3_DOWN | MOUSE_BN3_DOWN ;

   if( MouOpen ( 0L, &s_uMouHandle ) )          /* try to open mouse */
      s_uMouHandle = 0;                         /* no mouse found */
   else
      MouSetEventMask ( &fsEvents, s_uMouHandle );    /* mask some events */
}

static void hb_gt_os2_mouse_Exit( void )
{
   if( s_uMouHandle )
   {
      MouClose( s_uMouHandle );           /* relese mouse handle */
      s_uMouHandle = 0;
   }
}

static BOOL hb_gt_os2_mouse_IsPresent( void )
{
   return s_uMouHandle != 0;
}

static void hb_gt_os2_mouse_Show( void )
{
   if( s_uMouHandle )
      MouDrawPtr( s_uMouHandle );
}

static void hb_gt_os2_mouse_Hide( void )
{
   /*
      NOTE: mouse cursor always visible if not in full screen
    */
   NOPTRRECT rect;
   VIOMODEINFO vi;                             /* needed to get max Row/Col */
   if( s_uMouHandle )
   {
      /*
         QUESTION: should I call the GT MaxRow/Col function ?
         pro: encapsulating of the GetScreen function
         con: calling function from another module, GT must be linked in
         con: VioGetMode is been called twice
       */
      vi.cb = sizeof(VIOMODEINFO);
      VioGetMode( &vi, 0 );
      rect.row  = 0;                            /* x-coordinate upper left */
      rect.col  = 0;                            /* y-coordinate upper left */
      rect.cRow = vi.row - 1;                   /* x-coordinate lower right */
      rect.cCol = vi.col - 1;                   /* y-coordinate lower right */
      MouRemovePtr( &rect, s_uMouHandle );
   }
}

/*
   QUESTION: when getting mouse coordinate you normally need both
   row and column, we should think about using just one function
   hb_gt_os2_mouse_GetPos( &row, &col ) or something like that
*/

static void hb_gt_os2_mouse_GetPos( int * row, int * col )
{
   if( s_uMouHandle )
   {
      PTRLOC pos;
      MouGetPtrPos( &pos, s_uMouHandle );
      *row = ( int ) pos.row;
      *col = ( int ) pos.col;
   }
}

static void hb_gt_os2_mouse_SetPos( int row, int col )
{
   if( s_uMouHandle )
   {
      PTRLOC pos;
      pos.row = ( USHORT ) row;
      pos.col = ( USHORT ) col;
      MouSetPtrPos( &pos, s_uMouHandle );
   }
}

typedef struct
{
   BOOL     fDown;
   int      iPressed;
   int      iPressRow;
   int      iPressCol;
   ULONG    ulPressTime;
   int      iReleased;
   int      iReleaseRow;
   int      iReleaseCol;
   ULONG    ulReleaseTime;
} HB_OS2_BUTTONSTATE;

static HB_OS2_BUTTONSTATE s_ButtonState[ 3 ];
static ULONG   s_ButtonMask[ 3 ] =
                           { MOUSE_BN1_DOWN, MOUSE_BN2_DOWN, MOUSE_BN3_DOWN };
static ULONG   s_ulMouseLastState;

static void hb_gt_os2_mouse_ReadMouseState( void )
{
   if( s_uMouHandle )
   {
      USHORT WaitOption = 0;  /* 1 = wait until mouse event exist, 0 = don't */
      MOUEVENTINFO MouEvent;
      if( MouReadEventQue( &MouEvent, &WaitOption, s_uMouHandle ) == NO_ERROR )
      {
         ULONG ulDiff;
         int i;

         ulDiff = s_ulMouseLastState ^ MouEvent.fs;
         s_ulMouseLastState = MouEvent.fs;

         for( i = 0; i < 3; ++i )
         {
            if( ulDiff & s_ButtonMask[ i ] )
            {
               if( s_ulMouseLastState & s_ButtonMask[ i ] )
               {
                  s_ButtonState[ i ].fDown = TRUE;
                  s_ButtonState[ i ].iPressed++;
                  s_ButtonState[ i ].iPressRow = MouEvent.row;
                  s_ButtonState[ i ].iPressCol = MouEvent.col;
                  s_ButtonState[ i ].ulPressTime = MouEvent.time;
               }
               else
               {
                  s_ButtonState[ i ].fDown = FALSE;
                  s_ButtonState[ i ].iReleased++;
                  s_ButtonState[ i ].iReleaseRow = MouEvent.row;
                  s_ButtonState[ i ].iReleaseCol = MouEvent.col;
                  s_ButtonState[ i ].ulReleaseTime = MouEvent.time;
               }
            }
         }
      }
   }
}

static BOOL hb_gt_os2_mouse_ButtonState( int iButton )
{
   hb_gt_os2_mouse_ReadMouseState();

   if( s_uMouHandle && iButton >= 0 && iButton < 3 )
      return s_ButtonState[ iButton ].fDown;
   else
      return FALSE;
}

static BOOL hb_gt_os2_mouse_ButtonPressed( int iButton, int * piRow, int * piCol )
{
   hb_gt_os2_mouse_ReadMouseState();

   if( s_uMouHandle && iButton >= 0 && iButton < 3 )
   {
      if( s_ButtonState[ iButton ].iPressed )
      {
         s_ButtonState[ iButton ].iPressed = 0;
         *piRow = s_ButtonState[ iButton ].iPressRow;
         *piCol = s_ButtonState[ iButton ].iPressCol;
         return TRUE;
      }
   }

   return FALSE;
}

static BOOL hb_gt_os2_mouse_ButtonReleased( int iButton, int * piRow, int * piCol )
{
   hb_gt_os2_mouse_ReadMouseState();

   if( s_uMouHandle && iButton >= 0 && iButton < 3 )
   {
      if( s_ButtonState[ iButton ].iReleased )
      {
         s_ButtonState[ iButton ].iReleased = 0;
         *piRow = s_ButtonState[ iButton ].iReleaseRow;
         *piCol = s_ButtonState[ iButton ].iReleaseCol;
         return TRUE;
      }
   }

   return FALSE;
}

static int hb_gt_os2_mouse_CountButton( void )
{
   USHORT usButtons = 0;
   if( s_uMouHandle )
      MouGetNumButtons ( &usButtons, s_uMouHandle );
   return ( int ) usButtons;
}

static void hb_gt_os2_GetCursorPosition( int * piRow, int * piCol )
{
   USHORT y, x;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_os2_GetCursorPosition(%p, %p)", piRow, piCol));

   VioGetCurPos( &y, &x, 0 );

   *piRow = y;
   *piCol = x;
}

static void hb_gt_os2_SetCursorPosition( int iRow, int iCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_os2_SetCursorPosition(%d, %d)", iRow, iCol));

   if( s_iCurRow != iRow || s_iCurCol != iCol )
   {
      VioSetCurPos( ( USHORT ) iRow, ( USHORT ) iCol, 0 );
      s_iCurRow = iRow;
      s_iCurCol = iCol;
   }
}

static void hb_gt_os2_SetCursorSize( char start, char end, int visible )
{
   VIOCURSORINFO vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_os2_SetCursorSize(%d, %d, %d)", (int) start, (int) end, visible));

   vi.yStart = start;
   vi.cEnd = end;
   vi.cx = 0;
   vi.attr = ( visible ? 0 : -1 );
   VioSetCurType( &vi, 0 );
}

static char hb_gt_os2_GetCharHeight()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_os2_GetCharHeight()"));

   return ( char )( s_vi.row ? ( s_vi.vres / s_vi.row ) - 1 : 0 );
}

static int hb_gt_os2_GetCursorStyle( void )
{
   int iStyle;
   char charheight;
   VIOCURSORINFO vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_os2_GetCursorStyle()"));

   VioGetCurType( &vi, 0 );

   if( vi.attr )
      iStyle = SC_NONE;
   else
   {
      charheight = hb_gt_os2_GetCharHeight();

      if( vi.yStart == 0 && vi.cEnd == 0 )
         iStyle = SC_NONE;

      else if( ( vi.yStart == charheight - 1 || vi.yStart == charheight - 2 ) &&
               vi.cEnd == charheight )
         iStyle = SC_NORMAL;

      else if( vi.yStart == charheight >> 1 && vi.cEnd == charheight )
         iStyle = SC_INSERT;

      else if( vi.yStart == 0 && vi.cEnd == charheight )
         iStyle = SC_SPECIAL1;

      else if( vi.yStart == 0 && vi.cEnd == charheight >> 1 )
         iStyle = SC_SPECIAL2;

      else
         iStyle = -1;
   }

   return iStyle;
}

static void hb_gt_os2_SetCursorStyle( int iStyle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_os2_SetCursorStyle(%d)", iStyle));

   if( iStyle != s_iCursorStyle )
   {
      char charheight;

      charheight = hb_gt_os2_GetCharHeight();

      switch( iStyle )
      {
         case SC_NONE:
            hb_gt_os2_SetCursorSize( 0, 0, 0 );
            break;

         case SC_NORMAL:
            hb_gt_os2_SetCursorSize( charheight - 1, charheight, 1 );
            break;

         case SC_INSERT:
            hb_gt_os2_SetCursorSize( charheight >> 1, charheight, 1 );
            break;

         case SC_SPECIAL1:
            hb_gt_os2_SetCursorSize( 0, charheight, 1 );
            break;

         case SC_SPECIAL2:
            hb_gt_os2_SetCursorSize( 0, charheight >> 1, 1 );
            break;

         default:
            return;
      }
      s_iCursorStyle = iStyle;
   }
}

static void hb_gt_os2_GetScreenContents( void )
{
   /* TODO: implement it if necessary */
   ;
}

static void hb_gt_os2_Init( FHANDLE hFilenoStdin, FHANDLE hFilenoStdout, FHANDLE hFilenoStderr )
{
   APIRET rc;           /* return code from DosXXX api call */

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_os2_Init(%p,%p,%p)", hFilenoStdin, hFilenoStdout, hFilenoStderr ) );

   s_vi.cb = sizeof( VIOMODEINFO );
   VioGetMode( &s_vi, 0 );        /* fill structure with current video mode settings */

   if( VioGetBuf( &s_ulLVBptr, &s_usLVBlength, 0 ) == NO_ERROR )
   {
      s_ulLVBptr = ( ULONG ) SELTOFLAT( s_ulLVBptr );
      VioShowBuf( 0, s_usLVBlength, 0 );
   }
   else
   {
      s_ulLVBptr = ( ULONG ) NULL;
   }

   /* Alloc tileable memory for calling a 16 subsystem */
   rc = DosAllocMem( ( PPVOID ) &s_hk, sizeof( HKBD ),
                     PAG_COMMIT | OBJ_TILE | PAG_WRITE );
   if( rc != NO_ERROR )
   {
      hb_errInternal( HB_EI_XGRABALLOC, "hb_gt_os2_ReadKey() memory allocation failure.", NULL, NULL );
   }

   /* it is a long after all, so I set it to zero only one time since it never changes */
   memset( s_hk, 0, sizeof( HKBD ) );

   rc = DosAllocMem( ( PPVOID ) &s_key, sizeof( KBDKEYINFO ),
                     PAG_COMMIT | OBJ_TILE | PAG_WRITE);
   if( rc != NO_ERROR )
   {
      hb_errInternal( HB_EI_XGRABALLOC, "hb_gt_os2_ReadKey() memory allocation failure.", NULL, NULL);
   }

   /* TODO: Is anything else required to initialize the video subsystem?
            I (Maurilio Longo) think that we should set correct codepage

      COMMENT: The correct behaviour is to inherit the codepage that is
               active when the program is started, which automatically
               happens by not setting the codepage. If somebody wants to
               change the codepage, there should be a separate function
               to do that. (David G. Holm <dholm@jsd-llc.com>)

      ANSWER:  But I have a different code page than you and box drawing
               chars are "wrong". So we need to set code page of
               box drawing chars. (Maurilio Longo - maurilio.longo@libero.it)
   */

   /* 21/08/2001 - <maurilio.longo@libero.it>
      NOTE: Box drawing characters need page 437 to show correctly, so, in your
            config.sys you need to have a CODEPAGE=x,y statement where x or y
            is equal to 437
   */

   VioGetCp( 0, &s_usOldCodePage, 0 );

   /* If I could not set codepage 437 I reset previous codepage,
      maybe I do not need to do this */
   if( VioSetCp( 0, 437, 0 ) != NO_ERROR )
   {
      VioSetCp( 0, s_usOldCodePage, 0 );
   }

   hb_gt_os2_GetCursorPosition( &s_iCurRow, &s_iCurCol );
   s_iCursorStyle = hb_gt_os2_GetCursorStyle();

   HB_GTSUPER_INIT( hFilenoStdin, hFilenoStdout, hFilenoStderr );
   HB_GTSUPER_RESIZE( s_vi.row, s_vi.col );
   HB_GTSUPER_SETPOS( s_iCurRow, s_iCurCol );
   if( s_iCursorStyle > 0 )
      HB_GTSUPER_SETCURSORSTYLE( s_iCursorStyle );
   hb_gt_os2_GetScreenContents();
}

static void hb_gt_os2_Exit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_os2_Exit()"));

   HB_GTSUPER_EXIT();

   DosFreeMem( s_key );
   DosFreeMem( s_hk );
   VioSetCp( 0, s_usOldCodePage, 0 );
}


static int hb_gt_os2_ReadKey( int iEventMask )
{
   int ch;              /* next char if any */

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_os2_ReadKey(%d)", iEventMask));

   /* zero out keyboard event record */
   memset( s_key, 0, sizeof( KBDKEYINFO ) );

   /* Get next character without wait */
   KbdCharIn( s_key, IO_NOWAIT, ( HKBD ) * s_hk );

   /* extended key codes have 00h or E0h as chChar */
   if( ( s_key->fbStatus & KBDTRF_EXTENDED_CODE ) &&
       ( s_key->chChar == 0x00 || s_key->chChar == 0xE0 ) )
   {
      /* It was an extended function key lead-in code, so read the actual function key and then offset it by 256,
         unless extended keyboard events are allowed, in which case offset it by 512 */
      if( ( s_key->chChar == 0xE0 ) && ( iEventMask & INKEY_RAW ) )
      {
         ch = ( int ) s_key->chScan + 512;
      }
      else
      {
         ch = ( int ) s_key->chScan + 256;
      }
   }
   else if ( s_key->fbStatus & KBDTRF_FINAL_CHAR_IN )
   {
      ch = ( int ) s_key->chChar;
   }
   else
   {
      ch = 0;
   }

   /* Perform key translations */
   switch( ch )
   {
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
   if( ch == 0 )
   {
      ch = hb_mouse_ReadKey( iEventMask );
   }

   return ch;
}

static BOOL hb_gt_os2_IsColor( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_os2_IsColor()"));

   return s_vi.fbType != 0;        /* 0 = monochrom-compatible mode */
}

static BOOL hb_gt_os2_GetBlink()
{
   VIOINTENSITY vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_os2_GetBlink()"));

   vi.cb   = sizeof( VIOINTENSITY );    /* 6                          */
   vi.type = 2;                         /* get intensity/blink toggle */
   VioGetState( &vi, 0 );
   return vi.fs == 0;                   /* 0 = blink, 1 = intens      */
}


static void hb_gt_os2_SetBlink( BOOL fBlink )
{
   VIOINTENSITY vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_os2_SetBlink(%d)", (int) fBlink));

   vi.cb   = sizeof( VIOINTENSITY );    /* 6                          */
   vi.type = 2;                         /* set intensity/blink toggle */
   vi.fs   = ( fBlink ? 0 : 1 );        /* 0 = blink, 1 = intens      */
   VioSetState( &vi, 0 );
}

static void hb_gt_os2_Tone( double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_os2_Tone(%lf, %lf)", dFrequency, dDuration));

   /* The conversion from Clipper timer tick units to
      milliseconds is * 1000.0 / 18.2. */

   dFrequency = HB_MIN( HB_MAX( 0.0, dFrequency ), 32767.0 );
   dDuration = dDuration * 1000.0 / 18.2; /* milliseconds */

   while( dDuration > 0.0 )
   {
      USHORT temp = ( USHORT ) HB_MIN( HB_MAX( 0, dDuration ), USHRT_MAX );

      dDuration -= temp;
      if( temp <= 0 )
      {
         /* Ensure that the loop gets terminated when
            only a fraction of the delay time remains. */
         dDuration = -1.0;
      }
      else
      {
         DosBeep( ( USHORT ) dFrequency, temp );
      }
   }
}

static char * hb_gt_os2_Version( int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_os2_Version(%d)", iType ) );

   if( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Harbour Terminal: OS/2 console";
}

static BOOL hb_gt_os2_SetMode( int iRows, int iCols )
{
   BOOL fResult;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_os2_SetMode(%d, %d)", iRows, iCols));

   s_vi.cb = sizeof( VIOMODEINFO );
   VioGetMode( &s_vi, 0 );    /* fill structure with current settings */
   s_vi.row = iRows;
   s_vi.col = iCols;
   fResult = VioSetMode( &s_vi, 0 ) == 0; /* 0 = Ok, other = Fail */

   if( !fResult )
   {
      s_vi.cb = sizeof( VIOMODEINFO );
      VioGetMode( &s_vi, 0 );    /* fill structure with current settings */
   }

   hb_gt_os2_GetCursorPosition( &s_iCurRow, &s_iCurCol );
   s_iCursorStyle = hb_gt_os2_GetCursorStyle();
   HB_GTSUPER_RESIZE( s_vi.row, s_vi.col );
   HB_GTSUPER_SETPOS( s_iCurRow, s_iCurCol );
   if( s_iCursorStyle > 0 )
      HB_GTSUPER_SETCURSORSTYLE( s_iCursorStyle );
   hb_gt_os2_GetScreenContents();

   return fResult;
}

static BOOL hb_gt_os2_PreExt()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_os2_PreExt()"));

   return TRUE;
}

static BOOL hb_gt_os2_PostExt()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_os2_PostExt()"));

   hb_gt_os2_GetCursorPosition( &s_iCurRow, &s_iCurCol );
   HB_GTSUPER_SETPOS( s_iCurRow, s_iCurCol );
   hb_gt_os2_GetScreenContents();

   return TRUE;
}

static BOOL hb_gt_os2_Suspend()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_os2_Suspend()"));

   return TRUE;
}

static BOOL hb_gt_os2_Resume()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_os2_Resume()"));

   s_vi.cb = sizeof( VIOMODEINFO );
   VioGetMode( &s_vi, 0 );    /* fill structure with current settings */
   hb_gt_os2_GetCursorPosition( &s_iCurRow, &s_iCurCol );
   s_iCursorStyle = hb_gt_os2_GetCursorStyle();
   HB_GTSUPER_RESIZE( s_vi.row, s_vi.col );
   HB_GTSUPER_SETPOS( s_iCurRow, s_iCurCol );
   if( s_iCursorStyle > 0 )
      HB_GTSUPER_SETCURSORSTYLE( s_iCursorStyle );
   hb_gt_os2_GetScreenContents();

   return TRUE;
}

static void hb_gt_os2_Redraw( int iRow, int iCol, int iSize )
{
   BYTE bColor, bAttr;
   USHORT usChar, usCell;
   int iLen = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_os2_Redraw(%d, %d, %d)", iRow, iCol, iSize ) );

   while( iLen < iSize )
   {
      if( !hb_gt_GetScrChar( iRow, iCol + iLen, &bColor, &bAttr, &usChar ) )
         break;

      /*
       * TODO: it can be very slow (I haven't tested it) because it
       * update screen with single characters so if necessary optimize
       * it by groping cells for the whole line or characters with
       * the same color. [druzus]
       */
      usCell = ( bColor << 8 ) + ( usChar & 0xff );
      VioWrtNCell( ( PBYTE ) &usCell, 1, iRow, iCol + iLen, 0 );

      iLen++;
   }
}

static void hb_gt_os2_Refresh( void )
{
   int iRow, iCol, iStyle;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_os2_Refresh()" ) );

   HB_GTSUPER_REFRESH();

   hb_gt_GetScrCursor( &iRow, &iCol, &iStyle );
   if( iStyle != SC_NONE )
   {
      if( iRow >= 0 && iCol >= 0 && iRow < s_vi.row && iCol < s_vi.col )
         hb_gt_os2_SetCursorPosition( iRow, iCol );
      else
         iStyle = SC_NONE;
   }
   hb_gt_os2_SetCursorStyle( iStyle );
}


/* *********************************************************************** */

static BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_FuncInit(%p)", pFuncTable));

   pFuncTable->Init                       = hb_gt_os2_Init;
   pFuncTable->Exit                       = hb_gt_os2_Exit;
   pFuncTable->IsColor                    = hb_gt_os2_IsColor;
   pFuncTable->SetMode                    = hb_gt_os2_SetMode;
   pFuncTable->Redraw                     = hb_gt_os2_Redraw;
   pFuncTable->Refresh                    = hb_gt_os2_Refresh;
   pFuncTable->SetBlink                   = hb_gt_os2_SetBlink;
   pFuncTable->GetBlink                   = hb_gt_os2_GetBlink;
   pFuncTable->Version                    = hb_gt_os2_Version;
   pFuncTable->Suspend                    = hb_gt_os2_Suspend;
   pFuncTable->Resume                     = hb_gt_os2_Resume;
   pFuncTable->PreExt                     = hb_gt_os2_PreExt;
   pFuncTable->PostExt                    = hb_gt_os2_PostExt;
   pFuncTable->Tone                       = hb_gt_os2_Tone;

   pFuncTable->ReadKey                    = hb_gt_os2_ReadKey;

   pFuncTable->MouseInit                  = hb_gt_os2_mouse_Init;
   pFuncTable->MouseExit                  = hb_gt_os2_mouse_Exit;
   pFuncTable->MouseIsPresent             = hb_gt_os2_mouse_IsPresent;
   pFuncTable->MouseShow                  = hb_gt_os2_mouse_Show;
   pFuncTable->MouseHide                  = hb_gt_os2_mouse_Hide;
   pFuncTable->MouseGetPos                = hb_gt_os2_mouse_GetPos;
   pFuncTable->MouseSetPos                = hb_gt_os2_mouse_SetPos;
   pFuncTable->MouseButtonState           = hb_gt_os2_mouse_ButtonState;
   pFuncTable->MouseButtonPressed         = hb_gt_os2_mouse_ButtonPressed;
   pFuncTable->MouseButtonReleased        = hb_gt_os2_mouse_ButtonReleased;
   pFuncTable->MouseCountButton           = hb_gt_os2_mouse_CountButton;

   return TRUE;
}

/* ********************************************************************** */

static HB_GT_INIT gtInit = { HB_GT_DRVNAME( HB_GT_NAME ),
                             hb_gt_FuncInit,
                             HB_GTSUPER };

HB_GT_ANNOUNCE( HB_GT_NAME );

HB_CALL_ON_STARTUP_BEGIN( _hb_startup_gt_Init_ )
   hb_gtRegister( &gtInit );
HB_CALL_ON_STARTUP_END( _hb_startup_gt_Init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_startup_gt_Init_
#elif defined(HB_MSC_STARTUP)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM hb_vm_auto__hb_startup_gt_Init_ = _hb_startup_gt_Init_;
   #pragma data_seg()
#endif
