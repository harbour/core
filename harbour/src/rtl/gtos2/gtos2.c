/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem for OS/2 compilers
 *
 * Copyright 1999-2001 {list of individual authors and e-mail addresses}
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_gt_os2_ReadKey()
 *
 * Copyright 1999 Chen Kedem <niki@synel.co.il>
 *    hb_gt_os2_mouse_Init()
 *    hb_gt_os2_mouse_Exit()
 *    hb_gt_os2_mouse_IsPresent()
 *    hb_gt_os2_mouse_Show()
 *    hb_gt_os2_mouse_Hide()
 *    hb_gt_os2_mouse_SetPos()
 *    hb_gt_os2_mouse_CountButton()
 *    hb_gt_os2_Tone()
 *    hb_gt_os2_IsColor()
 *    hb_gt_os2_SetCursorSize()
 *    hb_gt_os2_GetCharHeight()
 *    hb_gt_os2_GetCursorStyle()
 *    hb_gt_os2_SetCursorStyle()
 *    hb_gt_os2_GetBlink()
 *    hb_gt_os2_SetBlink()
 *
 * Copyright 2000-2001 Maurilio Longo <maurilio.longo@libero.it>
 *    hb_gt_DispBegin() / hb_gt_DispEnd()
 *    hb_gt_ScreenPtr() and hb_gt_xYYYY() functions and virtual screen support inside hb_gt_XXXX()s
 *    16 bit KBD subsystem use inside hb_gt_os2_ReadKey()
 *
 * See COPYING.txt for licensing terms.
 *
 */

/*
 * This module is partially based on VIDMGR by Andrew Clarke and modified for Harbour.
 */

/* NOTE: User programs should never call this layer directly! */

#define HB_GT_NAME  OS2

#include "hbgtcore.h"
#include "hbinit.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "inkey.ch"

#define INCL_BASE
#define INCL_VIO
#define INCL_KBD
#define INCL_MOU
#define INCL_DOSMEMMGR
#define INCL_DOSPROCESS
#define INCL_NOPMAPI

#include <os2.h>

/* convert 16:16 address to 0:32 */
#define SELTOFLAT( ptr )  ( void * ) ( ( ( ( ( ULONG ) ( ptr ) ) >> 19 ) << 16 ) | ( 0xFFFF & ( ( ULONG ) ( ptr ) ) ) )

#if defined( HB_OS_OS2_GCC )
   /* 2000.03.25 - maurilio.longo@libero.it
   OS/2 GCC hasn't got ToolKit headers available */
   #include <stdlib.h>
#else
   #include <bsedos.h>
   #ifndef KBDTRF_EXTENDED_CODE
      #define KBDTRF_EXTENDED_CODE  0x02
   #endif
#endif
#include <conio.h>

static int s_GtId;
static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER                  ( &SuperTable )
#define HB_GTID_PTR                 ( &s_GtId )

static int s_iCurRow;
static int s_iCurCol;
static int s_iCursorStyle;

/* buffer for single screen line */
static int    s_iLineBufSize = 0;
static char * s_sLineBuf;

/* Code page ID of active codepage at the time harbour program was start */
static HB_USHORT s_usOldCodePage;

/* Instead of calling VioGetMode() every time I need MaxRow() or MaxCol() I
   use this static which contains active mode info */
static VIOMODEINFO s_vi;

/* keyboard event record */
static PKBDKEYINFO s_key;
/* keyboard handle, 0 == default */
static PHKBD s_hk;

/* mouse logical handle */
static HMOU s_uMouHandle;

static void hb_gt_os2_mouse_Init( PHB_GT pGT )
{
   USHORT fsEvents = MOUSE_MOTION_WITH_BN1_DOWN | MOUSE_BN1_DOWN |
                     MOUSE_MOTION_WITH_BN2_DOWN | MOUSE_BN2_DOWN |
                     MOUSE_MOTION_WITH_BN3_DOWN | MOUSE_BN3_DOWN;

   HB_SYMBOL_UNUSED( pGT );

   if( MouOpen( 0L, &s_uMouHandle ) )              /* try to open mouse */
      s_uMouHandle = 0;                            /* no mouse found */
   else
      MouSetEventMask( &fsEvents, s_uMouHandle );  /* mask some events */
}

static void hb_gt_os2_mouse_Exit( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   if( s_uMouHandle )
   {
      MouClose( s_uMouHandle );           /* relese mouse handle */
      s_uMouHandle = 0;
   }
}

static HB_BOOL hb_gt_os2_mouse_IsPresent( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   return s_uMouHandle != 0;
}

static void hb_gt_os2_mouse_Show( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   if( s_uMouHandle )
      MouDrawPtr( s_uMouHandle );
}

static void hb_gt_os2_mouse_Hide( PHB_GT pGT )
{
   /*
      NOTE: mouse cursor always visible if not in full screen
    */
   HB_SYMBOL_UNUSED( pGT );

   if( s_uMouHandle )
   {
      NOPTRRECT rect;
      VIOMODEINFO vi;                           /* needed to get max Row/Col */

      /*
         QUESTION: should I call the GT MaxRow/Col function ?
         pro: encapsulating of the GetScreen function
         con: calling function from another module, GT must be linked in
         con: VioGetMode is been called twice
       */
      vi.cb = sizeof( VIOMODEINFO );
      VioGetMode( &vi, 0 );
      rect.row  = 0;                            /* x-coordinate upper left */
      rect.col  = 0;                            /* y-coordinate upper left */
      rect.cRow = vi.row - 1;                   /* x-coordinate lower right */
      rect.cCol = vi.col - 1;                   /* y-coordinate lower right */
      MouRemovePtr( &rect, s_uMouHandle );
   }
}

static void hb_gt_os2_mouse_GetPos( PHB_GT pGT, int * row, int * col )
{
   HB_SYMBOL_UNUSED( pGT );

   if( s_uMouHandle )
   {
      PTRLOC pos;
      MouGetPtrPos( &pos, s_uMouHandle );
      *row = ( int ) pos.row;
      *col = ( int ) pos.col;
   }
}

static void hb_gt_os2_mouse_SetPos( PHB_GT pGT, int row, int col )
{
   HB_SYMBOL_UNUSED( pGT );

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
   HB_BOOL fDown;
   int     iPressed;
   int     iPressRow;
   int     iPressCol;
   ULONG   ulPressTime;
   int     iReleased;
   int     iReleaseRow;
   int     iReleaseCol;
   ULONG   ulReleaseTime;
} HB_OS2_BUTTONSTATE;

static HB_OS2_BUTTONSTATE s_ButtonState[ 3 ];
static ULONG s_ButtonMask[ 3 ] =
{
   MOUSE_BN1_DOWN,
   MOUSE_BN2_DOWN,
   MOUSE_BN3_DOWN
};
static ULONG s_ulMouseLastState;

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
                  s_ButtonState[ i ].fDown = HB_TRUE;
                  s_ButtonState[ i ].iPressed++;
                  s_ButtonState[ i ].iPressRow = MouEvent.row;
                  s_ButtonState[ i ].iPressCol = MouEvent.col;
                  s_ButtonState[ i ].ulPressTime = MouEvent.time;
               }
               else
               {
                  s_ButtonState[ i ].fDown = HB_FALSE;
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

static HB_BOOL hb_gt_os2_mouse_ButtonState( PHB_GT pGT, int iButton )
{
   HB_SYMBOL_UNUSED( pGT );

   hb_gt_os2_mouse_ReadMouseState();

   if( s_uMouHandle && iButton >= 0 && iButton < 3 )
      return s_ButtonState[ iButton ].fDown;
   else
      return HB_FALSE;
}

static HB_BOOL hb_gt_os2_mouse_ButtonPressed( PHB_GT pGT, int iButton, int * piRow, int * piCol )
{
   HB_SYMBOL_UNUSED( pGT );

   hb_gt_os2_mouse_ReadMouseState();

   if( s_uMouHandle && iButton >= 0 && iButton < 3 )
   {
      if( s_ButtonState[ iButton ].iPressed )
      {
         s_ButtonState[ iButton ].iPressed = 0;
         *piRow = s_ButtonState[ iButton ].iPressRow;
         *piCol = s_ButtonState[ iButton ].iPressCol;
         return HB_TRUE;
      }
   }

   return HB_FALSE;
}

static HB_BOOL hb_gt_os2_mouse_ButtonReleased( PHB_GT pGT, int iButton, int * piRow, int * piCol )
{
   HB_SYMBOL_UNUSED( pGT );

   hb_gt_os2_mouse_ReadMouseState();

   if( s_uMouHandle && iButton >= 0 && iButton < 3 )
   {
      if( s_ButtonState[ iButton ].iReleased )
      {
         s_ButtonState[ iButton ].iReleased = 0;
         *piRow = s_ButtonState[ iButton ].iReleaseRow;
         *piCol = s_ButtonState[ iButton ].iReleaseCol;
         return HB_TRUE;
      }
   }

   return HB_FALSE;
}

static int hb_gt_os2_mouse_CountButton( PHB_GT pGT )
{
   USHORT usButtons = 0;

   HB_SYMBOL_UNUSED( pGT );

   if( s_uMouHandle )
      MouGetNumButtons( &usButtons, s_uMouHandle );

   return ( int ) usButtons;
}

static void hb_gt_os2_GetCursorPosition( int * piRow, int * piCol )
{
   USHORT y, x;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_os2_GetCursorPosition(%p, %p)", piRow, piCol ) );

   VioGetCurPos( &y, &x, 0 );

   *piRow = y;
   *piCol = x;
}

static void hb_gt_os2_SetCursorPosition( int iRow, int iCol )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_os2_SetCursorPosition(%d, %d)", iRow, iCol ) );

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

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_os2_SetCursorSize(%d, %d, %d)", ( int ) start, ( int ) end, visible ) );

   vi.yStart = start;
   vi.cEnd = end;
   vi.cx = 0;
   vi.attr = ( visible ? 0 : -1 );
   VioSetCurType( &vi, 0 );
}

static unsigned char hb_gt_os2_GetCharHeight()
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_os2_GetCharHeight()" ) );

   return ( unsigned char ) ( s_vi.row ? ( s_vi.vres / s_vi.row ) - 1 : 0 );
}

static int hb_gt_os2_GetCursorStyle( void )
{
   int iStyle;
   VIOCURSORINFO vi;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_os2_GetCursorStyle()" ) );

   VioGetCurType( &vi, 0 );

   if( vi.attr )
      iStyle = SC_NONE;
   else
   {
      unsigned char charheight = hb_gt_os2_GetCharHeight();

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
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_os2_SetCursorStyle(%d)", iStyle ) );

   if( iStyle != s_iCursorStyle )
   {
      unsigned char charheight = hb_gt_os2_GetCharHeight();

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

static void hb_gt_os2_GetScreenContents( PHB_GT pGT )
{
   PHB_CODEPAGE cdp;
   int iRow, iCol;
   char * pBufPtr;
   HB_BYTE bxAttr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_os2_GetScreenContents(%p)", pGT ) );

   bxAttr = 0;
   cdp = HB_GTSELF_CPTERM( pGT );
   if( ! cdp )
   {
      cdp = HB_GTSELF_CPBOX( pGT );
      if( cdp )
         bxAttr = HB_GT_ATTR_BOX;
      else
         cdp = HB_GTSELF_HOSTCP( pGT );
   }

   for( iRow = 0; iRow < s_vi.row; ++iRow )
   {
      USHORT usSize = s_vi.col << 1;
      VioReadCellStr( ( PCH ) s_sLineBuf, &usSize, iRow, 0, 0 );

      for( iCol = 0, pBufPtr = s_sLineBuf; iCol < s_vi.col; ++iCol, pBufPtr += 2 )
      {
         HB_USHORT usChar = hb_cdpGetU16( cdp, pBufPtr[ 0 ] );
         HB_GTSELF_PUTSCRCHAR( pGT, iRow, iCol, pBufPtr[ 1 ], bxAttr, usChar );
      }
   }
   HB_GTSELF_COLDAREA( pGT, 0, 0, s_vi.row, s_vi.col );
}

static PVOID hb_gt_os2_allocMem( int iSize )
{
   APIRET rc;     /* return code from DosXXX api call */
   PVOID pMem;

   rc = DosAllocMem( &pMem, iSize, PAG_COMMIT | OBJ_TILE | PAG_WRITE );
   if( rc != NO_ERROR )
      hb_errInternal( HB_EI_XGRABALLOC, "hb_gt_os2_allocMem() memory allocation failure.", NULL, NULL );

   return pMem;
}

static void hb_gt_os2_Init( PHB_GT pGT, HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout, HB_FHANDLE hFilenoStderr )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_os2_Init(%p,%p,%p,%p)", pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr ) );

   s_vi.cb = sizeof( VIOMODEINFO );
   VioGetMode( &s_vi, 0 );        /* fill structure with current video mode settings */

   /* Alloc tileable memory for calling a 16 subsystem */
   s_hk = ( PHKBD ) hb_gt_os2_allocMem( sizeof( HKBD ) );
   /* it is a long after all, so I set it to zero only one time since it never changes */
   memset( s_hk, 0, sizeof( HKBD ) );
   s_key = ( PKBDKEYINFO ) hb_gt_os2_allocMem( sizeof( KBDKEYINFO ) );

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

   /* 2001.08.21 - <maurilio.longo@libero.it>
      NOTE: Box drawing characters need page 437 to show correctly, so, in your
            config.sys you need to have a CODEPAGE=x,y statement where x or y
            is equal to 437
    */

   VioGetCp( 0, &s_usOldCodePage, 0 );

   /* If I could not set codepage 437 I reset previous codepage,
      maybe I do not need to do this */
   if( VioSetCp( 0, 437, 0 ) != NO_ERROR )
      VioSetCp( 0, s_usOldCodePage, 0 );

   hb_gt_os2_GetCursorPosition( &s_iCurRow, &s_iCurCol );
   s_iCursorStyle = hb_gt_os2_GetCursorStyle();

   HB_GTSUPER_INIT( pGT, hFilenoStdin, hFilenoStdout, hFilenoStderr );
   HB_GTSELF_RESIZE( pGT, s_vi.row, s_vi.col );
   HB_GTSELF_SETPOS( pGT, s_iCurRow, s_iCurCol );
   if( s_iCursorStyle > 0 )
      HB_GTSELF_SETCURSORSTYLE( pGT, s_iCursorStyle );
   hb_gt_os2_GetScreenContents( pGT );
}

static void hb_gt_os2_Exit( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_os2_Exit(%p)", pGT ) );

   HB_GTSUPER_EXIT( pGT );

   if( s_iLineBufSize > 0 )
   {
      DosFreeMem( ( PVOID ) s_sLineBuf );
      s_iLineBufSize = 0;
   }

   DosFreeMem( s_key );
   DosFreeMem( s_hk );
   VioSetCp( 0, s_usOldCodePage, 0 );
}

static int hb_gt_os2_ReadKey( PHB_GT pGT, int iEventMask )
{
   int ch;              /* next char if any */

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_os2_ReadKey(%p,%d)", pGT, iEventMask ) );

   /* zero out keyboard event record */
   memset( s_key, 0, sizeof( KBDKEYINFO ) );

   /* Get next character without wait */
   KbdCharIn( s_key, IO_NOWAIT, ( HKBD ) *s_hk );

   /* extended key codes have 00h or E0h as chChar */
   if( ( s_key->fbStatus & KBDTRF_EXTENDED_CODE ) &&
       ( s_key->chChar == 0x00 || s_key->chChar == 0xE0 ) )
   {
      /* It was an extended function key lead-in code, so read the actual function key and then offset it by 256,
         unless extended keyboard events are allowed, in which case offset it by 512 */
      if( ( s_key->chChar == 0xE0 ) && ( iEventMask & HB_INKEY_RAW ) )
         ch = ( int ) s_key->chScan + 512;
      else
         ch = ( int ) s_key->chScan + 256;
   }
   else if( s_key->fbStatus & KBDTRF_FINAL_CHAR_IN )
      ch = ( int ) s_key->chChar;
   else
      ch = 0;

   ch = hb_gt_dos_keyCodeTranslate( ch );

   if( ch == 0 )
      ch = HB_GTSELF_MOUSEREADKEY( pGT, iEventMask );
   else
   {
      int u = HB_GTSELF_KEYTRANS( pGT, ch );
      if( u )
         ch = HB_INKEY_NEW_UNICODE( u );
   }

   return ch;
}

static HB_BOOL hb_gt_os2_IsColor( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_os2_IsColor(%p)", pGT ) );

   HB_SYMBOL_UNUSED( pGT );

   return s_vi.fbType != 0;        /* 0 = monochrom-compatible mode */
}

static HB_BOOL hb_gt_os2_GetBlink( PHB_GT pGT )
{
   VIOINTENSITY vi;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_os2_GetBlink(%p)", pGT ) );

   HB_SYMBOL_UNUSED( pGT );

   vi.cb   = sizeof( VIOINTENSITY );    /* 6                          */
   vi.type = 2;                         /* get intensity/blink toggle */
   VioGetState( &vi, 0 );

   return vi.fs == 0;                   /* 0 = blink, 1 = intens      */
}

static void hb_gt_os2_SetBlink( PHB_GT pGT, HB_BOOL fBlink )
{
   VIOINTENSITY vi;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_os2_SetBlink(%p,%d)", pGT, ( int ) fBlink ) );

   HB_SYMBOL_UNUSED( pGT );

   vi.cb   = sizeof( VIOINTENSITY );    /* 6                          */
   vi.type = 2;                         /* set intensity/blink toggle */
   vi.fs   = ( fBlink ? 0 : 1 );        /* 0 = blink, 1 = intens      */
   VioSetState( &vi, 0 );
}

static void hb_gt_os2_Tone( PHB_GT pGT, double dFrequency, double dDuration )
{
   ULONG ulDuration;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_os2_Tone(%p,%lf,%lf)", pGT, dFrequency, dDuration ) );

   HB_SYMBOL_UNUSED( pGT );

   /* The conversion from Clipper timer tick units to
      milliseconds is * 1000.0 / 18.2. */

   if( dFrequency < 0.0 )
      dFrequency = 0.0;
   else if( dFrequency > 32767.0 )
      dFrequency = 32767.0;
   ulDuration = ( ULONG ) ( dDuration * 1000.0 / 18.2 ); /* milliseconds */

   while( ulDuration > 0 )
   {
      USHORT temp = ( USHORT ) HB_MIN( ulDuration, USHRT_MAX );
      ulDuration -= temp;
      DosBeep( ( USHORT ) dFrequency, temp );
   }
}

static const char * hb_gt_os2_Version( PHB_GT pGT, int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_os2_Version(%p,%d)", pGT, iType ) );

   HB_SYMBOL_UNUSED( pGT );

   if( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Harbour Terminal: OS/2 console";
}

static HB_BOOL hb_gt_os2_Resize( PHB_GT pGT, int iRows, int iCols )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_os2_Resize(%p,%d,%d)", pGT, iRows, iCols ) );

   if( HB_GTSUPER_RESIZE( pGT, iRows, iCols ) )
   {
      HB_GTSELF_GETSIZE( pGT, &iRows, &iCols );
      iRows <<= 1;
      if( s_iLineBufSize != iRows )
      {
         if( s_iLineBufSize != 0 )
            DosFreeMem( ( PVOID ) s_sLineBuf );
         if( iRows )
            s_sLineBuf = ( char * ) hb_gt_os2_allocMem( iRows );
         s_iLineBufSize = iRows;
      }
      return HB_TRUE;
   }
   return HB_FALSE;
}

static HB_BOOL hb_gt_os2_SetMode( PHB_GT pGT, int iRows, int iCols )
{
   HB_BOOL fResult;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_os2_SetMode(%p,%d,%d)", pGT, iRows, iCols ) );

   s_vi.cb = sizeof( VIOMODEINFO );
   VioGetMode( &s_vi, 0 );    /* fill structure with current settings */
   s_vi.row = iRows;
   s_vi.col = iCols;
   fResult = VioSetMode( &s_vi, 0 ) == 0; /* 0 = Ok, other = Fail */

   if( ! fResult )
   {
      s_vi.cb = sizeof( VIOMODEINFO );
      VioGetMode( &s_vi, 0 );    /* fill structure with current settings */
   }

   hb_gt_os2_GetCursorPosition( &s_iCurRow, &s_iCurCol );
   s_iCursorStyle = hb_gt_os2_GetCursorStyle();
   HB_GTSELF_RESIZE( pGT, s_vi.row, s_vi.col );
   HB_GTSELF_SETPOS( pGT, s_iCurRow, s_iCurCol );
   if( s_iCursorStyle > 0 )
      HB_GTSELF_SETCURSORSTYLE( pGT, s_iCursorStyle );
   hb_gt_os2_GetScreenContents( pGT );

   return fResult;
}

static HB_BOOL hb_gt_os2_PostExt( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_os2_PostExt(%p)", pGT ) );

   hb_gt_os2_GetCursorPosition( &s_iCurRow, &s_iCurCol );
   HB_GTSELF_SETPOS( pGT, s_iCurRow, s_iCurCol );
   hb_gt_os2_GetScreenContents( pGT );

   return HB_GTSUPER_POSTEXT( pGT );
}

static HB_BOOL hb_gt_os2_Resume( PHB_GT pGT )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_os2_Resume(%p)", pGT ) );

   s_vi.cb = sizeof( VIOMODEINFO );
   VioGetMode( &s_vi, 0 );    /* fill structure with current settings */
   hb_gt_os2_GetCursorPosition( &s_iCurRow, &s_iCurCol );
   s_iCursorStyle = hb_gt_os2_GetCursorStyle();
   HB_GTSELF_RESIZE( pGT, s_vi.row, s_vi.col );
   HB_GTSELF_SETPOS( pGT, s_iCurRow, s_iCurCol );
   if( s_iCursorStyle > 0 )
      HB_GTSELF_SETCURSORSTYLE( pGT, s_iCursorStyle );
   hb_gt_os2_GetScreenContents( pGT );

   return HB_GTSUPER_RESUME( pGT );
}

static void hb_gt_os2_Redraw( PHB_GT pGT, int iRow, int iCol, int iSize )
{
   char * pBufPtr = s_sLineBuf;
   int iColor;
   HB_BYTE bAttr;
   HB_UCHAR uc;
   int iLen = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_os2_Redraw(%p,%d,%d,%d)", pGT, iRow, iCol, iSize ) );

   while( iLen < iSize )
   {
      if( ! HB_GTSELF_GETSCRUC( pGT, iRow, iCol + iLen, &iColor, &bAttr, &uc, HB_TRUE ) )
         break;

      *pBufPtr++ = ( char ) uc;
      *pBufPtr++ = ( char ) iColor;
      ++iLen;
   }

   VioWrtCellStr( ( PCH ) s_sLineBuf, iLen << 1, iRow, iCol, 0 );
}

static void hb_gt_os2_Refresh( PHB_GT pGT )
{
   int iRow, iCol, iStyle;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_os2_Refresh(%p)", pGT ) );

   HB_GTSUPER_REFRESH( pGT );

   HB_GTSELF_GETSCRCURSOR( pGT, &iRow, &iCol, &iStyle );
   if( iStyle != SC_NONE )
   {
      if( iRow >= 0 && iCol >= 0 && iRow < s_vi.row && iCol < s_vi.col )
         hb_gt_os2_SetCursorPosition( iRow, iCol );
      else
         iStyle = SC_NONE;
   }
   hb_gt_os2_SetCursorStyle( iStyle );
}

static HB_BOOL hb_gt_os2_Info( PHB_GT pGT, int iType, PHB_GT_INFO pInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_os2_Info(%p,%d,%p)", pGT, iType, pInfo ) );

   switch( iType )
   {
      case HB_GTI_ISSCREENPOS:
      case HB_GTI_KBDSUPPORT:
         pInfo->pResult = hb_itemPutL( pInfo->pResult, HB_TRUE );
         break;

      default:
         return HB_GTSUPER_INFO( pGT, iType, pInfo );
   }

   return HB_TRUE;
}


/* *********************************************************************** */

static HB_BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_FuncInit(%p)", pFuncTable ) );

   pFuncTable->Init                       = hb_gt_os2_Init;
   pFuncTable->Exit                       = hb_gt_os2_Exit;
   pFuncTable->IsColor                    = hb_gt_os2_IsColor;
   pFuncTable->Resize                     = hb_gt_os2_Resize;
   pFuncTable->SetMode                    = hb_gt_os2_SetMode;
   pFuncTable->Redraw                     = hb_gt_os2_Redraw;
   pFuncTable->Refresh                    = hb_gt_os2_Refresh;
   pFuncTable->SetBlink                   = hb_gt_os2_SetBlink;
   pFuncTable->GetBlink                   = hb_gt_os2_GetBlink;
   pFuncTable->Version                    = hb_gt_os2_Version;
   pFuncTable->Resume                     = hb_gt_os2_Resume;
   pFuncTable->PostExt                    = hb_gt_os2_PostExt;
   pFuncTable->Tone                       = hb_gt_os2_Tone;
   pFuncTable->Info                       = hb_gt_os2_Info;

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

   return HB_TRUE;
}

/* *********************************************************************** */

#include "hbgtreg.h"

/* *********************************************************************** */
