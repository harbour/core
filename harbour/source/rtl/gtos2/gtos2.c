/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem for OS/2 compilers
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_gt_ReadKey()
 *
 * Copyright 1999 Chen Kedem <niki@actcom.co.il>
 *    hb_gt_Tone()
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
#define INCL_DOSPROCESS
#define INCL_NOPMAPI

#include "hbapigt.h"
#include "inkey.ch"

/* convert 16:16 address to 0:32 */
#define SELTOFLAT(ptr) (void *)(((((ULONG)(ptr))>>19)<<16)|(0xFFFF&((ULONG)(ptr))))

#if defined(HARBOUR_GCC_OS2)
   /* 25/03/2000 - maurilio.longo@libero.it
   OS/2 GCC hasn't got ToolKit headers available */
   #include <stdlib.h>
#else
   #include <bsedos.h>
#endif
#include <conio.h>

static char hb_gt_GetCellSize( void );
static char * hb_gt_ScreenPtr( USHORT cRow, USHORT cCol );
static void hb_gt_xGetXY( USHORT cRow, USHORT cCol, BYTE * attr, BYTE * ch );
static void hb_gt_xPutch( USHORT cRow, USHORT cCol, BYTE attr, BYTE ch );

/*
static void hb_gt_GetCursorSize( char * start, char * end );
*/

/* how many nested BeginDisp() */
static USHORT s_uiDispCount;

/* pointer to offscreen video buffer */
static ULONG s_ulLVBptr;

/* length of video buffer */
static USHORT s_usLVBlength;


void hb_gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init()"));

   s_uiDispCount = 0;

   if(VioGetBuf(&s_ulLVBptr, &s_usLVBlength, 0) == NO_ERROR) {
      s_ulLVBptr = (ULONG) SELTOFLAT(s_ulLVBptr);
      VioShowBuf(0, s_usLVBlength, 0);
   } else {
      s_ulLVBptr = (ULONG) NULL;
   }

   /* TODO: Is anything else required to initialize the video subsystem?
            I (Maurilio Longo) think that we should set correct codepage

      COMMENT: The correct behaviour is to inherit the codepage that is
               active when the program is started, which automatically
               happens by not setting the codepage. If somebody wants to
               change the codepage, there should be a separate function
               to do that. (David G. Holm <dholm@jsd-llc.com>)
   */
}

void hb_gt_Exit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Exit()"));

   /* TODO: */
}

BOOL hb_gt_AdjustPos( BYTE * pStr, ULONG ulLen )
{
   USHORT x, y;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_AdjustPos(%s, %lu)", pStr, ulLen ));

   HB_SYMBOL_UNUSED( pStr );
   HB_SYMBOL_UNUSED( ulLen );

   VioGetCurPos( &y, &x, 0 );
   hb_gtSetPos( ( SHORT ) y, ( SHORT ) x );

   return TRUE;
}


int hb_gt_ReadKey( HB_inkey_enum eventmask )
{
   int ch;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey(%d)", (int) eventmask));

#if defined(HARBOUR_GCC_OS2)
   /* 25/03/2000 - maurilio.longo@libero.it
      kbhit() isn't available when using emx/gcc under OS/2,
      there is _read_kbd(), instead, which can be used to fetch characters from
      keyboard buffer.
   */
   ch = _read_kbd(0, 0, 0);   /* readkey without echoing, waiting or breaking */
   if(ch == 0) {
         ch = _read_kbd(0, 0, 0);
         if(ch != EOF) {
            ch += 256;
         }
   }

   /* TODO: Use KBDxxx subsystem to fetch keys */
#else

   if( kbhit() )
   {
      /* A key code is available in the BIOS keyboard buffer, so read it */
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
         if( eventmask & INKEY_RAW ) ch = getch() + 512;
         else ch = getch() + 256;
      }
   }
   else
      ch = 0;
#endif

   /* Perform key translations */
   switch( ch )
   {
      case -1:  /* No key available */
         return 0;
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

   return ch;
}


BOOL hb_gt_IsColor( void )
{
/* Chen Kedem <niki@actcom.co.il> */
   VIOMODEINFO vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_IsColor()"));

   vi.cb = sizeof( VIOMODEINFO );
   VioGetMode( &vi, 0 );
   return vi.fbType != 0;        /* 0 = monochrom-compatible mode */
}

USHORT hb_gt_GetScreenWidth( void )
{
   VIOMODEINFO vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth()"));

   vi.cb = sizeof( VIOMODEINFO );
   VioGetMode( &vi, 0 );
   return vi.col;
}

USHORT hb_gt_GetScreenHeight( void )
{
   VIOMODEINFO vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeight()"));

   vi.cb = sizeof( VIOMODEINFO );
   VioGetMode( &vi, 0 );
   return vi.row;
}

void hb_gt_SetPos( SHORT iRow, SHORT iCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd)", iRow, iCol));

   VioSetCurPos( ( USHORT ) iRow, ( USHORT ) iCol, 0 );
}

SHORT hb_gt_Row( void )
{
   USHORT x, y;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));

   VioGetCurPos( &y, &x, 0 );
   return ( SHORT ) y;
}

SHORT hb_gt_Col( void )
{
   USHORT x, y;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));

   VioGetCurPos( &y, &x, 0 );
   return ( SHORT ) x;
}


void hb_gt_Scroll( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr, SHORT sVert, SHORT sHoriz )
{
/* Chen Kedem <niki@actcom.co.il> */

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hd, %hd)", usTop, usLeft, usBottom, usRight, (int) attr, sVert, sHoriz));

   if(s_uiDispCount > 0)
   {
      int iRows = sVert, iCols = sHoriz;

      /* NOTE: 'SHORT' is used intentionally to correctly compile
       *  with C++ compilers
       */
      SHORT usRow, usCol;
      USHORT usSize;
      int iLength = ( usRight - usLeft ) + 1;
      int iCount, iColOld, iColNew, iColSize;

      hb_gtGetPos( &usRow, &usCol );

      if( hb_gtRectSize( usTop, usLeft, usBottom, usRight, &usSize ) == 0 )
      {
         /* NOTE: 'unsigned' is used intentionally to correctly compile
          * with C++ compilers
          */
         BYTE * fpBlank = ( BYTE * ) hb_xgrab( iLength );
         BYTE * fpBuff = ( BYTE * ) hb_xgrab( iLength * 2 );

         memset( fpBlank, ' ', iLength );

         iColOld = iColNew = usLeft;
         if( iCols >= 0 )
         {
            iColOld += iCols;
            iColSize = ( int ) ( usRight - usLeft );
            iColSize -= iCols;
         }
         else
         {
            iColNew -= iCols;
            iColSize = ( int ) ( usRight - usLeft );
            iColSize += iCols;
         }

         for( iCount = ( iRows >= 0 ? usTop : usBottom );
         ( iRows >= 0 ? iCount <= usBottom : iCount >= usTop );
         ( iRows >= 0 ? iCount++ : iCount-- ) )
         {
            int iRowPos = iCount + iRows;

            /* Blank the scroll region in the current row */
            hb_gt_Puts( iCount, usLeft, attr, fpBlank, iLength );

            if( ( iRows || iCols ) && iRowPos <= usBottom && iRowPos >= usTop )
            {
               /* Read the text to be scrolled into the current row */
               hb_gt_GetText( iRowPos, iColOld, iRowPos, iColOld + iColSize, fpBuff );

               /* Write the scrolled text to the current row */
               hb_gt_PutText( iCount, iColNew, iCount, iColNew + iColSize, fpBuff );
            }
         }

         hb_xfree( fpBlank );
         hb_xfree( fpBuff );
      }

      hb_gtSetPos( usRow, usCol );

   }
   else
   {
      BYTE bCell[ 2 ];                          /* character/attribute pair */

      bCell [ 0 ] = ' ';
      bCell [ 1 ] = attr;
      if( ( sVert | sHoriz ) == 0 )             /* both zero, clear region */
      VioScrollUp ( usTop, usLeft, usBottom, usRight, 0xFFFF, bCell, 0 );
      else
      {
         if( sVert > 0 )                        /* scroll up */
            VioScrollUp ( usTop, usLeft, usBottom, usRight, sVert, bCell, 0 );
         else if( sVert < 0 )                   /* scroll down */
            VioScrollDn ( usTop, usLeft, usBottom, usRight, -sVert, bCell, 0 );

         if( sHoriz > 0 )                       /* scroll left */
            VioScrollLf ( usTop, usLeft, usBottom, usRight, sHoriz, bCell, 0 );
         else if( sHoriz < 0 )                  /* scroll right */
            VioScrollRt ( usTop, usLeft, usBottom, usRight, -sHoriz, bCell, 0 );
      }
   }
}

/* QUESTION: not been used, do we need this function ? */
/* Answer: In the dos version, this gets called by hb_gt_GetCursorStyle()
   as that function is written below, we don't need this */

/*
static void hb_gt_GetCursorSize( char * start, char * end )
{
   VIOCURSORINFO vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorSize(%p, %p)", start, end));

   VioGetCurType( &vi, 0 );
   *start = vi.yStart;
   *end = vi.cEnd;
}
*/

static void hb_gt_SetCursorSize( char start, char end, int visible )
{
/* Chen Kedem <niki@actcom.co.il> */
   VIOCURSORINFO vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorSize(%d, %d, %d)", (int) start, (int) end, visible));

   vi.yStart = start;
   vi.cEnd = end;
   vi.cx = 0;
   vi.attr = ( visible ? 0 : -1 );
   VioSetCurType( &vi, 0 );
}

static char hb_gt_GetCellSize()
{
/* Chen Kedem <niki@actcom.co.il> */
   char rc ;
   VIOMODEINFO vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCellSize()"));

   vi.cb = sizeof( VIOMODEINFO );
   VioGetMode( &vi, 0 );
   rc = ( char )( vi.row ? ( vi.vres / vi.row ) - 1 : 0 );
   return rc;
}

USHORT hb_gt_GetCursorStyle( void )
{
/* Chen Kedem <niki@actcom.co.il> */
   int rc;
   char cellsize;
   VIOCURSORINFO vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorStyle()"));

   VioGetCurType( &vi, 0 );

   if( vi.attr )
      rc = SC_NONE;
   else
   {
      cellsize = hb_gt_GetCellSize();

      if( vi.yStart == 0 && vi.cEnd == 0 )
         rc = SC_NONE;

      else if( ( vi.yStart == cellsize - 1 || vi.yStart == cellsize - 2 ) && vi.cEnd == cellsize )
         rc = SC_NORMAL;

      else if( vi.yStart == cellsize / 2 && vi.cEnd == cellsize )
         rc = SC_INSERT;

      else if( vi.yStart == 0 && vi.cEnd == cellsize )
         rc = SC_SPECIAL1;

      else if( vi.yStart == 0 && vi.cEnd == cellsize / 2 )
         rc = SC_SPECIAL2;

      else
         rc = SC_NONE;
   }

   return rc;
}

void hb_gt_SetCursorStyle( USHORT style )
{
/* Chen Kedem <niki@actcom.co.il> */
   char cellsize;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorStyle(%hu)", style));

   cellsize = hb_gt_GetCellSize();
   switch( style )
   {
   case SC_NONE:
      hb_gt_SetCursorSize( 0, 0, 0 );
      break;

   case SC_NORMAL:
      hb_gt_SetCursorSize( cellsize - 1, cellsize, 1 );
      break;

   case SC_INSERT:
      hb_gt_SetCursorSize( cellsize / 2, cellsize, 1 );
      break;

   case SC_SPECIAL1:
      hb_gt_SetCursorSize( 0, cellsize, 1 );
      break;

   case SC_SPECIAL2:
      hb_gt_SetCursorSize( 0, cellsize / 2, 1 );
      break;

   default:
      break;
   }
}


static char * hb_gt_ScreenPtr( USHORT cRow, USHORT cCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ScreenPtr(%hu, %hu)", cRow, cCol));

   return (char *) (s_ulLVBptr + ( cRow * hb_gt_GetScreenWidth() * 2 ) + ( cCol * 2 ));
}


static void hb_gt_xGetXY( USHORT cRow, USHORT cCol, BYTE * attr, BYTE * ch )
{
   char * p;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_xGetXY(%hu, %hu, %p, %p", cRow, cCol, ch, attr));

   p = hb_gt_ScreenPtr( cRow, cCol );
   *ch = *p;
   *attr = *( p + 1 );
}


static void hb_gt_xPutch( USHORT cRow, USHORT cCol, BYTE attr, BYTE ch )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_xPutch(%hu, %hu, %d, %d", cRow, cCol, (int) attr, (int) ch));

   {
      USHORT FAR * p = (USHORT FAR *) hb_gt_ScreenPtr( cRow, cCol );
      *p = (attr << 8) + ch;
   }
}


void hb_gt_Puts( USHORT usRow, USHORT usCol, BYTE attr, BYTE * str, ULONG len )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu)", usRow, usCol, (int) attr, str, len));

   if(s_uiDispCount > 0) {
      USHORT FAR *p;
      register USHORT byAttr = attr << 8;

      p = (USHORT FAR *) hb_gt_ScreenPtr( usRow, usCol );
      while( len-- )
      {
         *p++ = byAttr + (*str++);
      }
   } else {
      VioWrtCharStrAtt( ( char * ) str, ( USHORT ) len, usRow, usCol, ( BYTE * ) &attr, 0 );
   }
}

int hb_gt_RectSize( USHORT rows, USHORT cols )
{
   return rows * cols * 2;
}

void hb_gt_GetText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE *dest )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetText(%hu, %hu, %hu, %hu, %p)", usTop, usLeft, usBottom, usRight, dest));

   if(s_uiDispCount > 0) {
      USHORT x, y;

      for( y = usTop; y <= usBottom; y++ ) {
         for( x = usLeft; x <= usRight; x++ ) {
            hb_gt_xGetXY( y, x, dest + 1, dest );
            dest += 2;
         }
      }
   } else {
      USHORT width, y;

      width = ( USHORT ) ( ( usRight - usLeft + 1 ) * 2 );
      for( y = usTop; y <= usBottom; y++ )
      {
         VioReadCellStr( dest, &width, y, usLeft, 0 );
         dest += width;
      }
   }
}

void hb_gt_PutText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE *srce )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p)", usTop, usLeft, usBottom, usRight, srce));

   if(s_uiDispCount > 0) {
      USHORT x, y;

      for( y = usTop; y <= usBottom; y++ ) {
         for( x = usLeft; x <= usRight; x++ ) {
            hb_gt_xPutch( y, x, *( srce + 1 ), *srce );
            srce += 2;
         }
      }
   } else {
      USHORT width, y;

      width = ( USHORT ) ( ( usRight - usLeft + 1 ) * 2 );
      for( y = usTop; y <= usBottom; y++ ) {
         VioWrtCellStr( srce, width, y, usLeft, 0 );
         srce += width;
      }
   }
}

void hb_gt_SetAttribute( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr )
{
/* Chen Kedem <niki@actcom.co.il> */
   /*
      TODO: work with DispBegin DispEnd
   */

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetAttribute(%hu, %hu, %hu, %hu, %d)", usTop, usLeft, usBottom, usRight, (int) attr));

   if(s_uiDispCount >0) {

      USHORT x, y;

      for( y = usTop; y <= usBottom; y++ ) {

         BYTE scratchattr;
         BYTE ch;

         for( x = usLeft; x <= usRight; x++ ) {
            hb_gt_xGetXY( y, x, &scratchattr, &ch );
            hb_gt_xPutch( y, x, attr, ch );
         }
      }
   } else {

      USHORT width, y;

      /*
         assume top level check that coordinate are all valid and fall
         within visible screen, else if width cannot be fit on current line
         it is going to warp to the next line
      */
      width = ( USHORT ) ( usRight - usLeft + 1 );
      for( y = usTop; y <= usBottom; y++ )
         VioWrtNAttr( &attr, width, y, usLeft, 0 );
   }
}

void hb_gt_DispBegin( void )
{
   /* 02/04/2000 - maurilio.longo@libero.it
      added support for DispBegin() and DispEnd() functions.
      OS/2 has an off screen buffer for every vio session. When a program calls DispBegin()
      every function dealing with screen writes/reads uses this buffer. DispEnd() resyncronizes
      off screen buffer with screen
   */

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispBegin()"));

   /* pointer to the only one available screen buffer is set on startup,
      we only need to keep track of nesting */
   ++s_uiDispCount;
}

void hb_gt_DispEnd( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispEnd()"));

   if (--s_uiDispCount == 0) {
      VioShowBuf(0, s_usLVBlength, 0);   /* refresh everything */
   }
}

BOOL hb_gt_SetMode( USHORT uiRows, USHORT uiCols )
{
   VIOMODEINFO vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", uiRows, uiCols));

   vi.cb = sizeof( VIOMODEINFO );
   VioGetMode( &vi, 0 );        /* fill structure with current settings */
   vi.row = uiRows;
   vi.col = uiCols;
   return ! ( BOOL ) VioSetMode( &vi, 0 );   /* 0 = Ok, other = Fail */
}

BOOL hb_gt_GetBlink()
{
/* Chen Kedem <niki@actcom.co.il> */
   VIOINTENSITY vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetBlink()"));

   vi.cb   = sizeof( VIOINTENSITY );    /* 6                          */
   vi.type = 2;                         /* get intensity/blink toggle */
   VioGetState( &vi, 0 );
   return ( vi.fs == 0 );               /* 0 = blink, 1 = intens      */
}

void hb_gt_SetBlink( BOOL bBlink )
{
/* Chen Kedem <niki@actcom.co.il> */
   VIOINTENSITY vi;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetBlink(%d)", (int) bBlink));

   vi.cb   = sizeof( VIOINTENSITY );    /* 6                          */
   vi.type = 2;                         /* set intensity/blink toggle */
   vi.fs   = ( bBlink ? 0 : 1 );        /* 0 = blink, 1 = intens      */
   VioSetState( &vi, 0 );
}

void hb_gt_Tone( double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Tone(%lf, %lf)", dFrequency, dDuration));

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

char * hb_gt_Version( void )
{
   return "Harbour Terminal: OS/2 console";
}

USHORT hb_gt_DispCount()
{
   return s_uiDispCount;
}

void hb_gt_Replicate( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE byChar, ULONG nLength )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Replicate(%hu, %hu, %i, %i, %lu)", uiRow, uiCol, byAttr, byChar, nLength));
   {
      USHORT FAR *p;
      USHORT byte = (byAttr << 8) + byChar;

      p = (USHORT FAR *) hb_gt_ScreenPtr( uiRow, uiCol );
      while( nLength-- )
      {
         *p++ = byte;
      }
   }
}

USHORT hb_gt_Box( USHORT uiTop, USHORT uiLeft, USHORT uiBottom, USHORT uiRight,
                  BYTE *szBox, BYTE byAttr )
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
