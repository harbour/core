/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem for DOS compilers
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
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 *    hb_gt_CtrlBrkHandler()
 *    hb_gt_CtrlBrkRestore()
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_gt_ReadKey()
 *
 * See doc/license.txt for licensing terms.
 *
 */

/*
 *  This module is based on VIDMGR by Andrew Clarke and modified for
 *  the Harbour project
 */

/* NOTE: User programs should never call this layer directly! */

#include "hbapigt.h"
#include "hbset.h" /* For Ctrl+Break handling */
#include "hbvm.h" /* For Ctrl+Break handling */
#include "inkey.ch"

#include <string.h>
#include <time.h>
#include <conio.h>

#if defined(__DJGPP__)
   #include <pc.h>
   #include <sys\exceptn.h>
   #include <sys\farptr.h>
#elif defined(_MSC_VER)
   #include <signal.h>
#endif

/* For screen support */
#if defined(__POWERC) || (defined(__TURBOC__) && !defined(__BORLANDC__)) || (defined(__ZTC__) && !defined(__SC__))
   #define FAR far
#elif defined(HB_OS_DOS) && !defined(__DJGPP__) && !defined(__RSX32__) && !defined(__WATCOMC__)
   #define FAR _far
#else
   #define FAR
#endif

#if !defined(__DJGPP__)
   #ifndef MK_FP
      #define MK_FP( seg, off ) \
         ((void FAR *)(((unsigned long)(seg) << 16)|(unsigned)(off)))
   #endif
#endif

static void hb_gt_xGetXY( USHORT cRow, USHORT cCol, BYTE * attr, BYTE * ch );
static void hb_gt_xPutch( USHORT cRow, USHORT cCol, BYTE attr, BYTE ch );

static char hb_gt_GetScreenMode( void );
static void hb_gt_SetCursorSize( char start, char end );
static void hb_gt_GetCursorSize( char * start, char * end );

#if defined(__WATCOMC__)
   #if defined(__386__)
      #define FAR
   #endif
   #include <signal.h>
#endif
#if !defined(__DJGPP__)
   static char FAR * scrnPtr;
   static char FAR * scrnStealth = NULL;
   static char FAR * hb_gt_ScreenAddress( void );
#endif

static BOOL s_bBreak; /* Used to signal Ctrl+Break to hb_inkeyPoll() */
static USHORT s_uiDispCount;

#if defined(__RSX32__)

static int kbhit( void )
{
   union REGS regs;

   regs.h.ah = 0x0B;
   HB_DOS_INT86( 0x21, &regs, &regs );

   return regs.HB_XREGS.ax;
}

#endif

#if !defined(__DJGPP__) && !defined(__RSX32__)
#if defined(__WATCOMC__) || defined(_MSC_VER)
static void hb_gt_CtrlBreak_Handler( int iSignal )
{
   /* Ctrl-Break was pressed */
   /* NOTE: the layout of this function is forced by the compiler
    */
   HB_SYMBOL_UNUSED( iSignal );
   s_bBreak = TRUE;
}
#else
static int s_iOldCtrlBreak = 0;

static int hb_gt_CtrlBrkHandler( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_CtrlBrkHandler()"));
   s_bBreak = TRUE;
   return 1;
}
#endif

static void hb_gt_CtrlBrkRestore( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_CtrlBrkRestore()"));
   #if defined(__WATCOMC__)
      signal( SIGBREAK, SIG_DFL );
   #elif defined(_MSC_VER)
      signal( SIGINT, SIG_DFL );
   #elif !defined(__RSX32__)
      setcbrk( s_iOldCtrlBreak );
   #endif
}
#endif

void hb_gt_Init( int iFilenoStdin, int iFilenoStdout, int iFilenoStderr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Init()"));

   HB_SYMBOL_UNUSED( iFilenoStdin );
   HB_SYMBOL_UNUSED( iFilenoStdout );
   HB_SYMBOL_UNUSED( iFilenoStderr );

   s_bBreak = FALSE;
   s_uiDispCount = 0;

   /* Set the Ctrl+Break handler [vszakats] */

#if defined(__DJGPP__)

   gppconio_init();
   __djgpp_hwint_flags |= 2;     /* Count Ctrl+Break instead of killing program */
   __djgpp_set_ctrl_c( 0 );      /* Disable Ctrl+C */
   __djgpp_set_sigquit_key( 0 ); /* Disable Ctrl+\ */

#elif defined(__WATCOMC__)

   signal( SIGBREAK, hb_gt_CtrlBreak_Handler );
   atexit( hb_gt_CtrlBrkRestore );

#elif defined(_MSC_VER)

   signal( SIGINT, hb_gt_CtrlBreak_Handler );
   atexit( hb_gt_CtrlBrkRestore );

#elif defined(__RSX32__)

   /* TODO */

#else

   ctrlbrk( hb_gt_CtrlBrkHandler );
   s_iOldCtrlBreak = getcbrk();
   setcbrk( 1 );
   atexit( hb_gt_CtrlBrkRestore );

#endif

   /* */

#if !defined(__DJGPP__)
   scrnStealth = ( char * ) -1;
   scrnPtr = hb_gt_ScreenAddress();
#endif
}

void hb_gt_Exit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Exit()"));

#if !defined(__DJGPP__)
  if( scrnStealth != ( char * ) -1 )
     hb_xfree( scrnStealth );
#endif
}

int hb_gt_ReadKey( HB_inkey_enum eventmask )
{
   int ch = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ReadKey(%d)", (int) eventmask));

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
   if( s_bBreak )
   {
      s_bBreak = FALSE; /* Indicate that Ctrl+Break has been handled */
      ch = HB_BREAK_FLAG; /* Note that Ctrl+Break was pressed */
   }
#endif
   else if( kbhit() )
   {
      /* A key code is available in the BIOS keyboard buffer, so read it */
#if defined(__DJGPP__)
      if( eventmask & INKEY_RAW ) ch = getxkey();
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
         if( eventmask & INKEY_RAW ) ch = getch() + 512;
         else ch = getch() + 256;
      }
#endif
   }

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

BOOL hb_gt_AdjustPos( BYTE * pStr, ULONG ulLen )
{
   union REGS regs;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_AdjustPos(%s, %lu)", pStr, ulLen ));

   HB_SYMBOL_UNUSED( pStr );
   HB_SYMBOL_UNUSED( ulLen );

   regs.h.ah = 0x03;
   regs.h.bh = 0;
   HB_DOS_INT86( 0x10, &regs, &regs );

   hb_gtSetPos( regs.h.dh, regs.h.dl );

   return TRUE;
}

BOOL hb_gt_IsColor( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_IsColor()"));

   return hb_gt_GetScreenMode() != 7;
}

#if !defined(__DJGPP__)
static char FAR * hb_gt_ScreenAddress()
{
   char FAR * ptr;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ScreenAddress()"));

   #if defined(__WATCOMC__) && defined(__386__)
      if( hb_gt_IsColor() )
         ptr = ( char * ) ( 0xB800 << 4 );
      else
         ptr = ( char * )( 0xB000 << 4 );
   #else
      if( hb_gt_IsColor() )
         ptr = ( char FAR * ) MK_FP( 0xB800, 0x0000 );
      else
         ptr = ( char FAR * ) MK_FP( 0xB000, 0x0000 );
   #endif

   return ptr;
}
#endif

#if !defined(__DJGPP__)
char FAR * hb_gt_ScreenPtr( USHORT cRow, USHORT cCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_ScreenPtr(%hu, %hu)", cRow, cCol));

   return scrnPtr + ( cRow * hb_gt_GetScreenWidth() * 2 ) + ( cCol * 2 );
}
#endif

static char hb_gt_GetScreenMode( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenMode()"));

#if defined(__WATCOMC__) && defined(__386__)
   return *( ( char * ) 0x0449 );
#elif defined(__DJGPP__)
   return _farpeekb( 0x0040, 0x0049 );
#else
   return *( ( char FAR * ) MK_FP( 0x0040, 0x0049 ) );
#endif
}

USHORT hb_gt_GetScreenWidth( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenWidth()"));

#if defined(__WATCOMC__) && defined(__386__)
   return ( USHORT ) *( ( char * ) 0x044A );
#elif defined(__DJGPP__)
   return ( USHORT ) _farpeekb( 0x0040, 0x004A );
#else
   return ( USHORT ) *( ( char FAR * ) MK_FP( 0x0040, 0x004A ) );
#endif
}

USHORT hb_gt_GetScreenHeight( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetScreenHeigth()"));

#if defined(__WATCOMC__) && defined(__386__)
   return ( USHORT ) ( char ) ( *( ( char * ) 0x0484 ) + 1 );
#elif defined(__DJGPP__)
   return ( USHORT ) _farpeekb( 0x0040, 0x0084 ) + 1;
#else
   return ( USHORT ) ( ( char ) ( *( ( char FAR * ) MK_FP( 0x0040, 0x0084 ) ) + 1 ) );
#endif
}

void hb_gt_SetPos( SHORT iRow, SHORT iCol )
{
   union REGS regs;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetPos(%hd, %hd)", iRow, iCol));

   regs.h.ah = 0x02;
   regs.h.bh = 0;
   regs.h.dh = ( BYTE ) iRow;
   regs.h.dl = ( BYTE ) iCol;
   HB_DOS_INT86( 0x10, &regs, &regs );
}

static void hb_gt_SetCursorSize( char start, char end )
{
   union REGS regs;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorSize(%d, %d)", (int) start, (int) end));

   regs.h.ah = 0x01;
   regs.h.ch = start;
   regs.h.cl = end;
   HB_DOS_INT86( 0x10, &regs, &regs );
}

static void hb_gt_GetCursorSize( char * start, char *end )
{
   union REGS regs;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorSize(%p, %p)", start, end));

   regs.h.ah = 0x03;
   regs.h.bh = 0;
   HB_DOS_INT86( 0x10, &regs, &regs );
   *start = regs.h.ch;
   *end = regs.h.cl;
}

USHORT hb_gt_GetCursorStyle( void )
{
   char start, end;
   int rc;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetCursorStyle()"));

   hb_gt_GetCursorSize( &start, &end );

   if( ( start == 32 ) && ( end == 32 ) )
      rc = SC_NONE;

   else if( ( start == 6 ) && ( end == 7 ) )
      rc = SC_NORMAL;

   else if( ( start == 4 ) && ( end == 7 ) )
      rc = SC_INSERT;

   else if( ( start == 0 ) && ( end == 7 ) )
      rc = SC_SPECIAL1;

   else if( ( start == 0 ) && ( end == 3 ) )
      rc = SC_SPECIAL2;

   else
      rc = SC_NONE;

   return rc;
}

void hb_gt_SetCursorStyle( USHORT style )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetCursorStyle(%hu)", style));

   switch( style )
   {
   case SC_NONE:
      hb_gt_SetCursorSize( 32, 32 );
      break;

   case SC_NORMAL:
      hb_gt_SetCursorSize( 6, 7 );
      break;

   case SC_INSERT:
      hb_gt_SetCursorSize( 4, 7 );
      break;

   case SC_SPECIAL1:
      hb_gt_SetCursorSize( 0, 7 );
      break;

   case SC_SPECIAL2:
      hb_gt_SetCursorSize( 0, 3 );
      break;

   default:
      break;
   }
}

static void hb_gt_xGetXY( USHORT cRow, USHORT cCol, BYTE * attr, BYTE * ch )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_xGetXY(%hu, %hu, %p, %p", cRow, cCol, ch, attr));

#if defined(__DJGPP__TEXT)
   {
     short ch_attr;
     gettext( cCol + 1, cRow + 1, cCol + 1, cRow + 1, &ch_attr );
     *ch = ch_attr >> 8;
     *attr = ch_attr & 0xFF;
   }
#elif defined(__DJGPP__)
   {
      ScreenGetChar( (int *)ch, (int *)attr, cCol, cRow );
   }
#else
   {
     char FAR *p;
     p = hb_gt_ScreenPtr( cRow, cCol );
     *ch = *p;
     *attr = *( p + 1 );
   }
#endif
}

static void hb_gt_xPutch( USHORT cRow, USHORT cCol, BYTE attr, BYTE ch )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_xPutch(%hu, %hu, %d, %d", cRow, cCol, (int) attr, (int) ch));

#if defined(__DJGPP__TEXT)
   {
     long ch_attr;
     ch_attr = ( ch << 8 ) | attr;
     puttext( cCol + 1, cRow + 1, cCol + 1, cRow + 1, &ch_attr );
   }
#elif defined(__DJGPP)
   {
      ScreenPutChar( ch, attr, cCol, cRow );
   }
#else
   {
     USHORT FAR * p = (USHORT FAR *) hb_gt_ScreenPtr( cRow, cCol );
     *p = (attr << 8) + ch;
   }
#endif
}

void hb_gt_Puts( USHORT cRow, USHORT cCol, BYTE attr, BYTE *str, ULONG len )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Puts(%hu, %hu, %d, %p, %lu", cRow, cCol, (int) attr, str, len));

#if defined(__DJGPP__TEXT)
   {
     int i;
     int bottom, left, right, top;
     int width;
     BYTE * ch_attr;
     BYTE * ptr;

     i = ( int ) len;
     left = cCol;
     top = cRow;
     width = hb_gt_GetScreenWidth();
     ptr = ch_attr = hb_xgrab( i * 2 );
     while( i-- )
       {
         *ptr++ = *str++;
         *ptr++ = attr;
       }
     i = len - 1; /* We want end position, not next cursor position */
     right = left;
     bottom = top;
     if( right + i > width - 1 )
       {
         /*
          * Calculate end row position and the remainder size for the
          * end column adjust.
          */
         bottom += ( i / width );
         i = i % width;
       }
     right += i;
     if( right > width - 1 )
       {
         /* Column movement overflows into next row */
         bottom++;
         right -= width;
       }
     puttext( left + 1, top + 1, right + 1, bottom + 1, ch_attr );
     hb_xfree( ch_attr );
   }
#elif defined(__DJGPP__)
   {
      int i;
      for( i=0; i<len; i++ )
         ScreenPutChar( str[ i ], attr, cCol++, cRow );
   }
#else
   {
      USHORT FAR *p;
      register USHORT byAttr = attr << 8;

      p = (USHORT FAR *) hb_gt_ScreenPtr( cRow, cCol );
      while( len-- )
      {
         *p++ = byAttr + (*str++);
      }
   }
#endif
}

int hb_gt_RectSize( USHORT rows, USHORT cols )
{
   return rows * cols * 2;
}

void hb_gt_GetText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE * dest )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetText(%hu, %hu, %hu, %hu, %p", usTop, usLeft, usBottom, usRight, dest));

#if defined(__DJGPP__TEXT) || defined(__DJGPP__)
   {
     gettext( usLeft + 1, usTop + 1, usRight + 1, usBottom + 1, dest );
   }
#else
   {
     USHORT x, y;

     for( y = usTop; y <= usBottom; y++ )
       {
         for( x = usLeft; x <= usRight; x++ )
           {
             hb_gt_xGetXY( y, x, dest + 1, dest );
             dest += 2;
           }
       }
   }
#endif
}

void hb_gt_PutText( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE * srce )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_PutText(%hu, %hu, %hu, %hu, %p", usTop, usLeft, usBottom, usRight, srce));

#if defined(__DJGPP__TEXT)
   {
     puttext( usLeft + 1, usTop + 1, usRight + 1, usBottom + 1, srce );
   }
#else
   {
     USHORT x, y;

     for( y = usTop; y <= usBottom; y++ )
       {
         for( x = usLeft; x <= usRight; x++ )
           {
             hb_gt_xPutch( y, x, *( srce + 1 ), *srce );
             srce += 2;
           }
       }
   }
#endif
}

void hb_gt_SetAttribute( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr )
{
   USHORT x, y;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetAttribute(%hu, %hu, %hu, %hu, %d", usTop, usLeft, usBottom, usRight, (int) attr));

   for( y = usTop; y <= usBottom; y++ )
   {
      BYTE scratchattr;
      BYTE ch;

      for( x = usLeft; x <= usRight; x++ )
      {
         hb_gt_xGetXY( y, x, &scratchattr, &ch );
         hb_gt_xPutch( y, x, attr, ch );
      }
   }
}

SHORT hb_gt_Col( void )
{
   union REGS regs;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Col()"));

   regs.h.ah = 0x03;
   regs.h.bh = 0;
   HB_DOS_INT86( 0x10, &regs, &regs );

   return regs.h.dl;
}

SHORT hb_gt_Row( void )
{
   union REGS regs;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Row()"));

   regs.h.ah = 0x03;
   regs.h.bh = 0;
   HB_DOS_INT86( 0x10, &regs, &regs );

   return regs.h.dh;
}

void hb_gt_Scroll( USHORT usTop, USHORT usLeft, USHORT usBottom, USHORT usRight, BYTE attr, SHORT sVert, SHORT sHoriz )
{
   int iRows = sVert, iCols = sHoriz;

   /* NOTE: 'SHORT' is used intentionally to correctly compile
   *  with C++ compilers
   */
   SHORT usRow, usCol;
   USHORT uiSize;   /* gtRectSize returns int */
   int iLength = ( usRight - usLeft ) + 1;
   int iCount, iColOld, iColNew, iColSize;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Scroll(%hu, %hu, %hu, %hu, %d, %hd, %hd)", usTop, usLeft, usBottom, usRight, (int) attr, sVert, sHoriz));

   hb_gtGetPos( &usRow, &usCol );

   if( hb_gtRectSize( usTop, usLeft, usBottom, usRight, &uiSize ) == 0 )
   {
      /* NOTE: 'unsigned' is used intentionally to correctly compile
       * with C++ compilers
       */
      unsigned char * fpBlank = ( unsigned char * ) hb_xgrab( iLength );
      unsigned char * fpBuff = ( unsigned char * ) hb_xgrab( iLength * 2 );

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

void hb_gt_DispBegin( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispBegin()"));

/* ptucker */
#if !defined(__DJGPP__)
   if( ++s_uiDispCount == 1 )
   {
      char FAR * ptr;
      ULONG nSize;

      nSize = hb_gt_GetScreenWidth() * hb_gt_GetScreenHeight() * 2;

      ptr = scrnPtr;
      if( ( scrnPtr = scrnStealth ) == ( char * ) -1 )
         scrnPtr = ( char FAR * ) hb_xgrab( nSize );
      scrnStealth = ptr;
      memcpy( ( void * ) scrnPtr, ( void * ) ptr, nSize );
   }
#endif
}

void hb_gt_DispEnd( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_DispEnd()"));

/* ptucker */
#if !defined(__DJGPP__)
   if( --s_uiDispCount == 0 )
   {
      char FAR * ptr;
      ULONG nSize;

      nSize = hb_gt_GetScreenWidth() * hb_gt_GetScreenHeight() * 2;

      ptr = scrnPtr;
      scrnPtr = scrnStealth;
      scrnStealth = ptr;
      memcpy( ( void * ) scrnPtr, ( void * )ptr, nSize );
   }
#endif
}

BOOL hb_gt_SetMode( USHORT usRows, USHORT usCols )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetMode(%hu, %hu)", usRows, usCols));

   /* TODO: Implement this function. */

   HB_SYMBOL_UNUSED( usRows );
   HB_SYMBOL_UNUSED( usCols );

   return 0;
}

BOOL hb_gt_GetBlink()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_GetBlink()"));

#if defined(__WATCOMC__) && defined(__386__)
   return *( ( char * ) 0x0465 ) & 0x10;
#elif defined(__DJGPP__)
   return _farpeekb( 0x0040, 0x0065 ) & 0x10;
#else
   return *( ( char FAR * ) MK_FP( 0x0040, 0x0065 ) ) &0x10;
#endif
}

void hb_gt_SetBlink( BOOL bBlink )
{
   union REGS regs;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_SetBlink(%d)", (int) bBlink));

   regs.h.ah = 0x10;
   regs.h.al = 0x03;
   regs.h.bh = 0;
   regs.h.bl = bBlink;
   HB_DOS_INT86( 0x10, &regs, &regs );
}

void hb_gt_Tone( double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Tone(%lf, %lf)", dFrequency, dDuration));

   /* The conversion from Clipper timer tick units to
      milliseconds is * 1000.0 / 18.2. */

   dFrequency = HB_MIN( HB_MAX( 0.0, dFrequency ), 32767.0 );
   dDuration = dDuration * CLOCKS_PER_SEC / 18.2 ; /* clocks */

#if defined(__BORLANDC__) || defined(__WATCOMC__)
   sound( ( unsigned ) dFrequency );
#elif defined(__DJGPP__)
   sound( ( int ) dFrequency );
#endif

   while( dDuration > 0.0 )
   {
      /* Use USHORT, because this variable gets added to clock()
         to form end_clock and we want to minimize overflow risk */
      USHORT temp = ( USHORT ) HB_MIN( HB_MAX( 0, dDuration ), USHRT_MAX );
      clock_t end_clock;

      dDuration -= temp;
      if( temp <= 0 )
      {
         /* Ensure that the loop gets terminated when
            only a fraction of the delay time remains. */
         dDuration = -1.0;
      }
      else
      {
         /* Note: delay() in <dos.h> for DJGPP does not work and
                  delay() in <dos.h> for BORLANDC is not multi-
                  tasking friendly. */
         end_clock = clock() + temp;
         while( clock() < end_clock )
            hb_releaseCPU();
      }
   }

#if defined(__BORLANDC__) || defined(__WATCOMC__)
   nosound();
#elif defined(__DJGPP__)
   sound( 0 );
#endif
}

char * hb_gt_Version( void )
{
   return "Harbour Terminal: DOS console";
}

USHORT hb_gt_DispCount()
{
   return s_uiDispCount;
}

void hb_gt_Replicate( USHORT uiRow, USHORT uiCol, BYTE byAttr, BYTE byChar, ULONG nLength )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_Replicate(%hu, %hu, %i, %i, %lu)", uiRow, uiCol, byAttr, byChar, nLength));

#if defined(__DJGPP__TEXT)
   {
     int i;
     int bottom, left, right, top;
     int width;
     BYTE * ch_attr;
     BYTE * ptr;

     i = ( int ) nLength;
     left = cCol;
     top = cRow;
     width = hb_gt_GetScreenWidth();
     ptr = ch_attr = hb_xgrab( i * 2 );
     while( i-- )
       {
         *ptr++ = byChar;
         *ptr++ = byAttr;
       }
     i = nLength - 1; /* We want end position, not next cursor position */
     right = left;
     bottom = top;
     if( right + i > width - 1 )
       {
         /*
          * Calculate end row position and the remainder size for the
          * end column adjust.
          */
         bottom += ( i / width );
         i = i % width;
       }
     right += i;
     if( right > width - 1 )
       {
         /* Column movement overflows into next row */
         bottom++;
         right -= width;
       }
     puttext( left + 1, top + 1, right + 1, bottom + 1, ch_attr );
     hb_xfree( ch_attr );
   }
#elif defined(__DJGPP__)
   {
      while( nLength-- )
         ScreenPutChar( byChar, byAttr, uiCol++, uiRow );
   }
#else
   {
      USHORT FAR *p;
      USHORT byte = (byAttr << 8) + byChar;

      p = (USHORT FAR *) hb_gt_ScreenPtr( uiRow, uiCol );
      while( nLength-- )
      {
         *p++ = byte;
      }
   }
#endif
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
