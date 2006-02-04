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
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    hb_gt_CtrlBrkHandler()
 *    hb_gt_CtrlBrkRestore()
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_gt_ReadKey()
 *
 * Copyright 2000 Alejandro de Garate <alex_degarate@hotmail.com>
 *    vmode12x40()
 *    vmode25x40()
 *    vmode28x40()
 *    vmode50x40()
 *    vmode12x80()
 *    vmode25x80()
 *    vmode28x80()
 *    vmode43x80()
 *    vmode50x80()
 *    hb_gt_SetMode()
 *    hb_gt_GetDisplay()
 *
 * See doc/license.txt for licensing terms.
 *
 */

/*
 *  This module is based on VIDMGR by Andrew Clarke and modified for
 *  the Harbour project
 */

/* NOTE: User programs should never call this layer directly! */

#define HB_GT_NAME	DOS

#include "hbgtcore.h"
#include "hbinit.h"
#include "hbapicdp.h"
#include "inkey.ch"

#include <string.h>
#include <time.h>
#include <conio.h>


/*
 * use mouse driver save/restore state functions,
 * add other compilers for which calling real mode
 * interrupts with memory pointer is implemented.
 */
#if defined(__DJGPP__)
   #define HB_MOUSE_SAVE
#endif


#if defined(__DJGPP__)
   #include <pc.h>
   #include <sys\exceptn.h>
   #include <sys\farptr.h>
   #include <dpmi.h>
#elif defined(_MSC_VER) || defined(__WATCOMC__)
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
   static unsigned char FAR * s_pScreenAddres;
#endif

static HB_GT_FUNCS SuperTable;
#define HB_GTSUPER (&SuperTable)

static int  s_iRows;
static int  s_iCols;
static int  s_iCurRow;
static int  s_iCurCol;
static int  s_iCursorStyle;
static int  s_iScreenMode;

static BOOL s_bBreak; /* Used to signal Ctrl+Break to hb_inkeyPoll() */

static BYTE s_charTransRev[ 256 ];
static BYTE s_charTrans[ 256 ];
static BYTE s_keyTrans[ 256 ];

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
static int hb_gt_dos_CtrlBrkHandler( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_CtrlBrkHandler()"));
   s_bBreak = TRUE;
   return 1;
}
#endif

static void hb_gt_dos_CtrlBrkRestore( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_CtrlBrkRestore()"));

#if defined(__WATCOMC__)
   signal( SIGBREAK, SIG_DFL );
#elif defined(_MSC_VER)
   signal( SIGINT, SIG_DFL );
#else
   setcbrk( s_iOldCtrlBreak );
#endif
}
#endif

static int hb_gt_dos_GetScreenMode( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_GetScreenMode()"));

#if defined(__WATCOMC__) && defined(__386__)
   return ( int ) *( ( unsigned char * ) 0x0449 );
#elif defined(__DJGPP__)
   return ( int ) _farpeekb( 0x0040, 0x0049 );
#else
   return ( int ) *( ( unsigned char FAR * ) MK_FP( 0x0040, 0x0049 ) );
#endif
}

static void hb_gt_dos_GetScreenSize( int * piRows, int * piCols )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_GetScreenSize(%p, %p)", piRows, piCols));

#if defined(__WATCOMC__) && defined(__386__)
   *piRows = ( int ) *( ( unsigned char * ) 0x0484 ) + 1;
   *piCols = ( int ) *( ( unsigned char * ) 0x044A );
#elif defined(__DJGPP__)
   *piRows = ( int ) _farpeekb( 0x0040, 0x0084 ) + 1;
   *piCols = ( int ) _farpeekb( 0x0040, 0x004A );
#else
   *piRows = ( int ) *( ( unsigned char FAR * ) MK_FP( 0x0040, 0x0084 ) ) + 1;
   *piCols = ( int ) *( ( unsigned char FAR * ) MK_FP( 0x0040, 0x004A ) );
#endif
}

#if !defined(__DJGPP__)
static char FAR * hb_gt_dos_ScreenAddress()
{
   char FAR * ptr;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_ScreenAddress()"));

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

BYTE FAR * hb_gt_dos_ScreenPtr( int iRow, int iCol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_ScreenPtr(%d, %d)", iRow, iCol));

   return s_pScreenAddres + ( ( ( iRow * s_iCols ) + iCol ) << 1 );
}
#endif

static void hb_gt_dos_GetScreenContents( void )
{
   int iRow, iCol;
   BYTE bAttr, bChar;
#if !defined(__DJGPP__)
   BYTE * pScreenPtr = s_pScreenAddres;
#endif

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_GetScreenContents()"));

   for( iRow = 0; iRow < s_iRows; ++iRow )
   {
      for( iCol = 0; iCol < s_iCols; ++iCol )
      {
#if defined(__DJGPP__TEXT)
         short ch_attr;
         gettext( iCol + 1, iRow + 1, iCol + 1, iRow + 1, &ch_attr );
         bChar = ch_attr & 0xFF;
         bAttr = ch_attr >> 8;
#elif defined(__DJGPP__)
         int iChar, iAttr;
         ScreenGetChar( &iChar, &iAttr, iCol, iRow );
         bAttr = iAttr;
         bChar = iChar;
#else
         bChar = *pScreenPtr;
         bAttr = *( pScreenPtr + 1 );
         pScreenPtr += 2;
#endif
         hb_gt_PutChar( iRow, iCol, bAttr, 0, s_charTransRev[ bChar ] );
      }
   }
}

static void hb_gt_dos_GetCursorPosition( int * piRow, int * piCol )
{
   union REGS regs;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_GetCursorPosition(%p, %p)", piRow, piCol));

   regs.h.ah = 0x03;
   regs.h.bh = 0;
   HB_DOS_INT86( 0x10, &regs, &regs );
   *piRow = regs.h.dh;
   *piCol = regs.h.dl;
}

static void hb_gt_dos_SetCursorPosition( int iRow, int iCol )
{
   union REGS regs;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_SetCursorPosition(%d, %d)", iRow, iCol));

   if( s_iCurRow != iRow || s_iCurCol != iCol )
   {
      regs.h.ah = 0x02;
      regs.h.bh = 0;
      regs.h.dh = ( BYTE ) iRow;
      regs.h.dl = ( BYTE ) iCol;
      HB_DOS_INT86( 0x10, &regs, &regs );
      s_iCurRow = iRow;
      s_iCurCol = iCol;
   }
}

static void hb_gt_dos_SetCursorSize( unsigned char start, unsigned char end )
{
   union REGS regs;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_SetCursorSize(%d, %d)", (int) start, (int) end));

   regs.h.ah = 0x01;
   regs.h.ch = start;
   regs.h.cl = end;
   HB_DOS_INT86( 0x10, &regs, &regs );
}

static void hb_gt_dos_GetCursorSize( unsigned char * start, unsigned char *end )
{
   union REGS regs;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_GetCursorSize(%p, %p)", start, end));

   regs.h.ah = 0x03;
   regs.h.bh = 0;
   HB_DOS_INT86( 0x10, &regs, &regs );
   *start = regs.h.ch;
   *end = regs.h.cl;
}

static int hb_gt_dos_GetCursorStyle( void )
{
   unsigned char start, end;
   int iStyle;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_GetCursorStyle()"));

   hb_gt_dos_GetCursorSize( &start, &end );

   if( start == 32 && end == 32 )
      iStyle = SC_NONE;

   else if( start == 6 && end == 7 )
      iStyle = SC_NORMAL;

   else if( start == 4 && end == 7 )
      iStyle = SC_INSERT;

   else if( start == 0 && end == 7 )
      iStyle = SC_SPECIAL1;

   else if( start == 0 && end == 3 )
      iStyle = SC_SPECIAL2;

   else
      iStyle = -1;

   return iStyle;
}

static void hb_gt_dos_SetCursorStyle( int iStyle )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_SetCursorStyle(%d)", iStyle));

   if( iStyle != s_iCursorStyle )
   {
      switch( iStyle )
      {
         case SC_NONE:
            hb_gt_dos_SetCursorSize( 32, 32 );
            break;

         case SC_NORMAL:
            hb_gt_dos_SetCursorSize( 6, 7 );
            break;

         case SC_INSERT:
            hb_gt_dos_SetCursorSize( 4, 7 );
            break;

         case SC_SPECIAL1:
            hb_gt_dos_SetCursorSize( 0, 7 );
            break;

         case SC_SPECIAL2:
            hb_gt_dos_SetCursorSize( 0, 3 );
            break;

         default:
            return;
      }
      s_iCursorStyle = iStyle;
   }
}

/* *********************************************************************** */

static BOOL s_fMousePresent = FALSE;      /* Is there a mouse ? */
static BOOL s_fMouseVisible = FALSE;      /* Is mouse cursor visible ? */
static int  s_iMouseButtons = 0;          /* Mouse buttons */
static int  s_iMouseInitCol = 0;          /* Init mouse pos */
static int  s_iMouseInitRow = 0;          /* Init mouse pos */
static BOOL s_fMouseBound;
static int  s_iMouseTop;
static int  s_iMouseLeft;
static int  s_iMouseBottom;
static int  s_iMouseRight;

#ifdef HB_MOUSE_SAVE
   static int  s_iMouseStorageSize = 0;      /* size of mouse storage buffer */
#endif

static void hb_gt_dos_mouse_Init( void )
{
   union REGS regs;

   regs.HB_XREGS.ax = 0;
   HB_DOS_INT86( 0x33, &regs, &regs );
   s_fMousePresent = regs.HB_XREGS.ax;
   if( s_fMousePresent )
   {
      s_iMouseButtons = regs.HB_XREGS.bx;
      if( s_iMouseButtons == 0 )
         s_iMouseButtons = 3;
      if( s_iMouseButtons == 0xffff )
         s_iMouseButtons = 2;

      s_iMouseInitCol = hb_mouse_Col();
      s_iMouseInitRow = hb_mouse_Row();
   }
}

static void hb_gt_dos_mouse_Exit( void )
{
   if( s_fMousePresent )
   {
      int iHeight, iWidth;

      hb_gt_GetSize( &iHeight, &iWidth );
      hb_mouse_SetPos( s_iMouseInitRow, s_iMouseInitCol );
      hb_mouse_SetBounds( 0, 0, iHeight - 1, iWidth - 1 );
      s_fMousePresent = FALSE;
   }
}

static BOOL hb_gt_dos_mouse_IsPresent( void )
{
   return s_fMousePresent;
}

static void hb_gt_dos_mouse_Show( void )
{
   if( s_fMousePresent )
   {
      union REGS regs;

      regs.HB_XREGS.ax = 1;
      HB_DOS_INT86( 0x33, &regs, &regs );

      s_fMouseVisible = TRUE;
   }
}

static void hb_gt_dos_mouse_Hide( void )
{
   if( s_fMousePresent )
   {
      union REGS regs;

      regs.HB_XREGS.ax = 2;
      HB_DOS_INT86( 0x33, &regs, &regs );

      s_fMouseVisible = FALSE;
   }
}

static void hb_gt_dos_mouse_GetPos( int * piRow, int * piCol )
{
   if( s_fMousePresent )
   {
      union REGS regs;

      regs.HB_XREGS.ax = 3;
      HB_DOS_INT86( 0x33, &regs, &regs );

      *piRow = regs.HB_XREGS.dx >> 3;
      *piCol = regs.HB_XREGS.cx >> 3;
   }
   else
      *piRow = *piCol = 0;
}

static void hb_gt_dos_mouse_SetPos( int iRow, int iCol )
{
   if( s_fMousePresent )
   {
      union REGS regs;

      regs.HB_XREGS.ax = 4;
      regs.HB_XREGS.dx = iRow << 3;
      regs.HB_XREGS.cx = iCol << 3;
      HB_DOS_INT86( 0x33, &regs, &regs );
   }
}

static int hb_gt_dos_mouse_CountButton( void )
{
   if( s_fMousePresent )
      return s_iMouseButtons;
   else
      return 0;
}

static BOOL hb_gt_dos_mouse_ButtonState( int iButton )
{
   if( s_fMousePresent )
   {
      union REGS regs;

      regs.HB_XREGS.ax = 3;
      HB_DOS_INT86( 0x33, &regs, &regs );

      if( regs.HB_XREGS.bx & ( 1 << iButton ) )
         return TRUE;
   }

   return FALSE;
}

static BOOL hb_gt_dos_mouse_ButtonPressed( int iButton, int * piRow, int * piCol )
{
   if( s_fMousePresent )
   {
      union REGS regs;

      regs.HB_XREGS.ax = 5;
      regs.HB_XREGS.bx = iButton;
      HB_DOS_INT86( 0x33, &regs, &regs );

      if( regs.HB_XREGS.bx )
      {
         *piRow = regs.HB_XREGS.dx >> 3;
         *piCol = regs.HB_XREGS.cx >> 3;
         return TRUE;
      }
   }
   *piRow = *piCol = 0;

   return FALSE;
}

static BOOL hb_gt_dos_mouse_ButtonReleased( int iButton, int * piRow, int * piCol )
{
   if( s_fMousePresent )
   {
      union REGS regs;

      regs.HB_XREGS.ax = 6;
      regs.HB_XREGS.bx = iButton;
      HB_DOS_INT86( 0x33, &regs, &regs );

      if( regs.HB_XREGS.bx )
      {
         *piRow = regs.HB_XREGS.dx >> 3;
         *piCol = regs.HB_XREGS.cx >> 3;
         return TRUE;
      }
   }
   *piRow = *piCol = 0;

   return FALSE;
}

#ifdef HB_MOUSE_SAVE
static int hb_gt_dos_mouse_StorageSize( void )
{
   int iSize = 0;

   if( s_fMousePresent )
   {
      union REGS regs;

      regs.HB_XREGS.ax = 0x15;
      HB_DOS_INT86( 0x33, &regs, &regs );
      s_iMouseStorageSize = regs.HB_XREGS.bx;
      if( s_iMouseStorageSize )
         iSize = s_iMouseStorageSize + 1;
   }
   return iSize;
}

static void hb_gt_dos_mouse_SaveState( BYTE * pBuffer )
{
   if( s_fMousePresent )
   {
      union REGS regs;
      struct SREGS sregs;

      memset( &sregs, 0, sizeof( struct SREGS ) );

#if defined( __DJGPP__ )
{
      _go32_dpmi_seginfo info;

      info.size = ( s_iMouseStorageSize + 15 ) >> 4;
      _go32_dpmi_allocate_dos_memory( &info );

      regs.HB_XREGS.ax = 0x16;
      regs.HB_XREGS.bx = s_iMouseStorageSize;
      regs.HB_XREGS.dx = 0;
      sregs.es = info.rm_segment;

      HB_DOS_INT86X( 0x33, &regs, &regs, &sregs );

      dosmemget( info.rm_segment << 4, s_iMouseStorageSize, pBuffer );
      _go32_dpmi_free_dos_memory(&info);
}
#else
      regs.HB_XREGS.ax = 0x16;
      regs.HB_XREGS.bx = s_iMouseStorageSize;
      regs.HB_XREGS.dx = FP_OFF( pBuffer );
      sregs.es = FP_SEG( pBuffer );
      HB_DOS_INT86X( 0x33, &regs, &regs, &sregs );
#endif
      pBuffer[ s_iMouseStorageSize ] = hb_mouse_GetCursor() ? 1 : 0;
   }
}

static void hb_gt_dos_mouse_RestoreState( BYTE * pBuffer )
{
   if( s_fMousePresent )
   {
      union REGS regs;
      struct SREGS sregs;

      memset( &sregs, 0, sizeof( struct SREGS ) );

      /*
       * Calling hb_mouse_SetCursor is necessary only for
       * synchronization of internal variable s_fMouseVisible
       * because the real mouse cursor state will be also recovered
       * by status restoring
       */
      hb_mouse_SetCursor( pBuffer[ s_iMouseStorageSize ] );

#if defined( __DJGPP__ )
{
      _go32_dpmi_seginfo info;

      info.size = ( s_iMouseStorageSize + 15 ) >> 4;
      _go32_dpmi_allocate_dos_memory( &info );

      regs.HB_XREGS.ax = 0x17;
      regs.HB_XREGS.bx = s_iMouseStorageSize;
      regs.HB_XREGS.dx = 0;
      sregs.es = info.rm_segment;

      HB_DOS_INT86X( 0x33, &regs, &regs, &sregs );

      dosmemput( pBuffer, s_iMouseStorageSize, info.rm_segment << 4 );
      _go32_dpmi_free_dos_memory(&info);
}
#else
      regs.HB_XREGS.ax = 0x17;
      regs.HB_XREGS.bx = s_iMouseStorageSize;
      regs.HB_XREGS.dx = FP_OFF( pBuffer );
      sregs.es = FP_SEG( pBuffer );
      HB_DOS_INT86X( 0x33, &regs, &regs, &sregs );
#endif
   }
}
#endif

static void hb_gt_dos_mouse_SetBounds( int iTop, int iLeft, int iBottom, int iRight )
{
   if( s_fMousePresent )
   {
      union REGS regs;

      regs.HB_XREGS.ax = 7;
      regs.HB_XREGS.cx = iLeft << 3;
      regs.HB_XREGS.dx = iRight << 3;
      HB_DOS_INT86( 0x33, &regs, &regs );

      regs.HB_XREGS.ax = 8;
      regs.HB_XREGS.cx = iTop << 3;
      regs.HB_XREGS.dx = iBottom << 3;
      HB_DOS_INT86( 0x33, &regs, &regs );

      s_iMouseTop    = iTop;
      s_iMouseLeft   = iLeft;
      s_iMouseBottom = iBottom;
      s_iMouseRight  = iRight;
      s_fMouseBound  = TRUE;
   }
}

static void hb_gt_dos_mouse_GetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight )
{
   if( s_fMouseBound  )
   {
      *piTop    = s_iMouseTop;
      *piLeft   = s_iMouseLeft;
      *piBottom = s_iMouseBottom;
      *piRight  = s_iMouseRight;
   }
   else
   {
      *piTop = *piLeft = 0;
      hb_gt_GetSize( piBottom, piRight );
      --(*piBottom);
      --(*piRight);
   }
}

/* *********************************************************************** */

static void hb_gt_dos_Init( FHANDLE hFilenoStdin, FHANDLE hFilenoStdout, FHANDLE hFilenoStderr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_Init(%p,%p,%p)", hFilenoStdin, hFilenoStdout, hFilenoStderr));

   s_bBreak = FALSE;

   /* Set the Ctrl+Break handler [vszakats] */

#if defined(__DJGPP__)

   gppconio_init();
   __djgpp_hwint_flags |= 2;     /* Count Ctrl+Break instead of killing program */
   __djgpp_set_ctrl_c( 0 );      /* Disable Ctrl+C */
   __djgpp_set_sigquit_key( 0 ); /* Disable Ctrl+\ */

#elif defined(__RSX32__)

   /* TODO */

#elif defined(__WATCOMC__)

   signal( SIGBREAK, hb_gt_dos_CtrlBreak_Handler );
   atexit( hb_gt_dos_CtrlBrkRestore );

#elif defined(_MSC_VER)

   signal( SIGINT, hb_gt_dos_CtrlBreak_Handler );
   atexit( hb_gt_dos_CtrlBrkRestore );

#else

   ctrlbrk( hb_gt_dos_CtrlBrkHandler );
   s_iOldCtrlBreak = getcbrk();
   setcbrk( 1 );
   atexit( hb_gt_dos_CtrlBrkRestore );

#endif

   /* initialize code page translation */
   hb_gt_SetDispCP( NULL, NULL, FALSE );
   hb_gt_SetKeyCP( NULL, NULL );

   s_iScreenMode = hb_gt_dos_GetScreenMode();
#if !defined(__DJGPP__)
   s_pScreenAddres = hb_gt_dos_ScreenAddress();
#endif
   hb_gt_dos_GetScreenSize( &s_iRows, &s_iCols );
   hb_gt_dos_GetCursorPosition( &s_iCurRow, &s_iCurCol );
   s_iCursorStyle = hb_gt_dos_GetCursorStyle();
   HB_GTSUPER_INIT( hFilenoStdin, hFilenoStdout, hFilenoStderr );
   HB_GTSUPER_RESIZE( s_iRows, s_iCols );
   hb_gt_dos_GetScreenContents();
   hb_gt_SetPos( s_iCurRow, s_iCurCol );
   if( s_iCursorStyle > 0 )
      hb_gt_SetCursorStyle( s_iCursorStyle );
}

static void hb_gt_dos_Exit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_Exit()"));

   HB_GTSUPER_EXIT();
}

static int hb_gt_dos_ReadKey( int iEventMask )
{
   int ch = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_ReadKey(%d)", iEventMask));

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
      if( iEventMask & INKEY_RAW ) ch = getxkey();
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
         if( iEventMask & INKEY_RAW ) ch = getch() + 512;
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
   if( ch == 0 )
   {
      ch = hb_mouse_ReadKey( iEventMask );
   }
   else if( ch > 0 && ch <= 255 )
   {
      ch = s_keyTrans[ ch ];
   }

   return ch;
}

static BOOL hb_gt_dos_IsColor( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_IsColor()"));

   return s_iScreenMode != 7;
}

static BOOL hb_gt_dos_GetBlink()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_GetBlink()"));

#if defined(__WATCOMC__) && defined(__386__)
   return ( *( ( char * ) 0x0465 ) & 0x10 ) != 0;
#elif defined(__DJGPP__)
   return ( _farpeekb( 0x0040, 0x0065 ) & 0x10 ) != 0;
#else
   return ( *( ( char FAR * ) MK_FP( 0x0040, 0x0065 ) ) &0x10 ) != 0;
#endif
}

static void hb_gt_dos_SetBlink( BOOL fBlink )
{
   union REGS regs;

   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_SetBlink(%d)", (int) fBlink));

   regs.h.ah = 0x10;
   regs.h.al = 0x03;
   regs.h.bh = 0;
   regs.h.bl = fBlink ? 1 : 0;
   HB_DOS_INT86( 0x10, &regs, &regs );
}

static void hb_gt_dos_Tone( double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_Tone(%lf, %lf)", dFrequency, dDuration));

   dFrequency = HB_MIN( HB_MAX( 0.0, dFrequency ), 32767.0 );

#if defined(__BORLANDC__) || defined(__WATCOMC__)
   sound( ( unsigned ) dFrequency );
#elif defined(__DJGPP__)
   sound( ( int ) dFrequency );
#endif

   /* convert Clipper (DOS) timer tick units to seconds ( x / 18.2 ) */
   hb_idleSleep( dDuration / 18.2 );

#if defined(__BORLANDC__) || defined(__WATCOMC__)
   nosound();
#elif defined(__DJGPP__)
   sound( 0 );
#endif
}

static char * hb_gt_dos_Version( int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_dos_Version(%d)", iType ) );

   if( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Harbour Terminal: DOS console";
}

/* some definitions */
#define INT_VIDEO    0x10

#if defined(__DJGPP__)
   #define POKE_BYTE( s, o, b ) /* Do nothing */
   #define outport outportw     /* Use correct function name */
#elif defined(__RSX32__)
   #define inportb( p ) 0       /* Return 0 */
   #define outport( p, w )      /* Do nothing */
   #define outportb( p, b )     /* Do nothing */
   #define POKE_BYTE( s, o, b )  (*((BYTE FAR *)MK_FP((s),(o)) )=(BYTE)(b))
#elif defined(__WATCOMC__)
   #define outportb outp        /* Use correct function name */
   #define outport outpw        /* Use correct function name */
   #define inport inpw          /* Use correct function name */
   #define inportb inp          /* Use correct function name */
   #define POKE_BYTE( s, o, b )  (*((BYTE FAR *)MK_FP((s),(o)) )=(BYTE)(b))
#else
   #define POKE_BYTE( s, o, b )  (*((BYTE FAR *)MK_FP((s),(o)) )=(BYTE)(b))
#endif

static void vmode12x40( void )
{
   union REGS regs;
   regs.HB_XREGS.ax = 0x0001;               /* video mode 40 cols */
   HB_DOS_INT86( INT_VIDEO, &regs, &regs);
   outportb( 0x03D4, 0x09 );         /* update cursor size / pointers */
   regs.h.al = ( inportb( 0x03D5 ) | 0x80 );
   outportb( 0x03D5, regs.h.al );
   POKE_BYTE( 0x40, 0x84, 11);       /* 11 rows number update */
}

static void vmode25x40( void )
{
   union REGS regs;
   regs.HB_XREGS.ax = 0x0001;
   HB_DOS_INT86( INT_VIDEO, &regs, &regs);
}

static void vmode28x40( void )
{
   union REGS regs;
   regs.HB_XREGS.ax = 0x0001;               /* video mode 40 cols */
   HB_DOS_INT86( INT_VIDEO, &regs, &regs);
   regs.HB_XREGS.bx = 0;                    /* load block 0 (BL = 0) */
   regs.HB_XREGS.ax = 0x1111;               /* load 8x8 monochrome char set into RAM */
   HB_DOS_INT86( INT_VIDEO, &regs, &regs);
}

static void vmode50x40( void )
{
   union REGS regs;
   regs.HB_XREGS.ax = 0x0001;
   HB_DOS_INT86( INT_VIDEO, &regs, &regs);
   regs.HB_XREGS.bx = 0;                    /* load block 0 (BL = 0) */
   regs.HB_XREGS.ax = 0x1112;               /* load 8x8 double dot char set into RAM */
   HB_DOS_INT86( INT_VIDEO, &regs, &regs);
   outport( 0x03D4, 0x060A );
}

static void vmode12x80( void )
{
   union REGS regs;
   regs.HB_XREGS.ax = 0x0003;                  /* mode in AL, if bit 7 is on, No CLS */
   HB_DOS_INT86( INT_VIDEO, &regs, &regs);
   outportb( 0x03D4, 0x09 );            /* update cursor size / pointers */
   regs.h.al = ( inportb( 0x03D5 ) | 0x80 );
   outportb( 0x03D5, regs.h.al );
   POKE_BYTE( 0x40, 0x84, 11);          /* 11 rows number update */
}

static void vmode25x80( void )
{
   union REGS regs;
   regs.HB_XREGS.ax = 0x1202;              /* select 350 scan line mode */
   regs.h.bl = 0x30;
   HB_DOS_INT86( INT_VIDEO, &regs, &regs);
   regs.HB_XREGS.ax = 0x0083;              /* mode in AL, if higher bit is on, No CLS */
   HB_DOS_INT86( INT_VIDEO, &regs, &regs);
   regs.HB_XREGS.bx = 0;                   /* load block 0 (BL = 0) */
   regs.HB_XREGS.ax = 0x1114;              /* load 8x14 VGA char set into RAM */
   HB_DOS_INT86( INT_VIDEO, &regs, &regs);
}

static void vmode28x80( void )
{
   union REGS regs;
   regs.HB_XREGS.ax = 0x0003;              /* mode in AL, if higher bit is on, No CLS */
   HB_DOS_INT86( INT_VIDEO, &regs, &regs);
   regs.HB_XREGS.bx = 0;                   /* load block 0 (BL = 0) */
   regs.HB_XREGS.ax = 0x1111;              /* load 8x8 monochrome char set into RAM */
   HB_DOS_INT86( INT_VIDEO, &regs, &regs);
}

static void vmode43x80( void )
{
   union REGS regs;
   regs.HB_XREGS.ax = 0x1201;              /*  select 350 scan line mode */
   regs.h.bl = 0x30;
   HB_DOS_INT86( INT_VIDEO, &regs, &regs);
   regs.HB_XREGS.ax = 0x0003;              /* mode in AL, if higher bit is on, No CLS */
   HB_DOS_INT86( INT_VIDEO, &regs, &regs);
   regs.h.bh = 0x1;                 /* bytes per character */
   regs.h.bl = 0x0;                 /* load block 0 */
   regs.HB_XREGS.ax = 0x1112;              /* load 8x8 double dot char set into RAM */
   HB_DOS_INT86( INT_VIDEO, &regs, &regs);
   outport( 0x03D4, 0x060A );       /* update cursor size / pointers */
   POKE_BYTE( 0x40, 0x84, 42);      /* 42 rows number update */
}

static void vmode50x80( void )
{
   union REGS regs;
   regs.HB_XREGS.ax = 0x1202;               /*  select 400 scan line mode */
   regs.h.bl = 0x30;
   HB_DOS_INT86( INT_VIDEO, &regs, &regs);
   regs.HB_XREGS.ax = 0x0003;               /* mode in AL, if bit 7 is on, No CLS */
   HB_DOS_INT86( INT_VIDEO, &regs, &regs);
   regs.HB_XREGS.bx = 0;                    /* load block 0 (BL = 0) */
   regs.HB_XREGS.ax = 0x1112;               /* load 8x8 double dot char set into RAM */
   HB_DOS_INT86( INT_VIDEO, &regs, &regs);
}

/***************************************************************************
 * Return the display combination: monitor + video card
 *
 * INT 10 - VIDEO - GET DISPLAY COMBINATION CODE (PS,VGA/MCGA)
 *         AX = 1A00h
 * Return: AL = 1Ah if function was supported
 *         BL = active display code (see below)
 *         BH = alternate display code
 *
 * Values for display combination code:
 *  00h    no display
 *  01h    monochrome adapter w/ monochrome display
 *  02h    CGA w/ color display
 *  03h    reserved
 *  04h    EGA w/ color display
 *  05h    EGA w/ monochrome display
 *  06h    PGA w/ color display
 *  07h    VGA w/ monochrome analog display
 *  08h    VGA w/ color analog display
 *  09h    reserved
 *  0Ah    MCGA w/ digital color display
 *  0Bh    MCGA w/ monochrome analog display
 *  0Ch    MCGA w/ color analog display
 *  FFh    unknown display type
 ****************************************************************************/

static USHORT hb_gt_GetDisplay( void )
{
   union REGS regs;

   regs.HB_XREGS.ax = 0x1A00;
   HB_DOS_INT86( INT_VIDEO, &regs, &regs);

   return ( regs.h.al == 0x1A ) ? regs.h.bl : 0xFF;
}

static BOOL hb_gt_dos_SetMode( int iRows, int iCols )
{
   /* hb_gt_IsColor() test for color card, we need to know if it is a VGA board...*/
   BOOL bIsVGA, bIsVesa, bSuccess;

   HB_TRACE( HB_TR_DEBUG, ("hb_gt_dos_SetMode(%d, %d)", iRows, iCols) );

   bIsVGA = ( hb_gt_GetDisplay() == 8 );
   bIsVesa = FALSE;

   /* Available modes in B&N and color screens */
   if( iCols == 40 )
   {
      if( iRows == 12 )
          vmode12x40();
      else if( iRows == 25 )
          vmode25x40();
      else if( iRows == 28 )
          vmode28x40();
      else if( iRows == 50 )
          vmode50x40();
   }

   if( bIsVGA )
   {
      if( iCols == 80)
      {
         if( iRows == 12 )
             vmode12x80();
         else if( iRows == 25 )
             vmode25x80();
         else if( iRows == 28 )
             vmode28x80();
         else if( iRows == 43 )
             vmode43x80();
         else if( iRows == 50 )
             vmode50x80();
      }

      if( iCols > 80 && bIsVesa )
      {
         /* In development process
          * return( hb_gt_Modevesa( nMode) );
          */
      }
   }

   hb_gt_dos_GetScreenSize( &s_iRows, &s_iCols );

   /* Check for succesful */
   if( s_iRows == iRows && s_iCols == iCols )
   {
      bSuccess = TRUE;
   }
   else
   {
      bSuccess = FALSE;
      vmode25x80();
      hb_gt_dos_GetScreenSize( &s_iRows, &s_iCols );
   }
   s_iScreenMode = hb_gt_dos_GetScreenMode();
#if !defined(__DJGPP__)
   s_pScreenAddres = hb_gt_dos_ScreenAddress();
#endif
   hb_gt_dos_GetCursorPosition( &s_iCurRow, &s_iCurCol );
   s_iCursorStyle = hb_gt_dos_GetCursorStyle();
   HB_GTSUPER_RESIZE( s_iRows, s_iCols );
   hb_gt_dos_GetScreenContents();
   hb_gt_SetPos( s_iCurRow, s_iCurCol );
   if( s_iCursorStyle > 0 )
      hb_gt_SetCursorStyle( s_iCursorStyle );

   return bSuccess;
}

static BOOL hb_gt_dos_PreExt()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_PreExt()"));

   return TRUE;
}

static BOOL hb_gt_dos_PostExt()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_PostExt()"));

   hb_gt_dos_GetCursorPosition( &s_iCurRow, &s_iCurCol );
   hb_gt_dos_GetScreenContents();
   hb_gt_SetPos( s_iCurRow, s_iCurCol );

   return TRUE;
}

static BOOL hb_gt_dos_Suspend()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_Suspend()"));

   return TRUE;
}

static BOOL hb_gt_dos_Resume()
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_dos_Resume()"));

   s_iScreenMode = hb_gt_dos_GetScreenMode();
#if !defined(__DJGPP__)
   s_pScreenAddres = hb_gt_dos_ScreenAddress();
#endif
   hb_gt_dos_GetScreenSize( &s_iRows, &s_iCols );
   hb_gt_dos_GetCursorPosition( &s_iCurRow, &s_iCurCol );
   s_iCursorStyle = hb_gt_dos_GetCursorStyle();
   HB_GTSUPER_RESIZE( s_iRows, s_iCols );
   hb_gt_dos_GetScreenContents();
   hb_gt_SetPos( s_iCurRow, s_iCurCol );
   if( s_iCursorStyle > 0 )
      hb_gt_SetCursorStyle( s_iCursorStyle );

   return TRUE;
}

static BOOL hb_gt_dos_SetDispCP( char *pszTermCDP, char *pszHostCDP, BOOL fBox )
{
   int i;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_dos_SetDispCP(%s,%s,%d)", pszTermCDP, pszHostCDP, (int) fBox ) );

   HB_SYMBOL_UNUSED( fBox );

   for( i = 0; i < 256; i++ )
      s_charTrans[ i ] = ( BYTE ) i;

#ifndef HB_CDP_SUPPORT_OFF
   if( !pszHostCDP )
      pszHostCDP = hb_cdp_page->id;

   if( pszTermCDP && pszHostCDP )
   {
      PHB_CODEPAGE cdpTerm = hb_cdpFind( pszTermCDP ),
                   cdpHost = hb_cdpFind( pszHostCDP );
      if( cdpTerm && cdpHost && cdpTerm != cdpHost &&
          cdpTerm->nChars && cdpTerm->nChars == cdpHost->nChars )
      {
         for( i = 0; i < cdpHost->nChars; ++i )
         {
            s_charTrans[ ( BYTE ) cdpHost->CharsUpper[ i ] ] =
                         ( BYTE ) cdpTerm->CharsUpper[ i ];
            s_charTrans[ ( BYTE ) cdpHost->CharsLower[ i ] ] =
                         ( BYTE ) cdpTerm->CharsLower[ i ];
         }
      }
   }
#else
   HB_SYMBOL_UNUSED( pszTermCDP );
   HB_SYMBOL_UNUSED( pszHostCDP );
#endif
   for( i = 0; i < 256; i++ )
      s_charTransRev[ s_charTrans[ i ] ] = ( BYTE ) i;

   return TRUE;
}

/* *********************************************************************** */

static BOOL hb_gt_dos_SetKeyCP( char *pszTermCDP, char *pszHostCDP )
{
   int i;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_dos_SetKeyCP(%s,%s)", pszTermCDP, pszHostCDP ) );

   for( i = 0; i < 256; i++ )
      s_keyTrans[ i ] = ( BYTE ) i;

#ifndef HB_CDP_SUPPORT_OFF
   if( !pszHostCDP )
   {
      pszHostCDP = hb_cdp_page->id;
   }

   if( pszTermCDP && pszHostCDP )
   {
      PHB_CODEPAGE cdpTerm = hb_cdpFind( pszTermCDP ),
                   cdpHost = hb_cdpFind( pszHostCDP );
      if( cdpTerm && cdpHost && cdpTerm != cdpHost &&
          cdpTerm->nChars && cdpTerm->nChars == cdpHost->nChars )
      {
         for( i = 0; i < cdpHost->nChars; ++i )
         {
            s_keyTrans[ ( BYTE ) cdpHost->CharsUpper[ i ] ] =
                        ( BYTE ) cdpTerm->CharsUpper[ i ];
            s_keyTrans[ ( BYTE ) cdpHost->CharsLower[ i ] ] =
                        ( BYTE ) cdpTerm->CharsLower[ i ];
         }
      }
   }
#else
   HB_SYMBOL_UNUSED( pszTermCDP );
   HB_SYMBOL_UNUSED( pszHostCDP );
#endif

   return TRUE;
}

/* *********************************************************************** */

static void hb_gt_dos_Redraw( int iRow, int iCol, int iSize )
{
#if !defined(__DJGPP__)
   USHORT FAR *pScreenPtr = (USHORT FAR *) hb_gt_dos_ScreenPtr( iRow, iCol );
#endif
   BYTE bColor, bAttr;
   USHORT usChar;
   int iLen = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_dos_Redraw(%d, %d, %d)", iRow, iCol, iSize ) );

   while( iLen < iSize )
   {
      if( !hb_gt_GetScrChar( iRow, iCol + iLen, &bColor, &bAttr, &usChar ) )
         break;

#if defined(__DJGPP__TEXT)
      {
         short ch_attr = ( ( short ) bColor << 8 ) | s_charTrans[ usChar & 0xff ];
         puttext( iCol + iLen + 1, iRow + 1, iCol + iLen  + 1, iRow + 1, &ch_attr );
      }
#elif defined(__DJGPP__)
      ScreenPutChar( s_charTrans[ usChar & 0xff ], bColor, iCol + iLen, iRow );
#else
      *pScreenPtr++ = ( bColor << 8 ) + s_charTrans[ usChar & 0xff ];
#endif
      iLen++;
   }
}

static void hb_gt_dos_Refresh( void )
{
   int iRow, iCol, iStyle;

   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_dos_Refresh()" ) );

   HB_GTSUPER_REFRESH();

   hb_gt_GetScrCursor( &iRow, &iCol, &iStyle );
   if( iStyle != SC_NONE )
   {
      if( iRow >= 0 && iCol >= 0 && iRow < s_iRows && iCol < s_iCols )
         hb_gt_dos_SetCursorPosition( iRow, iCol );
      else
         iStyle = SC_NONE;
   }
   hb_gt_dos_SetCursorStyle( iStyle );
}


/* *********************************************************************** */

static BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_FuncInit(%p)", pFuncTable));

   pFuncTable->Init                       = hb_gt_dos_Init;
   pFuncTable->Exit                       = hb_gt_dos_Exit;
   pFuncTable->IsColor                    = hb_gt_dos_IsColor;
   pFuncTable->SetMode                    = hb_gt_dos_SetMode;
   pFuncTable->Redraw                     = hb_gt_dos_Redraw;
   pFuncTable->Refresh                    = hb_gt_dos_Refresh;
   pFuncTable->SetBlink                   = hb_gt_dos_SetBlink;
   pFuncTable->GetBlink                   = hb_gt_dos_GetBlink;
   pFuncTable->Version                    = hb_gt_dos_Version;
   pFuncTable->Suspend                    = hb_gt_dos_Suspend;
   pFuncTable->Resume                     = hb_gt_dos_Resume;
   pFuncTable->PreExt                     = hb_gt_dos_PreExt;
   pFuncTable->PostExt                    = hb_gt_dos_PostExt;
   pFuncTable->Tone                       = hb_gt_dos_Tone;
   pFuncTable->SetDispCP                  = hb_gt_dos_SetDispCP;
   pFuncTable->SetKeyCP                   = hb_gt_dos_SetKeyCP;

   pFuncTable->ReadKey                    = hb_gt_dos_ReadKey;

   pFuncTable->MouseInit                  = hb_gt_dos_mouse_Init;
   pFuncTable->MouseExit                  = hb_gt_dos_mouse_Exit;
   pFuncTable->MouseIsPresent             = hb_gt_dos_mouse_IsPresent;
   pFuncTable->MouseShow                  = hb_gt_dos_mouse_Show;
   pFuncTable->MouseHide                  = hb_gt_dos_mouse_Hide;
   pFuncTable->MouseGetPos                = hb_gt_dos_mouse_GetPos;
   pFuncTable->MouseSetPos                = hb_gt_dos_mouse_SetPos;
   pFuncTable->MouseSetBounds             = hb_gt_dos_mouse_SetBounds;
   pFuncTable->MouseGetBounds             = hb_gt_dos_mouse_GetBounds;
   pFuncTable->MouseCountButton           = hb_gt_dos_mouse_CountButton;
   pFuncTable->MouseButtonState           = hb_gt_dos_mouse_ButtonState;
   pFuncTable->MouseButtonPressed         = hb_gt_dos_mouse_ButtonPressed;
   pFuncTable->MouseButtonReleased        = hb_gt_dos_mouse_ButtonReleased;
#ifdef HB_MOUSE_SAVE
   pFuncTable->MouseStorageSize           = hb_gt_dos_mouse_StorageSize;
   pFuncTable->MouseSaveState             = hb_gt_dos_mouse_SaveState;
   pFuncTable->MouseRestoreState          = hb_gt_dos_mouse_RestoreState;
#endif

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
