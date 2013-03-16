/*
 * Harbour Project source code:
 * Mouse subsystem for gtsln
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.net)
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

/* *********************************************************************** */

#include "gtsln.h"
#include <sys/time.h>
#if defined( HB_HAS_GPM )
    #include <sys/types.h>
    #include <sys/stat.h>
    #include <fcntl.h>
    #include <gpm.h>
    Gpm_Connect Conn;
#endif

/* *********************************************************************** */

static int s_iMouseRow = 0;
static int s_iMouseCol = 0;

static HB_BOOL s_bMousePresent = HB_FALSE;
static int     s_iMouseButtons = -1;

static HB_USHORT s_usMouseState = 0;
static HB_USHORT s_usLastMouseState = 0;

static struct timeval mLeftDblckTime;
static struct timeval mMiddleDblckTime;
static struct timeval mRightDblckTime;


/* *********************************************************************** */

#if defined( HB_HAS_GPM )
static HB_BOOL GetGpmEvent( Gpm_Event * Evt )
{
   if( s_bMousePresent && gpm_fd >= 0 )
   {
      struct timeval tv = { 0, 0 };
      fd_set ReadFD;

      FD_ZERO( &ReadFD );
      FD_SET( gpm_fd, &ReadFD );

      if( select( gpm_fd + 1, &ReadFD, NULL, NULL, &tv ) > 0 )
         if( FD_ISSET( gpm_fd, &ReadFD ) )
            return Gpm_GetEvent( Evt ) > 0;
   }

   return HB_FALSE;
}
#endif

/* *********************************************************************** */

static HB_BOOL GetXtermEvent( int * Btn, int * Col, int * Row )
{
   /* Xterm mouse event consists of three chars */
   if( SLang_input_pending( 0 ) > 0 )
   {
      *Btn = SLang_getkey() - 0x20;

      if( SLang_input_pending( 0 ) > 0 )
      {
         *Col = SLang_getkey() - 0x21;
         if( SLang_input_pending( 0 ) > 0 )
         {
            *Row = SLang_getkey() - 0x21;
            return HB_TRUE;
         }
      }
   }

   return HB_FALSE;
}

/* *********************************************************************** */

static void hb_sln_CheckDoubleClick( void )
{
   HB_USHORT usNewButtons = ( s_usMouseState & ~s_usLastMouseState ) & M_BUTTON_KEYMASK;

   if( usNewButtons != 0 )
   {
      struct timeval evtTime;

      TIMEVAL_GET( evtTime );
      if( usNewButtons & M_BUTTON_LEFT )
      {
         if( TIMEVAL_LESS( evtTime, mLeftDblckTime ) )
            s_usMouseState |= M_BUTTON_LDBLCK;
         TIMEVAL_ADD( mLeftDblckTime, evtTime, hb_mouseGetDoubleClickSpeed() );
      }
      if( usNewButtons & M_BUTTON_MIDDLE )
      {
         if( TIMEVAL_LESS( evtTime, mMiddleDblckTime ) )
            s_usMouseState |= M_BUTTON_MDBLCK;
         TIMEVAL_ADD( mMiddleDblckTime, evtTime, hb_mouseGetDoubleClickSpeed() );
      }
      if( usNewButtons & M_BUTTON_RIGHT )
      {
         if( TIMEVAL_LESS( evtTime, mRightDblckTime ) )
            s_usMouseState |= M_BUTTON_RDBLCK;
         TIMEVAL_ADD( mRightDblckTime, evtTime, hb_mouseGetDoubleClickSpeed() );
      }
   }
}

/* *********************************************************************** */

void hb_gt_sln_mouse_ProcessTerminalEvent( void )
{
   int Btn, Col, Row;

   if( GetXtermEvent( &Btn, &Col, &Row ) )
   {
      if( s_iMouseRow != Row || s_iMouseCol != Col )
         s_usMouseState |= M_CURSOR_MOVE;

      s_iMouseRow = Row;
      s_iMouseCol = Col;

      switch( Btn & 0xE3 )
      {
         case 0:
            s_usMouseState |= M_BUTTON_LEFT;
            break;
         case 1:
            s_usMouseState |= M_BUTTON_MIDDLE;
            break;
         case 2:
            s_usMouseState |= M_BUTTON_RIGHT;
            break;
         case 3:
            s_usMouseState &= ~M_BUTTON_KEYMASK;
            break;
         case 0x40:
            s_usMouseState |= M_BUTTON_WHEELUP;
            break;
         case 0x41:
            s_usMouseState |= M_BUTTON_WHEELDOWN;
            break;
      }
      hb_sln_CheckDoubleClick();
   }
}

/* *********************************************************************** */

int hb_gt_sln_mouse_Inkey( int iEventMask, HB_BOOL fCheckNew )
{
   if( s_usMouseState != s_usLastMouseState )
   {
      if( s_usMouseState & M_CURSOR_MOVE )
      {
         s_usMouseState &= ~M_CURSOR_MOVE;
         return K_MOUSEMOVE;
      }
      else if( s_usMouseState & M_BUTTON_WHEELUP )
      {
         s_usMouseState &= ~M_BUTTON_WHEELUP;
         return K_MWFORWARD;
      }
      else if( s_usMouseState & M_BUTTON_WHEELDOWN )
      {
         s_usMouseState &= ~M_BUTTON_WHEELDOWN;
         return K_MWBACKWARD;
      }
      else
      {
         HB_USHORT usKeyDiff = ( s_usMouseState ^ s_usLastMouseState );

         if( usKeyDiff & M_BUTTON_LEFT )
         {
            s_usLastMouseState ^= M_BUTTON_LEFT;
            if( s_usMouseState & M_BUTTON_LEFT )
            {
               if( s_usMouseState & M_BUTTON_LDBLCK )
               {
                  s_usMouseState &= ~M_BUTTON_LDBLCK;
                  return K_LDBLCLK;
               }
               else
                  return K_LBUTTONDOWN;
            }
            else
               return K_LBUTTONUP;
         }
         else if( usKeyDiff & M_BUTTON_MIDDLE )
         {
            s_usLastMouseState ^= M_BUTTON_MIDDLE;
            if( s_usMouseState & M_BUTTON_MIDDLE )
            {
               if( s_usMouseState & M_BUTTON_MDBLCK )
               {
                  s_usMouseState &= ~M_BUTTON_MDBLCK;
                  return K_MDBLCLK;
               }
               else
                  return K_MBUTTONDOWN;
            }
            else
               return K_MBUTTONUP;
         }
         else if( usKeyDiff & M_BUTTON_RIGHT )
         {
            s_usLastMouseState ^= M_BUTTON_RIGHT;
            if( s_usMouseState & M_BUTTON_RIGHT )
            {
               if( s_usMouseState & M_BUTTON_RDBLCK )
               {
                  s_usMouseState &= ~M_BUTTON_RDBLCK;
                  return K_RDBLCLK;
               }
               else
                  return K_RBUTTONDOWN;
            }
            else
               return K_RBUTTONUP;
         }
         s_usLastMouseState = s_usMouseState;
      }
   }

#if defined( HB_HAS_GPM )

#define CHECK_BUTTON_DOWN( Mask, GpmBtn, InkBtn, InkDbl )              \
   if( ( iEventMask & Mask ) && ( Evt.buttons & GpmBtn ) )        \
   {                                                              \
      if( Evt.type & GPM_SINGLE )                                 \
         return InkBtn;                                           \
      else if( Evt.type & GPM_DOUBLE || Evt.type & GPM_TRIPLE )   \
         return InkDbl;                                           \
   }

   else if( hb_sln_UnderLinuxConsole && fCheckNew )
   {
      Gpm_Event Evt;

      if( GetGpmEvent( &Evt ) )
      {
         /* get the mouse event position */
         s_iMouseRow = Evt.y;
         s_iMouseCol = Evt.x;

         if( ( Evt.type & GPM_MOVE ) && ( iEventMask & INKEY_MOVE ) )
            return K_MOUSEMOVE;

         else if( Evt.type & GPM_DOWN )
         {
            CHECK_BUTTON_DOWN(INKEY_LDOWN,GPM_B_LEFT,K_LBUTTONDOWN,K_LDBLCLK)
            else
            CHECK_BUTTON_DOWN(INKEY_RDOWN,GPM_B_RIGHT,K_RBUTTONDOWN,K_RDBLCLK)
            else
            CHECK_BUTTON_DOWN(INKEY_MMIDDLE,GPM_B_MIDDLE,K_MBUTTONDOWN,K_MDBLCLK)
         }

         else if( Evt.type & GPM_UP )
         {
            if( ( iEventMask & INKEY_LUP ) && ( Evt.buttons & GPM_B_LEFT ) )
               return K_LBUTTONUP;
            else if( ( iEventMask & INKEY_RUP ) && ( Evt.buttons & GPM_B_RIGHT ) )
               return K_RBUTTONUP;
            else if( ( iEventMask & INKEY_MMIDDLE ) && ( Evt.buttons & GPM_B_MIDDLE ) )
               return K_MBUTTONUP;
         }
      }
   }
#else
   HB_SYMBOL_UNUSED( fCheckNew );
   HB_SYMBOL_UNUSED( iEventMask );
#endif

   return 0;
}

/* *********************************************************************** */

void hb_gt_sln_mouse_Init( void )
{
   if( hb_sln_UnderXterm )
   {
      const char * SaveHilit = "\033[?1001s"; /* save old hilit tracking */
      const char * EnabTrack = "\033[?1000h"; /* enable mouse tracking */

      /* force mouse usage under xterm */
      (void)SLtt_set_mouse_mode( 1, 1 );

      /* initial xterm settings */
      SLtt_write_string( ( char * ) SaveHilit );
      SLtt_write_string( ( char * ) EnabTrack );
      SLtt_flush_output();

      s_iMouseButtons = SLtt_tgetnum( ( char * ) "BT" );

      /* force two buttons mouse under xterm */
      if( s_iMouseButtons < 1 )
         s_iMouseButtons = 3;

      s_bMousePresent = HB_TRUE;
   }
#if defined( HB_HAS_GPM )
   else if( hb_sln_UnderLinuxConsole )
   {
#ifdef HB_GPM_NOICE_DISABLE
      int iNull, iErr;

      iErr = dup( STDERR_FILENO );
      iNull = open( "/dev/null", O_RDWR );
      dup2( iNull, STDERR_FILENO );
      close( iNull );
#endif
      Conn.eventMask = GPM_MOVE | GPM_UP | GPM_DOWN | GPM_DRAG | GPM_DOUBLE;
      /* give me move events but handle them anyway */
      Conn.defaultMask= GPM_MOVE | GPM_HARD;
      /* only pure mouse events, no Ctrl,Alt,Shft events */
      Conn.minMod = 0;    Conn.maxMod = 0;
      gpm_zerobased = 1;  gpm_visiblepointer = 1;

      if( Gpm_Open( &Conn, 0 ) >= 0 && gpm_fd >= 0 )
      {
         Gpm_Event Evt;

         s_bMousePresent = HB_TRUE;

         while( GetGpmEvent( &Evt ) );
         {
            s_iMouseRow = Evt.y;
            s_iMouseCol = Evt.x;
         }

         /*
          * In recent GPM versions it produce unpleasure noice on the screen
          * so I covered it with this macro, [druzus]
          */
#ifdef HB_GPM_USE_XTRA
         s_iMouseButtons = Gpm_GetSnapshot( NULL );
#else
         s_iMouseButtons = 3;
#endif
         hb_gt_sln_mouse_FixTrash();
      }
#ifdef HB_GPM_NOICE_DISABLE
      dup2( iErr, STDERR_FILENO );
      close( iErr );
#endif
   }
#endif
}

/* *********************************************************************** */

void hb_gt_sln_mouse_Exit( void )
{
   if( s_bMousePresent )
   {
      if( hb_sln_UnderXterm )
      {
         const char * DisabTrack = "\033[?1000l"; /* disable mouse tracking */
         const char * RestoHilit = "\033[?1001r"; /* restore old hilittracking */

         /* restore xterm settings */
         SLtt_write_string( ( char * ) DisabTrack );
         SLtt_write_string( ( char * ) RestoHilit );
         SLtt_flush_output();

         /* force mouse usage under xterm */
         (void)SLtt_set_mouse_mode( 0, 1 );
      }
#if defined( HB_HAS_GPM )
      else if( hb_sln_UnderLinuxConsole )
      {
         if( gpm_fd >= 0 )
            Gpm_Close();
      }
#endif
      s_bMousePresent = HB_FALSE;
   }
}

/* *********************************************************************** */

HB_BOOL hb_gt_sln_mouse_IsPresent( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   return s_bMousePresent;
}

/* *********************************************************************** */

void hb_gt_sln_mouse_Show( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

#if defined( HB_HAS_GPM )
   gpm_visiblepointer = 1;
   if( hb_sln_UnderLinuxConsole && s_bMousePresent )
      Gpm_DrawPointer( s_iMouseCol, s_iMouseRow, gpm_consolefd );
#endif
}

/* *********************************************************************** */

void hb_gt_sln_mouse_Hide( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

#if defined( HB_HAS_GPM )
   gpm_visiblepointer = 0;
#endif
}

/* *********************************************************************** */

void hb_gt_sln_mouse_GetPos( PHB_GT pGT, int * piRow, int * piCol )
{
   HB_SYMBOL_UNUSED( pGT );

   *piRow = s_iMouseRow;
   *piCol = s_iMouseCol;
}

/* *********************************************************************** */

void hb_gt_sln_mouse_SetPos( PHB_GT pGT, int iRow, int iCol )
{
   HB_SYMBOL_UNUSED( pGT );

   /* it does really nothing */
   s_iMouseRow = iRow;
   s_iMouseCol = iCol;
#if defined( HB_HAS_GPM )
   if( hb_sln_UnderLinuxConsole )
      if( s_bMousePresent && gpm_visiblepointer )
         Gpm_DrawPointer( iCol, iRow, gpm_consolefd );
#endif
}

/* *********************************************************************** */

HB_BOOL hb_gt_sln_mouse_ButtonState( PHB_GT pGT, int iButton )
{
   HB_SYMBOL_UNUSED( pGT );

   switch( iButton )
   {
      case 0:
         return ( s_usMouseState & M_BUTTON_LEFT ) != 0;
      case 1:
         return ( s_usMouseState & M_BUTTON_RIGHT ) != 0;
      case 2:
         return ( s_usMouseState & M_BUTTON_MIDDLE ) != 0;
   }

   return HB_FALSE;
}

/* *********************************************************************** */

int hb_gt_sln_mouse_CountButton( PHB_GT pGT )
{
   HB_SYMBOL_UNUSED( pGT );

   return s_iMouseButtons;
}

/* *********************************************************************** */

void hb_gt_sln_mouse_FixTrash( void )
{
#if defined( HB_HAS_GPM )
   if( hb_sln_UnderLinuxConsole )
      if( s_bMousePresent && gpm_visiblepointer )
         Gpm_DrawPointer( s_iMouseCol, s_iMouseRow, gpm_consolefd );
#endif
}

/* *********************************************************************** */
