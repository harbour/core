/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Video subsystem based on Slang screen library.
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

/* NOTE: User programs should never call this layer directly! */

/* *********************************************************************** */

#define HB_GT_NAME  SLN

#include "hbgtcore.h"
#include "hbinit.h"
#include "hbapifs.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "inkey.ch"
#include "hbapicdp.h"
#include "hbdate.h"

#if defined( HB_OS_DARWIN ) || \
    defined( HB_OS_AIX ) || \
    ( defined( HB_OS_LINUX ) && defined( __WATCOMC__ ) )
#define REAL_UNIX_SYSTEM  /* this is for slang.h to include some defs */
#endif
#include <slang.h>

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <signal.h>

#ifndef HB_OS_DARWIN
#include <time.h>
#endif

/*
 * It's a hack to detect UTF-8 patched version of slang, you may
 * need to modified it for your slang version because UTF-8 patches
 * are still unoficial
 */
#if SLANG_VERSION >= 20000
    #define HB_SLN_UTF8
#elif defined( UTF8 ) && defined( SLSMG_HLINE_CHAR_TERM )
    #define HB_SLN_UNICODE
#endif

/* missing defines in previous versions of Slang - this may not work ok ! */
#ifdef HB_SLN_UTF8

#define HB_SLN_SET_ACSC( slch )  \
   do { \
      ( slch ).color |= SLSMG_ACS_MASK; \
   } while( 0 )
#define HB_SLN_BUILD_CHAR( slch, ch, clr, attr )  \
   do { \
      SLsmg_Char_Type * outTab = ( attr ) & HB_GT_ATTR_BOX ? \
                                 s_outboxTab : s_outputTab; \
      ( slch ).color = outTab[ ( HB_BYTE ) ( ch ) ].color | \
                       s_colorTab[ ( HB_BYTE ) ( clr ) ]; \
      ( slch ).nchars      = 1; \
      ( slch ).wchars[ 0 ] = outTab[ ( HB_BYTE ) ( ch ) ].wchars[ 0 ]; \
   } while( 0 )

#define HB_SLN_BUILD_RAWCHAR( slch, ch, attr )  \
   do { \
      ( slch ).color       = ( attr ); \
      ( slch ).nchars      = 1; \
      ( slch ).wchars[ 0 ] = ( SLwchar_Type ) ( ch ); \
   } while( 0 )

#define HB_SLN_IS_CHAR( slch )             ( ( slch ).wchars[ 0 ] != 0 )

#else /* !defined( HB_SLN_UTF8 ) */

#if SLANG_VERSION < 10400
    typedef unsigned short SLsmg_Char_Type;
    #define SLSMG_EXTRACT_CHAR( x ) ( ( x ) & 0xFF )
    #define SLSMG_EXTRACT_COLOR( x ) ( ( ( x ) >> 8 ) & 0xFF )
    #define SLSMG_BUILD_CHAR( ch, color ) ( ( ( SLsmg_Char_Type ) ( unsigned char )( ch ) ) | ( ( color ) << 8 ) )

#if SLANG_VERSION < 10308
    #define SLSMG_DIAMOND_CHAR    0x04
    #define SLSMG_DEGREE_CHAR     0xF8
    #define SLSMG_PLMINUS_CHAR    0xF1
    #define SLSMG_BULLET_CHAR     0xF9
    #define SLSMG_LARROW_CHAR     0x1B
    #define SLSMG_RARROW_CHAR     0x1A
    #define SLSMG_DARROW_CHAR     0x19
    #define SLSMG_UARROW_CHAR     0x18
    #define SLSMG_BOARD_CHAR      0xB2
    #define SLSMG_BLOCK_CHAR      0xDB
    /*
    #define SLSMG_BOARD_CHAR      'h'
    #define SLSMG_BLOCK_CHAR      '0'
    */
#endif
#endif

#define HB_SLN_SET_ACSC( slch )  \
   do { \
      ( slch ) = SLSMG_BUILD_CHAR( ( slch ), 0x80 ); \
   } while( 0 )
#define HB_SLN_BUILD_CHAR( slch, ch, clr, attr )  \
   do { \
      ( slch ) = ( ( attr ) & HB_GT_ATTR_BOX ? \
                   s_outboxTab : s_outputTab )[ ( HB_BYTE ) ( ch ) ] | \
                 s_colorTab[ ( HB_BYTE ) ( clr ) ]; \
   } while( 0 )

#define HB_SLN_BUILD_RAWCHAR( slch, ch, attr )  \
   do { \
      ( slch ) = SLSMG_BUILD_CHAR( ( ch ), ( attr ) ); \
   } while( 0 )

#define HB_SLN_IS_CHAR( slch )  ( ( slch ) != 0 )

#endif /* HB_SLN_UTF8 */

/* *********************************************************************** */

/* if we can not manipulate cursor state */
#define SC_UNAVAIL          -1

/* xHarbour compatible definitions */
#if ! defined( K_SH_LEFT )
#define K_SH_LEFT           K_LEFT   /* Shift-Left  == Left  */
#define K_SH_UP             K_UP     /* Shift-Up    == Up    */
#define K_SH_RIGHT          K_RIGHT  /* Shift-Right == Right */
#define K_SH_DOWN           K_DOWN   /* Shift-Down  == Down  */
#define K_SH_INS            K_INS    /* Shift-Ins   == Ins   */
#define K_SH_DEL            K_DEL    /* Shift-Del   == Del   */
#define K_SH_HOME           K_HOME   /* Shift-Home  == Home  */
#define K_SH_END            K_END    /* Shift-End   == End   */
#define K_SH_PGUP           K_PGUP   /* Shift-PgUp  == PgUp  */
#define K_SH_PGDN           K_PGDN   /* Shift-PgDn  == PgDn  */
#define K_SH_RETURN         K_RETURN /* Shift-Enter == Enter */
#define K_SH_ENTER          K_ENTER  /* Shift-Enter == Enter */
#endif

/* *********************************************************************** */

#define M_BUTTON_LEFT       0x0001
#define M_BUTTON_RIGHT      0x0002
#define M_BUTTON_MIDDLE     0x0004
#define M_BUTTON_LDBLCK     0x0010
#define M_BUTTON_RDBLCK     0x0020
#define M_BUTTON_MDBLCK     0x0040
#define M_BUTTON_WHEELUP    0x0100
#define M_BUTTON_WHEELDOWN  0x0200
#define M_CURSOR_MOVE       0x0400
#define M_BUTTON_KEYMASK    ( M_BUTTON_LEFT | M_BUTTON_RIGHT | M_BUTTON_MIDDLE )
#define M_BUTTON_DBLMASK    ( M_BUTTON_LDBLCK | M_BUTTON_RDBLCK | M_BUTTON_MDBLCK )

#define TIMEVAL_GET( tv )         gettimeofday( &( tv ), NULL );
#define TIMEVAL_LESS( tv1, tv2 )  ( ( ( tv1 ).tv_sec == ( tv2 ).tv_sec ) ?     \
                                    ( ( tv1 ).tv_usec < ( tv2 ).tv_usec ) :     \
                                    ( ( tv1 ).tv_sec < ( tv2 ).tv_sec ) )
#define TIMEVAL_ADD( dst, src, n )  \
   {                                      \
      ( dst ).tv_sec = ( src ).tv_sec + n / 1000;                           \
      if( ( ( dst ).tv_usec = ( src ).tv_usec + ( n % 1000 ) * 1000 ) >= 1000000 ) {     \
         ( dst ).tv_usec -= 1000000; ( dst ).tv_sec++;                      \
      } \
   }

/* *********************************************************************** */

extern HB_BOOL hb_sln_Is_Unicode;
extern HB_BOOL hb_sln_UnderLinuxConsole;
extern HB_BOOL hb_sln_UnderXterm;
extern unsigned char hb_sln_inputTab[ 256 ];

/* delay for waiting on characters after ESC key */
extern int hb_sln_escDelay;

/* *********************************************************************** */

/* to convert DeadKey+letter to national character */
extern unsigned char hb_sln_convKDeadKeys[];

/* indicates that screen size has changed */
extern volatile HB_BOOL hb_sln_bScreen_Size_Changed;

extern int hb_sln_Init_Terminal( int phase );

extern int hb_gt_sln_ReadKey( PHB_GT pGT, int iEventMask );

extern void    hb_gt_sln_mouse_Init( void );
extern void    hb_gt_sln_mouse_Exit( void );
extern HB_BOOL hb_gt_sln_mouse_IsPresent( PHB_GT pGT );
extern void    hb_gt_sln_mouse_Show( PHB_GT pGT );
extern void    hb_gt_sln_mouse_Hide( PHB_GT pGT );
extern void    hb_gt_sln_mouse_GetPos( PHB_GT pGT, int * piRow, int * piCol );
extern void    hb_gt_sln_mouse_SetPos( PHB_GT pGT, int iRow, int iCol );
extern int     hb_gt_sln_mouse_CountButton( PHB_GT pGT );
extern HB_BOOL hb_gt_sln_mouse_ButtonState( PHB_GT pGT, int iButton );

extern void    hb_gt_sln_mouse_FixTrash( void );
extern int     hb_gt_sln_mouse_Inkey( int iEventMask, HB_BOOL fCheckNew );
extern void    hb_gt_sln_mouse_ProcessTerminalEvent( void );

/* *********************************************************************** */
