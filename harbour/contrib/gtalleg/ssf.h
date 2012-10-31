/*
 * $Id$
 */

/*
 *
 * This file was conceived while I was developing an allegro based gt
 * (gtAlleg) for xHarbour, so it is brought under the same license terms.
 *
 * Mauricio Abre
 *
 */

/*
 * xHarbour Project source code:
 * Simple Scalable Font library, main header file.
 *
 * Copyright 2004 Mauricio Abre <maurifull@datafull.com>
 * www - http: (yet to be constructed...)
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

#ifndef _SSF_H_
#define _SSF_H_

#if defined( __BORLANDC__ )
   #define ALLEGRO_NO_ASM
   #define ALLEGRO_BCC32
#elif defined( _MSC_VER )
   #if defined( _WIN64 )
      #define ALLEGRO_NO_ASM
   #endif
   #define ALLEGRO_MSVC
#elif defined( __MINGW32__ ) || defined( __CYGWIN__ )
   #define ALLEGRO_MINGW32
   #if defined( __CYGWIN__ )
      #define SCAN_DEPEND
   #endif
#elif defined( __DMC__ )
   #define ALLEGRO_DMC
#elif defined( __DJGPP__ )
   #define ALLEGRO_DJGPP
#elif defined( __WATCOMC__ )
   #define ALLEGRO_WATCOM
#elif defined( HB_OS_DARWIN )
   #define ALLEGRO_MACOSX
#endif

#include <allegro.h>

/* Hack to use old Allegro branches */
#ifndef AL_GFX_NONE
#define AL_GFX_NONE                 GFX_NONE
#define AL_GFX_SAFE                 GFX_SAFE
#ifdef GFX_XWINDOWS
#define AL_GFX_XDGA                 GFX_XDGA
#define AL_GFX_XDGA2                GFX_XDGA2
#define AL_GFX_XWINDOWS             GFX_XWINDOWS
#endif
#define AL_GFX_VBEAF                GFX_VBEAF
#ifdef GFX_FBCON
#define AL_GFX_FBCON                GFX_FBCON
#endif
#define AL_GFX_AUTODETECT_WINDOWED  GFX_AUTODETECT_WINDOWED
#define AL_GFX_AUTODETECT           GFX_AUTODETECT
#ifdef GFX_GDI
#define AL_GFX_GDI                  GFX_GDI
#define AL_GFX_DIRECTX_WIN          GFX_DIRECTX_WIN
#endif
#define AL_BITMAP                   BITMAP
#define AL_SCREEN_W                 SCREEN_W
#define AL_SCREEN_H                 SCREEN_H
#define AL_KEY_ESC                  KEY_ESC
#define AL_KEY_INSERT               KEY_INSERT
#define AL_KEY_HOME                 KEY_HOME
#define AL_KEY_PGUP                 KEY_PGUP
#define AL_KEY_PGDN                 KEY_PGDN
#define AL_KEY_END                  KEY_END
#define AL_KEY_DEL                  KEY_DEL
#define AL_KEY_UP                   KEY_UP
#define AL_KEY_DOWN                 KEY_DOWN
#define AL_KEY_LEFT                 KEY_LEFT
#define AL_KEY_RIGHT                KEY_RIGHT
#define AL_KEY_QUOTE                KEY_QUOTE
#define AL_KEY_A                    KEY_A
#define AL_KEY_B                    KEY_B
#define AL_KEY_C                    KEY_C
#define AL_KEY_D                    KEY_D
#define AL_KEY_E                    KEY_E
#define AL_KEY_F                    KEY_F
#define AL_KEY_G                    KEY_G
#define AL_KEY_H                    KEY_H
#define AL_KEY_I                    KEY_I
#define AL_KEY_J                    KEY_J
#define AL_KEY_K                    KEY_K
#define AL_KEY_L                    KEY_L
#define AL_KEY_M                    KEY_M
#define AL_KEY_N                    KEY_N
#define AL_KEY_O                    KEY_O
#define AL_KEY_P                    KEY_P
#define AL_KEY_Q                    KEY_Q
#define AL_KEY_R                    KEY_R
#define AL_KEY_S                    KEY_S
#define AL_KEY_T                    KEY_T
#define AL_KEY_U                    KEY_U
#define AL_KEY_V                    KEY_V
#define AL_KEY_W                    KEY_W
#define AL_KEY_X                    KEY_X
#define AL_KEY_Y                    KEY_Y
#define AL_KEY_Z                    KEY_Z
#define AL_KEY_F1                   KEY_F1
#define AL_KEY_F2                   KEY_F2
#define AL_KEY_F3                   KEY_F3
#define AL_KEY_F4                   KEY_F4
#define AL_KEY_F5                   KEY_F5
#define AL_KEY_F6                   KEY_F6
#define AL_KEY_F7                   KEY_F7
#define AL_KEY_F8                   KEY_F8
#define AL_KEY_F9                   KEY_F9
#define AL_KEY_F10                  KEY_F10
#define AL_KEY_F11                  KEY_F11
#define AL_KEY_F12                  KEY_F12
#define al_desktop_color_depth      desktop_color_depth
#define al_set_color_depth          set_color_depth
#define al_bitmap_color_depth       bitmap_color_depth
#define al_get_desktop_resolution   get_desktop_resolution
#define al_install_timer            install_timer
#define al_screen                   screen
#define al_set_gfx_mode             set_gfx_mode
#define al_set_window_title         set_window_title
#define al_text_mode                text_mode
#define al_make_color               makecol
#define al_key_shifts               key_shifts
#define al_install_keyboard         install_keyboard
#define al_keyboard_needs_poll      keyboard_needs_poll
#define al_poll_keyboard            poll_keyboard
#define al_key_pressed              keypressed
#define al_read_key                 readkey
#define al_set_keyboard_leds        set_leds
#define al_install_mouse            install_mouse
#define al_show_mouse               show_mouse
#define al_mouse_needs_poll         mouse_needs_poll
#define al_poll_mouse               poll_mouse
#define al_mouse_x                  mouse_x
#define al_mouse_y                  mouse_y
#define al_mouse_b                  mouse_b
#define al_scare_mouse              scare_mouse
#define al_scare_mouse_area         scare_mouse_area
#define al_unscare_mouse            unscare_mouse
#define al_position_mouse           position_mouse
#define al_set_mouse_range          set_mouse_range
#define al_create_bitmap            create_bitmap
#define al_create_system_bitmap     create_system_bitmap
#define al_destroy_bitmap           destroy_bitmap
#define al_acquire_screen           acquire_screen
#define al_release_screen           release_screen
#define al_acquire_bitmap           acquire_bitmap
#define al_release_bitmap           release_bitmap
#define al_drawing_mode             drawing_mode
#define al_get_pixel                getpixel
#define al_put_pixel                putpixel
#define al_draw_line                line
#define al_draw_vline               vline
#define al_draw_hline               hline
#define al_draw_rect                rect
#define al_draw_rect_fill           rectfill
#define al_draw_spline              spline
#define al_draw_circle              circle
#define al_draw_circle_fill         circlefill
#define al_draw_ellipse             ellipse
#define al_draw_ellipse_fill        ellipsefill
#define al_floodfill                floodfill
#define al_draw_triangle            triangle
#define al_blit                     blit
#define al_clear_to_color           clear_to_color
#ifndef al_set_clip
#if ALLEGRO_SUB_VERSION < 2
#define al_set_clip                 set_clip
#else
#define al_set_clip                 set_clip_rect
#endif
#endif
#endif

typedef enum
{
   SSF_NONE,
   SSF_LINE,
   SSF_BOX,
   SSF_SPLINE1,
   SSF_SPLINE2,
   SSF_TRIANGLE,
   SSF_COLOR
} ssfType;

#ifndef SSF_MAXFRAMES
#define SSF_MAXFRAMES  128
#endif

#define THICK_LEFT     0
#define THICK_UP       1
#define THICK_RIGHT    2
#define THICK_DOWN     3

typedef struct _ssfFrame
{
   char ftype;
   unsigned short left, top, right, bottom, thick;
   unsigned short thickdir;
} ssfFrame;

typedef struct _ssfGlyph
{
   int      num;
   ssfFrame frames[ SSF_MAXFRAMES ];
} ssfGlyph;

typedef struct _ssfFont
{
   unsigned short    fsize;
   const ssfGlyph ** chars;
} ssfFont;

extern void ssfCreateThinFont( ssfFont * sfont );
extern void ssfSetFontSize( ssfFont * sfont, unsigned short fsize );
extern unsigned short ssfDrawChar( AL_BITMAP * dst, ssfFont * sfont, char c, int x, int y, int color );
extern int ssfDrawText( AL_BITMAP * dst, ssfFont * sfont, const char * s, int x, int y, int color );

#endif  /* _SSF_H_ */
