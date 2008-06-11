/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * GT Graphics definitions (HB_GFX_*)
 *
 * Copyright 2004 Mauricio Abre <maurifull@datafull.com>
 * www - http://www.xharbour.org
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

/* NOTE: This file is also used by C code. */

#ifndef _HBGFXDEF_CH_
#define _HBGFXDEF_CH_

/*
 * NOTE: ACQUIRE / RELEASE screen pair must work same way DispBegin()/DispEnd() pair does
 *       (that is, with an internal counter), as lots of function may want to 'acquire/release' it.
 *       However, a GT must properly manage its gfx output if the user didn't requested to acquire the
 *       screen, so this is under user choice.
 *       (the user just needs to know that it is not the same to aquire the screen, draw 100 lines, then
 *       release screen, than simply drawing 100 lines -as the GT will be acquiring/releasing the screen
 *       100 times, which will slow down things a lot-) [Mauricio]
 */

/* Misc, internals */
#define HB_GFX_ACQUIRESCREEN   1  /* Some GTs may require that you 'acquire' the screen before doing gfx things */
#define HB_GFX_RELEASESCREEN   2  /* Release a previously 'acquired' screen */
#define HB_GFX_MAKECOLOR       3  /* Calculate gfx color number based on RGBA values */
/* Functions that affect drawing area */
#define HB_GFX_CLIPTOP        10
#define HB_GFX_CLIPLEFT       11
#define HB_GFX_CLIPBOTTOM     12
#define HB_GFX_CLIPRIGHT      13
#define HB_GFX_SETCLIP        14  /* NOTE: set to 0, 0, 0, 0 to disable ;) */
/* Functions that affect drawing mode */
#define HB_GFX_DRAWINGMODE    20
/* Drawing primitives */
#define HB_GFX_GETPIXEL       50
#define HB_GFX_PUTPIXEL       51
#define HB_GFX_LINE           52
#define HB_GFX_RECT           53
#define HB_GFX_FILLEDRECT     54
#define HB_GFX_CIRCLE         55
#define HB_GFX_FILLEDCIRCLE   56
#define HB_GFX_ELLIPSE        57
#define HB_GFX_FILLEDELLIPSE  58
#define HB_GFX_FLOODFILL      59

/* Drawing mode constants */
#define HB_GFX_MODE_SOLID      1  /* Solid mode, no translucency, no patterned primitives */
#define HB_GFX_MODE_XOR        2  /* XOR with current screen contents */
#define HB_GFX_MODE_ALPHA      3  /* Use alpha for transluced effect (SLOW) */
/* TODO: add patterned mode drawings */

/* Compatibility #defines.
   These codes are deprecated, _don't use them_. Please upgrade to the above versions. 
   For developers: Don't add any more new codes to this section. */

#ifndef HB_GT_NO_XHB

#define GFX_ACQUIRESCREEN   HB_GFX_ACQUIRESCREEN
#define GFX_RELEASESCREEN   HB_GFX_RELEASESCREEN
#define GFX_MAKECOLOR       HB_GFX_MAKECOLOR
#define GFX_CLIPTOP         HB_GFX_CLIPTOP
#define GFX_CLIPLEFT        HB_GFX_CLIPLEFT
#define GFX_CLIPBOTTOM      HB_GFX_CLIPBOTTOM
#define GFX_CLIPRIGHT       HB_GFX_CLIPRIGHT
#define GFX_SETCLIP         HB_GFX_SETCLIP
#define GFX_DRAWINGMODE     HB_GFX_DRAWINGMODE
#define GFX_GETPIXEL        HB_GFX_GETPIXEL
#define GFX_PUTPIXEL        HB_GFX_PUTPIXEL
#define GFX_LINE            HB_GFX_LINE
#define GFX_RECT            HB_GFX_RECT
#define GFX_FILLEDRECT      HB_GFX_FILLEDRECT
#define GFX_CIRCLE          HB_GFX_CIRCLE
#define GFX_FILLEDCIRCLE    HB_GFX_FILLEDCIRCLE
#define GFX_ELLIPSE         HB_GFX_ELLIPSE
#define GFX_FILLEDELLIPSE   HB_GFX_FILLEDELLIPSE
#define GFX_FLOODFILL       HB_GFX_FLOODFILL
#define GFX_MODE_SOLID      HB_GFX_MODE_SOLID
#define GFX_MODE_XOR        HB_GFX_MODE_XOR
#define GFX_MODE_ALPHA      HB_GFX_MODE_ALPHA

#endif

#endif  /* _HBGFXDEF_CH_ */
