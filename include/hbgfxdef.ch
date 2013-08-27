/*
 * xHarbour Project source code:
 * GT Graphics definitions (HB_GFX_*)
 *
 * Copyright 2004 Mauricio Abre <maurifull@datafull.com>
 * www - http://www.xharbour.org
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#endif /* _HBGFXDEF_CH_ */
