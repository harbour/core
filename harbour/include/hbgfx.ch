/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * GT Graphics functions
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

/*
 * WARNING: this file is also included in C code, so don't add xHarbour specific stuff,
 * or protect it under #ifdef __XHARBOUR__
 *
 */

#ifndef _HBGFX_CH_
#define _HBGFX_CH_

#include "hbgfxdef.ch"

/*
 * NOTE: ACQUIRE / RELEASE screen pair must work same way DispBegin()/DispEnd() pair does
 *       (that is, with an internal counter), as lots of function may want to 'acquire/release' it.
 *
 *       However, a GT must properly manage its gfx output if the user didn't requested to acquire the
 *       screen, so this is under user choice.
 *       (the user just needs to know that it is not the same to aquire the screen, draw 100 lines, then
 *        release screen, than simply drawing 100 lines -as the GT will be acquiring/releasing the screen
 *        100 times, which will slow down things a lot-)
 *
 * Mauricio
 *
 */
#xtranslate HB_GFXACQUIRESCREEN() => hb_gfxPrimitive(GFX_ACQUIRESCREEN)
#xtranslate HB_GFXRELEASESCREEN() => hb_gfxPrimitive(GFX_RELEASESCREEN)
#xtranslate HB_GFXMAKECOLOR(<nRed>, <nGreen>, <nBlue>[, <nAlpha>]) => hb_gfxPrimitive(GFX_MAKECOLOR, <nRed>, <nGreen>, <nBlue>[, <nAlpha>])
#xtranslate HB_GFXGETCLIP(<nTop>, <nLeft>, <nBottom>, <nRight>) => <nTop> := hb_gfxPrimitive(GFX_CLIPTOP); <nLeft> := hb_gfxPrimitive(GFX_CLIPLEFT); <nBottom> := hb_gfxPrimitive(GFX_CLIPBOTTOM); <nRight> := hb_gfxPrimitive(GFX_CLIPRIGHT)
#xtranslate HB_GFXSETCLIP(<nTop>, <nLeft>, <nBottom>, <nRight>) => hb_gfxPrimitive(GFX_SETCLIP, <nTop>, <nLeft>, <nBottom>, <nRight>)
#xtranslate HB_GFXDRAWINGMODE([<nMode>]) => hb_gfxPrimitive(GFX_DRAWINGMODE[, <nMode>])
#xtranslate HB_GFXGETPIXEL(<nY>, <nX>) => hb_gfxPrimitive(GFX_GETPIXEL, <nY>, <nX>)
#xtranslate HB_GFXPUTPIXEL(<nY>, <nX>, <nColor>) => hb_gfxPrimitive(GFX_PUTPIXEL, <nY>, <nX>, <nColor>)
#xtranslate HB_GFXLINE(<nTop>, <nLeft>, <nBottom>, <nRight>, <nColor>) => hb_gfxPrimitive(GFX_LINE, <nTop>, <nLeft>, <nBottom>, <nRight>, <nColor>)
#xtranslate HB_GFXRECT(<nTop>, <nLeft>, <nBottom>, <nRight>, <nColor>) => hb_gfxPrimitive(GFX_RECT, <nTop>, <nLeft>, <nBottom>, <nRight>, <nColor>)
#xtranslate HB_GFXFILLEDRECT(<nTop>, <nLeft>, <nBottom>, <nRight>, <nColor>) => hb_gfxPrimitive(GFX_FILLEDRECT, <nTop>, <nLeft>, <nBottom>, <nRight>, <nColor>)
#xtranslate HB_GFXCIRCLE(<nY>, <nX>, <nRadius>, <nColor>) => hb_gfxPrimitive(GFX_CIRCLE, <nY>, <nX>, <nRadius>, <nColor>)
#xtranslate HB_GFXFILLEDCIRCLE(<nY>, <nX>, <nRadius>, <nColor>) => hb_gfxPrimitive(GFX_FILLEDCIRCLE, <nY>, <nX>, <nRadius>, <nColor>)
#xtranslate HB_GFXELLIPSE(<nY>, <nX>, <nRadY>, <nRadX>, <nColor>) => hb_gfxPrimitive(GFX_ELLIPSE, <nY>, <nX>, <nRadY>, <nRadX>, <nColor>)
#xtranslate HB_GFXFILLEDELLIPSE(<nY>, <nX>, <nRadY>, <nRadX>, <nColor>) => hb_gfxPrimitive(GFX_FILLEDELLIPSE, <nY>, <nX>, <nRadY>, <nRadX>, <nColor>)
#xtranslate HB_GFXFLOODFILL(<nY>, <nX>, <nColor>) => hb_gfxPrimitive(GFX_FLOODFILL, <nY>, <nX>, <nColor>)

#endif  /* _HBGFX_CH_ */
