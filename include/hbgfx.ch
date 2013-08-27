/*
 * xHarbour Project source code:
 * GT Graphics functions
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

#ifndef _HBGFX_CH_
#define _HBGFX_CH_

#include "hbgfxdef.ch"

/*
 * NOTE: ACQUIRE / RELEASE screen pair must work same way DispBegin()/DispEnd() pair does
 *       (that is, with an internal counter), as lots of function may want to 'acquire/release' it.
 *       However, a GT must properly manage its gfx output if the user didn't requested to acquire the
 *       screen, so this is under user choice.
 *       (the user just needs to know that it is not the same to aquire the screen, draw 100 lines, then
 *       release screen, than simply drawing 100 lines -as the GT will be acquiring/releasing the screen
 *       100 times, which will slow down things a lot-) [Mauricio]
 */

#xtranslate hb_gfxAcquireScreen() => hb_gfxPrimitive( HB_GFX_ACQUIRESCREEN )
#xtranslate hb_gfxReleaseScreen() => hb_gfxPrimitive( HB_GFX_RELEASESCREEN )
#xtranslate hb_gfxMakeColor( <nRed>, <nGreen>, <nBlue>[, <nAlpha>] ) => hb_gfxPrimitive( HB_GFX_MAKECOLOR, <nRed>, <nGreen>, <nBlue>[, <nAlpha>] )
#xtranslate hb_gfxGetClip( <nTop>, <nLeft>, <nBottom>, <nRight> ) => <nTop> := hb_gfxPrimitive( HB_GFX_CLIPTOP ); <nLeft> := hb_gfxPrimitive( HB_GFX_CLIPLEFT ); <nBottom> := hb_gfxPrimitive( HB_GFX_CLIPBOTTOM ); <nRight> := hb_gfxPrimitive( HB_GFX_CLIPRIGHT )
#xtranslate hb_gfxSetClip( <nTop>, <nLeft>, <nBottom>, <nRight> ) => hb_gfxPrimitive( HB_GFX_SETCLIP, <nTop>, <nLeft>, <nBottom>, <nRight> )
#xtranslate hb_gfxDrawingMode( [<nMode>] ) => hb_gfxPrimitive( HB_GFX_DRAWINGMODE[, <nMode>] )
#xtranslate hb_gfxGetPixel( <nY>, <nX> ) => hb_gfxPrimitive( HB_GFX_GETPIXEL, <nY>, <nX> )
#xtranslate hb_gfxPutPixel( <nY>, <nX>, <nColor> ) => hb_gfxPrimitive( HB_GFX_PUTPIXEL, <nY>, <nX>, <nColor> )
#xtranslate hb_gfxLine( <nTop>, <nLeft>, <nBottom>, <nRight>, <nColor> ) => hb_gfxPrimitive( HB_GFX_LINE, <nTop>, <nLeft>, <nBottom>, <nRight>, <nColor> )
#xtranslate hb_gfxRect( <nTop>, <nLeft>, <nBottom>, <nRight>, <nColor> ) => hb_gfxPrimitive( HB_GFX_RECT, <nTop>, <nLeft>, <nBottom>, <nRight>, <nColor> )
#xtranslate hb_gfxFilledRect( <nTop>, <nLeft>, <nBottom>, <nRight>, <nColor> ) => hb_gfxPrimitive( HB_GFX_FILLEDRECT, <nTop>, <nLeft>, <nBottom>, <nRight>, <nColor> )
#xtranslate hb_gfxCircle( <nY>, <nX>, <nRadius>, <nColor> ) => hb_gfxPrimitive( HB_GFX_CIRCLE, <nY>, <nX>, <nRadius>, <nColor> )
#xtranslate hb_gfxFilledCircle( <nY>, <nX>, <nRadius>, <nColor> ) => hb_gfxPrimitive( HB_GFX_FILLEDCIRCLE, <nY>, <nX>, <nRadius>, <nColor> )
#xtranslate hb_gfxEllipse( <nY>, <nX>, <nRadY>, <nRadX>, <nColor> ) => hb_gfxPrimitive( HB_GFX_ELLIPSE, <nY>, <nX>, <nRadY>, <nRadX>, <nColor> )
#xtranslate hb_gfxFilledEllipse( <nY>, <nX>, <nRadY>, <nRadX>, <nColor> ) => hb_gfxPrimitive( HB_GFX_FILLEDELLIPSE, <nY>, <nX>, <nRadY>, <nRadX>, <nColor> )
#xtranslate hb_gfxFloodFill( <nY>, <nX>, <nColor> ) => hb_gfxPrimitive( HB_GFX_FLOODFILL, <nY>, <nX>, <nColor> )

#endif  /* _HBHB_GFX_CH_ */
