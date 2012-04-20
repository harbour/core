/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for Clipper Tools like window system
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#ifndef HB_CTWIN_H_
#define HB_CTWIN_H_

#define HB_CTW_UNDEF          -1
#define HB_CTW_HIDDEN         0
#define HB_CTW_VISIBLE        1

#define HB_CTW_SHADOW_OFF     -1
#define HB_CTW_SHADOW_UNDEF   -2
#define HB_CTW_SHADOW_EXT     -3
#define HB_CTW_SHADOW_EXT2    -4

#define HB_CTW_BOTTOM         0
#define HB_CTW_DEFAULT        1
#define HB_CTW_TOP            2

HB_EXTERN_BEGIN

extern HB_EXPORT HB_BOOL hb_ctwInit( void );
extern HB_EXPORT int     hb_ctwSetShadowAttr( int iAttr );
extern HB_EXPORT int     hb_ctwSetMoveMode( int iMode );
extern HB_EXPORT int     hb_ctwSetMoveStep( int iVertical, int iHorizontal );
extern HB_EXPORT int     hb_ctwSetWindowBoard( int iTop, int iLeft, int iBottom, int iRight );
extern HB_EXPORT int     hb_ctwSetBorderMode( int iTop, int iLeft, int iBottom, int iRight );
extern HB_EXPORT int     hb_ctwCreateWindow( int iTop, int iLeft, int iBottom, int iRight, HB_BOOL fClear, int iColor, HB_BOOL fVisible );
extern HB_EXPORT int     hb_ctwCloseAllWindows( void );
extern HB_EXPORT int     hb_ctwCloseWindow( int iWindow );
extern HB_EXPORT int     hb_ctwCurrentWindow( void );
extern HB_EXPORT int     hb_ctwSelectWindow( int iWindow, HB_BOOL fToTop );
extern HB_EXPORT int     hb_ctwVisible( int iWindow, int iVisible );
extern HB_EXPORT int     hb_ctwSetWindowShadow( int iWindow, int iAttr );
extern HB_EXPORT int     hb_ctwSetWindowLevel( int iWindow, int iLevel );
extern HB_EXPORT int     hb_ctwMaxWindow( void );
extern HB_EXPORT int     hb_ctwChangeMargins( int iWindow, int iTop, int iLeft, int iBottom, int iRight );
extern HB_EXPORT int     hb_ctwSetWindowClip( int iWindow, int iTop, int iLeft, int iBottom, int iRight );
extern HB_EXPORT int     hb_ctwGetWindowCords( int iWindow, HB_BOOL fCenter, int * piTop, int * piLeft, int * piBottom, int * piRight );
extern HB_EXPORT int     hb_ctwGetFormatCords( int iWindow, HB_BOOL fRelative, int * piTop, int * piLeft, int * piBottom, int * piRight );
extern HB_EXPORT int     hb_ctwMoveWindow( int iWindow, int iRow, int iCol );
extern HB_EXPORT int     hb_ctwCenterWindow( int iWindow, HB_BOOL fCenter );
extern HB_EXPORT int     hb_ctwAddWindowBox( int iWindow, const HB_WCHAR * szBoxW, int iColor );
extern HB_EXPORT int     hb_ctwSwapWindows( int iWindow1, int iWindow2 );
extern HB_EXPORT int     hb_ctwGetPosWindow( int iRow, int iCol );
extern HB_EXPORT int     hb_ctwLastKey( int * piNewKey );

HB_EXTERN_END

#endif /* HB_CTWIN_H_ */
