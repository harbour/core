/*
 * Harbour Project source code:
 * Windows communications library
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.net)
 * Copyright 2005-2009 Alex Strickland <sscc@mweb.co.za>
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

#ifndef __HBWIN_H
#define __HBWIN_H

#include "hbapi.h"
#include "hbwinuni.h"

#if defined( HB_OS_WIN )

#include <windows.h>

#define HB_WIN_COM_FUN_CREATEFILE          1
#define HB_WIN_COM_FUN_GETCOMMSTATE        2
#define HB_WIN_COM_FUN_SETCOMMSTATE        3
#define HB_WIN_COM_FUN_SETUPCOMM           4
#define HB_WIN_COM_FUN_GETCOMMTIMEOUTS     5
#define HB_WIN_COM_FUN_SETCOMMTIMEOUTS     6
#define HB_WIN_COM_FUN_CLOSEHANDLE         7
#define HB_WIN_COM_FUN_WRITEFILE           8
#define HB_WIN_COM_FUN_READFILE            9
#define HB_WIN_COM_FUN_GETCOMMMODEMSTATUS  10      /* win_com:Status() */
#define HB_WIN_COM_FUN_PURGECOMM           11
#define HB_WIN_COM_FUN_CLEARCOMMERROR      12      /* win_com:QueueStatus() */
#define HB_WIN_COM_FUN_ESCAPECOMMFUNCTION  13
#define HB_WIN_COM_FUN_GETCOMMPROPERTIES   14
#define HB_WIN_COM_FUN_MAX                 14

#define HB_WIN_COM_DBGBASIC                0x01
#define HB_WIN_COM_DBGFLOW                 0x02
#define HB_WIN_COM_DBGXTRAFLOW             0x04
#define HB_WIN_COM_DBGOTHER                0x08
#define HB_WIN_COM_DBGTIMEOUTS             0x10
#define HB_WIN_COM_DBGQUEUE                0x20
#define HB_WIN_COM_DBGALL                  0x3F

/* hbwin_bitmapType() return values */
#define HB_WIN_BITMAP_UNKNOWN              0
#define HB_WIN_BITMAP_BMP                  1
#define HB_WIN_BITMAP_JPEG                 2
#define HB_WIN_BITMAP_PNG                  3

HB_EXTERN_BEGIN

extern HB_EXPORT int hbwin_bitmapType( const void * pImgBuf, HB_SIZE size );

HB_EXTERN_END

#endif

#endif /* __HBWIN_H */
