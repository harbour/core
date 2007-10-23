/*
 * $Id: do.c 6821 2006-09-01 08:27:40Z druzus $
 */

/*
 * Copyright(C) 1999 by Jesus Salas
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR
 * PURPOSE.  See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to:
 *
 * The Free Software Foundation, Inc.,
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * You can contact me at: jsalas@gruposp.com jsalas@sp-editores.es
 *
 */

#define INITGUID

#include "windows.h"
#include "winuser.h"

#include "hbapi.h"
#include "hbvm.h"

#include "errno.h"
#include "objbase.h"
#include "tchar.h"
#include "stdio.h"
#include "math.h"
#include "string.h"
#include "stdlib.h"

#include "ddraw.h"

HB_EXTERN_BEGIN

/* Main Message Loop */

long _stdcall hb_dd_DDWndProc( HWND , UINT , WPARAM , LPARAM );

/* DDraw initialize */

void    hb_dd_DDrawStartup( HWND );
void    hb_dd_StartWindow( void );
void    hb_dd_CreateWindow( void );
void    hb_dd_CreateOffScreenBitmap( void );
void    hb_dd_ReleaseAllObjects( void );
void    hb_dd_RestoreAll( void );
long    hb_dd_checkError( HRESULT );
HRESULT hb_dd_DDCopyBitmap( IDirectDrawSurface4 * pdds, HBITMAP hbm, int x, int y, int dx, int dy);
void    hb_dd_RenderSprites( long );
DWORD   hb_dd_DDColorMatch( IDirectDrawSurface4 * pdds, COLORREF rgb );
void    hb_dd_WinError( void );
void    hb_dd_g_Error( char *, long , char *);

HB_EXTERN_END
