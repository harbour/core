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
#include "extend.h"
#include "ctoharb.h"
#include "errno.h"
#include "objbase.h"
#include "tchar.h"
#include "stdio.h"
#include "math.h"
#include "string.h"
#include "stdlib.h"
#include ".\include\ddraw.h"
#include ".\include\ddutil.h"



    // Main Message Loop

    long _stdcall hb_dd_DDWndProc ( HWND , UINT , WPARAM , LPARAM );
    void hb_dd_g_Error			  ( char *, long , char *);
	void hb_dd_CreateWindow		  ( void );
   
    // DDraw initialize

    void HB_DD_DDRAWSTARTUP		     ( HWND		);
	void hb_dd_StartWindow		     ( void		);
	void hb_dd_CreateOffScreenBitmap ( void		);
	void hb_dd_ReleaseAllObjects     ( void		);
	void hb_dd_RestoreAll		     ( void		);
	void hb_dd_checkError		     ( HRESULT  );

	HRESULT	hb_dd_DDCopyBitmap		 ( IDirectDrawSurface4 * pdds, HBITMAP hbm, int x, int y, int dx, int dy);
	void    hb_dd_RenderSprites		 ( long  );

extern "C" DWORD hb_dd_DDColorMatch  ( IDirectDrawSurface4 * pdds, COLORREF rgb );

//-------------------------------------------------------//
// API Functions
//-------------------------------------------------------//

    void DD_MsgBox          ( void );
    void WinError           ( void );

