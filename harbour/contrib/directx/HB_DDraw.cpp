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
#include "HB_DDraw.h"


    BOOL hb_dd_g_handling_events=FALSE;   // painting?

    #define HB_DD_TIMER_ID            1
	#define HB_DD_TIMER_RATE          100

	// DDRAW MANAGEMENT
	// Global Data

	LPDIRECTDRAW4               hb_dd_g_pDD = NULL;        // DirectDraw object
    HWND                        hb_dd_g_DDHwnd;            // Our hWnd

    long hb_dd_g_xWindow=0;
	long hb_dd_g_yWindow=0;

//------------------------------------------------------------------------------//
//                     Management Structs for surfaces...

	#define MAX_DDSURFACES 255

	LPDIRECTDRAWSURFACE4 hb_dd_g_DDSFaces[ MAX_DDSURFACES + 1 ];

	long hb_dd_g_DDSFaceCount=0;

//------------------------------------------------------------------------------//
//                          Sprites Management

	#define MAX_SPRITES 256

    struct st_Sprites
	{
		short int Type;			   // Type of Data
		long Surface;			   // Surface index for loaded Image(s)
		char * cName;			   // Sprite friendly Name
        //  2D Engine data
		long Width;				   // width of one frame
		long Height;			   // height of one frame
		long Images;			   // Number of frames in Surface
        short int Visible;         // Render it and do Hit detect?
		long Frame;				   // Actual Frame to Render
		long zOrder;			   // For painting
		long x;				       // x in Virtual Screen
		long y;				       // y in Virtual Screen
		long VisibleX;			   // "real" y in Visible Screen
		long VisibleY;			   // "real" x in Visible Screen
		long FrameSpeed;	       // Render frame time ratio
		long DrawInverted;	       // Draw Inverted
		long Direction;			   // Clock wise code 1-2-3-4-5-6-7-8-9
		long xIncrement;		   // x Increment Increment;
		long yIncrement;		   // y Increment
		long Solid;				   // is Solid object ( collision on )
        long Masked;               // is Masked?
        short int lCollision;      // Collision Detect On/Off

		// Sprite Event Handlers...

		char * OnRender;		   // Render event Handler
		char * OnCollision;		   // Collision Event Handler
		char * OnFirstFrame;	   // First Frame handler
		char * OnOutScreen;	       // Out of Bounds...

	}hb_dd_Sprites[MAX_SPRITES];

    long hb_dd_g_SpritesCount=0;

//------------------------------------------------------------------//
//                  Multi-Key control array.

	short int hb_dd_g_KeyDown[256];

    HARBOUR HB_DD_ISKEYPRESSED( void )
	{
		long nKey = hb_parnl( 1 );
		if ( hb_dd_g_KeyDown[ nKey ] )
			 hb_retl( 1 );
		else
			 hb_retl( 0 );
	}

//------------------------------------------------------------------//

	HARBOUR HB_DD_SPGETXY( void )
	{

		// This function is Broken ( hb_stornl fail );

		long n =hb_parnl( 1 );

		hb_reta( 2 );

		hb_stornl( hb_dd_Sprites[ n ].x, -1, 0  );
		hb_stornl( hb_dd_Sprites[ n ].y, -1, 1  );
	}

	HARBOUR HB_DD_SPGETX( void )
	{

		long n =hb_parnl( 1 );
		hb_retnl( hb_dd_Sprites[ n ].x );
	}

	HARBOUR HB_DD_SPGETY( void )
	{

		long n =hb_parnl( 1 );
		hb_retnl( hb_dd_Sprites[ n ].y );
	}

//------------------------------------------------------------------//

	HARBOUR HB_DD_SPGETVISIBLE( void )
	{
  	   hb_retl( hb_dd_Sprites[ hb_parnl( 1 ) ].Visible );
	}

//------------------------------------------------------------------//

	void HB_DD_SPSETVISIBLE( void )
	{
		long n = hb_parnl( 1 );
		hb_dd_Sprites[ n ].Visible = hb_parl( 2 );
	}

//------------------------------------------------------------------//

	HARBOUR HB_DD_SPONRENDER( void )
	{
		long n = hb_parnl( 1 );
		hb_dd_Sprites[ n ].OnRender = strdup( hb_parc(1) );
	}

//------------------------------------------------------------------//

	HARBOUR HB_DD_SPCLEARDIRECTION( void )
	{
		long n = hb_parnl( 1 );
		hb_dd_Sprites[ n ].xIncrement =  0;
		hb_dd_Sprites[ n ].yIncrement =  0;
		hb_dd_Sprites[ n ].Direction  = -1;
	}

//------------------------------------------------------------------//

	HARBOUR HB_DD_SPSETSOLID( void )
	{
		long n =hb_parnl( 1 );
		hb_dd_Sprites[hb_parnl( 1 ) ].Solid = hb_parl( 2 );
	}

//------------------------------------------------------------------//

	HARBOUR HB_DD_SPSETMASKED( void  )
	{
		long n =hb_parnl( 1 );
		hb_dd_Sprites[hb_parnl( 1 ) ].Masked = hb_parl( 2 );
	}

//------------------------------------------------------------------//

	HARBOUR HB_DD_SPSETDIRECTION( void )
	{
		long n		   =hb_parnl( 1 );

		hb_dd_Sprites[ n ].Direction   =hb_parnl( 2 );
		hb_dd_Sprites[ n ].xIncrement +=hb_parnl( 3 );
		hb_dd_Sprites[ n ].yIncrement +=hb_parnl( 4 );
	}

//------------------------------------------------------------------//

	HARBOUR HB_DD_SPONFIRSTFRAME( void )
	{
		long n =hb_parnl( 1 );
		hb_dd_Sprites[ n ].OnFirstFrame = strdup( hb_parc(2) );
	}

//------------------------------------------------------------------//

	HARBOUR HB_DD_SPONOUTSCREEN( void )
	{
		long n =hb_parnl( 1 );
		hb_dd_Sprites[ n ].OnOutScreen = strdup( hb_parc(2) );
	}

//------------------------------------------------------------------//

	HARBOUR HB_DD_SPONCOLLISION( void )
	{
		long n =hb_parnl( 1 );
		hb_dd_Sprites[ n ].OnCollision = strdup( hb_parc(2) );
		hb_dd_Sprites[ n ].lCollision = 1;
	}

//------------------------------------------------------------------//


	HARBOUR HB_DD_CREATESPRITE( void )
	{
		long n = hb_dd_g_SpritesCount;
		
		ZeroMemory(  &hb_dd_Sprites[ n ], sizeof( struct st_Sprites ) );

		hb_dd_Sprites[ n ].Surface	  = hb_parnl(1);
		hb_dd_Sprites[ n ].cName	  = strdup( hb_parc(2) );
		hb_dd_Sprites[ n ].Width	  = hb_parnl(3);
		hb_dd_Sprites[ n ].Height	  = hb_parnl(4);
		hb_dd_Sprites[ n ].Images	  = hb_parnl(5);
		hb_dd_Sprites[ n ].zOrder	  = hb_parnl(6);
		hb_dd_Sprites[ n ].Visible	  = hb_parl(7);
		hb_dd_Sprites[ n ].FrameSpeed =hb_parnl(8);

		hb_dd_g_SpritesCount++;
		
		hb_retnl( n );
	}

//------------------------------------------------------------------//

	HARBOUR HB_DD_SPSETXY( void )
	{
		long n =hb_parnl( 1 );
		long x =hb_parnl( 2 );
		long y =hb_parnl( 3 );
		hb_dd_Sprites[ n ].x = x;
		hb_dd_Sprites[ n ].y = y;
	}

//------------------------------------------------------------------//

	HARBOUR DD_SPSETINVERTED( void )
	{
		LPDIRECTDRAWSURFACE4 pdds;
		long n =hb_parnl(1);
		RECT rt;
		DDBLTFX todo;

		long dir =hb_parnl( 2 );
		if ( dir != hb_dd_Sprites[ n ].DrawInverted )
		{
			 rt.top   = 0;
			 rt.left   = 0;
			 rt.bottom= hb_dd_Sprites[ n ].Height;
			 rt.right = hb_dd_Sprites[ n ].Width * hb_dd_Sprites[ n ].Images;
			 ZeroMemory( &todo, sizeof( DDBLTFX ) );

			 hb_dd_Sprites[ n ].DrawInverted = dir;
			 todo.dwSize = sizeof( DDBLTFX );
			 todo.dwDDFX =  DDBLTFX_MIRRORLEFTRIGHT;
			 pdds = hb_dd_g_DDSFaces[ hb_dd_Sprites[ n ].Surface ];
	 		 pdds->Blt(&rt, pdds, &rt, DDBLT_DDFX ,&todo );
		}
	}


//------------------------------------------------------------------//

	HARBOUR HB_DD_MSGBOX( void )
	{
		char buff[20]="";
		long dwVal=0;

		char *m1;
		char *m2;
        char deftitle [100] = "";
		char defmsg   [2]   = "";

		if ( hb_pcount() > 0 )
			 m1 = hb_parc( 1 );
		else
			 m1 = defmsg;

		if ( hb_pcount() > 1 )
			 m2 = hb_parc( 2 );
		else
			 m2 = deftitle;

	    MessageBox( hb_dd_g_DDHwnd, m1, m2, NULL);

	}

//------------------------------------------------------------------//

    void hb_dd_WinError( void )
	{
		LPVOID lpMsgBuf;FormatMessage(     FORMAT_MESSAGE_ALLOCATE_BUFFER |
	    FORMAT_MESSAGE_FROM_SYSTEM |     FORMAT_MESSAGE_IGNORE_INSERTS,    NULL,
	    GetLastError(),
	    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
	    (LPTSTR) &lpMsgBuf,    0,    NULL );// Process any inserts in lpMsgBuf.
	    // ...// Display the string.
	    MessageBox( NULL, (LPCTSTR)lpMsgBuf, "Error", MB_OK | MB_ICONINFORMATION );
	    // Free the buffer.
	    LocalFree( lpMsgBuf );

	}

//------------------------------------------------------------------//

	extern "C" DWORD hb_dd_DDColorMatch(IDirectDrawSurface4 * pdds, COLORREF rgb)
	{
		COLORREF                rgbT;
		HDC                     hdc;
		DWORD                   dw = CLR_INVALID;
		DDSURFACEDESC2          ddsd;
		HRESULT                 hres;
	
		//
		//  Use GDI SetPixel to color match for us
		//
		if (rgb != CLR_INVALID && pdds->GetDC(&hdc) == DD_OK)
		{
			rgbT = GetPixel(hdc, 0, 0);     // Save current pixel value
			SetPixel(hdc, 0, 0, rgb);       // Set our value
			pdds->ReleaseDC(hdc);
	    }
	    //
	    // Now lock the surface so we can read back the converted color
	    //
	    ddsd.dwSize = sizeof(ddsd);
	    while ((hres = pdds->Lock(NULL, &ddsd, 0, NULL)) == DDERR_WASSTILLDRAWING)
	        ;
		if (hres == DD_OK)
		{
			dw = *(DWORD *) ddsd.lpSurface;                 // Get DWORD
			if (ddsd.ddpfPixelFormat.dwRGBBitCount < 32)
				dw &= (1 << ddsd.ddpfPixelFormat.dwRGBBitCount) - 1;  // Mask it to bpp
	        pdds->Unlock(NULL);
		}
		//
		//  Now put the color that was there back.
		//
		if (rgb != CLR_INVALID && pdds->GetDC(&hdc) == DD_OK)
		{
			SetPixel(hdc, 0, 0, rgbT);
	        pdds->ReleaseDC(hdc);
	    }

	    return dw;
	}


//------------------------------------------------------------------//

	void hb_dd_RenderSprites( long control )
	{

	HRESULT hRet;
	long cont,cont2;
	RECT rcRect;
	LPDIRECTDRAWSURFACE4        pdds;
	long o =0 ;
	PSYMBOL pVMCall; 
	PDYNSYM pDynSym;

	pDynSym = hb_FindDynSym( "DDONRENDER" );

    if( pDynSym )
	{
       pVMCall = pDynSym->pSymbol;
	   PushSymbol( pVMCall );
	   PushNil();
	   Do( 0 );
	}


	for( cont=0; cont<hb_dd_g_SpritesCount; cont++)
	{
		if ( hb_dd_Sprites[ cont ].Visible && hb_dd_Sprites[ cont ].Surface != - 1)
		{
			if ( hb_dd_Sprites[ cont ].Direction != -1 )
			{
				hb_dd_Sprites[ cont ].x += hb_dd_Sprites[ cont ].xIncrement ;
				hb_dd_Sprites[ cont ].y += hb_dd_Sprites[ cont ].yIncrement ;
			}

			// Collision detect

			if ( hb_dd_Sprites[ cont ].Solid  )
				 for( cont2=0;cont2 <  hb_dd_g_SpritesCount; cont2++ )
					 if ( cont != cont2 && hb_dd_Sprites[ cont2 ].Solid && hb_dd_Sprites[ cont2 ].Visible  )
					 {
						RECT r1,r2,res;
						r1.left   = hb_dd_Sprites[ cont ].x;
						r1.right  = hb_dd_Sprites[ cont ].x + hb_dd_Sprites[ cont ].Width;
						r1.top    = hb_dd_Sprites[ cont ].y;
						r1.bottom = hb_dd_Sprites[ cont ].y + hb_dd_Sprites[ cont ].Height ;

						r2.left   = hb_dd_Sprites[ cont2 ].x;
						r2.right  = hb_dd_Sprites[ cont2 ].x + hb_dd_Sprites[ cont2 ].Width;
						r2.top    = hb_dd_Sprites[ cont2 ].y;
						r2.bottom = hb_dd_Sprites[ cont2 ].y + hb_dd_Sprites[ cont2 ].Height ;

						if ( IntersectRect( &res,&r1,&r2 ) )
						{
							if ( hb_dd_Sprites[ cont ].lCollision )
							{
								 if (  hb_dd_Sprites[ cont ].OnCollision != NULL )
								 {
									   pDynSym = hb_FindDynSym( hb_dd_Sprites[ cont ].OnCollision );

									   if( pDynSym )
									   {
								           pVMCall = pDynSym->pSymbol;
									       PushSymbol( pVMCall );
									       PushNil();
									       PushLong( cont );
									       PushLong( cont2 );
									       Do( 2 );
									   }
								 }

							}
						}

					 }

			// Sequencer

			if ( hb_dd_Sprites[ cont ].Frame > hb_dd_Sprites[ cont ].Images )
			{

				 if ( hb_dd_Sprites[ cont ].OnFirstFrame != NULL )
				 {
					pDynSym = hb_FindDynSym( hb_dd_Sprites[ cont ].OnFirstFrame );
				    if( pDynSym )
					{
						pVMCall = pDynSym->pSymbol;
						PushSymbol( pVMCall );
						PushNil();
						Do( 0 );
					}
				 }
				 hb_dd_Sprites[ cont ].Frame = 1;
			}


			rcRect.left = ( hb_dd_Sprites[ cont ].Frame - 1 ) * hb_dd_Sprites[ cont ].Width;
			rcRect.top = 0;
			rcRect.right = ( hb_dd_Sprites[ cont ].Frame - 1 ) * hb_dd_Sprites[ cont ].Width + hb_dd_Sprites[ cont ].Width;
			rcRect.bottom = hb_dd_Sprites[ cont ].Height;

			if ( control == 1 )
			     hb_dd_Sprites[ cont ].Frame++;

			pdds = hb_dd_g_DDSFaces[ hb_dd_Sprites[ cont ].Surface ];

			while (TRUE)
			{

				if ( ! hb_dd_Sprites[ cont ].Masked  )
				    hRet = hb_dd_g_DDSFaces[1]->BltFast(hb_dd_Sprites[ cont ].x, hb_dd_Sprites[ cont ].y, pdds, &rcRect, FALSE );
				else
				{
					DDCOLORKEY ddck;

					ddck.dwColorSpaceLowValue = hb_dd_DDColorMatch( pdds, CLR_INVALID );
					ddck.dwColorSpaceHighValue = ddck.dwColorSpaceLowValue;

					pdds->SetColorKey( DDCKEY_SRCBLT   ,&ddck );

					hb_dd_g_DDSFaces[1]->SetColorKey( DDCKEY_DESTBLT ,&ddck );

					hRet = hb_dd_g_DDSFaces[1]->BltFast(hb_dd_Sprites[ cont ].x, hb_dd_Sprites[ cont ].y, pdds, &rcRect, DDBLTFAST_SRCCOLORKEY  );
				}
				hb_dd_checkError( hRet );
				if (hRet == DD_OK)
				    break;
				if (hRet == DDERR_SURFACELOST)
				{
					hRet = hb_dd_g_DDSFaces[1]->Restore();
					if (hRet != DD_OK)
					    break;
				}
				if (hRet != DDERR_WASSTILLDRAWING)
					break;
			}

			if (  hb_dd_Sprites[ cont ].OnOutScreen != NULL )
			{
				if ( hb_dd_Sprites[ cont ].x < 0 || hb_dd_Sprites[ cont ].y < 0 || hb_dd_Sprites[ cont ].x > 640 || hb_dd_Sprites[ cont ].y > 480 )
				{
					 if (  hb_dd_Sprites[ cont ].OnOutScreen != NULL )
					 {
						 pDynSym = hb_FindDynSym( hb_dd_Sprites[ cont ].OnOutScreen );

						 if( pDynSym )
						 {
							 pVMCall = pDynSym->pSymbol;
							 PushSymbol( pVMCall );
							 PushNil();
							 PushLong( cont );
						     Do( 1 );
						 }
					 }


					//SetParam( ( void * ) &cont , TSI_DWORD );
	  		        //VMCall( hb_dd_Sprites[ cont ].OnOutScreen );
				}
			}

		}
	}
}

//------------------------------------------------------------------//



   long _stdcall hb_dd_DDWndProc( HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam )
   {
	   HRESULT hRet;
 	   int   nVirtKey;

	switch( uMsg )
	{
		case WM_DESTROY:
             hb_dd_ReleaseAllObjects();
			 break;
		case WM_KEYUP:
			{
			 nVirtKey= (int) wParam;    // virtual-key code
			 hb_dd_g_KeyDown[ nVirtKey ] = 0;
			 break;
			}
		case WM_KEYDOWN:
			{
			 nVirtKey = (int) wParam;    // virtual-key code
			 hb_dd_g_KeyDown[ nVirtKey ] = 1;

			 if ( ( int ) wParam == 27 )
			 {
				 PostMessage( hWnd, WM_CLOSE,0,0);
				 PostMessage( hWnd, WM_DESTROY,0,0);
			 }
			 break;
			}
  case WM_TIMER:
            if ( HB_DD_TIMER_ID == wParam)
            {
				if ( ! hb_dd_g_handling_events )
				{

				hb_dd_g_handling_events = TRUE;
				hb_dd_RenderSprites(1);
                while (TRUE)
                {
                    hRet = hb_dd_g_DDSFaces[0]->Flip(NULL, 0);
                    if (hRet == DD_OK)
                        break;
                    if (hRet == DDERR_SURFACELOST)
                    {
                        hRet = hb_dd_g_DDSFaces[0]->Restore();
                        if (hRet != DD_OK)
                            break;
                    }
                    if (hRet != DDERR_WASSTILLDRAWING)
                        break;
                }
				hb_dd_g_handling_events = FALSE;
				}
            }
            break;
	}

    return DefWindowProc( hWnd, uMsg, wParam , lParam );
   }

//------------------------------------------------------------------//


	HARBOUR HB_DD_CREATEWINDOW( void )
	{

       HWND m_hWnd = NULL;
	   HINSTANCE m_hInstance = GetModuleHandle(NULL);
	   long x,y;

	   x= hb_pcount() > 10 ?hb_parnl( 1 ) : GetSystemMetrics(SM_CXSCREEN);
	   y= hb_pcount() > 11 ?hb_parnl( 2 ) : GetSystemMetrics(SM_CYSCREEN);

	   hb_dd_g_xWindow = x;
	   hb_dd_g_yWindow = y;


	   WNDCLASS wndClass = { CS_HREDRAW | CS_VREDRAW, hb_dd_DDWndProc, 0, 0, m_hInstance,
                             NULL,
                             LoadCursor(NULL, IDC_ARROW),
                             (HBRUSH)GetStockObject(BLACK_BRUSH),
				   		     NULL,
                             TEXT("4dNow") };

       RegisterClass( &wndClass );

       m_hWnd = CreateWindow( TEXT("4dNow"), TEXT("4dNow"),
                                WS_POPUP, 0,
                                0, x,y, NULL, NULL, m_hInstance, 0L );
	   
       if ( !m_hWnd )
	        hb_dd_WinError();

	   ShowWindow( m_hWnd,SW_SHOWMAXIMIZED );

	   HB_DD_DDRAWSTARTUP( m_hWnd );
       hb_retnl( ( long ) m_hWnd );
	}

//------------------------------------------------------------------//

	hb_dd_InitFail(HWND hWnd, HRESULT hRet, LPCTSTR szError,...)
	{
		char                        szBuff[128];
		va_list                     vl;
		va_start(vl, szError);
		vsprintf(szBuff, szError, vl);
		MessageBox(hWnd, szBuff, "4dNow extend sys.", MB_OK);
		DestroyWindow(hWnd);
		va_end(vl);
		exit(1);
		return hRet;
	}


//------------------------------------------------------------------//

	HARBOUR HB_DD_LOADBMPINTOSURFACE( void )
	{
		long   nSurface = hb_parnl( 1 );
		char * cBitmap	= hb_parc ( 2 );
		long   x	    = hb_parnl( 3 );
		long   y	    = hb_parnl( 4 );
		long   dx	    = hb_parnl( 5 );
		long   dy	    = hb_parnl( 6 );
		

		HBITMAP hbm;

	    // Load our bitmap resource.


	    hbm = (HBITMAP) LoadImage(GetModuleHandle(NULL), cBitmap, IMAGE_BITMAP, 0,
                              0, LR_CREATEDIBSECTION | LR_LOADFROMFILE);

		if (hbm == NULL)
			hb_dd_g_Error( "Can't load Bitmap.",100,cBitmap );

		if ( hb_dd_g_DDSFaces[ nSurface ] == NULL )
			 hb_dd_g_Error( "Invalid Surface",nSurface,"LoadBmpIntoSurface");

		if ( DD_OK != hb_dd_DDCopyBitmap( hb_dd_g_DDSFaces[ nSurface ], hbm, x,y, dx, dy ) )
			 hb_dd_g_Error( "DDCopyBitmap",nSurface,"LoadBmpIntoSurface");


		DeleteObject(hbm);
		
	}

//------------------------------------------------------------------//

	HARBOUR HB_DD_CREATEOFFSCREENBITMAP( void )
	{
		DDSURFACEDESC2              ddsd;
	    HRESULT                     hRet;
		
		if ( ! hb_dd_g_pDD )
			hb_dd_g_Error("No DDraw Initialized",1000,"");

		if  ( hb_dd_g_DDSFaceCount >= MAX_DDSURFACES )
			hb_dd_g_Error( "No more DDSurfaces -> Limit reached -> CreateDDSurface()",1000,"DDraw extend sys");
		else
		{
			ZeroMemory(&ddsd, sizeof(ddsd));
			ddsd.dwSize = sizeof(ddsd);
			ddsd.dwFlags = DDSD_CAPS | DDSD_HEIGHT | DDSD_WIDTH  ;
			ddsd.ddsCaps.dwCaps = DDSCAPS_OFFSCREENPLAIN;
			ddsd.dwHeight = hb_dd_g_yWindow;
			ddsd.dwWidth = hb_dd_g_xWindow;
			hRet = hb_dd_g_pDD->CreateSurface(&ddsd, &hb_dd_g_DDSFaces[ hb_dd_g_DDSFaceCount ], NULL);
			if (hRet != DD_OK)
		        hb_dd_InitFail(hb_dd_g_DDHwnd, hRet, "CreateSurface FAILED");
		}

		hb_retnl( hb_dd_g_DDSFaceCount );
		hb_dd_g_DDSFaceCount++;
		
	}


//------------------------------------------------------------------//

	void hb_RestoreAll(void)
	{
		HRESULT                     hRet;
		long t;

		hRet = hb_dd_g_DDSFaces[0]->Restore();
		if (hRet == DD_OK)
		{
		    for( t=2;t<MAX_DDSURFACES ;t++ )
			{
				if (hb_dd_g_DDSFaces[t] != NULL)
					hb_dd_g_DDSFaces[t]->Restore();
			}
		}
	}

//------------------------------------------------------------------//

	void hb_dd_ReleaseAllObjects(void)
	{
		long t;
		if (hb_dd_g_pDD != NULL)
		{
	        if (hb_dd_g_DDSFaces[0] != NULL)
			{
	            hb_dd_g_DDSFaces[0]->Release();
				hb_dd_g_DDSFaces[0] = NULL;
			}
			for( t=2;t<MAX_DDSURFACES ;t++ )
			{
				if (hb_dd_g_DDSFaces[t] != NULL)
				{
		            hb_dd_g_DDSFaces[t]->Release();
					hb_dd_g_DDSFaces[t] = NULL;
				}
			}
			hb_dd_g_pDD->Release();
			hb_dd_g_pDD = NULL;
		}
	}


//------------------------------------------------------------------//

	HARBOUR HB_DD_DDRAWSTARTUP( HWND hWnd )
	{


		DDSURFACEDESC2              ddsd;	    
	    DDSCAPS2                    ddscaps;
	    HRESULT                     hRet;
	    LPDIRECTDRAW                pDD;
		long t;
		
		hb_dd_g_DDHwnd = hWnd;
		

	    hRet = DirectDrawCreate(NULL, &pDD, NULL);
	    if (hRet != DD_OK)
            hb_dd_InitFail(hWnd, hRet, "DirectDrawCreate FAILED");

		
	    hRet = pDD->QueryInterface(IID_IDirectDraw4, (LPVOID *) &hb_dd_g_pDD );

	    if (hRet != DD_OK)
			hb_dd_InitFail(hWnd, hRet, "QueryInterface FAILED");
	
	    hRet = pDD->SetCooperativeLevel(hWnd, DDSCL_EXCLUSIVE | DDSCL_FULLSCREEN | DDSCL_ALLOWREBOOT | DDSCL_ALLOWMODEX  );

	    if (hRet != DD_OK)
			hb_dd_InitFail(hWnd, hRet, "SetCooperativeLevel FAILED");

		hRet = hb_dd_g_pDD->SetDisplayMode(hb_dd_g_xWindow , hb_dd_g_yWindow , 16, 0, 0);
		if (hRet != DD_OK)
	        hb_dd_InitFail(hWnd, hRet, "SetDisplayMode FAILED");

		for( t=0;t<= MAX_DDSURFACES;t++ )
			hb_dd_g_DDSFaces[t] = NULL;
		for( t=0;t< MAX_SPRITES;t++)
		    hb_dd_Sprites[t].Surface = -1;
		for( t=0;t<256;t++)
		     hb_dd_g_KeyDown[t] = 0;
	    ZeroMemory(&ddsd, sizeof(ddsd));

		ddsd.dwSize = sizeof(ddsd);
		ddsd.dwFlags = DDSD_CAPS | DDSD_BACKBUFFERCOUNT | DDSD_CKSRCBLT;
		ddsd.ddsCaps.dwCaps = DDSCAPS_PRIMARYSURFACE |
	    DDSCAPS_FLIP |
		DDSCAPS_COMPLEX  ;
		ddsd.dwBackBufferCount = 1;
		hRet = hb_dd_g_pDD->CreateSurface(&ddsd, &hb_dd_g_DDSFaces[0] , NULL);
		if (hRet != DD_OK)
			hb_dd_InitFail(hWnd, hRet, "CreateSurface FAILED");
		
	    ddscaps.dwCaps = DDSCAPS_BACKBUFFER;

	    hRet = hb_dd_g_DDSFaces[0]->GetAttachedSurface(&ddscaps,&hb_dd_g_DDSFaces[1] );

	    if (hRet != DD_OK)
			hb_dd_InitFail(hWnd, hRet, "GetAttachedSurface FAILED");

		if (HB_DD_TIMER_ID != SetTimer(hWnd, HB_DD_TIMER_ID, HB_DD_TIMER_RATE, NULL))
	        hb_dd_InitFail( hWnd, hRet, "SetTimer FAILED" );
		
		hb_dd_g_DDSFaceCount = 2;
	}

//------------------------------------------------------------------//

	HARBOUR HB_DD_STARTWINDOW( void )
	{
		HWND m_hWnd = ( HWND ) hb_parnl(1);
		MSG msg;
		BOOL loop = TRUE;

		while(  loop  )
		{    
		     if ( PeekMessage( &msg, NULL, 0U, 0U, PM_REMOVE ) )
			 {
			      TranslateMessage( &msg );
                  DispatchMessage( &msg );
			 }
			 if ( WM_CLOSE == msg.message  )
				  loop = FALSE;

		}
	}

//------------------------------------------------------------------//
HRESULT	hb_dd_DDCopyBitmap(IDirectDrawSurface4 * pdds, HBITMAP hbm, int x, int y,
             int dx, int dy)
{
    HDC                     hdcImage;
    HDC                     hdc;
    BITMAP                  bm;
    DDSURFACEDESC2          ddsd;
    HRESULT                 hr;

    if (hbm == NULL || pdds == NULL)
        return E_FAIL;

    pdds->Restore();

    hdcImage = CreateCompatibleDC(NULL);
    if (!hdcImage)
        OutputDebugString("createcompatible dc failed\n");
    SelectObject(hdcImage, hbm);

    GetObject(hbm, sizeof(bm), &bm);
    dx = dx == 0 ? bm.bmWidth : dx;     // Use the passed size, unless zero
    dy = dy == 0 ? bm.bmHeight : dy;

    ddsd.dwSize = sizeof(ddsd);
    ddsd.dwFlags = DDSD_HEIGHT | DDSD_WIDTH;

    pdds->GetSurfaceDesc(&ddsd);

    if ((hr = pdds->GetDC(&hdc)) == DD_OK)
    {
        StretchBlt(hdc, 0, 0, ddsd.dwWidth, ddsd.dwHeight, hdcImage, x, y,
                   dx, dy, SRCCOPY);
        pdds->ReleaseDC(hdc);
    }
    DeleteDC(hdcImage);
    return hr;
}

void hb_dd_checkError( HRESULT hr )
{
	long p=0;
	switch( hr )
	{
	case  DDERR_EXCEPTION  : p++;break;
	case  DDERR_GENERIC   :  p++;;break;
	case  DDERR_INVALIDOBJECT   : p++;;break;
	case  DDERR_INVALIDPARAMS   : p++;;break;
	case  DDERR_INVALIDRECT   :   p++;;break;
	case  DDERR_NOBLTHW   :       p++;;break;
	case  DDERR_SURFACEBUSY   :   p++;;break;
	case  DDERR_SURFACELOST   :   p++;;break;
	case  DDERR_UNSUPPORTED   :   p++;;break;
	case  DDERR_WASSTILLDRAWING : p++;;break;
    case  DDERR_NOOVERLAYHW :p++;;break;
	case  DDERR_NOTAOVERLAYSURFACE :p++;;break;
	case  DDERR_INVALIDSURFACETYPE :p++;;break;
		
	}
}



void hb_dd_g_Error( char *, long , char *)
{
}