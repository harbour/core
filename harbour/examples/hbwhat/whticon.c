/*
 * $Id$
 */


// Icon functions



#undef _WIN32_WINNT
#define _WIN32_WINNT   0x0400

#include "hbwhat.h"

//#include <shlobj.h>
#include <windows.h>
//#include <commctrl.h>
#include "hbapiitm.h"
//#include "hbapiitm.h"
#include "hbapi.h"
//#include "hbvm.h"
//#include "hbstack.h"




//-----------------------------------------------------------------------------

// WINUSERAPI HICON WINAPI LoadIconA( IN HINSTANCE hInstance, IN LPCSTR lpIconName);

HB_FUNC( VWN_LOADICON )
{
   HB_RETWH( LoadIcon(  ( HB_ISNIL(1) ? NULL : (HINSTANCE) HB_PARWH( 1 ) ) ,
             (hb_parinfo(2)==HB_IT_STRING ? hb_parcx(2) : MAKEINTRESOURCE( (WORD) hb_parni(2))) ) );
}


//-----------------------------------------------------------------------------

// WINUSERAPI HICON WINAPI CreateIcon( IN HINSTANCE hInstance, IN int nWidth, IN int nHeight, IN BYTE cPlanes, IN BYTE cBitsPixel, IN CONST BYTE *lpbANDbits, IN CONST BYTE *lpbXORbits);


HB_FUNC( VWN_CREATEICON )
{

   HB_RETWH( CreateIcon(  HB_ISNIL( 1 ) ? GetModuleHandle(NULL) : (HINSTANCE) HB_PARWH( 1 ),
                                hb_parni( 2 )            ,
                                hb_parni( 3 )            ,
                                (BYTE) hb_parni( 4 )     ,
                                (BYTE) hb_parni( 5 )     ,
                                (BYTE *) hb_parcx( 5 )    ,
                                (BYTE *) hb_parcx( 6 )
                              ) );
}


//-----------------------------------------------------------------------------

// WINUSERAPI BOOL WINAPI DestroyIcon( IN HICON hIcon);

HB_FUNC( VWN_DESTROYICON )
{
   hb_retl( DestroyIcon( (HICON) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------

// WINUSERAPI int WINAPI LookupIconIdFromDirectory( IN PBYTE presbits, IN BOOL fIcon);

HB_FUNC( VWN_LOOKUPICONIDFROMDIRECTORY )
{

   hb_retni( LookupIconIdFromDirectory( (PBYTE) hb_parcx( 1 ), hb_parl( 2 ) ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI LookupIconIdFromDirectoryEx( IN PBYTE presbits, IN BOOL fIcon, IN int cxDesired, IN int cyDesired, IN UINT Flags);


HB_FUNC( VWN_LOOKUPICONIDFROMDIRECTORYEX )
{

   hb_retni( LookupIconIdFromDirectoryEx( (PBYTE) hb_parcx( 1 ) ,
                                          hb_parl( 2 )         ,
                                          hb_parni( 3 )        ,
                                          hb_parni( 4 )        ,
                                          (UINT) hb_parni( 5 )
                                        ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI HICON WINAPI CreateIconFromResource( IN PBYTE presbits, IN DWORD dwResSize, IN BOOL fIcon, IN DWORD dwVer);

HB_FUNC( VWN_CREATEICONFROMRESOURCE )
{

   HB_RETWH( CreateIconFromResource( (PBYTE) hb_parcx( 1 ) ,
                                            (DWORD) hb_parnl( 2 ),
                                            hb_parl( 3 )         ,
                                            (DWORD) hb_parnl( 4 )
                                          ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI HICON WINAPI CreateIconFromResourceEx( IN PBYTE presbits, IN DWORD dwResSize, IN BOOL fIcon, IN DWORD dwVer, IN int cxDesired, IN int cyDesired, IN UINT Flags);

HB_FUNC( VWN_CREATEICONFROMRESOURCEEX )
{

   HB_RETWH( CreateIconFromResourceEx( (PBYTE) hb_parcx( 1 )  ,
                                              (DWORD) hb_parnl( 2 ) ,
                                              hb_parl( 3 )          ,
                                              (DWORD) hb_parnl( 4 ) ,
                                              hb_parni( 5 )         ,
                                              hb_parni( 6 )         ,
                                              (UINT) hb_parni( 7 )
                                            ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI HANDLE WINAPI LoadImageA( IN HINSTANCE, IN LPCSTR, IN UINT, IN int, IN int, IN UINT);


HB_FUNC( VWN_LOADIMAGE )
{
   HB_RETWH( LoadImage( HB_ISNIL( 1 ) ? GetModuleHandle(NULL) : (HINSTANCE) HB_PARWH( 1 ),
                               (LPCSTR) hb_parcx( 2 )    ,
                               (UINT) hb_parni( 3 )     ,
                               hb_parni( 4 )            ,
                               hb_parni( 5 )            ,
                               (UINT) hb_parni( 6 )
                             ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI HANDLE WINAPI CopyImage( IN HANDLE, IN UINT, IN int, IN int, IN UINT);


HB_FUNC( VWN_COPYIMAGE )
{
   HB_RETWH( CopyImage( (HANDLE) HB_PARWH( 1 ),
                               (UINT) hb_parni( 2 )  ,
                               hb_parni( 3 )         ,
                               hb_parni( 4 )         ,
                               (UINT) hb_parni( 5 )
                             ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI DrawIcon( IN HDC hDC, IN int X, IN int Y, IN HICON hIcon);

HB_FUNC( VWN_DRAWICON )
{
   hb_retl( DrawIcon( (HDC) HB_PARWH( 1 )  ,
                      hb_parni( 2 )        ,
                      hb_parni( 3 )        ,
                      (HICON) HB_PARWH( 4 )
                    ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI DrawIconEx( IN HDC hdc, IN int xLeft, IN int yTop, IN HICON hIcon, IN int cxWidth, IN int cyWidth, IN UINT istepIfAniCur, IN HBRUSH hbrFlickerFreeDraw, IN UINT diFlags);


HB_FUNC( VWN_DRAWICONEX )
{
   hb_retl( DrawIconEx( (HDC) HB_PARWH( 1 )   ,
                        hb_parni( 2 )         ,
                        hb_parni( 3 )         ,
                        (HICON) HB_PARWH( 4 ) ,
                        hb_parni( 5 )         ,
                        hb_parni( 6 )         ,
                        (UINT) hb_parni( 7 )  ,
                        (HBRUSH) HB_PARWH( 8 ),
                        (UINT) hb_parni( 9 )
                      ) );
}

//-----------------------------------------------------------------------------

// WINUSERAPI HICON WINAPI CreateIconIndirect( IN PICONINFO piconinfo);

HB_FUNC( VWN_CREATEICONINDIRECT )
{
   ICONINFO *ii =  (ICONINFO * ) hb_parc( 1 ); //hb_param( 1, HB_IT_STRING )->item.asString.value;

   HB_RETWH( CreateIconIndirect( ii ) );
}


//-----------------------------------------------------------------------------

// WINUSERAPI HICON WINAPI CopyIcon( IN HICON hIcon);


HB_FUNC( VWN_COPYICON )
{
   HB_RETWH( CopyIcon( (HICON) HB_PARWH( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI GetIconInfo( IN HICON hIcon, OUT PICONINFO piconinfo);

HB_FUNC( VWN_GETICONINFO )
{
   ICONINFO ii;

   hb_retl( GetIconInfo( (HICON) HB_PARWH( 1 ), &ii ) );

   // verify !!
   // assign into structure

   hb_storclen( (char *) &ii, sizeof( ICONINFO ), 2 );

}


//-----------------------------------------------------------------------------
// SHSTDAPI_(HICON) DuplicateIcon(HINSTANCE hInst, HICON hIcon);

#ifndef __WATCOMC__
HB_FUNC( VWN_DUPLICATEICON )
{
   HB_RETWH( DuplicateIcon(  HB_ISNIL( 1 ) ? GetModuleHandle(NULL) : (HINSTANCE) HB_PARWH( 1 ),
                                   (HICON) HB_PARWH( 2 )
                                 ) );
}
#endif
//-----------------------------------------------------------------------------
// SHSTDAPI_(HICON) ExtractAssociatedIconA(HINSTANCE hInst, LPSTR lpIconPath, LPWORD lpiIcon);

HB_FUNC( VWN_EXTRACTASSOCIATEDICON )
{
   WORD lpiIcon  ;
   HICON  hiRet ;

   lpiIcon = (WORD) hb_parni( 2 );

   hiRet = ExtractAssociatedIcon( ( HB_ISNIL( 1 ) ? GetModuleHandle(NULL) : (HINSTANCE) HB_PARWH( 1 ) ) ,
                                             (LPSTR) hb_parcx( 2 )     ,
                                             &lpiIcon
                                );

   if ( hiRet )
      hb_storni( lpiIcon, 2 );

   HB_RETWH( hiRet );

}


//-----------------------------------------------------------------------------
// SHSTDAPI_(HICON) ExtractIconA(HINSTANCE hInst, LPCSTR lpszExeFileName, UINT nIconIndex);


HB_FUNC( VWN_EXTRACTICON )
{
   HB_RETWH( ExtractIcon( HB_ISNIL( 1 ) ? GetModuleHandle(NULL) : (HINSTANCE) HB_PARWH( 1 ),
                                 (LPCSTR) hb_parcx( 2 )    ,
                                 (UINT) hb_parni( 3 )
                               ) );
}
