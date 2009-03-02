/*
 * $Id$
 */
//----------------------------------------------------------------------//
/*
                       W A R N I N G   !!!

    *******************************************************
      Do not alter this code, unless you understand fully
    how it works, and, if you really know what you're doing
    *******************************************************

*/
//----------------------------------------------------------------------//
//
//HARBOUR C FUNCTIONS:

#undef _WIN32_WINNT
#define _WIN32_WINNT   0x0400

#include "hbwhat.h"

#include <windows.h>
#include <shlobj.h>
//#include <commctrl.h>

#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"
//----------------------------------------------------------------------//
LRESULT CALLBACK __WndProc   (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
LRESULT CALLBACK __WndProc2  (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
LRESULT CALLBACK __WndProc3  (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
LRESULT CALLBACK __WndProc4  (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
LRESULT CALLBACK __WndProc5  (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
LRESULT CALLBACK __WndProc6  (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
LRESULT CALLBACK __WndProc7  (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
LRESULT CALLBACK __WndProc8  (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
LRESULT CALLBACK __WndProc9  (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
LRESULT CALLBACK __WndProc10 (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);

BOOL CALLBACK __DlgProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);
//----------------------------------------------------------------------//
//
//  Helper routine.  Take an input pointer, return closest
//  pointer that is aligned on a DWORD (4 byte) boundary.
//
static LPWORD lpwAlign( LPWORD lpIn )
{
   HB_PTRDIFF ul = ( HB_PTRDIFF ) lpIn;
   ul += 3;
   ul >>=2;
   ul <<=2;
   return ( LPWORD ) ul;
}
//----------------------------------------------------------------------//
static int nCopyAnsiToWideChar (LPWORD lpWCStr, LPSTR lpAnsiIn)
{
  int nChar = 0;

  do {
      *lpWCStr++ = (WORD) *lpAnsiIn;
      nChar++;
     } while (*lpAnsiIn++);

  return nChar;
}
/*

alternative to the above function:

//----------------------------------------------------------------------//
//
// Helper routine.  Takes second parameter as Ansi string, copies
// it to first parameter as wide character (16-bits / char) string,
// and returns integer number of wide characters (words) in string
// (including the trailing wide char NULL).

static int nCopyAnsiToWideChar (LPWORD lpWCStr, LPSTR lpAnsiIn)
{
  int cchAnsi = lstrlen( lpAnsiIn );
  return MultiByteToWideChar(GetACP(), MB_PRECOMPOSED, lpAnsiIn, cchAnsi, lpWCStr, cchAnsi) +1;
}
*/
//----------------------------------------------------------------------//
//
// note: specifying (HBRUSH) COLOR_WINDOW+1 makes it not working
//       (the window background stays black)
//       but not specifying it generates a WARNING, but it works OK.

HB_FUNC( WHT__REGISTERCLASS )
{
   WNDCLASS *wndclass = ( WNDCLASS *) hb_parc( 1 ); //hb_param( 1, HB_IT_STRING )->item.asString.value ;
   wndclass->lpfnWndProc   = __WndProc ;

   /*
   wndclass.style         = (ISNIL(1) ? (CS_HREDRAW | CS_VREDRAW | CS_OWNDC | CS_DBLCLKS)  : hb_parni(1) );
   wndclass.lpfnWndProc   = __WndProc ;
   wndclass.cbClsExtra    = ( ISNIL(2)  ? 0 : hb_parni(2));
   wndclass.cbWndExtra    = ( ISNIL(3)  ? 0 : hb_parni(3));
   wndclass.hInstance     = ( ISNIL(4)  ? GetModuleHandle(NULL) : (HANDLE) HB_PARWH(4) );
   wndclass.hIcon         = ( ISNIL(5)  ? LoadIcon(GetModuleHandle(NULL),"") : (HICON) HB_PARWH(5) );
   wndclass.hCursor       = ( ISNIL(6)  ? LoadCursor (NULL, IDC_ARROW) : (HCURSOR) HB_PARWH(6) );
   wndclass.hbrBackground = ( ISNIL(7)  ? (INT) COLOR_WINDOW  + 1 :  (HBRUSH) HB_PARWH(7) );
   wndclass.lpszMenuName  = (LPCSTR) ( !ISNIL(8) ? hb_parc(8) : NULL );
   wndclass.lpszClassName = (LPCSTR) hb_parc(9);
   */

   hb_retl( RegisterClass (wndclass));
}
//----------------------------------------------------------------------//
HB_FUNC( WHT__UNREGISTERCLASS )
{
   HANDLE hInst = ( ISNIL(2) ? GetModuleHandle(NULL) : (HANDLE) HB_PARWH(2) );

   hb_retl( UnregisterClass( hb_parc(1), (HINSTANCE) hInst ) );
}
//----------------------------------------------------------------------//
// hMenu or Window ID are interchangable
// casting Id to hMenu stops it from working correctly
//
HB_FUNC( WHT__CREATEWINDOWEX )
{
   DWORD  dwExStyle  = (ISNIL(1)  ? 0 : hb_parnl(1));
   LPCSTR cClass     = (LPCSTR) hb_parc(2);
   LPCSTR cTitle     = (LPCSTR) hb_parc(3);
   DWORD  nStyle     = (ISNIL(4)  ? 0 : (DWORD) hb_parnd(4) );
   int    x          = (ISNIL(5)  ? ( int ) CW_USEDEFAULT : hb_parni(5));
   int    y          = (ISNIL(6)  ? ( int ) CW_USEDEFAULT : hb_parni(6));
   int    nWidth     = (ISNIL(7)  ? ( int ) CW_USEDEFAULT : hb_parni(7));
   int    nHeight    = (ISNIL(8)  ? ( int ) CW_USEDEFAULT : hb_parni(8));
   HWND   hWndParent = (ISNIL(9)  ? (HWND) NULL : (HWND) HB_PARWH(9));
   HMENU  hMenu      = (ISNIL(10) ? (HMENU) NULL : (HMENU) HB_PARWH(10));
   HANDLE hInstance  = (ISNIL(11) ? GetModuleHandle( NULL ) : (HANDLE) HB_PARWH(11));
   LPVOID lParam     = (ISNIL(12) ? NULL : (LPVOID) HB_PARWH(12));

   HWND hWnd = CreateWindowEx( dwExStyle, cClass, cTitle,
                               nStyle, x, y, nWidth, nHeight,
                               hWndParent, hMenu, (HINSTANCE) hInstance, lParam )  ;

   HB_RETWH( hWnd );
}
//----------------------------------------------------------------------//
//  Creates child MDI window
//
HB_FUNC( WHT__CREATEMDIWINDOW )
{
   LPCSTR cClass     = (LPCSTR) hb_parc(1);
   LPCSTR cTitle     = (LPCSTR) hb_parc(2);
   DWORD  nStyle     = (ISNIL(3)  ? WS_MAXIMIZE : (DWORD) hb_parnd(3) );
   int    x          = (ISNIL(4)  ? ( int ) CW_USEDEFAULT : hb_parni(4));
   int    y          = (ISNIL(5)  ? ( int ) CW_USEDEFAULT : hb_parni(5));
   int    nWidth     = (ISNIL(6)  ? ( int ) CW_USEDEFAULT : hb_parni(6));
   int    nHeight    = (ISNIL(7)  ? ( int ) CW_USEDEFAULT : hb_parni(7));
   HWND   hWndParent = (ISNIL(8)  ? (HWND) NULL : (HWND) HB_PARWH(8));
   HANDLE hInstance  = (ISNIL(9)  ? GetModuleHandle( NULL ) : (HANDLE) HB_PARWH(9));
   LPARAM lParam     = (ISNIL(10) ? 0 : (LPARAM) hb_parnint(10));

#if defined(__DMC__)
   HWND hWnd = CreateMDIWindow( ( LPSTR ) cClass, ( LPSTR ) cTitle, nStyle,
                                x, y, nWidth, nHeight,
                                hWndParent, (HINSTANCE) hInstance, lParam );
#else
   HWND hWnd = CreateMDIWindow( cClass, cTitle,nStyle,
                                x, y, nWidth, nHeight,
                                hWndParent, (HINSTANCE) hInstance, lParam );
#endif

   HB_RETWH( hWnd );
}
//----------------------------------------------------------------------//
LRESULT CALLBACK __WndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   static PHB_DYNS pSymTest = 0 ;
   long int res;

   if ( !pSymTest )
     pSymTest = hb_dynsymFind( "WHT__PROCESSMSG" );

   if ( pSymTest )
   {
      hb_vmPushDynSym( pSymTest );
      hb_vmPushNil();
      hb_vmPushNumInt( ( HB_PTRDIFF ) hWnd );
      hb_vmPushLong( (LONG ) message );
      hb_vmPushNumInt( ( HB_LONG ) wParam );
      hb_vmPushNumInt( ( HB_LONG ) lParam );
      hb_vmPushLong( 1 );
      hb_vmDo( 5 );
      res = hb_itemGetNL( hb_param( -1, HB_IT_ANY ) );

      return res;
   }
   else // shouldn't happen
      return( DefWindowProc( hWnd, message, wParam, lParam ));
}
//----------------------------------------------------------------------//
LRESULT CALLBACK __WndProc2( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   static PHB_DYNS pSymTest = 0 ;
   long int res;

   if ( !pSymTest )
     pSymTest = hb_dynsymFind( "WHT__PROCESSMSG" );

   if ( pSymTest )
   {
      hb_vmPushDynSym( pSymTest );
      hb_vmPushNil();
      hb_vmPushNumInt( ( HB_PTRDIFF ) hWnd );
      hb_vmPushLong( (LONG ) message );
      hb_vmPushNumInt( ( HB_LONG ) wParam );
      hb_vmPushNumInt( ( HB_LONG ) lParam );
      hb_vmPushLong( 2 );
      hb_vmDo( 5 );
      res = hb_itemGetNL( hb_param( -1, HB_IT_ANY ) );

      return res;
   }
   else // shouldn't happen
      return( DefWindowProc( hWnd, message, wParam, lParam ));
}
//----------------------------------------------------------------------//
LRESULT CALLBACK __WndProc3( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   static PHB_DYNS pSymTest = 0 ;
   long int res;

   if ( !pSymTest )
     pSymTest = hb_dynsymFind( "WHT__PROCESSMSG" );

   if ( pSymTest )
   {
      hb_vmPushDynSym( pSymTest );
      hb_vmPushNil();
      hb_vmPushNumInt( ( HB_PTRDIFF ) hWnd );
      hb_vmPushLong( (LONG ) message );
      hb_vmPushNumInt( ( HB_LONG ) wParam );
      hb_vmPushNumInt( ( HB_LONG ) lParam );
      hb_vmPushLong( 3 );
      hb_vmDo( 5 );
      res = hb_itemGetNL( hb_param( -1, HB_IT_ANY ) );

      return res;
   }
   else // shouldn't happen
      return( DefWindowProc( hWnd, message, wParam, lParam ));
}
//----------------------------------------------------------------------//
LRESULT CALLBACK __WndProc4( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   static PHB_DYNS pSymTest = 0 ;
   long int res;

   if ( !pSymTest )
     pSymTest = hb_dynsymFind( "WHT__PROCESSMSG" );

   if ( pSymTest )
   {
      hb_vmPushDynSym( pSymTest );
      hb_vmPushNil();
      hb_vmPushNumInt( ( HB_PTRDIFF ) hWnd );
      hb_vmPushLong( (LONG ) message );
      hb_vmPushNumInt( ( HB_LONG ) wParam );
      hb_vmPushNumInt( ( HB_LONG ) lParam );
      hb_vmPushLong( 4 );
      hb_vmDo( 5 );
      res = hb_itemGetNL( hb_param( -1, HB_IT_ANY ) );

      return res;
   }
   else // shouldn't happen
      return( DefWindowProc( hWnd, message, wParam, lParam ));
}
//----------------------------------------------------------------------//
LRESULT CALLBACK __WndProc5( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   static PHB_DYNS pSymTest = 0 ;
   long int res;

   if ( !pSymTest )
     pSymTest = hb_dynsymFind( "WHT__PROCESSMSG" );

   if ( pSymTest )
   {
      hb_vmPushDynSym( pSymTest );
      hb_vmPushNil();
      hb_vmPushNumInt( ( HB_PTRDIFF ) hWnd );
      hb_vmPushLong( (LONG ) message );
      hb_vmPushNumInt( ( HB_LONG ) wParam );
      hb_vmPushNumInt( ( HB_LONG ) lParam );
      hb_vmPushLong( 5 );
      hb_vmDo( 5 );
      res = hb_itemGetNL( hb_param( -1, HB_IT_ANY ) );

      return res;
   }
   else // shouldn't happen
      return( DefWindowProc( hWnd, message, wParam, lParam ));
}
//----------------------------------------------------------------------//
LRESULT CALLBACK __WndProc6( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   static PHB_DYNS pSymTest = 0 ;
   long int res;

   if ( !pSymTest )
     pSymTest = hb_dynsymFind( "WHT__PROCESSMSG" );

   if ( pSymTest )
   {
      hb_vmPushDynSym( pSymTest );
      hb_vmPushNil();
      hb_vmPushNumInt( ( HB_PTRDIFF ) hWnd );
      hb_vmPushLong( (LONG ) message );
      hb_vmPushNumInt( ( HB_LONG ) wParam );
      hb_vmPushNumInt( ( HB_LONG ) lParam );
      hb_vmPushLong( 6 );
      hb_vmDo( 5 );
      res = hb_itemGetNL( hb_param( -1, HB_IT_ANY ) );

      return res;
   }
   else // shouldn't happen
      return( DefWindowProc( hWnd, message, wParam, lParam ));
}
//----------------------------------------------------------------------//
LRESULT CALLBACK __WndProc7( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   static PHB_DYNS pSymTest = 0 ;
   long int res;

   if ( !pSymTest )
     pSymTest = hb_dynsymFind( "WHT__PROCESSMSG" );

   if ( pSymTest )
   {
      hb_vmPushDynSym( pSymTest );
      hb_vmPushNil();
      hb_vmPushNumInt( ( HB_PTRDIFF ) hWnd );
      hb_vmPushLong( (LONG ) message );
      hb_vmPushNumInt( ( HB_LONG ) wParam );
      hb_vmPushNumInt( ( HB_LONG ) lParam );
      hb_vmPushLong( 7 );
      hb_vmDo( 5 );
      res = hb_itemGetNL( hb_param( -1, HB_IT_ANY ) );

      return res;
   }
   else // shouldn't happen
      return( DefWindowProc( hWnd, message, wParam, lParam ));
}
//----------------------------------------------------------------------//
LRESULT CALLBACK __WndProc8( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   static PHB_DYNS pSymTest = 0 ;
   long int res;

   if ( !pSymTest )
     pSymTest = hb_dynsymFind( "WHT__PROCESSMSG" );

   if ( pSymTest )
   {
      hb_vmPushDynSym( pSymTest );
      hb_vmPushNil();
      hb_vmPushNumInt( ( HB_PTRDIFF ) hWnd );
      hb_vmPushLong( (LONG ) message );
      hb_vmPushNumInt( ( HB_LONG ) wParam );
      hb_vmPushNumInt( ( HB_LONG ) lParam );
      hb_vmPushLong( 8 );
      hb_vmDo( 5 );
      res = hb_itemGetNL( hb_param( -1, HB_IT_ANY ) );

      return res;
   }
   else // shouldn't happen
      return( DefWindowProc( hWnd, message, wParam, lParam ));
}
//----------------------------------------------------------------------//
LRESULT CALLBACK __WndProc9( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   static PHB_DYNS pSymTest = 0 ;
   long int res;

   if ( !pSymTest )
     pSymTest = hb_dynsymFind( "WHT__PROCESSMSG" );

   if ( pSymTest )
   {
      hb_vmPushDynSym( pSymTest );
      hb_vmPushNil();
      hb_vmPushNumInt( ( HB_PTRDIFF ) hWnd );
      hb_vmPushLong( (LONG ) message );
      hb_vmPushNumInt( ( HB_LONG ) wParam );
      hb_vmPushNumInt( ( HB_LONG ) lParam );
      hb_vmPushLong( 9 );
      hb_vmDo( 5 );
      res = hb_itemGetNL( hb_param( -1, HB_IT_ANY ) );

      return res;
   }
   else // shouldn't happen
      return( DefWindowProc( hWnd, message, wParam, lParam ));
}
//----------------------------------------------------------------------//
LRESULT CALLBACK __WndProc10( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
   static PHB_DYNS pSymTest = 0 ;
   long int res;

   if ( !pSymTest )
     pSymTest = hb_dynsymFind( "WHT__PROCESSMSG" );

   if ( pSymTest )
   {
      hb_vmPushDynSym( pSymTest );
      hb_vmPushNil();
      hb_vmPushNumInt( ( HB_PTRDIFF ) hWnd );
      hb_vmPushLong( (LONG ) message );
      hb_vmPushNumInt( ( HB_LONG ) wParam );
      hb_vmPushNumInt( ( HB_LONG ) lParam );
      hb_vmPushLong( 10 );
      hb_vmDo( 5 );
      res = hb_itemGetNL( hb_param( -1, HB_IT_ANY ) );

      return res;
   }
   else // shouldn't happen
      return( DefWindowProc( hWnd, message, wParam, lParam ));
}
//----------------------------------------------------------------------//
// called once, on start-up
//
HB_FUNC( WHT_GETWNDPROC )
{
   switch ( hb_parni(1) )
   {
      case 10:
        HB_RETWI( __WndProc10 );
        return ;

      case 9:
        HB_RETWI( __WndProc9 );
        return ;

      case 8:
        HB_RETWI( __WndProc8 );
        return ;

      case 7:
        HB_RETWI( __WndProc7 );
        return ;

      case 6:
        HB_RETWI( __WndProc6 );
        return ;

      case 5:
        HB_RETWI( __WndProc5 );
        return ;

      case 4:
        HB_RETWI( __WndProc4 );
        return ;

      case 3:
        HB_RETWI( __WndProc3 );
        return ;

      case 2:
        HB_RETWI( __WndProc2 );
        return ;

      case 1:
        HB_RETWI( __WndProc );
        return ;

      default:
        HB_RETWI( NULL );
        return ;

   }
}
//----------------------------------------------------------------------//
HB_FUNC( WHT__GETDLGPROC )
{
   HB_RETWI( __DlgProc );
}
//----------------------------------------------------------------------//
BOOL CALLBACK __DlgProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   static PHB_DYNS pSymTest = 0 ;
   BOOL res;

   if ( !pSymTest )
     pSymTest = hb_dynsymFind( "WHT__PROCESSDLGMSG" );

   if ( pSymTest )
   {
      hb_vmPushDynSym( pSymTest );
      hb_vmPushNil();
      hb_vmPushLong( (LONG ) hWnd );
      hb_vmPushLong( (LONG ) message );
      hb_vmPushLong( (LONG ) wParam );
      hb_vmPushLong( (LONG ) lParam );
      hb_vmDo( 4 );
      res = hb_itemGetNL( hb_stackReturnItem() );

      return res;
   }
   else // shouldn't happen
      return FALSE ;
}
//----------------------------------------------------------------------//
HB_FUNC( WHT__DIALOGBOX )
{
  HB_RETWI( DialogBox( (ISNIL(1)  ? GetModuleHandle(NULL) : (HINSTANCE) HB_PARWH(1) )  ,
                       (hb_parinfo(2)==HB_IT_STRING ? hb_parc(2) : MAKEINTRESOURCE( (WORD) hb_parni(2))) ,
                       (ISNIL(3) ?  NULL : (HWND) HB_PARWH(3) )        ,
                       (DLGPROC) HB_PARWH(4)
                     ));
}

//----------------------------------------------------------------------//
HB_FUNC( WHT__DIALOGBOXINDIRECT )
{
   HB_RETWI( DialogBoxIndirect( (ISNIL(1)  ? GetModuleHandle(NULL) : (HINSTANCE) HB_PARWH(1) ),
                               (LPDLGTEMPLATE) hb_parc(2),
                               (ISNIL(3) ?  NULL : (HWND) HB_PARWH(3) ),
                               (DLGPROC) HB_PARWH(4)
                             ));
}
//----------------------------------------------------------------------//
HB_FUNC( WHT__CREATEDIALOG )
{
  HB_RETWH( CreateDialog( (ISNIL(1)  ? GetModuleHandle(NULL) : (HINSTANCE) HB_PARWH(1) ),
                                  (hb_parinfo(2)==HB_IT_STRING ? hb_parc(2) : MAKEINTRESOURCE( (WORD) hb_parni(2))),
                                  (ISNIL(3) ?  NULL : (HWND) HB_PARWH(3) ),
                                  (DLGPROC) HB_PARWH(4)
                                ) );
}
//----------------------------------------------------------------------//
HB_FUNC( WHT__CREATEDIALOGINDIRECT )
{
  HB_RETWH( CreateDialogIndirect(
            (ISNIL(1)  ? GetModuleHandle(NULL) : (HINSTANCE) HB_PARWH(1) ),
            (LPDLGTEMPLATE) hb_parc(2),
            (ISNIL(3) ?  NULL : (HWND) HB_PARWH(3) ),
            (DLGPROC) HB_PARWH(4)
          ));
}
//----------------------------------------------------------------------//
//
// Create dynamic dialog from the Harbour array
//
HB_FUNC( WHT__MAKEDLGTEMPLATE )

{
   WORD *p, *pdlgtemplate;
   WORD  nItems = ( WORD ) hb_parni( 1, 4 );
   int   i, nchar;
   DWORD lStyle;

   // Parameters: 12 arrays
   // 1 for DLG template
   // 11 for item properties

   pdlgtemplate = p = (PWORD) LocalAlloc(LPTR, 65534)  ; // 64k allow to build up to 255 items on the dialog

   //---------------

    lStyle = hb_parnl(1,3);

    // start to fill in the dlgtemplate information.  addressing by WORDs

    *p++ = 1                   ; // version
    *p++ = 0xFFFF                   ; // signature
    *p++ = LOWORD ( hb_parnl(1,1) ); // Help Id
    *p++ = HIWORD ( hb_parnl(1,1) );

    *p++ = LOWORD ( hb_parnl(1,2) ); // ext. style
    *p++ = HIWORD ( hb_parnl(1,2) );

    *p++ = LOWORD (lStyle);
    *p++ = HIWORD (lStyle);

    *p++ = (WORD)   nItems;  // NumberOfItems
    *p++ = (short)  hb_parni(1,5);  // x
    *p++ = (short)  hb_parni(1,6);  // y
    *p++ = (short)  hb_parni(1,7);  // cx
    *p++ = (short)  hb_parni(1,8);  // cy
    *p++ = (short)  0;  // Menu (ignored for now.)
    *p++ = (short)  0x00;  // Class also ignored

    if ( hb_parinfa(1,11) == HB_IT_STRING )
    {
        nchar = nCopyAnsiToWideChar( p, ( LPSTR ) hb_parc(1,11) );
        p += nchar;
    }
    else
      *p++ =0 ;

    // add in the wPointSize and szFontName here iff the DS_SETFONT bit on

    if ( (lStyle & DS_SETFONT ) ) {
      *p++ = (short) hb_parni(1,12);
      *p++ = (short) hb_parni(1,13);
      *p++ = (short) hb_parni(1,14);

      nchar = nCopyAnsiToWideChar( p, ( LPSTR ) hb_parc(1,15) );
      p += nchar ;

    } ;

    //---------------
    // Now, for the items

   for ( i = 1 ; i <= nItems ; i++ ) {

      // make sure each item starts on a DWORD boundary
      p = lpwAlign (p);


      *p++ = LOWORD ( hb_parnl(2,i) );    // help id
      *p++ = HIWORD ( hb_parnl(2,i) );

      *p++ = LOWORD ( hb_parnl(3,i) ); // ext. style
      *p++ = HIWORD ( hb_parnl(3,i) );

      *p++ = LOWORD ( hb_parnl(4,i) ); // style
      *p++ = HIWORD ( hb_parnl(4,i) );

      *p++ = (short)  hb_parni(5,i);  // x
      *p++ = (short)  hb_parni(6,i);  // y
      *p++ = (short)  hb_parni(7,i);  // cx
      *p++ = (short)  hb_parni(8,i);  // cy

      *p++ = LOWORD( hb_parnl(9,i) );  // id
      *p++ = HIWORD( hb_parnl(9,i) );  // id   // 0;

      if( hb_parinfa(10,i) == HB_IT_STRING )
      {
          nchar = nCopyAnsiToWideChar(p, ( LPSTR ) hb_parc(10,i) ); // class
          p += nchar;
      }
      else
      {
         *p++ = 0xFFFF;
         *p++ = (WORD) hb_parni(10,i);
      }

      if ( hb_parinfa(11,i) == HB_IT_STRING )
      {
         nchar = nCopyAnsiToWideChar(p, ( LPSTR ) hb_parc(11,i) );  // text
         p += nchar;
      }
      else
      {
         *p++ = 0xFFFF ;
         *p++ = (WORD) hb_parni(11,i);
      }

      *p++ = 0x00 ;  // extras ( in array 12 )
    } ;
    p = lpwAlign (p)  ;

    hb_retclen( (LPSTR) pdlgtemplate, ( ( HB_PTRDIFF ) p - ( HB_PTRDIFF ) pdlgtemplate ) );

    LocalFree (LocalHandle (pdlgtemplate) );
}
//----------------------------------------------------------------------//
