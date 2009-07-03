/*
 * $Id$
 */

//----------------------------------------------------------------------------//
//----------------------------------------------------------------------------//
//----------------------------------------------------------------------------//
//
//                                 hbwhat
//                             Clipboard functions
//
//----------------------------------------------------------------------------//
//----------------------------------------------------------------------------//
//----------------------------------------------------------------------------//

#define HB_OS_WIN_USED
#undef _WIN32_WINNT
#define _WIN32_WINNT   0x0400

#include "hbwhat.h"

//----------------------------------------------------------------------------//

#include <windows.h>
#include <shlobj.h>

#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"

//----------------------------------------------------------------------------//

HBITMAP DuplicateBitmap( HBITMAP hbmpSrc );

//----------------------------------------------------------------------------//

HB_FUNC( VWN_CLOSECLIPBOARD )
{
   hb_retl( CloseClipboard() );
}

//----------------------------------------------------------------------------//

HB_FUNC( VWN_COUNTCLIPBOARDFORMATS )
{
   hb_retni( CountClipboardFormats() );
}

//----------------------------------------------------------------------------//

HB_FUNC( VWN_EMPTYCLIPBOARD )
{
   hb_retl( EmptyClipboard() );
}

//-------------------------------------------------------------------------------

HB_FUNC( VWN_ENUMCLIPBOARDFORMATS )
{
   hb_retni( EnumClipboardFormats( (UINT) hb_parni(1) ) );
}

//----------------------------------------------------------------------------//

HB_FUNC( VWN_SETCLIPBOARDVIEWER )
{
   HB_RETWH( SetClipboardViewer( (HWND) HB_PARWH(1) ) );
}

//----------------------------------------------------------------------------//
//
// WINUSERAPI BOOL WINAPI ChangeClipboardChain( IN HWND hWndRemove, IN HWND hWndNewNext);
//
HB_FUNC( VWN_CHANGECLIPBOARDCHAIN )
{
   hb_retl( ChangeClipboardChain( (HWND) HB_PARWH( 1 ), (HWND) HB_PARWH( 2 ) ) );
}


//----------------------------------------------------------------------------//
//
// WINUSERAPI HWND WINAPI GetOpenClipboardWindow( VOID);
//
HB_FUNC( VWN_GETOPENCLIPBOARDWINDOW )
{
   HB_RETWH( GetOpenClipboardWindow(  ) );
}


//----------------------------------------------------------------------------//
//
// WINUSERAPI int WINAPI GetPriorityClipboardFormat( OUT UINT *paFormatPriorityList, IN int cFormats);
//
HB_FUNC( VWN_GETPRIORITYCLIPBOARDFORMAT )
{
   UINT *p, *paFP ;
   UINT i;

   p = paFP = (PUINT) hb_xgrab( hb_parinfa( 1, 0 ) * sizeof(UINT)  );
   for ( i=1; i<=hb_parinfa( 1, 0 ); i++ )
   {
     *p++ = hb_parni( 1, i );
   }
   hb_retni( GetPriorityClipboardFormat( (UINT *) paFP, hb_parinfa( 1, 0 ) ) );

}

//----------------------------------------------------------------------------//
//
// WINUSERAPI DWORD WINAPI GetClipboardSequenceNumber( VOID);
//
#if(WINVER >= 0x0500)

HB_FUNC( VWN_GETCLIPBOARDSEQUENCENUMBER )
{
   hb_retnl( (LONG) GetClipboardSequenceNumber(  ) );
}

#endif

//----------------------------------------------------------------------------//

HB_FUNC( VWN_GETCLIPBOARDOWNER )
{
   HB_RETWH( GetClipboardOwner() );
}

//----------------------------------------------------------------------------//

HB_FUNC( VWN_GETCLIPBOARDVIEWER )
{
   HB_RETWH( GetClipboardViewer() );
}

//----------------------------------------------------------------------------//

HB_FUNC( VWN_ISCLIPBOARDFORMATAVAILABLE )
{
   hb_retl( IsClipboardFormatAvailable( hb_parni(1) ) );
}

//----------------------------------------------------------------------------//

HB_FUNC( VWN_OPENCLIPBOARD )
{
   hb_retl( OpenClipboard( (HWND) HB_PARWH(1) ) );
}

//----------------------------------------------------------------------------//

HB_FUNC( VWN_REGISTERCLIPBOARDFORMAT )
{
   hb_retni( RegisterClipboardFormat( (LPCSTR) hb_parcx(1) ) );
}

//----------------------------------------------------------------------------//
/*
HB_FUNC( VWN_GETCLIPBOARDDATA )
{
   HANDLE hClipMem ;
   LPSTR  lpClip ;

   hClipMem = GetClipboardData( (UINT) hb_parni(1) );

   if( hClipMem )
    {
      lpClip = (LPSTR)  GlobalLock(hClipMem);
      hb_retclen( lpClip , GlobalSize(hClipMem) );
      GlobalUnlock( hClipMem );
    }
}
*/
//----------------------------------------------------------------------------//

HB_FUNC( VWN_GETCLIPBOARDDATA )
{
   WORD    wType = ( HB_ISNIL( 1 ) ? CF_TEXT : ( WORD ) hb_parni( 1 ) );
   HGLOBAL hMem ;
   HANDLE  hClipMem ;
   LPSTR   lpClip ;

   switch( wType )
   {
      case CF_TEXT:
         hMem = GetClipboardData( CF_TEXT );
         if( hMem )
         {
            hb_retc( ( char * ) GlobalLock( hMem ) );
            GlobalUnlock( hMem );
         }
         else
            hb_retc_null();
         break;

      case CF_BITMAP:
         if( IsClipboardFormatAvailable( CF_BITMAP ) )
            HB_RETWH( DuplicateBitmap( ( HBITMAP ) GetClipboardData( CF_BITMAP ) ) );
         else
            HB_RETWH( 0 );
         break;

      default:
         hClipMem = GetClipboardData( ( UINT ) hb_parni( 1 ) );

         if( hClipMem )
         {
            lpClip = ( LPSTR )  GlobalLock( hClipMem );
            hb_retclen( lpClip, GlobalSize( hClipMem ) );
            GlobalUnlock( hClipMem );
         }
         break;
   }
}

//----------------------------------------------------------------------------//

HB_FUNC( VWN_GETCLIPBOARDFORMATNAME )
{
   int nRet ;
   char cName[128] ;

   nRet = GetClipboardFormatName( (UINT) hb_parni(1), cName, 127 );

   if ( nRet == 0 )
      hb_retc(NULL);
   else
      hb_retclen(cName, nRet);
}

//----------------------------------------------------------------------------//
/*
HB_FUNC( VWN_SETCLIPBOARDDATA )
{
   HANDLE hMem ;
   void *pMem ;
   DWORD dwLen ;

   if ( hb_pcount() > 1 )
   {
      dwLen = (DWORD) hb_parclen(2) + ( hb_parni(1) == CF_TEXT ? 1 : 0 );
      hMem = GlobalAlloc( ( GMEM_MOVEABLE | GMEM_DDESHARE) , dwLen );
      if (  hMem )
      {
          pMem = GlobalLock( hMem);
          memcpy(pMem, hb_parcx(2), dwLen );
          GlobalUnlock( hMem );
          HB_RETWH( SetClipboardData( (UINT) hb_parni(1), hMem ) );
      }
      else
         hb_retnl(0);
   }
}
*/
//----------------------------------------------------------------------------//

HB_FUNC( VWN_SETCLIPBOARDDATA )
{
   WORD    wType = ( WORD ) hb_parni( 1 );
   HGLOBAL hMem ;
   DWORD   dwLen;
   void    *pMem;

   switch( wType )
   {
      case CF_TEXT:
         hMem = GetClipboardData( CF_TEXT );
         if( hMem )
         {
            hb_retc( ( char * ) GlobalLock( hMem ) );
            GlobalUnlock( hMem );
         }
         else
            hb_retc_null();
         break;

      case CF_BITMAP:
         if( IsClipboardFormatAvailable( CF_BITMAP ) )
            hb_retl( SetClipboardData( CF_BITMAP, DuplicateBitmap( ( HBITMAP ) HB_PARWH( 2 ) ) ) != NULL );
         else
            hb_retl( FALSE );
         break;

      default:
         dwLen = ( DWORD ) hb_parclen( 2 );
         hMem  = GlobalAlloc( ( GMEM_MOVEABLE | GMEM_DDESHARE) , dwLen );
         if ( hMem )
         {
            pMem = GlobalLock( hMem );
            memcpy( pMem, hb_parcx( 2 ), dwLen );
            GlobalUnlock( hMem );
            HB_RETWH( SetClipboardData( ( UINT ) hb_parni( 1 ), hMem ) );
         }
         else
         {
            HB_RETWH( 0 );
         }
         break;
   }
}

//----------------------------------------------------------------------------//

HBITMAP DuplicateBitmap( HBITMAP hbmpSrc )
{
   HBITMAP hbmpOldSrc, hbmpOldDest, hbmpNew;
   HDC     hdcSrc, hdcDest;
   BITMAP  bmp;

   hdcSrc  = CreateCompatibleDC( NULL );
   hdcDest = CreateCompatibleDC( hdcSrc );

   GetObject( hbmpSrc, sizeof( BITMAP ), &bmp );

   hbmpOldSrc = ( HBITMAP ) SelectObject( hdcSrc, hbmpSrc );

   hbmpNew = CreateCompatibleBitmap( hdcSrc, bmp.bmWidth, bmp.bmHeight );

   hbmpOldDest = ( HBITMAP ) SelectObject( hdcDest, hbmpNew );

   BitBlt( hdcDest, 0, 0, bmp.bmWidth, bmp.bmHeight, hdcSrc, 0, 0, SRCCOPY);

   SelectObject( hdcDest, hbmpOldDest );
   SelectObject( hdcSrc, hbmpOldSrc );

   DeleteDC( hdcDest );
   DeleteDC( hdcSrc );

   return hbmpNew;
}

//----------------------------------------------------------------------------//
