/*
 * $Id$
 */


#define INCL_BASE
#define INCL_PM


#include <os2.h>
#include "hbapi.h"


HAB hb_pm_GetHab( void );


HB_FUNC( WINREGISTERCLASS )
{

   hb_retl( WinRegisterClass( hb_pm_GetHab(),   /* anchor block handle */
                              hb_parc( 1 ),     /* Class Name */
                              ( PFNWP ) WinDefWindowProc,  /* default Class procedure */
                              hb_parnl( 2 ),     /* style */
                              hb_parnl( 3 ) ) ); /* extra bytes */
}

HB_FUNC( WINCREATESTDWINDOW )
{
   ULONG lFrame = hb_parnl( 3 );
   HWND hWndClient, hWndFrame;

   hb_retnl( ( LONG ) hWndFrame =
             WinCreateStdWindow( ( HWND ) hb_parnl( 1 ), /* hWndParent */
                                 hb_parnl( 2 ), /* style */
                                 &lFrame,       /* lFrame */
                                 hb_parc( 4 ),  /* cClassName */
                                 hb_parc( 5 ),  /* cCaption */
                                 hb_parnl( 6 ), /* lStyleClient */
                                 hb_parnl( 7 ), /* hModule */
                                 hb_parnl( 8 ), /* nId */
                       ( PHWND ) &hWndClient ) ); /* Window client handle */

   hb_stornl( ( LONG ) hWndClient, 9 );
}

HB_FUNC( HB_PM_SHOWMODAL )
{
   QMSG qmsg;
   HAB  hab = hb_pm_GetHab();

   while( WinGetMsg( hab, &qmsg, 0, 0, 0 ) )
   {
      WinDispatchMsg( hab, &qmsg );
   }
}

/* nOr() is a very used function */

HB_FUNC( NOR )
{
   LONG lRet = 0;
   USHORT i = 0;

   while( i < hb_pcount() )
      lRet = lRet | hb_parnl( ++i );

   hb_retnl( lRet );
}


HB_FUNC( WINSETWINDOWTEXT )
{
   hb_retl( WinSetWindowText( ( HWND ) hb_parnl( 1 ), hb_parc( 2 ) ) );
}


HB_FUNC( WINGETTEXT )
{
   BYTE bBuffer[ 255 ];

   WinQueryWindowText( ( HWND ) hb_parnl( 1 ), 254, bBuffer );
   hb_retc( bBuffer );
}


HB_FUNC( MSGINFO )
{
   HWND hWnd = WinQueryActiveWindow( HWND_DESKTOP);
   PSZ szCaption = ( hb_pcount() > 1 && ISCHAR( 2 ) ? hb_parc( 2 ) : "Information");

   hb_retnl( WinMessageBox( HWND_DESKTOP, hWnd, hb_parc( 1 ), szCaption,
             0, MB_INFORMATION | MB_OK | MB_MOVEABLE | MB_APPLMODAL ) );
}


HAB hb_pm_GetHab() {

   HWND hWnd = WinQueryActiveWindow( HWND_DESKTOP);
   return WinQueryAnchorBlock(hWnd);

}


HB_FUNC( GETHAB )
{
   hb_retnl( ( LONG ) hb_pm_GetHab() );
}



