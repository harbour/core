/*
 * $Id$
 */

// Windows message functions


#define _WIN32_WINNT   0x0400

#include "hbwhat.h"

#include <windows.h>
#include <shlobj.h>
//#include <commctrl.h>

#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"





//-----------------------------------------------------------------------------

HB_FUNC( _ISDIALOGMESSAGE )
{
  hb_retl(IsDialogMessage( (HWND) HB_PARWH(1), (MSG*) hb_parcx(2) ));
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI TranslateMDISysAccel( IN HWND hWndClient, IN LPMSG lpMsg);

HB_FUNC( TRANSLATEMDISYSACCEL )
{
   hb_retl( TranslateMDISysAccel( (HWND) HB_PARWH( 1 ), (MSG*) hb_parcx( 2 ) ) );
}


//-----------------------------------------------------------------------------

HB_FUNC( TRANSLATEMESSAGE )
{
  hb_retl(TranslateMessage( (MSG*) hb_parcx(1)));
}

//-----------------------------------------------------------------------------

HB_FUNC( DISPATCHMESSAGE )
{
  HB_RETWI( DispatchMessage( ( MSG * ) hb_parcx( 1 ) ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( POSTQUITMESSAGE )
{
  PostQuitMessage(hb_parni(1));
}

//-----------------------------------------------------------------------------


HB_FUNC( POSTMESSAGE )
{

   char *cText = NULL;

   if (ISBYREF(4))
   {
      cText = (char*) hb_xgrab( hb_parcsiz(4) );
      hb_xmemcpy( cText, hb_parcx(4), hb_parcsiz(4) );
   }


   hb_retnl( (LONG) PostMessage( (HWND) HB_PARWH( 1 ), (UINT) hb_parni( 2 ),
                                (ISNIL(3) ? 0 : (WPARAM) hb_parnl( 3 ))   ,
                                (ISNIL(4) ? 0 : ( ISBYREF(4)? (LPARAM) (LPSTR) cText : ( ISCHAR(4) ? (LPARAM)(LPSTR) hb_parcx(4) : (LPARAM) hb_parnl( 4 ))))
                               )
            );


   if( ISBYREF( 4 ) )
   {
      hb_storclen( cText, hb_parcsiz(4), 4 );
      hb_xfree( cText );
   }
}

//-----------------------------------------------------------------------------

HB_FUNC( SENDMESSAGE )
{

   char *cText = NULL;


   if( ISBYREF(4) )
   {
      cText = (char*) hb_xgrab( hb_parcsiz(4) );
      hb_xmemcpy( cText, hb_parcx(4), hb_parcsiz(4) );
   }

   hb_retnl( (ULONG) SendMessage( (HWND) HB_PARWH( 1 ), (UINT) hb_parni( 2 ),
                                  (ISNIL(3) ? 0 : (WPARAM) hb_parnl( 3 ))   ,
                                  (ISNIL(4) ? 0 : ( ISBYREF(4)? (LPARAM) (LPSTR) cText : ( ISCHAR(4) ? (LPARAM)(LPSTR) hb_parcx(4) : (LPARAM) hb_parnl( 4 ))))
                                )
           );


   if (ISBYREF( 4 ))
   {
      hb_storclen( cText, hb_parcsiz(4), 4 );
      hb_xfree( cText );
   }
}


//-----------------------------------------------------------------------------

HB_FUNC( SENDDLGITEMMESSAGE )
{
   char *cText;
   PHB_ITEM pText = hb_param( 5, HB_IT_STRING );

   if( pText )
   {
      cText = (char*) hb_xgrab( hb_itemGetCLen( pText )+1 ); //pText->item.asString.length + 1 );
      //hb_xmemcpy( cText, pText->item.asString.value, pText->item.asString.length + 1 );
      hb_xmemcpy( cText, hb_itemGetC( pText ), hb_itemGetCLen( pText ) + 1 );
   }
   else
   {
      cText = NULL;
   }

   hb_retnl( (LONG) SendDlgItemMessage( (HWND) HB_PARWH( 1 ) ,
                                        (int)  hb_parni( 2 ) ,
                                        (UINT) hb_parni( 3 ) ,
                                        (ISNIL(4) ? 0 : (WPARAM) hb_parnl( 4 ))   ,
                                        (cText ? (LPARAM) cText : (LPARAM) hb_parnl( 5 ))
                                      )
            );

  // Will be ignored if not BYREF.
  if( pText )
  {
     //hb_storclen( cText, pText->item.asString.length, 5 );
     hb_storclen( cText, hb_itemGetCLen( pText ), 5 );
  }

  if( cText )
  {
     hb_xfree( cText );
  }

/*
   hb_retnl( SendDlgItemMessage( (HWND) HB_PARWH(1) ,     // handle of dialog box
                                (int)   hb_parni(2) ,     // identifier of control
                                (UINT)  hb_parni(3) ,     // message to send
                                (ISNIL(4) ? 0 : (WPARAM) hb_parni(4) ) ,  // first message parameter
                                (ISNIL(5) ? 0 : (hb_parinfo(5)==HB_IT_STRING ? (LPARAM) (LPSTR) hb_parcx(5) : (LPARAM) hb_parnl( 5 )) )   // second message parameter
                              ));

*/

}

//-----------------------------------------------------------------------------

// consider passing an array instead of MSG string ( but it will be slower )

// add error handling and recovery

HB_FUNC( GETMESSAGE )
{
  MSG Msg ;

  if (GetMessage( &Msg,
                  ISNIL(2) ? NULL : (HWND) HB_PARWH(2),
                  ISNIL(3) ? 0 : hb_parnl(3),
                  ISNIL(4) ? 0 : hb_parnl(4) ) )
    {
      hb_storclen( (LPSTR) &Msg, sizeof(MSG), 1 );
      hb_retl( 1 );
    }
  else
    hb_retl ( 0 );
}

//-----------------------------------------------------------------------------

HB_FUNC( PEEKMESSAGE )
{
  MSG Msg ;

   if (PeekMessage( (MSG*) &Msg,
                    ISNIL(2) ? NULL :(HWND) HB_PARWH(2),
                    ISNIL(3) ? 0 : hb_parnl(3),
                    ISNIL(4) ? 0 : hb_parnl(4),
                    ISNIL(5) ? PM_NOREMOVE : hb_parnl(5)))
     {
       hb_storclen( (LPSTR) &Msg, sizeof(MSG),1);
       hb_retl( 1 );
     }
  else
     hb_retl ( 0 );

}


//-----------------------------------------------------------------------------
// WINUSERAPI UINT WINAPI RegisterWindowMessage( IN LPCWSTR lpString);


HB_FUNC( REGISTERWINDOWMESSAGE )
{
   hb_retni( RegisterWindowMessageA( (LPCSTR) hb_parcx( 1 ) ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI SetMessageQueue( IN int cMessagesMax);


HB_FUNC( SETMESSAGEQUEUE )
{
   hb_retl( SetMessageQueue( hb_parni( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI DWORD WINAPI GetMessagePos( VOID);


HB_FUNC( GETMESSAGEPOS )
{
   hb_retnl( (LONG) GetMessagePos(  ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI LONG WINAPI GetMessageTime( VOID);


HB_FUNC( GETMESSAGETIME )
{
   hb_retnl( (LONG) GetMessageTime(  ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI LPARAM WINAPI GetMessageExtraInfo( VOID);


HB_FUNC( GETMESSAGEEXTRAINFO )
{
   hb_retnl( (LONG) GetMessageExtraInfo(  ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI LPARAM WINAPI SetMessageExtraInfo( IN LPARAM lParam);


HB_FUNC( SETMESSAGEEXTRAINFO )
{
   hb_retnl( (LONG) SetMessageExtraInfo( (LPARAM) hb_parnl( 1 ) ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI LRESULT WINAPI SendMessageTimeoutA( IN HWND hWnd, IN UINT Msg, IN WPARAM wParam, IN LPARAM lParam, IN UINT fuFlags, IN UINT uTimeout, OUT PDWORD_PTR lpdwResult);

/*

HB_FUNC( SENDMESSAGETIMEOUT )
{
   PDWORD_PTR lpdwResult ;

   // Your code goes here

   hb_retnl( (LONG) SendMessageTimeout( (HWND) HB_PARWH( 1 )  ,
                                        (UINT) hb_parni( 2 )  ,
                                        (WPARAM) hb_parnl( 3 ),
                                        (LPARAM) hb_parnl( 4 ),
                                        (UINT) hb_parni( 5 )  ,
                                        (UINT) hb_parni( 6 )  ,
                                        lpdwResult
                                      ) );
}

*/

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI SendNotifyMessageA( IN HWND hWnd, IN UINT Msg, IN WPARAM wParam, IN LPARAM lParam);


HB_FUNC( SENDNOTIFYMESSAGE )
{
   hb_retl( SendNotifyMessage( (HWND) HB_PARWH( 1 )  ,
                               (UINT) hb_parni( 2 )  ,
                               (WPARAM) hb_parnl( 3 ),
                               (LPARAM) hb_parnl( 4 )
                             ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI PostThreadMessageA( IN DWORD idThread, IN UINT Msg, IN WPARAM wParam, IN LPARAM lParam);


HB_FUNC( POSTTHREADMESSAGE )
{
   hb_retl( PostThreadMessage( (DWORD) hb_parnl( 1 ) ,
                               (UINT) hb_parni( 2 )  ,
                               (WPARAM) hb_parnl( 3 ),
                               (LPARAM) hb_parnl( 4 )
                             ) );
}

//----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI ReplyMessage( IN LRESULT lResult);

HB_FUNC( REPLYMESSAGE )
{
   hb_retl( ReplyMessage( (LRESULT) hb_parnl( 1 ) ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI WaitMessage( VOID);

HB_FUNC( WAITMESSAGE )
{
   hb_retl( WaitMessage(  ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI DWORD WINAPI WaitForInputIdle( IN HANDLE hProcess, IN DWORD dwMilliseconds);

HB_FUNC( WAITFORINPUTIDLE )
{
   hb_retnl( (LONG) WaitForInputIdle( (HANDLE) HB_PARWH( 1 ),
                                      (DWORD) hb_parnl( 2 )
                                    ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI InSendMessage( VOID);

HB_FUNC( INSENDMESSAGE )
{
   hb_retl( InSendMessage(  ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI DWORD WINAPI InSendMessageEx( IN LPVOID lpReserved);


#if(WINVER >= 0x0500)

HB_FUNC( INSENDMESSAGEEX )
{

   hb_retnl( (LONG) InSendMessageEx( NULL ) ); // param reserved must be NULL
}

#endif


//-----------------------------------------------------------------------------
// WINUSERAPI DWORD WINAPI MsgWaitForMultipleObjects( IN DWORD nCount, IN CONST HANDLE *pHandles, IN BOOL fWaitAll, IN DWORD dwMilliseconds, IN DWORD dwWakeMask);


HB_FUNC( MSGWAITFORMULTIPLEOBJECTS )
{
   hb_retnl( (LONG) MsgWaitForMultipleObjects( (DWORD) hb_parnl( 1 ) ,
                                               (HANDLE *) HB_PARWI( 2 ),
                                               hb_parl( 3 ) ,
                                               (DWORD) hb_parnl( 4 ) ,
                                               (DWORD) hb_parnl( 5 )
                                             ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI DWORD WINAPI MsgWaitForMultipleObjectsEx( IN DWORD nCount, IN CONST HANDLE *pHandles, IN DWORD dwMilliseconds, IN DWORD dwWakeMask, IN DWORD dwFlags);


HB_FUNC( MSGWAITFORMULTIPLEOBJECTSEX )
{
   hb_retnl( (LONG) MsgWaitForMultipleObjectsEx( (DWORD) hb_parnl( 1 ) ,
                                                 (HANDLE *) HB_PARWI( 2 ),
                                                 (DWORD) hb_parnl( 3 ) ,
                                                 (DWORD) hb_parnl( 4 ) ,
                                                 (DWORD) hb_parnl( 5 )
                                               ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI SendMessageCallbackA( IN HWND hWnd, IN UINT Msg, IN WPARAM wParam, IN LPARAM lParam, IN SENDASYNCPROC lpResultCallBack, IN ULONG_PTR dwData);

/*

HB_FUNC( SENDMESSAGECALLBACK )
{
   SENDASYNCPROC lpResultCallBack ;
   ULONG_PTR     dwData           ;

   // Your code goes here

   hb_retl( SendMessageCallback( (HWND) HB_PARWH( 1 )  ,
                                 (UINT) hb_parni( 2 )  ,
                                 (WPARAM) hb_parnl( 3 ),
                                 (LPARAM) hb_parnl( 4 ),
                                 lpResultCallBack      ,
                                 dwData
                               ) );
}

*/

//-----------------------------------------------------------------------------
// WINUSERAPI long WINAPI BroadcastSystemMessageA( IN DWORD, IN LPDWORD, IN UINT, IN WPARAM, IN LPARAM);

/*

HB_FUNC( BROADCASTSYSTEMMESSAGE )
{
   LPDWORD lpdWord ;

   // Your code goes here

   hb_retnl( (LONG) BroadcastSystemMessage( (DWORD) hb_parnl( 1 ) ,
                                            lpdWord               ,
                                            (UINT) hb_parni( 3 )  ,
                                            (WPARAM) hb_parnl( 4 ),
                                            (LPARAM) hb_parnl( 5 )
                                          ) );
}

*/

//-----------------------------------------------------------------------------
// WINUSERAPI long WINAPI BroadcastSystemMessage( IN DWORD, IN LPDWORD, IN UINT, IN WPARAM, IN LPARAM);

/*

HB_FUNC( BROADCASTSYSTEMMESSAGE )
{
   LPDWORD lpdWord ;

   // Your code goes here

   hb_retnl( (LONG) BroadcastSystemMessage( (DWORD) hb_parnl( 1 ) ,
                                            lpdWord               ,
                                            (UINT) hb_parni( 3 )  ,
                                            (WPARAM) hb_parnl( 4 ),
                                            (LPARAM) hb_parnl( 5 )
                                          ) );
}

*/

//-----------------------------------------------------------------------------
// WINUSERAPI HDEVNOTIFY WINAPI RegisterDeviceNotificationA( IN HANDLE hRecipient, IN LPVOID NotificationFilter, IN DWORD Flags );

/*

HB_FUNC( REGISTERDEVICENOTIFICATION )
{
   LPVOID NotificationFilter ;

   // Your code goes here

   HB_RETWH( RegisterDeviceNotification( (HANDLE) HB_PARWH( 1 ),
                                                NotificationFilter    ,
                                                (DWORD) hb_parnl( 3 )
                                              ) );
}

*/

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI UnregisterDeviceNotification( IN HDEVNOTIFY Handle );

#if(WINVER >= 0x0500)

HB_FUNC( UNREGISTERDEVICENOTIFICATION )
{
   hb_retl( UnregisterDeviceNotification( (HDEVNOTIFY) HB_PARWH( 1 ) ) );
}

#endif

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI AttachThreadInput( IN DWORD idAttach, IN DWORD idAttachTo, IN BOOL fAttach);


HB_FUNC( ATTACHTHREADINPUT )
{

   hb_retl( AttachThreadInput( (DWORD) hb_parnl( 1 ) ,
                               (DWORD) hb_parnl( 2 ) ,
                               hb_parl( 3 )
                             ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI CallMsgFilterA( IN LPMSG lpMsg, IN int nCode);

HB_FUNC( CALLMSGFILTER )
{
   MSG *Msg = (MSG * ) hb_parc( 1 ); //hb_param( 1, HB_IT_STRING )->item.asString.value;

   hb_retl( CallMsgFilter( Msg, hb_parni( 2 ) ) );
}
