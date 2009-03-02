/*
 * $Id$
 */


// WHAT 32
// Keyboard functions

/*

Modified functions:

  GetKeyboardState() -> cKeyboard
  GetKeyNameText() -> cKeyName
  CreateAcceleratorTable( aTable ) -> lSuccess

*/


#define HB_OS_WIN_USED
#undef _WIN32_WINNT
#define _WIN32_WINNT   0x0400

#include "hbwhat.h"

#include <windows.h>
//#include <commctrl.h>
//#include <winuser.h>
#include "hbapiitm.h"
#include "hbapi.h"
//#include "commctrl.h"


//-----------------------------------------------------------------------------
// WINUSERAPI DWORD WINAPI OemKeyScan( IN WORD wOemChar);

HB_FUNC( VWN_OEMKEYSCAN )
{
   hb_retnl( OemKeyScan( (WORD) hb_parni(1) ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI SHORT WINAPI VkKeyScanA( IN CHAR ch);

HB_FUNC( VWN_VKKEYSCAN )
{
   char *Buffer ;
   Buffer =hb_parcx( 1 );

   hb_retni( VkKeyScan( *Buffer ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI SHORT WINAPI VkKeyScanExA( IN CHAR ch, IN HKL dwhkl);

HB_FUNC( VWN_VKKEYSCANEX )
{
   char *Buffer ;
   Buffer = hb_parcx( 1 );

   hb_retni( VkKeyScanEx( *Buffer, (HKL) HB_PARWH( 2 ) ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI UINT WINAPI GetKBCodePage( VOID);


HB_FUNC( VWN_GETKBCODEPAGE )
{
   hb_retni( GetKBCodePage(  ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI SHORT WINAPI GetKeyState( IN int nVirtKey);


HB_FUNC( VWN_GETKEYSTATE )
{
   hb_retni( GetKeyState( hb_parni( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI SHORT WINAPI GetAsyncKeyState( IN int vKey);


HB_FUNC( VWN_GETASYNCKEYSTATE )
{
  hb_retni( GetAsyncKeyState( hb_parni( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI GetKeyboardState( OUT PBYTE lpKeyState);

// Syntax:
// GetKeyboardState() -> cKeyboardState

HB_FUNC( VWN_GETKEYBOARDSTATE )
{
   BYTE lpKeyState[256] ;

   if ( GetKeyboardState( lpKeyState ))
     hb_retclen( ( char *) lpKeyState, 256 );
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI SetKeyboardState( IN LPBYTE lpKeyState);

// Syntax:
// SetKeyboardState(cKeyboardState) -> lSuccess
// cKeyboardState must be 256 char long

HB_FUNC( VWN_SETKEYBOARDSTATE )
{
   hb_retl( SetKeyboardState( (LPBYTE) hb_parcx(1) ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI GetKeyNameTextA( IN LONG lParam, OUT LPSTR lpString, IN int nSize );

// syntax:
// GetKeyNameText( nKey) -> cText

HB_FUNC( VWN_GETKEYNAMETEXT )
{

   char cText[MAX_PATH] ;
   int iRet = GetKeyNameText( hb_parnl( 1 ), cText, MAX_PATH );
   if ( iRet )
     hb_retclen( cText, iRet );

}


//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI GetKeyboardType( IN int nTypeFlag);


HB_FUNC( VWN_GETKEYBOARDTYPE )
{
   hb_retni( GetKeyboardType( hb_parni( 1 ) ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI UINT WINAPI MapVirtualKeyA( IN UINT uCode, IN UINT uMapType);


HB_FUNC( VWN_MAPVIRTUALKEY )
{
   hb_retni( MapVirtualKey( (UINT) hb_parni( 1 ), (UINT) hb_parni( 2 ) ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI UINT WINAPI MapVirtualKeyExA( IN UINT uCode, IN UINT uMapType, IN HKL dwhkl);


HB_FUNC( VWN_MAPVIRTUALKEYEX )
{
   hb_retni( MapVirtualKeyEx( (UINT) hb_parni( 1 ),
                              (UINT) hb_parni( 2 ),
                              (HKL) HB_PARWH( 3 )
                            ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI GetInputState( VOID);

HB_FUNC( VWN_GETINPUTSTATE )
{
   hb_retl( GetInputState(  ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI DWORD WINAPI GetQueueStatus( IN UINT flags);


HB_FUNC( VWN_GETQUEUESTATUS )
{
   hb_retnl( (LONG) GetQueueStatus( (UINT) hb_parni( 1 ) ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI HACCEL WINAPI LoadAcceleratorsA( IN HINSTANCE hInstance, IN LPCSTR lpTableName);


HB_FUNC( VWN_LOADACCELERATORS )
{
   HB_RETWH( LoadAccelerators( (HINSTANCE) HB_PARWH( 1 ),
                                      (LPCSTR) hb_parcx( 2 )
                                    ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI HACCEL WINAPI CreateAcceleratorTableA( IN LPACCEL, IN int);

// requires array of arrays corresponding to ACCEL structure

// SYNTAX:
// CreateAcceleratorTable(aAccel) -> hAccelTable or 0

// to be tested

HB_FUNC( VWN_CREATEACCELERATORTABLE )
{

   ACCEL * aAccel ;
   INT iCount ;
   INT i ;
   PHB_ITEM aSub ;
   PHB_ITEM aParam ;

   if ( hb_parinfo( 1 ) == HB_IT_ARRAY  )
   {
       iCount = hb_parinfa( 1, 0 );
       aAccel = (ACCEL *) hb_xgrab( iCount*sizeof(ACCEL) );
       aParam = hb_param( 1, HB_IT_ARRAY );
       for ( i= 0 ; i<iCount ; i++ )
          {
             aSub = hb_itemArrayGet( aParam, i+1 );
             aAccel[i].fVirt = (BYTE) hb_arrayGetNI(aSub,1);
             aAccel[i].key   = (WORD) hb_arrayGetNI(aSub,2);
             aAccel[i].cmd   = (WORD) hb_arrayGetNI(aSub,3);
          }
      HB_RETWH( CreateAcceleratorTable( aAccel, iCount ) );
      hb_xfree(aAccel);
   }
   else
      hb_retnl(0);
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI DestroyAcceleratorTable( IN HACCEL hAccel);

HB_FUNC( VWN_DESTROYACCELERATORTABLE )
{
   hb_retl( DestroyAcceleratorTable( (HACCEL) HB_PARWH( 1 ) ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI CopyAcceleratorTableA( IN HACCEL hAccelSrc, OUT LPACCEL lpAccelDst, IN int cAccelEntries);

// SYNTAX
// CopyAcceleratorTable(hAccel,aAccel) -> nEntries, or nCopied

// to be tested

HB_FUNC( VWN_COPYACCELERATORTABLE )
{
   LPACCEL lpAccelDst = NULL;
   int iCount = 0;
   int iRet ;
   PHB_ITEM aParam ;
   PHB_ITEM aSub ;
   int i ;

   if ( ISARRAY(2) && ((iCount=hb_parinfa(2,0)) > 0 ) )
      lpAccelDst = (LPACCEL) hb_xgrab( iCount * sizeof(ACCEL) );

   iRet = CopyAcceleratorTable( (HACCEL) HB_PARWH( 1 ) ,
                                   (iCount==0 ? NULL : lpAccelDst ) ,
                                   iCount
                              );

   if ( ( iCount > 0 ) && (iRet > 0 ) )
   {
      // read accelerator table elements into a subarrays
      // and store them into the original array elements

      aParam = hb_param( 2, HB_IT_ARRAY );
      aSub = hb_itemArrayNew( 3 );
      for ( i = 0 ; i < iCount ; i++ )
      {
         hb_arraySetNI( aSub, 1, lpAccelDst->fVirt );
         hb_arraySetNI( aSub, 2, lpAccelDst->key   );
         hb_arraySetNI( aSub, 3, lpAccelDst->cmd   );
         hb_arraySet( aParam, i + 1, hb_arrayClone( aSub ) );
      }
      hb_itemRelease( aSub );
   }

   if( iCount > 0 )
      hb_xfree( lpAccelDst );
   hb_retni( iRet );
}


//-----------------------------------------------------------------------------
// WINUSERAPI int WINAPI TranslateAcceleratorA( IN HWND hWnd, IN HACCEL hAccTable, IN LPMSG lpMsg);


HB_FUNC( VWN_TRANSLATEACCELERATOR )
{
   LPMSG  lpMsg = ( MSG * ) hb_parc( 3 ); //hb_param(3, HB_IT_STRING)->item.asString.value;
   hb_retni( TranslateAccelerator( (HWND) HB_PARWH( 1 )  ,
                                   (HACCEL) HB_PARWH( 2 ),
                                   lpMsg
                                 ) );
}

/*


//LRESULT CallNextHookEx(
//  HHOOK hhk,      // handle to current hook
//  int nCode,      // hook code passed to hook procedure
//  WPARAM wParam,  // value passed to hook procedure
//  LPARAM lParam   // value passed to hook procedure
//);

HB_FUNC( VWN_CALLNEXTHOOKEX )
{
    CallNextHookEx( (HHOOK) hb_parni(1), (int) hb_parni(2), (WPARAM) hb_parnint(3), (LPARAM) hb_parnint(4) );
}




//HHOOK SetWindowsHookEx(
//  int idHook,        // type of hook to install
//  HOOKPROC lpfn,     // address of hook procedure
//  HINSTANCE hMod,    // handle to application instance
//  DWORD dwThreadId   // identity of thread to install hook for
//);

HB_FUNC( VWN_SETWINDOWSHOOKEX )
{
    SetWindowsHookEx( (int) hb_parni(1), (HOOKPROC) HB_PARWH(2), (HINSTANCE) HB_PARWH(3), (DWORD) hb_parnl(4) );
}

HB_FUNC( VWN_MAKEPROCINSTANCE )
{
   hb_retc( (FARPROC) MakeProcInstance( (FARPROC) hb_parcx(1), (HINSTANCE) HB_PARWH(2) ) );
}


//BOOL UnhookWindowsHookEx(
//  HHOOK hhk   // handle to hook procedure to remove
//);

HB_FUNC( VWN_UNHOOKWINDOWSHOOKEX )
{
    UnhookWindowsHookEx( (HHOOK) HB_PARWH(1) );
}


HB_FUNC( VWN_GETCURRENTPROCESS )
{
   GetCurrentProcess();
}
*/
