/*
 * $Id$
 */


// hbwhat
// Miscellaneous functions

// AJ Wos


#undef _WIN32_WINNT
#define _WIN32_WINNT   0x0400

#include "hbwhat.h"

#include <windows.h>
#include <shlobj.h>
//#include <commctrl.h>

#include <math.h>
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"


//-----------------------------------------------------------------------------

HB_FUNC( WHT_STR2PTR )
{
   char *cStr = hb_parcx( 1 );
   HB_RETWI( cStr );
}

//-----------------------------------------------------------------------------

HB_FUNC( WHT_PEEKW )
{
   hb_retni( ( int ) ( * ( LPWORD ) HB_PARWI( 1 ) ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( WHT_PEEKL )
{
   hb_retnl( ( long ) ( * (LPDWORD) HB_PARWI( 1 ) ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( WHT_PEEKB )
{
   hb_retni( ( int ) ( * ( LPBYTE ) HB_PARWI( 1 ) ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( WHT_POKEW )
{
   * ( LPWORD ) HB_PARWI( 1 ) = (WORD) hb_parni( 2 );
}

//-----------------------------------------------------------------------------

HB_FUNC( WHT_POKEL )
{
   * ( LPLONG ) HB_PARWI( 1 ) = (DWORD) hb_parnl( 2 );
}


//-----------------------------------------------------------------------------

HB_FUNC( WHT_POKEB )
{
   * ( LPBYTE ) HB_PARWI( 1 ) = ( BYTE ) hb_parni( 2 );
}


//-----------------------------------------------------------------------------
//peek(nPtr,[nLen])->cBuffer

HB_FUNC( WHT_PEEK )
{
 if ( hb_pcount()==2 )
    hb_retclen( (char *) HB_PARWI( 1 ), hb_parnl( 2 ) );
 else
    hb_retc( (char *) HB_PARWI( 1 ) );
}

//-----------------------------------------------------------------------------
// poke(nPtr,[nLen])->NIL

HB_FUNC( WHT_POKE )
{
   hb_xmemcpy( (char *) HB_PARWI(1), hb_parcx( 2 ), hb_pcount() == 3 ? ( ULONG ) hb_parnl( 3 ) : ( ULONG ) hb_parclen( 2 ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( WHT_D2BIN )
{
   BYTE  *Buffer;
   Buffer = (BYTE *) hb_xgrab( sizeof(double) );

   *( (double *) ( Buffer ) ) = ( double ) hb_parnd( 1 );
   hb_retclen( ( char *)Buffer, sizeof(double) );
   hb_xfree(Buffer);
}

//-----------------------------------------------------------------------------

HB_FUNC( WHT_F2BIN )
{
   BYTE  *Buffer;
   Buffer = (BYTE *) hb_xgrab( sizeof(float) );

   *( ( float *) ( Buffer ) ) = (float) hb_parnd( 1 );

   hb_retclen( ( char *)Buffer,sizeof(float) );
   hb_xfree(Buffer);

}

//-----------------------------------------------------------------------------

HB_FUNC( WHT_BIN2D )
{
  hb_retnd( *( (double *) hb_parcx( 1 ) ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( WHT_BIN2F )
{
   hb_retnd( (double) *( (float *) hb_parcx( 1 ) ) );
}


// Add PRG interface
// Add POINT conversion


//-----------------------------------------------------------------------------

// Not API - internal use

BOOL Array2Rect(PHB_ITEM aRect, RECT *rc )
{
   if (HB_IS_ARRAY(aRect) && hb_arrayLen(aRect) == 4) {
      rc->left   = hb_arrayGetNL(aRect,1);
      rc->top    = hb_arrayGetNL(aRect,2);
      rc->right  = hb_arrayGetNL(aRect,3);
      rc->bottom = hb_arrayGetNL(aRect,4);
      return TRUE ;
   }
   return FALSE;
}

/*

//-----------------------------------------------------------------------------

// Not API - internal use

BOOL Array2Rect(PHB_ITEM aRect, RECT *rc )
{
   if (HB_IS_ARRAY(aRect) && hb_itemSize(aRect) == 4) {

      PHB_ITEM item;

      if (!HB_IS_NUMERIC(item = hb_itemArrayGet(aRect, 1)))
         return FALSE;
      rc->left = hb_itemGetNL(item);

      if (!HB_IS_NUMERIC(item = hb_itemArrayGet(aRect, 2)))
         return FALSE;
      rc->top = hb_itemGetNL(item);

      if (!HB_IS_NUMERIC(item = hb_itemArrayGet(aRect, 3)))
         return FALSE;
      rc->right = hb_itemGetNL(item);

      if (!HB_IS_NUMERIC(item = hb_itemArrayGet(aRect, 4)))
         return FALSE;
      rc->bottom = hb_itemGetNL(item);

      return TRUE ;

   }
   return FALSE;
}

*/

//-----------------------------------------------------------------------------

// Not API - internal use

PHB_ITEM Rect2Array( RECT * rc  )
{
   PHB_ITEM aRect = hb_itemArrayNew( 4 );

   hb_arraySetNL( aRect, 1, rc->left );
   hb_arraySetNL( aRect, 2, rc->top );
   hb_arraySetNL( aRect, 3, rc->right );
   hb_arraySetNL( aRect, 4, rc->bottom );

   return aRect;
}


//-----------------------------------------------------------------------------

// Not API - internal use

BOOL Array2Point(PHB_ITEM aPoint, POINT *pt )
{
   if (HB_IS_ARRAY(aPoint) && hb_arrayLen(aPoint) == 2) {
      pt->x = hb_arrayGetNL(aPoint,1);
      pt->y = hb_arrayGetNL(aPoint,2);
      return TRUE ;
   }
   return FALSE;
}


/*

//-----------------------------------------------------------------------------

// Not API - internal use

BOOL Array2Point(PHB_ITEM aPoint, POINT *pt )
{
   if (HB_IS_ARRAY(aPoint) && hb_itemSize(aPoint) == 2) {

      PHB_ITEM item;

      if (!HB_IS_NUMERIC(item = hb_itemArrayGet(aPoint, 1)))
         return FALSE;
      pt->x = hb_itemGetNL(item);

      if (!HB_IS_NUMERIC(item = hb_itemArrayGet(aPoint, 2)))
         return FALSE;
      pt->y = hb_itemGetNL(item);

      return TRUE ;

   }
   return FALSE;
}
*/

//-----------------------------------------------------------------------------

// Not API - internal use

PHB_ITEM Point2Array( POINT * pt )
{
   PHB_ITEM aPoint = hb_itemArrayNew( 2 );

   hb_arraySetNL( aPoint, 1, pt->x );
   hb_arraySetNL( aPoint, 2, pt->y );

   return aPoint;
}

//-----------------------------------------------------------------------------

BOOL Array2Size(PHB_ITEM aSize, SIZE *siz )
{
   if( HB_IS_ARRAY( aSize ) && hb_arrayLen( aSize ) == 2 )
   {
      siz->cx = hb_arrayGetNL(aSize,1);
      siz->cy = hb_arrayGetNL(aSize,2);
      return TRUE;
   }
   return FALSE;
}

/*
//-----------------------------------------------------------------------------

BOOL Array2Size(PHB_ITEM aSize, SIZE *siz )
{
   if (HB_IS_ARRAY(aSize) && hb_itemSize(aSize) == 2) {

      PHB_ITEM item;

      if (!HB_IS_NUMERIC(item = hb_itemArrayGet(aSize, 1)))
         return FALSE;
      siz->cx = hb_itemGetNL(item);

      if (!HB_IS_NUMERIC(item = hb_itemArrayGet(aSize, 2)))
         return FALSE;
      siz->cy = hb_itemGetNL(item);

      return TRUE ;

   }
   return FALSE;
}
*/


//-----------------------------------------------------------------------------

// Not API - internal use

PHB_ITEM Size2Array( SIZE * siz )
{
   PHB_ITEM aSize = hb_itemArrayNew( 2 );

   hb_arraySetNL( aSize, 1, siz->cx );
   hb_arraySetNL( aSize, 2, siz->cy );

   return aSize;
}


//-----------------------------------------------------------------------------
// LPTSTR MAKEINTRESOURCE( WORD wInteger )  // integer to convert
// #define MAKEINTRESOURCE( i )  (LPTSTR) ((DWORD) ((WORD) (i)))

HB_FUNC( VWN_MAKEINRESOURCE )
{
  hb_retc( MAKEINTRESOURCE( (WORD) hb_parni( 1 ) ) );
}


//-----------------------------------------------------------------------------

HB_FUNC( VWN_MESSAGEBOX )
{
 // LPCSTR lpCaption =  hb_parcx(3);

  hb_retnl( MessageBox( ISNIL(1) ? NULL : (HWND) HB_PARWH(1) ,
                        (LPCSTR) hb_parcx(2),
                        ISNIL(3) ? NULL : (LPCSTR) hb_parcx(3) ,
                        ISNIL(4) ? 0 : (UINT) hb_parnl(4) ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_MESSAGEBEEP )
{
  hb_retl( MessageBeep( ISNIL(1) ? ( UINT ) 0xFFFFFFFF : ( UINT ) hb_parnl(1) ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_SETBIT )
{
   if( hb_pcount() < 3 || hb_parni( 3 ) )
      hb_retnl( hb_parnl(1) | ( 1 << (hb_parni(2)-1) ) );
   else
      hb_retnl( hb_parnl(1) & ~( 1 << (hb_parni(2)-1) ) );
}


//-----------------------------------------------------------------------------

HB_FUNC( VWN_CHECKBIT )
{
   hb_retl( hb_parnl(1) & ( 1 << (hb_parni(2)-1) ) );
}


//-----------------------------------------------------------------------------
// WINBASEAPI LPSTR WINAPI GetEnvironmentStrings( VOID );

// note: returns the actual pointer

HB_FUNC( VWN_GETENVIRONMENTSTRINGS )
{
   HB_RETWI( GetEnvironmentStrings() );
}



//-----------------------------------------------------------------------------
// WINBASEAPI LPSTR WINAPI GetEnvironmentStrings( VOID );

HB_FUNC( VWN_FREEENVIRONMENTSTRINGS )
{
   hb_retl( (LONG) FreeEnvironmentStrings( (LPTSTR) HB_PARWI( 1 ) ) );
}


//-----------------------------------------------------------------------------
// WINBASEAPI VOID WINAPI Sleep( IN DWORD dwMilliseconds );


HB_FUNC( VWN_SLEEP )
{
   Sleep( (DWORD) hb_parnl( 1 ) );
}

//-----------------------------------------------------------------------------
// WINBASEAPI UINT WINAPI SetHandleCount( IN UINT uNumber );


HB_FUNC( VWN_SETHANDLECOUNT )
{
   hb_retni( SetHandleCount( (UINT) hb_parni( 1 ) ) );
}

//-----------------------------------------------------------------------------
// WINBASEAPI DWORD WINAPI GetEnvironmentVariableA( IN LPCSTR lpName, OUT LPSTR lpBuffer, IN DWORD nSize );


HB_FUNC( VWN_GETENVIRONMENTVARIABLE )
{
   UINT dwLen = MAX_PATH ;
   char *cText = (char*) hb_xgrab( MAX_PATH+1 );
   DWORD dwRet ;

   dwRet = GetEnvironmentVariableA( (LPCSTR) hb_parcx( 1 ),
                                    (LPSTR) cText ,
                                    (DWORD) dwLen
                                  );
   hb_retclen( cText, dwRet );
   hb_xfree( cText );

}

//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI SetEnvironmentVariableA( IN LPCSTR lpName, IN LPCSTR lpValue );


HB_FUNC( VWN_SETENVIRONMENTVARIABLE )
{
   hb_retl( SetEnvironmentVariableA( (LPCSTR) hb_parcx( 1 ),
                                     (LPCSTR) hb_parcx( 2 )
                                     ) );
}


//-----------------------------------------------------------------------------
// WINBASEAPI UINT WINAPI WinExec( IN LPCSTR lpCmdLine, IN UINT uCmdShow );


HB_FUNC( VWN_WINEXEC )
{
   hb_retni( WinExec( (LPCSTR) hb_parcx( 1 ), (UINT) hb_parni( 2 ) ) );
}


// Mutex functions

//----------------------------------------------------------------------------
// HANDLE CreateMutex(LPSECURITY_ATTRIBUTES lpMutexAttributes, BOOL bInitialOwner, LPCTSTR lpName )

HB_FUNC( VWN_CREATEMUTEX )
{
   SECURITY_ATTRIBUTES *sa = NULL;

   if( ISCHAR(1) )
   {
       sa = (SECURITY_ATTRIBUTES *) hb_parc( 1 ); //hb_param( 1, HB_IT_STRING )->item.asString.value;
   }

   HB_RETWH( CreateMutex( ISNIL( 1 ) ? NULL : sa, hb_parnl( 2 ), hb_parcx( 3 ) ) );
}

//----------------------------------------------------------------------------
// HANDLE OpenMutex(DWORD dwDesiredAccess, BOOL bInheritHandle, LPCTSTR lpName )

HB_FUNC( VWN_OPENMUTEX )
{
  HB_RETWH( OpenMutex( hb_parnl( 1 ), hb_parl( 2 ), hb_parcx( 3 ) ) );
}

//----------------------------------------------------------------------------
// BOOL ReleaseMutex( HANDLE hMutex )

HB_FUNC( VWN_RELEASEMUTEX )
{
  hb_retl( ReleaseMutex( (HANDLE) HB_PARWH( 1 ) ) );
}



//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI RegisterHotKey( IN HWND hWnd, IN int id, IN UINT fsModifiers, IN UINT vk);


HB_FUNC( VWN_REGISTERHOTKEY )
{
   hb_retl( RegisterHotKey( (HWND) HB_PARWH( 1 ),
                            hb_parni( 2 )       ,
                            (UINT) hb_parni( 3 ),
                            (UINT) hb_parni( 4 )
                          ) );
}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI UnregisterHotKey( IN HWND hWnd, IN int id);


HB_FUNC( VWN_UNREGISTERHOTKEY )
{
   hb_retl( UnregisterHotKey( (HWND) HB_PARWH( 1 ), hb_parni( 2 ) ) );
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI GetClassInfoA( IN HINSTANCE hInstance, IN LPCSTR lpClassName, OUT LPWNDCLASSA lpWndClass);


// syntax:
// GetClasInfo(hInst,cClass) -> WNDCLASS struct or NIL


HB_FUNC( VWN_GETCLASSINFO )
{
   WNDCLASS WndClass  ;

   if ( GetClassInfo( ISNIL(1) ? NULL : (HINSTANCE) HB_PARWH( 1 ),
                      (LPCSTR) hb_parcx( 2 ), &WndClass ) )


     hb_retclen( (char*) &WndClass, sizeof(WNDCLASS) );

   // the line below GPFs !
   // hb_itemPutCRaw( hb_param( -1, HB_IT_ANY ), (char *) &WndClass, sizeof( WNDCLASS ) );

}




//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI GetClassInfoExA( IN HINSTANCE, IN LPCSTR, OUT LPWNDCLASSEXA);

// syntax:
// GetClasInfoEx(hInst,cClass) -> WNDCLASS struct or NIL


HB_FUNC( VWN_GETCLASSINFOEX )
{
   WNDCLASSEX WndClassEx ;

   if ( GetClassInfoEx( ISNIL(1) ? NULL : (HINSTANCE) HB_PARWH( 1 ),
                            (LPCSTR) hb_parcx( 2 ), &WndClassEx ) )

      hb_retclen( (char*) &WndClassEx, sizeof(WNDCLASSEX) );
      //hb_itemPutCRaw( hb_param( -1, HB_IT_ANY ), (char *) &WndClassEx, sizeof( WNDCLASSEX ) );

}


// Trig functions

//-----------------------------------------------------------------------------

HB_FUNC( VWN_SIN )
{
   hb_retnd(sin(hb_parnd(1)));
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_COS )
{
   hb_retnd(cos(hb_parnd(1)));
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_TAN )
{
   hb_retnd(tan(hb_parnd(1)));
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_ASIN )
{
   hb_retnd(asin(hb_parnd(1)));
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_ACOS )
{
   hb_retnd(acos(hb_parnd(1)));
}

//-----------------------------------------------------------------------------

HB_FUNC( VWN_ATAN )
{
   hb_retnd(atan(hb_parnd(1)));
}

//-----------------------------------------------------------------------------

/* Extended function for Array CopyRight Luiz Rafael Culik Guimaraes Culikr@uol.com.br */
void  Rect2ArrayEx( RECT * rc, PHB_ITEM aRect )
{
   hb_arraySetNL( aRect, 1, rc->left );
   hb_arraySetNL( aRect, 2, rc->top );
   hb_arraySetNL( aRect, 3, rc->right );
   hb_arraySetNL( aRect, 4, rc->bottom );
}

//-----------------------------------------------------------------------------

void Point2ArrayEx( POINT *pt, PHB_ITEM aPoint )
{
   hb_arraySetNL( aPoint, 1, pt->x );
   hb_arraySetNL( aPoint, 2, pt->y );
}

//-----------------------------------------------------------------------------

void Size2ArrayEx( SIZE * siz, PHB_ITEM aSize )
{
   hb_arraySetNL( aSize, 1, siz->cx );
   hb_arraySetNL( aSize, 2, siz->cy );
}
