/*
 * $Id$
 */


// what32.lib
// Miscellaneous functions

// AJ Wos


#define _WIN32_WINNT   0x0400

#include <windows.h>
#include <shlobj.h>
//#include <commctrl.h>

#include <math.h>
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"


//-----------------------------------------------------------------------------

HB_FUNC( STR2PTR )
{
   char *cStr = hb_parcx( 1 )    ;
   hb_retnl( ( LONG_PTR ) cStr ) ;
}

//-----------------------------------------------------------------------------

HB_FUNC( PEEKW )
{
   hb_retni( * ( LPWORD ) hb_parnl( 1 ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( PEEKL )
{
   hb_retnl( * (LPDWORD) hb_parnl( 1 ) ) ;
}

//-----------------------------------------------------------------------------

HB_FUNC( PEEKB )
{
   hb_retni( * ( LPBYTE ) hb_parnl( 1 ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( POKEW )
{
   * ( LPWORD ) hb_parnl( 1 ) = (WORD) hb_parni( 2 ) ;
}

//-----------------------------------------------------------------------------

HB_FUNC( POKEL )
{
   * ( LPLONG ) hb_parnl( 1 ) = (DWORD) hb_parnl( 2 ) ;
}


//-----------------------------------------------------------------------------

HB_FUNC( POKEB )
{
   * ( LPBYTE ) hb_parnl( 1 ) = hb_parni( 2 ) ;
}


//-----------------------------------------------------------------------------
//peek(nPtr,[nLen])->cBuffer

HB_FUNC( PEEK )
{
 if ( hb_pcount()==2 )
    hb_retclen( (char *) hb_parnl( 1 ), hb_parnl( 2 ) );
 else
    hb_retc( (char *) hb_parnl( 1 ) );
}

//-----------------------------------------------------------------------------
// poke(nPtr,[nLen])->NIL

HB_FUNC( POKE )
{
   if( hb_pcount() ==3 )
      hb_xmemcpy( (char *) hb_parnl(1), hb_parcx( 2 ), hb_parnl( 3 ) );
   else
      hb_xmemcpy( (char *) hb_parnl(1), hb_parcx( 2 ), hb_parclen( 2 ) );
}



//-----------------------------------------------------------------------------

HB_FUNC( D2BIN )
{
   BYTE  *Buffer;
   Buffer = (BYTE *) hb_xgrab( sizeof(double) );

   *( (double *) ( Buffer ) ) = ( double ) hb_parnd( 1 );
   hb_retclen( ( char *)Buffer, sizeof(double) );
   hb_xfree(Buffer);
}

//-----------------------------------------------------------------------------

HB_FUNC( F2BIN )
{
   BYTE  *Buffer;
   Buffer = (BYTE *) hb_xgrab( sizeof(float) );

   *( ( float *) ( Buffer ) ) = (float) hb_parnd( 1 );

   hb_retclen( ( char *)Buffer,sizeof(float) ) ;
   hb_xfree(Buffer);

}

//-----------------------------------------------------------------------------

HB_FUNC( BIN2D )
{
  hb_retnd( *( (double *) hb_parcx( 1 ) ) );
}

//-----------------------------------------------------------------------------

HB_FUNC( BIN2F )
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

PHB_ITEM Rect2Array( RECT *rc  )
{
   PHB_ITEM aRect = hb_itemArrayNew(4);
   PHB_ITEM element = hb_itemNew(NULL);

   hb_arraySet(aRect, 1, hb_itemPutNL(element, rc->left));
   hb_arraySet(aRect, 2, hb_itemPutNL(element, rc->top));
   hb_arraySet(aRect, 3, hb_itemPutNL(element, rc->right));
   hb_arraySet(aRect, 4, hb_itemPutNL(element, rc->bottom));
   hb_itemRelease(element);
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

PHB_ITEM Point2Array( POINT *pt  )
{
   PHB_ITEM aPoint = hb_itemArrayNew(2);
   PHB_ITEM element = hb_itemNew(NULL);

   hb_arraySet(aPoint, 1, hb_itemPutNL(element, pt->x));
   hb_arraySet(aPoint, 2, hb_itemPutNL(element, pt->y));
   hb_itemRelease(element);
   return aPoint;
}

//-----------------------------------------------------------------------------

BOOL Array2Size(PHB_ITEM aSize, SIZE *siz )
{
   if (HB_IS_ARRAY(aSize) && hb_arrayLen(aSize) == 2) {
      siz->cx = hb_arrayGetNL(aSize,1);
      siz->cy = hb_arrayGetNL(aSize,2);
      return TRUE ;
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

PHB_ITEM Size2Array( SIZE *siz  )
{
   PHB_ITEM aSize = hb_itemArrayNew(2);
   PHB_ITEM element = hb_itemNew(NULL);

   hb_arraySet(aSize, 1, hb_itemPutNL(element, siz->cx));
   hb_arraySet(aSize, 2, hb_itemPutNL(element, siz->cy));
   hb_itemRelease(element);
   return aSize;
}


//-----------------------------------------------------------------------------
// LPTSTR MAKEINTRESOURCE( WORD wInteger )  // integer to convert
// #define MAKEINTRESOURCE( i )  (LPTSTR) ((DWORD) ((WORD) (i)))

HB_FUNC( MAKEINRESOURCE )
{
  hb_retc( MAKEINTRESOURCE( (WORD) hb_parni( 1 ) ) ) ;
}


//-----------------------------------------------------------------------------

HB_FUNC( MESSAGEBOX )
{
 // LPCSTR lpCaption =  hb_parcx(3) ;

  hb_retnl( MessageBox( ISNIL(1) ? NULL : (HWND) hb_parnl(1) ,
                        (LPCSTR) hb_parcx(2),
                        ISNIL(3) ? NULL : (LPCSTR) hb_parcx(3) ,
                        ISNIL(4) ? 0 : (UINT) hb_parnl(4) ) ) ;
}

//-----------------------------------------------------------------------------

HB_FUNC( MESSAGEBEEP )
{
  hb_retl( MessageBeep( ISNIL(1) ?  (0xFFFFFFFF) : hb_parnl(1) ) ) ;
}

//-----------------------------------------------------------------------------

HB_FUNC( SETBIT )
{
   if( hb_pcount() < 3 || hb_parni( 3 ) )
      hb_retnl( hb_parnl(1) | ( 1 << (hb_parni(2)-1) ) );
   else
      hb_retnl( hb_parnl(1) & ~( 1 << (hb_parni(2)-1) ) );
}


//-----------------------------------------------------------------------------

HB_FUNC( CHECKBIT )
{
   hb_retl( hb_parnl(1) & ( 1 << (hb_parni(2)-1) ) );
}


//-----------------------------------------------------------------------------
// WINBASEAPI LPSTR WINAPI GetEnvironmentStrings( VOID );

// note: returns the actual pointer

HB_FUNC( GETENVIRONMENTSTRINGS )
{
   hb_retnl( (LONG) GetEnvironmentStrings(  ) ) ;
}



//-----------------------------------------------------------------------------
// WINBASEAPI LPSTR WINAPI GetEnvironmentStrings( VOID );

HB_FUNC( FREEENVIRONMENTSTRINGS )
{
   hb_retl( (LONG) FreeEnvironmentStrings( (LPTSTR) hb_parnl(1) ) ) ;
}


//-----------------------------------------------------------------------------
// WINBASEAPI VOID WINAPI Sleep( IN DWORD dwMilliseconds );


HB_FUNC( SLEEP )
{
   Sleep( (DWORD) hb_parnl( 1 ) ) ;
}

//-----------------------------------------------------------------------------
// WINBASEAPI UINT WINAPI SetHandleCount( IN UINT uNumber );


HB_FUNC( SETHANDLECOUNT )
{
   hb_retni( SetHandleCount( (UINT) hb_parni( 1 ) ) ) ;
}

//-----------------------------------------------------------------------------
// WINBASEAPI DWORD WINAPI GetEnvironmentVariableA( IN LPCSTR lpName, OUT LPSTR lpBuffer, IN DWORD nSize );


HB_FUNC( GETENVIRONMENTVARIABLE )
{
   UINT dwLen = MAX_PATH ;
   char *cText = (char*) hb_xgrab( MAX_PATH+1 );
   DWORD dwRet ;

   dwRet = GetEnvironmentVariableA( (LPCSTR) hb_parcx( 1 ),
                                    (LPSTR) cText ,
                                    (DWORD) dwLen
                                  ) ;
   hb_retclen( cText, dwRet );
   hb_xfree( cText );

}

//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI SetEnvironmentVariableA( IN LPCSTR lpName, IN LPCSTR lpValue );


HB_FUNC( SETENVIRONMENTVARIABLE )
{
   hb_retl( SetEnvironmentVariableA( (LPCSTR) hb_parcx( 1 ),
                                     (LPCSTR) hb_parcx( 2 )
                                     ) ) ;
}


//-----------------------------------------------------------------------------
// WINBASEAPI UINT WINAPI WinExec( IN LPCSTR lpCmdLine, IN UINT uCmdShow );


HB_FUNC( WINEXEC )
{
   hb_retni( WinExec( (LPCSTR) hb_parcx( 1 ), (UINT) hb_parni( 2 ) ) ) ;
}

//-----------------------------------------------------------------------------

//  Helper routine.  Take an input pointer, return closest
//  pointer that is aligned on a DWORD (4 byte) boundary.

LPWORD lpwAlign ( LPWORD lpIn)
{
  ULONG ul;
  ul = (ULONG) lpIn;
  ul +=3;
  ul >>=2;
  ul <<=2;
  return (LPWORD) ul;
}

//-----------------------------------------------------------------------------


int nCopyAnsiToWideChar (LPWORD lpWCStr, LPSTR lpAnsiIn)
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

//----------------------------------------------------------------------------

// Helper routine.  Takes second parameter as Ansi string, copies
// it to first parameter as wide character (16-bits / char) string,
// and returns integer number of wide characters (words) in string
// (including the trailing wide char NULL).

int nCopyAnsiToWideChar (LPWORD lpWCStr, LPSTR lpAnsiIn)
{
  int cchAnsi = lstrlen( lpAnsiIn );
  return MultiByteToWideChar(GetACP(), MB_PRECOMPOSED, lpAnsiIn, cchAnsi, lpWCStr, cchAnsi) +1;
}

*/


// Mutex functions

//----------------------------------------------------------------------------
// HANDLE CreateMutex(LPSECURITY_ATTRIBUTES lpMutexAttributes, BOOL bInitialOwner, LPCTSTR lpName )

HB_FUNC( CREATEMUTEX )
{
   SECURITY_ATTRIBUTES *sa = NULL;

   if( ISCHAR(1) )
   {
       sa = (SECURITY_ATTRIBUTES *) hb_parc( 1 ); //hb_param( 1, HB_IT_STRING )->item.asString.value;
   }

   hb_retnl( (ULONG) CreateMutex( ISNIL( 1 ) ? NULL : sa, hb_parnl( 2 ), hb_parcx( 3 ) ) );
}

//----------------------------------------------------------------------------
// HANDLE OpenMutex(DWORD dwDesiredAccess, BOOL bInheritHandle, LPCTSTR lpName )

HB_FUNC( OPENMUTEX )
{
  hb_retnl( (ULONG) OpenMutex( hb_parnl( 1 ), hb_parl( 2 ), hb_parcx( 3 ) ) );
}

//----------------------------------------------------------------------------
// BOOL ReleaseMutex( HANDLE hMutex )

HB_FUNC( RELEASEMUTEX )
{
  hb_retl( ReleaseMutex( (HANDLE) hb_parnl( 1 ) ) );
}



//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI RegisterHotKey( IN HWND hWnd, IN int id, IN UINT fsModifiers, IN UINT vk);


HB_FUNC( REGISTERHOTKEY )
{
   hb_retl( RegisterHotKey( (HWND) hb_parnl( 1 ),
                            hb_parni( 2 )       ,
                            (UINT) hb_parni( 3 ),
                            (UINT) hb_parni( 4 )
                          ) ) ;
}

//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI UnregisterHotKey( IN HWND hWnd, IN int id);


HB_FUNC( UNREGISTERHOTKEY )
{
   hb_retl( UnregisterHotKey( (HWND) hb_parnl( 1 ), hb_parni( 2 ) ) ) ;
}


//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI GetClassInfoA( IN HINSTANCE hInstance, IN LPCSTR lpClassName, OUT LPWNDCLASSA lpWndClass);


// syntax:
// GetClasInfo(hInst,cClass) -> WNDCLASS struct or NIL


HB_FUNC( GETCLASSINFO )
{
   WNDCLASS WndClass  ;

   if ( GetClassInfo( ISNIL(1) ? NULL : (HINSTANCE) hb_parnl( 1 ),
                      (LPCSTR) hb_parcx( 2 ), &WndClass ) )


     hb_retclen( (char*) &WndClass, sizeof(WNDCLASS) ) ;

   // the line below GPFs !
   // hb_itemPutCRaw( hb_param( -1, HB_IT_ANY ), (char *) &WndClass, sizeof( WNDCLASS ) );

}




//-----------------------------------------------------------------------------
// WINUSERAPI BOOL WINAPI GetClassInfoExA( IN HINSTANCE, IN LPCSTR, OUT LPWNDCLASSEXA);

// syntax:
// GetClasInfoEx(hInst,cClass) -> WNDCLASS struct or NIL


HB_FUNC( GETCLASSINFOEX )
{
   WNDCLASSEX WndClassEx ;

   if ( GetClassInfoEx( ISNIL(1) ? NULL : (HINSTANCE) hb_parnl( 1 ),
                            (LPCSTR) hb_parcx( 2 ), &WndClassEx ) )

      hb_retclen( (char*) &WndClassEx, sizeof(WNDCLASSEX) ) ;
      //hb_itemPutCRaw( hb_param( -1, HB_IT_ANY ), (char *) &WndClassEx, sizeof( WNDCLASSEX ) );

}


// Trig functions

//-----------------------------------------------------------------------------

HB_FUNC( SIN )
{
   hb_retnd(sin(hb_parnd(1)));
}

//-----------------------------------------------------------------------------

HB_FUNC( COS )
{
   hb_retnd(cos(hb_parnd(1)));
}

//-----------------------------------------------------------------------------

HB_FUNC( TAN )
{
   hb_retnd(tan(hb_parnd(1)));
}

//-----------------------------------------------------------------------------

HB_FUNC( ASIN )
{
   hb_retnd(asin(hb_parnd(1)));
}

//-----------------------------------------------------------------------------

HB_FUNC( ACOS )
{
   hb_retnd(acos(hb_parnd(1)));
}

//-----------------------------------------------------------------------------

HB_FUNC( ATAN )
{
   hb_retnd(atan(hb_parnd(1)));
}

//-----------------------------------------------------------------------------

/* Extended function for Array CopyRight Luiz Rafael Culik Guimaraes Culikr@uol.com.br */
void  Rect2ArrayEx( RECT *rc ,PHB_ITEM aRect )
{
   PHB_ITEM element = hb_itemNew(NULL);

   hb_arraySet(aRect, 1, hb_itemPutNL(element, rc->left));
   hb_arraySet(aRect, 2, hb_itemPutNL(element, rc->top));
   hb_arraySet(aRect, 3, hb_itemPutNL(element, rc->right));
   hb_arraySet(aRect, 4, hb_itemPutNL(element, rc->bottom));
   hb_itemRelease(element);
}

//-----------------------------------------------------------------------------

void Point2ArrayEx( POINT *pt  , PHB_ITEM aPoint)
{

   PHB_ITEM element = hb_itemNew(NULL);

   hb_arraySet(aPoint, 1, hb_itemPutNL(element, pt->x));
   hb_arraySet(aPoint, 2, hb_itemPutNL(element, pt->y));
   hb_itemRelease(element);

}

//-----------------------------------------------------------------------------

void Size2ArrayEx( SIZE *siz ,PHB_ITEM aSize )
{
   PHB_ITEM element = hb_itemNew(NULL);

   hb_arraySet(aSize, 1, hb_itemPutNL(element, siz->cx));
   hb_arraySet(aSize, 2, hb_itemPutNL(element, siz->cy));
   hb_itemRelease(element);

}
