/*
 * $Id$
 */

// What32
// Windows type definitions


/*
NOTE: If you define INT as the C type, (eg. in WinTypes.ch)
It may clash with INT() function in Harbour.

Make sure that when you use INT() function, you type it in mixed case
eg. Int(), otherwise, a difficult to trace and understand compile time
error will be occur.
*/


//#include "cstruct.ch"


#Ifndef VOID

#define VOID           0
#define UINT          -3 //CTYPE_UNSIGNED_INT
#define int            3 //CTYPE_INT
#define HANDLE        -4 //CTYPE_UNSIGNED_LONG
#define HICON         -4 //CTYPE_UNSIGNED_LONG
#define HBITMAP       -4 //CTYPE_UNSIGNED_LONG
#define HCURSOR       -4 //CTYPE_UNSIFNED_LONG
#define HBRUSH        -4 //CTYPE_UNSIGNED_LONG
#define LPCSTR        10 //CTYPE_CHAR_PTR
#define WNDPROC       -4 //CTYPE_UNSIGNED_LONG
#define BOOL           4 // 4 //CTYPE_LONG          // ADD CTYPE_BOOL to xHarbour structures ( Please !!! )
#define LPVOID         7 //CTYPE_VOID_PTR
#define DWORD         -4 //CTYPE_UNSIGNED_LONG
#define WORD          -2 //CTYPE_UNSIGNED_SHORT
#define LPCTSTR       10 //CTYPE_CHAR_PTR
#define COLORREF      -4 //CTYPE_UNSIGNED_LONG
#define BYTE           1 //CTYPE_CHAR
#define TCHAR         -1 //CTYPE_UNSIGNED_CHAR
#define HINSTANCE     -4 //CTYPE_UNSIGNED_LONG
#define HMENU         -4 //CTYPE_UNSIGNED_LONG
#define HTREEITEM     -4 //CTYPE_UNSIGNED_LONG
#define INT            3 //CTYPE_INT
#define HWND          -4 //CTYPE_UNSIGNED_LONG
#define LPARAM         4 //CTYPE_LONG
#define HGLOBAL       -4 //CTYPE_UNSIGNED_LONG
#define WPARAM         3 //CTYPE_INT
#define HKEY          -4 //CTYPE_UNSIGNED_LONG
#define char          -1 //CTYPE_CHAR
#define LONG           4 //CTYPE_LONG
#define BCHAR         -1 //CTYPE_UNSIGNED_CHAR
#define WCHAR         -2 //CTYPE_UNSIGNED_SHORT
#define DOUBLE         6 //CTYPE_DOUBLE
#define LPTSTR        10 //CTYPE_CHAR_PTR
#define LPSTR         10 //CTYPE_CHAR_PTR
#define ULONG         -4 //CTYPE_UNSIGNED_LONG
#define UCHAR         -1 //CTYPE_UNSIGNED_CHAR
#define SHORT          2
#define USHORT        -2
#define PVOID          7 //CTYPE_VOID_PTR
#define ULONG_PTR    -40 //CTYPE_UNSIGNED_LONG_PTR


#define LPOFNHOOKPROC   -4 // CTYPE_UNSIGNED_LONG
#define LPCFHOOKPROC    -4
#define LPFRHOOKPROC    -4
#define LPPAGESETUPHOOK -4
#define LPPAGEPAINTHOOK -4
#define LPPRINTHOOKPROC -4
#define LPSETUPHOOKPROC -4

#define BFFCALLBACK     -4

#define HDC           -4
#define HIMAGELIST    -4

#endif

