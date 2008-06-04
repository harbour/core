/*
 * $Id$
 */


// What32
// DLL access functions

#define _WIN32_WINNT   0x0400


#include <windows.h>
#include <shlobj.h>
//#include <commctrl.h>

//#include "hbdate.h"
#include "hbvmpub.h"
#include "hbinit.h"
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include <ocidl.h>
#include <olectl.h>
#include "math.h"
#include "ctype.h"
#include "stdlib.h"
#include <time.h>

//------------------------------------------------------------------
#include <stdio.h>
//#include <stdlib.h>
#define  WIN32_LEAN_AND_MEAN
//#include <windows.h>
//#include "dynacall.h"


//-----------------------------------------------------------------------------
// WINBASEAPI HMODULE WINAPI LoadLibraryA( IN LPCSTR lpLibFileName );
/*

HB_FUNC( LOADLIBRARY )
{
   hb_retnl( (LONG) LoadLibraryA( (LPCSTR) hb_parcx( 1 ) ) ) ;
}
*/
//-----------------------------------------------------------------------------
// WINBASEAPI HMODULE WINAPI LoadLibraryExA( IN LPCSTR lpLibFileName, IN HANDLE hFile, IN DWORD dwFlags );


HB_FUNC( LOADLIBRARYEX )
{
   hb_retnl( (LONG) LoadLibraryExA( (LPCSTR) hb_parcx( 1 ) ,
                                    (HANDLE) hb_parnl( 2 ),
                                    (DWORD) hb_parnl( 3 )
                                    ) ) ;
}


//-----------------------------------------------------------------------------
// WINBASEAPI BOOL WINAPI FreeLibrary( IN OUT HMODULE hLibModule );
/*

HB_FUNC( FREELIBRARY )
{
   hb_retl( FreeLibrary( (HMODULE) hb_parnl( 1 ) ) ) ;
}
*/

//-----------------------------------------------------------------------------
// WINBASEAPI DECLSPEC_NORETURN VOID WINAPI FreeLibraryAndExitThread( IN HMODULE hLibModule, IN DWORD dwExitCode );


HB_FUNC( FREELIBRARYANDEXITTHREAD )
{
   FreeLibraryAndExitThread( (HMODULE) hb_parnl( 1 ),
                             (DWORD) hb_parnl( 2 )
                            ) ;
}

//-----------------------------------------------------------------------------
// WINBASEAPI FARPROC WINAPI GetProcAddress( IN HMODULE hModule, IN LPCSTR lpProcName );
/*
HB_FUNC( GETPROCADDRESS )
{
  ULONG dwProcAddr;
  char  cFuncName[MAX_PATH];

    if ((dwProcAddr = (ULONG) GetProcAddress( (HMODULE) hb_parnl(1),
                                              ISCHAR( 2 ) ? (LPCSTR) hb_parcx(2) :
                                              (LPCSTR) MAKELONG((WORD) hb_parni(2), 0) ) ) == 0 )
    {
       if ( ISCHAR( 2 ) )
       {
            // try forced ANSI flavour ?
           strcpy(cFuncName, hb_parcx(2));
           strcat(cFuncName, "A");
           dwProcAddr = (ULONG) GetProcAddress((HMODULE) hb_parnl(1), cFuncName);
       }
    }

    hb_retnl( dwProcAddr );

}
*/

//------------------------------------------------------------------


//////////////////////////////////////////////////////
//
//   This part used modified code of Vic McClung.
//   The modifications were to separate the library
//   loading and getting the procedure address
//   from the actual function call.
//   The parameters have been slightly re-arranged
//   to allow for C-like syntax, on function
//   declaration. The changes allow to load the library
//   and to get the procedure addresses in advance,
//   which makes it work similarly to C import libraries.
//   From experience, when using dynamic libraries, loading
//   the library and getting the address of the procedure
//   part of using the DLL.
//   Additionally the changes will allow to use standard
//   xHarbour C type defines, as used with structure types,
//   ande defined in cstruct.ch .
//
//
//   Andrew Wos.
//   20/07/2002.
//
//
//////////////////////////////////////////////////////




/*

Copyright 2002 Vic McClung <vicmcclung@vicmcclung.com>
www - http://www.vicmcclung.com

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).

As a special exception, you have permission for
additional uses of the text contained in this release of VMGUI.

The exception is that, if you link the VMGUI library with other
files to produce an executable, this does not by itself cause the
resulting executable to be covered by the GNU General Public License.
Your use of that executable is in no way restricted on account of
linking the VMGUI library code into it.

*/



#define  DC_MICROSOFT           0x0000      // Default
#define  DC_BORLAND             0x0001      // Borland compat
#define  DC_CALL_CDECL          0x0010      // __cdecl
#define  DC_CALL_STD            0x0020      // __stdcall
#define  DC_RETVAL_MATH4        0x0100      // Return value in ST
#define  DC_RETVAL_MATH8        0x0200      // Return value in ST

#define  DC_CALL_STD_BO         (DC_CALL_STD | DC_BORLAND)
#define  DC_CALL_STD_MS         (DC_CALL_STD | DC_MICROSOFT)
#define  DC_CALL_STD_M8         (DC_CALL_STD | DC_RETVAL_MATH8)

#define  DC_FLAG_ARGPTR         0x00000002

#pragma pack(1)

typedef union RESULT {          // Various result types
    int     Int;                // Generic four-byte type
    long    Long;               // Four-byte long
    void   *Pointer;            // 32-bit pointer
    float   Float;              // Four byte real
    double  Double;             // 8-byte real
    __int64 int64;              // big int (64-bit)
} RESULT;

typedef struct DYNAPARM {
    DWORD       dwFlags;        // Parameter flags
    int         nWidth;         // Byte width
    union {                     //
        DWORD   dwArg;          // 4-byte argument
        void   *pArg;           // Pointer to argument
    };
} DYNAPARM;

#pragma pack()


#define CTYPE_VOID 0
#define CTYPE_CHAR 1

#define CTYPE_UNSIGNED_CHAR -1
#define CTYPE_CHAR_PTR 10
#define CTYPE_UNSIGNED_CHAR_PTR -10

#define CTYPE_SHORT 2
#define CTYPE_UNSIGNED_SHORT -2
#define CTYPE_SHORT_PTR 20
#define CTYPE_UNSIGNED_SHORT_PTR -20

#define CTYPE_INT 3
#define CTYPE_UNSIGNED_INT -3
#define CTYPE_INT_PTR 30
#define CTYPE_UNSIGNED_INT_PTR -30

#define CTYPE_LONG 4
#define CTYPE_UNSIGNED_LONG -4
#define CTYPE_LONG_PTR 40
#define CTYPE_UNSIGNED_LONG_PTR -40

#define CTYPE_FLOAT 5
#define CTYPE_FLOAT_PTR 50

#define CTYPE_DOUBLE 6
#define CTYPE_DOUBLE_PTR 60

#define CTYPE_VOID_PTR 7

#define CTYPE_BOOL 8 // NOTE: Not in xHarbour for structure support
                     //       Hopefully it will be added !!!

// ***Must*** be smaller than CTYPE_STRUCTURE_PTR
//  #define CTYPE_STRUCTURE 1000
#define CTYPE_STRUCTURE_PTR 10000

extern RESULT DynaCall(int Flags, DWORD lpFunction,
                int nArgs, DYNAPARM Parm[],
                LPVOID pRet, int nRetSiz);

//------------------------------------------------------------------

// CallDll( hInstDLL, pFunctAddr, nFlags, nRetType, nParmType1, xParm1, nParmType2, xParm2, nParmTypeN, xParmN)

HB_FUNC( CALLDLL )
{
    int i;
    int iCnt = 0;
    int iParams = hb_pcount();
    int iArgCnt = iParams - 3;
    int iReturn = ISNIL( 4 ) ? 0 : hb_parni( 4 );
    int Flags;
    double DblParms[15];
    DYNAPARM   Parm[15];
    HINSTANCE  hInst = (HINSTANCE) hb_parnl( 1 );
    DWORD      lpFunction = (DWORD) hb_parnl( 2 );
    RESULT     rc;

    if ( hInst == NULL )
       return;

    if ((LPVOID)lpFunction == NULL)
       return;

    if ( ISNIL(3) )
       Flags = DC_CALL_STD_BO;
    else
       Flags = hb_parni( 3 );
    memset(Parm, 0, sizeof(Parm));
    memset(DblParms, 0, sizeof(DblParms));

    if(iArgCnt > 0)
        iArgCnt /= 2;

    //printf( "\nNo. Parameters: %i\n", iArgCnt ) ;
    if( iArgCnt > 0)
    {
        for( i = 6; i <= iParams; i += 2)
        {
            //printf( "\nParameter Type: %i\n", hb_parni( i ) ) ;
            //printf( "Parameter: %i\n", hb_parni( i + 2) ) ;
            //printf( "Parameter: %i\n", hb_parni( i + 4) ) ;
            //printf( "Parameter: %i\n", hb_parni( i + 6) ) ;
            switch ( hb_parni( i-1 ) )
            {
            case CTYPE_CHAR_PTR          :
                Parm[iCnt].nWidth = sizeof(  char * );
                if ( ISNIL(i) ) Parm[iCnt].pArg = NULL;
                else if (ISNUM(i) ) Parm[iCnt].dwArg = (DWORD) MAKEINTRESOURCE( hb_parni( i ) );
                else Parm[iCnt].dwArg = ( DWORD ) hb_parc ( i  );
                iCnt++;
                break;
            case CTYPE_STRUCTURE_PTR     :
            case CTYPE_UNSIGNED_CHAR_PTR :
                Parm[iCnt].nWidth = sizeof( unsigned char * );
                if ( ISNIL(i) ) Parm[iCnt].pArg = NULL;
                else Parm[iCnt].dwArg = ( DWORD ) hb_parc ( i  );
                iCnt++;
                break;
            case CTYPE_BOOL              :
                Parm[iCnt].nWidth = sizeof( BOOL );
                Parm[iCnt].dwArg = ( DWORD ) hb_parl( i );
                iCnt++;
                break;
            case CTYPE_CHAR              :
                Parm[iCnt].nWidth = sizeof( char );
                if ( ISNIL(i) ) Parm[iCnt].pArg = NULL;
                else Parm[iCnt].dwArg = ( DWORD ) hb_parni( i );
                iCnt++;
                break;
            case CTYPE_UNSIGNED_CHAR     :
                Parm[iCnt].nWidth = sizeof( unsigned char );
                if ( ISNIL(i) ) Parm[iCnt].pArg = NULL;
                else Parm[iCnt].dwArg = ( DWORD ) hb_parni( i );
                iCnt++;
                break;
            case CTYPE_SHORT             :
            case CTYPE_UNSIGNED_SHORT    :
                Parm[iCnt].nWidth = sizeof( short int );
                if ( ISNIL(i) ) Parm[iCnt].pArg = NULL;
                else Parm[iCnt].dwArg = ( DWORD ) hb_parni( i );
                iCnt++;
                break;
            case CTYPE_INT               :
            case CTYPE_UNSIGNED_INT      :
                Parm[iCnt].nWidth = sizeof( unsigned int );
                if ( ISNIL(i) ) Parm[iCnt].pArg = NULL;
                else Parm[iCnt].dwArg = ( DWORD ) hb_parnd( i );
                iCnt++;
                break;
            case CTYPE_LONG              :
            case CTYPE_UNSIGNED_LONG     :
                Parm[iCnt].nWidth = sizeof( unsigned long int );
                if ( ISNIL(i) ) Parm[iCnt].dwArg = 0;
                else Parm[iCnt].dwArg = ( DWORD ) hb_parnd( i );
                iCnt++;
                break;
            case CTYPE_LONG_PTR          :
            case CTYPE_UNSIGNED_LONG_PTR :
            case CTYPE_VOID_PTR          :
            case CTYPE_INT_PTR           :
            case CTYPE_UNSIGNED_INT_PTR  :
            case CTYPE_SHORT_PTR         :
            case CTYPE_UNSIGNED_SHORT_PTR:
                Parm[iCnt].nWidth = sizeof( unsigned long int * );
                if ( ISNIL(i) ) Parm[iCnt].pArg = NULL;
                else Parm[iCnt].dwArg   = ( DWORD ) hb_parnl( i );
                iCnt++;
                break;
            case CTYPE_FLOAT_PTR         :
                Parm[iCnt].nWidth = sizeof( float * );
                Parm[iCnt].dwArg   = ( DWORD ) hb_parnl( i );
                iCnt++;
                break;
            case CTYPE_DOUBLE_PTR        :
                Parm[iCnt].nWidth = sizeof( double * );
                Parm[iCnt].dwArg   = ( DWORD ) hb_parnl( i );
                iCnt++;
                break;
            case CTYPE_FLOAT             :
                Parm[iCnt].nWidth = sizeof( float );
                DblParms[iCnt] = ( double ) hb_parnd( i );
                Parm[iCnt].pArg   = &DblParms[iCnt];
                Flags |= DC_RETVAL_MATH4;
                iCnt++;
                break;
            case CTYPE_DOUBLE            :
                Parm[iCnt].nWidth = sizeof( double );
                DblParms[iCnt] = ( double ) hb_parnd( i );
                Parm[iCnt].pArg   = &DblParms[iCnt];
                Parm[iCnt].dwFlags = DC_FLAG_ARGPTR;  // use the pointer
                Flags |= DC_RETVAL_MATH8;
                iCnt++;
                break;
            default:
                MessageBox( GetActiveWindow(), "UNKNOWN Parameter Type!", "CallDll Parameter Error!", MB_OK | MB_ICONERROR );
                printf( "Bad Parameter: %i\n", hb_parni( i-1 ) ) ;
                return;
            }
        }
    }

    rc = DynaCall(Flags, lpFunction, iArgCnt, Parm, NULL, 0);

    // return the correct value
    switch ( iReturn ) {
        case CTYPE_BOOL :
            //printf("\nAt BOOL Return\n");
            hb_retl( (BOOL) rc.Long );
            break;
        case CTYPE_VOID :
            break;
        case CTYPE_CHAR              :
            hb_retni ( (char) rc.Int );
            break;
        case CTYPE_SHORT             :
        case CTYPE_UNSIGNED_SHORT    :
            hb_retni ( (int) rc.Int );
            break;
        case CTYPE_INT               :
            hb_retni ( (int) rc.Long );
            break;
        case CTYPE_LONG              :
            hb_retnl ( (LONG) rc.Long );
            break;
        case CTYPE_CHAR_PTR          :
        case CTYPE_UNSIGNED_CHAR_PTR :
        case CTYPE_UNSIGNED_CHAR     :
        case CTYPE_UNSIGNED_INT      :
        case CTYPE_INT_PTR           :
        case CTYPE_UNSIGNED_SHORT_PTR:
        case CTYPE_UNSIGNED_LONG     :
        case CTYPE_UNSIGNED_INT_PTR  :
        case CTYPE_STRUCTURE_PTR     :
        case CTYPE_LONG_PTR          :
        case CTYPE_UNSIGNED_LONG_PTR :
        case CTYPE_VOID_PTR          :
        case CTYPE_FLOAT_PTR         :
        case CTYPE_DOUBLE_PTR        :
            hb_retnl ( (DWORD) rc.Long );
            break;
        case CTYPE_FLOAT             :
            hb_retnd( rc.Float );
            break;
        case CTYPE_DOUBLE            :
            hb_retnd( rc.Double );
            break;
        default:
            MessageBox( GetActiveWindow(), "UNKNOW Return Type!", "CallDll Parameter Error!", MB_OK | MB_ICONERROR );
            break;
    }
}



