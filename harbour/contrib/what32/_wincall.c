/*
 * $Id$
 */

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

#ifndef __MINGW32__

//------------------------------------------------------------------
#include <stdio.h>
//#include <stdlib.h>
#define  WIN32_LEAN_AND_MEAN
//#include <windows.h>
//#include "dynacall.h"
//------------------------------------------------------------------

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


//------------------------------------------------------------------

RESULT DynaCall(int Flags, DWORD lpFunction,
                                  int nArgs, DYNAPARM Parm[],
                                  LPVOID pRet, int nRetSiz)
{
    // Call the specified function with the given parameters. Build a
    // proper stack and take care of correct return value processing.
    RESULT  Res = { 0 };
    int     i, nInd, nSize;
    DWORD   dwEAX, dwEDX, dwVal, *pStack, dwStSize = 0;
    BYTE   *pArg;

    // Reserve 256 bytes of stack space for our arguments
    _asm mov pStack, esp
    _asm sub esp, 0x100

    // Push args onto the stack. Every argument is aligned on a
    // 4-byte boundary. We start at the rightmost argument.
    for (i = 0; i < nArgs; i++) {
        nInd  = (nArgs - 1) - i;
        // Start at the back of the arg ptr, aligned on a DWORD
        nSize = (Parm[nInd].nWidth + 3) / 4 * 4;
        pArg  = (BYTE *)Parm[nInd].pArg + nSize - 4;
        dwStSize += (DWORD)nSize; // Count no of bytes on stack
        while (nSize > 0) {
            // Copy argument to the stack
            if (Parm[nInd].dwFlags & DC_FLAG_ARGPTR) {
                // Arg has a ptr to a variable that has the arg
                dwVal = *(DWORD *)pArg; // Get first four bytes
                pArg -= 4;              // Next part of argument
            }
            else {
                // Arg has the real arg
                dwVal = Parm[nInd].dwArg;
            }
            // Do push dwVal
            pStack--;           // ESP = ESP - 4
            *pStack = dwVal;    // SS:[ESP] = dwVal
            nSize -= 4;
        }
    }
    if ((pRet != NULL) && ((Flags & DC_BORLAND) || (nRetSiz > 8))) {
        // Return value isn't passed through registers, memory copy
        // is performed instead. Pass the pointer as hidden arg.
        dwStSize += 4;          // Add stack size
        pStack--;               // ESP = ESP - 4
        *pStack = (DWORD)pRet;  // SS:[ESP] = pMem
    }

    _asm add esp, 0x100         // Restore to original position
    _asm sub esp, dwStSize      // Adjust for our new parameters

    // Stack is now properly built, we can call the function
    _asm call [lpFunction]

    _asm mov dwEAX, eax         // Save eax/edx registers
    _asm mov dwEDX, edx         //

    // Possibly adjust stack and read return values.
    if (Flags & DC_CALL_CDECL) {
        _asm add esp, dwStSize
    }
    if (Flags & DC_RETVAL_MATH4) {
        _asm fstp dword ptr [Res]
    }
    else if (Flags & DC_RETVAL_MATH8) {
        _asm fstp qword ptr [Res]
    }
    else if (pRet == NULL) {
        _asm mov  eax, [dwEAX]
        _asm mov  DWORD PTR [Res], eax
        _asm mov  edx, [dwEDX]
        _asm mov  DWORD PTR [Res + 4], edx
    }
    else if (((Flags & DC_BORLAND) == 0) && (nRetSiz <= 8)) {
        // Microsoft optimized less than 8-bytes structure passing
        _asm mov ecx, DWORD PTR [pRet]
        _asm mov eax, [dwEAX]
        _asm mov DWORD PTR [ecx], eax
        _asm mov edx, [dwEDX]
        _asm mov DWORD PTR [ecx + 4], edx
    }
    return Res;
}

#endif
