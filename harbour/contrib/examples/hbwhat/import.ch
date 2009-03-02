/*
 * $Id$
 */

/*

hbwhat.lib
AJ Wos: IMPORT syntax added

Modified from:
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


// these are the flags for CallDll
#define  DC_MICROSOFT           0x0000      // Microsoft compatible
#define  DC_BORLAND             0x0001      // Borland compat - default
#define  DC_CALL_CDECL          0x0010      // __cdecl
#define  DC_CALL_STD            0x0020      // __stdcall


//----------------------------------------------------------------------------//

// C syntax
// very fast and efficient




//----------------------------------------------------------------------------//
// void

#xcommand IMPORT <hInstDLL> [<flags>] [<static:STATIC>] FUNCTION <return> ;
          <FuncName>( 0 );
       => ;
          [<static>] function <FuncName>( ) ;;
             STATIC nProcAddr;;
             IF nProcAddr==NIL;;
                nProcAddr:=GetProcAddress(<hInstDLL>,<(FuncName)>);;
             ENDIF;;
             return CallDLL(<hInstDLL>,nProcAddr,[<flags>], <return>  )

//----------------------------------------------------------------------------//
// void + alias

#xcommand IMPORT <hInstDLL> [<flags>] [<static:STATIC>] FUNCTION <return> ;
          <FuncName>( 0 ) AS <alias> ;
       => ;
          [<static>] function <alias>( ) ;;
             STATIC nProcAddr;;
             IF nProcAddr==NIL;;
                nProcAddr:=GetProcAddress(<hInstDLL>,<(FuncName)>);;
             ENDIF;;
             return CallDLL(<hInstDLL>,nProcAddr,[<flags>], <return> )

//----------------------------------------------------------------------------//

#xcommand IMPORT <hInstDLL> [<flags>] [<static:STATIC>] FUNCTION <return> <FuncName> => ;
   __IMPORT <hInstDLL> [<flags>] [<static>] FUNCTION <return> <FuncName>;;
   #translate * \<!literal!> => * 10 \<literal>


#xcommand __IMPORT <hInstDLL> [<flags>] [<static:STATIC>] FUNCTION <return> ;
          <!FuncName!>( [ <type1> <!uParam1!>] [, <typeN> <!uParamN!>]);
       => ;
          [<static>] function <FuncName>( [<uParam1>] [,<uParamN>] ) ;;
             STATIC nProcAddr;;
             IF nProcAddr==NIL;;
                nProcAddr:=GetProcAddress(<hInstDLL>,<(FuncName)>);;
             ENDIF;;
             return CallDLL(<hInstDLL>,nProcAddr,[<flags>], <return>[,<type1>,<uParam1> ] [,<typeN>,<uParamN> ] );;
             #xuntranslate * \<!literal!> //=> * 10 \<literal>


// old
/*
#xcommand IMPORT <hInstDLL> [<flags>] [<static:STATIC>] FUNCTION <return> ;
          <FuncName>( [ <type1> <uParam1>] [, <typeN> <uParamN>] );
       => ;
          [<static>] function <FuncName>( [<uParam1>] [,<uParamN>] ) ;;
             STATIC nProcAddr;;
             IF nProcAddr==NIL;;
                nProcAddr:=GetProcAddress(<hInstDLL>,<(FuncName)>);;
             ENDIF;;
             return CallDLL(<hInstDLL>,nProcAddr,[<flags>], <return> [,<type1>,<uParam1> ] [,<typeN>,<uParamN> ] )

*/

//----------------------------------------------------------------------------//
// aliased function name


#xcommand IMPORT <hInstDLL> [<flags>] [<static:STATIC>] FUNCTION <return> <FuncName> AS <alias> => ;
   __IMPORT <hInstDLL> [<flags>] [<static>] FUNCTION <return> <FuncName> AS <alias> ;;
   #translate * \<!literal!> => * 10 \<literal>

#xcommand __IMPORT <hInstDLL> [<flags>] [<static:STATIC>] FUNCTION <return> ;
          <!FuncName!>( [ <type1> <!uParam1!>] [, <typeN> <!uParamN!>]) AS <alias> ;
       => ;
          [<static>] function <alias>( [<uParam1>] [,<uParamN>] ) ;;
             STATIC nProcAddr;;
             IF nProcAddr==NIL;;
                nProcAddr:=GetProcAddress(<hInstDLL>,<(FuncName)>);;
             ENDIF;;
             return CallDLL(<hInstDLL>,nProcAddr,[<flags>], <return>[,<type1>,<uParam1> ] [,<typeN>,<uParamN> ] );;
             #untranslate * \<!literal!> => * 10 \<literal>


// old
/*
#xcommand IMPORT <hInstDLL> [<flags>] [<static:STATIC>] FUNCTION <return> ;
          <FuncName>( [ <type1> <uParam1> ] [, <typeN> <uParamN> ] ) AS <alias> ;;
       => ;
          [<static>] function <FuncName>( [<uParam1>] [,<uParamN>] ) ;;
             STATIC nProcAddr;;
             IF nProcAddr==NIL;;
                nProcAddr:=GetProcAddress(<hInstDLL>,<(alias)>);;
             ENDIF;;
             return CallDLL(<hInstDLL>, nProcAddr, [<flags>], <return> [,<type1>, <uParam1> ] [,<utypeN>, <uParamN> ] )
*/

//----------------------------------------------------------------------------//

// Xbase++ syntax
// based on Vic McClung's code
// Thank you Vic

// to be tested !

//----------------------------------------------------------------------------//

#xcommand DLL [<static:STATIC>] FUNCTION <FuncName>( [ <uParam1> AS <type1> ] ;
                                                     [, <uParamN> AS <typeN> ] ) ;
             AS <return> LIB <*DllName*> [FLAGS <flags>]  ;
       => ;
          [<static>] function <FuncName>( [<uParam1>] [,<uParamN>] ) ;;
             local uResult ;;
             Local hInstDLL  :=LoadLibrary(<(DllName)>);;
             Local nProcAddr :=GetProcAddress(hInstDLL,<(FuncName)>);;
             uResult = CallDLL(hInstDLL, nProcAddr, [<flags>], <return> [, <type1>, <uParam1> ] [, <typeN>, <uParamN> ] ) ;;
             FreeLibrary(hInstDLL);;
             return uResult

//----------------------------------------------------------------------------//

#xcommand DLL [<static:STATIC>] FUNCTION <FuncName>( [ <uParam1> AS <type1> ] ;
                                                     [, <uParamN> AS <typeN> ] ) ;
             AS <return> LIB <DllName> ALIAS <alias> [FLAGS <flags>];
       => ;
          [<static>] function <FuncName>( [<uParam1>] [,<uParamN>] ) ;;
             local uResult ;;
             Local hInstDLL  :=LoadLibrary(<(DllName)>);;
             Local nProcAddr :=GetProcAddress(hInstDLL,<(alias)>);;
             uResult = CallDLL(hInstDLL, nProcAddr, [<flags>], <return> [, <type1>, <uParam1> ] [, <typeN>, <uParamN> ] ) ;;
             FreeLibrary(hInstDLL);;
             return uResult


