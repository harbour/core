/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 *
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "dll.ch"

#ifndef __DLL_CH
#define __DLL_CH

#define DC_MICROSOFT           0x0000      // Default
#define DC_BORLAND             0x0001      // Borland compat
#define DC_CALL_CDECL          DLL_CDECL
#define DC_CALL_STD            DLL_STDCALL
#define DC_RETVAL_MATH4        0x0100      // Return value in ST
#define DC_RETVAL_MATH8        0x0200      // Return value in ST

// Avoid xHarbour code when included by dllcall.c
#if defined( __PLATFORM__WINDOWS )

#include "cstruct.ch"
#include "wintypes.ch"

// Native syntax
#xcommand IMPORT [<Qualifier: STATIC>] [<CallConvention: 0x0000, 0x0001, 0x0010, 0x0020, 0x0100, 0x0200> ] ;
                 <FuncName>( [<ArgName> [, <ArgNameN>] ] ) FROM [<!DllFuncName!> OF ] <(DllName)> => ;
    [<Qualifier>] FUNCTION <FuncName>( <ArgName> [, <ArgNameN>] ) ;;
       THREAD STATIC t_cTemplate ;;
       IF t_cTemplate == NIL ;;
          t_cTemplate := DllPrepareCall( <(DllName)>, iif( <.CallConvention.>, <CallConvention>, DC_CALL_STD ), iif( <.DllFuncName.>, #<DllFuncName>, #<FuncName> ) ) ;;
       END ;;
       IF <.ArgName.>;;
          RETURN DllExecuteCall( t_cTemplate, <ArgName> [, <ArgNameN>] ) ;;
       END ;;
    RETURN DllExecuteCall( t_cTemplate )

#xcommand IMPORT [<Qualifier: STATIC>] [<CallConvention: 0x0000, 0x0001, 0x0010, 0x0020, 0x0100, 0x0200> ] ;
                 <FuncName>( [<ArgName> [, <ArgNameN>] ] ) FROM <(DllName)> [EXPORTED AS <!DllFuncName!>] => ;
    [<Qualifier>] FUNCTION <FuncName>( <ArgName> [, <ArgNameN>] ) ;;
       THREAD STATIC t_cTemplate ;;
       IF t_cTemplate == NIL ;;
          t_cTemplate := DllPrepareCall( <(DllName)>, iif( <.CallConvention.>, <CallConvention>, DC_CALL_STD ), iif( <.DllFuncName.>, #<DllFuncName>, #<FuncName> ) ) ;;
       END ;;
       IF <.ArgName.>;;
          RETURN DllExecuteCall( t_cTemplate, <ArgName> [, <ArgNameN>] ) ;;
       END ;;
    RETURN DllExecuteCall( t_cTemplate )


#xcommand IMPORT [<Qualifier: STATIC>] [<CallConvention: 0x0000, 0x0001, 0x0010, 0x0020, 0x0100, 0x0200> ] ;
                 <Type> <FuncName>( [0] [<ArgType> <ArgName> [, <ArgTypeN> <ArgNameN>] ] ) FROM <(DllName)> [EXPORTED AS <!DllFuncName!>] => ;
    [<Qualifier>] FUNCTION <FuncName>( <ArgName> [, <ArgNameN>] ) ;;
       THREAD STATIC t_cTemplate ;;
       IF t_cTemplate == NIL ;;
          t_cTemplate := DllPrepareCall( <(DllName)>, iif( <.CallConvention.>, <CallConvention>, DC_CALL_STD ), iif( <.DllFuncName.>, #<DllFuncName>, #<FuncName> ) ) ;;
       END ;;
       IF <.ArgName.>;;
          RETURN DllExecuteCall( t_cTemplate, <ArgName> [, <ArgNameN>] ) ;;
       END ;;
    RETURN DllExecuteCall( t_cTemplate )

#xcommand IMPORT [<Qualifier: STATIC>] [<CallConvention: 0x0000, 0x0001, 0x0010, 0x0020, 0x0100, 0x0200> ] ;
                 <Type> <FuncName>( [0] [<ArgType> <ArgName> [, <ArgTypeN> <ArgNameN>] ] ) FROM [<!DllFuncName!> OF ] <(DllName)> => ;
    [<Qualifier>] FUNCTION <FuncName>( <ArgName> [, <ArgNameN>] ) ;;
       THREAD STATIC t_cTemplate ;;
       IF t_cTemplate == NIL ;;
          t_cTemplate := DllPrepareCall( <(DllName)>, iif( <.CallConvention.>, <CallConvention>, DC_CALL_STD ), iif( <.DllFuncName.>, #<DllFuncName>, #<FuncName> ) ) ;;
       END ;;
       IF <.ArgName.>;;
          RETURN DllExecuteCall( t_cTemplate, <ArgName> [, <ArgNameN>] ) ;;
       END ;;
    RETURN DllExecuteCall( t_cTemplate )

// FoxPro syntax
#xcommand DECLARE <!Type!> <!FuncName!> IN <(DllName)> [AS <(DllFuncName)>] [<ArgType> [@] [<ArgName>] [, <ArgTypeN> [@] [<ArgNameN>]]] => ;
    FUNCTION <FuncName>( ... ) ;;
       THREAD STATIC t_cTemplate ;;
       IF t_cTemplate == NIL ;;
          t_cTemplate := DllPrepareCall( <(DllName)>, DC_CALL_STD, iif( <.DllFuncName.>, <(DllFuncName)>, #<FuncName> ) ) ;;
       END ;;
    RETURN DllExecuteCall( t_cTemplate, ... )

// FWH
#xcommand DLL [<Qualifier: STATIC>] FUNCTION <FuncName>( [ <ArgName> AS <ArgType> ] [, <ArgNameN> AS <ArgTypeN> ] ) AS <Type> [<Pascal: PASCAL>] [ FROM <DllFuncName> ] LIB <(DllName)> => ;
    [<Qualifier>] FUNCTION <FuncName>( <ArgName> [, <ArgNameN>] ) ;;
       THREAD STATIC t_cTemplate ;;
       IF t_cTemplate == NIL ;;
          t_cTemplate := DllPrepareCall( <(DllName)>, iif( <.Pascal.>, DC_CALL_STD, DC_CALL_CDECL ), iif( <.DllFuncName.>, <(DllFuncName)>, #<FuncName> ) ) ;;
       END ;;
    RETURN DllExecuteCall( t_cTemplate [, <ArgName>] [, <ArgNameN>] )

#xcommand DLL32 [<Qualifier: STATIC>] FUNCTION <FuncName>( [ <ArgName> AS <ArgType> ] [, <ArgNameN> AS <ArgTypeN> ] ) AS <Type> [<Pascal: PASCAL>] [ FROM <DllFuncName> ] LIB <(DllName)> => ;
    [<Qualifier>] FUNCTION <FuncName>( <ArgName> [, <ArgNameN>] ) ;;
       THREAD STATIC t_cTemplate ;;
       IF t_cTemplate == NIL ;;
          t_cTemplate := DllPrepareCall( <(DllName)>, iif( <.Pascal.>, DC_CALL_STD, DC_CALL_CDECL ), iif( <.DllFuncName.>, <(DllFuncName)>, #<FuncName> ) ) ;;
       END ;;
    RETURN DllExecuteCall( t_cTemplate [, <ArgName>] [, <ArgNameN>] )

#endif
#endif
