 ;
 ; $Id$
 ;
 ; Harbour Project source code:
 ; Harbour startup for Win32 when not using any C compiler startup
 ;
 ; Copyright 1999 Antonio Linares <alinares@fivetech.com>
 ; www - http://www.harbour-project.org
 ;
 ; This program is free software; you can redistribute it and/or modify
 ; it under the terms of the GNU General Public License as published by
 ; the Free Software Foundation; either version 2, or (at your option)
 ; any later version.
 ;
 ; This program is distributed in the hope that it will be useful,
 ; but WITHOUT ANY WARRANTY; without even the implied warranty of
 ; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 ; GNU General Public License for more details.
 ;
 ; You should have received a copy of the GNU General Public License
 ; along with this software; see the file COPYING.  If not, write to
 ; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 ; Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 ;
 ; As a special exception, the Harbour Project gives permission for
 ; additional uses of the text contained in its release of Harbour.
 ;
 ; The exception is that, if you link the Harbour libraries with other
 ; files to produce an executable, this does not by itself cause the
 ; resulting executable to be covered by the GNU General Public License.
 ; Your use of that executable is in no way restricted on account of
 ; linking the Harbour library code into it.
 ;
 ; This exception does not however invalidate any other reasons why
 ; the executable file might be covered by the GNU General Public License.
 ;
 ; This exception applies only to the code released by the Harbour
 ; Project under the name Harbour.  If you copy code from other
 ; Harbour Project or Free Software Foundation releases into a copy of
 ; Harbour, as the General Public License permits, the exception does
 ; not apply to the code that you add in this way.  To avoid misleading
 ; anyone as to the status of such modified files, you must delete
 ; this exception notice from them.
 ;
 ; If you write modifications of your own for Harbour, it is your choice
 ; whether to permit this exception to apply to your modifications.
 ; If you do not wish that, delete this exception notice.
 ;

.386

_TEXT   segment para use32 public 'CODE'
_TEXT   ends

_NULL   segment dword use32 public 'DATA'
_NULL   ends

_DATA   segment dword use32 public 'DATA'
_DATA	ends

_BSS    segment dword use32 public 'BSS'
_BSS	ends

FLAT    group
DGROUP  group _NULL, _BSS, _DATA

    extrn   GetModuleHandleA:near
    extrn   WinMain:near

    public  _hb_startup

_NULL       segment
            db      16 dup (0)              ;Windows
            db       4 dup (0)              ;destructor count
            db       2 dup (0)              ;exception list
            db       4 dup (0)              ;exception vptr
            db       6 dup (0)              ;reserved
            db       2 dup (0)              ;VBX control jump vector
						;MUST be at SS:20h
            db       2 dup (0)              ;reserved
            db      50 dup (0)              ;antonio
            ends

_BSS       segment
__bss      label word
_BSS       ends

_DATA       segment
            public __hInstance
__hInstance dw  0
            public __hPrev
__hPrev     dw  0
            public __pszCmdLine
__pszCmdLine dw 0
            public __CmdShow
__CmdShow   dw 0
            public __PSP
__PSP       dw  0
_DATA       ends

_TEXT       segment
            assume  CS:_TEXT, DS:DGROUP

_hb_startup: push 0
            call near ptr GetModuleHandleA
            mov dword ptr __hInstance, eax
            push 0
            push 0
            push 0
            push 0
            call near ptr WinMain
            ret

_TEXT       ends

            end _hb_startup
