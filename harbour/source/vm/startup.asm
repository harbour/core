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
 ; the Free Software Foundation; either version 2 of the License, or
 ; (at your option) any later version, with one exception:
 ;
 ; The exception is that if you link the Harbour Runtime Library (HRL)
 ; and/or the Harbour Virtual Machine (HVM) with other files to produce
 ; an executable, this does not by itself cause the resulting executable
 ; to be covered by the GNU General Public License. Your use of that
 ; executable is in no way restricted on account of linking the HRL
 ; and/or HVM code into it.
 ;
 ; This program is distributed in the hope that it will be useful,
 ; but WITHOUT ANY WARRANTY; without even the implied warranty of
 ; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 ; GNU General Public License for more details.
 ;
 ; You should have received a copy of the GNU General Public License
 ; along with this program; if not, write to the Free Software
 ; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 ; their web site at http://www.gnu.org/).
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
