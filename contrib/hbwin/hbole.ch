/*
 * OLE header
 *
 * Copyright 2012 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

#ifndef HBOLE_CH_
#define HBOLE_CH_

/* Variant types */
#define WIN_VT_EMPTY                0
#define WIN_VT_NULL                 1
#define WIN_VT_I2                   2
#define WIN_VT_I4                   3
#define WIN_VT_R4                   4
#define WIN_VT_R8                   5
#define WIN_VT_CY                   6
#define WIN_VT_DATE                 7
#define WIN_VT_BSTR                 8
#define WIN_VT_DISPATCH             9
#define WIN_VT_ERROR                10
#define WIN_VT_BOOL                 11
#define WIN_VT_VARIANT              12
#define WIN_VT_UNKNOWN              13
#define WIN_VT_DECIMAL              14
#define WIN_VT_I1                   16
#define WIN_VT_UI1                  17
#define WIN_VT_UI2                  18
#define WIN_VT_UI4                  19
#define WIN_VT_I8                   20
#define WIN_VT_UI8                  21
#define WIN_VT_INT                  22
#define WIN_VT_UINT                 23
#define WIN_VT_VOID                 24
#define WIN_VT_HRESULT              25
#define WIN_VT_PTR                  26
#define WIN_VT_SAFEARRAY            27
#define WIN_VT_CARRAY               28
#define WIN_VT_USERDEFINED          29
#define WIN_VT_LPSTR                30
#define WIN_VT_LPWSTR               31
#define WIN_VT_RECORD               36
#define WIN_VT_INT_PTR              37
#define WIN_VT_UINT_PTR             38
#define WIN_VT_FILETIME             64
#define WIN_VT_BLOB                 65
#define WIN_VT_STREAM               66
#define WIN_VT_STORAGE              67
#define WIN_VT_STREAMED_OBJECT      68
#define WIN_VT_STORED_OBJECT        69
#define WIN_VT_BLOB_OBJECT          70
#define WIN_VT_CF                   71
#define WIN_VT_CLSID                72
#define WIN_VT_VERSIONED_STREAM     73

#define WIN_VT_BSTR_BLOB            0x0fff
#define WIN_VT_VECTOR               0x1000
#define WIN_VT_ARRAY                0x2000
#define WIN_VT_BYREF                0x4000
#define WIN_VT_RESERVED             0x8000
#define WIN_VT_ILLEGAL              0xffff
#define WIN_VT_ILLEGALMASKED        0x0fff
#define WIN_VT_TYPEMASK             0x0fff

/* errors explicitly recognized by win_oleErrorText() */
#define WIN_CO_E_CLASSSTRING        0x800401F3
#define WIN_OLE_E_WRONGCOMPOBJ      0x8004000E
#define WIN_REGDB_E_WRITEREGDB      0x80040151
#define WIN_REGDB_E_CLASSNOTREG     0x80040154
#define WIN_E_PENDING               0x8000000A
#define WIN_E_NOTIMPL               0x80004001
#define WIN_E_NOINTERFACE           0x80004002
#define WIN_E_POINTER               0x80004003
#define WIN_E_ABORT                 0x80004004
#define WIN_E_FAIL                  0x80004005
#define WIN_E_UNEXPECTED            0x8000FFFF
#define WIN_E_ACCESSDENIED          0x80070005
#define WIN_E_HANDLE                0x80070006
#define WIN_E_OUTOFMEMORY           0x8007000E
#define WIN_E_INVALIDARG            0x80070057
#define WIN_DISP_E_UNKNOWNINTERFACE 0x80020001
#define WIN_DISP_E_MEMBERNOTFOUND   0x80020003
#define WIN_DISP_E_PARAMNOTFOUND    0x80020004
#define WIN_DISP_E_TYPEMISMATCH     0x80020005
#define WIN_DISP_E_UNKNOWNNAME      0x80020006
#define WIN_DISP_E_NONAMEDARGS      0x80020007
#define WIN_DISP_E_BADVARTYPE       0x80020008
#define WIN_DISP_E_EXCEPTION        0x80020009
#define WIN_DISP_E_OVERFLOW         0x8002000A
#define WIN_DISP_E_BADINDEX         0x8002000B
#define WIN_DISP_E_UNKNOWNLCID      0x8002000C
#define WIN_DISP_E_ARRAYISLOCKED    0x8002000D
#define WIN_DISP_E_BADPARAMCOUNT    0x8002000E
#define WIN_DISP_E_PARAMNOTOPTIONAL 0x8002000F
#define WIN_DISP_E_BADCALLEE        0x80020010
#define WIN_DISP_E_NOTACOLLECTION   0x80020011
#define WIN_DISP_E_DIVBYZERO        0x80020012
#define WIN_DISP_E_BUFFERTOOSMALL   0x80020013

#endif /* HBOLE_CH_ */
