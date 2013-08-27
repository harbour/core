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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#ifndef __WINTYPES_CH
#define __WINTYPES_CH

#include "cstruct.ch"

#define VOID                 CTYPE_VOID
#define UINT                 CTYPE_UNSIGNED_INT
#define HANDLE               CTYPE_UNSIGNED_LONG
#define HICON                CTYPE_UNSIGNED_LONG
#define HBITMAP              CTYPE_UNSIGNED_LONG
#define HCURSOR              CTYPE_UNSIGNED_LONG
#define HBRUSH               CTYPE_UNSIGNED_LONG
#define LPCSTR               CTYPE_CHAR_PTR
#define WNDPROC              CTYPE_UNSIGNED_LONG
#define BOOL                 CTYPE_LONG
#define LPVOID               CTYPE_VOID_PTR
#define DWORD                CTYPE_UNSIGNED_LONG
#define WORD                 CTYPE_UNSIGNED_SHORT
#define LPCTSTR              CTYPE_CHAR_PTR
#define COLORREF             CTYPE_UNSIGNED_LONG
#define BYTE                 CTYPE_CHAR
#define TCHAR                CTYPE_UNSIGNED_CHAR
#define HINSTANCE            CTYPE_UNSIGNED_LONG
#define HMENU                CTYPE_UNSIGNED_LONG
#define HTREEITEM            CTYPE_UNSIGNED_LONG
#define INT                  CTYPE_INT
#define int                  CTYPE_INT
#define HWND                 CTYPE_UNSIGNED_LONG
#define LPARAM               CTYPE_LONG
#define HGLOBAL              CTYPE_UNSIGNED_LONG
#define WPARAM               CTYPE_INT
#define HKEY                 CTYPE_UNSIGNED_LONG
#define char                 CTYPE_CHAR
#define LONG                 CTYPE_LONG
#define BCHAR                CTYPE_UNSIGNED_CHAR
#define WCHAR                CTYPE_UNSIGNED_SHORT
#define DOUBLE               CTYPE_DOUBLE
#define LPTSTR               CTYPE_CHAR_PTR
#define LPSTR                CTYPE_CHAR_PTR
#define ULONG                CTYPE_UNSIGNED_LONG
#define UCHAR                CTYPE_UNSIGNED_CHAR
#define SHORT                CTYPE_SHORT
#define USHORT               CTYPE_UNSIGNED_SHORT
#define PVOID                CTYPE_VOID_PTR
#define ULONG_PTR            CTYPE_UNSIGNED_LONG_PTR

#define LPOFNHOOKPROC        CTYPE_UNSIGNED_LONG
#define LPCFHOOKPROC         CTYPE_UNSIGNED_LONG
#define LPFRHOOKPROC         CTYPE_UNSIGNED_LONG
#define LPPAGESETUPHOOK      CTYPE_UNSIGNED_LONG
#define LPPAGEPAINTHOOK      CTYPE_UNSIGNED_LONG
#define LPPRINTHOOKPROC      CTYPE_UNSIGNED_LONG
#define LPSETUPHOOKPROC      CTYPE_UNSIGNED_LONG

#define BFFCALLBACK          CTYPE_UNSIGNED_LONG

#define HDC                  CTYPE_UNSIGNED_LONG
#define HIMAGELIST           CTYPE_UNSIGNED_LONG

#ytranslate CTYPE_Int( <x> ) => Int( <x> ) /* Fixes conflict with Int() function */

#endif /* __WINTYPES_CH */
