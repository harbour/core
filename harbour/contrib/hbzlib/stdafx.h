// stdafx.h : include file for standard system include files,
//  or project specific include files that are used frequently, but
//      are changed infrequently
// Part of the ZipArchive library
// 
// Copyright (C) 2000 - 2001 Tadeusz Dracz.
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
// 
// For the licensing details see the file License.txt
////////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_STDAFX_H__926F70F4_1B34_49AA_9532_498E8D2F3495__INCLUDED_)
#define AFX_STDAFX_H__926F70F4_1B34_49AA_9532_498E8D2F3495__INCLUDED_

#ifdef __BORLANDC__
#define ZIPINLINE
#else
#define ZIPINLINE inline
#endif

#define ZIP_ARCHIVE_STL

#if _MSC_VER > 1000
	#pragma once
//because of STL
	#pragma warning (disable : 4710) // 'function' : function not inlined
	#pragma warning (disable : 4514) // unreferenced inline/local function has been removed
#endif // _MSC_VER > 1000


// some Windows typical definitions

#if defined (_UNICODE) && !defined (UNICODE)
	#define UNICODE
#endif
#if defined (UNICODE) && !defined (_UNICODE)
	#define _UNICODE
#endif

#ifndef _WIN32
	#ifndef NULL
		#define NULL    0
	#endif

	#include <ctype.h>
	typedef int HFILE;
	typedef void*				HANDLE;
	typedef unsigned long       DWORD;
	typedef int                 BOOL;
	typedef unsigned char       BYTE;
	typedef unsigned short      WORD;
	typedef unsigned int        UINT;

	typedef unsigned short WCHAR;    // wc,   16-bit UNICODE character
	typedef const WCHAR *LPCWSTR;
	typedef const char *LPCSTR;
	typedef WCHAR *LPWSTR;
	typedef char *LPSTR;

	#ifdef  _UNICODE
		typedef wchar_t TCHAR;
		typedef LPCWSTR LPCTSTR;
		typedef LPWSTR LPTSTR;
		#define _T(x)      L ## x
	#else   /* _UNICODE */               // r_winnt
		typedef char TCHAR;
		typedef LPCSTR LPCTSTR;
		typedef LPSTR LPTSTR;
		#define _T(x)      x
	#endif /* _UNICODE */                // r_winnt


#else
   #include <tchar.h>
         #include <windows.h>
      	#ifndef STRICT
		#define STRICT
	#endif
        #include <windows.h>
        #      define ZEXPORT __declspec(dllexport) WINAPI
        #      define ZEXPORTRVA __declspec(dllexport) WINAPIV


#endif	// #ifndef _WIN32

#ifndef ASSERT
	#include <assert.h>
	#define ASSERT(f) assert((f))
#endif
#ifndef VERIFY
	#ifdef _DEBUG
		#define VERIFY(x) ASSERT((x))
	#else
		#define VERIFY(x) x
	#endif
#endif


#ifndef TRACE
	#define TRACE
#endif

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_STDAFX_H__926F70F4_1B34_49AA_9532_498E8D2F3495__INCLUDED_)
