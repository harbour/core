// ZipString.cpp : CZipString STL implemmentation
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


#ifndef __ZIPSTRING_H__
#define __ZIPSTRING_H__
#include "stdafx.h"

#if defined(_MSC_VER) && (_MSC_VER > 1100) 
	#pragma warning( push, 3 ) // STL requrements
#endif

#include <string>
#include <algorithm>
#include <locale>
#include <stdarg.h>
#include <stdio.h>


#ifndef _vsntprintf 
	#ifdef  _UNICODE
		#define _vsntprintf _vsnwprintf
	#else
		#define _vsntprintf _vsnprintf
	#endif
#endif


#if defined _MSC_VER && !defined __BORLANDC__/*_MSC_VER may be defined in Borland after converting the VC project */
	#define _ZIPUSEFACET(loc, fac) std::use_facet(loc, (fac *)0, true)
#else
	#define _ZIPUSEFACET(loc, fac) std::use_facet< fac > (loc)
#endif


typedef std::basic_string<TCHAR> stdbs;
/**
	This is not a full MFC - CString replacement.
	For now it contains mostly the methods required by ZipArchive library.
*/
class CZipString : public stdbs
{
	void TrimInternalL(size_type iPos)
	{
		if (iPos == npos)
			erase ();
		if (iPos)
			erase(0, iPos);
	}
	void TrimInternalR(size_type iPos)
	{
		if (iPos == npos)
			erase ();
		erase(++iPos);
	}
	static int zslen(const TCHAR* lpsz)
	{
		return lpsz ? std::char_traits<TCHAR>::length(lpsz) : 0;
	}
public:
	CZipString(){}
	explicit CZipString (TCHAR ch, int nRepeat = 1):stdbs(nRepeat, ch){}
	CZipString( const CZipString& stringSrc ) {assign(stringSrc);}
	CZipString( const stdbs& stringSrc ) {assign(stringSrc);}
	CZipString( LPCTSTR lpsz ):stdbs(lpsz){}
	operator LPCTSTR() const{return c_str();}
	
	int GetLength() const {return size();}
	bool IsEmpty() const {return empty();}
	void Empty() {erase(begin(), end());}
	TCHAR GetAt (int iIndex) const{return at(iIndex);}
	TCHAR operator[] (int iIndex) const{return at(iIndex);}
	void SetAt( int nIndex, TCHAR ch ) {at(nIndex) = ch;}
	LPTSTR GetBuffer(int nMinBufLength)
	{
		if (size() < nMinBufLength)
			resize(nMinBufLength);
		return empty() ? const_cast<TCHAR*>(data()) : &(at(0));
	}
	void ReleaseBuffer( int nNewLength = -1 ) { resize(nNewLength > -1 ? nNewLength : zslen(c_str()));}
	void TrimLeft( TCHAR chTarget )
	{
		TrimInternalL(find_first_not_of(chTarget));
	}
	void TrimLeft( LPCTSTR lpszTargets )
	{
		TrimInternalL(find_first_not_of(lpszTargets));
	}
	void TrimRight( TCHAR chTarget )
	{
		TrimInternalR(find_last_not_of(chTarget));
	}
	void TrimRight( LPCTSTR lpszTargets )
	{
		TrimInternalR(find_last_not_of(lpszTargets));
	}
	void Format(LPCTSTR lpszFormat, ...)
	{
		va_list arguments;
		va_start (arguments, lpszFormat);
		TCHAR* pBuf = NULL;
		int iCounter = 1, uTotal = 0;
		do 
		{
			int nLen = sizeof(TCHAR) * iCounter*1024;
			pBuf = (TCHAR*)realloc((void*)pBuf, nLen);
			if (!pBuf)
				return;
			uTotal = _vsntprintf(pBuf, nLen - 1, lpszFormat, arguments);
			if (uTotal == -1 || (uTotal == nLen - 1) ) // for some implementations
			{
				pBuf[nLen - 1] = _T('\0');
				if (iCounter == 7)
					break;
			}
			else
			{
				pBuf[uTotal] = _T('\0');
				break;
			}

		} while (true);
		
		va_end (arguments);
		*this = pBuf;
		free(pBuf);
	}
	void Insert( int nIndex, LPCTSTR pstr ){insert(nIndex, pstr, zslen(pstr));}
	void Insert( int nIndex, TCHAR ch ) {insert(nIndex, 1, ch);}
	int Delete( int nIndex, int nCount = 1 )
	{
		int iSize = size();
		int iToDelete = iSize < nIndex + nCount ? iSize - nIndex : nCount;
		if (iToDelete > 0)
		{
			erase(nIndex, iToDelete);
			iSize -= iToDelete;
		}
		return iSize;
	}
	void MakeLower() 
	{
		TCHAR* pBegin = &(at(0));
		_ZIPUSEFACET(std::locale(), std::ctype<TCHAR>).tolower(pBegin, pBegin + size());
	}
	void MakeUpper() 
	{
		TCHAR* pBegin = &(at(0));
		_ZIPUSEFACET(std::locale(), std::ctype<TCHAR>).toupper(pBegin, pBegin + size());
	}
	void MakeReverse()
	{
		std::reverse(begin(), end());

	}
	CZipString Left( int nCount ) const { return substr(0, nCount);}
	CZipString Right( int nCount) const 
	{
		nCount = size() < nCount ? size() : nCount;
		return substr(size() - nCount);
	}
	CZipString Mid( int nFirst ) const {return substr(nFirst);}
	CZipString Mid( int nFirst, int nCount ) const {return substr(nFirst, nCount);}
	int Collate( LPCTSTR lpsz ) const
	{
		return _ZIPUSEFACET(std::locale(), std::collate<TCHAR>).compare(c_str(), c_str() + size(), lpsz, lpsz + zslen(lpsz));
	}
	int CollateNoCase( LPCTSTR lpsz ) const
	{
		CZipString s1(c_str()), s2(lpsz);
		s1.MakeLower();
		s2.MakeLower();
		return s1.Collate(s2);
	}
	int Compare( LPCTSTR lpsz ) const
	{
		return compare(lpsz);
	}
	int CompareNoCase( LPCTSTR lpsz ) const
	{
		CZipString s1(c_str()), s2(lpsz);
		s1.MakeLower();
		s2.MakeLower();
		return s1.Compare(s2);
	}
	bool operator != (LPCTSTR lpsz)
	{
		return Compare(lpsz) != 0;
	}
	bool operator == (LPCTSTR lpsz)
	{
		return Compare(lpsz) == 0;
	}
	
};

#if defined(_MSC_VER) && (_MSC_VER > 1100)
	#pragma warning( pop)
#endif


#endif //__ZIPSTRING_H__