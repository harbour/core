// ZipMemFile.cpp: implementation of the CZipMemFile class.
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

#include "stdafx.h"
#include "zipmemfile.h"
#include "zipexception.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

void CZipMemFile::Grow(long nGrowTo)
{
	if (m_nBufSize < (UINT)nGrowTo)
	{
		if (m_nGrowBy == 0)
			CZipException::Throw(CZipException::memError);
		long nNewSize = m_nBufSize;
		while (nNewSize < nGrowTo)
			nNewSize += m_nGrowBy;
		BYTE* lpNew;
		if (m_lpBuf)
			lpNew = (BYTE*)realloc((void*) m_lpBuf, nNewSize);
		else
			lpNew = (BYTE*)malloc(nNewSize);

		if (!lpNew)
			CZipException::Throw(CZipException::memError);
		m_nBufSize = nNewSize;
		m_lpBuf = lpNew;
	}
}

void CZipMemFile::SetLength(long nNewLen)
{
	if (m_nBufSize < (UINT)nNewLen)
		Grow(nNewLen);
	else
		m_nPos = nNewLen;
	m_nDataSize = nNewLen;
}

UINT CZipMemFile::Read(void *lpBuf, UINT nCount)
{
	if (m_nPos > m_nDataSize)
		return 0;
	UINT nToRead = (m_nPos + nCount > m_nDataSize) ? m_nDataSize - m_nPos : nCount;
	memcpy(lpBuf, m_lpBuf + m_nPos, nToRead);
	m_nPos += nToRead;
	return nToRead;

}

void CZipMemFile::Write(const void *lpBuf, UINT nCount)
{
	if (!nCount)
		return;

	if (m_nPos + nCount > m_nBufSize)
		Grow(m_nPos + nCount);
	memcpy(m_lpBuf + m_nPos, lpBuf, nCount);
	m_nPos += nCount;
	if (m_nPos > m_nDataSize)
		m_nDataSize = m_nPos;
}

long CZipMemFile::Seek(long lOff, int nFrom)
{
	long lNew = m_nPos;

	if (nFrom == CZipAbstractFile::begin)
		lNew = lOff;
	else if (nFrom == CZipAbstractFile::current)
		lNew += lOff;
	else if (nFrom == CZipAbstractFile::end)
		lNew = m_nDataSize + lOff;
	else
		return lNew;

	if (lNew< 0)
		CZipException::Throw(CZipException::memError);

	m_nPos = lNew;
	return lNew;
}
bool CZipMemFile::Open(LPCTSTR lpszFileName, UINT openFlags, bool shareMode)
{
return 1;
}

