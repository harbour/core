// ZipFile.cpp: implementation of the ZipFiles class.
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
#include "zipfile.h"
#include "zipexception.h"
#include "zipplatform.h"

#include <fcntl.h>

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CZipFile::CZipFile()
{
	m_hFile = -1;
}




void CZipFile::ThrowError()
{
	CZipException::Throw(errno, m_szFileName);
}


DWORD CZipFile::GetLength()
{

	DWORD dwLen, dwCur;
	dwCur = Seek(0L, current);
	dwLen = Seek(0L, end);
	Seek(dwCur, begin);
	return dwLen;

}


bool CZipFile::Open(LPCTSTR lpszFileName, UINT openFlags, bool bThrow)
{
	if (!IsClosed())
		Close();
	UINT iNewFlags = O_BINARY;
	bool bReadOnly = false;
	if (openFlags & CZipFile::modeCreate)
		iNewFlags |= O_CREAT;
	if ((openFlags & CZipFile::modeReadWrite) == CZipFile::modeReadWrite)
		iNewFlags |= O_RDWR;
	else if (openFlags & CZipFile::modeRead)
	{
		// O_RDONLY is defined as 0
		bReadOnly = true;
		iNewFlags |= O_RDONLY;
	}
	else if (openFlags & CZipFile::modeWrite)
		iNewFlags |= O_WRONLY;

	if (!(openFlags & CZipFile::modeNoTruncate) && !bReadOnly)
		iNewFlags |= O_TRUNC;

	m_hFile = ZipPlatform::OpenFile(lpszFileName, iNewFlags, openFlags & 0x1C);
	if (m_hFile == -1)
		if (bThrow)
			ThrowError();
		else
			return false;
	m_szFileName = lpszFileName;
	return true;
}


void CZipFile::SetLength(long nNewLen)
{
	ZipPlatform::TruncateFile(m_hFile, nNewLen);	
}


void  CZipFile::Flush()
{
	if (!ZipPlatform::FlushFile(m_hFile)) 
		ThrowError();
}

CZipFile::operator HFILE()
{
	HFILE hf = ZipPlatform::GetFileSystemHandle(m_hFile);
	if (hf == -1)
		ThrowError();
	return hf;
}
