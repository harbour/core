// ZipFile.h: interface for the ZipFiles class.
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

#if !defined(AFX_ZIPFILE_H__80609DE0_2C6D_4C94_A90C_0BE34A50C769__INCLUDED_)
#define AFX_ZIPFILE_H__80609DE0_2C6D_4C94_A90C_0BE34A50C769__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
#include "zipabstractfile.h"
#include "zipstring.h"
#include <io.h>

ZEXPORT class CZipFile :public CZipAbstractFile
{
	void ThrowError();
public:
	int m_hFile;
	operator HFILE();
	enum OpenModes
	{
		modeRead =          0x0001,
		modeWrite =         0x0002,
		modeReadWrite =     modeRead | modeWrite,
		shareDenyWrite =    0x0004,
		shareDenyRead =     0x0008,
		shareDenyNone =     0x0010,
		modeCreate =        0x0020,
		modeNoTruncate =    0x0040,
	};
	
	CZipFile(LPCTSTR lpszFileName, UINT openFlags)
	{
		m_hFile = -1;
		Open(lpszFileName, openFlags, true);
	}
	void Flush();
	DWORD GetLength();
	CZipString GetFilePath(){return m_szFileName;}
	bool IsClosed(){ return m_hFile == -1;}
	bool Open(LPCTSTR lpszFileName, UINT openFlags, bool bThrow);
	void Close() 
	{
		if (IsClosed())
			return;
		if (close(m_hFile) != 0)
			ThrowError();
		else
		{
			m_szFileName.empty();
			m_hFile = -1;
		}
	}
	void Write(const void* lpBuf, UINT nCount)
	{
		if (write(m_hFile, lpBuf, nCount) != (int) nCount)
			ThrowError();
	}
	long GetPosition()
	{
		long ret = tell(m_hFile);
		if (ret == -1L)
			ThrowError();
		return ret;
	}
	void SetLength(long nNewLen);
	UINT Read(void *lpBuf, UINT nCount)
	{
		errno = 0;
		int ret = read(m_hFile, lpBuf, nCount);
		if (ret < (int) nCount && errno != 0)
			ThrowError();
		return ret;

	}
	long Seek(long dOff, int nFrom)
	{
		long ret = lseek(m_hFile, dOff, nFrom);
		if (ret == -1)
			ThrowError();
		return ret;
	}
	CZipFile ();
	virtual ~CZipFile (){Close();};
protected:
	CZipString m_szFileName;

};

#endif // !defined(AFX_ZIPFILE_H__80609DE0_2C6D_4C94_A90C_0BE34A50C769__INCLUDED_)
