// ZipAbstractFile.h: interface for the ZipAbstractFile class.
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
#if !defined(AFX_ZIPABSTRACTFILE_H__46F247DE_21A6_4D12_AF64_B5A6B3CF4D57__INCLUDED_)
#define AFX_ZIPABSTRACTFILE_H__46F247DE_21A6_4D12_AF64_B5A6B3CF4D57__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
#include "zipstring.h"
class CZipAbstractFile
{
public:

	enum {	begin	= SEEK_SET,
			end		= SEEK_END,
			current = SEEK_CUR
	};
	CZipAbstractFile(){}
	virtual bool Open(LPCTSTR lpszFileName, UINT openFlags, bool bThrow) = 0;
	virtual void Close() = 0;
	virtual void Flush() = 0;
	virtual long GetPosition() = 0;
	virtual void SeekToBegin(){Seek(0, begin);}
	virtual void SeekToEnd(){Seek(0, end);}
	virtual CZipString GetFilePath() = 0;
	virtual void SetLength(long nNewLen) = 0;
	virtual UINT Read(void *lpBuf, UINT nCount) = 0;
	virtual void Write(const void* lpBuf, UINT nCount) = 0;
	virtual long Seek(long lOff, int nFrom) = 0;
	virtual bool IsClosed() = 0;
	virtual DWORD	GetLength() = 0;
	virtual ~CZipAbstractFile(){};

};



#endif // !defined(AFX_ZIPABSTRACTFILE_H__46F247DE_21A6_4D12_AF64_B5A6B3CF4D57__INCLUDED_)
