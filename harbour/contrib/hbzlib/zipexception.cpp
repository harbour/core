// ZipException.cpp: implementation of the CZipException class.
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
#include "zipexception.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
#ifdef _MFC_VER
	IMPLEMENT_DYNAMIC( CZipException, CException)
#endif

CZipException::CZipException(int iCause, LPCTSTR lpszZipName)
#ifdef _MFC_VER
	:CException(TRUE)
#endif
{
	m_iCause = iCause;

	if (lpszZipName)
		m_szFileName = lpszZipName;	
}

CZipException::~CZipException()
{

}

void CZipException::Throw(int iZipError, LPCTSTR lpszZipName)
{
#ifdef _MFC_VER
	throw new CZipException(iZipError, lpszZipName);
#else
	CZipException e(iZipError, lpszZipName);
	throw e;
#endif
}

int CZipException::ZlibErrToZip(int iZlibError)
{
	switch (iZlibError)
	{
	case 2://Z_NEED_DICT:
		return CZipException::needDict;
	case 1://Z_STREAM_END:
		return CZipException::streamEnd;
	case -1://Z_ERRNO:
		return CZipException::errNo;
	case -2://Z_STREAM_ERROR:
		return CZipException::streamError;
	case -3://Z_DATA_ERROR:
		return CZipException::dataError;
	case -4://Z_MEM_ERROR:
		return CZipException::memError;
	case -5://Z_BUF_ERROR:
		return CZipException::bufError;
	case -6://Z_VERSION_ERROR:
		return CZipException::versionError;
	default:
		return CZipException::generic;
	}
	
}
