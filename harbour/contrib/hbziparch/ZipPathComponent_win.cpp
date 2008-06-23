////////////////////////////////////////////////////////////////////////////////
// This source file is part of the ZipArchive library source distribution and
// is Copyrighted 2000 - 2007 by Artpol Software - Tadeusz Dracz
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
// 
// For the licensing details refer to the License.txt file.
//
// Web Site: http://www.artpol-software.com
////////////////////////////////////////////////////////////////////////////////

#include "_platform.h"

#ifdef ZIP_ARCHIVE_WIN

#include "stdafx.h"
#include "ZipPathComponent.h"

CZipPathComponent::~CZipPathComponent()
{

}

void CZipPathComponent::SetFullPath(LPCTSTR lpszFullPath)
{

	TCHAR szDrive[_MAX_DRIVE];
#if defined _UNICODE && _MSC_VER >= 1400
	TCHAR szDir[32767];
#else
	TCHAR szDir[_MAX_DIR];
#endif
	TCHAR szFname[_MAX_FNAME];
	TCHAR szExt[_MAX_EXT];
	
	
	CZipString szTempPath(lpszFullPath);
	const CZipString szPrefix = _T("\\\\?\\unc\\");
	int i = -1, iLen = szPrefix.GetLength();
	if (iLen > szTempPath.GetLength())
		iLen = szTempPath.GetLength();
	CZipString szPossiblePrefix = szTempPath.Left(iLen);
	szPossiblePrefix.MakeLower(); // must perform case insensitive comparison
	while (++i < iLen && szPossiblePrefix[i] == szPrefix[i]); 
	if (i == 2 || i == 4 || i == 8) // unc path, unicode path or unc path meeting windows file name conventions
	{
		m_szPrefix = szTempPath.Left(i);
		szTempPath = szTempPath.Mid(i);		
	}
	else
		m_szPrefix.Empty();
#if _MSC_VER >= 1400	
	_tsplitpath_s(szTempPath, szDrive , szDir, szFname, szExt);
#else
	_tsplitpath(szTempPath, szDrive , szDir, szFname, szExt);
#endif
	
	m_szDrive = szDrive;
	m_szDirectory = szDir;
	
	m_szDirectory.TrimLeft(m_cSeparator);
	m_szDirectory.TrimRight(m_cSeparator);
	SetExtension(szExt);
	m_szFileTitle = szFname;
}


CZipString CZipPathComponent::GetNoDrive() const
{
	CZipString szPath = m_szDirectory;
	CZipString szFileName = GetFileName();
	if (!szFileName.IsEmpty() && !szPath.IsEmpty())
		szPath += m_cSeparator;

	szPath += szFileName;
	return szPath;	
}

#endif // ZIP_ARCHIVE_WIN
