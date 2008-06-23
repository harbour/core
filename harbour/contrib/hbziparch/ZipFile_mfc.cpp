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

#include "_features.h"

#if defined ZIP_ARCHIVE_MFC && !defined ZIP_FILE_USES_STL

#include "stdafx.h"
#include "ZipFile.h"

// IMPLEMENT_DYNAMIC(CZipAbstractFile, CFile)
IMPLEMENT_DYNAMIC(CZipFile, CFile)

CZipFile::CZipFile()
{
}

CZipFile::~CZipFile()
{
	Close();
}

CZipFile::operator HANDLE()
{
	return (HANDLE)m_hFile;
}

#endif
