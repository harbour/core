/**
* \file ZipFileMapping.h
* Interface for the CZipFileMapping class.
*
* \author Tadeusz Dracz
* 
*/
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

#if !defined(AFX_AUTOHANDLE_H__D68326EA_D7FA_4792_AB1F_68D09533E399__INCLUDED_)
#define AFX_AUTOHANDLE_H__D68326EA_D7FA_4792_AB1F_68D09533E399__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

namespace ziparchv
{
	

	struct CZipFileMapping
	{
		CZipFileMapping()
		{
			m_hFileMap = NULL;
			m_pFileMap = NULL;
		}
		bool CreateMapping(HANDLE hFile)
		{
			if (!hFile)
				return false;
			m_hFileMap = CreateFileMapping(hFile, NULL, PAGE_READWRITE,
				0, 0, _T("ZipArchive Mapping File"));
			if (!m_hFileMap)
				return false;
			// Get pointer to memory representing file
			m_pFileMap = MapViewOfFile(m_hFileMap, FILE_MAP_WRITE, 0, 0, 0);
			return (m_pFileMap != NULL);
		}
		void RemoveMapping()
		{
			if (m_pFileMap)
			{
				UnmapViewOfFile(m_pFileMap);
				m_pFileMap = NULL;
			}
			if (m_hFileMap)
			{
				CloseHandle(m_hFileMap);
				m_hFileMap = NULL;
			}
			
		}
		~CZipFileMapping()
		{
			RemoveMapping();
		}
		char* GetMappedMemory()
		{
			return reinterpret_cast<char*> (m_pFileMap);
		}
	protected:
		HANDLE m_hFileMap;
		LPVOID m_pFileMap;

	};
}

#endif // !defined(AFX_AUTOHANDLE_H__D68326EA_D7FA_4792_AB1F_68D09533E399__INCLUDED_)
