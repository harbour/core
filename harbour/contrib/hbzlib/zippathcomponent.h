/**
* \file ZipPathComponent.h
* Interface for the CZipPathComponent class.
*
* \author Tadeusz Dracz
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

#if !defined(AFX_ZIPPATHCOMPONENT_H__9B222C08_AD11_4138_96CC_1237511E3E37__INCLUDED_)
#define AFX_ZIPPATHCOMPONENT_H__9B222C08_AD11_4138_96CC_1237511E3E37__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
#include "zipstring.h"

/**
	A class splitting a file path into components.
*/
class CZipPathComponent  
{
public:
	CZipPathComponent(){}
	virtual ~CZipPathComponent();

	static TCHAR m_cSeparator; ///< A system - specific default path separator. Defined in ZipPlatform.cpp.
/**
	Append a path separator to \e szPath if it is not already there.
	\param	szPath
*/
	static void AppendSeparator(CZipString& szPath)
	{
// 		szPath.TrimRight(m_cSeparator);
		szPath.TrimRight(_T("\\/"));
		szPath += m_cSeparator;
	}
	
/**
	Construct the object and set a path.
	\param	szFullPath
		the full path of the file
	\see SetFullPath
*/
	CZipPathComponent(const CZipString& szFullPath)
	{
		SetFullPath(szFullPath);
	}
	
	// full path of the file (not a directory alone)
/**
	Set the path.
	\param	szFullPath
		a full path to the file (including a filename - the last element in the
		path is assumed to be a filename)
*/
	void SetFullPath(const CZipString& szFullPath);

/**
	\return	the filename (without an extension)
*/
	CZipString GetFileName() { return m_szFileName;}

/**
	Set the filename only (without extension).
	\param	szFileName
		
	\return	
*/
	void SetFileName(const CZipString& szFileName) { m_szFileName = szFileName;}

	
/**
	Set the extension alone.
	\param	lpszExt
		may but not have to contain a dot at the beginning
*/
	void SetExtension(LPCTSTR lpszExt) 
	{
		m_szFileExt = lpszExt;
		m_szFileExt.TrimLeft(_T('.'));
	}

/**
	\return	the file extension without a dot
*/
	CZipString GetFileExt() { return m_szFileExt;}
/**
	\return	the drive (no path separator at the end)
*/
	CZipString GetFileDrive() { return m_szDrive;}
/**
	\return	the full path without the drive (no separator at the beginning)
*/
	CZipString GetNoDrive();

/**
	\return	the filename including the extension
*/
	CZipString GetFullFileName()
	{
		CZipString szFullFileName = m_szFileName;
		if (!m_szFileExt.IsEmpty())
		{
			szFullFileName += _T(".");
			szFullFileName += m_szFileExt;
		}
		return szFullFileName;
	}
/**
	\return	the full path of the file (including the filename)
*/
	CZipString GetFullPath()
	{
		CZipString szFullPath = GetFilePath();
		CZipString szFileName = GetFullFileName();
		if (!szFileName.IsEmpty())
		{
			szFullPath  += m_cSeparator;
			szFullPath  += szFileName;
		}
		return szFullPath;

	}
/**
	\return	the path of the file (without the separator at the end)
*/
	CZipString GetFilePath()
	{
			CZipString szDrive = m_szDrive;
			CZipString szDir = m_szDirectory;
			if (!szDrive.IsEmpty() && !szDir.IsEmpty())
				szDrive += m_cSeparator;

			return m_szPrefix + szDrive + szDir;	

	}
protected:
	/**
		\name Path components
	*/
	//@{
	CZipString m_szDirectory,	///< a directory(ies) (one or more) without the path separators at the end and the beginning
		m_szFileName,			///< a filename without an extension
		m_szFileExt,			///< a file extension without a dot
		m_szDrive,				///< a drive (if the system path standard uses it) without a path separator at the end
		m_szPrefix;				///< a prefix (e.g. for the UNC path or Unicode path under Windows)
	//@}
	
};

#endif // !defined(AFX_ZIPPATHCOMPONENT_H__9B222C08_AD11_4138_96CC_1237511E3E37__INCLUDED_)
