/**
* \file ZipException.h
* Interface for the CZipException class.
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

#if !defined(AFX_ZIPEXCEPTION_H__E3546921_D728_11D3_B7C7_E77339672847__INCLUDED_)
#define AFX_ZIPEXCEPTION_H__E3546921_D728_11D3_B7C7_E77339672847__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000


#include "zipstring.h"
#include "zipbaseexception.h"
 
/**
	A class representing exceptions specific to the ZipArchive library.
	Library exception class derived in the MFC version from CException 
	and in non-MFC version from std::exception.	
*/
ZEXPORT class CZipException : public CZipBaseException
{
public:
		CZipException(int iCause = generic, LPCTSTR lpszZipName = NULL);
		CZipException::CZipException(CZipException& e)
		{
			m_szFileName = e.m_szFileName;
			m_iCause = e.m_iCause;
		}

/**
		Throw an exception.
		Throw CZipException* in the MFC version of the library
		(the object must be deleted with Delete() method)
		and CZipException in other versions.
		
	The arguments are the same as in CZipException().

	\param	iZipError
	\param	lpszZipName
	
	\see CZipException()
	
*/
	static void Throw(int iZipError = CZipException::generic, LPCTSTR lpszZipName = NULL);
	 
/**
	Convert a zlib library error code to a \link #ZipErrors CZipException error code \endlink
	\param	iZlibError
		zlib library error code
	\return	\link #ZipErrors CZipException error code \endlink
*/
	static int ZlibErrToZip(int iZlibError);

	/**
		The name of the zip file where the error occurred.
	*/
	CZipString m_szFileName;

	/**
		The codes of errors thrown by the ZipArchive library
	*/
	enum ZipErrors
	{
		noError,			///< no error 
// 			 1 - 42 reserved for errno (from STL) values - used only in non-MFC versions
// 			 43 - 99 reserved
		generic		= 100,	///< unknown error
		badZipFile,			///< damaged or not a zip file
		badCrc,				///< crc mismatched
		noCallback,			///< no callback function set
		aborted,			///< disk change callback function returned \c false
		nonRemovable,		///< the disk selected for pkzipSpan archive is non removable
		tooManyVolumes,		///< limit of the maximum volumes reached (999)
		tooLongFileName,	///< the filename of the file added to the archive is too long
		badPassword,		///< incorrect password set for the file being decrypted
		dirWithSize,		///< during testing: found the directory with the size greater than 0
		internal,			///< internal error
		notRemoved,			///< error while removing a file (under Windows call GetLastError() to find out more)
		notRenamed,			///< error while renaming a file (under Windows call GetLastError() to find out more)
		platfNotSupp,		///< the platform that the zip file was created under is not supported
		cdirNotFound,		///< the central directory was not found in the archive (it is thrown also when the last disk of multi-disk archive is not in the drive when opening the archive)
		streamEnd	= 500,	///< zlib library error
		needDict,			///< zlib library error
		errNo,				///< zlib library error
		streamError,		///< zlib library error
		dataError,			///< zlib library error
		memError,			///< zlib library error thrown by CZipMemFile as well
		bufError,			///< zlib library error
		versionError,		///< zlib library error
	};
	/**
		A cause of the error - takes one of the #ZipErrors enumeration codes.
	*/
	int m_iCause;


/**
	\param	iCause
		error cause (takes one of the #ZipErrors enumeration codes)
	\param	lpszZipName
		the name of the file where the error occurred (if applicable)
*/
	
	virtual ~CZipException();
#ifdef _MFC_VER
	DECLARE_DYNAMIC(CZipException)
#endif
};


#endif // !defined(AFX_ZIPEXCEPTION_H__E3546921_D728_11D3_B7C7_E77339672847__INCLUDED_)
