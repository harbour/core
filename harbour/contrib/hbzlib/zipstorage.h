/**
* \file ZipStorage.h
* Interface for the CZipStorage class.	
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

#if !defined(AFX_ZIPSTORAGE_H__941824FE_3320_4794_BDE3_BE334ED8984B__INCLUDED_)
#define AFX_ZIPSTORAGE_H__941824FE_3320_4794_BDE3_BE334ED8984B__INCLUDED_

#include "zipfile.h" 
#include "zipautobuffer.h"
#include "zipstring.h"
#include "zipmemfile.h"


#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000


typedef bool (*ZIPCALLBACKFUN )(DWORD, int , void* );

/**
	Class that represents a physical storage of the archive
*/
ZEXPORT class CZipStorage  
{
	friend class CZipCentralDir;
public:
	
	/**
		The type of the disk spanning archive.
		\see CZipArchive::GetSpanMode
	*/
	enum ZipSpanMode {
		noSpan,			///< no disk spanning
		pkzipSpan,		///< \ref PKSpan "disk spanning compatible with PKZIP"
		tdSpan,			///< \ref TDSpan "TD mode disk spanning archive"
		/**
			Detect the type automatically.
			If the archive is on the removable device assume PKZIP compatible,
			otherwise TD mode compatible.
		*/
		suggestedAuto,	
		/**
			If the disk spanning archive is on the removable device 
			assume it is TD mode compatible
		*/
		suggestedTd
	};

	CZipStorage();
	virtual ~CZipStorage();

/**
	Open the archive in memory (new or existing).
	The parameters are the same as CZipArchive::ZipOpenMode.
	\param	mf
	\param	iMode
	\note Throws exceptions.
	\see CZipArchive::Open(CZipMemFile& , int);
*/
	void Open(CZipMemFile& mf, int iMode);

/**
	Open or create an archive.
	The parameters are the same as CZipArchive::Open.
	\param	szPathName
	\param	iMode
	\param	iVolumeSize
	\note Throws exceptions.
	\see CZipArchive::Open
*/
	void Open(LPCTSTR szPathName, int iMode, int iVolumeSize);

/**
	Called only by CZipCentralDir::Read() when opening an existing archive.
	\param	uLastDisk
		the disk number the central directory is on
	\note Throws exceptions.

*/
	void UpdateSpanMode(WORD uLastDisk);


/**
	Write chunk of data to the archive.
	\param	pBuf
		buffer with data
	\param	iSize
		bytes to write
	\param	bAtOnce
		if \c true, the whole data must fit in the current volume, 
		otherwise the disk change is performed
	\note Throws exceptions.
*/
	void Write(const void *pBuf, DWORD iSize, bool bAtOnce);

/**
	Read chunk of data from the archive.
	\param	pBuf
		buffer to receive data
	\param	iSize
		bytes to read
	\param	bAtOnce
		if \c true, the specified count of bytes must be read from the same 
		volume (no disk change allowed)
	\note Throws exceptions.
*/
	DWORD Read(void* pBuf, DWORD iSize, bool bAtOnce);

	/**
		Return the position in the file, taking into account the bytes in the write buffer.
		\note Throws exceptions.
	*/
	DWORD GetPosition();


	/**
		Flush the data from the read buffer to the disk.
		\note Throws exceptions.
	*/
	void Flush();

/**
	A method used to change disks during writing to the disk spanning archive.
	\param	iNeeded
		no of bytes needed on the disk
	\param	lpszFileName
		the archive filename
	\note Throws exceptions.
*/
	void NextDisk(int iNeeded, LPCTSTR lpszFileName = NULL);


/**
	\return a zero-based number of the current disk
*/
	int GetCurrentDisk();

 
/**
	Change the disk during extract operations.
	\param	iNumber
		a zero-based disk number requested
	\return	
*/
	void ChangeDisk(int iNumber);

/**
	Detect the span mode.
	\return	
		- -1 - existing disk spanning archive opened
		- 0 - no disk spanning archive
		- 1 - disk spanning archive in creation

*/
	int IsSpanMode();

/**
	
	\param	bAfterException
		set to \c true after the library has throw an exception.
		The simplified mode is used then.
		In this case it'll be possible to reuse the object to operate on another
		archive, but the current archive file will not be valid anyway.
	\note Throws exceptions.
*/
	void Close(bool bAfterException);


	/**
		The size of the write buffer. 
		Set before opening the archive. 
		It is usually set with CZipArchive::SetAdvanced
		(specify this value as the first argument).
		\see CZipArchive::SetAdvanced
	*/
	int m_iWriteBufferSize;

	/**
		The User data to be passed to the callback function as the third parameter.
		\see \ref callDes "CZipArchive::SetSpanCallback"
	*/
	void* m_pCallbackData;

	/**
		The physical archive file (on a storage device).
		Not used when opening archive in memory
		with Open(CZipMemFile& , int).
	*/
	CZipFile m_internalfile;

	/**
		The buffer representing the archive. 
		It is a physical file or a memory buffer depending on what
		method was used to open the archive. In the first case it is 
		a pointer to #m_internalfile.
		\see Open(LPCTSTR, int, int);
		\see Open(CZipMemFile& mf, int iMode)
		
	*/
	CZipAbstractFile* m_pFile;	

	/**
		Takes one of the values of #ZipSpanMode.	
	*/
	int m_iSpanMode;

	/**
		A callback function called when there is a need for a disk change 
		while operating on a #pkzipSpan archive.
		\see \ref callDes "CZipArchive::SetSpanCallback"
	*/
	ZIPCALLBACKFUN m_pChangeDiskFunc;

	/**
		The signature of the extended header	
	*/
	static char m_gszExtHeaderSignat[];
protected:
/**
	Open a physical file.
	\param	lpszName
		the name of the file to open
	\param	uFlags
		file open flags
	\param	bThrow
		if \c true then throw an exception in case of failure
	\return	\c true if successful
*/
	bool OpenFile(LPCTSTR lpszName, UINT uFlags, bool bThrow = true);
/**
	Throw an exception with the given code.
	\param	err
	\see CZipException::Throw
*/
	void ThrowError(int err);

/**
	Return the number of bytes left on the current volume.		
*/
	DWORD VolumeLeft();


/**
	Write data to the internal buffer.
	\param	*pBuf
		the buffer to copy data from
	\param	uSize
		bytes to write
	\note Throws exceptions.
*/
	void WriteInternalBuffer(const char *pBuf, DWORD uSize);

/**
	\return	the number of free bytes on the current removable disk
*/
	DWORD GetFreeVolumeSpace();

/**
	Call the callback function.
	Throw an exception if the callback function returns \c false.
	\param	iCode
		a code to be passed to the callback function
	\param	szTemp
		a string to be used as a filename (the second argument
		of CZipException::Throw) when the exception must be thrown
	\note Throws exceptions.
	\see \ref callDes "CZipArchive::SetSpanCallback"
	\see CZipException::Throw
*/
	void CallCallback(int iCode, CZipString szTemp);


/**
	Construct the name of the volume in #tdSpan mode.
	\param	bLast
		must be \c true if constructing the last volume name (an extension "zip" is given)
	\param	lpszZipName
		the name of the archive
	\return	
		the new volume name
*/
	CZipString GetTdVolumeName(bool bLast, LPCTSTR lpszZipName = NULL);

	/**
		Change the disk in #tdSpan mode
	*/
	CZipString ChangeTdRead();

	/**
		Change the disk in #pkzipSpan mode
	*/
	CZipString ChangePkzipRead();
	

	/**
		Used only in \ref TDSpan "TD span mode" . The value it holds depends on the open mode.
		- Opened existing disk spanning archive - store the number of the last
		disk ( the one with "zip" extension).
		- Disk spanning archive in creation - the size of the volume.

		\see CZipArchive::Open
		\see CZipArchive::GetSpanMode

	*/
	int m_iTdSpanData;

	/**
		\return	the count bytes left free in the write buffer
	*/
	DWORD GetFreeInBuffer();
	
	/**
		Number of bytes available in the write buffer.		
	*/
	DWORD m_uBytesInWriteBuffer;

/**
	The value it holds depends on the open mode:
	\par
	- #tdSpan : the total size of the current volume
	- #pkzipSpan: a free space on the current volume
*/
	DWORD m_uCurrentVolSize;


	/**
		number of bytes left free in the write buffer		
	*/
	DWORD m_uVolumeFreeInBuffer;

	/**
		Write buffer caching data.
	*/
	CZipAutoBuffer m_pWriteBuffer;


	/**
		Used only during disk spanning archive creation.
		Tells how many bytes have been written physically to the current volume.
	*/
	DWORD m_iBytesWritten;

	/**
		\c True, if the current archive is a new disk spanning archive.
	*/
	bool m_bNewSpan;

	/**
		The current disk in a disk spanning archive.
		Disk no 0 is the first disk.
	*/
	int m_iCurrentDisk;

	/**
		It is set to \e true when an archive is created in memory; \e false otherwise.
	*/
	bool m_bInMemory;
	
};

#endif // !defined(AFX_ZIPSTORAGE_H__941824FE_3320_4794_BDE3_BE334ED8984B__INCLUDED_)
