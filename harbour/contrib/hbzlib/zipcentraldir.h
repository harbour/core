/**
* \file ZipCentralDir.h
*	Interface for the CZipCentralDir class.
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

#if !defined(AFX_CENTRALDIR_H__859029E8_8927_4717_9D4B_E26E5DA12BAE__INCLUDED_)
#define AFX_CENTRALDIR_H__859029E8_8927_4717_9D4B_E26E5DA12BAE__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
#include "zipexception.h"
#include "zipfileheader.h"
#include "zipautobuffer.h"
#include "zipcollections.h"
#include "zipcompatibility.h"
/**
	Used in fast finding files by the filename.
	\see CZipCentralDir::m_findarray
	\see CZipArchive::FindFile
*/
ZEXPORT struct CZipFindFast
{
	CZipFindFast()
	{
		m_uIndex = 0;
		m_pHeader= NULL;
	}
	CZipFindFast(CZipFileHeader* pHeader, WORD uIndex):m_pHeader(pHeader), m_uIndex(uIndex){}
	/**
		We extract a name from it.
	*/
	CZipFileHeader* m_pHeader;

	/**
		The index in the central directory of the \e m_pHeader.
	*/
	WORD m_uIndex;
};

/**
	A class representing the central directory record in the archive.
*/
ZEXPORT class CZipCentralDir  
{
	
public:

	CZipCentralDir();
	virtual ~CZipCentralDir();

	static char m_gszSignature[]; ///< central dir signature

	char m_szSignature[4];	///< end of central dir signature (must be 0x06054b50)
	WORD m_uThisDisk;		///< number of this disk
	WORD m_uDiskWithCD;		///< number of the disk with the start of the central directory
	WORD m_uDiskEntriesNo;	///< total number of entries in the central dir on this disk
	WORD m_uEntriesNumber;	///< total number of entries in the central dir
	DWORD m_uSize;			///< size of the central directory
	DWORD m_uOffset;		///< offset of start of central directory with respect to the starting disk number

	CZipAutoBuffer m_pszComment;	///< the archive comment
	CZipAutoBuffer m_pLocalExtraField; ///< a local extra field
	CZipFileHeader* m_pOpenedFile;	///< points to a currently opened file or NULL if no file is opened

	/**
		Called by CZipArchive::OpenInternal.
	*/
	void Init();

	/**
		Read the central directory from the archive.
		\note Throws exceptions.
	*/
	void Read();

	/**
		Open the file.
		\param uIndex
			zero-based index of the file to open
		\note Throws exceptions.
	*/
	void OpenFile(WORD uIndex);

/**	
	Test if the given file header index is valid.
	\param	uIndex
		a zero-based index 
	\return	\c true if the file with the given index exists inside the archive; otherwise \c false;
*/
	bool IsValidIndex(int uIndex);

/**
	Remove the file header from the central directory.
	\param	uIndex
		a zero-based index of the file header to remove
	\note Throws exceptions.
*/
	void RemoveFile(WORD uIndex);

/**
	Cleanup the structure.
	\param	bEverything
		- \c true - clear some attributes and remove all the file headers from memory
		- \c false - do not remove the file headers. It is called in that manner
		from CZipArchive::CloseFileAfterTestFailed so that the 
		next file can be tested for the integrity
	\see CZipArchive::CloseFileAfterTestFailed
*/
	void Clear(bool bEverything = true);

/**
	Add a new file to the central directory.
	\param	header
		copy data from it to the new file header
	\note Throws exceptions.
*/
	void AddNewFile(const CZipFileHeader & header);

/**
	Remove physically the central directory from the archive.
	Called during adding or deleting files.
	\note Throws exceptions.
*/
	void RemoveFromDisk();

/**
	Get the central directory size.
	\param	bWhole
		if \c true, include the size of the file headers
	\return	the size of the central directory
*/
	DWORD GetSize(bool bWhole = false);

	/**
		Close a file inside archive opened for reading.
		\note Throws exceptions.
	*/
	void CloseFile();

	/**
		Close a file inside archive opened for reading.
		\note Throws exceptions.
	*/
	void CloseNewFile();

	/**
		Write the central directory to the archive.
		\note Throws exceptions.
	*/
	void Write();
	

	/**
		Points to CZipArchive::m_storage.
	*/
	CZipStorage* m_pStorage;

	DWORD m_uCentrDirPos;	///< the position of the beginning of the central directory
	/**
		The count of bytes before the actual zip archive in a file.
		It is non-zero for self-extracting archives.
	*/
	DWORD m_uBytesBeforeZip;
	
	/**
		The size of the buffer used in searching for the central dir.
		Set before opening the archive.
		It is usually set with CZipArchive::SetAdvanced
		(specify this value as the third argument).
		\see CZipArchive::SetAdvanced
	*/
	int m_iBufferSize;
	

	/**
		Used in fast finding files by the filename.
		\see CZipFindFast
		\see m_bFindFastEnabled
		\see CZipArchive::FindFile
	*/
	CZipFindFastArray m_findarray;
	typedef CZipPtrList<CZipFindFast*>::iterator CZipFindFastLstIter;

	/**
		If \c true, the there is an additional array build, to speed up the 
		finding process
		CZipArchive::FindFile uses this array to perform a 
		binary search.
		\b Default: \c false
		\see CZipArchive::EnableFindFast
		\see CZipArchive::FindFile
		\see CZipCentralDir::m_findarray
	*/
	bool m_bFindFastEnabled;

/**
	Clear #m_findarray contents and free memory.
*/
/**
	Build #m_findarray.
*/
	void BuildFindFastArray();

	/**
		Holds all the files inside archive info.
		\see CZipFileHeader
	*/
	CZipPtrList<CZipFileHeader*> m_headers;
	typedef CZipPtrList<CZipFileHeader*>::iterator CZipFileHdrLstIter;



	/**
		\name Helpers
		Helper methods to operate on the CZipPtrList
		
	*/	
	//@{
	
	CZipFileHeader* operator[](int iIndex);
	CZipFileHdrLstIter GetIterator(int iIndex);
	CZipFileHeader* GetFileHeader(const CZipFileHdrLstIter &iterator);
	//@}
	/**
		- If \c true, the conversion of the filenames takes 
		place after opening the archive (after reading the central directory 
		from the file), and before writing the central directory back to
		the archive.
		- If \c false, the conversion takes place on each call to CZipArchive::GetFileInfo

		Set it to \c true when you plan to use CZipArchive::FindFile or get the stored files information. <BR>
		Set it to \c false when you plan mostly to only modify the archive.

		\b Default: \c true
		\note Set it before opening the archive.
		\see ConvertFileName
	*/
	bool m_bConvertAfterOpen;



/**
	Convert the filename of the CZipFileHeader depending on the current system
	and the system the zip file was created on (change slash to backslash or
	vice versa, perform ANSI-OEM conversion if necessary).
	\param	bFromZip
		if \c true, convert from archive format
	\param	bAfterOpen
		if \c true, called after opening the archive or before closing
	\param	pHeader		
		the header to have filename converted; if \c NULL convert the currently
		opened file
	\see ZipCompatibility::FileNameUpdate
	\see m_bConvertAfterOpen
*/
	void ConvertFileName(bool bFromZip, bool bAfterOpen, CZipFileHeader* pHeader = NULL)
	{
		if (bAfterOpen != m_bConvertAfterOpen)
			return;
		if (!pHeader)
		{
			pHeader = m_pOpenedFile;
			ASSERT(pHeader);
		}
		ZipCompatibility::FileNameUpdate(*pHeader, bFromZip);
	}

/**
	Convert all the filenames to the system form.
	Called by CZipArchive::FindFile
	\see CZipArchive::FindFile
*/
	void ConvertAll();

/**
	\param	lpszFileName
		the name of the file to find, must be exactly the same (apart from case)
		as it appears in the archive
	\param	bCaseSensitive
		\c true if perform a case sensitive search	
	\return	the index in #m_findarray with the appropriate CZipFindFast structure
	or \c -1 if there is no file with the given name
	\see CZipArchive::FindFile
*/
	int FindFileNameIndex(LPCTSTR lpszFileName, bool bCaseSensitive);

	
protected:
/**
	The \e lpszFileName and \e bCaseSensitive arguments 
	are the same as in the #FindFileNameIndex. The function get CZipFindFast
	structure pointed by \e uIndex and compares the filename of CZipFileHeader
	class stored in this structure with \e lpszFileName.
	\param	lpszFileName
	\param	uIndex
		the index from #m_findarray
	\param	bCaseSensitive		
	\return 
	- 0 if the filenames are the same
	- < 0 if the filename stored in the array is less than \e lpszFileName
	- > 0 if the filename stored in the array is greater than \e lpszFileName
*/
ZIPINLINE   int CompareElement(LPCTSTR lpszFileName, WORD uIndex, bool bCaseSensitive);

/**
	Insert a new CZipFindFast element to the #m_findarray.
	Initialize CZipFindFast object with \e pHeader and \e uIndex values.
	\param	pHeader
	\param	uIndex
*/
	void InsertFindFastElement(CZipFileHeader* pHeader, WORD uIndex);
	/**
		\c true if the central directory is in physically present in the archive
	*/
	bool m_bOnDisk;

	/**
		\return the location of the beginning of the central dir end record in the archive
		\note Throws exceptions.
	*/
	DWORD Locate();	
	/**
		Read the file headers from the file.
		\note Throws exceptions.
	*/
	void ReadHeaders();

	/**
		Free the memory allocated for the CZipFileHeader structures.
	*/
	void RemoveHeaders();
/**
	Remove data descriptors from the write buffer in the disk spanning volume
	that turned out to be one-disk only.
	We do not remove them from password encrypted files.

	\param	bFromBuffer
		if \c true, remove from the buffer in memory otherwise from the file on a disk
	\return	\c false if the file mapping to memory was not successful
	Can happen only when \e bFormBuffer is \c false.
	\note Throws exceptions.
*/
	bool RemoveDataDescr(bool bFromBuffer);
/**
	Write the file headers to the archive.
	\note Throws exceptions.
*/
	void WriteHeaders();
/**
	Write the central directory end record.
	\return	the size of the record
	\note Throws exceptions.
*/
	DWORD WriteCentralEnd();

/**
	Throw an exception with the given code.
	\param	err
	\see CZipException::Throw
*/
	void ThrowError(int err);
};


#endif // !defined(AFX_CENTRALDIR_H__859029E8_8927_4717_9D4B_E26E5DA12BAE__INCLUDED_)
