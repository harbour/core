/**
* \file ZipArchive.h
*	Interface for the CZipArchive class.
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
//
// Check the site http://software.artpol.com.pl for the updated version of the library.
////////////////////////////////////////////////////////////////////////////////
//   
//	The following information files are distributed along with this library:
//		license.txt		- licensing information
//		gpl.txt			- General Public License text
//		readme.txt		- general information
//		changelog.txt	- revision history
//		faq.txt			- frequently asked questions
//		appnote.txt		- details on the zip format
//							( also available at ftp://ftp.pkware.com/appnote.zip)
//
// 


#if !defined(AFX_ZIPARCHIVE_H__A7F528A6_1872_4071_BE66_D56CC2DDE0E6__INCLUDED_)
#define AFX_ZIPARCHIVE_H__A7F528A6_1872_4071_BE66_D56CC2DDE0E6__INCLUDED_

/**
	\namespace ziparchv
	A helper namespace.
	\par Members
		- CZipFileMapping

*/
	
/**
    \struct CZipFileMapping ZipFileMapping.h

	Maps a file to the memory. A system-specific implementation.	
	Stored in ziparchv namespace.

	\c #include "ZipFileMapping.h"
*/


#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

// to ensure that the correct files are copied
// (see "Compatibility" chapter in the documentation).
// Rebuild the project after copying the files.
#ifdef ZIP_ARCHIVE_MFC_PROJ 
	#ifndef ZIP_ARCHIVE_MFC
		#error You need to copy files from the MFC subdirectory \
			to the ZipArchive root directory
	#endif 
#elif defined ZIP_ARCHIVE_STL_PROJ
	#ifndef ZIP_ARCHIVE_STL
		#error You need to copy files from the STL subdirectory \
			to the ZipArchive root directory
	#endif 
#endif //

#include "zipexception.h"
#include "zipcentraldir.h" 
#include "zipstorage.h"
#include "zipinternalinfo.h" 
#include "zippathcomponent.h"
#include "zipstring.h"


/**
	The representation of the archive file.	
	This class provides all the operarions on the zip archive.
*/
ZEXPORT class CZipArchive  
{
	
public:


	CZipArchive();
	virtual ~CZipArchive();

/**
	In non-UNICODE version just copy \e szSingle to \e szWide.
	In UNICODE version works the same way as ZipPlatform::SingleToWide
	\param	szSingle
	\param	szWide
		
	\return	(in non-UNICODE version the number of characters copied)
	\see ZipPlatform::SingleToWide
*/
	static int SingleToWide(const CZipAutoBuffer &szSingle, CZipString& szWide);

/**
	In non-UNICODE version just copy \e lpWide to \e szSingle.
	In UNICODE version works the same way as ZipPlatform::WideToSingle
	\param	lpWide
	\param	szSingle
	\return	(in non-UNICODE version the number of characters copied)
	\see ZipPlatform::WideToSingle

*/
	static int WideToSingle(LPCTSTR lpWide, CZipAutoBuffer &szSingle);


/** 
	Set the password for the file to be opened or created.
	Use this method BEFORE opening or adding a file
	\param	lpszPassword
		set it to NULL to clear password
	\return	\c false if the password contains ASCII characters
		with values 128 or higher or the file inside archive is	opened		

*/
	bool SetPassword(LPCTSTR lpszPassword = NULL);

/**
	\return	the current archive password or an empty string if there is no password
*/
	CZipString GetPassword();

/**
	Set the buffer sizes. No buffer can be set smaller than 1024.
	Use this method before opening the archive. The optimal size for 
	the write buffer in the disk spanning archive is the size of the volume.

	\param	iWriteBuffer
		buffer used during write operation to the disk
	\see CZipStorage::m_iWriteBufferSize
	\param	iExtractBuffer
		buffer used in extracting and compressing files
	\see CZipInternalInfo::m_iBufferSize
	\param	iSearchBuffer
		the buffer used in searching for the central dir
	\see CZipCentralDir::m_iBufferSize
*/
	void SetAdvanced(int iWriteBuffer = 65535, int iExtractBuffer = 16384, int iSearchBuffer = 32768);



/**
	Set callback function used during operations on a 
	PKZIP compatible disk spanning archive to change disks.
	Set it before opening the archive for reading. If you open the archive
	in the \ref PKSpan "PKZIP compatible mode" and don't set the callback function, 
	the exception CZipException::noCallback will be thrown.

	\anchor callDes
	<B>
	Callback function description.
	</B>

	Callback function is called when there is a need for a disk change.
	Calling CZipArchive methods from inside this function may result in an unexpected behavior

	The callback function is defined as follows: <BR>
	typedef bool (*ZIPCALLBACKFUN )(DWORD, int , void* );

	- the first argument \c (DWORD)	- a disk number needed (counting from 1);
	- the second \c (int) - the reason for calling:
		- -1 : a disk needed for reading		<BR>
	other codes occurs during writing and signalize errors:
		- >=0 : number of bytes needed (not enough disk space)
		- -2 : the file with the archive name already exists on the disk
		- -3 : the disk is probably write - protected
		- -4 : couldn't create a file
	- the third argument \c (void*)	- user defined data passed to #SetSpanCallback
	as the second parameter.

	Return \c false from the callback function to abort operation: the proper exception will be thrown.
	
	\param	pFunc
		callback function
	\param	pData
		user data to be passed to the callback function as the last parameter
	\see GetSpanMode
*/
	void SetSpanCallback(ZIPCALLBACKFUN pFunc, void* pData = NULL);

	/**
		archive open modes	
	*/
	enum ZipOpenMode
	{
		zipOpen,			///< open an existing archive
		/**
			Open an existing archive as a read only file.
			This mode is intended to use in a self extract code,
			if you try to add or delete a file in this mode,
			an exception will be thrown.
		*/
		zipOpenReadOnly,
		zipCreate,			///< create a new archive
		zipCreateSpan		///< create a disk spanning archive
	};



/**
	Open or create a zip archive.

	The archive creation mode depends on \e iMode and \e iVolumesSize values:
	- if \e iMode == #zipCreateSpan and \e iVolumeSize <= 0 then create disk spanning 
		archive in \ref PKSpan "PKZIP compatible mode" (pkzipSpan)
	- if \e iMode == #zipCreateSpan and \e iVolumeSize > 0 then create disk spanning 
		archive in \ref TDSpan "TD compatible mode" (tdSpan)
	- if \e iMode == #zipOpen and the existing archive is a spanned archive
		then pkzipSpan mode is assumed if the archive is on a removable device
		or tdSpan otherwise;			<BR>
		if you want to open tdSpan archive on a removable device, set \e iVolumeSize
		to a value different from 0	
	- if \e iMode == #zipCreate then \e iVolumeSize doesn't matter

	\param	szPathName
		the path to the archive
	\param	iMode
		the open mode
	\param	iVolumeSize
		the volume size in the disk spanning archive
	\note Throws exceptions.
	\see GetSpanMode
*/
	void Open(LPCTSTR szPathName, int iMode = zipOpen, int iVolumeSize = 0);


/**
	Open or create the archive in memory. The CZipMemFile object is not closed
	after closing the archive, so that is it possible to work with it afterwards.
	\param	mf
		CZipMemFile structure to create archive in or extract from
	\param	iMode
		Open mode. 
		The following modes are valid:  #zipOpen, #zipOpenReadOnly, #zipCreate
	\note	Throws exceptions.
*/
	void Open(CZipMemFile& mf, int iMode = zipOpen);

/**
	Set #m_szRootPath to a specified value. Use it if you don't want to set
	\e bFullPath argument in #AddNewFile or #ExtractFile to true and you 
	don't want to strip the whole path neither, but only a specific beginning.
	Use it after opening the archive and before using #AddNewFile or #ExtractFile.
	See \ref q9 "the FAQ" for the example of use.
	\param	szPath
		Set it to the string that you want to be omitted from the beginning of the path of the file
		in the archive 

		if \c NULL - clears the #m_szRootPath and no path beginning will be matched against it
	\see #AddNewFile 
	\see #ExtractFile.

*/
	void SetRootPath(LPCTSTR szPath = NULL);

/**
	Add a new file to the archive.

	\anchor callbFas
	<B>
	Callback function description.
	</B>

	To set the callback function for this operation pass its pointer as the 
	argument (do not use \c SetSpanCallback for it - its for different purposes).

	The callback function, if set, is called after reading and writing one portion of data.
	- the first argument \c (DWORD):
			total number of bytes to read (the size of the file)
	- the second one \c (int) :
			total number bytes already read
	- the third argument \c (void*): 
		pUserData argument passed to #AddNewFile

	Return \c true from inside the callback function to continue 
	or \c false to abort the operation (an exception will be most probably thrown).

	\param	lpszFilePath
		the full path to the file to be added
	\param	iLevel 
		the compression level, see #OpenNewFile
	\param	bFullPath <BR>
		 - if \c true, include full path of the file inside the archive;
		 - if \c false only the filename without a path is stored in the archive <BR>
		in this case if #m_szRootPath is set previously with #SetRootPath 
		and if the beginning of \e lpszFilePath equals #m_szRootPath
		then the filename is set to the remaining part of \e lpszFilePath
		(you can say to \e lpszFilePath minus #m_szRootPath)
		

	\param	pCallback
		callback function (may be NULL)
	\param	pUserData
		user - defined data passed on to the callback function
		(doesn't matter if there is no callback function defined)
	\param	nBufSize 
		the size of the buffer used
	\return	\c if it returns false then the file was not added, but the internal
	state allows you to add other files (which is not possible after throwing 
	an exception)
	\note Throws exceptions.
*/
	bool AddNewFile(LPCTSTR lpszFilePath, int iLevel = -1, bool bFullPath = true, ZIPCALLBACKFUN pCallback = NULL, void* pUserData = NULL, unsigned long nBufSize = 65535);
   bool AddNewFileDrv(LPCTSTR lpszFilePath, int iLevel = -1, bool bFullPath = true, ZIPCALLBACKFUN pCallback = NULL, void* pUserData = NULL, unsigned long nBufSize = 65535);

/**
	Add a new file to the opened zip archive. The zip archive cannot be 
	an existing disk spanning archive (existing at the moment of opening archive),
	because modifying such an archive is not possible (at least not with this
	version ).

	\param	header
		The structure that provides additional information about the added file.
		The following fields are valid:
		- \e m_uMethod - file compression method; can be 0 (storing) or Z_DEFLATE (deflating)
			otherwise Z_DEFLATE is assumed
		- \e m_uModDate, \e m_uModTime - Use CZipFileHeader::SetTime to set them.
			If \e lpszFilePath is not NULL this fields are overwritten and updated automatically.
			See CZipFileHeader::SetTime
		- \e m_uExternalAttr - Attributes of the file.
			If \e lpszFilePath is not NULL this field is overwritten and updated automatically.
			Use #SetFileHeaderAttr to set them. See #SetFileHeaderAttr.
		- \e m_szFileName - A filename (may be with path) to be stored inside archive
			to represent this file. See CZipFileHeader::SetFileName
		- \e m_szComment - A file comment.	See CZipFileHeader::SetComment
		- \e m_pExtraField - LOCAL extra field, use #SetExtraField after opening 
			a new file, but before closing it to set the not local extra field 
			in the header in the central directory. See #SetExtraField	<BR>
	Other fields are ignored - they are updated automatically.
	If the function returns \c true, \link #GetSystemCompatibility 
	system compatibility \endlink for this object is
	set to the correct value (the same as #m_iArchiveSystCompatib),
		additionally if \e lpszFilePath was not NULL, the attributes and 
	the time fields are filled with information retrieved from 
	the file pointed by \e lpszFilePath.		
	\param	iLevel
		The level of compression (-1, 0 - 9).
		The are several preset values for the decompression level:
		- Z_DEFAULT_COMPRESSION	: -1
		- Z_NO_COMPRESSION		: 0
		- Z_BEST_SPEED			: 1
		- Z_BEST_COMPRESSION	: 9			
	\param	lpszFilePath
		The path to the file to retrieve date stamp and attributes from.
		These values are stored inside the archive.

	\return	\c false in the following cases:
	- the \e lpszFilePath is not NULL and the file attributes and data was not correctly retrieved
	- a file is already opened for extraction or compression
	- archive is an existing disk span archive
	- maximum file count inside archive already reached (65535)
	\note Throws exceptions.

*/
	bool OpenNewFile(CZipFileHeader & header, int iLevel = Z_DEFAULT_COMPRESSION, LPCTSTR lpszFilePath = NULL);

/**
	Compress the contents of the buffer and write it to a new file.
	\param	pBuf
		the buffer containing the data to be compressed and written
	\param	iSize
		the number of bytes to be written from the buffer
	\return	\c false if the new file hasn't been opened yet
	\note Throws exceptions.
	\see OpenNewFile
*/
	bool WriteNewFile(const void *pBuf, DWORD iSize);


/**
	Set the extra field in the central directory of the currently opened file.
	Must be used after opening a new file in the archive, but before closing it
	To set the local extra field, set it in the CZipFileHeader structure passed
	as the argument to the #OpenNewFile
	\param	pBuf
		the bufer with the data to be copied
	\param	iSize
		the size of the extra field in the buffer
	\see OpenNewFile
*/
	void SetExtraField(const char *pBuf, WORD iSize);

/**
	Close the new file in the archive.
	\return \c false if there is no new file opened 
	\note Throws exceptions.
	\see OpenNewFile
*/
	bool CloseNewFile();

/**
	Extract the file from the archive.
	The argument \e lpszNewName may point to the full path and is influenced by \e bFullPath 
	argument (if \e lpszNewName contains drive specification then it is removed)
	\param	uIndex
		the index of the file to extract
	\param	lpszPath
		The PATH only to extract the file to. May not be NULL. If you wish to 
		use UNC path you need to replace \\\\ at the beginning of UNC path with \\\\?\UNC\ .
	\param	bFullPath <BR>
		- if \c true, then extract with the full path - in this case the resulting
		file path is \e lpszPath plus the path stored in the archive or plus \e lpszNewName 
		if \e lpszNewName is not NULL. <BR>
		  If the path of the file stored in the archive or \e lpszNewName contains a drive letter,
		it is removed.
		- if \c false, the destination file path is \e lpszPath + \e the filename only
		extracted from the path stored in the archive or from \e lpszNewName if
		\e lpszNewName is specified; <BR>
		if #m_szRootPath is set previously with #SetRootPath then to \e lpszPath
		is added the path stored in the archive (or \e lpszNewName if
		\e lpszNewName is specified) that has removed the beginning that equals 
		#m_szRootPath (if there is no common beginning then is behaves like 
		#m_szRootPath was empty)
	\param	lpszNewName 
			The new name of the file after extraction. 
			If NULL the original filename stored in the archive is used.
			May point to the full path but, if \e bFullPath is \c false, only the filename is extracted from this argument,
			if the \e bFullPath is \c true, the drive letter (if exists) is removed.
	\param	pCallback
		A callback function called after reading and writing a portion of data.
		See a note at \ref callbFas "CZipArchive::AddNewFile"
	\param	pUserData
		user - defined data passed on to the callback function
	\param	nBufSize 
		the size of the buffer used
	\return	\c true if successful
	\note Throws exceptions.
*/
	bool ExtractFile(WORD uIndex, LPCTSTR lpszPath, bool bFullPath = true, LPCTSTR lpszNewName = NULL, ZIPCALLBACKFUN pCallback = NULL, void* pUserData = NULL, DWORD nBufSize = 65535);

/**
	Open the file with the given index in the archive for extracting.
	Not successfull opening the file doesn't lock the whole archive, so 
	you can try to open another one (after catching an exception if it was 
	thrown).
	\param	uIndex
		the index of the file
	\return	\c true if successful
	\note Throws exceptions.
*/
	bool OpenFile(WORD uIndex);

/**
	Decompress currently opened file to the buffer.
	\param	pBuf
		buffer to receive data
	\param	iSize
		the size of the buffer
	\return	the number of bytes read
	\see OpenFile
	\note Throws exceptions.
*/
	DWORD ReadFile(void *pBuf, DWORD iSize);


/**
	Test the file with the given index for the integrity.
 	The method throws exceptions but performs all the necessary cleanup
	before, so that the next file can be tested after catching the exception.
	\param	uIndex
		index of the file to test
	\param	pCallback
		\ref callbFas "callback function" (may be NULL) 
	\param	pUserData
		user - defined data passed on to the callback function
		(doesn't matter if there is no callback function defined)
	\param	uBufSize
		the size of the buffer used during extraction
	\return \c false if the incorrect action has been taken by 
	the user or the programmer (it is when #OpenFile or 
	#GetFileInfo returned \c false or \e uBufSize is 0. If the file is didn't 
	passed the test or there was a disk I/O error, an exception is thrown.
	\note Throws exceptions.
	
*/
	bool TestFile(WORD uIndex, ZIPCALLBACKFUN pCallback = NULL, void* pUserData = NULL, DWORD uBufSize = 65535);

/**
	Perform the necessary cleanup after an exception was thrown
	while testing the archive so that next files in the archive can be tested.
	Called by #TestFile. Does not remove the file headers
	information from the central directory.
	\see TestFile
	\see CZipCentralDir::Clear
*/
	void CloseFileAfterTestFailed();

/**
	Get the local extra filed of the currently opened 
	for extraction file in the archive.
	\param	pBuf
		the buffer to receive the data
	\param	iSize
		the size of the buffer
	\return	If \e pBuf is NULL or iSize is 0, returns the size of the local extra field.
	Returns -1 if there is no file opened for the extraction.
*/
	int GetLocalExtraField(char* pBuf, int iSize);

/**
	The same as CZipArchive::CloseFile(LPCTSTR), but additionally
	closes \e file.
	\param	file
		OPENED CZipFile structure of the extracted file
	\return	
	\note Throws exceptions.
	\see CZipArchive::CloseFile(LPCTSTR)
*/
	int CloseFile(CZipFile &file);


/**
	Close the file opened for extraction in the archive and copy its date and 
	attributes to the file pointed by \e lpszFilePath
	\param	lpszFilePath 
		Points to the path of the file to have the date and attributes information updated.
	\param bAfterException
		Set to \c true to close the file inside archive after an exception has been 
		thrown, to allow futher operations on the archive.
	\warning Close the file pointed by \e lpszFilePath before using this method, 
		because the system may not be able to retrieve information from it.
	\return	<BR>
	-  "1" = ok
	-  "-1" = some bytes left to uncompress - probably due to a bad password
	-  "-2" = setting extracted file date and attributes was not successful	
	\note Throws exceptions.
*/
	int CloseFile(LPCTSTR lpszFilePath = NULL, bool bAfterException = false);

/**
	Delete the file from the archive with the given index. 

	\param	uIndex
		a zero-based index
	\return	\c false if:
		- \e uIndex is not a valid index
		- there is no file opened
		- the archive is an existing disk spanning archive (you cannot modify
		an existing disk spanning archive)
	\note Throws exceptions.
	\see AddNewFile
*/
	bool DeleteFile(WORD uIndex);

/**
	Delete files from the archive.
	Sort \e aIndexes array in an ascending order.
	\param	aIndexes
		an array of zero-based indexes of the files inside the archive	
	\note Throws exceptions.
*/
	void DeleteFiles(CZipWordArray &aIndexes);


/**
	Delete files from the archive.
	\param	&aNames
		an array of filenames inside the archive; they must be the 
		same as they appear in the archive (the name and the path - if persists - 
		is required)
	\param	bCaseSensitive
		if \c true, perform a case sensitive comparison
	\note Throws exceptions.
*/
	void DeleteFiles(const CZipStringArray &aNames, bool bCaseSensitive = false);


/**
	Set the global comment in the archive.
	\param	lpszComment
		the file comment		
	\return \c false if the archive is closed or if it is an existing disk spanning archive
	\note Throws exceptions.
*/
	bool SetGlobalComment(LPCTSTR lpszComment);


/**
	\return the global comment or an empty string if the archive is closed	
*/
	CZipString GetGlobalComment();


/**
	Set the comment of the file with the given index inside the archive.
	\param	uIndex
		zero-based index of the file in the archive
	\param	lpszComment
		a comment to add
	\return	\c false if the comment change is impossible
	\note Throws exceptions.
*/
	bool SetFileComment(WORD uIndex, LPCTSTR lpszComment);

/**
	\return the path of the currently opened archive volume	
*/
	CZipString GetArchivePath();

/**
	\return	<BR>
	- a one-based number of the current disk
	- 0 if there is no current disk (the archive is closed)
*/
	int GetCurrentDisk();

/**
	Return the disk spanning mode of the current archive.

	Disk spanning archive modes:
	
	\anchor PKSpan
	PKZIP compatible mode (pkzipSpan):
		- only on removable devices
		- auto-detect the size of the volume
		- write a label to the disk
		- there is a need to set the span callback function

	\anchor TDSpan
	TD mode (tdSpan):
		- may be created on non removable devices
		- uses user-defined volume size
		- no need to set the span callback function

	\return	<BR>
	- -2 - exisiting TD mode compatible disk spanning archive
	- -1 - exisiting PKZIP compatible 
	- 0 - no disk spanning
	- 1 - PKZIP compatible in creation
	- 2 - TD compatible in creation

  This two disk spanning modes create volumes with compatible internal 
  structure. It means that you can easily convert the volumes created 
  in one mode to the other one by renaming the files 
  (in TD mode each volume but last has a number as an extension). 
  To convert the archive from TD to PKZIP compatible archive, 
  Copy each file to the removable media, giving them the extension.
  ".zip". You should also label each disk with the appropriate label 
  starting from "pkback# 001".

*/
	int GetSpanMode();


/**
	Find the file in the archive.
	If the archive wasn't opened with #m_bConvertAfterOpen set to \c true, 
	this function automatically convert all the filenames with the function
	CZipCentralDir::ConvertAll and set #m_bConvertAfterOpen to \c true.
	This function requires \link CZipCentralDir::m_bFindFastEnabled FindFast \endlink
	feature enabled.
	\param	lpszFileName
		the name of the file to be found
	\param	bCaseSensitive
		if \c true - perform a case sensitive search
	\return	<BR>
	- the index of the file found 
	- -1 if there is no such a file in the archive

	\see CZipCentralDir::FindFileNameIndex
	\see EnableFindFast
	\see CZipCentralDir::ConvertAll
	\see SetConvertAfterOpen
*/
	int FindFile(LPCTSTR lpszFileName, bool bCaseSensitive);

/**
	Get the info of the file with the given index.
	\param	fhInfo
		the structure to receive info			
	\param	uIndex
		a zero-based index of the file inside the archive
	\return	\c true if successful
*/
	bool GetFileInfo(CZipFileHeader & fhInfo, WORD uIndex);


/**
	\return	the number of files in the archive
*/
	int GetNoEntries();

/**
	Close the archive.
	\param	bAfterException
		set it to \c true if you want to close and reuse CZipArchive after is has thrown an exception
		( this method doesn't write any data to the file then but only makes some cleaning)
	\note Throws exceptions if \e bAfterException is \c false
*/
	void Close(bool bAfterException = false);


/**
	Test if the archive is closed (a whole or the current volume only)
	\param	bArchive <BR>
		- \c true: test for the whole archive
		- \c false: test for the volume file only		

	\return	\c true if closed
*/
	bool IsClosed(bool bArchive = true);



/**
	Return the system compatibility of the current archive.
	System compatibility value for the single file in the archive
	(represented by CZipFileHeader) influences file attributes conversion
	(the file attributes are defined differently across the platforms).
	When opening an existing archive CZipArchive assumes the system compatibility
	of the whole archive to be the same as of the first file in the archive
	(if present). In other cases the current system value is assumed which is
	taken from ZipPlatform::GetSystemID during creating or opening an archive
	\remark
	If the existing archive during opening is empty, ZipPlatform::GetSystemID
	is assumed to be the default system for the files that will be added to the archive.
		
	\return	
		one of the enum values defined in \link ZipCompatibility::ZipPlatforms
		ZipCompatibility.h \endlink
	\see ZipCompatibility::ZipPlatforms
	\see ZipPlatform::GetSystemID
	\see CZipFileHeader::GetSystemCompatibility
*/
	int GetSystemCompatibility() {return m_iArchiveSystCompatib;}

/**
	Set the system compatibility of the archive. By default it is set to the 
	current system value (the one returned by ZipPlatform::GetSystemID() function).
	Use it after opening the archive, but before adding a new file or using 
	SetFileHeaderAttr() function
	\param iSystemComp
		can be one of ZipCompatibility::ZipPlatforms values
	\return
		return \c false if the value \e iSystemComp is not supported 
		(ZipCompatibility::IsPlatformSupported returns \c false for the value)
		or it is not possible to set it right now
*/
	bool SetSystemCompatibility(int iSystemComp);

/**
	Set the attributes for CZipFileHeader structure to be used
	in #OpenNewFile method as an argument.
	This special procedure is taken, because the system compatibility must
	be set for CZipFileHeader prior to the value, which must be identical to
	the return value of #GetSystemCompatibility method.
	\param	header
		the structure to have attributes set
	\param	uAttr
		attributes to set
	\note Throws exceptions if the archive system or the current system 
	is not supported by the ZipArchive library.
	\see GetSystemCompatibility
*/
	void SetFileHeaderAttr(CZipFileHeader& header, DWORD uAttr);


/**
	A helper for a various purposes.
	\return	the pointer to the static CRC table in the zlib library
	
*/
	static const DWORD* GetCRCTable();

/**
	Return the underlying archive storage medium.
	\warning A method for a very advanced use - you normally never need it.
	\return	the pointer to #m_storage
	\see CZipStorage
*/
	CZipStorage* GetStorage(){return &m_storage;}


/**
	Set #m_bDetectZlibMemoryLeaks value.
	\param	bDetect
	\note Use before opening a file in the archive.
	\see m_bDetectZlibMemoryLeaks
		
*/
	void SetDetectZlibMemoryLeaks(bool bDetect)
	{
		if (m_iFileOpened != nothing)
		{
			TRACE(_T("Set it before opening a file in the archive"));
			return;
		}
		m_bDetectZlibMemoryLeaks = bDetect;
		
	}

/**
	Set CZipCentralDir::m_bConvertAfterOpen value.
	\param	bConvertAfterOpen
	\note Use before opening the archive.
	\see CZipCentralDir::m_bConvertAfterOpen		
*/
	void SetConvertAfterOpen (bool bConvertAfterOpen)
	{
		if (!IsClosed())
		{
			TRACE(_T("Set it before opening the archive"));
			return;
		}
		m_centralDir.m_bConvertAfterOpen = bConvertAfterOpen;

	}

/**
	Enable fast finding by the file name of the files inside the archive.
	Set CZipCentralDir::m_bFindFastEnabled to \c true, which is required by #FindFile.
	\note call after opening the archive.
	\param	bEnable
	\see CZipCentralDir::m_bFindFastEnabled
	\see FindFile
*/
	void EnableFindFast(bool bEnable = true);
protected:
	/**
		Internal data.
		\see CZipInternalInfo 
	*/
	CZipInternalInfo m_info;

	
	/**
		Physical layer of the archive.
		\see CZipStorage
	*/
	CZipStorage m_storage;

	/**
		A central directory object.
		\see CZipCentralDir
	*/
	CZipCentralDir m_centralDir;

	/**
		The open mode of the current file inside archive.	
	*/
	enum ZipOpenFileType
	{
		extract = -1,	///< current file opened for extraction
		nothing,		///< no file inside archive opened
		compress		///< a new file opened for compression
	};
	
	/**
		Takes one of the #ZipOpenFileType enum values.
	*/
	char m_iFileOpened;

/**
	The root path to be omitted in #AddNewFile and #ExtractFile functions
	from the beginning of the full file path. Set by #SetRootPath
	\see TrimRootPath	
	\see SetRootPath
*/
	CZipString m_szRootPath;

/**
	Open the archive in the given mode.
	Called by #Open(LPCTSTR, int, int) and #Open(CZipMemFile&, int).
	\param	iMode
		an opening mode		
	\note Throws exceptions.
*/
	void OpenInternal(int iMode);

	/**
		The system code of the current archive. All new files in the archive 
		will be created regarding this value. Can be one of the enum values 
		defined in \link ZipCompatibility::ZipPlatforms	ZipCompatibility.h
		\endlink

		\see GetSystemCompatibility
	*/
	int m_iArchiveSystCompatib;

/**
	Free the memory allocated by the zlib library that hasn't been freed
	due to an error in the zlib library (usually never happens).
*/
	void EmptyPtrList();


/**
	
	\internal
	\param	uIndex
	\note Throws exceptions.
	\todo eliminate unnecessary copying when removing more than one file.
*/
	void DeleteInternal(WORD uIndex);

/**
	Remove the range of data from the archive specified by the \e uStartOffset
	and \e uEndOffset
	\param	uStartOffset
		
	\param	uEndOffset
		
	\return	the count of removed bytes
	
*/
	DWORD RemovePackedFile(DWORD uStartOffset, DWORD uEndOffset);
/**
	\return	 the currently opened for compression or decompression
	file inside the archive NULL if there is no file opened
*/
	CZipFileHeader* CurrentFile();

/**
	If the parameter \e iErr signalizes a zlib library error, throw CZipException
	\param	iErr
		a zlib library error to check
	\note Throws exceptions.
*/
	void CheckForError(int iErr);

/**
	Throw a CZipException error.
	\param	err
		the error code
	\see CZipException::ZipErrors
	\param	bZlib
		if \c true, treat \e err as a zlib error code and perform the conversion to the one of CZipException codes.
	\see CZipException::Throw
*/
	void ThrowError(int err, bool bZlib = false);


/**
	Function used in cojunction with #m_szRootPath to trim paths in #AddNewFile and #ExtractFile
		\param	zpc
		\see SetRootPath

*/
	CZipString TrimRootPath(CZipPathComponent& zpc);

	typedef CZipPtrList<void*>::iterator CZipPtrListIter;
	CZipPtrList<void*> m_list; ///< a list holding pointers to the memory areas allocated by the zlib library


	static void* _zliballoc(void* opaque, UINT items, UINT size); ///< memory allocator called by the zlib library
	static void _zlibfree(void* opaque, void* address); ///< memory deallocator called by the zlib library

	
	/**
		Specify whether to control memory allocation and freeing by the zlib library.
		It is strongly suggested to set it to \c true.

		\b Default: \c true
		\note Set it before opening a file (new or existing) in the archive.
		\see SetDetectZlibMemoryLeaks
		
	*/
	bool m_bDetectZlibMemoryLeaks;



	/**
		Copyright string.
	*/
	static const TCHAR m_gszCopyright[];
	/**
		\name Crypthography
		Methods performing data encryption and decryption
		and attributes used by them.
	*/
	//@{
/**
	Decode \e nCount bytes from the internal buffer.
	\see m_info
	\see CZipInternalInfo::m_pBuffer
	\param	uCount
*/
	void CryptDecodeBuffer(DWORD uCount);
/**
	Encode the internal buffer.
	\see m_info
	\see CZipInternalInfo::m_pBuffer
*/
	void CryptEncodeBuffer();

/**
	Encode the character \e c and update \link #m_keys encryption keys \endlink.
	\param	&c
*/
	void CryptEncode(char &c);
/**
	Create an encryption header for the new file in the archive.
	\param	iCrc
		A control value. Use the two lower bytes of CZipFileHeader::m_uModTime.
		This entails the need for a data description presence at the end of 
		the compressed data. We cannot use the real CRC now, because we don't know
		it yet.
	\param	buf
		a buffer to receive the header
	\see CryptCheck
*/
	void CryptCryptHeader(long iCrc, CZipAutoBuffer& buf);
/**
	\internal
	\param	l		
	\param	c
	\return	
*/
	DWORD CryptCRC32(DWORD l, char c);
/**
	Decode the character \e c and update \link #m_keys encryption keys \endlink.
	\param	&c
*/
	void CryptDecode(char &c);
	/**
		\internal
	*/
	char CryptDecryptByte();

/**
	Decrypt the encryption header and check its control value.
	The control value depends on the presence of the data descriptor.
	\return	\c true if the control value is correct
	\see CryptCryptHeader
*/
	bool CryptCheck();
/**
	Update \link #m_keys encryption keys \endlink with the given value.
	\param	c
			
*/
	void CryptUpdateKeys(char c);
/**
	Initialize \link #m_keys encryption keys \endlink.
*/
	void CryptInitKeys();
	/**
		The archive password. If empty, the new file will not be encrypted.
	*/
	CZipAutoBuffer m_pszPassword;

	/**
		Encryption keys.
		The key values are initialized using a supplied encryption password.
		\see CryptInitKeys
	*/
	DWORD m_keys[3];

	//@}


};

#endif // !defined(AFX_ZIPARCHIVE_H__A7F528A6_1872_4071_BE66_D56CC2DDE0E6__INCLUDED_)
