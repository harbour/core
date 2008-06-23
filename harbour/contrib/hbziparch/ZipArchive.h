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
// Check the site http://www.artpol-software.com for the updated version of the library.
//   
//	The following information files are distributed along with this library:
//		License.txt		- licensing information
//		Appnote.txt		- details on the zip format
//							( also available at ftp://ftp.pkware.com/appnote.zip)
//
 


/**
* \file ZipArchive.h
*	Includes the CZipArchive class.
*
*/

#if !defined(ZIPARCHIVE_ZIPARCHIVE_DOT_H)
#define ZIPARCHIVE_ZIPARCHIVE_DOT_H

#if (_MSC_VER > 1000) && (defined ZIP_HAS_DLL)
	#pragma warning (push)
	#pragma warning( disable : 4251 ) // needs to have dll-interface to be used by clients of class
	#pragma warning( disable : 4275 ) // non dll-interface class used as base for dll-interface
#endif

#include "_features.h"
#include "ZipException.h"
#include "ZipAutoBuffer.h"
#include "ZipCentralDir.h"	
#include "ZipStorage.h"
#include "ZipPathComponent.h"
#include "ZipString.h"
#include "ZipExport.h"
#include "ZipCryptograph.h"
#include "FileFilter.h"
#include "DirEnumerator.h"
#include "ZipCompressor.h"
#include "ZipCallbackProvider.h"

#define ZIP_AUTODETECT_VOLUME_SIZE ZIP_SIZE_TYPE(-1)

/**
	The structure used as a parameter in CZipArchive::AddNewFile(CZipAddNewFileInfo& ).
	Use one of the provided constructors and then adjust the member variables as needed.

	\see
		<a href="kb">0610231446|simple</a>
*/
struct ZIP_API CZipAddNewFileInfo
{
	/**
		Initializes a new instance of the CZipAddNewFileInfo class.
		
		\param lpszFilePath
			Sets #m_szFilePath.
		
		\param bFullPath
			Sets #m_bFullPath.
	 */
	CZipAddNewFileInfo(LPCTSTR lpszFilePath, bool bFullPath = true)
		: m_szFilePath(lpszFilePath),m_bFullPath(bFullPath)
	{
		m_pFile = NULL;
		Defaults();
	}

	/**
		Initializes a new instance of the CZipAddNewFileInfo class.
		
		\param lpszFilePath
			Sets #m_szFilePath.
		
		\param lpszFileNameInZip
			Sets #m_szFileNameInZip.
	 */
	CZipAddNewFileInfo(LPCTSTR lpszFilePath, LPCTSTR lpszFileNameInZip)
		: m_szFilePath(lpszFilePath), m_szFileNameInZip(lpszFileNameInZip)
	{
		m_pFile = NULL;
		Defaults();
	}

	/**
		Initializes a new instance of the CZipAddNewFileInfo class.
		
		\param pFile
			Sets #m_pFile.
		
		\param lpszFileNameInZip
			Sets #m_szFileNameInZip.
	 */
	CZipAddNewFileInfo(CZipAbstractFile* pFile, LPCTSTR lpszFileNameInZip)
		: m_pFile(pFile), m_szFileNameInZip(lpszFileNameInZip)
	{
		Defaults();
	}

	/**			
		Initialize this field to set the source data for compression to be taken from 
		the \c CZipAbstractFile object (such as \c CZipMemFile) instead of from a physical file.
		
		 \note 
		  - You have to leave #m_szFilePath empty if you set #m_pFile to not \c NULL.
		  - The time of a file in zip will be set to the current time, and the attributes to the default
		  file attributes (depending on the system).
		  - You cannot add directories this way.
	*/
	CZipAbstractFile* m_pFile;

	/**
		The full path to the file to be added. If it is empty, you need to initialize #m_pFile.
		If #m_bFullPath is \c true and the path contains a drive letter, the drive letter is removed unless
		CZipArchive::m_bRemoveDriveLetter is set to \c false.
	*/
	CZipString m_szFilePath;		

	/**
		The file name that will be stored in the archive. If the file is a directory,
		there will be a path separator automatically appended. The CZipArchive::SetRootPath
		method has no effect on this parameter.
	*/
	CZipString m_szFileNameInZip;
	
	/**
		It has only the meaning when CZipAddNewFileInfo::m_szFileNameInZip is not specified and 
		CZipAddNewFileInfo::m_szFilePath is not empty. 

		- If set to \c true, instructs to store the full path of the file inside the archive,
		even if CZipArchive::m_szRootPath is set.
		- If set to \c false only a filename without a path is stored in the archive. 
		In this case, if CZipArchive::m_szRootPath is set previously with 
		CZipArchive::SetRootPath, and if the beginning of #m_szFilePath equals 
		CZipArchive::m_szRootPath, then the filename is set to the not matched part 
		of #m_szFilePath (you could say to #m_szFilePath minus CZipArchive::m_szRootPath).
	*/
	bool m_bFullPath;

	/**
		The level of compression. You can use values from 0 to 9 and -1 (meaning the default compression)
		or one of the CZipCompressor::CompressionLevel values.
	*/
	int m_iComprLevel;				

	/**
		The smartness level of the file adding process. Can be one or combined CZipArchive::Smartness
		values (you can use the logical \c OR).
	*/
	int m_iSmartLevel;

	/**
		The index of an existing file in the archive to be replaced by the file being added.
		See <a href="kb">0610231944|replace</a> for more information.
		The meaning of its values is as follows:

		- >= 0 : the index of the file to be replaced.
		- \c ZIP_FILE_INDEX_UNSPECIFIED : do not replace any file and add the new file at 
		the end of the archive (default). Use this value in segmented archives.

		\note
			- If you use an invalid index, the action will fail. 
			- If you specify the last file in the archive to be replaced, it'll be removed 
			and the usual action will be taken.

		\see CZipArchive::SetAdvanced
		\see CZipArchive::WillBeDuplicated
	*/
	ZIP_INDEX_TYPE m_uReplaceIndex;		

	/**
		The size of the buffer used while file operations.
	*/
	unsigned long m_nBufSize;		

	/**
		Sets default values for #m_iSmartLevel, #m_uReplaceIndex, #m_nBufSize and #m_iComprLevel.
		Examine the source code for the current values.
	*/
	void Defaults();
};



/// <summary>
/// Represents a zip archive file.	
/// </summary>
class ZIP_API CZipArchive  
{	
public:
	static const char m_gszCopyright[];
	static const char m_gszVersion[];
		
	CZipArchive();

	virtual ~CZipArchive();
	
	/** 
		Sets a password for the file to be opened or created.
		Use this method before opening or adding a file, but after opening the archive.
		The password is cleared during opening the archive. For the encryption to work,
		encryption method must be set to other value than CZipCryptograph::encNone.

		\param	lpszPassword
			The password. Set it to \c NULL or an empty string to clear the password.

		\return
			\c false if the password cannot be changed at this time; \c true otherwise.

		\see
			<a href="kb">0610201627</a>
		\see
			SetEncryptionMethod
		\see 
			WillEncryptNextFile
	*/
	bool SetPassword(LPCTSTR lpszPassword = NULL);	

	/**
		Gets the current archive password or an empty string if there is no password set. 
		\return
			The current password.
		\see
			SetPassword
	*/
	CZipString GetPassword()const ;

	/**
		Returns the value indicating whether the next file will be encrypted or not. To encrypt a file, 
		a password must be set with the #SetPassword method and an encryption method must be set 
		to a value different from CZipCryptograph::encNone.
		\return
			\c true, if the next file added to the archive will be encrypted; \c false otherwise.
		\see
			<a href="kb">0610201627</a>
		\see
			SetEncryptionMethod
		\see
			GetEncryptionMethod
		\see
			SetPassword
		\see
			GetPassword
	*/	
	bool WillEncryptNextFile() const
	{
		return m_pszPassword.GetSize() != 0 && m_iEncryptionMethod != CZipCryptograph::encNone;
	}

	/**
		Sets the encryption method when encrypting files.
		You can encrypt different files using different methods.
		You need to set the password with the #SetPassword method for the encryption to work.
		It is important to set the encryption method only when compressing. When decompressing, 
		the encryption method will be detected automatically.
		
		\param iEncryptionMethod
			One of the CZipCryptograph::EncryptionMethod values.
		
		\return
			\c false if selected encryption method is not supported or a 
			file is opened for compression; \c true otherwise.

		\see
			<a href="kb">0610201627</a>
		\see
			SetPassword
		\see
			CZipCryptograph::EncryptionMethod
		\see
			GetEncryptionMethod	
		\see 
			WillEncryptNextFile
	 */
	bool SetEncryptionMethod(int iEncryptionMethod = CZipCryptograph::encStandard);

	/**	
		Gets the current encryption method.

		\return
			One of CZipCryptograph::EncryptionMethod values.

		\see
			SetEncryptionMethod		
		\see 
			WillEncryptNextFile
	 */
	int GetEncryptionMethod() const
	{
		return m_iEncryptionMethod;
	}

	/**
		Encrypts an existing file with a given index using the current encryption method.

		\param uIndex
			The index of the file to encrypt.

		\return 
			\c false, if the file could not be encrypted; \c true otherwise.

		\note
			- The method will not encrypt a file, if it already is encrypted.
			- Calls the CZipActionCallback::cbEncryptPrepare, CZipActionCallback::cbMultiEncrypt, 
			CZipActionCallback::cbEncryptMoveData and CZipActionCallback::cbEncrypt callbacks.
			- Throws exceptions.

		\see
			<a href="kb">0610201627|existing</a>
		\see
			SetEncryptionMethod
		\see
			SetPassword
		\see 
			WillEncryptNextFile
		\see
			EncryptFiles
		\see
			EncryptAllFiles		
			
	*/
	bool EncryptFile(ZIP_INDEX_TYPE uIndex)
	{
		CZipIndexesArray aIndexes;
		aIndexes.Add(uIndex);
		return EncryptFilesInternal(&aIndexes);
	}
	
	/**
		Encrypts existing files with given indexes using the current encryption method.

		\param aIndexes
			The indexes of files to encrypt.

		\return 
			\c false, if the files could not be encrypted; \c true otherwise.

		\note
			- The method will not encrypt files that are already encrypted.
			- Calls the CZipActionCallback::cbEncryptPrepare, CZipActionCallback::cbMultiEncrypt, 
			CZipActionCallback::cbEncryptMoveData and CZipActionCallback::cbEncrypt callbacks.
			- Throws exceptions.

		\see
			<a href="kb">0610201627|existing</a>
		\see
			SetEncryptionMethod
		\see
			SetPassword
		\see 
			WillEncryptNextFile
		\see
			EncryptFile
		\see
			EncryptAllFiles		
			
	*/
	bool EncryptFiles(CZipIndexesArray &aIndexes)
	{
		return EncryptFilesInternal(&aIndexes);
	}

	/**
		Encrypts all existing files in the archive using the current encryption method.

		\return 
			\c false, if the files could not be encrypted; \c true otherwise.

		\note
			- The method will not encrypt files that are already encrypted.
			- Calls the CZipActionCallback::cbEncryptPrepare, CZipActionCallback::cbMultiEncrypt, 
			CZipActionCallback::cbEncryptMoveData and CZipActionCallback::cbEncrypt callbacks.
			- Throws exceptions.

		\see
			<a href="kb">0610201627|existing</a>
		\see
			SetEncryptionMethod
		\see
			SetPassword
		\see 
			WillEncryptNextFile
		\see
			EncryptFile
		\see
			EncryptFiles		
			
	*/
	bool EncryptAllFiles()
	{
		return EncryptFilesInternal(NULL);
	}

	/**
		Sets the compression method used when compressing file. If affects the files that are  
		added to the archive after calling this method.

		\param uCompressionMethod
			The compression method to use. Valid values are CZipCompressor::methodDeflate and CZipCompressor::methodBzip2.
			To store files (without compression), use the CZipCompressor::levelStore compression level 
			when adding files to the archive.

		\return
			\c true, if the compression method is supported by the ZipArchive Library and was successfully set;
			\c false otherwise.

		\note
			The compression method used for extraction is automatically determined from the information 
			written in the archive.

		\see
			<a href="kb">0610231446|methods</a>
		\see
			CZipCompressor::IsCompressionSupported
		\see 
			GetCompressionMethod
		\see
			GetCurrentCompressor

	*/
	bool SetCompressionMethod(WORD uCompressionMethod = CZipCompressor::methodDeflate);

	/**
		Returns the current compression method used when adding files to the archive.

		\return
			The current compression method. Is one of the CZipCompressor::CompressionMethod values.

		\see
			SetCompressionMethod
	*/
	WORD GetCompressionMethod() const
	{
		return m_uCompressionMethod;
	}

	/**
		Sets the compression options for an appropriate compressor. The library automatically detects to
		which compressor the options apply by examining the return value from the CZipCompressor::COptions::GetType() method.
		If a file is currently opened for compression or decompression, the options will have no effect on the current file
		processing. A sample that illustrates setting options can be found at <a href="kb">0610231446|options</a>.

		\param pOptions 
			The options to set. The \c NULL value has no effect.
			The object is no longer needed and can be safely released after this method returns.

		\see
			<a href="kb">0610231446|options</a>
		\see
			GetCurrentCompressor
		\see
			CZipCompressor::GetOptions
		\see
			SetAdvanced
	*/
	void SetCompressionOptions(CZipCompressor::COptions* pOptions)
	{
		if (m_iFileOpened)
		{
			ZIPTRACE("%s(%i) : The options will have no effect on the current file processing.\n");
		}
		m_compressorsOptions.Set(pOptions);
	}

	/**
		Sets the internal buffer sizes. No buffer size can be set smaller than 1024.
		Use this method before opening the archive. The optimal size for 
		the write buffer for a segmented archive is the size of the volume.

		\param iWriteBuffer
			The write buffer size. See also CZipStorage::m_iWriteBufferSize.

		\param iGeneralBuffer
			A helper buffer used in moving data, deleting, getting (#GetFromArchive)
			files, renaming and replacing. This buffer is not used for
			compression and decompression. For these purposes, use the CZipCompressor::COptions::m_iBufferSize option.

		\param iSearchBuffer
			A buffer used in searching for the central directory. See also CZipStorage::m_iLocateBufferSize.
			
		\see
			GetAdvanced
		\see
			SetCompressionOptions
	*/
	void SetAdvanced(int iWriteBuffer = 65536, int iGeneralBuffer = 65536, int iSearchBuffer = 32768);

	
	/**
		Retrieves buffer sizes as set with the SetAdvanced() method.
	
		\param piWriteBuffer
				
		\param piGeneralBuffer
	
		\param piSearchBuffer

		\see
			SetAdvanced
	 */
	void GetAdvanced(int* piWriteBuffer = NULL, int* piGeneralBuffer = NULL, int* piSearchBuffer= NULL)
	{
		if (piWriteBuffer)
			*piWriteBuffer = 	m_storage.m_iWriteBufferSize;
		if (piGeneralBuffer)
			*piGeneralBuffer = m_iBufferSize;
		if (piSearchBuffer)
			*piSearchBuffer = m_storage.m_iLocateBufferSize;
	}


    /**
		Registers the callback object to receive specified notifications.		

		\param pCallback
			The callback object to receive notifications.
			Set it to \c NULL to stop receiving the selected notifications.

		\param iWhich
			The callback type to register for (or unregister from). Can be one or more of the CZipActionCallback::CallbackType values.

		\see
			<a href="kb">0610231200</a>
		\see
			CZipActionCallback
		\see
			CZipActionCallback::CallbackType
		\see
			GetCallback
		
     */
	void SetCallback(CZipActionCallback* pCallback = NULL, int iWhich = CZipActionCallback::cbAll)
	{
		m_callbacks.Set(pCallback, iWhich);
	}

	/**
		Gets the callback object registered for a given notification.
		\param iWhich
			The callback type. Can be one or more of CZipActionCallback::CallbackType values.
		\return
			The callback object set previously with #SetCallback.
		\see
			<a href="kb">0610231200</a>
		\see
			SetCallback
		\see
			CZipActionCallback
		\see
			CZipActionCallback::CallbackType
	*/
	CZipActionCallback* GetCallback(CZipActionCallback::CallbackType iWhich)
	{
		return m_callbacks.Get(iWhich);
	}

	/**
		The type of the callback object used during changing volumes in a segmented archive.

		\see
			CZipArchive::SetSegmCallback
	*/
	enum SegmCallbackType
	{
		scSpan	= 0x01,				///< The callback object will be set for operations on spanned archives only.
		scSplit	= 0x02,				///< The callback object will be set for operations on split archives only.
		scAll	= scSpan | scSplit	///< The callback object will be set for operations spanned and split archives.
	};

	/**
		Sets the callback object used during operations on a segmented archive to change volumes in spanned and split archives.
		Set it before opening the archive. If you open a spanned archive and don't set the callback object, 
		the exception CZipException::noCallback will be thrown. Setting the callback object is not required for 
		a split archive.

		Callback object's method CZipSegmCallback::Callback is called when the next volume is processed in a segmented archive.
		Calling CZipArchive methods from inside this method may result in an unexpected behavior.

		\param pCallback
			The address of the callback object. Set it to \c NULL to clear the callback.

		\param callbackType
			The type of the callback to set. It can be one of the #SegmCallbackType values.

		\see
			<a href="kb">0610051553</a>
		\see
			CZipSegmCallback
	*/
	void SetSegmCallback(CZipSegmCallback* pCallback = NULL, int callbackType = scSpan);

	
	/**
		Archive open modes used in the CZipArchive::Open(LPCTSTR, int, ZIP_SIZE_TYPE)
		and CZipArchive::Open(CZipAbstractFile&, int ) methods.
	*/
	enum OpenMode
	{
		zipOpen,			///< Opens an existing archive.
		zipOpenReadOnly,	///< Opens an existing archive as a read only file. This mode is intended to use in a self extract code, when opening an archive on a storage without the write access (e.g. CD-ROMS) or when sharing the central directory (see OpenFrom()). If you try to modify the archive in this mode, an exception will be thrown.
		zipCreate,			///< Creates a new archive.
		zipCreateSegm,		///< Creates a segmented archive.
		zipCreateAppend		///< Creates a new archive, but allows appending the archive to an existing file (which can be for example a self-extracting stub).
	};

	/**
		Opens or creates a zip archive.

		The archive creation mode depends on the \a iMode and \a uVolumesSize values:
		- If \a iMode == #zipCreateSegm and \a uVolumeSize is \c ZIP_AUTODETECT_VOLUME_SIZE then a spanned archive is created.
		- If \a iMode == #zipCreateSegm and \a uVolumeSize > 0 then a split archive is created.
		- If \a iMode == #zipOpen or \a iMode == #zipOpenReadOnly and the existing archive is a segmented archive
			then CZipStorage::spannedArchive mode is assumed if the archive is on a removable device
			or CZipStorage::splitArchive otherwise. If you want to open a split archive on a removable device, 
			set \a uVolumeSize to a value different from 0 (under Linux, the device is always assumed to be removable due to lack of 
			implementation of the ZipPlatform::IsDriveRemovable method).
		- If \a iMode == #zipCreate or a \a iMode == #zipCreateAppend then \a uVolumeSize doesn't matter.

		\param	szPathName
			The path to the archive.

		\param	iMode
			Can be one of the #OpenMode values.

		\param	uVolumeSize
			The volume size in a split archive. The size of the volume may be from 1 to 4,294,967,295.
		The bigger this value is - the faster is creation and extraction of segmented archives,
		because there are no volume changes.

		\return
			\c true, if the archive was opened successfully; \c false, if the archive was already opened before.
		
		\note
			Throws exceptions.

		\see
			<a href="kb">0610231446</a>
		\see
			<a href="kb">0610241003</a>
		\see
			<a href="kb">0610051553</a>
		\see
			Open(CZipAbstractFile&, int);
		\see
			GetSegmMode
	*/
	bool Open(LPCTSTR szPathName, int iMode = zipOpen, ZIP_SIZE_TYPE uVolumeSize = 0);


	/**
		Opens or creates an archive in memory. The CZipAbstractFile object is not closed
		after closing the archive, so that it is possible to work with it afterwards.

		\param	af
			\c CZipAbstractFile object to store the archive data.

		\param	iMode
			The open mode. The following values are valid:  #zipOpen, #zipOpenReadOnly, #zipCreate, #zipCreateAppend.
			If you use #zipCreate, the contents of the memory file are cleared. Use #zipCreateAppend, if you want to 
			append the archive at the end of the data contained in the memory file.

		\return
			\c true, if the archive was opened successfully; \c false, if the archive was already opened before or an 
			invalid open mode was specified.

		\note
			Throws exceptions.

		\see
			<a href="kb">0610231924</a>
		\see
			Open(LPCTSTR, int, ZIP_SIZE_TYPE);		
	*/	
	bool Open(CZipAbstractFile& af, int iMode = zipOpen);

	/**
		Opens the archive from the already opened archive. Both archives will share the same central directory.
		The \a zip archive must be opened in read-only mode and the newly opened archive will open as read-only as well.

		\param zip
			The archive that provides the reference to the central directory. It must be a read-only archive and it cannot be in memory.

		\return 
			\c true, if the new archive was successfully opened; \c false otherwise.

		\note
			If you use this method in a multithreaded environment, 
			make sure that \c ZIP_ARCHIVE_USE_LOCKING is defined in the \e _features.h file. Also make sure, that \a zip stays open until 
			this method returns.
		\note
			Throws exceptions.

		\see 
			<a href="kb">0610241003|thread</a>
		\see 
			zipOpenReadOnly

	*/
	bool OpenFrom(CZipArchive& zip);

	/**
		Sets the path fragment to be removed from the beginning of the full path when adding or extracting a file.

		Use it, if you don't want to use the full path (by setting the \a bFullPath parameter in #AddNewFile or #ExtractFile to \c true)
		and you don't want to remove the whole path neither (be setting the same parameter to \c false), but you want to remove only a specific beginning.
		Use it after opening the archive and before calling the #AddNewFile or #ExtractFile methods.

		\param	szPath
			The string that you want to be removed from the beginning of the path of the file
			in the archive. Set it to \c NULL stop removing the path beginning.

		\note
			Set the case-sensitivity with the #SetCaseSensitivity method.

		\see
			AddNewFile 
		\see
			ExtractFile
		\see
			GetRootPath

	*/
	void SetRootPath(LPCTSTR szPath = NULL);


    /**
		Gets the value set previously with the SetRootPath() method.

		\return
			The current value of the #m_szRootPath field.

		\see
			SetRootPath
     */
	CZipString GetRootPath()const 
	{
		return m_szRootPath;
	}

	/**
		The levels of smartness of the adding files action (CZipArchive::AddNewFile).
		\note If you use #zipsmCheckForEff, you should use 
		#zipsmNotCompSmall as well, because the small file will be surely larger 
		after compression, so that you can add it not compressed straight away. The compression level 
		is always ignored for a directory and set to 0.
	*/
	enum Smartness
	{
		zipsmLazy			= 0x0000,		///< All the smartness options turned off.
		zipsmCPassDir		= 0x0001,		///< Clears the password for directories.
		zipsmCPFile0		= 0x0002,		///< Clears the password for files of 0 size.

		/**
			Does not compress files smaller than 5 bytes - they are always stored larger than uncompressed.
		*/
		zipsmNotCompSmall	= 0x0004,

		/**
			Checks whether the compressed file is larger than uncompressed and if so, remove it and store 
			without the compression. In a segmented archive, a temporary file is used for that: 
			if the file compression is efficient, the data is not compressed again, but moved from 
			the temporary file to the archive. You can use the #SetTempPath method to set the path where the 
			file will be created or you can let the library figure it out (see #SetTempPath).
			If the the library could not find enough free space for the temporary file, 
			the compression goes the usual way.
		*/
		zipsmCheckForEff	= 0x0008,

		/**
			Combine it with #zipsmCheckForEff or use #zipsmCheckForEffInMem, the temporary file 
			will be created in memory then. This flag is effective also when replacing files 
			(see the note at CZipAddNewFileInfo::m_uReplaceIndex).
		*/
		zipsmMemoryFlag		= 0x0010,

		/**
			The same as #zipsmCheckForEff, but the temporary file is created created in memory instead.
			It has the meaning only in a segmented archive, not segmented archives don't need a temporary file.
		*/
		zipsmCheckForEffInMem = zipsmMemoryFlag | zipsmCheckForEff,
		zipsmSmartPass	= zipsmCPassDir | zipsmCPFile0,			///< The password policy (a combination of #zipsmCPassDir and #zipsmCPFile0).
		zipsmSmartAdd = zipsmNotCompSmall | zipsmCheckForEff,	///< Smart adding (a combination of #zipsmNotCompSmall and #zipsmCheckForEff).
		zipsmSafeSmart = zipsmSmartPass | zipsmNotCompSmall,    ///< Safe smart (smartest without checking for efficiency).
		zipsmSmartest = zipsmSmartPass | zipsmSmartAdd,			///< Smartness at its best.
		zipsmInternal01		= 0xf000   ///< Intended for the internal use only.
	};

	
	/**
		Adds a new file to the opened archive.

		\param info
			See CZipAddNewFileInfo for more information.

		\return	
			If \c false then the file was not added (in this case, you can still try to add other files); \c true otherwise.
		
		\note 
			- If you abort the operation from a callback object, while adding a file in a not segmented archive, the added data will be removed from the archive.
			- Throws exceptions. When an exception is thrown, you may need to call #CloseNewFile with \a bAfterException set to \c true, to make the archive reusable.
		
		\see
			<a href="kb">0610231446</a>
		\see
			<a href="kb">0610231200</a>
		\see
			<a href="kb">0610231924</a>
		\see
			AddNewFile(LPCTSTR, LPCTSTR, int, int, unsigned long)
		\see 
			AddNewFile(LPCTSTR, int, bool, int, unsigned long)
		\see 
			AddNewFile(CZipMemFile&, LPCTSTR, int, int, unsigned long)
		\see
			AddNewFiles(LPCTSTR, ZipArchiveLib::CFileFilter&, bool, int, bool, int, unsigned long)
		\see
			AddNewFiles(LPCTSTR, LPCTSTR, bool, int, bool, int, unsigned long)
		\see
			SetCallback
	*/
	bool AddNewFile(CZipAddNewFileInfo& info);


	/**
		Adds a new file to the opened archive.
		See the #AddNewFile(CZipAddNewFileInfo& ) method.
		The parameters are equivalent to CZipAddNewFileInfo member variables.
	
	*/
	bool AddNewFile(LPCTSTR lpszFilePath, int iComprLevel = -1, bool bFullPath = true,
		int iSmartLevel = zipsmSafeSmart, unsigned long nBufSize = 65536);


	/**
		Adds a new file to the opened archive.
		See the #AddNewFile(CZipAddNewFileInfo& ) method.
		The parameters are equivalent to CZipAddNewFileInfo member variables.
	*/
	bool AddNewFile(LPCTSTR lpszFilePath,
							 LPCTSTR lpszFileNameInZip,
                             int iComprLevel = -1,                             
							 int iSmartLevel = zipsmSafeSmart,
                             unsigned long nBufSize = 65536);

	/**
		Adds a new file to the opened archive.
		See the #AddNewFile(CZipAddNewFileInfo& ) method.
		The parameters are equivalent to CZipAddNewFileInfo member variables.
	*/
	bool AddNewFile(CZipMemFile& mf,
							 LPCTSTR lpszFileNameInZip,
                             int iComprLevel = -1,                             
							 int iSmartLevel = zipsmSafeSmart,
                             unsigned long nBufSize = 65536);


	/**
		Adds new files to the opened archive from the specified directory using a filter.

		\param lpszPath
			The root directory containing the files to add.

		\param filter
			A filter that determines which files should be added.

		\param bRecursive
			\c true, if the files from the subfolders of \a lpszPath should also be added; \c false otherwise.

		\param iComprLevel
			The level of compression. You can use values from 0 to 9 and -1 (meaning the default compression)
			or one of the CZipCompressor::CompressionLevel values.

		\param bSkipInitialPath
			\c true, if the \a lpszPath directory should be trimmed out of the paths stored in the archive;
			\c false otherwise.

		\param iSmartLevel
			The smartness level of the file adding process. Can be one or combined #Smartness
			values (you can use the logical \c OR).

		\param nBufSize
			The size of the buffer used while file operations.

		\return	
			If \c false then some files were probably not added (in this case, you can still try to add other files); \c true otherwise.

		\note
			Throws exceptions.

		\see
			<a href="kb">0610231446|filters</a>
		\see
			ZipArchiveLib::CFileFilter
		\see 
			ZipArchiveLib::CNameFileFilter
		\see 
			ZipArchiveLib::CGroupFileFilter
		\see
			AddNewFiles(LPCTSTR, LPCTSTR, bool, int, bool, int, unsigned long)
		\see 
			AddNewFile(CZipAddNewFileInfo&)
		\see
			AddNewFile(LPCTSTR, LPCTSTR, int, int, unsigned long)
		\see 
			AddNewFile(LPCTSTR, int, bool, int, unsigned long)
		\see 
			AddNewFile(CZipMemFile&, LPCTSTR, int, int, unsigned long)

	*/
	bool AddNewFiles(LPCTSTR lpszPath,
						ZipArchiveLib::CFileFilter& filter,
						bool bRecursive = true,
						int iComprLevel = -1,
						bool bSkipInitialPath = true,
						int iSmartLevel = zipsmSafeSmart,
						unsigned long nBufSize = 65536);

	/**
		Adds new files to the opened archive from the specified directory using a filename mask.

		\param lpszPath
			The root directory containing the files to add.

		\param lpszFileMask
			The filename mask to filter the files. Only files that has a filename matching this mask will
			be added to the archive. This method internally uses ZipArchiveLib::CNameFileFilter.

		\param bRecursive
			\c true, if the files from the subfolders of \a lpszPath should also be added; \c false otherwise.

		\param iComprLevel
			The level of compression. You can use values from 0 to 9 and -1 (meaning the default compression)
			or one of the CZipCompressor::CompressionLevel values.

		\param bSkipInitialPath
			\c true, if the \a lpszPath directory should be trimmed out of the paths stored in the archive;
			\c false otherwise.

		\param iSmartLevel
			The smartness level of the file adding process. Can be one or combined #Smartness
			values (you can use the logical \c OR).

		\param nBufSize
			The size of the buffer used while file operations.

		\return	
			If \c false then some files were probably not added (in this case, you can still try to add other files); \c true otherwise.

		\note
			Throws exceptions.

		\see
			name: <a href="kb">0610242025|wildcards</a>
		\see
			AddNewFiles(LPCTSTR, ZipArchiveLib::CFileFilter&, bool, int, bool, int, unsigned long)
		\see 
			AddNewFile(CZipAddNewFileInfo&)
		\see
			AddNewFile(LPCTSTR, LPCTSTR, int, int, unsigned long)
		\see 
			AddNewFile(LPCTSTR, int, bool, int, unsigned long)
		\see 
			AddNewFile(CZipMemFile&, LPCTSTR, int, int, unsigned long)
	*/
	bool AddNewFiles(LPCTSTR lpszPath, 
						LPCTSTR lpszFileMask = _T("*.*"), 
						bool bRecursive = true, 
						int iComprLevel = -1, 
						bool bSkipInitialPath = true, 
						int iSmartLevel = zipsmSafeSmart, 
						unsigned long nBufSize = 65536)
	{
		ZipArchiveLib::CNameFileFilter filter(lpszFileMask);
		return AddNewFiles(lpszPath, filter, bRecursive, iComprLevel, 
			bSkipInitialPath, iSmartLevel, nBufSize);
	}

	/**
		Adds a new file to the opened archive. The archive cannot be 
		an existing (at the moment of opening the archive) segmented archive,
		because modifying such an archive is not possible with this version.

		\param	header
			The structure that provides additional information about the added file and serves as a template for
			the properties of the new file. The following fields are used:
			\li CZipFileHeader::m_uModDate and CZipFileHeader::m_uModTime - the modification time. Use CZipFileHeader::SetTime to set them.
				If \a lpszFilePath is not \c NULL these fields are overwritten and updated automatically.
			\li CZipFileHeader::m_uExternalAttr - the attributes of the file. Use #SetFileHeaderAttr to set them.
				If \a lpszFilePath is not \c NULL this field is overwritten and updated automatically.
			\li \c CZipFileHeader::m_pszFileName - the filename (may be with a path) to be stored inside the archive
				to represent this file. Use CZipFileHeader::SetFileName to set it.
			\li CZipFileHeader::m_uLocalComprSize - predicted compressed size. Set it when using the Zip64 functionality and not using 
				encryption or segmentation - you may set it to the size of the file that you want to compress.
				This is for prediction if the Zip64 extensions are needed, but you should not worry too much about setting 
				this value, The ZipArchive Library will fix the headers if necessary - it just may save some processing.
			\li \c CZipFileHeader::m_pszComment - the file comment. Use CZipFileHeader::SetComment to set it.
			\li CZipFileHeader::m_aLocalExtraData - the local extra fields.
			\li CZipFileHeader::m_aCentralExtraData - the central extra fields.
		
		Other fields are ignored - they are updated automatically.
		If the method returns \c true, \link #SetSystemCompatibility 
		system compatibility \endlink for this object is set to the correct value.
		Additionally if \a lpszFilePath was not \c NULL, the attributes and 
		the time fields are filled with information retrieved from 
		the file pointed by \a lpszFilePath.

		\param	iLevel
			The level of compression. You can use values from 0 to 9 and -1 (meaning the default compression)
			or one of the CZipCompressor::CompressionLevel values.

		\param	lpszFilePath
			The path to the file to retrieve the date stamp and attributes from.
			These values are stored inside the archive.

		\return
			\c false in the following cases:
		\li The \a lpszFilePath is not \c NULL and the file attributes and data was not correctly retrieved.
		\li A file is already opened for extraction or compression.
		\li The archive is an existing segmented archive.
		\li The maximum file count inside the archive has already been reached
		(65,535 for a standard archive and \c 0xFFFFFFFFFFFFFFFF for an archive in the Zip64 format).

		\c true otherwise.

		\note
			Throws exceptions.

		\see
			<a href="kb">0610231446|advanced</a>
		\see
			<a href="kb">0610242300</a>
		\see 
			WriteNewFile
		\see 
			CloseNewFile
	*/
	bool OpenNewFile(CZipFileHeader& header, int iLevel = CZipCompressor::levelDefault, LPCTSTR lpszFilePath = NULL)
	{
		return OpenNewFile(header, iLevel, lpszFilePath, ZIP_FILE_INDEX_UNSPECIFIED);
	}

	/**
		Compresses the contents of the buffer and write it to a new file.

		\param	pBuf
			The buffer containing the data to be compressed and written.

		\param	uSize
			The number of bytes to be written from \a pBuf.

		\return
			\c false, if the new file hasn't been opened yet; \c true otherwise.

		\note
			Throws exceptions.

		\see
			<a href="kb">0610231446|advanced</a>
		\see
			OpenNewFile
		\see
			CloseNewFile
	*/
	bool WriteNewFile(const void *pBuf, DWORD uSize);

	/**
		Closes the new file in the archive.
		\return
			\c false, if there is no new file opened; \c true otherwise.

		\param bAfterException 
			If \c true, the new file will be closed without writing anything. Set it to \c true
			after an exception was thrown. Use it also this way if an exception other than 
			CZipException::abortedSafely was thrown from one of the #AddNewFile methods.

		\note
			Throws exceptions.

		\see
			<a href="kb">0610231446|advanced</a>
		\see
			OpenNewFile
		\see 
			WriteNewFile
	*/
	bool CloseNewFile(bool bAfterException = false);

	/**
		Acquires a file with the given index from another archive. 
		The compressed data of the file from another archive is copied to the current archive 
		without decompression.

		\param zip 
			The opened archive to get the file from (must not be a segmented archive).

		\param	uIndex
			A zero-based index of the file to get from the \a zip archive.

		\param lpszNewFileName
			The new filename to replace the old one from the \a zip archive. Can be \c NULL to leave the filename the same.

		\param uReplaceIndex
			The same as CZipAddNewFileInfo::m_uReplaceIndex. Can be \c ZIP_FILE_INDEX_UNSPECIFIED.
			
		\param bKeepSystComp
			If \c false, then the system compatibility of the file from the \a zip archive
			is converted to the current archive system compatibility, if they differ. Otherwise
			the source system compatibility is copied.

		\return
			\c false, if the operation could not be performed (either of archives is closed, 
		a file inside either of archives is opened, \a zip archive is segmented or the current
		archive is an existing segmented archive.

		\note 
			This method will encrypt data, if an encryption method and a password are set and the file is not already encrypted.

		\note
			Throws exceptions. When an exception is thrown, you may need to call #CloseNewFile with 
			\a bAfterException set to \c true, to make the archive reusable.

		\note
			It is safe to abort the action (by returning \c false from the CZipActionCallback::Callback) 
			in a not segmented archive and when no replacing is taking place. The file that is not 
			entirely added is removed from the archive then.
		
		\see
			<a href="kb">0610231446|get</a>
		\see
			<a href="kb">0610231200</a>
		\see
			GetFromArchive(CZipArchive&, CZipIndexesArray&, bool)
		\see
			GetFromArchive(CZipArchive&, CZipStringArray&, bool)		
		\see
			SetCallback
		\see
			SetAdvanced 
		\see 
			SetEncryptionMethod
		\see 
			SetPassword
	*/
	bool GetFromArchive(CZipArchive& zip, ZIP_INDEX_TYPE uIndex, LPCTSTR lpszNewFileName = NULL, ZIP_INDEX_TYPE uReplaceIndex = ZIP_FILE_INDEX_UNSPECIFIED, bool bKeepSystComp = false)
	{				
		InitBuffer();
		bool bRet;
		try
		{
			bRet = GetFromArchive(zip, uIndex, lpszNewFileName, uReplaceIndex, bKeepSystComp, GetCallback(CZipActionCallback::cbGet));
		}
		catch(...)
		{
			ReleaseBuffer();
			throw;
		}
		ReleaseBuffer();
		if (bRet && m_bAutoFlush)
			Flush();

		return bRet;
	}

	/**
		Acquires files with the given indexes from another archive. 
		The compressed data of the file from another archive is copied to the current archive 
		without decompression.

		\param zip 
			The opened archive to get the file from (must not be a segmented archive).

		\param	aIndexes
			An array of zero-based indexes of the files to acquire from the \a zip archive.

		\param bKeepSystComp
			If \c false, then the system compatibility of the file from the \a zip archive
			is converted to the current archive system compatibility, if they differ. Otherwise
			the source system compatibility is copied.

		\return
			\c false, if the operation could not be performed (either of archives is closed, 
		a file inside either of archives is opened, \a zip archive is segmented or the current
		archive is an existing segmented archive.

		\note 
			This method will encrypt data, if an encryption method and a password are set and the file is not already encrypted.

		\note
			Throws exceptions. When an exception is thrown, you may need to call #CloseNewFile with 
			\a bAfterException set to \c true, to make the archive reusable.

		\note
			It is safe to abort the action (by returning \c false from the CZipActionCallback::Callback) 
			in a not segmented archive and when no replacing is taking place. The file that is not 
			entirely added is removed from the archive then.
		
		\see
			<a href="kb">0610231446|get</a>
		\see
			<a href="kb">0610231200</a>
		\see
			GetFromArchive(CZipArchive&, ZIP_INDEX_TYPE, LPCTSTR, ZIP_INDEX_TYPE, bool)
		\see
			GetFromArchive(CZipArchive&, CZipStringArray&, bool)		
		\see
			SetCallback
		\see
			SetAdvanced 
		\see 
			SetEncryptionMethod
		\see 
			SetPassword
	*/
	bool GetFromArchive(CZipArchive& zip, CZipIndexesArray &aIndexes, bool bKeepSystComp = false);
	
	/**
		Acquires files with the given names from another archive. 
		The compressed data of the file from another archive is copied to the current archive 
		without decompression.

		\param zip 
			The opened archive to get the file from (must not be a segmented archive).

		\param	aNames
			An array of filenames to acquire from the \a zip archive.

		\param bKeepSystComp
			If \c false, then the system compatibility of the file from the \a zip archive
			is converted to the current archive system compatibility, if they differ. Otherwise
			the source system compatibility is copied.

		\return
			\c false, if the operation could not be performed (either of archives is closed, 
		a file inside either of archives is opened, \a zip archive is segmented or the current
		archive is an existing segmented archive.

		\note 
			This method will encrypt data, if an encryption method and a password are set and the file is not already encrypted.

		\note
			Throws exceptions. When an exception is thrown, you may need to call #CloseNewFile with 
			\a bAfterException set to \c true, to make the archive reusable.

		\note
			It is safe to abort the action (by returning \c false from the CZipActionCallback::Callback) 
			in a not segmented archive and when no replacing is taking place. The file that is not 
			entirely added is removed from the archive then.

		\note
			This method calls #GetIndexes on the \a zip archive.
		
		\see
			<a href="kb">0610231446|get</a>
		\see
			<a href="kb">0610231200</a>
		\see
			GetFromArchive(CZipArchive&, ZIP_INDEX_TYPE, LPCTSTR, ZIP_INDEX_TYPE, bool)
		\see
			GetFromArchive(CZipArchive&, CZipIndexesArray&, bool)		
		\see
			SetCallback
		\see
			SetAdvanced
		\see
			SetEncryptionMethod
		\see 
			SetPassword
	*/
	
	bool GetFromArchive(CZipArchive& zip, CZipStringArray &aNames, bool bKeepSystComp = false)
	{
		CZipIndexesArray indexes;
		zip.GetIndexes(aNames, indexes);
		return GetFromArchive(zip, indexes, bKeepSystComp);
	}

	/**
		Gets indexes of the files with names stored in \a aNames and puts them into \a aIndexes.
		If a filename was not found, \c ZIP_FILE_INDEX_NOT_FOUND is inserted at the appropriate 
		position in \a aIndexes.

		\param	aNames
			An array of filenames inside the archive.
		\param aIndexes
			An array of indexes to be found.

		\note 
			Use the #SetCaseSensitivity method to set case-sensitivity.

		\see
			<a href="kb">0610242025|findfast</a>
		\see 
			EnableFindFast
	*/
	void GetIndexes(const CZipStringArray &aNames, CZipIndexesArray& aIndexes);

	/**
		Extracts a file from the archive.
		The argument \a lpszNewName may point to a full path and is influenced by \a bFullPath.
		If \a lpszNewName contains a drive specification then the drive part is removed
		unless #m_bRemoveDriveLetter is set to \c false.

		\param	uIndex
			The index of the file to extract.

		\param	lpszPath
			The directory to extract the file to. May not be \c NULL.

		\param	bFullPath
			In this paragraph the source path means:
			\li The full filename stored in the archive, if \a lpszNewName is \c NULL
			\li \a lpszNewName, if it is not \c NULL

			The meaning of \a bFullPath is following:
			\li If it is \c true, then the method extracts using the full source path, even if #m_szRootPath is set.
			In this case the resulting file path is \a lpszPath plus the source path. 
			\li If it is \c false, then the destination file path is \a lpszPath plus 
			the filename only extracted from the source path.
			In this case, if #m_szRootPath is set (with #SetRootPath), then before adding the source path
			to \a lpszPath, from the source path is removed the part that matches #m_szRootPath
			(matching is performed starting from the beginning of both strings).
			

		\param	lpszNewName 
			The new name of the file after extraction. 
			If \c NULL, the original filename stored in the archive is used.
			May include a path information, but if \a bFullPath is \c false, 
			only the filename is extracted from this value.

		\param pSeekPair
			If not NULL, the file will be extracted starting from the seek position described by this value.

		\param	nBufSize 
			The size of the buffer used while file operations.

		\return
			\c true if successful; \c false otherwise.

		\note 
		- To extract files which filenames that match a specified pattern, use the #FindMatches method.
		- Throws exceptions.
		
		\see
			<a href="kb">0610241003</a>
		\see
			<a href="kb">0610231200</a>
		\see
			<a href="kb">0711101739</a>
		\see 
			ExtractFile(ZIP_INDEX_TYPE, CZipMemFile&, bool, CZipCompressor::COffsetsPair*, DWORD)
		\see
			FindMatches
		\see
			SetCallback
	*/
	bool ExtractFile(ZIP_INDEX_TYPE uIndex,
		LPCTSTR lpszPath,
		bool bFullPath = true,
		LPCTSTR lpszNewName = NULL,
		DWORD nBufSize = 65536);
	
	/**
		The same as #ExtractFile(ZIP_INDEX_TYPE, LPCTSTR, bool, LPCTSTR, CZipCompressor::COffsetsPair*, DWORD )
		but instead to a physical file, this method extracts data into a \c CZipMemFile object.

		\param	uIndex
			The index of the file to extract.

		\param mf
			The memory file to receive the decompressed data.

		\param bRewind
			If \c true, the memory file pointer is positioned at the beginning of the compressed data after compression.
			The rewind operation is performed even if extraction was aborted, but rewinding will not take 
			place, if other exception than CZipException::abortedAction or CZipException::abortedSafely was thrown in the meantime.
		\param pSeekPair
			If not NULL, the file will be extracted starting from the seek position described by this value.
		\param	nBufSize 
			The size of the buffer used while file operations.

		\note 
		- Writing of the decompressed data starts at the current position of the memory file. 
		Keep this in mind, when you pass a \c CZipMemFile object that already contains data 
		(or has a buffer attached) - its contents may be overwritten.
		- If you try to extract a directory, the method will return \c false.

		\see
			<a href="kb">0610231924</a>
		\see
			<a href="kb">0711101739</a>
		\see
			ExtractFile(ZIP_INDEX_TYPE, LPCTSTR, bool, LPCTSTR, CZipCompressor::COffsetsPair*, DWORD )
	*/
	bool ExtractFile(ZIP_INDEX_TYPE uIndex,
		CZipMemFile& mf,
		bool bRewind = true,
		DWORD nBufSize = 65536);

	/**
		Opens the file with the given index in the archive for extracting.
		Not successful opening of the file doesn't lock the whole archive, so 
		you can try to open another one (after catching an exception, if it was 
		thrown)
		\param	uIndex
			The index of the file to open.

		\return
			\c true, if successful; \c false otherwise. This method will also return \c false, if the compression method
			of the file is not supported by the ZipArchive Library.

		\throws CZipException
			with the CZipException::badPassword code, if the file is encrypted 
			and the password was not set.

		\see
			<a href="kb">0610241003|advanced</a>
		\see
			ReadFile
		\see
			CloseFile
	*/
	bool OpenFile(ZIP_INDEX_TYPE uIndex);

	/**
		Reads data from the currently opened file and decompresses it to \a pBuf.
		\param	pBuf
			The buffer to receive the decompressed data.

		\param	uSize
			The size of \a pBuf.

		\return
			The number of bytes read.

		\see
			<a href="kb">0610241003|advanced</a>
		\see
			OpenFile
		\see
			CloseFile

		\note Throws exceptions.
	*/
	DWORD ReadFile(void *pBuf, DWORD uSize);


	/**
		Closes the file opened for extraction in the archive and copy its date and 
		attributes to the file pointed by \a lpszFilePath.

		\param	lpszFilePath 
			The path of the file to have the date and attributes information updated.
			Make sure you have the read access to this file.

		\param bAfterException
			Set it to \c true, when you call this method after an exception was
			thrown, to allow further operations on the archive.

		\return
			One of the following values:
			-  \c 1 : The operation was successful.
			-  \c 0 : There is no file opened.
			-  \c -1 : Some bytes were left to uncompress - probably due to a bad password or a corrupted archive.
			-  \c -2 : Setting extracted file date and attributes was not successful.

		\see
			<a href="kb">0610241003|advanced</a>
		\see
			OpenFile
		\see
			ReadFile
		\see
			CloseFile(CZipFile&)

		\note Throws exceptions.
	*/
	int CloseFile(LPCTSTR lpszFilePath = NULL, bool bAfterException = false);

	/**
		Closes \a file and then calls the CloseFile(LPCTSTR, bool) method using the path of \a file as an argument.		
		Don't call this method, if an exception was thrown before, call the #CloseFile(LPCTSTR, bool) method instead.

		\param	file
			A \c CZipFile object of the extracted file. It must be opened.

		\return	
			The same values as the #CloseFile(LPCTSTR, bool) method.

		\note Throws exceptions.

		\see
			<a href="kb">0610241003|advanced</a>
		\see
			OpenFile
		\see
			ReadFile
		\see
			CloseFile(LPCTSTR, bool)
	*/
	int CloseFile(CZipFile &file);

	/**
		Tests the file with the given index for the integrity.
 		The method throws exceptions but performs all the necessary cleanup
		before, so that the next file can be tested after catching the exception.

		\param	uIndex
			The index of the file to test.

		\param	uBufSize
			The size of the buffer used during extraction.

		\return
			\c false, if the incorrect action has been taken (when #OpenFile or #GetFileInfo returned \c false or \a uBufSize is 0); \c true otherwise.
		If the file didn't passed the test or there was a disk I/O error or the supplied password was incorrect, an exception is thrown.

		\note
			Throws exceptions.

		\see
			<a href="kb">0610241003</a>
		\see
			SetCallback		
	*/
	bool TestFile(ZIP_INDEX_TYPE uIndex, DWORD uBufSize = 65536);

	/**
		Deletes the file with the given index from the archive.		
		If you plan to delete more than one file, use the #RemoveFiles(CZipIndexesArray&) or 
		#RemoveFiles(const CZipStringArray&) method. These methods are optimized for deleting multiple files.

		\param	uIndex
			A zero-based index of the file to delete.
		\return
			\c false, if the file could not be removed; \c true otherwise.

		\note
			Throws exceptions.

		\see
			<a href="kb">0610231944|delete</a>
		\see	
			RemoveFiles(CZipIndexesArray&)
		\see
			RemoveFiles(const CZipStringArray&)
		\see
			SetCallback
		\see
			FindMatches
	*/
	bool RemoveFile(ZIP_INDEX_TYPE uIndex);

	/**
		Deletes files from the archive.	
		Sorts \a aIndexes in the ascending order.

		\param	aIndexes
			An array of zero-based indexes of the files to delete.

		\return
			\c false, if files could not be removed; \c true otherwise.

		\note 
		- To remove files which filenames match a specified pattern, use the #FindMatches method before to find the indexes.
		- Throws exceptions.


		\see
			<a href="kb">0610231944|delete</a>
		\see
			RemoveFile
		\see
			RemoveFiles(const CZipStringArray& )
		\see
			SetCallback
		\see
			FindMatches
	*/
	bool RemoveFiles(CZipIndexesArray& aIndexes);


	/**
		Delete files from the archive.

		\param	aNames
			An array of filenames to delete; 	
		\return
			\c false, if files could not be removed; \c true otherwise.

		\note 
		- Use the #SetCaseSensitivity method to set case-sensitivity.
		- This method uses Find Fast (see <a href="kb">0610242025|findfast</a> for more information).
		- Throws exceptions.

		\see
			<a href="kb">0610231944|delete</a>
		\see	
			RemoveFile
		\see
			RemoveFiles(CZipIndexesArray&)
		\see
			SetCallback
		\see
			EnableFindFast
	*/
	bool RemoveFiles(const CZipStringArray& aNames);

	/**
		Shifts data inside the archive to make an empty space at the beginning of the archive. 
		Into this empty space, you can copy for example a self-extracting stub. To perform shifting, 
		the archive must be opened and no file must be opened for compression or extraction. This method 
		will not work with segmented archives or archives, for which #GetBytesBeforeZip method returns value
		different from \c 0.

		\param uOffset
			The number of bytes to shift the archive data by.

		\return
			\c true, if data was shifted successfully; \c false otherwise.
			
		\note
			- Calls the CZipActionCallback::cbMoveData callback.
			- Throws exceptions.

		\see
			<a href="kb">0610242241|convert</a>
		\see
			PrependData(CZipAbstractFile&, LPCTSTR)
		\see
			PrependData(LPCTSTR, LPCTSTR)
	*/
	bool ShiftData(ZIP_SIZE_TYPE uOffset);

	/**
		Inserts data contained in the file pointed by the \a lpszFilePath path before the archive data.
		You can use this method for example to convert the archive into a self-extracting archive (store 
		a self-extracting stub inside \a lpszFilePath file).
		To perform prepending, the archive must be opened and no file must be opened for compression or extraction. This method 
		will not work with segmented archives or archives, for which #GetBytesBeforeZip method returns value
		different from \c 0.

		\param lpszFilePath
			The path of the file to prepend to the archive.

		\param lpszNewExt
			If it is not \c NULL, then the extension of the archive is changed to this value after prepending data.
			Under linux additionally, this method sets executable permission for the owner after prepeding. If renaming is performed,
			then the archive is closed before it. If this value is \c NULL, the no renaming is performed and the archive stays opened.

		\return
			\c true, if data from \a lpszFilePath file was prepended (and optionally renamed) successfully; \c false otherwise.
			
		\note
			- Calls the CZipActionCallback::cbMoveData callback.
			- Throws exceptions.

		\see
			<a href="kb">0610242241|convert</a>
		\see
			ShiftData
		\see
			PrependData(CZipAbstractFile&, LPCTSTR)
	*/
	bool PrependData(LPCTSTR lpszFilePath, LPCTSTR lpszNewExt = 
#ifdef ZIP_ARCHIVE_WIN
		_T("exe")
#else
		_T("")
#endif
		);

	/**
		Inserts data contained in the \a file before the archive data. You can use this method for example to convert
		the archive into a self-extracting archive (store a self-extracting stub inside \a file).
		To perform prepending, the archive must be opened and no file must be opened for compression or extraction. This method 
		will not work with segmented archives or archives, for which #GetBytesBeforeZip method returns value
		different from \c 0.

		\param file
			The file containing data to insert before the archive data. The file should be opened.

		\param lpszNewExt
			If it is not \c NULL, then the extension of the archive is changed to this value after prepending data.
			Under linux additionally, this method sets executable permission for the owner after prepeding. If renaming is performed,
			then the archive is closed before it. If this value is \c NULL, the no renaming is performed and the archive stays opened

		\return
			\c true, if data from \a file was prepended successfully; \c false otherwise.
			
		\note
			- Calls the CZipActionCallback::cbMoveData callback.
			- Throws exceptions.

		\see
			<a href="kb">0610242241|convert</a>
		\see
			ShiftData
		\see
			PrependData(LPCTSTR, LPCTSTR)
	*/
	bool PrependData(CZipAbstractFile& file, LPCTSTR lpszNewExt = NULL);

	/**
		Sets the global comment in the archive.

		\param	lpszComment
			The comment to set.

		\return
			\c false, if the archive is closed or it is an existing segmented archive;
			\c true otherwise.

		\note
			Throws exceptions.
		\see
			<a href="kb">0610231944|comment</a>
		\see
			GetGlobalComment
	*/
	bool SetGlobalComment(LPCTSTR lpszComment);


	/**
		Gets the global comment for the archive.

		\return
			The global comment or an empty string if the archive is closed.

		\see
			<a href="kb">0610231944|comment</a>
		\see
			SetGlobalComment
	*/
	CZipString GetGlobalComment()const ;


	/**
		Sets the comment for the file with the given index.

		\param	uIndex
			A zero-based index of the file.

		\param	lpszComment
			The comment to set.

		\return
			\c false, if the comment change is impossible; \c true otherwise.

		\note
			Throws exceptions.

		\see
			<a href="kb">0610231944|comment</a>
		\see
			GetFileComment
	*/
	bool SetFileComment(ZIP_INDEX_TYPE uIndex, LPCTSTR lpszComment);

	/**
		Gets the comment for the file with the given index.

		\param uIndex
			A zero-based index of the file.

		\return
			The file comment or an empty string, if the archive is closed.

		\see
			<a href="kb">0610231944|comment</a>
		\see
			SetFileComment
	*/
	CZipString GetFileComment(ZIP_INDEX_TYPE uIndex) const
	{
		const CZipFileHeader* info = GetFileInfo(uIndex);
		return info == NULL ? CZipString(_T("")) : info->GetComment();
	}

	/**
		Gets the path of the currently opened archive segment.
		\return
			The current segments' path or an empty string, if the archive is closed.

		\see
			<a href="kb">0610051553</a>
	*/
	CZipString GetArchivePath()const;

	/**
		Gets the archive volume number currently being processed. The first volume has the number 1.

		This method is useful while working with a segmented archive in creation to find out
		how many parts were already created. To find out how many parts are in an existing segmented archive,
		use the #GetCentralDirInfo method.

		\return
			A one-based number of the current volume or 0, if there is no current volume (the archive is closed).

		\see
			<a href="kb">0610051553</a>
	*/
	ZIP_VOLUME_TYPE GetCurrentVolume() const ;

	/**
		Gets the segmentation mode of the current archive.

		\return
			One of the following values:
			- \c -2 : An existing split archive.
			- \c -1 : An existing spanned archive.
			- \c 0 : No archive segmentation or one-volume only.
			- \c 1 : A spanned archive in creation.
			- \c 2 : A split archive in creation.

		\see
			<a href="kb">0610051553</a>
	*/
	int GetSegmMode()const 
	{
		return m_storage.m_iSegmMode * m_storage.IsSegmented();
	}

	/**
		Case-sensitivity values used as the \a iCaseSensitive argument in the FindFile() method.
	*/
	enum FFCaseSens
	{
		/**
			Uses the default case-sensitivity as set with the #SetCaseSensitivity method.
			If the Find Fast array was built before with a different case-sensitivity,
			it is rebuilt again, if it hasn't been built so far, it is built now with the
			default case-sensitivity.
		*/
		ffDefault,

		/**
			Performs a case-sensitive search. If the CZipArchive is non-case-sensitive,
			a less effective search is performed. It does not rebuild the Find Fast array,
			but if the array hasn't been built yet, it is built now as \b non-case-sensitive
			(you can use \c SetCaseSensitivity(true) and then #ffDefault to build it as case-sensitive).
		*/
		ffCaseSens,

		/**
			Performs a non-case-sensitive search. If the CZipArchive is case-sensitive,
			a less effective search is performed. It does not rebuild the Find Fast array,
 			but if the array hasn't been built yet, it is build now as \b case-sensitive
			(you can use \c SetCaseSensitivity(false) and then #ffDefault to build it as non-case-sensitive).
		*/
		ffNoCaseSens
	};

	/**
		Finds a file in the archive.	
		This method enables the Find Fast feature, if it is not enabled already.

		\param	lpszFileName
			The name of the file to be found in the archive. If the file in the archive is stored with the 
			path information, you must specify it here or set \a bFileNameOnly to \c true.
			Use the same path separators as they are defined for your system as default
			(\e "\" for Windows and \e "/" for Unix/Linux).

		\param iCaseSensitive
			It can be one of the #FFCaseSens values.

		\param bFileNameOnly
			If \c true, the method tries to find a filename without a path (a less effective search is performed).
			If you wish to find a directory name, do not end it with a path separator.
			The path separator is required, if you set \a bFileNameOnly to \c false.

		\note 
			Use the #SetCaseSensitivity method to set the global case-sensitivity.

		\return
			The index of the file, if the file was found; \c ZIP_FILE_INDEX_NOT_FOUND if the file was not found.

		\see
			<a href="kb">0610242025|findfast</a>		
		\see
			EnableFindFast
	*/
	ZIP_INDEX_TYPE FindFile(LPCTSTR lpszFileName, int iCaseSensitive = ffDefault, bool bFileNameOnly = false);

	/**
		Gets the information about the file with the given index.
		The data is copied to \a fhInfo. This may not be 
		optimal for querying large number of file. To avoid copying data, see the 
		#GetFileInfo(ZIP_INDEX_TYPE) method.

		\param	fhInfo
			The object to receive data.

		\param	uIndex
			A zero-based index of the file to get the information about.

		\return
			\c true if successful; \c false otherwise.
		\see
			<a href="kb">0610242128|file</a>
		\see
			GetFileInfo(ZIP_INDEX_TYPE)
		\see
			GetFileInfo(ZIP_INDEX_TYPE) const
		\see
			operator[](ZIP_INDEX_TYPE)			
		\see
			operator[](ZIP_INDEX_TYPE) const
	*/
	bool GetFileInfo(CZipFileHeader& fhInfo, ZIP_INDEX_TYPE uIndex) const;

	/**
		Gets the information about the file with the given index.
		This method provides a direct access to the file data. You should normally not modify 
		the returned object apart from the reasons given in <a href="kb">0610242128|file</a>.

		\param	uIndex
			A zero-based index of the file to get the information about.

		\return
			A CZipFileHeader object that directly points to a central directory entry in memory;
			\c NULL if the archive is closed or there is no such index in the archive.

		\see
			<a href="kb">0610242128|file</a>
		\see
			GetFileInfo(CZipFileHeader&, ZIP_INDEX_TYPE) const
		\see
			GetFileInfo(ZIP_INDEX_TYPE) const
		\see
			operator[](ZIP_INDEX_TYPE)	
		\see
			operator[](ZIP_INDEX_TYPE) const

	*/
	CZipFileHeader* GetFileInfo(ZIP_INDEX_TYPE uIndex);

	/**
		Gets the information about the file with the given index.
		This method provides a direct access to the file data. This method does not allow
		modification of the returned object.

		\param	uIndex
			A zero-based index of the file to get the information about.

		\return
			A CZipFileHeader object that directly points to a central directory entry in memory;
			\c NULL if the archive is closed or there is no such index in the archive.

		\see
			<a href="kb">0610242128|file</a>
		\see
			GetFileInfo(CZipFileHeader&, ZIP_INDEX_TYPE) const
		\see
			GetFileInfo(ZIP_INDEX_TYPE)
		\see
			operator[](ZIP_INDEX_TYPE)	
		\see
			operator[](ZIP_INDEX_TYPE) const

	*/
	const CZipFileHeader* GetFileInfo(ZIP_INDEX_TYPE uIndex) const;

	/**
		Gets the information about the file with the given index.
		This method provides a direct access to the file data. You should normally not modify 
		the returned object apart from the reasons given in <a href="kb">0610242128|file</a>.

		\param	uIndex
			A zero-based index of the file to get the information about.

		\return
			A CZipFileHeader object that directly points to a central directory entry in memory;
			\c NULL if the archive is closed or there is no such index in the archive.

		\see
			<a href="kb">0610242128|file</a>
		\see
			GetFileInfo(CZipFileHeader&, ZIP_INDEX_TYPE) const
		\see
			GetFileInfo(ZIP_INDEX_TYPE)
		\see
			GetFileInfo(ZIP_INDEX_TYPE) const
		\see
			operator[](ZIP_INDEX_TYPE) const		

	*/
	CZipFileHeader* operator[](ZIP_INDEX_TYPE uIndex)
	{
		return GetFileInfo(uIndex);
	}

	/**
		Gets the information about the file with the given index.
		This method provides a direct access to the file data. This method does not allow
		modification of the returned object.

		\param	uIndex
			A zero-based index of the file to get the information about.

		\return
			A CZipFileHeader object that directly points to a central directory entry in memory;
			\c NULL if the archive is closed or there is no such index in the archive.

		\see
			<a href="kb">0610242128|file</a>
		\see
			GetFileInfo(CZipFileHeader&, ZIP_INDEX_TYPE) const
		\see
			GetFileInfo(ZIP_INDEX_TYPE)
		\see
			GetFileInfo(ZIP_INDEX_TYPE) const
		\see
			operator[](ZIP_INDEX_TYPE)
	*/
	const CZipFileHeader* operator[](ZIP_INDEX_TYPE uIndex) const
	{
		return GetFileInfo(uIndex);
	}


	/**
		Gets the number of files in the archive.

		\param	bOnlyFiles
			If \c true, directories are not included in the total count; otherwise all entries are included.

		\return
			The number of files in the archive.
			
		\see
			<a href="kb">0610242128|file</a>
	*/
	ZIP_INDEX_TYPE GetCount(bool bOnlyFiles)
	{
		if (IsClosed())
			return 0;

		ZIP_INDEX_TYPE iTotalCount = GetCount();
		if (bOnlyFiles)
		{
			ZIP_INDEX_TYPE iCount = 0;
			for (ZIP_INDEX_TYPE i = 0; i < iTotalCount; i++)
			{
				if (!m_centralDir[i]->IsDirectory())
					iCount++;
			}
			return iCount;
		}
		else
			return iTotalCount;
	}

	/**
		Gets the number of files in the archive.

		\return
			The number of files in the archive. 
			Note that under some compilation configuration this maybe an unsigned value and a signed value under other compilations.
	*/
	ZIP_INDEX_TYPE GetCount() const
	{
		return (ZIP_INDEX_TYPE) m_centralDir.GetCount();
	}


	/** 
		Calculates the actual size (in bytes) currently occupied by the archive.

		\return
			The sum of the sizes: the number of bytes already written to the file, the number of bytes in the write buffer and the whole size of the central directory.
			If the archive or a volume is closed, the return value is \c 0.

		\see
			<a href="kb">0610242128|archive</a>			
	*/
	ZIP_SIZE_TYPE GetOccupiedSpace() const
	{
		if (IsClosed(true) || IsClosed(false))
		{
			ZIPTRACE("%s(%i) : ZipArchive or the current volume file is closed.\n");
			return 0;
		}
		return m_storage.GetOccupiedSpace() + m_centralDir.GetSize(true);
	}

	/**
		The values used in the CZipArchive::Close() method.
	*/
	enum CloseAfterException
	{
		/**
			Normal closing. Use it, when no exception was thrown while processing the archive.
		*/
		afNoException,

		/**
			Use when an exception was thrown.
			The #Close method doesn't write any data but performs necessary cleaning to reuse the 
			CZipArchive object for another archive processing.
		*/
		afAfterException,

		/**	
			Use when an exception was thrown.
			The #Close method writes the central directory structure to the archive, 
			so that the archive should be usable.
		*/
		afWriteDir
	};

	/**
		Closes the archive.

		\param	iAfterException
			One of the #CloseAfterException values.

		\param bUpdateTimeStamp
			If \c true, the method sets the modification date of the zip file to the date of the newest file in the archive.
			In a segmented archive, only the last segment file will have the time stamp updated.
			You can use this option even without performing any additional processing on the archive, just open and close the archive.

		\note
			Throws exceptions. Does not throw any exceptions, if \a iAfterException is set to the #afAfterException value.

		\see
			<a href="kb">0610222049</a>
		\see
			IsClosed
	*/
	void Close(int iAfterException = afNoException, bool bUpdateTimeStamp = false);


	/**
		Tests if the whole archive or the current volume is closed.

		\param	bArchive <BR>
			If \c true, test for the whole archive. If \c false, test for the current volume only.

		\return
			\c true if a file closed; \c false otherwise.

		\see
			Close
	*/
	bool IsClosed(bool bArchive = true)const
	{
		return m_storage.IsClosed(bArchive);
	}

	/**
		Writes the central directory to the archive and flushes the internal buffers to the disk.
		After that the archive is finalized on the disk, but you can still modify it, if it is not a segmented archive.		

		\note 
		- This method cannot be used on existing segmented archives - they are not modifiable.
		- If you have an archive with a huge central directory, it'll influence the performance calling this function without a reason.
		- Throws exceptions.

		\see
			<a href="kb">0610231446|flush</a>		
		\see
			FlushBuffers
		\see
			SetAutoFlush
		\see
			GetAutoFlush
		\see
			GetSegmMode
	*/
	void Flush();

	/**
		Writes internal buffers to the storage file and flushes the file buffers to the disk.

		\see
			<a href="kb">0610231446|flush</a>
		\see
			Flush
	*/
	void FlushBuffers()
	{
		if (IsClosed())
		{
			ZIPTRACE("%s(%i) : ZipArchive should be opened first.\n");
			return;
		}

		m_storage.FlushBuffers();
	}


	/**
		Sets the CZipArchive object to call the Flush() method after each operation that modifies the archive (apart from changing central extra fields).
		It is useful when you want to prevent the loss of data in case of the program crash - the zip file will then be finalized on the disk.
		Use it after opening the archive.

		\note 
		- You can set auto-flush only for non-segmented archives.
		- If you have an archive with a huge central directory, enabling auto-flush will influence the performance.

		\see
			<a href="kb">0610231446|flush</a>
		\see
			Flush	
		\see
			GetAutoFlush
	*/
	void SetAutoFlush(bool bAutoFlush = true);
	

	/**
		Gets the current auto-flush value.

		\return 
			The current auto-flush value.

		\see
			<a href="kb">0610231446|flush</a>
		\see
			SetAutoFlush
		\see
			Flush	
		
	*/
	bool GetAutoFlush()const {return m_bAutoFlush;}

	/**
		Sets the system compatibility of the archive. Use it after opening the archive,
		but before adding a new file or using the #SetFileHeaderAttr method.
		If the filename code page is standard for the current system compatibility, 
		the ZipArchive Library will change the filename code page to be default for the 
		new system compatibility (see <a href="kb">0610051525</a> for more information).
		
		\param iSystemComp
			The new system compatibility to use. It can be one of the ZipCompatibility::ZipPlatforms values.
		\return
			\c false, if the value \a iSystemComp is not supported 
			or it is not possible to set the value at this moment; \c true otherwise.

		\see
			<a href="kb">0610231446|compatibility</a>
		\see
			GetSystemCompatibility
		\see
			CZipFileHeader::GetSystemCompatibility
		\see
			ZipCompatibility::ZipPlatforms
		\see
			ZipPlatform::GetSystemID
	*/
	bool SetSystemCompatibility(int iSystemComp);

	/**
		Gets the system compatibility of the current archive.			
		\return	
			One of the ZipCompatibility::ZipPlatforms values.

		\see
			<a href="kb">0610231446|compatibility</a>
		\see
			SetSystemCompatibility
		\see
			CZipFileHeader::SetSystemCompatibility
		\see
			ZipCompatibility::ZipPlatforms
		\see
			ZipPlatform::GetSystemID		
	*/
	int GetSystemCompatibility() const {return m_iArchiveSystCompatib;}



	/**
		Sets the attributes for the CZipFileHeader object to be used
		in the OpenNewFile(CZipFileHeader&, int, LPCTSTR) method.

		This special procedure is required, because the attributes value depends 
		on the system compatibility of the archive.

		\param	header
			The object to have the attributes set.

		\param	uAttr
			The attributes to set.

		\throws CZipException
			with the CZipException::platfNotSupp code, if the system compatibility
			is not supported by the ZipArchive Library.

		\see
			<a href="kb">0610231446|advanced</a>
		\see
			SetSystemCompatibility
	*/
	void SetFileHeaderAttr(CZipFileHeader& header, DWORD uAttr)const;

	/**
		Gets the underlying archive storage object.

		\return
			The pointer to CZipStorage.

		\see
			CZipStorage
	*/
	CZipStorage* GetStorage(){return &m_storage;}

	/**
		Sets the current settings that control storing of filenames and comments in the archive.

		\param settings
			The settings to set.

		\see
			<a href="kb">0610051525</a>
		\see
			SetStringStoreSettings(UINT, bool, UINT)
		\see
			SetStringStoreSettings(UINT, bool)
		\see
			ResetStringStoreSettings
		\see
			GetStringStoreSettings		
	*/
	void SetStringStoreSettings(const CZipStringStoreSettings& settings)
	{
		m_stringSettings = settings;
	}

	/**
		Sets the current settings that control storing of filenames and comments in the archive.

		\param uFileNameCodePage
			The code page for filenames.
	
		\param bStoreNameInExtraData
			If \c true, the encoded filenames are stored in central extra fields.
	
		\param uCommentCodePage
			The code page for comments.

		\see
			<a href="kb">0610051525</a>
		\see
			SetStringStoreSettings(const CZipStringStoreSettings&)
		\see
			SetStringStoreSettings(UINT, bool)
		\see
			ResetStringStoreSettings
		\see
			GetStringStoreSettings		
	*/
	void SetStringStoreSettings(UINT uFileNameCodePage, bool bStoreNameInExtraData, UINT uCommentCodePage)
	{
		m_stringSettings.Set(uFileNameCodePage, bStoreNameInExtraData, uCommentCodePage);
	}

	/**
		Sets the current settings that control storing of filenames and comments in the archive.
		The code page for comments stays the same when calling this method.

		\param uFileNameCodePage
			The code page for filenames.
	
		\param bStoreNameInExtraData
			If \c true, the encoded filenames are stored in central extra fields.
	
		\see
			<a href="kb">0610051525</a>
		\see
			SetStringStoreSettings(const CZipStringStoreSettings&)
		\see
			SetStringStoreSettings(UINT, bool, UINT)
		\see
			ResetStringStoreSettings
		\see
			GetStringStoreSettings		
	*/
	void SetStringStoreSettings(UINT uFileNameCodePage, bool bStoreNameInExtraData = false)
	{
		SetStringStoreSettings(uFileNameCodePage, bStoreNameInExtraData,  m_stringSettings.m_uCommentCodePage);
	}
	
	/**
		Sets the current settings that control storing of filenames and comments in the archive to their default values considering the 
		current system compatibility of the archive (see GetSystemCompatibility()).

		\see
			<a href="kb">0610051525</a>
		\see
			SetStringStoreSettings(const CZipStringStoreSettings&)
		\see
			SetStringStoreSettings(UINT, bool, UINT)
		\see
			SetStringStoreSettings(UINT, bool)
		\see
			GetStringStoreSettings		
	*/
	void ResetStringStoreSettings()
	{
		m_stringSettings.Reset(m_iArchiveSystCompatib);
	}

	/**
		Gets the current settings that control storing of filenames and comments in the archive.
	
		\return
			The current string store settings.

		\see
			<a href="kb">0610051525</a>
		\see
			SetStringStoreSettings(const CZipStringStoreSettings&)
		\see
			SetStringStoreSettings(UINT, bool, UINT)
		\see
			SetStringStoreSettings(UINT, bool)
		\see
			ResetStringStoreSettings		
		\see
			CZipFileHeader::GetStringStoreSettings
	 */
	CZipStringStoreSettings& GetStringStoreSettings()
	{
		return m_stringSettings;
	}

	/**
		Enables or disables fast searching for files inside the archive using filenames.

		\see
			<a href="kb">0610242025|findfast</a>
		\see
			FindFile
		\see
			GetIndexes
		\see
			GetFindFastIndex
	*/
	void EnableFindFast(bool bEnable = true);


    /**
		Allows to retrieve the order of sorted files after you enabled the Find Fast feature with the EnableFindFast() method.
       
       \param iFindFastIndex
			The index of the file in the sorted array.

       \return 
	        The index of the file in the central directory. You can use the return value
			in other methods that require the file index (such as #GetFileInfo).
			This method returns \c -1, if you have not called 
			#EnableFindFast before or the archive is closed or the \a iFindFastIndex
			is out of range.

		\see
			<a href="kb">0610242025|findfast</a>		
		\see
			EnableFindFast
     */
	ZIP_INDEX_TYPE GetFindFastIndex(ZIP_INDEX_TYPE iFindFastIndex) const
	{
		if (IsClosed())
		{
			ZIPTRACE("CZipArchive::GetFindFastIndex: ZipArchive should be opened first.\n");
			return ZIP_FILE_INDEX_UNSPECIFIED;
		}
		
		return m_centralDir.GetFindFastIndex(iFindFastIndex);
	}


	/**
		Sets the temporary path used when compressing files and there is a need for a temporary archive.
		Temporary files are used when replacing or when compressing a segmented archive with the #zipsmCheckForEff flag.
		If the path is not set or it does not exists or there is not enough free space in it, 
		then the ZipArchive Library first tries the the following folders
		in the given order for sufficient free space:
		\li system default temporary directory,
		\li the current directory.

		If all above fails, no temporary file is created and the compression goes the usual way.

		\param lpszPath
			The path used for storing temporary files. Set it to \c NULL, to clear the temporary path and let the 
			ZipArchive Library figure it out (see above).

		\param bForce
			If \a lpszPath is not \c NULL and this parameter set to \c true
			the directory is created, if it doesn't exist. Otherwise the the ZipArchive Library
			tries to figure out the location for temporary files by itself (see above).

		\see
			GetTempPath
		\see
			AddNewFile
		\see
			Smartness
	*/
	void SetTempPath(LPCTSTR lpszPath = NULL, bool bForce = true);

	/**   
		Gets the current temporary path used when compressing files.
       
		\return
			The current temporary path.

		\see
			SetTempPath
     */
	CZipString GetTempPath()const 
	{
		return m_szTempPath;
	}

	/**
		The values used in the PredictFileNameInZip() method.
	*/
	enum Predict
	{
	
		prDir,  ///< If \a lpszFilePath is a directory, appends a separator.
		prFile, ///< Treats \a lpszFilePath as a common file.
		prAuto  ///< Treats \a lpszFilePath as a directory only if it has a path separator appended.				
	};

    /**
       Predicts the filename as it would be stored in the archive, when given parameters would be used with 
	   one of the AddNewFile() methods. The method takes into account the root path set with the #SetRootPath method. 
	   You can use this method to eliminate duplicates before adding a list of files.
       
       \param lpszFilePath
			The file path, the same as CZipAddNewFileInfo::m_szFilePath.
	   \param bFullPath
			The same as CZipAddNewFileInfo::m_bFullPath.
       \param iWhat
			One of the #Predict values to interpret \a lpszFilePath correctly.
       \return
			The filename as it would be stored in the archive.
     */
	CZipString PredictFileNameInZip(LPCTSTR lpszFilePath, bool bFullPath, int iWhat = prAuto)const ;

	/**
		Calculates the maximum number of bytes that the file represented by #CZipFileHeader would occupy in the current archive.

		You need to set the following members in the structure:
			\li File attributes (use the #SetFileHeaderAttr method). It is for determining whether the file is a directory or not.
			You can use ZipPlatform::GetFileAttr.
			\li The filename as it would appear in the archive (use CZipFileHeader::SetFileName). You can use #PredictFileNameInZip.
			\li The compressed size of the file (use CZipFileHeader::m_uLocalComprSize). You can use an uncompressed file size
			here for the maximum value estimate.

		Additionally you may set:
			- A file comment (use the CZipFileHeader::SetComment method).
			- Extra fields (use CZipFileHeader::m_aLocalExtraData and CZipFileHeader::m_aCentralExtraData).

		\param fh
			A template object pre-filled with data.

		\return
			The maximum number of bytes the file would occupy in the archive.

		\note
		- The method takes into account if the current archive is a segmented archive.
		- If the archive has a password set, the method will assume that the file would be stored encrypted in the archive (extra bytes may be added then depending on the encryption method).
		- The method calls CZipFileHeader::PrepareData
		- Zip64 only: The method takes into account the current file pointer position in the archive to determine the need for the Zip64 extensions 
			and updates CZipFileHeader::m_uVolumeStart and CZipFileHeader::m_uOffset.
		- The method does not take into account a situation when a file would be compressed, but mostly stored blocks would be emitted by the Huffman compression engine.
		In this case extra 5 bytes are added per a single stored block. You should remove the file and store it instead when it happens (see #zipsmCheckForEff).

		\see
			PredictMaximumFileSizeInArchive(LPCTSTR, bool) const
	*/
	ZIP_SIZE_TYPE PredictMaximumFileSizeInArchive(CZipFileHeader& fh) const;

	 /**
		Calls the PredictMaximumFileSizeInArchive(CZipFileHeader&) const method. 
		Before calling, fills the #CZipFileHeader structure with the filename as it 
		would appear in the archive and sets the proper file size (unless \a lpszFilePath is a directory).

		\param lpszFilePath
			A path to the file for which you want to predict the maximum size it would occupy.

		\param bFullPath 
			The same as CZipAddNewFileInfo::m_bFullPath.

		\return
			The maximum number of bytes the file would occupy in the archive.

		\see
			PredictMaximumFileSizeInArchive(CZipFileHeader&) const
	 */
	ZIP_SIZE_TYPE PredictMaximumFileSizeInArchive(LPCTSTR lpszFilePath, bool bFullPath) const;
	

	/**
		Checks if the filename of the given file will be duplicated in the archive, if added to the archive with the given parameters.

		\param lpszFilePath
			The file path. You normally use it in one of the #AddNewFile methods.

	   \param bFullPath
			The same as CZipAddNewFileInfo::m_bFullPath.

		\param bFileNameOnly
			If \c true, the method assumes that the filename is duplicated if only the name part (no path)
			is the same (\a bFullPath is ignored), otherwise the whole filename with a path is taken into account.

		\param iWhat
			One of the #Predict values to interpret \a lpszFilePath correctly.

		\return
			The zero-based index of the file in the archive which filename would be duplicated, 
			or \c ZIP_FILE_INDEX_NOT_FOUND, if the filename would be unique.
	*/
	ZIP_INDEX_TYPE WillBeDuplicated(LPCTSTR lpszFilePath, bool bFullPath, bool bFileNameOnly = false, int iWhat = prAuto);

    /**
       Predicts the full resulting filename with path after extraction. 
	   The parameters (except for the first) are in the form you'd pass
	   to the #ExtractFile(ZIP_INDEX_TYPE , LPCTSTR , bool , LPCTSTR , CZipCompressor::COffsetsPair*, DWORD ) method.
	   The method takes into account the root path set with the #SetRootPath method.

       \param lpszFileNameInZip
			The filename of the file inside the archive (may be \c NULL if \a lpszNewName is not \c NULL).

       \param lpszPath

       \param bFullPath

       \param lpszNewName       

       \return
			The predicted resulting file path.
     */
	CZipString PredictExtractedFileName(LPCTSTR lpszFileNameInZip, LPCTSTR lpszPath, bool bFullPath, LPCTSTR lpszNewName = NULL)const ;


/**
		Removes the root path from \a zpc.

		\param	zpc
			The path to have the common beginning removed from.

		\see
			SetRootPath
*/
	CZipString TrimRootPath(CZipPathComponent& zpc) const ;

    /**
		Removes \a lpszBeginning from the beginning of \a szPath. Both argument are
		considered to be paths - they must match up to the path separator.
       
		\param lpszBeginning
			The beginning to remove.

		\param szPath
			The path to have the beginning removed.

		\param pCompareFunction 
			The compare function used (see #m_pZipCompare).

		\return
			\c true, if the path beginning was removed; \c false otherwise.
     */
	static bool RemovePathBeginning(LPCTSTR lpszBeginning, CZipString& szPath, ZIPSTRINGCOMPARE pCompareFunction);

	/**
		Sets the default archive case-sensitivity. The default CZipArchive case-sensitivity depends
		on the system and is set as follows:
			\li on Windows:	\c false
			\li on Linux:	\c true
			
		Calling this method affects the following methods:

		- #FindFile
		- #GetIndexes
		- #FindMatches		
		- #EnableFindFast
		- #TrimRootPath
		- #RemoveFiles(const CZipStringArray&)
			
		\param bCaseSensitive
			The case-sensitivity to be used.

		\note
			Set it before using one of the mentioned methods or leave it as it was set by default.
	*/
	void SetCaseSensitivity(bool bCaseSensitive) 
	{
		m_bCaseSensitive = bCaseSensitive;
		m_pZipCompare = GetCZipStrCompFunc(bCaseSensitive);
	}

	/*
		Gets the current case sensitivity.
		
		\return 
			\c true, if the archive is case-sensitive; \c false otherwise.

		\see
			SetCaseSensitivity
	*/
	bool GetCaseSensitivity() const
	{
		return m_bCaseSensitive;
	}

	/**
		Gets the central directory information.
		\param info
			The object to retrieve information data.

		\see
			GetCentralDirSize
	*/
	void GetCentralDirInfo(CZipCentralDir::CInfo& info)const;
	

	/**
		Gets the central directory size.

		\param bWhole
			If \c true, the return value includes the size of file headers.

		\return
			The size of the central directory.

		\see
			GetCentralDirInfo
	*/
	ZIP_SIZE_TYPE GetCentralDirSize(bool bWhole = true) const
	{
		if (IsClosed())
		{
			ZIPTRACE("%s(%i) : ZipArchive is closed.\n");
			return 0;
		}
		return m_centralDir.GetSize(bWhole);
	}

	/**
		Gets a value indicating whether the archive can be modified or not.
		An archive can be read-only when it is an existing segmented archive
		or it was opened with the #zipOpenReadOnly flag.

		\return
			\c true if the archive is read-only; \c false otherwise.
	*/
	bool IsReadOnly(){return m_storage.IsReadOnly();} const

	/**
		Sets the number of non-archive bytes that are present before the actual archive in the archive file.
		The library usually tries to automatically calculate this value, but this may not be possible
		under some conditions.
		
		\param uCount
			The number of bytes before the actual archive.
				
		\see
			GetBytesBeforeZip
	 */
	void SetBytesBeforeZip(ZIP_SIZE_TYPE uCount = 0)
	{
		if (!IsClosed())
		{
			ZIPTRACE("%s(%i) : Set it before opening the archive.\n");
			return;
		}
		m_storage.m_uBytesBeforeZip = uCount;
	}

	/**
		Gets the number of non-archive bytes that are present before the actual archive in the archive file.

		\return
			The number of bytes before the actual archive.
		\see
			SetBytesBeforeZip
	 */
	ZIP_SIZE_TYPE GetBytesBeforeZip() const
	{
		return m_storage.m_uBytesBeforeZip;
	}

	/**
		Values describing various archive consistency checks that the library performs. 
		Instruct the library to skip selected checks using the #SetIgnoredConsistencyChecks method.
	*/
	enum ConsistencyCheck
	{		
		checkNone,							///< If used in the the #SetIgnoredConsistencyChecks method, checks for all inconsistencies in an archive.
		checkCRC				= 0x0001,	///< Check CRC after decompression. Use it when working with Java <sup><small>TM</small></sup> Archives (jar). The CRC check is performed using CRC written in a central header when closing a file after extraction.
		checkLocalMethod		= 0x0002,	///< Check if the compression method written in a local header matches the compression method written in a central header.
		checkLocalSizes			= 0x0004,	///< Check if sizes of compressed and uncompressed data written in a local header match their counterparts in a central header. The compressed size in the local header is always ignored, if it is 0.
		checkLocalCRC			= 0x0008,	///< Check if the CRC written in a local header matches the CRC written in a central header. 
		checkLocalFlag			= 0x0010,	///< Check if the general purpose flag value written in a local header matches its counterpart written in a central header. 
		checkLocalAll			= checkLocalMethod | checkLocalSizes | checkLocalCRC | checkLocalFlag, ///< Check for inconsistencies between central and local headers at all. These checks are performed when opening a file for extraction. 
		checkDataDescriptor		= 0x0100, ///< Check if values written in extra data descriptor match values written in central header. This check is performed when closing a file after extraction, only if a file has a data descriptor (see CZipFileHeader::IsDataDescriptor()). Ignored by default (it is consistent with behavior of popular archivers).
		checkAll				= checkCRC | checkLocalAll | checkDataDescriptor, ///< Logical sum of all possible checks.
		checkIgnoredByDefault	= checkDataDescriptor, ///< Checks that are ignored by default by the ZipArchive Library
	};

	/**
		Set the consistency checks to ignore while processing an archive. Allows opening archives which are not entirely 
		consistent, but nevertheless the compressed data is correct. The level is reset to #checkIgnoredByDefault when
		opening or creating an archive.
		\param iLevel
			The consistency check. Can be one or more (OR-ed together) of #ConsistencyCheck values.

		\see
			ConsistencyCheck
		\see
			GetIgnoredConsistencyChecks
	 */
	void SetIgnoredConsistencyChecks(int iLevel = checkIgnoredByDefault)
	{
		if (IsClosed())
		{
			ZIPTRACE("%s(%i) : Set it after opening the archive.\n");
			return;
		}
		m_centralDir.m_iIgnoredChecks = iLevel;
	}
	
	/**
		Return the currently ignored consitency checks. Can be one or more of #ConsistencyCheck values.

		\see 
			SetIgnoredConsistencyChecks
	*/
	int GetIgnoredConsistencyChecks() const
	{
		return m_centralDir.m_iIgnoredChecks;
	}

	/**
		Forces reading all headers from the central directory, even if the number of files reported by the archive is different. 
		By default, the ZipArchive Library reads only the number of headers declared by the archive. Call this method before opening an archive.

		\note This method is useful when dealing with archives created with
		external software that put more files inside an archive that it is permitted by the zip format. Such a situation
		can take place with archives created with e.g. BOMArchiveHelper (Mac OS X utility), when the number of files exceeds 65,535.

		\see
			GetExhaustiveRead
	*/
	void SetExhaustiveRead(bool bExhaustiveRead)
	{
		if (!IsClosed())
		{
			ZIPTRACE("%s(%i) : Set it before opening the archive.\n");
			return;
		}
		m_bExhaustiveRead = bExhaustiveRead;
	}

	/**
		Returns the value indicating whether the exhaustive read function is active or not.

		\return
			\c true, if the exhaustive read function is active; \c false otherwise.

		\see
			SetExhaustiveRead
	*/
	bool GetExhaustiveRead() const
	{
		return m_bExhaustiveRead;
	}

    /**
		Finds indexes of the files, which filenames match the specified pattern. The indexes are stored in the \a ar array.
		The indexes can be used then e.g. in deleting or extracting files.
       
       \param lpszPattern
			The pattern to match. The case-sensitivity of the pattern 
			is set to the global archive case-sensitivity (set with the #SetCaseSensitivity method).

       \param ar
			The array which will contain the resulting indexes. The contents of \a ar are not cleared, but the 
			indexes are appended to it.

	   \param bFullPath
			\li If \c true, the method matches the filename with path (if present) of the file.
			If the file is a directory, end it with a path separator or use a pattern that will recognize it.
			\li If \c false, the method matches only the name of the file. If the file is a directory, 
			do not end it with a path separator.

		\see
			<a href="kb">0610242025|match</a>
		\see
			SetCaseSensitivity
		\see
			ZipArchiveLib::CWildcard
       
     */
	void FindMatches(LPCTSTR lpszPattern, CZipIndexesArray& ar, bool bFullPath = true);


    /**
		Renames the file with the given index in the archive.

		\param uIndex
			A zero-based index of the file to rename.

		\param lpszNewName
			The new name for the file.

		\return 
			\c true, if the file was renamed; \c false otherwise.

		\note
			Throws exceptions.
		\see
			<a href="kb">0610231944|rename</a>
     */
	bool RenameFile(ZIP_INDEX_TYPE uIndex, LPCTSTR lpszNewName);
	
	/**
		Removes the central directory from the archive. 
		You may then modify central extra fields and write the central directory back to the archive afterwards (use the #Close method).

		\return 
			\c true, if the central directory was successfully removed; \c false otherwise.

		\see
			<a href="kb">0610242300|central</a>
	*/
	bool RemoveCentralDirectoryFromArchive();

	/**
		Reads the local header information of the file with the given index.

		\param uIndex
			The index of the file for which to update the local information.

		\return 
			\c true, if the information was successfully updated; \c false otherwise.

		\note
			Throws exceptions.

		\see
			<a href="kb">0610231944|time</a>
		\see
			OverwriteLocalHeader
	*/
	bool ReadLocalHeader(ZIP_INDEX_TYPE uIndex);

	/**
		Writes the local header information of the file with the given index back to the archive.

		\param uIndex
			The index of the file for which to write the local information.

		\return 
			\c true, if the information was successfully written; \c false otherwise.

		\note
			Throws exceptions.

		\see
			<a href="kb">0610231944|time</a>
		\see
			ReadLocalHeader
	*/
	bool OverwriteLocalHeader(ZIP_INDEX_TYPE uIndex);

	/**
		Retrieves the current compressor. The type of the compressor depends on the compression method 
		used for compressing or decompressing data.

		\see
			SetCompressionMethod
	*/
	const CZipCompressor* GetCurrentCompressor() const
	{
		return m_pCompressor;
	}

	/**
		If \c true, the drive letter is removed from the filename stored inside the archive when adding
		a new file to the archive or extracting an existing one.
		It affects #AddNewFile, #ExtractFile, #PredictFileNameInZip, #PredictExtractedFileName,
		#WillBeDuplicated methods. The default value is \c true.
	*/
	bool m_bRemoveDriveLetter;

protected:

	/**
		Reads the local header information of the file with the given index.

		\param uIndex
			The index of the file for which to update the local information.

		\note
			Throws exceptions.
	*/
	void ReadLocalHeaderInternal(ZIP_INDEX_TYPE uIndex)
	{
		// update sizes of local filename and extra field - they may differ from the ones in the central directory
		m_centralDir.OpenFile(uIndex);
		// skip checking the data descriptor, we are not there yet
		m_centralDir.CloseFile(true);
	}

	/**
		See the description of #EncryptFiles.

		\param pIndexes

		\return 
			\c false, if the files could not be encrypted; \c true otherwise.

		\note
			Throws exceptions.

		\see
			EncryptFiles
	*/
	bool EncryptFilesInternal(CZipIndexesArray* pIndexes);


	/**
		The value set with #SetExhaustiveRead.
	*/
	bool m_bExhaustiveRead;

	/**
		See the description of #OpenNewFile(CZipFileHeader&, int, LPCTSTR)

		\param header
		\param iLevel
		\param lpszFilePath
		\param uReplaceIndex
			For internal use only.
		\see
			OpenNewFile(CZipFileHeader&, int, LPCTSTR)
	*/
	bool OpenNewFile(CZipFileHeader & header, int iLevel, LPCTSTR lpszFilePath, ZIP_INDEX_TYPE uReplaceIndex);
	
	/**
		See CZipCallbackProvider.
	*/
	ZipArchiveLib::CZipCallbackProvider m_callbacks;
	

	/**
		Writes the central directory notifying a callback object if available.
	*/
	void WriteCentralDirectory(bool bFlush = true);

	/**
		The value set with #SetCaseSensitivity.
	*/
	bool m_bCaseSensitive;
	

	/**
		A pointer to a method used to compare strings. 
		Can point to \c Compare, \c CompareNoCase, \c Collate or \c CollateNoCase method.		
	*/
	ZIPSTRINGCOMPARE m_pZipCompare;


	/**
		Physical layer of the archive.
		\see
			CZipStorage
	*/
	CZipStorage m_storage;

	/**
		A central directory object.
		\see
			CZipCentralDir
	*/
	CZipCentralDir m_centralDir;

	/**
		The open mode of the current file inside archive.	
	*/
	enum OpenFileType
	{
		extract = -1,	///< A file is opened for extraction.
		nothing,		///< There is no file inside the archive opened.
		compress		///< A new file is opened for compression.
	};
	
	/**
		Takes one of the CZipArchive::OpenFileType enum values.
	*/
	int m_iFileOpened;

	/**
		The value set with SetAutoFlush().
	*/
	bool m_bAutoFlush;

	/**
		The value set with SetRootPath().
	*/
	CZipString m_szRootPath;

	/**
		The value set with SetTempPath().
	*/
	CZipString m_szTempPath;


	/**
		Opens the archive in the given mode.
		Called by #Open(LPCTSTR, int, ZIP_SIZE_TYPE) and #Open(CZipAbstractFile&, int).
		\param	iMode
			The mode.
		\note
			Throws exceptions.
	*/
	void OpenInternal(int iMode);

	/**
		Initializes the archive during opening.

		\param iArchiveSystCompatib
			The system's compatibility of the archive.

		\param pSource
			If not \c NULL, then it specifies the central directory for sharing.

		\note
			Throws exceptions.
	*/
	void InitOnOpen(int iArchiveSystCompatib, CZipCentralDir* pSource = NULL);

	/**
		The value set with #SetSystemCompatibility.
	*/
	int m_iArchiveSystCompatib;

	/**
		The value set with #SetPassword.
	*/
	CZipAutoBuffer m_pszPassword;

	/**
		Gets the file currently opened for compression or decompression.
		\return
			The currently opened file or \c NULL, if there is no file opened.
	*/
	CZipFileHeader* CurrentFile();

	/**
		Releases the current cryptograph.
	*/
	void ClearCryptograph()
	{
		if (m_pCryptograph)
		{
			delete m_pCryptograph;
			m_pCryptograph = NULL;
		}
	}

	/**
		Creates a new cryptograph. You can override this method and implement your own cryptograph.

		\param iEncryptionMethod
			The requested encryption method.

		\see
			CZipCryptograph::EncryptionMethod
	*/
	virtual void CreateCryptograph(int iEncryptionMethod)
	{
		if (m_pCryptograph != NULL)
			if (m_pCryptograph->CanHandle(iEncryptionMethod))
				return;

		ClearCryptograph();
		m_pCryptograph = CZipCryptograph::CreateCryptograph(iEncryptionMethod);
	}

	/**
		The current cryptograph.
	*/
	CZipCryptograph* m_pCryptograph;

	/**
		Releases the current compressor.
	*/
	void ClearCompressor()
	{
		if (m_pCompressor)
		{
			delete m_pCompressor;
			m_pCompressor = NULL;
		}
	}

	/**
		Creates a new compressor. You can override this method and implement your own compressor.

		\param uMethod
			The requested data compression method.

		\see 
			CZipCompressor::CompressionMethod
	*/
	virtual void CreateCompressor(WORD uMethod)
	{
		if (m_pCompressor == NULL || !m_pCompressor->CanProcess(uMethod))
		{
			ClearCompressor();
			m_pCompressor = CZipCompressor::CreateCompressor(uMethod, &m_storage);
		}
		m_pCompressor->UpdateOptions(m_compressorsOptions);
	}

	/**
		The current compressor.
	*/
	CZipCompressor* m_pCompressor;

	/**
		The value set with #SetEncryptionMethod.
	*/
	int m_iEncryptionMethod;

	/**
		The value set with #SetCompressionMethod.
	*/
	WORD m_uCompressionMethod;
	

	/**
		A helper buffer used during various IO operations.

		\see 
			m_iBufferSize
		\see
			SetAdvanced
	*/
	CZipAutoBuffer m_pBuffer;

	/**
		The size of the #m_pBuffer buffer. Set it before opening the archive.
		It is usually set with the #SetAdvanced method
		(specify this value as the second argument).

		\see
			SetAdvanced
	*/
	DWORD m_iBufferSize;

	/**
		The value set with the SetStringStoreSettings() method.
	*/
	CZipStringStoreSettings m_stringSettings;

private:
	CZipCompressor::COptionsMap m_compressorsOptions;
	void Initialize();
	void MakeSpaceForReplace(ZIP_INDEX_TYPE iReplaceIndex, ZIP_SIZE_TYPE uTotal, LPCTSTR lpszFileName);

	void MovePackedFiles(ZIP_SIZE_TYPE uStartOffset, ZIP_SIZE_TYPE uEndOffset, ZIP_SIZE_TYPE uMoveBy, CZipActionCallback* pCallback, bool bForward = false, bool bLastCall = true);
	
	bool RemoveLast(bool bRemoveAnyway = false);

	bool GetFromArchive(CZipArchive& zip, ZIP_INDEX_TYPE uIndex, LPCTSTR lpszNewFileName, ZIP_INDEX_TYPE iReplaceIndex, bool bKeepSystComp, CZipActionCallback* pCallback);

	bool UpdateReplaceIndex(ZIP_INDEX_TYPE& iReplaceIndex);
	
	void ThrowError(int err);		

	void InitBuffer()
	{
		m_pBuffer.Allocate(m_iBufferSize);
	}
	void ReleaseBuffer()
	{
		m_pBuffer.Release();
	}
};

#if (_MSC_VER > 1000) && (defined ZIP_HAS_DLL)
	#pragma warning (pop)
#endif

#endif // !defined(ZIPARCHIVE_ZIPARCHIVE_DOT_H)
