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

/**
* \file ZipCentralDir.h
*	Includes the CZipCentralDir class.
*
*/

#if !defined(ZIPARCHIVE_ZIPCENTRALDIR_DOT_H)
#define ZIPARCHIVE_ZIPCENTRALDIR_DOT_H

#if _MSC_VER > 1000
#pragma once
#endif

#if (_MSC_VER > 1000) && (defined ZIP_HAS_DLL)
	#pragma warning (push)
	#pragma warning( disable : 4251 ) // needs to have dll-interface to be used by clients of class
#endif

#include "ZipException.h"
#include "ZipFileHeader.h"
#include "ZipAutoBuffer.h"
#include "ZipCollections.h"
#include "ZipCompatibility.h"
#include "ZipExport.h"
#include "ZipCallbackProvider.h"
#include "ZipMutex.h"


/**
	Represents the central directory record in the archive.
*/
class ZIP_API CZipCentralDir  
{
public:

	/**
		Used in fast finding files by the filename.
		A structure for the internal use only.
		\see
			CZipCentralDir::m_pFindArray
		\see
			CZipArchive::FindFile
		\see
			CZipArchive::EnableFindFast
	*/
	struct ZIP_API CZipFindFast
	{
		CZipFindFast()
		{
			m_uIndex = 0;
			m_pHeader= NULL;
		}
		CZipFindFast(CZipFileHeader* pHeader, ZIP_INDEX_TYPE uIndex):m_pHeader(pHeader), m_uIndex(uIndex){}

		/**
			A pointer to the structure in CZipCentralDir. We extract a name from it.
		*/
		CZipFileHeader* m_pHeader;

		/**
			The index in the central directory of the #m_pHeader.
		*/
		ZIP_INDEX_TYPE m_uIndex;
	};
	

	/**
		Stores general information about the central directory.
		\see
			CZipArchive::GetCentralDirInfo
	*/
	struct ZIP_API CInfo
	{
		
		/**
			The position of the End of Central Directory Record.
			In the Zip64 it points to the Zip64 counterpart.
		*/
		ZIP_SIZE_TYPE   m_uEndOffset;

		/**
			The zero-based number of the volume with the End of Central Directory Record. To determine the total number of segments in an archive,
			add one to this value. Request this information with CZipArchive::GetCentralDirInfo.
		*/
		ZIP_VOLUME_TYPE m_uLastVolume;
		ZIP_VOLUME_TYPE m_uVolumeWithCD;		///< The number of the volume with the start of the central directory.
		ZIP_INDEX_TYPE m_uVolumeEntriesNo;	///< The total number of entries in the central directory on the last volume.
		ZIP_INDEX_TYPE m_uEntriesNumber;	///< The total number of entries in the central directory.

		/**
			The size of the central directory. 
			This value is valid only if #m_bInArchive is \c true; in other cases use the #GetSize method instead.
		*/
		ZIP_SIZE_TYPE m_uSize;

		/**
			The offset of the start of the central directory with respect to the starting volume number.
			It is the value written in the central directory record.
			This value is valid only if #m_bInArchive is \c true.
		*/
		ZIP_SIZE_TYPE m_uOffset;


		/**
			This value is \c true if the central directory is physically present in the archive; \c false otherwise.
		*/
		bool m_bInArchive;

	private:
		friend class CZipCentralDir;		
		void Init()
		{
			m_iReference = 1;
#ifdef ZIP_ARCHIVE_USE_LOCKING
			m_mutex.Open();
#endif
			m_pCompare = GetCZipStrCompFunc(ZipPlatform::GetSystemCaseSensitivity());
			m_bCaseSensitive = false;
			m_bFindFastEnabled = false;
			m_pszComment.Release();
			// initialize ( necessary when using 64 bits - we are copying only 4 bytes in Read())	
			m_bInArchive = false;
			m_uEndOffset = 0;
			m_uLastVolume = 0;
			m_uVolumeWithCD = 0;
			m_uVolumeEntriesNo = 0;
			m_uEntriesNumber = 0;
			m_uSize = 0;  
			m_uOffset = 0;
		}
		bool CheckIfOK_1()
		{
			return (m_uEndOffset >= m_uOffset + m_uSize);
		}
		ZIP_SIZE_TYPE CalculateBytesBeforeZip()
		{
			return m_uEndOffset - m_uSize - m_uOffset;
		}
		bool CheckIfOK_2()
		{
			return (m_uSize || !m_uEntriesNumber) && (m_uEntriesNumber || !m_uSize);
		}
		void DiskChange(ZIP_VOLUME_TYPE uCurrentVolume)
		{
			m_uLastVolume = uCurrentVolume;
			if (m_uEntriesNumber)
			{
				m_uVolumeEntriesNo = 0;	
			}
			else
			{
				m_uVolumeWithCD = m_uLastVolume;
				m_uOffset = 0;
			}
		}
		
		/**
			Gets a value indicating if the current archive properties requires the Zip64 format.
			
			\return
				\c true, if the Zip64 is needed; \c false otherwise.
		
			\see
				<a href="kb">0610051629</a>
		 */
		bool NeedsZip64() const
		{
			return m_uLastVolume >= USHRT_MAX || m_uVolumeWithCD >= USHRT_MAX || m_uVolumeEntriesNo >= USHRT_MAX || m_uEntriesNumber >= USHRT_MAX || m_uSize >= UINT_MAX || m_uOffset >= UINT_MAX;
		}

		CZipAutoBuffer m_pszComment;	///< The global archive comment.		

		/**
			The case-sensitivity of CZipCentralDir::m_pFindArray sorting.
		*/
		bool m_bCaseSensitive;

		/**
			The value set with the CZipCentralDir::EnableFindFast method.
		*/
		bool m_bFindFastEnabled;	

	private:
		/**
			The method used in string comparisons. It is set depending on the current case-sensitivity.
		*/
		ZIPSTRINGCOMPARE m_pCompare;
		int m_iReference;
#ifdef ZIP_ARCHIVE_USE_LOCKING
		ZipArchiveLib::CZipMutex m_mutex;
#endif
	};

	CZipCentralDir();
	virtual ~CZipCentralDir();

	static char m_gszSignature[]; ///< The End of Central Directory Record signature.
	static char m_gszSignature64Locator[]; ///< The Zip64 End of Central Directory Locator signature.
	CZipFileHeader* m_pOpenedFile;	///< It points to the currently opened file or it is \c NULL, if no file is opened.

	/**
		Initializes the object.

		\param pStorage
			The current storage to use.

		\param pCallbacks
			The current callbacks provider.

		\param pStringSettings
			The current string settings.

		\param pSource
			If not \c NULL, then it specifies the central directory for sharing.

		\note
			Throws exceptions.
			
	*/
	void Init(CZipStorage* pStorage, ZipArchiveLib::CZipCallbackProvider* pCallbacks, CZipStringStoreSettings* pStringSettings, CZipCentralDir* pSource = NULL);

	/**
		Reads the central directory from the archive.

		\param bExhaustiveRead
			\c true, if the exhaustive read should be performed, \c false otherwise.

		\note
			Throws exceptions.

		\see
			CZipArchive::SetExhaustiveRead
	*/
	void Read(bool bExhaustiveRead);

	/**
		Opens the file with the given index.

		\param uIndex
			A zero-based index of the file to open.

		\note
			Throws exceptions.
	*/
	void OpenFile(ZIP_INDEX_TYPE uIndex);

	/**	
		Tests if the given file index is valid.

		\param	uIndex
			A zero-based index to test.

		\return
			\c true, if the file with the given index exists inside the archive; \c false otherwise.
	*/
	bool IsValidIndex(ZIP_INDEX_TYPE uIndex)const;

	/**
		Removes the file header from the central directory.

		\param	pHeader
			The header to remove.

		\param uIndex
			The index of the header to remove. Use \c ZIP_FILE_INDEX_UNSPECIFIED, if the index is not known.

		\param	bShift 
			If \c true, the data inside the archive is moved over the hole created after removing the file.
			If \c false, the unused area inside the archive remains.

		\note
			Throws exceptions.
	*/
	void RemoveFile(CZipFileHeader* pHeader, ZIP_INDEX_TYPE uIndex = ZIP_FILE_INDEX_UNSPECIFIED, bool bShift = true);


    /**
       Removes last file from the central directory.       

	   \param	pHeader
			The header to remove.

		\param uIndex
			The index of the header to remove. Use \c ZIP_FILE_INDEX_UNSPECIFIED, if the index is not known.
	   
     */
	void RemoveLastFile(CZipFileHeader* pHeader = NULL, ZIP_INDEX_TYPE uIndex = ZIP_FILE_INDEX_UNSPECIFIED);

	/**
		Removes all files.

		\note
			Throws exceptions.
	*/
	void RemoveAll();

	/**
		Closes the central directory.
	*/
	void Close();

	/**
		Adds a new file to the central directory.

		\param	header
			Used as a template for the data stored inside the archive.

		\param uReplaceIndex
			The index of the file to be replaced. Use \c ZIP_FILE_INDEX_UNSPECIFIED, if the index is not known.

		\param iLevel
			The compression level.
	
		\param bRichHeaderTemplateCopy
			\c true, if copy crc and sizes values from \a header;  \c false otherwise.

		\return
			The new header.

		\note
			Throws exceptions.
	*/	
	CZipFileHeader* AddNewFile(const CZipFileHeader & header, ZIP_INDEX_TYPE uReplaceIndex, int iLevel, bool bRichHeaderTemplateCopy = false);

	/**
		Removes physically the central directory from the archive.

		\note
			Throws exceptions.
	*/
	void RemoveFromDisk();

	/**
		Gets the central directory size.
		\param	bWhole
			If \c true, include the size of the file headers.
			If \c false, the size of the file headers is not included.

		\return
			The size of the central directory.

		\see
			CZipArchive::GetCentralDirSize
	*/
	ZIP_SIZE_TYPE GetSize(bool bWhole = false) const;

	/**
		Closes a file opened for reading inside the archive.
		\param skipCheckingDataDescriptor
			If \c true, the data descriptor that is located after the compressed data in the archive is checked for validity.
			Set this value to \c false after closing the file after an exception was thrown.

		\note
			Throws exceptions.
	*/
	void CloseFile(bool skipCheckingDataDescriptor = false);

	/**
		Closes a file opened for reading inside the archive.

		\note
			Throws exceptions.
	*/
	void CloseNewFile();

	/**
		Writes the central directory to the archive.
		
		\note
			Throws exceptions.
	*/
	void Write();
	
	/**
		Enables Find Fast.

		\see
			CZipArchive::EnableFindFast
	*/
	void EnableFindFast(bool bEnable, bool bCaseSensitive);

	/**
		Searches for the file.

		\see
			CZipArchive::FindFile
	*/
	ZIP_INDEX_TYPE FindFile(LPCTSTR lpszFileName, bool bCaseSensitive, bool bSporadically, bool bFileNameOnly);


	/**
		Gets the Find Fast index.

		\see
			CZipArchive::GetFindFastIndex
	*/
	ZIP_INDEX_TYPE GetFindFastIndex(ZIP_INDEX_TYPE uFindFastIndex)const
	{
		if (!IsValidIndex(uFindFastIndex) || !m_pInfo->m_bFindFastEnabled)
			return ZIP_FILE_INDEX_NOT_FOUND;
		
		return (*m_pFindArray)[(ZIP_ARRAY_SIZE_TYPE)uFindFastIndex]->m_uIndex;
	}

	/**
		Points to CZipArchive::m_storage.
	*/
	CZipStorage* m_pStorage;

	/**
		Points to the current callback provider.
	*/
	ZipArchiveLib::CZipCallbackProvider* m_pCallbacks;
		
	/**
		Gets the information about the file with the given index.

		\see
			CZipArchive::operator[](ZIP_INDEX_TYPE)
	*/
	CZipFileHeader* operator[](ZIP_INDEX_TYPE uIndex)
	{
		return (*m_pHeaders)[(ZIP_ARRAY_SIZE_TYPE)uIndex];
	}

	/**
		Gets the information about the file with the given index.

		\see
			CZipArchive::operator[](ZIP_INDEX_TYPE) const
	*/
	const CZipFileHeader* operator[](ZIP_INDEX_TYPE uIndex) const
	{
		return (*m_pHeaders)[(ZIP_ARRAY_SIZE_TYPE)uIndex];
	}

	/**
		Gets the number of files in the archive.

		\return
			The number of files in the archive.
			
	*/
	ZIP_ARRAY_SIZE_TYPE GetCount() const
	{
		return m_pHeaders == NULL ? 0 : m_pHeaders->GetSize();
	}
	
	/**
		Sets the global comment.

		\see
			CZipArchive::SetGlobalComment
	*/
	void SetComment(LPCTSTR lpszComment);

	/**
		Gets the global comment.

		\see
			CZipArchive::GetGlobalComment
	*/
	void GetComment(CZipString& szComment) const
	{
		ZipCompatibility::ConvertBufferToString(szComment, m_pInfo->m_pszComment, m_pStringSettings->m_uCommentCodePage);
	}
	
	/**
		Sets the file comment.

		\see
			CZipArchive::SetFileComment
	*/
	bool SetFileComment(ZIP_INDEX_TYPE uIndex, LPCTSTR lpszComment);


	/**
		Finds the index of the file with the given name.

		\param	lpszFileName
			The name of the file to find.

		\return
			The index in CZipCentralDir::m_pFindArray with the corresponding CZipFindFast structure
			or \c ZIP_FILE_INDEX_NOT_FOUND, if there is no such file with the given name.

		\see
			CZipArchive::FindFile
	*/
	ZIP_INDEX_TYPE FindFileNameIndex(LPCTSTR lpszFileName) const;

	/**
		Gets the information about the central directory.
			
		\see
			CZipArchive::GetCentralDirInfo
	*/
	void GetInfo(CInfo& info) const {info = *m_pInfo;}

	/**
		Gets a value indicating whether the Find Fast feature is enabled or not.

		\return
			The value of CInfo::m_bFindFastEnabled.
	*/
	bool IsFindFastEnabled(){return m_pInfo->m_bFindFastEnabled;}
	
	/**
		Rebuilds the CZipCentralDir::m_pFindArray array.
	*/
	void RebuildFindFastArray()
	{
		if (m_pInfo->m_bFindFastEnabled)
			BuildFindFastArray(m_pInfo->m_bCaseSensitive);
	}

	/**
		The current string store settings.

		\see
			SetStringStoreSettings
	*/
	CZipStringStoreSettings* m_pStringSettings;

	/**
		Consistency checks to ignore. Can be one or more of the CZipArchive::ConsistencyCheck values.
		\see 
			CZipArchive::SetIgnoredConsistencyChecks
	*/
	int m_iIgnoredChecks;

	/**
		Checks if the specified consistency check should be performed or not.
		\param iLevel
			The level to check. Can be one or more of CZipArchive::ConsistencyCheck values.

		\return
			\c true, if the specified check should be performed; \c false otherwise.

		\see
			m_iIgnoredChecks
	*/
	bool IsConsistencyCheckOn(int iLevel)
	{
		// check, if not ignored
		return (m_iIgnoredChecks & iLevel) == 0;
	}


protected:


#if _MSC_VER > 1000
	#pragma warning( push )
	#pragma warning (disable : 4702) // unreachable code
#endif
	
	/**
		The method used in comparison when sorting headers.
	*/
	static int CompareHeaders(const void *pArg1, const void *pArg2)
	{
		CZipFileHeader* pw1 = *(CZipFileHeader**)pArg1;
		CZipFileHeader* pw2 = *(CZipFileHeader**)pArg2;
		if (pw1 == pw2)
			return 0;

		if (pw1->m_uVolumeStart == pw2->m_uVolumeStart)
		{
			if (pw1->m_uOffset < pw2->m_uOffset)
				return -1;
			else if (pw1->m_uOffset > pw2->m_uOffset)
				return 1;
				ASSERT(FALSE);


			// two files with the same offsets in the same volume???
			CZipException::Throw(CZipException::badZipFile);
			return 0; // just for the compiler comfort (and discomfort of another)
		}
		else if (pw1->m_uVolumeStart < pw2->m_uVolumeStart)
			return -1;
		else // if (pw1->m_uVolumeStart > pw2->m_uVolumeStart)
			return 1;		
	}

#if _MSC_VER > 1000
	#pragma warning( pop )
#endif

	static int CompareFindFastCollate(const void* pArg1, const void* pArg2)
	{
		CZipFindFast* pHeader1 = *(CZipFindFast**)pArg1;
		CZipFindFast* pHeader2 = *(CZipFindFast**)pArg2;		
		return pHeader1->m_pHeader->GetFileName().Collate(pHeader2->m_pHeader->GetFileName());
	}

	static int CompareFindFastCollateNoCase(const void* pArg1, const void* pArg2)
	{
		CZipFindFast* pHeader1 = *(CZipFindFast**)pArg1;
		CZipFindFast* pHeader2 = *(CZipFindFast**)pArg2;		
		return pHeader1->m_pHeader->GetFileName().CollateNoCase(pHeader2->m_pHeader->GetFileName());
	}

	/**
		Holds the information about all the files inside the archive.

		\see
			CZipFileHeader
	*/
	CZipArray<CZipFileHeader*>* m_pHeaders;


	/**
		Builds the CZipCentralDir::m_pFindArray array.
	*/
	void BuildFindFastArray( bool bCaseSensitive );

	void ClearFindFastArray()
	{
		ZIP_ARRAY_SIZE_TYPE uCount = m_pFindArray->GetSize();
		for (ZIP_ARRAY_SIZE_TYPE i = 0; i < uCount; i++)
			delete (*m_pFindArray)[i];
		m_pFindArray->RemoveAll();
	}

	/**
		The Find Fast array.

		\see
			CZipFindFast
		\see
			CInfo::m_bFindFastEnabled
		\see
			CZipArchive::FindFile
	*/
	CZipArray<CZipFindFast*>* m_pFindArray;


	/**
		The method used in comparison involving the Find Fast feature.

		\param	lpszFileName
			The name of the file.

		\param	uIndex
			The index from the CZipCentralDir::m_pFindArray array.

		\return 
			The return value has the following meaning:
			- 0 if the filenames are the same
			- < 0 if the filename stored in the array is less than \a lpszFileName
			- > 0 if the filename stored in the array is greater than \a lpszFileName
	*/
	int CompareElement(LPCTSTR lpszFileName, ZIP_INDEX_TYPE uIndex) const
	{
		return ((*m_pFindArray)[(ZIP_ARRAY_SIZE_TYPE)uIndex]->m_pHeader->GetFileName().*(m_pInfo->m_pCompare))(lpszFileName);
	}

	/**
		Inserts a new CZipFindFast element to the CZipCentralDir::m_pFindArray array
		Initializes the CZipFindFast object with \a pHeader and \a uIndex values.

		\param pHeader
			The element to insert.

		\param uIndex
			The original index of \a pHeader in the central directory. 
			If set to \c ZIP_FILE_INDEX_UNSPECIFIED, it is assumed to be the last element.

		\return
			The index in the CZipCentralDir::m_pFindArray array.
	*/
	ZIP_INDEX_TYPE InsertFindFastElement(CZipFileHeader* pHeader, ZIP_INDEX_TYPE uIndex);


	/**
		The central directory information.

		\see
			CInfo
	*/
	CInfo* m_pInfo;

	/**
		Reads file headers from the archive.

		\param bExhaustiveRead
			\c true, if the exhaustive read should be performed, \c false otherwise.

		\note
			Throws exceptions.

		\see
			CZipArchive::SetExhaustiveRead
	*/
	void ReadHeaders(bool bExhaustiveRead);

	/**
		Frees the memory allocated for the CZipFileHeader structures.
	*/
	void RemoveHeaders();

	/**
		Removes data descriptors from a segmented archive that turned out to be one-segment only.
		It is not called for encrypted files.

		\param	bFromBuffer
			If \c true, removes from the write buffer in memory otherwise from the file on the disk.

		\return
			\c false, if the file mapping to memory was not successful
			(can happen only when \a bFromBuffer is \c false); \c true otherwise.
		
		\note
			Throws exceptions.
	*/
	bool RemoveDataDescr(bool bFromBuffer);

	/**
		Writes the file headers to the archive.
		\note
			Throws exceptions.
	*/
	void WriteHeaders(bool bOneDisk);

	/**
		Writes the End of Central Directory Record.

		\return
			The size of the record.

		\note
			Throws exceptions.
	*/
	void WriteCentralEnd();


	/**
		Throws an exception with the given code.
	*/
	void ThrowError(int err) const;

	/**
		Creates data that can be shared between different archive objects.

		\see 
			DestroySharedData
	*/
	void CreateSharedData();

	/**
		Destroys data shared between different archive objects, if the usage reference count
		of the data is zero.

		\note
			Throws exceptions.

		\see 
			CreateSharedData
	*/
	void DestroySharedData();

#ifdef ZIP_ARCHIVE_USE_LOCKING
	/**
		Locks the access to the shared data.

		\note
			Throws exceptions.

		\see
			UnlockAccess
		\see 
			CreateSharedData
	*/
	void LockAccess()
	{

		ASSERT(m_pInfo);
		m_pInfo->m_mutex.Lock();
	}

	/**
		Unlocks the access to the shared data.

		\note
			Throws exceptions.

		\see
			LockAccess
		\see 
			CreateSharedData
	*/
	void UnlockAccess()
	{
		if (m_pInfo)
			m_pInfo->m_mutex.Unlock();
	}
#endif

};

#if (_MSC_VER > 1000) && (defined ZIP_HAS_DLL)
	#pragma warning (pop)	
#endif


#endif // !defined(ZIPARCHIVE_ZIPCENTRALDIR_DOT_H)
