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
* \file ZipFileHeader.h
* Includes the CZipFileHeader class.
*
*/

#if !defined(ZIPARCHIVE_ZIPFILEHEADER_DOT_H)
#define ZIPARCHIVE_ZIPFILEHEADER_DOT_H

#if _MSC_VER > 1000
#pragma once
#endif

#include "ZipExport.h"
#include "ZipStorage.h"
#include "ZipAutoBuffer.h"
#include "sys/types.h"
#include "ZipCompatibility.h"
#include "ZipCollections.h"
#include "ZipExtraField.h"
#include "ZipStringStoreSettings.h"
#include "ZipCryptograph.h"


class CZipCentralDir;

/**
	Represents a single file stored in a zip archive.
*/
class ZIP_API CZipFileHeader  
{
	friend class CZipCentralDir;
	friend class CZipArchive;
public:	
	CZipFileHeader();
	CZipFileHeader(const CZipFileHeader& header)
	{
		*this = header;
	}
	CZipFileHeader& operator=(const CZipFileHeader& header);
	virtual ~CZipFileHeader();

	/**
		Predicts the filename size after conversion using the current filename code page.

		\return 
			The number of characters not including a terminating \c NULL character.
	*/
	int PredictFileNameSize() const 
	{
		if (m_pszFileNameBuffer.IsAllocated())
			return m_pszFileNameBuffer.GetSize();
		CZipAutoBuffer buffer;
		ConvertFileName(buffer);
		return buffer.GetSize();
	}

	/**
		Gets the comment size.

		\return
			The number of characters in the comment not including a terminating \c NULL character.
	*/
	WORD GetCommentSize() const {return (WORD)m_pszComment.GetSize();}


	/**
		Gets the filename. If necessary, performs the conversion using the current filename code page.
		Caches the result of conversion for faster access the next time.

		\param bClearBuffer
			If \c true, releases the internal buffer after performing the filename conversion.
			If \c false, the internal buffer is not released and both representations of the 
			filename are kept in memory (converted and not converted). This takes more memory, but the 
			conversion does not take place again when the central directory is written back to the archive.

		\return
			The converted filename.

		\see
			<a href="kb">0610051525</a>
		\see 
			GetStringStoreSettings
		\see
			CZipStringStoreSettings::m_uNameCodePage
	*/
	CZipString& GetFileName(bool bClearBuffer = true);

	/**
		Sets the filename.

		\param	lpszFileName
			The filename to set.
	*/
	void SetFileName(LPCTSTR lpszFileName);

	/**
		Gets the file comment.

		\return
			The file comment.
	*/
	CZipString GetComment() const;

	/**
		Sets the file comment.

		\param lpszComment
			The file comment.
	*/
	void SetComment(LPCTSTR lpszComment);

	/**
		Gets a value indicating whether the data descriptor is present or not.

		\return
			\c true, if the data descriptor is present; \c false otherwise.
	*/
	bool IsDataDescriptor()const {	return (m_uFlag & (WORD) 8) != 0;}

	/**
		Gets the data descriptor size as it is required for the current file.
		Takes into account various factors, such as the archive segmentation type,
		encryption and the need for the Zip64 format.

		\param pStorage
			The storage to test for segmentation type.

		\return 
			The required data descriptor size in bytes.
	*/
	WORD GetDataDescriptorSize(const CZipStorage* pStorage) const
	{
		return GetDataDescriptorSize(NeedsSignatureInDataDescriptor(pStorage));
	}

	/**
		Gets the data descriptor size as it is required for the current file.
		Takes into account various factors, such as the need for the data descriptor signature
		or for the Zip64 format.

		\param bConsiderSignature
			\c true, if the data descriptor signature is needed; \c false otherwise.

		\return 
			The required data descriptor size in bytes.
	*/
	WORD GetDataDescriptorSize(bool bConsiderSignature = false) const;
	

	/**
		Gets the size of the compressed data.
	
		\param bUseLocal
			If \c true, uses #m_uLocalComprSize; otherwise uses #m_uComprSize;
	
		\param bReal
			If \c true, the returned value does not include the encrypted information size, only the data size.
			If \c false, the encrypted information size is added (you should not use this value 
			when the file exists in the archive).
	
		\return
			The compressed data size in bytes.

		\see
			GetEncryptedInfoSize
	 */
	ZIP_SIZE_TYPE GetDataSize(bool bUseLocal = false, bool bReal = true) const
	{
		ZIP_SIZE_TYPE uSize = bUseLocal ? m_uLocalComprSize : m_uComprSize;
		DWORD uEncrSize = GetEncryptedInfoSize();
		return bReal ? (uSize - uEncrSize) : (uSize + uEncrSize);
	}

	/**
		Gets the encrypted information size. The returned value depends on the used encryption method.

		\return
			The  encrypted information size in bytes.
	*/
	DWORD GetEncryptedInfoSize() const
	{
		return CZipCryptograph::GetEncryptedInfoSize(m_uEncryptionMethod);
	}

	/**
		Gets the total size of the structure in the central directory.

		\return
			The total size in bytes.
	*/
	DWORD GetSize()const;

	/**
		Gets the local header size. Before calling this method, the local information must be up-to-date
		(see <a href="kb">0610242128|local</a> for more information).

		\param bReal
			If \c true, uses the real local filename size.
			If \c false, predicts the filename size. 			

		\return
			The local header size in bytes.
	*/
	DWORD GetLocalSize(bool bReal) const;	

	/**
		Gets a value indicating if the compression is efficient.

		\return 
			\c true if the compression is efficient; \c false if the file should be 
			stored without the compression instead.
	*/
	bool CompressionEfficient()
	{
		ZIP_SIZE_TYPE uBefore = m_uUncomprSize;
		// ignore the length of encryption info 
		ZIP_SIZE_TYPE uAfter = GetDataSize(false, true);
		return  uAfter <= uBefore;
	}

	/**
		Gets the compression ratio.

		\return
			The compression ratio of the file.
	*/
	float GetCompressionRatio()
	{
#if _MSC_VER >= 1300 || !defined(_ZIP64)
		return m_uUncomprSize ? ((float)m_uComprSize * 100 ) / m_uUncomprSize: 0;
#else
		return m_uUncomprSize ? ((float)(__int64)(m_uComprSize) / (float)(__int64)m_uUncomprSize) * 100: 0;
#endif
	}

	/**
		Sets the file modification date.

		\param ttime
			The date to set. If this value is incorrect, the date defaults to January 1, 1980.

		\see
			GetTime
	*/
	void SetTime(const time_t& ttime);

	/**
		Gets the file modification time.

		\return
			The modification time.

		\see
			SetTime
	*/
	time_t GetTime()const;

	/**
		Gets the file system compatibility.
		External software can use this information e.g. to determine end-of-line 
		format for text files etc.
		The ZipArchive Library uses it to perform a proper file attributes conversion.

		\return
			The file system compatibility. Can be one of the ZipCompatibility::ZipPlatforms values.
		\see
			CZipArchive::GetSystemComatibility
		\see
			ZipPlatform::GetSystemID
	*/
	int GetSystemCompatibility()const
	{
		return (m_uVersionMadeBy & 0xFF00) >> 8;
	}

	/**
		Gets the file attributes.

		\return
			The file attributes, converted if necessary to be compatible with the current system.

		\note 
			Throws an exception, if the archive system or the current system 
			is not supported by the ZipArchive Library.			

		\see
			GetOriginalAttributes
	*/
	DWORD GetSystemAttr();

	/**
		Gets the file attributes exactly as they are stored in the archive.

		\return 
			The file attributes as they are stored in the archive.
			No conversion is performed.

		\note
			The attributes for Linux are stored shifted left by 16 bits in this field.
		\see 
			GetSystemAttr
	*/
	DWORD GetOriginalAttributes() const {return m_uExternalAttr;}

	/**
		Gets a value indicating whether the file represents a directory or not.
		This method checks the file attributes. If the attributes value is zero, 
		the method checks for the presence of a path 
		separator at the end of the filename. If the path separator is present, 
		the file is assumed to be a directory.

		\return
			\c true, if the file represents a directory; \c false otherwise.		
	*/
	bool IsDirectory();

	
	/**
		Gets the string store settings for the file.
	
		\return
			The string store settings.

		\see
			<a href="kb">0610051525</a>
		\see
			CZipArchive::GetStringStoreSettings
	 */
	CZipStringStoreSettings GetStringStoreSettings()
	{
		return m_stringSettings;
	}

	/**
		Gets a value indicating if the file is encrypted or not.
		If the file is encrypted, you need to set the password with the 
		CZipArchive::SetPassword method before decompressing the file.

		\return
			\c true if the file is encrypted; \c false otherwise.

		\see
			CZipArchive::SetPassword
	*/
	bool IsEncrypted()const {	return m_uEncryptionMethod != CZipCryptograph::encNone;}

	/**
		Gets the encryption method of the file.

		\return
			The file encryption method. Can be one of the CZipCryptograph::EncryptionMethod values.
	*/
	int GetEncryptionMethod() const {return m_uEncryptionMethod;}

	/**
		Gets a value indicating if the file is encrypted using WinZip AES encryption method or not.

		\return
			\c true, if the file is encrypted using WinZip AES encryption method; \c false otherwise.
	*/
	bool IsWinZipAesEncryption() const
	{
		return CZipCryptograph::IsWinZipAesEncryption(m_uEncryptionMethod);
	}

	/**
		Gets an approximate file compression level.

		\return 
			The compression level. May not be the real value used when compressing the file.
	*/
	int GetCompressionLevel() const;

	/**
		Returns the value indicating whether the current CZipFileHeader object has the time set or not.

		\return
			\c true, if the time is set; \c false otherwise.
	*/
	bool HasTime()
	{
		return m_uModTime != 0 || m_uModDate != 0;
	}

	static char m_gszSignature[];		///< The central file header signature.
	static char m_gszLocalSignature[];	///< The local file header signature.
	WORD m_uVersionMadeBy;				///< The "made by" version and the system compatibility.
	WORD m_uVersionNeeded;				///< The version needed to extract the file.
	WORD m_uFlag;						///< A general purpose bit flag.
	WORD m_uMethod;						///< The compression method. Can be one of the CZipCompressor::CompressionMethod values.
	WORD m_uModTime;					///< The file last modification time.
	WORD m_uModDate;					///< The file last modification date.
	DWORD m_uCrc32;						///< The crc-32 value.
	ZIP_SIZE_TYPE m_uComprSize;			///< The compressed size.
	ZIP_SIZE_TYPE m_uUncomprSize;		///< The uncompressed size.
	ZIP_VOLUME_TYPE m_uVolumeStart;		///< The volume number at which the compressed file starts.
	WORD m_uInternalAttr;				///< Internal file attributes.
	ZIP_SIZE_TYPE m_uLocalComprSize;	///< The compressed size written in the local header.
	ZIP_SIZE_TYPE m_uLocalUncomprSize;	///< The uncompressed size written in the local header.
	ZIP_SIZE_TYPE m_uOffset;			///< Relative offset of the local header with respect to CZipFileHeader::m_uVolumeStart.
	CZipExtraField m_aLocalExtraData;	///< The local extra field. Do not modify after you have started compressing the file.
	CZipExtraField m_aCentralExtraData; ///< The central extra field.
protected:
	DWORD m_uExternalAttr;				///< External file attributes.
 	WORD m_uLocalFileNameSize;			///< The local filename length.
	BYTE m_uEncryptionMethod;			///< The file encryption method. Can be one of the CZipCryptograph::EncryptionMethod values.
	bool m_bIgnoreCrc32;				///< A value indicating whether to ignore Crc32 checking or not. 

	

	/**
		Sets the file system compatibility.

		\param	iSystemID
			The file system compatibility. Can be one of the ZipCompatibility::ZipPlatforms values.

		\see
			GetSystemCompatibility
	*/
	void SetSystemCompatibility(int iSystemID)
	{
		m_uVersionMadeBy &= 0x00FF;
		m_uVersionMadeBy |= (WORD)(iSystemID << 8);
	}

	/**
		Sets the file attributes.
		To set the attributes of this structure use the CZipArchive::SetFileHeaderAttr method.

		\param	uAttr
			The attributes to set.

		\note
			Throws exceptions, if the archive system or the current system 
			is not supported by the ZipArchive Library.

		\see
			CZipArchive::SetFileHeaderAttr
		\see	
			GetSystemAttr
	*/
	void SetSystemAttr(DWORD uAttr);
	
	/**
		Prepares the filename for writing to the archive.
	*/
	void PrepareFileName()
	{
		if (m_pszFileNameBuffer.IsAllocated() || m_pszFileName == NULL)
			return;
		ConvertFileName(m_pszFileNameBuffer);
	}

	/**
		Validates an existing data descriptor after file decompression.

		\param pStorage
			The storage to read the data descriptor from.

		\return
			\c true, if the data descriptor is valid; \c false otherwise.
	*/
	bool CheckDataDescriptor(CZipStorage* pStorage) const;


	/**
		Prepares the data for writing when adding a new file. When Zip64 extensions are required for this file, 
		this method adds Zip64 extra data to #m_aLocalExtraData.

		\param	iLevel
			The compression level.

		\param bSegm
			Set to \c true, if the archive is segmented; \c false otherwise.
	*/
	void PrepareData(int iLevel, bool bSegm);

	/**	
		Writes the local file header to the \a pStorage. 
		The filename and extra field are the same as those that will be stored in the central directory.

		\param	pStorage
			The storage to write the local file header to.

		\note
			Throws exceptions.
	*/
	void WriteLocal(CZipStorage *pStorage);

	/**
		Reads the local file header from an archive and validates the read data.

		\param	centralDir
			The current central directory.

		\return
			\c true, if read data is consistent; \c false otherwise.

		\note
			Throws exceptions.

		\see 
			CZipArchive::SetIgnoredConsistencyChecks
	*/
	bool ReadLocal(CZipCentralDir& centralDir);

	/**
		Writes the central file header to \a pStorage.

		\param	pStorage
			The storage to write the central file header to.

		\return
			The size of the file header.

		\note
			Throws exceptions.
	*/
	DWORD Write(CZipStorage *pStorage);

	/**
		Reads the central file header from \a pStorage and validates the read data.

		\param	centralDir
			The current central directory.

		\param bReadSignature
			\c true, if the the central header signature should be read; \c false otherwise.


		\return
			\c true, if the read data is consistent; \c false otherwise.

		\note
			Throws exceptions.
	*/
	bool Read(CZipCentralDir& centralDir, bool bReadSignature);


	/**
		Validates the member fields lengths. 
		The tested fields are: filename, extra fields and comment.

		\return
			\c false, if any of the lengths exceeds the allowed value.
	*/
	bool CheckLengths(bool local) const
	{
		if (m_pszComment.GetSize() > USHRT_MAX || m_pszFileNameBuffer.GetSize() > USHRT_MAX)
			return false;
		else if (local)
			return m_aLocalExtraData.Validate();
		else
			return m_aCentralExtraData.Validate();
	}

	/**
		Writes the Crc32 to \a pBuf.

		\param pBuf
			The buffer to write the Crc32 to. Must have be of at least 4 bytes size.
	*/
	void WriteCrc32(char* pBuf) const;
	
	
	/**
		Gets a value indicating whether the file needs the data descriptor.
		The data descriptor is needed when a file is encrypted or the Zip64 format needs to be used.

		\return 
			\c true, if the data descriptor is needed; \c false otherwise.
	*/
	bool NeedsDataDescriptor() const;
	

	/**
		Writes the data descriptor.

		\param pDest
			The buffer to receive the data.

		\param bLocal
			Set to \c true, if the local sizes are used; \c false otherwise.
	*/
	void WriteSmallDataDescriptor(char* pDest, bool bLocal = true);

	/**
		Writes the data descriptor taking into account the Zip64 format.

		\param pStorage
			The storage to write the data descriptor to.
	*/
	void WriteDataDescriptor(CZipStorage* pStorage);

	bool NeedsSignatureInDataDescriptor(const CZipStorage* pStorage) const
	{
		return pStorage->IsSegmented() != 0 || IsEncrypted();
	}

	/**
		Updates the local header in the archive after is has already been written.

		\param pStorage
			The storage to update the data descriptor in.
	*/
	void UpdateLocalHeader(CZipStorage* pStorage);

	/**
		Verifies the central header signature.

		\param buf
			The buffer that contains the signature to verify.

		\return 
			\c true, if the signature is valid; \c false otherwise.
	*/
	static bool VerifySignature(CZipAutoBuffer& buf)
	{
		return memcmp(buf, m_gszSignature, 4) == 0;
	}

	/**
		Updates the general purpose bit flag. 

		\param bSegm
			\c true, if the current archive is a segmented archive; \c false otherwise.
	*/
	void UpdateFlag(bool bSegm)
	{
		if (bSegm || m_uEncryptionMethod == CZipCryptograph::encStandard)
			m_uFlag  |= 8; // data descriptor present

		if (IsEncrypted())
			m_uFlag  |= 1;		// encrypted file		
	}


private:

	/**
		Sets the "made by" version.

		\param uVersion
			The version to set.
	*/
	void SetVersion(WORD uVersion)
	{
		if ((m_uVersionMadeBy & 0x00FF) != (uVersion & 0x00FF)) 
		{
			m_uVersionMadeBy &= 0xFF00;
			m_uVersionMadeBy |= (WORD)(uVersion & 0x00FF);
		}
	}

	void ConvertFileName(CZipAutoBuffer& buffer) const;
	void ConvertFileName(CZipString& szFileName) const;

	void ClearFileName()
	{
		if (m_stringSettings.m_bStoreNameInExtraData)
			// we are keeping m_pszFileName, clear the buffer, we need the original, when writing extra header and when accessing the filename
			m_pszFileNameBuffer.Release();
		else if (m_pszFileName != NULL)
		{
			delete m_pszFileName;
			m_pszFileName = NULL;
		}
	}

	void GetCrcAndSizes(char* pBuffer)const;

	bool NeedsZip64() const
	{
		return m_uComprSize >= UINT_MAX || m_uUncomprSize >= UINT_MAX || m_uVolumeStart >= USHRT_MAX || m_uOffset >= UINT_MAX;
	}


	void OnNewFileClose(CZipStorage* pStorage)
	{
		UpdateLocalHeader(pStorage);
		WriteDataDescriptor(pStorage);
		pStorage->Flush();
	}

	CZipAutoBuffer m_pszFileNameBuffer;

	CZipString* m_pszFileName;

	CZipStringStoreSettings m_stringSettings;

	CZipAutoBuffer m_pszComment;
};

#endif // !defined(ZIPARCHIVE_ZIPFILEHEADER_DOT_H)
