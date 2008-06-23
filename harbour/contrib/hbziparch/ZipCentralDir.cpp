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


#include "stdafx.h"
//#include <string>
#include "ZipCentralDir.h"
#include "ZipArchive.h"
#include "ZipFileMapping.h"
#include "ZipPlatform.h"
#include "BytesWriter.h"


#define CENTRAL_DIR_END_SIZE	22


using namespace ZipArchiveLib;

char CZipCentralDir::m_gszSignature[] = {0x50, 0x4b, 0x05, 0x06};
char CZipCentralDir::m_gszSignature64Locator[] = {0x50, 0x4b, 0x06, 0x07};



CZipCentralDir::CZipCentralDir()
{
	m_pInfo = NULL;
	m_pHeaders = NULL;
	m_pFindArray = NULL;
	m_pStorage = NULL;
	m_pOpenedFile = NULL;
	m_iIgnoredChecks = 0;
	m_pCallbacks = NULL;
}

void CZipCentralDir::Init(CZipStorage* pStorage, ZipArchiveLib::CZipCallbackProvider* pCallbacks, CZipStringStoreSettings* pStringSettings, CZipCentralDir* pSource)
{	
	m_pStorage = pStorage;
	m_pCallbacks = pCallbacks;
	m_pStringSettings = pStringSettings;
	m_pOpenedFile = NULL;	
	m_iIgnoredChecks = CZipArchive::checkIgnoredByDefault;
	// just in case
	DestroySharedData();
	if (pSource != NULL)
	{	
#ifdef ZIP_ARCHIVE_USE_LOCKING
		pSource->LockAccess();
#endif
		m_pInfo = pSource->m_pInfo;
		m_pInfo->m_iReference++;
		m_pHeaders = pSource->m_pHeaders;
		m_pFindArray = pSource->m_pFindArray;				
#ifdef ZIP_ARCHIVE_USE_LOCKING
		// points to the same object now
		UnlockAccess();
#endif

		m_pStorage->UpdateSegmMode(m_pInfo->m_uLastVolume);
		m_pStorage->m_uBytesBeforeZip = pSource->m_pStorage->m_uBytesBeforeZip;
		
	}
	else
		CreateSharedData();
}

CZipCentralDir::~CZipCentralDir()
{
	DestroySharedData();
}

void CZipCentralDir::Read(bool bExhaustiveRead)
{
	if (!m_pStorage)
	{
		ASSERT(FALSE);
		return;
	}
	m_pStorage->m_pFile->SeekToEnd();
	
	// maximum size of end of central dir record	
	m_pInfo->m_uEndOffset = (ZIP_SIZE_TYPE)m_pStorage->LocateSignature(m_gszSignature, 0xFFFF + CENTRAL_DIR_END_SIZE);	
	
	if (m_pInfo->m_uEndOffset == CZipStorage::SignatureNotFound)	
		ThrowError(CZipException::cdirNotFound);
		
	m_pStorage->m_pFile->Seek((ZIP_FILE_USIZE)(m_pInfo->m_uEndOffset + 4));
	CZipAutoBuffer buf(CENTRAL_DIR_END_SIZE);

	// we can skip the signature, we already know it is good - it was found 
	int uRead = m_pStorage->m_pFile->Read(buf, CENTRAL_DIR_END_SIZE - 4);
	if (uRead != CENTRAL_DIR_END_SIZE - 4)
		ThrowError(CZipException::badZipFile);	

	WORD uCommentSize;
	CBytesWriter::ReadBytes(m_pInfo->m_uLastVolume,		buf, 2);
	CBytesWriter::ReadBytes(m_pInfo->m_uVolumeWithCD,	buf + 2, 2);
	CBytesWriter::ReadBytes(m_pInfo->m_uVolumeEntriesNo,buf + 4, 2);
	CBytesWriter::ReadBytes(m_pInfo->m_uEntriesNumber,buf + 6, 2);
	CBytesWriter::ReadBytes(m_pInfo->m_uSize,			buf + 8, 4);
	CBytesWriter::ReadBytes(m_pInfo->m_uOffset,		buf + 12, 4);
	CBytesWriter::ReadBytes(uCommentSize,			buf + 16);
	buf.Release();		

	if (uCommentSize)
	{
		m_pInfo->m_pszComment.Allocate(uCommentSize);
		uRead = m_pStorage->m_pFile->Read(m_pInfo->m_pszComment, uCommentSize);
		if (uRead != uCommentSize)
			ThrowError(CZipException::badZipFile);
	}

	if ( m_pInfo->NeedsZip64() )
	{
		m_pStorage->m_pFile->Seek((ZIP_FILE_USIZE)(m_pInfo->m_uEndOffset));
		ULONGLONG uPosition = m_pStorage->LocateSignature(m_gszSignature64Locator, ZIP_SIZE_TYPE(-1));				
		if (uPosition != CZipStorage::SignatureNotFound)
			ThrowError(CZipException::noZip64);
		// when the zip64 locator is not found, try to treat this archive as normal
	}
	
	// if m_uLastVolume is not zero, it is enough to say that it is a multi-volume archive
	ASSERT((!m_pInfo->m_uLastVolume && (m_pInfo->m_uEntriesNumber == m_pInfo->m_uVolumeEntriesNo) && !m_pInfo->m_uVolumeWithCD) || m_pInfo->m_uLastVolume);

		m_pStorage->UpdateSegmMode(m_pInfo->m_uLastVolume);

	if (!m_pStorage->IsSegmented() && !m_pInfo->CheckIfOK_1())
		ThrowError(CZipException::badZipFile);

	if (m_pStorage->m_uBytesBeforeZip == 0 && m_pInfo->m_uLastVolume == 0)
		m_pStorage->m_uBytesBeforeZip = m_pInfo->CalculateBytesBeforeZip();

	if (!m_pInfo->CheckIfOK_2())
		ThrowError(CZipException::badZipFile);

	m_pInfo->m_bInArchive = true;
	m_pStorage->ChangeVolume(m_pInfo->m_uVolumeWithCD);

	if (!m_pInfo->m_uSize)
		return;

	ReadHeaders(bExhaustiveRead);
}


void CZipCentralDir::ThrowError(int err) const
{
	CZipException::Throw(err, m_pStorage->m_pFile->GetFilePath());
}


void CZipCentralDir::ReadHeaders(bool bExhaustiveRead)
{
	m_pStorage->Seek(m_pInfo->m_uOffset);
	RemoveHeaders(); //just in case
	for (ZIP_INDEX_TYPE i = 0; i < m_pInfo->m_uEntriesNumber; i++)
	{
		CZipFileHeader* pHeader = new CZipFileHeader;
		m_pHeaders->Add(pHeader);

		if (!pHeader->Read(*this, true))
			ThrowError(CZipException::badZipFile);
	}

	if (bExhaustiveRead)
		{
			ZIP_FILE_USIZE uPosition = m_pStorage->m_pFile->GetPosition();
			// different offset, or different parts
			if (uPosition != m_pInfo->m_uEndOffset || m_pStorage->IsSegmented() && m_pStorage->GetCurrentVolume() != m_pInfo->m_uLastVolume)
				for(;;)
				{
					CZipAutoBuffer buf(4);
					m_pStorage->Read(buf, 4, true);
					if (!CZipFileHeader::VerifySignature(buf))
						break;
					CZipFileHeader* pHeader = new CZipFileHeader;
					m_pHeaders->Add(pHeader);
					if (!pHeader->Read(*this, false))
						ThrowError(CZipException::badZipFile);			
				}
		}
	
	// this is necessary when removing data descriptors, CZipArchive::MakeSpaceForReplace, deleting, replacing or encrypting files
	// sort always, to yield the same results in requesting files by index regardless of the reason for opening
	m_pHeaders->Sort(CompareHeaders); 
	RebuildFindFastArray();
}

void CZipCentralDir::Close()
{
	m_pOpenedFile = NULL;
	DestroySharedData();
	// do not reference it anymore
	m_pInfo = NULL;
	m_pHeaders = NULL;
	m_pFindArray = NULL;
}

bool CZipCentralDir::IsValidIndex(ZIP_INDEX_TYPE uIndex)const
{
	return uIndex < (ZIP_INDEX_TYPE)m_pHeaders->GetSize() && uIndex != ZIP_FILE_INDEX_UNSPECIFIED;
}

void CZipCentralDir::OpenFile(ZIP_INDEX_TYPE uIndex)
{
	CZipFileHeader* pOpenedFile = (*this)[uIndex];
	m_pStorage->ChangeVolume(pOpenedFile->m_uVolumeStart);
	m_pStorage->Seek(pOpenedFile->m_uOffset);
	if (!pOpenedFile->ReadLocal(*this))
		ThrowError(CZipException::badZipFile);
	m_pOpenedFile = pOpenedFile;
}


void CZipCentralDir::CloseFile(bool skipCheckingDataDescriptor)
{
	if (!m_pOpenedFile)
		return;
	if (!skipCheckingDataDescriptor && IsConsistencyCheckOn(CZipArchive::checkDataDescriptor)
		&& !m_pOpenedFile->CheckDataDescriptor(m_pStorage))
		ThrowError(CZipException::badZipFile);
	m_pOpenedFile = NULL;
}

// add new header using the argument as a template
CZipFileHeader* CZipCentralDir::AddNewFile(const CZipFileHeader & header, ZIP_INDEX_TYPE uReplaceIndex, int iLevel, bool bRichHeaderTemplateCopy)
{
	// copy some of the template data
	m_pOpenedFile = NULL;
	ZIP_INDEX_TYPE uIndex;
	CZipFileHeader* pHeader = new CZipFileHeader();	
	try
	{
		pHeader->m_uMethod = header.m_uMethod;
		pHeader->m_uModDate = header.m_uModDate;
		pHeader->m_uModTime = header.m_uModTime;


		pHeader->m_uExternalAttr = header.m_uExternalAttr;
		pHeader->m_uLocalComprSize = header.m_uLocalComprSize;
		pHeader->m_uLocalUncomprSize = header.m_uLocalUncomprSize;
		if (header.m_pszFileName != NULL)
			pHeader->m_pszFileName = new CZipString(*header.m_pszFileName);

		pHeader->m_pszFileNameBuffer = header.m_pszFileNameBuffer;
		pHeader->m_pszComment = header.m_pszComment;
		pHeader->m_aLocalExtraData = header.m_aLocalExtraData;
		// local will be removed in a moment in PrepareData
		pHeader->m_aCentralExtraData = header.m_aCentralExtraData;
		pHeader->m_aCentralExtraData.RemoveInternalHeaders();	
		pHeader->SetSystemCompatibility(header.GetSystemCompatibility());
		pHeader->m_uEncryptionMethod = header.m_uEncryptionMethod;

		// current settings
		pHeader->m_stringSettings = *m_pStringSettings;

		// set only when adding a new file, not in PrepareData (which may be called under different circumstances)
		// we need the proper encryption method to be set already
		RemoveFromDisk();
		
		bool bReplace = IsValidIndex(uReplaceIndex);
			
		pHeader->PrepareData(iLevel, m_pStorage->IsSegmented() != 0);
		
		if (bRichHeaderTemplateCopy)
		{
			// call here, because PrepareData will zero them
			pHeader->m_uCrc32 = header.m_uCrc32;
			pHeader->m_uComprSize = header.m_uComprSize;
			pHeader->m_uUncomprSize = header.m_uUncomprSize;
		}
		// local extra field is updated if needed, so we can check the lengths
		if (!pHeader->CheckLengths(true))
			ThrowError(CZipException::tooLongData);

		// now that everything is all right, we can add the new file		
		if (bReplace)
		{
			CZipFileHeader* pfh = (*m_pHeaders)[(ZIP_ARRAY_SIZE_TYPE)uReplaceIndex];
			m_pStorage->Seek(pfh->m_uOffset);
			RemoveFile(pfh, uReplaceIndex, false);
			m_pHeaders->InsertAt((ZIP_ARRAY_SIZE_TYPE)uReplaceIndex, pHeader);
			m_pOpenedFile = pHeader;				
			uIndex = uReplaceIndex;		
		}
		else
		{
			uIndex = (ZIP_INDEX_TYPE)m_pHeaders->Add(pHeader);
			m_pOpenedFile = pHeader;
			m_pStorage->m_pFile->SeekToEnd();
		}
	}
	catch(...)
	{
		// otherwise it is added to the collection and will be auto-deleted
		if (pHeader != NULL && m_pOpenedFile == NULL)
			delete pHeader;
		throw;
	}

	if (m_pInfo->m_bFindFastEnabled)
		InsertFindFastElement(pHeader, uIndex); // GetCount > 0, because we have just added a header

	return pHeader;
}

void CZipCentralDir::SetComment(LPCTSTR lpszComment)
{
	ZipCompatibility::ConvertStringToBuffer(lpszComment, m_pInfo->m_pszComment, m_pStringSettings->m_uCommentCodePage);
	RemoveFromDisk();
}

bool CZipCentralDir::SetFileComment(ZIP_INDEX_TYPE uIndex, LPCTSTR lpszComment)
{
	if (!IsValidIndex(uIndex))
	{
		ASSERT(FALSE);
		return false;
	}
	CZipFileHeader* pHeader = (*this)[uIndex];
	pHeader->m_stringSettings.m_uCommentCodePage = m_pStringSettings->m_uCommentCodePage;
	pHeader->SetComment(lpszComment);
	RemoveFromDisk();
	return true;
}

void CZipCentralDir::RemoveFromDisk()
{
	if (m_pInfo->m_bInArchive)
	{
		ASSERT(!m_pStorage->IsSegmented()); // you can't add files to an existing segmented archive or to delete them from it
		m_pStorage->m_pFile->SetLength((ZIP_FILE_USIZE)(m_pStorage->m_uBytesBeforeZip + m_pInfo->m_uOffset));
		m_pInfo->m_bInArchive = false;
	}
	else
		m_pStorage->Flush(); // if remove from disk is requested, then the archive modification will follow, so flush the buffers
}


void CZipCentralDir::CloseNewFile()
{
	m_pOpenedFile->OnNewFileClose(m_pStorage);
	m_pOpenedFile = NULL;
}

void CZipCentralDir::Write()
{
	if (m_pInfo->m_bInArchive)
		return;

	m_pInfo->m_uEntriesNumber = (ZIP_INDEX_TYPE)m_pHeaders->GetSize();	

	if (!m_pStorage->IsSegmented())
	{
		m_pStorage->Flush();
		m_pStorage->m_pFile->SeekToEnd();
	}
// 	else
// 		we are at the end already

	m_pInfo->m_uSize = 0;
	bool bDontAllowVolumeChange = false;
	
	if (m_pStorage->IsSegmented())
	{
		// segmentation signature at the beginning (4 bytes) + the size of the data descr. for each file
		ZIP_SIZE_TYPE uSize = GetSize(true);
		// if there is a segmented archive in creation and it is only one-volume,
		//	(current volume number is 0 so far, no bytes has been written so we know they are 
		//  all in the buffer)	make sure that it will be after writing central dir 
		// and make it a not segmented archive
		if (m_pStorage->GetCurrentVolume() == 0)
		{
			// calculate the size of data descriptors already in the buffer or on the disk
			// (they will be removed in the not segmented archive).
			ZIP_SIZE_TYPE uToGrow = uSize - 4;
			for (ZIP_INDEX_TYPE i = 0; i < m_pInfo->m_uEntriesNumber; i++)
			{
				CZipFileHeader* pHeader = (*this)[i];
				if (pHeader->NeedsDataDescriptor())
				{
					if (!pHeader->IsEncrypted())
						uToGrow -= 4; // remove the signature only
				}
				else
					uToGrow -= pHeader->GetDataDescriptorSize(true);
			}

			ZIP_SIZE_TYPE uVolumeFree = m_pStorage->VolumeLeft();
			
			if (uVolumeFree >= uToGrow) 
			// lets make sure it will be one-volume archive
			{
				// can the operation be done only in the buffer?
				if (!m_pStorage->m_uBytesWritten && // no bytes on the disk yet
					(m_pStorage->GetFreeInBuffer() >= uToGrow)) // is the buffer big enough?
				{
						RemoveDataDescr(true);
						bDontAllowVolumeChange = true; // if a volume change occurs somehow, we'll throw an error later
				}
				else
				{
					m_pStorage->Flush();
					if (RemoveDataDescr(false))
						bDontAllowVolumeChange = true; // if a volume change occurs somehow, we'll throw an error later
				}
			}
		}

		// make sure that in a segmented archive, the whole central directory will fit on the single volume
		if (!bDontAllowVolumeChange)
			m_pStorage->AssureFree(uSize);
	}

	try
	{
		WriteHeaders(bDontAllowVolumeChange || !m_pStorage->IsSegmented());
				
		WriteCentralEnd();

		if (bDontAllowVolumeChange)
		{
			if (m_pStorage->GetCurrentVolume() != 0)
				ThrowError(CZipException::badZipFile);
		}		
	}
	catch (...)
	{
		if (bDontAllowVolumeChange)
		{
			m_pStorage->FinalizeSegm();
			m_pInfo->m_uLastVolume = 0;
		}
		throw;
	}
	m_pInfo->m_bInArchive = true;
}

void CZipCentralDir::WriteHeaders(bool bOneDisk)
{
	CZipActionCallback* pCallback = m_pCallbacks->Get(CZipActionCallback::cbSave);
	m_pInfo->m_uVolumeEntriesNo = 0;
	m_pInfo->m_uVolumeWithCD = m_pStorage->GetCurrentVolume();
	
	m_pInfo->m_uOffset = m_pStorage->GetPosition();
	if (!m_pInfo->m_uEntriesNumber)
		return;

	ZIP_VOLUME_TYPE uDisk = m_pInfo->m_uVolumeWithCD;

	if (pCallback)
	{
		pCallback->Init();
		pCallback->SetTotal(m_pInfo->m_uEntriesNumber);
	}

	int iAborted = 0;
	if (m_pInfo->m_uEntriesNumber > 0)
	{
		ZIP_INDEX_TYPE uLast = (ZIP_INDEX_TYPE)(m_pInfo->m_uEntriesNumber - 1);
		ZIP_INDEX_TYPE i = 0;
		for (;;)
		{
			CZipFileHeader* pHeader = (*this)[i];
			
			m_pInfo->m_uSize += pHeader->Write(m_pStorage);

			if (m_pStorage->GetCurrentVolume() != uDisk)
			{
				m_pInfo->m_uVolumeEntriesNo = 1;
				uDisk = m_pStorage->GetCurrentVolume();
				// update the information about the offset and starting volume if the 
				// first header was written in the new volume
				if (i == 0)
				{
					m_pInfo->m_uOffset = 0;
					m_pInfo->m_uVolumeWithCD = uDisk;
				}
			}
			else 
				m_pInfo->m_uVolumeEntriesNo++;

			if (pCallback)
			{
				bool ret, last;
				if (i == uLast)
				{
					ret = pCallback->RequestLastCallback(1);
					last = true;
				}
				else
				{
					ret = pCallback->RequestCallback();
					last = false;
				}

				if (ret)
				{
					if (last)
						break;
				}
				else
				{
					if (bOneDisk) 
					{						
						ASSERT(!m_pStorage->IsSegmented());
						// if segmented, would need to m_pStorage->Flush(), but the headers can span multiple volumes
						m_pStorage->EmptyWriteBuffer();						
						// remove saved part from the volume
						m_pStorage->m_pFile->SetLength((ZIP_FILE_USIZE)(m_pStorage->m_uBytesBeforeZip + m_pInfo->m_uOffset));
	//	 				We can now abort safely
						iAborted = CZipException::abortedSafely;
					}
					else
						iAborted = CZipException::abortedAction;
					break;
				}
			}
			else if (i == uLast)
				break;

			i++;
		}
	}

	if (pCallback)
	{
		pCallback->CallbackEnd();
		if (iAborted)
			ThrowError(iAborted);
	}
}


void CZipCentralDir::WriteCentralEnd()
{
	ZIP_SIZE_TYPE uSize = CENTRAL_DIR_END_SIZE + m_pInfo->m_pszComment.GetSize();

	CZipAutoBuffer buf((DWORD)uSize);
	char* pBuf = buf;
	ZIP_VOLUME_TYPE uDisk = m_pStorage->GetCurrentVolume();
	if (m_pStorage->IsSegmented() != 0)
	{
		// update the volume number
		m_pStorage->AssureFree(uSize);
		m_pInfo->m_uLastVolume = m_pStorage->GetCurrentVolume();
	}
	if (m_pInfo->m_uLastVolume != uDisk)
		m_pInfo->m_uVolumeEntriesNo = 0;


	WORD uCommentSize = (WORD)m_pInfo->m_pszComment.GetSize();
	memcpy(pBuf, m_gszSignature, 4);
	CBytesWriter::WriteBytes(pBuf + 4,  CBytesWriter::WriteSafeU16(m_pInfo->m_uLastVolume));
	CBytesWriter::WriteBytes(pBuf + 6,  CBytesWriter::WriteSafeU16(m_pInfo->m_uVolumeWithCD));
	CBytesWriter::WriteBytes(pBuf + 8,  CBytesWriter::WriteSafeU16(m_pInfo->m_uVolumeEntriesNo));
	CBytesWriter::WriteBytes(pBuf + 10, CBytesWriter::WriteSafeU16(m_pInfo->m_uEntriesNumber));
	CBytesWriter::WriteBytes(pBuf + 12, CBytesWriter::WriteSafeU32(m_pInfo->m_uSize));
	CBytesWriter::WriteBytes(pBuf + 16, CBytesWriter::WriteSafeU32(m_pInfo->m_uOffset));
	CBytesWriter::WriteBytes(pBuf + 20, uCommentSize);
	memcpy(pBuf + 22, m_pInfo->m_pszComment, uCommentSize);
	if (uSize > (DWORD)(-1))
		CZipException::Throw(CZipException::internalError);
	m_pStorage->Write(buf, (DWORD)uSize, true);
}

void CZipCentralDir::RemoveAll()
{
	ClearFindFastArray();
	RemoveHeaders();
}


void CZipCentralDir::RemoveFile(CZipFileHeader* pHeader, ZIP_INDEX_TYPE uIndex, bool bShift)
{
	if (uIndex == ZIP_FILE_INDEX_UNSPECIFIED)
	{
		// we need to know the index to remove
		ZIP_INDEX_TYPE uCount = (ZIP_INDEX_TYPE)m_pHeaders->GetSize();
		for (ZIP_INDEX_TYPE i = 0; i < uCount; i++)
			if (pHeader == (*m_pHeaders)[(ZIP_ARRAY_SIZE_TYPE)i])
			{
				uIndex = i;
				break;
			}
	}
	ASSERT(uIndex != ZIP_FILE_INDEX_UNSPECIFIED || pHeader);
	if (!pHeader)
		pHeader = (*m_pHeaders)[(ZIP_ARRAY_SIZE_TYPE)uIndex];

	if (m_pInfo->m_bFindFastEnabled)
	{
		ZIP_INDEX_TYPE i = FindFileNameIndex(pHeader->GetFileName());
		ASSERT(i != ZIP_FILE_INDEX_NOT_FOUND);
		CZipFindFast* pFindFast = (*m_pFindArray)[(ZIP_ARRAY_SIZE_TYPE)i];
		ZIP_INDEX_TYPE uBorderIndex = pFindFast->m_uIndex;
		delete pFindFast;
		pFindFast = NULL;
		m_pFindArray->RemoveAt((ZIP_ARRAY_SIZE_TYPE)i);
		// shift down the indexes
		
		if (bShift)
		{
			ZIP_INDEX_TYPE uSize = (ZIP_INDEX_TYPE)m_pFindArray->GetSize();
			for (ZIP_INDEX_TYPE j = 0; j < uSize; j++)
			{
				if ((*m_pFindArray)[(ZIP_ARRAY_SIZE_TYPE)j]->m_uIndex > uBorderIndex)
					(*m_pFindArray)[(ZIP_ARRAY_SIZE_TYPE)j]->m_uIndex--;
			}
		}
	}

	if (uIndex != ZIP_FILE_INDEX_UNSPECIFIED)
	{
		delete pHeader;
		m_pHeaders->RemoveAt((ZIP_ARRAY_SIZE_TYPE)uIndex);
	}
}


void CZipCentralDir::RemoveLastFile(CZipFileHeader* pHeader, ZIP_INDEX_TYPE uIndex)
{
	if (uIndex == ZIP_FILE_INDEX_UNSPECIFIED)
	{
		if (m_pHeaders->GetSize() == 0)
			return;			
		uIndex = (ZIP_VOLUME_TYPE)(m_pHeaders->GetSize() - 1);
	}
	if (!pHeader)
		pHeader = (*m_pHeaders)[(ZIP_ARRAY_SIZE_TYPE)uIndex];
	ZIP_SIZE_TYPE uNewSize = pHeader->m_uOffset + m_pStorage->m_uBytesBeforeZip;
	// then remove
	RemoveFile(pHeader, uIndex);

	m_pStorage->Flush();
	m_pStorage->m_pFile->SetLength((ZIP_FILE_USIZE)uNewSize);
	m_pInfo->m_bInArchive = false; // it is true when AutoFlush is set to true
}


ZIP_SIZE_TYPE CZipCentralDir::GetSize(bool bWhole) const
{
	ZIP_SIZE_TYPE uTotal = CENTRAL_DIR_END_SIZE + m_pInfo->m_pszComment.GetSize();
	ZIP_INDEX_TYPE uCount = (ZIP_INDEX_TYPE)m_pHeaders->GetSize();
	if (bWhole)
	{
		for (ZIP_INDEX_TYPE i = 0; i < uCount; i++)
		{
			const CZipFileHeader* pHeader = (*m_pHeaders)[(ZIP_ARRAY_SIZE_TYPE)i];
			uTotal += pHeader->GetSize();
		}
	}


	return uTotal;
}

bool CZipCentralDir::RemoveDataDescr(bool bFromBuffer)
{
	// this will not work if there are bytes before zip
	CZipFileMapping fm;
	char* pFile;
	ZIP_SIZE_TYPE uSize;
	if (bFromBuffer)
	{
		uSize = m_pStorage->m_uBytesInWriteBuffer;
		pFile = m_pStorage->m_pWriteBuffer;
	}
	else
	{
		uSize = (ZIP_SIZE_TYPE)m_pStorage->m_pFile->GetLength();
		// we cannot use CZipMemFile in multi-volume archive
		// so it must be CZipFile
		if (!fm.CreateMapping(static_cast<CZipFile*>(m_pStorage->m_pFile)))
			return false;
		pFile = fm.GetMappedMemory();
	}

	ZIP_SIZE_TYPE uOffsetToChange = 4;
	ZIP_SIZE_TYPE uPosInBuffer = 0;
	WORD uExtraHeaderLen;
	ZIP_INDEX_TYPE uCount = (ZIP_INDEX_TYPE)m_pHeaders->GetSize();
	for (ZIP_INDEX_TYPE i = 0; i < uCount; i++)
	{
		CZipFileHeader* pHeader = (*m_pHeaders)[(ZIP_ARRAY_SIZE_TYPE)i];
		char* pSour = pFile + pHeader->m_uOffset;

		if (pHeader->NeedsDataDescriptor())
			uExtraHeaderLen = (WORD)(pHeader->IsEncrypted() ? 0 : 4);
		else
		{
			uExtraHeaderLen = pHeader->GetDataDescriptorSize(true);
			// removing data descriptor
			pHeader->m_uFlag &= ~8;
			// update local header:
			// write modified flag in the local header
			CBytesWriter::WriteBytes(pSour + 6, pHeader->m_uFlag);			
			pHeader->WriteSmallDataDescriptor(pSour + 14, false);
		}

		ZIP_SIZE_TYPE uToCopy = (i == (uCount - 1) ? uSize : (*m_pHeaders)[(ZIP_ARRAY_SIZE_TYPE)(i + 1)]->m_uOffset)
			- pHeader->m_uOffset - uExtraHeaderLen;
		if (uToCopy > 0)
			// TODO: [postponed] the size_t limit on uToCopy, but creating such a big segment is unlikely (at least at the moment of writing)
			memmove(pFile + uPosInBuffer, pSour, (size_t)uToCopy);

		uPosInBuffer += uToCopy;
		pHeader->m_uOffset -= uOffsetToChange;
		uOffsetToChange += uExtraHeaderLen;
	}

	if (bFromBuffer)
		m_pStorage->m_uBytesInWriteBuffer = (DWORD)uPosInBuffer;
	else
	{
		m_pStorage->m_uBytesWritten = uPosInBuffer;
		fm.RemoveMapping();
		m_pStorage->m_pFile->SetLength((ZIP_FILE_USIZE)uPosInBuffer);
	}
	return true;
}

void CZipCentralDir::RemoveHeaders()
{
	ZIP_INDEX_TYPE uCount = (ZIP_INDEX_TYPE)m_pHeaders->GetSize();
	for (ZIP_INDEX_TYPE i = 0; i < uCount; i++)
		delete (*m_pHeaders)[(ZIP_ARRAY_SIZE_TYPE)i];
	m_pHeaders->RemoveAll();
}

void CZipCentralDir::BuildFindFastArray( bool bCaseSensitive )
{
	ClearFindFastArray();
	m_pInfo->m_bCaseSensitive = bCaseSensitive;
	// for later
	m_pInfo->m_pCompare = GetCZipStrCompFunc(bCaseSensitive);
	ZIP_INDEX_TYPE uCount = (ZIP_INDEX_TYPE)m_pHeaders->GetSize();

	for (ZIP_INDEX_TYPE i = 0; i < uCount; i++)
		m_pFindArray->Add(new CZipFindFast((*m_pHeaders)[(ZIP_ARRAY_SIZE_TYPE)i], i));
	
	m_pFindArray->Sort(bCaseSensitive ? CompareFindFastCollate : CompareFindFastCollateNoCase);
}

void CZipCentralDir::EnableFindFast(bool bEnable, bool bCaseSensitive)
{
	if (m_pInfo->m_bFindFastEnabled == bEnable)
		return;
	m_pInfo->m_bFindFastEnabled = bEnable;
	if (bEnable)
		BuildFindFastArray(bCaseSensitive);
	else
		m_pFindArray->RemoveAll();
}

ZIP_INDEX_TYPE CZipCentralDir::FindFile(LPCTSTR lpszFileName, bool bCaseSensitive, bool bSporadically, bool bFileNameOnly)
{
	if (!m_pInfo->m_bFindFastEnabled)
		EnableFindFast(true, bSporadically ? !bCaseSensitive : bCaseSensitive);
	ZIP_INDEX_TYPE uResult = ZIP_FILE_INDEX_NOT_FOUND;
	if (bFileNameOnly)
	{
		//  a non-effective search (treat an array as unsorted)

		// set the proper compare function
		ZIPSTRINGCOMPARE pCompare = bCaseSensitive == m_pInfo->m_bCaseSensitive ? m_pInfo->m_pCompare : GetCZipStrCompFunc(bCaseSensitive);
		
		ZIP_INDEX_TYPE uSize = (ZIP_INDEX_TYPE)m_pFindArray->GetSize();
		for (ZIP_INDEX_TYPE i = 0; i < uSize; i++)
		{
			CZipString sz = (*m_pFindArray)[(ZIP_ARRAY_SIZE_TYPE)i]->m_pHeader->GetFileName();
			CZipPathComponent::RemoveSeparators(sz); // to find a dir
			CZipPathComponent zpc(sz);
			sz = zpc.GetFileName();
			if ((sz.*pCompare)(lpszFileName) == 0)
			{
				uResult = i;
				break;
			}
		}
	}
	else if (bCaseSensitive == m_pInfo->m_bCaseSensitive)
		uResult = FindFileNameIndex(lpszFileName);
	else
	{
		if (bSporadically)
		{
			// a non-effective search (treat an array as unsorted)
			// do not use m_pInfo->m_pCompare, as it may be shared
			ZIPSTRINGCOMPARE pCompare = GetCZipStrCompFunc(bCaseSensitive);			
			ZIP_INDEX_TYPE uSize = (ZIP_INDEX_TYPE)m_pFindArray->GetSize();
			for (ZIP_INDEX_TYPE i = 0; i < uSize; i++)
				if (((*m_pFindArray)[(ZIP_ARRAY_SIZE_TYPE)i]->m_pHeader->GetFileName().*pCompare)(lpszFileName) == 0)
				{
					uResult = i;
					break;
				}
		}
		else
		{
			BuildFindFastArray(bCaseSensitive);		
			uResult = FindFileNameIndex(lpszFileName);
		}
	}
	return uResult == ZIP_FILE_INDEX_NOT_FOUND ? ZIP_FILE_INDEX_NOT_FOUND : (*m_pFindArray)[(ZIP_ARRAY_SIZE_TYPE)uResult]->m_uIndex;	
}

ZIP_INDEX_TYPE CZipCentralDir::InsertFindFastElement(CZipFileHeader* pHeader, ZIP_INDEX_TYPE uIndex)
{
	CZipString fileName = pHeader->GetFileName();
	ZIP_ARRAY_SIZE_TYPE uSize = m_pFindArray->GetSize();

	//	Our initial binary search range encompasses the entire array of filenames:
	ZIP_ARRAY_SIZE_TYPE start = 0;
	ZIP_ARRAY_SIZE_TYPE end = uSize;

	//	Keep halving our search range until we find the right place
	//	to insert the new element:
	while ( start < end )
	{
		//	Find the midpoint of the search range:
		ZIP_ARRAY_SIZE_TYPE midpoint = ( start + end ) / 2;

		//	Compare the filename with the filename at the midpoint of the current search range:
		int result = CompareElement(fileName, (ZIP_INDEX_TYPE)midpoint);

		//	If our filename is larger, it must fall in the first half of the search range:
		if ( result > 0 )
		{
			end = midpoint;
		}

		//	If it's smaller, it must fall in the last half:
		else if ( result < 0 )
		{
			start = midpoint + 1;
		}

		//	If they're equal, we can go ahead and insert here:
		else
		{
			start = midpoint;
			break;
		}
	}
	m_pFindArray->InsertAt(start, new CZipFindFast(pHeader, uIndex == ZIP_FILE_INDEX_UNSPECIFIED ? (ZIP_INDEX_TYPE)uSize : uIndex /* just in case */)); 
	return (ZIP_INDEX_TYPE)start;
}

ZIP_INDEX_TYPE CZipCentralDir::FindFileNameIndex(LPCTSTR lpszFileName) const
{
	ZIP_ARRAY_SIZE_TYPE start = 0;

	ZIP_ARRAY_SIZE_TYPE end = m_pFindArray->GetSize();
	if (end == 0)
		return ZIP_FILE_INDEX_NOT_FOUND;
	else
		end--;

	//	Keep halving our search range until we find the given element:
	while ( start <= end )
	{
		//	Find the midpoint of the search range:
		ZIP_ARRAY_SIZE_TYPE midpoint = ( start + end ) / 2;

		//	Compare the given filename with the filename at the midpoint of the search range:
		int result = CompareElement(lpszFileName, (ZIP_INDEX_TYPE)midpoint);

		//	If our filename is smaller, it must fall in the first half of the search range:
		if ( result > 0 )
		{
			if (midpoint == 0)
				return ZIP_FILE_INDEX_NOT_FOUND;
			end = midpoint - 1;
		}

		//	If it's larger, it must fall in the last half:
		else if ( result < 0 )
		{
			start = midpoint + 1;
		}

		//	If they're equal, return the result:
		else
		{
			return (ZIP_INDEX_TYPE)midpoint;
		}
	}

	//	Signal failure:
	return ZIP_FILE_INDEX_NOT_FOUND;
}


void CZipCentralDir::CreateSharedData()
{
	m_pInfo = new CInfo();
	m_pInfo->Init();	
	m_pHeaders = new CZipArray<CZipFileHeader*>();
	m_pFindArray = new CZipArray<CZipFindFast*>();
}

void CZipCentralDir::DestroySharedData()
{
	if (!m_pInfo)
		return;
#ifdef ZIP_ARCHIVE_USE_LOCKING
	LockAccess();
	try
	{
#endif
		m_pInfo->m_iReference--;
		if (m_pInfo->m_iReference <= 0) // <= is just in case instead of ==
		{

			if (m_pHeaders != NULL)
			{
				RemoveHeaders();
				delete m_pHeaders;
				m_pHeaders = NULL;
			}
			
			if (m_pFindArray != NULL)
			{
				ClearFindFastArray();
				delete m_pFindArray;
				m_pFindArray = NULL;
			}
#ifdef ZIP_ARCHIVE_USE_LOCKING				
			UnlockAccess();
#endif
			delete m_pInfo;
			m_pInfo = NULL;
		}
#ifdef ZIP_ARCHIVE_USE_LOCKING
	}
	catch(...)
	{
		UnlockAccess();
		throw;
	}
	UnlockAccess();
#endif

}
