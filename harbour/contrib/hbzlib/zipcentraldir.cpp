// ZipCentralDir.cpp: implementation of the CZipCentralDir class.
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


#include "stdafx.h"
#include "zipcentraldir.h"
#include "ziparchive.h"
#include "zipfilemapping.h"


#define CENTRALDIRSIZE	22

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
char CZipCentralDir::m_gszSignature[] = {0x50, 0x4b, 0x05, 0x06};
CZipCentralDir::CZipCentralDir()
{
	m_bConvertAfterOpen  = true;
	m_bFindFastEnabled = false;
	m_pStorage = NULL;
	m_pOpenedFile = NULL;
	m_iBufferSize = 32768;
	
}

void CZipCentralDir::Init()
{
	m_bOnDisk = false;
	m_uBytesBeforeZip = m_uCentrDirPos = 0;
	m_pOpenedFile = NULL;
	m_pszComment.Release();
}

CZipCentralDir::~CZipCentralDir()
{
	Clear();
}

void CZipCentralDir::Read()
{
	ASSERT(m_pStorage);
	WORD uCommentSize;
	m_uCentrDirPos = Locate();
	m_pStorage->m_pFile->Seek(m_uCentrDirPos, CZipAbstractFile::begin);
	CZipAutoBuffer buf(CENTRALDIRSIZE);

	int uRead = m_pStorage->m_pFile->Read(buf, CENTRALDIRSIZE);
	if (uRead != CENTRALDIRSIZE)
		ThrowError(CZipException::badZipFile);
	memcpy(&m_szSignature,		buf, 4);
	memcpy(&m_uThisDisk,		buf + 4, 2);
	memcpy(&m_uDiskWithCD,		buf + 6, 2);
	memcpy(&m_uDiskEntriesNo,	buf + 8, 2);
	memcpy(&m_uEntriesNumber,	buf + 10, 2);
	memcpy(&m_uSize,			buf + 12, 4);
	memcpy(&m_uOffset,			buf + 16, 4);
	memcpy(&uCommentSize,		buf + 20, 2);
	buf.Release();


	m_pStorage->UpdateSpanMode(m_uThisDisk);
	// if m_uThisDisk is not zero, it is enough to say that it is a multi disk archive
	ASSERT((!m_uThisDisk && (m_uEntriesNumber == m_uDiskEntriesNo) && !m_uDiskWithCD) || m_uThisDisk);

			

	if (!m_pStorage->IsSpanMode() && ((DWORD)m_uCentrDirPos < m_uOffset + m_uSize))
		ThrowError(CZipException::badZipFile);

	if (uCommentSize)
	{
		m_pszComment.Allocate(uCommentSize);
		uRead = m_pStorage->m_pFile->Read(m_pszComment, uCommentSize);
		if (uRead != uCommentSize)
			ThrowError(CZipException::badZipFile);
	}
	
	m_uBytesBeforeZip = m_pStorage->IsSpanMode() ? 0 : m_uCentrDirPos - m_uSize - m_uOffset;

	if ((!m_uSize && m_uEntriesNumber) || (!m_uEntriesNumber && m_uSize))
		ThrowError(CZipException::badZipFile);

	m_bOnDisk = true;
	m_pStorage->ChangeDisk(m_uDiskWithCD);

	if (!m_uSize)
		return;

	ReadHeaders();
}

DWORD CZipCentralDir::Locate()
{

	// maximum size of end of central dir record
	long uMaxRecordSize = 0xffff + CENTRALDIRSIZE;
	DWORD uFileSize = m_pStorage->m_pFile->GetLength();

	if ((DWORD)uMaxRecordSize > uFileSize)
		uMaxRecordSize = uFileSize;

	CZipAutoBuffer buf(m_iBufferSize);

	long uPosInFile = 0;
	int uRead = 0;
	// backward reading
	while (uPosInFile < uMaxRecordSize)
	{
		uPosInFile = uRead + m_iBufferSize;
		if (uPosInFile > uMaxRecordSize)
			uPosInFile = uMaxRecordSize;

		int iToRead = uPosInFile - uRead;

		m_pStorage->m_pFile->Seek(-uPosInFile, CZipAbstractFile::end);
		int iActuallyRead = m_pStorage->m_pFile->Read(buf, iToRead);
		if (iActuallyRead != iToRead)
			ThrowError(CZipException::badZipFile);
		// search from the very last bytes to prevent an error if inside archive 
		// there are packed other arhives
		for (int i = iToRead - 4; i >=0 ; i--)
			if (!memcmp((char*)buf + i, m_gszSignature, 4))	
				return uFileSize - (uPosInFile - i);

		uRead += iToRead - 3;

	}
	
	ThrowError(CZipException::cdirNotFound);
	return 0;
}

void CZipCentralDir::ThrowError(int err)
{
	CZipException::Throw(err, m_pStorage->m_pFile->GetFilePath());
}


void CZipCentralDir::ReadHeaders()
{
	m_pStorage->m_pFile->Seek(m_uOffset + m_uBytesBeforeZip, CZipAbstractFile::begin);
	RemoveHeaders(); //just in case
	for (int i = 0; i < m_uEntriesNumber; i++)
	{
		CZipFileHeader* pHeader = new CZipFileHeader;
		m_headers.AddTail(pHeader); // bezpoœrednio nastêpuje w razie wyj¹tku

		if (!pHeader->Read(m_pStorage))
			ThrowError(CZipException::badZipFile);
		ConvertFileName(true, true, pHeader);
	}
	if (m_bFindFastEnabled)
		BuildFindFastArray();
}

void CZipCentralDir::Clear(bool bEverything)
{
	m_pOpenedFile = NULL;
	m_pLocalExtraField.Release();

	if (bEverything)
	{
		RemoveHeaders();
		m_findarray.RemoveAll();
		m_pszComment.Release();
		
	}
}


bool CZipCentralDir::IsValidIndex(int uIndex)
{

	bool ret = uIndex < m_headers.GetCount();
#ifdef _DEBUG
	if (!ret)
			TRACE(_T("Not a valid index.\n"));
#endif 
	return ret;
}


void CZipCentralDir::OpenFile(WORD uIndex)
{
	WORD uLocalExtraFieldSize;
	m_pOpenedFile = (*this)[uIndex];
	m_pStorage->ChangeDisk(m_pOpenedFile->m_uDiskStart);
	m_pStorage->m_pFile->Seek(m_pOpenedFile->m_uOffset + m_uBytesBeforeZip, CZipAbstractFile::begin);	
	if (!m_pOpenedFile->ReadLocal(m_pStorage, uLocalExtraFieldSize))
		ThrowError(CZipException::badZipFile);


	m_pLocalExtraField.Release(); // just in case
	if (uLocalExtraFieldSize)
	{
		int iCurrDsk = m_pStorage->GetCurrentDisk();
		m_pLocalExtraField.Allocate(uLocalExtraFieldSize);
		m_pStorage->Read(m_pLocalExtraField, uLocalExtraFieldSize, true);
		if (m_pStorage->GetCurrentDisk() != iCurrDsk)
			ThrowError(CZipException::badZipFile);
	}
	
}

void CZipCentralDir::CloseFile()
{
	if (!m_pOpenedFile)
		return;
	m_pLocalExtraField.Release();
	if (m_pOpenedFile->IsDataDescr())
	{
		CZipAutoBuffer buf(12);
		m_pStorage->Read(buf, 4, false);
		// in span mode, files that are divided between disks have bit 3 of flag set
		// which tell about the presence of the data descriptor after the compressed data
		// This signature may be in the disk spanning archive that is one volume only
		// (it is detected as a non disk spanning archive)
		if (memcmp(buf, CZipStorage::m_gszExtHeaderSignat, 4) != 0) // there is no signature
				m_pStorage->m_pFile->Seek(-4, CZipAbstractFile::current);

		
		m_pStorage->Read(buf, 12, false);
		if (!m_pOpenedFile->CheckCrcAndSizes(buf))
			ThrowError(CZipException::badZipFile);
	}
	m_pOpenedFile = NULL;
}

// add new header using the argument as a template
void CZipCentralDir::AddNewFile(const CZipFileHeader & header)
{
	CZipFileHeader* pHeader = new CZipFileHeader(header);
	m_pOpenedFile = pHeader;
	m_headers.AddTail(pHeader);
	if (m_bFindFastEnabled)
		InsertFindFastElement(pHeader, WORD(m_headers.GetCount() - 1)); // GetCount > 0, 'cos we've just added a header
	RemoveFromDisk();
	m_pStorage->m_pFile->SeekToEnd();
}


void CZipCentralDir::RemoveFromDisk()
{
	if (m_bOnDisk)
	{
		ASSERT(!m_pStorage->IsSpanMode()); // you can't add files to the existing disk span archive or to delete them from it
		m_pStorage->m_pFile->SetLength(m_uBytesBeforeZip + m_uOffset);
		m_bOnDisk = false;
	}
}


void CZipCentralDir::CloseNewFile()
{
	CZipAutoBuffer buf(12 + 4);
	short iToWrite = 0;
	bool bIsSpan = m_pStorage->IsSpanMode() != 0;
	bool bEncrypted = m_pOpenedFile->IsEncrypted();
	if (m_pOpenedFile->IsDataDescr())
	{
		if (bIsSpan || bEncrypted)
		{
			memcpy(buf, m_pStorage->m_gszExtHeaderSignat, 4);
			iToWrite += 4;
		}
	}
	else /*if (!IsSpan)*/
	{
		ASSERT(!bIsSpan && !bEncrypted);
		m_pStorage->Flush();
		m_pStorage->m_pFile->Seek(m_pOpenedFile->m_uOffset + 14, CZipAbstractFile::begin);
		// we don't have to restore the pointer, because before adding a new file, 
		// the pointer is moved to the end
	}

	m_pOpenedFile->GetCrcAndSizes(buf + iToWrite);
	iToWrite += 12;

	// offset set during writing the local header
	m_pOpenedFile->m_uOffset -= m_uBytesBeforeZip;
	
	// write the data descriptor and a disk spanning signature at once
	m_pStorage->Write(buf, iToWrite, true);
	if (!bIsSpan && bEncrypted)
	{
		// write the information to the local header too
		m_pStorage->Flush();
		m_pStorage->m_pFile->Seek(m_pOpenedFile->m_uOffset + 14, CZipAbstractFile::begin);
		m_pStorage->Write(buf + 4, 12, true);
	}

	if (!bIsSpan)
		m_pStorage->Flush();

	m_pOpenedFile = NULL;

}

void CZipCentralDir::Write()
{
	if (m_bOnDisk)
		return;
	if (!m_pStorage->IsSpanMode())
	{
		m_pStorage->Flush();
		m_pStorage->m_pFile->SeekToEnd();
	}
	m_uEntriesNumber = (WORD)m_headers.GetCount();
	m_uSize = 0;
	bool bDontAllowDiskChange = false;
	// if there is a disk spanning archive in creation and it is only one-volume,
	//	(current disk is 0 so far, no bytes has been written so we know they are 
	//  all in the buffer)	make sure that it will be after writing central dir 
	// and make it a non disk spanning archive
	if (m_pStorage->IsSpanMode() && m_pStorage->GetCurrentDisk() == 0)
	{
		DWORD uVolumeFree = m_pStorage->VolumeLeft();
		// calculate the size of data descriptors already in the buffer or on the disk
		// (they will be removed in the non disk spanning archive):
		// multi span signature at the beginnig (4 bytes) + the size of the data 
		// descr. for each file (multi span signature + 12 bytes data)
		// the count of bytes to add: central dir size - total to remove;
		DWORD uToGrow = GetSize(true) - (4 + m_uEntriesNumber * (4 + 12)); 
		if (uVolumeFree >= uToGrow) 
		// lets make sure it will be one-disk archive
		{
			// can the operation be done only in the buffer?
			if (!m_pStorage->m_iBytesWritten && // no bytes on the disk yet
				(m_pStorage->GetFreeInBuffer() >= uToGrow)) // is the buffer big enough?
			{
					RemoveDataDescr(true);
					bDontAllowDiskChange = true; // if the disk change occurs somehow, we'll throw an error later
			}
			else
			{
				m_pStorage->Flush();
				m_pStorage->m_pFile->Flush();
				if (RemoveDataDescr(false))
					bDontAllowDiskChange = true; // if the disk change occurs somehow, we'll throw an error later
			}
		}
	}

	WriteHeaders();
	m_uThisDisk = (WORD)m_pStorage->GetCurrentDisk();
	DWORD uSize = WriteCentralEnd();
	if (bDontAllowDiskChange && (m_pStorage->GetCurrentDisk() != 0))
		ThrowError(CZipException::badZipFile);
	// if after adding a central directory there is a disk change, 
	// update the information and write it again
	if (m_uThisDisk != m_pStorage->GetCurrentDisk())
	{
		m_uThisDisk = (WORD)m_pStorage->GetCurrentDisk();
		if (m_uEntriesNumber)
		{
			m_uDiskEntriesNo = 0;	
		}
		else
		{
			m_uDiskWithCD = m_uThisDisk;
			m_uOffset = 0;
		}

		if (m_pStorage->m_uBytesInWriteBuffer >= uSize)
			// if the data is still in the buffer, simply remove it
			m_pStorage->m_uBytesInWriteBuffer -= uSize;
		else
		{
			m_pStorage->Flush();
			m_pStorage->m_iBytesWritten -= uSize;
			m_pStorage->m_pFile->SeekToBegin();	
		}
		
		WriteCentralEnd();
	}

}

void CZipCentralDir::WriteHeaders()
{
	m_uDiskEntriesNo = 0;
	m_uDiskWithCD = (WORD)m_pStorage->GetCurrentDisk();
	m_uOffset = m_pStorage->GetPosition() - m_uBytesBeforeZip;
	if (!m_uEntriesNumber)
		return;

	WORD iDisk = m_uDiskWithCD;
	for (int i = 0; i < m_uEntriesNumber; i++)
	{
		CZipFileHeader* pHeader = (*this)[i];
		ConvertFileName(false, true, pHeader);
		m_uSize += pHeader->Write(m_pStorage);
		if (m_pStorage->GetCurrentDisk() != iDisk)
		{
			m_uDiskEntriesNo = 1;
			iDisk = (WORD)m_pStorage->GetCurrentDisk();
			// update the information about the offset and starting disk if the 
			// first header was written on the new disk
			if (i == 0)
			{
				m_uOffset = 0;
				m_uDiskWithCD = iDisk;
			}
		}
		else 
			m_uDiskEntriesNo++;
	}
}

DWORD CZipCentralDir::WriteCentralEnd()
{
	DWORD uSize = GetSize();
	CZipAutoBuffer buf(uSize);
	WORD uCommentSize = (WORD)m_pszComment.GetSize();
	memcpy(buf, m_gszSignature, 4);
	memcpy(buf + 4, &m_uThisDisk, 2);
	memcpy(buf + 6, &m_uDiskWithCD, 2);
	memcpy(buf + 8, &m_uDiskEntriesNo, 2);
	memcpy(buf + 10, &m_uEntriesNumber, 2);
	memcpy(buf + 12, &m_uSize, 4);
	memcpy(buf + 16, &m_uOffset, 4);
	memcpy(buf + 20, &uCommentSize, 2);
	memcpy(buf + 22, m_pszComment, uCommentSize);
	m_pStorage->Write(buf, uSize, true);
	return uSize;
}


void CZipCentralDir::RemoveFile(WORD uIndex)
{
	CZipFileHdrLstIter iterator = GetIterator(uIndex);
	CZipFileHeader* pHeader = GetFileHeader(iterator);
	if (m_bFindFastEnabled)
	{
		int i = FindFileNameIndex(pHeader->GetFileName(), true);
		ASSERT(i != -1);
		int uIndex = m_findarray[i].m_uIndex;
		m_findarray.RemoveAt(i);
		// shift down the indexes
		for (int j = 0; j < m_findarray.GetSize(); j++)
		{
			if (m_findarray[j].m_uIndex > uIndex)
				m_findarray[j].m_uIndex--;
		}
	}
	delete pHeader;
	m_headers.RemoveAt(iterator);
}


DWORD CZipCentralDir::GetSize(bool bWhole)
{
	DWORD uHeaders = 0;
	if (bWhole)
	{
		for (CZipFileHdrLstIter iter = m_headers.GetHeadPosition(); m_headers.IteratorValid(iter); )
		{
			CZipFileHeader* pHeader = m_headers.GetNext(iter);
			uHeaders += pHeader->GetSize();
		}
	}
	return CENTRALDIRSIZE + m_pszComment.GetSize() + uHeaders;
}

bool CZipCentralDir::RemoveDataDescr(bool bFromBuffer)
{
	ziparchv::CZipFileMapping fm;
	char* pFile = NULL;
	DWORD uSize;
	if (bFromBuffer)
	{
		uSize = m_pStorage->m_uBytesInWriteBuffer;
		pFile = m_pStorage->m_pWriteBuffer;
	}
	else
	{
		uSize = m_pStorage->m_pFile->GetLength();
		// we cannot use CZipMemFile in multidisk archive
		// so it MUST be CZipFile
		if (!fm.CreateMapping((HANDLE)static_cast<CZipFile*>(m_pStorage->m_pFile)->operator HFILE()))
			return false;
		pFile = fm.GetMappedMemory();
	}

	DWORD uOffsetToChange = 4;
	DWORD uToCopy = 0;
	DWORD uPosInBuffer = 0;
	DWORD uExtraHeaderLen;
	// this will work providing the order in the m_headers is the same as 
	// in the archive
	int i = 0;
	for (CZipFileHdrLstIter iter = m_headers.GetHeadPosition(); m_headers.IteratorValid(iter); i++)
	{
		// update the flag value in the local and central header
// 		int uDataDescr = (m_headers[i]->m_uFlag & 8) ? (4 + 12) : 0;

		CZipFileHeader* pHeader = m_headers.GetNext(iter);


		char* pSour = pFile + pHeader->m_uOffset;
		
		if (!pHeader->IsEncrypted())
		{
			// removing data descriptor
			pHeader->m_uFlag &= ~8;
			// update local header:
			// write modified flag in the local header
			memcpy(pSour + 6, &pHeader->m_uFlag, 2);
			uExtraHeaderLen = 4/*ext. header signature*/ + 12/*data descriptor*/;
		}
		else
			// do not remove data descriptors from encrypted files
			uExtraHeaderLen = 0;

		// update crc32 and sizes' values
		pHeader->GetCrcAndSizes(pSour+ 14);

		uToCopy = (i == (m_headers.GetCount() - 1) ? uSize : (*this)[i + 1]->m_uOffset)
			- pHeader->m_uOffset - uExtraHeaderLen;

		memmove(pFile + uPosInBuffer, pSour, uToCopy);

		uPosInBuffer += uToCopy;
		pHeader->m_uOffset -= uOffsetToChange;
		uOffsetToChange += uExtraHeaderLen;
	}

	if (bFromBuffer)
		m_pStorage->m_uBytesInWriteBuffer = uPosInBuffer;
	else
	{
		m_pStorage->m_iBytesWritten = uPosInBuffer;
		fm.RemoveMapping();
		m_pStorage->m_pFile->SetLength(uPosInBuffer);
	}
	return true;
}

CZipFileHeader* CZipCentralDir::operator[](int iIndex)
{
	return GetFileHeader(GetIterator(iIndex));
}

void CZipCentralDir::RemoveHeaders()
{
	for (CZipFileHdrLstIter iter = m_headers.GetHeadPosition(); m_headers.IteratorValid(iter); )
		delete m_headers.GetNext(iter);
	m_headers.RemoveAll();
}

CZipCentralDir::CZipFileHdrLstIter CZipCentralDir::GetIterator(int iIndex)
{
	if (!IsValidIndex(iIndex))
		CZipException::Throw(CZipException::internal);

	return m_headers.FindIndex(iIndex);
}

CZipFileHeader* CZipCentralDir::GetFileHeader(const CZipFileHdrLstIter &iterator)
{
	CZipFileHeader* pHeader = m_headers.GetAt(iterator);
	ASSERT(pHeader);
	if (!pHeader)
		CZipException::Throw(CZipException::internal);
	return pHeader;	
}

void CZipCentralDir::ConvertAll()
{
	ASSERT(!m_bConvertAfterOpen);
	for (CZipFileHdrLstIter iter = m_headers.GetHeadPosition(); m_headers.IteratorValid(iter); )
		ConvertFileName(true, false, m_headers.GetNext(iter));
	m_bConvertAfterOpen = true;
}

void CZipCentralDir::BuildFindFastArray()
{
	m_findarray.RemoveAll();// just in case
 	WORD iIndex = 0;
	for (CZipFileHdrLstIter iter = m_headers.GetHeadPosition(); m_headers.IteratorValid(iter); iIndex++)
		InsertFindFastElement(m_headers.GetNext(iter), iIndex);
}

void CZipCentralDir::InsertFindFastElement(CZipFileHeader* pHeader, WORD uIndex)
{
	CZipString fileName = pHeader->GetFileName();

	
	int iSize = m_findarray.GetSize();

	//	Our initial binary search range encompasses the entire array of filenames:
	int start = 0;
	int end = iSize;

	//	Keep halving our search range until we find the right place
	//	to insert the new element:
	while ( start < end )
	{
		//	Find the midpoint of the search range:
		int midpoint = ( start + end ) / 2;

		//	Compare the filename with the filename at the midpoint of the current search range:
		int result = CompareElement(fileName, (WORD)midpoint, true);

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
			start = midpoint; break;
		}
	}
	m_findarray.InsertAt(start, CZipFindFast(pHeader, WORD(uIndex == -1 ? iSize : uIndex /* just in case */))); 
}

int CZipCentralDir::FindFileNameIndex(LPCTSTR lpszFileName, bool bCaseSensitive)
{
	int start = 0;
	int end = m_findarray.GetUpperBound();

	//	Keep halving our search range until we find the given element:
	while ( start <= end )
	{
		//	Find the midpoint of the search range:
		int midpoint = ( start + end ) / 2;

		//	Compare the given filename with the filename at the midpoint of the search range:
		int result = CompareElement(lpszFileName, (WORD)midpoint, bCaseSensitive);

		//	If our filename is smaller, it must fall in the first half of the search range:
		if ( result > 0 )
		{
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
			return midpoint;
		}
	}

	//	Signal failure:
	return -1;
}

ZIPINLINE int CZipCentralDir::CompareElement(LPCTSTR lpszFileName, WORD uIndex, bool bCaseSensitive)
{
	return bCaseSensitive ? m_findarray[uIndex].m_pHeader->GetFileName().Collate(lpszFileName)
		: m_findarray[uIndex].m_pHeader->GetFileName().CollateNoCase(lpszFileName);
}
