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
#include "ZipStorage.h"
#include "ZipArchive.h"
#include "ZipPlatform.h"

char CZipStorage::m_gszExtHeaderSignat[] = {0x50, 0x4b, 0x07, 0x08};
const ZIP_FILE_USIZE CZipStorage::SignatureNotFound = ZIP_FILE_USIZE(-1);


CZipStorage::CZipStorage()
{
	Initialize();
}

void CZipStorage::Initialize()
{	
	m_pSplitChangeVolumeFunc = m_pSpanChangeVolumeFunc = m_pChangeVolumeFunc = NULL;
	m_iWriteBufferSize = 65536;	
	m_pFile = NULL;
	m_szSplitExtension = _T("zip");
	m_iLocateBufferSize = 32768;
	m_uBytesBeforeZip = 0;
	m_uCurrentVolume = ZIP_VOLUME_NUMBER_UNSPECIFIED;
	m_szArchiveName.Empty();
}

CZipStorage::~CZipStorage()
{

}

DWORD CZipStorage::Read(void *pBuf, DWORD iSize, bool bAtOnce)
{
	if (iSize == 0)
		return 0;
	DWORD iRead;
	for(;;)
	{
		iRead = m_pFile->Read(pBuf, iSize);
		if (!iRead)
		{
			if (IsSegmented())
				ChangeVolume();
			else
				ThrowError(CZipException::badZipFile);
		}
		else
			break;
	}

	if (iRead == iSize)
		return iRead;
	else if (bAtOnce || !IsSegmented())
		ThrowError(CZipException::badZipFile);

	while (iRead < iSize)
	{
		ChangeVolume();
		UINT iNewRead = m_pFile->Read((char*)pBuf + iRead, iSize - iRead);
		if (!iNewRead && iRead < iSize)
			ThrowError(CZipException::badZipFile);
		iRead += iNewRead;
	}

	return iRead;
}

void CZipStorage::Open(LPCTSTR lpszPathName, int iMode, ZIP_SIZE_TYPE uVolumeSize)
{
	m_uCurrentVolume = ZIP_VOLUME_NUMBER_UNSPECIFIED;
	m_pWriteBuffer.Allocate(m_iWriteBufferSize); 
	m_uBytesInWriteBuffer = 0;
	m_bNewSegm = false;
	m_pFile = &m_internalfile;
	m_bInMemory = false;
	m_szArchiveName = lpszPathName;
	m_pChangeVolumeFunc = NULL;

	if (iMode ==  CZipArchive::zipCreate || iMode ==  CZipArchive::zipCreateSegm
		|| iMode ==  CZipArchive::zipCreateAppend) // create new archive
	{
		m_bReadOnly = false;
		m_uCurrentVolume = 0;
		if (iMode ==  CZipArchive::zipCreate || iMode ==  CZipArchive::zipCreateAppend)
		{
			m_iSegmMode = noSegments;
			OpenFile(lpszPathName, (iMode ==  CZipArchive::zipCreate ? CZipFile::modeCreate : CZipFile::modeNoTruncate) | CZipFile::modeReadWrite);
		}
		else // create a segmented archive
		{
			m_bNewSegm = true;
			m_uBytesWritten = 0;
			if (uVolumeSize == ZIP_AUTODETECT_VOLUME_SIZE) // spanned archive
			{
				if (!m_pSpanChangeVolumeFunc)
					ThrowError(CZipException::noCallback);
				if (!ZipPlatform::IsDriveRemovable(lpszPathName))
					ThrowError(CZipException::nonRemovable);
				m_iSegmMode = spannedArchive;
				m_pChangeVolumeFunc = m_pSpanChangeVolumeFunc;
			}
			else
			{
				m_uSplitData = uVolumeSize;
				m_iSegmMode = splitArchive;
				m_pChangeVolumeFunc = m_pSplitChangeVolumeFunc;
			}

			NextVolume(4);
			Write(m_gszExtHeaderSignat, 4, true);
		}
	}
	else // open existing
	{
		m_bReadOnly = iMode ==  CZipArchive::zipOpenReadOnly;
		OpenFile(lpszPathName, CZipFile::modeNoTruncate |
			(m_bReadOnly ? CZipFile::modeRead : CZipFile::modeReadWrite));
		// m_uData and m_iSegmMode are automatically set during reading the central dir
		m_iSegmMode = uVolumeSize == 0 ? suggestedAuto : suggestedSplit;
	}
		
}


void CZipStorage::Open(CZipAbstractFile& af, int iMode)
{
	m_pWriteBuffer.Allocate(m_iWriteBufferSize); 
	m_uBytesInWriteBuffer = 0;
	m_bNewSegm = false;
	m_pFile = &af;
	m_bInMemory = true;

	if (iMode ==  CZipArchive::zipCreate || iMode ==  CZipArchive::zipCreateAppend)
	{
		m_uCurrentVolume = 0;
		m_iSegmMode = noSegments;
		if (iMode ==  CZipArchive::zipCreate)
			af.SetLength(0);
		else
			af.SeekToEnd();
	}
	else // open existing
	{
		af.SeekToBegin();
		m_iSegmMode = suggestedAuto;
	}
}


void CZipStorage::ChangeVolume(ZIP_VOLUME_TYPE uNumber)
{
	if (uNumber == m_uCurrentVolume || m_iSegmMode == noSegments) // the second condition may happen in some bad archives
		return;

	m_uCurrentVolume = uNumber;
	OpenFile(IsSpanned() ? ChangeSpannedRead() : ChangeSplitRead(),
		CZipFile::modeNoTruncate | CZipFile::modeRead);
}

void CZipStorage::ThrowError(int err)
{
	CZipException::Throw(err, m_pFile->GetFilePath());
}

bool CZipStorage::OpenFile(LPCTSTR lpszName, UINT uFlags, bool bThrow)
{
	return m_pFile->Open(lpszName, uFlags | CZipFile::shareDenyWrite, bThrow);
}


CZipString CZipStorage::ChangeSpannedRead()
{
	CZipString szTemp = m_pFile->GetFilePath();
	m_pFile->Close();
	CallCallback(0, CZipSegmCallback::scVolumeNeededForRead, szTemp);
	return szTemp;
}

CZipString CZipStorage::ChangeSplitRead()
{
	bool lastPart = (ZIP_SIZE_TYPE)m_uCurrentVolume == m_uSplitData;
	CZipString szTemp = GetSplitVolumeName(lastPart);
	if (m_pChangeVolumeFunc)
	{
		int iCode = CZipSegmCallback::scVolumeNeededForRead;
		for(;;)
		{
			CallCallback(lastPart ? ZIP_SPLIT_LAST_VOLUME : 0, iCode, szTemp);
			if (ZipPlatform::FileExists(m_pChangeVolumeFunc->m_szExternalFile))
			{
				szTemp = m_pChangeVolumeFunc->m_szExternalFile;
				break;
			}
			else
				iCode = CZipSegmCallback::scFileNotFound;
		}
	}
	m_pFile->Close();
	return szTemp;
}

CZipString CZipStorage::RenameLastFileInSplitArchive()
{
	ASSERT(IsSplit());
		// give to the last volume the zip extension
	CZipString szFileName = m_pFile->GetFilePath();
	CZipString szNewFileName = GetSplitVolumeName(true);
	if (m_pChangeVolumeFunc)
	{
		int code = CZipSegmCallback::scVolumeNeededForWrite;
		for(;;)
		{
			CallCallback(ZIP_SPLIT_LAST_VOLUME, code, szNewFileName);
			szNewFileName = m_pChangeVolumeFunc->m_szExternalFile;
			if (ZipPlatform::FileExists(szNewFileName))
				code = CZipSegmCallback::scFileNameDuplicated;
			else
				break;
		}
	}
	if (!m_bInMemory)
	{
		m_pFile->Flush();
		m_pFile->Close();
	}
	if (!m_pChangeVolumeFunc && ZipPlatform::FileExists(szNewFileName))
		ZipPlatform::RemoveFile(szNewFileName);
	ZipPlatform::RenameFile(szFileName, szNewFileName);
	return szNewFileName;
}

CZipString CZipStorage::Close(bool bAfterException)
{
	bool bClose = true;
	CZipString sz;
	if (!bAfterException)
	{
		Flush();
		if (IsSplit() && m_bNewSegm)
		{
			sz = RenameLastFileInSplitArchive();
			bClose = false;// already closed in RenameLastFileInSplitArchive
		}
	}
	if (sz.IsEmpty())
		sz = m_pFile->GetFilePath();
	if (bClose && !m_bInMemory)
	{
		if (!bAfterException)
			FlushFile();
		m_pFile->Close();
	}

	m_pWriteBuffer.Release();
	m_uCurrentVolume = ZIP_VOLUME_NUMBER_UNSPECIFIED;
	m_iSegmMode = noSegments;
	m_pFile = NULL;
	m_uBytesBeforeZip = 0;
	return sz;
}

CZipString CZipStorage::GetSplitVolumeName(bool bLast) const
{
	CZipString szFilePath = m_szArchiveName;
	CZipPathComponent zpc(szFilePath);
	CZipString szExt;
	if (bLast)
		szExt = m_szSplitExtension;
	else
	{
		DWORD vol = m_uCurrentVolume + 1;
		if (vol < 100)
			szExt.Format(_T("z%.2u"), vol);
		else
			szExt.Format(_T("z%u"), vol);
	}
	zpc.SetExtension(szExt);
	return zpc.GetFullPath();
}

void CZipStorage::NextVolume(ZIP_SIZE_TYPE uNeeded)
{
	Flush();
	ASSERT(m_iSegmMode != noSegments);
	bool bSpan = IsSpanned();
	if (m_uBytesWritten)
	{
		m_uBytesWritten = 0;
		m_uCurrentVolume++;
		ZIP_VOLUME_TYPE uMaxVolumes = (ZIP_VOLUME_TYPE)(bSpan ? 999 : 0xFFFF);
		if (m_uCurrentVolume >= uMaxVolumes) // m_uCurrentVolume is a zero-based index
			ThrowError(CZipException::tooManyVolumes);
	} 

	CZipString szFileName;
	
	if (bSpan)
		szFileName  = m_szArchiveName;
	else
		szFileName =  GetSplitVolumeName(false);

	if (!m_pFile->IsClosed())
	{
		m_pFile->Flush();
		m_pFile->Close();
	}

	if (m_pChangeVolumeFunc)
	{
		int iCode = CZipSegmCallback::scVolumeNeededForWrite;
		for(;;)
		{
			CallCallback(uNeeded, iCode, szFileName);
			if (!bSpan)
				// allow the user to change the filename
				szFileName = m_pChangeVolumeFunc->m_szExternalFile;

			if (ZipPlatform::FileExists(szFileName))
				iCode = CZipSegmCallback::scFileNameDuplicated;
			else
			{
				if (bSpan)
				{
					CZipString label;
					label.Format(_T("pkback# %.3d"), m_uCurrentVolume + 1);
					if (!ZipPlatform::SetVolLabel(szFileName, label))
					{
						iCode = CZipSegmCallback::scCannotSetVolLabel;
						continue;
					}
				}					
				
				if (OpenFile(szFileName, CZipFile::modeCreate | CZipFile::modeReadWrite, false))
					break;
				else
					iCode = CZipSegmCallback::scFileCreationFailure;
			}

		}
		m_uCurrentVolSize = bSpan ? GetFreeVolumeSpace() : m_uSplitData;
	}
	else
	{
		if (bSpan)
			ThrowError(CZipException::internalError);
		m_uCurrentVolSize = m_uSplitData;
		OpenFile(szFileName, CZipFile::modeCreate | CZipFile::modeReadWrite);
	}
}

void CZipStorage::CallCallback(ZIP_SIZE_TYPE uNeeded, int iCode, CZipString szTemp)
{
	if (!m_pChangeVolumeFunc)
		ThrowError(CZipException::internalError);
	m_pChangeVolumeFunc->m_szExternalFile = szTemp;
	m_pChangeVolumeFunc->m_uVolumeNeeded = (ZIP_VOLUME_TYPE)(m_uCurrentVolume + 1);
	m_pChangeVolumeFunc->m_iCode = iCode; 
	if (!m_pChangeVolumeFunc->Callback(uNeeded))
		CZipException::Throw(CZipException::aborted, szTemp);
}

ZIP_SIZE_TYPE CZipStorage::GetFreeVolumeSpace() const
{
	ASSERT (IsSpanned());
	CZipString szTemp = m_pFile->GetFilePath();
	if (szTemp.IsEmpty()) // called once when creating a segmented archive
		return 0;
	else
	{
		CZipPathComponent zpc(szTemp);
        ULONGLONG ret = ZipPlatform::GetDeviceFreeSpace(zpc.GetFilePath());
        if (ret > (ZIP_SIZE_TYPE)(-1))
                return (ZIP_SIZE_TYPE)(-1);
        else
                return (ZIP_SIZE_TYPE)ret;
	}
}


void CZipStorage::UpdateSegmMode(ZIP_VOLUME_TYPE uLastDisk)
{
	m_uCurrentVolume = uLastDisk;
	if (uLastDisk)
	{
		// segmentation detected
		CZipString szFilePath = m_pFile->GetFilePath();
		if (m_iSegmMode == suggestedAuto)
			m_iSegmMode = ZipPlatform::IsDriveRemovable(szFilePath) ? 
				spannedArchive : splitArchive;
		else
		{
			ASSERT(m_iSegmMode == suggestedSplit);
			m_iSegmMode = splitArchive;
		}

		if (IsSpanned())
		{
			if (!m_pSpanChangeVolumeFunc)
				ThrowError(CZipException::noCallback);
			m_pChangeVolumeFunc = m_pSpanChangeVolumeFunc;
		}
		else /*if (IsSplit())*/
		{
			m_uSplitData = uLastDisk; // volume with .zip extension ( the last one)
			m_pChangeVolumeFunc = m_pSplitChangeVolumeFunc;
		}
		CZipPathComponent zpc(szFilePath);
		m_szSplitExtension = zpc.GetFileExt();
		m_pWriteBuffer.Release(); // no need for this in this case
	}
	else 
		m_iSegmMode = noSegments;

}

ZIP_SIZE_TYPE CZipStorage::AssureFree(ZIP_SIZE_TYPE uNeeded)
{
	ZIP_SIZE_TYPE uFree;
	while ((uFree = VolumeLeft()) < uNeeded)
	{
		if (IsSplit() && !m_uBytesWritten && !m_uBytesInWriteBuffer)
			// in the splitArchive mode, if the size of the archive is less 
			// than the size of the packet to be written at once,
			// increase once the size of the volume
			m_uCurrentVolSize = uNeeded;
		else
			NextVolume(uNeeded);
	}
	return uFree;
}

void CZipStorage::Write(const void *pBuf, DWORD iSize, bool bAtOnce)
{
	if (!IsSegmented())
		WriteInternalBuffer((char*)pBuf, iSize);
	else
	{
		// if not at once, one byte is enough free space
		DWORD iNeeded = bAtOnce ? iSize : 1; 
		DWORD uTotal = 0;

		while (uTotal < iSize)
		{			
			ZIP_SIZE_TYPE uFree = AssureFree(iNeeded);
			DWORD uLeftToWrite = iSize - uTotal;
			DWORD uToWrite = uFree < uLeftToWrite ? (DWORD)uFree : uLeftToWrite;
			WriteInternalBuffer((char*)pBuf + uTotal, uToWrite);
			if (bAtOnce)
				return;
			else
				uTotal += uToWrite;
		}

	}
}


void CZipStorage::WriteInternalBuffer(const char *pBuf, DWORD uSize)
{
	DWORD uWritten = 0;
	while (uWritten < uSize)
	{
		DWORD uFreeInBuffer = GetFreeInBuffer();
		if (uFreeInBuffer == 0)
		{
			Flush();
			uFreeInBuffer = m_pWriteBuffer.GetSize();
		}
		DWORD uLeftToWrite = uSize - uWritten;
		DWORD uToCopy = uLeftToWrite < uFreeInBuffer ? uLeftToWrite : uFreeInBuffer;
		memcpy((char*)m_pWriteBuffer + m_uBytesInWriteBuffer, pBuf + uWritten, uToCopy);
		uWritten += uToCopy;
		m_uBytesInWriteBuffer += uToCopy;
	}
}

ZIP_SIZE_TYPE CZipStorage::VolumeLeft() const
{
	// for spanned archives m_uCurrentVolSize is updated after each flush()
	ZIP_SIZE_TYPE uBytes = m_uBytesInWriteBuffer + (IsSpanned() ? 0 : m_uBytesWritten);	
	return uBytes > m_uCurrentVolSize ? 0 : m_uCurrentVolSize - uBytes;
}

void CZipStorage::Flush()
{	
	if (m_uBytesInWriteBuffer)
	{
		m_pFile->Write(m_pWriteBuffer, m_uBytesInWriteBuffer);
		if (m_iSegmMode != noSegments)
			m_uBytesWritten += m_uBytesInWriteBuffer;
		m_uBytesInWriteBuffer = 0;
	}
	if (IsSpanned()) 
		// after writing it is difficult to predict the free space due to 
		// not completly written clusters, write operation may start from a new cluster
		m_uCurrentVolSize = GetFreeVolumeSpace();
	
}

ZIP_FILE_USIZE CZipStorage::LocateSignature(char* szSignature, ZIP_SIZE_TYPE uMaxDepth)
{
	const int recordSize = 4;
	CZipAutoBuffer buffer(m_iLocateBufferSize);
	ZIP_FILE_USIZE uFileLength = m_pFile->GetLength();
	ZIP_SIZE_TYPE max = (ZIP_SIZE_TYPE)(uFileLength < uMaxDepth ? uFileLength : uMaxDepth);
	ZIP_SIZE_TYPE position = (ZIP_SIZE_TYPE)(uFileLength - m_pFile->GetPosition());
	int offset = 0;
	int leftToFind = recordSize - 1;
	int toRead = m_iLocateBufferSize;
	bool found = false; // for fast checking if leftToFind needs resetting
	while ( position < max )
	{
		position += toRead;
		if ( position > max )
		{
			int diff = (int) ( position - max );
			toRead -= diff;
			offset = diff;
			position = max;
		}
		Seek(position, seekFromEnd);	
		int actuallyRead = m_pFile->Read((char*)buffer + offset, toRead);
		if (actuallyRead != toRead)
			ThrowError(CZipException::badZipFile);
		int pos = m_iLocateBufferSize - 1;
		while ( pos >= offset )
		{
			if ( buffer[pos] == szSignature[leftToFind] )
			{
				if ( leftToFind == 0 )
					return (ZIP_FILE_USIZE)(uFileLength - ( position - ( pos - offset ) ));
				if ( !found )
					found = true;
				leftToFind--;
				pos--;
			}
			else if ( found )
			{
				leftToFind = recordSize - 1;
				found = false;
				// do not decrease position, the current pos may be the first to find
			}
			else
				pos--;
		}
	}
	return SignatureNotFound;
}

ULONGLONG CZipStorage::Seek(ULONGLONG lOff, SeekType iSeekType)
{	
	if (iSeekType == seekCurrent)
	{
		ZIP_SIZE_TYPE uPosition = (ZIP_SIZE_TYPE)m_pFile->GetPosition();
		if (IsSegmented() == -1)
		{
			ZIP_FILE_USIZE uLength = m_pFile->GetLength();
			while (uPosition + lOff >= uLength)
			{
				ZIP_SIZE_TYPE uCanSeek = (ZIP_SIZE_TYPE)(uLength - uPosition);
				lOff -= uCanSeek;
				ChangeVolume();
				uPosition = 0;
				uLength = m_pFile->GetLength();
			}
			return lOff > 0 ? m_pFile->Seek((ZIP_FILE_USIZE)lOff) : 0;
		}
		else
			return m_pFile->Seek((ZIP_FILE_SIZE)lOff, CZipAbstractFile::current);			
	}
	else
	{
		if (m_uCurrentVolume == 0 && m_uBytesBeforeZip > 0)
			lOff += m_uBytesBeforeZip;
		return m_pFile->Seek((ZIP_FILE_USIZE)lOff, iSeekType == seekFromBeginning);
	}
}

void CZipStorage::FinalizeSegm()
{
	ASSERT(IsSegmented() == 1); // spanned archive in creation
	ASSERT(!m_bInMemory);

	CZipString szFileName;
	if (IsSplit() && m_bNewSegm)
		szFileName = RenameLastFileInSplitArchive();
	else
	{
		szFileName = m_pFile->GetFilePath();
		// the file is already closed
		m_pFile->Close();
	}
	m_bNewSegm = false;
	if (m_uCurrentVolume == 0) // one-volume segmented archive was converted to normal archive
		m_iSegmMode = noSegments;
	else
		m_uSplitData = m_uCurrentVolume;
	
	OpenFile(szFileName, CZipFile::modeNoTruncate | (m_iSegmMode == noSegments ? CZipFile::modeReadWrite : CZipFile::modeRead));	
}

