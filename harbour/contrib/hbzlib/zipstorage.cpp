// ZipStorage.cpp: implementation of the CZipStorage class.
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
#include "zipstorage.h"
#include "ziparchive.h"
// #include "ZipPathComponent.h"
#include "zipplatform.h"

//////////////////////////////////////////////////////////////////////
// disk spanning objectives:
// - sinature at the first disk at the beginning
// - headers and central dir records not divided between disks
// - each file has a data descriptor preceded by the signature
//	(bit 3 set in flag);


char CZipStorage::m_gszExtHeaderSignat[] = {0x50, 0x4b, 0x07, 0x08};
CZipStorage::CZipStorage()
{
	m_pCallbackData = m_pChangeDiskFunc = NULL;
	m_iWriteBufferSize = 65535;
	m_iCurrentDisk = -1;
	m_pFile = NULL;
}

CZipStorage::~CZipStorage()
{

}

DWORD CZipStorage::Read(void *pBuf, DWORD iSize, bool bAtOnce)
{
	if (iSize == 0)
		return 0;
	DWORD iRead = 0;
	while (!iRead)
	{
		iRead = m_pFile->Read(pBuf, iSize);
		if (!iRead)
			if (IsSpanMode())
				ChangeDisk(m_iCurrentDisk + 1);
			else
				ThrowError(CZipException::badZipFile);
	}

	if (iRead == iSize)
		return iRead;
	else if (bAtOnce || !IsSpanMode())
		ThrowError(CZipException::badZipFile);

	while (iRead < iSize)
	{
		ChangeDisk(m_iCurrentDisk + 1);
		UINT iNewRead = m_pFile->Read((char*)pBuf + iRead, iSize - iRead);
		if (!iNewRead && iRead < iSize)
			ThrowError(CZipException::badZipFile);
		iRead += iNewRead;
	}

	return iRead;
}

void CZipStorage::Open(LPCTSTR szPathName, int iMode, int iVolumeSize)
{
	m_pWriteBuffer.Allocate(m_iWriteBufferSize); 
	m_uBytesInWriteBuffer = 0;
	m_bNewSpan = false;
	m_pFile = &m_internalfile;
	m_bInMemory = false;

	if ((iMode == CZipArchive::zipCreate) ||(iMode == CZipArchive::zipCreateSpan)) // create new archive
	{
		m_iCurrentDisk = 0;
		if (iMode == CZipArchive::zipCreate)
		{
			m_iSpanMode = noSpan;
			OpenFile(szPathName, CZipFile::modeCreate | CZipFile::modeReadWrite);
		}
		else // create disk spanning archive
		{
			m_bNewSpan = true;
			m_iBytesWritten = 0;
			if (iVolumeSize <= 0) // pkzip span
			{
				if (!m_pChangeDiskFunc)
					ThrowError(CZipException::noCallback);
				if (!ZipPlatform::IsDriveRemovable(szPathName))
					ThrowError(CZipException::nonRemovable);
				m_iSpanMode = pkzipSpan;
			}
			else
			{
				m_iTdSpanData = iVolumeSize;
				m_iSpanMode = tdSpan;
			}

			NextDisk(4, szPathName);
			Write(m_gszExtHeaderSignat, 4, true);
		}
	}
	else // open existing
	{
		OpenFile(szPathName, CZipFile::modeNoTruncate |
			((iMode == CZipArchive::zipOpenReadOnly) ? CZipFile::modeRead : CZipFile::modeReadWrite));
		// m_uData, m_bAllowModif i m_iSpanMode are automatically set during reading the central dir
		m_iSpanMode = iVolumeSize == 0 ? suggestedAuto : suggestedTd;
	}
		
}


void CZipStorage::Open(CZipMemFile& mf, int iMode)
{
	m_pWriteBuffer.Allocate(m_iWriteBufferSize); 
	m_uBytesInWriteBuffer = 0;
	m_bNewSpan = false;
	m_pFile = &mf;
	m_bInMemory = true;

	if (iMode == CZipArchive::zipCreate)
	{
		m_iCurrentDisk = 0;
		m_iSpanMode = noSpan;
		mf.SetLength(0);
	}
	else // open existing
	{
		mf.SeekToBegin();
		m_iSpanMode = suggestedAuto;
	}
}


int CZipStorage::IsSpanMode()
{
	return m_iSpanMode == noSpan ? 0 : (m_bNewSpan ? 1 : -1);
}

void CZipStorage::ChangeDisk(int iNumber)
{
	if (iNumber == m_iCurrentDisk)
		return;

	ASSERT(m_iSpanMode != noSpan);
	m_iCurrentDisk = iNumber;
	OpenFile(m_iSpanMode == pkzipSpan ? ChangePkzipRead() : ChangeTdRead(),
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

int CZipStorage::GetCurrentDisk()
{
	return m_iCurrentDisk;
}

CZipString CZipStorage::ChangePkzipRead()
{
	CZipString szTemp = m_pFile->GetFilePath();
	m_pFile->Close();
	CallCallback(-1 , szTemp);
	return szTemp;
}

CZipString CZipStorage::ChangeTdRead()
{
	CZipString szTemp = GetTdVolumeName(m_iCurrentDisk == m_iTdSpanData);
	m_pFile->Close();
	return szTemp;
}

void CZipStorage::Close(bool bAfterException)
{
	if (!bAfterException)
	{
		Flush();
		if ((m_iSpanMode == tdSpan) && (m_bNewSpan))
		{
			// give to the last volume the zip extension
			CZipString szFileName = m_pFile->GetFilePath();
			CZipString szNewFileName = GetTdVolumeName(true);
			if (!m_bInMemory)
				m_pFile->Close();
			if (ZipPlatform::FileExists(szNewFileName))
				ZipPlatform::RemoveFile(szNewFileName);
			ZipPlatform::RenameFile(szFileName, szNewFileName);
		}
		else
		{
			if (!m_bInMemory)
				m_pFile->Close();
		}
	}
	else
	{
		if (!m_bInMemory)
				m_pFile->Close();
	}
		


	m_pWriteBuffer.Release();
	m_iCurrentDisk = -1;
	m_iSpanMode = noSpan;
	m_pFile = NULL;
}

CZipString CZipStorage::GetTdVolumeName(bool bLast, LPCTSTR lpszZipName)
{
	CZipString szFilePath = lpszZipName ? lpszZipName : (LPCTSTR)m_pFile->GetFilePath();
	CZipPathComponent zpc(szFilePath);
	CZipString szExt;
	if (bLast)
		szExt = _T("zip");
	else
		szExt.Format(_T("%.3d"), m_iCurrentDisk);
	zpc.SetExtension(szExt);
	return zpc.GetFullPath();
}

void CZipStorage::NextDisk(int iNeeded, LPCTSTR lpszFileName)
{
	Flush();
	ASSERT(m_iSpanMode != noSpan);
	if (m_iBytesWritten)
	{
		m_iBytesWritten = 0;
		m_iCurrentDisk++;
		if (m_iCurrentDisk >= 999)
			ThrowError(CZipException::tooManyVolumes);
	} 
	CZipString szFileName;
	bool bPkSpan = (m_iSpanMode == pkzipSpan);
	if (bPkSpan)
		szFileName  = lpszFileName ? lpszFileName : (LPCTSTR)m_pFile->GetFilePath();
	else
		szFileName =  GetTdVolumeName(false, lpszFileName);

	m_pFile->Close();

	if (bPkSpan)
	{
		int iCode = iNeeded;
		while (true)
		{
			CallCallback(iCode, szFileName);
			if (ZipPlatform::FileExists(szFileName))
				iCode = -2;
			else
			{
				CZipString label;
				label.Format(_T("pkback# %.3d"), m_iCurrentDisk + 1);
				if (!ZipPlatform::SetVolLabel(szFileName, label))
					iCode = -3;
				else if (!OpenFile(szFileName, CZipFile::modeCreate | CZipFile::modeReadWrite, false))
					iCode = -4;
				else
					break;
			}

		}
		m_uCurrentVolSize = GetFreeVolumeSpace();
	}
	else
	{
		m_uCurrentVolSize = m_iTdSpanData;
		OpenFile(szFileName, CZipFile::modeCreate | CZipFile::modeReadWrite);
	}
}

void CZipStorage::CallCallback(int iCode, CZipString szTemp)
{
	ASSERT(m_pChangeDiskFunc);
	if (!(*m_pChangeDiskFunc)(m_iCurrentDisk + 1, iCode, m_pCallbackData))
		CZipException::Throw(CZipException::aborted, szTemp);
}

DWORD CZipStorage::GetFreeVolumeSpace()
{
	ASSERT (m_iSpanMode == pkzipSpan);
	CZipString szTemp = m_pFile->GetFilePath();
	if (szTemp.IsEmpty()) // called once when creating a disk spanning archive
		return 0;
	else
		return ZipPlatform::GetDeviceFreeSpace(szTemp);
}


void CZipStorage::UpdateSpanMode(WORD uLastDisk)
{
	m_iCurrentDisk = uLastDisk;
	if (uLastDisk)
	{
		// disk spanning detected

		if (m_iSpanMode == suggestedAuto)
			m_iSpanMode = ZipPlatform::IsDriveRemovable(m_pFile->GetFilePath()) ? 
				pkzipSpan : tdSpan;
		else
			m_iSpanMode = tdSpan;

		if (m_iSpanMode == pkzipSpan)
		{
			if (!m_pChangeDiskFunc)
					ThrowError(CZipException::noCallback);
		}
		else /*if (m_iSpanMode == tdSpan)*/
			m_iTdSpanData = uLastDisk; // disk with .zip extension ( the last one)
			
		m_pWriteBuffer.Release(); // no need for this in this case
	}
	else 
		m_iSpanMode = noSpan;

}

void CZipStorage::Write(const void *pBuf, DWORD iSize, bool bAtOnce)
{
	if (!IsSpanMode())
		WriteInternalBuffer((char*)pBuf, iSize);
	else
	{
		// if not at once, one byte is enough free space
		DWORD iNeeded = bAtOnce ? iSize : 1; 
		DWORD uTotal = 0;

		while (uTotal < iSize)
		{
			DWORD uFree;
			while ((uFree = VolumeLeft()) < iNeeded)
			{
				if ((m_iSpanMode == tdSpan) && !m_iBytesWritten && !m_uBytesInWriteBuffer)
					// in the tdSpan mode, if the size of the archive is less 
					// than the size of the packet to be written at once,
					// increase once the size of the volume
					m_uCurrentVolSize = iNeeded;
				else
					NextDisk(iNeeded);
			}

			DWORD uLeftToWrite = iSize - uTotal;
			DWORD uToWrite = uFree < uLeftToWrite ? uFree : uLeftToWrite;
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
		memcpy(m_pWriteBuffer + m_uBytesInWriteBuffer, pBuf + uWritten, uToCopy);
		uWritten += uToCopy;
		m_uBytesInWriteBuffer += uToCopy;
	}
}

DWORD CZipStorage::VolumeLeft()
{
	// for pkzip span m_uCurrentVolSize is updated after each flush()
	return m_uCurrentVolSize  - m_uBytesInWriteBuffer - ((m_iSpanMode == pkzipSpan) ? 0 : m_iBytesWritten);
}

void CZipStorage::Flush()
{
	m_iBytesWritten += m_uBytesInWriteBuffer;
	if (m_uBytesInWriteBuffer)
	{
		m_pFile->Write(m_pWriteBuffer, m_uBytesInWriteBuffer);
		m_uBytesInWriteBuffer = 0;
	}
	if (m_iSpanMode == pkzipSpan) 
		// after writing it is difficult to predict the free space due to 
		// not completly written clusters, write operation may start from 
		// the new cluster
		m_uCurrentVolSize = GetFreeVolumeSpace();
}

DWORD CZipStorage::GetPosition()
{
	return m_pFile->GetPosition() + m_uBytesInWriteBuffer;
}


DWORD CZipStorage::GetFreeInBuffer()
{
	return m_pWriteBuffer.GetSize() - m_uBytesInWriteBuffer;
}
