// ZipArchive.cpp: implementation of the CZipArchive class.
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
#include "ziparchive.h"
// #include "zippathcomponent.h"
#include "zipplatform.h"
#include "zipcompatibility.h"

#include <time.h>

#ifndef DEF_MEM_LEVEL
#if MAX_MEM_LEVEL >= 8
#  define DEF_MEM_LEVEL 8
#else
#  define DEF_MEM_LEVEL  MAX_MEM_LEVEL
#endif
#endif
#define ENCR_HEADER_LEN 12
//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
const TCHAR CZipArchive::m_gszCopyright[] = {_T("ZipArchive library Copyright 2000 - 2001 Tadeusz Dracz")};
CZipArchive::CZipArchive()
{
	m_bDetectZlibMemoryLeaks = true;
	m_centralDir.m_pStorage= &m_storage;
	m_info.m_stream.zalloc = (alloc_func)_zliballoc;
	m_info.m_stream.zfree = (free_func)_zlibfree;
	m_iFileOpened = nothing;
}


CZipArchive::~CZipArchive()
{
	// 	Close(); // cannot be here: if an exception is thrown strange things can happen
	EmptyPtrList();	

}



void CZipArchive::Open(LPCTSTR szPathName, int iMode, int iVolumeSize)
{
	if (!IsClosed())
	{
		TRACE(_T("ZipArchive already opened.\n"));
		return;
	}
	m_storage.Open(szPathName, iMode, iVolumeSize);
	OpenInternal(iMode);
}

void CZipArchive::Open(CZipMemFile& mf,int iMode)
{
	if (!IsClosed())
	{
		TRACE(_T("ZipArchive already opened.\n"));
		return;
	}
	if (iMode != zipOpen && iMode != zipOpenReadOnly && iMode != zipCreate)
	{
		TRACE(_T("Mode not supported.\n"));
		return;
	}
	m_storage.Open(mf, iMode);
	OpenInternal(iMode);
}


void CZipArchive::OpenInternal(int iMode)
{
	m_pszPassword.Release();
	m_iFileOpened = nothing;
	m_centralDir.Init();
	m_iArchiveSystCompatib = ZipPlatform::GetSystemID();
	m_szRootPath.Empty();
	if ((iMode == zipOpen) ||(iMode == zipOpenReadOnly))
	{
		m_centralDir.Read();
		// if there is at least one file, get system comp. from the first one
		if (m_centralDir.IsValidIndex(0))
		{
			m_iArchiveSystCompatib = m_centralDir[0]->GetSystemCompatibility();
			if (!ZipCompatibility::IsPlatformSupported(m_iArchiveSystCompatib))
					CZipException::Throw(CZipException::platfNotSupp);
		}
	}

}


bool CZipArchive::IsClosed(bool bArchive)
{
	return  bArchive ?(m_storage.GetCurrentDisk() == -1):(!m_storage.m_pFile || m_storage.m_pFile->IsClosed());
}


void CZipArchive::ThrowError(int err, bool bZlib)
{
	if (bZlib)
		err = CZipException::ZlibErrToZip(err);
	CZipException::Throw(err, IsClosed() ? _T("") : (LPCTSTR)m_storage.m_pFile->GetFilePath());
}

bool CZipArchive::DeleteFile(WORD uIndex)
{
	if (m_storage.IsSpanMode())
	{
		TRACE(_T("You cannot delete files from the disk spannig archive.\n"));
		return false;
	}
	
	if (m_iFileOpened)
	{
		TRACE(_T("You cannot delete files if there is a file opened.\n"));
		return false;
	}
	
	if (!m_centralDir.IsValidIndex(uIndex))
		return false;
	
	m_info.Init();
	m_centralDir.RemoveFromDisk();
	DeleteInternal(uIndex);
	m_info.m_pBuffer.Release();
	return true;
}

int CZipArchive::GetNoEntries()
{
	return m_centralDir.m_headers.GetCount();
}


bool CZipArchive::GetFileInfo(CZipFileHeader & fhInfo, WORD uIndex)
{
	if (IsClosed())
	{
		TRACE(_T("ZipArchive is closed.\n"));
		return false;
	}
	
	if (!m_centralDir.IsValidIndex(uIndex))
		return false;
	
	fhInfo = *(m_centralDir[uIndex]);
	m_centralDir.ConvertFileName(true, false, &fhInfo);
	return true;
}

int CZipArchive::FindFile(LPCTSTR lpszFileName, bool bCaseSensitive)
{
	if (IsClosed())
	{
		TRACE(_T("ZipArchive is closed.\n"));
		return (int)-1;
	}
	// this is required for fast finding and is done only once
	if (!m_centralDir.m_bConvertAfterOpen)
	{
		TRACE(_T("Converting all filenames."));
		m_centralDir.ConvertAll();
	}
	if (!m_centralDir.m_bFindFastEnabled)
		EnableFindFast();
	int iResult = m_centralDir.FindFileNameIndex(lpszFileName, bCaseSensitive);
	return iResult == -1 ? -1 : m_centralDir.m_findarray[iResult].m_uIndex;
}

bool CZipArchive::OpenFile(WORD uIndex)
{	
	if (!m_centralDir.IsValidIndex(uIndex))
		return false;	
	if (m_storage.IsSpanMode() == 1)
	{
		TRACE(_T("You cannot extract from the span in creation.\n"));
		return false;
	}
	
	
	if (m_iFileOpened)
	{
		TRACE(_T("A file already opened.\n"));
		return false;
	}
	
	m_info.Init();
	m_centralDir.OpenFile(uIndex);
	if (CurrentFile()->IsEncrypted())
	{
		
		if (m_pszPassword.GetSize() == 0)
		{
			TRACE(_T("Password not set for the encrypted file.\n"));
			return false;
		}
		CryptInitKeys();
		if (!CryptCheck())
			ThrowError(CZipException::badPassword); // invalid password

	}
	else if (m_pszPassword.GetSize() != 0)
	{
		TRACE(_T("Password set for a not encrypted file. Ignoring password.\n"));
	}
	
	WORD uMethod = CurrentFile()->m_uMethod;

	if ((uMethod != 0) &&(uMethod != Z_DEFLATED))
		ThrowError(CZipException::badZipFile);
			
	if (uMethod == Z_DEFLATED)
	{
		m_info.m_stream.opaque =  m_bDetectZlibMemoryLeaks ? &m_list : 0;
		int err = inflateInit2(&m_info.m_stream, -MAX_WBITS);
		//			* windowBits is passed < 0 to tell that there is no zlib header.
		//          * Note that in this case inflate *requires* an extra "dummy" byte
		//          * after the compressed stream in order to complete decompression and
		//          * return Z_STREAM_END. 
		CheckForError(err);
	}
	m_info.m_uComprLeft = CurrentFile()->m_uComprSize;
	if (CurrentFile()->IsEncrypted())
		m_info.m_uComprLeft -= ENCR_HEADER_LEN;
	m_info.m_uUncomprLeft = CurrentFile()->m_uUncomprSize;
	m_info.m_uCrc32 = 0;
	m_info.m_stream.total_out = 0;
	m_info.m_stream.avail_in = 0;
	
	m_iFileOpened = extract;
	return true;
}


int CZipArchive::GetLocalExtraField(char *pBuf, int iSize)
{
	if (IsClosed())
	{
		TRACE(_T("ZipArchive is closed.\n"));
		return -1;
	}
	
	if (m_iFileOpened != extract)
	{
		TRACE(_T("A file must be opened to get the local extra field.\n"));
		return -1;
	}
	
	int size = m_centralDir.m_pLocalExtraField.GetSize();
	if (!pBuf|| !size)
		return size;
	
	if (iSize < size)
		size = iSize;
	
	memcpy(pBuf, m_centralDir.m_pLocalExtraField, size);
	return size;
}

void* CZipArchive::_zliballoc(void* opaque, UINT items, UINT size)
{
	void* p = new char[size * items];
	if (opaque)
	{
		CZipPtrList<void*>* list  = (CZipPtrList<void*>*) opaque;
		list->AddTail(p);
	}
	return p;
}

void CZipArchive::_zlibfree(void* opaque, void* address)
{
	if (opaque)
	{
		CZipPtrList<void*>* list  = (CZipPtrList<void*>*) opaque;
		CZipPtrListIter iter = list->Find(address);
		if (list->IteratorValid(iter))
			list->RemoveAt(iter);
	}
	delete[] address;
}

void CZipArchive::CheckForError(int iErr)
{
	if ((iErr == Z_OK) ||(iErr == Z_NEED_DICT))
		return;
	
	ThrowError(iErr, true);
}

CZipFileHeader* CZipArchive::CurrentFile()
{
	ASSERT(m_centralDir.m_pOpenedFile);
	return m_centralDir.m_pOpenedFile;
}

DWORD CZipArchive::ReadFile(void *pBuf,     
                            DWORD iSize)
{
	if (m_iFileOpened != extract)
	{
		TRACE(_T("Current file must be opened.\n"));
		return 0;
	}
	
	if (!pBuf || !iSize)
		return 0;
	
	m_info.m_stream.next_out = (Bytef*)pBuf;
	m_info.m_stream.avail_out = iSize > m_info.m_uUncomprLeft 
		? m_info.m_uUncomprLeft : iSize;
	
	
	DWORD iRead = 0;

	// may happen when the file is 0 sized
	bool bForce = m_info.m_stream.avail_out == 0 && m_info.m_uComprLeft > 0;
	while (m_info.m_stream.avail_out > 0 || (bForce && m_info.m_uComprLeft > 0))
	{
		if ((m_info.m_stream.avail_in == 0) &&
			(m_info.m_uComprLeft > 0))
		{
			DWORD uToRead = m_info.m_pBuffer.GetSize();
			if (m_info.m_uComprLeft < uToRead)
				uToRead = m_info.m_uComprLeft;
			
			if (uToRead == 0)
				return 0;
			
			m_storage.Read(m_info.m_pBuffer, uToRead, false);
			CryptDecodeBuffer(uToRead);
			m_info.m_uComprLeft -= uToRead;
			
			m_info.m_stream.next_in = (Bytef*)(char*)m_info.m_pBuffer;
			m_info.m_stream.avail_in = uToRead;
		}
		
		if (CurrentFile()->m_uMethod == 0)
		{
			DWORD uToCopy = m_info.m_stream.avail_out < m_info.m_stream.avail_in 
				? m_info.m_stream.avail_out : m_info.m_stream.avail_in;
			
			memcpy(m_info.m_stream.next_out, m_info.m_stream.next_in, uToCopy);
			
			m_info.m_uCrc32 = crc32(m_info.m_uCrc32, m_info.m_stream.next_out, uToCopy);
			
			m_info.m_uUncomprLeft -= uToCopy;
			m_info.m_stream.avail_in -= uToCopy;
			m_info.m_stream.avail_out -= uToCopy;
			m_info.m_stream.next_out += uToCopy;
			m_info.m_stream.next_in += uToCopy;
            m_info.m_stream.total_out += uToCopy;
			iRead += uToCopy;
		}
		else
		{
			DWORD uTotal = m_info.m_stream.total_out;
			Bytef* pOldBuf =  m_info.m_stream.next_out;
			int err = inflate(&m_info.m_stream, Z_SYNC_FLUSH);
			DWORD uToCopy = m_info.m_stream.total_out - uTotal;
			
			m_info.m_uCrc32 = crc32(m_info.m_uCrc32, pOldBuf, uToCopy);
			
			m_info.m_uUncomprLeft -= uToCopy;
			iRead += uToCopy;
            
			if (err == Z_STREAM_END)
				return iRead;
			
			CheckForError(err);
		}
	}
	
	return iRead;
}

void CZipArchive::Close(bool bAfterException)
{
	// if after an exception - the archive may be closed, but the file may be opened
	if (IsClosed() && (!bAfterException || IsClosed(false)))
	{
		TRACE(_T("ZipArchive is already closed.\n"));
		return;
	}
	
	if (!bAfterException)
	{
		if (m_iFileOpened == extract)
			CloseFile(NULL);
		
		if (m_iFileOpened == compress)
			CloseNewFile();

		// write central directory
		m_centralDir.Write();
	}
	else
	{
		m_info.m_pBuffer.Release();
		m_iFileOpened = nothing;
		EmptyPtrList();
	}

	m_centralDir.Clear();
	m_storage.Close(bAfterException);
	
}

void CZipArchive::SetSpanCallback(ZIPCALLBACKFUN pFunc, void* pData)
{
	m_storage.m_pChangeDiskFunc = pFunc;
	m_storage.m_pCallbackData = pData;
}

void CZipArchive::SetAdvanced(int iWriteBuffer, int iExtractBuffer, int iSearchBuffer)
{
	if (!IsClosed())
	{
		TRACE(_T("Set this options before opening the archive.\n"));
		return;
	}
	
	m_storage.m_iWriteBufferSize = iWriteBuffer < 1024 ? 1024 : iWriteBuffer;
	m_info.m_iBufferSize = iExtractBuffer < 1024 ? 1024 : iExtractBuffer;
	m_centralDir.m_iBufferSize = iSearchBuffer < 1024 ? 1024 : iSearchBuffer;
}

int CZipArchive::CloseFile(CZipFile &file)
{
	CZipString temp = file.GetFilePath();
	file.Close();
	return CloseFile(temp);
}

int CZipArchive::CloseFile(LPCTSTR lpszFilePath, bool bAfterException)
{
	if (m_iFileOpened != extract)
	{
		TRACE(_T("No opened file.\n"));
		return false;
	}

	int iRet = 1;
	if (!bAfterException)
	{
		if (m_info.m_uUncomprLeft == 0)
		{
			if (m_info.m_uCrc32 != CurrentFile()->m_uCrc32)
				ThrowError(CZipException::badCrc);
		}
		else
			iRet = -1;

				
		if (CurrentFile()->m_uMethod == Z_DEFLATED)
			inflateEnd(&m_info.m_stream);
		
		
		if (lpszFilePath)
		{
			
			if (!ZipCompatibility::IsPlatformSupported(CurrentFile()->GetSystemCompatibility())
				// the line above is to avoid exception in GetSystemAttr(), we cannot
				// catch it here because we wouldn't know what to do with it (delete or what)								
				||!ZipPlatform::SetFileModTime(lpszFilePath, CurrentFile()->GetTime())
				||!ZipPlatform::SetFileAttr(lpszFilePath, CurrentFile()->GetSystemAttr()))
					iRet = -2;
		}
	}
	m_centralDir.CloseFile();
	m_iFileOpened = nothing;
	m_info.m_pBuffer.Release();
	EmptyPtrList();
	return iRet;
}

bool CZipArchive::OpenNewFile(CZipFileHeader & header,     
                              int iLevel,                  
                              LPCTSTR lpszFilePath)
{
	if (IsClosed())
	{
		TRACE(_T("ZipArchive is closed.\n"));
		return false;
	}
	
	if (m_iFileOpened)
	{
		TRACE(_T("A file already opened.\n"));
		return false;
	}
	
	if (m_storage.IsSpanMode() == -1)
	{
		TRACE(_T("You cannot add the files to the existing disk spannig archive.\n"));
		return false;
	}
	
	if (GetNoEntries() ==(WORD)USHRT_MAX)
	{
		TRACE(_T("Maximum file count inside archive reached.\n"));
		return false;
	}
	
	DWORD uAttr = 0; // ..compiler
	time_t ttime;
	int iCode = 0;
	if (lpszFilePath)
	{
		iCode = (ZipPlatform::GetFileAttr(lpszFilePath, uAttr)
			&& ZipPlatform::GetFileModTime(lpszFilePath, ttime)) ? 1 : -1;
		if (iCode == -1)
			// do not continue - if the file was a directory then not recognizing it will cause 
			// serious errors (need uAttr to recognize it)
			return false;
	}

	m_info.Init();
	
	
	if (iCode == 1)
	{
		header.SetTime(ttime);
		SetFileHeaderAttr(header, uAttr); // set system compatibility as well
	}
	else
		header.SetSystemCompatibility(m_iArchiveSystCompatib);

	m_centralDir.AddNewFile(header);


	CZipString szFileName = CurrentFile()->GetFileName();
	if (szFileName.IsEmpty())
	{
		szFileName.Format(_T("file%i"), GetNoEntries());
		CurrentFile()->SetFileName(szFileName);
	}

	bool bIsDirectory = CurrentFile()->IsDirectory();
	if (bIsDirectory)
	{
		CZipString szEnding = szFileName.Right(1);
		if (szEnding != _T("/") && szEnding != _T("\\"))
		{
			szFileName += CZipPathComponent::m_cSeparator;
			CurrentFile()->SetFileName(szFileName);
		}
	}




	// this ensures the conversion will take place anyway (must take because we are going 
	// 	to write the local header in a moment
	m_centralDir.ConvertFileName(false, m_centralDir.m_bConvertAfterOpen);


	
	bool bEncrypted = m_pszPassword.GetSize() != 0;

#ifdef _DEBUG
	if (bIsDirectory && bEncrypted)
		TRACE(_T("Warning! Encrypting a directory. Possible but pointless.\n\
		Clear the password before adding a directory.\n"));
#endif	

	if (!CurrentFile()->PrepareData(iLevel, m_storage.IsSpanMode() == 1, bEncrypted))
			ThrowError(CZipException::tooLongFileName);

	CurrentFile()->WriteLocal(m_storage);

	// we have written the local header, but if we keep filenames not converted
	// in memory , we have to restore the non-converted value
	if (m_centralDir.m_bConvertAfterOpen)
		CurrentFile()->SetFileName(szFileName);

	if (bEncrypted)
	{
		CZipAutoBuffer buf(ENCR_HEADER_LEN);
		// use pseudo-crc since we don't know it yet
		CryptCryptHeader((long)header.m_uModTime << 16, buf);
		m_storage.Write(buf, ENCR_HEADER_LEN, false);
	}
	
	
	m_info.m_uComprLeft = 0;
    m_info.m_stream.avail_in = (uInt)0;
    m_info.m_stream.avail_out = (uInt)m_info.m_pBuffer.GetSize();
    m_info.m_stream.next_out = (Bytef*)(char*)m_info.m_pBuffer;
    m_info.m_stream.total_in = 0;
    m_info.m_stream.total_out = 0;
	
	if (bIsDirectory && (CurrentFile()->m_uMethod != 0))
		CurrentFile()->m_uMethod = 0;
	
	if (CurrentFile()->m_uMethod == Z_DEFLATED)
    {
        m_info.m_stream.opaque = m_bDetectZlibMemoryLeaks ? &m_list : 0;
		
        int err = deflateInit2(&m_info.m_stream, iLevel,
			Z_DEFLATED, -MAX_WBITS, DEF_MEM_LEVEL, Z_DEFAULT_STRATEGY);
		
		CheckForError(err);
    }
	m_iFileOpened = compress;
	return true;
}


bool CZipArchive::ExtractFile(WORD uIndex,                  
                              LPCTSTR lpszPath,             
                              bool bFullPath,              
                              LPCTSTR lpszNewName,          
                              ZIPCALLBACKFUN pCallback,     
                              void* pUserData,              
                              DWORD nBufSize)
{
	if (!nBufSize && !lpszPath)
		return false;
	
	CZipFileHeader header;
	GetFileInfo(header, uIndex); // to ensure that slash and oem conversions take place
	CZipString szFile = lpszPath;
	CZipPathComponent::AppendSeparator(szFile);
	CZipPathComponent zpc(lpszNewName ? lpszNewName : (LPCTSTR)header.GetFileName());
	szFile += bFullPath ? zpc.GetNoDrive() : TrimRootPath(zpc);

	if (header.IsDirectory())
	{
		ZipPlatform::ForceDirectory(szFile);
		ZipPlatform::SetFileAttr(szFile, header.GetSystemAttr());
	}
	else
	{
		if (!OpenFile(uIndex))
			return false;

		CZipPathComponent zpc(szFile);
		ZipPlatform::ForceDirectory(zpc.GetFilePath());
		CZipFile f(szFile, CZipFile::modeWrite | 
			CZipFile::modeCreate | CZipFile::shareDenyWrite);
		DWORD iRead, iFileLength = pCallback ? header.GetSize() : 0, iSoFar = 0;
		CZipAutoBuffer buf(nBufSize);
		do
		{
			iRead = ReadFile(buf, buf.GetSize());
			if (iRead)
			{	
				f.Write(buf, iRead);
				iSoFar += iRead;
				if (pCallback)
					if (!pCallback(iFileLength, iSoFar, pUserData))
						break;
			}
		}
		while (iRead == buf.GetSize());
		return CloseFile(f) == 1;
	}	
	return true;
}

void CZipArchive::SetExtraField(const char *pBuf, WORD iSize)
{
	if (m_iFileOpened != compress)
	{
		TRACE(_T("A new file must be opened.\n"));
		return;
	}
	if (!pBuf || !iSize)
		return;
	
	CurrentFile()->m_pExtraField.Allocate(iSize);
	memcpy(CurrentFile()->m_pExtraField, pBuf, iSize);
}

bool CZipArchive::WriteNewFile(const void *pBuf, DWORD iSize)
{
	if (m_iFileOpened != compress)
	{
		TRACE(_T("A new file must be opened.\n"));
		return false;
	}
	
	
    m_info.m_stream.next_in = (Bytef*)pBuf;
    m_info.m_stream.avail_in = iSize;
    CurrentFile()->m_uCrc32 = crc32(CurrentFile()->m_uCrc32, (Bytef*)pBuf, iSize);
	
	
    while (m_info.m_stream.avail_in > 0)
    {
        if (m_info.m_stream.avail_out == 0)
        {
			CryptEncodeBuffer();
			m_storage.Write(m_info.m_pBuffer, m_info.m_uComprLeft, false);
			m_info.m_uComprLeft = 0;
            m_info.m_stream.avail_out = m_info.m_pBuffer.GetSize();
            m_info.m_stream.next_out = (Bytef*)(char*)m_info.m_pBuffer;
        }
		
        if (CurrentFile()->m_uMethod == Z_DEFLATED)
        {
            DWORD uTotal = m_info.m_stream.total_out;
            int err = deflate(&m_info.m_stream,  Z_NO_FLUSH);
			CheckForError(err);
            m_info.m_uComprLeft += m_info.m_stream.total_out - uTotal;
        }
        else
        {
            DWORD uToCopy = (m_info.m_stream.avail_in < m_info.m_stream.avail_out) 
				? m_info.m_stream.avail_in : m_info.m_stream.avail_out;
			
			memcpy(m_info.m_stream.next_out, m_info.m_stream.next_in, uToCopy);
			
            m_info.m_stream.avail_in -= uToCopy;
            m_info.m_stream.avail_out -= uToCopy;
            m_info.m_stream.next_in += uToCopy;
            m_info.m_stream.next_out += uToCopy;
            m_info.m_stream.total_in += uToCopy;
            m_info.m_stream.total_out += uToCopy;
            m_info.m_uComprLeft += uToCopy;
        }
    }
	
	return true;
}

bool CZipArchive::CloseNewFile()
{
	if (m_iFileOpened != compress)
	{
		TRACE(_T("A new file must be opened.\n"));
		return false;
	}
	
    m_info.m_stream.avail_in = 0;
    
	int err = Z_OK;
    if (CurrentFile()->m_uMethod == Z_DEFLATED)
        while (err == Z_OK)
		{
			if (m_info.m_stream.avail_out == 0)
			{
				CryptEncodeBuffer();
				m_storage.Write(m_info.m_pBuffer, m_info.m_uComprLeft, false);
				m_info.m_uComprLeft = 0;
				m_info.m_stream.avail_out = m_info.m_pBuffer.GetSize();
				m_info.m_stream.next_out = (Bytef*)(char*)m_info.m_pBuffer;
			}
			DWORD uTotal = m_info.m_stream.total_out;
			err = deflate(&m_info.m_stream,  Z_FINISH);
			m_info.m_uComprLeft += m_info.m_stream.total_out - uTotal;
		}
		
	if (err == Z_STREAM_END)
		err = Z_OK;
	CheckForError(err);
	
	if (m_info.m_uComprLeft > 0)
	{
		CryptEncodeBuffer();
		m_storage.Write(m_info.m_pBuffer, m_info.m_uComprLeft, false);
	}
	
	if (CurrentFile()->m_uMethod == Z_DEFLATED)
	{
		err = deflateEnd(&m_info.m_stream);
		CheckForError(err);
	}
	
	
	// it may be increased by the encrypted header size
	CurrentFile()->m_uComprSize += m_info.m_stream.total_out;
	CurrentFile()->m_uUncomprSize = m_info.m_stream.total_in;
	
	m_centralDir.CloseNewFile();
	m_iFileOpened = nothing;
	m_info.m_pBuffer.Release();
	EmptyPtrList();
	return true;
}

void CZipArchive::DeleteFiles(const CZipStringArray &aNames,   
                              bool bCaseSensitive)
{
	CZipWordArray indexes;
	
	for (WORD i = 0; i < GetNoEntries(); i++)
	{
		CZipFileHeader fh;
		GetFileInfo(fh, i);
		CZipString szFileName = fh.GetFileName();
		for (int j = 0; j < aNames.GetSize(); j++)
		{
			bool bEqual = (bCaseSensitive ? aNames[j].Collate(szFileName)
				: aNames[j].CollateNoCase(szFileName)) == 0;
			if (bEqual)
			{
				indexes.Add(i);
				break;
			}
		}
	}
	
	DeleteFiles(indexes);
}


void CZipArchive::DeleteFiles(CZipWordArray &aIndexes)
{
	if (IsClosed())
	{
		TRACE(_T("ZipArchive is closed.\n"));
		return;
	}
	
	if (m_storage.IsSpanMode())
	{
		TRACE(_T("You cannot delete files from the disk spannig archive.\n"));
		return;
	}
	
	if (m_iFileOpened)
	{
		TRACE(_T("You cannot delete files if there is a file opened.\n"));
		return;
	}
	
	// sorting the index table

	int uSize = aIndexes.GetSize();
	if (!uSize)
		return;

	aIndexes.Sort(true);
	
	m_centralDir.RemoveFromDisk();
	
	m_info.Init();
	// remove in a reverse order
	for (int i = uSize - 1; i >= 0; i--)
		DeleteInternal(aIndexes[i]);
	m_info.m_pBuffer.Release();
}

DWORD CZipArchive::RemovePackedFile(DWORD uStartOffset, DWORD uEndOffset)
{
	uStartOffset += m_centralDir.m_uBytesBeforeZip;
	uEndOffset += m_centralDir.m_uBytesBeforeZip;
	DWORD BytesToCopy = m_storage.m_pFile->GetLength() - uEndOffset;
	DWORD uTotalToWrite = BytesToCopy;
	
	char* buf = (char*)m_info.m_pBuffer;
	if (BytesToCopy > m_info.m_pBuffer.GetSize()) 
		BytesToCopy = m_info.m_pBuffer.GetSize();
	
	DWORD TotalWritten = 0;
	DWORD size_read;
	
	do
	{
		m_storage.m_pFile->Seek(uEndOffset + TotalWritten, CZipAbstractFile::begin);
		size_read = m_storage.m_pFile->Read(buf, BytesToCopy);
		if (size_read > 0)
		{
			m_storage.m_pFile->Seek(uStartOffset + TotalWritten, CZipAbstractFile::begin);
			m_storage.m_pFile->Write(buf, size_read);
			TotalWritten += size_read;
		}
		
	}
	while (size_read == BytesToCopy);
	if (uTotalToWrite != TotalWritten)
		ThrowError(CZipException::generic);
	DWORD uRemoved = (uEndOffset - uStartOffset);
	m_storage.m_pFile->SetLength(m_storage.m_pFile->GetLength() - uRemoved);
	return uRemoved;
}


void CZipArchive::DeleteInternal(WORD uIndex)
{
	CZipFileHeader* pfh = m_centralDir[uIndex];
	DWORD uOtherOffsetChanged = 0;
	
	if (uIndex == GetNoEntries() - 1) // last entry or the only one entry
		m_storage.m_pFile->SetLength(pfh->m_uOffset + m_centralDir.m_uBytesBeforeZip);						
	else
		uOtherOffsetChanged = RemovePackedFile(pfh->m_uOffset, m_centralDir[uIndex + 1]->m_uOffset);
	
	
	m_centralDir.RemoveFile(uIndex);
	
	// teraz uaktualnij offsety w pozosta³ych pozycjach central dir 
	// (update offsets in file headers in the central dir)
	if (uOtherOffsetChanged)
	{
		for (CZipCentralDir::CZipFileHdrLstIter iter = m_centralDir.GetIterator(uIndex);
			m_centralDir.m_headers.IteratorValid(iter); )
		{
			CZipFileHeader* pHeader = m_centralDir.m_headers.GetNext(iter);
			pHeader->m_uOffset -= uOtherOffsetChanged;
		}
	}
}

bool CZipArchive::AddNewFile(LPCTSTR lpszFilePath,
                             int iLevel,          
                             bool bFullPath,      
                             ZIPCALLBACKFUN pCallback,
                             void* pUserData,         
                             unsigned long nBufSize)
{
	if (!nBufSize)
		return false;
	
	CZipFileHeader header;
	CZipPathComponent zpc(lpszFilePath);
	header.SetFileName(bFullPath ? zpc.GetNoDrive() : 
		TrimRootPath(zpc));
	if (header.GetFileNameSize() == 0)
		return false;


	DWORD uAttr;
	time_t ttime;
	if (!ZipPlatform::GetFileAttr(lpszFilePath, uAttr) || !ZipPlatform::GetFileModTime(lpszFilePath, ttime))
		return false;
	SetFileHeaderAttr(header, uAttr);
	header.SetTime(ttime);

	if (header.IsDirectory())
	{
		// clear password for a directory
		bool bRet = false;
		CZipString sz = GetPassword();
		if (!sz.IsEmpty())
			SetPassword();
		try
		{
			bRet = OpenNewFile(header, iLevel);
			CloseNewFile();
		}
		catch(...)
		{
			if (!sz.IsEmpty())
				SetPassword(sz);
			throw;
		}
		if (!sz.IsEmpty())
			SetPassword(sz);
		return bRet;
	}

	CZipFile f;

	// try to open before adding
	if (!f.Open(lpszFilePath, CZipFile::modeRead | CZipFile::shareDenyWrite, false)
		|| !OpenNewFile(header, iLevel))
		return false;
	
	DWORD iRead, iFileLength = pCallback ? f.GetLength() : 0, iSoFar = 0;
	CZipAutoBuffer buf(nBufSize);
	do
	{
		iRead = f.Read(buf, nBufSize);
		if (iRead)
		{
			WriteNewFile(buf, iRead);
			iSoFar += iRead;
			if (pCallback)
				if (!pCallback(iFileLength, iSoFar, pUserData))
					break;
		}
		
	}
	while (iRead == buf.GetSize());
	CloseNewFile();
	return true;
}

bool CZipArchive::AddNewFileDrv(LPCTSTR lpszFilePath,
                             int iLevel,          
                             bool bFullPath,      
                             ZIPCALLBACKFUN pCallback,
                             void* pUserData,         
                             unsigned long nBufSize)
{
	if (!nBufSize)
		return false;
	
	CZipFileHeader header;
	CZipPathComponent zpc(lpszFilePath);
   header.SetFileName(lpszFilePath);

	if (header.GetFileNameSize() == 0)
		return false;


	DWORD uAttr;
	time_t ttime;
	if (!ZipPlatform::GetFileAttr(lpszFilePath, uAttr) || !ZipPlatform::GetFileModTime(lpszFilePath, ttime))
		return false;
	SetFileHeaderAttr(header, uAttr);
	header.SetTime(ttime);

	if (header.IsDirectory())
	{
		// clear password for a directory
		bool bRet = false;
		CZipString sz = GetPassword();
		if (!sz.IsEmpty())
			SetPassword();
		try
		{
			bRet = OpenNewFile(header, iLevel);
			CloseNewFile();
		}
		catch(...)
		{
			if (!sz.IsEmpty())
				SetPassword(sz);
			throw;
		}
		if (!sz.IsEmpty())
			SetPassword(sz);
		return bRet;
	}

	CZipFile f;

	// try to open before adding
	if (!f.Open(lpszFilePath, CZipFile::modeRead | CZipFile::shareDenyWrite, false)
		|| !OpenNewFile(header, iLevel))
		return false;
	
	DWORD iRead, iFileLength = pCallback ? f.GetLength() : 0, iSoFar = 0;
	CZipAutoBuffer buf(nBufSize);
	do
	{
		iRead = f.Read(buf, nBufSize);
		if (iRead)
		{
			WriteNewFile(buf, iRead);
			iSoFar += iRead;
			if (pCallback)
				if (!pCallback(iFileLength, iSoFar, pUserData))
					break;
		}
		
	}
	while (iRead == buf.GetSize());
	CloseNewFile();
	return true;
}

int CZipArchive::GetSpanMode()
{
	return m_storage.m_iSpanMode * m_storage.IsSpanMode();
}

CZipString CZipArchive::GetArchivePath()
{
	return m_storage.m_pFile->GetFilePath();
}

CZipString CZipArchive::GetGlobalComment()
{
	if (IsClosed())
	{
		TRACE(_T("ZipArchive is closed.\n"));
		return _T("");
	}
	CZipString temp;	
	return SingleToWide(m_centralDir.m_pszComment, temp) != -1 ? (LPCTSTR)temp : _T("");
}

bool CZipArchive::SetGlobalComment(LPCTSTR lpszComment)
{
	if (IsClosed())
	{
		TRACE(_T("ZipArchive is closed.\n"));
		return false;
	}
	if (m_storage.IsSpanMode() == -1)
	{
		TRACE(_T("You cannot modify the global comment of the existing disk spanning archive.\n"));
		return false;
	}

	WideToSingle(lpszComment, m_centralDir.m_pszComment);
	m_centralDir.RemoveFromDisk();
	return true;
}



int CZipArchive::GetCurrentDisk()
{
	return m_storage.GetCurrentDisk() + 1;
}

bool CZipArchive::SetFileComment(WORD uIndex, LPCTSTR lpszComment)
{
	if (IsClosed())
	{
		TRACE(_T("ZipArchive is closed.\n"));
		return false;
	}
	if (m_storage.IsSpanMode() == -1)
	{
		TRACE(_T("You cannot modify the file comment in the existing disk spanning archive.\n"));
		return false;
	}
	
	if (!m_centralDir.IsValidIndex(uIndex))
		return false;
	m_centralDir[uIndex]->SetComment(lpszComment);
	m_centralDir.RemoveFromDisk();
	return true;
}


void CZipArchive::CryptInitKeys()
{
	ASSERT(m_pszPassword.GetSize());
	m_keys[0] = 305419896L;
	m_keys[1] = 591751049L;
	m_keys[2] = 878082192L;
	for (DWORD i = 0; i < m_pszPassword.GetSize(); i++)
		CryptUpdateKeys(m_pszPassword[i]);
}

void CZipArchive::CryptUpdateKeys(char c)
{
	
	m_keys[0] = CryptCRC32(m_keys[0], c);
	m_keys[1] += m_keys[0] & 0xff;
	m_keys[1] = m_keys[1] * 134775813L + 1;
	c = char(m_keys[1] >> 24);
	m_keys[2] = CryptCRC32(m_keys[2], c);
}

bool CZipArchive::CryptCheck()
{
	CZipAutoBuffer buf(ENCR_HEADER_LEN);
	m_storage.Read(buf, ENCR_HEADER_LEN, false);
	BYTE b = 0;
	for (int i = 0; i < ENCR_HEADER_LEN; i++)
	{
		b = buf[i]; // only temporary
		CryptDecode((char&)b);
	}
	// check the last byte
	return CurrentFile()->IsDataDescr() ?
		(BYTE(CurrentFile()->m_uModTime >> 8) == b) : (BYTE(CurrentFile()->m_uCrc32 >> 24) == b);
}

char CZipArchive::CryptDecryptByte()
{
	int temp = (m_keys[2] & 0xffff) | 2;
	return (char)(((temp * (temp ^ 1)) >> 8) & 0xff);
}

void CZipArchive::CryptDecode(char &c)
{
	c ^= CryptDecryptByte();
	CryptUpdateKeys(c);
}

bool CZipArchive::SetPassword(LPCTSTR lpszPassword)
{
	if (m_iFileOpened != nothing)
	{
		TRACE(_T("You cannot change the password when the file is opened\n"));
		return false; // it's important not to change the password when the file inside archive is opened
	}
	if (IsClosed())
	{
		TRACE(_T("Setting the password for a closed archive has no effect.\n"));
	}
	if (lpszPassword)
	{
		int iLen = WideToSingle(lpszPassword, m_pszPassword);
		if (iLen == -1)
			return false;
		for (size_t i = 0; (int)i < iLen; i++)
			if (m_pszPassword[i] > 127)
			{
				m_pszPassword.Release();
				TRACE(_T("The password contains forbidden characters. Password cleared.\n"));
				return false;
			}
	}
	else
		m_pszPassword.Release();
	return true;
}

CZipString CZipArchive::GetPassword()
{
	CZipString temp;
	CZipArchive::SingleToWide(m_pszPassword, temp);
	return temp;
}

DWORD CZipArchive::CryptCRC32(DWORD l, char c)
{
	const DWORD *CRC_TABLE = get_crc_table();
	return CRC_TABLE[(l ^ c) & 0xff] ^ (l >> 8);
}

void CZipArchive::CryptCryptHeader(long iCrc, CZipAutoBuffer &buf)
{
	CryptInitKeys();
	srand(UINT(GetTickCount()*time(NULL)));
	// genereate pseudo-random sequence
	char c;
	for (int i = 0; i < ENCR_HEADER_LEN - 2; i++)
	{
		int t1 = rand();
		c = (char)(t1 >> 6);
		if (!c)
			c = (char)t1;
		CryptEncode(c);
		buf[i] = c;

	}
	c = (char)((iCrc >> 16) & 0xff);
	CryptEncode(c);
	buf[ENCR_HEADER_LEN - 2] = c;
	c = (char)((iCrc >> 24) & 0xff);
	CryptEncode(c);
	buf[ENCR_HEADER_LEN - 1] = c;
}

void CZipArchive::CryptEncode(char &c)
{
	char t = CryptDecryptByte();
	CryptUpdateKeys(c);
	c ^= t;
}

void CZipArchive::CryptEncodeBuffer()
{
	if (CurrentFile()->IsEncrypted())
		for (DWORD i = 0; i < m_info.m_uComprLeft; i++)
			CryptEncode(m_info.m_pBuffer[i]);
}

void CZipArchive::CloseFileAfterTestFailed()
{
	if (m_iFileOpened != extract)
	{
		TRACE(_T("No file opened.\n"));
		return;
	}
	m_info.m_pBuffer.Release();
	m_centralDir.Clear(false);
	m_iFileOpened = nothing;
}

bool CZipArchive::TestFile(WORD uIndex, ZIPCALLBACKFUN pCallback, void* pUserData, DWORD uBufSize)
{
	if (!uBufSize)
		return false;
	CZipFileHeader* pHeader = m_centralDir[uIndex];
	if (pHeader->IsDirectory())
	{
		// we do not test whether the password for the encrypted directory
		// is correct, since it seems to be senseless (anyway password 
		// encrypted directories should be avoided - it adds 12 bytes)
		DWORD iSize = pHeader->m_uComprSize;
		if ((iSize != 0 || iSize != pHeader->m_uUncomprSize)
			// different treating compressed directories
			&& !(pHeader->IsEncrypted() && iSize == 12 && !pHeader->m_uUncomprSize))
			CZipException::Throw(CZipException::dirWithSize);
		return true;
	}
	else
	{
		try
		{
			if (!OpenFile(uIndex))
				return false;
			CZipAutoBuffer buf(uBufSize);
			DWORD iRead, iSoFar = 0;
			do
			{	
				iRead = ReadFile(buf, buf.GetSize());
				iSoFar += iRead;
				if (pCallback)
					if (!pCallback(pHeader->m_uUncomprSize, iSoFar, pUserData))
						break;
			}
			while (iRead == buf.GetSize());
			CloseFile();
		}
		catch(...)
		{
			CloseFileAfterTestFailed();
			throw;
		}
	}
	return true;

}

int CZipArchive::WideToSingle(LPCTSTR lpWide, CZipAutoBuffer &szSingle)
{
#ifdef _UNICODE
	return ZipPlatform::WideToSingle(lpWide, szSingle);
#else
	
	size_t iLen = strlen(lpWide);
	// if not UNICODE just copy
	// 	iLen does not include the NULL character
	szSingle.Allocate(iLen);
	memcpy(szSingle, lpWide, iLen);
	return iLen;
#endif

}

int CZipArchive::SingleToWide(const CZipAutoBuffer &szSingle, CZipString& szWide)
{
	
#ifdef _UNICODE	
	return ZipPlatform::SingleToWide(szSingle, szWide);
#else // if not UNICODE just copy
	int singleLen = szSingle.GetSize();
	// 	iLen does not include the NULL character
	memcpy(szWide.GetBuffer(singleLen),szSingle.GetBuffer(), singleLen);
	szWide.ReleaseBuffer(singleLen);
	return singleLen;
#endif
}

const DWORD* CZipArchive::GetCRCTable()
{
	return get_crc_table();
}

void CZipArchive::CryptDecodeBuffer(DWORD uCount)
{
	if (CurrentFile()->IsEncrypted())
		for (DWORD i = 0; i < uCount; i++)
			CryptDecode(m_info.m_pBuffer[i]);
}

void CZipArchive::EmptyPtrList()
{
	if (m_list.GetCount())
	{
		// if some memory hasn't been freed due to an error in zlib, so free it now
		CZipPtrListIter iter = m_list.GetHeadPosition();
		while (m_list.IteratorValid(iter))
			delete[] m_list.GetNext(iter);
	}
	m_list.RemoveAll();
}



void CZipArchive::SetFileHeaderAttr(CZipFileHeader& header, DWORD uAttr)
{
	header.SetSystemCompatibility(m_iArchiveSystCompatib);
	header.SetSystemAttr(uAttr);
}

void CZipArchive::EnableFindFast(bool bEnable)
{
	if (IsClosed())
	{
		TRACE(_T("Set it after opening the archive"));
		return;
	}

	if (m_centralDir.m_bFindFastEnabled == bEnable)
		return;
	m_centralDir.m_bFindFastEnabled = bEnable;
	if (bEnable)
		m_centralDir.BuildFindFastArray();
	else
		m_centralDir.m_findarray.RemoveAll();
}

bool CZipArchive::SetSystemCompatibility(int iSystemComp)
{
	if (IsClosed())
	{
		TRACE(_T("Set it after opening the archive"));
		return false;
	}

	if (m_iFileOpened == compress)
	{
		TRACE(_T("Set it before opening a file inside archive.\n"));
		return false;
	}

	if (!ZipCompatibility::IsPlatformSupported(iSystemComp))
		return false;
	m_iArchiveSystCompatib = iSystemComp;
	return true;
}

void CZipArchive::SetRootPath(LPCTSTR szPath)
{
	if (szPath)
	{
		m_szRootPath = szPath;
		CZipPathComponent::AppendSeparator(m_szRootPath);
	}
	else
		m_szRootPath.Empty();
}

CZipString CZipArchive::TrimRootPath(CZipPathComponent &zpc)
{
	if (m_szRootPath.IsEmpty())
		return zpc.GetFullFileName();
	CZipString szPath = zpc.GetFullPath();

	if (szPath.Left(m_szRootPath.GetLength()).CompareNoCase(m_szRootPath) == 0)
		return szPath.Mid(m_szRootPath.GetLength());
	else
		return zpc.GetFullFileName();
}
