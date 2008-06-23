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
#include "ZipArchive.h"
#include "ZipExtraData.h"
#include "BytesWriter.h"

using namespace ZipArchiveLib;

bool CZipExtraData::Read(char* buffer, WORD uSize)
{
	if (uSize < 4)
			return false;
	WORD size;
	CBytesWriter::ReadBytes(m_uHeaderID, buffer);
	CBytesWriter::ReadBytes(size, buffer + 2);
	if (uSize - 4 < size)
		return false;
	m_data.Allocate(size);
	memcpy(m_data, buffer + 4, size);
	return true;
}

WORD CZipExtraData::Write(char* buffer)const
{
	CBytesWriter::WriteBytes(buffer, m_uHeaderID);
	WORD size = (WORD)m_data.GetSize();
	CBytesWriter::WriteBytes(buffer + 2, size);
	memcpy(buffer + 4, m_data, size);
	return (WORD)(size + 4);
}
