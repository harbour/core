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
* \file ZipExtraData.h
* Includes the CZipExtraData class.
*
*/

#if !defined(ZIPARCHIVE_ZIPEXTRADATA_DOT_H)
#define ZIPARCHIVE_ZIPEXTRADATA_DOT_H

#if _MSC_VER > 1000
#pragma once
#endif

#include "ZipExport.h"
#include "ZipAutoBuffer.h"
#include "ZipExtraField.h"
#include "memory.h"

/**
	Represents a single data record in an extra field.

	\see
		<a href="kb">0610242300</a>
*/
class ZIP_API CZipExtraData
{	
	friend class CZipExtraField;
public:

	/**
		The custom data contained by this record.
	*/
	CZipAutoBuffer m_data;

	CZipExtraData()
	{
		m_uHeaderID = 0;
	}

	CZipExtraData(const CZipExtraData& extra)
	{
		*this = extra;
	}

	/**
		Initializes a new instance of the CZipExtraData class.

		\param uHeaderID
			The unique ID of the data.
	*/
	CZipExtraData(WORD uHeaderID)
	{
		m_uHeaderID = uHeaderID;
	}

	CZipExtraData& operator=(const CZipExtraData& extra)
	{
		m_uHeaderID = extra.m_uHeaderID;
		DWORD uSize = extra.m_data.GetSize();
		m_data.Allocate(uSize);
		if (uSize > 0)
			memcpy(m_data, extra.m_data, uSize);
		return *this;
	}
	bool operator==(const CZipExtraData& extra)
	{
		return m_uHeaderID == extra.m_uHeaderID && m_data.GetSize() == extra.m_data.GetSize() && memcmp(m_data, extra.m_data, m_data.GetSize()) == 0;
	}
	bool operator != (const CZipExtraData& extra)
	{
		return !(*this == extra);
	}
	bool operator > (const CZipExtraData& extra)
	{
		return m_uHeaderID > extra.m_uHeaderID;
	}
	bool operator < (const CZipExtraData& extra)
	{
		return m_uHeaderID < extra.m_uHeaderID;
	}
	bool operator >= (const CZipExtraData& extra)
	{
		return m_uHeaderID > extra.m_uHeaderID || *this == extra;
	}

	bool operator <= (const CZipExtraData& extra)
	{
		return m_uHeaderID < extra.m_uHeaderID || *this == extra;
	}

	/**
		Gets the total size, the extra data will occupy in the archive.

		\return
			The size in bytes.
	*/
	int GetTotalSize() const
	{
		return 4 + m_data.GetSize();
	}
	
	/**
		Gets the data ID.

		\return 
			The data ID.
	*/
	WORD GetHeaderID() const
	{
		return m_uHeaderID;
	}

protected:
	
	/**
		Reads the extra data record from \a buffer.

		\param buffer
			The buffer to read the data from.

		\param uSize
			The size of the data to read.

		\return
			\c false, if \a uSize was smaller than the declared extra data size; \c true otherwise.
	*/
	bool Read(char* buffer, WORD uSize);

	/**
		Writes the extra data record to \a buffer.

		\param buffer
			The buffer to write to.

		\return
			The total size of extra data in bytes.
	*/
	WORD Write(char* buffer)const;

private:
	WORD m_uHeaderID;
};

#endif // !defined(ZIPARCHIVE_ZIPEXTRADATA_DOT_H)
