// ZipInternalInfo.cpp: implementation of the CZipInternalInfo class.
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
#include "zipinternalinfo.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CZipInternalInfo::CZipInternalInfo()
{
	m_iBufferSize = 16384;
}

CZipInternalInfo::~CZipInternalInfo()
{

}

void CZipInternalInfo::Init()
{
	m_pBuffer.Allocate(m_iBufferSize);
}
