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
* \file ZipStringStoreSettings.h
* Includes the CZipStringStoreSettings class.
*
*/

#if !defined(ZIPARCHIVE_ZIPSTRINGSTRINGSTORESETTINGS_DOT_H)
#define ZIPARCHIVE_ZIPSTRINGSTRINGSTORESETTINGS_DOT_H

#if _MSC_VER > 1000
#pragma once
#endif

#include "stdafx.h"
#include "ZipPlatform.h"
#include "ZipCompatibility.h"


/**
	Settings used in storing strings inside archives.

	\see
		<a href="kb">0610051525</a>
	\see
		CZipArchive::SetStringStoreSettings.
*/
class ZIP_API CZipStringStoreSettings
{
public:

	/**
		Gets the default filename code page for the given platform.

		\param iPlatform
			One of the ZipCompatibility::ZipPlatforms values.	

		\return 
			The default filename code page.
	*/
	static UINT GetDefaultNameCodePage(int iPlatform)
	{
		return iPlatform == ZipCompatibility::zcDosFat ? CP_OEMCP : CP_ACP;
	}

	/**
		Gets the default filename code page for the current platform.

		\return 
			The default filename code page.
	*/
	static UINT GetDefaultNameCodePage()
	{
		return GetDefaultNameCodePage(ZipPlatform::GetSystemID());
	}

	/**
		Gets the default comment code page. It is not platform-dependent.
	*/
	static UINT GetDefaultCommentCodePage()
	{
		return CP_ACP;
	}

	/**
		Initializes a new instance of the CZipStringStoreSettings class.
	 */
	CZipStringStoreSettings()
	{
		Reset();
	}

	/**
		Sets the default filename code page depening on the given platform.
	
		\param iPlatform
			One of the ZipCompatibility::ZipPlatforms values.	
	 */
	void SetDefaultNameCodePage(int iPlatform)
	{
		m_uNameCodePage = GetDefaultNameCodePage(iPlatform);
	}

	/**
		Gets a value indicating whether the current filename code page is
		standard for the current platform or not.

		\return
			\c true, if the current filename code page is standard; \c false otherwise.

		\see 
			ZipPlatform::GetSystemID
	*/
	bool IsStandardNameCodePage() const
	{
		return m_uNameCodePage == GetDefaultNameCodePage();
	}

	/**
		Gets a value indicating whether the current filename code page is
		standard for the given platform or not.

		\param iPlatform
			One of the ZipCompatibility::ZipPlatforms values.

		\return
			\c true, if the current filename code page is standard; \c false otherwise.
	*/
	bool IsStandardNameCodePage(int iPlatform) const
	{
		return m_uNameCodePage == GetDefaultNameCodePage(iPlatform);
	}

	/**
		Gets a value indicating whether the current comment code page is standard.

		\return
			\c true, if the current comment code page is standard; \c false otherwise.
	*/
	bool IsStandardCommentCodePage() const
	{
		return m_uCommentCodePage == GetDefaultCommentCodePage();
	}

	/**
		Gets a value indicating whether the current settings are
		standard for the given platform or not.

		\param iPlatform
			One of the ZipCompatibility::ZipPlatforms values.

		\return
			\c true, if the current settings are standard; \c false otherwise.
	*/
	bool IsStandard(int iPlatform) const
	{
		return !m_bStoreNameInExtraData && IsStandardNameCodePage(iPlatform) && IsStandardCommentCodePage();
	}

	/**
		Reset the settings to its default values for the given platform.

		\param iPlatform
			One of the ZipCompatibility::ZipPlatforms values.
		\see
			Reset
	*/
	void Reset(int iPlatform)
	{
		m_bStoreNameInExtraData = false;
		SetDefaultNameCodePage(iPlatform);
		m_uCommentCodePage = GetDefaultCommentCodePage();
	}

	/**
		Reset the settings to its default values for the current platform.
		
		\see
			Reset(int)
		\see
			ZipPlatform::GetSystemID
	*/
	void Reset()
	{
		Reset(ZipPlatform::GetSystemID());
	}

	/**
		Sets the string store settings.

		\see
			CZipArchive::SetStringStoreSettings(UINT, bool, UINT)
	*/
	void Set(UINT uFileNameCodePage, bool bStoreNameInExtraData, UINT uCommentCodePage)
	{
		m_uNameCodePage = uFileNameCodePage;
		m_bStoreNameInExtraData = bStoreNameInExtraData;
		m_uCommentCodePage = uCommentCodePage;
	}

	/**
		If \c true, the converted filenames are stored in extra field in the archive.
	*/
	bool m_bStoreNameInExtraData;

	/**
		The current filename code page.
	*/
	UINT m_uNameCodePage;

	/**
		The current comment code page.
	*/
	UINT m_uCommentCodePage;
};

#endif // !defined(ZIPARCHIVE_ZIPSTRINGSTRINGSTORESETTINGS_DOT_H)
