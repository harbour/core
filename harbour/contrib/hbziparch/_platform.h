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
* \file _platform.h
*	Contains definitions that determine the target compilation platform.
*
*/

#if !defined(ZIPARCHIVE_PLATFORM_DOT_H)
#define ZIPARCHIVE_PLATFORM_DOT_H

#if _MSC_VER > 1000
#pragma once
#endif

/************ Feel free to adjust the definitions in the following block ************/
/************************************ BLOCK START ***********************************/

//#define ZIP_ARCHIVE_MFC
//#define ZIP_ARCHIVE_LNX

// simplified endianess detection
#ifdef __APPLE__
	#ifdef __LITTLE_ENDIAN__
		#define ZIP_ARCHIVE_LITTLE_ENDIAN
	#endif
#else
	#define ZIP_ARCHIVE_LITTLE_ENDIAN
#endif

/************************************* BLOCK END ***********************************/
/********* The contents below this line are not intended for modification **********/

#ifndef ZIP_ARCHIVE_MFC
	#define ZIP_ARCHIVE_STL
#else
	#ifdef ZIP_ARCHIVE_STL
		#undef ZIP_ARCHIVE_STL
	#endif
#endif

#ifndef ZIP_ARCHIVE_LNX
	#define ZIP_ARCHIVE_WIN
#else
	#ifdef ZIP_ARCHIVE_WIN
		#undef ZIP_ARCHIVE_WIN
	#endif
#endif

#ifndef ZIP_ARCHIVE_LITTLE_ENDIAN
	#define ZIP_ARCHIVE_BIG_ENDIAN
#else
	#ifdef ZIP_ARCHIVE_BIG_ENDIAN
		#undef ZIP_ARCHIVE_BIG_ENDIAN
	#endif
#endif

#if defined (ZIP_ARCHIVE_LNX) && defined (ZIP_ARCHIVE_MFC)
	#undef ZIP_ARCHIVE_MFC
	#define ZIP_ARCHIVE_STL
	#error Using MFC under a non-Windows platform is not supported
#endif

#endif // !defined(ZIPARCHIVE_PLATFORM_DOT_H)
