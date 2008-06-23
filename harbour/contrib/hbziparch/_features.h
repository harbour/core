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
* \file _features.h
*	Contains definitions that enable or disable certain features in the ZipArchive Library.
*
*/

#if !defined(ZIPARCHIVE_FEATURES_DOT_H)
/// @cond
#define ZIPARCHIVE_FEATURES_DOT_H 
/// @endcond

#if _MSC_VER > 1000
#pragma once
#endif

#include "_platform.h"

#ifdef __GNUC__

#ifndef __int64
	#define __int64 long long
#endif

#endif

/************ Feel free to adjust the definitions in the following block ************/
/************************************ BLOCK START ***********************************/

/**
	Make sure it is defined, if you use ZIP64. Comment this out otherwise.

	\see
		<a href="kb">0610051629</a>
*/
// #define _ZIP64

/**
	Make sure it is defined, if you use AES. Comment this out otherwise.

	\see
		<a href="kb">0610201627|aes</a>
*/
// #define _ZIP_AES

/**
	Make sure it is defined, if you use the BZIP2 algorithm for compression. Comment this out otherwise.

	\see
		<a href="kb">0610231446|bzip2</a>
*/
// #define _BZIP2

/**
	Make sure it is defined, if you want to create seekable data.

	\see
		<a href="kb">0711101739</a>

*/
// #define _ZIP_SEEK

/**
	Make sure it is defined, if you use the AES encryption in a multithreaded environment or archive sharing (CZipArchive::OpenFrom). Comment this out otherwise.

	\see
		<a href="kb">0610201627|aes</a>
	\see
		<a href="kb">0610241003|thread</a>
*/
// #define ZIP_ARCHIVE_USE_LOCKING

#ifndef _ZIP64
// Uncomment this to have the index and volume numbers types defined as WORD. Otherwise they are defined as int.
#define _ZIP_STRICT_U16
#endif


/************************************* BLOCK END ***********************************/
/***** The contents below this line are usually not intended for modification ******/



#endif // !defined(ZIPARCHIVE_FEATURES_DOT_H)
