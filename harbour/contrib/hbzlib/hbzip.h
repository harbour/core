/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Zlib API,
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */


#ifndef HB_APIZLIB_H_
#define HB_APIZLIB_H_

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapifs.h"
#include "hbapigt.h"
#include "hbvmpub.h"
#include "hbvm.h"
#define WIN32
#include "zip.h"
#include "unzip.h"
#if defined(HB_EXTERN_C)
extern "C" {
#endif

extern uLong hb___filetime(char *f, tm_zip *tmzip, uLong *dt);
extern char *hb___CheckFile( char * szFile);
extern int hb___CompressOneFile(char *szFile,char *szFiletoCompress,int iCompLevel,PHB_ITEM pBlock,BOOL iOverWrite);
extern int hb___CompressMultipleFile(char *szFile,PHB_ITEM pArray,int iCompLevel,PHB_ITEM pBlock,BOOL iOverWrite);
extern int hb___unZipFiles(char *szFile,PHB_ITEM pBlock);
extern int hb___ExtractCurrentFile(unzFile uf,const int* popt_extract_without_path,int* popt_overwrite,PHB_ITEM pBlock);
extern void hb____ChangeFileDate(const char *filename,uLong dosdate,tm_unz tmu_date);
extern int hb___MyMkdir(const char *DirectoryName);
extern int hb___MakeDir(char *NewDirectory);
#if defined(HB_EXTERN_C)
}
#endif

#endif /* HB_APIEXT_H_ */
