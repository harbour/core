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
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */


#ifndef HB_APIZLIB_H_
#define HB_APIZLIB_H_
#include <hbsetup.h>
#include <hbapi.h>
#include <hbapiitm.h>
#include <hbapifs.h>
#include <hbapigt.h>
#include <hbvmpub.h>
#include <hbvm.h>
#include "zip.h"
#include "unzip.h"
#if defined(HB_EXTERN_C)
extern "C" {
#endif
#define filePos 1
#define Lenght 2
#define Method 3
#define Size 4
#define Ratio 5
#define Date 6
#define Time 7
#define Crc32 8

extern uLong hb___filetime(char *f, tm_zip *tmzip, uLong *dt);
extern char *hb___CheckFile( char * szFile);
extern int hb___CompressOneFile(char *szFile,char *szFiletoCompress,int iCompLevel,PHB_ITEM pBlock,BOOL bOverWrite);
extern int hb___CompressMultipleFile(char *szFile,PHB_ITEM pArray,int iCompLevel,PHB_ITEM pBlock,BOOL bOverWrite);
extern BOOL hb___unZipFiles(char *szFile,PHB_ITEM pBlock,BOOL bExtractPath);
extern int hb___ExtractCurrentFile(unzFile uf,BOOL popt_extract_without_path,BOOL popt_overwrite,PHB_ITEM pBlock);
extern void hb____ChangeFileDate(char *filename,uLong dosdate,tm_unz tmu_date);
extern int hb___MakeDir(char *szNewDirectory);
extern int hb___GetNumbersofFilestoUnzip(char *szFile);
extern PHB_ITEM hb___GetFilesNamesFromZip(char *szFile,BOOL iMode);
#if defined(HB_EXTERN_C)
}
#endif

#endif /* HB_APIEXT_H_ */
