/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SORT RDD module
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
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

#ifndef HB_DBSORT_H_
#define HB_DBSORT_H_

#include "hbrdddbf.h"

#if defined(HB_EXTERN_C)
extern "C" {
#endif

/*
 *  DBQUICKSORT
 *  -----------
 *  The Quick Sort Item Structure
 */

typedef struct _DBQUICKSORT
{
   FHANDLE hFile;
   BYTE szTempName[ _POSIX_PATH_MAX + 1 ];
   BYTE * pBuffer;
   BYTE * pSwapBufferA;
   BYTE * pSwapBufferB;
   BYTE * pCmpBufferA;
   BYTE * pCmpBufferB;
   USHORT uiRecordLen;
   USHORT uiMaxRecords;
   LPDBSORTINFO pSortInfo;
} DBQUICKSORT;

typedef DBQUICKSORT * LPDBQUICKSORT;

/*
 *  PROTOTYPES
 *  ----------
 */
extern BOOL hb_dbQSortInit( LPDBQUICKSORT pQuickSort, LPDBSORTINFO pSortInfo, USHORT uiRecordLen );
extern void hb_dbQSortExit( LPDBQUICKSORT pQuickSort );
extern BOOL hb_dbQSortAdvance( LPDBQUICKSORT pQuickSort, USHORT uiCount );
extern void hb_dbQSortComplete( LPDBQUICKSORT pQuickSort );

#if defined(HB_EXTERN_C)
}
#endif

#endif /* HB_DBSORT_H_ */