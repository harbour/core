/*
 * Harbour Project source code:
 *    DELIM RDD
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#ifndef HB_RDDDEL_H_
#define HB_RDDDEL_H_

#include "hbapirdd.h"

HB_EXTERN_BEGIN

/* DELIMITED default file extensions */
#define DELIM_TABLEEXT                    ".txt"


/*
 *  DELIM WORKAREA
 *  ------------
 *  The Workarea Structure of DELIM RDD
 *
 */

typedef struct _DELIMAREA
{
   AREA area;

   /*
   *  DELIM's additions to the workarea structure
   *
   *  Warning: The above section MUST match WORKAREA exactly!  Any
   *  additions to the structure MUST be added below, as in this
   *  example.
   */

   PHB_FILE    pFile;                  /* Data file handle */
   char *      szFileName;             /* Name of data file */
   char *      szEol;                  /* EOL marker */
   HB_USHORT   uiEolLen;               /* Size of EOL marker */
   char        cDelim;                 /* Character field delimiter */
   char        cSeparator;             /* Field separator */
   HB_USHORT   uiRecordLen;            /* Size of record */
   HB_USHORT * pFieldOffset;           /* Pointer to field offset array */
   HB_BYTE *   pRecord;                /* Buffer of record data */
   HB_BYTE *   pBuffer;                /* Read/Write */
   HB_SIZE     nBufferSize;            /* IO buffer size */
   HB_SIZE     nBufferRead;            /* Number of bytes in read buffer */
   HB_SIZE     nBufferIndex;           /* Index to read read buffer */
   HB_FOFFSET  nRecordOffset;          /* Current record offest */
   HB_FOFFSET  nNextOffset;            /* Next record offest */
   HB_FOFFSET  nFileSize;              /* File table size in export mode */
   HB_FOFFSET  nBufferStart;           /* Start offset of read buffer */
   HB_ULONG    ulRecNo;                /* Current record */
   HB_ULONG    ulRecCount;             /* Number of records (in export) */
   HB_BOOL     fTransRec;              /* Can put whole records */
   HB_BOOL     fFlush;                 /* Data was written to table and not commited */
   HB_BOOL     fShared;                /* Shared file */
   HB_BOOL     fReadonly;              /* Read only file */
   HB_BOOL     fPositioned;            /* Positioned record */
   HB_BOOL     fRecordChanged;         /* Record changed */
} DELIMAREA;

typedef DELIMAREA * LPDELIMAREA;

#ifndef DELIMAREAP
#define DELIMAREAP LPDELIMAREA
#endif

HB_EXTERN_END

#endif /* HB_RDDDEL_H_ */
