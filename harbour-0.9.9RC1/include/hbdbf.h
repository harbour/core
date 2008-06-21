/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DBF structures
 *
 * Copyright 1999 Bruno Cantero <bruno@issnet.net>
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

#ifndef HB_DBF_H_
#define HB_DBF_H_

#include "hbapirdd.h"

HB_EXTERN_BEGIN

/* DBF header */

typedef struct _DBFHEADER
{
   BYTE   bVersion;
   BYTE   bYear;
   BYTE   bMonth;
   BYTE   bDay;
   BYTE   ulRecCount[ 4 ];
   BYTE   uiHeaderLen[ 2 ];
   BYTE   uiRecordLen[ 2 ];
   BYTE   bReserved1[ 2 ];
   BYTE   bTransaction;       /* 1-transaction begin */
   BYTE   bEncrypted;         /* 1-encryptpted table */
   BYTE   bReserved2[ 12 ];
   BYTE   bHasTags;           /* bit filed: 1-production index, 2-memo file in VFP */
   BYTE   bCodePage;
   BYTE   bReserved3[ 2 ];
} DBFHEADER;

typedef DBFHEADER * LPDBFHEADER;



/* DBF fields */

typedef struct _DBFFIELD
{
   BYTE bName[ 11 ];
   BYTE bType;
   BYTE bReserved1[ 4 ];      /* offset from record begin in FP */
   BYTE bLen;
   BYTE bDec;
   BYTE bFieldFlags;          /* 1-system column, 2-nullable, 4-binary */
   BYTE bCounter[4];          /* autoincrement counter */
   BYTE bStep;                /* autoincrement step */
   BYTE bReserved2[ 7 ];
   BYTE bHasTag;
} DBFFIELD;

typedef DBFFIELD * LPDBFFIELD;



/* SMT MEMO field */

typedef struct _SMTFIELD
{
   BYTE     type[2];
   BYTE     length[4];
   BYTE     block[4];
} SMTFIELD;

typedef SMTFIELD * LPSMTFIELD;


HB_EXTERN_END

#endif /* HB_DBF_H_ */
