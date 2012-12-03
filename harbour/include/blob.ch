/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the RDD API Index OrderInfo and DBInfo support
 *
 * Copyright 2004 {list of individual authors and e-mail addresses}
 * www - http://www.xharbour.org
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

#ifndef HB_BLOB_CH_
#define HB_BLOB_CH_


#ifndef HB_DBINFO_CH_
  #include "dbinfo.ch"
#endif

#xtranslate BLOBRootLock()          => dbInfo( DBI_BLOB_ROOT_LOCK )
#xtranslate BLOBRootUnlock()        => dbInfo( DBI_BLOB_ROOT_UNLOCK )
#xtranslate BLOBRootGet()           => dbInfo( DBI_BLOB_ROOT_GET )
#xtranslate BLOBRootPut( <xBlob> )  => dbInfo( DBI_BLOB_ROOT_PUT, <xBlob> )
#xtranslate BLOBRootDelete()        => BLOBRootPut( "" )

#xtranslate BLOBDirectExport( <nPointer>, <cTargetFile> [, <kMode>] )   => ;
            dbInfo( DBI_BLOB_DIRECT_EXPORT, { <nPointer>, <cTargetFile>, ;
                    iif( <.kMode.>, <kMode>, BLOB_EXPORT_OVERWRITE ) } )
#xtranslate BLOBDirectGet( <nPointer> [, <nStart> [, <nCount> ]] )      => ;
            dbInfo( DBI_BLOB_DIRECT_GET, { <nPointer>, <nStart>, <nCount> } )
#xtranslate BLOBDirectImport( <nOldPointer>, <cSourceFile> )            => ;
            dbInfo( DBI_BLOB_DIRECT_IMPORT, { <nOldPointer>, <cSourceFile> } )
#xtranslate BLOBDirectPut( [<nOldPointer>], <xBlob> )                   => ;
            dbInfo( DBI_BLOB_DIRECT_PUT, { <nOldPointer>, <xBlob> } )
#xtranslate BLOBGet( <nFieldNo> [, <nStart> [, <nCount>]] )             => ;
            dbFieldInfo( DBS_BLOB_GET, <nFieldNo>, { <nStart>, <nCount> } )
#xtranslate BLOBExport( <nFieldNo>, <cTargetFile>, <nMode> )            => ;
            dbFileGet( <nFieldNo>, <cTargetFile>, <nMode> )
#xtranslate BLOBImport( <nFieldNo>, <cSourceFile> )                     => ;
            dbFilePut( <nFieldNo>, <cSourceFile> )

#endif  /* HB_BLOB_CH_ */
