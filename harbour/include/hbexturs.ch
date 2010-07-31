/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The declarations for User RDD functions.
 *
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
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

#ifndef HB_EXTURS_CH_
#define HB_EXTURS_CH_

/* User RDD functions */
EXTERNAL UR_SUPER_BOF
EXTERNAL UR_SUPER_EOF
EXTERNAL UR_SUPER_FOUND
EXTERNAL UR_SUPER_GOBOTTOM
EXTERNAL UR_SUPER_GOTOP
EXTERNAL UR_SUPER_GOTO
EXTERNAL UR_SUPER_GOTOID
EXTERNAL UR_SUPER_SEEK
EXTERNAL UR_SUPER_SKIP
EXTERNAL UR_SUPER_SKIPFILTER
EXTERNAL UR_SUPER_SKIPRAW
EXTERNAL UR_SUPER_DELETED
EXTERNAL UR_SUPER_ADDFIELD
EXTERNAL UR_SUPER_FIELDDISPLAY
EXTERNAL UR_SUPER_FIELDNAME
EXTERNAL UR_SUPER_APPEND
EXTERNAL UR_SUPER_DELETE
EXTERNAL UR_SUPER_RECALL
EXTERNAL UR_SUPER_FIELDCOUNT
EXTERNAL UR_SUPER_FLUSH
EXTERNAL UR_SUPER_GOCOLD
EXTERNAL UR_SUPER_GOHOT
EXTERNAL UR_SUPER_PUTREC
EXTERNAL UR_SUPER_GETREC
EXTERNAL UR_SUPER_GETVALUE
EXTERNAL UR_SUPER_PUTVALUE
EXTERNAL UR_SUPER_GETVARLEN
EXTERNAL UR_SUPER_RECCOUNT
EXTERNAL UR_SUPER_RECINFO
EXTERNAL UR_SUPER_RECNO
EXTERNAL UR_SUPER_RECID
EXTERNAL UR_SUPER_FIELDINFO
EXTERNAL UR_SUPER_CREATEFIELDS
EXTERNAL UR_SUPER_SETFIELDEXTENT
EXTERNAL UR_SUPER_ALIAS
EXTERNAL UR_SUPER_CLOSE
EXTERNAL UR_SUPER_CREATE
EXTERNAL UR_SUPER_OPEN
EXTERNAL UR_SUPER_INFO
EXTERNAL UR_SUPER_DBEVAL
EXTERNAL UR_SUPER_PACK
EXTERNAL UR_SUPER_PACKREC
EXTERNAL UR_SUPER_SORT
EXTERNAL UR_SUPER_TRANS
EXTERNAL UR_SUPER_TRANSREC
EXTERNAL UR_SUPER_ZAP
EXTERNAL UR_SUPER_CHILDEND
EXTERNAL UR_SUPER_CHILDSTART
EXTERNAL UR_SUPER_CHILDSYNC
EXTERNAL UR_SUPER_SYNCCHILDREN
EXTERNAL UR_SUPER_CLEARREL
EXTERNAL UR_SUPER_FORCEREL
EXTERNAL UR_SUPER_RELAREA
EXTERNAL UR_SUPER_RELEVAL
EXTERNAL UR_SUPER_RELTEXT
EXTERNAL UR_SUPER_SETREL
EXTERNAL UR_SUPER_ORDLSTADD
EXTERNAL UR_SUPER_ORDLSTCLEAR
EXTERNAL UR_SUPER_ORDLSTDELETE
EXTERNAL UR_SUPER_ORDLSTFOCUS
EXTERNAL UR_SUPER_ORDLSTREBUILD
EXTERNAL UR_SUPER_ORDSETCOND
EXTERNAL UR_SUPER_ORDCREATE
EXTERNAL UR_SUPER_ORDDESTROY
EXTERNAL UR_SUPER_ORDINFO
EXTERNAL UR_SUPER_CLEARFILTER
EXTERNAL UR_SUPER_CLEARLOCATE
EXTERNAL UR_SUPER_CLEARSCOPE
EXTERNAL UR_SUPER_FILTERTEXT
EXTERNAL UR_SUPER_SETFILTER
EXTERNAL UR_SUPER_SETLOCATE
EXTERNAL UR_SUPER_LOCATE
EXTERNAL UR_SUPER_COMPILE
EXTERNAL UR_SUPER_ERROR
EXTERNAL UR_SUPER_EVALBLOCK
EXTERNAL UR_SUPER_RAWLOCK
EXTERNAL UR_SUPER_LOCK
EXTERNAL UR_SUPER_UNLOCK
EXTERNAL UR_SUPER_CLOSEMEMFILE
EXTERNAL UR_SUPER_CREATEMEMFILE
EXTERNAL UR_SUPER_OPENMEMFILE
EXTERNAL UR_SUPER_GETVALUEFILE
EXTERNAL UR_SUPER_PUTVALUEFILE
EXTERNAL UR_SUPER_READDBHEADER
EXTERNAL UR_SUPER_WRITEDBHEADER
EXTERNAL UR_SUPER_DROP
EXTERNAL UR_SUPER_EXISTS
EXTERNAL UR_SUPER_RENAME
EXTERNAL UR_SUPER_RDDINFO

#endif /* HB_EXTURS_CH_ */
