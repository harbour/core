/*
 * $Id$
 */


/*
 * Harbour Project source code:
 * mSQL DBMS defines
 * These defines are clipper code level equivalent of msql.h
 *
 * Copyright 2000 Maurilio Longo <maurilio.longo@libero.it>
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


// mSQL fields type
#define MSQL_INT_TYPE         1
#define MSQL_CHAR_TYPE        2
#define MSQL_REAL_TYPE        3
#define MSQL_IDENT_TYPE       4
#define MSQL_NULL_TYPE        5
#define MSQL_TEXT_TYPE        6
#define MSQL_DATE_TYPE        7

// NOTE: UINT is used to map clipper logical values to mSQL tables, so 0 == .F., 1 == .T.
#define MSQL_UINT_TYPE        8
#define MSQL_MONEY_TYPE       9
#define MSQL_TIME_TYPE        10
#define MSQL_LAST_REAL_TYPE   10
#define MSQL_IDX_TYPE	      253
#define MSQL_SYSVAR_TYPE      254
#define MSQL_ANY_TYPE         255



// mSQL field structure item number (C level structure is translated
// to a clipper array)

#define MSQL_FS_NAME          1
#define MSQL_FS_TABLE         2
#define MSQL_FS_TYPE          3
#define MSQL_FS_LENGTH        4
#define MSQL_FS_FLAGS         5

