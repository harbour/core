/*
 * Harbour Project source code:
 * PostgreSQL RDBMS wrapper header.
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.net) (GC support)
 * Copyright 2003 Rodrigo Moreno rodrigo_moreno@yahoo.com
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

#ifndef __HBPGSQL_H
#define __HBPGSQL_H

#include "hbapi.h"

#include "libpq-fe.h"
#include "pg_config.h"

#define VARHDRSZ        4
#define BOOLOID         16
#define INT8OID         20
#define INT2OID         21
#define INT4OID         23
#define TEXTOID         25
#define OIDOID          26
#define FLOAT4OID       700
#define FLOAT8OID       701
#define CASHOID         790
#define BPCHAROID       1042
#define VARCHAROID      1043
#define DATEOID         1082
#define TIMEOID         1083
#define TIMESTAMPOID    1114
#define TIMESTAMPTZOID  1184
#define TIMETZOID       1266
#define BITOID          1560
#define VARBITOID       1562
#define NUMERICOID      1700

#define INV_WRITE       0x00020000
#define INV_READ        0x00040000

#ifndef PG_VERSION_NUM
#define PG_VERSION_NUM  0
#endif

HB_EXTERN_BEGIN

extern HB_EXPORT void hb_PGconn_ret( PGconn * p );
extern HB_EXPORT PGconn * hb_PGconn_par( int iParam );
extern HB_EXPORT void hb_PGresult_ret( PGresult * p );
extern HB_EXPORT PGresult * hb_PGresult_par( int iParam );

HB_EXTERN_END

#endif /* __HBPGSQL_H */
