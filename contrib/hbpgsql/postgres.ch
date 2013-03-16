/*
 * Harbour Project source code:
 * PostgreSQL RDBMS low level (client api) interface code.
 *
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

#ifndef HBPOSTGRES_CH_
#define HBPOSTGRES_CH_

#define CONNECTION_OK                   0
#define CONNECTION_BAD                  1
#define CONNECTION_STARTED              2
#define CONNECTION_MADE                 3
#define CONNECTION_AWAITING_RESPONSE    4
#define CONNECTION_AUTH_OK              5
#define CONNECTION_SETENV               6
#define CONNECTION_SSL_STARTUP          7
#define CONNECTION_NEEDED               8

#define PGRES_EMPTY_QUERY               0
#define PGRES_COMMAND_OK                1
#define PGRES_TUPLES_OK                 2
#define PGRES_COPY_OUT                  3
#define PGRES_COPY_IN                   4
#define PGRES_BAD_RESPONSE              5
#define PGRES_NONFATAL_ERROR            6
#define PGRES_FATAL_ERROR               7

#define PQTRANS_IDLE                    0
#define PQTRANS_ACTIVE                  1
#define PQTRANS_INTRANS                 2
#define PQTRANS_INERROR                 3
#define PQTRANS_UNKNOWN                 4

/* PQmetadata() positions for array returned */
#define HBPG_META_FIELDNAME             1
#define HBPG_META_FIELDTYPE             2
#define HBPG_META_FIELDLEN              3
#define HBPG_META_FIELDDEC              4
#define HBPG_META_TABLE                 5
#define HBPG_META_TABLECOL              6
#define HBPG_META_LEN_                  6

#endif
