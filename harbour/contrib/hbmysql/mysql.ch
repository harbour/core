/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * MySQL DBMS defines
 * These defines are clipper code level equivalent of mysql.h and mysql_com.h
 *
 * Copyright 2000 Maurilio Longo <maurilio.longo@libero.it>
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

/* MySQL field types */
#define MYSQL_TYPE_DECIMAL      0
#define MYSQL_TYPE_TINY         1  /* NOTE: TINY is used to map clipper logical values to MySQL tables, so 0 == .F., 1 == .T. */
#define MYSQL_TYPE_SHORT        2
#define MYSQL_TYPE_LONG         3
#define MYSQL_TYPE_FLOAT        4
#define MYSQL_TYPE_DOUBLE       5
#define MYSQL_TYPE_NULL         6
#define MYSQL_TYPE_TIMESTAMP    7
#define MYSQL_TYPE_LONGLONG     8
#define MYSQL_TYPE_INT24        9
#define MYSQL_TYPE_DATE         10
#define MYSQL_TYPE_TIME         11
#define MYSQL_TYPE_DATETIME     12
#define MYSQL_TYPE_YEAR         13
#define MYSQL_TYPE_NEWDATE      14
#define MYSQL_TYPE_VARCHAR      15
#define MYSQL_TYPE_BIT          16
#define MYSQL_TYPE_NEWDECIMAL   246
#define MYSQL_TYPE_ENUM         247
#define MYSQL_TYPE_SET          248
#define MYSQL_TYPE_TINY_BLOB    249
#define MYSQL_TYPE_MEDIUM_BLOB  250
#define MYSQL_TYPE_LONG_BLOB    251
#define MYSQL_TYPE_BLOB         252
#define MYSQL_TYPE_VAR_STRING   253
#define MYSQL_TYPE_STRING       254
#define MYSQL_TYPE_GEOMETRY     255

/* MySQL field structure item number
   (C level structure is translated to a clipper array) */
#define MYSQL_FS_NAME           1     /* Name of column */
#define MYSQL_FS_TABLE          2     /* Table of column if column was a field */
#define MYSQL_FS_DEF            3     /* Default value (set by mysql_list_fields) */
#define MYSQL_FS_TYPE           4     /* Type of field. Se mysql_com.h for types */
#define MYSQL_FS_LENGTH         5     /* Width of column */
#define MYSQL_FS_MAXLEN         6     /* Max width of selected set */
#define MYSQL_FS_FLAGS          7     /* Div flags */
#define MYSQL_FS_DECIMALS       8     /* Number of decimals in field */

/* MySQL field flags */
#define NOT_NULL_FLAG           1     /* Field can't be NULL */
#define PRI_KEY_FLAG            2     /* Field is part of a primary key */
#define UNIQUE_KEY_FLAG         4     /* Field is part of a unique key */
#define MULTIPLE_KEY_FLAG       8     /* Field is part of a key */
#define BLOB_FLAG               16    /* Field is a blob */
#define UNSIGNED_FLAG           32    /* Field is unsigned */
#define ZEROFILL_FLAG           64    /* Field is zerofill */
#define BINARY_FLAG             128
/* The following are only sent to new clients */
#define ENUM_FLAG               256   /* field is an enum */
#define AUTO_INCREMENT_FLAG     512   /* field is a autoincrement field */
#define TIMESTAMP_FLAG          1024  /* Field is a timestamp */
#define PART_KEY_FLAG           16384 /* Intern; Part of some key */
#define GROUP_FLAG              32768 /* Intern group field */

/* Extension to DBS_xxx defines to encompass NOT NULL fields, needed by indexes */
#define DBS_NOTNULL             5     /* True if field has to be NOT NULL */
