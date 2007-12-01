/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * Firebird RDBMS low level (client api) interface code.
 *
 * Copyright 2003 Rodrigo Moreno rodrigo_moreno@yahoo.com
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
 * See doc/license.txt for licensing terms.
 *
 */

#define _CLIPDEFS_H

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>

#define HB_OS_WIN_32_USED

#include "hbapi.h"
#include "extend.api"
#include "item.api"
#include "hbapiitm.h"
#include "ibase.h"

#define DIALECT                1
#define MAX_FIELDS             5
#define MAX_LEN              256
#define MAX_BUFFER          1024

#define ERREXIT(status) { _retnl(isc_sqlcode(status)); return; }

#ifndef ISC_INT64_FORMAT

#if (defined(_MSC_VER) && defined(WIN32)) || (defined(__BORLANDC__) && defined(__WIN32__))
#define ISC_INT64_FORMAT        "I64"
#else
#define ISC_INT64_FORMAT        "ll"
#endif
#endif

HB_FUNC(FBCREATEDB)
{
    isc_db_handle   newdb = NULL;
    isc_tr_handle   trans = NULL;
    long            status[20];
    char            create_db[MAX_BUFFER];

    char            *db_name;
    char            *user;
    char            *pass;
    char            *charset;
    int             page;
    int             dialect;

    if (hb_pcount() != 6)
    {
        _retni(0);
        return;
     }

    db_name = hb_parcx(1);
    user = hb_parcx(2);
    pass = hb_parcx(3);
    page = hb_parni(4);
    charset = hb_parcx(5);
    dialect = hb_parni(6);

    sprintf(create_db,
            "CREATE DATABASE '%s' USER '%s' PASSWORD '%s' PAGE_SIZE = %i DEFAULT CHARACTER SET %s",
            db_name, user, pass, page, charset );

    if (isc_dsql_execute_immediate(status, &newdb, &trans, 0, create_db, dialect, NULL))
        ERREXIT(status);

    _retni(1);
}


HB_FUNC(FBCONNECT)
{
    ISC_STATUS      status[MAX_FIELDS];
    isc_db_handle   db = NULL;
    char            *db_connect = NULL;
    char            *user = NULL;
    char            *passwd = NULL;
    char            dpb[128];
    int             i = 0, len;

    PHB_ITEM db_handle;

    if (hb_parinfo(1))
        db_connect = hb_parcx(1);

    if (hb_parinfo(2))
        user = hb_parcx(2);

    if (hb_parinfo(3))
        passwd = hb_parcx(3);

    dpb[i++] = isc_dpb_version1;

    dpb[i++] = isc_dpb_user_name;
    len = strlen(user);
    dpb[i++] = (char) len;
    strncpy(&(dpb[i]), user, len);
    i += len;

    dpb[i++] = isc_dpb_password;
    len = strlen (passwd);
    dpb[i++] = len;
    strncpy(&(dpb[i]), passwd, len);
    i += len;

    if ( isc_attach_database ( status, 0, db_connect, &db, i, dpb ) )
        ERREXIT(status);

    db_handle = hb_itemPutPtr( NULL, ( void * ) db );
    _itemReturn(db_handle);
    _itemRelease(db_handle);
}


HB_FUNC(FBCLOSE)
{
    isc_db_handle   db = NULL;
    long            status[20];

    db = ( isc_db_handle ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

    if ( isc_detach_database ( status, &db ) )
        ERREXIT(status);

    _retnl(1);
}


HB_FUNC(FBERROR)
{
    int sqlcode;
    char msg[MAX_BUFFER];

    sqlcode = hb_parni(1);

    isc_sql_interprete(sqlcode, msg, 512);

    _retc(msg);
}

HB_FUNC(FBSTARTTRANSACTION)
{
    isc_db_handle   db = NULL;
    isc_tr_handle   trans = NULL;
    long            status[MAX_FIELDS];

    PHB_ITEM        var;

    db = ( isc_db_handle ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

    if (isc_start_transaction(status, &trans, 1, &db, 0, NULL))
        ERREXIT(status);

    var = hb_itemPutPtr( NULL, ( void * ) trans );
    _itemReturn(var);
    _itemRelease(var);
}


HB_FUNC(FBCOMMIT)
{
    isc_tr_handle   trans = NULL;
    long status[MAX_FIELDS];

    trans = ( isc_db_handle ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

    if (isc_commit_transaction(status, &trans))
        ERREXIT(status);

    trans = NULL;
    _retnl(1);
}

HB_FUNC(FBROLLBACK)
{
    isc_tr_handle   trans = NULL;
    long status[MAX_FIELDS];

    trans = ( isc_db_handle ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );

    if (isc_rollback_transaction(status, &trans))
        ERREXIT(status);

    trans = NULL;
    _retnl(1);
}


HB_FUNC(FBEXECUTE)
{
    isc_db_handle   db = NULL;
    isc_tr_handle   trans = NULL;
    char            *exec_str;
    long            status[20];
    long            status_rollback[20];
    int             dialect;

    db = ( isc_db_handle ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
    exec_str = hb_parcx(2);
    dialect = hb_parni(3);

    if (hb_parinfo(4)) {
        trans = (isc_tr_handle) hb_itemGetPtr( hb_param( 4, HB_IT_POINTER ) );
    } else {
        if ( isc_start_transaction ( status, &trans, 1, &db, 0, NULL ) )
            ERREXIT(status);
    }

    if (isc_dsql_execute_immediate(status, &db, &trans, 0, exec_str, dialect, NULL)) {
        if (! hb_parinfo(4))
            isc_rollback_transaction ( status_rollback, &trans );

        ERREXIT(status);
    }

    if (!hb_parinfo(4))
        if ( isc_commit_transaction ( status, &trans ) )
            ERREXIT(status);

    _retnl(1);
}

HB_FUNC(FBQUERY)
{
    isc_db_handle       db = NULL;
    isc_tr_handle       trans = NULL;
    ISC_STATUS          status[MAX_FIELDS];
    XSQLDA  ISC_FAR *   sqlda;
    isc_stmt_handle     stmt = NULL;
    XSQLVAR             *var;

    char                sel_str[MAX_LEN];
    int                 dialect;
    int                 n, i, dtype;
    int                 num_cols;

    ITEM qry_handle;
    ITEM temp;
    ITEM aTemp;
    ITEM aNew;

    ITEM itemSqlname;
    ITEM itemSqltype;
    ITEM itemSqllen ;
    ITEM itemSqlscale;
    ITEM itemRelname;

    db = ( isc_db_handle ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
    strcpy(sel_str, hb_parcx(2));

    if (hb_parinfo(3)) {
        dialect = hb_parni(3);
    } else {
        dialect = DIALECT;
    }

    if (hb_parinfo(4)) {
        trans = (isc_tr_handle) hb_itemGetPtr( hb_param( 4, HB_IT_POINTER ) );

    } else if ( isc_start_transaction ( status, &trans, 1, &db, 0, NULL ) )
        ERREXIT(status);

    // Allocate an output SQLDA. Just to check number of columns
    sqlda = ( XSQLDA * ) hb_xgrab( XSQLDA_LENGTH ( 1 ) );
    sqlda->sqln = 1;
    sqlda->version = 1;

    // Allocate a statement
    if (isc_dsql_allocate_statement(status, &db, &stmt))
        ERREXIT(status);

    // Prepare the statement.
    if (isc_dsql_prepare(status, &trans, &stmt, 0, sel_str, dialect, sqlda))
        ERREXIT(status);

    // Describe sql contents
    if (isc_dsql_describe(status, &stmt, dialect, sqlda))
        ERREXIT(status);

    num_cols = sqlda->sqld;
    aNew = _itemArrayNew( num_cols );

    // Relocate necessary number of columns
    if ( sqlda->sqld > sqlda->sqln ) {
        hb_xfree( sqlda );
        n = sqlda->sqld;
        sqlda = ( XSQLDA * ) hb_xgrab( XSQLDA_LENGTH ( n ) );
        sqlda->sqln = n;
        sqlda->version = 1;

        if (isc_dsql_describe(status, &stmt, dialect, sqlda))
            ERREXIT(status);
    }

    for ( i = 0, var = sqlda->sqlvar; i < sqlda->sqld; i++, var++ ) {
        dtype = ( var->sqltype & ~1 );
        switch ( dtype ) {
        case SQL_VARYING:
            var->sqltype = SQL_TEXT;
            var->sqldata = ( char * ) hb_xgrab( sizeof ( char ) * var->sqllen + 2 );
            break;
        case SQL_TEXT:
            var->sqldata = ( char * ) hb_xgrab( sizeof ( char ) * var->sqllen + 2 );
            break;
        case SQL_LONG:
            var->sqltype = SQL_LONG;
            var->sqldata = ( char * ) hb_xgrab( sizeof ( long ) );
            break;
        default:
            var->sqldata = ( char * ) hb_xgrab( sizeof ( char ) * var->sqllen );
            break;
        }
        if ( var->sqltype & 1 ) {
            var->sqlind = ( short * ) hb_xgrab( sizeof ( short ) );
        }

        aTemp = _itemArrayNew( 5 );

        itemSqlname = _itemPutC( NULL, sqlda->sqlvar[i].sqlname );
        _itemArrayPut( aTemp, 1, itemSqlname );

        itemSqltype = _itemPutNL( NULL, (long)dtype );
        _itemArrayPut( aTemp, 2, itemSqltype );

        itemSqllen = _itemPutNL( NULL, sqlda->sqlvar[i].sqllen );
        _itemArrayPut( aTemp, 3, itemSqllen );

        itemSqlscale = _itemPutNL( NULL, sqlda->sqlvar[i].sqlscale );
        _itemArrayPut( aTemp, 4, itemSqlscale );

        itemRelname = _itemPutC( NULL, sqlda->sqlvar[i].relname );
        _itemArrayPut( aTemp, 5, itemRelname );

        _itemRelease( itemSqlname );
        _itemRelease( itemSqltype );
        _itemRelease( itemSqllen );
        _itemRelease( itemSqlscale );
        _itemRelease( itemRelname );

        _itemArrayPut( aNew, i+1, aTemp );
        _itemRelease( aTemp );
    }

    if ( !sqlda->sqld ) {
        // Execute and commit non-select querys
        if ( isc_dsql_execute ( status, &trans, &stmt, dialect, NULL ) )
            ERREXIT(status);

    } else {
        if ( isc_dsql_execute ( status, &trans, &stmt, dialect, sqlda ) )
             ERREXIT(status);
    }

    qry_handle = _itemArrayNew(6);

    temp = hb_itemPutPtr( NULL, ( void * ) stmt );
    hb_arraySet(qry_handle, 1, temp);
    _itemRelease(temp);

    temp = hb_itemPutPtr( NULL, ( void * ) sqlda );
    hb_arraySet(qry_handle, 2, temp);
    _itemRelease(temp);

    if (! hb_parinfo(4))
    {
        temp = hb_itemPutPtr( NULL, ( void * ) trans );
        hb_arraySet(qry_handle, 3, temp);
        _itemRelease(temp);
    }

    temp = _itemPutNL(NULL, (long) num_cols);
    hb_arraySet(qry_handle, 4, temp);
    _itemRelease(temp);

    temp = _itemPutNL(NULL, (long) dialect);
    hb_arraySet(qry_handle, 5, temp);
    _itemRelease(temp);

    hb_arraySet(qry_handle, 6, aNew);

    _itemReturn(qry_handle);
    _itemRelease(qry_handle);
    _itemRelease(aNew);
}


HB_FUNC(FBFETCH)
{
    isc_stmt_handle     stmt = NULL;
    ISC_STATUS          status[MAX_FIELDS];
    XSQLDA              ISC_FAR * sqlda;
    long                fetch_stat;
    int                 dialect;

    PHB_ITEM aParam ;

    if (ISARRAY( 1 ) )
    {
        aParam = hb_param(1,HB_IT_ARRAY);

        stmt = ( isc_stmt_handle ) hb_itemGetPtr(hb_itemArrayGet( aParam, 1 ));
        sqlda = ( XSQLDA ISC_FAR * ) hb_itemGetPtr(hb_itemArrayGet( aParam, 2 ));
        dialect = hb_itemGetNI(hb_itemArrayGet( aParam, 5 ));

        fetch_stat = isc_dsql_fetch(status, &stmt, dialect, sqlda);

        if (fetch_stat != 100L)
            ERREXIT(status);

    }
    _retnl(fetch_stat);
}


HB_FUNC(FBFREE)
{
    isc_stmt_handle     stmt = NULL;
    isc_tr_handle       trans = NULL;
    ISC_STATUS          status[MAX_FIELDS];
    XSQLDA              ISC_FAR *  sqlda;

    PHB_ITEM aParam ;

    if (ISARRAY( 1 ) )
    {
        aParam = hb_param(1,HB_IT_ARRAY);

        stmt = ( isc_stmt_handle ) hb_itemGetPtr(hb_itemArrayGet( aParam, 1 ));
        sqlda = ( XSQLDA ISC_FAR * ) hb_itemGetPtr(hb_itemArrayGet( aParam, 2 ));
        trans = ( isc_tr_handle ) hb_itemGetPtr( hb_itemArrayGet( aParam, 3 ));

        if (isc_dsql_free_statement(status, &stmt, DSQL_drop))
             ERREXIT(status);

        if (trans)
            if (isc_commit_transaction(status, &trans))
                ERREXIT(status);

        if ( sqlda )
            hb_xfree( sqlda );

        _retnl(1);
    }
    else
        _retnl(0);

}


HB_FUNC(FBGETDATA)
{
    int             pos;
    short           dtype;
    char            data[MAX_BUFFER], *p;
    char            date_s[25];

    struct          tm times;
    XSQLVAR         *var;
    XSQLDA          ISC_FAR * sqlda;
    ISC_STATUS      status[MAX_FIELDS];
    ISC_QUAD        *blob_id;

    PHB_ITEM aParam ;
    PHB_ITEM temp;

    aParam = hb_param(1,HB_IT_ARRAY);

    sqlda = ( XSQLDA ISC_FAR * ) hb_itemGetPtr(hb_itemArrayGet( aParam, 2 ));
    pos = (int) hb_parnl(2);

    pos--;

    if ( ( pos + 1 ) > sqlda->sqln )
        ERREXIT(status);

        var = sqlda->sqlvar;

        var += pos;

        dtype = var->sqltype & ~1;
        p = data;

        if ( ( var->sqltype & 1 ) && ( *var->sqlind < 0 ) ) {
            /* null field */
             _ret();

        } else {
                switch ( dtype ) {
                case SQL_TEXT:
                case SQL_VARYING:
                    _retclen( var->sqldata, var->sqllen );
                    break;

                case SQL_TIMESTAMP:
                    isc_decode_timestamp ( ( ISC_TIMESTAMP ISC_FAR * ) var->sqldata, &times );
                    sprintf ( date_s, "%04d-%02d-%02d %02d:%02d:%02d.%04lu",
                              times.tm_year + 1900,
                              times.tm_mon + 1,
                              times.tm_mday,
                              times.tm_hour,
                              times.tm_min,
                              times.tm_sec,
                              ( ( ISC_TIMESTAMP * ) var->sqldata )->timestamp_time % 10000 );
                    sprintf ( p, "%*s ", 24, date_s );

                    _retc(p);
                    break;

                case SQL_TYPE_DATE:
                    isc_decode_sql_date ( ( ISC_DATE ISC_FAR * ) var->sqldata, &times );
                    sprintf ( date_s, "%04d-%02d-%02d", times.tm_year + 1900, times.tm_mon + 1, times.tm_mday );
                    sprintf ( p, "%*s ", 8, date_s );

                    _retc(p);
                    break;

                case SQL_TYPE_TIME:
                    isc_decode_sql_time ( ( ISC_TIME ISC_FAR * ) var->sqldata, &times );
                    sprintf ( date_s, "%02d:%02d:%02d.%04lu",
                              times.tm_hour,
                              times.tm_min,
                              times.tm_sec, ( *( ( ISC_TIME * ) var->sqldata ) ) % 10000 );
                    sprintf ( p, "%*s ", 13, date_s );

                    _retc(p);
                    break;

                case SQL_BLOB:

                    blob_id = ( ISC_QUAD * ) var->sqldata;

                    temp = hb_itemPutPtr( NULL, ( void * ) blob_id );
                    _itemReturn(temp);
                    _itemRelease(temp);

                    break;

                case SQL_SHORT:
                case SQL_LONG:
                    case SQL_INT64:
                        {
                        ISC_INT64       value;
                        short           field_width;
                        short           dscale;
                        switch (dtype)
                            {
                                case SQL_SHORT:
                                        value = (ISC_INT64) *(short ISC_FAR *) var->sqldata;
                                        field_width = 6;
                                        break;

                                case SQL_LONG:
                                        value = (ISC_INT64) *(long ISC_FAR *) var->sqldata;
                                        field_width = 11;
                                        break;

                                case SQL_INT64:
                                        value = (ISC_INT64) *(ISC_INT64 ISC_FAR *) var->sqldata;
                                        field_width = 21;
                                        break;
                    }

                            dscale = var->sqlscale;

                            if (dscale < 0)
                            {
                                ISC_INT64   tens;
                                short       i;

                            tens = 1;
                            for (i = 0; i > dscale; i--)
                                    tens *= 10;

                                if (value >= 0)
                                        sprintf (p, "%*" ISC_INT64_FORMAT "d.%0*" ISC_INT64_FORMAT "d",
                                                field_width - 1 + dscale,
                                                (ISC_INT64) value / tens,
                                                -dscale,
                                                (ISC_INT64) value % tens);

                                else if ((value / tens) != 0)
                                        sprintf (p, "%*" ISC_INT64_FORMAT "d.%0*" ISC_INT64_FORMAT "d",
                                                field_width - 1 + dscale,
                                                (ISC_INT64) (value / tens),
                                                -dscale,
                                                (ISC_INT64) -(value % tens));

                                else
                                        sprintf (p, "%*s.%0*" ISC_INT64_FORMAT "d",
                                                field_width - 1 + dscale,
                                                "-0",
                                                -dscale,
                                                (ISC_INT64) -(value % tens));
                            }
                            else if (dscale)
                            sprintf (p, "%*" ISC_INT64_FORMAT "d%0*d", field_width, (ISC_INT64) value, dscale, 0);
                            else
                                sprintf (p, "%*" ISC_INT64_FORMAT "d", field_width, (ISC_INT64) value);
                };
                    _retc(p);
                    break;

                case SQL_FLOAT:
                    sprintf(p, "%15g ", *(float ISC_FAR *) (var->sqldata));
                    _retc(p);
                    break;

                case SQL_DOUBLE:
                            sprintf(p, "%24f ", *(double ISC_FAR *) (var->sqldata));
                    _retc(p);
                    break;

                default:
                    _ret();
                    break;
                }
        }
}


HB_FUNC(FBGETBLOB)
{
    ISC_STATUS          status[MAX_FIELDS];
    isc_db_handle       db = NULL;
    isc_tr_handle       trans = NULL;
    isc_blob_handle     blob_handle = NULL;
    short               blob_seg_len;
    char                *blob_segment;
    ISC_QUAD            *blob_id;
    char                p[MAX_BUFFER];
    long                blob_stat;

    ITEM temp;
    ITEM aNew;

    db = ( isc_db_handle ) hb_itemGetPtr( hb_param( 1, HB_IT_POINTER ) );
    blob_id = ( ISC_QUAD * ) hb_itemGetPtr( hb_param( 2, HB_IT_POINTER ) );

    if (_parinfo(3)) {
        trans = (isc_tr_handle) hb_itemGetPtr( hb_param( 3, HB_IT_POINTER ) );
    } else {
        if ( isc_start_transaction ( status, &trans, 1, &db, 0, NULL ) ) {
            ERREXIT(status);
        }
    }

    if (isc_open_blob2(status, &db, &trans, &blob_handle, blob_id, 0, NULL))
        ERREXIT(status);

    // Get blob segments and their lengths and print each segment.
    blob_stat = isc_get_segment(status, &blob_handle,
                               (unsigned short ISC_FAR *) &blob_seg_len,
                                sizeof(blob_segment), blob_segment);

    if (blob_stat == 0 || status[1] == isc_segment)
        aNew = _itemArrayNew( 0 );

    while (blob_stat == 0 || status[1] == isc_segment)
    {
        //p = ( char * ) hb_xgrab( blob_seg_len + 1 );
        sprintf( p, "%*.*s", blob_seg_len, blob_seg_len, blob_segment);

        temp = _itemPutC( NULL, p );
        hb_arrayAdd( aNew, temp ) ;
        _itemRelease(temp);

        //hb_xfree(p);
        blob_stat = isc_get_segment(status, &blob_handle,
                                   (unsigned short ISC_FAR *)&blob_seg_len,
                                    sizeof(blob_segment), blob_segment);
    }

    if (isc_close_blob(status, &blob_handle)) {
        _itemRelease(aNew);
        ERREXIT(status);
    }

    if (!_parinfo(3))
        if ( isc_commit_transaction ( status, &trans ) )
            ERREXIT(status);

    _itemReturn(aNew);
    _itemRelease(aNew);
}
