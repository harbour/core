/*
 * $Id$
 *
 * xHarbour Project source code:
 * PostgreSQL RDBMS low level (client api) interface code.
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "libpq-fe.h"

#define _CLIPDEFS_H
#if defined(HB_OS_WIN_32_USED)
   #include <windows.h>
#endif

#define VARHDRSZ              4
#define BOOLOID               16
#define INT8OID			20
#define INT2OID			21
#define INT4OID			23
#define TEXTOID			25
#define OIDOID			26
#define FLOAT4OID             700
#define FLOAT8OID             701
#define CASHOID               790                                                                
#define BPCHAROID             1042
#define VARCHAROID            1043
#define DATEOID               1082
#define TIMEOID               1083
#define TIMESTAMPOID          1114
#define TIMESTAMPTZOID        1184
#define TIMETZOID             1266
#define BITOID                1560
#define VARBITOID             1562
#define NUMERICOID            1700

#define INV_WRITE       0x00020000
#define INV_READ        0x00040000

#ifndef HB_PGVERSION
#  ifdef PG_DIAG_INTERNAL_POSITION
#     define HB_PGVERSION   0x0800
#  else
#     define HB_PGVERSION   0x0700
#  endif
#endif

/* 
 * Connection handling functions 
 */

HB_FUNC( PQCONNECT )
{
   char     conninfo[128];
   PGconn   *conn;

   if( hb_pcount() == 5 )
      sprintf( conninfo, "dbname = %s host = %s user = %s password = %s port = %i",
               hb_parcx(1), hb_parcx(2), hb_parcx(3), hb_parcx(4), (int) hb_parni(5) );

   conn = PQconnectdb( conninfo );
   hb_retptr( conn );
}

HB_FUNC( PQSETDBLOGIN )
{
    const char *pghost;
    const char *pgport;
    const char *pgoptions;
    const char *pgtty;
    const char *dbName;
    const char *login;
    const char *pwd;
                                         
    pghost    = hb_parcx(1);   
    pgport    = hb_parcx(2);   
    pgoptions = hb_parcx(3);
    pgtty     = hb_parcx(4);    
    dbName    = hb_parcx(5);   
    login     = hb_parcx(6);    
    pwd       = hb_parcx(8);      
                                         
    if (hb_pcount() == 7)
        hb_retptr( ( PGconn * ) PQsetdbLogin( pghost, pgport, pgoptions, pgtty, dbName, login, pwd) );
}   
                    
HB_FUNC(PQCLOSE)
{
    if (hb_parinfo(1))
        PQfinish(( PGconn * ) hb_parptr(1));
}

HB_FUNC(PQRESET)
{
    if (hb_parinfo(1))
        PQreset(( PGconn * ) hb_parptr(1));
}

HB_FUNC(PQPROTOCOLVERSION)
{
    if (hb_parinfo(1))
        hb_retni(PQprotocolVersion(( PGconn * ) hb_parptr(1)));
}

HB_FUNC(PQCLIENTENCODING)
{
    if (hb_parinfo(1))
        hb_retni(PQclientEncoding(( PGconn * ) hb_parptr(1)));
}

HB_FUNC(PQSETCLIENTENCODING)
{
    if (hb_pcount() == 2)
        hb_retni(PQsetClientEncoding(( PGconn * ) hb_parptr(1), hb_parcx(2)));
}
    
HB_FUNC(PQDB)
{   
    if (hb_parinfo(1))
        hb_retc(PQdb( ( PGconn * ) hb_parptr(1) ));
}
        
HB_FUNC(PQUSER)
{
    if (hb_parinfo(1))
       hb_retc(PQuser( ( PGconn * ) hb_parptr(1) ));
}
            
HB_FUNC(PQPASS)
{
    if (hb_parinfo(1))
        hb_retc(PQpass( ( PGconn * ) hb_parptr(1) ));
}
            
HB_FUNC(PQHOST)
{
    if (hb_parinfo(1))
        hb_retc(PQhost( ( PGconn * ) hb_parptr(1) ));
}
            
HB_FUNC(PQPORT)
{
    if (hb_parinfo(1))
        hb_retc(PQport( ( PGconn * ) hb_parptr(1) ));
}
            
HB_FUNC(PQTTY)
{
    if (hb_parinfo(1))
        hb_retc(PQtty( ( PGconn * ) hb_parptr(1) ));
}
            
HB_FUNC(PQOPTIONS)
{
    if (hb_parinfo(1))
        hb_retc(PQoptions( ( PGconn * ) hb_parptr(1) ));
}

/* 
 * Query handling functions 
 */

HB_FUNC(PQCLEAR)
{
    if (hb_parinfo(1))
        PQclear(( PGresult * ) hb_parptr(1));
}

HB_FUNC(PQEXEC)
{
    PGresult   *res = NULL;

    if (hb_pcount() == 2)
        res = PQexec(( PGconn * ) hb_parptr(1), hb_parcx(2));

    hb_retptr( res );        
}

HB_FUNC(PQEXECPARAMS)
{
    PGresult   *res = NULL;
    const char **paramvalues;
    int        i;
    long       n;

    PHB_ITEM   aParam;

    if (hb_pcount() == 3)
    {
        aParam = hb_param(3,HB_IT_ARRAY);

        n = hb_arrayLen(aParam);

        paramvalues = (const char **) hb_xgrab( sizeof( char *) * n );

        for (i=0;i < n;i++)
            paramvalues[i] = hb_arrayGetCPtr( aParam, i + 1 );

        res = PQexecParams(( PGconn * ) hb_parptr(1), hb_parcx(2), n, NULL, paramvalues, NULL, NULL, 1);

        hb_xfree(paramvalues);
    }
    hb_retptr( res );        
}

HB_FUNC(PQFCOUNT)
{
    PGresult   *res;
    int nFields = 0;

    if (hb_parinfo(1))
    {
        res = ( PGresult * ) hb_parptr(1);

        if (PQresultStatus(res) == PGRES_TUPLES_OK)
                nFields = PQnfields(res);
    }

    hb_retni(nFields);
}

HB_FUNC(PQLASTREC)
{
    PGresult   *res;
    int nRows = 0;

    if (hb_parinfo(1))
    {
        res = ( PGresult * ) hb_parptr(1);

        if (PQresultStatus(res) == PGRES_TUPLES_OK)
            nRows = PQntuples(res);
    }
    hb_retni(nRows);
}

HB_FUNC(PQGETVALUE)
{
    PGresult   *res;
    int         nRow, nCol;

    if (hb_pcount() == 3)
    {
        res = ( PGresult * ) hb_parptr(1);

        if (PQresultStatus(res) == PGRES_TUPLES_OK)
        {
            nRow = hb_parni(2) - 1;
            nCol = hb_parni(3) - 1;

            if (! PQgetisnull(res, nRow, nCol))
                hb_retc(PQgetvalue(res, nRow, nCol));
        }
    }
}

HB_FUNC(PQGETLENGTH)
{
    PGresult   *res;
    int         nRow, nCol;
    int         result = 0;

    if (hb_pcount() == 3)
    {
        res = ( PGresult * ) hb_parptr(1);

        if (PQresultStatus(res) == PGRES_TUPLES_OK)
        {
            nRow = hb_parni(2) - 1;
            nCol = hb_parni(3) - 1;

            result = PQgetlength(res, nRow, nCol);
        }
    }
    hb_retni(result);
}

HB_FUNC( PQMETADATA )
{
   PGresult *res;
   if( hb_parinfo( 1 ) )
   {
      res = ( PGresult * ) hb_parptr( 1 );
      if( PQresultStatus( res ) == PGRES_TUPLES_OK )
      {
         int nFields = PQnfields( res ), i;
         PHB_ITEM pResult = hb_itemArrayNew( nFields ), pField;

         for( i = 0; i < nFields; i++ )
         {
            char  buf[256];
            Oid   type_oid = PQftype( res, i );
            int   typemod = PQfmod( res, i );
            int   length = 0;
            int   decimal = 0;

            switch( type_oid )
            {
               case BITOID:
                  if( typemod >= 0 )
                     length = ( int ) typemod;
                  strcpy( buf, "bit" );
                  break;

               case BOOLOID:
                  length = 1;
                  strcpy( buf, "boolean" );
                  break;

               case BPCHAROID:
                  if( typemod >= 0 )
                     length = ( int ) ( typemod - VARHDRSZ );
                  strcpy( buf, "character" );
                  break;

               case FLOAT4OID:
                  strcpy( buf, "real" );
                  break;

               case FLOAT8OID:
                  strcpy( buf, "double precision" );
                  break;

               case INT2OID:
                  strcpy( buf, "smallint" );
                  break;

               case INT4OID:
                  strcpy( buf, "integer" );
                  break;

               case OIDOID:
                  strcpy( buf, "bigint" );
                  break;

               case INT8OID:
                  strcpy( buf, "bigint" );
                  break;

               case NUMERICOID:
                  length = ( ( typemod - VARHDRSZ ) >> 16 ) & 0xffff;
                  decimal = ( typemod - VARHDRSZ ) & 0xffff;
                  strcpy( buf, "numeric" );
                  break;

               case DATEOID:
                  strcpy( buf, "date" );
                  break;

               case TIMEOID:
               case TIMETZOID:
                  strcpy( buf, "timezone" );
                  break;

               case TIMESTAMPOID:
               case TIMESTAMPTZOID:
                  strcpy( buf, "timestamp" );
                  break;

               case VARBITOID:
                  if( typemod >= 0 )
                     length = (int) typemod;
                  strcpy( buf, "bit varying" );
                  break;

               case VARCHAROID:
                  if( typemod >= 0 )
                     length = ( int ) ( typemod - VARHDRSZ );
                  strcpy( buf, "character varying" );
                  break;

               case TEXTOID:
                  strcpy(buf, "text");
                  break;

               case CASHOID:
                  strcpy( buf, "money" );
                  break;

               default:
                 strcpy( buf, "not supported" );
                 break;
            }

            pField = hb_arrayGetItemPtr( pResult, i + 1 );
            hb_arrayNew ( pField, 6 );
            hb_itemPutC ( hb_arrayGetItemPtr( pField, 1 ), PQfname( res, i ) );
            hb_itemPutC ( hb_arrayGetItemPtr( pField, 2 ), buf );
            hb_itemPutNI( hb_arrayGetItemPtr( pField, 3 ), length );
            hb_itemPutNI( hb_arrayGetItemPtr( pField, 4 ), decimal );
            hb_itemPutNL( hb_arrayGetItemPtr( pField, 5 ), PQftable( res, i ) );
            hb_itemPutNI( hb_arrayGetItemPtr( pField, 6 ), PQftablecol( res, i ) );
         }
         hb_itemRelease( hb_itemReturnForward( pResult ) );
      }
   }
}

HB_FUNC(PQTRANSACTIONSTATUS)
{
    if (hb_parinfo(1))
        hb_retni(PQtransactionStatus(( PGconn * ) hb_parptr(1) ));
}

HB_FUNC(PQERRORMESSAGE)
{
    if (hb_parinfo(1))
        hb_retc(PQerrorMessage(( PGconn * ) hb_parptr(1) ));
}

HB_FUNC(PQSTATUS)
{
    if (hb_parinfo(1))
        hb_retni(PQstatus(( PGconn * ) hb_parptr(1) ));
}

HB_FUNC(PQRESULTERRORMESSAGE)
{
    if (hb_parinfo(1))
        hb_retc(PQresultErrorMessage(( PGresult * ) hb_parptr(1)));
}

HB_FUNC(PQRESULTSTATUS)
{
    if (hb_parinfo(1))
        hb_retni(PQresultStatus(( PGresult * ) hb_parptr(1) ));
}


HB_FUNC(PQCMDSTATUS)
{
    if (hb_parinfo(1))
        hb_retc(PQcmdStatus( (PGresult *) hb_parptr(1) ));
}


HB_FUNC(PQCMDTUPLES)
{
    if (hb_parinfo(1))
        hb_retc(PQcmdTuples( (PGresult *) hb_parptr(1) ));
}


HB_FUNC(PQESCAPESTRING)
{
    char *source;
    char *dest;
    size_t size;
        
    source = hb_parcx(1);
    dest = (char *) hb_xgrab( strlen(source) * 2 + 1);
    size = strlen(source);
    
    PQescapeString(dest, source, size);
    
    hb_retc(dest);
    hb_xfree( (char *) dest);    
}


HB_FUNC(PQESCAPEBYTEA)
{
    char *from;
    char *to;
    size_t from_length;
    size_t to_length;
        
    from = hb_parcx(1);
    from_length = strlen(from);
    to_length = strlen(from) * 5 + 1;
    
    to = PQescapeBytea(from, from_length, &to_length);
    hb_retc(to);
    PQfreemem(to);
}


HB_FUNC(PQUNESCAPEBYTEA)
{
    char *from;
    size_t to_length;        
    
    from = PQunescapeBytea(hb_parcx(1), &to_length);
    hb_retclen(from, to_length);
    PQfreemem(from);
}


HB_FUNC(PQOIDVALUE)
{
    if (hb_parinfo(1))
        hb_retnl( ( Oid ) PQoidValue(( PGresult * ) hb_parptr(1) ));
}

HB_FUNC(PQOIDSTATUS)
{
    if (hb_parinfo(1))
        hb_retc( PQoidStatus(( PGresult * ) hb_parptr(1) ));
}

HB_FUNC(PQBINARYTUPLES)
{
    if (hb_parinfo(1))
        hb_retl( PQbinaryTuples(( PGresult * ) hb_parptr(1) ));
}

HB_FUNC(PQFTABLE)
{
    if (hb_pcount() == 2)
        hb_retnl( ( Oid ) PQftable(( PGresult * ) hb_parptr(1), hb_parni(2) - 1 ));
}

HB_FUNC(PQFTYPE)
{
    if (hb_pcount() == 2)
        hb_retnl( ( Oid ) PQftype(( PGresult * ) hb_parptr(1), hb_parni(2) - 1 ));
}

HB_FUNC(PQFNAME)
{
    if (hb_pcount() == 2)
        hb_retc( PQfname(( PGresult * ) hb_parptr(1), hb_parni(2) - 1 ));
}

HB_FUNC(PQFMOD)
{
    if (hb_pcount() == 2)
        hb_retni( PQfmod(( PGresult * ) hb_parptr(1), hb_parni(2) - 1 ));
}

HB_FUNC(PQFSIZE)
{
    if (hb_pcount() == 2)
        hb_retni( PQfsize(( PGresult * ) hb_parptr(1), hb_parni(2) - 1 ));
}

HB_FUNC(PQGETISNULL)
{
    if (hb_pcount() == 3)
        hb_retl( PQgetisnull(( PGresult * ) hb_parptr(1), hb_parni(2) - 1 , hb_parni(3) - 1));
}

HB_FUNC(PQFNUMBER)
{
    if (hb_pcount() == 2)
        hb_retni( PQfnumber(( PGresult * ) hb_parptr(1), hb_parcx(2) ) + 1);
}

HB_FUNC(PQNTUPLES)
{
    if (hb_parinfo(1))
        hb_retnl( PQntuples(( PGresult * ) hb_parptr(1) ));
}

HB_FUNC(PQNFIELDS)
{
    if (hb_parinfo(1))
        hb_retnl( PQnfields(( PGresult * ) hb_parptr(1) ));
}

/* 
 * Asynchronous functions 
 */

HB_FUNC(PQSENDQUERY)
{
    int res = 0;        

    if (hb_pcount() == 2)
        res = PQsendQuery(( PGconn * ) hb_parptr(1), hb_parcx(2));

    hb_retl( res );        
}

HB_FUNC(PQGETRESULT)
{
    PGresult   *res = NULL;

    if (hb_parinfo(1))
        res = PQgetResult(( PGconn * ) hb_parptr(1));

    /* when null, no more result to catch */
    if (res)
        hb_retptr( res );        
}

HB_FUNC(PQCONSUMEINPUT)
{
    int res = 0;        

    if (hb_parinfo(1))
        res = PQconsumeInput(( PGconn * ) hb_parptr(1));

    hb_retl( res );        
}

HB_FUNC(PQISBUSY)
{
    int res = 0;        

    if (hb_parinfo(1))
        res = PQisBusy(( PGconn * ) hb_parptr(1));

    hb_retl( res );        
}

HB_FUNC(PQREQUESTCANCEL) /* deprecated */
{
    int res = 0;        

    if (hb_parinfo(1))
        res = PQrequestCancel(( PGconn * ) hb_parptr(1));

    hb_retl( res );        
}


HB_FUNC(PQFLUSH)
{
    if (hb_parinfo(1))
        hb_retni( PQflush(( PGconn * ) hb_parptr(1)) );
}


HB_FUNC(PQSETNONBLOCKING)
{
    if (hb_pcount() == 2)
        hb_retl( PQsetnonblocking( ( PGconn * ) hb_parptr(1), hb_parl(2) ) );
}

HB_FUNC(PQISNONBLOCKING)
{
    if (hb_parinfo(1))
        hb_retl( PQisnonblocking( ( PGconn * ) hb_parptr(1) ) );
}

/* 
 * Trace Connection handling functions 
 */

HB_FUNC(PQCREATETRACE)
{
    FILE * pFile;

    if (hb_parinfo(1))
    {
        pFile = fopen( hb_parcx(1), "w+b");
    
        if (pFile != NULL)
            hb_retptr( ( FILE * ) pFile );
    }            
}

HB_FUNC(PQCLOSETRACE)
{
    if (hb_parinfo(1))
        fclose( ( FILE * ) hb_parptr(1) );
}

HB_FUNC(PQTRACE)
{
    if (hb_pcount() == 2)
        PQtrace( ( PGconn * ) hb_parptr(1), ( FILE * ) hb_parptr(2) );
}

HB_FUNC(PQUNTRACE)
{
    if (hb_parinfo(1))
        PQuntrace( ( PGconn * ) hb_parptr(1) );
}

HB_FUNC(PQSETERRORVERBOSITY)
{
    /* PQERRORS_TERSE   0
       PQERRORS_DEFAULT 1
       PQERRORS_VERBOSE 2
    */
    
    if (hb_pcount() == 2)
        hb_retni( ( PGVerbosity ) PQsetErrorVerbosity( ( PGconn * ) hb_parptr(1), ( PGVerbosity ) hb_parni(2) ) );
}


/* 
 * Large Object functions 
 */


HB_FUNC(LO_IMPORT)
{
    int ret = 0; 
    
    if (hb_pcount() == 2)
         ret = lo_import( ( PGconn * ) hb_parptr(1), hb_parcx(2) );

    hb_retni(ret);         
}

HB_FUNC(LO_EXPORT)
{
    int ret = 0; 
    
    if (hb_pcount() == 3)
    {
        ret = lo_export( ( PGconn * ) hb_parptr(1), ( Oid ) hb_parnl(2), hb_parcx(3) );

        if (ret != 1)
            ret = 0;
    }            

    hb_retl(ret);        
}

HB_FUNC(LO_UNLINK)
{   
    int ret = 0; 
           
    if (hb_pcount() == 2)
    {
        ret = lo_unlink( ( PGconn * ) hb_parptr(1), ( Oid ) hb_parnl(2) );        
        
        if (ret != 1)
            ret = 0;
    }            

    hb_retl(ret);        
}



#if HB_PGVERSION >= 0x0800

HB_FUNC(PQSERVERVERSION)
{
    if (hb_parinfo(1))
        hb_retni(PQserverVersion(( PGconn * ) hb_parptr(1)));
}

HB_FUNC(PQGETCANCEL)
{
    if (hb_parinfo(1))
        hb_retptr( ( PGcancel * ) PQgetCancel( ( PGconn * ) hb_parptr(1) ) );
}        

HB_FUNC(PQCANCEL)
{
    char errbuf[256];
    int ret = 0;

    if (hb_parinfo(1))
        if (PQcancel( ( PGcancel * ) hb_parptr(1), errbuf, 255) == 1)
        {
            ret = 1;                
            hb_storc( errbuf, 2 );
        }            
        
    hb_retl(ret);            
}        

HB_FUNC(PQFREECANCEL)
{
    if (hb_parinfo(1))
        PQfreeCancel( ( PGcancel * ) hb_parptr(1) ) ;
}        

#endif


/*

TODO: Implement Full Large Objects Support
TODO: Implement Prepared Query handling

extern int	lo_open(PGconn *conn, Oid lobjId, int mode);
extern int	lo_close(PGconn *conn, int fd);
extern int	lo_read(PGconn *conn, int fd, char *buf, size_t len);
extern int	lo_write(PGconn *conn, int fd, char *buf, size_t len);
extern int	lo_lseek(PGconn *conn, int fd, int offset, int whence);
extern Oid	lo_creat(PGconn *conn, int mode);
extern int	lo_tell(PGconn *conn, int fd);

PGresult *PQprepare(PGconn *conn,
                    const char *stmtName,
                    const char *query,
                    int nParams,
                    const Oid *paramTypes);
  
  
PGresult *PQexecPrepared(PGconn *conn,
                         const char *stmtName,
                         int nParams,
                         const char * const *paramValues,
                         const int *paramLengths,
                         const int *paramFormats,
                         int resultFormat);
                         
int PQsendQueryParams(PGconn *conn,
                      const char *command,
                      int nParams,
                      const Oid *paramTypes,
                      const char * const *paramValues,
                      const int *paramLengths,
                      const int *paramFormats,
                      int resultFormat);
                      
int PQsendPrepare(PGconn *conn,
                  const char *stmtName,
                  const char *query,
                  int nParams,
                  const Oid *paramTypes);
                  
int PQsendQueryPrepared(PGconn *conn,
                        const char *stmtName,
                        int nParams,
                        const char * const *paramValues,
                        const int *paramLengths,
                        const int *paramFormats,
                        int resultFormat);                                                                                     
                                                
*/
