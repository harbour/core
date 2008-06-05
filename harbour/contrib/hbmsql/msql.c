/*
 * $Id$
 */


/*
 * Harbour Project source code:
 * mSQL DBMS low level (client api) interface code.
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

#include "hbapi.h"
#include "hbapiitm.h"

#ifdef HB_OS_WIN_32
   /* This is needed by msql.h to indicate the platform. */
   #define _OS_WIN32
#endif

#include "msql.h"

HB_FUNC(MSQLCONNEC) /* int msqlConnect(char *) */
{
   hb_retni(msqlConnect(hb_parc(1)));
}


HB_FUNC(MSQLCLOSE) /* void msqlClose(int) */
{
   msqlClose(hb_parni(1));
}


HB_FUNC(MSQLSELECT) /* int msqlSelectDB(int, char *) */
{
   hb_retni(msqlSelectDB(hb_parni(1), hb_parc(2)));
}


HB_FUNC(MSQLQUERY) /* int msqlQuery(int, char *) */
{
   hb_retni(msqlQuery(hb_parni(1), hb_parc(2)));
}


HB_FUNC(MSQLSTORER) /* m_result *msqlStoreResult() */
{
   hb_retnl((long) msqlStoreResult());
}


HB_FUNC(MSQLFREER) /* void msqlFreeResult(m_result *) */
{
   msqlFreeResult((m_result *)hb_parnl(1));
}


/* NOTE: need number of retrieved fields */
HB_FUNC(MSQLFETCHR) /* m_row msqlFetchRow(m_result *, int) */
{
   m_result *mresult = (m_result *)hb_parnl(1);
   int num_fields = hb_parnl(2);

   PHB_ITEM aRow = hb_itemArrayNew(num_fields);
   PHB_ITEM temp;
   m_row mrow;
   int i;

   mrow = msqlFetchRow(mresult);

   for (i = 0; i < num_fields; i++) {

      /* if field is not empty */
      if (mrow[i] != NULL) {
         temp = hb_itemPutC(NULL, mrow[i]);
       } else {
         temp = hb_itemPutC(NULL, "");
       }

      hb_itemArrayPut(aRow, i + 1, temp);
      hb_itemRelease(temp);
   }
   hb_itemReturnRelease(aRow);
}


HB_FUNC(MSQLDATASE) /* void msqlDataSeek(m_result *, int) */
{
   msqlDataSeek((m_result *)hb_parnl(1), hb_parni(2));
}


HB_FUNC(MSQLNUMROW) /* int msqlNumRows(m_result *) */
{
   hb_retni(msqlNumRows(((m_result *)hb_parnl(1))));
}


HB_FUNC(MSQLFETCHF) /* m_field *msqlFetchField(m_result *) */
{
   /* NOTE: m_field structure of mSQL 2.x has 5 members */
   PHB_ITEM aField = hb_itemArrayNew(5);

   PHB_ITEM temp;
   m_field *mfield;

   mfield = msqlFetchField((m_result *)hb_parnl(1));
   if (!(mfield == NULL)) {
      temp = hb_itemPutC(NULL, mfield->name);
      hb_itemArrayPut(aField, 1, temp);
      hb_itemRelease(temp);
      temp = hb_itemPutC(NULL, mfield->table);
      hb_itemArrayPut(aField, 2, temp);
      hb_itemRelease(temp);
      temp = hb_itemPutNL(NULL, mfield->type);
      hb_itemArrayPut(aField, 3, temp);
      hb_itemRelease(temp);
      temp = hb_itemPutNL(NULL, mfield->length);
      hb_itemArrayPut(aField, 4, temp);
      hb_itemRelease(temp);
      temp = hb_itemPutNL(NULL, mfield->flags);
      hb_itemArrayPut(aField, 5, temp);
      hb_itemRelease(temp);
   }
   hb_itemReturnRelease(aField);
}


HB_FUNC(MSQLFIELDS) /* void msqlFieldSeek(m_result *, int) */
{
   msqlFieldSeek((m_result *)hb_parnl(1), hb_parni(2));
}


HB_FUNC(MSQLNUMFIE) /* int msqlNumFields(m_result *) */
{
   hb_retni(msqlNumFields(((m_result *)hb_parnl(1))));
}


HB_FUNC(MSQLLISTFI) /* m_result *msqlListFields(int, char *); */
{
   hb_retnl((long) msqlListFields(hb_parni(1), hb_parc(2)));
}


HB_FUNC(MSQLGETERR) /* char *msqlGetErrMsg(char *); */
{
   hb_retc(msqlGetErrMsg(NULL));
}


HB_FUNC(MSQLLISTDB) /* m_result * msqlListDBs(int); */
{
   int sock = hb_parnl(1);
   m_result *mresult;
   m_row mrow;
   long nr, i;
   PHB_ITEM aDBs;
   PHB_ITEM temp;

   mresult = msqlListDBs(sock);
   nr = msqlNumRows(mresult);
   aDBs = hb_itemArrayNew(nr);

   for (i = 0; i < nr; i++) {
      mrow = msqlFetchRow(mresult);
      temp = hb_itemPutC(NULL, mrow[0]);

      hb_itemArrayPut(aDBs, i + 1, temp);
      hb_itemRelease(temp);
   }

   msqlFreeResult(mresult);
   hb_itemReturnRelease(aDBs);
}


HB_FUNC(MSQLLISTTA) /* m_result * msqlListTables(int); */
{
   int sock = hb_parnl(1);
   m_result *mresult;
   m_row mrow;
   long nr, i;
   PHB_ITEM aTables;
   PHB_ITEM temp;

   mresult = msqlListTables(sock);
   nr = msqlNumRows(mresult);
   aTables = hb_itemArrayNew(nr);

   for (i = 0; i < nr; i++) {

      mrow = msqlFetchRow(mresult);
      temp = hb_itemPutC(NULL, mrow[0]);

      hb_itemArrayPut(aTables, i + 1, temp);
      hb_itemRelease(temp);
   }

   msqlFreeResult(mresult);
   hb_itemReturnRelease(aTables);
}

