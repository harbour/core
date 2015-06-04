/*
 * Copyright 2006-2015 Pritpal Bedi <bedipritpal@hotmail.com>
 * Copyright 2006-2015 CURACAO - http://www.icuracao.com
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


#include "cacherdd.ch"
#include "common.ch"


#define TBL_DEF_DBFNAME                           1
#define TBL_DEF_ALIAS                             2
#define TBL_DEF_STRUCT                            3
#define TBL_DEF_INDEXES                           4
#define TBL_DEF_TRIGGERS                          5
#define TBL_DEF_KEEPDELETEDS                      6

#define TBL_DEF_SIZE                              6

#define TILT                                      "~"
#define PARAM                                     "|"
#define HASH                                      "#"


/*
   Stores the various information about a table represented by its
   name/schema/server/namespace.

   Returns previously set definition.

   Will be used TO auto CREATE/index/reindex/uploads/downloads/triggers/
   AND other features as may be coming on way, outside of the RDD.

   The main goal of this API is TO control database behavior per table basis
   via stand alone utilities a developer could provide TO his client without
   any code changes TO the existing applications.
*/
FUNCTION CacheSetDefinition( cTable, aTableDef, cSchema, nConxn )
   LOCAL i, aNames
   LOCAL cCache := ""
   LOCAL cIdx   := ""
   LOCAL cStr   := ""
   LOCAL cTrg   := ""

   DEFAULT nConxn  TO CacheSetConnection()
   DEFAULT cSchema TO CacheSetSchema( , nConxn )

   aNames := CacheResolveNames( cTable )

   IF "||" $ cTable
      cSchema := aNames[ NME_SCHEMA ]
   ENDIF
   IF ! CacheSetSchemaAsIs()
      cSchema := Upper( cSchema )
   ENDIF

   cTable := aNames[ NME_TABLE ]

   cCache += cSchema                                                    // 1
   cCache += PARAM + cTable                                             // 2

   DEFAULT aTableDef TO {}
   aSize( aTableDef, TBL_DEF_SIZE )

   DEFAULT aTableDef[ TBL_DEF_DBFNAME      ] TO ""
   DEFAULT aTableDef[ TBL_DEF_ALIAS        ] TO ""
   DEFAULT aTableDef[ TBL_DEF_STRUCT       ] TO {}
   DEFAULT aTableDef[ TBL_DEF_INDEXES      ] TO {}
   DEFAULT aTableDef[ TBL_DEF_TRIGGERS     ] TO {}
   DEFAULT aTableDef[ TBL_DEF_KEEPDELETEDS ] TO .F.

   cCache += PARAM + aTableDef[ TBL_DEF_DBFNAME ]                       // 3
   cCache += PARAM + aTableDef[ TBL_DEF_ALIAS   ]                       // 4

   FOR i := 1 TO len( aTableDef[ TBL_DEF_STRUCT ] )
      /*
      cStr += StrTran( aTableDef[ TBL_DEF_STRUCT, i, 1 ], '_','=' ) +' '+ ;
                     aTableDef[ TBL_DEF_STRUCT, i, 2 ]   +' '+ ;
              NTRIM( aTableDef[ TBL_DEF_STRUCT, i, 3 ] ) +' '+ ;
              NTRIM( aTableDef[ TBL_DEF_STRUCT, i, 4 ] ) +' '+ ;
              TILT
      */
      cStr +=        aTableDef[ TBL_DEF_STRUCT, i, 1 ]   +" "+ ;
                     aTableDef[ TBL_DEF_STRUCT, i, 2 ]   +" "+ ;
              NTRIM( aTableDef[ TBL_DEF_STRUCT, i, 3 ] ) +" "+ ;
              NTRIM( aTableDef[ TBL_DEF_STRUCT, i, 4 ] ) +" "+ ;
              TILT
   NEXT

   cCache += PARAM + cStr                                               // 5

   FOR i := 1 TO len( aTableDef[ TBL_DEF_INDEXES ] )
      aSize( aTableDef[ TBL_DEF_INDEXES, i ], 3 )

      DEFAULT aTableDef[ TBL_DEF_INDEXES, i, 1 ] TO ""
      DEFAULT aTableDef[ TBL_DEF_INDEXES, i, 2 ] TO ""
      DEFAULT aTableDef[ TBL_DEF_INDEXES, i, 3 ] TO ""

      cIdx += aTableDef[ TBL_DEF_INDEXES, i, 1 ] + HASH + ;
              aTableDef[ TBL_DEF_INDEXES, i, 2 ] + HASH + ;
              aTableDef[ TBL_DEF_INDEXES, i, 3 ] + HASH + ;
              TILT
   NEXT

   cCache += PARAM+ cIdx                                               // 6

   FOR i := 1 TO Len( aTableDef[ TBL_DEF_TRIGGERS ] )
      cTrg += ''
   NEXT
                                                                       // 7
   cCache += PARAM + cTrg
   cCache += PARAM + iif( aTableDef[ TBL_DEF_KEEPDELETEDS ], "T", "F" )// 8
   cCache += PARAM
   cCache := StrTran( cCache, "'", '"' )

   RETURN CacheSetGet( nConxn, 0, 121, cCache )                        // -> aDefinition


FUNCTION CacheGetDefinition( cTable, cSchema, nConxn )
   LOCAL aNames
   LOCAL cCache := ""

   DEFAULT nConxn  TO CacheSetConnection()
   DEFAULT cSchema TO CacheSetSchema( , nConxn )

   aNames := CacheResolveNames( cTable )

   IF ( "||" $ cTable )
      cSchema := aNames[ NME_SCHEMA ]
   ENDIF
   IF ! CacheSetSchemaAsIs()
      cSchema := Upper( cSchema )
   ENDIF

   cTable := aNames[ NME_TABLE ]

   cCache += cSchema
   cCache += PARAM + cTable

   RETURN CacheSetGet( nConxn, 0, 122, cCache )

