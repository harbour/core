/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * ADORDD - RDD to automatically manage Microsoft ADO
 *
 * Copyright 2007 Fernando Mancera <fmancera@viaopen.com> and
 * Antonio Linares <alinares@fivetechsoft.com>
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

#ifndef _ADORDD_CH
#define _ADORDD_CH

// Cursor Type
#define adOpenForwardOnly     0
#define adOpenKeyset          1
#define adOpenDynamic         2
#define adOpenStatic          3

// Lock Types
#define adLockReadOnly        1
#define adLockPessimistic     2
#define adLockOptimistic      3
#define adLockBatchOptimistic 4

// Field Types
#define adEmpty                           0
#define adTinyInt                         16
#define adSmallInt                        2
#define adInteger                         3
#define adBigInt                          20
#define adUnsignedTinyInt                 17
#define adUnsignedSmallInt                18
#define adUnsignedInt                     19
#define adUnsignedBigInt                  21
#define adSingle                          4
#define adDouble                          5
#define adCurrency                        6
#define adDecimal                         14
#define adNumeric                         131
#define adBoolean                         11
#define adError                           10
#define adUserDefined                     132
#define adVariant                         12
#define adIDispatch                       9
#define adIUnknown                        13
#define adGUID                            72
#define adDate                            7
#define adDBDate                          133
#define adDBTime                          134
#define adDBTimeStamp                     135
#define adBSTR                            8
#define adChar                            129
#define adVarChar                         200
#define adLongVarChar                     201
#define adWChar                           130
#define adVarWChar                        202
#define adLongVarWChar                    203
#define adBinary                          128
#define adVarBinary                       204
#define adLongVarBinary                   205
#define adChapter                         136
#define adFileTime                        64
#define adPropVariant                     138
#define adVarNumeric                      139
#define adArray                           // &H2000

#define adRecDeleted                      4

#define adUseNone             1
#define adUseServer           2
#define adUseClient           3
#define adUseClientBatch      3

#define adKeyForeign          2

#command USE <(db)> [VIA <rdd>] [ALIAS <a>] [<nw: NEW>] ;
            [<ex: EXCLUSIVE>] [<sh: SHARED>] [<ro: READONLY>] ;
            [CODEPAGE <cp>] [INDEX <(index1)> [, <(indexN)>]] ;
            [ TABLE <cTable> ] ;
            [ <dbEngine: ACCESS, MYSQL, ORACLE, INFORMIX, SQL> ];
            [ FROM <cServer> ] ;
            [ QUERY <cQuery> ] ; 
            [ USER <cUser> PASSWORD <cPassword> ]=> ;
         [ HB_AdoSetTable( <cTable> ) ; ] ;
         [ HB_AdoSetEngine( <(dbEngine)> ) ; ] ;
         [ HB_AdoSetServer( <cServer> ) ; ] ;
         [ HB_AdoSetQuery( <cQuery> ) ; ] ;
         [ HB_AdoSetUser( <cUser> ); HB_AdoSetPassword( <cPassword> ) ; ] ;   
         dbUseArea( <.nw.>, <rdd>, <(db)>, <(a)>, ;
                    if(<.sh.> .or. <.ex.>, !<.ex.>, NIL), <.ro.> [, <cp>] ) ;
         [; dbSetIndex( <(index1)> )] ;
         [; dbSetIndex( <(indexN)> )]
         
#command LOCATE [FOR <for>] [WHILE <while>] [NEXT <next>] ;
                [RECORD <rec>] [<rest:REST>] [ALL] => ;
         [ HB_AdoSetLocateFor( <(for)> ); ] ;        
         __dbLocate( <{for}>, <{while}>, <next>, <rec>, <.rest.> )

#endif