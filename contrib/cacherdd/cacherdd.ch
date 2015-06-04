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


#ifndef __CACHERDD_CH
   #define __CACHERDD_CH


#ifndef __XHARBOUR__
   /* Hash item functions */
   #xtranslate Hash( [<x,...>] )           => hb_Hash( <x> )
   #xtranslate HHasKey( [<x,...>] )        => hb_HHasKey( <x> )
   #xtranslate HGetPos( [<x,...>] )        => hb_HPos( <x> )
   #xtranslate HGet( [<x,...>] )           => hb_HGet( <x> )
   #xtranslate HSet( [<x,...>] )           => hb_HSet( <x> )
   #xtranslate HDel( [<x,...>] )           => hb_HDel( <x> )
   #xtranslate HGetKeyAt( [<x,...>] )      => hb_HKeyAt( <x> )
   #xtranslate HGetValueAt( [<x,...>] )    => hb_HValueAt( <x> )
   #xtranslate HSetValueAt( [<x,...>] )    => hb_HValueAt( <x> )
   #xtranslate HGetPairAt( [<x,...>] )     => hb_HPairAt( <x> )
   #xtranslate HDelAt( [<x,...>] )         => hb_HDelAt( <x> )
   #xtranslate HGetKeys( [<x,...>] )       => hb_HKeys( <x> )
   #xtranslate HGetValues( [<x,...>] )     => hb_HValues( <x> )
   #xtranslate HFill( [<x,...>] )          => hb_HFill( <x> )
   #xtranslate HClone( [<x,...>] )         => hb_HClone( <x> )
   #xtranslate HCopy( [<x,...>] )          => hb_HCopy( <x> )
   #xtranslate HMerge( [<x,...>] )         => hb_HMerge( <x> )
   #xtranslate HEval( [<x,...>] )          => hb_HEval( <x> )
   #xtranslate HScan( [<x,...>] )          => hb_HScan( <x> )
   #xtranslate HSetCaseMatch( <x>[, <z>] ) => ( hb_HCaseMatch( <x>[, <z>] ), <x> )
   #xtranslate HGetCaseMatch( [<x,...>] )  => hb_HCaseMatch( <x> )
   #xtranslate HSetAutoAdd( <x>[, <z>] )   => ( hb_HAutoAdd( <x>[, <z>] ), <x> )
   #xtranslate HGetAutoAdd( [<x,...>] )    => hb_HAutoAdd( <x> )
   #xtranslate HAllocate( [<x,...>] )      => hb_HAllocate( <x> )
   #xtranslate HDefault( [<x,...>] )       => hb_HDefault( <x> )
   #xtranslate HSetPartition( [<x,...>] )  =>

   /* TRY / CATCH / FINALLY / END */
   #xcommand TRY  => BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
   #xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
   #xcommand FINALLY => ALWAYS

   #xtranslate CreateObject( [<x,...>] )   => Win_OleCreateObject( <x> )

#endif

// REQUEST Constants to set/get information
//
#define CACHE_TUNETABLE                           55

#define CACHE_DROPINDEX                           65
//      RLOCKS                                    66
#define CACHE_EXISTINDEXBYBAG                     67
#define CACHE_LASTINFO                            68
#define CACHE_DROPTABLE                           69
#define CACHE_EXISTINDEXBYTAG                     70
#define CACHE_BEGINTRANSACTION                    71
#define CACHE_ENDTRANSACTION                      72
#define CACHE_ROLLBACKTRANSACTION                 73

#define CACHE_SETSTRUCTURE                        74
#define CACHE_EXISTTABLE                          75
#define CACHE_CREATEINDEX                         76

#define CACHE_STRUCTURE                           93
#define CACHE_OWNERLOCKLIST                       98
#define CACHE_SYSTEMFILELOCKLIST                  99

#define CACHE_TABLEINFO                           2201
#define CACHE_LASTERROR                           2202


#define NTRIM( n )                                LTrim( Str( n ) )
#define COMPILE( c )                              &( '{|v|' + c + '}' )
#define CRLF                                      Chr( 13 ) + Chr( 10 )


// WorkArea Structure Members
//
#define WA_DATABASE                               1
#define WA_WORKAREA                               2
#define WA_OPENINFO                               3
#define WA_RECNO                                  4
#define WA_BOF                                    5
#define WA_FORCEBOF                               6
#define WA_EOF                                    7
#define WA_TOP                                    8
#define WA_BOTTOM                                 9
#define WA_FOUND                                  10
#define WA_LOCKS                                  11
#define WA_STRUCT                                 12
#define WA_ORDER                                  13
#define WA_ORDINFO                                14
#define WA_SCOPEINFO                              15
#define WA_LENFIELDS                              16
#define WA_ISHOT                                  17
#define WA_APPENDLOCKREC                          18
#define WA_BUFFER                                 19
#define WA_TABLENAME                              20
#define WA_CACHENAME                              21
#define WA_PREVREC                                22
#define WA_PREVBUFFER                             23
#define WA_REBUILD                                24
#define WA_FILTER                                 25
#define WA_LASTRECORD                             26
#define WA_CONXN                                  27
#define WA_ORDCOND                                28
#define WA_SCHEMA                                 29
#define WA_FETCH                                  30
#define WA_RELATIONS                              31
#define WA_ISMEMO                                 32
#define WA_TABLEID                                33
#define WA_EMPTYBUFFER                            34
#define WA_ISMOVED                                35
#define WA_FIELDNOSTR                             36
#define WA_RECBUFFER                              37
#define WA_FIELDOFFSET                            38
#define WA_FIELDLEN                               39
#define WA_REPOSITION                             40

#define WA_SIZEOF                                 40


//  To define Create/Open <CARGO> Info
//
#define CG_SCHEMA                                 1
#define CG_TRIGGERS                               2

#define CG_SIZE                                   2

#define TRG_NAME                                  1
#define TRG_EVENT                                 2
#define TRG_TIME                                  3
#define TRG_CODE                                  4


#define CACHE_FIELDS_FETCH_MODE_RECORD            1
#define CACHE_FIELDS_FETCH_MODE_FIELDS            2


#define DBCI_CONXN                                1
#define DBCI_IP                                   2
#define DBCI_PORT                                 3
#define DBCI_DATABASE                             4
#define DBCI_SCHEMA                               5
#define DBCI_CONXNSTRING                          6
#define DBCI_USEEXCLUSIVE                         7

#define DBCI_SIZE                                 7


#define INSERT_LOCK_MODE_CLIPPER                  1
#define INSERT_LOCK_MODE_ADVANTAGE                2
#define INSERT_LOCK_MODE_NOLOCK                   3


// SQL Variable Types
//
#define SQL_UNKNOWN_TYPE                          0
#define SQL_CHAR                                  1
#define SQL_NUMERIC                               2
#define SQL_DECIMAL                               3
#define SQL_INTEGER                               4
#define SQL_SMALLINT                              5
#define SQL_FLOAT                                 6
#define SQL_REAL                                  7
#define SQL_DOUBLE                                8
#define SQL_DATETIME                              9
#define SQL_VARCHAR                               12


// Element structure of CacheGetProcesses()
//
#define PROCESS_NUMBER                            1     // Character
#define PROCESS_EXE                               2     // Character
#define PROCESS_IP                                3     // Character
#define PROCESS_LINES                             4     // Character
#define PROCESS_NODE                              5     // Character
#define PROCESS_INTRANSACTION                     6     // Character
#define PROCESS_NAMESPACE                         7     // Character
#define PROCESS_OSUSERNAME                        8     // Character
#define PROCESS_STATE                             9     // Character
#define PROCESS_CACHEUSERNAME                     10    // Character
#define PROCESS_USERINFO                          11    // Character


#define NME_FULLNAME                              1
#define NME_SCHEMA                                2
#define NME_TABLE                                 3
#define NME_CACHETABLE                            4
#define NME_ALIAS                                 5
#define NME_CACHESQLNAME                          6
#define NME_CACHECLASSNAME                        7

#define NME_SIZE                                  7

#if 0
#command USE <(db)> [VIA <rdd>] [ALIAS <a>] [<nw: NEW>] ;
            [<ex: EXCLUSIVE>] [<sh: SHARED>] [<ro: READONLY>] ;
            [CODEPAGE <cp>] [CONNECTION <nConn>] [INDEX <(index1)> [, <(indexN)>]] => ;
         dbUseArea( <.nw.>, <rdd>, <(db)>, <(a)>, ;
                    if(<.sh.> .or. <.ex.>, !<.ex.>, NIL), <.ro.>,  [<cp>], [<nConn>] ) ;
         [; dbSetIndex( <(index1)> )] ;
         [; dbSetIndex( <(indexN)> )]
#endif

#ifdef __XHARBOUR__
#xtranslate Thread Static => STATIC
#endif


#endif                                            //__CACHERDD_CH

