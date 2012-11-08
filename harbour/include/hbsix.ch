/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    SIX compatible library PP rules
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#ifndef HB_SIX_CH_
#define HB_SIX_CH_

#include "hbsxdef.ch"

/*
 * obsolete SIx Driver functions
 */
#xtranslate Sx_IndexFilter( [<nOrder>] )           => ordFor( [<nOrder>] )
#xtranslate Sx_TagName([<nOrder>])                 => iif( Used(), ordName( [<nOrder>] ), "" )
#xtranslate Sx_SetTagOrder( [<xOrder>] [,<cBag>] ) => sx_SetTag( [<xOrder>] [,<cBag>] )
#xtranslate Sx_SetTagOrd( [<xOrder>] [,<cBag>] )   => sx_SetTag( [<xOrder>] [,<cBag>] )
#xtranslate Sx_SetTagNo( [<xOrder>] )              => sx_SetTag( [<xOrder>], iif( ordNumber()>0, ordBagName(), ordBagName(1) ) )
#xtranslate Sx_SetTagNo( <xOrder>, <cBag> )        => sx_SetTag( <xOrder>, <cBag> )
#xtranslate _sxCondSet( [<params,...>] )           => ordCondSet( [<params>] )
#xtranslate SetRDD( [<cRDDname>] )                 => rddSetDefault( [<cRDDname>] )
/* SIx 2.0 Compatibity */
#command SET DIRTYREAD ON                       => sx_SetTurbo( .T. )
#command SET DIRTYREAD OFF                      => sx_SetTurbo( .F. )
#xtranslate Sx_SetDirty( [<param>] )            => sx_SetTurbo( [<param>] )
#xtranslate Sx_DirtyArea( [<param>] )           => sx_TurboArea( [<param>] )


/*
 * USE command with support for TRIGGER and PASSWORD clauses
 */
#command USE <(db)> [VIA <rdd>] [ALIAS <a>] [<nw: NEW>] ;
            [<ex: EXCLUSIVE>] [<sh: SHARED>] [<ro: READONLY>] ;
            [CODEPAGE <cp>] [INDEX <(index1)> [, <(indexN)>]] ;
            [TRIGGER <trig>] [PASSWORD <pass>] => ;
         [sx_SetTrigger( TRIGGER_PENDING, <trig>, <rdd> ); ] <-trig-> ;
         [sx_SetPass( <pass>, 1, <rdd> ); ] <-pass-> ;
         dbUseArea( <.nw.>, <rdd>, <(db)>, <(a)>, ;
                    iif(<.sh.> .or. <.ex.>, ! <.ex.>, NIL), <.ro.> [, <cp>] ) ;
         [; dbSetIndex( <(index1)> )] ;
         [; dbSetIndex( <(indexN)> )]


/*
 * SORT command with USECURRENT clause
 */
#command SORT [TO <(f)>] [ON <fields,...>] ;
              [FOR <for>] [WHILE <while>] [NEXT <next>] ;
              [RECORD <rec>] [<rest:REST>] [<all:ALL>] ;
              [<cur: USECURRENT>] [NOOPTIMIZE] => ;
         sx_SortOption(<.cur.>); ;
         __dbSort( <(f)>, { <(fields)> }, ;
                   <{for}>, <{while}>, <next>, <rec>, <.rest.> )


/*
 * Seek using wildcards
 */
#xcommand WILDSEEK <str>      => sx_WildSeek( <str> )
#xcommand WILDSEEKNEXT <str>  => sx_WildSeek( <str>, .T. )

/*
 * order management commands
 */
#command CLEAR ORDER <order>                    => sx_ClearOrder( <order> )
#command SET TAGORDER TO <order>                => ordSetFocus( <order> )
#command SET TAGORDER TO                        => ordSetFocus( 0 )
#command SET ORDER TO TAG <(tag)> [OF <(bag)>]  => ;
         ordSetFocus( <(tag)> [, <(bag)>] )
#command SET TAG TO <tag> [OF <(bag)>]          => ;
         ordSetFocus( <(tag)> [, <(bag)>] )
#command SET TAG TO                             => ordSetFocus( 0 )

#command REINDEX OPTION <eval> [STEP <step>]    => ;
         REINDEX EVAL <eval> [EVERY <step>]
#command DELETE TAG <(tag1)> [OF <(bag1)>] [, <(tagN)> [OF <(bagN)>]] => ;
         ordDestroy( <(tag1)>, <(bag1)> )[ ; ordDestroy( <(tagN)>, <(bagN)> ) ]
#command DELETE TAG ALL [OF <(bag)>]            => sx_KillTag( .T., <(bag)> )


/*
 * order scope commands
 */
#command CLEAR SCOPE                            => sx_ClrScope()
#xcommand SET SCOPETOP TO <value>               => sx_SetScope( 0, <value> )
#xcommand SET SCOPETOP TO                       => sx_ClrScope( 0 )
#xcommand SET SCOPEBOTTOM TO <value>            => sx_SetScope( 1, <value> )
#xcommand SET SCOPEBOTTOM TO                    => sx_ClrScope( 1 )
#command SET SCOPE TO                           => sx_ClrScope()
#command SET SCOPE TO <value>                   => sx_SetScope( 0, <value> ) ;
                                                 ; sx_SetScope( 1, <value> )

/*
 * TURBO(DIRTY) READ commands
 */
#command SET TURBOREAD ON                       => sx_SetTurbo( .T. )
#command SET TURBOREAD OFF                      => sx_SetTurbo( .F. )


/*
 * MEMO commands
 */
#command MEMOPACK [BLOCK <size>] [OPTION <opt> [STEP <step>]] => ;
            sx_MemoPack( <size>, <{opt}>, <step> )
#command SET MEMOBLOCK TO <value>               => sx_SetMemoBlock( <value> )

/*
 * indexing
 */
#command SUBINDEX ON <key> TO <(file)> ;
               [OPTION <eval> [STEP <every>]] ;
               [<filter: FILTERON>] ;
               [<cust: EMPTY>] ;
               [FOR <for>] [WHILE <while>] [NEXT <next>] ;
               [RECORD <rec>] [<rest:REST>] [<all:ALL>] ;
               [EVAL <eval>] [EVERY <every>] [<unique: UNIQUE>] ;
               [<ascend: ASCENDING>] [<descend: DESCENDING>] ;
               [<add: ADDITIVE>] [<cust: CUSTOM>] ;
               [<noopt: NOOPTIMIZE>] [<mem: MEMORY, TEMPORARY>] ;
               [<filter: USEFILTER>] [<ex: EXCLUSIVE>] => ;
         INDEX ON <key> TO <(file)> USECURRENT ;
            [FOR <for>] [WHILE <while>] [NEXT <next>] ;
            [RECORD <rec>] <rest> <all> ;
            [EVAL <eval>] [EVERY <every>] <unique> ;
            <ascend> <descend> <add> [<-cust-> CUSTOM] ;
            <noopt> <mem> [<-filter-> USEFILTER]

#command SUBINDEX ON <key> TAG <(tag)> [OF <(bag)>] [TO <(bag)>] ;
               [OPTION <eval> [STEP <every>]] ;
               [<filter: FILTERON>] ;
               [<cust: EMPTY>] ;
               [FOR <for>] [WHILE <while>] [NEXT <next>] ;
               [RECORD <rec>] [<rest:REST>] [<all:ALL>] ;
               [EVAL <eval>] [EVERY <every>] [<unique: UNIQUE>] ;
               [<ascend: ASCENDING>] [<descend: DESCENDING>] ;
               [<add: ADDITIVE>] [<cust: CUSTOM>] ;
               [<noopt: NOOPTIMIZE>] [<mem: MEMORY, TEMPORARY>] ;
               [<filter: USEFILTER>] [<ex: EXCLUSIVE>] => ;
         INDEX ON <key> TAG <(tag)> [TO <(bag)>] USECURRENT ;
            [FOR <for>] [WHILE <while>] [NEXT <next>] ;
            [RECORD <rec>] <rest> <all> ;
            [EVAL <eval>] [EVERY <every>] <unique> ;
            <ascend> <descend> <add> [<-cust-> CUSTOM] ;
            <noopt> <mem> [<-filter-> USEFILTER]

#command INDEX ON <key> [TAG <(tag)>] TO <(bag)> ;
               [OPTION <eval> [STEP <every>]] ;
               [<filter: FILTERON>] ;
               [<cust: EMPTY>] ;
               [<cur: SUBINDEX>] ;
               [FOR <for>] [WHILE <while>] [NEXT <next>] ;
               [RECORD <rec>] [<rest:REST>] [<all:ALL>] ;
               [EVAL <eval>] [EVERY <every>] [<unique: UNIQUE>] ;
               [<ascend: ASCENDING>] [<descend: DESCENDING>] ;
               [<add: ADDITIVE>] [<cur: USECURRENT>] [<cust: CUSTOM>] ;
               [<noopt: NOOPTIMIZE>] [<mem: MEMORY, TEMPORARY>] ;
               [<filter: USEFILTER>] [<ex: EXCLUSIVE>] => ;
         ordCondSet( <"for">, <{for}>, [<.all.>], <{while}>, ;
                     <{eval}>, <every>, RecNo(), <next>, <rec>, ;
                     [<.rest.>], [<.descend.>],, ;
                     [<.add.>], [<.cur.>], [<.cust.>], [<.noopt.>], ;
                     <"while">, [<.mem.>], [<.filter.>], [<.ex.>] ) ;;
         ordCreate( <(bag)>, <(tag)>, <"key">, <{key}>, [<.unique.>] )

#command INDEX ON <key> TAG <(tag)> [OF <(bag)>] [TO <(bag)>] ;
               [OPTION <eval> [STEP <every>]] ;
               [<filter: FILTERON>] ;
               [<cust: EMPTY>] ;
               [<cur: SUBINDEX>] ;
               [FOR <for>] [WHILE <while>] [NEXT <next>] ;
               [RECORD <rec>] [<rest:REST>] [<all:ALL>] ;
               [EVAL <eval>] [EVERY <every>] [<unique: UNIQUE>] ;
               [<ascend: ASCENDING>] [<descend: DESCENDING>] ;
               [<add: ADDITIVE>] [<cur: USECURRENT>] [<cust: CUSTOM>] ;
               [<noopt: NOOPTIMIZE>] [<mem: MEMORY, TEMPORARY>] ;
               [<filter: USEFILTER>] [<ex: EXCLUSIVE>] => ;
         ordCondSet( <"for">, <{for}>, [<.all.>], <{while}>, ;
                     <{eval}>, <every>, RecNo(), <next>, <rec>, ;
                     [<.rest.>], [<.descend.>],, ;
                     [<.add.>], [<.cur.>], [<.cust.>], [<.noopt.>], ;
                     <"while">, [<.mem.>], [<.filter.>], [<.ex.>] ) ;;
         ordCreate( <(bag)>, <(tag)>, <"key">, <{key}>, [<.unique.>] )

#endif /* HB_SIX_CH_ */
