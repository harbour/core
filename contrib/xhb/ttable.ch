/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for Table,Record and Field Class
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

#ifndef _OTABLE_CH_

// --> Network messages
#define _NET_USE_FAIL_MSG  "Net Open Fail !!!"


// --> ::dbMove() constants
#define _DB_TOP             -999999999999
#define _DB_BOTTOM          -888888888888
#define _DB_BOF             -777777777777
#define _DB_EOF             -666666666666
#define NET_RECLOCK           1
#define NET_FILELOCK          2
#define NET_APPEND            3
#define NET_OPEN_MODE        .T.
#define EXCLUSIVE_OPEN_MODE  .F.
#define RETRY_MSG            "NETWORK ERROR;Continue Attempt to Lock Record/File ?"
#define YESNO_COLOR          "R/W"
#define MAX_TABLE_AREAS      680

#xcommand DEFAULT <uVar1> := <uVal1> ;
               [, <uVarN> := <uValN> ] => ;
                  <uVar1> := iif( <uVar1> == NIL, <uVal1>, <uVar1> ) ;;
                [ <uVarN> := iif( <uVarN> == NIL, <uValN>, <uVarN> ); ]

#xcommand DEFAULT <v1> TO <x1> [, <vn> TO <xn> ]                        ;
          =>                                                            ;
          IF <v1> == NIL ; <v1> := <x1> ; END                           ;
          [; IF <vn> == NIL ; <vn> := <xn> ; END ]

// --> OOPs
#xtranslate BYNAME <V> [, <VN> ]     => ::<V> := <V> [; ::<VN> := <VN> ]
#xtranslate BYNAME <V> DEFAULT <Val> => ::<V> := BYDEFAULT <V>, <Val>
#xtranslate BYDEFAULT <V>, <Val>     => iif( <V> == NIL, <Val>, <V> )

#xcommand  NETUSE <(cDBF)>  ;
           [ALIAS <cAlias>] ;
           [VIA <cRDD>]     ;
           [TIMER <nSecs>]  ;
           [<new: NEW>]     ;
           [<ro: READONLY>] ;
           => ;
           NetDbUse( <(cDBF)>, <(cAlias)>, <nSecs>, <cRDD>, ;
                   <.new.>, NET_OPEN_MODE, <.ro.> )


// --> new table object
#xcommand DEFINE TABLE <oTable>         ;
                 [FILE <cFileDBF>]      ;
                 [INDEX <cFileIDX>]     ;
                 [ALIAS <cAlias>]       ;
                 [VIA <cDriver>]        ;
                 [PATH <cPathDBF>]      ;
                 [<lshared: SHARED>]    ;
                 [<lnew: NEW>]          ;
                 [<lrdonly: READONLY>]  ;
                 =>;
                 <oTable> := TableNew(                  ;
                                          <(cFileDBF)>, ;
                                          <"cAlias">,   ;
                                          <(cFileIDX)>, ;
                                          <(cDriver)>,  ;
                                          <.lshared.>,  ;
                                          <(cPathDBF)>, ;
                                          <.lnew.>,     ;
                                          <.lrdonly.> )


// --> new order object
#xcommand DEFINE ORDER [<oOrder>]   ;
                 ON [KEY] <key>     ;
                 [TAG <cTag>]       ;
                 [LABEL <cLabel>]   ;
                 [FOR <for>]        ;
                 [WHILE <while>]    ;
                 [EVAL <eval>]      ;
                 [EVERY <every>]    ;
                 [<unique: UNIQUE>] ;
                 [TO <cOrderFile>];
                 IN <oTable>        ;
                 =>;
                 [<oOrder>:=] <oTable>:AddOrder(               ;
                                                <(cTag)>,      ;
                                                <"key">,       ;
                                                <(cLabel)>,       ;
                                                <"for">,       ;
                                                <"while">,     ;
                                                [<.unique.>],  ;
                                                <{eval}>,      ;
                                                <every>,       ;
                                                <(cOrderFile)>;
                                                 )



#xcommand ADD FIELD <cFld> DATA [<xpression,...>] TO <oObj> ;
          => ;
          <oObj>:ClassAdd( <"cFld">,, {| Self | [<xpression>] },,)

#xcommand DEFINE FIELD <cFld> DATA [<xpression,...>] TO <oObj> ;
          => ;
          <oObj>:ClassAdd( <"cFld">,, {| Self | [<xpression>] },,)


// --> ::unDo() buffer constants

#define _WRITE_BUFFER        1
#define _DELETE_BUFFER       2
#define _RECALL_BUFFER       3


#xcommand BEGIN TRANSACTION IN <oTable> => <oTable>:SetMonitor( .T. )

#xcommand ROLLBACK <nType> ;
          [STEP <n>]       ;
          IN <oTable>      ;
          =>               ;
          <oTable>:UnDo( <nType>, [<n>] )

#xcommand END TRANSACTION IN <oTable>   => <oTable>:SetMonitor( .F. )

#command SKIP        in <o>           => <o>:dbSkip( 1 )
#command SKIP <n>    in <o>           => <o>:dbSkip( <n> )

#command SEEK <xpr>                                                        ;
         [<soft: SOFTSEEK>]                                                ;
         [<last: LAST>]     in <o>                                         ;
      => <o>:dbSeek( <xpr>, iif( <.soft.>, .T., NIL ), iif( <.last.>, .T., NIL ) )

#translate  CSY_TYPE Character  =>  "C"

#xtranslate CSY_TYPE Numeric    =>  "N"
#xtranslate CSY_TYPE Date       =>  "D"
#xtranslate CSY_TYPE Memo    =>  "M"
#xtranslate CSY_TYPE Logical    => "L"
#xtranslate CSY_TYPE Auto    => "A"
#xcommand CREATE DATABASE <o> FILE <file> => <o>:=HBTable():CreateTable(<(file)>);#define _TABLE_ <o>
#xtranslate FIELD [ <oFld> ]                        ;
                [ NAME <(cName)> ]                  ;
                [ TYPE <cType> ]                    ;
                [ LEN <nLen> ]                      ;
                [ DEC <nDec> ]                      ;
                OF <oDbf>                           ;
                => ;
    [ <oFld> := ] _TABLE_:AddField( <(cName)>,CSY_TYPE <cType>, <nLen>, <nDec>)
#xcommand BUILD TABLE <o> => _TABLE_:Gentable()
#define _OTABLE_CH_
#endif
