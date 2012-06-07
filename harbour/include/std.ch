/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#ifndef HB_STD_CH_
#define HB_STD_CH_

#include "set.ch"

/* To suppress unused variable -w2 warnings. The code snippet will be
   optimized out by the compiler, so it won't cause any overhead.
   It can be used in codeblocks, too. */
#define HB_SYMBOL_UNUSED( symbol )  ( ( symbol ) )

#ifdef HB_CLP_STRICT
   #command END SEQUENCE      => end
   #command ENDSEQUENCE       => end
#else
   #command END SEQUENCE      => endsequence
#endif
/* Harbour extensions */
#command END SWITCH        => endswitch
#command END WITH          => endwith
#command END OBJECT        => endwith

#command DO WHILE <exp>    => while <exp>
#command END <x>           => end
#command ENDDO <*x*>       => enddo
#command ENDIF <*x*>       => endif
#command ENDCASE <*x*>     => endcase
#command ENDFOR [<*x*>]    => next
#command NEXT <v> [TO <x>] [STEP <s>]  => next

#command DO <proc>.prg [WITH <p,...>]  => do <proc> [ WITH <p>]
#command CALL <proc>() [WITH <p,...>]  => call <proc> [ WITH <p>]
#command STORE <v> TO <v1> [,<vN>]     => <v1> := [ <vN> :=] <v>

#command SET ECHO <*x*>       =>
#command SET HEADING <*x*>    =>
#command SET MENU <*x*>       =>
#command SET STATUS <*x*>     =>
#command SET STEP <*x*>       =>
#command SET SAFETY <*x*>     =>
#command SET TALK <*x*>       =>
#command SET PROCEDURE TO     =>
#command SET PROCEDURE TO <f> => _ProcReq_( <(f)> )
#command SET FORMAT TO <id>            => ;
         _ProcReq_( <(id)> + ".fmt" ) ; __SetFormat( {|| <id>()} )
#command SET FORMAT TO <id>.<ext>      => ;
         _ProcReq_( <(id)> + "." + <(ext)> ) ; __SetFormat( {|| <id>()} )
#command SET FORMAT TO <id:&>          => ;
         if ( Empty(<(id)>) ) ; SET FORMAT TO ; else ;;
         __SetFormat( &("{||" + <(id)> + "()}") ) ; end
#command SET FORMAT TO                 => __SetFormat()

#define _DFSET(x, y) Set( _SET_DATEFORMAT, if(__SetCentury(), x, y) )
#command SET DATE [TO] AMERICAN        => _DFSET( "mm/dd/yyyy", "mm/dd/yy" )
#command SET DATE [TO] ANSI            => _DFSET( "yyyy.mm.dd", "yy.mm.dd" )
#command SET DATE [TO] BRITISH         => _DFSET( "dd/mm/yyyy", "dd/mm/yy" )
#command SET DATE [TO] FRENCH          => _DFSET( "dd/mm/yyyy", "dd/mm/yy" )
#command SET DATE [TO] GERMAN          => _DFSET( "dd.mm.yyyy", "dd.mm.yy" )
#command SET DATE [TO] ITALIAN         => _DFSET( "dd-mm-yyyy", "dd-mm-yy" )
#command SET DATE [TO] JAPANESE        => _DFSET( "yyyy/mm/dd", "yy/mm/dd" )
#command SET DATE [TO] USA             => _DFSET( "mm-dd-yyyy", "mm-dd-yy" )

#command SET CENTURY <x:ON,OFF,&>      => __SetCentury( <(x)> )
#command SET CENTURY (<x>)             => __SetCentury( <x> )
#command SET DATE FORMAT [TO] <f>      => Set( _SET_DATEFORMAT, <f> )
#command SET TIME FORMAT [TO] <f>      => Set( _SET_TIMEFORMAT, <f> )
#command SET EPOCH TO <y>              => Set( _SET_EPOCH, <y> )

#command SET DECIMALS TO <x>           => Set( _SET_DECIMALS, <x> )
#command SET DECIMALS TO               => Set( _SET_DECIMALS, 0 )
#command SET DEFAULT TO <(path)>       => Set( _SET_DEFAULT, <(path)> )
#command SET DEFAULT TO                => Set( _SET_DEFAULT, "" )
#command SET PATH TO <*pth*>           => Set( _SET_PATH, <(pth)> )
#command SET PATH TO                   => Set( _SET_PATH, "" )

#command SET EXACT <x:ON,OFF,&>        => Set( _SET_EXACT, <(x)> )
#command SET EXACT (<x>)               => Set( _SET_EXACT, <x> )
#command SET FIXED <x:ON,OFF,&>        => Set( _SET_FIXED, <(x)> )
#command SET FIXED (<x>)               => Set( _SET_FIXED, <x> )
#command SET EXCLUSIVE <x:ON,OFF,&>    => Set( _SET_EXCLUSIVE, <(x)> )
#command SET EXCLUSIVE (<x>)           => Set( _SET_EXCLUSIVE, <x> )
#command SET SOFTSEEK <x:ON,OFF,&>     => Set( _SET_SOFTSEEK, <(x)> )
#command SET SOFTSEEK (<x>)            => Set( _SET_SOFTSEEK, <x> )
#command SET UNIQUE <x:ON,OFF,&>       => Set( _SET_UNIQUE, <(x)> )
#command SET UNIQUE (<x>)              => Set( _SET_UNIQUE, <x> )
#command SET DELETED <x:ON,OFF,&>      => Set( _SET_DELETED, <(x)> )
#command SET DELETED (<x>)             => Set( _SET_DELETED, <x> )
#command SET CONSOLE <x:ON,OFF,&>      => Set( _SET_CONSOLE, <(x)> )
#command SET CONSOLE (<x>)             => Set( _SET_CONSOLE, <x> )
#command SET BELL <x:ON,OFF,&>         => Set( _SET_BELL, <(x)> )
#command SET BELL (<x>)                => Set( _SET_BELL, <x> )
#command SET CONFIRM <x:ON,OFF,&>      => Set( _SET_CONFIRM, <(x)> )
#command SET CONFIRM (<x>)             => Set( _SET_CONFIRM, <x> )
#command SET ESCAPE <x:ON,OFF,&>       => Set( _SET_ESCAPE, <(x)> )
#command SET ESCAPE (<x>)              => Set( _SET_ESCAPE, <x> )
#command SET INTENSITY <x:ON,OFF,&>    => Set( _SET_INTENSITY, <(x)> )
#command SET INTENSITY (<x>)           => Set( _SET_INTENSITY, <x> )
#command SET SCOREBOARD <x:ON,OFF,&>   => Set( _SET_SCOREBOARD, <(x)> )
#command SET SCOREBOARD (<x>)          => Set( _SET_SCOREBOARD, <x> )
#command SET WRAP <x:ON,OFF,&>         => Set( _SET_WRAP, <(x)> )
#command SET WRAP (<x>)                => Set( _SET_WRAP, <x> )
#command SET DELIMITERS <x:ON,OFF,&>   => Set( _SET_DELIMITERS, <(x)> )
#command SET DELIMITERS (<x>)          => Set( _SET_DELIMITERS, <x> )
#command SET DELIMITERS TO <c>         => Set( _SET_DELIMCHARS, <c> )
#command SET DELIMITERS TO [DEFAULT]   => Set( _SET_DELIMCHARS, "::" )
#command SET ALTERNATE <x:ON,OFF,&>    => Set( _SET_ALTERNATE, <(x)> )
#command SET ALTERNATE (<x>)           => Set( _SET_ALTERNATE, <x> )
#command SET ALTERNATE TO              => Set( _SET_ALTFILE, "" )
#command SET ALTERNATE TO <(file)> [<add: ADDITIVE>] => ;
               Set( _SET_ALTFILE, <(file)>, <.add.> )
#command SET PRINTER <x:ON,OFF,&>      => Set( _SET_PRINTER, <(x)> )
#command SET PRINTER (<x>)             => Set( _SET_PRINTER, <x> )
#command SET PRINTER TO                => Set( _SET_PRINTFILE, "" )
#command SET PRINTER TO <(file)> [<add: ADDITIVE>]    => ;
               Set( _SET_PRINTFILE, <(file)>, <.add.> )
#command SET CURSOR <x:ON,OFF,&>       => ;
               SetCursor( if(Upper(<(x)>) == "ON", 1, 0) )
#command SET CURSOR (<x>)              => SetCursor( if(<x>, 1, 0) )
#command SET MARGIN TO <x>             => Set( _SET_MARGIN, <x> )
#command SET MARGIN TO                 => Set( _SET_MARGIN, 0 )
#command SET DEVICE TO SCREEN          => Set( _SET_DEVICE, "SCREEN" )
#command SET DEVICE TO PRINTER         => Set( _SET_DEVICE, "PRINTER" )
#command SET COLOR TO [<*c*>]          => SetColor( #<c> )
#command SET COLOR TO ( <c> )          => SetColor( <c> )
#command SET COLOUR TO [<*c*>]         => SET COLOR TO [<c>]
#command SET ORDER TO <tg> [IN <(bg)>] => ordSetFocus( <tg> [, <(bg)>] )
#command SET ORDER TO TAG <(tg)> [IN <(bg)>] => ordSetFocus( <(tg)> [, <(bg)>] )
#command SET INDEX TO [<(i1)> [,<(iN)>]] [<add: ADDITIVE>] => ;
         if !<.add.> ; ordListClear() ; end ;
         [ ; ordListAdd( <(i1)> )] [ ; ordListAdd( <(iN)> )]
#command SET MESSAGE TO <n> [<cent: CENTER, CENTRE>] => ;
            Set( _SET_MESSAGE, <n> ) ; Set( _SET_MCENTER, <.cent.> )
#command SET MESSAGE TO                => Set( _SET_MESSAGE, 0 ) ;;
                                          Set( _SET_MCENTER, .f. )
#command SET TYPEAHEAD TO <x>          => Set( _SET_TYPEAHEAD, <x> )
#command SET KEY <n> TO <f>            => SetKey( <n>, {|p, l, v| <f>(p, l, v)} )
#command SET KEY <n> TO <f>([<p,...>]) => SET KEY <n> TO <f>
#command SET KEY <n> TO <f:&>          => ;
         if ( Empty(<(f)>) ) ; SetKey( <n>, NIL ) ; else ;;
            SetKey( <n>, {|p, l, v| <f>(p, l, v)} ) ; end
#command SET KEY <n> [TO]              => SetKey( <n>, NIL )
#command SET FUNCTION <n> [TO] [<f>]   => __SetFunction( <n>, <f> )

#ifdef HB_COMPAT_C53
   #command SET EVENTMASK TO <x>        => Set( _SET_EVENTMASK, <x> )
   #command SET VIDEOMODE TO <x>        => Set( _SET_VIDEOMODE, <x> )
   #command SET SCOPETOP TO             => ordScope( 0, nil )
   #command SET SCOPETOP TO <x>         => ordScope( 0, <x> )
   #command SET SCOPEBOTTOM TO          => ordScope( 1, nil )
   #command SET SCOPEBOTTOM TO <x>      => ordScope( 1, <x> )
   #command SET SCOPE TO                => ordScope( 0, nil ) ; ordScope( 1, nil )
   #command SET SCOPE TO <x>, <y>       => ordScope( 0, <x> ) ; ordScope( 1, <y> )
   #command SET SCOPE TO <x>            => ordScope( 0, <x> ) ; ordScope( 1, <x> )
   #command SET SCOPE TO , <x>          => ordScope( 1, <x> )
   #command SET ORDER TO                => ordSetFocus( 0 )
   #command SET DESCENDING ON           => ordDescend( ,, .T. )
   #command SET DESCENDING OFF          => ordDescend( ,, .F. )
   #command SET AUTORDER TO             => Set( _SET_AUTORDER, 0 )
   #command SET AUTORDER TO <x>         => Set( _SET_AUTORDER, <x> )
   #command SET AUTOSHARE TO            => Set( _SET_AUTOSHARE, 0 )
   #command SET AUTOSHARE TO <x>        => Set( _SET_AUTOSHARE, <x> )
   #command SET MBLOCKSIZE TO <x>       => Set( _SET_MBLOCKSIZE, <x> )
   #command SET MEMOBLOCK TO <x>        => Set( _SET_MBLOCKSIZE, <x> )
   #command SET MFILEEXT TO <x>         => Set( _SET_MFILEEXT, <x> )
   #command SET STRICTREAD <x:ON,OFF,&> => Set( _SET_STRICTREAD, <(x)> )
   #command SET STRICTREAD (<x>)        => Set( _SET_STRICTREAD, <x> )
   #command SET OPTIMIZE <x:ON,OFF,&>   => Set( _SET_OPTIMIZE, <(x)> )
   #command SET OPTIMIZE (<x>)          => Set( _SET_OPTIMIZE, <x> )
   #command SET AUTOPEN <x:ON,OFF,&>    => Set( _SET_AUTOPEN, <(x)> )
   #command SET AUTOPEN (<x>)           => Set( _SET_AUTOPEN, <x> )
#endif

#command ?  [<explist,...>]         => QOut( <explist> )
#command ?? [<explist,...>]         => QQOut( <explist> )
#command EJECT                      => __Eject()
#command TEXT                       => text QOut, QQOut
#command TEXT TO FILE <(f)>         => __TextSave( <(f)> ) ;;
                                       text QOut, __TextRestore
#command TEXT TO PRINTER            => __TextSave( "PRINTER" ) ;;
                                       text QOut, __TextRestore
#ifdef HB_COMPAT_C53
   #xcommand TEXTBLOCK <*cText*>    =>
#endif

#command CLS                        => Scroll() ; SetPos(0,0)
#command CLEAR SCREEN               => CLS
#command SAVE SCREEN                => __XSaveScreen()
#command RESTORE SCREEN             => __XRestScreen()
#command SAVE SCREEN TO <v>         => <v> := SaveScreen( 0, 0, Maxrow(), Maxcol() )
#command RESTORE SCREEN FROM <v>    => RestScreen( 0, 0, Maxrow(), Maxcol(), <v> )

#command @ <row>, <col>             => Scroll( <row>, <col>, <row> ) ;;
                                       SetPos( <row>, <col> )
#command @ <top>, <left> CLEAR      => Scroll( <top>, <left> ) ;;
                                       SetPos( <top>, <left> )
#command @ <top>, <left> CLEAR TO <bottom>, <right>   => ;
         Scroll( <top>, <left>, <bottom>, <right> ) ; SetPos( <top>, <left> )
#command @ <top>, <left>, <bottom>, <right> BOX <string> [COLOR <clr>] => ;
         DispBox( <top>, <left>, <bottom>, <right>, <string> [, <clr>] )
#command @ <top>, <left> TO <bottom>, <right> [DOUBLE] [COLOR <clr>] => ;
         DispBox( <top>, <left>, <bottom>, <right>, 2 [, <clr>] )
#command @ <top>, <left> TO <bottom>, <right> [COLOR <clr>] => ;
         DispBox( <top>, <left>, <bottom>, <right>, 1 [, <clr>] )
#command @ <row>, <col> SAY <exp> [PICTURE <pic>] [COLOR <clr>] => ;
         DevPos( <row>, <col> ) ; DevOutPict( <exp>, <pic> [, <clr>] )
#command @ <row>, <col> SAY <exp> [COLOR <clr>] => ;
         DevPos( <row>, <col> ) ; DevOut( <exp> [, <clr>] )
#command @ <row>, <col> PROMPT <prompt> [MESSAGE <msg>] => ;
         __AtPrompt( <row>, <col>, <prompt> , <msg> )
#command MENU TO <v> => ;
         <v> := __MenuTo( {|_1| if(PCount() == 0, <v>, <v> := _1)}, #<v> )

#command WAIT [<msg>]                  => __Wait( <msg> )
#command WAIT [<msg>] TO <v>           => <v> := __Wait( <msg> )
#command ACCEPT [<msg>] TO <v>         => <v> := __Accept( <msg> )
#command INPUT [<msg>] TO <v>          => ;
         if ( !Empty(__Accept(<msg>)) ) ; <v> := &( __AcceptStr() ) ; end
#command KEYBOARD <x>                  => __Keyboard( <x> )
#command CLEAR TYPEAHEAD               => __Keyboard()
#command CLEAR MEMORY                  => __MVClear()
#command RELEASE <v,...>               => __MVXRelease( <"v"> )
#command RELEASE ALL                   => __MVRelease( "*", .t. )
#command RELEASE ALL LIKE <p>          => __MVRelease( #<p>, .t. )
#command RELEASE ALL EXCEPT <p>        => __MVRelease( #<p>, .f. )
#command RESTORE [FROM <(f)>] [<a:ADDITIVE>] => __MVRestore( <(f)>, <.a.> )
#command SAVE TO <(f)> ALL LIKE <p>    => __MVSave( <(f)>, <(p)>, .t. )
#command SAVE ALL LIKE <p> TO <(f)>    => __MVSave( <(f)>, <(p)>, .t. )
#command SAVE ALL EXCEPT <p> TO <(f)>  => __MVSave( <(f)>, <(p)>, .f. )
#command SAVE TO <(f)> ALL EXCEPT <p>  => __MVSave( <(f)>, <(p)>, .f. )
#command SAVE [TO <(f)>] [ALL]         => __MVSave( <(f)>, "*", .t. )

#command RESTORE HBV [FROM <(f)>] [<a:ADDITIVE>] => hb_MVRestore( <(f)>, <.a.> )
#command SAVE HBV TO <(f)> ALL LIKE <p>    => hb_MVSave( <(f)>, <(p)>, .t. )
#command SAVE HBV ALL LIKE <p> TO <(f)>    => hb_MVSave( <(f)>, <(p)>, .t. )
#command SAVE HBV ALL EXCEPT <p> TO <(f)>  => hb_MVSave( <(f)>, <(p)>, .f. )
#command SAVE HBV TO <(f)> ALL EXCEPT <p>  => hb_MVSave( <(f)>, <(p)>, .f. )
#command SAVE HBV [TO <(f)>] [ALL]         => hb_MVSave( <(f)>, "*", .t. )

#command ERASE <(f)>                   => FErase( <(f)> )
#command DELETE FILE <(f)>             => FErase( <(f)> )
#command RENAME <(old)> TO <(new)>     => FRename( <(old)>, <(new)> )
#command COPY FILE <(src)> TO <(dst)>  => __CopyFile( <(src)>, <(dst)> )
#command DIR [<(mask)>]                => __Dir( <(mask)> )
#command TYPE <(f)> [<prn:TO PRINTER>] [TO FILE <(dst)>] => ;
         __TypeFile( <(f)>, <.prn.> ) [ ; COPY FILE <(f)> TO <(dst)> ]
#command TYPE <(f)> [<prn:TO PRINTER>] => __TypeFile( <(f)>, <.prn.> )

#command REQUEST <func,...>      => EXTERNAL <func>
#command CANCEL                  => __Quit()
#command QUIT                    => __Quit()
#command RUN <*cmd*>             => __Run( #<cmd> )
#command RUN ( <cmd> )           => __Run( <cmd> )
#command ! <*cmd*>               => RUN <cmd>
#command RUN = <xpr>             => ( run := <xpr> )
#command RUN := <xpr>            => ( run := <xpr> )

#command CLOSE <a>               => <a>->( dbCloseArea() )
#command CLOSE                   => dbCloseArea()
#command CLOSE DATABASES         => dbCloseAll()
#command CLOSE ALTERNATE         => Set(_SET_ALTFILE, "")
#command CLOSE FORMAT            => __SetFormat(NIL)
#command CLOSE INDEXES           => dbClearIndex()
#command CLOSE PROCEDURE         =>
#command CLOSE ALL               => CLOSE DATABASES ; SELECT 1 ; CLOSE FORMAT
#command CLEAR                   => CLEAR SCREEN ; CLEAR GETS
#command CLEAR ALL               => CLOSE DATABASES ; CLOSE FORMAT ;;
                                    CLEAR MEMORY ; CLEAR GETS ;;
                                    SET ALTERNATE OFF ; SET ALTERNATE TO

#command SELECT <area>           => dbSelectArea( <(area)> )
#command SELECT <f>([<p,...>])   => dbSelectArea( <f>(<p>) )
#command USE                     => dbCloseArea()
#command USE <(db)> [VIA <rdd>] [ALIAS <a>] [<nw: NEW>] ;
            [<ex: EXCLUSIVE>] [<sh: SHARED>] [<ro: READONLY>] ;
            [CODEPAGE <cp>] [INDEX <(index1)> [, <(indexN)>]] => ;
         dbUseArea( <.nw.>, <rdd>, <(db)>, <(a)>, ;
                    if(<.sh.> .or. <.ex.>, !<.ex.>, NIL), <.ro.> [, <cp>] ) ;
         [; dbSetIndex( <(index1)> )] ;
         [; dbSetIndex( <(indexN)> )]

#command APPEND BLANK            => dbAppend()
#command PACK                    => __dbPack()
#command ZAP                     => __dbZap()
#command UNLOCK                  => dbUnlock()
#command UNLOCK ALL              => dbUnlockAll()
#command COMMIT                  => dbCommitAll()
#command GOTO <x>                => dbGoto(<x>)
#command GO <x>                  => dbGoto(<x>)
#command GOTO TOP                => dbGoTop()
#command GO TOP                  => dbGoTop()
#command GOTO BOTTOM             => dbGoBottom()
#command GO BOTTOM               => dbGoBottom()
#command SKIP                    => dbSkip(1)
#command SKIP <x>                => dbSkip( <x> )
#command SKIP ALIAS <a>          => <a>->( dbSkip(1) )
#command SKIP <x> ALIAS <a>      => <a>->( dbSkip(<x>) )
#command FIND <*text*>           => dbSeek( <(text)> )
#command FIND := <xpr>           => ( find := <xpr> )
#command FIND = <xpr>            => ( find := <xpr> )
#command CONTINUE                => __dbContinue()
#ifdef HB_COMPAT_C53
   #command SEEK <exp> [<soft: SOFTSEEK>] [<last: LAST>] => ;
            dbSeek( <exp>, if( <.soft.>, .T., NIL ), if( <.last.>, .T., NIL ) )
#else
   #command SEEK <exp> [<soft: SOFTSEEK>] => ;
            dbSeek( <exp>, if( <.soft.>, .T., NIL ) )
#endif
#command LOCATE [FOR <for>] [WHILE <while>] [NEXT <next>] ;
                [RECORD <rec>] [<rest:REST>] [ALL] => ;
         __dbLocate( <{for}>, <{while}>, <next>, <rec>, <.rest.> )

#command SET RELATION TO         => dbClearRelation()
#command SET RELATION [<add:ADDITIVE>] ;
               [TO  <exp1> INTO <(alias1)> [<scp1:SCOPED>];
            [, [TO] <expN> INTO <(aliasN)> [<scpN:SCOPED>]]] => ;
         if ( !<.add.> ) ; dbClearRelation() ; end ;
           ; dbSetRelation( <(alias1)>, <{exp1}>, <"exp1">, <.scp1.> ) ;
         [ ; dbSetRelation( <(aliasN)>, <{expN}>, <"expN">, <.scpN.> )]

#command SET FILTER TO           => dbClearFilter(NIL)
#command SET FILTER TO <exp>     => dbSetFilter( <{exp}>, <"exp"> )
#command SET FILTER TO <x:&>     => if ( Empty(<(x)>) ) ; dbClearFilter() ;;
                                    else ; dbSetFilter( <{x}>, <(x)> ) ; end
#command REPLACE [ <f1> WITH <x1> [, <fN> WITH <xN>] ] ;
                 [FOR <for>] [WHILE <while>] [NEXT <next>] ;
                 [RECORD <rec>] [<rest:REST>] [ALL] => ;
         DBEval( {|| _FIELD-><f1> := <x1> [, _FIELD-><fN> := <xN>]}, ;
                 <{for}>, <{while}>, <next>, <rec>, <.rest.> )
#command REPLACE <f1> WITH <v1> [, <fN> WITH <vN> ] => ;
         _FIELD-><f1> := <v1> [; _FIELD-><fN> := <vN>]

#command DELETE [FOR <for>] [WHILE <while>] [NEXT <next>] ;
                [RECORD <rec>] [<rest:REST>] [ALL] => ;
         DBEval( {|| dbDelete()}, <{for}>, <{while}>, <next>, <rec>, <.rest.> )
#command DELETE                  =>  dbDelete()

#command RECALL [FOR <for>] [WHILE <while>] [NEXT <next>] ;
                [RECORD <rec>] [<rest:REST>] [ALL] => ;
         DBEval( {|| dbRecall()}, <{for}>, <{while}>, <next>, <rec>, <.rest.> )
#command RECALL                  =>  dbRecall()

#command CREATE <(db)> [FROM <(src)>] [VIA <rdd>] [ALIAS <a>] ;
                [<new: NEW>] [CODEPAGE <cp>] => ;
         __dbCreate( <(db)>, <(src)>, <rdd>, <.new.>, <(a)> [, <cp>] )

#command COPY [STRUCTURE] [EXTENDED] [TO <(f)>] => __dbCopyXStruct( <(f)> )
#command COPY [STRUCTURE] [TO <(f)>] [FIELDS <fields,...>] => ;
         __dbCopyStruct( <(f)>, { <(fields)> } )

#command COPY [TO <(f)>] [FIELDS <fields,...>] ;
              [FOR <for>] [WHILE <while>] [NEXT <next>] ;
              [RECORD <rec>] [<rest:REST>] [ALL] ;
              [DELIMITED [WITH <*delim*>]] [CODEPAGE <cp>] => ;
         __dbDelim( .T., <(f)>, <(delim)>, { <(fields)> }, ;
                    <{for}>, <{while}>, <next>, <rec>, <.rest.>, <cp> )

#command COPY [TO <(f)>] [SDF] [FIELDS <fields,...>] ;
              [FOR <for>] [WHILE <while>] [NEXT <next>] ;
              [RECORD <rec>] [<rest:REST>] [ALL] [CODEPAGE <cp>] => ;
         __dbSDF( .T., <(f)>, { <(fields)> }, ;
                  <{for}>, <{while}>, <next>, <rec>, <.rest.>, <cp> )

#command COPY [TO <(f)>] [FIELDS <fields,...>] ;
              [FOR <for>] [WHILE <while>] [NEXT <next>] ;
              [RECORD <rec>] [<rest:REST>] [ALL] [VIA <rdd>] [CODEPAGE <cp>] => ;
         __dbCopy( <(f)>, { <(fields)> }, ;
                   <{for}>, <{while}>, <next>, <rec>, <.rest.>, <rdd>, , <cp> )

#command APPEND [FROM <(f)>] [FIELDS <fields,...>] ;
                [FOR <for>] [WHILE <while>] [NEXT <next>] ;
                [RECORD <rec>] [<rest:REST>] [ALL] ;
                [DELIMITED [WITH <*delim*>]] [CODEPAGE <cp>] => ;
         __dbDelim( .F., <(f)>, <(delim)>, { <(fields)> }, ;
                    <{for}>, <{while}>, <next>, <rec>, <.rest.>, <cp> )

#command APPEND [FROM <(f)>] [SDF] [FIELDS <fields,...>] ;
                [FOR <for>] [WHILE <while>] [NEXT <next>] ;
                [RECORD <rec>] [<rest:REST>] [ALL] [CODEPAGE <cp>] => ;
         __dbSDF( .F., <(f)>, { <(fields)> }, ;
                  <{for}>, <{while}>, <next>, <rec>, <.rest.>, <cp> )

#command APPEND [FROM <(f)>] [FIELDS <fields,...>] ;
                [FOR <for>] [WHILE <while>] [NEXT <next>] ;
                [RECORD <rec>] [<rest:REST>] [ALL] [VIA <rdd>] ;
                [CODEPAGE <cp>] => ;
         __dbApp( <(f)>, { <(fields)> }, ;
                  <{for}>, <{while}>, <next>, <rec>, <.rest.>, <rdd>, , <cp> )

#command SORT [TO <(f)>] [ON <fields,...>] ;
              [FOR <for>] [WHILE <while>] [NEXT <next>] ;
              [RECORD <rec>] [<rest:REST>] [ALL] [CODEPAGE <cp>] => ;
         __dbSort( <(f)>, { <(fields)> }, ;
                   <{for}>, <{while}>, <next>, <rec>, <.rest.>, , , <cp> )

#command TOTAL [TO <(f)>] [ON <key>] [FIELDS <fields,...>] ;
               [FOR <for>] [WHILE <while>] [NEXT <next>] ;
               [RECORD <rec>] [<rest:REST>] [ALL] [CODEPAGE <cp>] => ;
         __dbTotal( <(f)>, <{key}>, { <(fields)> }, ;
                    <{for}>, <{while}>, <next>, <rec>, <.rest.>, , , <cp> )

#command UPDATE [FROM <(alias)>] [ON <key>] [<rand:RANDOM>] ;
                [REPLACE <f1> WITH <x1> [, <fN> WITH <xN>]] => ;
         __dbUpdate( <(alias)>, <{key}>, <.rand.>, ;
                     {|| _FIELD-><f1> := <x1> [, _FIELD-><fN> := <xN>]} )

#command JOIN [WITH <(alias)>] [TO <f>] [FIELDS <fields,...>] [FOR <for>] => ;
         __dbJoin( <(alias)>, <(f)>, { <(fields)> }, ;
                   if(EMPTY(#<for>), {|| .T. }, <{for}> ) )

#command COUNT [TO <v>] ;
               [FOR <for>] [WHILE <while>] [NEXT <next>] ;
               [RECORD <rec>] [<rest:REST>] [ALL] => ;
         <v> := 0 ; DBEval( {|| <v> := <v> + 1}, ;
                            <{for}>, <{while}>, <next>, <rec>, <.rest.> )

#command SUM [ <x1> [, <xN>]  TO  <v1> [, <vN>] ] ;
             [FOR <for>] [WHILE <while>] [NEXT <next>] ;
             [RECORD <rec>] [<rest:REST>] [ALL] => ;
         <v1> := [ <vN> := ] 0 ;;
         DBEval( {|| <v1> := <v1> + <x1> [, <vN> := <vN> + <xN> ]}, ;
                 <{for}>, <{while}>, <next>, <rec>, <.rest.> )

#command AVERAGE [ <x1> [, <xN>]  TO  <v1> [, <vN>] ] ;
                 [FOR <for>] [WHILE <while>] [NEXT <next>] ;
                 [RECORD <rec>] [<rest:REST>] [ALL] => ;
         M->__Avg := <v1> := [ <vN> := ] 0 ;;
         DBEval( {|| M->__Avg := M->__Avg + 1, ;
                 <v1> := <v1> + <x1> [, <vN> := <vN> + <xN>] }, ;
                 <{for}>, <{while}>, <next>, <rec>, <.rest.> ) ;;
         <v1> := <v1> / M->__Avg [ ; <vN> := <vN> / M->__Avg ]

#command LIST [<v,...>] [<off:OFF>] [<prn:TO PRINTER>] [TO FILE <(f)>] ;
              [FOR <for>] [WHILE <while>] [NEXT <next>] ;
              [RECORD <rec>] [<rest:REST>] [ALL] => ;
         __dbList( <.off.>, { <{v}> }, .t., ;
                   <{for}>, <{while}>, <next>, <rec>, <.rest.>, <.prn.>, <(f)> )

#command DISPLAY [<v,...>] [<off:OFF>] [<prn:TO PRINTER>] [TO FILE <(f)>] ;
                 [FOR <for>] [WHILE <while>] [NEXT <next>] ;
                 [RECORD <rec>] [<rest:REST>] [<all:ALL>] => ;
         __dbList( <.off.>, { <{v}> }, <.all.>, ;
                   <{for}>, <{while}>, <next>, <rec>, <.rest.>, <.prn.>, <(f)> )

#command REPORT FORM <frm> [HEADING <head>] [<plain:PLAIN>] [<noej:NOEJECT>] ;
                           [<sum:SUMMARY>] [<nocon:NOCONSOLE>] ;
                           [<prn:TO PRINTER>] [TO FILE <(f)>] ;
                           [FOR <for>] [WHILE <while>] [NEXT <next>] ;
                           [RECORD <rec>] [<rest:REST>] [ALL] => ;
         __ReportForm( <(frm)>, <.prn.>, <(f)>, <.nocon.>, ;
                       <{for}>, <{while}>, <next>, <rec>, <.rest.>, ;
                       <.plain.>, <head>, <.noej.>, <.sum.> )

#command LABEL FORM <lbl> [<smp:SAMPLE>] [<nocon:NOCONSOLE>] ;
                          [<prn:TO PRINTER>] [TO FILE <(f)>] ;
                          [FOR <for>] [WHILE <while>] [NEXT <next>] ;
                          [RECORD <rec>] [<rest:REST>] [ALL] => ;
         __LabelForm( <(lbl)>, <.prn.>, <(f)>, <.nocon.>, ;
                      <{for}>, <{while}>, <next>, <rec>, <.rest.>, <.smp.> )

#command DELETE TAG <(tag1)> [IN <(bag1)>] [, <(tagN)> [IN <(bagN)>]] => ;
         ordDestroy( <(tag1)>, <(bag1)> )[ ; ordDestroy( <(tagN)>, <(bagN)> ) ]

#command INDEX ON <key> [TAG <(tag)>] TO <(bag)> ;
               [FOR <for>] [WHILE <while>] [NEXT <next>] ;
               [RECORD <rec>] [<rest:REST>] [<all:ALL>] ;
               [EVAL <eval>] [EVERY <every>] [<unique: UNIQUE>] ;
               [<ascend: ASCENDING>] [<descend: DESCENDING>] ;
               [<add: ADDITIVE>] [<cur: USECURRENT>] [<cust: CUSTOM>] ;
               [<noopt: NOOPTIMIZE>] [<mem: MEMORY, TEMPORARY>] ;
               [<filter: USEFILTER>] [<ex: EXCLUSIVE>] => ;
         ordCondSet( <"for">, <{for}>, [<.all.>], <{while}>, ;
                     <{eval}>, <every>, RECNO(), <next>, <rec>, ;
                     [<.rest.>], [<.descend.>],, ;
                     [<.add.>], [<.cur.>], [<.cust.>], [<.noopt.>], ;
                     <"while">, [<.mem.>], [<.filter.>], [<.ex.>] ) ;;
         ordCreate( <(bag)>, <(tag)>, <"key">, <{key}>, [<.unique.>] )

#command INDEX ON <key> TAG <(tag)> [TO <(bag)>] ;
               [FOR <for>] [WHILE <while>] [NEXT <next>] ;
               [RECORD <rec>] [<rest:REST>] [<all:ALL>] ;
               [EVAL <eval>] [EVERY <every>] [<unique: UNIQUE>] ;
               [<ascend: ASCENDING>] [<descend: DESCENDING>] ;
               [<add: ADDITIVE>] [<cur: USECURRENT>] [<cust: CUSTOM>] ;
               [<noopt: NOOPTIMIZE>] [<mem: MEMORY, TEMPORARY>] ;
               [<filter: USEFILTER>] [<ex: EXCLUSIVE>] => ;
         ordCondSet( <"for">, <{for}>, [<.all.>], <{while}>, ;
                     <{eval}>, <every>, RECNO(), <next>, <rec>, ;
                     [<.rest.>], [<.descend.>],, ;
                     [<.add.>], [<.cur.>], [<.cust.>], [<.noopt.>], ;
                     <"while">, [<.mem.>], [<.filter.>], [<.ex.>] ) ;;
         ordCreate( <(bag)>, <(tag)>, <"key">, <{key}>, [<.unique.>] )

#command INDEX ON <key> TO <(file)> [<u: UNIQUE>] => ;
            dbCreateIndex( <(file)>, <"key">, <{key}>, if( <.u.>, .t., NIL ) )

#command REINDEX [EVAL <eval>] [EVERY <every>] [<lNoOpt: NOOPTIMIZE>] => ;
            ordCondSet(,,,, <{eval}>, <every>,,,,,,,,,, <.lNoOpt.>) ;;
            ordListRebuild()
#command REINDEX        => ordListRebuild()


#command READ           => ReadModal(GetList) ; GetList := {} ; ( GetList )
#command READ SAVE      => ReadModal(GetList)
#command CLEAR GETS     => ReadKill(.T.) ; GetList := {} ; ( GetList )

#xcommand @ [<exp,...>] COLOUR [<nextexp,...>] => @ [ <exp>] COLOR [ <nextexp>]

#command @ <row>, <col> SAY <say> [<sayexp,...>] GET <get> [<getexp,...>] => ;
         @ <row>, <col> SAY <say> [ <sayexp>] ;;
         @ Row(), Col() + 1 GET <get> [ <getexp>]

#command @ <row>, <col> GET <v> [<exp,...>] RANGE <l>, <h> [<nextexp,...>] => ;
         @ <row>, <col> GET <v> [ <exp>] ;
                        VALID {|_1| RangeCheck( _1, , <l>, <h> ) } [ <nextexp>]

#command @ <row>, <col> GET <v> [<exp,...>] COLOR <clr> [<nextexp,...>] => ;
         @ <row>, <col> GET <v> [ <exp>] SEND colorDisp(<clr>) [ <nextexp>]

#ifdef HB_COMPAT_C53

   #command READ [MENU <oMenu>] ;
                 [MSG AT <row>, <left>, <right> [MSG COLOR <color>]] => ;
         ReadModal( GetList, NIL, <oMenu>, <row>, <left>, <right>, <color> ) ;;
         GetList := {} ; ( GetList )

   #command READ SAVE [MENU <oMenu>] ;
                      [MSG AT <row>, <left>, <right> [MSG COLOR <color>]] => ;
         ReadModal( GetList, NIL, <oMenu>, <row>, <left>, <right>, <color> )

   #command @ <row>, <col> GET <v> [PICTURE <pic>] ;
                           [VALID <valid>] [WHEN <when>] [SEND <snd>] ;
                           [CAPTION <cap>] [MESSAGE <msg>] => ;
         SetPos( <row>, <col> ) ;;
         AAdd( GetList, _GET_( <v>, <"v">, <pic>, <{valid}>, <{when}> ) ) ;;
       [ ATail(GetList):Caption := <cap> ;;
         ATail(GetList):CapRow := ATail(Getlist):row ;;
         ATail(GetList):CapCol := ATail(Getlist):col - __CapLength(<cap>) - 1 ;] ;
       [ ATail(GetList):message := <msg> ;] [ ATail(GetList):<snd> ;] ;
         ATail(GetList):Display()

   #command @ <row>, <col> GET <v> CHECKBOX [VALID <valid>] [WHEN <when>] ;
                           [CAPTION <cap>] [MESSAGE <msg>] [COLOR <clr>] ;
                           [FOCUS <fb>] [STATE <sb>] [STYLE <stl>] ;
                           [SEND <snd>] [GUISEND <gsnd>] [BITMAPS <bmaps>] => ;
         SetPos( <row>, <col> ) ;;
         AAdd( GetList, _GET_( <v>, <(v)>, NIL, <{valid}>, <{when}> ) ) ;;
         ATail(GetList):Control := _CheckBox_( <v>, <cap>, ;
                     <msg>, <clr>, <{fb}>, <{sb}>, <stl>, <bmaps> ) ;;
         ATail(GetList):reader := {| a, b, c, d | GuiReader( a, b, c, d ) } ;;
       [ ATail(GetList):<snd> ;] [ ATail(GetList):Control:<gsnd> ;] ;
         ATail(GetList):Control:Display()

   #command @ <top>, <left>, <bottom>, <right> GET <v> LISTBOX <items> ;
                  [VALID <valid>] [WHEN <when>] ;
                  [CAPTION <cap>] [MESSAGE <msg>] [COLOR <clr>] ;
                  [FOCUS <fb>] [STATE <sb>] [<dd:DROPDOWN>] [<sbar:SCROLLBAR>] ;
                  [SEND <snd>] [GUISEND <gsnd>] [BITMAP <bmap>] => ;
         SetPos( <top>, <left> ) ;;
         AAdd( GetList, _GET_( <v>, <(v)>, NIL, <{valid}>, <{when}> ) ) ;;
         ATail(GetList):Control := _ListBox_( ATail(Getlist):row, ;
               ATail(Getlist):col, <bottom>, <right>, <v>, <items>, <cap>, ;
               <msg>, <clr>, <{fb}>, <{sb}>, <.dd.>, <.sbar.>, <bmap> ) ;;
         ATail(GetList):reader := {| a, b, c, d | GuiReader( a, b, c, d ) } ;;
       [ ATail(GetList):<snd> ;] [ ATail(GetList):Control:<gsnd> ;] ;
         ATail(GetList):Control:Display()

   #command @ <row>, <col> GET <v> PUSHBUTTON ;
                  [VALID <valid>] [WHEN <when>] ;
                  [CAPTION <cap>] [MESSAGE <msg>] [COLOR <clr>] ;
                  [FOCUS <fb>] [STATE <sb>] [STYLE <stl>] ;
                  [SEND <snd>] [GUISEND <gsnd>] [BITMAP <bmap>] ;
                  [SIZE X <sX> Y <sY>] [CAPOFF X <cX> Y <cY> ] ;
                  [BMPOFF X <bX> Y <bY>] => ;
         SetPos( <row>, <col> ) ;;
         AAdd( GetList, _GET_( <v>, <(v)>, NIL, <{valid}>, <{when}> ) ) ;;
         ATail(GetList):Control := _PushButt_( <cap>, <msg>, <clr>, <{fb}>,;
               <{sb}>, <stl>, <sX>, <sY>, <cX>, <cY>, <bmap>, <bX>, <bY> ) ;;
         ATail(GetList):reader := {| a, b, c, d | GuiReader( a, b, c, d ) } ;;
       [ ATail(GetList):<snd> ;] [ ATail(GetList):Control:<gsnd> ;] ;
         ATail(GetList):Control:Display()

   #command @ <top>, <left>, <bottom>, <right> GET <v> RADIOGROUP <buttons> ;
                  [VALID <valid>] [WHEN <when>] ;
                  [CAPTION <cap>] [MESSAGE <msg>] [COLOR <clr>] ;
                  [FOCUS <fb>] [STYLE <stl>] ;
                  [SEND <snd>] [GUISEND <gsnd>] => ;
         SetPos( <top>, <left> ) ;;
         AAdd( GetList, _GET_( <v>, <(v)>, NIL, <{valid}>, <{when}> ) ) ;;
         ATail(GetList):Control := _RadioGrp_( ATail(Getlist):row, ;
               ATail(Getlist):col, <bottom>, <right>, <v>, <buttons>, <cap>,;
               <msg>, <clr>, <{fb}>, <stl> ) ;;
         ATail(GetList):reader := {| a, b, c, d | GuiReader( a, b, c, d ) } ;;
       [ ATail(GetList):<snd> ;] [ ATail(GetList):Control:<gsnd> ;] ;
         ATail(GetList):Control:Display()

   #command @ <top>, <left>, <bottom>, <right> GET <v> TBROWSE <oBrowse> ;
                  [VALID <valid>] [WHEN <when>] ;
                  [MESSAGE <msg>] [SEND <snd>] [GUISEND <gsnd>] => ;
         SetPos( <top>, <left> ) ;;
         AAdd( GetList, _GET_( <v>, <(v)>, NIL, <{valid}>, <{when}> ) ) ;;
         <oBrowse>:ntop := ATail(Getlist):row ;;
         <oBrowse>:nleft := ATail(Getlist):col ;;
         <oBrowse>:nbottom := <bottom> ; <oBrowse>:nright := <right> ;;
         <oBrowse>:Configure() ; ATail(GetList):Control := <oBrowse> ;;
         ATail(GetList):reader := {| a, b, c, d | TBReader( a, b, c, d ) } ;
     [ ; ATail(GetList):Control:Message := <msg>] ;
     [ ; ATail(GetList):<snd>] [ ; ATail(GetList):Control:<gsnd>]

   #command @ <top>, <left>, <bottom>, <right> GET <v> TBROWSE <oBrowse> ;
                  ALIAS <a> [VALID <valid>] [WHEN <when>] ;
                  [MESSAGE <msg>] [SEND <snd>] [GUISEND <gsnd>] => ;
         SetPos( <top>, <left> ) ;;
         AAdd( GetList, _GET_( <v>, <(v)>, NIL, <{valid}>, <{when}> ) ) ;;
         <oBrowse>:ntop := ATail(Getlist):row ;;
         <oBrowse>:nleft := ATail(Getlist):col ;;
         <oBrowse>:nbottom := <bottom> ; <oBrowse>:nright := <right> ;;
         <oBrowse>:Configure() ; ATail(GetList):Control := <oBrowse> ;;
         ATail(GetList):reader := {| a, b, c, d | <a>->( TBReader( a, b, c, d ) ) } ;
     [ ; ATail(GetList):Control:Message := <msg>] ;
     [ ; ATail(GetList):<snd>] [ ; ATail(GetList):Control:<gsnd>]

#else

   #command @ <row>, <col> GET <v> [PICTURE <pic>] ;
                           [VALID <valid>] [WHEN <when>] [SEND <snd>] => ;
         SetPos( <row>, <col> ) ; AAdd( GetList, ;
            _GET_( <v>, <"v">, <pic>, <{valid}>, <{when}> ):Display() ) ;
      [; ATail(GetList):<snd>]

#endif /* HB_COMPAT_C53 */


/* Harbour extensions */
#command SET FILECASE <(x)>            => Set( _SET_FILECASE, <(x)> )
#command SET FILECASE (<x>)            => Set( _SET_FILECASE, <x> )
#command SET DIRCASE <(x)>             => Set( _SET_DIRCASE, <(x)> )
#command SET DIRCASE (<x>)             => Set( _SET_DIRCASE, <x> )
#command SET DIRSEPARATOR <x>          => Set( _SET_DIRSEPARATOR, <x> )
#command SET DBFLOCKSCHEME TO <x>      => Set( _SET_DBFLOCKSCHEME, <x> )
#command SET DBFLOCKSCHEME TO          => Set( _SET_DBFLOCKSCHEME, 0 )
#command SET HARDCOMMIT <x:ON,OFF,&>   => Set( _SET_HARDCOMMIT, <(x)> )
#command SET HARDCOMMIT (<x>)          => Set( _SET_HARDCOMMIT, <x> )
#command SET EOL <x>                   => Set( _SET_EOL, <x> )

#endif /* HB_STD_CH_ */
