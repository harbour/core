/*
 * $Id$

   Harbour Project source code

   This file contains the tables of predefined #define and #command
   statements of preprocessor.

   Copyright 1999  Alexander S.Kresin <alex@belacy.belgorod.su>
   www - http://www.harbour-project.org

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version, with one exception:

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).
 */

#include <stdio.h>
#include "hbpp.h"

   static DEFINES sD___01 = {"__HARBOUR__",NULL,-1,"", NULL };
   static DEFINES sD___02 =  {"_SET_ALTERNATE",NULL,-1,"1", &sD___01 };
   static DEFINES sD___03 =  {"_SET_ALTFILE",NULL,-1,"2", &sD___02 };
   static DEFINES sD___04 =  {"_SET_BELL",NULL,-1,"3", &sD___03 };
   static DEFINES sD___05 =  {"_SET_CANCEL",NULL,-1,"4", &sD___04 };
   static DEFINES sD___06 =  {"_SET_COLOR",NULL,-1,"5", &sD___05 };
   static DEFINES sD___07 =  {"_SET_CONFIRM",NULL,-1,"6", &sD___06 };
   static DEFINES sD___08 =  {"_SET_CONSOLE",NULL,-1,"7", &sD___07 };
   static DEFINES sD___09 =  {"_SET_CURSOR",NULL,-1,"8", &sD___08 };
   static DEFINES sD___10 =  {"_SET_DATEFORMAT",NULL,-1,"9", &sD___09 };
   static DEFINES sD___11 =  {"_SET_DEBUG",NULL,-1,"10", &sD___10 };
   static DEFINES sD___12 =  {"_SET_DECIMALS",NULL,-1,"11", &sD___11 };
   static DEFINES sD___13 =  {"_SET_DEFAULT",NULL,-1,"12", &sD___12 };
   static DEFINES sD___14 =  {"_SET_DELETED",NULL,-1,"13", &sD___13 };
   static DEFINES sD___15 =  {"_SET_DELIMCHARS",NULL,-1,"14", &sD___14 };
   static DEFINES sD___16 =  {"_SET_DELIMITERS",NULL,-1,"15", &sD___15 };
   static DEFINES sD___17 =  {"_SET_DEVICE",NULL,-1,"16", &sD___16 };
   static DEFINES sD___18 =  {"_SET_EPOCH",NULL,-1,"17", &sD___17 };
   static DEFINES sD___19 =  {"_SET_ESCAPE",NULL,-1,"18", &sD___18 };
   static DEFINES sD___20 =  {"_SET_EVENTMASK",NULL,-1,"19", &sD___19 };
   static DEFINES sD___21 =  {"_SET_EXACT",NULL,-1,"20", &sD___20 };
   static DEFINES sD___22 =  {"_SET_EXCLUSIVE",NULL,-1,"21", &sD___21 };
   static DEFINES sD___23 =  {"_SET_EXIT",NULL,-1,"22", &sD___22 };
   static DEFINES sD___24 =  {"_SET_EXTRA",NULL,-1,"23", &sD___23 };
   static DEFINES sD___25 =  {"_SET_EXTRAFILE",NULL,-1,"24", &sD___24 };
   static DEFINES sD___26 =  {"_SET_FIXED",NULL,-1,"25", &sD___25 };
   static DEFINES sD___27 =  {"_SET_INSERT",NULL,-1,"26", &sD___26 };
   static DEFINES sD___28 =  {"_SET_INTENSITY",NULL,-1,"27", &sD___27 };
   static DEFINES sD___29 =  {"_SET_MARGIN",NULL,-1,"28", &sD___28 };
   static DEFINES sD___30 =  {"_SET_MCENTER",NULL,-1,"29", &sD___29 };
   static DEFINES sD___31 =  {"_SET_MESSAGE",NULL,-1,"30", &sD___30 };
   static DEFINES sD___32 =  {"_SET_PATH",NULL,-1,"31", &sD___31 };
   static DEFINES sD___33 =  {"_SET_PRINTER",NULL,-1,"32", &sD___32 };
   static DEFINES sD___34 =  {"_SET_PRINTFILE",NULL,-1,"33", &sD___33 };
   static DEFINES sD___35 =  {"_SET_SCOREBOARD",NULL,-1,"34", &sD___34 };
   static DEFINES sD___36 =  {"_SET_SCROLLBREAK",NULL,-1,"35", &sD___35 };
   static DEFINES sD___37 =  {"_SET_SOFTSEEK",NULL,-1,"36", &sD___36 };
   static DEFINES sD___38 =  {"_SET_TYPEAHEAD",NULL,-1,"37", &sD___37 };
   static DEFINES sD___39 =  {"_SET_UNIQUE",NULL,-1,"38", &sD___38 };
   static DEFINES sD___40 =  {"_SET_WRAP",NULL,-1,"39", &sD___39 };
   static DEFINES sD___41 =  {"_SET_CH",NULL,-1,NULL, &sD___40 };
   static DEFINES sD___42 =  {"_DFSET","x,y",2,"Set( _SET_DATEFORMAT, if(__SetCentury(), x, y) )", &sD___41 };
   DEFINES *topDefine = &sD___42;

int kolcomm = 240;
COMMANDS aCommands[] =
{
 {0,"NOTE","\1A30",NULL},
 {0,"DO","WHILE \1A00","while \1A00"},
 {0,"END","\1A00","end"},
 {0,"END","SEQUENCE","end"},
 {0,"ENDSEQUENCE","","end"},
 {0,"ENDDO","\1A30","enddo"},
 {0,"ENDIF","\1A30","endif"},
 {0,"ENDCASE","\1A30","endcase"},
 {0,"ENDFOR","[ \1A30 ]","next"},
 {0,"NEXT","\1A00 [TO \1B00] [STEP \1C00]","next"},
 {0,"DO","\1A00.PRG [WITH \1B10]","do \1A00 [ WITH \1B00]"},
 {0,"CALL","\1A00() [WITH \1B10]","call \1A00 [ WITH \1B00]"},
 {0,"STORE","\1A00 TO \1B00 [, \1C00 ]","\1B00 := [ \1C00 := ] \1A00"},
 {0,"SET","ECHO \1A30",NULL},
 {0,"SET","HEADING \1A30",NULL},
 {0,"SET","MENU \1A30",NULL},
 {0,"SET","STATUS \1A30",NULL},
 {0,"SET","STEP \1A30",NULL},
 {0,"SET","SAFETY \1A30",NULL},
 {0,"SET","TALK \1A30",NULL},
 {0,"SET","PROCEDURE TO",NULL},
 {0,"SET","PROCEDURE TO \1A00","_ProcReq_( \1A30 )"},
 {0,"SET","EXACT \1A20ON,OFF,&>","Set( _SET_EXACT, \1A30 )"},
 {0,"SET","EXACT (\1A00)","Set( _SET_EXACT, \1A00 )"},
 {0,"SET","FIXED \1A20ON,OFF,&>","Set( _SET_FIXED, \1A30 )"},
 {0,"SET","FIXED (\1A00)","Set( _SET_FIXED, \1A00 )"},
 {0,"SET","DECIMALS TO \1A00","Set( _SET_DECIMALS, \1A00 )"},
 {0,"SET","DECIMALS TO","Set( _SET_DECIMALS, 0 )"},
 {0,"SET","PATH TO \1A30","Set( _SET_PATH, \1A30 )"},
 {0,"SET","PATH TO","Set( _SET_PATH, \"\" )"},
 {0,"SET","DEFAULT TO \1A40","Set( _SET_DEFAULT, \1A30 )"},
 {0,"SET","DEFAULT TO","Set( _SET_DEFAULT, \"\" )"},
 {0,"SET","CENTURY \1A20ON,OFF,&>","__SetCentury( \1A30 )"},
 {0,"SET","CENTURY (\1A00)","__SetCentury( \1A00 )"},
 {0,"SET","EPOCH TO \1A00","Set( _SET_EPOCH, \1A00 )"},
 {0,"SET","DATE FORMAT [TO] \1A00","Set( _SET_DATEFORMAT, \1A00 )"},
 {0,"SET","DATE [TO] AMERICAN","_DFSET( 'mm/dd/yyyy', 'mm/dd/yy' )"},
 {0,"SET","DATE [TO] ANSI","_DFSET( 'yyyy.mm.dd', 'yy.mm.dd' )"},
 {0,"SET","DATE [TO] BRITISH","_DFSET( 'dd/mm/yyyy', 'dd/mm/yy' )"},
 {0,"SET","DATE [TO] FRENCH","_DFSET( 'dd/mm/yyyy', 'dd/mm/yy' )"},
 {0,"SET","DATE [TO] GERMAN","_DFSET( 'dd.mm.yyyy', 'dd.mm.yy' )"},
 {0,"SET","DATE [TO] ITALIAN","_DFSET( 'dd-mm-yyyy', 'dd-mm-yy' )"},
 {0,"SET","DATE [TO] JAPANESE","_DFSET( 'yyyy/mm/dd', 'yy/mm/dd' )"},
 {0,"SET","DATE [TO] USA","_DFSET( 'mm-dd-yyyy', 'mm-dd-yy' )"},
 {0,"SET","ALTERNATE \1A20ON,OFF,&>","Set( _SET_ALTERNATE, \1A30 )"},
 {0,"SET","ALTERNATE (\1A00)","Set( _SET_ALTERNATE, \1A00 )"},
 {0,"SET","ALTERNATE TO","Set( _SET_ALTFILE, \"\" )"},
 {0,"SET","ALTERNATE TO \1A40 [\1B20 ADDITIVE>]","Set( _SET_ALTFILE, \1A30, \1B50 )"},
 {0,"SET","CONSOLE \1A20ON,OFF,&>","Set( _SET_CONSOLE, \1A30 )"},
 {0,"SET","CONSOLE (\1A00)","Set( _SET_CONSOLE, \1A00 )"},
 {0,"SET","MARGIN TO \1A00","Set( _SET_MARGIN, \1A00 )"},
 {0,"SET","MARGIN TO","Set( _SET_MARGIN, 0 )"},
 {0,"SET","PRINTER \1A20ON,OFF,&>","Set( _SET_PRINTER, \1A30 )"},
 {0,"SET","PRINTER (\1A00)","Set( _SET_PRINTER, \1A00 )"},
 {0,"SET","PRINTER TO","Set( _SET_PRINTFILE, \"\" )"},
 {0,"SET","PRINTER TO \1A40 [\1B20 ADDITIVE>]","Set( _SET_PRINTFILE, \1A30, \1B50 )"},
 {0,"SET","DEVICE TO SCREEN","Set( _SET_DEVICE, 'SCREEN' )"},
 {0,"SET","DEVICE TO PRINTER","Set( _SET_DEVICE, 'PRINTER' )"},
 {0,"SET","COLOR TO [\1A30]","SetColor( \1A10 )"},
 {0,"SET","COLOR TO ( \1A00 )","SetColor( \1A00 )"},
 {0,"SET","COLOUR TO [\1A30]","SET COLOR TO [\1A00]"},
 {0,"SET","CURSOR \1A20ON,OFF,&>","SetCursor( if(Upper(\1A30) == 'ON', 1, 0) )"},
 {0,"SET","CURSOR (\1A00)","SetCursor( if(\1A00, 1, 0) )"},
 {0,"?","[ \1A10]","QOut( \1A00 )"},
 {0,"??","[ \1A10]","QQOut( \1A00 )"},
 {0,"EJECT","","__Eject()"},
 {0,"TEXT","","text QOut, QQOut"},
 {0,"TEXT","TO FILE \1A40","__TextSave( \1A30 ) ; text QOut, __TextRestore"},
 {0,"TEXT","TO PRINTER","__TextSave('PRINTER') ; text QOut, __TextRestore"},
 {0,"CLS","","Scroll() ; SetPos(0,0)"},
 {0,"CLEAR","SCREEN","CLS"},
 {0,"@","\1A00, \1B00","Scroll( \1A00, \1B00, \1A00 ) ; SetPos( \1A00, \1B00 )"},
 {0,"@","\1A00, \1B00 CLEAR","Scroll( \1A00, \1B00 ) ; SetPos( \1A00, \1B00 )"},
 {0,"@","\1A00, \1B00 CLEAR TO \1C00, \1D00","Scroll( \1A00, \1B00, \1C00, \1D00 ) ; SetPos( \1A00, \1B00 )"},
 {0,"@","\1A00, \1B00, \1C00, \1D00 BOX \1E00 [COLOR \1F00]","DispBox( \1A00, \1B00, \1C00, \1D00, \1E00 [, \1F00 ] )"},
 {0,"@","\1A00, \1B00 TO \1C00, \1D00 [DOUBLE] [COLOR \1E00]","DispBox( \1A00, \1B00, \1C00, \1D00, 2 [, \1E00 ] )"},
 {0,"@","\1A00, \1B00 TO \1C00, \1D00 [COLOR \1E00]","DispBox( \1A00, \1B00, \1C00, \1D00, 1 [, \1E00 ] )"},
 {0,"@","\1A00, \1B00 SAY \1C00 [PICTURE \1D00] [COLOR \1E00]","DevPos( \1A00, \1B00 ) ; DevOutPict( \1C00, \1D00 [, \1E00] )"},
 {0,"@","\1A00, \1B00 SAY \1C00 [COLOR \1D00]","DevPos( \1A00, \1B00 ) ; DevOut( \1C00 [, \1D00] )"},
 {0,"SET","BELL \1A20ON,OFF,&>","Set( _SET_BELL, \1A30 )"},
 {0,"SET","BELL (\1A00)","Set( _SET_BELL, \1A00 )"},
 {0,"SET","CONFIRM \1A20ON,OFF,&>","Set( _SET_CONFIRM, \1A30 )"},
 {0,"SET","CONFIRM (\1A00)","Set( _SET_CONFIRM, \1A00 )"},
 {0,"SET","ESCAPE \1A20ON,OFF,&>","Set( _SET_ESCAPE, \1A30 )"},
 {0,"SET","ESCAPE (\1A00)","Set( _SET_ESCAPE, \1A00 )"},
 {0,"SET","INTENSITY \1A20ON,OFF,&>","Set( _SET_INTENSITY, \1A30 )"},
 {0,"SET","INTENSITY (\1A00)","Set( _SET_INTENSITY, \1A00 )"},
 {0,"SET","SCOREBOARD \1A20ON,OFF,&>","Set( _SET_SCOREBOARD, \1A30 )"},
 {0,"SET","SCOREBOARD (\1A00)","Set( _SET_SCOREBOARD, \1A00 )"},
 {0,"SET","DELIMITERS \1A20ON,OFF,&>","Set( _SET_DELIMITERS, \1A30 )"},
 {0,"SET","DELIMITERS (\1A00)","Set( _SET_DELIMITERS, \1A00 )"},
 {0,"SET","DELIMITERS TO \1A00","Set( _SET_DELIMCHARS, \1A00 )"},
 {0,"SET","DELIMITERS TO DEFAULT","Set( _SET_DELIMCHARS, '::' )"},
 {0,"SET","DELIMITERS TO","Set( _SET_DELIMCHARS, '::' )"},
 {0,"SET","FORMAT TO \1A00","_ProcReq_( \1A30 + '.FMT' ) ; __SetFormat( {|| \1A00()} )"},
 {0,"SET","FORMAT TO \1A00.\1B00","_ProcReq_( \1A30 + '.' + \1B30 ) ; __SetFormat( {|| \1A00()} )"},
 {0,"SET","FORMAT TO \1A20&>","if ( Empty(\1A30) ) ;   SET FORMAT TO ; else ;   __SetFormat( &('{||' + \1A30 + '()}') ) ; end"},
 {0,"SET","FORMAT TO","__SetFormat()"},
 {0,"@","\1A00, \1B00 GET \1C00 [PICTURE \1D00] [VALID \1E00] [WHEN \1F00] [SEND \1G00]",
   "SetPos( \1A00, \1B00 ) ; AAdd( GetList, _GET_( \1C00, \1C20, \1D00, \1E40, \1F40 ):display() ) [; ATail(GetList):\1G00]"},
 {0,"@","\1A00, \1B00 SAY \1C00 [\1D10] GET \1E00 [\1F10]","@ \1A00, \1B00 SAY \1C00 [\1D00] ; @ Row(), Col()+1 GET \1E00 [\1F00]"},
 {0,"@","\1A00, \1B00 GET \1C00 [\1D10] RANGE \1E00, \1F00 [\1G10]",
   "@ \1A00, \1B00 GET \1C00 [\1D00] VALID {|_1| RangeCheck(_1,, \1E00, \1F00)} [\1G00]"},
 {0,"@","\1A00, \1B00 GET \1C00 [\1D10] COLOR \1E00 [\1F10]","@ \1A00, \1B00 GET \1C00 [\1D00] SEND colorDisp(\1E00) [\1F00]"},
 {0,"READ","SAVE","ReadModal(GetList)"},
 {0,"READ","","ReadModal(GetList) ; GetList := {}"},
 {0,"CLEAR","GETS","ReadKill(.T.) ; GetList := {}"},
 {0,"@","[\1A10] COLOUR [\1B10]","@ [\1A00] COLOR [\1B00]"},
 {0,"SET","WRAP \1A20ON,OFF,&>","Set( _SET_WRAP, \1A30 )"},
 {0,"SET","WRAP (\1A00)","Set( _SET_WRAP, \1A00 )"},
 {0,"SET","MESSAGE TO \1A00 [\1B20 CENTER, CENTRE>]","Set( _SET_MESSAGE, \1A00 ) ; Set( _SET_MCENTER, \1B50 )"},
 {0,"SET","MESSAGE TO","Set( _SET_MESSAGE, 0 ) ; Set( _SET_MCENTER, .f. )"},
 {0,"@","\1A00, \1B00 PROMPT \1C00 [MESSAGE \1D00]","__AtPrompt( \1A00, \1B00, \1C00 , \1D00 )"},
 {0,"MENU","TO \1A00","\1A00 := __MenuTo( {|_1| if(PCount() == 0, \1A00, \1A00 := _1)}, \1A10 )"},
 {0,"SAVE","SCREEN","__XSaveScreen()"},
 {0,"RESTORE","SCREEN","__XRestScreen()"},
 {0,"SAVE","SCREEN TO \1A00","\1A00 := SaveScreen( 0, 0, Maxrow(), Maxcol() )"},
 {0,"RESTORE","SCREEN FROM \1A00","RestScreen( 0, 0, Maxrow(), Maxcol(), \1A00 )"},
 {0,"WAIT","[\1A00]","__Wait( \1A00 )"},
 {0,"WAIT","[\1A00] TO \1B00","\1B00 := __Wait( \1A00 )"},
 {0,"ACCEPT","[\1A00] TO \1B00","\1B00 := __Accept( \1A00 )"},
 {0,"INPUT","[\1A00] TO \1B00", "if ( !Empty(__Accept(\1A00)) ) ;    \1B00 := &( __AcceptStr() ) ; end"},
 {0,"KEYBOARD","\1A00","__Keyboard( \1A00 )"},
 {0,"CLEAR","TYPEAHEAD","__Keyboard()"},
 {0,"SET","TYPEAHEAD TO \1A00","Set( _SET_TYPEAHEAD, \1A00 )"},
 {0,"SET","KEY \1A00 TO \1B00","SetKey( \1A00, {|p, l, v| \1B00(p, l, v)} )"},
 {0,"SET","KEY \1A00 TO \1B00 ( [\1C10] )","SET KEY \1A00 TO \1B00"},
 {0,"SET","KEY \1A00 TO \1B20&>","if ( Empty(\1B30) ) ;   SetKey( \1A00, NIL ) ; else ;   SetKey( \1A00, {|p, l, v| \1B00(p, l, v)} ) ; end"},
 {0,"SET","KEY \1A00 [TO]","SetKey( \1A00, NIL )"},
 {0,"SET","FUNCTION \1A00 [TO] [\1B00]","__SetFunction( \1A00, \1B00 )"},
 {0,"CLEAR","MEMORY","__MVClear()"},
 {0,"RELEASE"," \1A00","__MVXRelease( \1A30 )"},
 {0,"RELEASE","ALL","__MVRelease('*', .t.)"},
 {0,"RELEASE","ALL LIKE \1A00","__MVRelease( \1A10, .t. )"},
 {0,"RELEASE","ALL EXCEPT \1A00","__MVRelease( \1A10, .f. )"},
 {0,"RESTORE","[FROM \1A40] [\1B20 ADDITIVE>]","__MVRestore( \1A30, \1B50 )"},
 {0,"SAVE","ALL LIKE \1A00 TO \1B40","__MVSave( \1B30, \1A30, .t. )"},
 {0,"SAVE","TO \1A40 ALL LIKE \1B00","__MVSave( \1A30, \1B30, .t. )"},
 {0,"SAVE","ALL EXCEPT \1A00 TO \1B40","__MVSave( \1B30, \1A30, .f. )"},
 {0,"SAVE","TO \1A40 ALL EXCEPT \1B00","__MVSave( \1A30, \1B30, .f. )"},
 {0,"SAVE","[TO \1A40] [ALL]","__MVSave( \1A30, '*', .t. )"},
 {0,"ERASE","\1A40","FErase( \1A30 )"},
 {0,"DELETE","FILE \1A40","FErase( \1A30 )"},
 {0,"RENAME","\1A40 TO \1B40","FRename( \1A30, \1B30 )"},
 {0,"COPY","FILE \1A40 TO \1B40","__CopyFile( \1A30, \1B30 )"},
 {0,"DIR","[\1A40]","__Dir( \1A30 )"},
 {0,"TYPE","\1A40 [\1B20 TO PRINTER>] [TO FILE \1C40]","__TypeFile( \1A30, \1B50 ) [; COPY FILE \1A30 TO \1C30 ]"},
 {0,"TYPE","\1A40 [\1B20 TO PRINTER>]","__TypeFile( \1A30, \1B50 )"},
 {0,"REQUEST","\1A10","EXTERNAL \1A00"},
 {0,"CANCEL","","__Quit()"},
 {0,"QUIT","","__Quit()"},
 {0,"RUN","\1A30","__Run( \1A10 )"},
 {0,"RUN","( \1A00 )","__Run( \1A00 )"},
 {0,"!","\1A30","RUN \1A00"},
 {0,"RUN","= \1A00","( run := \1A00 )"},
 {0,"RUN",":= \1A00","( run := \1A00 )"},
 {0,"SET","EXCLUSIVE \1A20ON,OFF,&>","Set( _SET_EXCLUSIVE, \1A30 )"},
 {0,"SET","EXCLUSIVE (\1A00)","Set( _SET_EXCLUSIVE, \1A00 )"},
 {0,"SET","SOFTSEEK \1A20ON,OFF,&>","Set( _SET_SOFTSEEK, \1A30 )"},
 {0,"SET","SOFTSEEK (\1A00)","Set( _SET_SOFTSEEK, \1A00 )"},
 {0,"SET","UNIQUE \1A20ON,OFF,&>","Set( _SET_UNIQUE, \1A30 )"},
 {0,"SET","UNIQUE (\1A00)","Set( _SET_UNIQUE, \1A00 )"},
 {0,"SET","DELETED \1A20ON,OFF,&>","Set( _SET_DELETED, \1A30 )"},
 {0,"SET","DELETED (\1A00)","Set( _SET_DELETED, \1A00 )"},
 {0,"SELECT","\1A00","dbSelectArea( \1A30 )"},
 {0,"SELECT","\1A00([\1B10])","dbSelectArea( \1A00(\1B00) )"},
 {0,"USE","","dbCloseArea()"},
 {0,"USE","\1A40 [VIA \1B00] [ALIAS \1C00] [\1D20 NEW>] [\1E20 EXCLUSIVE>] [\1F20 SHARED>] [\1G20 READONLY>] [INDEX \1H40 [, \1I40]]",
   "dbUseArea( \1D50, \1B00, \1A30, \1C30, if(\1F50 .or. \1E50, !\1E50, NIL), \1G50 ) [; dbSetIndex( \1H30 )] [; dbSetIndex( \1I30 )]"},
 {0,"APPEND","BLANK","dbAppend()"},
 {0,"PACK","","__dbPack()"},
 {0,"ZAP","","__dbZap()"},
 {0,"UNLOCK","","dbUnlock()"},
 {0,"UNLOCK","ALL","dbUnlockAll()"},
 {0,"COMMIT","","dbCommitAll()"},
 {0,"GOTO","\1A00","dbGoto(\1A00)"},
 {0,"GO","\1A00","dbGoto(\1A00)"},
 {0,"GOTO","TOP","dbGoTop()"},
 {0,"GO","TOP","dbGoTop()"},
 {0,"GOTO","BOTTOM","dbGoBottom()"},
 {0,"GO","BOTTOM","dbGoBottom()"},
 {0,"SKIP","","dbSkip(1)"},
 {0,"SKIP","\1A00","dbSkip( \1A00 )"},
 {0,"SKIP","ALIAS \1A00","\1A00 -> ( dbSkip(1) )"},
 {0,"SKIP","\1A00 ALIAS \1B00","\1B00 -> ( dbSkip(\1A00) )"},
 {0,"SEEK","\1A00 [\1B20 SOFTSEEK>]","dbSeek( \1A00, if( \1B50, .T., NIL ) )"},
 {0,"FIND","\1A30","dbSeek( \1A30 )"},
 {0,"FIND",":= \1A00","( find := \1A00 )"},
 {0,"FIND","= \1A00","( find := \1A00 )"},
 {0,"CONTINUE","","__dbContinue()"},
 {0,"LOCATE","[FOR \1A00] [WHILE \1B00] [NEXT \1C00] [RECORD \1D00] [\1E20REST>] [ALL]",
   "__dbLocate(\1A40,\1B40,\1C00,\1D00,\1E50)"},
 {0,"SET","RELATION TO","dbClearRel()"},
 {0,"SET","RELATION [\1A20ADDITIVE>] [TO \1B00 INTO \1C40 [, [TO] \1D00 INTO \1E40]]",
   "if ( !\1A50 ) ;    dbClearRel() ; end ; dbSetRelation(\1C30,\1B40,\1B20) [; dbSetRelation(\1E30,\1D40,\1D20)]"},
 {0,"SET","FILTER TO","dbClearFilter(NIL)"},
 {0,"SET","FILTER TO \1A00","dbSetFilter( \1A40, \1A20 )"},
 {0,"SET","FILTER TO \1A20&>","if ( Empty(\1A30) ) ;    dbClearFilter() ; else ;    dbSetFilter(\1A40,\1A30) ; end"},
 {0,"REPLACE","[ \1A00 WITH \1B00 [, \1C00 WITH \1D00] ] [FOR \1E00] [WHILE \1F00] [NEXT \1G00] [RECORD \1H00] [\1I20REST>] [ALL]",
   "DBEval( {|| _FIELD->\1A00 := \1B00 [,_FIELD->\1C00 := \1D00]},\1E40,\1F40,\1G00,\1H00,\1I50)"},
 {0,"REPLACE","\1A00 WITH \1B00 [, \1C00 WITH \1D00 ]","_FIELD->\1A00 := \1B00 [; _FIELD->\1C00 := \1D00]"},
 {0,"DELETE","[FOR \1A00] [WHILE \1B00] [NEXT \1C00] [RECORD \1D00] [\1E20REST>] [ALL]",
   "DBEval( {|| dbDelete()}, \1A40, \1B40, \1C00, \1D00, \1E50 )"},
 {0,"RECALL","[FOR \1A00] [WHILE \1B00] [NEXT \1C00] [RECORD \1D00] [\1E20REST>] [ALL]",
   "DBEval( {|| dbRecall()}, \1A40, \1B40, \1C00, \1D00, \1E50 )"},
 {0,"DELETE","","dbDelete()"},
 {0,"RECALL","","dbRecall()"},
 {0,"CREATE","\1A40 [FROM \1B40] [VIA \1C00] [ALIAS \1D00] [\1E20 NEW>]","__dbCreate( \1A30, \1B30, \1C00, \1E50, \1D30 )"},
 {0,"COPY","[STRUCTURE] [EXTENDED] [TO \1A40]","__dbCopyXStruct( \1A30 )"},
 {0,"COPY","[STRUCTURE] [TO \1A40] [FIELDS \1B10]","__dbCopyStruct( \1A30, { \1B30 } )"},
 {0,"COPY","[TO \1A40] [DELIMITED [WITH \1B30]] [FIELDS \1C10] [FOR \1D00] [WHILE \1E00] [NEXT \1F00] [RECORD \1G00] [\1H20REST>] [ALL]",
   "__dbDelim( .T., \1A30, \1B30, { \1C30 }, \1D40, \1E40, \1F00, \1G00, \1H50 )"},
 {0,"COPY","[TO \1A40] [SDF] [FIELDS \1B10] [FOR \1C00] [WHILE \1D00] [NEXT \1E00] [RECORD \1F00] [\1G20REST>] [ALL]",
   "__dbSDF( .T., \1A30, { \1B30 }, \1C40, \1D40, \1E00, \1F00, \1G50 )"},
 {0,"COPY","[TO \1A40] [FIELDS \1B10] [FOR \1C00] [WHILE \1D00] [NEXT \1E00] [RECORD \1F00] [\1G20REST>] [VIA \1H00] [ALL]",
   "__dbCopy( \1A30, { \1B30 }, \1C40, \1D40, \1E00, \1F00, \1G50, \1H00 )"},
 {0,"APPEND","[FROM \1A40] [DELIMITED [WITH \1B30]] [FIELDS \1C10] [FOR \1D00] [WHILE \1E00] [NEXT \1F00] [RECORD \1G00] [\1H20REST>] [ALL]",
   "__dbDelim( .F., \1A30, \1B30, { \1C30 }, \1D40, \1E40, \1F00, \1G00, \1H50 )"},
 {0,"APPEND","[FROM \1A40] [SDF] [FIELDS \1B10] [FOR \1C00] [WHILE \1D00] [NEXT \1E00] [RECORD \1F00] [\1G20REST>] [ALL]",
   "__dbSDF( .F., \1A30, { \1B30 }, \1C40, \1D40, \1E00, \1F00, \1G50 )"},
 {0,"APPEND","[FROM \1A40] [FIELDS \1B10] [FOR \1C00] [WHILE \1D00] [NEXT \1E00] [RECORD \1F00] [\1G20REST>] [VIA \1H00] [ALL]",
   "__dbApp( \1A30, { \1B30 }, \1C40, \1D40, \1E00, \1F00, \1G50, \1H00 )"},
 {0,"SORT","[TO \1A40] [ON \1B10] [FOR \1C00] [WHILE \1D00] [NEXT \1E00] [RECORD \1F00] [\1G20REST>] [ALL]",
   "__dbSort( \1A30, { \1B30 }, \1C40, \1D40, \1E00, \1F00, \1G50 )"},
 {0,"TOTAL","[TO \1A40] [ON \1B00] [FIELDS \1C10] [FOR \1D00] [WHILE \1E00] [NEXT \1F00] [RECORD \1G00] [\1H20REST>] [ALL]",
   "__dbTotal( \1A30, \1B40, { \1C30 }, \1D40, \1E40, \1F00, \1G00, \1H50 )"},
 {0,"UPDATE","[FROM \1A40] [ON \1B00] [REPLACE \1C00 WITH \1D00 [, \1E00 WITH \1F00]] [\1G20RANDOM>]",
   "__dbUpdate( \1A30, \1B40, \1G50, {|| _FIELD->\1C00 := \1D00 [, _FIELD->\1E00 := \1F00]} )"},
 {0,"JOIN","[WITH \1A40] [TO \1B00] [FIELDS \1C10] [FOR \1D00]","__dbJoin( \1A30, \1B30, { \1C30 }, \1D40 )"},
 {0,"COUNT","[TO \1A00] [FOR \1B00] [WHILE \1C00] [NEXT \1D00] [RECORD \1E00] [\1F20REST>] [ALL]",
   "\1A00 := 0 ; DBEval( {|| \1A00 := \1A00 + 1}, \1B40, \1C40, \1D00, \1E00, \1F50 )"},
 {0,"SUM","[ \1A00 [, \1B00]  TO  \1C00 [, \1D00] ] [FOR \1E00] [WHILE \1F00] [NEXT \1G00] [RECORD \1H00] [\1I20REST>] [ALL]",
   "\1C00 := [ \1D00 := ] 0 ; DBEval( {|| \1C00 := \1C00 + \1A00 [, \1D00 := \1D00 + \1B00 ]}, \1E40, \1F40, \1G00, \1H00, \1I50 )"},
 {0,"AVERAGE","[ \1A00 [, \1B00]  TO  \1C00 [, \1D00] ] [FOR \1E00] [WHILE \1F00] [NEXT \1G00] [RECORD \1H00] [\1I20REST>] [ALL]",
   "M->__Avg := \1C00 := [ \1D00 := ] 0 ; DBEval( {|| M->__Avg := M->__Avg + 1, \1C00 := \1C00 + \1A00 [, \1D00 := \1D00 + \1B00] }, \1E40, \1F40, \1G00, \1H00, \1I50 ) ; \1C00 := \1C00 / M->__Avg [; \1D00 := \1D00 / M->__Avg ]"},
 {0,"LIST","[\1A10] [\1B20OFF>] [\1C20 TO PRINTER>] [TO FILE \1D40] [FOR \1E00] [WHILE \1F00] [NEXT \1G00] [RECORD \1H00] [\1I20REST>] [ALL]",
   "__dbList( \1B50, { \1A40 }, .t., \1E40, \1F40, \1G00, \1H00, \1I50, \1C50, \1D30 )"},
 {0,"DISPLAY","[\1A10] [\1B20OFF>] [\1C20 TO PRINTER>] [TO FILE \1D40] [FOR \1E00] [WHILE \1F00] [NEXT \1G00] [RECORD \1H00] [\1I20REST>] [\1J20ALL>]",
   "__DBList( \1B50, { \1A40 }, \1J50, \1E40, \1F40, \1G00, \1H00, \1I50, \1C50, \1D30 )"},
 {0,"REPORT","FORM \1A00 [HEADING \1B00] [\1C20 PLAIN>] [\1D20 NOEJECT>] [\1E20 SUMMARY>] [\1F20 NOCONSOLE>] [\1G20 TO PRINTER>] [TO FILE \1H40] [FOR \1I00] [WHILE \1J00] [NEXT \1K00] [RECORD \1L00] [\1M20REST>] [ALL]",
   "__ReportForm( \1A30, \1G50, \1H30, \1F50, \1I40, \1J40, \1K00, \1L00, \1M50, \1C50, \1B00, \1D50, \1E50 )"},
 {0,"LABEL","FORM \1A00 [\1B20 SAMPLE>] [\1C20 NOCONSOLE>] [\1D20 TO PRINTER>] [TO FILE \1E40] [FOR \1F00] [WHILE \1G00] [NEXT \1H00] [RECORD \1I00] [\1J20REST>] [ALL]",
   "__LabelForm( \1A30, \1D50, \1E30, \1C50, \1F40, \1G40, \1H00, \1I00, \1J50, \1B50 )"},
 {0,"CLOSE","\1A00","\1A00->( dbCloseArea() )"},
 {0,"CLOSE","","dbCloseArea()"},
 {0,"CLOSE","DATABASES","dbCloseAll()"},
 {0,"CLOSE","ALTERNATE","Set(_SET_ALTFILE, \"\")"},
 {0,"CLOSE","FORMAT","__SetFormat(NIL)"},
 {0,"CLOSE","INDEXES","dbClearIndex()"},
 {0,"CLOSE","PROCEDURE",NULL},
 {0,"CLOSE","ALL","CLOSE DATABASES ; SELECT 1 ; CLOSE FORMAT"},
 {0,"CLEAR","","CLEAR SCREEN ; CLEAR GETS"},
 {0,"CLEAR","ALL","CLOSE DATABASES ; CLOSE FORMAT ; CLEAR MEMORY ; CLEAR GETS ; SET ALTERNATE OFF ; SET ALTERNATE TO"},
 {0,"INDEX","ON \1A00 [TAG \1B40 ] TO \1C40 [FOR \1D00] [\1E20ALL>] [WHILE \1F00] [NEXT \1G00] [RECORD \1H00] [\1I20REST>] [EVAL \1J00] [EVERY \1K00] [\1L20 UNIQUE>] [\1M20 ASCENDING>] [\1N20 DESCENDING>]",
   "ordCondSet( \1D20, \1D40, [\1E50], \1F40, \1J40, \1K00, RECNO(), \1G00, \1H00, [\1I50], [\1N50] ) ;  ordCreate(\1C30, \1B30, \1A20, \1A40, [\1L50] )"},
 {0,"INDEX","ON \1A00 TAG \1B40 [TO \1C40] [FOR \1D00] [\1E20ALL>] [WHILE \1F00] [NEXT \1G00] [RECORD \1H00] [\1I20REST>] [EVAL \1J00] [EVERY \1K00] [\1L20 UNIQUE>] [\1M20 ASCENDING>] [\1N20 DESCENDING>]",
   "ordCondSet( \1D20, \1D40, [\1E50], \1F40, \1J40, \1K00, RECNO(), \1G00, \1H00, [\1I50], [\1N50] ) ;  ordCreate(\1C30, \1B30, \1A20, \1A40, [\1L50] )"},
 {0,"INDEX","ON \1A00 TO \1B40 [\1C20 UNIQUE>]","dbCreateIndex( \1B30, \1A20, \1A40, if( \1C50, .t., NIL ) )"},
 {0,"DELETE","TAG \1A40 [ IN \1B40 ] [, \1C40 [ IN \1D40 ] ]","ordDestroy( \1A30, \1B30 ) [; ordDestroy( \1C30, \1D30 ) ]"},
 {0,"REINDEX","[EVAL \1A00] [EVERY \1B00]","ordCondSet(,,,, \1A40, \1B00,,,,,,,) ;  ordListRebuild()"},
 {0,"REINDEX","","ordListRebuild()"},
 {0,"SET","INDEX TO [ \1A40 [, \1B40]] [\1C20 ADDITIVE>]",
   "if !\1C50 ; ordListClear() ; end [; ordListAdd( \1A30 )] [; ordListAdd( \1B30 )]"},
 {0,"SET","ORDER TO \1A00 [IN \1B40]","ordSetFocus( \1A00 [, \1B30] )"},
 {0,"SET","ORDER TO TAG \1A40 [IN \1B40]","ordSetFocus( \1A30 [, \1B30] )"},
 {0,"SET","ORDER TO","ordSetFocus(0)"},
 {0,"ANNOUNCE","\1A10","procedure \1A00 ; return"}
};
