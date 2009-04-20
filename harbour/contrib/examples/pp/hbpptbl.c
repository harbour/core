/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Preprocessor precompiled std.ch and some additions ( mainly generated )
 *
 * Copyright 1999 Alexander S.Kresin <alex@belacy.belgorod.su>
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

/*
 * Avoid tracing in preprocessor/compiler.
 */
#if ! defined(HB_TRACE_UTILS)
   #if defined(HB_TRACE_LEVEL)
      #undef HB_TRACE_LEVEL
   #endif
#endif

#include <stdio.h>

#include "hbppdef.h"

COMMANDS * hb_pp_topCommand = NULL;
COMMANDS * hb_pp_topTranslate = NULL;
DEFINES * hb_pp_topDefine = NULL;

void hb_pp_Table( void )
{
   static DEFINES sD___01 = {"_SET_ALTERNATE",     14,NULL,-1,"18", NULL };
   static DEFINES sD___02 = {"_SET_ALTFILE",       12,NULL,-1,"19", &sD___01 };
   static DEFINES sD___03 = {"_SET_BELL",           9,NULL,-1,"26", &sD___02 };
   static DEFINES sD___04 = {"_SET_CANCEL",        11,NULL,-1,"12", &sD___03 };
   static DEFINES sD___05 = {"_SET_COLOR",         10,NULL,-1,"15", &sD___04 };
   static DEFINES sD___06 = {"_SET_CONFIRM",       12,NULL,-1,"27", &sD___05 };
   static DEFINES sD___07 = {"_SET_CONSOLE",       12,NULL,-1,"17", &sD___06 };
   static DEFINES sD___08 = {"_SET_CURSOR",        11,NULL,-1,"16", &sD___07 };
   static DEFINES sD___09 = {"_SET_DATEFORMAT",    15,NULL,-1,"4", &sD___08 };
   static DEFINES sD___10 = {"_SET_DEBUG",         10,NULL,-1,"13", &sD___09 };
   static DEFINES sD___11 = {"_SET_DECIMALS",      13,NULL,-1,"3", &sD___10 };
   static DEFINES sD___12 = {"_SET_DEFAULT",       12,NULL,-1,"7", &sD___11 };
   static DEFINES sD___13 = {"_SET_DELETED",       12,NULL,-1,"11", &sD___12 };
   static DEFINES sD___14 = {"_SET_DELIMCHARS",    15,NULL,-1,"34", &sD___13 };
   static DEFINES sD___15 = {"_SET_DELIMITERS",    15,NULL,-1,"33", &sD___14 };
   static DEFINES sD___16 = {"_SET_DEVICE",        11,NULL,-1,"20", &sD___15 };
   static DEFINES sD___17 = {"_SET_EPOCH",         10,NULL,-1,"5", &sD___16 };
   static DEFINES sD___18 = {"_SET_ESCAPE",        11,NULL,-1,"28", &sD___17 };
   static DEFINES sD___19 = {"_SET_EVENTMASK",     14,NULL,-1,"39", &sD___18 };
   static DEFINES sD___20 = {"_SET_EXACT",         10,NULL,-1,"1", &sD___19 };
   static DEFINES sD___21 = {"_SET_EXCLUSIVE",     14,NULL,-1,"8", &sD___20 };
   static DEFINES sD___22 = {"_SET_EXIT",           9,NULL,-1,"30", &sD___21 };
   static DEFINES sD___23 = {"_SET_EXTRA",         10,NULL,-1,"21", &sD___22 };
   static DEFINES sD___24 = {"_SET_EXTRAFILE",     14,NULL,-1,"22", &sD___23 };
   static DEFINES sD___25 = {"_SET_FIXED",         10,NULL,-1,"2", &sD___24 };
   static DEFINES sD___26 = {"_SET_INSERT",        11,NULL,-1,"29", &sD___25 };
   static DEFINES sD___27 = {"_SET_INTENSITY",     14,NULL,-1,"31", &sD___26 };
   static DEFINES sD___28 = {"_SET_MARGIN",        11,NULL,-1,"25", &sD___27 };
   static DEFINES sD___29 = {"_SET_MCENTER",       12,NULL,-1,"37", &sD___28 };
   static DEFINES sD___30 = {"_SET_MESSAGE",       12,NULL,-1,"36", &sD___29 };
   static DEFINES sD___31 = {"_SET_PATH",           9,NULL,-1,"6", &sD___30 };
   static DEFINES sD___32 = {"_SET_PRINTER",       12,NULL,-1,"23", &sD___31 };
   static DEFINES sD___33 = {"_SET_PRINTFILE",     14,NULL,-1,"24", &sD___32 };
   static DEFINES sD___34 = {"_SET_SCOREBOARD",    15,NULL,-1,"32", &sD___33 };
   static DEFINES sD___35 = {"_SET_SCROLLBREAK",   16,NULL,-1,"38", &sD___34 };
   static DEFINES sD___36 = {"_SET_SOFTSEEK",      13,NULL,-1,"9", &sD___35 };
   static DEFINES sD___37 = {"_SET_TYPEAHEAD",     14,NULL,-1,"14", &sD___36 };
   static DEFINES sD___38 = {"_SET_UNIQUE",        11,NULL,-1,"10", &sD___37 };
   static DEFINES sD___39 = {"_SET_WRAP",           9,NULL,-1,"35", &sD___38 };
   static DEFINES sD___40 = {"_SET_COUNT",         10,NULL,-1,"47", &sD___39 };
   static DEFINES sD___41 = {"_SET_CH",             7,NULL,-1,NULL, &sD___40 };
   static DEFINES sD___42 = {"_DFSET",              6,"\001x,\001y",2,"Set( _SET_DATEFORMAT, if(__SetCentury(), \001x, \001y) )", &sD___41 };
   static DEFINES sD___43 = {"_SET_VIDEOMODE",     14,NULL,-1,"40", &sD___42 };
   static DEFINES sD___44 = {"_SET_MBLOCKSIZE",    15,NULL,-1,"41", &sD___43 };
   static DEFINES sD___45 = {"_SET_MFILEEXT",      13,NULL,-1,"42", &sD___44 };
   static DEFINES sD___46 = {"_SET_STRICTREAD",    15,NULL,-1,"43", &sD___45 };
   static DEFINES sD___47 = {"_SET_OPTIMIZE",      13,NULL,-1,"44", &sD___46 };
   static DEFINES sD___48 = {"_SET_AUTOPEN",       12,NULL,-1,"45", &sD___47 };
   static DEFINES sD___49 = {"_SET_AUTORDER",      13,NULL,-1,"46", &sD___48 };
   static DEFINES sD___50 = {"_SET_AUTOSHARE",     14,NULL,-1,"47", &sD___49 };
   static DEFINES sD___51 = {"_SET_LANGUAGE",      13,NULL,-1,"100", &sD___50 };
   static DEFINES sD___52 = {"_SET_IDLEREPEAT",    15,NULL,-1,"101", &sD___51 };
   static DEFINES sD___53 = {"_SET_FILECASE",      13,NULL,-1,"102", &sD___52 };
   static DEFINES sD___54 = {"_SET_DIRCASE",       12,NULL,-1,"103", &sD___53 };
   static DEFINES sD___55 = {"_SET_DIRSEPARATOR",  17,NULL,-1,"104", &sD___54 };
   static DEFINES sD___56 = {"_SET_EOF",            8,NULL,-1,"105", &sD___55 };
   static DEFINES sD___57 = {"_SET_HARDCOMMIT",    15,NULL,-1,"106", &sD___56 };
   static DEFINES sD___58 = {"_SET_FORCEOPT",      13,NULL,-1,"107", &sD___57 };
   static DEFINES sD___59 = {"_SET_DBFLOCKSCHEME", 18,NULL,-1,"108", &sD___58 };
   static DEFINES sD___60 = {"_SET_DEFEXTENSIONS", 18,NULL,-1,"109", &sD___59 };

   static COMMANDS sC___1 = {0,"NOTE",    4,"\1A30",NULL,NULL };
   static COMMANDS sC___2 = {0,"DO",2,"WHILE \1A00","while \1A00",&sC___1 };
   static COMMANDS sC___3 = {0,"END",3,"\1A00","end",&sC___2 };
   static COMMANDS sC___4 = {0,"END",3,"SEQUENCE","end",&sC___3 };
   static COMMANDS sC___5 = {0,"ENDSEQUENCE",11,"","end",&sC___4 };
   static COMMANDS sC___6 = {0,"ENDDO",5,"\1A30","enddo",&sC___5 };
   static COMMANDS sC___7 = {0,"ENDIF",5,"\1A30","endif",&sC___6 };
   static COMMANDS sC___8 = {0,"ENDCASE",7,"\1A30","endcase",&sC___7 };
   static COMMANDS sC___9 = {0,"ENDFOR",6,"\2 \1A30 \3","next",&sC___8 };
   static COMMANDS sC___10 = {0,"NEXT",4,"\1A00 \2TO \1B00\3 \2STEP \1C00\3","next",&sC___9 };
   static COMMANDS sC___11 = {0,"DO",2,"\1A00.PRG \2WITH \1B10\3","do \1A00 \2 WITH \1B00\3",&sC___10 };
   static COMMANDS sC___12 = {0,"CALL",4,"\1A00() \2WITH \1B10\3","call \1A00 \2 WITH \1B00\3",&sC___11 };
   static COMMANDS sC___13 = {0,"STORE",5,"\1A00 TO \1B00 \2, \1C00 \3","\1B00 := \2 \1C00 := \3 \1A00",&sC___12 };
   static COMMANDS sC___14 = {0,"SET",3,"ECHO \1A30",NULL,&sC___13 };
   static COMMANDS sC___15 = {0,"SET",3,"HEADING \1A30",NULL,&sC___14 };
   static COMMANDS sC___16 = {0,"SET",3,"MENU \1A30",NULL,&sC___15 };
   static COMMANDS sC___17 = {0,"SET",3,"STATUS \1A30",NULL,&sC___16 };
   static COMMANDS sC___18 = {0,"SET",3,"STEP \1A30",NULL,&sC___17 };
   static COMMANDS sC___19 = {0,"SET",3,"SAFETY \1A30",NULL,&sC___18 };
   static COMMANDS sC___20 = {0,"SET",3,"TALK \1A30",NULL,&sC___19 };
   static COMMANDS sC___21 = {0,"SET",3,"PROCEDURE TO",NULL,&sC___20 };
   static COMMANDS sC___22 = {0,"SET",3,"PROCEDURE TO \1A00","_ProcReq_( \1A30 )",&sC___21 };
   static COMMANDS sC___23 = {0,"SET",3,"EXACT \1A20ON,OFF,&>","Set( _SET_EXACT, \1A30 )",&sC___22 };
   static COMMANDS sC___24 = {0,"SET",3,"EXACT (\1A00)","Set( _SET_EXACT, \1A00 )",&sC___23 };
   static COMMANDS sC___25 = {0,"SET",3,"FIXED \1A20ON,OFF,&>","Set( _SET_FIXED, \1A30 )",&sC___24 };
   static COMMANDS sC___26 = {0,"SET",3,"FIXED (\1A00)","Set( _SET_FIXED, \1A00 )",&sC___25 };
   static COMMANDS sC___27 = {0,"SET",3,"DECIMALS TO \1A00","Set( _SET_DECIMALS, \1A00 )",&sC___26 };
   static COMMANDS sC___28 = {0,"SET",3,"DECIMALS TO","Set( _SET_DECIMALS, 0 )",&sC___27 };
   static COMMANDS sC___29 = {0,"SET",3,"PATH TO \1A30","Set( _SET_PATH, \1A30 )",&sC___28 };
   static COMMANDS sC___30 = {0,"SET",3,"PATH TO","Set( _SET_PATH, "" )",&sC___29 };
   static COMMANDS sC___31 = {0,"SET",3,"DEFAULT TO \1A40","Set( _SET_DEFAULT, \1A30 )",&sC___30 };
   static COMMANDS sC___32 = {0,"SET",3,"DEFAULT TO","Set( _SET_DEFAULT, "" )",&sC___31 };
   static COMMANDS sC___33 = {0,"SET",3,"CENTURY \1A20ON,OFF,&>","__SetCentury( \1A30 )",&sC___32 };
   static COMMANDS sC___34 = {0,"SET",3,"CENTURY (\1A00)","__SetCentury( \1A00 )",&sC___33 };
   static COMMANDS sC___35 = {0,"SET",3,"EPOCH TO \1A00","Set( _SET_EPOCH, \1A00 )",&sC___34 };
   static COMMANDS sC___36 = {0,"SET",3,"DATE FORMAT \2TO\3 \1A00","Set( _SET_DATEFORMAT, \1A00 )",&sC___35 };
   static COMMANDS sC___37 = {0,"SET",3,"DATE \2TO\3 AMERICAN","_DFSET( 'mm/dd/yyyy', 'mm/dd/yy' )",&sC___36 };
   static COMMANDS sC___38 = {0,"SET",3,"DATE \2TO\3 ANSI","_DFSET( 'yyyy.mm.dd', 'yy.mm.dd' )",&sC___37 };
   static COMMANDS sC___39 = {0,"SET",3,"DATE \2TO\3 BRITISH","_DFSET( 'dd/mm/yyyy', 'dd/mm/yy' )",&sC___38 };
   static COMMANDS sC___40 = {0,"SET",3,"DATE \2TO\3 FRENCH","_DFSET( 'dd/mm/yyyy', 'dd/mm/yy' )",&sC___39 };
   static COMMANDS sC___41 = {0,"SET",3,"DATE \2TO\3 GERMAN","_DFSET( 'dd.mm.yyyy', 'dd.mm.yy' )",&sC___40 };
   static COMMANDS sC___42 = {0,"SET",3,"DATE \2TO\3 ITALIAN","_DFSET( 'dd-mm-yyyy', 'dd-mm-yy' )",&sC___41 };
   static COMMANDS sC___43 = {0,"SET",3,"DATE \2TO\3 JAPANESE","_DFSET( 'yyyy/mm/dd', 'yy/mm/dd' )",&sC___42 };
   static COMMANDS sC___44 = {0,"SET",3,"DATE \2TO\3 USA","_DFSET( 'mm-dd-yyyy', 'mm-dd-yy' )",&sC___43 };
   static COMMANDS sC___45 = {0,"SET",3,"ALTERNATE \1A20ON,OFF,&>","Set( _SET_ALTERNATE, \1A30 )",&sC___44 };
   static COMMANDS sC___46 = {0,"SET",3,"ALTERNATE (\1A00)","Set( _SET_ALTERNATE, \1A00 )",&sC___45 };
   static COMMANDS sC___47 = {0,"SET",3,"ALTERNATE TO","Set( _SET_ALTFILE, "" )",&sC___46 };
   static COMMANDS sC___48 = {0,"SET",3,"ALTERNATE TO \1A40 \2\1B20 ADDITIVE>\3","Set( _SET_ALTFILE, \1A30, \1B50 )",&sC___47 };
   static COMMANDS sC___49 = {0,"SET",3,"CONSOLE \1A20ON,OFF,&>","Set( _SET_CONSOLE, \1A30 )",&sC___48 };
   static COMMANDS sC___50 = {0,"SET",3,"CONSOLE (\1A00)","Set( _SET_CONSOLE, \1A00 )",&sC___49 };
   static COMMANDS sC___51 = {0,"SET",3,"MARGIN TO \1A00","Set( _SET_MARGIN, \1A00 )",&sC___50 };
   static COMMANDS sC___52 = {0,"SET",3,"MARGIN TO","Set( _SET_MARGIN, 0 )",&sC___51 };
   static COMMANDS sC___53 = {0,"SET",3,"PRINTER \1A20ON,OFF,&>","Set( _SET_PRINTER, \1A30 )",&sC___52 };
   static COMMANDS sC___54 = {0,"SET",3,"PRINTER (\1A00)","Set( _SET_PRINTER, \1A00 )",&sC___53 };
   static COMMANDS sC___55 = {0,"SET",3,"PRINTER TO","Set( _SET_PRINTFILE, '""' )",&sC___54 };
   static COMMANDS sC___56 = {0,"SET",3,"PRINTER TO \1A40 \2\1B20 ADDITIVE>\3","Set( _SET_PRINTFILE, \1A30, \1B50 )",&sC___55 };
   static COMMANDS sC___57 = {0,"SET",3,"DEVICE TO SCREEN","Set( _SET_DEVICE, 'SCREEN' )",&sC___56 };
   static COMMANDS sC___58 = {0,"SET",3,"DEVICE TO PRINTER","Set( _SET_DEVICE, 'PRINTER' )",&sC___57 };
   static COMMANDS sC___59 = {0,"SET",3,"COLOR TO \2\1A30\3","SetColor( \1A10 )",&sC___58 };
   static COMMANDS sC___60 = {0,"SET",3,"COLOR TO ( \1A00 )","SetColor( \1A00 )",&sC___59 };
   static COMMANDS sC___61 = {0,"SET",3,"COLOUR TO \2\1A30\3","SET COLOR TO \2\1A00\3",&sC___60 };
   static COMMANDS sC___62 = {0,"SET",3,"CURSOR \1A20ON,OFF,&>","SetCursor( if(Upper(\1A30) == 'ON', 1, 0) )",&sC___61 };
   static COMMANDS sC___63 = {0,"SET",3,"CURSOR (\1A00)","SetCursor( if(\1A00, 1, 0) )",&sC___62 };
   static COMMANDS sC___64 = {0,"?",1,"\2 \1A10\3","QOut( \1A00 )",&sC___63 };
   static COMMANDS sC___65 = {0,"?",1,"? \2 \1A10\3","QQOut( \1A00 )",&sC___64 };
   static COMMANDS sC___66 = {0,"EJECT",5,"","__Eject()",&sC___65 };
   static COMMANDS sC___67 = {0,"TEXT",4,"","#pragma __text|QOut(%s)|QQOut()",&sC___66 };
   static COMMANDS sC___68 = {0,"TEXT",4,"TO FILE \1A40","#pragma __text|QOut(%s)|__TextRestore()|__TextSave( \1A30 )",&sC___67 };
   static COMMANDS sC___69 = {0,"TEXT",4,"TO PRINTER","#pragma __text|QOut(%s)|__TextRestore()|__TextSave('PRINTER')",&sC___68 };
   static COMMANDS sC___70 = {0,"CLS",3,"","Scroll() ; SetPos(0,0)",&sC___69 };
   static COMMANDS sC___71 = {0,"CLEAR",5,"SCREEN","CLS",&sC___70 };
   static COMMANDS sC___72 = {0,"@",1,"\1A00, \1B00","Scroll( \1A00, \1B00, \1A00 ) ; SetPos( \1A00, \1B00 )",&sC___71 };
   static COMMANDS sC___73 = {0,"@",1,"\1A00, \1B00 CLEAR","Scroll( \1A00, \1B00 ) ; SetPos( \1A00, \1B00 )",&sC___72 };
   static COMMANDS sC___74 = {0,"@",1,"\1A00, \1B00 CLEAR TO \1C00, \1D00",
       "Scroll( \1A00, \1B00, \1C00, \1D00 ) ; SetPos( \1A00, \1B00 )",&sC___73 };
   static COMMANDS sC___75 = {0,"@",1,"\1A00, \1B00, \1C00, \1D00 BOX \1E00 \2COLOR \1F00\3",
       "DispBox( \1A00, \1B00, \1C00, \1D00, \1E00 \2, \1F00 \3 )",&sC___74 };
   static COMMANDS sC___76 = {0,"@",1,"\1A00, \1B00 TO \1C00, \1D00 \2DOUBLE\3 \2COLOR \1E00\3",
       "DispBox( \1A00, \1B00, \1C00, \1D00, 2 \2, \1E00 \3 )",&sC___75 };
   static COMMANDS sC___77 = {0,"@",1,"\1A00, \1B00 TO \1C00, \1D00 \2COLOR \1E00\3",
       "DispBox( \1A00, \1B00, \1C00, \1D00, 1 \2, \1E00 \3 )",&sC___76 };
   static COMMANDS sC___78 = {0,"@",1,"\1A00, \1B00 SAY \1C00 \2PICTURE \1D00\3 \2COLOR \1E00\3",
       "DevPos( \1A00, \1B00 ) ; DevOutPict( \1C00, \1D00 \2, \1E00\3 )",&sC___77 };
   static COMMANDS sC___79 = {0,"@",1,"\1A00, \1B00 SAY \1C00 \2COLOR \1D00\3",
       "DevPos( \1A00, \1B00 ) ; DevOut( \1C00 \2, \1D00\3 )",&sC___78 };
   static COMMANDS sC___80 = {0,"SET",3,"BELL \1A20ON,OFF,&>","Set( _SET_BELL, \1A30 )",&sC___79 };
   static COMMANDS sC___81 = {0,"SET",3,"BELL (\1A00)","Set( _SET_BELL, \1A00 )",&sC___80 };
   static COMMANDS sC___82 = {0,"SET",3,"CONFIRM \1A20ON,OFF,&>","Set( _SET_CONFIRM, \1A30 )",&sC___81 };
   static COMMANDS sC___83 = {0,"SET",3,"CONFIRM (\1A00)","Set( _SET_CONFIRM, \1A00 )",&sC___82 };
   static COMMANDS sC___84 = {0,"SET",3,"ESCAPE \1A20ON,OFF,&>","Set( _SET_ESCAPE, \1A30 )",&sC___83 };
   static COMMANDS sC___85 = {0,"SET",3,"ESCAPE (\1A00)","Set( _SET_ESCAPE, \1A00 )",&sC___84 };
   static COMMANDS sC___86 = {0,"SET",3,"INTENSITY \1A20ON,OFF,&>","Set( _SET_INTENSITY, \1A30 )",&sC___85 };
   static COMMANDS sC___87 = {0,"SET",3,"INTENSITY (\1A00)","Set( _SET_INTENSITY, \1A00 )",&sC___86 };
   static COMMANDS sC___88 = {0,"SET",3,"SCOREBOARD \1A20ON,OFF,&>","Set( _SET_SCOREBOARD, \1A30 )",&sC___87 };
   static COMMANDS sC___89 = {0,"SET",3,"SCOREBOARD (\1A00)","Set( _SET_SCOREBOARD, \1A00 )",&sC___88 };
   static COMMANDS sC___90 = {0,"SET",3,"DELIMITERS \1A20ON,OFF,&>","Set( _SET_DELIMITERS, \1A30 )",&sC___89 };
   static COMMANDS sC___91 = {0,"SET",3,"DELIMITERS (\1A00)","Set( _SET_DELIMITERS, \1A00 )",&sC___90 };
   static COMMANDS sC___92 = {0,"SET",3,"DELIMITERS TO \1A00","Set( _SET_DELIMCHARS, \1A00 )",&sC___91 };
   static COMMANDS sC___93 = {0,"SET",3,"DELIMITERS TO DEFAULT","Set( _SET_DELIMCHARS, '::' )",&sC___92 };
   static COMMANDS sC___94 = {0,"SET",3,"DELIMITERS TO","Set( _SET_DELIMCHARS, '::' )",&sC___93 };
   static COMMANDS sC___95 = {0,"SET",3,"FORMAT TO \1A00","_ProcReq_( \1A30 + '.fmt' ) ; __SetFormat( {|| \1A00()} )",&sC___94 };
   static COMMANDS sC___96 = {0,"SET",3,"FORMAT TO \1A00.\1B00",
       "_ProcReq_( \1A30 + '.' + \1B30 ) ; __SetFormat( {|| \1A00()} )",&sC___95 };
   static COMMANDS sC___97 = {0,"SET",3,"FORMAT TO \1A20&>",
       "if ( Empty(\1A30) ) ;   SET FORMAT TO ; else ;   __SetFormat( &('{||' + \1A30 + '()}') ) ; end",&sC___96 };
   static COMMANDS sC___98 = {0,"SET",3,"FORMAT TO","__SetFormat()",&sC___97 };
   static COMMANDS sC___99 = {0,"@",1,"\1A00, \1B00 GET \1C00 \2PICTURE \1D00\3 \2VALID \1E00\3 \2WHEN \1F00\3 \2SEND \1G00\3",
       "SetPos( \1A00, \1B00 ) ; AAdd( GetList, _GET_( \1C00, \1C20, \1D00, \1E40, \1F40 ):display() ) \2; ATail(GetList):\1G00\3",&sC___98 };
   static COMMANDS sC___100 = {0,"@",1,"\1A00, \1B00 SAY \1C00 \2\1D10\3 GET \1E00 \2\1F10\3",
       "@ \1A00, \1B00 SAY \1C00 \2\1D00\3 ; @ Row(), Col()+1 GET \1E00 \2\1F00\3",&sC___99 };
   static COMMANDS sC___101 = {0,"@",1,"\1A00, \1B00 GET \1C00 \2\1D10\3 RANGE \1E00, \1F00 \2\1G10\3",
       "@ \1A00, \1B00 GET \1C00 \2\1D00\3 VALID {|_1| RangeCheck(_1,, \1E00, \1F00)} \2\1G00\3",&sC___100 };
   static COMMANDS sC___102 = {0,"@",1,"\1A00, \1B00 GET \1C00 \2\1D10\3 COLOR \1E00 \2\1F10\3",
       "@ \1A00, \1B00 GET \1C00 \2\1D00\3 SEND colorDisp(\1E00) \2\1F00\3",&sC___101 };
   static COMMANDS sC___103 = {0,"READ",4,"SAVE","ReadModal(GetList)",&sC___102 };
   static COMMANDS sC___104 = {0,"READ",4,"","ReadModal(GetList) ; GetList := {}",&sC___103 };
   static COMMANDS sC___105 = {0,"CLEAR",5,"GETS","ReadKill(.T.) ; GetList := {}",&sC___104 };
   static COMMANDS sC___106 = {0,"@",1,"\2\1A10\3 COLOUR \2\1B10\3","@ \2\1A00\3 COLOR \2\1B00\3",&sC___105 };
   static COMMANDS sC___107 = {0,"SET",3,"WRAP \1A20ON,OFF,&>","Set( _SET_WRAP, \1A30 )",&sC___106 };
   static COMMANDS sC___108 = {0,"SET",3,"WRAP (\1A00)","Set( _SET_WRAP, \1A00 )",&sC___107 };
   static COMMANDS sC___109 = {0,"SET",3,"MESSAGE TO \1A00 \2\1B20 CENTER, CENTRE>\3",
       "Set( _SET_MESSAGE, \1A00 ) ; Set( _SET_MCENTER, \1B50 )",&sC___108 };
   static COMMANDS sC___110 = {0,"SET",3,"MESSAGE TO","Set( _SET_MESSAGE, 0 ) ; Set( _SET_MCENTER, .F. )",&sC___109 };
   static COMMANDS sC___111 = {0,"@",1,"\1A00, \1B00 PROMPT \1C00 \2MESSAGE \1D00\3 \2COLOR \1E00\3",
       "__AtPrompt( \1A00, \1B00, \1C00 , \1D00 , \1E00 )",&sC___110 };
   static COMMANDS sC___112 = {0,"MENU",4,"TO \1A00","\1A00 := __MenuTo( {|_1| if(PCount() == 0, \1A00, \1A00 := _1)}, \1A10 )",&sC___111 };
   static COMMANDS sC___113 = {0,"SAVE",4,"SCREEN","__XSaveScreen()",&sC___112 };
   static COMMANDS sC___114 = {0,"RESTORE",7,"SCREEN","__XRestScreen()",&sC___113 };
   static COMMANDS sC___115 = {0,"SAVE",4,"SCREEN TO \1A00","\1A00 := SaveScreen( 0, 0, Maxrow(), Maxcol() )",&sC___114 };
   static COMMANDS sC___116 = {0,"RESTORE",7,"SCREEN FROM \1A00","RestScreen( 0, 0, Maxrow(), Maxcol(), \1A00 )",&sC___115 };
   static COMMANDS sC___117 = {0,"WAIT",4,"\2\1A00\3","__Wait( \1A00 )",&sC___116 };
   static COMMANDS sC___118 = {0,"WAIT",4,"\2\1A00\3 TO \1B00","\1B00 := __Wait( \1A00 )",&sC___117 };
   static COMMANDS sC___119 = {0,"ACCEPT",6,"\2\1A00\3 TO \1B00","\1B00 := __Accept( \1A00 )",&sC___118 };
   static COMMANDS sC___120 = {0,"INPUT",5,"\2\1A00\3 TO \1B00",
       "if ( !Empty(__Accept(\1A00)) ) ;    \1B00 := &( __AcceptStr() ) ; end",&sC___119 };
   static COMMANDS sC___121 = {0,"KEYBOARD",8,"\1A00","__Keyboard( \1A00 )",&sC___120 };
   static COMMANDS sC___122 = {0,"CLEAR",5,"TYPEAHEAD","__Keyboard()",&sC___121 };
   static COMMANDS sC___123 = {0,"SET",3,"TYPEAHEAD TO \1A00","Set( _SET_TYPEAHEAD, \1A00 )",&sC___122 };
   static COMMANDS sC___124 = {0,"SET",3,"KEY \1A00 TO \1B00","SetKey( \1A00, {|p, l, v| \1B00(p, l, v)} )",&sC___123 };
   static COMMANDS sC___125 = {0,"SET",3,"KEY \1A00 TO \1B00 ( \2\1C10\3 )","SET KEY \1A00 TO \1B00",&sC___124 };
   static COMMANDS sC___126 = {0,"SET",3,"KEY \1A00 TO \1B20&>",
       "if ( Empty(\1B30) ) ;   SetKey( \1A00, NIL ) ; else ;   SetKey( \1A00, {|p, l, v| \1B00(p, l, v)} ) ; end",&sC___125 };
   static COMMANDS sC___127 = {0,"SET",3,"KEY \1A00 \2TO\3","SetKey( \1A00, NIL )",&sC___126 };
   static COMMANDS sC___128 = {0,"SET",3,"FUNCTION \1A00 \2TO\3 \2\1B00\3","__SetFunction( \1A00, \1B00 )",&sC___127 };
   static COMMANDS sC___129 = {0,"CLEAR",5,"MEMORY","__MVClear()",&sC___128 };
   static COMMANDS sC___130 = {0,"RELEASE",7," \1A10","__MVXRelease( \1A30 )",&sC___129 };
   static COMMANDS sC___131 = {0,"RELEASE",7,"ALL","__MVRelease('*', .T.)",&sC___130 };
   static COMMANDS sC___132 = {0,"RELEASE",7,"ALL LIKE \1A00","__MVRelease( \1A10, .T. )",&sC___131 };
   static COMMANDS sC___133 = {0,"RELEASE",7,"ALL EXCEPT \1A00","__MVRelease( \1A10, .F. )",&sC___132 };
   static COMMANDS sC___134 = {0,"RESTORE",7,"\2FROM \1A40\3 \2\1B20 ADDITIVE>\3","__MVRestore( \1A30, \1B50 )",&sC___133 };
   static COMMANDS sC___135 = {0,"SAVE",4,"ALL LIKE \1A00 TO \1B40","__MVSave( \1B30, \1A30, .T. )",&sC___134 };
   static COMMANDS sC___136 = {0,"SAVE",4,"TO \1A40 ALL LIKE \1B00","__MVSave( \1A30, \1B30, .T. )",&sC___135 };
   static COMMANDS sC___137 = {0,"SAVE",4,"ALL EXCEPT \1A00 TO \1B40","__MVSave( \1B30, \1A30, .F. )",&sC___136 };
   static COMMANDS sC___138 = {0,"SAVE",4,"TO \1A40 ALL EXCEPT \1B00","__MVSave( \1A30, \1B30, .F. )",&sC___137 };
   static COMMANDS sC___139 = {0,"SAVE",4,"\2TO \1A40\3 \2ALL\3","__MVSave( \1A30, '*', .T. )",&sC___138 };
   static COMMANDS sC___140 = {0,"ERASE",5,"\1A40","FErase( \1A30 )",&sC___139 };
   static COMMANDS sC___141 = {0,"DELETE",6,"FILE \1A40","FErase( \1A30 )",&sC___140 };
   static COMMANDS sC___142 = {0,"RENAME",6,"\1A40 TO \1B40","FRename( \1A30, \1B30 )",&sC___141 };
   static COMMANDS sC___143 = {0,"COPY",4,"FILE \1A40 TO \1B40","__CopyFile( \1A30, \1B30 )",&sC___142 };
   static COMMANDS sC___144 = {0,"DIR",3,"\2\1A40\3","__Dir( \1A30 )",&sC___143 };
   static COMMANDS sC___145 = {0,"TYPE",4,"\1A40 \2\1B20 TO PRINTER>\3 \2TO FILE \1C40\3",
       "__TypeFile( \1A30, \1B50 ) \2; COPY FILE \1A30 TO \1C30 \3",&sC___144 };
   static COMMANDS sC___146 = {0,"TYPE",4,"\1A40 \2\1B20 TO PRINTER>\3","__TypeFile( \1A30, \1B50 )",&sC___145 };
   static COMMANDS sC___147 = {0,"REQUEST",7,"\1A10","EXTERNAL \1A00",&sC___146 };
   static COMMANDS sC___148 = {0,"CANCEL",6,"","__Quit()",&sC___147 };
   static COMMANDS sC___149 = {0,"QUIT",4,"","__Quit()",&sC___148 };
   static COMMANDS sC___150 = {0,"RUN",3,"\1A30","__Run( \1A10 )",&sC___149 };
   static COMMANDS sC___151 = {0,"RUN",3,"( \1A00 )","__Run( \1A00 )",&sC___150 };
   static COMMANDS sC___152 = {0,"!",1,"\1A30","RUN \1A00",&sC___151 };
   static COMMANDS sC___153 = {0,"RUN",3,"= \1A00","( run := \1A00 )",&sC___152 };
   static COMMANDS sC___154 = {0,"RUN",3,":= \1A00","( run := \1A00 )",&sC___153 };
   static COMMANDS sC___155 = {0,"SET",3,"EXCLUSIVE \1A20ON,OFF,&>","Set( _SET_EXCLUSIVE, \1A30 )",&sC___154 };
   static COMMANDS sC___156 = {0,"SET",3,"EXCLUSIVE (\1A00)","Set( _SET_EXCLUSIVE, \1A00 )",&sC___155 };
   static COMMANDS sC___157 = {0,"SET",3,"SOFTSEEK \1A20ON,OFF,&>","Set( _SET_SOFTSEEK, \1A30 )",&sC___156 };
   static COMMANDS sC___158 = {0,"SET",3,"SOFTSEEK (\1A00)","Set( _SET_SOFTSEEK, \1A00 )",&sC___157 };
   static COMMANDS sC___159 = {0,"SET",3,"UNIQUE \1A20ON,OFF,&>","Set( _SET_UNIQUE, \1A30 )",&sC___158 };
   static COMMANDS sC___160 = {0,"SET",3,"UNIQUE (\1A00)","Set( _SET_UNIQUE, \1A00 )",&sC___159 };
   static COMMANDS sC___161 = {0,"SET",3,"DELETED \1A20ON,OFF,&>","Set( _SET_DELETED, \1A30 )",&sC___160 };
   static COMMANDS sC___162 = {0,"SET",3,"DELETED (\1A00)","Set( _SET_DELETED, \1A00 )",&sC___161 };
   static COMMANDS sC___163 = {0,"SELECT",6,"\1A00","dbSelectArea( \1A30 )",&sC___162 };
   static COMMANDS sC___164 = {0,"SELECT",6,"\1A00(\2\1B10\3)","dbSelectArea( \1A00(\1B00) )",&sC___163 };
   static COMMANDS sC___165 = {0,"USE",3,"","dbCloseArea()",&sC___164 };
   static COMMANDS sC___166 = {0,"USE",3,"\1A40 \2VIA \1B00\3 \2ALIAS \1C00\3 \2\1D20 NEW>\3 \2\1E20 EXCLUSIVE>\3 \2\1F20 SHARED>\3 \2\1G20 READONLY>\3 \2CODEPAGE \1H00\3 \2INDEX \1I40 \2, \1J40\3\3",
       "dbUseArea( \1D50, \1B00, \1A30, \1C30, if(\1F50 .or. \1E50, !\1E50, NIL), \1G50, \1H30 ) \2; dbSetIndex( \1I30 )\3 \2; dbSetIndex( \1J30 )\3",&sC___165 };
   static COMMANDS sC___167 = {0,"APPEND",6,"BLANK","dbAppend()",&sC___166 };
   static COMMANDS sC___168 = {0,"PACK",4,"","__dbPack()",&sC___167 };
   static COMMANDS sC___169 = {0,"ZAP",3,"","__dbZap()",&sC___168 };
   static COMMANDS sC___170 = {0,"UNLOCK",6,"","dbUnlock()",&sC___169 };
   static COMMANDS sC___171 = {0,"UNLOCK",6,"ALL","dbUnlockAll()",&sC___170 };
   static COMMANDS sC___172 = {0,"COMMIT",6,"","dbCommitAll()",&sC___171 };
   static COMMANDS sC___173 = {0,"GOTO",4,"\1A00","dbGoto(\1A00)",&sC___172 };
   static COMMANDS sC___174 = {0,"GO",2,"\1A00","dbGoto(\1A00)",&sC___173 };
   static COMMANDS sC___175 = {0,"GOTO",4,"TOP","dbGoTop()",&sC___174 };
   static COMMANDS sC___176 = {0,"GO",2,"TOP","dbGoTop()",&sC___175 };
   static COMMANDS sC___177 = {0,"GOTO",4,"BOTTOM","dbGoBottom()",&sC___176 };
   static COMMANDS sC___178 = {0,"GO",2,"BOTTOM","dbGoBottom()",&sC___177 };
   static COMMANDS sC___179 = {0,"SKIP",4,"","dbSkip(1)",&sC___178 };
   static COMMANDS sC___180 = {0,"SKIP",4,"\1A00","dbSkip( \1A00 )",&sC___179 };
   static COMMANDS sC___181 = {0,"SKIP",4,"ALIAS \1A00","\1A00 -> ( dbSkip(1) )",&sC___180 };
   static COMMANDS sC___182 = {0,"SKIP",4,"\1A00 ALIAS \1B00","\1B00 -> ( dbSkip(\1A00) )",&sC___181 };
   static COMMANDS sC___183 = {0,"SEEK",4,"\1A00 \2\1B20 SOFTSEEK>\3","dbSeek( \1A00, if( \1B50, .T., NIL ) )",&sC___182 };
   static COMMANDS sC___184 = {0,"FIND",4,"\1A30","dbSeek( \1A30 )",&sC___183 };
   static COMMANDS sC___185 = {0,"FIND",4,":= \1A00","( find := \1A00 )",&sC___184 };
   static COMMANDS sC___186 = {0,"FIND",4,"= \1A00","( find := \1A00 )",&sC___185 };
   static COMMANDS sC___187 = {0,"CONTINUE",8,"","__dbContinue()",&sC___186 };
   static COMMANDS sC___188 = {0,"LOCATE",6,"\2FOR \1A00\3 \2WHILE \1B00\3 \2NEXT \1C00\3 \2RECORD \1D00\3 \2\1E20REST>\3 \2ALL\3",
       "__dbLocate(\1A40,\1B40,\1C00,\1D00,\1E50)",&sC___187 };
   static COMMANDS sC___189 = {0,"SET",3,"RELATION TO","dbClearRelation()",&sC___188 };
   static COMMANDS sC___190 = {0,"SET",3,"RELATION \2\1A20 ADDITIVE>\3 \2TO \1C00 INTO \1D40 \2\1B20 SCOPED>\3 \2,\2TO\3 \1E00 INTO \1F40 \2\1G20 SCOPED>\3\3\3",
       "if (!\1A50 ) ; dbClearRelation() ; end ; dbSetRelation(\1D30,\1C40,\1C20,\1B50 ) \2; dbSetRelation(\1F30,\1E40,\1E20,\1G50 )\3",&sC___189 };
   static COMMANDS sC___191 = {0,"SET",3,"FILTER TO","dbClearFilter(NIL)",&sC___190 };
   static COMMANDS sC___192 = {0,"SET",3,"FILTER TO \1A00","dbSetFilter( \1A40, \1A20 )",&sC___191 };
   static COMMANDS sC___193 = {0,"SET",3,"FILTER TO \1A20&>",
       "if ( Empty(\1A30) ) ;    dbClearFilter() ; else ;    dbSetFilter(\1A40,\1A30) ; end",&sC___192 };
   static COMMANDS sC___194 = {0,"REPLACE",7,"\2 \1A00 WITH \1B00 \2, \1C00 WITH \1D00\3 \3 \2FOR \1E00\3 \2WHILE \1F00\3 \2NEXT \1G00\3 \2RECORD \1H00\3 \2\1I20REST>\3 \2ALL\3",
       "DBEval( {|| _FIELD->\1A00 := \1B00 \2,_FIELD->\1C00 := \1D00\3},\1E40,\1F40,\1G00,\1H00,\1I50)",&sC___193 };
   static COMMANDS sC___195 = {0,"REPLACE",7,"\1A00 WITH \1B00 \2, \1C00 WITH \1D00 \3",
       "_FIELD->\1A00 := \1B00 \2; _FIELD->\1C00 := \1D00\3",&sC___194 };
   static COMMANDS sC___196 = {0,"DELETE",6,"\2FOR \1A00\3 \2WHILE \1B00\3 \2NEXT \1C00\3 \2RECORD \1D00\3 \2\1E20REST>\3 \2ALL\3",
       "DBEval( {|| dbDelete()}, \1A40, \1B40, \1C00, \1D00, \1E50 )",&sC___195 };
   static COMMANDS sC___197 = {0,"RECALL",6,"\2FOR \1A00\3 \2WHILE \1B00\3 \2NEXT \1C00\3 \2RECORD \1D00\3 \2\1E20REST>\3 \2ALL\3",
       "DBEval( {|| dbRecall()}, \1A40, \1B40, \1C00, \1D00, \1E50 )",&sC___196 };
   static COMMANDS sC___198 = {0,"DELETE",6,"","dbDelete()",&sC___197 };
   static COMMANDS sC___199 = {0,"RECALL",6,"","dbRecall()",&sC___198 };
   static COMMANDS sC___200 = {0,"CREATE",6,"\1A40 \2FROM \1B40\3 \2VIA \1C00\3 \2ALIAS \1D00\3 \2\1E20 NEW>\3 \2CODEPAGE \1F00\3",
       "__dbCreate( \1A30, \1B30, \1C00, \1E50, \1D30, \1F30 )",&sC___199 };
   static COMMANDS sC___201 = {0,"COPY",4,"\2STRUCTURE\3 \2EXTENDED\3 \2TO \1A40\3","__dbCopyXStruct( \1A30 )",&sC___200 };
   static COMMANDS sC___202 = {0,"COPY",4,"\2STRUCTURE\3 \2TO \1A40\3 \2FIELDS \1B10\3","__dbCopyStruct( \1A30, { \1B30 } )",&sC___201 };
   static COMMANDS sC___203 = {0,"COPY",4,"\2TO \1A40\3 \2DELIMITED \2WITH \1B30\3\3 \2FIELDS \1C10\3 \2FOR \1D00\3 \2WHILE \1E00\3 \2NEXT \1F00\3 \2RECORD \1G00\3 \2\1H20REST>\3 \2ALL\3",
       "__dbDelim( .T., \1A30, \1B30, { \1C30 }, \1D40, \1E40, \1F00, \1G00, \1H50 )",&sC___202 };
   static COMMANDS sC___204 = {0,"COPY",4,"\2TO \1A40\3 \2SDF\3 \2FIELDS \1B10\3 \2FOR \1C00\3 \2WHILE \1D00\3 \2NEXT \1E00\3 \2RECORD \1F00\3 \2\1G20REST>\3 \2ALL\3",
       "__dbSDF( .T., \1A30, { \1B30 }, \1C40, \1D40, \1E00, \1F00, \1G50 )",&sC___203 };
   static COMMANDS sC___205 = {0,"COPY",4,"\2TO \1A40\3 \2FIELDS \1B10\3 \2FOR \1C00\3 \2WHILE \1D00\3 \2NEXT \1E00\3 \2RECORD \1F00\3 \2\1G20REST>\3 \2VIA \1H00\3 \2ALL\3",
       "__dbCopy( \1A30, { \1B30 }, \1C40, \1D40, \1E00, \1F00, \1G50, \1H00 )",&sC___204 };
   static COMMANDS sC___206 = {0,"APPEND",6,"\2FROM \1A40\3 \2DELIMITED \2WITH \1B30\3\3 \2FIELDS \1C10\3 \2FOR \1D00\3 \2WHILE \1E00\3 \2NEXT \1F00\3 \2RECORD \1G00\3 \2\1H20REST>\3 \2ALL\3",
       "__dbDelim( .F., \1A30, \1B30, { \1C30 }, \1D40, \1E40, \1F00, \1G00, \1H50 )",&sC___205 };
   static COMMANDS sC___207 = {0,"APPEND",6,"\2FROM \1A40\3 \2SDF\3 \2FIELDS \1B10\3 \2FOR \1C00\3 \2WHILE \1D00\3 \2NEXT \1E00\3 \2RECORD \1F00\3 \2\1G20REST>\3 \2ALL\3",
       "__dbSDF( .F., \1A30, { \1B30 }, \1C40, \1D40, \1E00, \1F00, \1G50 )",&sC___206 };
   static COMMANDS sC___208 = {0,"APPEND",6,"\2FROM \1A40\3 \2FIELDS \1B10\3 \2FOR \1C00\3 \2WHILE \1D00\3 \2NEXT \1E00\3 \2RECORD \1F00\3 \2\1G20REST>\3 \2VIA \1H00\3 \2ALL\3",
       "__dbApp( \1A30, { \1B30 }, \1C40, \1D40, \1E00, \1F00, \1G50, \1H00 )",&sC___207 };
   static COMMANDS sC___209 = {0,"SORT",4,"\2TO \1A40\3 \2ON \1B10\3 \2FOR \1C00\3 \2WHILE \1D00\3 \2NEXT \1E00\3 \2RECORD \1F00\3 \2\1G20REST>\3 \2ALL\3",
       "__dbSort( \1A30, { \1B30 }, \1C40, \1D40, \1E00, \1F00, \1G50 )",&sC___208 };
   static COMMANDS sC___210 = {0,"TOTAL",5,"\2TO \1A40\3 \2ON \1B00\3 \2FIELDS \1C10\3 \2FOR \1D00\3 \2WHILE \1E00\3 \2NEXT \1F00\3 \2RECORD \1G00\3 \2\1H20REST>\3 \2ALL\3",
       "__dbTotal( \1A30, \1B40, { \1C30 }, \1D40, \1E40, \1F00, \1G00, \1H50 )",&sC___209 };
   static COMMANDS sC___211 = {0,"UPDATE",6,"\2FROM \1A40\3 \2ON \1B00\3 \2REPLACE \1C00 WITH \1D00 \2, \1E00 WITH \1F00\3\3 \2\1G20RANDOM>\3",
       "__dbUpdate( \1A30, \1B40, \1G50, {|| _FIELD->\1C00 := \1D00 \2, _FIELD->\1E00 := \1F00\3} )",&sC___210 };
   static COMMANDS sC___212 = {0,"JOIN",4,"\2WITH \1A40\3 \2TO \1B00\3 \2FIELDS \1C10\3 \2FOR \1D00\3",
       "__dbJoin( \1A30, \1B30, { \1C30 }, \1D40 )",&sC___211 };
   static COMMANDS sC___213 = {0,"COUNT",5,"\2TO \1A00\3 \2FOR \1B00\3 \2WHILE \1C00\3 \2NEXT \1D00\3 \2RECORD \1E00\3 \2\1F20REST>\3 \2ALL\3",
       "\1A00 := 0 ; DBEval( {|| \1A00++}, \1B40, \1C40, \1D00, \1E00, \1F50 )",&sC___212 };
   static COMMANDS sC___214 = {0,"SUM",3,"\2 \1A00 \2, \1B00\3  TO  \1C00 \2, \1D00\3 \3 \2FOR \1E00\3 \2WHILE \1F00\3 \2NEXT \1G00\3 \2RECORD \1H00\3 \2\1I20REST>\3 \2ALL\3",
       "\1C00 := \2 \1D00 := \3 0 ; DBEval( {|| \1C00 += \1A00 \2, \1D00 += \1B00 \3}, \1E40, \1F40, \1G00, \1H00, \1I50 )",&sC___213 };
   static COMMANDS sC___215 = {0,"AVERAGE",7,"\2 \1A00 \2, \1B00\3  TO  \1C00 \2, \1D00\3 \3 \2FOR \1E00\3 \2WHILE \1F00\3 \2NEXT \1G00\3 \2RECORD \1H00\3 \2\1I20REST>\3 \2ALL\3",
       "M->__Avg := \1C00 := \2 \1D00 := \3 0 ; DBEval( {|| M->__Avg := M->__Avg + 1, \1C00 := \1C00 + \1A00 \2, \1D00 := \1D00 + \1B00\3 }, \1E40, \1F40, \1G00, \1H00, \1I50 ) ; \1C00 := \1C00 / M->__Avg \2; \1D00 := \1D00 / M->__Avg \3",&sC___214 };
   static COMMANDS sC___216 = {0,"LIST",4,"\2\1A10\3 \2\1B20OFF>\3 \2\1C20 TO PRINTER>\3 \2TO FILE \1D40\3 \2FOR \1E00\3 \2WHILE \1F00\3 \2NEXT \1G00\3 \2RECORD \1H00\3 \2\1I20REST>\3 \2ALL\3",
       "__dbList( \1B50, { \1A40 }, .T., \1E40, \1F40, \1G00, \1H00, \1I50, \1C50, \1D30 )",&sC___215 };
   static COMMANDS sC___217 = {0,"DISPLAY",7,"\2\1A10\3 \2\1B20OFF>\3 \2\1C20 TO PRINTER>\3 \2TO FILE \1D40\3 \2FOR \1E00\3 \2WHILE \1F00\3 \2NEXT \1G00\3 \2RECORD \1H00\3 \2\1I20REST>\3 \2\1J20ALL>\3",
       "__DBList( \1B50, { \1A40 }, \1J50, \1E40, \1F40, \1G00, \1H00, \1I50, \1C50, \1D30 )",&sC___216 };
   static COMMANDS sC___218 = {0,"REPORT",6,"FORM \1A00 \2HEADING \1B00\3 \2\1C20 PLAIN>\3 \2\1D20 NOEJECT>\3 \2\1E20 SUMMARY>\3 \2\1F20 NOCONSOLE>\3 \2\1G20 TO PRINTER>\3 \2TO FILE \1H40\3 \2FOR \1I00\3 \2WHILE \1J00\3 \2NEXT \1K00\3 \2RECORD \1L00\3 \2\1M20REST>\3 \2ALL\3",
       "__ReportForm( \1A30, \1G50, \1H30, \1F50, \1I40, \1J40, \1K00, \1L00, \1M50, \1C50, \1B00, \1D50, \1E50 )",&sC___217 };
   static COMMANDS sC___219 = {0,"LABEL",5,"FORM \1A00 \2\1B20 SAMPLE>\3 \2\1C20 NOCONSOLE>\3 \2\1D20 TO PRINTER>\3 \2TO FILE \1E40\3 \2FOR \1F00\3 \2WHILE \1G00\3 \2NEXT \1H00\3 \2RECORD \1I00\3 \2\1J20REST>\3 \2ALL\3",
       "__LabelForm( \1A30, \1D50, \1E30, \1C50, \1F40, \1G40, \1H00, \1I00, \1J50, \1B50 )",&sC___218 };
   static COMMANDS sC___220 = {0,"CLOSE",5,"\1A00","\1A00->( dbCloseArea() )",&sC___219 };
   static COMMANDS sC___221 = {0,"CLOSE",5,"","dbCloseArea()",&sC___220 };
   static COMMANDS sC___222 = {0,"CLOSE",5,"DATABASES","dbCloseAll()",&sC___221 };
   static COMMANDS sC___223 = {0,"CLOSE",5,"ALTERNATE","Set(_SET_ALTFILE, "")",&sC___222 };
   static COMMANDS sC___224 = {0,"CLOSE",5,"FORMAT","__SetFormat(NIL)",&sC___223 };
   static COMMANDS sC___225 = {0,"CLOSE",5,"INDEXES","dbClearIndex()",&sC___224 };
   static COMMANDS sC___226 = {0,"CLOSE",5,"PROCEDURE",NULL,&sC___225 };
   static COMMANDS sC___227 = {0,"CLOSE",5,"ALL","CLOSE DATABASES ; SELECT 1 ; CLOSE FORMAT",&sC___226 };
   static COMMANDS sC___228 = {0,"CLEAR",5,"","CLEAR SCREEN ; CLEAR GETS",&sC___227 };
   static COMMANDS sC___229 = {0,"CLEAR",5,"ALL",
       "CLOSE DATABASES ; CLOSE FORMAT ; CLEAR MEMORY ; CLEAR GETS ; SET ALTERNATE OFF ; SET ALTERNATE TO",&sC___228 };
   static COMMANDS sC___230 = {0,"INDEX",5,"ON \1A00 \2TAG \1B40 \3 TO \1C40 \2FOR \1D00\3 \2\1E20ALL>\3 \2WHILE \1F00\3 \2NEXT \1G00\3 \2RECORD \1H00\3 \2\1I20REST>\3 \2EVAL \1J00\3 \2EVERY \1K00\3 \2\1L20 UNIQUE>\3 \2\1M20 ASCENDING>\3 \2\1N20 DESCENDING>\3 \2\1O20 USECURRENT>\3 \2\1P20 ADDITIVE>\3 \2\1R20 CUSTOM>\3 \2\1S20 NOOPTIMIZE>\3",
       "ordCondSet( \1D20, \1D40, \2\1E50\3, \1F40, \1J40, \1K00, RECNO(), \1G00, \1H00, \2\1I50\3, \2\1N50\3,, \2\1P50\3, \2\1O50\3, \2\1R50\3, \2\1S50\3, \1F20 ) ;  ordCreate(\1C30, \1B30, \1A20, \1A40, \2\1L50\3 )",&sC___229 };
   static COMMANDS sC___231 = {0,"INDEX",5,"ON \1A00 TAG \1B40 \2TO \1C40\3 \2FOR \1D00\3 \2\1E20ALL>\3 \2WHILE \1F00\3 \2NEXT \1G00\3 \2RECORD \1H00\3 \2\1I20REST>\3 \2EVAL \1J00\3 \2EVERY \1K00\3 \2\1L20 UNIQUE>\3 \2\1M20 ASCENDING>\3 \2\1N20 DESCENDING>\3 \2\1O20 USECURRENT>\3 \2\1P20 ADDITIVE>\3 \2\1R20 CUSTOM>\3 \2\1S20 NOOPTIMIZE>\3",
       "ordCondSet( \1D20, \1D40, \2\1E50\3, \1F40, \1J40, \1K00, RECNO(), \1G00, \1H00, \2\1I50\3, \2\1N50\3,, \2\1P50\3, \2\1O50\3, \2\1R50\3, \2\1S50\3, \1F20 ) ;  ordCreate(\1C30, \1B30, \1A20, \1A40, \2\1L50\3 )",&sC___230 };
   static COMMANDS sC___232 = {0,"INDEX",5,"ON \1A00 TO \1B40 \2\1C20 UNIQUE>\3",
       "dbCreateIndex( \1B30, \1A20, \1A40, if( \1C50, .T., NIL ) )",&sC___231 };
   static COMMANDS sC___233 = {0,"DELETE",6,"TAG \1A40 \2 IN \1B40 \3 \2, \1C40 \2 IN \1D40 \3 \3",
       "ordDestroy( \1A30, \1B30 ) \2; ordDestroy( \1C30, \1D30 ) \3",&sC___232 };
   static COMMANDS sC___234 = {0,"REINDEX",7,"\2EVAL \1A00\3 \2EVERY \1B00\3",
       "ordCondSet(,,,, \1A40, \1B00,,,,,,,) ;  ordListRebuild()",&sC___233 };
   static COMMANDS sC___235 = {0,"REINDEX",7,"","ordListRebuild()",&sC___234 };
   static COMMANDS sC___236 = {0,"SET",3,"INDEX TO \2 \1A40 \2, \1B40\3\3 \2\1C20 ADDITIVE>\3",
       "if !\1C50 ; ordListClear() ; end \2; ordListAdd( \1A30 )\3 \2; ordListAdd( \1B30 )\3",&sC___235 };
   static COMMANDS sC___237 = {0,"SET",3,"ORDER TO \1A00 \2IN \1B40\3","ordSetFocus( \1A00 \2, \1B30\3 )",&sC___236 };
   static COMMANDS sC___238 = {0,"SET",3,"ORDER TO TAG \1A40 \2IN \1B40\3","ordSetFocus( \1A30 \2, \1B30\3 )",&sC___237 };
   static COMMANDS sC___239 = {0,"SET",3,"ORDER TO","ordSetFocus(0)",&sC___238 };
   static COMMANDS sC___240 = {0,"SET",3,"EVENTMASK TO \1A00","Set( _SET_EVENTMASK, \1A00 )",&sC___239 };
   static COMMANDS sC___241 = {0,"SET",3,"OPTIMIZE \1A20ON,OFF,&>","Set( _SET_OPTIMIZE, \1A30 )",&sC___240 };
   static COMMANDS sC___242 = {0,"SET",3,"OPTIMIZE (\1A00)","Set( _SET_OPTIMIZE, \1A00 )",&sC___241 };
   static COMMANDS sC___243 = {0,"SET",3,"AUTOPEN \1A20 ON,OFF,&>","Set(_SET_AUTOPEN,\1A30 )",&sC___242 };
   static COMMANDS sC___244 = {0,"SET",3,"AUTOPEN (\1A00)","Set(_SET_AUTOPEN,\1A00 )",&sC___243 };
   static COMMANDS sC___245 = {0,"SET",3,"FILECASE \1A30","Set(_SET_FILECASE, \1A10 )",&sC___244 };
   static COMMANDS sC___246 = {0,"SET",3,"FILECASE ( \1A00 )","Set(_SET_FILECASE, \1A00 )",&sC___245 };
   static COMMANDS sC___247 = {0,"SET",3,"DIRCASE \1A30","Set(_SET_DIRCASE, \1A10 )",&sC___246 };
   static COMMANDS sC___248 = {0,"SET",3,"DIRCASE ( \1A00 )","Set(_SET_DIRCASE, \1A00 )",&sC___247 };
   static COMMANDS sC___249 = {0,"SET",3,"DIRSEPARATOR \1A30","Set(_SET_DIRSEPARATOR, \1A10 )",&sC___248 };
   static COMMANDS sC___250 = {0,"SET",3,"DIRSEPARATOR ( \1A00 )","Set(_SET_DIRSEPARATOR, \1A00 )",&sC___249 };
   static COMMANDS sC___251 = {0,"SET",3,"MBLOCKSIZE TO \1A00","Set( _SET_MBLOCKSIZE, \1A00 )",&sC___250 };
   static COMMANDS sC___252 = {0,"SET",3,"MEMOBLOCK TO \1A00","Set( _SET_MBLOCKSIZE, \1A00 )",&sC___251 };
   static COMMANDS sC___253 = {0,"SET",3,"MFILEEXT TO \1A00","Set( _SET_MFILEEXT, \1A00 )",&sC___252 };
   static COMMANDS sC___254 = {0,"SET",3,"AUTOSHARE TO \1A00","Set( _SET_AUTOSHARE, \1A00 )",&sC___253 };
   static COMMANDS sC___255 = {0,"SET",3,"AUTOSHARE TO","Set( _SET_AUTOSHARE, 0 )",&sC___254 };
   static COMMANDS sC___256 = {0,"SET",3,"AUTORDER TO \1A00","Set( _SET_AUTORDER, \1A00 )",&sC___255 };
   static COMMANDS sC___257 = {0,"SET",3,"AUTORDER TO","Set( _SET_AUTORDER, 0 )",&sC___256 };
   static COMMANDS sC___258 = {0,"SET",3,"STRICTREAD \1A20 ON,OFF,&>","Set(_SET_STRICTREAD,\1A30 )",&sC___257 };
   static COMMANDS sC___259 = {0,"SET",3,"STRICTREAD (\1A00)","Set(_SET_STRICTREAD,\1A00 )",&sC___258 };
   static COMMANDS sC___260 = {0,"SET",3,"HARDCOMMIT \1A20 ON,OFF,&>","Set(_SET_HARDCOMMIT,\1A30 )",&sC___259 };
   static COMMANDS sC___261 = {0,"SET",3,"HARDCOMMIT (\1A00)","Set(_SET_HARDCOMMIT,\1A00 )",&sC___260 };
   static COMMANDS sC___262 = {0,"SET",3,"DBFLOCKSCHEME TO \1A00","Set(_SET_DBFLOCKSCHEME, \1A00 )",&sC___261 };
   static COMMANDS sC___263 = {0,"SET",3,"DBFLOCKSCHEME TO","Set(_SET_DBFLOCKSCHEME, 0 )",&sC___262 };

   hb_pp_topDefine = &sD___60;
   hb_pp_topCommand = &sC___263;
   hb_pp_topTranslate = NULL;
}
