/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DEFAULT.CH some default definition to HTMLLIB
 *
 * Copyright 2000 Manos Aspradakis <maspr@otenet.gr>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */
/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 *    Porting this library to Harbour
 *
 * See doc/license.txt for licensing terms.
 *
 */

#ifndef _DEFAULT_CH_

//#ifndef _BOX_CH
//#include "box.ch"
//#endif

#ifndef _INKEY_CH
#include "inkey.ch"
#endif

#ifndef _COMMON_CH
#include "common.ch"
#endif

#xTranslate PERCENT( <nVal>, <nPCent> ) => ;
            ( ( <nVal> * <nPCent> ) / 100 )

// --> Default parameters
#xcommand DEFAULT <uVar1> := <uVal1> ;
               [, <uVarN> := <uValN> ] => ;
                  <uVar1> := IIf( <uVar1> == nil, <uVal1>, <uVar1> ) ;;
                [ <uVarN> := IIf( <uVarN> == nil, <uValN>, <uVarN> ); ]

#xcommand DEFAULT <v1> TO <x1> [, <vn> TO <xn> ]                        ;
          =>                                                            ;
          IF <v1> == NIL ; <v1> := <x1> ; END                           ;
          [; IF <vn> == NIL ; <vn> := <xn> ; END ]

// --> OOPs
#xtranslate BYNAME <V> [, <VN> ]     => ::<V> := <V> [; ::<VN> := <VN> ]
#xtranslate BYNAME <V> DEFAULT <Val> => ::<V> := BYDEFAULT <V>, <Val>
#xtranslate BYDEFAULT <V>, <Val>     => IIF( <V> == NIL, <Val>, <V> )


// --> Save/Restore video state...
#xtranslate SaveState()         => { row(), ;
                                     col(), ;
                                     SetColor(), ;
                                     SetCursor(), ;
                                     SaveScreen(,,,,) }


#xtranslate RestState(<a>)      =>   DispBegin()                 ;;
                                     RestScreen(,,,,<a>\[\5])    ;;
                                     SetColor(<a>\[\3] )         ;;
                                     SetCursor(<a>\[\4])         ;;
                                     SetPos(<a>\[\1], <a>\[\2] ) ;;
                                     DispEnd()


// --> Save/Restore Table state
#xTranslate DbSaveState()        =>  IIF( USED(),;
                                         { Select(),     ;
                                           Recno(),      ;
                                           OrdBagName(0),;
                                           OrdSetFocus() ;
                                         },;
                                         NIL )

#xTranslate DbRestState( <a> )   =>   IIF( <a> != NIL, ;
                                        ( Select( <a>\[\1] ),    ;
                                          OrdListAdd(<a>\[\3] ), ;
                                          OrdSetFocus(<a>\[\4] ), ;
                                          DbGoto(<a>\[\2] )),)


// --> Display a Message at MAXROW() with optional colour...
#xTranslate Message(<cMsg>,<c>) => ;
            DispOutAt( maxrow(), 0, PadC(<cMsg>, MaxCol()+1 ), ;
                       IIF( EMPTY(#<c>), "R/W", #<c> ) )


// --> Display a backdrop desktop with optional color
#xTranslate DeskTop([<c>])      => ;
            DispBox( 0,0,maxrow(),maxcol(), replicate("±",9), [<c>] )


// --> Display a box with shadow (without savescreen() )
#xTranslate ShadBox( <nTR>, <nTC>, <nBR>, <nBC>, <cStyle>, <cClrs> ) ;
            => ;
               DispBegin() ;;
               RESTSCREEN( <nTR>+1,<nTC>+2,<nBR>+1,<nBC>+2,;
                           TRANSFORM( ;
                           SAVESCREEN( <nTR>+1,<nTC>+2,<nBR>+1,<nBC>+2 ),;
                           REPLICATE( 'X', ( <nBR>-<nTR>+1 ) * ( <nBC>-<nTC>+1 ) ) ) );;
               DispBox( <nTR>, <nTC>, <nBR>, <nBC>, [<cStyle>], [<cClrs>] );;
               SetPos( <nTR>+1, <nTC>+1 );;
               DispEnd()


// --> Display a box with shadow: Saves screen for WClose()
// --> *MUST* pass to variable, e.g.
// -->
// --> LOCAL aWin := WOpen( 10, 10, 20, 30, B_DOUBLE + " ", "N/W" )
// -->
#xTranslate WOpen( <nTR>, <nTC>, <nBR>, <nBC>, [<cStyle>], [<cClrs>] ) ;
            => ;
               { <nTR>, <nTC>, <nBR>+1, <nBC>+2, ;
                 SAVESCREEN( <nTR>, <nTC>, <nBR>+1, <nBC>+2 ) } ;;
               DispBegin() ;;
               RESTSCREEN( <nTR>+1,<nTC>+2,<nBR>+1,<nBC>+2,;
                           TRANSFORM( SAVESCREEN( <nTR>+1,<nTC>+2,<nBR>+1,<nBC>+2 ),;
                           REPLICATE( 'X', ( <nBR>-<nTR>+1 ) * ( <nBC>-<nTC>+1 ) ) ) );;
               DispBox( <nTR>, <nTC>, <nBR>, <nBC>, ;
                        IIF(EMPTY(#<cStyle>), "ÚÄ¿³ÙÄÀ³ ", <cStyle> ), ;
                        IIF(EMPTY(#<cClrs>),"W/B", <cClrs> ) );;
               SetPos( <nTR>+1, <nTC>+1 );;
               DispEnd()


// --> Display a Caption for a WOpen() window
#xTranslate  WTitle( <aWin>, <cTitle>, <cClr> )  =>   ;
                   DispBox( <aWin>\[1\], <aWin>\[2\], ;
                            <aWin>\[1\], <aWin>\[4\]-2, ;
                            replicate(" ",9),         ;
                            IIF( EMPTY(#<cClr>), "b/w", #<cClr> ) ) ;;
                   DispOutAt( <aWin>\[1\], <aWin>\[2\], ;
                              PADC(<cTitle>, (<aWin>\[4\]-<aWin>\[2\])-1, " " ), ;
                              IIF( EMPTY(#<cClr>), "b/w", #<cClr> ))

// --> Closes a window created with WOpen() - Restores screen
#xTranslate  WClose(<aWin>)     => RestScreen( <aWin>\[1\], <aWin>\[2\], ;
                                               <aWin>\[3\], <aWin>\[4\], ;
                                               <aWin>\[5\] )


// --> Save/Restore full screen - *MUST* pass to/from var
#xtranslate ScreenSave()        => SAVESCREEN( 0, 0, 24, 79 )
#xtranslate ScreenRest( <c> )   => RESTSCREEN( 0, 0, 24, 79, <c> )

// --> Build a Picture template
#xtranslate CAPFIRST(<foo>)     => ( "!" + REPLICATE( "X", LEN( <foo> ) -1  ))

// --> Array shrink
#xTranslate ASHRINK( <array> )  => ;
            ADEL ( <array>, LEN( <array> ) )        ;
         ;  ASIZE( <array>, LEN( <array> ) - 1 )


// --> Number to Trimmed String
#xTranslate NTRIM( <nNum> )     => LTRIM(STR( <nNum> ))
#xTranslate NUMTRIM( <nNum> )     => LTRIM(STR( <nNum> ))
// --> Convert logical to character
#xtranslate LTOC(<l>)           => IIF( <l>, "T", "F")

// --> Convert character to logical
#xTranslate CTOL(<c>)           => IIF( <c> $ "TtYy", .T., .F.)


// --> Left trim a numeric
#xtranslate LSTR(<n>)           => LTRIM( Str( <n> ) )


// --> Carriage Return + Line Feed
#xtranslate  CRLF(<str>)        => ( <str> + CHR(13)+CHR(10) )
#xtranslate  CRLF()             =>  CHR(13) + CHR(10)


// --> create a Get/Set Block
#define GSB( xVar)  {|x| IIF(x == NIL, xVar, xVar := x )}

#xtranslate GetSetBlock(<xVar>) => {|x| IIF(x == NIL, <xVar>, <xVar> := x )}

#translate GETSET( <xVal>, <xParm> ) => ;
           <xVal> := IIF( <xParm> == NIL, <xVal>, <xVal> := <xParm>)


// --> Convert Character String to Code Block
#xTranslate COMPILE(<c>)        => &("{||" + <c> + "}")


// --> Errors...
#define Beep() Tone(300,3)

#xTranslate ErrorTone()         => ( TONE( 1000,.01), ;
                                     TONE( 1400,.01), ;
                                     TONE( 1800,.01)  ;
                                   )


#define DEF_PATH  SET(_SET_DEFAULT)


// --> GETs ...
//#define OK_GETS() LASTKEY() != K_ESC .AND. UPDATED()



/*
  DATA TYPES
*/
#xtranslate IS_ARRAY(<foo>) => (VALTYPE(<foo>)=="A")
#xtranslate IS_BLOCK(<foo>) => (VALTYPE(<foo>)=="B")
#xtranslate IS_CHAR(<foo>)  => (VALTYPE(<foo>)=="C")
#xtranslate IS_DATA(<foo>)  => (<foo> \>=32 .AND. <foo> \<= 253)
#xtranslate IS_DATE(<foo>)  => (VALTYPE(<foo>)=="D")
#xtranslate IS_DEF(<foo>)   => !(TYPE(<foo>) $ "UE")
#xtranslate IS_DIGIT(<foo>) => ISDIGIT(<foo>)
#xtranslate IS_INT(<foo>)   => (<foo>)==INT(<foo>) )
#xtranslate IS_LOGIC(<foo>) => (VALTYPE(<foo>)=="L")
#xtranslate IS_MEMO(<foo>)  => (VALTYPE(<foo>)=="M")
#xtranslate IS_NUM(<foo>)   => (VALTYPE(<foo>)=="N")
#xtranslate IS_OBJECT(<foo>)=> (VALTYPE(<foo>)=="O")
#xtranslate IS_TIME(<foo>)  => (VAL(LEFT <foo>,2)) \< 24 .AND. ;
                                VAL(SUBSTR(<foo>,4,2)) \< 60 .AND. ;
                                VAL(RIGHT(<foo>,2 )) \<60 )

#translate ISNIL( <v1> )         => ( <v1> == NIL )
#translate ISARRAY( <v1> )       => ( valtype( <v1> ) == "A" )
#translate ISBLOCK( <v1> )       => ( valtype( <v1> ) == "B" )
#translate ISCHARACTER( <v1> )   => ( valtype( <v1> ) == "C" )
#translate ISDATE( <v1> )        => ( valtype( <v1> ) == "D" )
#translate ISLOGICAL( <v1> )     => ( valtype( <v1> ) == "L" )
#translate ISMEMO( <v1> )        => ( valtype( <v1> ) == "M" )
#translate ISNUMBER( <v1> )      => ( valtype( <v1> ) == "N" )
#translate ISOBJECT( <v1> )      => ( valtype( <v1> ) == "O" )


#command  REPEAT    =>   DO WHILE .T.
#command  UNTIL <*lexpr*>          =>   IF (<lexpr>); EXIT ; END ; ENDDO

#command  IF <lexpr> THEN <*statement*>  =>;
          IIF(<lexpr>) ; <statement> ; END

#command IF <lexpr> THEN <statement1> ELSE <statement2> =>;
         IIF(<lexpr>) ; <statement1> ; ELSE ; <statement2> ; END

#define _DEFAULT_CH_
#endif
