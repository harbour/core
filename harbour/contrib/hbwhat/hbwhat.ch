/*
 * $Id$
 */

#include "common.ch"

#xCommand ? ? < x > = > OutputDebugString( asString( < x > ) )
#xCommand ? < x > = > OutputDebugString( asString( < x > ) + chr( 13 ) )

#Xtranslate a2bin(<aBuff>,<atypes>) => Array2Bin( <aBuff>, #<atypes> )
#Xtranslate bin2a(<cBuff>,<atypes>) => Bin2Array( <cBuff>, #<atypes> )

#ifndef TRUE
#Define TRUE                .T.
#endif

#ifndef FALSE
#Define FALSE               .F.
#endif

#Define CRLF               CHR(13) + CHR(10)
#Define CR                 CHR(13)   //   Carriage return
#define TAB                CHR(9)   //   Tab

#xtranslate MAKELONG(<nLow>, <nHigh>)   => ((<nLow>) + (<nHigh>) * 65536)
#xtranslate MAKELPARAM(<nLow>, <nHigh>) => ((<nLow>) + (<nHigh>) * 65536)
#xtranslate MAKEWORD(<nLow>, <nHigh>)   => ((<nLow>) + (<nHigh>) * 256)

#xcommand DEFAULT <v> TO <x> [, <vN> TO <xN>]            ;
       => IF <v> == nil ; <v> := <x> ; END            ;
          [; IF <vN> == nil ; <vN> := <xN> ; END]


//////////////////
// WinGet command
//////////////////


#Command @ DIALOG < hWndDlg > ;
ID < nId > ;
GET < Var > ;
[ PICTURE < pic > ] ;
[ VALID < valid > ] ;
[ WHEN < when > ] ;
[ COLOR < cColorSpec > ] ;
[ < lAllowOverstrike: OVERSTRIKE > ] ;
[ < lWantEnter: LIKEDOS > ] ;
[ IN < oObj > ] ;
[ FONT < hFont > ] ;
[ < lReadOnly: READONLY > ] ;
= > aAdd( ;
          GetList, ;
          [< oObj > := ] WinGet( GetDlgItem( < hWndDlg > , < nId > ) , < Var > , < pic > , < { when } > , < { valid } > , < cColorSpec > , < .lAllowOverstrike. > , < .lWantEnter. >, <hFont>, < .lReadOnly. > ) ;
        )

        
////////////////////////////////////////////        
// Variable type identifier pseudo-functions
////////////////////////////////////////////

#translate ISCHAR( <v1> )      => ( hb_isString( <v1> ) )
#translate ISSTRING( <v1> )    => ( hb_isString( <v1> ) )
#translate ISNUMERIC( <v1> )   => ( hb_isNumeric( <v1> ) )

#translate IFNIL( <v1>,<exp1>,<exp2> )       => IF( (<v1>) == NIL,<exp1>,<exp2> )
#translate IFARRAY( <v1>,<exp1>,<exp2> )     => IF( hb_isArray( <v1> ),<exp1>,<exp2> )
#translate IFBLOCK( <v1>,<exp1>,<exp2> )     => IF( hb_isBlock( <v1> ),<exp1>,<exp2> )
#translate IFCHARACTER( <v1>,<exp1>,<exp2> ) => IF( hb_isString( <v1> ),<exp1>,<exp2> )
#translate IFCHAR( <v1>,<exp1>,<exp2> )      => IF( hb_isString( <v1> ),<exp1>,<exp2> )
#translate IFSTRING( <v1>,<exp1>,<exp2> )    => IF( hb_isString( <v1> ),<exp1>,<exp2> )
#translate IFDATE( <v1>,<exp1>,<exp2> )      => IF( hb_isDate( <v1> ),<exp1>,<exp2> )
#translate IFLOGICAL( <v1>,<exp1>,<exp2> )   => IF( hb_isLogical( <v1> ),<exp1>,<exp2> )
#translate IFNUMBER( <v1>,<exp1>,<exp2> )    => IF( hb_isNumeric( <v1> ),<exp1>,<exp2> )
#translate IFNUMERIC( <v1>,<exp1>,<exp2> )   => IF( hb_isNumeric( <v1> ),<exp1>,<exp2> )
#translate IFOBJECT( <v1>,<exp1>,<exp2> )    => IF( hb_isObject( <v1> ),<exp1>,<exp2> )
#translate IFEMPTY( <v1>,<exp1>,<exp2> )     => IF( Empty( <v1> ),<exp1>,<exp2> )

/////////////////////////////////////
// Abbreviated flow control modifiers
/////////////////////////////////////

#xcommand BREAKIF <log>       => IF (<log>) ; BREAK ; ENDIF
#xcommand EXITIF <log>        => IF (<log>) ; EXIT ; ENDIF
#xcommand LOOPIF <log>        => IF (<log>) ; LOOP ; ENDIF

// Extended commands
  
#command UPDATE <v1> IF <exp> TO <v2> ;
         =>                           ;
         IF <exp> ; <v1> := <v2> ; ENDIF

#xtranslate frac(<num>) => (<num>-int(<num>))


/////////////////////////
// whBrowse column styles
/////////////////////////

#define TBC_READWRITE 1  // Can the user modify the data in the column's cells?
#define TBC_MOVE      2  // Can the user move the column to another position in the browse?
#define TBC_SIZE      4  // Can the user modify the width of the column?

#define TBC_POSITION  0 // Current column position
#define TBC_INDEX     1 // Original Column Index 

////////////////////
// whBrowse commands
////////////////////

#xCommand BROWSE [<datasource>] AS <obj>;
          [USE WINDOW <hWnd>] ;
          [IMAGES IN <imagelist>] =>;
          <obj> := whBrowse():INIT(<datasource>,<hWnd>,<imagelist>); WITH OBJECT <obj>

#xCommand ADD COLUMN <cTitle> DATA <bContent> [WIDTH <nWidth>] => ;
          :AddColumn( whColumn():INIT(<cTitle>,<bContent>,,<nWidth>))

#xCommand CREATE WINDOW [IN WINDOW <hWndParent>] => :Create(<hWndParent>)

#Xcommand END BROWSE => :Configure() ; END WITH


//////////////////////
// AutoForm constants
//////////////////////                         

#Define CT_BUTTON      1
#Define CT_EDIT        2
#Define CT_CHECKBOX    3
#Define CT_COMBOBOX    4          // Standard combo-box - pick from list
#Define CT_RADIOBUTTON 5
#Define CT_GET         6
#Define CT_TEXT        7
#Define CT_TEXTBOLD    8
#Define CT_LISTBOX     9
#Define CT_MULTIEDIT  10
#Define CT_BITMAP     11


