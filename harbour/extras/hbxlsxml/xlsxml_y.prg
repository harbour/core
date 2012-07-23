/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2011 Fausto Di Creddo Trautwein, ftwein@yahoo.com.br
 * www - http://www.xharbour.org http://harbour-project.org
 *
 * Thanks TO Robert F Greer, PHP original version
 * http://sourceforge.net/projects/excelwriterxml/
 *
 * This program is free software; you can redistribute it AND/OR modify
 * it under the terms of the GNU General PUBLIC License as published by
 * the Free Software Foundation; either version 2, OR( at your option )
 * any later version.
 *
 * This program is distributed IN the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General PUBLIC License FOR more details.
 *
 * You should have received a copy of the GNU General PUBLIC License
 * along WITH this software; see the file COPYING.  IF NOT, write TO
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA( OR visit the web site http://www.gnu.org/ ).
 *
 * As a special exception, the Harbour Project gives permission FOR
 * additional uses of the text contained IN its release of Harbour.
 *
 * The exception is that, IF you link the Harbour libraries WITH other
 * files TO produce an executable, this does NOT by itself cause the
 * resulting executable TO be covered by the GNU General PUBLIC License.
 * Your use of that executable is IN no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does NOT however invalidate any other reasons why
 * the executable file might be covered by the GNU General PUBLIC License.
 *
 * This exception applies only TO the code released by the Harbour
 * Project under the name Harbour.  IF you copy code FROM other
 * Harbour Project OR Free Software Foundation releases into a copy of
 * Harbour, as the General PUBLIC License permits, the exception does
 * NOT apply TO the code that you add IN this way.  TO avoid misleading
 * anyone as TO the status of such modified files, you must delete
 * this exception notice FROM them.
 *
 * IF you write modifications of your own FOR Harbour, it is your choice
 * whether TO permit this exception TO apply TO your modifications.
 * IF you DO NOT wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/

#include "hbclass.ch"

/*----------------------------------------------------------------------*/

CREATE CLASS ExcelWriterXML_Style

   DATA   id
   DATA   name
   DATA   useAlignment                            INIT .f.
   DATA   useFont                                 INIT .f.
   DATA   useBorder                               INIT .f.
   DATA   useInterior                             INIT .f.

   DATA   valign
   DATA   halign
   DATA   rotate
   DATA   shrinktofit                             INIT 0
   DATA   verticaltext                            INIT 0
   DATA   wraptext                                INIT 0

   DATA   fontColor                               INIT 'Automatic'
   DATA   fontName
   DATA   fontFamily
   DATA   fontSize
   DATA   bold
   DATA   italic
   DATA   underline
   DATA   strikethrough
   DATA   shadow
   DATA   outline

   DATA   borderTop                               INIT {=>}
   DATA   borderBottom                            INIT {=>}
   DATA   borderLeft                              INIT {=>}
   DATA   borderRight                             INIT {=>}
   DATA   borderDL                                INIT {=>}
   DATA   borderDR                                INIT {=>}

   DATA   interiorColor
   DATA   interiorPattern
   DATA   interiorPatternColor

   DATA   numberFormat

   DATA   formatErrors                            INIT {=>}
   DATA   namedColorsIE                           INIT {=>}

   METHOD new( id )
   METHOD getID()
   METHOD getStyleXML()
   METHOD checkColor( color )
   METHOD setName( name )
   METHOD alignVertical( valign )
   METHOD alignHorizontal( halign )
   METHOD alignRotate( rotate )
   METHOD alignShrinktofit()
   METHOD alignVerticaltext()
   METHOD alignWraptext()
   METHOD setFontSize( fontSize )
   METHOD setFontColor( fontColor )
   METHOD setFontName( fontName )
   METHOD setFontFamily( fontFamily )
   METHOD setFontBold()
   METHOD setFontItalic()
   METHOD setFontStrikethrough()
   METHOD setFontUnderline( uStyle )
   METHOD setFontShadow()
   METHOD setFontOutline()
   METHOD border( position, weight, color, linestyle )
   METHOD bgColor( color, pattern, patternColor )
   METHOD bgPattern( pattern, color )
   METHOD bgPatternColor( color )
   METHOD setNumberFormat( formatString )
   METHOD setNumberFormatDate()
   METHOD setNumberFormatTime()
   METHOD setNumberFormatDatetime()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:New( id )

   ::id:= id
   ::namedColorsIE := getColorIE()

   RETURN SELF

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:getID()

   RETURN ::id

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:getStyleXML()
   LOCAL fontcolor, positions, position, auxdata, pData, bLinestyle, bColor, bWeight, xml
   LOCAL numberFormat

   LOCAL name                 := ''
   LOCAL valign               := ''
   LOCAL halign               := ''
   LOCAL rotate               := ''
   LOCAL shrinktofit          := ''
   LOCAL verticaltext         := ''
   LOCAL wraptext             := ''

   LOCAL bold                 := ''
   LOCAL italic               := ''
   LOCAL strikethrough        := ''
   LOCAL underline            := ''
   LOCAL outline              := ''
   LOCAL shadow               := ''
   LOCAL fontName             := ''
   LOCAL fontFamily           := ''
   LOCAL fontSize             := ''

   LOCAL borders              := ''

   LOCAL interior             := ''
   LOCAL interiorColor        := ''
   LOCAL interiorPattern      := ''
   LOCAL interiorPatternColor := ''

   IF ! empty( ::name )
      name := 'ss:Name="'+::name+'"'
   ENDIF

   IF ::useAlignment
      IF ! empty( ::valign )
         valign := 'ss:Vertical="' + ::valign + '"'
      ENDIF
      IF ! empty( ::halign )
         halign := 'ss:Horizontal="' + ::halign + '"'
      ENDIF
      IF ! empty( ::rotate )
         rotate := 'ss:Rotate="' + ::rotate + '"'
      ENDIF
      IF ! empty( ::shrinktofit )
         shrinktofit := 'ss:ShrinkToFit="1"'
      ENDIF
      IF ! empty( ::verticaltext )
         verticaltext := 'ss:VerticalText="1"'
      ENDIF
      IF ! empty( ::wraptext )
         wraptext := 'ss:WrapText="1"'
      ENDIF
   ENDIF

   IF ::useFont
      IF ! empty( ::fontColor )
         fontColor := 'ss:Color="' + ::fontColor + '"'
      ENDIF
      IF ! empty( ::bold )
         bold := 'ss:Bold="1"'
      ENDIF
      IF ! empty( ::italic )
         italic := 'ss:Italic="1"'
      ENDIF
      IF ! empty( ::strikethrough )
         strikethrough := 'ss:StrikeThrough="' + ::strikethrough + '"'
      ENDIF
      IF ! empty( ::underline )
         underline := 'ss:Underline="' + ::underline + '"'
      ENDIF
      IF ! empty( ::outline )
         outline := 'ss:Outline="1"'
      ENDIF
      IF ! empty( ::shadow )
         shadow := 'ss:Shadow="1"'
      ENDIF
      IF ! empty( ::fontName )
         fontName := 'ss:FontName="' + ::fontName + '"'
      ENDIF
      IF ! empty( ::fontFamily )
         fontFamily := 'x:Family="' + ::fontFamily + '"'
      ENDIF
      IF ! empty( ::fontSize )
         fontSize := 'ss:Size="' + ALLTRIM( STR( ::fontSize, 10 ) ) + '"'
      ENDIF
   ENDIF

   IF ::useBorder
      borders := '      <Borders>'+HB_OsNewLine()
      positions := { ;
         'Top'           => ::borderTop,    ;
         'Bottom'        => ::borderBottom, ;
         'Left'          => ::borderLeft,   ;
         'Right'         => ::borderRight,  ;
         'DiagonalLeft'  => ::borderDL,     ;
         'DiagonalRight' => ::borderDR      }
      auxdata := NIL
      FOR EACH auxdata IN positions
         position := auxdata:Key
         pData    := auxdata:Value
         IF empty( pData )
            LOOP
         ENDIF
         bLinestyle := iif( hb_hPos( pData, 'LineStyle' ) > 0,;
            'ss:LineStyle="' + pData[ 'LineStyle' ] + '"', ;
            '' )
         bColor := iif( hb_hPos( pData, 'Color' ) > 0,;
            'ss:Color="' + pData[ 'Color' ] + '"',;
            '' )
         bWeight := iif( hb_hPos( pData, 'Weight' ) > 0,;
            'ss:Weight="' + STR( pData[ 'Weight' ], 1 ) + '"',;
            '' )
         borders += '<Border ss:Position="' + position + '" ' + bLinestyle + ' ' + bColor + ' ' + bWeight + '/>' + HB_OsNewLine()
      NEXT
      borders += '</Borders>' + HB_OsNewLine()
   ENDIF

   IF ::useInterior
      IF ! empty( ::interiorColor )
         interiorColor := 'ss:Color="' + ::interiorColor + '"'
      ENDIF
      IF ! empty( ::interiorPattern )
         interiorPattern := 'ss:Pattern="' + ::interiorPattern + '"'
      ENDIF
      IF ! empty( ::interiorPatternColor )
         interiorPatternColor := 'ss:PatternColor="' + ::interiorPatternColor + '"'
      ENDIF
      interior := '      <Interior ' + interiorColor + ' ' + interiorPattern + ' ' + interiorPatternColor + '/>' + HB_OsNewLine()
   ENDIF

   IF ! empty( ::numberFormat )
      numberFormat := '      <NumberFormat ss:Format="' + ::numberFormat + '"/>' + HB_OsNewLine()
   ELSE
      numberFormat := '      <NumberFormat/>' + HB_OsNewLine()
   ENDIF

   xml := '   <Style ss:ID="' + ::id + '" ' + name + '>' + HB_OsNewLine()
   IF ::useAlignment
      xml += '      <Alignment ' + valign + ' ' + halign + ' ' + rotate + ' ' + shrinktofit + ' ' + wraptext + ' ' + verticaltext + '/>' + HB_OsNewLine()
   ENDIF
   IF ::useBorder
      xml += borders
   ENDIF
   IF ::useFont
      xml += '      <Font ' +fontSize + ' ' + fontColor + ' ' + bold + ' ' + italic + ' ' + strikethrough + ' ' + underline + ' ' + shadow + ' ' + outline + ' ' + fontName + ' ' + fontFamily + '/>' + HB_OsNewLine()
   ENDIF
   IF ::useInterior
      xml += interior
   ENDIF
   xml += numberFormat
   xml += '      <Protection/>'+HB_OsNewLine()
   xml += '   </Style>'+HB_OsNewLine()

   RETURN xml

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:checkColor( color )

   IF LEFT( color, 1 ) == "#"
      RETURN color
   ELSEIF hb_hPos( ::namedColorsIE, lower( color ) ) > 0
      color := ::namedColorsIE[ lower( color ) ]
      RETURN color
   ELSE
      RETURN ''
   ENDIF
   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:setName( name )

   ::name := name

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:alignVertical( valign )

   IF ( valign != 'Automatic' .AND.;
        valign != 'Top'       .AND.;
        valign != 'Bottom'    .AND.;
        valign != 'Center' )
      RETURN NIL
   ENDIF
   ::valign := valign
   ::useAlignment := .t.

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:alignHorizontal( halign )

   IF ( halign != 'Automatic' .AND.;
        halign != 'Left'      .AND.;
        halign != 'Center'    .AND.;
        halign != 'Right'          )
      halign := 'Automatic'
   ENDIF
   ::halign := halign
   ::useAlignment := .t.

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:alignRotate( rotate )

   IF !HB_ISNUMERIC( rotate )
      RETURN NIL
   ENDIF
   IF abs( rotate ) > 90
      rotate := rotate % 90
   ENDIF
   ::rotate := ALLTRIM( STR( rotate, 3 ) )
   ::useAlignment := .t.

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:alignShrinktofit()

   ::shrinktofit  := 1
   ::useAlignment := .t.

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:alignVerticaltext()

   ::verticaltext := 1
   ::useAlignment := .t.

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:alignWraptext()

   ::wraptext     := 1
   ::useAlignment := .t.

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:setFontSize( fontSize )

   IF ! HB_ISNUMERIC( fontSize )
      fontSize := 10
   ENDIF
   IF fontSize <= 0
      fontSize := 10
   ENDIF
   ::fontSize := fontSize
   ::useFont := .t.

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:setFontColor( fontColor )

   fontColor := ::checkColor( fontColor )
   IF LEFT( fontColor,1 ) != "#"
      fontColor := 'Automatic'
   ENDIF
   ::fontColor := fontColor
   ::useFont := .t.

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:setFontName( fontName )

   IF fontname == NIL
      fontname := 'Arial'
   ENDIF
   ::fontName := fontName
   ::useFont := .t.

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:setFontFamily( fontFamily )

   IF fontFamily == NIL
      fontFamily:= 'Swiss'
   ENDIF

   IF ( fontFamily != 'Automatic'  .AND.;
        fontFamily != 'Decorative' .AND.;
        fontFamily != 'Modern'     .AND.;
        fontFamily != 'Roman'      .AND.;
        fontFamily != 'Script'     .AND.;
        fontFamily != 'Swiss'            )
      RETURN NIL
   ENDIF
   ::fontFamily := fontFamily
   ::useFont := .t.

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:setFontBold()

   ::bold := 1
   ::useFont := .t.

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:setFontItalic()

   ::italic := 1
   ::useFont := .t.

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:setFontStrikethrough()

   ::strikethrough := 1
   ::useFont := .t.

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:setFontUnderline( uStyle )

   IF uStyle == NIL
      uStyle := 'Single'
   ENDIF
   IF ( uStyle != 'None'             .AND.;
        uStyle != 'Single'           .AND.;
        uStyle != 'Double'           .AND.;
        uStyle != 'SingleAccounting' .AND.;
        uStyle != 'DoubleAccounting'      )

      RETURN NIL
   ENDIF
   ::underline := uStyle
   ::useFont := .t.

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:setFontShadow()

   ::shadow := 1
   ::useFont := .t.

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:setFontOutline()

   ::outline := 1
   ::useFont := .t.

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:border( position,weight,color,linestyle )
   LOCAL tmp

   IF position == NIL
      position := 'All'          // All, Left, Top, Right, Bottom, DiagonalLeft, DiagonalRight
   ENDIF
   IF weight == NIL
      weight := '1'              // 0-Hairline, 1-Thin, 2-Medium, 3-Thick
   ENDIF
   IF color == NIL
      color := 'Automatic'       // Automatic, 6-hexadecimal digit number IN "#rrggbb" format OR it can be any of the MS Internet Explorer named colors
   ENDIF
   IF linestyle == NIL
      linestyle := 'Continuous'  // None, Continuous, Dash, Dot, DashDot, DashDotDot, SlantDashDot, Double
   ENDIF

   IF ( position != 'All'           .AND.;
        position != 'Left'          .AND.;
        position != 'Top'           .AND.;
        position != 'Right'         .AND.;
        position != 'Bottom'        .AND.;
        position != 'DiagonalLeft'  .AND.;
        position != 'DiagonalRight'      )

      position := 'All'
   ENDIF

   IF HB_ISNUMERIC( weight )
      IF abs( weight ) > 3
         weight := 3
      ENDIF
   ELSE
      weight := 1
   ENDIF

   color:= ::checkColor( color )
   IF LEFT( color,1 ) != "#"
      color := 'Automatic'
   ENDIF

   IF ( linestyle != 'None'         .AND.;
        linestyle != 'Continuous'   .AND.;
        linestyle != 'Dash'         .AND.;
        linestyle != 'Dot'          .AND.;
        linestyle != 'DashDot'      .AND.;
        linestyle != 'DashDotDot'   .AND.;
        linestyle != 'SlantDashDot' .AND.;
        linestyle != 'Double'            )

      linestyle:= 'Continuous'
   ENDIF

   tmp := { ;
      'LineStyle' => linestyle,;
      'Color'     => color,    ;
      'Weight'    => weight }

   IF position == 'Top'    .OR. position == 'All'
      ::borderTop := tmp
   ENDIF
   IF position == 'Bottom' .OR. position == 'All'
      ::borderBottom := tmp
   ENDIF
   IF position == 'Left'    .OR. position == 'All'
      ::borderLeft := tmp
   ENDIF
   IF position == 'Right'    .OR. position == 'All'
      ::borderRight := tmp
   ENDIF
   IF position == 'DiagonalLeft'
      ::borderDL := tmp
   ENDIF
   IF position == 'DiagonalRight'
      ::borderDR := tmp
   ENDIF

   ::useBorder := .t.

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:bgColor( color, pattern, patternColor )

   IF color == NIL
      color := 'Yellow'
   ENDIF
   IF pattern == NIL
      pattern := 'Solid'
   ENDIF

   color := ::checkColor( color )
   IF LEFT( color,1 ) != "#"
      color := 'Yellow'
   ENDIF
   ::interiorColor := color
   IF pattern != 'None'
      ::bgPattern( pattern, patternColor )
   ENDIF
   ::useInterior:= .t.

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:bgPattern( pattern, color )

   IF pattern == NIL
      pattern := 'None'
   ENDIF

   IF ( pattern != 'None'                  .AND.;
        pattern != 'Solid'                 .AND.;
        pattern != 'Gray75'                .AND.;
        pattern != 'Gray50'                .AND.;
        pattern != 'Gray25'                .AND.;
        pattern != 'Gray125'               .AND.;
        pattern != 'Gray0625'              .AND.;
        pattern != 'HorzStripe'            .AND.;
        pattern != 'VertStripe'            .AND.;
        pattern != 'ReverseDiagStripe'     .AND.;
        pattern != 'DiagStripe'            .AND.;
        pattern != 'DiagCross'             .AND.;
        pattern != 'ThickDiagCross'        .AND.;
        pattern != 'ThinHorzStripe'        .AND.;
        pattern != 'ThinVertStripe'        .AND.;
        pattern != 'ThinReverseDiagStripe' .AND.;
        pattern != 'ThinDiagStripe'        .AND.;
        pattern != 'ThinHorzCross'         .AND.;
        pattern != 'ThinDiagCross'              )

      pattern:= 'None'
   ENDIF

   ::interiorPattern := pattern
   IF color != NIL
      ::bgPatternColor( color )
   ENDIF
   ::useInterior := .t.

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:bgPatternColor( color )

   IF color == NIL
      color := 'Yellow'
   ENDIF
   IF color != 'Automatic'
      color := ::checkColor( color )
      IF LEFT( color,1 ) != "#"
         color := 'Automatic'
      ENDIF
   ENDIF
   ::interiorPatternColor := color
   ::useInterior := .t.

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:setNumberFormat( formatString )

   ::numberFormat := formatString

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:setNumberFormatDate()

   ::setNumberFormat( 'mm/dd/yy' )

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:setNumberFormatTime()

   ::setNumberFormat( 'hh:mm:ss' )

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD ExcelWriterXML_Style:setNumberFormatDatetime()

   ::setNumberFormat( 'mm/dd/yy\ hh:mm:ss' )

   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION getColorIE()
   LOCAL hcolor:= { => }

   hcolor[ 'aliceblue'            ] := '#F0F8FF'
   hcolor[ 'antiquewhite'         ] := '#FAEBD7'
   hcolor[ 'aqua'                 ] := '#00FFFF'
   hcolor[ 'aquamarine'           ] := '#7FFFD4'
   hcolor[ 'azure'                ] := '#F0FFFF'
   hcolor[ 'beige'                ] := '#F5F5DC'
   hcolor[ 'bisque'               ] := '#FFE4C4'
   hcolor[ 'black'                ] := '#000000'
   hcolor[ 'blanchedalmond'       ] := '#FFEBCD'
   hcolor[ 'blue'                 ] := '#0000FF'
   hcolor[ 'blueviolet'           ] := '#8A2BE2'
   hcolor[ 'brown'                ] := '#A52A2A'
   hcolor[ 'burlywood'            ] := '#DEB887'
   hcolor[ 'cadetblue'            ] := '#5F9EA0'
   hcolor[ 'chartreuse'           ] := '#7FFF00'
   hcolor[ 'chocolate'            ] := '#D2691E'
   hcolor[ 'coral'                ] := '#FF7F50'
   hcolor[ 'cornflowerblue'       ] := '#6495ED'
   hcolor[ 'cornsilk'             ] := '#FFF8DC'
   hcolor[ 'crimson'              ] := '#DC143C'
   hcolor[ 'cyan'                 ] := '#00FFFF'
   hcolor[ 'darkblue'             ] := '#00008B'
   hcolor[ 'darkcyan'             ] := '#008B8B'
   hcolor[ 'darkgoldenrod'        ] := '#B8860B'
   hcolor[ 'darkgray'             ] := '#A9A9A9'
   hcolor[ 'darkgreen'            ] := '#006400'
   hcolor[ 'darkkhaki'            ] := '#BDB76B'
   hcolor[ 'darkmagenta'          ] := '#8B008B'
   hcolor[ 'darkolivegreen'       ] := '#556B2F'
   hcolor[ 'darkorange'           ] := '#FF8C00'
   hcolor[ 'darkorchid'           ] := '#9932CC'
   hcolor[ 'darkred'              ] := '#8B0000'
   hcolor[ 'darksalmon'           ] := '#E9967A'
   hcolor[ 'darkseagreen'         ] := '#8FBC8F'
   hcolor[ 'darkslateblue'        ] := '#483D8B'
   hcolor[ 'darkslategray'        ] := '#2F4F4F'
   hcolor[ 'darkturquoise'        ] := '#00CED1'
   hcolor[ 'darkviolet'           ] := '#9400D3'
   hcolor[ 'deeppink'             ] := '#FF1493'
   hcolor[ 'deepskyblue'          ] := '#00BFFF'
   hcolor[ 'dimgray'              ] := '#696969'
   hcolor[ 'dodgerblue'           ] := '#1E90FF'
   hcolor[ 'firebrick'            ] := '#B22222'
   hcolor[ 'floralwhite'          ] := '#FFFAF0'
   hcolor[ 'forestgreen'          ] := '#228B22'
   hcolor[ 'fuchsia'              ] := '#FF00FF'
   hcolor[ 'gainsboro'            ] := '#DCDCDC'
   hcolor[ 'ghostwhite'           ] := '#F8F8FF'
   hcolor[ 'gold'                 ] := '#FFD700'
   hcolor[ 'goldenrod'            ] := '#DAA520'
   hcolor[ 'gray'                 ] := '#808080'
   hcolor[ 'green'                ] := '#008000'
   hcolor[ 'greenyellow'          ] := '#ADFF2F'
   hcolor[ 'honeydew'             ] := '#F0FFF0'
   hcolor[ 'hotpink'              ] := '#FF69B4'
   hcolor[ 'indianred'            ] := '#CD5C5C'
   hcolor[ 'indigo'               ] := '#4B0082'
   hcolor[ 'ivory'                ] := '#FFFFF0'
   hcolor[ 'khaki'                ] := '#F0E68C'
   hcolor[ 'lavender'             ] := '#E6E6FA'
   hcolor[ 'lavenderblush'        ] := '#FFF0F5'
   hcolor[ 'lawngreen'            ] := '#7CFC00'
   hcolor[ 'lemonchiffon'         ] := '#FFFACD'
   hcolor[ 'lightblue'            ] := '#ADD8E6'
   hcolor[ 'lightcoral'           ] := '#F08080'
   hcolor[ 'lightcyan'            ] := '#E0FFFF'
   hcolor[ 'lightgoldenrodyellow' ] := '#FAFAD2'
   hcolor[ 'lightgreen'           ] := '#90EE90'
   hcolor[ 'lightgrey'            ] := '#D3D3D3'
   hcolor[ 'lightpink'            ] := '#FFB6C1'
   hcolor[ 'lightsalmon'          ] := '#FFA07A'
   hcolor[ 'lightseagreen'        ] := '#20B2AA'
   hcolor[ 'lightskyblue'         ] := '#87CEFA'
   hcolor[ 'lightslategray'       ] := '#778899'
   hcolor[ 'lightsteelblue'       ] := '#B0C4DE'
   hcolor[ 'lightyellow'          ] := '#FFFFE0'
   hcolor[ 'lime'                 ] := '#00FF00'
   hcolor[ 'limegreen'            ] := '#32CD32'
   hcolor[ 'linen'                ] := '#FAF0E6'
   hcolor[ 'magenta'              ] := '#FF00FF'
   hcolor[ 'maroon'               ] := '#800000'
   hcolor[ 'mediumaquamarine'     ] := '#66CDAA'
   hcolor[ 'mediumblue'           ] := '#0000CD'
   hcolor[ 'mediumorchid'         ] := '#BA55D3'
   hcolor[ 'mediumpurple'         ] := '#9370DB'
   hcolor[ 'mediumseagreen'       ] := '#3CB371'
   hcolor[ 'mediumslateblue'      ] := '#7B68EE'
   hcolor[ 'mediumspringgreen'    ] := '#00FA9A'
   hcolor[ 'mediumturquoise'      ] := '#48D1CC'
   hcolor[ 'mediumvioletred'      ] := '#C71585'
   hcolor[ 'midnightblue'         ] := '#191970'
   hcolor[ 'mintcream'            ] := '#F5FFFA'
   hcolor[ 'mistyrose'            ] := '#FFE4E1'
   hcolor[ 'moccasin'             ] := '#FFE4B5'
   hcolor[ 'navajowhite'          ] := '#FFDEAD'
   hcolor[ 'navy'                 ] := '#000080'
   hcolor[ 'oldlace'              ] := '#FDF5E6'
   hcolor[ 'olive'                ] := '#808000'
   hcolor[ 'olivedrab'            ] := '#6B8E23'
   hcolor[ 'orange'               ] := '#FFA500'
   hcolor[ 'orangered'            ] := '#FF4500'
   hcolor[ 'orchid'               ] := '#DA70D6'
   hcolor[ 'palegoldenrod'        ] := '#EEE8AA'
   hcolor[ 'palegreen'            ] := '#98FB98'
   hcolor[ 'paleturquoise'        ] := '#AFEEEE'
   hcolor[ 'palevioletred'        ] := '#DB7093'
   hcolor[ 'papayawhip'           ] := '#FFEFD5'
   hcolor[ 'peachpuff'            ] := '#FFDAB9'
   hcolor[ 'peru'                 ] := '#CD853F'
   hcolor[ 'pink'                 ] := '#FFC0CB'
   hcolor[ 'plum'                 ] := '#DDA0DD'
   hcolor[ 'powderblue'           ] := '#B0E0E6'
   hcolor[ 'purple'               ] := '#800080'
   hcolor[ 'red'                  ] := '#FF0000'
   hcolor[ 'rosybrown'            ] := '#BC8F8F'
   hcolor[ 'royalblue'            ] := '#4169E1'
   hcolor[ 'saddlebrown'          ] := '#8B4513'
   hcolor[ 'salmon'               ] := '#FA8072'
   hcolor[ 'sandybrown'           ] := '#F4A460'
   hcolor[ 'seagreen'             ] := '#2E8B57'
   hcolor[ 'seashell'             ] := '#FFF5EE'
   hcolor[ 'sienna'               ] := '#A0522D'
   hcolor[ 'silver'               ] := '#C0C0C0'
   hcolor[ 'skyblue'              ] := '#87CEEB'
   hcolor[ 'slateblue'            ] := '#6A5ACD'
   hcolor[ 'slategray'            ] := '#708090'
   hcolor[ 'snow'                 ] := '#FFFAFA'
   hcolor[ 'springgreen'          ] := '#00FF7F'
   hcolor[ 'steelblue'            ] := '#4682B4'
   hcolor[ 'tan'                  ] := '#D2B48C'
   hcolor[ 'teal'                 ] := '#008080'
   hcolor[ 'thistle'              ] := '#D8BFD8'
   hcolor[ 'tomato'               ] := '#FF6347'
   hcolor[ 'turquoise'            ] := '#40E0D0'
   hcolor[ 'violet'               ] := '#EE82EE'
   hcolor[ 'wheat'                ] := '#F5DEB3'
   hcolor[ 'white'                ] := '#FFFFFF'
   hcolor[ 'whitesmoke'           ] := '#F5F5F5'
   hcolor[ 'yellow'               ] := '#FFFF00'
   hcolor[ 'yellowgreen'          ] := '#9ACD32'

   RETURN hcolor

/*----------------------------------------------------------------------*/
