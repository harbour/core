/*
 * Copyright 2011 Fausto Di Creddo Trautwein, ftwein@yahoo.com.br
 *
 * Thanks TO Robert F Greer, PHP original version
 * https://sourceforge.net/projects/excelwriterxml/
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#include "hbclass.ch"

CREATE CLASS ExcelWriterXML_Style

   VAR id
   VAR name
   VAR useAlignment         INIT .F.
   VAR useFont              INIT .F.
   VAR useBorder            INIT .F.
   VAR useInterior          INIT .F.

   VAR valign
   VAR halign
   VAR rotate
   VAR shrinktofit          INIT 0
   VAR verticaltext         INIT 0
   VAR wraptext             INIT 0

   VAR fontColor            INIT "Automatic"
   VAR fontName
   VAR fontFamily
   VAR fontSize
   VAR bold
   VAR italic
   VAR underline
   VAR strikethrough
   VAR shadow
   VAR outline

   VAR borderTop            INIT { => }
   VAR borderBottom         INIT { => }
   VAR borderLeft           INIT { => }
   VAR borderRight          INIT { => }
   VAR borderDL             INIT { => }
   VAR borderDR             INIT { => }

   VAR interiorColor
   VAR interiorPattern
   VAR interiorPatternColor

   VAR numberFormat

   VAR formatErrors         INIT { => }
   VAR namedColorsIE        INIT { => }

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

METHOD ExcelWriterXML_Style:New( id )

   ::id := id

   ::namedColorsIE := { ;
      "aliceblue"            => "#F0F8FF", ;
      "antiquewhite"         => "#FAEBD7", ;
      "aqua"                 => "#00FFFF", ;
      "aquamarine"           => "#7FFFD4", ;
      "azure"                => "#F0FFFF", ;
      "beige"                => "#F5F5DC", ;
      "bisque"               => "#FFE4C4", ;
      "black"                => "#000000", ;
      "blanchedalmond"       => "#FFEBCD", ;
      "blue"                 => "#0000FF", ;
      "blueviolet"           => "#8A2BE2", ;
      "brown"                => "#A52A2A", ;
      "burlywood"            => "#DEB887", ;
      "cadetblue"            => "#5F9EA0", ;
      "chartreuse"           => "#7FFF00", ;
      "chocolate"            => "#D2691E", ;
      "coral"                => "#FF7F50", ;
      "cornflowerblue"       => "#6495ED", ;
      "cornsilk"             => "#FFF8DC", ;
      "crimson"              => "#DC143C", ;
      "cyan"                 => "#00FFFF", ;
      "darkblue"             => "#00008B", ;
      "darkcyan"             => "#008B8B", ;
      "darkgoldenrod"        => "#B8860B", ;
      "darkgray"             => "#A9A9A9", ;
      "darkgreen"            => "#006400", ;
      "darkkhaki"            => "#BDB76B", ;
      "darkmagenta"          => "#8B008B", ;
      "darkolivegreen"       => "#556B2F", ;
      "darkorange"           => "#FF8C00", ;
      "darkorchid"           => "#9932CC", ;
      "darkred"              => "#8B0000", ;
      "darksalmon"           => "#E9967A", ;
      "darkseagreen"         => "#8FBC8F", ;
      "darkslateblue"        => "#483D8B", ;
      "darkslategray"        => "#2F4F4F", ;
      "darkturquoise"        => "#00CED1", ;
      "darkviolet"           => "#9400D3", ;
      "deeppink"             => "#FF1493", ;
      "deepskyblue"          => "#00BFFF", ;
      "dimgray"              => "#696969", ;
      "dodgerblue"           => "#1E90FF", ;
      "firebrick"            => "#B22222", ;
      "floralwhite"          => "#FFFAF0", ;
      "forestgreen"          => "#228B22", ;
      "fuchsia"              => "#FF00FF", ;
      "gainsboro"            => "#DCDCDC", ;
      "ghostwhite"           => "#F8F8FF", ;
      "gold"                 => "#FFD700", ;
      "goldenrod"            => "#DAA520", ;
      "gray"                 => "#808080", ;
      "green"                => "#008000", ;
      "greenyellow"          => "#ADFF2F", ;
      "honeydew"             => "#F0FFF0", ;
      "hotpink"              => "#FF69B4", ;
      "indianred"            => "#CD5C5C", ;
      "indigo"               => "#4B0082", ;
      "ivory"                => "#FFFFF0", ;
      "khaki"                => "#F0E68C", ;
      "lavender"             => "#E6E6FA", ;
      "lavenderblush"        => "#FFF0F5", ;
      "lawngreen"            => "#7CFC00", ;
      "lemonchiffon"         => "#FFFACD", ;
      "lightblue"            => "#ADD8E6", ;
      "lightcoral"           => "#F08080", ;
      "lightcyan"            => "#E0FFFF", ;
      "lightgoldenrodyellow" => "#FAFAD2", ;
      "lightgreen"           => "#90EE90", ;
      "lightgrey"            => "#D3D3D3", ;
      "lightpink"            => "#FFB6C1", ;
      "lightsalmon"          => "#FFA07A", ;
      "lightseagreen"        => "#20B2AA", ;
      "lightskyblue"         => "#87CEFA", ;
      "lightslategray"       => "#778899", ;
      "lightsteelblue"       => "#B0C4DE", ;
      "lightyellow"          => "#FFFFE0", ;
      "lime"                 => "#00FF00", ;
      "limegreen"            => "#32CD32", ;
      "linen"                => "#FAF0E6", ;
      "magenta"              => "#FF00FF", ;
      "maroon"               => "#800000", ;
      "mediumaquamarine"     => "#66CDAA", ;
      "mediumblue"           => "#0000CD", ;
      "mediumorchid"         => "#BA55D3", ;
      "mediumpurple"         => "#9370DB", ;
      "mediumseagreen"       => "#3CB371", ;
      "mediumslateblue"      => "#7B68EE", ;
      "mediumspringgreen"    => "#00FA9A", ;
      "mediumturquoise"      => "#48D1CC", ;
      "mediumvioletred"      => "#C71585", ;
      "midnightblue"         => "#191970", ;
      "mintcream"            => "#F5FFFA", ;
      "mistyrose"            => "#FFE4E1", ;
      "moccasin"             => "#FFE4B5", ;
      "navajowhite"          => "#FFDEAD", ;
      "navy"                 => "#000080", ;
      "oldlace"              => "#FDF5E6", ;
      "olive"                => "#808000", ;
      "olivedrab"            => "#6B8E23", ;
      "orange"               => "#FFA500", ;
      "orangered"            => "#FF4500", ;
      "orchid"               => "#DA70D6", ;
      "palegoldenrod"        => "#EEE8AA", ;
      "palegreen"            => "#98FB98", ;
      "paleturquoise"        => "#AFEEEE", ;
      "palevioletred"        => "#DB7093", ;
      "papayawhip"           => "#FFEFD5", ;
      "peachpuff"            => "#FFDAB9", ;
      "peru"                 => "#CD853F", ;
      "pink"                 => "#FFC0CB", ;
      "plum"                 => "#DDA0DD", ;
      "powderblue"           => "#B0E0E6", ;
      "purple"               => "#800080", ;
      "red"                  => "#FF0000", ;
      "rosybrown"            => "#BC8F8F", ;
      "royalblue"            => "#4169E1", ;
      "saddlebrown"          => "#8B4513", ;
      "salmon"               => "#FA8072", ;
      "sandybrown"           => "#F4A460", ;
      "seagreen"             => "#2E8B57", ;
      "seashell"             => "#FFF5EE", ;
      "sienna"               => "#A0522D", ;
      "silver"               => "#C0C0C0", ;
      "skyblue"              => "#87CEEB", ;
      "slateblue"            => "#6A5ACD", ;
      "slategray"            => "#708090", ;
      "snow"                 => "#FFFAFA", ;
      "springgreen"          => "#00FF7F", ;
      "steelblue"            => "#4682B4", ;
      "tan"                  => "#D2B48C", ;
      "teal"                 => "#008080", ;
      "thistle"              => "#D8BFD8", ;
      "tomato"               => "#FF6347", ;
      "turquoise"            => "#40E0D0", ;
      "violet"               => "#EE82EE", ;
      "wheat"                => "#F5DEB3", ;
      "white"                => "#FFFFFF", ;
      "whitesmoke"           => "#F5F5F5", ;
      "yellow"               => "#FFFF00", ;
      "yellowgreen"          => "#9ACD32" }

   RETURN Self

METHOD ExcelWriterXML_Style:getID()

   RETURN ::id

METHOD ExcelWriterXML_Style:getStyleXML()

   LOCAL fontcolor, positions, position, auxdata, pData, bLinestyle, bColor, bWeight, xml
   LOCAL numberFormat

   LOCAL name                 := ""
   LOCAL valign               := ""
   LOCAL halign               := ""
   LOCAL rotate               := ""
   LOCAL shrinktofit          := ""
   LOCAL verticaltext         := ""
   LOCAL wraptext             := ""

   LOCAL bold                 := ""
   LOCAL italic               := ""
   LOCAL strikethrough        := ""
   LOCAL underline            := ""
   LOCAL outline              := ""
   LOCAL shadow               := ""
   LOCAL fontName             := ""
   LOCAL fontFamily           := ""
   LOCAL fontSize             := ""

   LOCAL borders              := ""

   LOCAL interior             := ""
   LOCAL interiorColor        := ""
   LOCAL interiorPattern      := ""
   LOCAL interiorPatternColor := ""

   IF ! Empty( ::name )
      name := 'ss:Name="' + ::name + '"'
   ENDIF

   IF ::useAlignment
      IF ! Empty( ::valign )
         valign := 'ss:Vertical="' + ::valign + '"'
      ENDIF
      IF ! Empty( ::halign )
         halign := 'ss:Horizontal="' + ::halign + '"'
      ENDIF
      IF ! Empty( ::rotate )
         rotate := 'ss:Rotate="' + ::rotate + '"'
      ENDIF
      IF ! Empty( ::shrinktofit )
         shrinktofit := 'ss:ShrinkToFit="1"'
      ENDIF
      IF ! Empty( ::verticaltext )
         verticaltext := 'ss:VerticalText="1"'
      ENDIF
      IF ! Empty( ::wraptext )
         wraptext := 'ss:WrapText="1"'
      ENDIF
   ENDIF

   IF ::useFont
      IF ! Empty( ::fontColor )
         fontColor := 'ss:Color="' + ::fontColor + '"'
      ENDIF
      IF ! Empty( ::bold )
         bold := 'ss:Bold="1"'
      ENDIF
      IF ! Empty( ::italic )
         italic := 'ss:Italic="1"'
      ENDIF
      IF ! Empty( ::strikethrough )
         strikethrough := 'ss:StrikeThrough="' + ::strikethrough + '"'
      ENDIF
      IF ! Empty( ::underline )
         underline := 'ss:Underline="' + ::underline + '"'
      ENDIF
      IF ! Empty( ::outline )
         outline := 'ss:Outline="1"'
      ENDIF
      IF ! Empty( ::shadow )
         shadow := 'ss:Shadow="1"'
      ENDIF
      IF ! Empty( ::fontName )
         fontName := 'ss:FontName="' + ::fontName + '"'
      ENDIF
      IF ! Empty( ::fontFamily )
         fontFamily := 'x:Family="' + ::fontFamily + '"'
      ENDIF
      IF ! Empty( ::fontSize )
         fontSize := 'ss:Size="' + hb_ntos( ::fontSize ) + '"'
      ENDIF
   ENDIF

   IF ::useBorder
      borders := "      <Borders>" + hb_eol()
      positions := { ;
         "Top"           => ::borderTop,    ;
         "Bottom"        => ::borderBottom, ;
         "Left"          => ::borderLeft,   ;
         "Right"         => ::borderRight,  ;
         "DiagonalLeft"  => ::borderDL,     ;
         "DiagonalRight" => ::borderDR      }

      FOR EACH auxdata IN positions
         position := auxdata:Key
         pData    := auxdata:Value
         IF Empty( pData )
            LOOP
         ENDIF
         bLinestyle := iif( "LineStyle" $ pData, ;
            'ss:LineStyle="' + pData[ "LineStyle" ] + '"', ;
            "" )
         bColor := iif( "Color" $ pData, ;
            'ss:Color="' + pData[ "Color" ] + '"', ;
            "" )
         bWeight := iif( "Weight" $ pData, ;
            'ss:Weight="' + hb_ntos( pData[ "Weight" ] ) + '"', ;
            "" )
         borders += '<Border ss:Position="' + AllTrim( position + '" ' + bLinestyle + " " + bColor + " " + bWeight ) + "/>" + hb_eol()
      NEXT
      borders += "</Borders>" + hb_eol()
   ENDIF

   IF ::useInterior
      IF ! Empty( ::interiorColor )
         interiorColor := 'ss:Color="' + ::interiorColor + '"'
      ENDIF
      IF ! Empty( ::interiorPattern )
         interiorPattern := 'ss:Pattern="' + ::interiorPattern + '"'
      ENDIF
      IF ! Empty( ::interiorPatternColor )
         interiorPatternColor := 'ss:PatternColor="' + ::interiorPatternColor + '"'
      ENDIF
      interior := "      <Interior " + AllTrim( interiorColor + " " + interiorPattern + " " + interiorPatternColor ) + "/>" + hb_eol()
   ENDIF

   IF Empty( ::numberFormat )
      numberFormat := "      <NumberFormat/>" + hb_eol()
   ELSE
      numberFormat := '      <NumberFormat ss:Format="' + ::numberFormat + '"/>' + hb_eol()
   ENDIF

   xml := '   <Style ss:ID="' + ::id + '" ' + name + ">" + hb_eol()
   IF ::useAlignment
      xml += "      <Alignment " + AllTrim( valign + " " + halign + " " + rotate + " " + shrinktofit + " " + wraptext + " " + verticaltext ) + "/>" + hb_eol()
   ENDIF
   IF ::useBorder
      xml += borders
   ENDIF
   IF ::useFont
      xml += "      <Font " + AllTrim( fontSize + " " + fontColor + " " + bold + " " + italic + " " + strikethrough + " " + underline + " " + shadow + " " + outline + " " + fontName + " " + fontFamily ) + "/>" + hb_eol()
   ENDIF
   IF ::useInterior
      xml += interior
   ENDIF
   xml += numberFormat
   xml += "      <Protection/>" + hb_eol()
   xml += "   </Style>" + hb_eol()

   RETURN xml

METHOD ExcelWriterXML_Style:checkColor( color )

   IF hb_LeftEq( color, "#" )
      RETURN COLOR
   ELSEIF Lower( color ) $ ::namedColorsIE
      color := ::namedColorsIE[ Lower( color ) ]
      RETURN COLOR
   ENDIF

   RETURN ""

METHOD PROCEDURE ExcelWriterXML_Style:setName( name )

   ::name := name

   RETURN

METHOD PROCEDURE ExcelWriterXML_Style:alignVertical( valign )

   IF !( valign == "Automatic" ) .AND. ;
      !( valign == "Top"       ) .AND. ;
      !( valign == "Bottom"    ) .AND. ;
      !( valign == "Center"    )
      RETURN
   ENDIF
   ::valign := valign
   ::useAlignment := .T.

   RETURN

METHOD PROCEDURE ExcelWriterXML_Style:alignHorizontal( halign )

   IF !( halign == "Automatic" ) .AND. ;
      !( halign == "Left"      ) .AND. ;
      !( halign == "Center"    ) .AND. ;
      !( halign == "Right"     )
      halign := "Automatic"
   ENDIF
   ::halign := halign
   ::useAlignment := .T.

   RETURN

METHOD PROCEDURE ExcelWriterXML_Style:alignRotate( rotate )

   IF HB_ISNUMERIC( rotate )
      IF Abs( rotate ) > 90
         rotate := rotate % 90
      ENDIF
      ::rotate := hb_ntos( rotate )
      ::useAlignment := .T.
   ENDIF

   RETURN

METHOD PROCEDURE ExcelWriterXML_Style:alignShrinktofit()

   ::shrinktofit  := 1
   ::useAlignment := .T.

   RETURN

METHOD PROCEDURE ExcelWriterXML_Style:alignVerticaltext()

   ::verticaltext := 1
   ::useAlignment := .T.

   RETURN

METHOD PROCEDURE ExcelWriterXML_Style:alignWraptext()

   ::wraptext     := 1
   ::useAlignment := .T.

   RETURN

METHOD PROCEDURE ExcelWriterXML_Style:setFontSize( fontSize )

   hb_default( @fontSize, 10 )

   IF fontSize <= 0
      fontSize := 10
   ENDIF
   ::fontSize := fontSize
   ::useFont := .T.

   RETURN

METHOD PROCEDURE ExcelWriterXML_Style:setFontColor( fontColor )

   fontColor := ::checkColor( fontColor )
   IF ! hb_LeftEq( fontColor, "#" )
      fontColor := "Automatic"
   ENDIF
   ::fontColor := fontColor
   ::useFont := .T.

   RETURN

METHOD PROCEDURE ExcelWriterXML_Style:setFontName( fontName )

   ::fontName := hb_defaultValue( fontname, "Arial" )
   ::useFont := .T.

   RETURN

METHOD PROCEDURE ExcelWriterXML_Style:setFontFamily( fontFamily )

   hb_default( @fontFamily, "Swiss" )

   IF !( fontFamily == "Automatic"  ) .AND. ;
      !( fontFamily == "Decorative" ) .AND. ;
      !( fontFamily == "Modern"     ) .AND. ;
      !( fontFamily == "Roman"      ) .AND. ;
      !( fontFamily == "Script"     ) .AND. ;
      !( fontFamily == "Swiss"      )
      RETURN
   ENDIF
   ::fontFamily := fontFamily
   ::useFont := .T.

   RETURN

METHOD PROCEDURE ExcelWriterXML_Style:setFontBold()

   ::bold := 1
   ::useFont := .T.

   RETURN

METHOD PROCEDURE ExcelWriterXML_Style:setFontItalic()

   ::italic := 1
   ::useFont := .T.

   RETURN

METHOD PROCEDURE ExcelWriterXML_Style:setFontStrikethrough()

   ::strikethrough := 1
   ::useFont := .T.

   RETURN

METHOD PROCEDURE ExcelWriterXML_Style:setFontUnderline( uStyle )

   hb_default( @uStyle, "Single" )

   IF !( uStyle == "None"             ) .AND. ;
      !( uStyle == "Single"           ) .AND. ;
      !( uStyle == "Double"           ) .AND. ;
      !( uStyle == "SingleAccounting" ) .AND. ;
      !( uStyle == "DoubleAccounting" )
      RETURN
   ENDIF

   ::underline := uStyle
   ::useFont := .T.

   RETURN

METHOD PROCEDURE ExcelWriterXML_Style:setFontShadow()

   ::shadow := 1
   ::useFont := .T.

   RETURN

METHOD PROCEDURE ExcelWriterXML_Style:setFontOutline()

   ::outline := 1
   ::useFont := .T.

   RETURN

METHOD PROCEDURE ExcelWriterXML_Style:border( position, weight, color, linestyle )

   LOCAL tmp

   hb_default( @position, "All" )          // All, Left, Top, Right, Bottom, DiagonalLeft, DiagonalRight
   hb_default( @weight, 1 )                // 0-Hairline, 1-Thin, 2-Medium, 3-Thick
   hb_default( @color, "Automatic" )       // Automatic, 6-hexadecimal digit number IN "#rrggbb" format OR it can be any of the MS Internet Explorer named colors
   hb_default( @linestyle, "Continuous" )  // None, Continuous, Dash, Dot, DashDot, DashDotDot, SlantDashDot, Double

   IF !( position == "All"           ) .AND. ;
      !( position == "Left"          ) .AND. ;
      !( position == "Top"           ) .AND. ;
      !( position == "Right"         ) .AND. ;
      !( position == "Bottom"        ) .AND. ;
      !( position == "DiagonalLeft"  ) .AND. ;
      !( position == "DiagonalRight" )

      position := "All"
   ENDIF

   IF weight > 3 .OR. weight < 0
      weight := 3
   ENDIF

   color := ::checkColor( color )
   IF ! hb_LeftEq( color, "#" )
      color := "Automatic"
   ENDIF

   IF !( linestyle == "None"         ) .AND. ;
      !( linestyle == "Continuous"   ) .AND. ;
      !( linestyle == "Dash"         ) .AND. ;
      !( linestyle == "Dot"          ) .AND. ;
      !( linestyle == "DashDot"      ) .AND. ;
      !( linestyle == "DashDotDot"   ) .AND. ;
      !( linestyle == "SlantDashDot" ) .AND. ;
      !( linestyle == "Double"       )

      linestyle := "Continuous"
   ENDIF

   tmp := { ;
      "LineStyle" => linestyle, ;
      "Color"     => color, ;
      "Weight"    => weight }

   IF position == "Top"    .OR. position == "All"
      ::borderTop := tmp
   ENDIF
   IF position == "Bottom" .OR. position == "All"
      ::borderBottom := tmp
   ENDIF
   IF position == "Left"   .OR. position == "All"
      ::borderLeft := tmp
   ENDIF
   IF position == "Right"  .OR. position == "All"
      ::borderRight := tmp
   ENDIF
   IF position == "DiagonalLeft"
      ::borderDL := tmp
   ENDIF
   IF position == "DiagonalRight"
      ::borderDR := tmp
   ENDIF

   ::useBorder := .T.

   RETURN

METHOD PROCEDURE ExcelWriterXML_Style:bgColor( color, pattern, patternColor )

   hb_default( @color, "Yellow" )
   hb_default( @pattern, "Solid" )

   color := ::checkColor( color )
   IF ! hb_LeftEq( color, "#" )
      color := "Yellow"
   ENDIF
   ::interiorColor := color
   IF !( pattern == "None" )
      ::bgPattern( pattern, patternColor )
   ENDIF
   ::useInterior := .T.

   RETURN

METHOD PROCEDURE ExcelWriterXML_Style:bgPattern( pattern, color )

   hb_default( @pattern, "None" )

   IF !( pattern == "None"                  ) .AND. ;
      !( pattern == "Solid"                 ) .AND. ;
      !( pattern == "Gray75"                ) .AND. ;
      !( pattern == "Gray50"                ) .AND. ;
      !( pattern == "Gray25"                ) .AND. ;
      !( pattern == "Gray125"               ) .AND. ;
      !( pattern == "Gray0625"              ) .AND. ;
      !( pattern == "HorzStripe"            ) .AND. ;
      !( pattern == "VertStripe"            ) .AND. ;
      !( pattern == "ReverseDiagStripe"     ) .AND. ;
      !( pattern == "DiagStripe"            ) .AND. ;
      !( pattern == "DiagCross"             ) .AND. ;
      !( pattern == "ThickDiagCross"        ) .AND. ;
      !( pattern == "ThinHorzStripe"        ) .AND. ;
      !( pattern == "ThinVertStripe"        ) .AND. ;
      !( pattern == "ThinReverseDiagStripe" ) .AND. ;
      !( pattern == "ThinDiagStripe"        ) .AND. ;
      !( pattern == "ThinHorzCross"         ) .AND. ;
      !( pattern == "ThinDiagCross"         )

      pattern := "None"
   ENDIF

   ::interiorPattern := pattern
   IF HB_ISSTRING( color )
      ::bgPatternColor( color )
   ENDIF
   ::useInterior := .T.

   RETURN

METHOD PROCEDURE ExcelWriterXML_Style:bgPatternColor( color )

   hb_default( @color, "Yellow" )

   IF !( color == "Automatic" )
      color := ::checkColor( color )
      IF ! hb_LeftEq( color, "#" )
         color := "Automatic"
      ENDIF
   ENDIF
   ::interiorPatternColor := color
   ::useInterior := .T.

   RETURN

METHOD PROCEDURE ExcelWriterXML_Style:setNumberFormat( formatString )

   ::numberFormat := formatString

   RETURN

METHOD PROCEDURE ExcelWriterXML_Style:setNumberFormatDate()

   ::setNumberFormat( "mm/dd/yy" )

   RETURN

METHOD PROCEDURE ExcelWriterXML_Style:setNumberFormatTime()

   ::setNumberFormat( "hh:mm:ss" )

   RETURN

METHOD PROCEDURE ExcelWriterXML_Style:setNumberFormatDatetime()

   ::setNumberFormat( "mm/dd/yy\ hh:mm:ss" )

   RETURN
