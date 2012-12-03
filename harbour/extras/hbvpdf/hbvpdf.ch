/*
 * $Id$
 */

#ifndef HBVPDF_CH
#define HBVPDF_CH

#define NORMAL        0
#define BOLD          1
#define ITALIC        2
#define BOLDITALIC    3

#define BOOKLEVEL     1
#define BOOKTITLE     2
#define BOOKPARENT    3
#define BOOKPREV      4
#define BOOKNEXT      5
#define BOOKFIRST     6
#define BOOKLAST      7
#define BOOKCOUNT     8
#define BOOKPAGE      9
#define BOOKCOORD     10

#define FONTNAME      1   // font name
#define FONTSIZE      2   // font size
#define LPI           3   // lines per inch
#define PAGESIZE      4   // page size
#define PAGEORIENT    5   // page orientation
#define PAGEX         6
#define PAGEY         7
#define REPORTWIDTH   8   // report width
#define REPORTPAGE    9   // report page
#define REPORTLINE    10  // report line
#define FONTNAMEPREV  11  // prev font name
#define FONTSIZEPREV  12  // prev font size
#define PAGEBUFFER    13  // page buffer
#define REPORTOBJ     14  // current obj
#define DOCLEN        15  // document length
#define TYPE1         16  // array of type 1 fonts
#define MARGINS       17  // recalc margins ?
#define HEADEREDIT    18  // edit header ?
#define NEXTOBJ       19  // next obj
#define PDFTOP        20  // top row
#define PDFLEFT       21  // left & right margin in mm
#define PDFBOTTOM     22  // bottom row
#define HANDLE        23  // handle
#define PAGES         24  // array of pages
#define REFS          25  // array of references
#define BOOKMARK      26  // array of bookmarks
#define HEADER        27  // array of headers
#define FONTS         28  // array of report fonts
#define IMAGES        29  // array of report images
#define PAGEIMAGES    30  // array of current page images
#define PAGEFONTS     31  // array of current page fonts
#define FONTWIDTH     32  // array of fonts width's
#define OPTIMIZE      33  // optimized ?
#define PDFCPI        34  // char per inch
#define PARAMLEN      34  // number of report elements

#define ALIGN_LEFT    1
#define ALIGN_CENTER  2
#define ALIGN_RIGHT   3
#define ALIGN_JUSTIFY 4

#define IMAGE_WIDTH   1
#define IMAGE_HEIGHT  2
#define IMAGE_XRES    3
#define IMAGE_YRES    4
#define IMAGE_BITS    5
#define IMAGE_FROM    6
#define IMAGE_LENGTH  7
#define IMAGE_SPACE   8

#define BYTE          1
#define ASCII         2
#define SHORT         3
#define LONG          4
#define RATIONAL      5
#define SBYTE         6
#define UNDEFINED     7
#define SSHORT        8
#define SLONG         9
#define SRATIONAL     10
#define FLOAT         11
#define DOUBLE        12

#define pdf_ALICEBLUE            "F0F8FF"
#define pdf_ANTIQUEWHITE         "FAEBD7"
#define pdf_AQUA                 "00FFFF"
#define pdf_AQUAMARINE           "7FFFD4"
#define pdf_AZURE                "F0FFFF"
#define pdf_BEIGE                "F5F5DC"
#define pdf_BISQUE               "FFE4C4"
#define pdf_BLACK                "000000"
#define pdf_BLANCHEDALMOND       "FFEBCD"
#define pdf_BLUE                 "0000FF"
#define pdf_BLUEVIOLET           "8A2BE2"
#define pdf_BROWN                "A52A2A"
#define pdf_BURLYWOOD            "DEB887"
#define pdf_CADETBLUE            "5F9EA0"
#define pdf_CHARTREUSE           "7FFF00"
#define pdf_CHOCOLATE            "D2691E"
#define pdf_CORAL                "FF7F50"
#define pdf_CORNFLOWERBLUE       "6495ED"
#define pdf_CORNSILK             "FFF8DC"
#define pdf_CRIMSON              "DC143C"
#define pdf_CYAN                 "00FFFF"
#define pdf_DARKBLUE             "00008B"
#define pdf_DARKCYAN             "008B8B"
#define pdf_DARKGOLDENROD        "B8860B"
#define pdf_DARKGRAY             "A9A9A9"
#define pdf_DARKGREEN            "006400"
#define pdf_DARKKHAKI            "BDB76B"
#define pdf_DARKMAGENTA          "8B008B"
#define pdf_DARKOLIVEGREEN       "556B2F"
#define pdf_DARKORANGE           "FF8C00"
#define pdf_DARKORCHID           "9932CC"
#define pdf_DARKRED              "8B0000"
#define pdf_DARKSALMON           "E9967A"
#define pdf_DARKSEAGREEN         "8FBC8F"
#define pdf_DARKSLATEBLUE        "483D8B"
#define pdf_DARKSLATEGRAY        "2F4F4F"
#define pdf_DARKTURQUOISE        "00CED1"
#define pdf_DARKVIOLET           "9400D3"
#define pdf_DEEPPINK             "FF1493"
#define pdf_DEEPSKYBLUE          "00BFFF"
#define pdf_DIMGRAY              "696969"
#define pdf_DODGERBLUE           "1E90FF"
#define pdf_FIREBRICK            "B22222"
#define pdf_FLORALWHITE          "FFFAF0"
#define pdf_FORESTGREEN          "228B22"
#define pdf_FUCHSIA              "FF00FF"
#define pdf_GAINSBORO            "DCDCDC"
#define pdf_GHOSTWHITE           "F8F8FF"
#define pdf_GOLD                 "FFD700"
#define pdf_GOLDENROD            "DAA520"
#define pdf_GRAY                 "808080"
#define pdf_GREEN                "008000"
#define pdf_GREENYELLOW          "ADFF2F"
#define pdf_HONEYDEW             "F0FFF0"
#define pdf_HOTPINK              "FF69B4"
#define pdf_INDIANRED            "CD5C5C"
#define pdf_INDIGO               "4B0082"
#define pdf_IVORY                "FFFFF0"
#define pdf_KHAKI                "F0E68C"
#define pdf_LAVENDER             "E6E6FA"
#define pdf_LAVENDERBLUSH        "FFF0F5"
#define pdf_LAWNGREEN            "7CFC00"
#define pdf_LEMONCHIFFON         "FFFACD"
#define pdf_LIGHTBLUE            "ADD8E6"
#define pdf_LIGHTCORAL           "F08080"
#define pdf_LIGHTCYAN            "E0FFFF"
#define pdf_LIGHTGOLDENRODYELLOW "FAFAD2"
#define pdf_LIGHTGREEN           "90EE90"
#define pdf_LIGHTGREY            "D3D3D3"
#define pdf_LIGHTPINK            "FFB6C1"
#define pdf_LIGHTSALMON          "FFA07A"
#define pdf_LIGHTSEAGREEN        "20B2AA"
#define pdf_LIGHTSKYBLUE         "87CEFA"
#define pdf_LIGHTSLATEGRAY       "778899"
#define pdf_LIGHTSTEELBLUE       "B0C4DE"
#define pdf_LIGHTYELLOW          "FFFFE0"
#define pdf_LIME                 "00FF00"
#define pdf_LIMEGREEN            "32CD32"
#define pdf_LINEN                "FAF0E6"
#define pdf_MAGENTA              "FF00FF"
#define pdf_MAROON               "800000"
#define pdf_MEDIUMAQUAMARINE     "66CDAA"
#define pdf_MEDIUMBLUE           "0000CD"
#define pdf_MEDIUMORCHID         "BA55D3"
#define pdf_MEDIUMPURPLE         "9370DB"
#define pdf_MEDIUMSEAGREEN       "3CB371"
#define pdf_MEDIUMSLATEBLUE      "7B68EE"
#define pdf_MEDIUMSPRINGGREEN    "00FA9A"
#define pdf_MEDIUMTURQUOISE      "48D1CC"
#define pdf_MEDIUMVIOLETRED      "C71585"
#define pdf_MIDNIGHTBLUE         "191970"
#define pdf_MINTCREAM            "F5FFFA"
#define pdf_MISTYROSE            "FFE4E1"
#define pdf_MOCCASIN             "FFE4B5"
#define pdf_NAVAJOWHITE          "FFDEAD"
#define pdf_NAVY                 "000080"
#define pdf_OLDLACE              "FDF5E6"
#define pdf_OLIVE                "808000"
#define pdf_OLIVEDRAB            "6B8E23"
#define pdf_ORANGE               "FFA500"
#define pdf_ORANGERED            "FF4500"
#define pdf_ORCHID               "DA70D6"
#define pdf_PALEGOLDENROD        "EEE8AA"
#define pdf_PALEGREEN            "98FB98"
#define pdf_PALETURQUOISE        "AFEEEE"
#define pdf_PALEVIOLETRED        "DB7093"
#define pdf_PAPAYAWHIP           "FFEFD5"
#define pdf_PEACHPUFF            "FFDAB9"
#define pdf_PERU                 "CD853F"
#define pdf_PINK                 "FFC0CB"
#define pdf_PLUM                 "DDADDD"
#define pdf_POWDERBLUE           "B0E0E6"
#define pdf_PURPLE               "800080"
#define pdf_RED                  "FF0000"
#define pdf_ROSYBROWN            "BC8F8F"
#define pdf_ROYALBLUE            "4169E1"
#define pdf_SADDLEBROWN          "8B4513"
#define pdf_SALMON               "FA8072"
#define pdf_SANDYBROWN           "F4A460"
#define pdf_SEAGREEN             "2E8B57"
#define pdf_SEASHELL             "FFF5EE"
#define pdf_SIENNA               "A0522D"
#define pdf_SILVER               "C0C0C0"
#define pdf_SKYBLUE              "87CEEB"
#define pdf_SLATEBLUE            "6A5ACD"
#define pdf_SLATEGRAY            "708090"
#define pdf_SNOW                 "FFFAFA"
#define pdf_SPRINGGREEN          "00FF7F"
#define pdf_STEELBLUE            "4682B4"
#define pdf_TAN                  "D2B48C"
#define pdf_TEAL                 "008080"
#define pdf_THISTLE              "D8BFD8"
#define pdf_TOMATO               "FF6347"
#define pdf_TURQUOISE            "40E0D0"
#define pdf_VIOLET               "EE82EE"
#define pdf_WHEAT                "F5DEB3"
#define pdf_WHITE                "FFFFFF"
#define pdf_WHITESMOKE           "F5F5F5"
#define pdf_YELLOW               "FFFF00"
#define pdf_YELLOWGREEN          "9ACD32"

#endif
