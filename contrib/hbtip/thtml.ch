/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Directives for HTML Classes
 *
 * Copyright 2007 Hannes Ziegler <hz/at/knowleXbase.com>
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

#ifndef  _HB_THTML
#define  _HB_THTML

/*
   content model shortcut encoding taken from Tidy library
   (www.sourceforge.net/tidy)
*/

#define CM_UNKNOWN      0
#define CM_EMPTY        0x000001   /* Elements with no content. Map to HTML specification. */
#define CM_HTML         0x000002   /* Elements that appear outside of "BODY". */
#define CM_HEAD         0x000004   /* Elements that can appear within HEAD. */
#define CM_BLOCK        0x000008   /* HTML "block" elements. */
#define CM_INLINE       0x000010   /* HTML "inline" elements. */
#define CM_LIST         0x000020   /* Elements that mark list item ("LI"). */
#define CM_DEFLIST      0x000040   /* Elements that mark definition list item ("DL", "DT"). */
#define CM_TABLE        0x000080   /* Elements that can appear inside TABLE. */
#define CM_ROWGRP       0x000100   /* Used for "THEAD", "TFOOT" or "TBODY". */
#define CM_ROW          0x000200   /* Used for "TD", "TH" */
#define CM_FIELD        0x000400   /* Elements whose content must be protected against white space movement.
                                      Includes some elements that can found in forms. */
#define CM_OBJECT       0x000800   /* Used to avoid propagating inline emphasis inside some elements
                                      such as OBJECT or APPLET. */
#define CM_PARAM        0x001000   /* Elements that allows "PARAM". */
#define CM_FRAMES       0x002000   /* "FRAME", "FRAMESET", "NOFRAMES". Used in ParseFrameSet. */
#define CM_HEADING      0x004000   /* Heading elements (h1, h2, ...). */
#define CM_OPT          0x008000   /* Elements with an optional end tag. */
#define CM_IMG          0x010000   /* Elements that use "align" attribute for vertical position. */
#define CM_MIXED        0x020000   /* Elements with inline and block model. Used to avoid calling InlineDup. */
#define CM_NO_INDENT    0x040000   /* Elements whose content needs to be indented only if containing one
                                      CM_BLOCK element. */
#define CM_OBSOLETE     0x080000   /* Elements that are obsolete (such as "dir", "menu"). */
#define CM_NEW          0x100000   /* User defined elements. Used to determine how attributes wihout value
                                      should be printed. */
#define CM_OMITST       0x200000   /* Elements that cannot be omitted. */

/* Constants for HTML attributes adopted from Tidy library (www.sourceforge.net/tidy) */

#define HTML_ATTR_ABBR                      1
#define HTML_ATTR_ACCEPT                    2
#define HTML_ATTR_ACCEPT_CHARSET            3
#define HTML_ATTR_ACCESSKEY                 4
#define HTML_ATTR_ACTION                    5
#define HTML_ATTR_ADD_DATE                  6
#define HTML_ATTR_ALIGN                     7
#define HTML_ATTR_ALINK                     8
#define HTML_ATTR_ALT                       9
#define HTML_ATTR_ARCHIVE                   10
#define HTML_ATTR_AXIS                      11
#define HTML_ATTR_BACKGROUND                12
#define HTML_ATTR_BGCOLOR                   13
#define HTML_ATTR_BGPROPERTIES              14
#define HTML_ATTR_BORDER                    15
#define HTML_ATTR_BORDERCOLOR               16
#define HTML_ATTR_BOTTOMMARGIN              17
#define HTML_ATTR_CELLPADDING               18
#define HTML_ATTR_CELLSPACING               19
#define HTML_ATTR_CHAR                      20
#define HTML_ATTR_CHAROFF                   21
#define HTML_ATTR_CHARSET                   22
#define HTML_ATTR_CHECKED                   23
#define HTML_ATTR_CITE                      24
#define HTML_ATTR_CLASS                     25
#define HTML_ATTR_CLASSID                   26
#define HTML_ATTR_CLEAR                     27
#define HTML_ATTR_CODE                      28
#define HTML_ATTR_CODEBASE                  29
#define HTML_ATTR_CODETYPE                  30
#define HTML_ATTR_COLOR                     31
#define HTML_ATTR_COLS                      32
#define HTML_ATTR_COLSPAN                   33
#define HTML_ATTR_COMPACT                   34
#define HTML_ATTR_CONTENT                   35
#define HTML_ATTR_COORDS                    36
#define HTML_ATTR_DATA                      37
#define HTML_ATTR_DATAFLD                   38
#define HTML_ATTR_DATAFORMATAS              39
#define HTML_ATTR_DATAPAGESIZE              40
#define HTML_ATTR_DATASRC                   41
#define HTML_ATTR_DATETIME                  42
#define HTML_ATTR_DECLARE                   43
#define HTML_ATTR_DEFER                     44
#define HTML_ATTR_DIR                       45
#define HTML_ATTR_DISABLED                  46
#define HTML_ATTR_ENCODING                  47
#define HTML_ATTR_ENCTYPE                   48
#define HTML_ATTR_EVENT                     49
#define HTML_ATTR_FACE                      50
#define HTML_ATTR_FOR                       51
#define HTML_ATTR_FRAME                     52
#define HTML_ATTR_FRAMEBORDER               53
#define HTML_ATTR_FRAMESPACING              54
#define HTML_ATTR_GRIDX                     55
#define HTML_ATTR_GRIDY                     56
#define HTML_ATTR_HEADERS                   57
#define HTML_ATTR_HEIGHT                    58
#define HTML_ATTR_HREF                      59
#define HTML_ATTR_HREFLANG                  60
#define HTML_ATTR_HSPACE                    61
#define HTML_ATTR_HTTP_EQUIV                62
#define HTML_ATTR_ID                        63
#define HTML_ATTR_ISMAP                     64
#define HTML_ATTR_LABEL                     65
#define HTML_ATTR_LANG                      66
#define HTML_ATTR_LANGUAGE                  67
#define HTML_ATTR_LAST_MODIFIED             68
#define HTML_ATTR_LAST_VISIT                69
#define HTML_ATTR_LEFTMARGIN                70
#define HTML_ATTR_LINK                      71
#define HTML_ATTR_LONGDESC                  72
#define HTML_ATTR_LOWSRC                    73
#define HTML_ATTR_MARGINHEIGHT              74
#define HTML_ATTR_MARGINWIDTH               75
#define HTML_ATTR_MAXLENGTH                 76
#define HTML_ATTR_MEDIA                     77
#define HTML_ATTR_METHOD                    78
#define HTML_ATTR_METHODS                   79
#define HTML_ATTR_MULTIPLE                  80
#define HTML_ATTR_N                         81
#define HTML_ATTR_NAME                      82
#define HTML_ATTR_NOHREF                    83
#define HTML_ATTR_NORESIZE                  84
#define HTML_ATTR_NOSHADE                   85
#define HTML_ATTR_NOWRAP                    86
#define HTML_ATTR_OBJECT                    87
#define HTML_ATTR_ONAFTERUPDATE             88
#define HTML_ATTR_ONBEFOREUNLOAD            89
#define HTML_ATTR_ONBEFOREUPDATE            90
#define HTML_ATTR_ONBLUR                    91
#define HTML_ATTR_ONCHANGE                  92
#define HTML_ATTR_ONCLICK                   93
#define HTML_ATTR_ONDATAAVAILABLE           94
#define HTML_ATTR_ONDATASETCHANGED          95
#define HTML_ATTR_ONDATASETCOMPLETE         96
#define HTML_ATTR_ONDBLCLICK                97
#define HTML_ATTR_ONERRORUPDATE             98
#define HTML_ATTR_ONFOCUS                   99
#define HTML_ATTR_ONKEYDOWN                 100
#define HTML_ATTR_ONKEYPRESS                101
#define HTML_ATTR_ONKEYUP                   102
#define HTML_ATTR_ONLOAD                    103
#define HTML_ATTR_ONMOUSEDOWN               104
#define HTML_ATTR_ONMOUSEMOVE               105
#define HTML_ATTR_ONMOUSEOUT                106
#define HTML_ATTR_ONMOUSEOVER               107
#define HTML_ATTR_ONMOUSEUP                 108
#define HTML_ATTR_ONRESET                   109
#define HTML_ATTR_ONROWENTER                110
#define HTML_ATTR_ONROWEXIT                 111
#define HTML_ATTR_ONSELECT                  112
#define HTML_ATTR_ONSUBMIT                  113
#define HTML_ATTR_ONUNLOAD                  114
#define HTML_ATTR_PROFILE                   115
#define HTML_ATTR_PROMPT                    116
#define HTML_ATTR_RBSPAN                    117
#define HTML_ATTR_READONLY                  118
#define HTML_ATTR_REL                       119
#define HTML_ATTR_REV                       120
#define HTML_ATTR_RIGHTMARGIN               121
#define HTML_ATTR_ROWS                      122
#define HTML_ATTR_ROWSPAN                   123
#define HTML_ATTR_RULES                     124
#define HTML_ATTR_SCHEME                    125
#define HTML_ATTR_SCOPE                     126
#define HTML_ATTR_SCROLLING                 127
#define HTML_ATTR_SDAFORM                   128
#define HTML_ATTR_SDAPREF                   129
#define HTML_ATTR_SDASUFF                   130
#define HTML_ATTR_SELECTED                  131
#define HTML_ATTR_SHAPE                     132
#define HTML_ATTR_SHOWGRID                  133
#define HTML_ATTR_SHOWGRIDX                 134
#define HTML_ATTR_SHOWGRIDY                 135
#define HTML_ATTR_SIZE                      136
#define HTML_ATTR_SPAN                      137
#define HTML_ATTR_SRC                       138
#define HTML_ATTR_STANDBY                   139
#define HTML_ATTR_START                     140
#define HTML_ATTR_STYLE                     141
#define HTML_ATTR_SUMMARY                   142
#define HTML_ATTR_TABINDEX                  143
#define HTML_ATTR_TARGET                    144
#define HTML_ATTR_TEXT                      145
#define HTML_ATTR_TITLE                     146
#define HTML_ATTR_TOPMARGIN                 147
#define HTML_ATTR_TYPE                      148
#define HTML_ATTR_UNKNOWN                   149
#define HTML_ATTR_URN                       150
#define HTML_ATTR_USEMAP                    151
#define HTML_ATTR_VALIGN                    152
#define HTML_ATTR_VALUE                     153
#define HTML_ATTR_VALUETYPE                 154
#define HTML_ATTR_VERSION                   155
#define HTML_ATTR_VLINK                     156
#define HTML_ATTR_VSPACE                    157
#define HTML_ATTR_WIDTH                     158
#define HTML_ATTR_WRAP                      159
#define HTML_ATTR_XMLNS                     160
#define HTML_ATTR_XML_LANG                  161
#define HTML_ATTR_XML_SPACE                 162

#define HTML_ATTR_COUNT                     162

#define HTML_ATTR_TYPE_UNKNOWN              0
#define HTML_ATTR_TYPE_ACTION               1
#define HTML_ATTR_TYPE_ALIGN                2
#define HTML_ATTR_TYPE_BOOL                 3
#define HTML_ATTR_TYPE_BORDER               4
#define HTML_ATTR_TYPE_CHARACTER            5
#define HTML_ATTR_TYPE_CHARSET              6
#define HTML_ATTR_TYPE_CLEAR                7
#define HTML_ATTR_TYPE_COLOR                8
#define HTML_ATTR_TYPE_COLS                 9
#define HTML_ATTR_TYPE_COORDS               10
#define HTML_ATTR_TYPE_DATE                 11
#define HTML_ATTR_TYPE_FBORDER              12
#define HTML_ATTR_TYPE_FSUBMIT              13
#define HTML_ATTR_TYPE_IDDEF                14
#define HTML_ATTR_TYPE_IDREF                15
#define HTML_ATTR_TYPE_IDREFS               16
#define HTML_ATTR_TYPE_LANG                 17
#define HTML_ATTR_TYPE_LENGTH               18
#define HTML_ATTR_TYPE_LINKTYPES            19
#define HTML_ATTR_TYPE_MEDIA                20
#define HTML_ATTR_TYPE_NAME                 21
#define HTML_ATTR_TYPE_NUMBER               22
#define HTML_ATTR_TYPE_PCDATA               23
#define HTML_ATTR_TYPE_SCOPE                24
#define HTML_ATTR_TYPE_SCRIPT               25
#define HTML_ATTR_TYPE_SCROLL               26
#define HTML_ATTR_TYPE_SHAPE                27
#define HTML_ATTR_TYPE_TARGET               28
#define HTML_ATTR_TYPE_TEXTDIR              29
#define HTML_ATTR_TYPE_TFRAME               30
#define HTML_ATTR_TYPE_TRULES               31
#define HTML_ATTR_TYPE_TYPE                 32
#define HTML_ATTR_TYPE_URL                  33
#define HTML_ATTR_TYPE_URLS                 34
#define HTML_ATTR_TYPE_VALIGN               35
#define HTML_ATTR_TYPE_VTYPE                36
#define HTML_ATTR_TYPE_XTYPE                37

#define HTML_ATTR_TYPE_COUNT                37

#endif /* _HB_THTML */
