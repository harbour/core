/*
 * Directives for HTML Classes
 *
 * Copyright 2007 Hannes Ziegler <hz/at/knowleXbase.com>
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

#ifndef _HB_THTML
#define _HB_THTML

/* Content model shortcut encoding taken from Tidy library
   https://github.com/htacg/tidy-html5 */

#define CM_UNKNOWN                  0
#define CM_EMPTY                    0x000001  /* Elements with no content. Map to HTML specification. */
#define CM_HTML                     0x000002  /* Elements that appear outside of "body". */
#define CM_HEAD                     0x000004  /* Elements that can appear within HEAD. */
#define CM_BLOCK                    0x000008  /* HTML "block" elements. */
#define CM_INLINE                   0x000010  /* HTML "inline" elements. */
#define CM_LIST                     0x000020  /* Elements that mark list item ("li"). */
#define CM_DEFLIST                  0x000040  /* Elements that mark definition list item ("dl", "dt"). */
#define CM_TABLE                    0x000080  /* Elements that can appear inside TABLE. */
#define CM_ROWGRP                   0x000100  /* Used for "thead", "tfoot" or "tbody". */
#define CM_ROW                      0x000200  /* Used for "td", "th" */
#define CM_FIELD                    0x000400  /* Elements whose content must be protected against white space movement.
                                                 Includes some elements that can found in forms. */
#define CM_OBJECT                   0x000800  /* Used to avoid propagating inline emphasis inside some elements
                                                 such as OBJECT or APPLET. */
#define CM_PARAM                    0x001000  /* Elements that allows "PARAM". */
#define CM_FRAMES                   0x002000  /* "FRAME", "FRAMESET", "NOFRAMES". Used in ParseFrameSet. */
#define CM_HEADING                  0x004000  /* Heading elements (h1, h2, ...). */
#define CM_OPT                      0x008000  /* Elements with an optional end tag. */
#define CM_IMG                      0x010000  /* Elements that use "align" attribute for vertical position. */
#define CM_MIXED                    0x020000  /* Elements with inline and block model. Used to avoid calling InlineDup. */
#define CM_NO_INDENT                0x040000  /* Elements whose content needs to be indented only if containing one
                                                 CM_BLOCK element. */
#define CM_OBSOLETE                 0x080000  /* Elements that are obsolete (such as "dir", "menu"). */
#define CM_NEW                      0x100000  /* User defined elements. Used to determine how attributes without value
                                                 should be printed. */
#define CM_OMITST                   0x200000  /* Elements that cannot be omitted. */

/* Constants for HTML attributes adopted from Tidy library */

#define HTML_ATTR_ABBR                  1
#define HTML_ATTR_ACCEPT                2
#define HTML_ATTR_ACCEPT_CHARSET        3
#define HTML_ATTR_ACCESSKEY             4
#define HTML_ATTR_ACTION                5
#define HTML_ATTR_ADD_DATE              6
#define HTML_ATTR_ALIGN                 7
#define HTML_ATTR_ALINK                 8
#define HTML_ATTR_ALT                   9
#define HTML_ATTR_ARCHIVE               10
#define HTML_ATTR_AXIS                  11
#define HTML_ATTR_BACKGROUND            12
#define HTML_ATTR_BGCOLOR               13
#define HTML_ATTR_BGPROPERTIES          14
#define HTML_ATTR_BORDER                15
#define HTML_ATTR_BORDERCOLOR           16
#define HTML_ATTR_BOTTOMMARGIN          17
#define HTML_ATTR_CELLPADDING           18
#define HTML_ATTR_CELLSPACING           19
#define HTML_ATTR_CHAR                  20
#define HTML_ATTR_CHAROFF               21
#define HTML_ATTR_CHARSET               22
#define HTML_ATTR_CHECKED               23
#define HTML_ATTR_CITE                  24
#define HTML_ATTR_CLASS                 25
#define HTML_ATTR_CLASSID               26
#define HTML_ATTR_CLEAR                 27
#define HTML_ATTR_CODE                  28
#define HTML_ATTR_CODEBASE              29
#define HTML_ATTR_CODETYPE              30
#define HTML_ATTR_COLOR                 31
#define HTML_ATTR_COLS                  32
#define HTML_ATTR_COLSPAN               33
#define HTML_ATTR_COMPACT               34
#define HTML_ATTR_CONTENT               35
#define HTML_ATTR_COORDS                36
#define HTML_ATTR_DATA                  37
#define HTML_ATTR_DATAFLD               38
#define HTML_ATTR_DATAFORMATAS          39
#define HTML_ATTR_DATAPAGESIZE          40
#define HTML_ATTR_DATASRC               41
#define HTML_ATTR_DATETIME              42
#define HTML_ATTR_DECLARE               43
#define HTML_ATTR_DEFER                 44
#define HTML_ATTR_DIR                   45
#define HTML_ATTR_DISABLED              46
#define HTML_ATTR_ENCODING              47
#define HTML_ATTR_ENCTYPE               48
#define HTML_ATTR_EVENT                 49
#define HTML_ATTR_FACE                  50
#define HTML_ATTR_FOR                   51
#define HTML_ATTR_FRAME                 52
#define HTML_ATTR_FRAMEBORDER           53
#define HTML_ATTR_FRAMESPACING          54
#define HTML_ATTR_GRIDX                 55
#define HTML_ATTR_GRIDY                 56
#define HTML_ATTR_HEADERS               57
#define HTML_ATTR_HEIGHT                58
#define HTML_ATTR_HREF                  59
#define HTML_ATTR_HREFLANG              60
#define HTML_ATTR_HSPACE                61
#define HTML_ATTR_HTTP_EQUIV            62
#define HTML_ATTR_ID                    63
#define HTML_ATTR_ISMAP                 64
#define HTML_ATTR_LABEL                 65
#define HTML_ATTR_LANG                  66
#define HTML_ATTR_LANGUAGE              67
#define HTML_ATTR_LAST_MODIFIED         68
#define HTML_ATTR_LAST_VISIT            69
#define HTML_ATTR_LEFTMARGIN            70
#define HTML_ATTR_LINK                  71
#define HTML_ATTR_LONGDESC              72
#define HTML_ATTR_LOWSRC                73
#define HTML_ATTR_MARGINHEIGHT          74
#define HTML_ATTR_MARGINWIDTH           75
#define HTML_ATTR_MAXLENGTH             76
#define HTML_ATTR_MEDIA                 77
#define HTML_ATTR_METHOD                78
#define HTML_ATTR_METHODS               79
#define HTML_ATTR_MULTIPLE              80
#define HTML_ATTR_N                     81
#define HTML_ATTR_NAME                  82
#define HTML_ATTR_NOHREF                83
#define HTML_ATTR_NORESIZE              84
#define HTML_ATTR_NOSHADE               85
#define HTML_ATTR_NOWRAP                86
#define HTML_ATTR_OBJECT                87
#define HTML_ATTR_ONAFTERUPDATE         88
#define HTML_ATTR_ONBEFOREUNLOAD        89
#define HTML_ATTR_ONBEFOREUPDATE        90
#define HTML_ATTR_ONBLUR                91
#define HTML_ATTR_ONCHANGE              92
#define HTML_ATTR_ONCLICK               93
#define HTML_ATTR_ONDATAAVAILABLE       94
#define HTML_ATTR_ONDATASETCHANGED      95
#define HTML_ATTR_ONDATASETCOMPLETE      96
#define HTML_ATTR_ONDBLCLICK            97
#define HTML_ATTR_ONERRORUPDATE         98
#define HTML_ATTR_ONFOCUS               99
#define HTML_ATTR_ONKEYDOWN             100
#define HTML_ATTR_ONKEYPRESS            101
#define HTML_ATTR_ONKEYUP               102
#define HTML_ATTR_ONLOAD                103
#define HTML_ATTR_ONMOUSEDOWN           104
#define HTML_ATTR_ONMOUSEMOVE           105
#define HTML_ATTR_ONMOUSEOUT            106
#define HTML_ATTR_ONMOUSEOVER           107
#define HTML_ATTR_ONMOUSEUP             108
#define HTML_ATTR_ONRESET               109
#define HTML_ATTR_ONROWENTER            110
#define HTML_ATTR_ONROWEXIT             111
#define HTML_ATTR_ONSELECT              112
#define HTML_ATTR_ONSUBMIT              113
#define HTML_ATTR_ONUNLOAD              114
#define HTML_ATTR_PROFILE               115
#define HTML_ATTR_PROMPT                116
#define HTML_ATTR_RBSPAN                117
#define HTML_ATTR_READONLY              118
#define HTML_ATTR_REL                   119
#define HTML_ATTR_REV                   120
#define HTML_ATTR_RIGHTMARGIN           121
#define HTML_ATTR_ROWS                  122
#define HTML_ATTR_ROWSPAN               123
#define HTML_ATTR_RULES                 124
#define HTML_ATTR_SCHEME                125
#define HTML_ATTR_SCOPE                 126
#define HTML_ATTR_SCROLLING             127
#define HTML_ATTR_SDAFORM               128
#define HTML_ATTR_SDAPREF               129
#define HTML_ATTR_SDASUFF               130
#define HTML_ATTR_SELECTED              131
#define HTML_ATTR_SHAPE                 132
#define HTML_ATTR_SHOWGRID              133
#define HTML_ATTR_SHOWGRIDX             134
#define HTML_ATTR_SHOWGRIDY             135
#define HTML_ATTR_SIZE                  136
#define HTML_ATTR_SPAN                  137
#define HTML_ATTR_SRC                   138
#define HTML_ATTR_STANDBY               139
#define HTML_ATTR_START                 140
#define HTML_ATTR_STYLE                 141
#define HTML_ATTR_SUMMARY               142
#define HTML_ATTR_TABINDEX              143
#define HTML_ATTR_TARGET                144
#define HTML_ATTR_TEXT                  145
#define HTML_ATTR_TITLE                 146
#define HTML_ATTR_TOPMARGIN             147
#define HTML_ATTR_TYPE                  148
#define HTML_ATTR_UNKNOWN               149
#define HTML_ATTR_URN                   150
#define HTML_ATTR_USEMAP                151
#define HTML_ATTR_VALIGN                152
#define HTML_ATTR_VALUE                 153
#define HTML_ATTR_VALUETYPE             154
#define HTML_ATTR_VERSION               155
#define HTML_ATTR_VLINK                 156
#define HTML_ATTR_VSPACE                157
#define HTML_ATTR_WIDTH                 158
#define HTML_ATTR_WRAP                  159
#define HTML_ATTR_XMLNS                 160
#define HTML_ATTR_XML_LANG              161
#define HTML_ATTR_XML_SPACE             162
#define HTML_ATTR_ALLOWFULLSCREEN       163
#define HTML_ATTR_DOWNLOAD              164
#define HTML_ATTR_IS                    165
#define HTML_ATTR_ITEMID                166
#define HTML_ATTR_ITEMPROP              167
#define HTML_ATTR_ITEMREF               168
#define HTML_ATTR_ITEMSCOPE             169
#define HTML_ATTR_ITEMTYPE              170
#define HTML_ATTR_ROLE                  171
#define HTML_ATTR_SRCSET                172
#define HTML_ATTR_TRANSLATE             173
#define HTML_ATTR_ASYNC                 174
#define HTML_ATTR_AUTOCOMPLETE          175
#define HTML_ATTR_AUTOFOCUS             176
#define HTML_ATTR_AUTOPLAY              177
#define HTML_ATTR_CHALLENGE             178
#define HTML_ATTR_CONTENTEDITABLE       179
#define HTML_ATTR_CONTEXTMENU           180
#define HTML_ATTR_CONTROLS              181
#define HTML_ATTR_CROSSORIGIN           182
#define HTML_ATTR_DEFAULT               183
#define HTML_ATTR_DIRNAME               184
#define HTML_ATTR_DRAGGABLE             185
#define HTML_ATTR_DROPZONE              186
#define HTML_ATTR_FORM                  187
#define HTML_ATTR_FORMACTION            188
#define HTML_ATTR_FORMENCTYPE           189
#define HTML_ATTR_FORMMETHOD            190
#define HTML_ATTR_FORMNOVALIDATE        191
#define HTML_ATTR_FORMTARGET            192
#define HTML_ATTR_HIDDEN                193
#define HTML_ATTR_HIGH                  194
#define HTML_ATTR_ICON                  195
#define HTML_ATTR_KEYTYPE               196
#define HTML_ATTR_KIND                  197
#define HTML_ATTR_LIST                  198
#define HTML_ATTR_LOOP                  199
#define HTML_ATTR_LOW                   200
#define HTML_ATTR_MANIFEST              201
#define HTML_ATTR_MAX                   202
#define HTML_ATTR_MEDIAGROUP            203
#define HTML_ATTR_MIN                   204
#define HTML_ATTR_MUTED                 205
#define HTML_ATTR_NOVALIDATE            206
#define HTML_ATTR_OPEN                  207
#define HTML_ATTR_OPTIMUM               208
#define HTML_ATTR_ONABORT               209
#define HTML_ATTR_ONAFTERPRINT          210
#define HTML_ATTR_ONBEFOREPRINT         211
#define HTML_ATTR_ONCANPLAY             212
#define HTML_ATTR_ONCANPLAYTHROUGH      213
#define HTML_ATTR_ONCONTEXTMENU         214
#define HTML_ATTR_ONCUECHANGE           215
#define HTML_ATTR_ONDRAG                216
#define HTML_ATTR_ONDRAGEND             217
#define HTML_ATTR_ONDRAGENTER           218
#define HTML_ATTR_ONDRAGLEAVE           219
#define HTML_ATTR_ONDRAGOVER            220
#define HTML_ATTR_ONDRAGSTART           221
#define HTML_ATTR_ONDROP                222
#define HTML_ATTR_ONDURATIONCHANGE      223
#define HTML_ATTR_ONEMPTIED             224
#define HTML_ATTR_ONENDED               225
#define HTML_ATTR_ONERROR               226
#define HTML_ATTR_ONHASHCHANGE          227
#define HTML_ATTR_ONINPUT               228
#define HTML_ATTR_ONINVALID             229
#define HTML_ATTR_ONLOADEDDATA          230
#define HTML_ATTR_ONLOADEDMETADATA      231
#define HTML_ATTR_ONLOADSTART           232
#define HTML_ATTR_ONMESSAGE             233
#define HTML_ATTR_ONMOUSEWHEEL          234
#define HTML_ATTR_ONOFFLINE             235
//AQUI
#define HTML_ATTR_ONONLINE              236
#define HTML_ATTR_ONPAGEHIDE            237
#define HTML_ATTR_ONPAGESHOW            238
#define HTML_ATTR_ONPAUSE               239
#define HTML_ATTR_ONPLAY                240
#define HTML_ATTR_ONPLAYING             241
#define HTML_ATTR_ONPOPSTATE            242
#define HTML_ATTR_ONPROGRESS            243
#define HTML_ATTR_ONRATECHANGE          244
#define HTML_ATTR_ONREADYSTATECHANGE    245
#define HTML_ATTR_ONREDO                246
#define HTML_ATTR_ONRESIZE              247
#define HTML_ATTR_ONSCROLL              248
#define HTML_ATTR_ONSEEKED              249
#define HTML_ATTR_ONSEEKING             250
#define HTML_ATTR_ONSHOW                251
#define HTML_ATTR_ONSTALLED             252
#define HTML_ATTR_ONSTORAGE             253
#define HTML_ATTR_ONSUSPEND             254
#define HTML_ATTR_ONTIMEUPDATE          255
#define HTML_ATTR_ONUNDO                256
#define HTML_ATTR_ONVOLUMECHANGE        257
#define HTML_ATTR_ONWAITING             258
#define HTML_ATTR_PATTERN               259
#define HTML_ATTR_PLACEHOLDER           260
#define HTML_ATTR_PLAYSINLINE           261
#define HTML_ATTR_POSTER                262
#define HTML_ATTR_PRELOAD               263
#define HTML_ATTR_PUBDATE               264
#define HTML_ATTR_RADIOGROUP            265
#define HTML_ATTR_REQUIRED              266
#define HTML_ATTR_REVERSED              267
#define HTML_ATTR_SANDBOX               268
#define HTML_ATTR_SCOPED                269
#define HTML_ATTR_SEAMLESS              270
#define HTML_ATTR_SIZES                 271
#define HTML_ATTR_SPELLCHECK            272
#define HTML_ATTR_SRCDOC                273
#define HTML_ATTR_SRCLANG               274
#define HTML_ATTR_STEP                  275
#define HTML_ATTR_ARIA_ACTIVEDESCENDANT 276
#define HTML_ATTR_ARIA_ATOMIC           277
#define HTML_ATTR_ARIA_AUTOCOMPLETE     278
#define HTML_ATTR_ARIA_BUSY             279
#define HTML_ATTR_ARIA_CHECKED          280
#define HTML_ATTR_ARIA_CONTROLS         281
#define HTML_ATTR_ARIA_DESCRIBEDBY      282
#define HTML_ATTR_ARIA_DISABLED         283
#define HTML_ATTR_ARIA_DROPEFFECT       284
#define HTML_ATTR_ARIA_EXPANDED         285
#define HTML_ATTR_ARIA_FLOWTO           286
#define HTML_ATTR_ARIA_GRABBED          287
#define HTML_ATTR_ARIA_HASPOPUP         288
#define HTML_ATTR_ARIA_HIDDEN           289
#define HTML_ATTR_ARIA_INVALID          290
#define HTML_ATTR_ARIA_LABEL            291
#define HTML_ATTR_ARIA_LABELLEDBY       292
#define HTML_ATTR_ARIA_LEVEL            293
#define HTML_ATTR_ARIA_LIVE             294
#define HTML_ATTR_ARIA_MULTILINE        295
#define HTML_ATTR_ARIA_MULTISELECTABLE  296
#define HTML_ATTR_ARIA_ORIENTATION      297
#define HTML_ATTR_ARIA_OWNS             298
#define HTML_ATTR_ARIA_POSINSET         299
#define HTML_ATTR_ARIA_PRESSED          300
#define HTML_ATTR_ARIA_READONLY         301
#define HTML_ATTR_ARIA_RELEVANT         302
#define HTML_ATTR_ARIA_REQUIRED         303
#define HTML_ATTR_ARIA_SELECTED         304
#define HTML_ATTR_ARIA_SETSIZE          305
#define HTML_ATTR_ARIA_SORT             306
#define HTML_ATTR_ARIA_VALUEMAX         307
#define HTML_ATTR_ARIA_VALUEMIN         308
#define HTML_ATTR_ARIA_VALUENOW         309
#define HTML_ATTR_ARIA_VALUETEXT
/* SVG attributes (SVG 1.1) */
#define HTML_ATTR_X                     310
#define HTML_ATTR_Y                     311
#define HTML_ATTR_VIEWBOX               312
#define HTML_ATTR_PRESERVEASPECTRATIO   313
#define HTML_ATTR_ZOOMANDPAN            314
#define HTML_ATTR_BASEPROFILE           315
#define HTML_ATTR_CONTENTSCRIPTTYPE     316
#define HTML_ATTR_CONTENTSTYLETYPE      317
/* MathML <math> attributes */
#define HTML_ATTR_DISPLAY               318
/* RDFa global attributes */
#define HTML_ATTR_ABOUT                 319
#define HTML_ATTR_DATATYPE              320
#define HTML_ATTR_INLIST                321
#define HTML_ATTR_PREFIX                322
#define HTML_ATTR_PROPERTY              323
#define HTML_ATTR_RESOURCE              324
#define HTML_ATTR_TYPEOF                325
#define HTML_ATTR_VOCAB                 326
#define HTML_ATTR_INTEGRITY             327
#define HTML_ATTR_AS                    328
#define HTML_ATTR_XMLNSXLINK            329
#define HTML_ATTR_SLOT                  330
#define HTML_ATTR_LOADING               331
/* SVG paint attributes (SVG 1.1) */
#define HTML_ATTR_FILL                  332
#define HTML_ATTR_FILLRULE              333
#define HTML_ATTR_STROKE                334
#define HTML_ATTR_STROKEDASHARRAY       335
#define HTML_ATTR_STROKEDASHOFFSET      336
#define HTML_ATTR_STROKELINECAP         337
#define HTML_ATTR_STROKELINEJOIN        338
#define HTML_ATTR_STROKEMITERLIMIT      339
#define HTML_ATTR_STROKEWIDTH           340
#define HTML_ATTR_COLORINTERPOLATION    341
#define HTML_ATTR_COLORRENDERING        342
#define HTML_ATTR_OPACITY               343
#define HTML_ATTR_STROKEOPACITY         344
#define HTML_ATTR_FILLOPACITY           345
#define HTML_ATTR_AUTOCAPITALIZE        346
#define HTML_ATTR_ENTERKEYHINT          347
#define HTML_ATTR_EXPORTPARTS           348
#define HTML_ATTR_INPUTMODE             349
#define HTML_ATTR_NONCE                 350
#define HTML_ATTR_PART                  351

#define HTML_ATTR_COUNT                 351

#define HTML_ATTR_TYPE_UNKNOWN          0
#define HTML_ATTR_TYPE_ACTION           1
#define HTML_ATTR_TYPE_ALIGN            2
#define HTML_ATTR_TYPE_BOOL             3
#define HTML_ATTR_TYPE_BORDER           4
#define HTML_ATTR_TYPE_CHARACTER        5
#define HTML_ATTR_TYPE_CHARSET          6
#define HTML_ATTR_TYPE_CLEAR            7
#define HTML_ATTR_TYPE_COLOR            8
#define HTML_ATTR_TYPE_COLS             9
#define HTML_ATTR_TYPE_COORDS           10
#define HTML_ATTR_TYPE_DATE             11
#define HTML_ATTR_TYPE_FBORDER          12
#define HTML_ATTR_TYPE_FSUBMIT          13
#define HTML_ATTR_TYPE_IDDEF            14
#define HTML_ATTR_TYPE_IDREF            15
#define HTML_ATTR_TYPE_IDREFS           16
#define HTML_ATTR_TYPE_LANG             17
#define HTML_ATTR_TYPE_LENGTH           18
#define HTML_ATTR_TYPE_LINKTYPES        19
#define HTML_ATTR_TYPE_MEDIA            20
#define HTML_ATTR_TYPE_NAME             21
#define HTML_ATTR_TYPE_NUMBER           22
#define HTML_ATTR_TYPE_PCDATA           23
#define HTML_ATTR_TYPE_SCOPE            24
#define HTML_ATTR_TYPE_SCRIPT           25
#define HTML_ATTR_TYPE_SCROLL           26
#define HTML_ATTR_TYPE_SHAPE            27
#define HTML_ATTR_TYPE_TARGET           28
#define HTML_ATTR_TYPE_TEXTDIR          29
#define HTML_ATTR_TYPE_TFRAME           30
#define HTML_ATTR_TYPE_TRULES           31
#define HTML_ATTR_TYPE_TYPE             32
#define HTML_ATTR_TYPE_URL              33
#define HTML_ATTR_TYPE_URLS             34
#define HTML_ATTR_TYPE_VALIGN           35
#define HTML_ATTR_TYPE_VTYPE            36
#define HTML_ATTR_TYPE_XTYPE            37
#define HTML_ATTR_TYPE_FVALIDATE        38
#define HTML_ATTR_TYPE_ICON             39
#define HTML_ATTR_TYPE_LIST             40

#define HTML_ATTR_TYPE_COUNT            40

#endif /* _HB_THTML */
