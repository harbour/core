/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   Header file for CT error codes
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
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

/* NOTE: This file is also used by C code. */

#ifndef _CTERROR_CH
#define _CTERROR_CH

/* sub code ranges */
#define CT_ERROR_MATHLIB_RANGEL     100  /* math lib errors */
#define CT_ERROR_MATHLIB_RANGEH     199

#define CT_ERROR_GENERAL_RANGEL    1000 /* general functions */
#define CT_ERROR_GENERAL_RANGEH    1099

#define CT_ERROR_WINDOW_RANGEL     1110 /* windowing functions */
#define CT_ERROR_WINDOW_RANGEH     1399

#define CT_ERROR_EXTDRV_RANGEL     1410 /* extended driver functions */
#define CT_ERROR_EXTDRV_RANGEH     1999

#define CT_ERROR_SERIAL_RANGEL     2110 /* serial communication functions */
#define CT_ERROR_SERIAL_RANGEH     2599

#define CT_ERROR_STRING_RANGEL     3110 /* string functions */
#define CT_ERROR_STRING_RANGEH     4099

#define CT_ERROR_NUMBIT_RANGEL     4110 /* number and bit manipulation functions */
#define CT_ERROR_NUMBIT_RANGEH     4399

#define CT_ERROR_VIDEO_RANGEL      4410 /* video functions */
#define CT_ERROR_VIDEO_RANGEH      5099

#define CT_ERROR_DISC_RANGEL       5110 /* disc functions */
#define CT_ERROR_DISC_RANGEH       5699

#define CT_ERROR_PRINT_RANGEL      5710 /* printer functions */
#define CT_ERROR_PRINT_RANGEH      5899

#define CT_ERROR_DATE_RANGEL       5910 /* date & time functions */
#define CT_ERROR_DATE_RANGEH       6199

#define CT_ERROR_DBF_RANGEL        6210 /* DBF functions */
#define CT_ERROR_DBF_RANGEH        6299

#define CT_ERROR_SWITCH_RANGEL     6310 /* switch functions */
#define CT_ERROR_SWITCH_RANGEH     6799

#define CT_ERROR_SYSINF_RANGEL     6810 /* system info functions */
#define CT_ERROR_SYSINF_RANGEH     7099

#define CT_ERROR_MISC_RANGEL       7110 /* misc. functions */
#define CT_ERROR_MISC_RANGEH       7399

#define CT_ERROR_MATH_RANGEL       7410 /* math functions */
#define CT_ERROR_MATH_RANGEH       7699

#define CT_ERROR_PEEK_RANGEL       7810 /* peek & poke functions */
#define CT_ERROR_PEEK_RANGEH       7899

#define CT_ERROR_GETREAD_RANGEL    7910 /* get & read functions */
#define CT_ERROR_GETREAD_RANGEH    8099

/* C math lib error sub codes */
#define CT_ERROR_MATHLIB           100  /* unknown math lib error */
#define CT_ERROR_MATHLIB_DOMAIN    101  /* a domain error has occured, such as sqrt( -1 ) */
#define CT_ERROR_MATHLIB_SING      102  /* a singularity will result, such as pow( 0, -2 ) */
#define CT_ERROR_MATHLIB_OVERFLOW  103  /* an overflow will result, such as pow( 10, 100 ) */
#define CT_ERROR_MATHLIB_UNDERFLOW 104  /* an underflow will result, such as pow( 10, -100 ) */
#define CT_ERROR_MATHLIB_TLOSS     105  /* total loss of significance will result, such as exp( 1000 ) */
#define CT_ERROR_MATHLIB_PLOSS     106  /* partial loss of significance will result, such as sin( 10e70 ) */

/*             
 * function error sub codes 
 * 
 * The sub code simply defines the function that throws the error.
 * The last digit stands for the type of the return value of the function:
 *
 *    0 == NIL
 *    1 == String/Memo
 *    2 == Numeric (Integer)
 *    3 == Numeric (Float)
 *    4 == Boolean
 *    5 == Date
 *    6 == Block
 *    7 == Array
 *    8 == Object
 *    9 == can not be specified
 *
 * This can be useful for custom errorblocks, since the CT3 library allows
 * to set a return value when (for example) an argument error occurs.
 *
 */

/* general functions */
#define CT_ERROR_CTINIT          1014
#define CT_ERROR_CTEXIT          1020
#define CT_ERROR_CSETARGERR      1032

/* windowing functions */
#define CT_ERROR_WACLOSE         1112
#define CT_ERROR_WBOARD          1122
#define CT_ERROR_WBOX            1132
#define CT_ERROR_WCENTER         1142
#define CT_ERROR_WCLOSE          1152
#define CT_ERROR_WCOL            1162
#define CT_ERROR_WFCOL           1172
#define CT_ERROR_WFLASTCOL       1192
#define CT_ERROR_WFORMAT         1202
#define CT_ERROR_WFROW           1212
#define CT_ERROR_WLASTCOL        1222
#define CT_ERROR_WLASTROW        1232
#define CT_ERROR_WNUM            1242
#define CT_ERROR_WMODE           1252
#define CT_ERROR_WMOVE           1262
#define CT_ERROR_WOPEN           1272
#define CT_ERROR_WROW            1282
#define CT_ERROR_WSELECT         1292
#define CT_ERROR_WSETMOVE        1304
#define CT_ERROR_WSETSHADOW      1312
#define CT_ERROR_WSTEP           1322
                                    
/* extended driver */
#define CT_ERROR_CGA40           1414
#define CT_ERROR_CGA80           1424
#define CT_ERROR_DSETKBIOS       1434
#define CT_ERROR_DSETNOLINE      1444
#define CT_ERROR_DSETQFILE       1454
#define CT_ERROR_DSETTYPE        1462
#define CT_ERROR_DSETWINDOW      1474
#define CT_ERROR_EGA43           1484
#define CT_ERROR_FIRSTCOL        1492
#define CT_ERROR_FIRSTROW        1502
#define CT_ERROR_GETBOXGROW      1512
#define CT_ERROR_GETCURSOR       1522
#define CT_ERROR_GETKXLAT        1532
#define CT_ERROR_GETKXTAB        1541
#define CT_ERROR_GETLINES        1552
#define CT_ERROR_GETMODE         1561
#define CT_ERROR_GETPAGE         1572
#define CT_ERROR_GETPBIOS        1582
#define CT_ERROR_GETPXLAT        1591
#define CT_ERROR_GETSCRMODE      1602
#define CT_ERROR_GETTAB          1611
#define CT_ERROR_INKEYTRAP       1622
#define CT_ERROR_INPUTMODE       1632
#define CT_ERROR_KEYREAD         1641
#define CT_ERROR_KEYSEND         1654
#define CT_ERROR_MAXCOL          1662
#define CT_ERROR_MAXPAGE         1672
#define CT_ERROR_MAXROW          1682
#define CT_ERROR_MONOCHROME      1694
#define CT_ERROR_PAGECOPY        1704
#define CT_ERROR_PRINTERROR      1712
#define CT_ERROR_SETBELL         1721
#define CT_ERROR_SETBOXGROW      1731
#define CT_ERROR_SETCURSOR       1742
#define CT_ERROR_SETKXLAT        1754
#define CT_ERROR_SETKXTAB        1764
#define CT_ERROR_SETLINES        1771
#define CT_ERROR_SETMAXCOL       1784
#define CT_ERROR_SETMAXROW       1794
#define CT_ERROR_SETPAGE         1804
#define CT_ERROR_SETPBIOS        1814
#define CT_ERROR_SETPXLAT        1824
#define CT_ERROR_SETQNAME        1834
#define CT_ERROR_SETSCRMODE      1844
#define CT_ERROR_SETTAB          1854
#define CT_ERROR_TRAPANYKEY      1861
#define CT_ERROR_TRAPINPUT       1871
#define CT_ERROR_TRAPSHIFT       1881
#define CT_ERROR_VGA28           1894
#define CT_ERROR_VGA50           1904

/* serial communication */
#define CT_ERROR_COM_BREAK       2114
#define CT_ERROR_COM_CLOSE       2124
#define CT_ERROR_COM_COUNT       2132
#define CT_ERROR_COM_CRC         2142
#define CT_ERROR_COM_CTS         2154
#define CT_ERROR_COM_DCD         2164
#define CT_ERROR_COM_DOSCON      2171
#define CT_ERROR_COM_DSR         2184
#define CT_ERROR_COM_DTR         2194
#define CT_ERROR_COM_ERRCHR      2204
#define CT_ERROR_COM_EVENT       2212
#define CT_ERROR_COM_FLUSH       2224
#define CT_ERROR_COM_GETIO       2232
#define CT_ERROR_COM_GETIRQ      2242
#define CT_ERROR_COM_HARD        2254     
#define CT_ERROR_COM_INIT        2264
#define CT_ERROR_COM_KEY         2274
#define CT_ERROR_COM_LSR         2282
#define CT_ERROR_COM_MCR         2292
#define CT_ERROR_COM_MSR         2302
#define CT_ERROR_COM_NUM         2312
#define CT_ERROR_COM_OPEN        2324
#define CT_ERROR_COM_READ        2331
#define CT_ERROR_COM_REMOTE      2344
#define CT_ERROR_COM_RING        2354
#define CT_ERROR_COM_RTS         2364
#define CT_ERROR_COM_SCOUNT      2372
#define CT_ERROR_COM_SEND        2382
#define CT_ERROR_COM_SETIO       2394
#define CT_ERROR_COM_SETIRQ      2404
#define CT_ERROR_COM_SFLUSH      2414
#define CT_ERROR_COM_SKEY        2424
#define CT_ERROR_COM_SMODE       2432
#define CT_ERROR_COM_SOFT        2444     
#define CT_ERROR_COM_SOFT_R      2454
#define CT_ERROR_COM_SOFT_S      2464
#define CT_ERROR_XMOBLOCK        2471     
#define CT_ERROR_XMOCHECK        2482     
#define CT_ERROR_ZEROINSERT      2491     
#define CT_ERROR_ZEROREMOVE      2501     

/* string functions */
#define CT_ERROR_ADDASCII        3111
#define CT_ERROR_AFTERATNUM      3121
#define CT_ERROR_ASCIISUM        3132
#define CT_ERROR_ASCPOS          3142
#define CT_ERROR_ATADJUST        3151
#define CT_ERROR_ATNUM           3162
#define CT_ERROR_ATREPL          3171
#define CT_ERROR_ATTOKEN         3182
#define CT_ERROR_BEFORATNUM      3191
#define CT_ERROR_CENTER          3201
#define CT_ERROR_CHARADD         3211
#define CT_ERROR_CHARAND         3221
#define CT_ERROR_CHAREVEN        3231
#define CT_ERROR_CHARHIST        3247
#define CT_ERROR_CHARLIST        3251
#define CT_ERROR_CHARMIRR        3261
#define CT_ERROR_CHARMIX         3271
#define CT_ERROR_CHARNOLIST      3281
#define CT_ERROR_CHARNOT         3291
#define CT_ERROR_CHARODD         3301
#define CT_ERROR_CHARONE         3311
#define CT_ERROR_CHARONLY        3321
#define CT_ERROR_CHAROR          3331
#define CT_ERROR_CHARPACK        3341
#define CT_ERROR_CHARRELA        3352
#define CT_ERROR_CHARRELREP      3361
#define CT_ERROR_CHARREM         3371
#define CT_ERROR_CHARREPL        3381
#define CT_ERROR_CHARRLL         3391
#define CT_ERROR_CHARRLR         3401
#define CT_ERROR_CHARSHL         3411
#define CT_ERROR_CHARSHR         3421
#define CT_ERROR_CHARSLIST       3431
#define CT_ERROR_CHARSORT        3441
#define CT_ERROR_CHARSPREAD      3451
#define CT_ERROR_CHARSUB         3461
#define CT_ERROR_CHARSWAP        3471
#define CT_ERROR_CHARUNPACK      3481
#define CT_ERROR_CHARXOR         3491
#define CT_ERROR_CHECKSUM        3502
#define CT_ERROR_COUNTLEFT       3512
#define CT_ERROR_COUNTRIGHT      3522
#define CT_ERROR_CRYPT           3531
#define CT_ERROR_CSETATMUPA      3544
#define CT_ERROR_CSETREF         3554
#define CT_ERROR_EXPAND          3561
#define CT_ERROR_JUSTLEFT        3571
#define CT_ERROR_JUSTRIGHT       3581
#define CT_ERROR_LIKE            3594
#define CT_ERROR_LTOC            3601
#define CT_ERROR_MAXLINE         3612
#define CT_ERROR_NUMAT           3622
#define CT_ERROR_NUMLINE         3632
#define CT_ERROR_NUMTOKEN        3642
#define CT_ERROR_PADLEFT         3651
#define CT_ERROR_PADRIGHT        3661
#define CT_ERROR_POSALPHA        3672
#define CT_ERROR_POSCHAR         3681
#define CT_ERROR_POSDEL          3691
#define CT_ERROR_POSDIFF         3702
#define CT_ERROR_POSEQUAL        3712
#define CT_ERROR_POSINS          3721
#define CT_ERROR_POSLOWER        3732
#define CT_ERROR_POSRANGE        3742
#define CT_ERROR_POSREPL         3751
#define CT_ERROR_POSUPPER        3762
#define CT_ERROR_RANGEREM        3771
#define CT_ERROR_RANGEREPL       3781
#define CT_ERROR_REMALL          3791
#define CT_ERROR_REMLEFT         3801
#define CT_ERROR_REMRIGHT        3811
#define CT_ERROR_REPLALL         3821
#define CT_ERROR_REPLLEFT        3831
#define CT_ERROR_REPLRIGHT       3841
#define CT_ERROR_RESTTOKEN       3851
#define CT_ERROR_SAVETOKEN       3861
#define CT_ERROR_SETATLIKE       3872
#define CT_ERROR_STRDIFF         3882
#define CT_ERROR_STRSWAP         3891
#define CT_ERROR_TABEXPAND       3901
#define CT_ERROR_TABPACK         3911
#define CT_ERROR_TOKEN           3921
#define CT_ERROR_TOKENAT         3932
#define CT_ERROR_TOKENEND        3944
#define CT_ERROR_TOKENINIT       3954
#define CT_ERROR_TOKENLOWER      3961
#define CT_ERROR_TOKENNEXT       3971
#define CT_ERROR_TOKENSEP        3981
#define CT_ERROR_TOKENUPPER      3991
#define CT_ERROR_VALPOS          4002
#define CT_ERROR_WORDONE         4011
#define CT_ERROR_WORDONLY        4021
#define CT_ERROR_WORDREM         4031
#define CT_ERROR_WORDREPL        4041
#define CT_ERROR_WORDSWAP        4051
#define CT_ERROR_WORDTOCHAR      4061
                                 
/* number and bit manipulation */
#define CT_ERROR_BITTOC          4111
#define CT_ERROR_CELSIUS         4123
#define CT_ERROR_CLEARBIT        4132
#define CT_ERROR_CTOBIT          4142
#define CT_ERROR_CTOF            4153
#define CT_ERROR_CTON            4162
#define CT_ERROR_EXPONENT        4172
#define CT_ERROR_FAHRENHEIT      4183
#define CT_ERROR_FTOC            4191
#define CT_ERROR_INFINITY        4203
#define CT_ERROR_INTNEG          4212
#define CT_ERROR_INTPOS          4222
#define CT_ERROR_ISBIT           4234
#define CT_ERROR_LTON            4242
#define CT_ERROR_MANTISSA        4253
#define CT_ERROR_NTOC            4261
#define CT_ERROR_NUMAND          4272
#define CT_ERROR_NUMCOUNT        4282
#define CT_ERROR_NUMHIGH         4292
#define CT_ERROR_NUMLOW          4302
#define CT_ERROR_NUMMIRR         4312
#define CT_ERROR_NUMNOT          4322
#define CT_ERROR_NUMOR           4332
#define CT_ERROR_NUMROL          4342
#define CT_ERROR_NUMXOR          4352
#define CT_ERROR_RAND            4363
#define CT_ERROR_RANDOM          4372
#define CT_ERROR_SETBIT          4382
                                    
/* video functions */
#define CT_ERROR_CHARPIX         4412
#define CT_ERROR_CHARWIN         4421
#define CT_ERROR_CLEAREOL        4431
#define CT_ERROR_CLEARSLOW       4441
#define CT_ERROR_CLEARWIN        4451
#define CT_ERROR_CLEOL           4461
#define CT_ERROR_CLWIN           4471
#define CT_ERROR_COLORREPL       4481
#define CT_ERROR_COLORTON        4492
#define CT_ERROR_COLORWIN        4501
#define CT_ERROR_EGAPALETTE      4514
#define CT_ERROR_ENHANCED        4521
#define CT_ERROR_FILESCREEN      4532
#define CT_ERROR_FONTLOAD        4542
#define CT_ERROR_FONTRESET       4554
#define CT_ERROR_FONTROTATE      4561
#define CT_ERROR_FONTSELECT      4572
#define CT_ERROR_GETCLEARA       4582
#define CT_ERROR_GETCLEARB       4592
#define CT_ERROR_GETFONT         4601
#define CT_ERROR_GETSCRSTR       4611
#define CT_ERROR_GETVGAPAL       4622
#define CT_ERROR_INVERTATTR      4632
#define CT_ERROR_INVERTWIN       4641
#define CT_ERROR_ISCGA           4654
#define CT_ERROR_ISEGA           4664
#define CT_ERROR_ISHERCULES      4674
#define CT_ERROR_ISMCGA          4684
#define CT_ERROR_ISMONO          4694
#define CT_ERROR_ISPGA           4704
#define CT_ERROR_ISVGA           4714
#define CT_ERROR_MAXFONT         4722
#define CT_ERROR_MONISWITCH      4734
#define CT_ERROR_NTOCOLOR        4741
#define CT_ERROR_NUMCOL          4752
#define CT_ERROR_RESTCURSOR      4761
#define CT_ERROR_SAVECURSOR      4772
#define CT_ERROR_SAYDOWN         4781
#define CT_ERROR_SAYMOVEIN       4791
#define CT_ERROR_SAYSCREEN       4801
#define CT_ERROR_SAYSPREAD       4811
#define CT_ERROR_SCREENATTR      4822
#define CT_ERROR_SCREENFILE      4832
#define CT_ERROR_SCREENMARK      4844
#define CT_ERROR_SCREENMIX       4851
#define CT_ERROR_SCREENSIZE      4862
#define CT_ERROR_SCREENSTR       4871
#define CT_ERROR_SETCLEARA       4881
#define CT_ERROR_SETCLEARB       4891
#define CT_ERROR_SETFONT         4902
#define CT_ERROR_SETRC           4911
#define CT_ERROR_SETSCRSTR       4924
#define CT_ERROR_STANDARD        4931
#define CT_ERROR_STRSCREEN       4941
#define CT_ERROR_UNSELECTED      4951
#define CT_ERROR_UNTEXTWIN       4961
#define CT_ERROR_VGAPALETTE      4974
#define CT_ERROR_VIDEOINIT       4989
#define CT_ERROR_VIDEOSETUP      4992
#define CT_ERROR_VIDEOTYPE       5002
                                   
/* disc functions */
#define CT_ERROR_DELETEFILE      5112
#define CT_ERROR_DIRCHANGE       5122
#define CT_ERROR_DIRMAKE         5132
#define CT_ERROR_DIRNAME         5141
#define CT_ERROR_DIRREMOVE       5152
#define CT_ERROR_DISKCHANGE      5164
#define CT_ERROR_DISKCHECK       5172
#define CT_ERROR_DISKFORMAT      5182
#define CT_ERROR_DISKFREE        5192
#define CT_ERROR_DISKNAME        5201
#define CT_ERROR_DISKREADY       5214
#define CT_ERROR_DISKREADYW      5224
#define CT_ERROR_DISKSPEED       5232
#define CT_ERROR_DISKSTAT        5242
#define CT_ERROR_DISKTOTAL       5252
#define CT_ERROR_DISKTYPE        5262
#define CT_ERROR_DRIVETYPE       5272
#define CT_ERROR_FILEAPPEND      5282
#define CT_ERROR_FILEATTR        5292
#define CT_ERROR_FILECCLOSE      5304
#define CT_ERROR_FILECCONT       5312
#define CT_ERROR_FILECDATI       5324
#define CT_ERROR_FILECHECK       5332
#define CT_ERROR_FILECOPEN       5344
#define CT_ERROR_FILECOPY        5352
#define CT_ERROR_FILEDATE        5365
#define CT_ERROR_FILEDELETE      5374
#define CT_ERROR_FILEMOVE        5382
#define CT_ERROR_FILESEEK        5391
#define CT_ERROR_FILESIZE        5402
#define CT_ERROR_FILESTR         5411
#define CT_ERROR_FILETIME        5421
#define CT_ERROR_FILEVALID       5434
#define CT_ERROR_FLOPPYTYPE      5442
#define CT_ERROR_GETSHARE        5452
#define CT_ERROR_NUMDISKF        5462
#define CT_ERROR_NUMDISKH        5472
#define CT_ERROR_NUMDISKL        5482
#define CT_ERROR_RENAMEFILE      5492
#define CT_ERROR_RESTFSEEK       5501
#define CT_ERROR_SAVEFSEEK       5511
#define CT_ERROR_SETFATTR        5522
#define CT_ERROR_SETFCREATE      5532
#define CT_ERROR_SETFDATI        5544
#define CT_ERROR_SETSHARE        5554
#define CT_ERROR_STRFILE         5562
#define CT_ERROR_TEMPFILE        5571
#define CT_ERROR_TRUENAME        5581
#define CT_ERROR_VOLSERIAL       5592
#define CT_ERROR_VOLUME          5604
                                   
/* printer functions */
#define CT_ERROR_NUMPRINTER      5712                
#define CT_ERROR_PRINTFILE       5724                
#define CT_ERROR_PRINTINIT       5732                
#define CT_ERROR_PRINTREADY      5744                
#define CT_ERROR_PRINTSCR        5751
#define CT_ERROR_PRINTSCRX       5764
#define CT_ERROR_PRINTSEND       5772
#define CT_ERROR_PRINTSTAT       5782
#define CT_ERROR_SPOOLACTIV      5794
#define CT_ERROR_SPOOLADD        5804
#define CT_ERROR_SPOOLCOUNT      5812
#define CT_ERROR_SPOOLDEL        5824
#define CT_ERROR_SPOOLENTRY      5831
#define CT_ERROR_SPOOLFLUSH      5844
#define CT_ERROR_TOF             5854

/* date & time functions */
#define CT_ERROR_ADDMONTH        5915
#define CT_ERROR_BOM             5925
#define CT_ERROR_BOQ             5935
#define CT_ERROR_BOY             5945
#define CT_ERROR_CTODOW          5952
#define CT_ERROR_CTOMONTH        5962
#define CT_ERROR_DMY             5971
#define CT_ERROR_DOY             5982
#define CT_ERROR_EOM             5995
#define CT_ERROR_EOQ             6005
#define CT_ERROR_EOY             6015
#define CT_ERROR_ISLEAP          6024
#define CT_ERROR_LASTDAYOM       6032
#define CT_ERROR_MDY             6041
#define CT_ERROR_NTOCDOW         6051
#define CT_ERROR_NTOCMONTH       6061
#define CT_ERROR_QUARTER         6072
#define CT_ERROR_SECTOTIME       6081
#define CT_ERROR_SETDATE         6094
#define CT_ERROR_SETTIME         6104
#define CT_ERROR_SHOWTIME        6111
#define CT_ERROR_STOD            6125
#define CT_ERROR_TIMETOSEC       6132
#define CT_ERROR_TIMEVALID       6144
#define CT_ERROR_WAITPERIOD      6154
#define CT_ERROR_WEEK            6162
#define CT_ERROR_WOM             6172

/* DBF functions */
#define CT_ERROR_DBFDSKSIZE      6212
#define CT_ERROR_DBFSIZE         6222
#define CT_ERROR_FIELDDECI       6232
#define CT_ERROR_FIELDNUM        6242
#define CT_ERROR_FIELDSIZE       6252
#define CT_ERROR_FIELDTYPE       6261
#define CT_ERROR_ISDBT           6274

/* switch and state functions */
#define CT_ERROR_CSETALL         6310   /* TODO: change last digit */
#define CT_ERROR_CSETCLIP        6320
#define CT_ERROR_CSETDATE        6330
#define CT_ERROR_CSETDECI        6340
#define CT_ERROR_CSETDEFA        6350
#define CT_ERROR_CSETFUNC        6360
#define CT_ERROR_CSETKEY         6370
#define CT_ERROR_CSETLDEL        6380
#define CT_ERROR_CSETMARG        6390
#define CT_ERROR_CSETPATH        6400
#define CT_ERROR_CSETRDEL        6410
#define CT_ERROR_CSETRDONLY      6420
#define CT_ERROR_CSETSAFETY      6430
#define CT_ERROR_CSETSNOW        6440
#define CT_ERROR_CSETALTE        6450
#define CT_ERROR_CSETBELL        6460
#define CT_ERROR_CSETCARR        6470
#define CT_ERROR_CSETCENT        6480
#define CT_ERROR_CSETCONF        6490
#define CT_ERROR_CSETCONS        6500
#define CT_ERROR_CSETCURS        6510
#define CT_ERROR_CSETDELE        6520
#define CT_ERROR_CSETDELI        6530
#define CT_ERROR_CSETDEVI        6540
#define CT_ERROR_CSETESCA        6550
#define CT_ERROR_CSETEXAC        6560
#define CT_ERROR_CSETEXCL        6570
#define CT_ERROR_CSETFIXE        6580
#define CT_ERROR_CSETINTE        6590
#define CT_ERROR_CSETPRIN        6600
#define CT_ERROR_CSETSCOR        6610
#define CT_ERROR_CSETSOFT        6620
#define CT_ERROR_CSETUNIQ        6630
#define CT_ERROR_CSETWRAP        6640
#define CT_ERROR_ISDEBUG         6650
#define CT_ERROR_KSETCAPS        6660
#define CT_ERROR_KSETINS         6670
#define CT_ERROR_KSETNUM         6680
#define CT_ERROR_KSETSCROLL      6690
#define CT_ERROR_LASTKFUNC       6700
#define CT_ERROR_LASTKLINE       6710
#define CT_ERROR_LASTKPROC       6720
#define CT_ERROR_NUMFKEY         6730
#define CT_ERROR_SETLASTKEY      6740
                                    
/* system info functions */
#define CT_ERROR_BIOSDATE        6810    /* TODO: change last digit */
#define CT_ERROR_BOOTCOLD        6820
#define CT_ERROR_BOOTWARM        6830
#define CT_ERROR_CPUTYPE         6840
#define CT_ERROR_DOSPARAM        6850
#define CT_ERROR_ENVPARAM        6860
#define CT_ERROR_ERRORACT        6870
#define CT_ERROR_ERRORBASE       6880
#define CT_ERROR_ERRORCODE       6890
#define CT_ERROR_ERRORORG        6900
#define CT_ERROR_EXENAME         6910
#define CT_ERROR_FILESFREE       6920
#define CT_ERROR_FILESMAX        6930
#define CT_ERROR_GETCOUNTRY      6940
#define CT_ERROR_ISANSI          6950
#define CT_ERROR_ISAT            6960
#define CT_ERROR_ISMATH          6970
#define CT_ERROR_MEMSIZE         6980
#define CT_ERROR_NUMBUFFERS      6990
#define CT_ERROR_NUMFILES        7000
#define CT_ERROR_OSVER           7010
#define CT_ERROR_PCTYPE          7020
#define CT_ERROR_SSETBREAK       7030
#define CT_ERROR_SSETVERIFY      7040
                                    
/* 3.3 misc. functions */
#define CT_ERROR_ALLOFREE        7110  /* TODO: change last digit */
#define CT_ERROR_BLANK           7120
#define CT_ERROR_COMPLEMENT      7130
#define CT_ERROR_DATATYPE        7140
#define CT_ERROR_GETTIC          7150
#define CT_ERROR_KBDDISABLE      7160
#define CT_ERROR_KBDEMULATE      7170
#define CT_ERROR_KBDSPEED        7180
#define CT_ERROR_KBDSTAT         7190
#define CT_ERROR_KBDTYPE         7200
#define CT_ERROR_KEYSEC          7210
#define CT_ERROR_KEYTIME         7220
#define CT_ERROR_MILLISEC        7230
#define CT_ERROR_NUL             7240
#define CT_ERROR_SCANKEY         7250
#define CT_ERROR_SETTIC          7260
#define CT_ERROR_SHOWKEY         7270
#define CT_ERROR_SOUND           7280
#define CT_ERROR_SPEED           7290
#define CT_ERROR_STACKFREE       7300
#define CT_ERROR_TOOLVER         7310
#define CT_ERROR_XTOC            7320
                                   
/* math functions */
#define CT_ERROR_ACOS            7413
#define CT_ERROR_ASIN            7423
#define CT_ERROR_ATAN            7433
#define CT_ERROR_ATN2            7443
#define CT_ERROR_CEILING         7452
#define CT_ERROR_COS             7463
#define CT_ERROR_COT             7473
#define CT_ERROR_DTOR            7483
#define CT_ERROR_EXPA            7493
#define CT_ERROR_FACT            7502
#define CT_ERROR_FLOOR           7512
#define CT_ERROR_FV              7523
#define CT_ERROR_GETPREC         7532
#define CT_ERROR_LOG10           7543
#define CT_ERROR_LOGA            7553
#define CT_ERROR_PAYMENT         7563
#define CT_ERROR_PERIODS         7572
#define CT_ERROR_PI              7583
#define CT_ERROR_PV              7593
#define CT_ERROR_RATE            7603
#define CT_ERROR_ROOT            7613
#define CT_ERROR_RTOD            7623
#define CT_ERROR_SETMATHERR      7632
#define CT_ERROR_SETPREC         7642
#define CT_ERROR_SIGN            7652
#define CT_ERROR_SIN             7663
#define CT_ERROR_TAN             7673
                                   
/* peek and poke functions */
#define CT_ERROR_INBYTE          7810  /* TODO: change last digit */
#define CT_ERROR_INWORD          7820
#define CT_ERROR_OUTBYTE         7830
#define CT_ERROR_OUTWORD         7840
#define CT_ERROR_PEEKBYTE        7850
#define CT_ERROR_PEEKSTR         7860
#define CT_ERROR_PEEKWORD        7870
#define CT_ERROR_POKEBYTE        7880
#define CT_ERROR_POKEWORD        7890

/* GET/READ functions */
#define CT_ERROR_COUNTGETS       7910  /* TODO: change last digit */
#define CT_ERROR_CURRENTGET      7920
#define CT_ERROR_GETFLDCOL       7930
#define CT_ERROR_GETFLDROW       7940
#define CT_ERROR_GETFLDVAR       7950
#define CT_ERROR_GETINPUT        7960
#define CT_ERROR_GETSECRET       7970
#define CT_ERROR_RESTGETS        7980
#define CT_ERROR_RESTSETKEY      7990
#define CT_ERROR_SAVEGETS        8000
#define CT_ERROR_SAVESETKEY      8010

/* TODO: add network functions */

#endif /* _CTERROR_CH */

