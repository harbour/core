/*
 * Harbour Project source code:
 *   Header file for CT error codes
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
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

/* NOTE: This file is also used by C code. */

#ifndef _CTERROR_CH
#define _CTERROR_CH

/* sub code ranges */
#define CT_ERROR_MATHLIB_RANGEL     100 /* math lib errors */
#define CT_ERROR_MATHLIB_RANGEH     199

#define CT_ERROR_GENERAL_RANGEL    1000 /* general functions */
#define CT_ERROR_GENERAL_RANGEH    1199

#define CT_ERROR_WINDOW_RANGEL     1200 /* windowing functions */
#define CT_ERROR_WINDOW_RANGEH     1599

#define CT_ERROR_EXTDRV_RANGEL     1600 /* extended driver functions */
#define CT_ERROR_EXTDRV_RANGEH     2399

#define CT_ERROR_SERIAL_RANGEL     2400 /* serial communication functions */
#define CT_ERROR_SERIAL_RANGEH     3099

#define CT_ERROR_STRING_RANGEL     3100 /* string functions */
#define CT_ERROR_STRING_RANGEH     4499

#define CT_ERROR_NUMBIT_RANGEL     4500 /* number and bit manipulation functions */
#define CT_ERROR_NUMBIT_RANGEH     4999

#define CT_ERROR_VIDEO_RANGEL      5000 /* video functions */
#define CT_ERROR_VIDEO_RANGEH      5699

#define CT_ERROR_DISC_RANGEL       5700 /* disc functions */
#define CT_ERROR_DISC_RANGEH       6399

#define CT_ERROR_PRINT_RANGEL      6400 /* printer functions */
#define CT_ERROR_PRINT_RANGEH      6699

#define CT_ERROR_DATE_RANGEL       6700 /* date & time functions */
#define CT_ERROR_DATE_RANGEH       7099

#define CT_ERROR_DBF_RANGEL        7100 /* DBF functions */
#define CT_ERROR_DBF_RANGEH        7299

#define CT_ERROR_SWITCH_RANGEL     7300 /* switch functions */
#define CT_ERROR_SWITCH_RANGEH     7799

#define CT_ERROR_SYSINF_RANGEL     7800 /* system info functions */
#define CT_ERROR_SYSINF_RANGEH     8099

#define CT_ERROR_MISC_RANGEL       8100 /* misc. functions */
#define CT_ERROR_MISC_RANGEH       8399

#define CT_ERROR_MATH_RANGEL       8400 /* math functions */
#define CT_ERROR_MATH_RANGEH       8699

#define CT_ERROR_PEEK_RANGEL       8800 /* peek & poke functions */
#define CT_ERROR_PEEK_RANGEH       8899

#define CT_ERROR_GETREAD_RANGEL    8900 /* get & read functions */
#define CT_ERROR_GETREAD_RANGEH    9099

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
#define CT_ERROR_WACLOSE         1212
#define CT_ERROR_WBOARD          1222
#define CT_ERROR_WBOX            1232
#define CT_ERROR_WCENTER         1242
#define CT_ERROR_WCLOSE          1252
#define CT_ERROR_WCOL            1262
#define CT_ERROR_WFCOL           1272
#define CT_ERROR_WFLASTCOL       1292
#define CT_ERROR_WFORMAT         1302
#define CT_ERROR_WFROW           1312
#define CT_ERROR_WLASTCOL        1322
#define CT_ERROR_WLASTROW        1332
#define CT_ERROR_WNUM            1342
#define CT_ERROR_WMODE           1352
#define CT_ERROR_WMOVE           1362
#define CT_ERROR_WOPEN           1372
#define CT_ERROR_WROW            1382
#define CT_ERROR_WSELECT         1392
#define CT_ERROR_WSETMOVE        1404
#define CT_ERROR_WSETSHADOW      1412
#define CT_ERROR_WSTEP           1422

/* extended driver */
#define CT_ERROR_CGA40           1614
#define CT_ERROR_CGA80           1624
#define CT_ERROR_DSETKBIOS       1634
#define CT_ERROR_DSETNOLINE      1644
#define CT_ERROR_DSETQFILE       1654
#define CT_ERROR_DSETTYPE        1662
#define CT_ERROR_DSETWINDOW      1674
#define CT_ERROR_EGA43           1684
#define CT_ERROR_FIRSTCOL        1692
#define CT_ERROR_FIRSTROW        1702
#define CT_ERROR_GETBOXGROW      1712
#define CT_ERROR_GETCURSOR       1722
#define CT_ERROR_GETKXLAT        1732
#define CT_ERROR_GETKXTAB        1741
#define CT_ERROR_GETLINES        1752
#define CT_ERROR_GETMODE         1761
#define CT_ERROR_GETPAGE         1772
#define CT_ERROR_GETPBIOS        1782
#define CT_ERROR_GETPXLAT        1791
#define CT_ERROR_GETSCRMODE      1802
#define CT_ERROR_GETTAB          1811
#define CT_ERROR_INKEYTRAP       1822
#define CT_ERROR_INPUTMODE       1832
#define CT_ERROR_KEYREAD         1841
#define CT_ERROR_KEYSEND         1854
#define CT_ERROR_MAXCOL          1862
#define CT_ERROR_MAXPAGE         1872
#define CT_ERROR_MAXROW          1882
#define CT_ERROR_MONOCHROME      1894
#define CT_ERROR_PAGECOPY        1904
#define CT_ERROR_PRINTERROR      1912
#define CT_ERROR_SETBELL         1921
#define CT_ERROR_SETBOXGROW      1931
#define CT_ERROR_SETCURSOR       1942
#define CT_ERROR_SETKXLAT        1954
#define CT_ERROR_SETKXTAB        1964
#define CT_ERROR_SETLINES        1971
#define CT_ERROR_SETMAXCOL       1984
#define CT_ERROR_SETMAXROW       1994
#define CT_ERROR_SETPAGE         2004
#define CT_ERROR_SETPBIOS        2014
#define CT_ERROR_SETPXLAT        2024
#define CT_ERROR_SETQNAME        2034
#define CT_ERROR_SETSCRMODE      2044
#define CT_ERROR_SETTAB          2054
#define CT_ERROR_TRAPANYKEY      2061
#define CT_ERROR_TRAPINPUT       2071
#define CT_ERROR_TRAPSHIFT       2081
#define CT_ERROR_VGA28           2094
#define CT_ERROR_VGA50           2104

/* serial communication */
#define CT_ERROR_COM_BREAK       2414
#define CT_ERROR_COM_CLOSE       2424
#define CT_ERROR_COM_COUNT       2432
#define CT_ERROR_COM_CRC         2442
#define CT_ERROR_COM_CTS         2454
#define CT_ERROR_COM_DCD         2464
#define CT_ERROR_COM_DOSCON      2471
#define CT_ERROR_COM_DSR         2484
#define CT_ERROR_COM_DTR         2494
#define CT_ERROR_COM_ERRCHR      2504
#define CT_ERROR_COM_EVENT       2512
#define CT_ERROR_COM_FLUSH       2524
#define CT_ERROR_COM_GETIO       2532
#define CT_ERROR_COM_GETIRQ      2542
#define CT_ERROR_COM_HARD        2554
#define CT_ERROR_COM_INIT        2564
#define CT_ERROR_COM_KEY         2574
#define CT_ERROR_COM_LSR         2582
#define CT_ERROR_COM_MCR         2592
#define CT_ERROR_COM_MSR         2602
#define CT_ERROR_COM_NUM         2612
#define CT_ERROR_COM_OPEN        2624
#define CT_ERROR_COM_READ        2631
#define CT_ERROR_COM_REMOTE      2644
#define CT_ERROR_COM_RING        2654
#define CT_ERROR_COM_RTS         2664
#define CT_ERROR_COM_SCOUNT      2672
#define CT_ERROR_COM_SEND        2682
#define CT_ERROR_COM_SETIO       2694
#define CT_ERROR_COM_SETIRQ      2704
#define CT_ERROR_COM_SFLUSH      2714
#define CT_ERROR_COM_SKEY        2724
#define CT_ERROR_COM_SMODE       2732
#define CT_ERROR_COM_SOFT        2744
#define CT_ERROR_COM_SOFT_R      2754
#define CT_ERROR_COM_SOFT_S      2764
#define CT_ERROR_XMOBLOCK        2771
#define CT_ERROR_XMOCHECK        2782
#define CT_ERROR_ZEROINSERT      2791
#define CT_ERROR_ZEROREMOVE      2801

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
#define CT_ERROR_TOKENNUM        3982
#define CT_ERROR_TOKENSEP        3991
#define CT_ERROR_TOKENUPPER      4001
#define CT_ERROR_VALPOS          4012
#define CT_ERROR_WORDONE         4021
#define CT_ERROR_WORDONLY        4031
#define CT_ERROR_WORDREM         4041
#define CT_ERROR_WORDREPL        4051
#define CT_ERROR_WORDSWAP        4061
#define CT_ERROR_WORDTOCHAR      4071

/* number and bit manipulation */
#define CT_ERROR_BITTOC          4511
#define CT_ERROR_CELSIUS         4523
#define CT_ERROR_CLEARBIT        4532
#define CT_ERROR_CTOBIT          4542
#define CT_ERROR_CTOF            4553
#define CT_ERROR_CTON            4562
#define CT_ERROR_EXPONENT        4572
#define CT_ERROR_FAHRENHEIT      4583
#define CT_ERROR_FTOC            4591
#define CT_ERROR_INFINITY        4603
#define CT_ERROR_INTNEG          4612
#define CT_ERROR_INTPOS          4622
#define CT_ERROR_ISBIT           4634
#define CT_ERROR_LTON            4642
#define CT_ERROR_MANTISSA        4653
#define CT_ERROR_NTOC            4661
#define CT_ERROR_NUMAND          4672
#define CT_ERROR_NUMCOUNT        4682
#define CT_ERROR_NUMHIGH         4692
#define CT_ERROR_NUMLOW          4702
#define CT_ERROR_NUMMIRR         4712
#define CT_ERROR_NUMNOT          4722
#define CT_ERROR_NUMOR           4732
#define CT_ERROR_NUMROL          4742
#define CT_ERROR_NUMXOR          4752
#define CT_ERROR_RAND            4763
#define CT_ERROR_RANDOM          4772
#define CT_ERROR_SETBIT          4782
#define CT_ERROR_NUMANDX         4792
#define CT_ERROR_NUMMIRRX        4802
#define CT_ERROR_NUMNOTX         4812
#define CT_ERROR_NUMORX          4822
#define CT_ERROR_NUMROLX         4832
#define CT_ERROR_NUMXORX         4842

/* video functions */
#define CT_ERROR_CHARPIX         5012
#define CT_ERROR_CHARWIN         5021
#define CT_ERROR_CLEAREOL        5031
#define CT_ERROR_CLEARSLOW       5041
#define CT_ERROR_CLEARWIN        5051
#define CT_ERROR_CLEOL           5061
#define CT_ERROR_CLWIN           5071
#define CT_ERROR_COLORREPL       5081
#define CT_ERROR_COLORTON        5092
#define CT_ERROR_COLORWIN        5101
#define CT_ERROR_EGAPALETTE      5114
#define CT_ERROR_ENHANCED        5121
#define CT_ERROR_FILESCREEN      5132
#define CT_ERROR_FONTLOAD        5142
#define CT_ERROR_FONTRESET       5154
#define CT_ERROR_FONTROTATE      5161
#define CT_ERROR_FONTSELECT      5172
#define CT_ERROR_GETCLEARA       5182
#define CT_ERROR_GETCLEARB       5192
#define CT_ERROR_GETFONT         5201
#define CT_ERROR_GETSCRSTR       5211
#define CT_ERROR_GETVGAPAL       5222
#define CT_ERROR_INVERTATTR      5232
#define CT_ERROR_INVERTWIN       5241
#define CT_ERROR_ISCGA           5254
#define CT_ERROR_ISEGA           5264
#define CT_ERROR_ISHERCULES      5274
#define CT_ERROR_ISMCGA          5284
#define CT_ERROR_ISMONO          5294
#define CT_ERROR_ISPGA           5304
#define CT_ERROR_ISVGA           5314
#define CT_ERROR_MAXFONT         5322
#define CT_ERROR_MONISWITCH      5334
#define CT_ERROR_NTOCOLOR        5341
#define CT_ERROR_NUMCOL          5352
#define CT_ERROR_RESTCURSOR      5361
#define CT_ERROR_SAVECURSOR      5372
#define CT_ERROR_SAYDOWN         5381
#define CT_ERROR_SAYMOVEIN       5491
#define CT_ERROR_SAYSCREEN       5401
#define CT_ERROR_SAYSPREAD       5411
#define CT_ERROR_SCREENATTR      5422
#define CT_ERROR_SCREENFILE      5432
#define CT_ERROR_SCREENMARK      5444
#define CT_ERROR_SCREENMIX       5451
#define CT_ERROR_SCREENSIZE      5462
#define CT_ERROR_SCREENSTR       5471
#define CT_ERROR_SETCLEARA       5481
#define CT_ERROR_SETCLEARB       5491
#define CT_ERROR_SETFONT         5502
#define CT_ERROR_SETRC           5511
#define CT_ERROR_SETSCRSTR       5524
#define CT_ERROR_STANDARD        5531
#define CT_ERROR_STRSCREEN       5541
#define CT_ERROR_UNSELECTED      5551
#define CT_ERROR_UNTEXTWIN       5561
#define CT_ERROR_VGAPALETTE      5574
#define CT_ERROR_VIDEOINIT       5589
#define CT_ERROR_VIDEOSETUP      5592
#define CT_ERROR_VIDEOTYPE       5602

/* disc functions */
#define CT_ERROR_DELETEFILE      5712
#define CT_ERROR_DIRCHANGE       5722
#define CT_ERROR_DIRMAKE         5732
#define CT_ERROR_DIRNAME         5741
#define CT_ERROR_DIRREMOVE       5752
#define CT_ERROR_DISKCHANGE      5764
#define CT_ERROR_DISKCHECK       5772
#define CT_ERROR_DISKFORMAT      5782
#define CT_ERROR_DISKFREE        5792
#define CT_ERROR_DISKNAME        5801
#define CT_ERROR_DISKREADY       5814
#define CT_ERROR_DISKREADYW      5824
#define CT_ERROR_DISKSPEED       5832
#define CT_ERROR_DISKSTAT        5842
#define CT_ERROR_DISKTOTAL       5852
#define CT_ERROR_DISKTYPE        5862
#define CT_ERROR_DRIVETYPE       5872
#define CT_ERROR_FILEAPPEND      5882
#define CT_ERROR_FILEATTR        5892
#define CT_ERROR_FILECCLOSE      5904
#define CT_ERROR_FILECCONT       5912
#define CT_ERROR_FILECDATI       5924
#define CT_ERROR_FILECHECK       5932
#define CT_ERROR_FILECOPEN       5944
#define CT_ERROR_FILECOPY        5952
#define CT_ERROR_FILEDATE        5965
#define CT_ERROR_FILEDELETE      5974
#define CT_ERROR_FILEMOVE        5982
#define CT_ERROR_FILESEEK        5991
#define CT_ERROR_FILESIZE        6002
#define CT_ERROR_FILESTR         6011
#define CT_ERROR_FILETIME        6021
#define CT_ERROR_FILEVALID       6034
#define CT_ERROR_FLOPPYTYPE      6042
#define CT_ERROR_GETSHARE        6052
#define CT_ERROR_NUMDISKF        6062
#define CT_ERROR_NUMDISKH        6072
#define CT_ERROR_NUMDISKL        6082
#define CT_ERROR_RENAMEFILE      6092
#define CT_ERROR_RESTFSEEK       6101
#define CT_ERROR_SAVEFSEEK       6111
#define CT_ERROR_SETFATTR        6122
#define CT_ERROR_SETFCREATE      6132
#define CT_ERROR_SETFDATI        6144
#define CT_ERROR_SETSHARE        6154
#define CT_ERROR_STRFILE         6162
#define CT_ERROR_TEMPFILE        6171
#define CT_ERROR_TRUENAME        6181
#define CT_ERROR_VOLSERIAL       6192
#define CT_ERROR_VOLUME          6204

/* printer functions */
#define CT_ERROR_NUMPRINTER      6412
#define CT_ERROR_PRINTFILE       6424
#define CT_ERROR_PRINTINIT       6432
#define CT_ERROR_PRINTREADY      6444
#define CT_ERROR_PRINTSCR        6451
#define CT_ERROR_PRINTSCRX       6464
#define CT_ERROR_PRINTSEND       6472
#define CT_ERROR_PRINTSTAT       6482
#define CT_ERROR_SPOOLACTIV      6494
#define CT_ERROR_SPOOLADD        6504
#define CT_ERROR_SPOOLCOUNT      6512
#define CT_ERROR_SPOOLDEL        6524
#define CT_ERROR_SPOOLENTRY      6531
#define CT_ERROR_SPOOLFLUSH      6544
#define CT_ERROR_TOF             6554

/* date & time functions */
#define CT_ERROR_ADDMONTH        6715
#define CT_ERROR_BOM             6725
#define CT_ERROR_BOQ             6735
#define CT_ERROR_BOY             6745
#define CT_ERROR_CTODOW          6752
#define CT_ERROR_CTOMONTH        6762
#define CT_ERROR_DMY             6771
#define CT_ERROR_DOY             6782
#define CT_ERROR_EOM             6795
#define CT_ERROR_EOQ             6805
#define CT_ERROR_EOY             6815
#define CT_ERROR_ISLEAP          6824
#define CT_ERROR_LASTDAYOM       6832
#define CT_ERROR_MDY             6841
#define CT_ERROR_NTOCDOW         6851
#define CT_ERROR_NTOCMONTH       6861
#define CT_ERROR_QUARTER         6872
#define CT_ERROR_SECTOTIME       6881
#define CT_ERROR_SETDATE         6894
#define CT_ERROR_SETTIME         6904
#define CT_ERROR_SHOWTIME        6911
#define CT_ERROR_STOD            6925
#define CT_ERROR_TIMETOSEC       6932
#define CT_ERROR_TIMEVALID       6944
#define CT_ERROR_WAITPERIOD      6954
#define CT_ERROR_WEEK            6962
#define CT_ERROR_WOM             6972

/* DBF functions */
#define CT_ERROR_DBFDSKSIZE      7112
#define CT_ERROR_DBFSIZE         7122
#define CT_ERROR_FIELDDECI       7132
#define CT_ERROR_FIELDNUM        7142
#define CT_ERROR_FIELDSIZE       7152
#define CT_ERROR_FIELDTYPE       7161
#define CT_ERROR_ISDBT           7174

/* switch and state functions */
#define CT_ERROR_CSETALL         7310  /* TODO: change last digit */
#define CT_ERROR_CSETCLIP        7320
#define CT_ERROR_CSETDATE        7330
#define CT_ERROR_CSETDECI        7340
#define CT_ERROR_CSETDEFA        7350
#define CT_ERROR_CSETFUNC        7360
#define CT_ERROR_CSETKEY         7370
#define CT_ERROR_CSETLDEL        7380
#define CT_ERROR_CSETMARG        7390
#define CT_ERROR_CSETPATH        7400
#define CT_ERROR_CSETRDEL        7410
#define CT_ERROR_CSETRDONLY      7420
#define CT_ERROR_CSETSAFETY      7430
#define CT_ERROR_CSETSNOW        7440
#define CT_ERROR_CSETALTE        7450
#define CT_ERROR_CSETBELL        7460
#define CT_ERROR_CSETCARR        7470
#define CT_ERROR_CSETCENT        7480
#define CT_ERROR_CSETCONF        7490
#define CT_ERROR_CSETCONS        7500
#define CT_ERROR_CSETCURS        7510
#define CT_ERROR_CSETDELE        7520
#define CT_ERROR_CSETDELI        7530
#define CT_ERROR_CSETDEVI        7540
#define CT_ERROR_CSETESCA        7550
#define CT_ERROR_CSETEXAC        7560
#define CT_ERROR_CSETEXCL        7570
#define CT_ERROR_CSETFIXE        7580
#define CT_ERROR_CSETINTE        7590
#define CT_ERROR_CSETPRIN        7600
#define CT_ERROR_CSETSCOR        7610
#define CT_ERROR_CSETSOFT        7620
#define CT_ERROR_CSETUNIQ        7630
#define CT_ERROR_CSETWRAP        7640
#define CT_ERROR_ISDEBUG         7650
#define CT_ERROR_KSETCAPS        7660
#define CT_ERROR_KSETINS         7670
#define CT_ERROR_KSETNUM         7680
#define CT_ERROR_KSETSCROLL      7690
#define CT_ERROR_LASTKFUNC       7700
#define CT_ERROR_LASTKLINE       7710
#define CT_ERROR_LASTKPROC       7720
#define CT_ERROR_NUMFKEY         7730
#define CT_ERROR_SETLASTKEY      7740

/* system info functions */
#define CT_ERROR_BIOSDATE        7810  /* TODO: change last digit */
#define CT_ERROR_BOOTCOLD        7820
#define CT_ERROR_BOOTWARM        7830
#define CT_ERROR_CPUTYPE         7840
#define CT_ERROR_DOSPARAM        7850
#define CT_ERROR_ENVPARAM        7860
#define CT_ERROR_ERRORACT        7870
#define CT_ERROR_ERRORBASE       7880
#define CT_ERROR_ERRORCODE       7890
#define CT_ERROR_ERRORORG        7900
#define CT_ERROR_EXENAME         7910
#define CT_ERROR_FILESFREE       7920
#define CT_ERROR_FILESMAX        7930
#define CT_ERROR_GETCOUNTRY      7940
#define CT_ERROR_ISANSI          7950
#define CT_ERROR_ISAT            7960
#define CT_ERROR_ISMATH          7970
#define CT_ERROR_MEMSIZE         7980
#define CT_ERROR_NUMBUFFERS      7990
#define CT_ERROR_NUMFILES        8000
#define CT_ERROR_OSVER           8010
#define CT_ERROR_PCTYPE          8020
#define CT_ERROR_SSETBREAK       8030
#define CT_ERROR_SSETVERIFY      8040

/* 3.3 misc. functions */
#define CT_ERROR_ALLOFREE        8110  /* TODO: change last digit */
#define CT_ERROR_BLANK           8120
#define CT_ERROR_COMPLEMENT      8130
#define CT_ERROR_DATATYPE        8140
#define CT_ERROR_GETTIC          8150
#define CT_ERROR_KBDDISABLE      8160
#define CT_ERROR_KBDEMULATE      8170
#define CT_ERROR_KBDSPEED        8180
#define CT_ERROR_KBDSTAT         8190
#define CT_ERROR_KBDTYPE         8200
#define CT_ERROR_KEYSEC          8210
#define CT_ERROR_KEYTIME         8220
#define CT_ERROR_MILLISEC        8230
#define CT_ERROR_NUL             8240
#define CT_ERROR_SCANKEY         8250
#define CT_ERROR_SETTIC          8260
#define CT_ERROR_SHOWKEY         8270
#define CT_ERROR_SOUND           8280
#define CT_ERROR_SPEED           8290
#define CT_ERROR_STACKFREE       8300
#define CT_ERROR_TOOLVER         8310
#define CT_ERROR_XTOC            8320

/* math functions */
#define CT_ERROR_ACOS            8413
#define CT_ERROR_ASIN            8423
#define CT_ERROR_ATAN            8433
#define CT_ERROR_ATN2            8443
#define CT_ERROR_CEILING         8452
#define CT_ERROR_COS             8463
#define CT_ERROR_COT             8473
#define CT_ERROR_DTOR            8483
#define CT_ERROR_FACT            8492
#define CT_ERROR_FLOOR           8502
#define CT_ERROR_FV              8513
#define CT_ERROR_GETPREC         8522
#define CT_ERROR_LOG10           8533
#define CT_ERROR_PAYMENT         8543
#define CT_ERROR_PERIODS         8552
#define CT_ERROR_PI              8563
#define CT_ERROR_PV              8573
#define CT_ERROR_RATE            8583
#define CT_ERROR_RTOD            8593
#define CT_ERROR_SETPREC         8612
#define CT_ERROR_SIGN            8622
#define CT_ERROR_SIN             8633
#define CT_ERROR_TAN             8643
#define CT_ERROR_SINH            8653
#define CT_ERROR_COSH            8663
#define CT_ERROR_TANH            8673

/* peek and poke functions */
#define CT_ERROR_INBYTE          8810  /* TODO: change last digit */
#define CT_ERROR_INWORD          8820
#define CT_ERROR_OUTBYTE         8830
#define CT_ERROR_OUTWORD         8840
#define CT_ERROR_PEEKBYTE        8850
#define CT_ERROR_PEEKSTR         8860
#define CT_ERROR_PEEKWORD        8870
#define CT_ERROR_POKEBYTE        8880
#define CT_ERROR_POKEWORD        8890

/* GET/READ functions */
#define CT_ERROR_COUNTGETS       8910  /* TODO: change last digit */
#define CT_ERROR_CURRENTGET      8920
#define CT_ERROR_GETFLDCOL       8930
#define CT_ERROR_GETFLDROW       8940
#define CT_ERROR_GETFLDVAR       8950
#define CT_ERROR_GETINPUT        8960
#define CT_ERROR_GETSECRET       8970
#define CT_ERROR_RESTGETS        8980
#define CT_ERROR_RESTSETKEY      8990
#define CT_ERROR_SAVEGETS        9000
#define CT_ERROR_SAVESETKEY      9010

/* TODO: add network functions */

#endif /* _CTERROR_CH */
