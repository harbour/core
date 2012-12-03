/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for Inkey() function
 *
 * Copyright 2001 David G. Holm
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

#ifndef HB_HBINKEY_CH_
#define HB_HBINKEY_CH_

#define HB_INKEY_NONE                   256
#define HB_INKEY_ALT                    384
#define HB_INKEY_CTRL                   512
#define HB_INKEY_SHIFT                  640
#define HB_INKEY_ENHANCED               768

#define HB_K_ALT_A                      414
#define HB_K_ALT_B                      432
#define HB_K_ALT_C                      430
#define HB_K_ALT_D                      416
#define HB_K_ALT_E                      402
#define HB_K_ALT_F                      417
#define HB_K_ALT_G                      418
#define HB_K_ALT_H                      419
#define HB_K_ALT_I                      407
#define HB_K_ALT_J                      420
#define HB_K_ALT_K                      421
#define HB_K_ALT_L                      422
#define HB_K_ALT_M                      434
#define HB_K_ALT_N                      433
#define HB_K_ALT_O                      408
#define HB_K_ALT_P                      409
#define HB_K_ALT_Q                      400
#define HB_K_ALT_R                      403
#define HB_K_ALT_S                      415
#define HB_K_ALT_T                      404
#define HB_K_ALT_U                      406
#define HB_K_ALT_V                      431
#define HB_K_ALT_W                      401
#define HB_K_ALT_X                      429
#define HB_K_ALT_Y                      405
#define HB_K_ALT_Z                      428

#define HB_K_CTRL_A                     1
#define HB_K_CTRL_B                     2
#define HB_K_CTRL_C                     3
#define HB_K_CTRL_D                     4
#define HB_K_CTRL_E                     5
#define HB_K_CTRL_F                     6
#define HB_K_CTRL_G                     7
#define HB_K_CTRL_H                     8
#define HB_K_CTRL_I                     9
#define HB_K_CTRL_J                     10
#define HB_K_CTRL_K                     11
#define HB_K_CTRL_L                     12
#define HB_K_CTRL_M                     13
#define HB_K_CTRL_N                     14
#define HB_K_CTRL_O                     15
#define HB_K_CTRL_P                     16
#define HB_K_CTRL_Q                     17
#define HB_K_CTRL_R                     18
#define HB_K_CTRL_S                     19
#define HB_K_CTRL_T                     20
#define HB_K_CTRL_U                     21
#define HB_K_CTRL_V                     22
#define HB_K_CTRL_W                     23
#define HB_K_CTRL_X                     24
#define HB_K_CTRL_Y                     25
#define HB_K_CTRL_Z                     26
#define HB_K_CTRL_LEFT_SQUARE           27
#define HB_K_CTRL_BACK_SLASH            28
#define HB_K_CTRL_RIGHT_SQUARE          29
#define HB_K_CTRL_HAT                   30
#define HB_K_CTRL_UNDERSCORE            31

#define HB_K_SPACE                      32

#define HB_K_ALT_1                      386
#define HB_K_ALT_2                      387
#define HB_K_ALT_3                      388
#define HB_K_ALT_4                      389
#define HB_K_ALT_5                      390
#define HB_K_ALT_6                      391
#define HB_K_ALT_7                      392
#define HB_K_ALT_8                      393
#define HB_K_ALT_9                      394
#define HB_K_ALT_0                      395
#define HB_K_ALT_EQUAL                  397

#define HB_K_CTRL_1                     514
#define HB_K_CTRL_2                     515
#define HB_K_CTRL_3                     516
#define HB_K_CTRL_4                     517
#define HB_K_CTRL_5                     518
#define HB_K_CTRL_6                     519
#define HB_K_CTRL_7                     520
#define HB_K_CTRL_8                     521
#define HB_K_CTRL_9                     522
#define HB_K_CTRL_0                     523
#define HB_K_CTRL_COLON                 551
#define HB_K_CTRL_SEMI_COLON            551
#define HB_K_CTRL_COMMA                 563
#define HB_K_CTRL_PERIOD                564
#define HB_K_CTRL_SLASH                 565

#define HB_K_CTRL_MINUS                 524
#define HB_K_CTRL_EQUALS                525
#define HB_K_CTRL_PLUS                  525
#define HB_K_CTRL_LEFT_CURLY            538
#define HB_K_CTRL_RIGHT_CURLY           539

#define HB_K_BACKSPACE                  8
#define HB_K_ALT_BACKSPACE              398
#define HB_K_CTRL_BACKSPACE             127
#define HB_K_SHIFT_BACKSPACE

#define HB_K_ENTER                      13
#define HB_K_ALT_ENTER                  412
#define HB_K_CTRL_ENTER                 10
#define HB_K_SHIFT_ENTER                668

#define HB_K_ESC                        27
#define HB_K_ALT_ESC                    385
#define HB_K_CTRL_ESC                   513
#define HB_K_SHIFT_ESC                  641

#define HB_K_TAB                        9
#define HB_K_ALT_TAB                    399
#define HB_K_CTRL_TAB                   527
#define HB_K_SHIFT_TAB                  655

#define HB_K_F1                         315
#define HB_K_F2                         316
#define HB_K_F3                         317
#define HB_K_F4                         318
#define HB_K_F5                         319
#define HB_K_F6                         320
#define HB_K_F7                         321
#define HB_K_F8                         322
#define HB_K_F9                         323
#define HB_K_F10                        324
#define HB_K_F11                        343
#define HB_K_F12                        344

#define HB_K_ALT_F1                     443
#define HB_K_ALT_F2                     444
#define HB_K_ALT_F3                     445
#define HB_K_ALT_F4                     446
#define HB_K_ALT_F5                     447
#define HB_K_ALT_F6                     448
#define HB_K_ALT_F7                     449
#define HB_K_ALT_F8                     450
#define HB_K_ALT_F9                     451
#define HB_K_ALT_F10                    452
#define HB_K_ALT_F11                    471
#define HB_K_ALT_F12                    472

#define HB_K_CTRL_F1                    571
#define HB_K_CTRL_F2                    572
#define HB_K_CTRL_F3                    573
#define HB_K_CTRL_F4                    574
#define HB_K_CTRL_F5                    575
#define HB_K_CTRL_F6                    576
#define HB_K_CTRL_F7                    577
#define HB_K_CTRL_F8                    578
#define HB_K_CTRL_F9                    579
#define HB_K_CTRL_F10                   580
#define HB_K_CTRL_F11                   599
#define HB_K_CTRL_F12                   600

#define HB_K_SHIFT_F1                   699
#define HB_K_SHIFT_F2                   700
#define HB_K_SHIFT_F3                   701
#define HB_K_SHIFT_F4                   702
#define HB_K_SHIFT_F5                   703
#define HB_K_SHIFT_F6                   704
#define HB_K_SHIFT_F7                   705
#define HB_K_SHIFT_F8                   706
#define HB_K_SHIFT_F9                   707
#define HB_K_SHIFT_F10                  708
#define HB_K_SHIFT_F11                  727
#define HB_K_SHIFT_F12                  728

#define HB_KP_MINUS                     330
#define HB_KP_ALT_MINUS                 458
#define HB_KP_CTRL_MINUS                586
#define HB_KP_SHIFT_MINUS               714

#define HB_KP_PLUS                      334
#define HB_KP_ALT_PLUS                  462
#define HB_KP_CTRL_PLUS                 590
#define HB_KP_SHIFT_PLUS                718

#define HB_KP_SLASH                     1077
#define HB_KP_ALT_SLASH                 1205
#define HB_KP_CTRL_SLASH                1333
#define HB_KP_SHIFT_SLASH               1461

#define HB_KP_STAR                      311
#define HB_KP_ALT_STAR                  439
#define HB_KP_CTRL_STAR                 567
#define HB_KP_SHIFT_STAR                695

#define HB_KP_ENTER                     1052
#define HB_KP_ALT_ENTER                 1180
#define HB_KP_CTRL_ENTER                1308
#define HB_KP_SHIFT_ENTER               1436

#define HB_KP_HOME                      327
#define HB_KP_UP                        328
#define HB_KP_PG_UP                     329
#define HB_KP_LEFT                      331
#define HB_KP_5                         332
#define HB_KP_RIGHT                     333
#define HB_KP_END                       335
#define HB_KP_DOWN                      336
#define HB_KP_PG_DN                     337
#define HB_KP_INS                       338
#define HB_KP_DEL                       339

#define HB_KP_ALT_HOME                  455
#define HB_KP_ALT_UP                    456
#define HB_KP_ALT_PG_UP                 457
#define HB_KP_ALT_LEFT                  459
#define HB_KP_ALT_5                     460
#define HB_KP_ALT_RIGHT                 461
#define HB_KP_ALT_END                   463
#define HB_KP_ALT_DOWN                  463
#define HB_KP_ALT_PG_DN                 465
#define HB_KP_ALT_INS                   466
#define HB_KP_ALT_DEL                   467

#define HB_KP_CTRL_HOME                 583
#define HB_KP_CTRL_UP                   584
#define HB_KP_CTRL_PG_UP                585
#define HB_KP_CTRL_LEFT                 587
#define HB_KP_CTRL_5                    588
#define HB_KP_CTRL_RIGHT                589
#define HB_KP_CTRL_END                  591
#define HB_KP_CTRL_DOWN                 592
#define HB_KP_CTRL_PG_DN                593
#define HB_KP_CTRL_INS                  594
#define HB_KP_CTRL_DEL                  595

#define HB_KP_SHIFT_HOME                711
#define HB_KP_SHIFT_UP                  712
#define HB_KP_SHIFT_PG_UP               713
#define HB_KP_SHIFT_LEFT                715
#define HB_KP_SHIFT_5                   716
#define HB_KP_SHIFT_RIGHT               717
#define HB_KP_SHIFT_END                 719
#define HB_KP_SHIFT_DOWN                720
#define HB_KP_SHIFT_PG_DN               721
#define HB_KP_SHIFT_INS                 722
#define HB_KP_SHIFT_DEL                 723

#define HB_K_HOME                       1095
#define HB_K_UP                         1096
#define HB_K_PG_UP                      1097
#define HB_K_LEFT                       1099
#define HB_K_RIGHT                      1101
#define HB_K_END                        1103
#define HB_K_DOWN                       1104
#define HB_K_PG_DN                      1105
#define HB_K_INS                        1106
#define HB_K_DEL                        1107

#define HB_K_ALT_HOME                   1223
#define HB_K_ALT_UP                     1224
#define HB_K_ALT_PG_UP                  1225
#define HB_K_ALT_LEFT                   1227
#define HB_K_ALT_RIGHT                  1229
#define HB_K_ALT_END                    1231
#define HB_K_ALT_DOWN                   1231
#define HB_K_ALT_PG_DN                  1233
#define HB_K_ALT_INS                    1234
#define HB_K_ALT_DEL                    1235

#define HB_K_CTRL_HOME                  1351
#define HB_K_CTRL_UP                    1352
#define HB_K_CTRL_PG_UP                 1353
#define HB_K_CTRL_LEFT                  1355
#define HB_K_CTRL_RIGHT                 1357
#define HB_K_CTRL_END                   1359
#define HB_K_CTRL_DOWN                  1360
#define HB_K_CTRL_PG_DN                 1361
#define HB_K_CTRL_INS                   1362
#define HB_K_CTRL_DEL                   1363

#define HB_K_SHIFT_HOME                 1479
#define HB_K_SHIFT_UP                   1480
#define HB_K_SHIFT_PG_UP                1481
#define HB_K_SHIFT_LEFT                 1483
#define HB_K_SHIFT_RIGHT                1485
#define HB_K_SHIFT_END                  1487
#define HB_K_SHIFT_DOWN                 1488
#define HB_K_SHIFT_PG_DN                1489
#define HB_K_SHIFT_INS                  1490
#define HB_K_SHIFT_DEL                  1491

#endif /* HB_HBINKEY_CH_ */
