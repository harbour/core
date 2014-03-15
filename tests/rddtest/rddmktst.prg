/*
 * Harbour Project source code:
 *    RDD tests
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

//#define _TEST_ADS_

//#define _CLIPPER53_

//#define _TEST_DESCEND_
//#define _TEST_UNIQUE_
//#define _TEST_SCOPE_
//#define _TEST_CMPDIDX_

#ifdef _TEST_ADS_
   #undef _TEST_DESCEND_
   #undef _TEST_UNIQUE_
#endif

#define _TEST_CREATE_

#include "rddtst.prg"

#ifndef _TEST_CMPDIDX_
   #command RDDTEST INDEX ON <key> TAG <tg> TO <fi> [ FOR <for> ] [ <desc: DESCENDING> ] => ;
            RDDTEST INDEX ON <key> TO <tg> [ FOR <for> ] [ <desc> ]
#endif


procedure test_main()
RDDTEST LOCAL n

RDDTEST rddSetDefault()

RDDTEST USE _DBNAME SHARED

/* movements in empty DB */
RDDTEST dbGoTop()
RDDTEST dbGoBottom()
RDDTEST dbSkip(0)
RDDTEST dbGoto(0)
RDDTEST dbSkip(1)
RDDTEST dbSkip(-1)
RDDTEST dbSkip(0)
RDDTEST SET DELETED ON
RDDTEST dbGoTop()
RDDTEST dbGoBottom()
RDDTEST dbSkip(0)
RDDTEST dbGoto(0)
RDDTEST dbSkip(1)
RDDTEST dbSkip(-1)
RDDTEST dbSkip(0)
RDDTEST SET DELETED OFF

RDDTEST INDEX on FNUM tag TG_N to _DBNAME
RDDTEST INDEX on FSTR tag TG_C to _DBNAME

RDDTEST ordSetFocus()

RDDTEST dbGoTop()
RDDTEST dbGoBottom()
RDDTEST dbSkip(0)
RDDTEST dbGoto(0)
RDDTEST dbSkip(1)
RDDTEST dbSkip(-1)
RDDTEST dbSkip(0)
RDDTEST SET DELETED ON
RDDTEST dbGoTop()
RDDTEST dbGoBottom()
RDDTEST dbSkip(0)
RDDTEST dbGoto(0)
RDDTEST dbSkip(1)
RDDTEST dbSkip(-1)
RDDTEST dbSkip(0)

RDDTEST dbGoto(0)
RDDTEST dbSeek('', .t.,.f.)
RDDTEST dbSeek('', .t.,.t.)
RDDTEST dbSeek('', .f.,.f.)
RDDTEST dbSeek('', .f.,.t.)

RDDTEST SET DELETED OFF

RDDTEST ;
  for n := 1 to N_LOOP                      ;;
     dbAppend()                             ;;
     replace FNUM with int( ( n + 2 ) / 3 ) ;;
     replace FSTR with Chr( FNUM + 48 )     ;;
  next

RDDTEST dbCommit()
RDDTEST dbUnlock()

// seeking test
RDDTEST ordSetFocus(1)
RDDTEST ordSetFocus()
RDDTEST dbSeek(0,.t.,.f.)
RDDTEST dbSeek(0,.t.,.t.)
RDDTEST dbSeek(0.5,.t.,.f.)
RDDTEST dbSeek(0.5,.t.,.t.)
RDDTEST dbSeek(1.0,.t.,.f.)
RDDTEST dbSeek(1.0,.t.,.t.)
RDDTEST dbSeek(2.0,.t.,.f.)
RDDTEST dbSeek(2.0,.t.,.t.)
RDDTEST dbSeek(2.5,.t.,.f.)
RDDTEST dbSeek(2.5,.t.,.t.)
RDDTEST dbSeek(5.0,.t.,.f.)
RDDTEST dbSeek(5.0,.t.,.t.)
RDDTEST ordSetFocus(2)
RDDTEST ordSetFocus()

RDDTEST dbSeek('', .t.,.f.)
RDDTEST dbSeek('', .t.,.t.)
RDDTEST dbSeek(' ',.t.,.f.)
RDDTEST dbSeek(' ',.t.,.t.)
RDDTEST dbSeek('0',.t.,.f.)
RDDTEST dbSeek('0',.t.,.t.)
RDDTEST dbSeek('1',.t.,.f.)
RDDTEST dbSeek('1',.t.,.t.)
RDDTEST dbSeek('2',.t.,.f.)
RDDTEST dbSeek('2',.t.,.t.)
RDDTEST dbSeek('3',.t.,.f.)
RDDTEST dbSeek('3',.t.,.t.)
RDDTEST dbSeek('4',.t.,.f.)
RDDTEST dbSeek('4',.t.,.t.)
RDDTEST dbSeek('5',.t.,.f.)
RDDTEST dbSeek('5',.t.,.t.)
RDDTEST dbSeek('6',.t.,.f.)
RDDTEST dbSeek('6',.t.,.t.)

#ifdef _TEST_SCOPE_
RDDTEST ordScope(TOPSCOPE,'3')
RDDTEST ordScope(BOTTOMSCOPE,'4')
RDDTEST dbOrderInfo(DBOI_SCOPETOP)
RDDTEST dbOrderInfo(DBOI_SCOPEBOTTOM)
RDDTEST dbSeek('', .t.,.f.)
RDDTEST dbSeek('', .t.,.t.)
RDDTEST dbSeek(' ',.t.,.f.)
RDDTEST dbSeek(' ',.t.,.t.)
RDDTEST dbSeek('0',.t.,.f.)
RDDTEST dbSeek('0',.t.,.t.)
RDDTEST dbSeek('1',.t.,.f.)
RDDTEST dbSeek('1',.t.,.t.)
RDDTEST dbSeek('2',.t.,.f.)
RDDTEST dbSeek('2',.t.,.t.)
RDDTEST dbSeek('3',.t.,.f.)
RDDTEST dbSeek('3',.t.,.t.)
RDDTEST dbSeek('4',.t.,.f.)
RDDTEST dbSeek('4',.t.,.t.)
RDDTEST dbSeek('5',.t.,.f.)
RDDTEST dbSeek('5',.t.,.t.)
RDDTEST dbSeek('6',.t.,.f.)
RDDTEST dbSeek('6',.t.,.t.)

RDDTEST ordScope(TOPSCOPE,'3')
RDDTEST ordScope(BOTTOMSCOPE,'2')
RDDTEST dbOrderInfo(DBOI_SCOPETOP)
RDDTEST dbOrderInfo(DBOI_SCOPEBOTTOM)
RDDTEST dbSeek('',.t.,.f.)
RDDTEST dbSeek('',.t.,.t.)
RDDTEST dbSeek('1',.t.,.f.)
RDDTEST dbSeek('1',.t.,.t.)
RDDTEST dbSeek('2',.t.,.f.)
RDDTEST dbSeek('2',.t.,.t.)
RDDTEST dbSeek('3',.t.,.f.)
RDDTEST dbSeek('3',.t.,.t.)
RDDTEST dbSeek('4',.t.,.f.)
RDDTEST dbSeek('4',.t.,.t.)
#endif

#ifdef _TEST_DESCEND_
RDDTEST ordDescend()
RDDTEST ordDescend(,,.t.)
RDDTEST ordDescend()
#endif
#ifdef _TEST_SCOPE_
RDDTEST dbOrderInfo(DBOI_SCOPETOP)
RDDTEST dbOrderInfo(DBOI_SCOPEBOTTOM)
RDDTEST ordScope(TOPSCOPE,NIL)
RDDTEST ordScope(BOTTOMSCOPE,NIL)
RDDTEST dbOrderInfo(DBOI_SCOPETOP)
RDDTEST dbOrderInfo(DBOI_SCOPEBOTTOM)
#endif
RDDTEST dbSeek('', .t.,.f.)
RDDTEST dbSeek('', .t.,.t.)
RDDTEST dbSeek(' ',.t.,.f.)
RDDTEST dbSeek(' ',.t.,.t.)
RDDTEST dbSeek('0',.t.,.f.)
RDDTEST dbSeek('0',.t.,.t.)
RDDTEST dbSeek('1',.t.,.f.)
RDDTEST dbSeek('1',.t.,.t.)
RDDTEST dbSeek('2',.t.,.f.)
RDDTEST dbSeek('2',.t.,.t.)
RDDTEST dbSeek('3',.t.,.f.)
RDDTEST dbSeek('3',.t.,.t.)
RDDTEST dbSeek('4',.t.,.f.)
RDDTEST dbSeek('4',.t.,.t.)
RDDTEST dbSeek('5',.t.,.f.)
RDDTEST dbSeek('5',.t.,.t.)
RDDTEST dbSeek('6',.t.,.f.)
RDDTEST dbSeek('6',.t.,.t.)
#ifdef _TEST_SCOPE_
RDDTEST ordScope(TOPSCOPE,'4')
RDDTEST ordScope(BOTTOMSCOPE,'3')
RDDTEST dbOrderInfo(DBOI_SCOPETOP)
RDDTEST dbOrderInfo(DBOI_SCOPEBOTTOM)
RDDTEST dbSeek('', .t.,.f.)
RDDTEST dbSeek('', .t.,.t.)
RDDTEST dbSeek(' ',.t.,.f.)
RDDTEST dbSeek(' ',.t.,.t.)
RDDTEST dbSeek('0',.t.,.f.)
RDDTEST dbSeek('0',.t.,.t.)
RDDTEST dbSeek('1',.t.,.f.)
RDDTEST dbSeek('1',.t.,.t.)
RDDTEST dbSeek('2',.t.,.f.)
RDDTEST dbSeek('2',.t.,.t.)
RDDTEST dbSeek('3',.t.,.f.)
RDDTEST dbSeek('3',.t.,.t.)
RDDTEST dbSeek('4',.t.,.f.)
RDDTEST dbSeek('4',.t.,.t.)
RDDTEST dbSeek('5',.t.,.f.)
RDDTEST dbSeek('5',.t.,.t.)
RDDTEST dbSeek('6',.t.,.f.)
RDDTEST dbSeek('6',.t.,.t.)

RDDTEST ordScope(TOPSCOPE,'3')
RDDTEST ordScope(BOTTOMSCOPE,'4')
RDDTEST dbOrderInfo(DBOI_SCOPETOP)
RDDTEST dbOrderInfo(DBOI_SCOPEBOTTOM)
RDDTEST dbSeek('', .t.,.f.)
RDDTEST dbSeek('', .t.,.t.)
RDDTEST dbSeek(' ',.t.,.f.)
RDDTEST dbSeek(' ',.t.,.t.)
RDDTEST dbSeek('0',.t.,.f.)
RDDTEST dbSeek('0',.t.,.t.)
RDDTEST dbSeek('1',.t.,.f.)
RDDTEST dbSeek('1',.t.,.t.)
RDDTEST dbSeek('2',.t.,.f.)
RDDTEST dbSeek('2',.t.,.t.)
RDDTEST dbSeek('3',.t.,.f.)
RDDTEST dbSeek('3',.t.,.t.)
RDDTEST dbSeek('4',.t.,.f.)
RDDTEST dbSeek('4',.t.,.t.)
RDDTEST dbSeek('5',.t.,.f.)
RDDTEST dbSeek('5',.t.,.t.)
RDDTEST dbSeek('6',.t.,.f.)
RDDTEST dbSeek('6',.t.,.t.)
#endif

RDDTEST INDEX on FSTR tag TG_C to _DBNAME DESCEND
RDDTEST dbSeek('',.t.,.f.)
RDDTEST dbSeek('',.t.,.t.)
RDDTEST dbSeek(' ',.t.,.f.)
RDDTEST dbSeek(' ',.t.,.t.)
RDDTEST dbSeek('0',.t.,.f.)
RDDTEST dbSeek('0',.t.,.t.)
RDDTEST dbSeek('1',.t.,.f.)
RDDTEST dbSeek('1',.t.,.t.)
RDDTEST dbSeek('2',.t.,.f.)
RDDTEST dbSeek('2',.t.,.t.)
RDDTEST dbSeek('3',.t.,.f.)
RDDTEST dbSeek('3',.t.,.t.)
RDDTEST dbSeek('4',.t.,.f.)
RDDTEST dbSeek('4',.t.,.t.)
RDDTEST dbSeek('5',.t.,.f.)
RDDTEST dbSeek('5',.t.,.t.)
RDDTEST dbSeek('6',.t.,.f.)
RDDTEST dbSeek('6',.t.,.t.)

#ifdef _TEST_SCOPE_
RDDTEST ordScope(TOPSCOPE,'4')
RDDTEST ordScope(BOTTOMSCOPE,'3')
RDDTEST dbOrderInfo(DBOI_SCOPETOP)
RDDTEST dbOrderInfo(DBOI_SCOPEBOTTOM)
RDDTEST dbSeek('', .t.,.f.)
RDDTEST dbSeek('', .t.,.t.)
RDDTEST dbSeek(' ',.t.,.f.)
RDDTEST dbSeek(' ',.t.,.t.)
RDDTEST dbSeek('0',.t.,.f.)
RDDTEST dbSeek('0',.t.,.t.)
RDDTEST dbSeek('1',.t.,.f.)
RDDTEST dbSeek('1',.t.,.t.)
RDDTEST dbSeek('2',.t.,.f.)
RDDTEST dbSeek('2',.t.,.t.)
RDDTEST dbSeek('3',.t.,.f.)
RDDTEST dbSeek('3',.t.,.t.)
RDDTEST dbSeek('4',.t.,.f.)
RDDTEST dbSeek('4',.t.,.t.)
RDDTEST dbSeek('5',.t.,.f.)
RDDTEST dbSeek('5',.t.,.t.)
RDDTEST dbSeek('6',.t.,.f.)
RDDTEST dbSeek('6',.t.,.t.)
#endif

// skiping test
RDDTEST INDEX on FSTR tag TG_C to _DBNAME
RDDTEST dbGoTop()
RDDTEST dbSkip(0)
RDDTEST dbSkip(-1)
RDDTEST dbSkip(0)
RDDTEST dbGoBottom()
RDDTEST dbSkip(0)
RDDTEST dbSkip(1)
RDDTEST dbSkip(0)

RDDTEST dbGoto(1)
RDDTEST dbSkip(-1)
RDDTEST dbSkip(1)
RDDTEST dbSkip(5)
RDDTEST dbSkip(5)
RDDTEST dbSkip(5)
RDDTEST dbSkip(-1)
RDDTEST dbSkip(-5)
RDDTEST dbSkip(10)
RDDTEST dbSkip(-5)
RDDTEST dbGoto(16)
RDDTEST dbSkip(-1)

#ifdef _TEST_SCOPE_
RDDTEST ordScope(TOPSCOPE,'3')
RDDTEST ordScope(BOTTOMSCOPE,'4')
RDDTEST dbOrderInfo(DBOI_SCOPETOP)
RDDTEST dbOrderInfo(DBOI_SCOPEBOTTOM)
#endif
RDDTEST dbGoto(1)
RDDTEST dbSkip(1)
RDDTEST dbGoto(1)
RDDTEST dbSkip(-1)

RDDTEST dbGoto(4)
RDDTEST dbSkip(1)
RDDTEST dbGoto(4)
RDDTEST dbSkip(-1)

RDDTEST dbGoto(6)
RDDTEST dbSkip(1)
RDDTEST dbGoto(6)
RDDTEST dbSkip(-1)

RDDTEST dbGoto(7)
RDDTEST dbSkip(1)
RDDTEST dbGoto(7)
RDDTEST dbSkip(-1)

RDDTEST dbGoto(12)
RDDTEST dbSkip(1)
RDDTEST dbGoto(12)
RDDTEST dbSkip(-1)

RDDTEST dbGoto(13)
RDDTEST dbSkip(1)
RDDTEST dbGoto(13)
RDDTEST dbSkip(-1)

RDDTEST dbGoto(14)
RDDTEST dbSkip(-1)
RDDTEST dbGoto(16)
RDDTEST dbSkip(-1)

RDDTEST ordSetFocus(0)
RDDTEST dbGoTop()
RDDTEST dbSkip(-1)
RDDTEST dbGoTop()
RDDTEST dbSkip(-10)

RDDTEST INDEX on FSTR tag TG_C to _DBNAME FOR FNUM>2 .and. FNUM<=4
RDDTEST dbGoto(1)
RDDTEST dbSkip(1)
RDDTEST dbGoto(1)
RDDTEST dbSkip(-1)

RDDTEST dbGoto(4)
RDDTEST dbSkip(1)
RDDTEST dbGoto(4)
RDDTEST dbSkip(-1)

RDDTEST dbGoto(6)
RDDTEST dbSkip(1)
RDDTEST dbGoto(6)
RDDTEST dbSkip(-1)

RDDTEST dbGoto(7)
RDDTEST dbSkip(1)
RDDTEST dbGoto(7)
RDDTEST dbSkip(-1)

RDDTEST dbGoto(12)
RDDTEST dbSkip(1)
RDDTEST dbGoto(12)
RDDTEST dbSkip(-1)

RDDTEST dbGoto(13)
RDDTEST dbSkip(1)
RDDTEST dbGoto(13)
RDDTEST dbSkip(-1)

RDDTEST dbGoto(14)
RDDTEST dbSkip(-1)
RDDTEST dbGoto(16)
RDDTEST dbSkip(-1)

RDDTEST INDEX on FSTR tag TG_C to _DBNAME FOR FNUM!=2 .and. FNUM<4
RDDTEST dbGoto(1)
RDDTEST dbSkip(1)
RDDTEST dbGoto(1)
RDDTEST dbSkip(-1)

RDDTEST dbGoto(4)
RDDTEST dbSkip(1)
RDDTEST dbGoto(4)
RDDTEST dbSkip(-1)

RDDTEST dbGoto(7)
RDDTEST dbSkip(1)
RDDTEST dbGoto(7)
RDDTEST dbSkip(-1)

RDDTEST dbGoto(10)
RDDTEST dbSkip(1)
RDDTEST dbGoto(10)
RDDTEST dbSkip(-1)

RDDTEST dbGoto(13)
RDDTEST dbSkip(1)
RDDTEST dbGoto(13)
RDDTEST dbSkip(-1)

RDDTEST dbGoto(14)
RDDTEST dbSkip(-1)
RDDTEST dbGoto(16)
RDDTEST dbSkip(-1)


RDDTEST dbGoTop()
RDDTEST dbSkip(1)
RDDTEST dbGoTop()
RDDTEST dbSkip(-1)

RDDTEST dbGoBottom()
RDDTEST dbSkip(1)
RDDTEST dbGoBottom()
RDDTEST dbSkip(-1)

#ifdef _TEST_SCOPE_
RDDTEST ordScope(TOPSCOPE,'5')
RDDTEST dbOrderInfo(DBOI_SCOPETOP)
#endif
RDDTEST dbGoto(1)
RDDTEST dbGoTop()
RDDTEST dbGoBottom()

RDDTEST INDEX on FSTR tag TG_C to _DBNAME FOR FNUM==6
RDDTEST dbGoto(1)
RDDTEST dbGoTop()
RDDTEST dbGoBottom()

#ifdef _TEST_SCOPE_
RDDTEST ordScope()
RDDTEST ordScope(TOPSCOPE)
RDDTEST ordScope(BOTTOMSCOPE)
RDDTEST dbOrderInfo(DBOI_SCOPETOP)
RDDTEST dbOrderInfo(DBOI_SCOPEBOTTOM)
RDDTEST ordScope(TOPSCOPE,NIL)
RDDTEST ordScope(BOTTOMSCOPE,NIL)
RDDTEST dbOrderInfo(DBOI_SCOPETOP)
RDDTEST dbOrderInfo(DBOI_SCOPEBOTTOM)
RDDTEST ordScope(TOPSCOPE,NIL)
RDDTEST ordScope(BOTTOMSCOPE,NIL)
RDDTEST dbOrderInfo(DBOI_SCOPETOP)
RDDTEST dbOrderInfo(DBOI_SCOPEBOTTOM)
RDDTEST ordScope(TOPSCOPE,{||'3'})
RDDTEST ordScope(BOTTOMSCOPE,{||'4'})
RDDTEST dbOrderInfo(DBOI_SCOPETOP)
RDDTEST dbOrderInfo(DBOI_SCOPEBOTTOM)
RDDTEST ordScope({},'3')
RDDTEST ordScope(BOTTOMSCOPE,'4')
RDDTEST ordScope()
RDDTEST ordScope(TOPSCOPE)
RDDTEST ordScope(BOTTOMSCOPE)
RDDTEST ordScope(TOPSCOPE)
RDDTEST ordScope(BOTTOMSCOPE)
RDDTEST ordScope(TOPSCOPE,'3')
RDDTEST ordScope(0)
RDDTEST ordScope(1)
RDDTEST ordScope(2)
RDDTEST ordScope(3)
#ifdef _TEST_DESCEND_
RDDTEST ordDescend(,,.t.)
RDDTEST ordScope(0)
RDDTEST ordScope(1)
RDDTEST ordScope(2)
RDDTEST ordScope(3)
RDDTEST ordDescend(,,.f.)
#endif
RDDTEST ordScope(TOPSCOPE,NIL)
#endif

#ifdef _TEST_UNIQUE_
RDDTEST INDEX on FSTR tag TG_C to _DBNAME
RDDTEST dbGoTop()
RDDTEST ordSkipUnique()
RDDTEST ordSkipUnique(1)
RDDTEST ordSkipUnique(2)
RDDTEST ordSkipUnique(-1)
RDDTEST ordSkipUnique(-2)
RDDTEST dbGoTop()
RDDTEST ordSkipUnique(-1)
RDDTEST ordSkipUnique()
RDDTEST dbGoBottom()
RDDTEST ordSkipUnique(-1)
RDDTEST ordSkipUnique()
RDDTEST ordSkipUnique()
RDDTEST ordSkipUnique(-1)

RDDTEST ordSetFocus(0)
RDDTEST dbGoto(1)
RDDTEST ordSkipUnique()
RDDTEST ordSkipUnique(-1)

RDDTEST INDEX on FSTR tag TG_C to _DBNAME FOR FNUM!=2 .and. FNUM<4
RDDTEST dbGoto(4)
RDDTEST ordSkipUnique(-1)
RDDTEST dbGoto(4)
RDDTEST ordSkipUnique()
RDDTEST dbGoto(13)
RDDTEST ordSkipUnique(-1)
RDDTEST dbGoto(13)
RDDTEST ordSkipUnique()
#endif

RDDTEST INDEX on FSTR tag TG_C to _DBNAME FOR RecNo()!=5 DESCEND
RDDTEST dbGoto(5)
RDDTEST dbSkip(-1)
RDDTEST dbGoto(5)
RDDTEST dbSkip(1)
#ifdef _TEST_UNIQUE_
RDDTEST dbGoto(5)
RDDTEST ordSkipUnique(-1)
RDDTEST dbGoto(5)
RDDTEST ordSkipUnique()
#endif

RDDTEST INDEX on FSTR tag TG_C to _DBNAME FOR RecNo()!=5
RDDTEST dbGoto(5)
RDDTEST dbSkip(-1)
RDDTEST dbGoto(5)
RDDTEST dbSkip(1)
#ifdef _TEST_UNIQUE_
RDDTEST dbGoto(5)
RDDTEST ordSkipUnique(-1)
RDDTEST dbGoto(5)
RDDTEST ordSkipUnique()
#endif

/* filter test and skipping */
RDDTEST ordSetFocus(0)
RDDTEST SET DELETED ON
RDDTEST FLock()
RDDTEST dbGoto(1)
RDDTEST dbDelete()
RDDTEST dbGoto(3)
RDDTEST dbDelete()
RDDTEST dbGoto(6)
RDDTEST dbDelete()
RDDTEST dbGoto(7)
RDDTEST dbDelete()
RDDTEST dbGoto(13)
RDDTEST dbDelete()
RDDTEST dbGoto(14)
RDDTEST dbDelete()
RDDTEST dbGoto(15)
RDDTEST dbDelete()
RDDTEST dbGoto(16)
RDDTEST dbDelete()
RDDTEST dbCommit()
RDDTEST dbUnlock()

RDDTEST dbGoTop()
RDDTEST dbSkip(-1)
RDDTEST dbGoTop()
RDDTEST dbSkip(-10)
RDDTEST dbSkip(5)
RDDTEST dbSkip(-5)
RDDTEST dbSkip(6)
RDDTEST dbSkip(-7)
RDDTEST dbSkip(8)
RDDTEST dbSkip(-20)
RDDTEST dbGoBottom()
RDDTEST dbSkip(1)
RDDTEST dbGoBottom()
RDDTEST dbSkip(10)
RDDTEST dbSkip(-5)
RDDTEST dbSkip(5)
RDDTEST dbSkip(-6)
RDDTEST dbSkip(7)
RDDTEST dbSkip(-8)
RDDTEST dbSkip(20)
RDDTEST dbGoBottom()
RDDTEST dbSkip(-20)

RDDTEST FLock()
RDDTEST DELETE ALL
RDDTEST dbUnlock()
RDDTEST dbGoTop()
RDDTEST dbGoBottom()
RDDTEST dbGoto(7)
RDDTEST dbSkip(1)
RDDTEST dbGoto(7)
RDDTEST dbSkip(-1)
RDDTEST dbSkip(1)
RDDTEST dbSkip(-1)
RDDTEST dbSkip(0)
RDDTEST dbGoto(0)
RDDTEST dbSkip(1)
RDDTEST dbSkip(-1)

RDDTEST dbGoto(7)
RDDTEST dbRLock()
RDDTEST dbRecall()
RDDTEST dbUnlock()
RDDTEST dbGoto(4)
RDDTEST dbSkip(-1)
RDDTEST dbGoto(4)
RDDTEST dbSkip(1)
RDDTEST dbGoto(11)
RDDTEST dbSkip(-1)
RDDTEST dbGoto(11)
RDDTEST dbSkip(1)

RDDTEST SET DELETED OFF
RDDTEST FLock()
RDDTEST RECALL ALL
RDDTEST dbUnlock()
RDDTEST SET DELETED ON
RDDTEST dbGoTop()
RDDTEST dbGoBottom()
RDDTEST dbCommit()

/* and the same but with active index */
RDDTEST ordSetFocus(1)
RDDTEST SET DELETED ON
RDDTEST FLock()
RDDTEST dbGoto(1)
RDDTEST dbDelete()
RDDTEST dbGoto(3)
RDDTEST dbDelete()
RDDTEST dbGoto(6)
RDDTEST dbDelete()
RDDTEST dbGoto(7)
RDDTEST dbDelete()
RDDTEST dbGoto(13)
RDDTEST dbDelete()
RDDTEST dbGoto(14)
RDDTEST dbDelete()
RDDTEST dbGoto(15)
RDDTEST dbDelete()
RDDTEST dbGoto(16)
RDDTEST dbDelete()
RDDTEST dbCommit()
RDDTEST dbUnlock()

RDDTEST dbGoTop()
RDDTEST dbSkip(-1)
RDDTEST dbGoTop()
RDDTEST dbSkip(-10)
RDDTEST dbSkip(5)
RDDTEST dbSkip(-5)
RDDTEST dbSkip(6)
RDDTEST dbSkip(-7)
RDDTEST dbSkip(8)
RDDTEST dbSkip(-20)
RDDTEST dbGoBottom()
RDDTEST dbSkip(1)
RDDTEST dbGoBottom()
RDDTEST dbSkip(10)
RDDTEST dbSkip(-5)
RDDTEST dbSkip(5)
RDDTEST dbSkip(-6)
RDDTEST dbSkip(7)
RDDTEST dbSkip(-8)
RDDTEST dbSkip(20)
RDDTEST dbGoBottom()
RDDTEST dbSkip(-20)

RDDTEST FLock()
RDDTEST DELETE ALL
RDDTEST dbCommit()
RDDTEST dbUnlock()
RDDTEST dbGoTop()
RDDTEST dbGoBottom()
RDDTEST dbGoto(7)
RDDTEST dbSkip(1)
RDDTEST dbGoto(7)
RDDTEST dbSkip(-1)
RDDTEST dbSkip(1)
RDDTEST dbSkip(-1)
RDDTEST dbSkip(0)
RDDTEST dbGoto(0)
RDDTEST dbSkip(1)
RDDTEST dbSkip(-1)

RDDTEST dbGoto(0)
RDDTEST dbSkip(1)
/* This test give unrepeatable results in Clipper and I don't know why yet,
   so I temporary diable it */
#ifdef _DISABLED_
RDDTEST dbSeek('', .t.,.f.)
RDDTEST dbSeek('', .t.,.t.)
RDDTEST dbSeek('', .f.,.f.)
RDDTEST dbSeek('', .f.,.t.)
RDDTEST dbSeek('2', .t.,.f.)
RDDTEST dbSeek('2', .t.,.t.)
RDDTEST dbSeek('2', .f.,.f.)
RDDTEST dbSeek('2', .f.,.t.)
#endif

RDDTEST dbGoto(7)
RDDTEST dbRLock()
RDDTEST dbRecall()
RDDTEST dbUnlock()
RDDTEST dbGoto(4)
RDDTEST dbSkip(-1)
RDDTEST dbGoto(4)
RDDTEST dbSkip(1)
RDDTEST dbGoto(11)
RDDTEST dbSkip(-1)
RDDTEST dbGoto(11)
RDDTEST dbSkip(1)

RDDTEST SET DELETED OFF
RDDTEST FLock()
RDDTEST RECALL ALL
RDDTEST dbUnlock()
RDDTEST SET DELETED ON
RDDTEST dbGoTop()
RDDTEST dbGoBottom()
RDDTEST dbCommit()

RDDTEST INDEX on FSTR tag TG_C to _DBNAME
RDDTEST dbSeek(PadR(' ',10)+" ",.t.,.f.)
RDDTEST dbSeek(PadR(' ',10)+" ",.t.,.t.)
RDDTEST dbSeek(PadR('0',10)+" ",.t.,.f.)
RDDTEST dbSeek(PadR('0',10)+" ",.t.,.t.)
RDDTEST dbSeek(PadR('1',10)+" ",.t.,.f.)
RDDTEST dbSeek(PadR('1',10)+" ",.t.,.t.)
RDDTEST dbSeek(PadR('2',10)+" ",.t.,.f.)
RDDTEST dbSeek(PadR('2',10)+" ",.t.,.t.)
RDDTEST dbSeek(PadR('3',10)+" ",.t.,.f.)
RDDTEST dbSeek(PadR('3',10)+" ",.t.,.t.)
RDDTEST dbSeek(PadR('4',10)+" ",.t.,.f.)
RDDTEST dbSeek(PadR('4',10)+" ",.t.,.t.)
RDDTEST dbSeek(PadR('5',10)+" ",.t.,.f.)
RDDTEST dbSeek(PadR('5',10)+" ",.t.,.t.)
RDDTEST dbSeek(PadR('6',10)+" ",.t.,.f.)
RDDTEST dbSeek(PadR('6',10)+" ",.t.,.t.)

RDDTEST dbSeek(PadR(' ',10)+"*",.t.,.f.)
RDDTEST dbSeek(PadR(' ',10)+"*",.t.,.t.)
RDDTEST dbSeek(PadR('0',10)+"*",.t.,.f.)
RDDTEST dbSeek(PadR('0',10)+"*",.t.,.t.)
RDDTEST dbSeek(PadR('1',10)+"*",.t.,.f.)
RDDTEST dbSeek(PadR('1',10)+"*",.t.,.t.)
RDDTEST dbSeek(PadR('2',10)+"*",.t.,.f.)
RDDTEST dbSeek(PadR('2',10)+"*",.t.,.t.)
RDDTEST dbSeek(PadR('3',10)+"*",.t.,.f.)
RDDTEST dbSeek(PadR('3',10)+"*",.t.,.t.)
RDDTEST dbSeek(PadR('4',10)+"*",.t.,.f.)
RDDTEST dbSeek(PadR('4',10)+"*",.t.,.t.)
RDDTEST dbSeek(PadR('5',10)+"*",.t.,.f.)
RDDTEST dbSeek(PadR('5',10)+"*",.t.,.t.)
RDDTEST dbSeek(PadR('6',10)+"*",.t.,.f.)
RDDTEST dbSeek(PadR('6',10)+"*",.t.,.t.)

#ifdef _TEST_SCOPE_
RDDTEST ordScope(TOPSCOPE,'3')
RDDTEST ordScope(BOTTOMSCOPE,'4')

RDDTEST dbSeek(PadR(' ',10)+" ",.t.,.f.)
RDDTEST dbSeek(PadR(' ',10)+" ",.t.,.t.)
RDDTEST dbSeek(PadR('0',10)+" ",.t.,.f.)
RDDTEST dbSeek(PadR('0',10)+" ",.t.,.t.)
RDDTEST dbSeek(PadR('1',10)+" ",.t.,.f.)
RDDTEST dbSeek(PadR('1',10)+" ",.t.,.t.)
RDDTEST dbSeek(PadR('2',10)+" ",.t.,.f.)
RDDTEST dbSeek(PadR('2',10)+" ",.t.,.t.)
RDDTEST dbSeek(PadR('3',10)+" ",.t.,.f.)
RDDTEST dbSeek(PadR('3',10)+" ",.t.,.t.)
RDDTEST dbSeek(PadR('4',10)+" ",.t.,.f.)
RDDTEST dbSeek(PadR('4',10)+" ",.t.,.t.)
RDDTEST dbSeek(PadR('5',10)+" ",.t.,.f.)
RDDTEST dbSeek(PadR('5',10)+" ",.t.,.t.)
RDDTEST dbSeek(PadR('6',10)+" ",.t.,.f.)
RDDTEST dbSeek(PadR('6',10)+" ",.t.,.t.)

RDDTEST dbSeek(PadR(' ',10)+"*",.t.,.f.)
RDDTEST dbSeek(PadR(' ',10)+"*",.t.,.t.)
RDDTEST dbSeek(PadR('0',10)+"*",.t.,.f.)
RDDTEST dbSeek(PadR('0',10)+"*",.t.,.t.)
RDDTEST dbSeek(PadR('1',10)+"*",.t.,.f.)
RDDTEST dbSeek(PadR('1',10)+"*",.t.,.t.)
RDDTEST dbSeek(PadR('2',10)+"*",.t.,.f.)
RDDTEST dbSeek(PadR('2',10)+"*",.t.,.t.)
RDDTEST dbSeek(PadR('3',10)+"*",.t.,.f.)
RDDTEST dbSeek(PadR('3',10)+"*",.t.,.t.)
RDDTEST dbSeek(PadR('4',10)+"*",.t.,.f.)
RDDTEST dbSeek(PadR('4',10)+"*",.t.,.t.)
RDDTEST dbSeek(PadR('5',10)+"*",.t.,.f.)
RDDTEST dbSeek(PadR('5',10)+"*",.t.,.t.)
RDDTEST dbSeek(PadR('6',10)+"*",.t.,.f.)
RDDTEST dbSeek(PadR('6',10)+"*",.t.,.t.)
#endif

return
