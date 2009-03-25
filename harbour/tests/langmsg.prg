/*
 * $Id$
 */

/*
 * Copyright 2003 Przemyslaw Czerpak <druzus@polbox.com>
 * See COPYING for licensing terms.
 */

#include "hblang.ch"

function main( cLng )
local i, a, aDay, aMnt, aErr, aInt, cDtFrm, cTrue, cFalse

HB_LANGSELECT("EN")
aDay:=GET_DAYS()
aMnt:=GET_MONTHS()
aErr:=GET_ERR()
aInt:=GET_IERR()
cDtFrm:=HB_LANGMESSAGE(HB_LANG_ITEM_BASE_TEXT )
cTrue :=HB_LANGMESSAGE(HB_LANG_ITEM_BASE_TEXT + 1 )
cFalse:=HB_LANGMESSAGE(HB_LANG_ITEM_BASE_TEXT + 2 )

if !empty( cLng )
  HB_LANGSELECT( UPPER( cLng ) )
endif

? HB_LANGNAME()
? "HB_LANG_ITEM_BASE_ID     ", "["+HB_LANGMESSAGE(HB_LANG_ITEM_BASE_ID     )+"]"
? "HB_LANG_ITEM_BASE_MONTH  ", "["+HB_LANGMESSAGE(HB_LANG_ITEM_BASE_MONTH  )+"]"
? "HB_LANG_ITEM_BASE_DAY    ", "["+HB_LANGMESSAGE(HB_LANG_ITEM_BASE_DAY    )+"]"
? "HB_LANG_ITEM_BASE_NATMSG ", "["+HB_LANGMESSAGE(HB_LANG_ITEM_BASE_NATMSG )+"]"
? "HB_LANG_ITEM_BASE_ERRDESC", "["+HB_LANGMESSAGE(HB_LANG_ITEM_BASE_ERRDESC)+"]"
? "HB_LANG_ITEM_BASE_ERRINTR", "["+HB_LANGMESSAGE(HB_LANG_ITEM_BASE_ERRINTR)+"]"
? "HB_LANG_ITEM_BASE_TEXT   ", "["+HB_LANGMESSAGE(HB_LANG_ITEM_BASE_TEXT   )+"]"
? "HB_LANGERRMSG(0)", HB_LANGERRMSG(0)
?
? "date format: " + HB_LANGMESSAGE(HB_LANG_ITEM_BASE_TEXT ) + " (" + cDtFrm + ")"
? "   TRUE val: " + HB_LANGMESSAGE(HB_LANG_ITEM_BASE_TEXT + 1 ) + " (" + cTrue + ")"
? "  FALSE val: " + HB_LANGMESSAGE(HB_LANG_ITEM_BASE_TEXT + 2 ) + " (" + cFalse + ")"
?
inkey(0)

? "Errors:"
? "-------"
a:=GET_ERR()
for i:=1 to len(a)
    ? padr(a[i,1],15) + "|" + padr(aErr[i,2],30) + "|" + padr(a[i,2],32)
next
?
inkey(0)

? "Internal errors:"
? "----------------"
a:=GET_IERR()
for i:=1 to len(a)
    ? padr(a[i,1],15) + "|" + padr(aInt[i,2],30) + "|" + padr(a[i,2],32)
next
?
inkey(0)

? "Days:"
? "-----"
a:=GET_DAYS()
for i:=1 to len(a)
    ? "  " + padr(aDay[i],15) + a[i]
next
?
inkey(0)

? "Months:"
? "-------"
a:=GET_MONTHS()
for i:=1 to len(a)
    ? "  " + padr(aMnt[i],15) + a[i]
next
?
inkey(0)

return nil

function GET_DAYS()
LOCAL i, n, aDays[7], dt:=date()
for i:=1 to 7
  n:=dow(dt)
  aDays[n]:=cdow(dt)
  ++dt
next
return (aDays)

function GET_MONTHS()
LOCAL i, n, aMonths[12], dt:=date()
dt-=day(dt)-1
for i:=1 to 12
  n:=month(dt)
  aMonths[n]:=cmonth(dt)
  dt+=31
  dt-=day(dt)-1
next
return (aMonths)

function GET_ERR()
local aErr
aErr := { { "EG_ARG         ", HB_LANGERRMSG(  1 ) }, ;
          { "EG_BOUND       ", HB_LANGERRMSG(  2 ) }, ;
          { "EG_STROVERFLOW ", HB_LANGERRMSG(  3 ) }, ;
          { "EG_NUMOVERFLOW ", HB_LANGERRMSG(  4 ) }, ;
          { "EG_ZERODIV     ", HB_LANGERRMSG(  5 ) }, ;
          { "EG_NUMERR      ", HB_LANGERRMSG(  6 ) }, ;
          { "EG_SYNTAX      ", HB_LANGERRMSG(  7 ) }, ;
          { "EG_COMPLEXITY  ", HB_LANGERRMSG(  8 ) }, ;
          { "               ", HB_LANGERRMSG(  9 ) }, ;
          { "               ", HB_LANGERRMSG( 10 ) }, ;
          { "EG_MEM         ", HB_LANGERRMSG( 11 ) }, ;
          { "EG_NOFUNC      ", HB_LANGERRMSG( 12 ) }, ;
          { "EG_NOMETHOD    ", HB_LANGERRMSG( 13 ) }, ;
          { "EG_NOVAR       ", HB_LANGERRMSG( 14 ) }, ;
          { "EG_NOALIAS     ", HB_LANGERRMSG( 15 ) }, ;
          { "EG_NOVARMETHOD ", HB_LANGERRMSG( 16 ) }, ;
          { "EG_BADALIAS    ", HB_LANGERRMSG( 17 ) }, ;
          { "EG_DUPALIAS    ", HB_LANGERRMSG( 18 ) }, ;
          { "EG_NOOBJECT    ", HB_LANGERRMSG( 19 ) }, ;
          { "EG_CREATE      ", HB_LANGERRMSG( 20 ) }, ;
          { "EG_OPEN        ", HB_LANGERRMSG( 21 ) }, ;
          { "EG_CLOSE       ", HB_LANGERRMSG( 22 ) }, ;
          { "EG_READ        ", HB_LANGERRMSG( 23 ) }, ;
          { "EG_WRITE       ", HB_LANGERRMSG( 24 ) }, ;
          { "EG_PRINT       ", HB_LANGERRMSG( 25 ) }, ;
          { "               ", HB_LANGERRMSG( 26 ) }, ;
          { "               ", HB_LANGERRMSG( 27 ) }, ;
          { "               ", HB_LANGERRMSG( 28 ) }, ;
          { "               ", HB_LANGERRMSG( 29 ) }, ;
          { "EG_UNSUPPORTED ", HB_LANGERRMSG( 30 ) }, ;
          { "EG_LIMIT       ", HB_LANGERRMSG( 31 ) }, ;
          { "EG_CORRUPTION  ", HB_LANGERRMSG( 32 ) }, ;
          { "EG_DATATYPE    ", HB_LANGERRMSG( 33 ) }, ;
          { "EG_DATAWIDTH   ", HB_LANGERRMSG( 34 ) }, ;
          { "EG_NOTABLE     ", HB_LANGERRMSG( 35 ) }, ;
          { "EG_NOORDER     ", HB_LANGERRMSG( 36 ) }, ;
          { "EG_SHARED      ", HB_LANGERRMSG( 37 ) }, ;
          { "EG_UNLOCKED    ", HB_LANGERRMSG( 38 ) }, ;
          { "EG_READONLY    ", HB_LANGERRMSG( 39 ) }, ;
          { "EG_APPENDLOCK  ", HB_LANGERRMSG( 40 ) }, ;
          { "EG_LOCK        ", HB_LANGERRMSG( 41 ) }, ;
          { "               ", HB_LANGERRMSG( 42 ) }, ;
          { "               ", HB_LANGERRMSG( 43 ) }, ;
          { "               ", HB_LANGERRMSG( 44 ) }, ;
          { "               ", HB_LANGERRMSG( 45 ) }, ;
          { "EG_ARRACCESS   ", HB_LANGERRMSG( 46 ) }, ;
          { "EG_ARRASSIGN   ", HB_LANGERRMSG( 47 ) }, ;
          { "EG_ARRDIMENSION", HB_LANGERRMSG( 48 ) }, ;
          { "EG_NOTARRAY    ", HB_LANGERRMSG( 49 ) }, ;
          { "EG_CONDITION   ", HB_LANGERRMSG( 50 ) } }
return aErr

function HB_LANGERRINTR(n)
return HB_LANGMESSAGE(HB_LANG_ITEM_BASE_ERRINTR + n - 9000 )

function GET_IERR()
local aErr
aErr := { ;
  { "HB_EI_ERRUNRECOV      ", HB_LANGERRINTR(9000) }, ;
  { "HB_EI_ERRRECFAILURE   ", HB_LANGERRINTR(9001) }, ;
  { "HB_EI_ERRNOBLOCK      ", HB_LANGERRINTR(9002) }, ;
  { "HB_EI_ERRTOOMANY      ", HB_LANGERRINTR(9003) }, ;
  { "HB_EI_RDDINVALID      ", HB_LANGERRINTR(9004) }, ;
  { "HB_EI_CLSINVMETHOD    ", HB_LANGERRINTR(9005) }, ;
  { "HB_EI_XGRABALLOC      ", HB_LANGERRINTR(9006) }, ;
  { "HB_EI_XREALLOCNULL    ", HB_LANGERRINTR(9007) }, ;
  { "HB_EI_XREALLOCINV     ", HB_LANGERRINTR(9008) }, ;
  { "HB_EI_XREALLOC        ", HB_LANGERRINTR(9009) }, ;
  { "HB_EI_XFREEINV        ", HB_LANGERRINTR(9010) }, ;
  { "HB_EI_XFREENULL       ", HB_LANGERRINTR(9011) }, ;
  { "HB_EI_VMBADSTARTUP    ", HB_LANGERRINTR(9012) }, ;
  { "HB_EI_VMNOSTARTUP     ", HB_LANGERRINTR(9013) }, ;
  { "HB_EI_VMBADOPCODE     ", HB_LANGERRINTR(9014) }, ;
  { "HB_EI_VMNOTSYMBOL     ", HB_LANGERRINTR(9015) }, ;
  { "HB_EI_VMINVSYMBOL     ", HB_LANGERRINTR(9016) }, ;
  { "HB_EI_VMNOTCBLOCK     ", HB_LANGERRINTR(9017) }, ;
  { "HB_EI_VMPOPINVITEM    ", HB_LANGERRINTR(9018) }, ;
  { "HB_EI_STACKUFLOW      ", HB_LANGERRINTR(9019) }, ;
  { "HB_EI_ITEMBADCOPY     ", HB_LANGERRINTR(9020) }, ;
  { "HB_EI_MVBADSYMBOL     ", HB_LANGERRINTR(9021) }, ;
  { "HB_EI_XMEMOVERFLOW    ", HB_LANGERRINTR(9022) }, ;
  { "HB_EI_XGRABNULLSIZE   ", HB_LANGERRINTR(9023) }, ;
  { "HB_EI_XREALLOCNULLSIZE", HB_LANGERRINTR(9024) }, ;
  { "HB_EI_XALLOCNULLSIZE  ", HB_LANGERRINTR(9025) } }
return aErr
