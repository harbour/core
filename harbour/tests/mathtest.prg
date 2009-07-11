//
// $Id$
//

#include "hbmath.ch"

function main()

local nOldMathErrMode
local bOldMathErr

  qout ("Testing math function: EXP(), LOG() and SQRT():")
  qout ("")
  qout ("  I) Test with correct arguments:")
  qout ("     exp(0.0) == 1.00         ? ", exp (0.0))
  qout ("     exp(1.0) == 2.71(8)...  ? ", exp (1.0))
  qout ("     exp(-1.0) == 0.36(7)... ? ", exp (-1.0))
  qout ("")
  qout ("     log(1.0) == 0.00        ? ", log (1.0))
  qout ("     log(2.7) == 0.99(3)...  ? ", log (2.7))
  qout ("     log(0.36) == -1.02(1)... ? ", log (0.36))
  qout ("")
  qout ("     sqrt(1.0) == 1.00      ? ", sqrt (1.0))
  qout ("     sqrt(4.0) == 2.00      ? ", sqrt (4.0))
  qout ("     sqrt(2.0) == 1.41(4).. ? ", sqrt (2.0))
  qout ("")
  qout ("  II) Test with numeric but incorrect arguments:")
  qout ("")
  qout ("  IIa) default error handling (by the functions themselves)")
  qout ("       exp (-1000) == 0.00   ?", exp (-1000))
  qout ("       exp (1000) == ****... ?", exp (1000))
  qout ("")
  qout ("       log (0) == ****...  ?", log (0))
  qout ("       log (-10) == *****... ?", log (-10))
  qout ("")
  qout ("       sqrt (-4) == 0.00 ?", sqrt (-4))
  qout ("")

  nOldMathErrMode := hb_MathErMode (HB_MATH_ERRMODE_USERDEFAULT)

  qout ("  IIb) error handling by error (hb_MathErMode() == HB_MATH_ERRMODE_USERDEFAULT)")
  qout ("       exp (-1000) == 0.00   ?", exp (-1000))
  qout ("       exp (1000) == ****... ?", exp (1000))
  qout ("")
  qout ("       log (0) == ****...  ?", log (0))
  qout ("       log (-10) == *****... ?", log (-10))
  qout ("")
  qout ("       sqrt (-4) == 0.00 ?", sqrt (-4))
  qout ("")

  hb_MathErMode (nOldMathErrMode)

  bOldMathErr := hb_MathErBlock ({|nType, cFuncname, cError, nArg1, nArg2, aInfo|;
                                  localmatherr (nType, cFuncname, cError, nArg1, nArg2, aInfo)})

  qout ("  IIc) error handling by callback block (hb_MathErBlock())")
  qout ("       exp (-1000) == ?", exp (-1000))
  qout ("       exp (1000) ==  ?", exp (1000))
  qout ("")
  qout ("       log (0) ==     ?", log (0))
  qout ("       log (-10) ==   ?", log (-10))
  qout ("")
  qout ("       sqrt (-4) ==   ?", sqrt (-4))

  hb_MathErBlock (bOldMathErr)

return nil

function localmatherr (nType, cFuncname, cError, nArg1, nArg2, aInfo)

local cStr := "!! Local handling of math error MATH/"

  cStr += alltrim(str(nType))+" in "+cFuncname+"("

  if valtype(nArg1) == "N"
    cStr += alltrim(str(nArg1))
  endif
  if valtype(nArg2) == "N"
    cStr += ","+alltrim(str(nArg2))
  endif
  cStr += "):"
  qout (cStr)
  qout ("!!                              "+cError)
  if aInfo[HB_MATHERRORBLOCK_HANDLED]
    qout ("!!                               --> already handled with return value: "+;
          alltrim(str(aInfo[HB_MATHERRORBLOCK_RETVAL])))
    return 1
  endif

  qout ("!!       setting return value to --> 5.0")

  aInfo[HB_MATHERRORBLOCK_RETVAL] := 5.0
  aInfo[HB_MATHERRORBLOCK_HANDLED] := .T.

return 1
