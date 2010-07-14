/*
 * $Id$
 */

/*
 * File......: sqzn.prg
 * Author....: Joseph D. Booth, Sr.
 * CIS ID....: 72040,2112
 *
 * This is an original work by Joseph D. Booth Sr. and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.1   15 Aug 1991 23:04:38   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   13 Jun 1991 15:21:36   GLENN
 * Initial revision.
 *
 */

function ft_sqzn(nValue,nSize,nDecimals)
  local tmpstr,cCompressed,k

  nSize       := iif(nSize    ==NIL,10,nSize )
  nDecimals   := iif(nDecimals==NIL, 0,nDecimals )
  nValue      := nValue * (10**nDecimals)
  nSize       := iif(nSize/2!=int(nSize/2),nSize+1,nSize)
  tmpstr      := str( abs(nValue),nSize )
  tmpstr      := strtran(tmpstr," ","0")
  cCompressed := chr( val(substr(tmpstr,1,2))+iif(nValue<0,128,0) )

  for k := 3 to len(tmpstr) step 2
     cCompressed += chr(val(substr(tmpstr,k,2)))
  next
  return cCompressed

function ft_unsqzn(cCompressed,nSize,nDecimals)
  local tmp:="",k,cValue,multi:=1

  nSize       := iif(nSize    ==NIL,10,nSize )
  nDecimals   := iif(nDecimals==NIL, 0,nDecimals)
  cCompressed := iif(multi    ==-1,substr(cCompressed,2),cCompressed)
  nSize       := iif(nSize/2!=int(nSize/2),nSize+1,nSize)
  if asc(cCompressed) > 127
     tmp         := str(asc(cCompressed)-128,2)
     multi       := -1
  else
     tmp         := str(asc(cCompressed),2)
  endif

  for k := 2 to len(cCompressed)
     tmp += str(asc(substr(cCompressed,k,1)),2)
  next

  tmp    := strtran(tmp," ","0")
  cValue := substr(tmp,1,nSize-nDecimals)+"."+substr(tmp,nSize-nDecimals+1)

  return val(cValue) * multi
