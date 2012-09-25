/*
 * $Id$
 */

/*
 * File......: miltime.prg
 * Author....: Alexander B. Spencer
 * CIS ID....: 76276,1012
 *
 * This is an original work by Alexander B. Spencer and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:04:02   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:22   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   14 Jun 1991 03:43:52   GLENN
 * Initial revision.
 *
 */

#ifdef FT_TEST

  PROCEDURE Main()

     cls
     ? "am-pm"
     ? ft_civ2mil(" 5:40 pm")
     ? ft_civ2mil("05:40 pm")
     ? ft_civ2mil(" 5:40 PM")
     ? ft_civ2mil(" 5:40 am")
     ? ft_civ2mil("05:40 am")
     ? ft_civ2mil(" 5:40 AM")
     ?
     inkey(0)
     cls
     ? "noon-midnight"
     ? ft_civ2mil("12:00 m")
     ? ft_civ2mil("12:00 M")
     ? ft_civ2mil("12:00 m")
     ? ft_civ2mil("12:00 n")
     ? ft_civ2mil("12:00 N")
     ? ft_civ2mil("12:00 n")
     ?
     inkey(0)
     cls
     ? "errors in noon-midnight"
     ? ft_civ2mil("12:01 n")
     ? ft_civ2mil("22:00 n")
     ? ft_civ2mil("12:01 m")
     ? ft_civ2mil("22:00 n")
     ?
     ? "sys to mil"
     ? time()
     ? ft_sys2mil()
  return

#endif

function FT_MIL2MIN(cMILTIME)
  return int(val(left(cMILTIME,2))*60 + val(right(cMILTIME,2)))

function FT_MIN2MIL(nMIN)
  nMIN := nMIN%1440
  return  right("00" + ltrim(str(INT(nMIN/60))),2) + ;
          right("00" + ltrim(str(INT(nMIN%60))),2)

function FT_MIL2CIV(cMILTIME)
  local cHRS,cMINS,nHRS,cCIVTIME

  nHRS  := val(LEFT(cMILTIME,2))
  cMINS := right(cMILTIME,2)

  do case
     case (nHRS == 24 .OR. nHRS == 0) .AND. (cMINS == "00")  // Midnight
        cCIVTIME := "12:00 m"
     case (nHRS == 12)                                       // Noon to 12:59pm
        if cMINS == "00"
           cCIVTIME := "12:00 n"
        else
           cCIVTIME := "12:" + cMINS + " pm"
        endif
     case (nHRS < 12)                                    // AM
        if nHRS == 0
           cHRS := "12"
        else
           cHRS := right("  " + ltrim(str(int(nHRS))),2)
        endif
        cCIVTIME := cHRS + ":" + cMINS + " am"

  otherwise                                           // PM
     cCIVTIME := right("  " + ltrim(str(int(nHRS - 12))), 2) + ;
                ":" + cMINS + " pm"
  endcase

  return cCIVTIME

function FT_CIV2MIL(cTIME)
  local cKEY, cMILTIME

*** Insure leading 0's
cTIME := REPLICATE("0", 3 - at(":", ltrim(cTIME))) + ltrim(cTIME)

*** Adjust for popular use of '12' for first hour after noon and midnight
if left(ltrim(cTIME),2) == "12"
   cTIME := stuff(cTIME, 1, 2, "00")
endif

*** am, pm, noon or midnight
cKEY := substr(ltrim(cTIME), 7, 1)

do case
case upper(cKEY) == "N"                           // noon
      if left(cTIME,2) + substr(cTIME,4,2) == "0000"
         cMILTIME := "1200"
      else
         cMILTIME := "    "
      endif
   case upper(cKEY) == "M"                           // midnight
      if left(cTIME,2) + substr(cTIME,4,2) == "0000"
         cMILTIME := "0000"
      else
         cMILTIME := "    "
      endif
   case upper(cKEY) == "A"                           // am
      cMILTIME := right("00" + ltrim(str(val(left(cTIME,2)))),2) + ;
                  substr(cTIME,4,2)
   case upper(cKEY) == "P"                           // pm
      cMILTIME := right("00" + ltrim(str(val(left(cTIME,2))+12)),2) + ;
                 substr(cTIME,4,2)
   otherwise
      cMILTIME := "    "                              // error
endcase

  return cMILTIME

function FT_SYS2MIL()
return left(stuff(time(),3,1,""),4)
