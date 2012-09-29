/*
 * $Id$
 */

/*
 * Author....: Glenn Scott
 * CIS ID....: 71620,1521
 *
 * This is an original work by Glenn Scott and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.7   28 Sep 1992 23:48:48   GLENN
 * Deleted #define for FLAG_CARRY as Toolkit v2.1's ftint86.ch has it.
 *
 *    Rev 1.6   03 Oct 1991 18:36:28   GLENN
 * Tim Wong from Nantucket pointed out that this DOS function actually
 * leaves a file handle in AX.  In order to preserve the functionality,
 * I now fclose() that handle if the call is succsessful.
 *
 *    Rev 1.5   15 Aug 1991 23:05:04   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.4   17 Jul 1991 22:11:18   GLENN
 * Stripped off hb_bchar(0)s in the return value (aRegs[DS])
 *
 *    Rev 1.3   03 Jul 1991 01:08:08   GLENN
 * Changed one line in FT_TEST driver ( cHide == "Y" )
 *
 *    Rev 1.2   14 Jun 1991 19:53:10   GLENN
 * Minor edit to file header
 *
 *    Rev 1.1   12 Jun 1991 02:45:40   GLENN
 * Documentation mods, and convert to new ft_int86() syntax, return value.
 *
 *    Rev 1.0   01 Apr 1991 01:02:24   GLENN
 * Nanforum Toolkit
 *
 */

#include "fileio.ch"

#ifdef HB_OS_DOS
#define FT_TEMPFILE_ORIGINAL
#endif

#ifdef HB_OS_DOS_32
#undef FT_TEMPFILE_ORIGINAL
#endif

#ifdef FT_TEMPFILE_ORIGINAL

#include "ftint86.ch"

#define DOS         33
#define TEMPNAME    90

FUNCTION FT_TEMPFIL( cPath, lHide, nHandle )

   LOCAL cRet, aRegs[ 3 ]

   cPath := iif( ! HB_ISSTRING( cPath ), ;
      Replicate( hb_BChar( 0 ), 13 ) , ;
      cPath += Replicate( hb_BChar( 0 ), 13 ) )

   lHide := iif( HB_ISLOGICAL( lHide ), lHide, .F. )
   /*
   aRegs[ AX ] := MAKEHI( TEMPNAME )
   aRegs[ CX ] := iif( lHide, 2, 0 )
   aRegs[ DS ] := cPath
   aRegs[ DX ] := REG_DS

   FT_INT86( DOS, aRegs )
   */
   aRegs := _ft_tempfil( cPath, lHide )
    /*  If carry flag is clear, then call succeeded and a file handle is
     *  sitting in AX that needs to be closed.
     */

   IF ! ft_isBitOn( aRegs[ 3 ], FLAG_CARRY )
      IF PCount() >= 3
         nHandle := aRegs[ 1 ]
      ELSE
         FClose( aRegs[ 1 ] )
      ENDIF
      cRet := AllTrim( StrTran( aRegs[ 2 ], hb_BChar( 0 ) ) )
   ELSE
      cRet := ""
   ENDIF

   RETURN cRet

#else

FUNCTION FT_TEMPFIL( cPath, lHide, nHandle )

   LOCAL cFile

   hb_default( @cPath, "." + hb_ps() )
   hb_default( @lHide, .F. )

   cPath := AllTrim( cPath )

   nHandle := hb_FTempCreate( cPath, NIL, iif( lHide, FC_HIDDEN, FC_NORMAL ), @cFile )

   IF PCount() <= 2
      FClose( nHandle )
   ENDIF

   RETURN cFile

#endif /* FT_TEMPFILE_ORIGINAL */
