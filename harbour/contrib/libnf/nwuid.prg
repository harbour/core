/*
 * File......: NWUID.PRG
 * Author....: Glenn Scott
 * CIS ID....: 71620,1521
 *
 * This is an original work by Glenn Scott and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.4   15 Aug 1991 23:04:10   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.3   14 Jun 1991 19:52:34   GLENN
 * Minor edit to file header
 *
 *    Rev 1.2   14 Jun 1991 04:31:30   GLENN
 * Return value still needs to have nulls (chr(0)) removed.  Put that back
 * in.
 *
 *    Rev 1.1   12 Jun 1991 02:25:22   GLENN
 * Documentation correction and revision of ft_int86() call to account
 * for Ted's new string passing conventions.
 *
 *    Rev 1.0   01 Apr 1991 01:01:56   GLENN
 * Nanforum Toolkit
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_NWUID()
 *  $CATEGORY$
 *     NetWare
 *  $ONELINER$
 *     Return the current Novell NetWare User ID
 *  $SYNTAX$
 *     FT_NWUID( [ <nConnection> ] ) -> cUid
 *  $ARGUMENTS$
 *     <nConnection> is a connection number, or logical station number,
 *     to find a userid for.  Under NetWare 286, this number can be from
 *     1 to 100.  Under NetWare 386, 1-250.  If not supplied, FT_NWUID()
 *     defaults to the current connection (i.e., the connection running
 *     the application).
 *  $RETURNS$
 *     A string containing the userid, or "login name."
 *     The maximum length of this string, as defined by current
 *     versions of Novell NetWare, is 48 characters.
 *  $DESCRIPTION$
 *     FT_NWUID() returns the current NetWare userid, or "login
 *     name."  This is useful for implementing security or audit
 *     trail procedures within your programs.
 *
 *     There is no simple way a user can "fool" this function into
 *     retrieving an incorrect value, provided a NetWare shell is loaded.
 *
 *     This function requires FT_INT86() and FT_NWLSTAT()
 *
 *     This function does NOT test for the existence of the NetWare shell.
 *     The behavior is undefined if no shell is loaded. You'll usually get
 *     garbage.  This function has not been tested on NetWare 386.
 *  $EXAMPLES$
 *     QOut( "I am: " + FT_NWUID() )
 *
 *     FOR x := 1 TO 100
 *       cUid := FT_NWUID( x )
 *       IF .NOT Empty( cUid )
 *         QOut( Str( x, 3 ) + Space(3) + cUid )
 *       ENDIF
 *     NEXT
 *  $END$
 */

#include "ftint86.ch"

#define DOS         33
#define NW_LOG     227

#ifdef FT_TEST
  FUNCTION MAIN()
     local x, cUid
     QOut( "I am: [" + FT_NWUID() + "]" )
     QOut( "---------------------" )

      for x:= 1 to 100
        cUid := FT_NWUID( x )
        if .not. empty( cUid )
          QOut( str( x, 3 ) + space(3) + cUid )
        endif
      next

  return ( nil )
#endif

FUNCTION FT_NWUID( nConn )
  LOCAL aRegs[ INT86_MAX_REGS ], ;
        cReqPkt,                 ;
        cRepPkt

  nConn := IIF( nConn == nil, FT_NWLSTAT(), nConn )

  // Set up request packet

  cReqPkt  :=  chr( 22    )          // Function 22: Get Connection Information
  cReqPkt  +=  chr( nConn )
  cReqPkt  :=  i2bin( len( cReqPkt ) ) + cReqPkt

  // Set up reply packet

  cRepPkt  :=  space(63)

  // Assign registers

  aRegs[ AX ]        :=  MAKEHI( NW_LOG )
  aRegs[ DS ]        :=  cReqPkt
  aRegs[ SI ]        :=  REG_DS
  aRegs[ ES ]        :=  cRepPkt
  aRegs[ DI ]        :=  REG_ES

  FT_INT86( DOS, aRegs )
  RETURN alltrim( strtran( substr( aRegs[ ES ], 9, 48 ), chr(0) )  )



