/*
 * $Id$
 */

/*
  tsix.prg
  Demo using TApollo Class
  Class is not complete yet, please contribute codes to make it complete :-)
*/

#include "simpleio.ch"

EXTERNAL HB_GT_WIN_DEFAULT

PROCEDURE MAIN()

  LOCAL oDBF  := TApollo():New()
  LOCAL oDBF1 := TApollo():New()
  LOCAL nsec

  SX_ERRORLEVEL( 0 )

  oDBF1:cDBFFile  := "test.dbf"
  oDBF1:cAlias    := "CUSTOMER"
  oDBF1:cRDD      := "SDENSX"

  oDBF:cDBFFile   := "test.dbf"
  oDBF:cAlias     := "MYTEST"
  oDBF:cRDD       := "SDENSX"

  oDBF1:Open()
  ? "oDbf1:Reccount()", oDbf1:Reccount()

  oDBF:Open()
  ? "oDbf:Reccount()", oDbf:Reccount()

  ? "oDBF1:nWorkArea()=", oDBF1:nWorkArea()
  ? "oDBF:nWorkArea()=", oDBF:nWorkArea()

  ? "oDBF1:cAlias=", oDBF1:cAlias
  ? "oDBF:cAlias=", oDBF:cAlias

  ? "oDBF1:cRDD=", oDBF1:cRDD
  ? "oDBF:cRDD=", oDBF:cRDD

  ? 'oDBF:FIRST=',oDBF:FIRST
  ? 'oDBF:AGE := 99'
     oDBF:AGE := 99
     oDBF:Commit()
  ? oDBF:AGE
  ? 'oDBF:AGE := 25'
     oDBF:AGE := 25
     oDBF:Commit()
  ? oDBF:AGE
  ? 'oDBF:FIRST=',oDBF:FIRST
  ? 'oDBF:FIRST="Andi"'
     oDBF:FIRST := "Andi"
  ? 'oDBF:FIRST=',oDBF:FIRST
  inkey(0)

  nsec  := seconds()

  oDBF:dbGotop()
  While !oDBF:Eof()
     ? oDbf:First + " " + oDbf:Last
     oDbf:dbSkip()
  Enddo

  ?

  While !oDBF:Bof()
     ? oDbf:First + " " + oDbf:Last
     oDbf:dbSkip(-1)
  Enddo

  ? seconds() - nsec
  ? "Now browsing oDBF1", odbf1:cDBFFile
  inkey(0)

  nsec  := seconds()
  oDBF1:dbGotop()
  While !oDBF1:Eof()
     ? oDbf1:First + " " + oDbf1:Last
     oDbf1:dbSkip()
  Enddo

  oDBF:Close()
  ? "oDBF Closed ..."
  ? seconds() - nsec
  ? sx_version()
  ? sx_sxapi_version()
