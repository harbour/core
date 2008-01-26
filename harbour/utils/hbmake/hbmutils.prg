/*
 * $Id$
 */
/*
 * xHarbour Project source code:
 * hbmutils.prg - utils for hbmake.
 *
 * Copyright 2000,2001,2002,2003,2004 Luiz Rafael Culik <culikr@uol.com.br>
 * www - http://www.xharbour.org
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
 * along with this software; see the file COPYING.  If not, write to
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

#include "common.ch"
#ifndef __HARBOUR__
    #include 'hbclip.ch'
#else
    DECLARE ExtenPrg( cExt AS STRING, nType AS NUMERIC ) AS STRING
    DECLARE Exten( cExt as string, nType as numeric ) as string
    DECLARE GetSourceFiles( lSubDir as logical ) as ARRAY
    DECLARE GetDirs( cPat as USUAL ) as Array
    DECLARE GetHarbourDir() as String
    DECLARE GetBccDir() as String
    DECLARE GetVccDir() as String
    DECLARE GetMakeDir() as String
    DECLARE HB_ARGV( n as numeric ) as string
    DECLARE HbMake_FileDate( c as String ) as string
    DECLARE GetPoccDir() as String

#endif

*--------------------------------------------
FUNCTION GetSourceFiles( lSubDir, lGcc, cOs )
*--------------------------------------------

   LOCAL aDirs
   LOCAL aRet      := {}
   LOCAL lLinux    := AT( 'linux', LOWER( cOs ) ) > 0 .OR. lGcc
   LOCAL cDir      := IIF( ! lLinux, '\' + CURDIR() + '\', '/' + CURDIR() + '/' )
   LOCAL aStru     := { cDir }
   LOCAL aData
   LOCAL nCounter  := 0
   LOCAL nArrayLen
   LOCAL nDatalen
   LOCAL y
   LOCAL cItem
   LOCAL cExt
   LOCAL cPath
   LOCAL cDrive
   LOCAL nPos
   LOCAL xItem
   LOCAL nLen
   LOCAL cFile
   LOCAL nPadr

   DEFAULT lSubDir TO .t.

   WHILE ++ nCounter <= LEN( aStru )

      IF ! EMPTY( aDirs := GetDirs( aStru[ nCounter ], lGcc ) )                  // There are elements!
         AEVAL( aDirs, { | xItem | AADD( aStru, xItem ) } )
      ENDIF

   ENDDO

   aDirs := {}

   ASort( aStru )
   nArrayLen := LEN( aStru )

   FOR nCounter := 1 TO nArrayLen

      IF LEN( aData := DIR_MULTI( aStru[ nCounter ] + "*.prg |" + aStru[ nCounter ] + "*.c |" + aStru[ nCounter ] + "*.cpp" ) ) != 0

         nDataLen := LEN( aData )

         nPadr := 12 // maximum Clipper/DOS source file name length with extension.
         // if this lenght is greater than 12, then reset nPadr.
         FOR y := 1 TO nDataLen
             nPadr := Max( AT('.PRG', UPPER( aData[ y, 1 ] ) )+3 , nPadr )
             nPadr := Max( AT('.C',   UPPER( aData[ y, 1 ] ) )+1 , nPadr )
             nPadr := Max( AT('.CPP', UPPER( aData[ y, 1 ] ) )+3 , nPadr )
         NEXT

         FOR y := 1 TO nDataLen

            IF AT( '.PRG', UPPER( aData[ y, 1 ] ) ) > 0 .OR. AT( '.C', UPPER( aData[ y, 1 ] ) ) > 0 .OR. AT( '.CPP', UPPER( aData[ y, 1 ] ) ) > 0

               IF lSubDir

                  nLen := AT( " ", aData[ y, 1 ] ) + 1

                  AADD( aRet, STRTRAN( aStru[ nCounter ], cDir, '' ) +;
                        PadR(aData[ y,1 ] ,nPadr) + ;         // prg name
                        STR(aData[ y, 2 ] , 8 ) + '  ' + ;    // prg size
                        DTOC(aData[ y, 3 ] ) + '  ' + ;       // prg date
                        aData[ y, 4 ] )                       // prg time

               ELSEIF ! lSubDir .AND. AT( IIF( lLinux, "/", "\" ), STRTRAN( aStru[ nCounter ], cDir, '' ) ) == 0

                  AADD( aRet, PadR(aData[ y, 1 ],nPadr) + ;   // prg name
                        STR( aData[ y, 2 ], 8 ) + '  ' + ;    // prg size
                        DTOC( aData[ y, 3 ] ) + '  ' + ;      // prg date
                        aData[ y, 4 ] )                       // prg time

               ENDIF

            ENDIF

         NEXT

      ENDIF

   NEXT

   //     For nCounter := 1 To Len( aRet )
   FOR EACH cFile IN aRet

      xItem := SUBSTR( cFile, RAT( IIF( lLinux, "/", '\' ), cFile ) + 1 )
      nPos  := ASCAN( aStru, { | x | x := SUBSTR( x, RAT( IIF( lLinux, "/", '\' ), x ) + 1 ), LEFT( x, AT( ".", x ) ) == LEFT( xitem, AT( ".", xitem ) ) } )

      IF nPos > 0
         ADEL( aStru, nPos )
         ASIZE( aStru, LEN( aStru ) - 1 )
      ENDIF

   NEXT

   FOR EACH cFile IN aStru

      HB_FNAMESPLIT( LEFT( cFile, AT( ' ', cFile ) - 1 ), @cPath, @cItem, @cExt, @cDrive )

      IF ( cExt == '.C' ) .OR. ( cExt == ".c" ) .OR. ( cExt == '.CPP' ) .OR. ( cExt == ".cpp" )
         AADD( aRet, cFile )
      ENDIF

   NEXT
RETURN aRet

*-------------------------------
FUNCTION ExtenPrg( cExt, nType )
*-------------------------------

   LOCAL aExt   := { "C", "c" }
   LOCAL nPos
   LOCAL cTemp  := ""

   nPos := ASCAN( aExt, { | a | a == cExt } )

   IF nPos > 0
      SWITCH nType
      CASE 1
         cTemp := STRTRAN( cExt, aExt[ nPos ], 'prg' )
         EXIT
      CASE  2
         cTemp := STRTRAN( cExt, aExt[ nPos ], 'prG' )
         EXIT
      CASE  3
         cTemp := STRTRAN( cExt, aExt[ nPos ], 'pRg' )
         EXIT
      CASE  4
         cTemp := STRTRAN( cExt, aExt[ nPos ], 'Prg' )
         EXIT
      CASE  5
         cTemp := STRTRAN( cExt, aExt[ nPos ], 'PRg' )
         EXIT
      CASE  6
         cTemp := STRTRAN( cExt, aExt[ nPos ], 'PrG' )
         EXIT
      CASE  7
         cTemp := STRTRAN( cExt, aExt[ nPos ], 'PRG' )
         EXIT
      END
   ENDIF

RETURN cTemp

*----------------------------------------
STATIC FUNCTION GetDirs( cPattern, lGcc )
*----------------------------------------

   LOCAL aDir   := {}
   LOCAL lLinux := AT( 'linux', LOWER( OS() ) ) > 0 .OR. lGcc

   AEVAL( DIRECTORY( cPattern + IIF( lLinux, "*", "*.*" ), "D" ), ;
          { | xItem | IIF( xItem[ 5 ] = "D" .AND. ;
          ( xItem[ 1 ] != "." .AND. xItem[ 1 ] != ".." ), ;
          AADD( aDir, cPattern + xItem[ 1 ] + IIF( lLinux, "/", '\' ) ), ;
          ) } )

RETURN ( aDir )

*-----------------------
FUNCTION GetHarbourDir()
*-----------------------

    LOCAL cPath   := ''
    LOCAL cEnv    := GETE( "PATH" )
    LOCAL lLinux  := "LINUX" $ upper(OS())
    LOCAL lUnix   := If( "UNIX" $ upper(OS()) .OR. "HP-UX" $ upper(OS()), .T., .F. )
    LOCAL aEnv     
    LOCAL cCurEnv := ""
    LOCAL cBar    := iif( lLinux .or. lUnix, "/" , "\" )
    LOCAL HBSTRG  := ""
    LOCAL cPathUni:= GETE( "PATH_HARBOUR" )

    hbstrg := IIF ( lLinux .or. lUnix,  "harbour" , "harbour.exe" )
    
    If lUnix 
       If cPathUni == Nil
          cPathUni := ""
       EndIF   
       cEnv += ":" + cPathUni 
    EndIf

    aEnv    := HB_ATokens( cEnv, iif(lLinux .or. lUnix,":",";") )

    FOR EACH cCurEnv IN aEnv

        IF FILE( cCurEnv + cBar + hbstrg ) .OR. FILE( UPPER( cCurEnv ) + cBar + upper(hbstrg) )
           cPath := cCurEnv
           cPath := LEFT( cPath, RAT( cBar, cPath ) - 1 )
           EXIT
        ENDIF

    NEXT

RETURN cPath

*-------------------
FUNCTION GetBccDir()
*-------------------

   LOCAL cPath   := ''
   LOCAL cEnv    := GETE( "PATH" )
   LOCAL aEnv    := HB_ATokens( cEnv, ";" )
   LOCAL cCurEnv := ""

   FOR EACH cCurEnv IN aEnv

      IF FILE( cCurEnv + '\bcc32.exe' ) .OR. FILE( UPPER( cCurEnv ) + '\BCC32.EXE' )
         cPath := cCurEnv
         cPath := LEFT( cPath, RAT( '\', cPath ) - 1 )
         EXIT
      ENDIF

   NEXT

RETURN cPath

*-------------------
FUNCTION GetVccDir()
*-------------------

   LOCAL cPath   := ''
   LOCAL cEnv    := GETE( "PATH" )
   LOCAL aEnv    := HB_ATokens( cEnv, ";" )
   LOCAL cCurEnv := ""

   FOR EACH cCurEnv IN aEnv

      IF FILE( cCurEnv + '\cl.exe' ) .OR. FILE( UPPER( cCurEnv ) + '\cl.EXE' )
         cPath := cCurEnv
         cPath := LEFT( cPath, RAT( '\', cPath ) - 1 )
         EXIT
      ENDIF

   NEXT

RETURN cPath

*--------------------
FUNCTION GetPoccDir()
*--------------------

   LOCAL cPath   := ''
   LOCAL cEnv    := GETE( "PATH" )
   LOCAL aEnv    := HB_ATokens( cEnv, ";" )
   LOCAL cCurEnv := ""

   FOR EACH cCurEnv IN aEnv

      IF FILE( cCurEnv + '\pocc.exe' ) .OR. FILE( UPPER( cCurEnv ) + '\POCC.EXE' )
         cPath := cCurEnv
         cPath := LEFT( cPath, RAT( '\', cPath ) - 1 )
         EXIT
      ENDIF

   NEXT

RETURN cPath

*----------------------------
FUNCTION Exten( cExt, nType )
*----------------------------

   LOCAL aExt    := { 'C', 'c', "CPP", "cpp" }
   LOCAL nPos
   LOCAL cTemp   := ""

   nPos := ASCAN( aExt, { | a | a == cExt } )
   IF nPos > 0

      SWITCH  nType
      CASE 1
         cTemp := STRTRAN( cExt, aExt[ nPos ], 'o' )
         EXIT

      CASE 2
         cTemp := STRTRAN( cExt, aExt[ nPos ], 'obj' )
         EXIT

      END

   ENDIF

RETURN cTemp

*--------------------
FUNCTION GetMakeDir()
*--------------------

   LOCAL cPath := ""
   LOCAL cExe  := HB_ARGV( 0 )

   cExe  := STRTRAN( cExe, "/", "\" )
   cPath := LEFT( cexe, RAT( "\", cexe ) - 1 )
   cPath := LEFT( cPath, RAT( "\", cPath ) - 1 )

RETURN cPath

*----------------------------
FUNCTION GetSourceDirMacros()
*----------------------------

   LOCAL aDirs
   LOCAL lLinux    := AT( 'linux', LOWER( OS() ) ) > 0
   LOCAL cDir      := IIF( lLinux, '/' + CURDIR() + '/', '\' + CURDIR() + '\' )
   LOCAL aStru     := { cDir }

   LOCAL nCounter  := 0
   LOCAL aMacros   := {}

   WHILE ++ nCounter <= LEN( aStru )

      IF ! EMPTY( aDirs := GetDirs( aStru[ nCounter ], lLinux ) )                // There are elements!
         AEVAL( aDirs, { | xItem | AADD( aStru, xItem ) } )
      ENDIF

   ENDDO

   FOR nCounter := 1 TO LEN( aStru )
      AADD( aMacros, { "SRC" + STRZERO( nCounter, 2, 0 ), STRTRAN( aStru[ nCounter ], cDir, '' ), .f. } )
   NEXT

RETURN aMacros

*------------------------------------
FUNCTION HbMake_FileDate( cFileName )
*------------------------------------

   LOCAL aFiles := DIRECTORY( cFileName )

RETURN IIF( LEN( aFiles ) == 1, aFiles[ 1, 3 ], CTOD( '' ) )

*------------------------------------
FUNCTION HbMake_FileTime( cFileName )
*------------------------------------

   LOCAL aFiles := DIRECTORY( cFileName )

RETURN IIF( LEN( aFiles ) == 1, aFiles[ 1, 4 ], '' )

*------------------------------
FUNCTION TD2JUL( CTIME, DDATE )
*------------------------------
RETURN DDATE - CTOD( '01/01/1900' ) + ( PRB_INT( TTOS( CTIME ) / 100000,, 5 ) )

*---------------------
STATIC FUNCTION TTOS( CTIME )
*---------------------

RETURN ( VAL( SUBSTR( CTIME, 7, 2 ) ) ) + ;
         ( VAL( SUBSTR( CTIME, 4, 2 ) ) * 60 ) + ;
         ( VAL( SUBSTR( CTIME, 1, 2 ) ) * 3600 )

*---------------------------------------------------
FUNCTION PRB_INT( SOMENUMBER, length, NUM_DECIMALS )
*---------------------------------------------------

   LOCAL NEGATIVE   := ( SOMENUMBER < 0 )
   LOCAL SOMESTRING
   LOCAL dotat

   DEFAULT NUM_DECIMALS TO 0
   DEFAULT length TO 19

   IF NEGATIVE
      SOMENUMBER := ABS( SOMENUMBER )
   ENDIF

   SOMENUMBER += .0000000000000005

   SOMESTRING := ALLTRIM( STR( SOMENUMBER ) )

   dotat := AT( '.', somestring )

   DO CASE
      CASE NUM_DECIMALS == 0
         IF dotat > 0
            somestring := LEFT( somestring, dotat - 1 )
         ENDIF

      CASE NUM_DECIMALS > 0
         IF dotat > 0
            somestring := LEFT( somestring, dotat + num_decimals )
         ENDIF

   ENDCASE

   IF NEGATIVE
      SOMESTRING := '-' + SOMESTRING
   ENDIF

RETURN VAL( SOMESTRING )

*---------------------------
FUNCTION Exte( cExt, nType )
*---------------------------

   LOCAL aExt  := { 'prg', 'prG', 'pRg', 'Prg', 'PRg', 'PrG', 'PRG' }
   LOCAL nPos
   LOCAL cTemp := ""

   nPos := ASCAN( aExt, { | a | a == cExt } )
   IF nPos > 0
      IF nType == 1
         cTemp := STRTRAN( cExt, aExt[ nPos ], 'c' )
      ELSEIF nType == 2
         cTemp := STRTRAN( cExt, aExt[ nPos ], 'obj' )
      ELSEIF nType == 3
         cTemp := STRTRAN( cExt, aExt[ nPos ], 'o' )
      ENDIF

   ENDIF

RETURN cTemp

*-----------------------------------------------
PROCEDURE ATTENTION( CSTRING, NLINENUM, CCOLOR )
*-----------------------------------------------

   LOCAL COLDCOLOR

   DEFAULT NLINENUM TO 24
   DEFAULT CCOLOR TO 'GR+/R'

   COLDCOLOR := SETCOLOR( CCOLOR )

   CSTRING := '  ' + ALLTRIM( CSTRING ) + '  '

   DEVPOS( NLINENUM, c( CSTRING ) )

   DEVOUT( CSTRING )

   SETCOLOR( COLDCOLOR )

RETURN

*--------------------
FUNCTION c( CSTRING )
*--------------------

RETURN MAX( ( MAXCOL() / 2 ) - INT( LEN( CSTRING ) / 2 ), 0 )

*----------------------
FUNCTION ReadLN( lEof )
*----------------------

   LOCAL cBuffer := ""
   cBuffer := FT_FREADLN()
   cBuffer := STRTRAN( cBuffer, CHR( 13 ), '' )
   cBuffer := STRTRAN( cBuffer, CHR( 10 ), '' )
   FT_FSKIP( 1 )
   lEof := ft_FEOF()

RETURN cBuffer

*--------------------------------------
FUNCTION GetInstaledLibs( clibs, lGcc )
*--------------------------------------

   LOCAL cSuffix := IIF( lGCC, ".a", ".lib" )
   LOCAL aReturnLibs := {}
   LOCAL aLibs       := DIRECTORY( clibs )
   LOCAL nPos
   LOCAL nCount
   LOCAL cItem
   LOCAL aDefLib := {}


   aadd(aDefLib,'ace32'+ cSuffix)
   aadd(aDefLib,'hbcpage'+ cSuffix)
   aadd(aDefLib,'hbcommon'+ cSuffix)
   aadd(aDefLib,'hbct'+cSuffix)
   aadd(aDefLib,'rdddbt'+ cSuffix)
   aadd(aDefLib,'rddcdx'+ cSuffix)
   aadd(aDefLib,'rddfpt'+ cSuffix)
   aadd(aDefLib,'rddntx'+ cSuffix)
   aadd(aDefLib,'hbdebug'+ cSuffix)
   aadd(aDefLib,'gtcgi'+ cSuffix)
   aadd(aDefLib,'gtdos'+ cSuffix)
   aadd(aDefLib,'gtpca'+ cSuffix)
   aadd(aDefLib,'gtnul'+ cSuffix)
   aadd(aDefLib,'gtsln'+ cSuffix)
   aadd(aDefLib,'gtstd'+ cSuffix)
   aadd(aDefLib,'gttrm'+ cSuffix)
   aadd(aDefLib,'gtwin'+ cSuffix)
   aadd(aDefLib,'gtwvt'+ cSuffix)
   aadd(aDefLib,'hbodbc'+ cSuffix)
   aadd(aDefLib,'hbpgsql'+ cSuffix)
   aadd(aDefLib,'hblang'+ cSuffix)
   aadd(aDefLib,'hbmisc'+ cSuffix)
   aadd(aDefLib,'hbnf'+ cSuffix)
   aadd(aDefLib,'hbgt'+ cSuffix)
   aadd(aDefLib,'hbmysql'+ cSuffix)
   aadd(aDefLib,'hbmacro'+ cSuffix)
   aadd(aDefLib,'hbnulrdd'+ cSuffix)
   aadd(aDefLib,'hbpp'+ cSuffix)
   aadd(aDefLib,'hbrdd'+ cSuffix)
   aadd(aDefLib,'rddads'+ cSuffix)
   aadd(aDefLib,'hbrtl'+ cSuffix)
   aadd(aDefLib,'hbclipsm'+ cSuffix)
   aadd(aDefLib,'hbtip'+cSuffix)
   aadd(aDefLib,'hbw32'+cSuffix)
   aadd(aDefLib,'hbvm'+ cSuffix)
   aadd(aDefLib,'hbziparch'+ cSuffix)


   IF lGcc
      AEval( aLibs, { | x, y | cItem := x[1], IIF( Left( cItem, 3 ) == "lib", aLibs[ y, 1 ] := SubStr( cItem, 4 ), ) } )
   ENDIF

   FOR nCount := 1 TO LEN( aLibs )

      cItem := Lower( aLibs[ nCount, 1 ] )

      nPos := AScan( aDefLib, { | a | Lower( a ) == cItem } )
      IF nPos == 0
         AAdd( aReturnLibs, aLibs[ nCount, 1 ] )
      ENDIF

   NEXT

RETURN aReturnLibs

*-----------------------------
FUNCTION GetLibs( lGcc, cDir )
*-----------------------------

   LOCAL lLinux        := AT( 'linux', LOWER( OS() ) ) > 0
   LOCAL cEnv          := GETENV( "HB_LIB_INSTALL" )
   LOCAL aInstaledLibs := GetInstaledLibs( IIF( ! lLinux, IIF( ! lGcc, cDir + "\*.lib", cDir + "\*.a" ),  '/usr/lib/harbour/*.a' ), lGcc )
   LOCAL cExt := iif(lGcc,".a",".lib")

                            /* 1234567890123456789 */
   LOCAL aLibsDesc     := { { "Harbour hbmisc      lib - hbmisc" + cExt                         , 'hbmisc' + cExt },;
                            { "Harbour NanFor Lib  lib - hbnf" + cExt                           , 'hbnf' + cExt },;
                            { "Harbour GT Lib      lib - hbgt"+cExt                             , 'hbgt' + cExt },;
                            { "Harbour ZipArch     lib - hbziparch"+cExt                        , 'hbziparch' + cExt + iif( lLinux, ' stdc++.a z.a', ' ' ) },;
                            { "Harbour ole (old)   lib - hbole"+ cExt                           , 'hbole' + cExt + ' ole2' + cExt },;
                            { "Harbour MySQL       lib - hbmysql" + cExt                        , 'hbmysql' + cExt },;
                            { "Harbour PostgreSQL  lib - hbpgsql"+cExt                          , 'hbpgsql' + cExt },;
                            { "Harbour samples     lib - hbclipsm"+cExt                         , 'hbclipsm' + cExt }  }


   AEVAL( aInstaledLibs, { | x | AAdd( aLibsDesc, { padr("Harbour contrib",19)+" lib - " + padr(x,15), x } ) } )

RETURN aLibsDesc

*-----------------------------------------
FUNCTION DIR_MULTI( cFileMaskList, cAttr )
*-----------------------------------------

   LOCAL aList := listasarray2( cFileMaskList, "|" )
   AEVAL( aList, { | tmp, tmp1 | aList[ tmp1 ] := DIRECTORY( tmp, cAttr ) } )

RETURN ArrayAJoin( alist )

*----------------------------
FUNCTION ArrayAJoin( aArray )
*----------------------------

   LOCAL tmp
   LOCAL nLenArray := LEN( aArray )
   LOCAL nLen
   LOCAL nPos      := LEN( aArray[ 1 ] ) + 1

   nLen := 0

   FOR tmp := 1 TO nLenArray
      nLen += LEN( aArray[ tmp ] )
   NEXT

   ASIZE( aArray[ 1 ], nLen )

   FOR tmp := 2 TO nLenArray
      ACOPY( aArray[ tmp ], aArray[ 1 ],,, nPos )
      nPos += LEN( aArray[ tmp ] )
   NEXT
RETURN aArray[ 1 ]

*-----------------------------------------
FUNCTION ListAsArray2( cList, cDelimiter )
*-----------------------------------------

   LOCAL nPos
   LOCAL aList  := {}              // Define an empty array

   IF cDelimiter = NIL
      cDelimiter := ","
   ENDIF
   //
   DO WHILE ( nPos := AT( cDelimiter, cList ) ) != 0
      AADD( aList, ALLTRIM( SUBSTR( cList, 1, nPos - 1 ) ) )                    // Add a new element
      cList := SUBSTR( cList, nPos + 1 )

   ENDDO
   AADD( aList, ALLTRIM( cList ) )      // Add final element
   //
RETURN aList        // Return the array

*--------------------
FUNCTION CreateLink()
*--------------------

    LOCAL nHandle := FCreate("hbtemp.c")
    
    FWrite( nHandle, '#include "hbapi.h"' + HB_OsNewLine())
    FWrite( nHandle, 'extern HB_FUNC( HB_GT_CRS );' + HB_OsNewLine())
    FWrite( nHandle, 'void hb_lnk_ForceLink_build( void )' + HB_OsNewLine())
    FWrite( nHandle, '{' + HB_OsNewLine())
    FWrite( nHandle, '   HB_FUNCNAME( HB_GT_CRS )();' + HB_OsNewLine())
    FWrite( nHandle, '}' + HB_OsNewLine())
    FClose( nHandle )
    

RETURN NIL

*----------------------
STATIC FUNCTION ISWIN()
*-----------------------
RETURN "WINDOWS" $ Upper( OS() )

// EOF: HBMUTILS.PRG
