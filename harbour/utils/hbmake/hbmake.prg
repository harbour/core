/*
 * $Id$
 */
/*
 * Harbour Project source code:
 * hbmake.Prg Harbour make utility main file
 *
 * Copyright 2000,2001 Luiz Rafael Culik <culik@sl.conex.net>
 * www - http://www.harbour-project.org
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
#include "fileio.ch"
#include "common.ch"
#include "radios.ch"

#ifdef __HARBOUR__
#include "hbgetcmt.ch"
#define EOL hb_osnewline()
#define CRLF hb_osnewline()
#else
#define EOL chr(13)+chr(10)
#define hb_osnewline() chr(13)+chr(10)
#define CRLF hb_osnewline()
#include "hbclip.ch"
#endif
#xtranslate timetosec(<x>) => ((val(substr(<x>,1,2))*3600)+(val(substr(<x>,4,2))*60)+(val(substr(<x>,7,2))))

#ifdef __HARBOUR__
#define datediff(x,y) (x-y)
#else
#translate datediff(<x>,<y>) => (<x>-<y>)
#endif
/*
      Beginning Static Variables Table

      Default Values for core variables are set here
      New Core vars should only be added on this section
      */

STATIC lPrint        := .f.
STATIC nHandle
STATIC aDefines      := {}
STATIC aBuildOrder   := {}
STATIC aCommands     := {}
STATIC aMacros       := {}
STATIC aPrgs         := {}
STATIC aCs           := {}
STATIC aObjs         := {}
STATIC aObjsc        := {}
STATIC lEof          := .F.
STATIC aRes          := {}
STATIC nLinkHandle
STATIC cLinker       := "makefile.tmp"
STATIC cLinkcomm     := ''
STATIC lBcc          := .T.
STATIC lGcc          := .F.
STATIC lVcc          := .F.
STATIC lForce        := .F.
STATIC lLinux        := .F.
STATIC szProject     := ""
STATIC lLibrary      := .f.
STATIC lIgnoreErrors := .F.
STATIC lExtended     := .F.
STATIC lOs2          := .F.
STATIC lRecurse      := .F.
STATIC lEditMode     := .F.
STATIC aDir
Static aLangMessages := {}
Static cDefLang


FUNCTION main( cFile, p1, p2, p3, p4, p5, p6 )

    LOCAL nPos
    LOCAL aFile    := {}
    LOCAL aDef     := {}
    LOCAL cOs      := Os()
    LOCAL allParam
    LOCAL nLang    := GETUSERLANG()

    IF Pcount() == 0

        cDefLang       := IF( nLang == 1 , "PT" , IF( nLang == 2 ,"EN" , "ES" ))
        aLangMessages  := BuildLangArray( cDefLang )
        ShowHelp()
        RETURN NIL

    ENDIF

    SET DATE Ansi
    SET SCORE Off
    SET CENTURY ON

    DEFAULT p1 TO ""
    DEFAULT p2 TO ""
    DEFAULT p3 TO ""
    DEFAULT p4 TO ""
    DEFAULT p5 TO ""
    DEFAULT p6 TO ""

    /* Assing Default C Compiler upon The OS */

    IF At( "OS/2", cOs ) > 0

        lGcc   := .t.
        lLinux := .f.
        lBcc   := .f.

    ENDIF

    IF At( "LINUX", Upper( cOs ) ) > 0

        lGcc   := .t.
        lLinux := .t.
        lBcc   := .f.

    ENDIF

    Allparam := ConvertParams( @cFile, aFile, p1, p2, p3, p4, p5, p6 )

    if cDefLang == NIL

       cDefLang       := IF( nLang == 1 , "PT" , IF( nLang == 2 ,"EN" , "ES" ))

    endif

    aLangMessages  := BuildLangArray( cDefLang )

    IF Len( aFile ) > 1

        ? "File defined more than once"
        RETURN NIL

    ENDIF

    IF Len( aFile ) > 0

        cFile := aFile[ 1 ]

    ELSE

        cFile := ""

    ENDIF

    IF ( Empty( cFile ) .and. !lEditMode )

        ? "File not Found"
        RETURN Nil

    ENDIF

    /* We have at least one parameter . check if is an valid file name */

    IF Pcount() >= 1

        IF File( cFile )

            ProcessParameters( AllParam )

        ELSE

            IF !lEditMode

            ? "File not Found"
            RETURN Nil

            ENDIF

        ENDIF

    ENDIF

    IF lEditMode

        IF lLibrary

            crtlibmakfile( cFile )

        ELSE

            crtmakfile( cFile )

        ENDIF

        RETURN nil

    ENDIF

    CLS

    /* Make file are parsed here */

    ParseMakfi( cFile )

    IF lPrint

        PrintMacros()

    ENDIF

    IF lForce

        Compfiles()

    ELSE

        CompUpdatedfiles()

    ENDIF

    Outstd( cLinkComm )
    ! ( cLinkcomm )

RETURN nil

FUNCTION ParseMakfi( cFile )

    LOCAL nPos
    LOCAL cBuffer     := {}
    LOCAL cMacro      := "#BCC"
    LOCAL cDep        := "#DEPENDS"
    LOCAL cOpt        := "#OPTS"
    LOCAL cCom        := "#COMMANDS"
    LOCAL cBuild      := "#BUILD"
    LOCAL cTemp       := ""
    LOCAL cTemp1      := ''
    LOCAL aTemp       := {}
    LOCAL lMacrosec   := .f.
    LOCAL lBuildSec   := .f.
    LOCAL lComSec     := .f.
    LOCAL aTemp1      := {}
    LOCAL cCfg        := ""
    LOCAL lCfgFound   := .F.
    LOCAL aTempCFiles := {}
    LOCAL lLinux      := At( 'linux', Lower( Os() ) ) > 0

    nHandle := FT_FUSE( cFile )
    IF nHandle < 0
        RETURN nil
    ENDIF

    cBuffer := Trim( Substr( ReadLN( @lEof ), 1 ) )

    Aadd( aDefines, { "HMAKEDIR", GetMakeDir() } )

    IF lBcc

        Aadd( aDefines, { "MAKEDIR", GetBccDir() } )

    ELSEIF lGcc

        Aadd( aDefines, { "MAKEDIR", GetGccDir() } )

    ELSEIF lVcc

        Aadd( aDefines, { "MAKEDIR", GetVccDir() } )

    ENDIF

    WHILE !leof

        IF At( cMacro, cBuffer ) > 0

            lMacroSec := .T.
            lBuildSec := .f.
            lComSec   := .f.

        ELSEIF At( cBuild, cBuffer ) > 0

            lMacroSec := .f.
            lBuildSec := .T.
            lComSec   := .f.

        ELSEIF At( cCom, cBuffer ) > 0

            lBuildSec := .f.
            lComSec   := .t.
            lMacroSec := .f.

        ELSE

            ? "Invalid Make File"
            Fclose( nHandle )
            RETURN Nil

        ENDIF

        cTemp := Trim( Substr( ReadLN( @lEof ), 1 ) )

        IF At( "//", ctemp ) > 0

            WHILE At( "//", ctemp ) > 0

                ctemp := Strtran( ctemp, " //", "" )
                cTemp += Trim( Substr( ReadLN( @lEof ), 1 ) )

            ENDDO

            ctemp := Strtran( ctemp, " //", "" )

        ENDIF

        aTemp := Listasarray2( Alltrim( cTemp ), "=" )

        IF lmacrosec

            IF Alltrim( Left( ctemp, 7 ) ) <> '!ifndef' .and. Alltrim( Left( ctemp, 6 ) ) <> "!endif" .and. Alltrim( Left( ctemp, 7 ) ) <> '!iffile' .and. Alltrim( Left( ctemp, 7 ) ) <> '!stdout' .and. Alltrim( Left( ctemp, 6 ) ) <> '!ifdef'

                IF Len( atemp ) > 1

                    IF At( "$", atemp[ 2 ] ) > 0

                        IF lgcc .and. aTemp[ 1 ] = "CFLAG1" .or. lGcc .and. aTemp[ 1 ] = "CFLAG2"

                            Aadd( amacros, { aTemp[ 1 ], Strtran( Replacemacros( atemp[ 2 ] ), "\", "/" ) } )

                        ELSE

                            Aadd( amacros, { aTemp[ 1 ], Replacemacros( atemp[ 2 ] ) } )

                        ENDIF

                    ELSE

                        IF lgcc .and. aTemp[ 1 ] = "CFLAG1" .or. lGcc .and. aTemp[ 1 ] = "CFLAG2"

                            Aadd( aMacros, { aTemp[ 1 ], Strtran( atemp[ 2 ], "\", "/" ) } )

                        ELSE

                            Aadd( aMacros, { aTemp[ 1 ], atemp[ 2 ] } )

                        ENDIF

                    ENDIF

                ENDIF

                IF aTemp[ 1 ] == "PROJECT"

                    IF At( '.lib', atemp[ 2 ] ) > 0 .or. At( '.a', atemp[ 2 ] ) > 0

                        lLibrary := .t.

                    ENDIF

                ENDIF

                IF aTemp[ 1 ] == "OBJFILES"

                    aObjs := Listasarray2( replacemacros( atemp[ 2 ] ), " " )

                ENDIF

                IF aTemp[ 1 ] == "OBJCFILES"

                    aTemp1 := Listasarray2( replacemacros( atemp[ 2 ] ), " " )

                    IF Len( atemp1 ) == 1

                        IF !Empty( atemp[ 1 ] )

                            aObjsC := Listasarray2( replacemacros( atemp[ 2 ] ), " " )

                        ENDIF
                    ELSE
                        aObjsC := Listasarray2( replacemacros( atemp[ 2 ] ), " " )

                    ENDIF

                ENDIF

                IF aTemp[ 1 ] == "PRGFILES"

                    aPrgs     := Listasarray2( replacemacros( atemp[ 2 ] ), " " )
                    lExtended := .T.
                    lCfgFound := findHarbourcfg( @cCfg )

                ENDIF

                IF aTemp[ 1 ] == "PRGFILE"

                    aPrgs := Listasarray2( replacemacros( atemp[ 2 ] ), " " )

                ENDIF

                IF atemp[ 1 ] == "CFILES"

                    IF lExtended

                        aTempCFiles := Listasarray2( replacemacros( atemp[ 2 ] ), " " )

                        IF ( Len( aTempCFiles ) == 1 )

                            IF !Empty( aTempCFiles[ 1 ] )

                                aCs := Listasarray2( replacemacros( atemp[ 2 ] ), " " )

                            ENDIF

                        ELSE

                            aCs := Listasarray2( replacemacros( atemp[ 2 ] ), " " )

                        ENDIF

                    ELSE

                        aCs := Listasarray2( replacemacros( atemp[ 2 ] ), " " )

                    ENDIF

                ENDIF

                IF atemp[ 1 ] == "RESFILES"

                    aRes := Listasarray2( replacemacros( atemp[ 2 ] ), " " )

                ENDIF

            ELSE

                IF At( '!ifndef', cTemp ) > 0

                    Checkdefine( cTemp )

                ELSEIF At( '!ifdef', ctemp ) > 0

                    CheckifDef( cTemp )

                ELSEIF At( '!iffile', cTemp ) > 0

                    checkiffile( cTemp )

                ELSEIF At( '!stdout', cTemp ) > 0

                    checkstdout( cTemp )

                ENDIF

            ENDIF

        ENDIF

        IF lbuildSec

            szProject   := cTemp
            aBuildOrder := Listasarray2( ctemp, ":" )

            IF !llibrary

                SetBuild()

            ELSE

                SetLibBuild()

            ENDIF

        ENDIF

        IF lComSec

            IF !Empty( ctemp )

                Setcommands( cTemp )

            ENDIF

        ENDIF
        IF cTemp = "#BUILD"

            cBuffer := cTEmp

        ELSEIF cTemp == "#COMMANDS"

            cbuffer := ctemp

        ENDIF

    ENDDO

    IF lExtended .and. !lCfgFound

        IF lBcc

            BuildBorCfgFile()

        ELSEIF lVcc

            BuildMSCCfgFile()

        ELSEIF lGcc .and. !lLinux

            BuildGccCfgFile()

        ELSEIF lGcc .and. lLinux

            BuildGccCfgFilel()

        ENDIF

    ENDIF

RETURN nil

FUNCTION Checkdefine( cTemp )

    LOCAL cDef
    LOCAL nPos
    LOCAL cRead
    LOCAL aSet     := {}
    LOCAL nMakePos

    IF cTemp == "!endif"
        RETURN nil
    ENDIF

    cTemp := Trim( Substr( ReadLN( @lEof ), 1 ) )
    cTemp := Strtran( cTemp, "!ifndef ", "" )
    cTemp := Strtran( cTemp, "\..", "" )
    cTemp := Strtran( cTemp, "/..", "" )

    IF At( "\..", cTemp ) > 0

        cTemp := Substr( cTemp, 1, At( "\..", cTemp ) - 1 )

    ELSEIF At( "/..", cTemp ) > 0

        cTemp := Substr( cTemp, 1, At( "/..", cTemp ) - 1 )

    ENDIF

    aSet := Listasarray2( ctemp, "=" )
    nPos := Ascan( adefines, { | x | x[ 1 ] == aset[ 1 ] } )

    IF nPos = 0

        cRead    := Alltrim( Strtran( aset[ 2 ], "$(", "" ) )
        cRead    := Strtran( cRead, ")", "" )
        nMakePos := Ascan( aDefines, { | x | x[ 1 ] == cRead } )

        IF nMakePos > 0

            Aadd( aDefines, { aset[ 1 ], aDefines[ nMakePos, 2 ] } )
            Aadd( amacros, { aset[ 1 ], aDefines[ nMakePos, 2 ] } )

        ENDIF

    ENDIF

RETURN nil

FUNCTION Setcommands( cTemP )

    LOCAL cRead        := Alltrim( readln( @leof ) )
    LOCAL nPos
    LOCAL nCount       := 0
    LOCAL aTempMacros  := {}
    LOCAL aLocalMacros := {}

    aTempMacros := Listasarray2( cREad, " " )

    Aeval( aTempMacros, { | xMacro | If( At( "$", xMacro ) > 0, ;
                          IF( At( ";", xMacro ) > 0, ( aLocalMacros := Listasarray2( xMacro, ";" ), ;
                          Aeval( aLocalMacros, { | x | Findmacro( x, @cRead ) } ) ), ;
                          Findmacro( xMacro, @cRead ) ), ) } )
    Aadd( aCommands, { cTemp, cRead } )

RETURN nil

FUNCTION Findmacro( cMacro, cRead )

    LOCAL nPos
    LOCAL cTemp
    LOCAL aLocalMacros := {}

    cMacro := Substr( cMacro, 1, At( ")", cMacro ) )

    IF At( "-", cMacro ) > 0

        cMacro := Substr( cMacro, 3 )

    ENDIF

    IF At( ";", cMacro ) > 0

        cMacro := Substr( cMacro, At( ";", cMacro ) + 1 )

    ENDIF

    nPos := Ascan( aMacros, { | x | "$(" + Alltrim( x[ 1 ] ) + ")" == cMacro } )

    IF nPos = 0

        cTemp := Strtran( cmacro, "$(", "" )
        cTemp := Strtran( ctemp, ")", "" )

        IF !Empty( cTemp )

            cRead := Alltrim( Strtran( cRead, cmacro, Gete( cTemp ) ) )

        ENDIF

    ELSE

        cRead := Alltrim( Strtran( cRead, cmacro, amacros[ npos, 2 ] ) )

    ENDIF

RETURN cRead

FUNCTION Replacemacros( cMacros )

    LOCAL nPos
    LOCAL nCount       := 0
    LOCAL aTempMacros  := {}
    LOCAL aLocalMacros := {}

    aTempMacros := Listasarray2( cMacros, " " )
    Aeval( aTempMacros, { | xMacro | If( At( "$", xMacro ) > 0, ;
                          IF( At( ";", xMacro ) > 0, ( aLocalMacros := Listasarray2( xMacro, ";" ), ;
                          Aeval( aLocalMacros, { | x | Findmacro( x, @cMacros ) } ) ), ;
                          Findmacro( xMacro, @cMacros ) ), ) } )

RETURN cmacros

FUNCTION setBuild()

    LOCAL cRead
    LOCAL nPos
    LOCAL aMacro
    LOCAL aTemp
    LOCAL nCount
    LOCAL cCurrentRead := ''

    cRead     := Alltrim( readln( @leof ) )
    szProject := cRead
    amacro    := Listasarray2( cRead, ":" )

    IF Len( amacro ) > 1

        aTemp := Listasarray2( amacro[ 2 ], " " )
        Aeval( atemp, { | xItem | Aadd( aBuildOrder, xItem ) } )

    ENDIF

    Aadd( aBuildOrder, amacro[ 1 ] )
    cRead := Strtran( cRead, "@&&!", "" )

    amacro := Listasarray2( cRead, '\' )
    Aeval( amacro, { | xMacro | If( At( "$", xmacro ) > 0, Findmacro( xMacro, @cRead ), ) } )

    IF !lLinux

        cLinkcomm   := cRead + "  @" + cLinker
        nLinkHandle := Fcreate( clinker )

    ELSE

        cLinkComm := cRead + " "

    ENDIF

    FOR nPos := 1 TO 7

        cRead        := Alltrim( readln( @leof ) )
        cCurrentRead := cRead
        amacro       := Listasarray2( cRead, " " )

        FOR ncount := 1 TO Len( amacro )

            IF At( "$", amacro[ nCount ] ) > 0

                Findmacro( amacro[ nCount ], @cRead )

                IF At( '$(PROJECT)', cCurrentRead ) > 0

                    IF !lGcc

                        IF !lLinux

                            Fwrite( nLinkhandle, cRead + CRLF )

                        ENDIF

                    ELSEIF lGcc .and. lLinux

                        cLinkComm += "-o " + cRead + " "

                    ELSEIF lGcc .and. !lLinux .and. At( '.exe', cread ) > 0

                        Fwrite( nLinkhandle, "-o " + cRead + CRLF )

                    ENDIF

                ELSE

                    IF !lLinux

                        Fwrite( nLinkhandle, cRead + CRLF )

                    ELSE

                        cLinkComm += cRead + " "

                    ENDIF

                ENDIF

            ENDIF

        NEXT

    NEXT

    IF !lLinux

        Fclose( nLinkhandle )

    ENDIF

RETURN nil

FUNCTION Compfiles()

    LOCAL cComm
    LOCAL cOld
    LOCAL nPos
    LOCAL nCount
    LOCAL nFiles
    LOCAL cErrText := ""
    LOCAL aOrder   := Listasarray2( aBuildOrder[ 2 ], " " )
    LOCAL lEnd     := .f.
    LOCAL xItem
    LOCAL lLinux   := At( 'linux', Lower( Os() ) ) > 0
    Local nFile := 1
    Local aGauge := GaugeNew( 5, 5, 7,40 , "W/B", "W+/B" ,'²')

    @ 4, 5 Say "Compiling :"
    FOR nCount := 1 TO Len( aOrder )

        IF !lExtended

            IF aOrder[ nCount ] == "$(CFILES)"

                nPos := Ascan( aCommands, { | x | x[ 1 ] == ".prg.c:" } )

                IF nPos > 0

                    cComm := aCommands[ nPos, 2 ]
                    cOld  := cComm

                ELSE

                    nPos := Ascan( aCommands, { | x | x[ 1 ] == ".PRG.C:" } )

                    IF nPos > 0

                        cComm := aCommands[ nPos, 2 ]
                        cOld  := cComm

                    ENDIF

                ENDIF

                FOR nFiles := 1 TO Len( aPrgs )

                    xItem := Substr( aPrgs[ nFiles ], Rat( If( lgcc, '/', '\' ), ;
                                     aPrgs[ nFiles ] ) + 1 )
                    nPos := Ascan( aCs, { | x | x := Substr( x, Rat( If( lgcc, '/', '\' ), x ) + 1 ), ;
                        Left( x, At( ".", x ) ) == Left( xItem, At( ".", xItem ) ) } )

                    IF nPos > 0

                        cComm := Strtran( cComm, "o$*", "o" + aCs[ nPos ] )
                        cComm := Strtran( cComm, "$**", aPrgs[ nFiles ] )

                        cComm += " > Test.out"
                        Outstd( cComm )
                        Outstd( hb_osnewline() )
                        ! ( cComm )
                        cErrText := Memoread( 'Test.out' )
                        lEnd     := 'C2006' $ cErrText .or. 'No code generated' $ cErrText

                        IF !lIgnoreErrors .and. lEnd

                            QUIT

                        ELSE

                            Ferase( 'Test.out' )

                        ENDIF

                        cComm := cold

                    ENDIF

                NEXT

            ENDIF

            IF aOrder[ nCount ] == "$(OBJFILES)"

                IF lGcc

                    nPos := Ascan( aCommands, { | x | x[ 1 ] == ".c.o:" } )

                ELSE

                    nPos := Ascan( aCommands, { | x | x[ 1 ] == ".c.obj:" } )

                ENDIF

                IF nPos > 0

                    cComm := aCommands[ nPos, 2 ]
                    cOld  := ccomm

                ELSE

                    IF lGcc

                        nPos := Ascan( aCommands, { | x | x[ 1 ] == ".C.O:" } )

                        IF nPos > 0

                            cComm := aCommands[ nPos, 2 ]
                            cOld  := cComm

                        ENDIF

                    ELSE

                        nPos := Ascan( aCommands, { | x | x[ 1 ] == ".C.OBJ:" } )

                        IF nPos > 0

                            cComm := aCommands[ nPos, 2 ]
                            cOld  := cComm

                        ENDIF

                    ENDIF

                ENDIF

                FOR nFiles := 1 TO Len( aCs )

                    xItem := Substr( aCs[ nFiles ], Rat( If( lgcc, '/', '\' ), ;
                                     aCs[ nFiles ] ) + 1 )
                    nPos := Ascan( aObjs, { | x | x := Substr( x, Rat( If( lgcc, '/', '\' ), x ) + 1 ), ;
                        Left( x, At( ".", x ) ) == Left( xItem, At( ".", xItem ) ) } )

                    IF nPos > 0

                        IF llinux

                            cComm := Strtran( cComm, "o$*", "o" + aObjs[ nPos ] )

                        ELSE

                            cComm := Strtran( cComm, "o$*", "o" + Strtran( aObjs[ nPos ], '/', '\' ) )

                        ENDIF

                        cComm := Strtran( cComm, "$**", acs[ nFiles ] )
                        Outstd( " " )

                        Outstd( cComm )
                        Outstd( hb_osnewline() )
                        ! ( cComm )
                        ccomm := cold

                    ENDIF

                NEXT

            ENDIF

        ELSE /****** Extended mode *****/

            IF aOrder[ nCount ] == "$(CFILES)"

                IF lGcc

                    nPos := Ascan( aCommands, { | x | x[ 1 ] == ".c.o:" } )

                ELSE

                    nPos := Ascan( aCommands, { | x | x[ 1 ] == ".c.obj:" } )

                ENDIF

                IF nPos > 0

                    cComm := aCommands[ nPos, 2 ]
                    cOld  := cComm

                ELSE
                    nPos := Ascan( aCommands, { | x | x[ 1 ] == ".C.OBJ:" } )

                    IF nPos > 0

                        cComm := aCommands[ nPos, 2 ]
                        cOld  := cComm

                    ENDIF

                ENDIF

                IF Len( acs ) > 0
                GaugeDisplay( aGauge )
                nFile:=1


                    FOR nFiles := 1 TO Len( acs )
                      @ 4,16 Say space(50)

                        xItem := Substr( acs[ nFiles ], Rat( If( lgcc, '/', '\' ), ;
                                         acs[ nFiles ] ) + 1 )
                        nPos := Ascan( aObjsc, { | x | x := Substr( x, Rat( If( lgcc, '/', '\' ), x ) + 1 ), ;
                            Left( x, At( ".", x ) ) == Left( xitem, At( ".", xitem ) ) } )

                        IF nPos > 0

                            IF llinux

                                cComm := Strtran( cComm, "o$*", "o" + aobjsc[ nPos ] )

                            ELSE

                                cComm := Strtran( cComm, "o$*", "o" + Strtran( aobjsc[ nPos ], '/', '\' ) )

                            ENDIF

                            cComm := Strtran( cComm, "$**", acs[ nFiles ] )
                            cComm += " > Test.out"
                        @ 4,16 Say acs[nFiles]
                        GaugeUpdate(aGauge,nFile/Len( aprgs ))

//                            Outstd( cComm )
//                            Outstd( hb_osnewline() )
                            ! ( cComm )
                            nFile++
                            cErrText := Memoread( 'Test.out' )
                            lEnd     := 'Error' $ cErrText

                            IF !lIgnoreErrors .and. lEnd
                                if(at("LINUX",upper(OS()))>0,__run("mcedit Test.out"),__run("Notepad Test.out"))

                                QUIT

                            ELSE

                                Ferase( 'Test.out' )

                            ENDIF

                            cComm := cold

                        ENDIF

                    NEXT

                ENDIF

            ENDIF

            IF aOrder[ nCount ] == "$(OBJFILES)"

                IF lGcc

                    nPos := Ascan( aCommands, { | x | x[ 1 ] == ".prg.o:" } )

                ELSE

                    nPos := Ascan( aCommands, { | x | x[ 1 ] == ".prg.obj:" } )

                ENDIF

                IF nPos > 0

                    cComm := aCommands[ nPos, 2 ]
                    cOld  := ccomm

                ELSE

                    IF lGcc

                        nPos := Ascan( aCommands, { | x | x[ 1 ] == ".PRG.O:" } )

                    ELSE

                        nPos := Ascan( aCommands, { | x | x[ 1 ] == ".PRG.OBJ:" } )

                    ENDIF

                ENDIF
                GaugeDisplay( aGauge )
                nFile:=1

                FOR nFiles := 1 TO Len( aprgs )

                    xItem := Substr( aprgs[ nFiles ], Rat( If( lgcc, '/', '\' ), ;
                                     aprgs[ nFiles ] ) + 1 )
                    nPos := Ascan( aobjs, { | x | x := Substr( x, Rat( If( lgcc, '/', '\' ), x ) + 1 ), ;
                        Left( x, At( ".", x ) ) == Left( xItem, At( ".", xitem ) ) } )

                    IF nPos > 0

                        IF llinux

                            cComm := Strtran( cComm, "o$*", "o" + aObjs[ nPos ] )

                        ELSE

                            cComm := Strtran( cComm, "o$*", "o" + Strtran( aObjs[ nPos ], '/', '\' ) )

                        ENDIF

                        cComm := Strtran( cComm, "$**", aprgs[ nFiles ] )
                        cComm += " > Test.out"
//                        Outstd( " " )

//                        Outstd( cComm )
//                        Outstd( hb_osnewline() )
                        @ 4,16 Say aprgs[ nFiles ]
                        GaugeUpdate(aGauge,nFile/Len( aprgs ))
//                        Outstd( hb_osnewline() )
    nFile++

                        ! ( cComm )
                        cErrText := Memoread( 'Test.out' )
                        lEnd     := 'C2006' $ cErrText .or. 'No code generated' $ cErrText

                        IF !lIgnoreErrors .and. lEnd
                                if(at("LINUX",upper(OS()))>0,__run("mcedit Test.out"),__run("Notepad Test.out"))
                            QUIT

                        ELSE

                            Ferase( 'Test.out' )

                        ENDIF

                        ccomm := cold

                    ENDIF

                NEXT

            ENDIF

        ENDIF
        IF aOrder[ nCount ] == "$(RESDEPEN)"

            nPos := Ascan( aCommands, { | x | x[ 1 ] == ".rc.res:" } )

            IF nPos > 0

                cComm := aCommands[ nPos, 2 ]
                cold  := ccomm 
            ENDIF

            FOR nFiles := 1 TO Len( aRes )

                IF !Empty( ares[ nFiles ] )

                    cComm := Strtran( cComm, "$<", aRes[ nFiles ] )
                    Outstd( " " )
                    ! ( cComm )

                ENDIF

                  ccomm := cold

            NEXT

        ENDIF

    NEXT

RETURN nil

FUNCTION GetParaDefines( cTemp )

    LOCAL nPos
    LOCAL cRead
    LOCAL aSet     := {}
    LOCAL nMakePos

    IF At( "\..", cTemp ) > 0

        cTemp := Substr( cTemp, 1, At( "\..", cTemp ) - 1 )

    ELSEIF At( "/..", cTemp ) > 0

        cTemp := Substr( cTemp, 1, At( "/..", cTemp ) - 1 )

    ENDIF

    aSet := Listasarray2( ctemp, "=" )
    nPos := Ascan( adefines, { | x | x[ 1 ] == aset[ 1 ] } )

    IF nPos == 0

        cRead    := Alltrim( Strtran( aset[ 2 ], "$(", "" ) )
        cRead    := Strtran( cRead, ")", "" )
        nMakePos := Ascan( aDefines, { | x | x[ 1 ] == cRead } )

        IF nMakePos = 0

            aset[ 2 ] := Strtran( aset[ 2 ], ",", " " )
            Aadd( aDefines, { aset[ 1 ], aset[ 2 ] } )
            Aadd( amacros, { aset[ 1 ], aset[ 2 ] } )

        ENDIF

    ENDIF

RETURN NIL

FUNCTION PrintMacros()

    LOCAL nPos

    Outstd( "HBMAKE Version ", Version(), "CopyRight (c) 2000-2002 The Harbour Project" + CRLF )
    Outstd( "" + CRLF )
    Outstd( "Macros:" + CRLF )
    Aeval( aMacros, { | xItem | Outstd( "     " + xItem[ 1 ] + " = " + xItem[ 2 ] + CRLF ) } )
    Outstd( "Implicit Rules:" + CRLF )
    Aeval( aCommands, { | xItem | Outstd( "     " + xItem[ 1 ] + hb_osnewline() + "        " + xItem[ 2 ] + CRLF ) } )
    Outstd( "" + CRLF )
    Outstd( "Targets:" )
    Outstd( "    " + szProject + ":" + CRLF )
    Outstd( "        " + "Flags :" + CRLF )
    Outstd( "        " + "Dependents :" )
    Aeval( acs, { | xItem | Outstd( xitem + " " ) } )
    Aeval( aobjs, { | xItem | Outstd( xitem + " " ) } )
    Outstd( " " + CRLF )
    Outstd( "        commands:" + aBuildOrder[ Len( aBuildOrder ) ] )
    Outstd( " " + CRLF )
    Outstd( " " + CRLF )
    Outstd( " " + CRLF )

RETURN Nil

FUNC crtmakfile( cFile )

    LOCAL ain          := {}
    LOCAL aOut         := {}
    LOCAL aOutc        := {}
    LOCAL aSrc         := Directory( "*.prg" )
    LOCAL nLenaSrc     := Len( asrc )
    LOCAL nLenaOut
    LOCAL lFwh         := .f.
    LOCAL lCw          := .f.
    LOCAL lMiniGui     := .f.
    LOCAL lRddAds      := .f.
    LOCAL cOs          := "Win32"
    LOCAL cCompiler    := "BCC"
    LOCAL cfwhpath     := Space( 40 )
    LOCAL ccwpath      := Space( 40 )
    LOCAL cMiniPath    := Space( 40 )
    LOCAL cObjDir      := "obj" + Space( 20 )
    LOCAL lAutomemvar  := .f.
    LOCAL lvarismemvar := .f.
    LOCAL ldebug       := .f.
    LOCAL lSupressline := .f.
    LOCAL nPos
    LOCAL cDefHarOpts  := ""

    LOCAL lCompMod       := .f.
    LOCAL x
    LOCAL lGenppo        := .f.
    LOCAL getlist        := {}
    LOCAL cTopFile       := ""
    LOCAL cDefBccLibs    := "lang.lib vm.lib rtl.lib rdd.lib macro.lib pp.lib dbfntx.lib dbfcdx.lib common.lib gtwin.lib"
    LOCAL cDefGccLibs    := "-lvm -lrtl -lgtdos -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon"
    LOCAL cgcclibsos2    := "-lvm -lrtl -lgtos2 -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon"
    LOCAL cDeflibGccLibs := "-lvm -lrtl -lgtsln -llang -lrdd -lrtl -lvm -lmacro -lpp -ldbfntx -ldbfcdx -lcommon -lslang -lm"
    LOCAL cLibs          := ""
    LOCAL citem          := ""
    LOCAL cExt           := ""
    LOCAL cDrive         := ""
    LOCAL cPath          := ""
    LOCAL cTest          := ""
    LOCAL cUserdef       := Space( 40 )
    LOCAL cUserInclude   := Space( 40 )
    LOCAL aLibs
    LOCAL aLibsin        := {}
    LOCAL aLibsout       := {}
    LOCAL lExternalLib   := .f.
    LOCAL cOldLib        := ""
    LOCAL cHtmlLib       := ""
    LOCAL lLinux         := At( 'linux', Lower( Os() ) ) > 0
    LOCAL nWriteFiles    := 0
    Local cResName       := Space(50)

    nLinkHandle := Fcreate( cFile )
    WriteMakeFileHeader()
    CLS
    Setcolor( 'w/b+,b+/w,w+/b,w/b+,w/b,w+/b' )
    @  0,  0, Maxrow(), Maxcol() BOX( Chr( 201 ) + Chr( 205 ) + Chr( 187 ) + Chr( 186 ) + Chr( 188 ) + Chr( 205 ) + Chr( 200 ) + Chr( 186 ) + Space( 1 ) )
    ATTENTION( aLangMessages[ 27 ] , 0 )
    @  1,  1 SAY aLangMessages[ 28 ] 
    @  1, 12 GET cos radio { "Win32", "OS/2", "Linux" }   VALID !Empty( cos )              
    @  1, 23 SAY aLangMessages[ 29 ] 
    @  1, 40 GET cCompiler radio { "BCC", "MSVC", "GCC" } VALID !Empty( cCompiler )        
    @  1, 48 SAY aLangMessages[ 30 ] 
    @  1, 64 GET lFwh checkbox caption "Use FWH"          WHEN Cos == "Win32"              
    @  2, 64 GET lcw checkbox caption "Use C4W"           WHEN Cos == "Win32"              
    @  3, 64 GET lRddads checkbox caption "Use RddAds"    WHEN Cos == "Win32" .OR. Cos == "Linux"
    @  4, 64 Get lMiniGui checkbox caption "Use Minigui"  WHEN Cos == "Win32"

    READ

    IF lFwh

        @  5,  1 SAY "FWH path" GET cfwhpath Pict "@s20"

    ELSEIF lCw

        @  5,  1 SAY "C4H path" GET ccwpath  Pict "@s20"

    ELSEIF lMiniGui

        @  5,  1 SAY "MinuGui path" GET  cMiniPath Pict "@s20"


    ENDIF

    @  5, 40 SAY "Obj Files Dir" GET cObjDir PICT "@s15" 
    ATTENTION( aLangMessages[ 31 ] , 6 )

    @  7,  1 GET lautomemvar checkbox caption aLangMessages[ 32 ]
    @  7, 40 GET lvarismemvar checkbox caption aLangMessages[ 33 ] 
    @  8,  1 GET lDebug checkbox caption  aLangMessages[ 34 ]
    @  8, 40 GET lSupressline checkbox caption aLangMessages[ 35 ] 
    @  9,  1 GET lGenppo checkbox caption aLangMessages[ 36 ] 
    @  9, 40 GET lCompMod checkbox caption aLangMessages[ 37 ] 
    @ 10,  1 SAY aLangMessages[ 38 ]   GET cUserDef     PICT "@s15"
    @ 10, 40 SAY aLangMessages[ 39 ]  GET cUserInclude PICT "@s15"
    @ 11,  1 GET lExternalLib checkbox caption aLangMessages[ 40 ] 
    @ 12,  1 Say "Resource file Name" Get CResName 
    READ

    IF !Empty( cUserDef )

        cDefHarOpts += " -D" + Alltrim( cUserDef ) + " "

    ENDIF

    IF !Empty( cUserInclude )

        cDefHarOpts += " -I" + Alltrim( cUserInclude ) + " "

    ENDIF

    lBcc    := IF( At( "BCC", cCompiler ) > 0, .t., .f. )
    lVcc    := IF( At( "MSVC", cCompiler ) > 0, .t., .f. )
    lGcc    := IF( At( "GCC", cCompiler ) > 0, .t., .f. )
    cObjDir := Alltrim( cObjDir )

    IF !Empty( cobjDir )

        IF Dirchange( cobjDir ) != 0

            Makedir( cobjDir )

        ELSE

            Dirchange( '..' )

        ENDIF

    ENDIF

    amacros := GetSourceDirMacros( lGcc, cos )

    IF lLinux

        cObjDir := Alltrim( cObjDir )

        IF !Empty( cObjDir )

            cObjDir += '/'

        ENDIF

        cTest := cObjDir

    ELSE
        cObjDir := Alltrim( cObjDir )

        IF !Empty( cObjDir )

            cObjDir += '\'

        ENDIF

        cTest := Upper( cObjDir ) + '\'

    ENDIF

    Aeval( amacros, { | x, y | cItem := Substr( x[ 2 ], 1, Len( x[ 2 ] ) ), If( At( citem, cTest ) > 0, ( amacros[ y, 1 ] := 'OBJ', amacros[ y, 2 ] := cObjDir ), ) } )

    IF lAutomemvar

        cDefHarOpts += " -a "

    ENDIF

    IF lvarismemvar

        cDefHarOpts += " -v "

    ENDIF

    IF ldebug

        cDefHarOpts    += " -b "
        cDefBccLibs    += " debug.lib "
        cDefGccLibs    += " -ldebug "
        cgcclibsos2    += " -ldebug "
        cDeflibGccLibs += " -ldebug "

    ENDIF

    IF lSupressline

        cDefHarOpts += " -l "

    ENDIF

    IF lGenppo

        cDefHarOpts += " -p "

    ENDIF

    IF lCompmod

        cDefHarOpts += " -m "

    ENDIF

    IF lBcc

        Aadd( aCommands, { ".cpp.obj:", "$(BCB)\BIN\bcc32 $(CFLAG1) $(CFLAG2) -o$* $*" } )
        Aadd( aCommands, { ".c.obj:", "$(BCB)\BIN\bcc32 -I$(BHC)\include $(CFLAG1) $(CFLAG2) -o$* $**" } )

        IF lExtended

            Aadd( aCommands, { ".prg.obj:", "$(BHC)\bin\harbour -n -go -I$(BHC)\include $(HARBOURFLAGS)" + if(lFwh," -I$(FWH)\include" ,if(lMinigui," -I$(MINIGUI)" , "" )) +" -o$* $**" } )

        ELSE

            Aadd( aCommands, { ".prg.c:", "$(BHC)\bin\harbour -n -I$(BHC)\include $(HARBOURFLAGS)" + if(lFwh," -I$(FWH)\include" ,if(lMinigui," -I$(MINIGUI)" , "" )) +" -o$* $**" } )

        ENDIF

        Aadd( aCommands, { ".rc.res:", "$(BCB)\BIN\brcc32 $(RFLAGS) $<" } )

    ELSEIF lGcc

        IF At( "linux", Getenv( "HB_ARCHITECTURE" ) ) > 0 .or. cOs == "Linux"

            Aadd( aCommands, { ".cpp.o:", "gcc $(CFLAG1) $(CFLAG2) -o$* $*" } )
            Aadd( aCommands, { ".c.o:", "gcc -I$(HB_INC_INSTALL) $(CFLAG1) $(CFLAG2) -I. -g -o$* $**" } )

            IF lExtended

                Aadd( aCommands, { ".prg.o:", "harbour -n  -go -I$(HB_INC_INSTALL) -I.  -o$* $**" } )

            ELSE

                Aadd( aCommands, { ".prg.c:", "harbour -n -I$(HB_INC_INSTALL) -I.  -o$* $**" } )

            ENDIF

        ELSE

            Aadd( aCommands, { ".cpp.o:", "$(BCB)\bin\gcc $(CFLAG1) $(CFLAG2) -o$* $*" } )
            Aadd( aCommands, { ".c.o:", "$(BCB)\bin\gcc -I$(BHC)/include $(CFLAG1) $(CFLAG2) -I. -o$* $**" } )

            IF lExtended

                Aadd( aCommands, { ".prg.o:", "$(BHC)\bin\harbour -n -go -I$(BHC)/include $(HARBOURFLAGS)  -o$* $**" } )

            ELSE

                Aadd( aCommands, { ".prg.c:", "$(BHC)\bin\harbour -n -I$(BHC)/include $(HARBOURFLAGS)  -o$* $**" } )

            ENDIF

        ENDIF

    ELSEIF lVcc

        Aadd( aCommands, { ".cpp.obj:", "$(BCB)\bin\cl $(CFLAG1) $(CFLAG2) -Fo$* $*" } )
        Aadd( aCommands, { ".c.obj:", "$(BCB)\bin\cl -I$(BHC)\include $(CFLAG1) $(CFLAG2) -Fo$* $**" } )

        IF lExtended

            Aadd( aCommands, { ".prg.obj:", "$(BHC)\bin\harbour -n -I$(BHC)\include $(HARBOURFLAGS) -go  -I$(C4W)\include -o$* $**" } )

        ELSE

            Aadd( aCommands, { ".prg.c:", "$(BHC)\bin\harbour -n -I$(BHC)\include $(HARBOURFLAGS) -I$(C4W)\include -o$* $**" } )

        ENDIF

        Aadd( aCommands, { ".rc.res:", "$(BCB)\rc $(RFLAGS) $<" } )

    ENDIF

    attention( aLangMessages[ 41 ] , 22 )

    IF !lRecurse

        ain      := GetSourceFiles( .f., lGcc, cOs )
        nLenaSrc := Len( ain )

    ELSE

        ain      := GetSourceFiles(, lGcc, cOs )
        nLenaSrc := Len( asrc )

    ENDIF

    aOut := Aclone( aIn )
    pickarry( 11, 15, 20, 64, aIn, aOut )
    nLenaOut := Len( aOut )
    Aeval( aout, { | x, y | aout[ y ] := Trim( Substr( aOut[ y ], 1, At( ' ', aout[ y ] ) ) ) } )
    aOut := Asort( aOut )

    IF Len( aOut ) == 1

        cTopFile := aOut[ 1 ]

    ELSE

        attention( 'Select the TOP MODULE of your executable', 22 )

        IF !lrecurse

            cTopFile := pickfile( "*.prg" )

        ELSE

            cTopFile := pickafile( ain )

        ENDIF

    ENDIF

    IF lExternalLib

        aLibs := Getlibs( lgcc, GetMakeDir() + '\lib' )
        attention( 'Spacebar to select, Enter to continue process', 22 )
        Aeval( aLibs, { | x | Aadd( aLibsin, x[ 1 ] ) } )
        Aeval( aLibs, { | x | Aadd( aLibsout, x[ 2 ] ) } )
        pickarry( 11, 15, 20, 64, aLibsIn, aLibsOut )

    ENDIF

    Aeval( aout, { | xItem | If( At( '.c', xItem ) > 0 .or. At( '.C', xItem ) > 0, Aadd( aoutc, xitem ), ) } )
    Aeval( aoutc, { | x, z | citem := x, z := Ascan( aout, { | t | t = citem } ), If( z > 0, Asize( Adel( aout, z ), Len( aout ) - 1 ), ) } )

    aOut  := Asort( aOut )
    aPrgs := Aclone( aout )

    aObjs := Aclone( aout )
    x     := Ascan( aobjs, { | x | Lower( x ) == Lower( cTopFile ) } )

    IF x > 0

        Adel( aobjs, x )
        Asize( aobjs, Len( aobjs ) - 1 )
        Asize( aobjs, Len( aobjs ) + 1 )
        Ains( aobjs, 1 )
        aobjs[ 1 ] := cTopFile

    ENDIF

    x := Ascan( aPrgs, { | x | Lower( x ) == Lower( cTopFile ) } )

    IF x > 0

        Adel( aPrgs, x )
        Asize( aPrgs, Len( aPrgs ) - 1 )
        Asize( aPrgs, Len( aPrgs ) + 1 )
        Ains( aPrgs, 1 )
        aPrgs[ 1 ] := cTopFile

    ENDIF

    Aeval( aobjs, { | xItem, x | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cext := Substr( cExt, 2 ), If( !lGcc, aObjs[ x ] := cObjDir + cTest + "." + exte( cExt, 2 ), aObjs[ x ] := cObjDir + cTest + "." + exte( cExt, 3 ) ) } )
    aCs := Aclone( aoutc )

    IF !lextended

        Aeval( aOutc, { | xItem, x | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cext := Substr( cExt, 2 ), If( !lGcc, Aadd( aObjs, cObjDir + cTest + "." + exten( cExt, 2 ) ), Aadd( aObjs, cObjDir + cTest + "." + exten( cExt, 1 ) ) ) } )
        Aeval( aout, { | xItem | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cExt := Substr( cExt, 2 ), Aadd( aCs, cObjDir + cTest + "." + exte( cExt, 1 ) ) } )

    ELSE
        aObjsc := Aclone( aoutc )
        Aeval( aoutc, { | xItem, x | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cext := Substr( cExt, 2 ), If( !lGcc, aObjsc[ x ] := If( !Empty( cObjDir ), cObjDir, '' ) + cTest + "." + exten( cExt, 2 ), aObjsc[ x ] := If( !Empty( cObjDir ), cObjDir, '' ) + cTest + "." + exten( cExt, 1 ) ) } )

    ENDIF

    IF lFwh

        Fwrite( nLinkHandle, "FWH = " + cfwhpath + CRLF )

    ELSEIF lCw

        Fwrite( nLinkHandle, "C4W =" + ccwpath + CRLF )

    ELSEIF lMiniGui

        Fwrite( nLinkHandle, "MINIGUI =" + cMiniPath + CRLF )


    ENDIF

    FOR x := 1 TO Len( amacros )

        IF !Empty( amacros[ x, 2 ] )

            cItem := amacros[ x, 2 ]
            nPos  := Ascan( aprgs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cpath == citem } )

            IF nPos > 0

                Aeval( aprgs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), If( cPath == citem, aprgs[ b ] := Strtran( a, cpath, "$(" + amacros[ x, 1 ] + ')\' ), ) } )

                IF !amacros[ x, 3 ]

                    Fwrite( nLinkHandle, amacros[ x, 1 ] + ' = ' + Left( amacros[ x, 2 ], Len( amacros[ x, 2 ] ) - 1 ) + " " + CRLF )
                    amacros[ x, 3 ] := .t.

                ENDIF

            ENDIF

            nPos := Ascan( acs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cpath == citem } )

            IF nPos > 0

                IF !amacros[ x, 3 ]

                    Aeval( acs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), If( cPath == citem, acs[ b ] := Strtran( a, cpath, "$(" + amacros[ x, 1 ] + If( lgcc, ")/", ')\' ) ), ) } )
                    Fwrite( nLinkHandle, amacros[ x, 1 ] + ' = ' + Left( amacros[ x, 2 ], Len( amacros[ x, 2 ] ) - 1 ) + " " + CRLF )
                    amacros[ x, 3 ] := .t.

                ENDIF

            ENDIF

            nPos := Ascan( aObjs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cpath == citem } )

            IF nPos > 0

                IF !Empty( cObjDir )

                    Aeval( aObjs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), If( cPath == citem, aObjs[ b ] := Strtran( a, cpath, "$(" + amacros[ x, 1 ] + If( lgcc, ")/", ')\' ) ), ) } )
                    Fwrite( nLinkHandle, amacros[ x, 1 ] + ' = ' + Left( amacros[ x, 2 ], Len( amacros[ x, 2 ] ) - 1 ) + " " + CRLF )

                ENDIF

            ENDIF

            IF lExtended

                nPos := Ascan( aObjsc, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cpath == citem } )

                IF nPos > 0

                    IF !Empty( cObjDir )

                        Aeval( aObjsc, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), If( cPath == citem, aObjsc[ b ] := Strtran( a, cpath, "$(" + amacros[ x, 1 ] + If( lgcc, ")/", ')\' ) ), ) } )

                    ENDIF

                ENDIF

            ENDIF

        ENDIF

    NEXT

    IF lGcc

        IF At( "linux", Getenv( "HB_ARCHITECTURE" ) ) > 0 .or. cOs == "Linux"

            hb_FNAMESPLIT( cTopfile, @cPath, @cTest, @cExt, @cDrive )
            cExt := Substr( cExt, 2 )
            Fwrite( nLinkHandle, "PROJECT = " + If( Isupper( cExt ), Strtran( cTopfile, ".PRG", "" ), Strtran( cTopfile, ".prg", "" ) ) + " $(PR) " + CRLF )

        ELSE

            hb_FNAMESPLIT( cTopfile, @cPath, @cTest, @cExt, @cDrive )
            cExt := Substr( cExt, 2 )
            Fwrite( nLinkHandle, "PROJECT = " + If( Isupper( cExt ), cTest + "." + Strtran( cExt, "PRG", "EXE" ), cTest + "." + Strtran( cExt, "prg", "exe" ) ) + " $(PR) " + CRLF )

        ENDIF

    ELSE

        hb_FNAMESPLIT( cTopfile, @cPath, @cTest, @cExt, @cDrive )
        cExt := Substr( cExt, 2 )
        Fwrite( nLinkHandle, "PROJECT = " + If( Isupper( cExt ), cTest + "." + Strtran( cExt, "PRG", "exe" ), cTest + "." + Strtran( cExt, "prg", "exe" ) ) + " $(PR) " + CRLF )

    ENDIF

    hb_FNAMESPLIT( cTopfile, @cPath, @cTest, @cExt, @cDrive )
    cExt := Substr( cExt, 2 )

    IF !lextended

        Fwrite( nLinkHandle, "OBJFILES = " )

        IF Len( aObjs ) < 1

            Fwrite( nLinkHandle, + " $(OB) " + CRLF )

        ELSE

            Aeval( aObjs, { | x, i | If( ( i <> Len( aObjs ) .and. x <> cTopfile ), Fwrite( nLinkHandle, ' ' + Alltrim( x ) ), Fwrite( nLinkHandle, " " + " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )

        ENDIF

        hb_FNAMESPLIT( cTopfile, @cPath, @cTest, @cExt, @cDrive )
        cExt := Substr( cExt, 2 )
        Fwrite( nLinkHandle, "CFILES =" )

        IF Len( aCs ) < 1

            Fwrite( nLinkHandle, + " $(CF)" + CRLF )

        ELSE

            Aeval( aCs, { | x, i | If( ( i <> Len( aCs ) .and. x <> cTopfile ), Fwrite( nLinkHandle, ' ' + Alltrim( x ) ), Fwrite( nLinkHandle, " " + Alltrim( x ) + " $(CF) " + CRLF ) ) } )

        ENDIF

        Fwrite( nLinkHandle, "PRGFILE =" )

        IF Len( aPrgs ) < 1

            Fwrite( nLinkHandle, + " $(PS)" + CRLF )

        ELSE

            Aeval( aPrgs, { | x, i | If( i <> Len( aPrgs ), Fwrite( nLinkHandle, ' ' + Alltrim( x ) ), Fwrite( nLinkHandle, " " + Alltrim( x ) + " $(PS) " + CRLF ) ) } )

        ENDIF

    ELSE

        Fwrite( nLinkHandle, "OBJFILES =" )

        IF Len( aObjs ) < 1

            Fwrite( nLinkHandle, + " $(OB) " + CRLF )

        ELSE

            Aeval( aobjs, { | x, i | nWriteFiles ++, If( ( i <> Len( aobjs ) .and. x <> cTopfile ), Fwrite( nLinkHandle, ' ' + Alltrim( x ) + If( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), Fwrite( nLinkHandle, " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )

        ENDIF

        nWriteFiles := 0
        Fwrite( nLinkHandle, "PRGFILES =" )

        IF Len( aPrgs ) < 1

            Fwrite( nLinkHandle, + " $(PS)" + CRLF )

        ELSE

            Aeval( aPrgs, { | x, i | nWriteFiles ++, If( i <> Len( aPrgs ), Fwrite( nLinkHandle, ' ' + Alltrim( x ) + If( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), Fwrite( nLinkHandle, " " + Alltrim( x ) + " $(PS) " + CRLF ) ) } )

        ENDIF

        nWriteFiles := 0
        Fwrite( nLinkHandle, "OBJCFILES =" )

        IF Len( aObjsc ) < 1

            Fwrite( nLinkHandle, + " $(OB) " + CRLF )

        ELSE

            Aeval( aObjsc, { | x, i | nWriteFiles ++, If( i <> Len( aobjsc ), Fwrite( nLinkHandle, ' ' + Alltrim( x ) + If( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), Fwrite( nLinkHandle, " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )

        ENDIF

        nWriteFiles := 0
        Fwrite( nLinkHandle, "CFILES =" )

        IF Len( aCs ) < 1

            Fwrite( nLinkHandle, + " $(CF)" + CRLF )

        ELSE

            Aeval( aCs, { | x, i | nWriteFiles ++, If( i <> Len( aCs ), Fwrite( nLinkHandle, ' ' + Alltrim( x ) + If( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), Fwrite( nLinkHandle, " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )

        ENDIF

    ENDIF

    CResName :=lower(CResName ) 
    Fwrite( nLinkHandle, "RESFILES = "+ CResName  + CRLF )
    Fwrite( nLinkHandle, "RESDEPEN = "+ strtran(CResName,".rc",".res")  + CRLF )

    IF lRddads

        cDefBccLibs += " rddads.lib ace32.lib"
        cDeflibGccLibs += " -lrddads -ladsloc"
    ENDIF

    IF Len( alibsout ) > 0 .and. lExternalLib

        IF lvcc .or. lbcc

            cOldLib := cDefBccLibs
            nPos    := Ascan( aLibsout, { | z | At( "html", Lower( z ) ) > 0 } )

            IF npos > 0

                cHtmlLib += aLibsout[ npos ]
                Adel( alibsout, nPos )
                Asize( alibsout, Len( alibsout ) - 1 )

            ENDIF

            Aeval( alibsout, { | cLib | cLibs += " " + cLib } )

            cDefBccLibs := cHtmlLib + " " + cOldLib + " " + cLibs

        ENDIF

        IF lGcc

            nPos := Ascan( aLibsout, { | z | At( "html", Lower( z ) ) > 0 } )

            IF npos > 0

                cHtmlLib += "-l" + Strtran( aLibsout[ npos ], '.a', "" )
                Adel( alibsout, nPos )
                Asize( alibsout, Len( alibsout ) - 1 )

            ENDIF

            Aeval( alibsout, { | cLib | cLibs += " -l" + Strtran( cLib, '.a', "" ) } )

            IF cOs == "Linux"

                cOldLib        := " " + cDeflibGccLibs
                cDeflibGccLibs := cHtmlLib + " " + cOldLib + " " + cLibs

            ELSEIF cOs == "OS/2"

                cOldLib     := " " + cgcclibsos2
                cgcclibsos2 := cHtmlLib + " " + cOldLib + " " + cLibs

            ELSE

                cOldLib     := " " + cDefGccLibs
                cDefGccLibs := cHtmlLib + " " + cOldLib + " " + cLibs

            ENDIF

        ENDIF

    ENDIF

    IF lBcc .or. lVcc

        IF lFwh

            Fwrite( nLinkHandle, "LIBFILES = $(FWH)\lib\fiveh.lib $(FWH)\lib\fivehc.lib " + cDefBccLibs + CRLF )

        ELSEIF lMiniGui

            Fwrite( nLinkHandle, "LIBFILES = Minigui.lib " + cDefBccLibs + CRLF )

        ELSEIF lCw

            Fwrite( nLinkHandle, "LIBFILES = $(C4W)\c4wclass.lib $(C4W)\wbrowset.lib $(C4W)\otabt.lib $(C4W)\clip4win.lib" + cDefBccLibs + CRLF )

        ELSE

            Fwrite( nLinkHandle, "LIBFILES = " + cDefBccLibs + CRLF )

        ENDIF

    ELSEIF lGcc

        IF cOs == "Linux"

            Fwrite( nLinkHandle, "LIBFILES = -Wl,--start-group " + cDeflibGccLibs + " -Wl,--end-group " + CRLF )

        ELSEIF cOs == "OS/2"

            Fwrite( nLinkHandle, "LIBFILES = " + cgcclibsos2 + CRLF )

        ELSE

            Fwrite( nLinkHandle, "LIBFILES = " + cDefgccLibs + CRLF )

        ENDIF

    ENDIF

    Fwrite( nLinkHandle, "DEFFILE = " + CRLF )
    Fwrite( nLinkHandle, "HARBOURFLAGS = " + cDefHarOpts + CRLF )

    IF lBcc

        Fwrite( nLinkHandle, "CFLAG1 =  -OS $(CFLAGS) -d -L$(BHC)\lib;$(FWH)\lib -c" + CRLF )
        Fwrite( nLinkHandle, "CFLAG2 =  -I$(BHC)\include;$(BCB)\include" + CRLF )

        Fwrite( nLinkHandle, "RFLAGS = " + CRLF )
        Fwrite( nLinkHandle, "LFLAGS = -L$(BCB)\lib\obj;$(BCB)\lib;$(BHC)\lib;$(FWH)\lib -Gn -M -m -s -Tpe" + If( lFWH, " -aa", IF( lMiniGui , " -aa" , " -ap" )) + CRLF )
        Fwrite( nLinkHandle, "IFLAGS = " + CRLF )
        Fwrite( nLinkHandle, "LINKER = ilink32" + CRLF )
        Fwrite( nLinkHandle, " " + CRLF )
        Fwrite( nLinkHandle, "ALLOBJ = " + If( lFwh, "c0w32.obj", "c0x32.obj" ) + " $(OBJFILES)" + If( lextended, " $(OBJCFILES)", " " ) + CRLF )
        Fwrite( nLinkHandle, "ALLRES = $(RESFILES)" + CRLF )
        Fwrite( nLinkHandle, "ALLLIB = $(LIBFILES) import32.lib cw32.lib" + CRLF )
        Fwrite( nLinkHandle, ".autodepend" + CRLF )

    ELSEIF lVcc

        Fwrite( nLinkHandle, "CFLAG1 =  -I$(INCLUDE_DIR) -TP -W3 -nologo $(C_USR) $(CFLAGS)" + CRLF )
        Fwrite( nLinkHandle, "CFLAG2 =  -c" + CRLF )
        Fwrite( nLinkHandle, "RFLAGS = " + CRLF )
        Fwrite( nLinkHandle, "LFLAGS = /LIBPATH:$(BCB)\lib;$(BHC)\lib;$(C4W)\lib /SUBSYSTEM:CONSOLE" + CRLF )
        Fwrite( nLinkHandle, "IFLAGS = " + CRLF )
        Fwrite( nLinkHandle, "LINKER = link" + CRLF )
        Fwrite( nLinkHandle, " " + CRLF )
        Fwrite( nLinkHandle, "ALLOBJ = " + If( lCw, "$(C4W)\initc.obj", "" ) + "$(OBJFILES)" + If( lextended, " $(OBJCFILES)", " " ) + CRLF )
        Fwrite( nLinkHandle, "ALLRES = $(RESFILES)" + CRLF )
        Fwrite( nLinkHandle, "ALLLIB = $(LIBFILES) comdlg32.lib shell32.lib user32.lib gdi32.lib" + CRLF )

    ELSEIF lGcc

        Fwrite( nLinkHandle, "CFLAG1 = " + If( At( "Linux", cOs ) > 0, "-I$(HB_INC_INSTALL)", " -I$(BHC)/include" ) + " -c -Wall" + CRLF )
        Fwrite( nLinkHandle, "CFLAG2 = " + If( At( "Linux", cOs ) > 0, "-L $(HB_LIB_INSTALL)", " -L $(BHC)/lib" ) + CRLF )
        Fwrite( nLinkHandle, "RFLAGS = " + CRLF )
        Fwrite( nLinkHandle, "LFLAGS = $(CFLAG2)" + CRLF )
        Fwrite( nLinkHandle, "IFLAGS = " + CRLF )
        Fwrite( nLinkHandle, "LINKER = gcc" + CRLF )
        Fwrite( nLinkHandle, " " + CRLF )
        Fwrite( nLinkHandle, "ALLOBJ = $(OBJFILES) " + If( lextended, " $(OBJCFILES)", " " ) + CRLF )
        Fwrite( nLinkHandle, "ALLRES = $(RESFILES) " + CRLF )
        Fwrite( nLinkHandle, "ALLLIB = $(LIBFILES) " + CRLF )
        Fwrite( nLinkHandle, ".autodepend" + CRLF )

    ENDIF

    Fwrite( nLinkHandle, " " + CRLF )
    Fwrite( nLinkHandle, "#COMMANDS" + CRLF )

    Aeval( aCommands, { | xItem | Fwrite( nLinkHandle, xitem[ 1 ] + CRLF ), Fwrite( nLinkHandle, xitem[ 2 ] + CRLF ), Fwrite( nLinkHandle, " " + CRLF ) } )

    IF lBcc

        Fwrite( nLinkHandle, "#BUILD" + CRLF )
        Fwrite( nLinkHandle, " " + CRLF )
        Fwrite( nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)" + CRLF )
        Fwrite( nLinkHandle, "    $(BCB)\BIN\$(LINKER) @&&!" + CRLF )
        Fwrite( nLinkHandle, "    $(LFLAGS) +" + CRLF )
        Fwrite( nLinkHandle, "    $(ALLOBJ), +" + CRLF )
        Fwrite( nLinkHandle, "    $(PROJECT),, +" + CRLF )
        Fwrite( nLinkHandle, "    $(ALLLIB), +" + CRLF )
        Fwrite( nLinkHandle, "    $(DEFFILE), +" + CRLF )
        Fwrite( nLinkHandle, "    $(ALLRES) " + CRLF )
        Fwrite( nLinkHandle, "!" + CRLF )

    ELSEIF lVcc

        Fwrite( nLinkHandle, "#BUILD" + CRLF )
        Fwrite( nLinkHandle, "" + CRLF )
        Fwrite( nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)" + CRLF )
        Fwrite( nLinkHandle, "    $(BCB)\BIN\$(LINKER) @&&!" + CRLF )
        Fwrite( nLinkHandle, "    $(LFLAGS)" + CRLF )
        Fwrite( nLinkHandle, "    $(ALLOBJ) " + CRLF )
        Fwrite( nLinkHandle, "    $(PROJECT)" + CRLF )
        Fwrite( nLinkHandle, "    $(PROJECTMAP)" + CRLF )
        Fwrite( nLinkHandle, "    $(ALLLIB) " + CRLF )
        Fwrite( nLinkHandle, "    $(DEFFILE) " + CRLF )
        Fwrite( nLinkHandle, "    $(ALLRES) " + CRLF )
        Fwrite( nLinkHandle, "!" + CRLF )

    ELSEIF lGcc

        Fwrite( nLinkHandle, "#BUILD" + CRLF )
        Fwrite( nLinkHandle, " " + CRLF )
        Fwrite( nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)" + CRLF )

        IF At( 'Linux', cOs ) > 0

            Fwrite( nLinkHandle, "    $(LINKER) @&&!" + CRLF )

        ELSE

            Fwrite( nLinkHandle, "    $(BCB)\bin\$(LINKER) @&&!" + CRLF )

        ENDIF

        Fwrite( nLinkHandle, "    $(PROJECT) " + CRLF )
        Fwrite( nLinkHandle, "    $(ALLOBJ)  " + CRLF )
        Fwrite( nLinkHandle, "    $(LFLAGS)  " + CRLF )
        Fwrite( nLinkHandle, "    $(ALLLIB)  " + CRLF )
        Fwrite( nLinkHandle, "!" + CRLF )

    ENDIF

RETURN nil

FUNCTION CompUpdatedfiles()

    LOCAL cComm
    LOCAL cOld
    LOCAL nPos
    LOCAL nCount
    LOCAL nFiles
    LOCAL aCtocompile := {}
    LOCAL aOrder      := Listasarray2( aBuildOrder[ 2 ], " " )
    LOCAL lEnd
    LOCAL cErrText    := ""
    LOCAL xItem
    LOCAL nObjPos
    FOR nCount := 1 TO Len( aOrder )

        IF !lextended

            IF aOrder[ nCount ] == "$(CFILES)"

                nPos := Ascan( aCommands, { | x | x[ 1 ] == ".prg.c:" } )

                IF nPos > 0

                    cComm := aCommands[ nPos, 2 ]
                    cOld  := cComm

                ENDIF

                FOR nFiles := 1 TO Len( aPrgs )

                    xItem   := Substr( aPrgs[ nFiles ], Rat( If( lgcc, '/', '\' ), aPrgs[ nFiles ] ) + 1 )
                    nPos    := Ascan( aCs, { | x | x := Substr( x, Rat( If( lgcc, '/', '\' ), x ) + 1 ), Left( x, At( ".", x ) ) == Left( xItem, At( ".", xItem ) ) } )
                    nObjPos := Ascan( aobjs, { | x | x := Substr( x, Rat( If( lgcc, '/', '\' ), x ) + 1 ), Left( x, At( ".", x ) ) == Left( xItem, At( ".", xItem ) ) } )

                    IF fileisnewer( aprgs[ nFiles ], aobjs[ nObjPos ] )

                        IF nPos > 0

                            Aadd( aCtocompile, acs[ nPos ] )
                            cComm := Strtran( cComm, "o$*", "o" + aCs[ nPos ] )
                            cComm := Strtran( cComm, "$**", aPrgs[ nFiles ] )
                            Outstd( cComm )
                            Outstd( hb_osnewline() )
                            ! ( cComm )
                            cErrText := Memoread( 'Test.out' )
                            lEnd     := 'C2006' $ cErrText .or. 'No code generated' $ cErrText

                            IF !lIgnoreErrors .and. lEnd

                                QUIT

                            ELSE

                                Ferase( 'Test.out' )

                            ENDIF

                            cComm := cold

                        ENDIF

                    ENDIF

                NEXT

            ENDIF

            IF aOrder[ nCount ] == "$(OBJFILES)"

                IF lGcc

                    nPos := Ascan( aCommands, { | x | x[ 1 ] == ".c.o:" } )

                ELSE

                    nPos := Ascan( aCommands, { | x | x[ 1 ] == ".c.obj:" } )

                ENDIF

                IF nPos > 0

                    cComm := aCommands[ nPos, 2 ]
                    cOld  := ccomm

                ENDIF

                IF Len( aCtoCompile ) >= 1

                    FOR nFiles := 1 TO Len( aCs )

                        nPos := Ascan( aCs, { | x | Left( x, At( ".", x ) ) == Left( aCtoCompile[ nfiles ], At( ".", aCtoCompile[ nfiles ] ) ) } )

                        IF nPos == 0

                            Aadd( aCtoCompile, acs[ nFiles ] )

                        ENDIF

                    NEXT

                ENDIF

                FOR nFiles := 1 TO Len( aCtocompile )

                    xItem := Substr( aCtocompile[ nFiles ], Rat( If( lgcc, '/', '\' ), aCtocompile[ nFiles ] ) + 1 )
                    nPos  := Ascan( aObjs, { | x | x := Substr( x, Rat( If( lgcc, '/', '\' ), x ) + 1 ), Left( x, At( ".", x ) ) == Left( aCtocompile[ nFiles ], At( ".", xItem ) ) } )

                    IF nPos > 0

                        cComm := Strtran( cComm, "o$*", "o" + aObjs[ nPos ] )
                        cComm := Strtran( cComm, "$**", aCtocompile[ nFiles ] )
                        Outstd( " " )

                        Outstd( cComm )
                        Outstd( hb_osnewline() )
                        ! ( cComm )
                        ccomm := cold

                    ENDIF

                NEXT

            ENDIF

        ELSE /**************Extended mode ******/           ////

            IF aOrder[ nCount ] == "$(CFILES)"

                nPos := Ascan( aCommands, { | x | x[ 1 ] == ".c.obj:" } )

                IF nPos > 0

                    cComm := aCommands[ nPos, 2 ]
                    cOld  := cComm

                ELSE

                    nPos := Ascan( aCommands, { | x | x[ 1 ] == ".C.OBJ:" } )

                    IF nPos > 0

                        cComm := aCommands[ nPos, 2 ]
                        cOld  := cComm

                    ENDIF

                ENDIF

                FOR nFiles := 1 TO Len( acs )

                    xItem := Substr( acs[ nFiles ], Rat( If( lgcc, '/', '\' ), acs[ nFiles ] ) + 1 )
                    nPos  := Ascan( aObjsc, { | x | x := Substr( x, Rat( If( lgcc, '/', '\' ), x ) + 1 ), Left( x, At( ".", x ) ) == Left( xitem, At( ".", xitem ) ) } )

                    IF fileisnewer( aCs[ nFiles ], aobjsc[ nPos ] )

                        IF nPos > 0

                            cComm := Strtran( cComm, "o$*", "o" + aobjsc[ nPos ] )
                            cComm := Strtran( cComm, "$**", acs[ nFiles ] )
                            cComm += " > Test.out"
                            Outstd( cComm )
                            Outstd( hb_osnewline() )
                            ! ( cComm )
                            cErrText := Memoread( 'Test.out' )
                            lEnd     := 'Error' $ cErrText

                            IF !lIgnoreErrors .and. lEnd

                                QUIT

                            ELSE

                                Ferase( 'Test.out' )

                            ENDIF

                            cComm := cold

                        ENDIF

                    ENDIF

                NEXT

            ENDIF

            IF aOrder[ nCount ] == "$(OBJFILES)"

                IF lGcc

                    nPos := Ascan( aCommands, { | x | x[ 1 ] == ".prg.o:" } )

                ELSE

                    nPos := Ascan( aCommands, { | x | x[ 1 ] == ".prg.obj:" } )

                ENDIF
                IF nPos > 0

                    cComm := aCommands[ nPos, 2 ]
                    cOld  := ccomm

                ELSE

                    IF lGcc

                        nPos := Ascan( aCommands, { | x | x[ 1 ] == ".PRG.O:" } )

                    ELSE

                        nPos := Ascan( aCommands, { | x | x[ 1 ] == ".PRG.OBJ:" } )

                    ENDIF

                ENDIF

                FOR nFiles := 1 TO Len( aprgs )

                    xItem := Substr( aprgs[ nFiles ], Rat( If( lgcc, '/', '\' ), aprgs[ nFiles ] ) + 1 )
                    nPos  := Ascan( aobjs, { | x | x := Substr( x, Rat( If( lgcc, '/', '\' ), x ) + 1 ), Left( x, At( ".", x ) ) == Left( xItem, At( ".", xitem ) ) } )

                    IF fileisnewer( aprgs[ nFiles ], aobjs[ npos ] )

                        IF nPos > 0

                            cComm := Strtran( cComm, "o$*", "o" + aObjs[ nPos ] )
                            cComm := Strtran( cComm, "$**", aprgs[ nFiles ] )
                            cComm += " > Test.out"
                            Outstd( " " )
                            Outstd( cComm )
                            Outstd( hb_osnewline() )
                            ! ( cComm )
                            cErrText := Memoread( 'Test.out' )
                            lEnd     := 'C2006' $ cErrText .or. 'No code generated' $ cErrText

                            IF !lIgnoreErrors .and. lEnd

                                QUIT

                            ELSE

                                Ferase( 'Test.out' )

                            ENDIF

                            ccomm := cold

                        ENDIF

                    ENDIF

                NEXT

            ENDIF

        ENDIF

        IF aOrder[ nCount ] == "$(RESDEPEN)"

            nPos := Ascan( aCommands, { | x | x[ 1 ] == ".rc.res:" } )

            IF nPos > 0

                cComm := aCommands[ nPos, 2 ]
                cold  := ccomm 

            ENDIF

            FOR nFiles := 1 TO Len( aRes )

                IF !Empty( ares[ nFiles ] )

                    cComm := Strtran( cComm, "$<", aRes[ nFiles ] )
                    Outstd( " " )
                    ! ( cComm )

                ENDIF
                  ccomm := cold
            NEXT

        ENDIF

    NEXT

RETURN nil

FUNCTION fileisnewer( cFile, as )

    LOCAL nCount := 0

    IF !lextended

        FOR nCount := 1 TO Len( aPrgs )

            adir := { cFile,, hbmake_filedate( cFile ), hbmake_filetime( cFile ), ;
                      as[ nCount ], hbmake_filedate( as[ nCount ] ), hbmake_filetime( as[ nCount ] ) }

            IF Empty( adir[ 7 ] )

                adir[ 2 ] := .t.

            ELSE

                adir[ 2 ] := td2jul( adir[ 4 ], adir[ 3 ] ) > td2jul( adir[ 7 ], adir[ 6 ] )

            ENDIF

        NEXT

    ELSE

        adir := { cFile,, hbmake_filedate( cFile ), hbmake_filetime( cFile ), ;
                  as, hbmake_filedate( as ), hbmake_filetime( as ) }

        IF Empty( adir[ 7 ] )

            adir[ 2 ] := .t.

        ELSE

            adir[ 2 ] := td2jul( adir[ 4 ], adir[ 3 ] ) > td2jul( adir[ 7 ], adir[ 6 ] )

        ENDIF

    ENDIF

RETURN aDir[ 2 ]

FUNC crtlibmakfile( cFile )

    LOCAL ain      := {}
    LOCAL aOut     := {}
    LOCAL aSrc     := Directory( "*.prg" )
    LOCAL nLenaSrc := Len( asrc )
    LOCAL nLenaOut

    LOCAL aOutC     := {}
    LOCAL aSrcC     := Directory( "*.c" )
    LOCAL cOs       := "Win32"
    LOCAL cCompiler := "BCC"
    LOCAL cfwhpath  := Left( cfile, At( '.', cfile ) - 1 ) + Space( 40 )

    LOCAL lAutomemvar  := .f.
    LOCAL lvarismemvar := .f.
    LOCAL ldebug       := .f.
    LOCAL lSupressline := .f.
    LOCAL cDefHarOpts := ""
    LOCAL cObjDir     := 'obj' + Space( 20 )
    LOCAL lCompMod := .f.
    LOCAL x
    LOCAL y
    LOCAL nPos as numeric
    LOCAL lGenppo  := .f.
    LOCAL getlist  := {}
    LOCAL citem        := ""
    LOCAL cExt         := ""
    LOCAL cDrive       := ""
    LOCAL cPath        := ""
    LOCAL cTest        := ""
    LOCAL cLast        := ''
    LOCAL cUserdef     := Space( 40 )
    LOCAL cUserInclude := Space( 40 )
    LOCAL nWriteFiles  := 0

    nLinkHandle := Fcreate( cFile )
    WriteMakeFileHeader()
    CLS
    Setcolor( 'w/b+,b+/w,w+/b,w/b+,w/b,w+/b' )
    @  0,  0, Maxrow(), Maxcol() BOX( Chr( 201 ) + Chr( 205 ) + Chr( 187 ) + Chr( 186 ) + Chr( 188 ) + Chr( 205 ) + Chr( 200 ) + Chr( 186 ) + Space( 1 ) )
    ATTENTION( aLangMessages[ 27 ], 0 )

    @  1,  1 SAY "Select Os"                                      
    @  1, 12 GET cos radio { "Win32", "OS/2", "Linux" }           
    @  1, 23 SAY "Select C Compiler"                              
    @  1, 40 GET cCompiler radio { "BCC", "MSVC", "GCC" }

    READ

    SET CURSOR ON

    @  4,  1 SAY "Library name with our extention" GET cfwhpath PICT "@s15"       
    @  4, 40 SAY "Obj Files Dir"                   GET cObjDir  PICT "@s15"

    ATTENTION( "Harbour Options", 5 )

    @  6,  1 GET lautomemvar checkbox caption "Automatic memvar declaration"                                         
    @  6, 40 GET lvarismemvar checkbox caption "Variables are assumed M->"                                           
    @  7,  1 GET lDebug checkbox caption "Debug info"                                                                
    @  7, 40 GET lSupressline checkbox caption "Suppress line number information"                                    
    @  8,  1 GET lGenppo checkbox caption "Generate pre-processed output"                                            
    @  8, 40 GET lCompMod checkbox caption "compile module only"                                                     
    @  9,  1 SAY "User Defines "                                                  GET cUserDef     PICT "@s15"       
    @  9, 40 SAY "User include Path"                                              GET cUserInclude PICT "@s15"

    READ

    IF !Empty( cUserDef )

        cDefHarOpts += " -D" + Alltrim( cUserDef ) + " "

    ENDIF

    IF !Empty( cUserInclude )

        cDefHarOpts += " -I" + Alltrim( cUserInclude ) + " "

    ENDIF

    lBcc    := IF( At( "BCC", cCompiler ) > 0, .t., .f. )
    lVcc    := IF( At( "MSVC", cCompiler ) > 0, .t., .f. )
    lGcc    := IF( At( "GCC", cCompiler ) > 0, .t., .f. )
    cObjDir := Alltrim( cObjDir )

    IF !Empty( cobjDir )

        IF Dirchange( cobjDir ) != 0

            Makedir( cobjDir )

        ELSE

            Dirchange( '..' )

        ENDIF

    ENDIF

    amacros := GetSourceDirMacros( lgcc, cos )

    IF lGcc

        cObjDir := Alltrim( cObjDir )

        IF !Empty( cObjDir )

            cObjDir += '/'

        ENDIF

        cTest := cObjDir + '/'

    ELSE
        cObjDir := Alltrim( cObjDir )

        IF !Empty( cObjDir )

            cObjDir += '\'

        ENDIF

        cTest := cObjDir + '\'

    ENDIF

    Aeval( amacros, { | x, y | cItem := Substr( x[ 2 ], 1, Len( x[ 2 ] ) ), If( At( citem, cTest ) > 0, ( amacros[ y, 1 ] := 'OBJ', amacros[ y, 2 ] := cObjDir ), ) } )

    IF lAutomemvar
     
        cDefHarOpts += " -a "

    ENDIF

    IF lvarismemvar

        cDefHarOpts += " -v "

    ENDIF

    IF ldebug

        cDefHarOpts += " -b "

    ENDIF

    IF lSupressline

        cDefHarOpts += " -l "

    ENDIF

    IF lGenppo

        cDefHarOpts += " -p "

    ENDIF

    IF lCompmod

        cDefHarOpts += " -m "

    ENDIF

    IF lBcc

        Aadd( aCommands, { ".cpp.obj:", "$(BCB)\BIN\bcc32 $(CFLAG1) $(CFLAG2) -o$* $*" } )
        Aadd( aCommands, { ".c.obj:", "$(BCB)\BIN\bcc32 -I$(BHC)\include $(CFLAG1) $(CFLAG2) -o$* $**" } )

        IF lextended

            Aadd( aCommands, { ".prg.obj:", "$(BHC)\bin\harbour -n -go -I$(BHC)\include $(HARBOURFLAGS) -I$(FWH)\include -o$* $**" } )

        ELSE

            Aadd( aCommands, { ".prg.c:", "$(BHC)\bin\harbour -n -I$(BHC)\include $(HARBOURFLAGS) -I$(FWH)\include -o$* $**" } )

        ENDIF

        Aadd( aCommands, { ".rc.res:", "$(BCB)\BIN\brcc32 $(RFLAGS) $<" } )

    ELSEIF lGcc

        IF At( "linux", Getenv( "HB_ARCHITECTURE" ) ) > 0 .or. cOs == "Linux"

            Aadd( aCommands, { ".cpp.o:", "gcc $(CFLAG1) $(CFLAG2) -o$* $*" } )
            Aadd( aCommands, { ".c.o:", "gcc -I$(HB_INC_INSTALL) $(CFLAG1) $(CFLAG2) -I. -o$* $**" } )

            IF lextended

                Aadd( aCommands, { ".prg.o:", "harbour -n $(HARBOURFLAGS) -I$(HB_INC_INSTALL) -I. -go  -o$* $**" } )

            ELSE

                Aadd( aCommands, { ".prg.c:", "harbour -n $(HARBOURFLAGS) -I$(HB_INC_INSTALL) -I.  -o$* $**" } )

            ENDIF

        ELSE

            Aadd( aCommands, { ".cpp.o:", "$(BCB)\bin\gcc $(CFLAG1) $(CFLAG2) -o$* $*" } )
            Aadd( aCommands, { ".c.o:", "$(BCB)\bin\gcc -I$(BHC)/include $(CFLAG1) $(CFLAG2) -I. -o$* $**" } )

            IF lextended

                Aadd( aCommands, { ".prg.o:", "$(BHC)\bin\harbour -n -go -I$(BHC)/include $(HARBOURFLAGS)  -o$* $**" } )

            ELSE

                Aadd( aCommands, { ".prg.c:", "$(BHC)\bin\harbour -n -I$(BHC)/include $(HARBOURFLAGS)  -o$* $**" } )

            ENDIF

        ENDIF

    ELSEIF lVcc

        Aadd( aCommands, { ".cpp.obj:", "$(BCB)\bin\cl $(CFLAG1) $(CFLAG2) -Fo$* $*" } )
        Aadd( aCommands, { ".c.obj:", "$(BCB)\bin\cl -I$(BHC)\include $(CFLAG1) $(CFLAG2) -Fo$* $**" } )

        IF lextended

            Aadd( aCommands, { ".prg.obj:", "$(BHC)\bin\harbour -go -n -I$(BHC)\include $(HARBOURFLAGS) -I$(C4W)\include -o$* $**" } )

        ELSE

            Aadd( aCommands, { ".prg.c:", "$(BHC)\bin\harbour -n -I$(BHC)\include $(HARBOURFLAGS) -I$(C4W)\include -o$* $**" } )

        ENDIF

        Aadd( aCommands, { ".rc.res:", "$(BCB)\BIN\rc $(RFLAGS) $<" } )

    ENDIF

    attention( 'Spacebar to select, Enter to continue process', 22 )

    IF !lRecurse

        ain      := GetSourceFiles( .f., lGcc, cOs )
        nLenaSrc := Len( ain )

    ELSE

        ain      := GetSourceFiles(, lGcc, cOs )
        nLenaSrc := Len( ain )

    ENDIF

    aOut := Aclone( aIn )
    pickarry( 10, 15, 19, 64, aIn, aOut )
    nLenaOut := Len( aOut )

    Aeval( aout, { | x, y | aout[ y ] := Trim( Substr( aOut[ y ], 1, At( ' ', aout[ y ] ) ) ) } )
    Aeval( aout, { | xItem | If( At( '.c', xItem ) > 0 .or. At( '.C', xItem ) > 0, Aadd( aoutc, xitem ), ) } )
    Aeval( aoutc, { | x, z | citem := x, z := Ascan( aout, { | t | t = citem } ), If( z > 0, Asize( Adel( aout, z ), Len( aout ) - 1 ), ) } )

    aOut  := Asort( aOut )
    aPrgs := Aclone( aout )

    aObjs := Aclone( aout )
    Aeval( aobjs, { | xItem, x | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cext := Substr( cExt, 2 ), If( !lGcc, aObjs[ x ] := cObjDir + cTest + "." + exte( cExt, 2 ), aObjs[ x ] := cObjDir + cTest + "." + exte( cExt, 3 ) ) } )
    aCs := Aclone( aoutc )

    IF !lextended

        Aeval( aOutc, { | xItem, x | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cext := Substr( cExt, 2 ), If( !lGcc, Aadd( aObjs, cObjDir + cTest + "." + exten( cExt, 2 ) ), Aadd( aObjs, cObjDir + cTest + "." + exten( cExt, 1 ) ) ) } )
        Aeval( aout, { | xItem | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cExt := Substr( cExt, 2 ), Aadd( aCs, cObjDir + cTest + "." + exte( cExt, 1 ) ) } )

    ELSE

        aObjsc := Aclone( aoutc )
        Aeval( aoutc, { | xItem, x | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cext := Substr( cExt, 2 ), If( !lGcc, aObjsc[ x ] := cObjDir + cTest + "." + exten( cExt, 2 ), aObjsc[ x ] := cObjDir + cTest + "." + exten( cExt, 1 ) ) } )

    ENDIF

    FOR x := 1 TO Len( amacros )

        IF !Empty( amacros[ x, 2 ] )

            cItem := amacros[ x, 2 ]
            nPos := Ascan( aprgs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cpath == citem } )

            IF nPos > 0

                Aeval( aprgs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), If( cPath == citem, aprgs[ b ] := Strtran( a, cpath, "$(" + amacros[ x, 1 ] + ')\' ), ) } )

                IF !amacros[ x, 3 ]

                    Fwrite( nLinkHandle, amacros[ x, 1 ] + ' = ' + Left( amacros[ x, 2 ], Len( amacros[ x, 2 ] ) - 1 ) + " " + CRLF )
                    amacros[ x, 3 ] := .t.

                ENDIF

            ENDIF

            nPos := Ascan( acs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cpath == citem } )

            IF nPos > 0

                Aeval( acs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), If( cPath == citem, acs[ b ] := Strtran( a, cpath, "$(" + amacros[ x, 1 ] + If( lgcc, ")/", ')\' ) ), ) } )

                IF !amacros[ x, 3 ]

                    Fwrite( nLinkHandle, amacros[ x, 1 ] + ' = ' + Left( amacros[ x, 2 ], Len( amacros[ x, 2 ] ) - 1 ) + " " + CRLF )
                    amacros[ x, 3 ] := .t.

                ENDIF

            ENDIF

            nPos := Ascan( aObjs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cpath == citem } )

            IF nPos > 0

                IF !Empty( cObjDir )

                    Aeval( aObjs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), If( cPath == citem, aObjs[ b ] := Strtran( a, cpath, "$(" + amacros[ x, 1 ] + If( lgcc, ")/", ')\' ) ), ) } )
                    Fwrite( nLinkHandle, amacros[ x, 1 ] + ' = ' + Left( amacros[ x, 2 ], Len( amacros[ x, 2 ] ) - 1 ) + " " + CRLF )

                ENDIF

            ENDIF

            IF lExtended

                nPos := Ascan( aObjsc, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cpath == citem } )

                IF nPos > 0

                    IF !Empty( cObjDir )

                        Aeval( aObjsc, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), If( cPath == citem, aObjsc[ b ] := Strtran( a, cpath, "$(" + amacros[ x, 1 ] + If( lgcc, ")/", ')\' ) ), ) } )

                    ENDIF

                ENDIF

            ENDIF

        ENDIF

    NEXT

    IF lGcc

        IF At( "linux", Getenv( "HB_ARCHITECTURE" ) ) > 0 .or. cOs == "Linux"

            Fwrite( nLinkHandle, "PROJECT = " + Alltrim( cfwhpath ) + ".a " + CRLF )

        ELSE

            Fwrite( nLinkHandle, "PROJECT = " + Alltrim( Lower( cfwhpath ) ) + ".a " + CRLF )

        ENDIF
    ELSE

        Fwrite( nLinkHandle, "PROJECT = " + Alltrim( Lower( cfwhpath ) ) + ".lib " + CRLF )

    ENDIF

    IF !lextended

        nWriteFiles := 0
        Fwrite( nLinkHandle, "OBJFILES =" )

        IF Len( aObjs ) < 1

            Fwrite( nLinkHandle, + " $(OB) " + CRLF )

        ELSE

            nWriteFiles := 0
            Aeval( aObjs, { | x, i | nWriteFiles ++, If( i <> Len( aobjs ), Fwrite( nLinkHandle, ' ' + Alltrim( x ) + If( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), Fwrite( nLinkHandle, " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )

        ENDIF

        nWriteFiles := 0
        Fwrite( nLinkHandle, "CFILES =" )

        IF Len( aCs ) < 1

            Fwrite( nLinkHandle, + " $(CF)" + CRLF )

        ELSE

            Aeval( aCs, { | x, i | nWriteFiles ++, If( i <> Len( aCs ), Fwrite( nLinkHandle, ' ' + Alltrim( x ) + If( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), Fwrite( nLinkHandle, " " + Alltrim( x ) + " $(CF) " + CRLF ) ) } )

        ENDIF

        Fwrite( nLinkHandle, "PRGFILE =" )
        nWriteFiles := 0

        IF Len( aPrgs ) < 1

            Fwrite( nLinkHandle, + " $(PS)" + CRLF )

        ELSE

            Aeval( aPrgs, { | x, i | nWriteFiles ++, If( i <> Len( aPrgs ), Fwrite( nLinkHandle, ' ' + Alltrim( x ) + If( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), Fwrite( nLinkHandle, " " + Alltrim( x ) + " $(PS) " + CRLF ) ) } )

        ENDIF

    ELSE /****extended moded ****/

        Fwrite( nLinkHandle, "OBJFILES =" )
        nWriteFiles := 0

        IF Len( aObjs ) < 1

            Fwrite( nLinkHandle, + " $(OB) " + CRLF )

        ELSE

            Aeval( aObjs, { | x, i | nWriteFiles ++, If( i <> Len( aobjs ), Fwrite( nLinkHandle, ' ' + Alltrim( x ) + If( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), Fwrite( nLinkHandle, " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )

        ENDIF

        Fwrite( nLinkHandle, "PRGFILES =" )
        nWriteFiles := 0

        IF Len( aPrgs ) < 1

            Fwrite( nLinkHandle, + " $(PS)" + CRLF )

        ELSE

            Aeval( aPrgs, { | x, i | nWriteFiles ++, If( i <> Len( aPrgs ), Fwrite( nLinkHandle, ' ' + Alltrim( x ) + If( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), Fwrite( nLinkHandle, " " + Alltrim( x ) + " $(PS) " + CRLF ) ) } )

        ENDIF

        nWriteFiles := 0

        IF Len( aObjsc ) > 0

            Fwrite( nLinkHandle, "OBJCFILES =" )

            IF Len( aObjsc ) < 1

                Fwrite( nLinkHandle, + " $(OB) " + CRLF )

            ELSE

                Aeval( aObjsc, { | x, i | nWriteFiles ++, If( i <> Len( aobjsc ), Fwrite( nLinkHandle, ' ' + Alltrim( x ) + If( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), Fwrite( nLinkHandle, " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )

            ENDIF

        ENDIF

        nWriteFiles := 0

        IF Len( acs ) > 0

            Fwrite( nLinkHandle, "CFILES =" )

            IF Len( aCs ) < 1

                Fwrite( nLinkHandle, + " $(CF)" + CRLF )

            ELSE

                Aeval( aCs, { | x, i | nWriteFiles ++, If( i <> Len( aCs ), Fwrite( nLinkHandle, ' ' + Alltrim( x ) + If( nWriteFiles % 10 == 0, " //" + CRLF, "" ) ), Fwrite( nLinkHandle, " " + Alltrim( x ) + " $(OB) " + CRLF ) ) } )

            ENDIF

        ENDIF

    ENDIF

    Fwrite( nLinkHandle, "RESFILES =" + CRLF )
    Fwrite( nLinkHandle, "RESDEPEN = $(RESFILES)" + CRLF )
    Fwrite( nLinkHandle, "DEFFILE = " + CRLF )
    Fwrite( nLinkHandle, "HARBOURFLAGS = " + cDefHarOpts + CRLF )

    IF lBcc

        Fwrite( nLinkHandle, "CFLAG1 =  -OS $(CFLAGS) -d -L$(BHC)\lib;$(FWH)\lib -c" + CRLF )
        Fwrite( nLinkHandle, "CFLAG2 =  -I$(BHC)\include;$(BCB)\include" + CRLF )
        Fwrite( nLinkHandle, "RFLAGS = " + CRLF )
        Fwrite( nLinkHandle, "LFLAGS = /P32 /0" + CRLF )
        Fwrite( nLinkHandle, "IFLAGS = " + CRLF )
        Fwrite( nLinkHandle, "LINKER = tlib $(LFLAGS) $(PROJECT)" + CRLF )
        Fwrite( nLinkHandle, " " + CRLF )
        Fwrite( nLinkHandle, "ALLOBJ =  $(OBJFILES) $(OBJCFILES)" + CRLF )
        Fwrite( nLinkHandle, "ALLRES = $(RESFILES)" + CRLF )
        Fwrite( nLinkHandle, "ALLLIB = " + CRLF )
        Fwrite( nLinkHandle, ".autodepend" + CRLF )

    ELSEIF lVcc

        Fwrite( nLinkHandle, "CFLAG1 =  -I$(INCLUDE_DIR) -TP -W3 -nologo $(C_USR) $(CFLAGS)" + CRLF )
        Fwrite( nLinkHandle, "CFLAG2 =  -c" + CRLF )
        Fwrite( nLinkHandle, "RFLAGS = " + CRLF )
        Fwrite( nLinkHandle, "LFLAGS = " + CRLF )
        Fwrite( nLinkHandle, "IFLAGS = " + CRLF )
        Fwrite( nLinkHandle, "LINKER = lib $(PROJECT)" + CRLF )
        Fwrite( nLinkHandle, " " + CRLF )
        Fwrite( nLinkHandle, "ALLOBJ = $(OBJFILES) $(OBJCFILES) " + CRLF )
        Fwrite( nLinkHandle, "ALLRES = $(RESFILES)" + CRLF )
        Fwrite( nLinkHandle, "ALLLIB = " + CRLF )

    ELSEIF lGcc

        Fwrite( nLinkHandle, "CFLAG1 = " + If( At( "linux", Getenv( "HB_ARCHITECTURE" ) ) > 0, "-I$(HB_INC_INSTALL)", " -I$(BHC)/include" ) + " -c -Wall" + CRLF )
        Fwrite( nLinkHandle, "CFLAG2 = " + If( At( "linux", Getenv( "HB_ARCHITECTURE" ) ) > 0, "-L $(HB_LIB_INSTALL)", " -L $(BHC)/lib" ) + CRLF )
        Fwrite( nLinkHandle, "RFLAGS = " + CRLF )
        Fwrite( nLinkHandle, "LFLAGS = " + CRLF )
        Fwrite( nLinkHandle, "IFLAGS = " + CRLF )

        IF At( "linux", Getenv( "HB_ARCHITECTURE" ) ) > 0 .or. cOs == "Linux" .or. At( "linux", Lower( Os() ) ) > 0

            Fwrite( nLinkHandle, "LINKER = ar -M " + CRLF )

        ELSE

            Fwrite( nLinkHandle, "LINKER = $(BCB)\ar -M " + CRLF )

        ENDIF

        Fwrite( nLinkHandle, " " + CRLF )
        Fwrite( nLinkHandle, "ALLOBJ = $(OBJFILES) $(OBJCFILES) " + CRLF )
        Fwrite( nLinkHandle, "ALLRES = $(RESFILES) " + CRLF )
        Fwrite( nLinkHandle, "ALLLIB = $(LIBFILES) " + CRLF )
        Fwrite( nLinkHandle, ".autodepend" + CRLF )

    ENDIF

    Fwrite( nLinkHandle, " " + CRLF )
    Fwrite( nLinkHandle, "#COMMANDS" + CRLF )
    Aeval( aCommands, { | xItem | Fwrite( nLinkHandle, xitem[ 1 ] + CRLF ), Fwrite( nLinkHandle, xitem[ 2 ] + CRLF ), Fwrite( nLinkHandle, " " + CRLF ) } )

    IF lBcc

        Fwrite( nLinkHandle, "#BUILD" + CRLF )
        Fwrite( nLinkHandle, " " + CRLF )
        Fwrite( nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES)" + CRLF )
        Fwrite( nLinkHandle, "    $(BCB)\BIN\$(LINKER) @&&!" + CRLF )
        Fwrite( nLinkHandle, "    $(ALLOBJ)" + CRLF )
        Fwrite( nLinkHandle, "!" + CRLF )

    ELSEIF lVcc

        Fwrite( nLinkHandle, "#BUILD" + CRLF )
        Fwrite( nLinkHandle, "" + CRLF )
        Fwrite( nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES)" + CRLF )
        Fwrite( nLinkHandle, "    $(BCB)\BIN\$(LINKER) @&&!" + CRLF )
        Fwrite( nLinkHandle, "    $(ALLOBJ) " + CRLF )
        Fwrite( nLinkHandle, "!" + CRLF )

    ELSEIF lGcc

        Fwrite( nLinkHandle, "#BUILD" + CRLF )
        Fwrite( nLinkHandle, " " + CRLF )
        Fwrite( nLinkHandle, "$(PROJECT): $(CFILES) $(OBJFILES) " + CRLF )

        IF At( "linux", Getenv( "HB_ARCHITECTURE" ) ) > 0 .or. cOs == "Linux"

            Fwrite( nLinkHandle, "    $(LINKER) @&&!" + CRLF )

        ELSE

            Fwrite( nLinkHandle, "    $(BCB)\$(LINKER) @&&!" + CRLF )

        ENDIF

        Fwrite( nLinkHandle, "    $(PROJECT) " + CRLF )
        Fwrite( nLinkHandle, "    $(ALLOBJ)  " + CRLF )
        Fwrite( nLinkHandle, "!" + CRLF )

    ENDIF

RETURN nil

FUNCTION setlibBuild()

    LOCAL cRead as String
    LOCAL nPos as Numeric
    LOCAL aMacro as Array
    LOCAL aTemp as Array
    LOCAL nCount as Numeric
    LOCAL aCurobjs as Array
    LOCAL nObjPos as Numeric
    LOCAL cLib

    cRead := Alltrim( readln( @leof ) )
    nLinkHandle := Fcreate( clinker )
    szProject := cRead
    amacro    := Listasarray2( cRead, ":" )

    IF Len( amacro ) > 1

        aTemp := Listasarray2( amacro[ 2 ], " " )
        Aeval( aTemp, { | xItem | Aadd( aBuildOrder, xItem ) } )

    ENDIF

    Aadd( aBuildOrder, amacro[ 1 ] )
    cRead := Strtran( cRead, "@&&!", "" )
    amacro := Listasarray2( cRead, '\' )
    Aeval( amacro, { | xMacro | Findmacro( xMacro, @cRead ) } )

    IF lbcc .or. lVcc

        cLinkcomm := cRead + "  @" + cLinker

    ELSE

        cLinkcomm := cRead + " < " + cLinker

    ENDIF

    FOR nPos := 1 TO 7

        cRead  := Alltrim( readln( @leof ) )
        amacro := Listasarray2( cRead, " " )

        FOR ncount := 1 TO Len( amacro )

            IF At( "$", amacro[ nCount ] ) > 0

                IF ( amacro[ nCount ] = "$(PROJECT)" ) .and. lGcc

                    Findmacro( amacro[ nCount ], @cRead )
                    Fwrite( nLinkHandle, "CREATE " + " lib" + cRead + CRLF )
                    cLib := "lib" + cRead

                ELSEIF ( amacro[ nCount ] == "$(ALLOBJ)" )

                    Findmacro( amacro[ nCount ], @cRead )
                    aCurObjs := Listasarray2( cRead, " " )

                    FOR nObjPos := 1 TO Len( aCurObjs )

                        IF lGcc

                            Fwrite( nLinkhandle, "ADDMOD " + aCurObjs[ nObjPos ] + CRLF )

                        ENDIF

                        IF lBcc .or. lVcc

                            IF nObjPos < Len( aCurObjs )

                                Fwrite( nLinkhandle, "+-" + aCurObjs[ nObjPos ] + " &" + CRLF )

                            ELSE

                                Fwrite( nLinkhandle, "+-" + aCurObjs[ nObjPos ] + CRLF )

                            ENDIF

                        ENDIF

                    NEXT

                ENDIF

            ENDIF

        NEXT

    NEXT

    IF lGcc

        Fwrite( nLinkHandle, "SAVE" + CRLF )
        Fwrite( nLinkHandle, "END " + CRLF )

    ENDIF

    Fclose( nLinkhandle )

    IF lLinux

        cLinkComm += " || rm -f " + cLib

    ENDIF

RETURN nil

FUNC FindCfile( citem, aSrcc )

    LOCAL nReturnPos := 0

    nReturnPos := Ascan( aSrcc, { | x | Lower( x[ 1 ] ) == cItem } )

RETURN nReturnPos

#ifndef __HARBOUR__
FUNCTION HB_OSNEWLINE()

    RETURN Chr( 13 ) + Chr( 10 )
#endif

FUNCTION checkiffile( cFile )

    LOCAL cNextLine := ''
    LOCAL cCommand  := ''
    LOCAL cTemp

    cTemp := Substr( cFile, At( " ", cFile ) + 1 )

    IF File( cTemp )

        cNextLine := Trim( Substr( ReadLN( @lEof ), 1 ) )

        IF At( "! ", cNextLine ) > 0

            cCommand := Substr( cNextLine, At( ' ', cNextLine ) + 1 )
            RUN( ccommand )

        ENDIF

        RETURN .T.

    ENDIF

RETURN .F.

FUNCTION checkstdout( cText )

    cText := Strtran( cText, "!stdout", "" )
    Outstd( cText )

RETURN nil

FUNCTION CheckifDef( cTemp )

    LOCAL nPos
    LOCAL cRead    := ""
    LOCAL aSet     := {}
    LOCAL nMakePos

    IF cTemp == "!endif"

        RETURN nil

    ENDIF

    WHILE At( "!endif", cRead ) == 0

        cRead := Trim( Substr( ReadLN( @lEof ), 1 ) )

        IF At( "!endif", cRead ) > 0

            FT_FSKIP( - 1 )
            EXIT

        ENDIF

        cTemp := Strtran( cTemp, "!ifdef ", "" )

        IF At( '=', cRead ) > 0

            IF At( "\..", cRead ) > 0

                cRead := Substr( cRead, 1, At( "\..", cRead ) - 1 )

            ELSEIF At( "/..", cRead ) > 0

                cRead := Substr( cRead, 1, At( "/..", cRead ) - 1 )

            ENDIF

            aSet := Listasarray2( cRead, "=" )
            nPos := Ascan( adefines, { | x | x[ 1 ] == cTemp } )

            IF nPos > 0

                cRead    := Alltrim( Strtran( aset[ 1 ], "$(", "" ) )
                cRead    := Strtran( cRead, ")", "" )
                nMakePos := Ascan( amaCros, { | x | x[ 1 ] == cRead } )

                IF nMakePos == 0

                    Aadd( amacros, { aset[ 1 ], aset[ 2 ] } )

                ENDIF

            ELSE /* Locate For !Else    */

                WHILE At( "!endif", cRead ) == 0

                    cRead := Trim( Substr( ReadLN( @lEof ), 1 ) )

                    IF At( "!else", cRead ) > 0

                        WHILE At( "!endif", cRead ) == 0

                            cRead := Trim( Substr( ReadLN( @lEof ), 1 ) )

                            IF At( "!endif", cRead ) > 0

                                FT_FSKIP( - 1 )
                                EXIT

                            ENDIF

                            aSet := Listasarray2( cRead, "=" )
                            Aadd( amacros, { aset[ 1 ], aset[ 2 ] } )

                        ENDDO

                    ENDIF

                ENDDO

            ENDIF

        ELSEIF At( '!stdout', cRead ) > 0

            checkstdout( cRead )

        ENDIF

    ENDDO

RETURN nil



FUNCTION BuildBorCfgFile()

    LOCAL nCfg

    IF !File( GetMakeDir() + '\bin\harbour.cfg' )

        nCfg := Fcreate( GetMakeDir() + '\bin\harbour.cfg' )
        Fwrite( nCfg, "CC=BCC32" + CRLF )
        Fwrite( nCfg, "CFLAGS= -c " + Replacemacros( "-I$(BHC)\include -OS $(CFLAGS) -d -L$(BHC)\lib" ) + CRLF )
        Fwrite( nCfg, "VERBOSE=NO" + CRLF )
        Fwrite( nCfg, "DELTMP=YES" + CRLF )
        Fclose( nCfg )

    ENDIF

RETURN Nil

FUNCTION BuildMSCCfgFile()

    LOCAL nCfg

    IF !File( GetMakeDir() + '\bin\harbour.cfg' )

        nCfg := Fcreate( GetMakeDir() + '\bin\harbour.cfg' )
        Fwrite( nCfg, "CC=cl" + CRLF )
        Fwrite( nCfg, "CFLAGS= -c " + Replacemacros( "-I$(INCLUDE_DIR) -TP -W3 -nologo $(C_USR) $(CFLAGS)" ) + CRLF )
        Fwrite( nCfg, "VERBOSE=NO" + CRLF )
        Fwrite( nCfg, "DELTMP=YES" + CRLF )
        Fclose( nCfg )

    ENDIF

RETURN Nil

FUNCTION BuildGccCfgFile()

    LOCAL nCfg
    LOCAL cDir := GetMakeDir()
    LOCAL cBhc := Alltrim( Strtran( replacemacros( '$(BHC)' ), '\', '/' ) )

    cDir := Strtran( cDir, '/', '\' )

    IF !File( cdir + '\bin\harbour.cfg' )

        nCfg := Fcreate( cdir + '\bin\harbour.cfg' )
        Fwrite( nCfg, "CC=gcc" + CRLF )
        Fwrite( nCfg, "CFLAGS= -c " + Replacemacros( "-I" + cBhc + "/include $(C_USR)  -L" + cBhc + "/lib" ) + CRLF )
        Fwrite( nCfg, "VERBOSE=NO" + CRLF )
        Fwrite( nCfg, "DELTMP=YES" + CRLF )
        Fclose( nCfg )

    ENDIF

RETURN Nil

FUNCTION BuildGccCfgFileL()

    LOCAL nCfg

    IF !File( '/etc/harbour.cfg' )

        nCfg := Fcreate( '/etc/harbour.cfg' )
        Fwrite( nCfg, "CC=gcc" + CRLF )
        Fwrite( nCfg, "CFLAGS= -c -I/usr/include/harbour" + CRLF )
        Fwrite( nCfg, "VERBOSE=YES" + CRLF )
        Fwrite( nCfg, "DELTMP=YES" + CRLF )
        Fclose( nCfg )

    ENDIF

RETURN Nil

FUNCTION findHarbourcfg( cCfg )

    LOCAL cPath AS STRING := ''
    LOCAL lFound AS LOGICAL := .f.
    LOCAL cEnv AS STRING
    LOCAL aEnv as Array of String
    LOCAL lLinux := At( 'linux', Lower( Os() ) ) > 0
    LOCAL nPos

    IF !lLinux .or. lOs2

        cEnv := Gete( "PATH" ) + ";" + Curdir()
        aEnv := Listasarray2( cEnv, ";" )

        FOR nPos := 1 TO Len( aEnv )

            IF File( aenv[ nPos ] + '\harbour.cfg' )

                cPath  := aenv[ nPos ]
                lFound := .T.
                EXIT

            ENDIF

        NEXT

    ELSE

        IF File( '/etc/harbour.cfg' )

            lFound := .t.
            cPath  := '/etc/harbour.cfg'

        ENDIF

        IF !lfound

            IF File( '/usr/local/etc/harbour.cfg' )

                lFound := .t.
                cPath  := '/usr/local/etc/harbour.cfg'

            ENDIF

        ENDIF

    ENDIF

    cCfg := cPath

RETURN lFound

FUNCTION TestforPrg( cFile )

    LOCAL aFiles AS ARRAY := {}
    LOCAL cPath AS STRING := ''
    LOCAL cTest AS STRING := ""
    LOCAL cDrive AS STRING := ""
    LOCAL cExt AS STRING := ""
    LOCAL cItem AS STRING := ""
    LOCAL aDir AS ARRAY
    LOCAL nPos AS NUMERIC
    LOCAL nFiles AS NUMERIC

    hb_FNAMESPLIT( cFile, @cPath, @cTest, @cExt, @cDrive )
    cExt := Substr( cExt, 2 )
    aDir := Directory( cTest + '.*' )

    FOR nPos := 1 TO 7

        cItem := cTest + "." + extenprg( cExt, nPos )
        Aadd( aFiles, cItem )

    NEXT

    FOR nFiles := 1 TO Len( aFiles )

        nPos := Ascan( aDir, { | a | a[ 1 ] == aFiles[ nFiles ] } )

        IF nPos > 0

            Aadd( aPrgs, aFiles[ nFiles ] )

        ENDIF

    NEXT

RETURN nil

FUNCTION GetGccDir()

    LOCAL cPath AS STRING := ''
    LOCAL cEnv AS STRING
    LOCAL aEnv AS Array of string
    LOCAL nPos as Numeric

    IF lLinux

        cpath := "."

    ELSE

        cEnv := Gete( "PATH" )
        aEnv := Listasarray2( cEnv, ";" )

        FOR nPos := 1 TO Len( aEnv )

            IF File( aenv[ nPos ] + '\gcc.exe' ) .or. File( Upper( aenv[ nPos ] ) + '\GCC.EXE' )

                cPath := aenv[ nPos ]
                cPath := Left( cPath, Rat( '\', cPath ) - 1 )
                EXIT

            ENDIF

        NEXT

    ENDIF

RETURN cPath

FUNCTION ConvertParams( cFile, aFile, p1, p2, p3, p4, p5, p6 )

    LOCAL cParam := ""

    IF !Empty( cFile )

        IF Left( cFile, 1 ) $ "- /"

            cParam += cFile

        ELSE

            cFile := cFile

            Aadd( aFile, cFile )

        ENDIF
    ENDIF

    IF !Empty( p1 )

        IF Left( p1, 1 ) $ "- /"

            cParam += p1

        ELSE

            cFile := p1

            Aadd( aFile, cFile )

        ENDIF

    ENDIF

    IF !Empty( p2 )

        IF Left( p2, 1 ) $ "- /"

            cParam += p2

        ELSE

            cFile := p2
            Aadd( aFile, cFile )

        ENDIF

    ENDIF

    IF !Empty( p3 )

        IF Left( p3, 1 ) $ "- /"

            cParam += p3

        ELSE

            cFile := p3
            Aadd( aFile, cFile )

        ENDIF

    ENDIF

    IF !Empty( p4 )

        IF Left( p4, 1 ) $ "- /"

            cParam += p4

        ELSE

            cFile := p4
            Aadd( aFile, cFile )

        ENDIF

    ENDIF

    IF !Empty( p5 )

        IF Left( p5, 1 ) $ "- /"

            cParam += p5

        ELSE

            cFile := p5
            Aadd( aFile, cFile )

        ENDIF

    ENDIF

    IF !Empty( p6 )

        IF Left( p6, 1 ) $ "- /"

            cParam += p6

        ELSE

            cFile := p6
            Aadd( aFile, cFile )

        ENDIF

    ENDIF

    cParam := Strtran( cParam, "/", "-" )
    cParam := Strtran( cParam, "-elx", "-ELX" )
    cParam := Strtran( cParam, "-el", "-ELX" )
    cParam := Strtran( cParam, "-ex", "-EX" )
    cParam := Strtran( cParam, "-e", "-EX" )
    cParam := Strtran( cParam, "-i", "-I" )
    cParam := Strtran( cParam, "-p", "-P" )
    cParam := Strtran( cParam, "-b", "-B" )
    cParam := Strtran( cParam, "-gl", "-GL" )
    cParam := Strtran( cParam, "-g", "-G" )
    cParam := Strtran( cParam, "-v", "-V" )
    cParam := Strtran( cParam, "-f", "-F" )
    cParam := Strtran( cParam, "-r", "-R" )
    cParam := Strtran( cParam, "-l", "-L" )

   If AT( "-EX" , cParam ) >0 .or. AT( "-ELX" , cParam )  >0

       lEditMode:=.T.

   ENDIF

    IF AT( "-L" , cParam ) > 0

        cDefLang := substr( cParam , AT( "-L" , cParam ) + 2 , 2 )

    ENDIF


RETURN cParam


FUNCTION ShowHelp()

    LOCAL cOs := Upper( Os() )

    ? aLangMessages[ 1 ]
    ? "Copyright 2000,2001,2002 Luiz Rafael Culik <culik@sl.conex.net>"
    ? ""
    ?  aLangMessages[ 2 ]
    ? ""
    ?  aLangMessages[ 3 ]
    ?  aLangMessages[ 4 ]
    ?  aLangMessages[ 5 ]
    ?  aLangMessages[ 6 ]
    ?  aLangMessages[ 7 ]
    ?  aLangMessages[ 8 ]

    IF At( "OS/2", cOs ) > 0

       ?  aLangMessages[ 9 ]
       ?  aLangMessages[ 10 ]
       ?  aLangMessages[ 13 ]

    ELSEIF At( 'LINUX', Upper( cOs ) ) > 0

       ?  aLangMessages[ 9 ]
       ?  aLangMessages[ 12 ]
       ?  aLangMessages[ 14 ]

    ELSE

       ?  aLangMessages[ 11 ]
       ?  aLangMessages[ 12 ]
       ?  aLangMessages[ 13 ]

    ENDIF

    ?  aLangMessages[ 15 ]
    ?  aLangMessages[ 16 ]
    ?  aLangMessages[ 17 ]
    ?  aLangMessages[ 18 ]
    ?  aLangMessages[ 19 ]
    ?  aLangMessages[ 20 ]
    ?  aLangMessages[ 21 ]
    ?  aLangMessages[ 22 ]
    ?  aLangMessages[ 23 ]
    ?  aLangMessages[ 24 ]
    ?  aLangMessages[ 25 ]
    ?  aLangMessages[ 26 ]
    /*
    ?? "Harbour Make Utility"
    ? "Copyright 2000,2001,2002 Luiz Rafael Culik <culik@sl.conex.net>"
    ? ""
    ? "Syntax:  hbmake cFile [options]"
    ? ""
    ? "Options:  /e[x]  Create a new Makefile. If /ex is"
    ? "          used it creates a new make file in extended mode"
    ? "          /el[x]  Create a new Makefile. If /elx is"
    ? "          used it creates a new make file to build a library in extended mode"
    ? "          /D  Define a macro"
    ? "          /p  Print all commands and depencies"
    IF At( "OS/2", cOs ) > 0
        ? "          /b  Use BCC as C compiler"
        ? "          /g+ Use GCC as C compiler"
        ? "          /gl Use GCC as C compiler in Linux"
    ELSEIF At( 'LINUX', Upper( cOs ) ) > 0
        ? "          /b  Use BCC as C compiler"
        ? "          /g  Use GCC as C compiler"
        ? "          /gl+ Use GCC as C compiler in Linux"
    ELSE
        ? "          /b+ Use BCC as C compiler"
        ? "          /g  Use GCC as C compiler"
        ? "          /gl Use GCC as C compiler in Linux"
    ENDIF

    ? "          /v  Use MSVC as C compiler"
    ? "          /f  Force recompiltion of all files"
    ? "          /i  Ignore errors returned by command"
    ? "          /r  Recurse Source Directory"

    ? "          Note: /p and /D can be used together"
    ? "          Note: /r and /e[x]/el[x] can be used together"
    ? "          Options with + are the default values"
    ? "          -D switch can accept multiple macros on the same line"
    ? "          or use one macro per -D switch"
    */
RETURN Nil

FUNCTION ProcessParameters( cParams )

    LOCAL aDef

    IF At( "-F", cParams ) > 0

        lForce  := .T.
        cParams := Strtran( cParams, "-F", "" )

    ENDIF

    IF At( "-B", cParams ) > 0

        lBcc    := .T.
        lGcc    := .F.
        lVcc    := .F.
        cParams := Strtran( cParams, "-B", "" )

    ENDIF

    IF At( "-GL", cParams ) > 0

        lBcc    := .F.
        lGcc    := .T.
        lVcc    := .F.
        lLinux  := .T.
        cParams := Strtran( cParams, "-GL", "" )

    ENDIF

    IF At( "-G", cParams ) > 0

        lBcc    := .F.
        lGcc    := .T.
        lVcc    := .F.
        cParams := Strtran( cParams, "-G", "" )

    ENDIF

    IF At( "-V", cParams ) > 0

        lBcc    := .F.
        lGcc    := .F.
        lVcc    := .T.
        cParams := Strtran( cParams, "-V", "" )

    ENDIF

    IF At( "-I", cParams ) > 0

        lIgnoreErrors := .T.
        cParams       := Strtran( cParams, "-I", "" )

    ENDIF

    IF At( "-R", cParams ) > 0

        lRecurse := .T.
        cParams  := Strtran( cParams, "-R", "" )

    ENDIF

    IF At( "-P", cParams ) > 0

        lPrint  := .t.
        cParams := Strtran( cParams, "-P", "" )

    ENDIF

    IF At( "-D", cParams ) > 0

        cParams := "-D" + Strtran( cParams, "-D", ";" )
        cParams := Strtran( cParams, "-D;", "-D" )

        adef := Listasarray2( Alltrim( Substr( cParams, 3 ) ), ";" )
        Aeval( aDef, { | xDef | If( At( '=', xDef ) > 0, GetParaDefines( xDef ), ) } )

    ENDIF

    IF At( "-EL", cParams ) > 0 .or. At( "-ELX", cParams ) > 0

        IF At( "-ELX", cParams ) > 0

            cParams   := Strtran( cParams, "-ELX", "" )

        ELSE

            cParams := Strtran( cParams, "-EL", "" )

        ENDIF

        lExtended := .T.
        lLibrary  := .T.
        lEditMode := .T.

    ENDIF

    IF At( "-E", cParams ) > 0 .or. At( "-EX", cParams ) > 0

        IF At( "-EX", cParams ) > 0

            cParams := Strtran( cParams, "-EX", "" )

        ELSE

            cParams := Strtran( cParams, "-E", "" )

        ENDIF

        lExtended := .T.
        lEditMode := .T.

    ENDIF

RETURN Nil

FUNCTION WriteMakeFileHeader()

    Fwrite( nLinkHandle, "#BCC" + CRLF )
    Fwrite( nLinkHandle, "VERSION=BCB.01" + CRLF )
    Fwrite( nLinkHandle, "!ifndef BCB" + CRLF )
    Fwrite( nLinkHandle, "BCB = $(MAKEDIR)" + CRLF )
    Fwrite( nLinkHandle, "!endif" + CRLF )
    Fwrite( nLinkHandle, CRLF )
    Fwrite( nLinkHandle, "!ifndef BHC" + CRLF )
    Fwrite( nLinkHandle, "BHC = $(HMAKEDIR)" + CRLF )
    Fwrite( nLinkHandle, "!endif" + CRLF )
    Fwrite( nLinkHandle, " " + CRLF )
    Fwrite( nLinkHandle, "RECURSE="+if(lRecurse," YES "," NO ") + CRLF )    
    Fwrite( nLinkHandle, " " + CRLF )

RETURN nil

FUNCTION BuildLangArray( cLang )

    LOCAL aLang := {}

    DEFAULT cLang TO "EN"


    IF cLang == "EN"

        Aadd( aLang, "Harbour Make Utility" )
        Aadd( alang, "Syntax:  hbmake cFile [options]" )
        Aadd( aLang, "Options:  /e[x]  Create a new Makefile. If /ex is" )
        Aadd( aLang, "          used it creates a new make file in extended mode" )
        Aadd( aLang, "          /el[x]  Create a new Makefile. If /elx is" )
        Aadd( aLang, "          used it creates a new make file to build a library in extended mode" )
        Aadd( aLang, "          /D  Define a macro" )
        Aadd( aLang, "          /p  Print all commands and depedencies" )
        Aadd( aLang, "          /b  Use BCC as C compiler" )
        Aadd( aLang, "          /g+ Use GCC as C compiler" )
        Aadd( aLang, "          /b+ Use BCC as C compiler" )
        Aadd( aLang, "          /g  Use GCC as C compiler" )
        Aadd( aLang, "          /gl Use GCC as C compiler in Linux" )
        Aadd( aLang, "          /gl+ Use GCC as C compiler in Linux" )
        Aadd( aLang, "          /v  Use MSVC as C compiler" )
        Aadd( aLang, "          /f  Force recompiltion of all files" )
        Aadd( aLang, "          /i  Ignore errors returned by command" )
        Aadd( aLang, "          /r  Recurse Source Directory" )
        Aadd( aLang, "          Note: /p and /D can be used together" )
        Aadd( aLang, "          Note: /r and /e[x]/el[x] can be used together" )
        Aadd( aLang, "          Options with + are the default values" )
        Aadd( aLang, "          -D switch can accept multiple macros on the same line" )
        Aadd( aLang, "          or use one macro per -D switch" )
        Aadd( aLang, "          /l[LANGID] Specify the language to be used on hbmake Texts LANGID = (EN/PT/ES) " )
        Aadd( aLang, "          On Windows System, the default will be the SO language if is found" )
        Aadd( aLang, "          Otherwise, will be English. On OS/2;FreeBSD/LINUX the default is English" )
        Aadd( aLang, "Enviroment options" )
        Aadd( aLang, "Select Os" )
        Aadd( aLang, "Select C Compiler" )
        Aadd( aLang, "Graphic Library" )
        Aadd( aLang, "Harbour Options" )
        Aadd( aLang, "Automatic memvar declaration" )
        Aadd( aLang, "Variables are assumed M->" )
        Aadd( aLang, "Debug info" )
        Aadd( aLang, "Suppress line number information" )
        Aadd( aLang, "Generate pre-processed output" )
        Aadd( aLang, "compile module only" )
        Aadd( aLang, "User Defines " )
        Aadd( aLang, "User include Path" )
        Aadd( aLang, "Use External Libs" )
        Aadd( aLang, "Spacebar to select, Enter to continue process" )

    ELSEIF cLang == "ES"

        Aadd( aLang, "Harbour Make Utility  -  Programa Make de Harbour" )
        Aadd( aLang, "Sintaxis:  hbmake cArchivo [opciones]" )
        Aadd( aLang, "Opciones:  /e[x]  Crea un Makefile nuevo. Si se usa /ex" )
        Aadd( aLang, "          se crea un nuevo makefile en modo extendido" )
        Aadd( aLang, "          /el[x]  Crea un Makefile nuevo. Si se usa /elx" )
        Aadd( aLang, "          se crea un nuevo makefile para construir una librería en modo extendido" )
        Aadd( aLang, "          /D  Define una macro" )
        Aadd( aLang, "          /p  Imprime todos los comandos y dependencias" )
        Aadd( aLang, "          /b  Usar BCC como compilador C" )
        Aadd( aLang, "          /g+ Usar GCC como compilador C" )
        Aadd( aLang, "          /b+ Usar BCC como compilador C" )
        Aadd( aLang, "          /g  Usar GCC como compilador C" )
        Aadd( aLang, "          /gl Usar GCC como compilador C en Linux" )
        Aadd( aLang, "          /gl+ Usar GCC como compilador C en Linux" )
        Aadd( aLang, "          /v  Usar MSVC como compilador C" )
        Aadd( aLang, "          /f  Forzar la recompilación de todos los archivos" )
        Aadd( aLang, "          /i  Ignorar los errores devueltos por el comando" )
        Aadd( aLang, "          /r  Recorrer el directorio fuente recursivamente" )
        Aadd( aLang, "          Nota: /p y /D pueden ser usados juntos" )
        Aadd( aLang, "          Nota: /r y /e[x]/el[x] pueden ser usados juntos" )
        Aadd( aLang, "          Las opciones con + son los valores por omisión" )
        Aadd( aLang, "          El parámetro -D puede aceptar múltiples macros en la misma línea" )
        Aadd( aLang, "          ou use uma macro por parƒmetro -D" )
        Aadd( aLang, "          /l[LANGID] especifica a linguagem a ser utilizada nos textos do hbmake LANGID = (EN/PT/ES) " )
        Aadd( aLang, "          Em sistemas Windows, O padrÆo e a linguagem do SO se encontrada" )
        Aadd( aLang, "          SenÆo, sera Ingles. Em OS/2;FreeBSD/LINUX o padrÆo ‚ Ingles" )
        Aadd( aLang, "Opciones de Ambiente")
        Aadd( aLang, "Selecion Os" )
        Aadd( aLang, "Selecion Compilador C" )
        Aadd( aLang, "Lib graphica" )
        Aadd( aLang, "Opciones do Harbour" )
        Aadd( aLang, "Declaracion Automatica de memvar" )
        Aadd( aLang, "Variaveis sÆo assumidas M->" )
        Aadd( aLang, "Info. Debug" )
        Aadd( aLang, "Suprime a info. de numero da linha" )
        Aadd( aLang, "Gene Sa¡da pre-processada" )
        Aadd( aLang, "Compile apenas o modulo " )
        Aadd( aLang, "User Defines " )
        Aadd( aLang, "User include Path" )
        Aadd( aLang, "Usa Libs Externas" )
        Aadd( aLang, "Espa‡o para selecionar, Enter p/ continuar processo" )

    ELSEIF cLang == "PT"

        Aadd( aLang, "Harbour Make Utility  -  Programa Make do Harbour" )
        Aadd( aLang, "Sintaxis:  hbmake cArquivo [op‡äes]" )
        Aadd( aLang, "Op‡äes:  /e[x]  Cria um Makefile novo. Se for usado /ex" )
        Aadd( aLang, "          cria um novo makefile em modo extendido" )
        Aadd( aLang, "          /el[x]  cria un Makefile novo. Se for usado /elx" )
        Aadd( aLang, "          cria um novo makefile para construir una Biblioteca em modo extendido" )
        Aadd( aLang, "          /D  Define uma macro" )
        Aadd( aLang, "          /p  Imprime todos los comandos e dependˆncias" )
        Aadd( aLang, "          /b Usar BCC como compilador C" )
        Aadd( aLang, "          /g+ Usar GCC como compilador C" )
        Aadd( aLang, "          /b+ Usar BCC como compilador C" )
        Aadd( aLang, "          /g  Usar GCC como compilador C" )
        Aadd( aLang, "          /gl Usar GCC como compilador C en Linux" )
        Aadd( aLang, "          /gl+ Usar GCC como compilador C en Linux" )
        Aadd( aLang, "          /v  Usar MSVC como compilador C" )
        Aadd( aLang, "          /f  For‡ar a recompila‡Æo de todos os arquivos" )
        Aadd( aLang, "          /i  Ignora os errores devolvidos pelo comando" )
        Aadd( aLang, "          /r  Recorrer o diret¢rio fonte recursivamente" )
        Aadd( aLang, "          Nota: /p e /D podem ser usados juntos" )
        Aadd( aLang, "          Nota: /r e /e[x]/el[x] podem ser usados juntos" )
        Aadd( aLang, "          As op‡äes com + sÆo os valores padrÆo" )
        Aadd( aLang, "          O parƒmetro -D pode aceitar multiplas macros na mesma linha" )
        Aadd( aLang, "          ou use una macro por parƒmetro -D" )
        Aadd( aLang, "          /l[LANGID] especifica a linguagem a ser utilizada nos textos do hbmake LANGID = (EN/PT/ES) " )
        Aadd( aLang, "          Em sistemas Windows, O padrÆo e a linguagem do SO se encontrada" )
        Aadd( aLang, "          SenÆo, sera Ingles. Em OS/2;FreeBSD/LINUX o padrÆo ‚ Ingles" )
        Aadd( aLang, "Op‡äes de Ambiente")
        Aadd( aLang, "Sele‡Æo Os" )
        Aadd( aLang, "Sele‡Æo Compilador C" )
        Aadd( aLang, "Lib Graf¡ca" )
        Aadd( aLang, "Op‡äes do Harbour" )
        Aadd( aLang, "Declara‡Æo Autom tica de memvar" )
        Aadd( aLang, "Variaveis sÆo assumidas M->" )
        Aadd( aLang, "Info. Debug" )
        Aadd( aLang, "Suprime a info. de numero da linha" )
        Aadd( aLang, "Gene Sa¡da pre-processada" )
        Aadd( aLang, "Compile apenas o modulo " )
        Aadd( aLang, "User Defines " )
        Aadd( aLang, "User include Path" )
        Aadd( aLang, "Usa Libs Externas" )
        Aadd( aLang, "Espa‡o para selecionar, Enter p/ continuar processo" )

    ENDIF

    RETURN aLang

