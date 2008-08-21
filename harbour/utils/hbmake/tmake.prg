/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * File generator for hbmake
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
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
 */

#include "hbclass.ch"
#include "common.ch"

STATIC s_lEof := .F.

CLASS THBMAKE

   EXPORTED:

   DATA  aDefines       Init  {}
   DATA  aBuildOrder    Init  {}
   DATA  aCommands      Init  {}
   DATA  aMacros        Init  {}
   DATA  aPrgs          Init  {}
   DATA  aExtLibs       Init  {}
   DATA  aCs            Init  {}
   DATA  aObjs          Init  {}
   DATA  aObjsc         Init  {}
   DATA  aRes           Init  {}
   DATA  nLinkHandle 
   DATA  cLinkcomm      Init  ""
   DATA  lCompress      Init .F.
   DATA  lForce         Init .F.
   DATA  lLinux         Init .F.
   DATA  szProject      Init ""
   DATA  lLibrary       Init .F.
   DATA  lInstallLib    Init .F.
   DATA  lIgnoreErrors  Init .F.
   DATA  lExtended      Init .T.
   DATA  lOs2           Init .F.
   DATA  lRecurse       Init .F.
   DATA  lEditMode      Init .F.
   DATA  aDir
   DATA  aLangMessages  init {}
   DATA  cDefLang
   DATA  lFwh           init .F.
   DATA  lxFwh          init .F.
   DATA  lCw            init .F.
   DATA  lMini          init .F.
   DATA  lHwgui         init .F.
   DATA  lGui           Init .F.
   DATA  lGtwvt         init .F.
   DATA  lGtwvw         init .F.
   DATA  lMWvW          init .F.
   DATA  lXWT           init .F.
   DATA  lxHGtk         init .F.
   DATA  lWhoo          init .F.
   DATA  lWhat32        init .F.
   DATA  lRddAds        init .F.
   DATA  lMediator      init .F.
   DATA  cMakefile      init ""
   DATA  lExternalLib   init .F.
   DATA  cObj           init ""
   DATA  cUserdef       init ""
   DATA  cUserInclude   init ""
   DATA  cUserLib       init ""
   DATA  lGenppo        init .F.
   DATA  lCompMod       init .F.
   DATA  lAutomemvar    init .F.
   DATA  lvarismemvar   init .F.
   DATA  ldebug         init .F.
   DATA  lSupressline   init .F.
   DATA  StartPath      init ""
   DATA  cFmc           init ""
   DATA  cMedpath       init ""
   DATA  cAppLibName    init ""
   DATA  cOs            init ""
   DATA  cTopfile       init ""
   DATA  aOut           init {}
   DATA  cFilesToAdd    init 5
   DATA  lMT            init .F.
   DATA  cWarningLevel  init 0
   DATA  cTopModule     init ""
   DATA  cRes           init ""
   DATA  cMacro         init ""
   DATA  lGenCsource    init .F.      // Ath added 31-05-2006
   DATA  cShell         init ""
   DATA  cEditor        init ""

   METHOD New()
   METHOD ReadMakefile(cFile)
   METHOD ReplaceMacros( cMacros )
   METHOD FindMacro( cMacro, cRead ) 

ENDCLASS

METHOD New() CLASS THbMake

   ::cObj           := "obj" + Space( 40 )
   ::cUserdef       := Space( 200 )
   ::cUserInclude   := Space( 200 )
   ::cUserLib       := Space( 200 )
   ::cFMC           := Space( 200 )
   ::cAppLibName    := Space( 20 )
   ::cTopModule     := Space( 20 )
   ::cEditor        := ""

   return self


METHOD ReadMakefile(cFile) CLASS THbMake

   LOCAL cBuffer     := {}
   LOCAL cMacro      := ::cMacro
   LOCAL cDep        := "#DEPENDS"
   LOCAL cOpt        := "#OPTS"
   LOCAL cCom        := "#COMMANDS"
   LOCAL cBuild      := "#BUILD"
   LOCAL cTemp       := ""
   LOCAL cTemp1      := ""
   LOCAL aTemp       := {}
   LOCAL lMacrosec   := .F.
   LOCAL lBuildSec   := .F.
   LOCAL lComSec     := .F.
   LOCAL aTemp1      := {}
   LOCAL cCfg        := ""
   LOCAL lCfgFound   := .F.
   LOCAL aTempCFiles := {}
   Local nHandle
   Local cObjitem
   Local cRes        := ""
   Local cItem
   LOCAL lLinux      := At( "linux", Lower( Os() ) ) > 0
   Local lExtended   := .T., szProject
   LOCAL lPrgObjRule := .F.

   nHandle := FT_FUSE( cFile )
   IF nHandle < 0
      RETURN self
   ENDIF
   cBuffer := Trim( ReadLN( @s_lEof ) )
   ::lLibrary :=.F.

   WHILE !s_lEof

      IF At( cMacro, cBuffer ) > 0
         lMacroSec := .T.
         lBuildSec := .F.
         lComSec   := .F.
      ELSEIF At( cBuild, cBuffer ) > 0
         lMacroSec := .F.
         lBuildSec := .T.
         lComSec   := .F.
      ELSEIF At( cCom, cBuffer ) > 0
         lBuildSec := .F.
         lComSec   := .T.
         lMacroSec := .F.
      ELSE
         ? "Invalid Make File"
         Fclose( nHandle )
         RETURN Nil
      ENDIF

      cTemp := Trim( ReadLN( @s_lEof ) )

      IF At( "//", ctemp ) > 0

         WHILE At( "//", ctemp ) > 0
            ctemp := Strtran( ctemp, " //", "" )
            cTemp += Trim( ReadLN( @s_lEof ) )
         ENDDO

         ctemp := Strtran( ctemp, " //", "" )
      ENDIF

      aTemp := ListasArray2( Alltrim( cTemp ), "=" )

      IF lmacrosec

         IF Alltrim( Left( ctemp, 7 ) ) <> "!ifndef" .and. Alltrim( Left( ctemp, 6 ) ) <> "!endif" .and. Alltrim( Left( ctemp, 7 ) ) <> "!iffile" .and. Alltrim( Left( ctemp, 7 ) ) <> "!stdout" .and. Alltrim( Left( ctemp, 6 ) ) <> "!ifdef"

            IF Len( aTemp ) > 1

               IF At( "$", aTemp[ 2 ] ) > 0
                  Aadd( ::aMacros, { aTemp[ 1 ], ::replacemacros( aTemp[ 2 ] ) } )
               ELSE
                  Aadd( ::aMacros, { aTemp[ 1 ], aTemp[ 2 ] } )
//                tracelog(aTemp[ 1 ], aTemp[ 2 ])
               ENDIF
            ENDIF

            IF aTemp[ 1 ] == "PROJECT"
               ::cAppLibName := aTemp[ 2 ]
               ::cAppLibName := strtran(::cAppLibName ,"$(PR)","")
               ::cAppLibName := strtran(::cAppLibName ,".exe","")
               ::cAppLibName := strtran(::cAppLibName ,".lib","")
            ENDIF

            IF aTemp[ 1 ] == "LIBFILES"
               ::lRddAds :=  "rddads" $ aTemp[ 2 ]
            ENDIF

            IF aTemp[ 1 ] == "C4W"
               ::cFMC := aTemp[2]
               ::lCw := .T.
            endif

            IF aTemp[ 1 ] == "FWH"
               ::cFMC := aTemp[2]
               ::lFwh := .T.
            endif

            IF aTemp[ 1 ] == "MINIGUI"
               ::cFMC := aTemp[2]
               ::lmini := .T.
            endif

            IF aTemp[ 1 ] == "HWGUI"
               ::cFMC := aTemp[2]
               ::lHwGui := .T.
            endif

            IF aTemp[ 1 ] == "GTWVT"
               ::cFMC := ""
               ::lGtwvt := .T.
            endif

            IF aTemp[ 1 ] == "GTWVW"
               ::cFMC := ""
               ::lGtwvw := .T.
            endif
            IF aTemp[ 1 ] == "MWVW"
               ::cFMC := ""
               ::lGtwvw := .T.
               ::lMWvW  := .T.
            endif

            IF aTemp[ 1 ] == "XWT"
               ::cFMC := ""
               ::lXWT := .T.
            endif

            IF aTemp[ 1 ] == "WHOO"
               ::cFMC := aTemp[2]
               ::lWhoo := .T.
            endif

            IF aTemp[ 1 ] == "WHAT32"
               ::cFMC := aTemp[2]
               ::lWhat32 := .T.
            endif

            IF aTemp[ 1 ] == "XHGTK"
               ::cFMC := aTemp[2]
               ::lxHGtk := .T.
            endif

            IF aTemp[ 1 ] == "MEDIATOR"
               ::cMedpath := aTemp[2]
               ::lmEDIATOR := .T.
            endif

            IF aTemp[ 1 ] == "COMPRESS"
               ::lCompress := "YES" $ aTemp[ 2 ]
            endif

            IF aTemp[ 1 ] == "SHELL"
               ::cShell := aTemp[ 2 ]
            endif

            IF aTemp[ 1 ] == "EXTERNALLIB"
               ::lExternalLib := "YES" $ aTemp[ 2 ]
            endif

            IF aTemp[ 1 ] == "XFWH"
               ::lxFwh := "YES" $ aTemp[ 2 ]
            endif

            IF aTemp[ 1 ] == "FILESTOADD"
               ::cFilesToAdd := Val( aTemp[ 2 ] )
            endif

            IF aTemp[ 1 ] == "MT"
               ::lMt := "YES" $ aTemp[ 2 ]
            endif

            IF aTemp[ 1 ] == "GUI"
               ::lGUI := "YES" $ aTemp[ 2 ]
            endif

            IF aTemp[ 1 ] == "WARNINGLEVEL"
               ::cWarningLevel := Val( aTemp[ 2 ] )
            endif

            IF aTemp[ 1 ] == "OBJFILES"
                cObjitem := Left( atemp[ 2 ], at(")",atemp[ 2 ]))
    
               ::cObj := ::replacemacros(cObjItem)
               ::aObjs := Listasarray2( ::replacemacros( atemp[ 2 ] ), " " )
            ENDIF

            IF aTemp[ 1 ] == "OBJCFILES"

               aTemp1 := Listasarray2( ::replacemacros( atemp[ 2 ] ), " " )

               IF Len( atemp1 ) == 1

                  IF !Empty( atemp[ 1 ] )
                     ::aObjsC := Listasarray2( ::replacemacros( atemp[ 2 ] ), " " )
                  ENDIF
               ELSE
                  ::aObjsC := Listasarray2( ::replacemacros( atemp[ 2 ] ), " " )
               ENDIF
            ENDIF

            IF aTemp[ 1 ] == "PRGFILES"
               ::aPrgs     := Listasarray2( ::replacemacros( atemp[ 2 ] ), " " )
               lExtended := .T.
            ENDIF

            IF aTemp[ 1 ] == "PRGFILE"
                ::aPrgs := Listasarray2( ::replacemacros( atemp[ 2 ] ), " " )
            ENDIF

            IF atemp[ 1 ] == "CFILES"

               IF lExtended

                  aTempCFiles := Listasarray2( ::replacemacros( atemp[ 2 ] ), " " )

                  IF ( Len( aTempCFiles ) == 1 )

                     IF !Empty( aTempCFiles[ 1 ] )
                        ::aCs := Listasarray2( ::replacemacros( atemp[ 2 ] ), " " )
                     ENDIF
                  ELSE
                     ::aCs := Listasarray2( ::replacemacros( atemp[ 2 ] ), " " )
                  ENDIF
               ELSE
                  ::aCs := Listasarray2( ::replacemacros( atemp[ 2 ] ), " " )
               ENDIF
            ENDIF

            IF aTemp[ 1 ] == "EXTLIBFILES"
               ::aExtLibs  := Listasarray2( ::replacemacros( atemp[ 2 ] ), " " )
            ENDIF


            IF atemp[ 1 ] == "RESFILES"

               ::aRes := Listasarray2( ::replacemacros( atemp[ 2 ] ), " " )

               FOR EACH cItem in :: aRes
                  ::cRes += cItem +" "
               NEXT
            ENDIF

            IF aTemp[ 1 ] == "RECURSE"
               ::lRecurse := AT( "YES" , aTemp[ 2 ] ) > 0
            ENDIF

            IF aTemp[ 1 ] == "LIBRARY"
               ::lLibrary := AT( "YES", aTemp[ 2 ] ) > 0
            ENDIF  

            IF aTemp[ 1 ] == "INSTALLLIB"
               ::lInstallLib := AT( "YES", aTemp[ 2 ] ) > 0
            ENDIF  

            IF aTemp[ 1 ] ==  "HARBOURFLAGS"

               ::lGenppo        := AT( "-p" , aTemp[ 2 ] ) > 0
               ::lCompMod       := AT( "-m" , aTemp[ 2 ] ) > 0
               ::lAutomemvar    := AT( "-a" , aTemp[ 2 ] ) > 0
               ::lvarismemvar   := AT( "-v" , aTemp[ 2 ] ) > 0
               ::ldebug         := AT( "-b" , aTemp[ 2 ] ) > 0
               ::lSupressline   := AT( "-l" , aTemp[ 2 ] ) > 0
               aTemp[ 2 ] := strtran(aTemp[ 2 ],"-p","")
               aTemp[ 2 ] := strtran(aTemp[ 2 ],"-m","")
               aTemp[ 2 ] := strtran(aTemp[ 2 ],"-a","")
               aTemp[ 2 ] := strtran(aTemp[ 2 ],"-v","")
               aTemp[ 2 ] := strtran(aTemp[ 2 ],"-b","")
               aTemp[ 2 ] := strtran(aTemp[ 2 ],"-l","")
               aTemp[ 2 ] := Alltrim( aTemp[ 2 ] )

            endif
              
            IF aTemp[ 1 ] == "USERDEFINE"
               ::cUserDef := aTemp[ 2 ]
            endif
        
            IF aTemp[ 1 ] == "USERINCLUDE"
               ::cUserInclude := aTemp[ 2 ]
            endif

            IF aTemp[ 1 ] == "USERLIBS"
               ::cUserLib := aTemp[ 2 ]
            endif

            IF aTemp[ 1 ] == "TOPMODULE"
               ::cTopModule := aTemp[ 2 ]
            endif

            IF aTemp[ 1 ] == "EDITOR"
               ::cEditor := aTemp[ 2 ]
            endif
         ENDIF
      ENDIF

      IF lbuildSec
         szProject := cTemp
         ::aBuildOrder := Listasarray2( cTemp, ":" )
      ENDIF

      IF lComSec        // Ath added 31-05-2006

         IF lPrgObjRule
            ::lGenCsource := "-go3" $ LOWER(cTemp)
            lPrgObjRule := .F.
         ENDIF
         IF aTemp[ 1 ] == ".prg.obj:" .OR. aTemp[ 1 ] == ".prg.o:"
            lPrgObjRule := .T.
         ENDIF
      ENDIF               // end Ath added 31-05-2006
      
      IF cTemp == "#BUILD" .OR. cTemp == "#COMMANDS"
         cBuffer := cTemp
      ENDIF
   ENDDO

   qout( nhandle)
   Fclose( nHandle )
   RETURN self

METHOD ReplaceMacros( cMacros ) CLass THBMAKE

   LOCAL nCount       := 0
   LOCAL aTempMacros  := {}
   LOCAL aLocalMacros := {}

   aTempMacros := Listasarray2( cMacros, " " )

   AEval( aTempMacros, { | xMacro | iif( At( "$", xMacro ) > 0, ;
                         iif( At( ";", xMacro ) > 0, ( aLocalMacros := Listasarray2( xMacro, ";" ), ;
                         Aeval( aLocalMacros, { | x | ::FindMacro( x, @cMacros ) } ) ), ;
                         ::FindMacro( xMacro, @cMacros ) ), ) } )

   RETURN cMacros

METHOD FindMacro( cMacro, cRead ) CLASS THBMAKE
   LOCAL nPos
   LOCAL cTemp
   LOCAL aLocalMacros := {}

   cMacro := Left( cMacro, At( ")", cMacro ) )

   IF At( "-", cMacro ) > 0
      cMacro := Substr( cMacro, 3 )
   ENDIF

   IF At( ";", cMacro ) > 0
      cMacro := Substr( cMacro, At( ";", cMacro ) + 1 )
   ENDIF

   nPos := Ascan( ::aMacros, { | x | "$(" + Alltrim( x[ 1 ] ) + ")" == cMacro } )

   IF nPos == 0

      cTemp := Strtran( cMacro, "$(", "" )
      cTemp := Strtran( cTemp, ")", "" )

      IF !Empty( cTemp )
         cRead := Alltrim( Strtran( cRead, cMacro, Gete( cTemp ) ) )
      ENDIF
   ELSE
      cRead := Alltrim( Strtran( cRead, cMacro, ::aMacros[ nPos, 2 ] ) )
   ENDIF

   RETURN cRead
