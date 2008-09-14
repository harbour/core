/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * xHarbour make utility main file
 *
 * Copyright 2000-2007 Luiz Rafael Culik <culikr@uol.com.br>
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

/*
 * Modified By Sandro Freire sandrorrfreire at yahoo.com.br
 * added support for Unix OS
 */

#include "hbclass.ch"

#include "achoice.ch"
#include "common.ch"
#include "directry.ch"
#include "fileio.ch"
#include "inkey.ch"

#define HB_FEOF  -1        // hb_FReadLine() returns -1 on End-of-File (EOF)

#define BLD_ALL   2        /* Rebuild All -F        */
#define BLD_SOME  1        /* rebuild changed files */
#define BLD_NONE  0        /* don't build           */
#define LANG_PT   1        /* portuguese brazilian  */
#define LANG_EN   2        /* english               */
#define LANG_ES   3        /* spanish               */
#define PGM_ERR  -1        // Return value for an abnormal exit
#define PGM_OK    0        // Return value for a normal exit
#define PGM_QUIT  1        // Quit before making file but without error

#Define HBM_USE_DEPENDS    // Set this to have section #DEPENDS parsed like RMake, Ath 2004-06
                           // An extra parameter is added to FileIsNewer() to have it check the INCLUDE paths also
                           // Interactive mode asks whether sources should be scanned for dependencies (#include, set procedure to, set proc to

/*
Beginning Static Variables Table
Default Values for core variables are set here
New Core vars should only be added on this section
*/

STATIC s_lErrors         := .F.
STATIC s_lPrint          := .F.
STATIC s_lEdit           := .F.
STATIC s_aDefines        := {}
STATIC s_aBuildOrder     := {}
STATIC s_aCommands       := {}
STATIC s_aMacros         := {}
STATIC s_aPrgs           := {}
STATIC s_aContribLibs    := {}
#IfDef HBM_USE_DEPENDS
STATIC s_aDepends        := {}
#Endif
STATIC s_aCFiles         := {}            // array of C source files
STATIC s_aObjs           := {}
STATIC s_aObjsc          := {}
STATIC s_aSrcPaths       := {}
STATIC s_lEof            := .F.
STATIC s_aResources      := {}
STATIC s_nMakeFileHandle := F_ERROR       // File starts closed
STATIC s_cLinkFile       := ""            // Will be set to [s_cAppName].lnk
STATIC s_cLinkCommands   := ""
STATIC s_lLinux          := .F.
STATIC s_lUnix           := .F.
STATIC s_lOS2            := .F.
STATIC s_lWin32          := .F.
STATIC s_lBcc            := .F.           // Borland C compiler
STATIC s_lPocc           := .F.           // Pelles C compiler
STATIC s_lMSVcc          := .F.           // MS-Visual C compiler
STATIC s_lGcc            := .F.           // GNU C compiler
STATIC s_lMinGW          := .F.           // MinGW C compiler
STATIC s_lForce          := .F.           // "-f" flag
STATIC s_szProject       := ""
STATIC s_lLibrary        := .F.
STATIC s_lIgnoreErrors   := .F.
STATIC s_lRecursive      := .F.
STATIC s_lCancelRecursive:= .F.
STATIC s_lEditMake       := .F.
STATIC s_lCompress       := .F.
STATIC s_lContribLib     := .F.
STATIC s_aDir
STATIC s_aLangMessages   := {}
STATIC s_cAppName        := ""
STATIC s_cDefLang
STATIC s_cLogFile        := ""            // log file name.
STATIC s_cMap            := ""            // map file name. For borland c
STATIC s_cTds            := ""            // state file name. For borland c
STATIC s_lGenPpo         := .F.
STATIC s_nLang           := LANG_EN       // default language is english
STATIC s_lMt             := .F.
STATIC s_cUserDefine     := ""
STATIC s_cUserInclude    := ""
STATIC s_cUserLibs       := ""            // user libs located other than "harbour\lib"
STATIC s_lxFwh           := .F.
STATIC s_nFilesToAdd     := 5
STATIC s_nWarningLevel   := 0
STATIC s_lAsDll          := .F.
STATIC s_cAlertMsg       := ""
STATIC s_cHarbourCfg     := "harbour.cfg" // don't change this file name.
STATIC s_cObjDir         := "obj"
STATIC s_lGui            := .F.
STATIC s_cEditor         := ""
STATIC s_cHarbourDir     := ""
STATIC s_lGenCsource     := .F.  // Generate PCode by default // Ath added 31-05-2006
STATIC s_cInvalidExt     := {".prg",".c",".cpp",".ch",".h",".ppo",".bat",".doc",".txt",".dbf",".ntx",".cdx",".dbt",".fpt",".mem"}
STATIC s_aEOL                    // for hb_FReadLine() array of End-of-Line (EOF) array
STATIC s_cEOL                    // EOL terminator used by system compiled on

*---------------------------------------------
FUNCTION MAIN()
*---------------------------------------------

   LOCAL cFile       := ""
   LOCAL cOldColor   := SETCOLOR()
   LOCAL nBuildType  := 0
   LOCAL aFile       := {}
   LOCAL cScreen
   LOCAL aDef        := {}
   LOCAL cMakeParams := ""
   LOCAL nLang       := GETUSERLANG()
   LOCAL cExt := ""
   LOCAL cExp,cLib
   LOCAL lCreateAndCompile := .F.  // not used but must be set to .f. for now
   LOCAL i, n, nRow

   IF ( PCount() == 0 ) .or. ( CmdLineParam( @cFile, @cMakeParams ) == PGM_ERR )
      ShowHelp()
      RETURN PGM_ERR
   ENDIF

   SET SCORE OFF
   SET CENTURY ON

   cExt := SubStr( cFile, At(".",cFile) )

   // TOFIX: Needs to be case sensitive for non-WinDOS [vszakats]
   IF ! Empty(cExt) .AND. AScan( s_cInvalidExt, {|x| x == lower(cExt) } ) > 0
      showhelp( "Invalid extension for hbmake script file <"+cFile+">." )
      Return PGM_ERR
   ENDIF

   s_cHarbourDir := GetHarbourDir()

   IF Empty( s_cHarbourDir )
      IF s_nlang     == LANG_PT        /* portuguese brazilian */
         s_cAlertMsg := "hbmake necessita do harbour bin no path."
      ELSEIF s_nLang == LANG_ES        /* spanish              */
         s_cAlertMsg := "hbmake necessita de lo harbour bin en lo path."
      ELSE                             /* english              */
         s_cAlertMsg := "hbmake needs harbour bin in the path."
      ENDIF

      @ ROW() + 1, 0 say s_cAlertMsg

      aFile := ListAsArray2( GETENV( "PATH" ), ";" )
      n := 0                           /* Show Path params     */
      nRow:= ROW() +1
      @ nRow, 2 say "Current Path: "
      FOR i:= 1 TO LEN( aFile ) - 1

         @ nRow, COL() + 2 say aFile[i]
         IF COL() + LEN( aFile[ i+1 ] ) + 2 > MAXCOL()
            SETPOS( ++nRow, 0 )
         ENDIF

      NEXT i

      IF COL() + 2 + LEN( aFile[ i ] ) > MAXCOL()
          SETPOS( ++nRow, 0 )
      ENDIF

      @ nRow, COL() + 2 say aFile[i]
      @ nRow+1, 0 say ""

      RETURN PGM_ERR
   ENDIF

   IF s_nlang == LANG_PT               /* portuguese brazilian */
      SET DATE BRITISH
   ELSE                                /* english or spanish   */
      SET DATE ANSI
   ENDIF

   IF s_lForce .and. !File( cFile )
      IF s_nlang     == LANG_PT        /* portuguese brazilian */
         s_cAlertMsg := "Arquivo <" + cFile + "> n∆o encontrado."
      ELSEIF s_nLang == LANG_ES        /* spanish              */
         s_cAlertMsg := "Fichero <" + cFile + "> no encontrado."
      ELSE                             /* english              */
         s_cAlertMsg := "File <" + cFile + "> not found."
      ENDIF

      showhelp( s_cAlertMsg )
      RETURN PGM_ERR
   ENDIF

   s_aLangMessages := BuildLangArray()
   s_cAppName      := Substr( cFile,1 , AT(".",cFile) -1)
   s_cLinkFile     := s_cAppName + ".lnk"
   s_cLogFile      := s_cAppName + ".log"

   cExp            := s_cAppName + ".exp"
   cLib            := s_cAppName + ".lib"

   FERASE( s_cLinkFile )
   FErase( s_cAppName + ".out" )
   FErase( s_cLogFile )

   IF ! s_lGenppo
      FERASE( s_cAppName + ".ppo" )
   ENDIF


   if s_lBcc
      /* if you need of these files, comment the lines below. */
      s_cMap    := s_cAppName + ".map"
      s_cTds    := s_cAppName + ".tds"
      FErase( s_cMap )
      FErase( s_cTds )
   endif

   /* Edit / Create MakeFile */

   IF s_lEdit .or. ! s_lForce

      IF ! File( cFile )
         lCreateAndCompile := ( "-C" $ cMakeParams  )
      ELSE
         lCreateAndCompile := .f.
         cMakeParams := StrTran( cMakeParams, "-C","" )
      ENDIF

      nBuildType := IIF( s_lLibrary, CreateLibMakeFile( cFile ), ;
                         /* Else */  CreateMakeFile( cFile, lCreateAndCompile  ) )

      DO CASE
      CASE nBuildType == PGM_ERR
         SET CURSOR on
         SETCOLOR( cOldColor )
         @ ROW() +1, 0 say "Error createing link file: " + s_cLinkFile
         RETURN PGM_ERR

      CASE nBuildType == BLD_NONE
         SET CURSOR on
         SETCOLOR( cOldColor )
         RETURN PGM_OK

      CASE nBuildType == BLD_SOME
         s_lForce := .F.

      CASE nBuildType == BLD_ALL
          s_lForce := .T.

      ENDCASE

   ENDIF

   // Compile MakeFile...

   // Make file is parsed here
   CLEAR SCREEN
   qqout( hbMakeID() )
   qout( hbMakeCopyright() )

   IF s_lBcc .OR. s_lPocc .OR. s_lMSVcc
      qout( version() + " / " + HB_Compiler() )
   ENDIF


   IF ParseMakeFile( cFile ) == PGM_ERR

      IF s_nLang     == LANG_PT        /* portuguese brazilian */
         s_cAlertMsg := "<" + cFile + "> n∆o pode ser aberto. FERROR(" + Ltrim(Str(FError())) + "). O HbMake ser† fechado."
      ELSEIF s_nLang == LANG_ES        /* spanish              */
         s_cAlertMsg := "<" + cFile + "> no pode ser abierto. FERROR(" + Ltrim(Str(FError())) + "). Lo HbMake ser† cerrado."
      ELSE                             /* english             */
         s_cAlertMsg := "<" + cFile + "> cannot be opened. FERROR(" + Ltrim(Str(FError())) + "). HbMake will close."
      ENDIF

      showhelp( s_cAlertMsg )
      RETURN PGM_ERR

   ENDIF

   IF s_lPrint
      PrintMacros()
   ENDIF

   set cursor off

   IF !hb_DirExists( s_cObjDir )
#ifdef HB_COMPAT_C53
      MakeDir( s_cObjDir )
#endif
   ENDIF

   IF s_lForce
      IF CompileFiles() == PGM_ERR
         RETURN PGM_ERR
      ENDIF
   ELSE
      IF CompileUpdatedFiles() == PGM_ERR
         RETURN PGM_ERR
      ENDIF
   ENDIF

   /* clean up screen */

   @ 4, 0 CLEAR TO MAXROW(), MAXCOL()
   @ 4, 5 say "Compiling: Finished"
   @ 5, 5 say "Linking ..."

   cScreen   := SAVESCREEN()
   s_lErrors := IIF( hb_run( s_cLinkCommands ) != 0, .T., .F. )
   RESTSCREEN(,,,, cScreen )
   @ 5, 5 say "Linking  : Finished"

   IF s_lCompress .AND. !s_lLibrary
      setpos(9,0)

      s_lErrors := IIF( hb_run( " upx -9 " + (s_cAppName) ) != 0, .T., .F. )

   ENDIF

   IF s_lasdll .or. lower(right(s_cAppName,4)) == ".dll"

       s_lErrors := IIF ( hb_run( ReplaceMacros("implib $(HB_DIR)\lib\" + ;
                       left( s_cAppName, at( ".", s_cAppName ) - 1 ) + ".lib " + ;
                       s_cAppName ) ), .T., .F. )
   ENDIF

   if s_lBcc
      /*
      NOTE: The TDS file is always created by borland linker.
      If you need of this file, comment the lines below and
      remove "-x" flag that is created by hbmake in the
      LFLAGS statment for Borland compiler.
      */
      FErase( s_cMap )
      FErase( s_cTds )
   endif

   IF s_lPocc
      FErase( cExp )
      IF !s_lLibrary
         FErase( cLib )
      ENDIF
   ENDIF

   SET CURSOR ON

RETURN PGM_OK

*------------------------------
FUNCTION ParseMakeFile( cFile )
*------------------------------

   LOCAL cBuffer
   LOCAL cMacro      := iif(s_lMSVcc,"#MSVC",iif(s_lPocc,"#POCC",iif(s_lGcc,"#GCC","#BCC")))
   LOCAL cDep        := "#DEPENDS"
   LOCAL cOpt        := "#OPTS"
   LOCAL cCom        := "#COMMANDS"
   LOCAL cBuild      := "#BUILD"
   LOCAL cTemp       := ""
   LOCAL cTemp1      := ""
   LOCAL aTemp       := {}
   LOCAL lMacrosec   := .T.
   LOCAL lBuildSec   := .F.
   LOCAL lComSec     := .F.
#IFDEF HBM_USE_DEPENDS
   LOCAL lDepSec     := .F.
#ENDIF
   LOCAL aTemp1      := {}
   LOCAL cCfg        := ""
   LOCAL lCfgFound   := .F.
   LOCAL aTempCFiles := {}
   LOCAL lLinux      :=  s_lLinux
   LOCAL lUnix       :=  s_lUnix
   LOCAL aLib
   LOCAL aLibx
   LOCAL lDjgpp      := "GNU C" $ HB_COMPILER()
   LOCAL x           := 1
   LOCAL nFHandle
   LOCAL cTrash :=""

   nFHandle := FOpen( cFile, FO_READ )

   IF nFHandle < 0
      RETURN PGM_ERR
   ENDIF

   #IFndef __PLATFORM__WINDOWS
      IF !FILE("hbtemp.c")
         CreateLink()
      ENDIF
   #ENDIF

   s_lEof  := (hb_FReadLine( nFHandle, @cBuffer, s_aEOL ) == HB_FEOF)
   cBuffer := Trim( cBuffer )

   AAdd( s_aDefines, { "HARBOUR_DIR", s_cHarbourDir } )

   IF s_lBcc
      AAdd( s_aDefines, { "MAKE_DIR", GetBccDir() } )
   ELSEIF s_lGcc
      AAdd( s_aDefines, { "MAKE_DIR", GetGccDir() } )
   ELSEIF s_lMSVcc
      AAdd( s_aDefines, { "MAKE_DIR", GetVccDir() } )
   ELSEIF s_lPocc
      AAdd( s_aDefines, { "MAKE_DIR", GetPoccDir() } )
   ENDIF

   WHILE ! s_lEof

      IF cMacro $ cBuffer
         lMacroSec := .T.
         lBuildSec := .F.
         lComSec   := .F.
#IFDEF HBM_USE_DEPENDS
         lDepSec   := .F.
#Endif
      ELSEIF  cBuild $ cBuffer
         lMacroSec := .F.
         lBuildSec := .T.
         lComSec   := .F.
#IfDef HBM_USE_DEPENDS
         lDepSec   := .F.
#Endif
      ELSEIF  cCom $ cBuffer
         lBuildSec := .F.
         lComSec   := .T.
         lMacroSec := .F.
#IfDef HBM_USE_DEPENDS
         lDepSec   := .F.
      ELSEIF  cDep $ cBuffer
         lBuildSec := .F.
         lComSec   := .F.
         lMacroSec := .F.
         lDepSec   := .T.
#Endif
      ELSE
         ? "Invalid Make File"
         FClose( nFHandle )
         RETURN PGM_ERR
      ENDIF

      s_lEof:= (hb_FReadLine( nFHandle, @ cTemp, s_aEOL ) == HB_FEOF)

      /*  Nothing to do if Line is empty
       */
      IF EMPTY( cTemp )
        LOOP
      ENDIF

      cTemp := Trim( cTemp )

      IF  "//" $ cTemp

         WHILE At( "//", cTemp ) > 0

            cTemp := Strtran( cTemp, " //", "" )
            s_lEof:= (hb_FReadLine( nFHandle, @cTemp1, s_aEOL ) == HB_FEOF)
            cTemp += Trim( cTemp1 )

         ENDDO

         cTemp := Strtran( cTemp, " //", "" )

      ENDIF

      aTemp := ListAsArray2( Alltrim( cTemp ), "=" )

      IF lmacrosec

         IF Alltrim( Left( cTemp, 7 ) ) <> "!ifndef" .AND. Alltrim( Left( cTemp, 6 ) ) <> "!endif" .AND. Alltrim( Left( cTemp, 7 ) ) <> "!IFfile" .AND. Alltrim( Left( cTemp, 7 ) ) <> "!stdout" .AND. Alltrim( Left( cTemp, 6 ) ) <> "!ifdef"

            IF Len( aTemp ) > 1

                IF  "$" $ aTemp[ 2 ]

                  IF ( s_lGcc .AND. ( aTemp[ 1 ] == "CFLAG1" ) ) .OR. ( s_lGcc .AND. ( aTemp[ 1 ] == "CFLAG2" ) )
                      AAdd( s_aMacros, { aTemp[ 1 ], Strtran( ReplaceMacros( aTemp[ 2 ] ), "\", "/" ) } )

                      x++
                  ELSE

                     IF aTemp[ 1 ] == "MT" .AND. aTemp[ 2 ] == "YES"
                        s_lMt := .T.
                     ENDIF

                     IF aTemp[ 1 ] == "HBLIBS" .AND. ! s_lMt

                        aLib := ListAsArray2( aTemp[ 2 ], " " )

                        FOR each aLibx in aLib

                           IF At( "mt.lib", Lower( aLibx ) ) > 0
                              s_lMt := .T.
                           ENDIF

                           IF "-l" $ Lower( aLibx )
                              s_lBcc    := .F.
                              s_lGcc    := .T.
                              s_lMSVcc  := .F.
                              s_lPocc   := .F.
                              s_aDefines[2] := { "MAKE_DIR", GetGccDir() }
                              s_aDefines[3] := { "HARBOUR_DIR", s_cHarbourDir }

                           ENDIF

                        NEXT

                     ENDIF

                     IF aTemp[ 1 ] == "ALLOBJ" .AND. ! s_lMt

                     ENDIF

                     AAdd( s_aMacros, { aTemp[ 1 ], ReplaceMacros( aTemp[ 2 ] ) } )

                  ENDIF

               ELSE

                  IF ( s_lGcc .AND. ( aTemp[ 1 ] == "CFLAG1" ) ) .OR. ( s_lGcc .AND. ( aTemp[ 1 ] == "CFLAG2" ) )
                     AAdd( s_aMacros, { aTemp[ 1 ], Strtran( aTemp[ 2 ], "\", "/" ) } )

                      x++

                  ELSE
                     IF aTemp[ 1 ] == "HBLIBS" .AND. ! s_lMt

                        aLib := ListAsArray2( aTemp[ 2 ], " " )

                        FOR each aLibx in aLib

                           IF AT( "mt.lib", LOWER( aLibx ) ) > 0
                              s_lMt := .T.
                           ENDIF

                           IF "-l" $ LOWER( aLibx )
                              s_lBcc    := .F.
                              s_lGcc    := .T.
                              s_lMSVcc  := .F.
                              s_lPocc   := .F.
                              s_aDefines[2] := { "MAKE_DIR", GetGccDir() }
                              s_aMacros[2,2] :=  GetGccDir()
                           ENDIF

                        NEXT

                     ELSEIF  aTemp[ 1 ] == "SHELL"
                             IF !empty( Atemp[ 2 ] )
                                s_lErrors  := IIF( hb_run( Atemp[ 2 ] + " > e.txt" ) != 0, .T., .F. )

                                Atemp[ 2 ] := ALLTRIM( MEMOREAD( "e.txt" )  )
                                aTemp[ 2 ] := STRTRAN( aTemp[ 2 ],chr(13),"")
                                aTemp[ 2 ] := STRTRAN( aTemp[ 2 ],chr(10),"")

                                FERASE("e.txt")
                             ENDIF
                     ENDIF
                        AAdd( s_aMacros, { aTemp[ 1 ], aTemp[ 2 ] } )
                  ENDIF

               ENDIF

            ENDIF

            IF aTemp[ 1 ] == "COMPRESS"
               s_lCompress := "YES" $ aTemp[ 2 ]
            ENDIF

            IF aTemp[ 1 ] == "GUI"
               s_lGui := "YES" $ aTemp[ 2 ]
            ENDIF

            IF aTemp[ 1 ] == "CONTRIBLIB"
               s_lContribLib := "YES" $ aTemp[ 2 ]
            ENDIF

            IF aTemp[ 1 ] == "SRC02"  // obj dir
               s_cObjDir := aTemp[ 2 ]
            ENDIF

            IF aTemp[ 1 ] == "PROJECT"

               IF At( ".lib", aTemp[ 2 ] ) > 0 .OR. At( ".a", aTemp[ 2 ] ) > 0
                  s_lLibrary := .T.
               ENDIF

               s_cAppName := SubStr( aTemp[ 2 ], 1, AT( " ", aTemp[ 2 ] ) -1 )

            ENDIF

            IF aTemp[ 1 ] == "OBJFILES"
               s_aObjs := ListAsArray2( ReplaceMacros( aTemp[ 2 ] ), " " )
            ENDIF

            IF aTemp[ 1 ] == "OBJCFILES"

               aTemp1 := ListAsArray2( ReplaceMacros( aTemp[ 2 ] ), " " )

               IF Len( aTemp1 ) == 1

                  IF ! Empty( aTemp[ 1 ] )
                      s_aObjsC := ListAsArray2( ReplaceMacros( aTemp[ 2 ] ), " " )
                   ENDIF

               ELSE
                  s_aObjsC := ListAsArray2( ReplaceMacros( aTemp[ 2 ] ), " " )
               ENDIF

            ENDIF

            IF aTemp[ 1 ] == "PRGFILES"
               s_aPrgs     := ListAsArray2( ReplaceMacros( aTemp[ 2 ] ), " " )
               lCfgFound := FindHarbourCfg( @cCfg )
            ENDIF

            IF aTemp[ 1 ] == "PRGFILE"
               s_aPrgs := ListAsArray2( ReplaceMacros( aTemp[ 2 ] ), " " )
            ENDIF

            IF aTemp[ 1 ] == "CONTRIBLIBS"
               s_aContribLibs  := ListAsArray2( ReplaceMacros( aTemp[ 2 ] ), " " )
            ENDIF

            IF aTemp[ 1 ] == "CFILES"

                  aTempCFiles := ListAsArray2( ReplaceMacros( aTemp[ 2 ] ), " " )

                  IF ( Len( aTempCFiles ) == 1 )

                     IF ! Empty( aTempCFiles[ 1 ] )
                        s_aCFiles := ListAsArray2( ReplaceMacros( aTemp[ 2 ] ), " " )
                     ENDIF

               ELSE
                  s_aCFiles := ListAsArray2( ReplaceMacros( aTemp[ 2 ] ), " " )
               ENDIF

            ENDIF

            IF aTemp[ 1 ] == "RESFILES"
               s_aResources := ListAsArray2( ReplaceMacros( aTemp[ 2 ] ), " " )
            ENDIF

            IF aTemp[ 1 ] == "EDITOR"
               s_cEditor  := Trim( aTemp[ 2 ] )
            ENDIF

         ELSE

            IF "!ifndef" $ cTemp
               CheckDefine( nFHandle, cTemp )
            ELSEIF  "!ifdef" $ cTemp
               CheckIFdef( nFHandle, cTemp )
            ELSEIF "!iffile" $ cTemp
               CheckIFFile( cTemp )
            ELSEIF  "!stdout" $ cTemp
               CheckStdOut( nFHandle, cTemp )
            ENDIF

         ENDIF

      ENDIF
      IF s_lMingw .and. s_lGcc
         x := ascan(s_aMacros,{|x|  X[1] == "HB_DIR"})
         IF x>0
            IF s_aMacros[x,2] != s_cHarbourDir
               s_aMacros[x,2] := s_cHarbourDir
            ENDIF
         ENDIF
      ENDIF

      IF lBuildSec

         s_szProject   := cTemp
         s_aBuildOrder := ListAsArray2( cTemp, ":" )

         IF !s_lLibrary
            IF SetBuild( nFHandle ) == PGM_ERR
               RETURN PGM_ERR
            ENDIF
         ELSE
            IF SetBuildLib( nFHandle ) == PGM_ERR
               RETURN PGM_ERR
            ENDIF
         ENDIF

      ENDIF

      IF lComSec

         IF ! Empty( cTemp )
            Setcommands( nFHandle, cTemp )
         ENDIF

      ENDIF

#IfDef HBM_USE_DEPENDS
      IF lDepSec

         IF ! Empty( cTemp )
            SetDependencies( cTemp )
         ENDIF

      ENDIF
#Endif

      IF cTemp == "#BUILD"
         cBuffer := cTemp
      ELSEIF cTemp == "#COMMANDS"
         cbuffer := cTemp
#IfDef HBM_USE_DEPENDS
      ELSEIF cTemp == "#DEPENDS"
         cbuffer := cTemp
#Endif
      ENDIF

   ENDDO

   FCLOSE( nFHandle )  // Close the opened file & release memory

   IF (!lCfgFound .or. s_lForce)

      IF s_lBcc
         BuildBccCfgFile()
      ELSEIF s_lMSVcc
         BuildMscCfgFile()
      ELSEIF s_lPocc
         BuildPccCfgFile()
      ELSEIF s_lGcc .AND. !lLinux
         BuildGccCfgFile()
      ELSEIF s_lGcc .AND. lLinux
         BuildGccCfgFileL()
      ENDIF

   ENDIF

RETURN PGM_OK

*----------------------------
FUNCTION Checkdefine( nFHandle, cTemp )
*----------------------------

   LOCAL nPos
   LOCAL cRead
   LOCAL aSet     := {}
   LOCAL nMakePos

   IF cTemp == "!endif"
      RETURN NIL
   ENDIF

   s_lEof:= (hb_FReadLine( nFHandle, @cTemp, s_aEOL ) == HB_FEOF)
   cTemp := Trim( cTemp )
   cTemp := Strtran( cTemp, "!ifndef ", "" )
   cTemp := Strtran( cTemp, "\..", "" )
   cTemp := Strtran( cTemp, "/..", "" )

   IF  "\.." $ cTemp
      cTemp := Substr( cTemp, 1, At( "\..", cTemp ) - 1 )
   ELSEIF  "/.." $ cTemp
      cTemp := Substr( cTemp, 1, At( "/..", cTemp ) - 1 )
   ENDIF

   aSet := ListAsArray2( cTemp, "=" )
   nPos := AScan( s_aDefines, { | x | x[ 1 ] == aSet[ 1 ] } )

   IF nPos == 0
      cRead    := Alltrim( Strtran( aSet[ 2 ], "$(", "" ) )
      cRead    := Strtran( cRead, ")", "" )
      nMakePos := AScan( s_aDefines, { | x | x[ 1 ] == cRead } )

      IF nMakePos > 0
         AAdd( s_aDefines, { aSet[ 1 ], s_aDefines[ nMakePos, 2 ] } )
         AAdd( s_aMacros, { aSet[ 1 ], s_aDefines[ nMakePos, 2 ] } )
      ENDIF

   ENDIF

RETURN NIL

*----------------------------
FUNCTION Setcommands( nFHandle, cTemp )
*----------------------------

   LOCAL cRead
   LOCAL nCount       := 0
   LOCAL aTempMacros  := {}
   LOCAL aLocalMacros := {}

   s_lEof      := ( hb_FReadLine( nFHandle, @cRead, s_aEOL ) == HB_FEOF )
   cRead       := ALLTRIM( cRead )
   aTempMacros := ListAsArray2( cRead, " " )

   AEval( aTempMacros, { | xMacro | IIF( At( "$", xMacro ) > 0, ;
                         IIF( At( ";", xMacro ) > 0, ( aLocalMacros := ListAsArray2( xMacro, ";" ), ;
                         AEval( aLocalMacros, { | x | Findmacro( x, @cRead ) } ) ), ;
                         Findmacro( xMacro, @cRead ) ), ) } )
   AAdd( s_aCommands, { cTemp, cRead } )

RETURN NIL

#IfDef HBM_USE_DEPENDS

*--------------------------------
FUNCTION SetDependencies( cTemp )
*--------------------------------

   LOCAL nCount       := 0
   LOCAL aTempMacros  := {}
   LOCAL aLocalMacros := {}
   LOCAL cTmp         := ""

   aTempMacros := ListAsArray2( ReplaceMacros(cTemp), " " )

   IF Len( aTempMacros ) > 1
      cTmp := aTempMacros[ 1 ]
      IF Right(cTmp,1) == ":"
         cTmp := Left(cTmp,Len(cTmp) - 1)
      ENDIF
      aTempMacros := ADel( aTempMacros , 1)
      ASize(aTempMacros,Len(aTempMacros) - 1)
      AAdd( s_aDepends, { cTmp, AClone( aTempMacros ) } )
   ENDIF

RETURN NIL
#Endif

*----------------------------------
FUNCTION Findmacro( cMacro, cRead )
*----------------------------------

   LOCAL nPos
   LOCAL cTemp
   LOCAL aLocalMacros := {}

   cMacro := Substr( cMacro, 1, At( ")", cMacro ) )

   IF  "-" $ cMacro
      cMacro := Substr( cMacro, 3 )
   ENDIF

   IF  ";" $ cMacro
      cMacro := Substr( cMacro, At( ";", cMacro ) + 1 )
   ENDIF

   nPos := AScan( s_aMacros, { | x | "$(" + Alltrim( x[ 1 ] ) + ")" == cMacro } )

   IF nPos == 0
      cTemp := Strtran( cMacro, "$(", "" )
      cTemp := Strtran( cTemp, ")", "" )

      IF ! Empty( cTemp )
         cRead := Alltrim( Strtran( cRead, cMacro, Gete( cTemp ) ) )
      ENDIF

   ELSE
      cRead := Alltrim( Strtran( cRead, cMacro, s_aMacros[ npos, 2 ] ) )
   ENDIF

RETURN cRead

*--------------------------------
FUNCTION ReplaceMacros( cMacros )
*--------------------------------

   LOCAL nCount       := 0
   LOCAL aTempMacros  := {}
   LOCAL aLocalMacros := {}
   LOCAL cLibPath := ""
   LOCAL cLibPath1 := ""
   LOCAL cLibPath2:= ""

   IF "/LIBPATH2:" $ cMacros
      cLibPath2 := "/LIBPATH2:"
      cMacros := StrTran(cMacros,cLibPath2,"cccccccccc ")
   ENDIF

   IF "/LIBPATH1:" $ cMacros
      cLibPath1 := "/LIBPATH1:"
      cMacros := StrTran(cMacros,cLibPath1,"bbbbbbbbbb ")
   ENDIF

   IF "/LIBPATH:" $ cMacros
      cLibPath := "/LIBPATH:"
      cMacros := StrTran(cMacros,cLibPath,"aaaaaaaaaa ")
   ENDIF

   aTempMacros := ListAsArray2( cMacros, " " )

   AEval( aTempMacros, { | xMacro | IIF(  "$" $ xMacro , ;
                         IIF(  ";" $ xMacro , ( aLocalMacros := ListAsArray2( xMacro, ";" ), ;
                         AEval( aLocalMacros, { | x | Findmacro( x, @cMacros ) } ) ), ;
                         Findmacro( xMacro, @cMacros ) ), ) } )

   IF !empty(cLibPath)

      cMacros := strtran(cMacros,"aaaaaaaaaa ","/LIBPATH:")
      cMacros := strtran(cMacros,"bbbbbbbbbb ","/LIBPATH:")
      cMacros := strtran(cMacros,"cccccccccc ","/LIBPATH:")

      IF s_lPocc
         cMacros := StrTran(cMacros,"\BIN","")
         cMacros := StrTran(cMacros,"\bin","")
      ENDIF

   ENDIF

RETURN cMacros

*------------------
FUNCTION SetBuild( nFHandle )
*------------------

   LOCAL cRead
   LOCAL nPos
   LOCAL aMacro
   LOCAL aTemp
   LOCAL cCurrentRead := ""
   LOCAL cMacro
   LOCAL xInfo
   LOCAL xItem

   s_lEof      := (hb_FReadLine( nFHandle, @cRead, s_aEOL ) == HB_FEOF)
   cRead       := ALLTRIM( cRead )
   s_szProject := cRead
   aMacro      := ListAsArray2( cRead, ":" )

   IF Len( aMacro ) > 1
      aTemp := ListAsArray2( aMacro[ 2 ], " " )
      AEval( aTemp, { | xItem | AAdd( s_aBuildOrder, xItem ) } )
   ENDIF

   AAdd( s_aBuildOrder, aMacro[ 1 ] )
   cRead := Strtran( cRead, "@&&!", "" )
   aMacro := ListAsArray2( cRead, "\" )

   AEval( aMacro, { | xMacro |  IIF(  "$" $ xMacro , FindMacro( xMacro, @cRead ), ) } )

   IF ! s_lLinux .AND. !s_lMinGW

      s_cLinkCommands   := cRead + "  @" + s_cLinkFile
      s_nMakeFileHandle := FCreate( s_cLinkFile )

      IF s_nMakeFileHandle == F_ERROR
         IF s_nLang     == LANG_PT     /* portuguese brazilian */
            s_cAlertMsg := "<" + s_cLinkFile + "> n∆o pode ser criado."
         ELSEIF s_nLang == LANG_ES     /* spanish              */
            s_cAlertMsg := "<" + s_cLinkFile + "> no pode ser criado."
         ELSE                          /* english             */
            s_cAlertMsg := "<" + s_cLinkFile + "> cannot be created."
         ENDIF
         Alert( s_cAlertMsg + " FERROR (" + Ltrim( Str( FError() ) ) + ")" )
         RETURN PGM_ERR
      ENDIF

   ELSE
      s_cLinkCommands := cRead + " "
   ENDIF

   FOR nPos := 1 TO 7

      s_lEof       := (hb_FReadLine( nFHandle, @cRead, s_aEOL ) == HB_FEOF)
      cRead        := Alltrim( cRead )
      cCurrentRead := cRead
      aMacro       := ListAsArray2( cRead, " " )

      FOR EACH cMacro IN aMacro

          IF "$" $ cMacro

             FindMacro( cMacro , @cRead )

             IF At( "$(PROJECT)", cCurrentRead ) > 0

                IF ! s_lGcc

                   IF ! s_lLinux
                      IF s_lMSVcc .OR. s_lPocc
                         cRead := strtran(cRead,",","")
                         cRead := strtran(cRead,"+","")
                         xInfo := iif(s_lMSVcc," -out:","/out:")
                         xInfo += cRead
                         cRead := xInfo
                      ENDIF
                      FWrite( s_nMakeFileHandle, cRead + s_cEOL )
                   ENDIF

                ELSEIF s_lGcc .AND. s_lLinux .OR. ( s_lGcc .AND. s_lMinGW)
                  s_cLinkCommands += "-o " + cRead + " "

                ELSEIF s_lGcc .AND. ! s_lLinux .AND. At( ".exe", cRead ) > 0
                  FWrite( s_nMakeFileHandle, "-o " + cRead + s_cEOL )

                ENDIF

             ELSE

                IF ! s_lLinux

                   IF s_lMsVcc .OR. s_lPocc

                     cRead := strtran(cRead,",","")
                     cRead := strtran(cRead,"+","")
//                   cRead := strtran(cRead," ", '"' +s_cEOL+'"')
                     aTemp := ListAsArray2( cRead, " " )
                     cRead :=""
                     FOR EACH xItem IN aTemp
                         cRead +=xItem+s_cEOL
                     NEXT
                     cRead := substr(cRead,1,rat(s_cEOL,cRead)-1)

                   ENDIF

                   IF s_lMinGW
                      s_cLinkCommands += strtran(cRead,"/","\") + " "
                   ELSE
                      FWrite( s_nMakeFileHandle, cRead + s_cEOL )
                   ENDIF

                ELSE
                  s_cLinkCommands += cRead + " "
                ENDIF

             ENDIF

          ENDIF

      NEXT

   NEXT

   //IF !s_lLinux .and. s_lMinGW
   IF s_lWin32 .OR. s_lOS2 .OR. s_lMinGW
      FClose( s_nMakeFileHandle )
      s_nMakeFileHandle:= F_ERROR  // Invalid handle now file is closed
   ENDIF

   IF s_lMsVcc
      s_cLinkCommands +=" /nologo " + IIF( s_lGui, "/SUBSYSTEM:WINDOWS"," /SUBSYSTEM:CONSOLE") + " /force:multiple "
   ENDIF

RETURN PGM_OK

*----------------------
FUNCTION CompileFiles()
*----------------------

   LOCAL cScreen
   LOCAL cComm
   LOCAL cOld
   LOCAL nPos
   LOCAL nFiles
   LOCAL cErrText := ""
   LOCAL aOrder   := ListAsArray2( s_aBuildOrder[ 2 ], " " )
   LOCAL lEnd     := .F.
   LOCAL xItem
   LOCAL lLinux   :=  s_lLinux
   LOCAL cPrg     := ""
   LOCAL cOrder   := ""
   LOCAL nFile    := 1
   LOCAL aGauge   := GaugeNew( 5, 5, 7, 40, "W/B", "W+/B", "≤" )
   LOCAL lErrs    := .F.

   FOR EACH cOrder IN aOrder

      IF cOrder == "$(CFILES)"

         IF s_lGcc
            nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".c.o:" .OR. x[ 1 ] == ".cpp.o:" } )
         ELSE
            nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".c.obj:" .OR. x[ 1 ] == ".cpp.obj:" } )
         ENDIF

         IF nPos > 0
               cComm := s_aCommands[ nPos, 2 ]
            cOld  := cComm
         ELSE
            nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".C.OBJ:" } )

            IF nPos > 0
               cComm := s_aCommands[ nPos, 2 ]
               cOld  := cComm
            ENDIF

         ENDIF

         IF Len( s_aCFiles ) > 0

            GaugeDisplay( aGauge )
            nFile := 1

            FOR nFiles := 1 TO Len( s_aCFiles )
               xItem := Substr( s_aCFiles[ nFiles ], Rat( IIF( s_lGcc, "/", "\" ), ;
                                s_aCFiles[ nFiles ] ) + 1 )
               nPos  := AScan( s_aObjsC, { | x | x := Substr( x, Rat( IIF( s_lGcc, "/", "\" ), x ) + 1 ), ;
                        Left( x, At( ".", x ) ) == Left( xitem, At( ".", xitem ) ) } )

               IF nPos > 0

                  IF llinux
                     cComm := Strtran( cComm, "o$*", "o" + s_aObjsC[ nPos ] )
                  ELSE
                     IF s_lMSVcc //.OR. s_lPocc
                        cComm := Strtran( cComm, "-Fo$*", "-Fo" + Strtran( s_aObjsC[ nPos ], "/", "\" ) )
                     ELSE
                        cComm := Strtran( cComm, "o$*", "o" + Strtran( s_aObjsC[ nPos ], "/", "\" ) )
                     ENDIF
                  ENDIF

                  cComm := Strtran( cComm, "$**", s_aCFiles[ nFiles ] )

                  cComm += IIF( s_lLinux ,  " "," >>" + ( s_cLogFile ) )

                  @ 4, 5 SAY "Compiling: " + PADR( s_aCFiles[ nFiles ], 50 )
                  GaugeUpdate( aGauge, nFile / Len( s_aCFiles ) )   // Changed s_aPrgs to s_aCFiles, Ath 2004-06-08

                  nFile ++
                  OUTERR( cComm + s_cEOL )
                  setpos(9,0)
                  if s_lMingw
                     cComm := strtran(cComm ,"\","/")
                  endif

                  cScreen := SAVESCREEN()
                  s_lErrors := IIF( hb_run( cComm ) != 0, .T., .F. )
                  RESTSCREEN( ,,,, cScreen )
                  cErrText := Memoread( s_cLogFile )
                  lEnd     := "Error E" $   cErrText

                  IF ! s_lIgnoreErrors .AND. lEnd

                     cScreen := SAVESCREEN()
                     s_lErrors := IIF( hb_run( s_cEditor + " " + s_cLogFile ), .T., .F. )
                     RESTSCREEN( ,,,, cScreen )
                     SET CURSOR ON
                     lErrs := .T.
                  ENDIF
                  lEnd := "Error F" $   cErrText

                  cComm := cOld

               ENDIF

            NEXT

         ENDIF

      ENDIF

      IF cOrder == "$(OBJFILES)"

         IF s_lGcc
         nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".prg.o:" } )
         ELSE
            nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".prg.obj:" } )
         ENDIF

         IF nPos > 0
            cComm := s_aCommands[ nPos, 2 ]
            cOld  := cComm
         ELSE

            IF s_lGcc
               nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".PRG.O:" } )
            ELSE
               nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".PRG.OBJ:" } )
            ENDIF

         ENDIF

         GaugeDisplay( aGauge )
         nFile := 1

         FOR EACH cPrg IN s_aPrgs

            xItem := Substr( cPrg, Rat( IIF( s_lGcc, "/", "\" ), ;
                          cPrg ) + 1 )
            nPos := AScan( s_aObjs, { | x | x := Substr( x, Rat( IIF( s_lGcc, "/", "\" ), x ) + 1 ), ;
                    Left( x, At( ".", x ) ) == Left( xItem, At( ".", xitem ) ) } )

            IF nPos > 0

               IF llinux
                  cComm := Strtran( cComm, "o$*", "o" + s_aObjs[ nPos ] )
               ELSE
                  IF s_lMSVcc //.OR. s_lPocc
                     cComm := Strtran( cComm, "-Fo$*", "-Fo" + Strtran( s_aObjs[ nPos ], "/", "\" ) )
                  ELSE
                     cComm := Strtran( cComm, "o$*", "o" + Strtran( s_aObjs[ nPos ], "/", "\" ) )
                  ENDIF
               ENDIF

               cComm := Strtran( cComm, "$**", cPrg )
               cComm += IIF( s_lLinux,  " > " + s_cLogFile, " >>" + s_cLogFile )

               @ 4,5 SAY "Compiling: " + PADR( cPrg, 50 )
               GaugeUpdate( aGauge, nFile / Len( s_aPrgs ) )
               OUTERR( s_cEOL )
               nFile ++
               setpos(9,0)

               cScreen := SAVESCREEN()
               s_lErrors := IIF( hb_run( cComm ) != 0, .T., .F. )
               RESTSCREEN( ,,,, cScreen )
               cErrText := Memoread( s_cLogFile )
               lEnd     :=   "C2006" $ cErrText .OR. "No code generated" $ cErrText .or. ;
                           "Error E" $ cErrText .OR. "Error F"           $ cErrText

               lErrs    := IIF( lEnd, .T., lErrs )

               IF ! s_lIgnoreErrors .AND. lEnd

                  s_lErrors := IIF( hb_run( s_cEditor + " " + s_cLogFile ), .T., .F. )
               ENDIF

               cComm := cOld

            ENDIF

         NEXT

         IF lErrs
            RETURN PGM_ERR
         ENDIF

      ENDIF

      IF cOrder == "$(RESDEPEN)"
         nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".rc.res:" } )

         IF nPos > 0
            cComm := s_aCommands[ nPos, 2 ]
            cOld  := cComm
         ENDIF

         FOR nFiles := 1 TO Len( s_aResources )

            IF ! Empty( s_aResources[ nFiles ] )
               cComm := Strtran( cComm, "$<", s_aResources[ nFiles ] )
               OutErr( " " )
               ? cComm
               setpos(9,0)

               cScreen := SAVESCREEN()
               s_lErrors := IIF( hb_run( cComm ) != 0, .T., .F. )
               lErrs     := IF( s_lErrors, .T., lErrs )
               RESTSCREEN( ,,,, cScreen )
            ENDIF

            cComm := cOld

         NEXT

      ENDIF

   NEXT

RETURN IIF ( lErrs, PGM_ERR, PGM_OK )

*-------------------------------
FUNCTION GetParaDefines( cTemp )
*-------------------------------

   LOCAL nPos
   LOCAL cRead
   LOCAL aSet     := {}
   LOCAL nMakePos

   IF  "\.." $ cTemp
      cTemp := Substr( cTemp, 1, At( "\..", cTemp ) - 1 )
   ELSEIF  "/.." $ cTemp
      cTemp := Substr( cTemp, 1, At( "/..", cTemp ) - 1 )
   ENDIF
   if at("=",ctemp) == 0
      cTemp += [=""]
   endif
   aSet := ListAsArray2( cTemp, "=" )
   nPos := AScan( s_aDefines, { | x | x[ 1 ] == aSet[ 1 ] } )

   IF nPos == 0
      cRead    := Alltrim( Strtran( aSet[ 2 ], "$(", "" ) )
      cRead    := Strtran( cRead, ")", "" )
      nMakePos := AScan( s_aDefines, { | x | x[ 1 ] == cRead } )

      IF nMakePos == 0
         if aSet[1] == "MYDEFINES"
            ASet[ 2 ] := Strtran( aSet[ 2 ], ",", ";" )
         else
            ASet[ 2 ] := Strtran( aSet[ 2 ], ",", " " )
         endif
         AAdd( s_aDefines, { aSet[ 1 ], aSet[ 2 ] } )
         AAdd( s_aMacros, { aSet[ 1 ], aSet[ 2 ] } )
      else
         s_aDefines[nMakepos,2] +=";"+aSet[ 2 ]
         s_aMacros[nMakepos,2]+=";"+ aSet[ 2 ]

      ENDIF

   ENDIF

RETURN NIL

*---------------------
FUNCTION PrintMacros()
*---------------------

   OutStd( hbMakeID()+ " "+hbMakeCopyright()+ s_cEOL )
   OutStd( "" + s_cEOL )
   OutStd( "Macros:" + s_cEOL )
   AEval( s_aMacros, { | xItem | OutStd( "     " + xItem[ 1 ] + " = " + xItem[ 2 ] + s_cEOL ) } )
   OutStd( "Implicit Rules:" + s_cEOL )
   AEval( s_aCommands, { | xItem | OutStd( "     " + xItem[ 1 ] + s_cEOL + "        " + xItem[ 2 ] + s_cEOL ) } )
   OutStd( "" + s_cEOL )
   OutStd( "Targets:" )
   OutStd( "    " + s_szProject + ":" + s_cEOL )
   OutStd( "        Flags :" + s_cEOL )
   OutStd( "        Dependents :" )
   AEval( s_aCFiles, { | xItem | OutStd( xitem + " " ) } )
   AEval( s_aObjs, { | xItem | OutStd( xitem + " " ) } )
   OutStd( " " + s_cEOL )
   OutStd( "        commands:" + s_aBuildOrder[ Len( s_aBuildOrder ) ] )
   OutStd( " " + s_cEOL )
   OutStd( " " + s_cEOL )
   OutStd( " " + s_cEOL )

RETURN NIL

*--------------------------------------------------
FUNCTION CreateMakeFile( cFile, lCreateAndCompile )
*--------------------------------------------------

   LOCAL nBuildReturn
   LOCAL aInFiles     := {}
   LOCAL aOutFiles    := {}
   LOCAL aOutc        := {}
   LOCAL aSrc         := Directory( "*.prg" )
   LOCAL nLenaSrc     := Len( aSrc )

   LOCAL lFwh         := .F.
// LOCAL lxFwh        := .F.
   LOCAL lC4W         := .F.
   LOCAL lMiniGui     := .F.
   LOCAL lHwGui       := .F.
   LOCAL lWhoo        := .F.
   LOCAL lHBWhat      := .F.
   LOCAL lGtWvt       := .F.
   LOCAL lGtWvw       := .F.
   LOCAL lMWvw        := .F.
   LOCAL lXwt         := .F.
   LOCAL lxHGtk       := .F.

   LOCAL lRddAds      := .F.
   LOCAL lMediator    := .F.
   LOCAL lApollo      := .F.

// LOCAL lMt          := .F.
   LOCAL cOS          := IIF( s_lUnix, "Unix", IIF( s_lLinux, "Linux", iif(s_lOS2,"OS/2","Win32") ) )
   LOCAL cCompiler    := IIF( s_lLinux .OR. s_lGcc, "GCC",iif(s_lPocc,"POCC",iif(s_lMSVcc,"MSVC","BCC")))

   // Contrib GUI Libs
   LOCAL cFwhPath     := Space( 200 )
   LOCAL cC4WPath     := Space( 200 )
   LOCAL cMiniPath    := Space( 200 )
   LOCAL cHwPath      := Space( 200 )
   LOCAL cxHGPath     := Space( 200 )

   LOCAL cMedPath     := Space( 200 )
   LOCAL cApolloPath  := Space( 200 )

   LOCAL cObjDir      := s_cObjDir + space( 20 )
   LOCAL lAutoMemvar  := .F.
   LOCAL lVarIsMemvar := .F.
   LOCAL lDebug       := .F.
   LOCAL lSupressLine := .F.
   LOCAL nPos
   LOCAL cHarbourFlags  := ""

// LOCAL nWarningLevel :=0

   LOCAL lUseharbourDll := .F.

   LOCAL lCompMod         := .F.

//   LOCAL lGenppo          := .F.
   LOCAL x
   LOCAL getlist          := {}
   LOCAL cTopFile         := Space(50)
   LOCAL cAppName         := padr(s_cAppName,50)
   LOCAL cDefaultLibs     := "hblang.lib hbvm.lib hbrtl.lib hbrdd.lib hbmacro.lib hbpp.lib rddntx.lib rddcdx.lib rddfpt.lib hbcommon.lib gtwin.lib hbcpage.lib hbpcre.lib hbzlib.lib hbhsx.lib hbsix.lib"
   LOCAL cDefGccLibs      := "-lhbvm -lhbrtl -lhbpcre -lgtdos -lhblang -lhbrdd -lhbrtl -lhbvm -lhbmacro -lhbpp -lrddntx -lrddcdx -lrddfpt -lhbhsx -lhbsix -lhbcommon -lhbcpage -lm"
   LOCAL cDefGccLibsUnix  := "-lhbvm -lhbcpage -ltef -lhbrtl -lhbrdd -lhbrtl -lhbvm -lhbmacro -lhbpp -lhblang -lhbcommon -lhbnulrdd -lbmrddcdx -lrddntx -lrddcdx -lrddfpt -lhbsix -lhbhsx -lhbusrrdd -lhbpcre -lgtsln -lshblang -lm -lrt"
   LOCAL cDefGccLibsw     := "-lhbvm -lhbrtl -lhbpcre -lgtwin -lhblang -lhbrdd -lhbrtl -lhbvm -lhbmacro -lhbpp -lrddntx -lrddcdx -lrddfpt -lhbhsx -lhbsix -lhbcommon -lhbcpage -lm"
   LOCAL cGccLibsOs2      := "-lhbvm -lhbrtl -lhbpcre -lgtos2 -lhblang -lhbrdd -lhbrtl -lhbvm -lhbmacro -lhbpp -lrddntx -lrddcdx -lrddfpt -lhbhsx -lhbsix -lhbcommon -lhbcpage -lm"
   LOCAL cDefLibGccLibs   := "-lhbvm -lhbrtl -lhbpcre -lgtcrs -lhblang -lhbrdd -lhbrtl -lhbvm -lhbmacro -lhbpp -lrddntx -lrddcdx -lrddfpt -lhbhsx -lhbsix -lhbcommon -lhbcpage"
   LOCAL cDefaultLibsMt   := "lang.lib vmmt.lib rtlmt.lib rddmt.lib macromt.lib ppmt.lib dbfntxmt.lib dbfcdxmt.lib  dbffptmt.lib common.lib gtwin.lib codepage.lib hbpcre.lib hbzlib.lib hsxmt.lib hbsixmt.lib"
   LOCAL cDefGccLibsUnixMt := "-lvmmt -lcodepage -ltef -lrtlmt  -lrddmt  -lrtl  -lvmmt  -lmacromt  -lpp  -llang  -lcommon  -lnulsys  -lbmdbfcdx  -ldbfntx  -ldbfcdx  -ldbffpt  -lhbsix  -lhsx  -lusrrdd -lhbpcre -lgtsln -lslang -lm -lrt"
   LOCAL cDefGccLibsMt    := "-lvmmt -lrtlmt -lhbpcre -lgtdos -llang -lrddmt -lrtlmt -lvmmt -lmacromt -lppmt -ldbfntxmt -ldbfcdxmt -ldbffptmt -lhsxmt -lhbsixmt -lcommon -lcodepage -lm"
   LOCAL cDefGccLibsMtw   := "-lvmmt -lrtlmt -lhbpcre -lgtwin -llang -lrddmt -lrtlmt -lvmmt -lmacromt -lppmt -ldbfntxmt -ldbfcdxmt -ldbffptmt -lhsxmt -lhbsixmt -lcommon -lcodepage -lm"
   LOCAL cGccLibsOs2Mt    := "-lvmmt -lrtlmt -lhbpcre -lgtos2 -llang -lrddmt -lrtlmt -lvmmt -lmacromt -lppmt -ldbfntxmt -ldbfcdxmt -ldbffptmt -lhsxmt -lhbsixmt -lcommon -lcodepage -lm"
// LOCAL cDefLibGccLibsMt := "-lvmmt -lrtlmt -lhbpcre -lgtcrs -llang -lrddmt -lrtlmt -lvmmt -lmacromt -lppmt -ldbfntxmt -ldbfcdxmt -ldbffptmt -lhsxmt -lhbsixmt -lcommon -lcodepage"
   LOCAL cDefLibGccLibsMt := "-lvmmt -lrtlmt -lhbpcre -lgtsln -llang -lrddmt -lrtlmt -lvmmt -lmacromt -lppmt -ldbfntxmt -ldbfcdxmt -ldbffptmt -lhsxmt -lhbsixmt -lcommon -lcodepage"
   LOCAL cHarbDll         := "harbour.lib"
   LOCAL cHARso           := "-lharbour -lncurses -lgpm -lslang -lpthread -lm"
   LOCAL cSystemLibs      := iif( s_lUnix, "", "-lncurses " ) + "-lslang " + iif( s_lUnix, "", "-lgpm " ) + " -lpthread -lm"

   LOCAL cLibs        := ""
   LOCAL citem        := ""
   LOCAL cExt         := ""
   LOCAL cDrive       := ""
   LOCAL cPath        := ""
   LOCAL cTest        := ""
   LOCAL cGuiLib      := "None"
   LOCAL aLibs
   LOCAL aLibsIn      := {}
   LOCAL aLibsOut     := {}
   LOCAL cGt          := ""

   LOCAL cOldLib      := ""
   LOCAL cHtmlLib     := ""
   LOCAL lLinux       := s_lLinux
   LOCAL lUnix        := s_lUnix
   LOCAL nWriteFiles  := 0
   LOCAL cResName     := space(200)
   LOCAL aSelFiles

   LOCAL cBuild       := " "
   LOCAL cBuildForced := " "
   LOCAL aUserDefs
   LOCAL cCurrentDef  := ""
   LOCAL cRdd         := "None"
   LOCAL cCurrentDir  := ""
   LOCAL nOption
   LOCAL lNew         := .F.
   LOCAL oMake
   LOCAL cAllRes      := ""
   LOCAL cTemp
   LOCAL cExtraLibs   :=""
   LOCAL cTempLibs    := ""
   LOCAL aTempLibs

   LOCAL aUserLibs
   LOCAL cUserLib

   LOCAL cHarbourLibDir := s_cHarbourDir + iif(s_lLinux,"/lib","\lib")
   LOCAL lCancelMake := .F.
   LOCAL cScreen     := SAVESCREEN()

   #IFdef HBM_USE_DEPENDS
      LOCAL cIncl              := ""
      LOCAL lScanIncludes      := .F.
      // Provisions for recursive scanning
      LOCAL lScanIncRecursive := .F.
      LOCAL cExcludeExts       := PadR(".ch",40)
   #ENDIF

   #ifndef __PLATFORM__WINDOWS
       LOCAL lHashhso := File("/usr/lib/libharbour.so")
       LOCAL lusexhb := FILE("/usr/bin/hb-build")
   #ELSE
       LOCAL lusexhb := .F.
   #ENDIF

   s_cUserInclude  := space(200)
   s_cUserDefine   := space(200)
   s_cUserLibs     := space(200)

   IF File( cFile )

      IF s_nLang     == LANG_PT        /* portuguese brazilian */
         nOption := Alert( "O makefile <" + cFile +"> j† existe.",{ "Editar", "Criar Novo" , "Cancelar" } )
      ELSEIF s_nLang == LANG_ES        /* spanish              */
         nOption := Alert( "Lo makefile <" + cFile +"> ya existe.",{ "Editar", "Crear Nuevo" , "Cancelar" } )
      ELSE                             /* english             */
         nOption := Alert( "The makefile <" + cFile +"> already exist ",{ "Edit" , "Create New" , "Cancel" } )
      ENDIF

      IF nOption == 1 // edit makefile

         // Verify if "cFile" can be openned to write mode.

         s_nMakeFileHandle := FOpen( cFile, FO_WRITE )

         IF s_nMakeFileHandle == F_ERROR

            CLS

            IF s_nLang     == LANG_PT  /* portuguese brazilian */
               s_cAlertMsg := "<"+cFile + "> n∆o pode ser aberto para ediá∆o."
            ELSEIF s_nLang == LANG_ES  /* spanish              */
               s_cAlertMsg := "<"+cFile + "> no pode ser abierto para edici¢n."
            ELSE                       /* english             */
               s_cAlertMsg := "<"+cFile + "> cannot be openned for editing."
            ENDIF

            Alert( s_cAlertMsg+" FERROR ("+LTrim(Str(FError()))+")" )

            RESTSCREEN( ,,,, cScreen )
            RETURN PGM_ERR
         ELSE
            FClose( s_nMakeFileHandle )
            s_nMakeFileHandle:= F_ERROR  // Invalid handle now file is closed
         ENDIF

         oMake :=THbMake():new()
         oMake:cMakefile := cFile
         oMake:cMacro := iif(s_lMSVcc,"#MSVC",iif(s_lPocc,"#POCC",iif(s_lGcc,"#GCC","#BCC")))
         oMake:ReadMakefile(cFile)

         FRename(cFile,cFile+".old")

         IF LEN(oMake:aRes) > 0
            FOR EACH cTemp IN oMake:aRes
                cAllRes += cTemp + " "
            NEXT
         ENDIF

         lAutoMemVar     := oMake:lAutomemvar
         lVarIsMemVar    := oMake:lvarismemvar
         lDebug          := oMake:ldebug
         lSupressline    := oMake:lSupressline
         lCompMod        := oMake:lCompMod
         s_lGenppo       := oMake:lGenppo
         s_lGui          := oMake:lGui
         cRdd            := IIF( oMake:lRddAds, "RDDADS", IIF( oMake:lMediator, "Mediator", "None" ) )
         cGuiLib         := IIF( oMake:lFwh   , "FWH", ;
                            IIF( oMake:lMini  , "MINIGUI", ;
                            IIF( oMake:lWhoo  , "WHOO", ;
                            IIF( oMake:lCw    , "C4W", ;
                            IIF( oMake:lHwGui , "HWGUI", ;
                            IIF( oMake:lGtWvt , "GTWVT", ;
                            IIF( oMake:lMWvW  , "GTWVW+MWVW", ;
                            IIF( oMake:lGtWvw , "GTWVW", ;
                            IIF( oMake:lXWt   , "XWT", ;
                            IIF( oMake:lHBWhat, "HBWHAT", ;
                            IIF( oMake:lxHGtk , "XHGTK", "" ) ) ) ) ) ) ) ) ) ))
         cFwhpath        := padr(oMake:cFmc,200)
         cApolloPath     := padr(oMake:cFmc,200)
         cC4WPath        := padr(oMake:cFmc,200)
         cMiniPath       := padr(oMake:cFmc,200)
         cHwPath         := padr(oMake:cFmc,200)
         cxHGPath        := padr(oMake:cFmc,200)
         cMedpath        := padr(oMake:cMedpath,200)
         cAppName        := padr(oMake:cAppLibName,50)
         s_cAppName      := cAppName
         s_lCompress     := oMake:lCompress
         s_lContribLib   := oMake:lContribLib
         s_cUserInclude  := padr(oMake:cUserInclude,200)
         s_cUserDefine   := padr(oMake:cUserDef,200)
         s_cUserLibs     := padr(oMake:cUserLib,200)
         s_lxFwh         := oMake:lxFwh
         s_nFilesToAdd   := oMake:cFilesToAdd
         s_lMt           := oMake:lMt
         s_nWarningLevel := oMake:cWarningLevel
         cTopFile        := PadR(oMake:cTopModule,50," ")
         cResName        := PadR(oMake:cRes,200)
         s_cObjDir       := oMake:cObj
         cObjDir         := s_cObjDir + space(20)
         s_lGenCsource   := oMake:lGenCsource
         s_cEditor       := trim(oMake:cEditor)

         if Empty( s_cEditor )
            if s_lOS2 .OR. s_lLinux
               s_cEditor := "mcedit"
            else
               s_cEditor := "edit"
            endif
         endif

         if !s_lRecursive
            s_lRecursive := oMake:lRecurse
         endif

         IF nLenaSrc == 0 .and. !s_lRecursive

            CLS

            IF s_nlang     == LANG_PT  /* portuguese brazilan */
               s_cAlertMsg := "N∆o h† nenhum prg na pasta "+CurDir()+". Use o modo recursivo -r"
            ELSEIF s_nLang == LANG_ES  /* spanish              */
               s_cAlertMsg := "No hay ning£n prg en la carpeta "+CurDir()+". Use lo modo recursivo -r"
            ELSE                       /* english              */
               s_cAlertMsg := "Does not have any prg in "+CurDir()+" folder. Use the recursive mode -r"
            ENDIF

            Alert( s_cAlertMsg )
            RESTSCREEN( ,,,, cScreen )
            RETURN PGM_ERR
         ENDIF

         if s_lCancelRecursive
            s_lRecursive := .F.
         endif

         // after oMake read, recreate other clean makefile to edit.
         s_nMakeFileHandle := FCreate(cFile)

         if s_nMakeFileHandle == F_ERROR

            CLS

            IF s_nLang     == LANG_PT  /* portuguese brazilian */
               s_cAlertMsg := "<"+cFile + "> n∆o pode ser aberto para ediá∆o."
            ELSEIF s_nLang == LANG_ES  /* spanish              */
               s_cAlertMsg := "<"+cFile + "> no pode ser abierto para edici¢n."
            ELSE                /* english             */
               s_cAlertMsg := "<"+cFile + "> cannot be openned for edition."
            ENDIF

            Alert( s_cAlertMsg+" FERROR ("+Ltrim(Str(FError()))+")" )

            RESTSCREEN( ,,,, cScreen )
            RETURN PGM_ERR

         endif

         WriteMakeFileHeader()

         s_lEditMake := .T.

      ELSEIF nOption == 2 // create a new makefile

         IF nLenaSrc == 0 .and. !s_lRecursive

            CLS

            IF s_nlang     == LANG_PT  /* portuguese brazilian */
               s_cAlertMsg := "N∆o h† nenhum prg na pasta "+CurDir()+". Use o modo recursivo -r"
            ELSEIF s_nLang == LANG_ES  /* spanish              */
               s_cAlertMsg := "No hay ning£n prg en la carpeta "+CurDir()+". Use lo modo recursivo -r"
            ELSE                       /* english              */
               s_cAlertMsg := "Does not have any prg in "+CurDir()+" folder. Use the recursive mode -r"
            ENDIF

            Alert( s_cAlertMsg )
            RESTSCREEN( cScreen )
            RETURN PGM_ERR
         ENDIF

         s_lEditMake := .F.

         s_nMakeFileHandle := FCreate( cFile )

         if s_nMakeFileHandle == F_ERROR

            CLS

            IF s_nLang     == LANG_PT  /* portuguese brazilian */
               s_cAlertMsg := "<"+cFile + "> n∆o pode ser criado."
            ELSEIF s_nLang == LANG_ES  /* spanish              */
               s_cAlertMsg := "<"+cFile + "> no pode ser criado."
            ELSE                       /* english             */
               s_cAlertMsg := "<"+cFile + "> cannot be created."
            ENDIF

            Alert( s_cAlertMsg+" FERROR ("+Ltrim(Str(FError()))+")" )
            RESTSCREEN( ,,,, cScreen )
            RETURN PGM_ERR

         endif

         WriteMakeFileHeader()
         lNew := .T.

      ELSE
         RESTSCREEN( ,,,, cScreen )
         RETURN PGM_ERR
      ENDIF

   ELSE

      s_nMakeFileHandle := FCreate( cFile )

      if s_nMakeFileHandle == F_ERROR

         CLS

         IF s_nLang     == LANG_PT     /* portuguese brazilian */
            s_cAlertMsg := "<"+cFile + "> n∆o pode ser criado."
         ELSEIF s_nLang == LANG_ES     /* spanish              */
            s_cAlertMsg := "<"+cFile + "> no pode ser criado."
         ELSE                          /* english             */
            s_cAlertMsg := "<"+cFile + "> cannot be created."
         ENDIF

         Alert( s_cAlertMsg+" FERROR ("+Ltrim(Str(FError()))+")" )
         RESTSCREEN( ,,,, cScreen )
         RETURN PGM_ERR

      ENDIF

      WriteMakeFileHeader()
      nOption := 2  // create a new makefile
      lNew := .T.

   ENDIF

IF ! lCreateAndCompile

While .t.

   CLS
   Setcolor( "w/b+,b+/w,w+/b,w/b+,w/b,w+/b" )
   @  0,  0, Maxrow(), Maxcol() BOX( Chr( 201 ) + Chr( 205 ) + Chr( 187 ) + Chr( 186 ) + Chr( 188 ) + Chr( 205 ) + Chr( 200 ) + Chr( 186 ) + Space( 1 ) )

   Attention( hbMakeID() + space(10) + s_aLangMessages[ 27 ], 0 )

   Attention( s_aLangMessages[ 47 ], maxrow() )

   @ 01,01       SAY s_aLangMessages[ 28 ]

   @ 01,16,06,21 GET cOS;
                 LISTBOX { "Win32", "OS/2", "Linux","Unix" };
                 MESSAGE s_aLangMessages[ 49 ];
                 STATE OsSpec(getlist,1,@cOS);
                 DROPDOWN

   @ 01,23       SAY s_aLangMessages[ 29 ]

   @ 01,47,08,52 GET cCompiler;
                 LISTBOX { "BCC", "MSVC", "GCC", "POCC","MINGW" };
                 MESSAGE s_aLangMessages[ 50 ];
                 STATE OsSpec(getlist,2,@cCompiler);
                 DROPDOWN

   @ 01,56       SAY s_aLangMessages[ 30 ]

   @ 01,67,10,78 GET cGuiLib;
                 LISTBOX { "None","C4W","FWH","GTWVT","GTWVW","GTWVW+MWVW","HWGUI","MINIGUI","XWT","HBWHAT","WHOO","XHGTK"};
                 STATE OsSpec(getlist,3,@cGuiLib);
                 DROPDOWN;
                 WHEN CheckCompiler(cOS);
                 MESSAGE s_aLangMessages[ 51 ]

   @ 02,01       SAY s_aLangMessages[ 48 ]

   @ 02,16,08,26 GET cRdd;
                 LISTBOX { "None","RDDADS","Mediator","Apollo"};
                 WHEN cOS == "Win32" .or. cOS == "Linux";
                 DROPDOWN;
                 MESSAGE s_aLangMessages[ 52 ]

   @ 02,30       GET s_lCompress;
                 CHECKBOX;
                 CAPTION s_aLangMessages[ 53 ];
                 STYLE "[X ]";
                 MESSAGE s_aLangMessages[ 54 ]

   @ 02,53       GET lUseHarbourDll;
                 CHECKBOX;
                 CAPTION "use harbour[.dll|.so]" style "[X ]";
                 WHEN cOS == "Win32" .or. cOS == "Linux";
                 MESSAGE s_aLangMessages[ 55 ]

   @ 03,01       SAY "Obj Files Dir";
                 GET cObjDir;
                 PICT "@S20";
                 MESSAGE s_aLangMessages[ 56 ]

   @ 03,47       SAY s_aLangMessages[ 66 ]

#ifdef __PLATFORM__WINDOWS
   @ 03,68,07,77 GET s_cEditor;
                 LISTBOX { "edit", "notepad" };
                 MESSAGE s_aLangMessages[ 67 ];
                 STATE   OsSpec(getlist,4,@s_cEditor);
                 DROPDOWN
#endif
   @ 04,01       SAY s_aLangMessages[ 45 ];
                 GET cAppName;
                 PICT "@S15";
                 VALID ! Empty( cAppName );
                 MESSAGE s_aLangMessages[ 57 ]

   @ 04,53       GET s_lasdll;
                 CHECKBOX;
                 CAPTION "Create dll";
                 STYLE "[X ]"

   READ MSG AT MaxRow() - 1, 1, MaxCol() - 1

   s_cAppName := alltrim( cAppName )

   IF cOS != "Linux" .or.  cOS != "Unix"
      IF s_lasdll
         s_cAppName += ".dll"
      ELSE
         s_cAppName += ".exe"
      ENDIF
   ENDIF

   if s_lasdll
      lUseharbourDll:= .T.
   endif

   lFwh      := "FWH"      $ alltrim(cGuiLib)
   lC4W      := "C4W"      $ alltrim(cGuiLib)
   lMiniGui  := "MINIGUI"  $ alltrim(cGuiLib)
   lHwGui    := "HWGUI"    $ alltrim(cGuiLib)
   lWhoo     := "WHOO"     $ alltrim(cGuiLib)
   lHBWhat   := "HBWHAT"   $ alltrim(cGuiLib)
   lGtWvt    := "GTWVT"    $ alltrim(cGuiLib)
   lGtWvw    := "GTWVW"    $ alltrim(cGuiLib)
   lMWvw     := "MWVW"     $ alltrim(cGuiLib)
   lXwt      := "XWT"      $ alltrim(cGuiLib)
   lxHGtk    := "XHGTK"    $ alltrim(cGuiLib)
   s_lGui := lWhoo .or. lFwh .or. lC4W .or. lMinigui .or. lGtWvt .or. lHwGui .or. lXwt .or. lHBWhat .or. lxHGtk .or. lGtWvw .or. lMWvw

   lRddAds   := "RDDADS"   $ cRdd
   lMediator := "Mediator" $ cRdd
   lApollo   := "Apollo"   $ cRdd

   IF lUseharbourDll
      cDefLibGccLibs := cHARso
      cDefaultLibs   := cHarbDll + " dllmain.lib "
   ENDIF

   IF lFwh
      @  3, 40 SAY "FWH path";
               GET cFwhPath;
               PICT "@S25"
   ELSEIF lC4W
      @  3, 40 SAY "C4W path";
               GET cC4WPath;
               PICT "@S25"
   ELSEIF lMiniGui
      @  3, 40 SAY "MiniGui path";
               GET cMiniPath;
               PICT "@S25"
   ELSEIF lHwGui
      @  3, 40 SAY "HwGUI path";
               GET cHwPath;
               PICT "@S25"
   ELSEIF lxHGtk
      @  3, 40 SAY "xHGtk path";
               GET cxHGPath;
               PICT "@S25"
   ENDIF

   IF lMediator
      @  3, 40 SAY "Mediator path";
               GET cMedPath;
               PICT "@S25"
   ENDIF

   IF lApollo
      @ 03, 40 SAY "Apollo path";
               GET cApolloPath;
               PICT "@S25"
   ENDIF

   IF nOption == 2 // create a new makefile
      cResName := PadR(alltrim(cResName)+iIF(!empty(cResName)," ","")+alltrim(cAllRes),200 )
   ENDIF

   Attention( s_aLangMessages[ 31 ], 5 )

   @ 06, 01 GET lAutoMemVar;
            CHECKBOX;
            CAPTION s_aLangMessages[ 32 ];
            STYLE "[X ]"

   @ 06, 40 GET lVarIsMemVar;
            CHECKBOX;
            CAPTION s_aLangMessages[ 33 ];
            STYLE "[X ]"

   @ 07, 01 GET lDebug;
            CHECKBOX;
            CAPTION s_aLangMessages[ 34 ];
            STYLE "[X ]"

   @ 07, 40 GET lSupressLine;
            CHECKBOX;
            CAPTION s_aLangMessages[ 35 ];
            STYLE "[X ]"

   @ 08, 01 GET s_lGenppo;
            CHECKBOX;
            CAPTION s_aLangMessages[ 36 ];
            STYLE "[X ]"

   @ 08, 40 GET lCompMod;
            CHECKBOX;
            CAPTION s_aLangMessages[ 37 ];
            STYLE "[X ]"

   @ 09, 01 SAY s_aLangMessages[ 38 ];
            GET s_cUserDefine;
            PICT "@s23"

   @ 09, 40 SAY s_aLangMessages[ 39 ];
            GET s_cUserInclude;
            PICT "@s18"

   @ 10, 01 GET s_lContribLib;
            CHECKBOX;
            CAPTION s_aLangMessages[ 40 ];
            STYLE "[X ]"

   @ 10, 40 GET s_lxFwh;
            CHECKBOX;
            CAPTION "Harbour FWH";
            STYLE "[X ]"

   @ 11, 01 SAY "Resource file Name: ";
            GET cResName;
            PICT "@S55"

   @ 12, 01 SAY s_aLangMessages[ 43 ];
            GET s_nFilestoAdd;
            PICT "99";
            VALID s_nFilestoAdd > 0

   @ 13, 01 GET s_lMt;
            CHECKBOX;
            CAPTION s_aLangMessages[ 44 ];
            STYLE "[X ]"

   @ 13, 40 SAY s_aLangMessages[ 46 ];
            GET s_nWarningLevel;
            PICT "9";
            VALID s_nWarningLevel>=0 .AND. s_nWarningLevel <= 4

   @ 14, 01 GET s_lGenCsource;
            CHECKBOX;
            CAPTION "Generate C-source, not PCode (-go3)";
            STYLE "[X ]"

   @ 15, 01 SAY s_aLangMessages[ 64 ];
            GET s_cUserLibs;
            PICT "@S58"

   READ msg at maxrow()-1,1,maxcol()-1

   IF LastKey() == K_ESC
      RESTSCREEN( ,,,, cScreen )
      RETURN PGM_ERR
   ENDIF

   IF Empty( cAppName )
      IF s_nLang     == LANG_PT        /* portuguese brazilian */
         Alert("Falta nome da aplicaá∆o.")
      ELSEIF s_nLang == LANG_ES        /* spanish              */
         Alert("Falta lo nombre de la aplicacion.")
      ELSE                             /* english              */
         Alert("Application name is missing.")
      ENDIF
   ELSE
      EXIT
   ENDIF

Enddo

Endif // Create and compile

   IF ! Empty( s_cUserDefine )
      aUserDefs := ListasArray2(Alltrim( s_cUserDefine ), ";")

      FOR EACH cCurrentDef in aUserDefs
         cHarbourFlags += " -D" + Alltrim( cCurrentDef ) + " "
      NEXT
   ENDIF

   IF ! Empty( s_cUserLibs )
      aUserLibs := ListAsArray2( alltrim(s_cUserLibs), ";" )
      FOR EACH cUserLib IN aUserLibs
          if !file( cUserLib )
             alert( "User Lib: "+cUserLib +" not found.")
             exit
          endif
      NEXT
   ENDIF

   IF ! Empty( s_cUserInclude )
      cHarbourFlags += " -I" + Alltrim( s_cUserInclude ) + " "
   ENDIF

   s_lBcc   := "BCC"   $ cCompiler
   s_lMSVcc := "MSVC"  $ cCompiler
   s_lGcc   := "GCC"   $ cCompiler
   s_lPocc  := "POCC"  $ cCompiler
   s_lMinGW := "MINGW" $ cCompiler

   if s_lMinGW
      s_lGcc := .T.
      s_lBcc := s_lMSVcc := s_lPocc := .F.
   endif

   cObjDir  := Alltrim( cObjDir )

   IF "Linux" $ cOS .or. "Unix" $ cOS
       cCurrentDir := "/"+CurDir()
   ELSE
       cCurrentDir := CurDrive()+":\"+CurDir()
   ENDIF

   IF ! Empty( cObjDir )

      IF !hb_DirExists( cObjDir )
#ifdef HB_COMPAT_C53
         MakeDir( cObjDir )
#endif
      ENDIF

   ENDIF

   s_aMacros := GetSourceDirMacros( s_lGcc, cOS )

   IF lLinux .or. lUnix
      cObjDir := Alltrim( cObjDir )

      IF ! Empty( cObjDir )
         cObjDir += "/"
      ENDIF

      cTest := cObjDir
   ELSE
      cObjDir := Alltrim( cObjDir )

      IF ! Empty( cObjDir )
         cObjDir += "\"
      ENDIF

      cTest := cObjDir + "\"
   ENDIF

   AEval( s_aMacros, { | x, y | cItem := Substr( x[ 2 ], 1, Len( x[ 2 ] ) ), IIF( At( citem, cTest ) > 0, ( s_aMacros[ y, 1 ] := "OBJ", s_aMacros[ y, 2 ] := cObjDir ), ) } )

   IF lAutomemvar
      cHarbourFlags += " -a "
   ENDIF

   IF lvarismemvar
      cHarbourFlags += " -v "
   ENDIF

   IF lDebug
      cHarbourFlags    += " -b "
      cDefaultLibs     += " hbdebug.lib "
      cDefGccLibs      += " -lhbdebug "
      cDefGccLibsw     += " -lhbdebug "
      cGccLibsOs2      += " -lhbdebug "
      cDefLibGccLibs   += " -lhbdebug "
      cDefGccLibsUnix  += " -lhbdebug "
      cDefGccLibsUnixMt+= " -lhbdebug "
      cDefaultLibsMt   += " hbdebug.lib "
      cDefGccLibsMt    += " -lhbdebug "
      cDefGccLibsMtw   += " -lhbdebug "
      cGccLibsOs2Mt    += " -lhbdebug "
      cDefLibGccLibsMt += " -lhbdebug "
   ENDIF

   IF lSupressline
      cHarbourFlags += " -l "
   ENDIF

   IF s_lGenppo
      cHarbourFlags += " -p "
   ENDIF

   IF lCompmod
      cHarbourFlags += " -m "
   ENDIF

   if lMWvw
      cHarbourFlags += " -u+mwvw.ch "
   endif

   IF s_nWarningLevel >= 0
      cHarbourFlags += " -w" + Str(s_nWarningLevel,1)
   ENDIF

   IF s_lBcc

      AAdd( s_aCommands, { ".cpp.obj:", "$(CC_DIR)\bin\bcc32 $(CFLAG1) $(CFLAG2) -o$* $**" } )
      AAdd( s_aCommands, { ".c.obj:", "$(CC_DIR)\bin\bcc32 -I$(HB_DIR)\include $(CFLAG1) $(CFLAG2) -o$* $**" } )
      AAdd( s_aCommands, { ".prg.obj:", "$(HB_DIR)\bin\harbour -n"+iif(s_lasdll,"1","")+" -go" + iif(s_lGenCsource,"3","") + " -I$(HB_DIR)\include $(HARBOURFLAGS)" + IIF( lFwh, " -I$(FWH)\include", IIF( lMinigui, " -I$(MINIGUI)\include",IIF( lHwgui, " -I$(HWGUI)\include","" ) ) )+IIF( lWhoo," -I$(WHOO)\include ","")+  IIF( lMediator," -I$(MEDIATOR)\include ","")+" -o$* $**" } )
      AAdd( s_aCommands, { ".rc.res:", "$(CC_DIR)\bin\brcc32 $(RFLAGS) $<" } )

   ELSEIF s_lGcc

      IF  ("LINUX" $ Upper( Getenv( "HB_ARCHITECTURE" ) )  .OR. cOS == "Linux" ) .or.;
          ("UNIX" $ Upper( Getenv( "HB_ARCHITECTURE" ) )  .OR. cOS == "Unix" .OR. cOS == "HP-UX" )
         AAdd( s_aCommands, { ".cpp.o:", "gcc $(CFLAG1) $(CFLAG2) -o$* $**" } )
         AAdd( s_aCommands, { ".c.o:", "gcc -I/usr/include/harbour $(CFLAG1) $(CFLAG2) -I. -g -o$* $**" } )
         AAdd( s_aCommands, { ".prg.o:", "harbour -n"+iif(s_lasdll,"1","")+"  -go" + iif(s_lGenCsource,"3","") + " -I/usr/include/harbour $(HARBOURFLAGS) -I.  -o$* $**" } )

      ELSE
         AAdd( s_aCommands, { ".cpp.o:", "$(CC_DIR)\bin\gcc $(CFLAG1) $(CFLAG2) -o$* $**" } )
         AAdd( s_aCommands, { ".c.o:", "$(CC_DIR)\bin\gcc -I$(HB_DIR)/include $(CFLAG1) $(CFLAG2) -I. -o$* $**" } )
         AAdd( s_aCommands, { ".prg.o:", "$(HB_DIR)\bin\harbour -n"+iif(s_lasdll,"1","")+" -go" + iif(s_lGenCsource,"3","") + " -I$(HB_DIR)/include $(HARBOURFLAGS) " +IIF( lHwgui, " -I$(HWGUI)/include","" ) +" -o$* $**" } )

      ENDIF

   ELSEIF s_lMSVcc

      AAdd( s_aCommands, { ".cpp.obj:", "$(CC_DIR)\bin\cl $(CFLAG1) $(CFLAG2) -Fo$* $**" } )
      AAdd( s_aCommands, { ".c.obj:", "$(CC_DIR)\bin\cl -I$(HB_DIR)\include $(CFLAG1) $(CFLAG2) -Fo$* $**" } )
      AAdd( s_aCommands, { ".prg.obj:", "$(HB_DIR)\bin\harbour -n -I$(HB_DIR)\include $(HARBOURFLAGS) -go" + iif(s_lGenCsource,"3","") + "  -I$(C4W)\include" + IIF( lMediator," -I$(MEDIATOR)\include ","")+ "-o$* $**" } )
      AAdd( s_aCommands, { ".rc.res:", "$(CC_DIR)\rc $(RFLAGS) $<" } )

   ELSEIF s_lPocc

      AAdd( s_aCommands, { ".cpp.obj:", "$(CC_DIR)\bin\pocc $(CFLAG1) $(CFLAG2) -Fo$* $**" } )
      AAdd( s_aCommands, { ".c.obj:", "$(CC_DIR)\bin\pocc -I$(HB_DIR)\include $(CFLAG1) $(CFLAG2) -Fo$* $**" } )
      AAdd( s_aCommands, { ".prg.obj:", "$(HB_DIR)\bin\harbour -n"+iif(s_lasdll,"1","")+" -go" + iif(s_lGenCsource,"3","") + " -I$(HB_DIR)\include $(HARBOURFLAGS)" + IIF( lFwh, " -I$(FWH)\include", IIF( lMinigui, " -I$(MINIGUI)\include",IIF( lHwgui, " -I$(HWGUI)\include","" ) ) )+IIF( lWhoo," -I$(WHOO)\include ","")+  IIF( lMediator," -I$(MEDIATOR)\include ","")+" -o$** $**" } )
      AAdd( s_aCommands, { ".rc.res:", "$(CC_DIR)\bin\porc $(RFLAGS) $<" } )

   ENDIF

   // Selecting .prg files.

   aInFiles := GetSourceFiles( s_lRecursive, s_lGcc, cOS )
   nLenaSrc := Len( aInFiles )

   IF nLenaSrc == 0

      IF s_nlang     == LANG_PT        /* portuguese brazilian */
         s_cAlertMsg := "Nenhum prg foi encontrado."
      ELSEIF s_nLang == LANG_ES        /* spanish              */
         s_cAlertMsg := "Ning£n prg foi encontrado."
      ELSE                             /* english              */
         s_cAlertMsg := "No one prg were found."
      ENDIF

      lCancelMake := .T.

      Alert( s_cAlertMsg )

   ENDIF

   aOutFiles := AClone( aInFiles )

   //if Len( aOutFiles ) > 1
   if Len( aOutFiles ) > 1 .AND. ! lCreateAndCompile

//      Attention( s_aLangMessages[ 41 ], 22 )

      if s_nlang     == LANG_PT        /* portuguese brazilian */
         s_cAlertMsg := "Selecione os .prgs a compilar"
      elseif s_nLang == LANG_ES        /* spanish             */
         s_cAlertMsg := "Seleccione los .prg a compilar"
      ELSE                             /* english             */
         s_cAlertMsg := "Select the .prg files to compile"
      endif

      IF nOption !=2 // not create a makefile
         PickArray( 11, 15, 20, 64, aInFiles, aOutFiles ,ArrayAJoin( { oMake:aPrgs, oMake:aCs } ), .T., s_cAlertMsg )
      ELSE
         PickArray( 11, 15, 20, 64, aInFiles, aOutFiles, {}, .T., s_cAlertMsg )
      ENDIF

      AEval( aOutFiles, { | x, y | HB_SYMBOL_UNUSED( x ), aOutFiles[ y ] := Trim( Substr( aOutFiles[ y ], 1, At( " ", aOutFiles[ y ] ) ) ) } )

      aOutFiles := ASort( aOutFiles )

      @ 22,01 say space( MAXCOL() - 1 )

      aSelFiles := GetSelFiles( aInFiles, aOutFiles )

      ASort( aSelFiles )

   else
       AEval( aOutFiles, { | x, y | HB_SYMBOL_UNUSED( x ), aOutFiles[ y ] := Trim( Substr( aOutFiles[ y ], 1, At( " ", aOutFiles[ y ] ) ) ) } )
       aSelFiles := aOutFiles
   endif

   if Len( aSelFiles ) == 1

      cTopFile := aSelFiles[1]
      cTopFile := PadR( Left(cTopfile,At(Upper(".prg"),Upper(cTopFile))+4 ), 50)

   elseif Len( aSelFiles ) == 0

      cTopFile := ""

      IF s_nlang     == LANG_PT        /* portuguese brazilian */
         s_cAlertMsg := "Nenhum .prg foi selecionado."
      ELSEIF s_nLang == LANG_ES        /* spanish              */
         s_cAlertMsg := "Ning£m .prg foi seleccionado."
      ELSE                             /* english              */
         s_cAlertMsg := "No .prg files were selected."
      ENDIF

      Alert( s_cAlertMsg )

   endif

   WHILE Len( aSelFiles ) > 1

     IF !lCreateAndCompile

      IF s_nlang     == LANG_PT        /* portuguese brazilian */
         s_cAlertMsg := "Informe o .prg principal da sua aplicaá∆o:"
      ELSEIF s_nLang == LANG_ES        /* spanish              */
         s_cAlertMsg := "Informe o .prg principale de su aplicacion:"
      ELSE                             /* english              */
         s_cAlertMsg := "Inform the main .prg of your application:"
      ENDIF

      @ 15,01 say s_cAlertMsg Get cTopFile pict "@S35" valid !empty(cTopFile)
      READ

      if LastKey() == K_ESC
         Exit
      endif

     ELSE
      cTopFile := alltrim(cAppName)+".prg"
     ENDIF

      IF ! File( alltrim(cTopFile) )

         IF s_nLang     == LANG_PT     /* portuguese brazilian */
            s_cAlertMsg := "Arquivo "+alltrim(cTopFile)+" n∆o encontrado."+iif(s_lRecursive," O flag -r est† ativo. Informe o subdir tambÇm se o .prg principal estiver dentro dele.","")
         ELSEIF s_nLang == LANG_ES     /* spanish              */
            s_cAlertMsg := "Fichero "+alltrim(cTopFile)+" no encontrado."+iif(s_lRecursive," Lo flag -r esta activado. Informe lo subdir tambiÇn si lo .prg principale est†s dentro dele.","")
         ELSE                          /* english              */
            s_cAlertMsg := "File "+alltrim(cTopFile)+" not found."+iif(s_lRecursive," The flag -r is active. Inform the subdir also if the main .prg is within it.","")
         ENDIF

         Alert( s_cAlertMsg )

         IF lCreateAndCompile
            FClose( s_nMakeFileHandle )
            s_nMakeFileHandle:= F_ERROR  // Invalid handle now file is closed
            FErase( cFile )
            RESTSCREEN( ,,,, cScreen )
            RETURN PGM_ERR
         ENDIF

      ELSE
         EXIT
      ENDIF

   END

   // Select Contrib Libs.
   IF s_lContribLib

      aLibs := GetLibs( s_lGcc, cHarbourLibDir )

      if len(aLibs)=0
         alert("aLibs is empty")
      endif

      IF s_nLang     == LANG_PT        /* portuguese brazilian */
         s_cAlertMsg := "<Espaáo> para selecionar. <Enter> para continuar o processo."
      ELSEIF s_nLang == LANG_EN        /* english              */
         s_cAlertMsg := "<Spacebar> to select. <Enter> to continue process"
      ELSEIF s_nLang == LANG_ES        /* spanish              */
         s_cAlertMsg := "<Espacio> para seleccionar. <Enter> para continuar o proceso."
      ENDIF

      Attention( s_cAlertMsg, 9 )

      AEval( aLibs, { | x | AAdd( aLibsIn, x[ 1 ] ) } )
      AEval( aLibs, { | x | AAdd( aLibsOut, x[ 2 ] ) } )

      if s_nlang     == LANG_PT        /* portuguese brazilian */
         s_cAlertMsg := "Selecione as LIBs contrib a compilar"
      elseif s_nLang == LANG_ES        /* spanish              */
         s_cAlertMsg := "Seleccione las LIB contrib a compilar"
      ELSE                             /* english              */
         s_cAlertMsg := "Select the contrib LIBs to compile"
      endif

      IF nOption != 2 // not create makefile
         PickArray( 11, 15, 20, 64, aLibsIn, aLibsOut ,oMake:aExtLibs, .T. , s_cAlertMsg, .T. )
      ELSE
         PickArray( 11, 15, 20, 64, aLibsIn, aLibsOut ,{}, .T., s_cAlertMsg, .T. )
      ENDIF

   ENDIF

#IFDEF HBM_USE_DEPENDS

   if ! lCreateAndCompile

   clear typeahead

   Attention( "HbMake options", 16 )

   @ 17, 01 GET lScanIncludes;
            CHECKBOX;
            CAPTION "Create #DEPENDS from #include";
            STYLE "[X ]"
   // Provisions for recursive scanning
   @ 17, 40 GET lScanIncRecursive;
            CHECKBOX;
            CAPTION "Scan recursive" style "[X ]" //when lScanIncludes
   @ 18, 01 SAY "Excluding these extensions :" ;
            GET cExcludeExts;
            WHEN lScanIncludes
   READ

   Endif

#ENDIF

   if ! lCreateAndCompile

   AEval( aOutFiles, { | xItem | IIF(  ".c" $ xItem  .OR.  ".C" $ xItem , AAdd( aOutc, xItem ), ) } )
   AEval( aOutc, { | x, z | cItem := x, z := AScan( aOutFiles, { | t | t = cItem } ), IIF( z > 0, aSize( aDel( aOutFiles, z ), Len( aOutFiles ) - 1 ), ) } )

   @ 22,01 say space(78)

   aOutFiles  := ASort( aOutFiles )

   else
      aOutFiles := { alltrim(cAppName) }
   endif

   s_aPrgs    := AClone( aOutFiles )
   s_aObjs    := AClone( aOutFiles )

   s_aContribLibs := AClone( aLibsOut )

   // searching for main prg file into obj array.
   // TOFIX: Needs to be case sensitive for non-WinDOS [vszakats]
   x := AScan( s_aObjs, { | x | Lower( x ) $ Lower( alltrim(cTopFile) ) } )

   // putting main prg in the top
   IF x > 0
      ADel( s_aObjs, x )
      ASize( s_aObjs, Len( s_aObjs ) - 1 )
      ASize( s_aObjs, Len( s_aObjs ) + 1 )
      AIns( s_aObjs, 1 )
      s_aObjs[ 1 ] := AllTrim( cTopFile )
   ENDIF

   // searching for main prg file into prg array.
   // TOFIX: Needs to be case sensitive for non-WinDOS [vszakats]
   x := AScan( s_aPrgs, { | x | Lower( x ) $ Lower( alltrim(cTopFile) ) } )

   // putting main prg in the top
   IF x > 0
      ADel( s_aPrgs, x )
      ASize( s_aPrgs, Len( s_aPrgs ) - 1 )
      ASize( s_aPrgs, Len( s_aPrgs ) + 1 )
      AIns( s_aPrgs, 1 )
      s_aPrgs[ 1 ] :=  AllTrim( cTopFile )
   ENDIF

   AEval( s_aObjs, { | xItem, x | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cext := Substr( cExt, 2 ), IIF( ! s_lGcc, s_aObjs[ x ] := cObjDir + cTest + "." + Exte( cExt, 2 ), s_aObjs[ x ] := cObjDir + cTest + "." + Exte( cExt, 3 ) ) } )
   s_aCFiles := aClone( aOutc )
   s_aObjsC := aClone( aOutc )
   AEval( aOutc, { | xItem, x | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cext := Substr( cExt, 2 ), IIF( ! s_lGcc, s_aObjsC[ x ] := IIF( ! Empty( cObjDir ), cObjDir, "" ) + cTest + "." + Exten( cExt, 2 ), s_aObjsC[ x ] := IIF( ! Empty( cObjDir ), cObjDir, "" ) + cTest + "." + Exten( cExt, 1 ) ) } )

   FWrite( s_nMakeFileHandle, ;
      "RECURSE      = " + IIF( s_lRecursive, " YES ", " NO " ) + s_cEOL +;
      s_cEOL +;
      "SHELL        = " + s_cEOL +;
      "COMPRESS = " + IIF( s_lCompress, "YES", "NO" ) + s_cEOL +;
      "CONTRIBS     = " + IIF( s_lContribLib, "YES", "NO" )    + s_cEOL +;
      "XFWH         = " + IIF( s_lxFwh, "YES", "NO" ) + s_cEOL +;
      "FILESTOADD   = " + LTrim( Str( s_nFilesToAdd, 2 ) ) + s_cEOL +;
      "WARNINGLEVEL = " + LTrim( Str( s_nWarningLevel, 2) ) + s_cEOL +;
      "USERDEFINE   = " + alltrim(s_cUserDefine) + s_cEOL +;
      "USERINCLUDE  = " + alltrim(s_cUserInclude) + s_cEOL +;
      "USERLIBS     = " + alltrim(s_cUserLibs) + s_cEOL +;
      "EDITOR       = " + s_cEditor + s_cEOL )

   IF lFwh
      FWrite( s_nMakeFileHandle, "FWH = " + alltrim(cFwhPath) + s_cEOL )
   ELSEIF lC4W
      FWrite( s_nMakeFileHandle, "C4W = " + alltrim(cC4WPath) + s_cEOL )
   ELSEIF lMiniGui
      FWrite( s_nMakeFileHandle, "MINIGUI = " + alltrim(cMiniPath) + s_cEOL )
   ELSEIF lHwGui
      FWrite( s_nMakeFileHandle, "HWGUI = " + iif(!s_lMinGW,alltrim(cHwPath),strtran(alltrim(cHwPath),"\","/")) + s_cEOL )
   ELSEIF lGtwvt
      FWrite( s_nMakeFileHandle, "GTWVT = " + s_cEOL )
   ELSEIF lGtwvw
      FWrite( s_nMakeFileHandle, "GTWVW = " + s_cEOL )

      IF lMwvw
         FWrite( s_nMakeFileHandle, "MWVW = " + s_cEOL )
      endif

   ELSEIF lXwt
      FWrite( s_nMakeFileHandle, "XWT = " + s_cEOL )
   ELSEIF lWhoo
      FWrite( s_nMakeFileHandle, "WHOO = " + s_cEOL )
   ELSEIF lHBWhat
      FWrite( s_nMakeFileHandle, "HBWHAT = " + s_cEOL )
   ELSEIF lxHGtk
      FWrite( s_nMakeFileHandle, "XHGTK = " + s_cEOL )
   ENDIF

   IF lMediator
      FWrite( s_nMakeFileHandle, "MEDIATOR = " + alltrim(cMedPath) + s_cEOL )
   ENDIF

   IF lApollo
      FWrite( s_nMakeFileHandle, "APOLLO = " + alltrim(cApolloPath) + s_cEOL )
   ENDIF

   FWrite( s_nMakeFileHandle, "GUI = " + iif(lWhoo .or. lFwh .or. lC4W .or. lMinigui .or. lGtWvt .or. lHwGui .or. lXwt .or. lHBWhat .or. lxHGtk .or. lGtWvw , "YES", "NO" ) + s_cEOL +;
      "MT = " + IIF( s_lMt, "YES", "NO" ) + s_cEOL )

   FOR x := 1 TO Len( s_aMacros )

      IF ! Empty( s_aMacros[ x, 2 ] )

         cItem := s_aMacros[ x, 2 ]
         nPos  := AScan( s_aPrgs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

         IF nPos > 0
            AEval( s_aPrgs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aPrgs[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ")\" ) ), ) } )

            IF ! s_aMacros[ x, 3 ]
               FWrite( s_nMakeFileHandle, s_aMacros[ x, 1 ] + " = " + Left( s_aMacros[ x, 2 ], Len( s_aMacros[ x, 2 ] ) - 1 ) + " " + s_cEOL )
               s_aMacros[ x, 3 ] := .T.
            ENDIF

         ENDIF

         nPos := AScan( s_aCFiles, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

         IF nPos > 0

            IF ! s_aMacros[ x, 3 ]
               AEval( s_aCFiles, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aCFiles[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ")\" ) ), ) } )
               FWrite( s_nMakeFileHandle, s_aMacros[ x, 1 ] + " = " + Left( s_aMacros[ x, 2 ], Len( s_aMacros[ x, 2 ] ) - 1 ) + " " + s_cEOL )
               s_aMacros[ x, 3 ] := .T.
            ENDIF

         ENDIF

         nPos := AScan( s_aObjs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

         IF nPos > 0

            IF ! Empty( cObjDir )
               AEval( s_aObjs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aObjs[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ")\" ) ), ) } )
               FWrite( s_nMakeFileHandle, s_aMacros[ x, 1 ] + " = " + Left( s_aMacros[ x, 2 ], Len( s_aMacros[ x, 2 ] ) - 1 ) + " " + s_cEOL )
            ENDIF

         ENDIF

         nPos := AScan( s_aObjsC, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

         IF nPos > 0

            IF ! Empty( cObjDir )
               AEval( s_aObjsC, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aObjsC[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ")\" ) ), ) } )
            ENDIF

         ENDIF

      ENDIF

   NEXT

   IF s_lGcc
      IF ( "LINUX" $ Upper( Getenv( "HB_ARCHITECTURE" ) )  .OR. cOS == "Linux" ) .or.;
         ( "UNIX" $ Upper( Getenv( "HB_ARCHITECTURE" ) )  .OR. cOS == "Unix" .or. cOS == "HP-UX" )
         FWrite( s_nMakeFileHandle, "PROJECT = " + Alltrim( cAppName ) + " $(PR) " + s_cEOL )
      ELSE
         FWrite( s_nMakeFileHandle, "PROJECT = " + Alltrim( cAppName ) + ".exe"   + " $(PR) " + s_cEOL )
      ENDIF
   ELSE
      FWrite( s_nMakeFileHandle, "PROJECT = " + Alltrim( cAppName ) + iif(s_lasdll,".dll",".exe" ) + " $(PR) " + s_cEOL )
   ENDIF

   FWrite( s_nMakeFileHandle, "OBJFILES =" )

   IF Len( s_aObjs ) < 1
      FWrite( s_nMakeFileHandle, + " $(OB) " + s_cEOL )
   ELSE
      AEval( s_aObjs, { | x, i | nWriteFiles ++, IIF( ( i <> Len( s_aObjs ) .AND. x <> alltrim(cTopfile)  ), FWrite( s_nMakeFileHandle, " " + Alltrim( x ) + IIF( nWriteFiles % s_nFilestoAdd == 0, " //" + s_cEOL, "" ) ), FWrite( s_nMakeFileHandle, " " + Alltrim( x ) + " $(OB) " + s_cEOL ) ) } )
   ENDIF

   nWriteFiles := 0
   FWrite( s_nMakeFileHandle, "PRGFILES =" )

   IF Len( s_aPrgs ) < 1
      FWrite( s_nMakeFileHandle, + " $(PS)" + s_cEOL )
   ELSE
      AEval( s_aPrgs, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aPrgs ), FWrite( s_nMakeFileHandle, " " + Alltrim( x ) + IIF( nWriteFiles % s_nFilestoAdd == 0, " //" + s_cEOL, "" ) ), FWrite( s_nMakeFileHandle, " " + Alltrim( x ) + " $(PS) " + s_cEOL ) ) } )
   ENDIF

   nWriteFiles := 0
   FWrite( s_nMakeFileHandle, "OBJCFILES =" )

   IF Len( s_aObjsC ) < 1
      FWrite( s_nMakeFileHandle, + " $(OBC) " + s_cEOL )
   ELSE
      AEval( s_aObjsC, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aObjsC ), FWrite( s_nMakeFileHandle, " " + Alltrim( x ) + IIF( nWriteFiles % s_nFilestoAdd == 0, " //" + s_cEOL, "" ) ), FWrite( s_nMakeFileHandle, " " + Alltrim( x ) + " $(OBC) " + s_cEOL ) ) } )
   ENDIF

   nWriteFiles := 0
   FWrite( s_nMakeFileHandle, "CFILES =" )

   IF Len( s_aCFiles ) < 1
      FWrite( s_nMakeFileHandle, + " $(CF)" + s_cEOL )
   ELSE
      AEval( s_aCFiles, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aCFiles ), FWrite( s_nMakeFileHandle, " " + Alltrim( x ) + IIF( nWriteFiles % s_nFilestoAdd == 0, " //" + s_cEOL, "" ) ), FWrite( s_nMakeFileHandle, " " + Alltrim( x ) + " $(OB) " + s_cEOL ) ) } )
   ENDIF

   cResName := alltrim(cResName)
   FWrite( s_nMakeFileHandle, ;
      "RESFILES = " + cResName + s_cEOL +;
      "RESDEPEN = " + StrTran( cResName, ".rc", ".res" ) + s_cEOL +;
      "TOPMODULE = " + alltrim(cTopFile) + s_cEOL )

   IF lRddads
      cDefaultLibs     += " rddads.lib ace32.lib"
      cDefLibGccLibs   += " -lrddads -ladsloc "
      cDefaultLibsMt   += " rddads.lib ace32.lib"
      cDefLibGccLibsMt += " -lrddads -ladsloc "
      cDefGccLibsw     += " -lrddads -lads32 "
      cExtraLibs += " -lrddads -ladsloc "
   ENDIF

   // remove obsolete bcc640xx.lib
   //
   IF "bcc640" $ cDefaultLibs
      cDefaultLibs   := StrTran( cDefaultLibs, "bcc640.lib", "")
   ENDIF

   IF "bcc640" $ cDefaultLibsMt
      cDefaultLibsMt := StrTran( cDefaultLibsMt, "bcc640mt.lib", "")
   ENDIF

   // if Contrib libs were selected...
   IF Len( aLibsOut ) > 0 .AND. s_lContribLib

      IF s_lMSVcc .OR. s_lBcc .OR. s_lPocc

         IF ! s_lMt
            cOldLib := cDefaultLibs
         ELSE
            cOldLib := cDefaultLibsMt
         ENDIF

         // searching for html lib...
         nPos := AScan( aLibsOut, { | z | At( "html", Lower( z ) ) > 0 } )

         IF nPos > 0
            cHtmlLib += aLibsOut[ nPos ]
            aDel( aLibsOut, nPos )
            aSize( aLibsOut, Len( aLibsOut ) - 1 )
            cOldLib := StrTran( cOldLib, "gtwin" , "gtcgi" )
         ENDIF

         // searching for mysql lib...
         AEval( aLibsOut, { | cLib | cLibs += " " + cLib } )
         nPos := AScan( aLibsOut, { | z | At( "mysql", Lower( z ) ) > 0 } )

         IF nPos >0
            cLibs += " libmysql.lib"
         ENDIF

         // searching for postgre lib...
         nPos := AScan( aLibsOut, { | z | At( "hbpg", Lower( z ) ) > 0 } )
         IF nPos >0
            cLibs += " libpq.lib"
            cLibs := strtran(cLibs,"hbpg","hbpg")
         ENDIF

         IF ! s_lmt
            cDefaultLibs   := cHtmlLib + " " + cOldLib
         ELSE
            cDefaultLibsMt := cHtmlLib + " " +cOldLib
         ENDIF

      ENDIF

      IF s_lGcc

         nPos := AScan( aLibsOut, { | z | At( "html", Lower( z ) ) > 0 } )

         IF nPos > 0
            cHtmlLib += "-l" + Strtran( aLibsOut[ nPos ], ".a", "" )
            aDel( aLibsOut, nPos )
            aSize( aLibsOut, Len( aLibsOut ) - 1 )
         ENDIF

         AEval( aLibsOut, { | cLib | iif( Len(aTempLibs :=ListAsArray2( cLib, " ") )> 0 ,cLibs += SetthisLibs(AtempLibs) ,cLibs += " -l" + Strtran( cLib, ".a", "" ))} )

         nPos := AScan( aLibsOut, { | z | At( "mysql", Lower( z ) ) > 0 } )

         if nPos >0
            cLibs += " -lmysqlclient"
         endif
         nPos := AScan( aLibsOut, { | z | At( "hbpg", Lower( z ) ) > 0 } )

         if nPos >0
            cLibs += " -lpq"
         endif

         cExtraLibs := cLibs

         IF cOS == "Linux"

            IF ! s_lMt
               cOldLib        := " " + cDefLibGccLibs
               cDefLibGccLibs := cHtmlLib + " " + cOldLib + " " + cLibs

               IF "html" $ cDefLibGccLibs
                   cDefLibGccLibs := StrTran( cDefLibGccLibs, "gtcrs" , "gtcgi" )
                   cDefLibGccLibs := StrTran( cDefLibGccLibs, "ncurses" , "" )
               ENDIF

            ELSE

               cOldLib          := " " + cDefLibGccLibsMt
               cDefLibGccLibsMt := cHtmlLib + " " + cOldLib + " " + cLibs

               IF "html" $ cDefLibGccLibsMt
                   cDefLibGccLibsMt := StrTran( cDefLibGccLibsMt, "gtcrs" , "gtcgi" )
                   cDefLibGccLibsMt := StrTran( cDefLibGccLibsMt, "ncurses" , "" )
               ENDIF

           ENDIF

         ELSEIF cOS == "OS/2"

            IF ! s_lMt
               cOldLib     := " " + cGccLibsOs2
               cGccLibsOs2 := cHtmlLib + " " + cOldLib + " " + cLibs

               IF "html" $ cGccLibsOs2
                   cGccLibsOs2 := StrTran( cGccLibsOs2, "gtos2" , "gtcgi" )
               ENDIF

            ELSE
               cOldLib       := " " + cGccLibsOs2Mt
               cGccLibsOs2Mt := cHtmlLib + " " + cOldLib + " " + cLibs
               IF "html" $ cGccLibsOs2Mt
                   cGccLibsOs2Mt := StrTran( cGccLibsOs2Mt, "gtos2" , "gtcgi" )
               ENDIF

            ENDIF

         ELSE

            IF s_lMt
               cOldLib       := " " + cDefGccLibsMt
               cDefGccLibsMt := cHtmlLib + " " + cOldLib + " " + cLibs
            else
               cOldLib       := " " + cDefGccLibsw
               cDefGccLibsw  := cHtmlLib + " " + cOldLib + " " + cLibs
            endif

        ENDIF

      ENDIF

   ENDIF

   IF s_lBcc .OR. s_lMSVcc .OR. s_lPocc
      if lFwh .or. lMiniGui .or. lC4W .or. lWhoo .or. lHwGui .or. lHBWhat
            cDefaultLibs   := strtran(cDefaultLibs,"gtwin.lib","gtgui.lib")
            cDefaultLibsMt := strtran(cDefaultLibsMt,"gtwin.lib","gtgui.lib")
      endif

      IF lFwh
         IF s_lxFwh
            FWrite( s_nMakeFileHandle, "HBLIBS = $(FWH)\lib\fivehx.lib $(FWH)\lib\fivehc.lib " + IIF( ! s_lMt, cDefaultLibs, cDefaultLibsMt ) + s_cEOL )
         ELSE
            FWrite( s_nMakeFileHandle, "HBLIBS = $(FWH)\lib\fiveh.lib $(FWH)\lib\fivehc.lib " + IIF( ! s_lMt, cDefaultLibs, cDefaultLibsMt ) + s_cEOL )
         ENDIF
      ELSEIF lMiniGui
         FWrite( s_nMakeFileHandle, "HBLIBS = minigui.lib " + IIF( ! s_lMt, cDefaultLibs, cDefaultLibsMt ) + s_cEOL )
      ELSEIF lWhoo
         FWrite( s_nMakeFileHandle, "HBLIBS = whoo.lib hbwhat.lib " + IIF( ! s_lMt, cDefaultLibs, cDefaultLibsMt ) + s_cEOL )
      ELSEIF lHBWhat
         FWrite( s_nMakeFileHandle, "HBLIBS = hbwhat.lib " + IIF( ! s_lMt, cDefaultLibs, cDefaultLibsMt ) + s_cEOL )
      ELSEIF lHwGui
         FWrite( s_nMakeFileHandle, "HBLIBS = hwgui.lib procmisc.lib hwg_qhtm.lib " + IIF( ! s_lMt, cDefaultLibs, cDefaultLibsMt ) + s_cEOL )
      ELSEIF lC4W
         FWrite( s_nMakeFileHandle, "HBLIBS = $(C4W)\c4wclass.lib $(C4W)\wbrowset.lib $(C4W)\otabt.lib $(C4W)\clip4win.lib "  + IIF( ! s_lMt, cDefaultLibs, cDefaultLibsMt ) + s_cEOL )
      ELSE
         if lGtwvt
            cDefaultLibs   := strtran(cDefaultLibs,"gtwin.lib","gtwvt.lib")
            cDefaultLibsMt := strtran(cDefaultLibsMt,"gtwin.lib","gtwvt.lib")
         elseif lGtwvw
            cDefaultLibs   := iif(lMWvW,"mwvw.lib ", "") + strtran(cDefaultLibs,"gtwin.lib","gtwvw.lib ")
            cDefaultLibsMt := iif(lMWvW,"mwvw.lib ", "") + strtran(cDefaultLibsMt,"gtwin.lib","gtwvw.lib ")

         endif

         FWrite( s_nMakeFileHandle, "HBLIBS = " + IIF( ! s_lMt, cDefaultLibs, cDefaultLibsMt ) + s_cEOL )
      ENDIF

   ELSEIF s_lGcc

      IF cOS == "Linux"
         FWrite( s_nMakeFileHandle, "HBLIBS = " + IIF(lusexhb, cExtraLibs , "-Wl,--start-group " + IIF( ! s_lMt, cDefLibGccLibs, cDefLibGccLibsMt ) + " -Wl,--end-group " + cSystemLibs ) + s_cEOL )
      ELSEIF cOS == "Unix" .or.  cOS == "UP-UX"
          FWrite( s_nMakeFileHandle, "HBLIBS = " + IIF(lusexhb, cExtraLibs , " " + IIF( ! s_lMt, cDefGccLibsUnix, cDefGccLibsUnixmt ) + " " + cSystemLibs ) + s_cEOL )
      ELSEIF cOS == "OS/2"
         FWrite( s_nMakeFileHandle, "HBLIBS = " + IIF( ! s_lMt, cGccLibsOs2, cGccLibsOs2Mt ) + s_cEOL )
      ELSEIF  "MINGW" $ cCompiler
         IF lHwGui
            cDefGccLibsw :=strtran( cDefGccLibsw,"-lgtwin" ,"-lgtgui")
            cDefGccLibsMtw :=strtran( cDefGccLibsMtw,"-lgtwin" ,"-lgtgui")
            FWrite( s_nMakeFileHandle, "HBLIBS = -Wl,--allow-multiple-definition -Wl,--start-group -lhwgui -lprocmisc -lhwg_qhtm " +  IIF( ! s_lMt, cDefGccLibsw, cDefGccLibsMtw ) + " -Wl,--end-group " + s_cEOL)
         else
            FWrite( s_nMakeFileHandle, "HBLIBS = -Wl,--allow-multiple-definition -Wl,--start-group " + IIF( ! s_lMt, cDefGccLibsw, cDefGccLibsMtw ) + " -Wl,--end-group " + s_cEOL )
         endif
      ELSE
         FWrite( s_nMakeFileHandle, "HBLIBS = " + IIF( ! s_lMt, cDefGccLibs, cDefGccLibs ) + s_cEOL )
      ENDIF

   ENDIF

   nWriteFiles := 0
   FWrite( s_nMakeFileHandle, "CONTRIBLIBS =" )

   if Len(s_aContribLibs) < 1
      FWrite( s_nMakeFileHandle, s_cEOL )
   else
      AEval( s_aContribLibs, { | x | nWriteFiles ++, FWrite( s_nMakeFileHandle, " " + Alltrim( x )  ) } )
      FWrite( s_nMakeFileHandle, s_cEOL )
   endif

   FWrite( s_nMakeFileHandle, ;
      "DEFFILE = " + s_cEOL +;
      "HARBOURFLAGS = " + cHarbourFlags + s_cEOL )

   IF s_lBcc

      FWrite( s_nMakeFileHandle, "CFLAG1 =  -OS $(SHELL)  $(CFLAGS) -d -c -L$(HB_DIR)\lib"+iif(lFwh,";$(FWH)\lib ","")+iif(!empty(s_cUserInclude)," -I" + alltrim( s_cUserInclude ),"") + " " +s_cEOL +;
         "CFLAG2 =  -I$(HB_DIR)\include;$(CC_DIR)\include" + iif( s_lMt, " -DHB_THREAD_SUPPORT " , "" ) + s_cEOL )

      /* added "-x" flag to LFLAGS statment to suppress creation of map file and speed up link. */
      FWrite( s_nMakeFileHandle, ;
         "RFLAGS = " + s_cEOL +;
         "LFLAGS = -L$(CC_DIR)\lib\obj;$(CC_DIR)\lib;$(HB_DIR)\lib -Gn -M -m -s -Tp"+ iif(s_lasdll,"d","e") + " -x" + IIF( lFWH .or. lMiniGui .or. lWhoo .or. lHwgui .or. lGtWvt .or. lGtWvw ," -aa"," -ap") + IIF( lMinigui, " -L$(MINIGUI)\lib",IIF( lFwh, " -L$(FWH)\lib",IIF( lHwgui, " -L$(HWGUI)\lib","" ))) + s_cEOL +;
         "IFLAGS = " + s_cEOL +;
         "LINKER = ilink32" + s_cEOL +;
         " " + s_cEOL +;
         "ALLOBJ = " + IIF( ( lWhoo .OR. lHBWhat .OR. lFwh .OR. lMinigui .OR. lHwgui .or. lGtWvt .or. lGtWvw .or. lXwt .or. lxHGtk ), "c0w32.obj", iif(s_lAsDll,"c0d32.obj","c0x32.obj" )) + " $(OBJFILES) $(OBJCFILES)" + s_cEOL +;
         "ALLRES = $(RESDEPEN)" + s_cEOL +;
         "ALLLIB = $(USERLIBS) $(CONTRIBLIBS) $(HBLIBS) import32.lib " + IIF( s_lMt,"cw32mt.lib", "cw32.lib" )+ s_cEOL +;
         ".autodepend" + s_cEOL )

   ELSEIF s_lMSVcc

      FWrite( s_nMakeFileHandle, ;
         "CFLAG1 =  -I$(INCLUDE_DIR) -TP -W3 -nologo $(C_USR) $(SHELL)  $(CFLAGS)" +IIF( s_lMt, " -DHB_THREAD_SUPPORT " , "" ) + s_cEOL +;
         "CFLAG2 =  -c" +" -I" + alltrim( s_cUserInclude ) + " " + s_cEOL +;
         "RFLAGS = " + s_cEOL +;
         "LFLAGS = /LIBPATH:$(CC_DIR)\lib /LIBPATH1:$(HB_DIR)\lib /LIBPATH2:$(C4W)\lib"  +IIF(s_lMt, " /Nodefaultlib:LIBC "," /Nodefaultlib:LIBCMT " ) + s_cEOL +;
         "IFLAGS = " + s_cEOL +;
         "LINKER = link" + s_cEOL +;
         " " + s_cEOL +;
         "ALLOBJ = " + IIF( lC4W, "$(C4W)\initc.obj", "" ) + "$(OBJFILES) $(OBJCFILES)" + s_cEOL +;
         "ALLRES = $(RESDEPEN)" + s_cEOL +;
         "ALLLIB = $(USERLIBS) $(CONTRIBLIBS) $(HBLIBS) kernel32.lib user32.lib gdi32.lib winspool.lib comctl32.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib mpr.lib vfw32.lib winmm.lib " + s_cEOL )

   ELSEIF s_lPocc

      FWrite( s_nMakeFileHandle, ;
         "CFLAG1 = $(SHELL)  /Ze /Go /Ot /Tx86-coff /I$(INCLUDE_DIR) $(C_USR) $(CFLAGS)" +IIF( s_lMt, ' /D"HB_THREAD_SUPPORT" /MT' , "" ) + s_cEOL +;
         "CFLAG2 = " + s_cEOL +;
         "RFLAGS = " + s_cEOL +;
         "LFLAGS = /LIBPATH:$(CC_DIR)\lib /LIBPATH:$(CC_DIR)\lib\win /LIBPATH:$(HB_DIR)\lib /MACHINE:IX86"+IIF(!s_lGui," /SUBSYSTEM:CONSOLE"," /SUBSYSTEM:WINDOWS") + s_cEOL +;
         "IFLAGS = " + s_cEOL +;
         "LINKER = polink" + s_cEOL +;
         " " + s_cEOL +;
         "ALLOBJ = " + IIF( lC4W, "$(C4W)\initc.obj", "" ) + "$(OBJFILES) $(OBJCFILES)" + s_cEOL +;
         "ALLRES = $(RESDEPEN)" + s_cEOL +;
         "ALLLIB = $(USERLIBS) $(CONTRIBLIBS) $(HBLIBS) "+IIF(s_lMT,"crtmt.lib","crt.lib") + " kernel32.lib user32.lib gdi32.lib winspool.lib comctl32.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib mpr.lib winmm.lib wsock32.lib schannel.lib" + s_cEOL )

   ELSEIF s_lGcc

      FWrite( s_nMakeFileHandle, ;
         "CFLAG1 = $(SHELL) " +IIF( !EMPTY(s_cUserInclude ) ," -I" + Alltrim( s_cUserInclude )  ,"") + IIF( "Unix" $ cOs , " -I/usr/include/harbour ", "" ) + IIF(  "Linux" $ cOS, "-I/usr/include/harbour", " -I$(HB_DIR)/include" ) + " -c -Wall" + IIF( s_lMt, " -DHB_THREAD_SUPPORT " , "" )  + iif(s_lmingw, " -mno-cygwin "," " )+ s_cEOL +;
         "CFLAG2 = " + IIF(  "Linux" $ cOS, "-L$(HB_LIB_INSTALL)", " -L$(HB_DIR)/lib  -L$(CC_DIR)/lib" ) +  IIF( "Unix" $ cOs , " -L/usr/lib/harbour ", "" ) + IIF( lHwgui, " -L$(HWGUI)\lib","" ) + s_cEOL +;
         "RFLAGS = " + s_cEOL +;
         "LFLAGS = " + iif(!s_lLinux," ","-Wl,--noinhibit-exec ") + IIF(lUseXhb ,IIF(lUseharbourDll,"","-static ") + iif(lXwt .or. lhwgui ,"-gtcgi " , "-gtcrs "), "$(CFLAG2)") + iif(lXwt,"`pkg-config --libs gtk+-2.0` -lxwt -lxwt_gtk -lxwt","") + iif( lxHGtk, "`pkg-config --libs gtk+-2.0 libglade-2.0` -lxhgtk ","") + iif( lhwgui .and. !s_lMinGW, " `pkg-config --libs gtk+-2.0 libglade-2.0 libgnomeprint-2.2` -hwgui ","")  + iif(lhwgui .and. s_lMinGW," -mwindows " ,"" )+  iif(s_lLinux .and. s_lmt ," -mt "," "  ) +s_cEOL +;
         "IFLAGS = " + s_cEOL +;
         "LINKER = "+ IIF(lusexhb,"hblnk","gcc") + s_cEOL +;
         " " + s_cEOL +;
         "ALLOBJ = $(OBJFILES)  $(OBJCFILES)" + s_cEOL +;
         "ALLRES = $(RESDEPEN) " + s_cEOL +;
         "ALLLIB = $(USERLIBS) $(CONTRIBLIBS) $(HBLIBS) " +iif(s_lMinGW," -luser32 -lwinspool -lgdi32 -lcomctl32 -lcomdlg32 -lole32 -loleaut32 -luuid -lmpr -lwsock32 -lws2_32 -lmapi32","") + s_cEOL +;
         ".autodepend" + s_cEOL )

   ENDIF

#IFdef HBM_USE_DEPENDS

   FWrite( s_nMakeFileHandle, ;
      " " + s_cEOL +;
      "#DEPENDS" + s_cEOL )

   IF lScanIncludes
      // Clipper/(x)Harbour sources: .prg
      IF Len( s_aPrgs ) == Len( s_aObjs )
         Attention( "Scanning .prg sources...", 19 )
         FOR nPos := 1 to Len(s_aPrgs)
            cIncl := ScanInclude( ReplaceMacros( s_aPrgs[ nPos ] ), lScanIncRecursive, cExcludeExts )
            // Only add in list if dependencies exist
            IF ! Empty(cIncl)
               FWrite( s_nMakeFileHandle, s_aObjs[ nPos ] + ": " + Alltrim( cIncl ) + s_cEOL, "" )
            ENDIF
         NEXT
      ENDIF

      // C-sources: .c
      IF Len( s_aCFiles ) == Len( s_aObjsC )
         Attention( "Scanning .c sources...", 19 )
         FOR nPos := 1 to Len(s_aCFiles)
            cIncl := ScanInclude( s_aCFiles[ nPos ], lScanIncRecursive, cExcludeExts )
            // Only add in list if dependencies exist
            IF ! Empty(cIncl)
               FWrite( s_nMakeFileHandle, s_aObjsC[ nPos ] + ": " + Alltrim( cIncl ) + s_cEOL, "" )
            ENDIF
         NEXT
      ENDIF

      // Cleanup message
      @ 19, 1 say Space(MaxCol() - 2)
   ENDIF

#ENDIF

   FWrite( s_nMakeFileHandle, ;
      " " + s_cEOL +;
      "#COMMANDS" + s_cEOL )

   AEval( s_aCommands, { | xItem | FWrite( s_nMakeFileHandle, xitem[ 1 ] + s_cEOL ), FWrite( s_nMakeFileHandle, xitem[ 2 ] + s_cEOL ), FWrite( s_nMakeFileHandle, " " + s_cEOL ) } )

   IF s_lBcc .OR. s_lMSVcc .OR. s_lPocc

      FWrite( s_nMakeFileHandle, ;
         "#BUILD" + s_cEOL +;
         " " + s_cEOL +;
         "$(PROJECT): $(CFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)" + s_cEOL +;
         "    $(CC_DIR)\bin\$(LINKER) @&&!  " + s_cEOL +;
         "    $(LFLAGS) +" + s_cEOL +;
         "    $(ALLOBJ), +" + s_cEOL +;
         "    $(PROJECT),, +" + s_cEOL +;
         "    $(ALLLIB), +" + s_cEOL +;
         "    $(DEFFILE), +" + s_cEOL +;
         "    $(ALLRES) " + s_cEOL +;
         "!" + s_cEOL )

   ELSEIF s_lGcc

      FWrite( s_nMakeFileHandle, ;
         "#BUILD" + s_cEOL +;
         " " + s_cEOL +;
         "$(PROJECT): $(CFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)" + s_cEOL )

      IF "Linux" $ cOS
         FWrite( s_nMakeFileHandle, "    $(LINKER) @&&!" + s_cEOL )
      ELSEIF "Unix" $ cOS .or. "HP-UX" $ cOS
         FWrite( s_nMakeFileHandle, "    $(LINKER) @&&!" + s_cEOL )
      ELSE
         FWrite( s_nMakeFileHandle, "    $(CC_DIR)\bin\$(LINKER) @&&!" + s_cEOL )
      ENDIF

      FWrite( s_nMakeFileHandle, ;
         "    $(PROJECT) " + s_cEOL +;
         "    $(ALLOBJ)  " + s_cEOL +;
         "    $(LFLAGS)  " + s_cEOL +;
         "    $(ALLLIB)  " + s_cEOL +;
         "!" + s_cEOL )

   ENDIF

   FClose( s_nMakeFileHandle )
   s_nMakeFileHandle:= F_ERROR  // Invalid handle now file is closed

   IF !lCancelMake

      IF s_nLang == LANG_PT .OR. ;     /* portuguese brazilian */
         s_nLang == LANG_ES            /* spanish              */

         s_cAlertMsg := "Compilar app ? (S/N) "

      ELSE                             /* english             */
         s_cAlertMsg := "Build app ? (Y/N) "
      ENDIF

      if ! lCreateAndCompile

      @ 20,5 SAY s_cAlertMsg;
             GET cBuild;
             PICT "!";
             VALID cBuild $ iif(s_nLang == LANG_EN,"YN","SN")
      READ

      else
         cBuild := "Y"
      endif

      IF ( cBuild == "S" .or. cBuild == "Y" )

         IF lNew  .OR. Len(aSelFiles)=1
            cBuildForced := "Y"
         ELSE

            IF s_nLang     == LANG_PT  /* portuguese brazilian */
               s_cAlertMsg := "Foráar recompilaá∆o para todos .prgs <-f> ? (S/N) "
            ELSEIF s_nLang == LANG_ES  /* spanish              */
               s_cAlertMsg := "Forzar recompilaci¢n para todos los .prgs <-f> ? (S/N) "
            ELSE                       /* english             */
               s_cAlertMsg := "Force recompiling for all .prgs <-f> ? (Y/N) "
            ENDIF

            @ 21,5 SAY s_cAlertMsg;
                   GET cBuildForced;
                   PICT "!";
                   VALID cBuildForced $ iif(s_nLang == LANG_EN,"YN","SN")
            READ

         ENDIF

      ENDIF

   ENDIF

   /*  Set Parameter for how to continue
    *  Cancell Make    : -1
    *  Compile Updated :  0
    *  Compile All     :  1
    */

   DO CASE
   CASE cBuildForced $ "SY"
      nBuildReturn :=  BLD_ALL

   CASE cBuild       $ "SY"
      nBuildReturn :=  BLD_SOME

   OTHERWISE
      nBuildReturn := BLD_NONE

   END CASE

   RESTSCREEN( ,,,, cScreen )
   RETURN nBuildReturn

#IfDef HBM_USE_DEPENDS

*------------------------------------------------------------
FUNCTION ScanInclude( cFile, lRecursive, cExclExtent, aFiles)
*------------------------------------------------------------
// Search for #include & Set Procedure To & Set Proc To

   LOCAL cFileList := ""
   LOCAL nHandle   := -1
   LOCAL lEof      := .F.
   LOCAL cTemp     := ""
   LOCAL cBuffer   := ""
   LOCAL aQuotes   := {{'"','"'},{"'","'"},{"[","]"},{"<",">"}}
   LOCAL cQuote    := ""
   LOCAL cQuoteA   := ""
   LOCAL cInclude  := ""
   LOCAL lPrg      := .F.
   LOCAL lC        := .F.
   LOCAL lCh       := .F.
   LOCAL cPath     := ""
   LOCAL cFnam     := ""
   LOCAL cExt      := ""
   LOCAL cDrive    := ""
   LOCAL cContinue := ""

   DEFAULT lRecursive  TO .F.
   DEFAULT cExclExtent TO ""    // specify extensions to exclude like ".ch.def" etc., including the dot
   DEFAULT aFiles      TO {}

   IF File(cFile)

       HB_FNAMESPLIT( cFile, @cPath, @cFnam, @cExt, @cDrive )

       lPrg := (Lower(cExt) == ".prg")
       lC := (Lower(cExt) == ".c")
       lCh := (Lower(cExt) == ".ch")
       cContinue := IIF(lPrg,";",IIF(lC,"\",""))

       nHandle := FOpen(cFile)

       IF nHandle != F_ERROR

           // Provisions for recursive scanning
           // Add current file to list, making it by default the first in the list

           IF s_lWin32
               IF AScan(aFiles, {| x | Lower( x ) == Lower( cFnam + cExt ) } ) == 0       // Case IN-sensitive!
                   AAdd(aFiles, cFnam + cExt )
               ENDIF
           ELSE
               IF AScan(aFiles, cFnam + cExt ) == 0       // Case Sensitive!
                   AAdd(aFiles, cFnam + cExt )
               ENDIF
           ENDIF

           lEof := ( HB_FReadLine( nHandle, @cTemp, s_aEOL ) == HB_FEOF )
           cTemp := LTrim( cTemp )
           // Loop reading file
           WHILE !lEof
               IF lPrg .OR. lC      // Check for line-continuation
                   WHILE Right(cTemp, 1 ) == cContinue

                       cTemp := Left( cTemp , Len( cTemp ) - 1)
                       IF !lEof
                          lEof := (HB_FReadLine(nHandle,@cBuffer,s_aEOL) == HB_FEOF)
                          cTemp += LTrim( cBuffer)
                       ENDIF

                   ENDDO
               ENDIF
               // Dependencies
               IF Lower(Left( cTemp, 8)) == "#include"
                   cTemp := AllTrim(SubStr( cTemp, 9))
               Else
                   IF lPrg .and. Upper(Left( cTemp, 16)) == "SET PROCEDURE TO"
                       cTemp := AllTrim(SubStr( cTemp, 17))
                   ELSE
                       IF lPrg .and. Upper(Left( cTemp, 11)) == "SET PROC TO"  // Alternative
                           cTemp := AllTrim(SubStr( cTemp, 12))
                       ELSE
                           cTemp := ""
                       ENDIF
                   ENDIF
               Endif
               // Something Ok?
               IF Len(cTemp) > 0
                  cQuote := Left( cTemp, 1)
                  cQuoteA := ""
                  AEval(aQuotes,{| x |Iif(x[1] == cQuote,cQuoteA := x[2],)})     // Determine closing quote
                  IF cQuoteA == ""
                      cInclude := AllTrim(Left(cTemp, At(" ", cTemp + " ") - 1)) // Handle set procedure to, not using quotes
                  ELSE
                      cTemp := SubStr(cTemp, 2)
                      cInclude := AllTrim(Left(cTemp, At(cQuoteA, cTemp) - 1))   // Find closing quote
                  ENDIF
                  IF Len(cInclude) > 0 .and. Len(Alltrim(cExclExtent)) > 0
                      HB_FNAMESPLIT( cInclude, @cPath, @cFnam, @cExt, @cDrive )
                      IF lPrg .AND. Len(cExt) == 0
                          cInclude := cInclude + ".prg"        // Handle set procedure to, using default extension
                      ENDIF
                      // TOFIX: Needs to be case sensitive for non-WinDOS? [vszakats]
                      IF AT(Lower(cExt), Lower(cExclExtent)) > 0
                          cInclude := ""
                      ENDIF
                  ENDIF
                  IF Len(cInclude) > 0
                      // Still Ok, add to list?
                      IF s_lWin32
                          IF AScan(aFiles, {| x | Lower( x ) == Lower( cInclude ) } ) == 0       // Case IN-sensitive!
                              AAdd(aFiles, (cInclude) )
                              // recursive scanning
                              IF lRecursive
                                  ScanInclude(FileInIncludePath(cInclude), lRecursive, cExclExtent, aFiles )
                              ENDIF
                          ENDIF
                      ELSE
                          IF AScan(aFiles, cInclude ) == 0       // Case Sensitive!
                              AAdd(aFiles, (cInclude) )
                              // recursive scanning
                              IF lRecursive
                                  ScanInclude(FileInIncludePath(cInclude), lRecursive, cExclExtent, aFiles )
                              ENDIF
                          ENDIF
                      ENDIF
                  ENDIF
               ENDIF
               IF !lEof
                   lEof := (HB_FReadLine(nHandle,@cTemp,s_aEOL) == HB_FEOF)
                   cTemp := LTrim( cTemp)
               ENDIF
           ENDDO

         FClose( nHandle )
       ENDIF

   ENDIF
   // Return results, a space-separated list of filenames, unsorted
   IF Len(aFiles) > 1   // Skip generation of list if only main source (1) was added, caller knows what to do
       AEval(aFiles,{| x | cFileList := cFileList + " " + x } )
   ENDIF

RETURN cFileList
#Endif

*-----------------------------
FUNCTION CompileUpdatedFiles()
*-----------------------------

   LOCAL cScreen
   LOCAL cComm
   LOCAL cOld
   LOCAL nPos

   LOCAL aCtocompile := {}
   LOCAL aOrder      := ListAsArray2( s_aBuildOrder[ 2 ], " " )
   LOCAL lEnd
   LOCAL cErrText    := ""
   LOCAL xItem
   LOCAL cOrder      := ""
   LOCAL cPrg        := ""
   LOCAL nFiles
   LOCAL nFile       := 1
   LOCAL lNewer      := .F.
#IfDef HBM_USE_DEPENDS
   LOCAL nPos1       := 0
   LOCAL cDepSrc     := ""
#Endif
   LOCAL aGauge      := GaugeNew( 5, 5, 7, 40, "W/B", "W+/B", "≤" )

   @ 4,5 SAY "Compiling :"

   FOR EACH cOrder in aOrder
      IF cOrder == "$(CFILES)"
         nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".c.obj:" .OR. x[ 1 ] == ".cpp.obj:" .or. x[ 1 ] == ".cpp.o:" .or. x[ 1 ] == ".c.o:"  } )

         IF nPos > 0
            cComm := s_aCommands[ nPos, 2 ]
            cOld  := cComm
         ELSE
            nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".C.OBJ:" } )

            IF nPos > 0
               cComm := s_aCommands[ nPos, 2 ]
               cOld  := cComm
            ENDIF

         ENDIF

//         GaugeDisplay( aGauge )

         FOR nFiles := 1 TO Len( s_aCFiles )
            @  4, 16 SAY Space( 50 )
            xItem := Substr( s_aCFiles[ nFiles ], Rat( IIF( s_lGcc, "/", "\" ), s_aCFiles[ nFiles ] ) + 1 )
            nPos  := AScan( s_aObjsC, { | x | x := Substr( x, Rat( IIF( s_lGcc, "/", "\" ), x ) + 1 ), Left( x, At( ".", x ) ) == Left( xitem, At( ".", xitem ) ) } )

#IfDef HBM_USE_DEPENDS
            lNewer := .F.
            // TOFIX: Needs to be case sensitive for non-WinDOS? [vszakats]
            nPos1 := AScan( s_aDepends, { | x |lower(x[1]) == lower( s_aObjs[ npos ] )})
            IF nPos1 > 0
               FOR EACH cDepSrc in s_aDepends[ nPos1 , 2 ]
                   lNewer := lNewer .OR. Fileisnewer( cDepSrc, s_aObjs[ npos ], .T. )
               NEXT
            ENDIF
#Endif
            IF lNewer .or. Fileisnewer( s_aCFiles[ nFiles ], s_aObjsC[ nPos ] )

               IF nPos > 0
                  IF s_lMSVcc //.OR. s_lPocc
                     cComm := Strtran( cComm, "-Fo$*", "-Fo" + s_aObjsC[ nPos ] )
                  ELSE
                     cComm := Strtran( cComm, "o$*", "o" + s_aObjsC[ nPos ] )
                  ENDIF
                  cComm := Strtran( cComm, "$**", s_aCFiles[ nFiles ] )
                  cComm += IIF( s_lLinux ,  " > " + s_cLogFile, " >>" + s_cLogFile )

                  @4,16 SAY s_aCFiles[ nFiles ]
                  GaugeUpdate( aGauge, nFile / Len( s_aCFiles ) )  // changed s_aPrgs to s_aCFiles Ath 2004-06-08
                  nFile ++
                  OUTERR( cComm + s_cEOL )
                  setpos(9,0)

                  cScreen := SAVESCREEN()
                  s_lErrors := IIF( hb_run( cComm ) != 0, .T., .F. )
                  RESTSCREEN( ,,,, cScreen )
                  cErrText := Memoread( s_cLogFile )
                  lEnd     := "Error E" $ cErrText

                  IF ! s_lIgnoreErrors .AND. lEnd

                     s_lErrors := IIF( hb_run( s_cEditor + " " + s_cLogFile ), .T., .F. )

                     IF s_lErrors
                        RETURN PGM_ERR
                     ENDIF

                     cComm := cOld

                  ENDIF
               ENDIF

            ENDIF

         NEXT
         //nFile++
      ENDIF

      GaugeDisplay( aGauge )

      IF cOrder == "$(OBJFILES)"

         IF s_lGcc
            nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".prg.o:" } )
         ELSE
            nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".prg.obj:" } )
         ENDIF

         IF nPos > 0
            cComm := s_aCommands[ nPos, 2 ]
            cOld  := cComm
         ELSE

            IF s_lGcc
               nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".PRG.O:" } )
            ELSE
               nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".PRG.OBJ:" } )
            ENDIF

         ENDIF

         nFile := 1

         FOR EACH cPrg IN s_aPrgs

            @  4, 16 SAY PADR( cPrg, 50, " " )
            xItem := Substr( cPrg, Rat( IIF( s_lGcc, "/", "\" ), cPrg ) + 1 )
            nPos  := AScan( s_aObjs, { | x | x := Substr( x, Rat( IIF( s_lGcc, "/", "\" ), x ) + 1 ), Left( x, At( ".", x ) ) == Left( xItem, At( ".", xitem ) ) } )

#IfDef HBM_USE_DEPENDS
            lNewer := .F.
            // TOFIX: Needs to be case sensitive for non-WinDOS? [vszakats]
            nPos1 := AScan( s_aDepends, { | x |lower(x[1]) == lower( s_aObjs[ npos ] )})
            IF nPos1 > 0
               FOR EACH cDepSrc in s_aDepends[ nPos1 , 2 ]
                   lNewer := lNewer .OR. Fileisnewer( cDepSrc, s_aObjs[ npos ], .T. )
               NEXT
            ENDIF
#Endif

            IF !empty( cPrg ) .AND. (lNewer .OR. Fileisnewer( cPrg, s_aObjs[ npos ] ))

               IF nPos > 0
                  IF s_lMSVcc //.OR. s_lPocc
                     cComm := Strtran( cComm, "-Fo$*", "-Fo" + s_aObjs[ nPos ] )
                  ELSE
                     cComm := Strtran( cComm, "o$*", "o" + s_aObjs[ nPos ] )
                  ENDIF
                  cComm := Strtran( cComm, "$**", cPrg )
                  cComm += IIF( s_lLinux ,  " > " + s_cLogFile, " >>" + s_cLogFile )

                  @4,16 SAY cPrg
                  GaugeUpdate( aGauge, nFile / Len( s_aPrgs ) )
                  nFile ++     // moved from outside "FOR EACH", Ath 2004-06-08

                  setpos(9,0)

                  cScreen := SAVESCREEN()
                  s_lErrors := IIF( hb_run( cComm ) != 0, .T., .F. )
                  RESTSCREEN( ,,,, cScreen )
                  cErrText := Memoread( s_cLogFile )
                  lEnd     := "C2006" $ cErrText .OR. "No code generated" $ cErrText .or. "Error E" $ cErrText .or. "Error F" $ cErrText

                  IF ! s_lIgnoreErrors .AND. lEnd

                     s_lErrors := IIF( hb_run( s_cEditor + " " + s_cLogFile ) != 0, .T., .F. )
                     IF s_lErrors
                        RETURN PGM_ERR
                     ENDIF
                  ENDIF

                  cComm := cOld

               ENDIF

            ENDIF

         NEXT

         // nFile ++    // removed, useless, Ath 2004-06-08

      ENDIF

      IF cOrder == "$(RESDEPEN)"
         nPos := AScan( s_aCommands, { | x | x[ 1 ] == ".rc.res:" } )

         IF nPos > 0
            cComm := s_aCommands[ nPos, 2 ]
            cOld  := cComm
         ENDIF

         FOR nFiles := 1 TO Len( s_aResources )

            IF ! Empty( s_aResources[ nFiles ] )
               cComm := Strtran( cComm, "$<", s_aResources[ nFiles ] )
               OutErr( " " )
               setpos(9,0)

               cScreen := SAVESCREEN()
               s_lErrors := IIF( hb_run( cComm ) != 0, .T., .F. )
               RESTSCREEN( ,,,, cScreen )
            ENDIF

            cComm := cOld

         NEXT

      ENDIF

   NEXT

RETURN NIL

*------------------------------------------
FUNCTION FileIsNewer( cFile, as, lInclude )
*------------------------------------------

   LOCAL nCount    := 0
   LOCAL cSrcPath  := ""

   DEFAULT lInclude TO .F.

   // Check all paths in INCLUDE environment variable, if requested
   IF lInclude
       cFile := FileInIncludePath(cFile)
   ENDIF

   s_aDir := { cFile,, Hbmake_Filedate( cFile ), hbmake_filetime( cFile ), ;
             as, Hbmake_Filedate( as ), hbmake_filetime( as ) }

   IF Empty( s_aDir[ 7 ] )
      s_aDir[ 2 ] := .T.
   ELSE
      s_aDir[ 2 ] := td2jul( s_aDir[ 4 ], s_aDir[ 3 ] ) > td2jul( s_aDir[ 7 ], s_aDir[ 6 ] )
   ENDIF

RETURN s_aDir[ 2 ]

*--------------------------------
FUNCTION FileInIncludePath(cFile)
*--------------------------------

 LOCAL cFilePath := ""
 LOCAL cSrcPath  := ""

    IF Len(s_aSrcPaths) == 0
        s_aSrcPaths := ListAsArray2( GetEnv( "INCLUDE" ) , HB_OSPATHLISTSEPARATOR() )
    ENDIF
    IF ! File(cFile)
        FOR EACH cSrcPath IN s_aSrcPaths
            IF Len(cSrcPath) > 0 .and. Right(cSrcPath,1) <> HB_OSPATHSEPARATOR()
                cSrcPath := cSrcPath + HB_OSPATHSEPARATOR()
            ENDIF
            IF File(cSrcPath + cFile)
                cFile := cSrcPath + cFile
                EXIT
            ENDIF
        NEXT
    ENDIF

RETURN cFile

*----------------------------------
FUNCTION CreateLibMakeFile( cFile )
*----------------------------------

   LOCAL nBuildReturn
   LOCAL aInFiles  := {}
   LOCAL aOutFiles := {}
   LOCAL aSrc      := Directory( "*.prg" )
   LOCAL nLenaSrc  := Len( aSrc )

   LOCAL aOutC     := {}
   LOCAL aSrcC     := Directory( "*.c" )
   LOCAL cOS       := IIF( s_lLinux, "Linux", "Win32")
   LOCAL cCompiler := IIF( s_lLinux, "GCC",IIF(s_lMSVcc,"MSVC",IIF(s_lPocc,"POCC","BCC")))
   LOCAL cLibName  := Padr( Left( cFile, At( ".", cFile ) - 1 ) ,40)

   LOCAL lAutomemvar     := .F.
   LOCAL lVarIsMemvar    := .F.
   LOCAL lDebug          := .F.
   LOCAL lSupressline    := .F.
   LOCAL cHarbourFlags   := ""
   LOCAL cObjDir         := s_cObjDir + space( 20 )
   LOCAL lCompMod        := .F.
   LOCAL lInstallLib     := .F.
   LOCAL x
   LOCAL nPos
//   LOCAL lGenppo         := .F.
   LOCAL GetList         := {}
   LOCAL cItem           := ""
   LOCAL cExt            := ""
   LOCAL cDrive          := ""
   LOCAL cPath           := ""
   LOCAL cTest           := ""
   LOCAL cLast           := ""
   LOCAL nWriteFiles     := 0
   LOCAL aUserDefs
   LOCAL cCurrentDef     := ""
   LOCAL cBuild          := " "
   LOCAL cBuildForced    := " "
   LOCAL lCancelMake     := .F.
   LOCAL nOption
   LOCAL lNew            := .F.
   LOCAL oMake
   LOCAL cAllRes         := ""
   LOCAL cTemp

   IF nLenaSrc == 0 .AND. !s_lRecursive
      IF s_nLang     == LANG_PT        /* portuguese brazilian */
         s_cAlertMsg := "N∆o h† prg na pasta "+curdir()
      ELSEIF s_nLang == LANG_ES        /* spanish              */
         s_cAlertMsg := "No hay ning£n prg en la carpeta "+curdir()
      ELSE                             /* english              */
         s_cAlertMsg := "Have not any prg in "+curdir()+" folder."
      ENDIF
      Alert( s_cAlertMsg )
      RETURN PGM_ERR
   ENDIF

   s_cUserInclude  := space(200)
   s_cUserDefine   := space(200)

   IF File( cFile )

      CLS

      IF s_nLang     == LANG_PT        /* portuguese brazilian */
         nOption := Alert( "O makefile <" + cFile +"> j† existe.",{ "Editar", "Criar Novo" , "Cancelar" } )
      ELSEIF s_nLang == LANG_ES        /* spanish              */
         nOption := Alert( "Lo makefile <" + cFile +"> ya existe.",{ "Editar", "Crear Nuevo" , "Cancelar" } )
      ELSE                             /* english             */
         nOption := Alert( "The makefile <" + cFile +"> already exist ",{ "Edit" , "Create New" , "Cancel" } )
      ENDIF

      IF nOption == 1 // edit makefile

         // Verify if "cFile" can be openned to write mode.

         s_nMakeFileHandle := FOpen( cFile, FO_WRITE )

         if s_nMakeFileHandle == F_ERROR

            CLS

            IF s_nLang     == LANG_PT  /* brazilian portuguese */
               s_cAlertMsg := "<"+cFile + "> n∆o pode ser aberto para ediá∆o."
            ELSEIF s_nLang == LANG_ES  /* spanish              */
               s_cAlertMsg := "<"+cFile + "> no pode ser abierto para edici¢n."
            ELSE                       /* english             */
               s_cAlertMsg := "<"+cFile + "> cannot be openned for edition."
            ENDIF

            Alert( s_cAlertMsg+" FERROR ("+LTrim(Str(FError()))+")" )

            RETURN PGM_ERR
         else
            FClose( s_nMakeFileHandle )
            s_nMakeFileHandle:= F_ERROR  // Invalid handle now file is closed
         endif

         oMake :=THbMake():new()
         oMake:cMakefile := cFile
         oMake:cMacro := iif(s_lMSVcc,"#MSVC",iif(s_lPocc,"#POCC",iif(s_lGcc,"#GCC","#BCC")))
         oMake:ReadMakefile(cFile)

         FRename(cFile,cFile+".old")

         IF LEN(oMake:aRes) >0
            FOR EACH cTemp IN oMake:aRes
                cAllRes += cTemp+ " "
            NEXT
         ENDIF

         lAutoMemVar     := oMake:lAutomemvar
         lVarIsMemVar    := oMake:lVarIsMemvar
         lDebug          := oMake:ldebug
         lSupressline    := oMake:lSupressline
         lCompMod        := oMake:lCompMod
         s_lGenppo       := oMake:lGenppo
         lInstallLib     := oMake:lInstallLib
         s_cUserInclude  := PadR(oMake:cUserInclude,200 )
         s_cUserDefine   := PadR(oMake:cUserDef,200 )
         s_cUserLibs     := PadR(oMake:cUserLib,200 )
         s_cEditor       := trim(oMake:cEditor)

         if Empty( s_cEditor )
            if s_lOS2 .OR. s_lLinux
               s_cEditor := "mcedit"
            else
               s_cEditor := "edit"
            endif
         endif

         if !empty(oMake:cFmc)
             cLibName    := PadR(oMake:cFmc,200)
         endif

         if !s_lRecursive
            s_lRecursive := oMake:lRecurse
         endif

         IF nLenaSrc == 0 .and. !s_lRecursive

            CLS

            IF s_nlang     == LANG_PT  /* portuguese brazilian */
               s_cAlertMsg := "N∆o h† nenhum prg na pasta "+CurDir()+". Use o modo recursivo -r"
            ELSEIF s_nLang == LANG_ES  /* spanish              */
               s_cAlertMsg := "No hay ning£n prg en la carpeta "+CurDir()+". Use lo modo recursivo -r"
            ELSE                       /* english              */
               s_cAlertMsg := "Does not have any prg in "+CurDir()+" folder. Use the recursive mode -r"
            ENDIF

            Alert( s_cAlertMsg )

            RETURN PGM_ERR
         ENDIF

         if s_lCancelRecursive
            s_lRecursive := .F.
         endif

         // after oMake read, recreate other clean makefile to edit.
         s_nMakeFileHandle := FCreate(cFile)

         if s_nMakeFileHandle == F_ERROR

            CLS

            IF s_nLang     == LANG_PT  /* portuguese brazilian */
               s_cAlertMsg := "<"+cFile + "> n∆o pode ser aberto para ediá∆o."
            ELSEIF s_nLang == LANG_ES  /* spanish              */
               s_cAlertMsg := "<"+cFile + "> no pode ser abierto para edici¢n."
            ELSE                       /* english             */
               s_cAlertMsg := "<"+cFile + "> cannot be openned for edition."
            ENDIF

            Alert( s_cAlertMsg+" FERROR ("+Ltrim(Str(FError()))+")" )

            RETURN PGM_ERR

         endif

         WriteMakeFileHeader()

         s_lEditMake := .T.

      ELSEIF nOption == 2 // create a new makefile

         IF nLenaSrc == 0 .and. !s_lRecursive

            CLS

            IF s_nlang     == LANG_PT  /* portuguese brazilian */
               s_cAlertMsg := "N∆o h† nenhum prg na pasta "+CurDir()+". Use o modo recursivo -r"
            ELSEIF s_nLang == LANG_ES  /* spanish              */
               s_cAlertMsg := "No hay ning£n prg en la carpeta "+CurDir()+". Use lo modo recursivo -r"
            ELSE                       /* english              */
               s_cAlertMsg := "Does not have any prg in "+CurDir()+" folder. Use the recursive mode -r"
            ENDIF

            Alert( s_cAlertMsg )
            RETURN PGM_ERR

         ENDIF

         s_lEditMake := .F.

         s_nMakeFileHandle := FCreate( cFile )

         if s_nMakeFileHandle == F_ERROR

            CLS

            IF s_nLang     == LANG_PT  /* portuguese brazilian */
               s_cAlertMsg := "<"+cFile + "> n∆o pode ser criado."
            ELSEIF s_nLang == LANG_ES  /* spanish              */
               s_cAlertMsg := "<"+cFile + "> no pode ser criado."
            ELSE                       /* english             */
               s_cAlertMsg := "<"+cFile + "> cannot be created."
            ENDIF

            Alert( s_cAlertMsg+" FERROR ("+Ltrim(Str(FError()))+")" )

            RETURN PGM_ERR

         endif

         WriteMakeFileHeader()
         lNew := .T.

      ELSE
         RETURN PGM_ERR
      ENDIF

   ELSE

      s_nMakeFileHandle := FCreate( cFile )

      if s_nMakeFileHandle == F_ERROR

         CLS

         IF s_nLang     == LANG_PT     /* portuguese brazilian */
            s_cAlertMsg := "<"+cFile + "> n∆o pode ser criado."
         ELSEIF s_nLang == LANG_ES     /* spanish              */
            s_cAlertMsg := "<"+cFile + "> no pode ser criado."
         ELSE                          /* english             */
            s_cAlertMsg := "<"+cFile + "> cannot be created."
         ENDIF

         Alert( s_cAlertMsg+" FERROR ("+Ltrim(Str(FError()))+")" )

         RETURN PGM_ERR

      ENDIF

      WriteMakeFileHeader()
      nOption := 2  // create a new makefile
      lNew := .T.

   ENDIF

   CLS
   Setcolor( "w/b+,b+/w,w+/b,w/b+,w/b,w+/b" )
   @  0,  0, Maxrow(), Maxcol() BOX( Chr( 201 ) + Chr( 205 ) + Chr( 187 ) + Chr( 186 ) + Chr( 188 ) + Chr( 205 ) + Chr( 200 ) + Chr( 186 ) + Space( 1 ) )

   Attention( hbMakeID() + space(10) + s_aLangMessages[ 27 ], 0 )

   SET CURSOR OFF

   @ 01,01 SAY s_aLangMessages[ 28 ]

   @ 01,17,06,24 GET cOS;
                 LISTBOX { "Win32", "OS/2", "Linux" };
                 MESSAGE s_aLangMessages[ 49 ];
                 STATE OsSpec(getlist,1,@cOS);
                 DROPDOWN

   @ 01,30 SAY s_aLangMessages[ 29 ]

   @ 01,54,06,59 GET cCompiler;
                 LISTBOX { "BCC", "MSVC", "POCC", "GCC","MINGW" };
                 MESSAGE s_aLangMessages[ 50 ];
                 STATE OsSpec(getlist,2,@cCompiler);
                 DROPDOWN

   @ 02,47       SAY s_aLangMessages[ 66 ]

#ifdef __PLATFORM__WINDOWS
   @ 02,68,06,77 GET s_cEditor;
                 LISTBOX { "edit", "notepad" };
                 MESSAGE s_aLangMessages[ 67 ];
                 STATE   OsSpec(getlist,3,@s_cEditor);
                 DROPDOWN
#endif

   @ 03,01 SAY s_aLangMessages[ 59 ];
           GET cLibName;
           PICT "@S15";
           MESSAGE s_aLangMessages[ 58 ]

   @ 03,35 SAY s_aLangMessages[ 60 ];
           GET cObjDir;
           PICT "@S15"

   Attention( "Harbour Options", 5 )

   @ 06,01 GET lAutoMemvar;
           CHECKBOX;
           CAPTION s_aLangMessages[ 32 ];
           STYLE "[X ]"

   @ 06,40 GET lVarIsMemvar;
           CHECKBOX;
           CAPTION s_aLangMessages[ 33 ];
           STYLE "[X ]"

   @ 07,01 GET lDebug;
           CHECKBOX;
           CAPTION s_aLangMessages[ 34 ];
           STYLE "[X ]"

   @ 07,40 GET lSupressLine;
           CHECKBOX;
           CAPTION s_aLangMessages[ 35 ];
           STYLE "[X ]"

   @ 08,01 GET s_lGenppo;
           CHECKBOX;
           CAPTION s_aLangMessages[ 36 ];
           STYLE "[X ]"

   @ 08,40 GET lCompMod;
           CHECKBOX;
           CAPTION s_aLangMessages[ 37 ];
           STYLE "[X ]"

   @ 09,01 SAY s_aLangMessages[ 38 ];
           GET s_cUserDefine;
           PICT "@S23"

   @ 09,40 SAY s_aLangMessages[ 39 ];
           GET s_cUserInclude;
           PICT "@S18"

   @ 10,01 GET lInstallLib;
           CHECKBOX;
           CAPTION s_aLangMessages[ 61 ];
           STYLE "[X ]"

   READ MSG AT MaxRow() - 1, 1, MaxCol() - 1

// SET CURSOR ON

   IF ! Empty( s_cUserDefine )
      aUserDefs := ListasArray2(Alltrim( s_cUserDefine ), ";")

      FOR EACH cCurrentDef in aUserDefs
         cHarbourFlags += " -D" + Alltrim( cCurrentDef ) + " "
      NEXT
   ENDIF

   IF ! Empty( s_cUserInclude )
      cHarbourFlags += " -I" + Alltrim( s_cUserInclude ) + " "
   ENDIF

   s_lBcc   :=  "BCC"   $ cCompiler
   s_lMSVcc :=  "MSVC"  $ cCompiler
   s_lGcc   :=  "GCC"   $ cCompiler
   s_lPocc  :=  "POCC"  $ cCompiler
   s_lMinGW :=  "MINGW" $ cCompiler

   if s_lMinGW
      s_lGcc := .T.
      s_lBcc :=  s_lMSVcc :=  s_lPocc :=  .F.
   endif

   cObjDir := Alltrim( cObjDir )

   IF ! Empty( cObjDir )

      IF !hb_DirExists( cObjDir )
#ifdef HB_COMPAT_C53
         MakeDir( cObjDir )
#endif
      ENDIF

   ENDIF

   s_aMacros := GetSourceDirMacros( s_lGcc, cOS )

   IF s_lGcc
      cObjDir := Alltrim( cObjDir )

      IF ! Empty( cObjDir )
         cObjDir += "/"
      ENDIF

      cTest := cObjDir + "/"
   ELSE
      cObjDir := Alltrim( cObjDir )

      IF ! Empty( cObjDir )
         cObjDir += "\"
      ENDIF

      cTest := cObjDir + "\"
   ENDIF

   AEval( s_aMacros, { | x, y | cItem := Substr( x[ 2 ], 1, Len( x[ 2 ] ) ), IIF( At( citem, cTest ) > 0, ( s_aMacros[ y, 1 ] := "OBJ", s_aMacros[ y, 2 ] := cObjDir ), ) } )

   IF lAutoMemvar
      cHarbourFlags += " -a "
   ENDIF

   IF lVarIsMemvar
      cHarbourFlags += " -v "
   ENDIF

   IF lDebug
      cHarbourFlags += " -b "
   ENDIF

   IF lSupressline
      cHarbourFlags += " -l "
   ENDIF

   IF s_lGenppo
      cHarbourFlags += " -p "
   ENDIF

   IF lCompmod
      cHarbourFlags += " -m "
   ENDIF

   IF s_lBcc
      AAdd( s_aCommands, { ".cpp.obj:", "$(CC_DIR)\bin\bcc32 $(CFLAG1) $(CFLAG2) -o$* $**" } )
      AAdd( s_aCommands, { ".c.obj:", "$(CC_DIR)\bin\bcc32 -I$(HB_DIR)\include $(CFLAG1) $(CFLAG2) -o$* $**" } )
      AAdd( s_aCommands, { ".prg.obj:", "$(HB_DIR)\bin\harbour -n -go" + iif(s_lGenCsource,"3","") + " -I$(HB_DIR)\include $(HARBOURFLAGS) -I$(FWH)\include -o$* $**" } )
      AAdd( s_aCommands, { ".rc.res:", "$(CC_DIR)\bin\brcc32 $(RFLAGS) $<" } )

   ELSEIF s_lGcc

      IF  "LINUX" $ Upper( Getenv( "HB_ARCHITECTURE" ) )  .OR. cOS == "Linux"
         AAdd( s_aCommands, { ".cpp.o:", "gcc $(CFLAG1) $(CFLAG2) -o$* $**" } )
         AAdd( s_aCommands, { ".c.o:", "gcc -I/usr/include/harbour $(CFLAG1) $(CFLAG2) -I. -o$* $**" } )
         AAdd( s_aCommands, { ".prg.o:", "harbour -n $(HARBOURFLAGS) -I/usr/include/harbour -I. -go" + iif(s_lGenCsource,"3","") + "  -o$* $**" } )
      ELSE
         AAdd( s_aCommands, { ".cpp.o:", "$(CC_DIR)\bin\gcc $(CFLAG1) $(CFLAG2) -o$* $**" } )
         AAdd( s_aCommands, { ".c.o:", "$(CC_DIR)\bin\gcc -I$(HB_DIR)/include $(CFLAG1) $(CFLAG2) -I. -o$* $**" } )
         AAdd( s_aCommands, { ".prg.o:", "$(HB_DIR)\bin\harbour -n -go" + iif(s_lGenCsource,"3","") + " -I$(HB_DIR)/include $(HARBOURFLAGS)  -o$* $**" } )
      ENDIF

   ELSEIF s_lMSVcc
      AAdd( s_aCommands, { ".cpp.obj:", "$(CC_DIR)\bin\cl $(CFLAG1) $(CFLAG2) -Fo$* $**" } )
      AAdd( s_aCommands, { ".c.obj:", "$(CC_DIR)\bin\cl -I$(HB_DIR)\include $(CFLAG1) $(CFLAG2) -Fo$* $**" } )
      AAdd( s_aCommands, { ".prg.obj:", "$(HB_DIR)\bin\harbour -go" + iif(s_lGenCsource,"3","") + " -n -I$(HB_DIR)\include $(HARBOURFLAGS) -I$(C4W)\include -o$* $**" } )
      AAdd( s_aCommands, { ".rc.res:", "$(CC_DIR)\bin\rc $(RFLAGS) $<" } )

   ELSEIF s_lPocc
      AAdd( s_aCommands, { ".cpp.obj:", "$(CC_DIR)\bin\pocc $(CFLAG1) $(CFLAG2) -Fo$* $**" } )
      AAdd( s_aCommands, { ".c.obj:", "$(CC_DIR)\bin\pocc -I$(HB_DIR)\include $(CFLAG1) $(CFLAG2) -Fo$* $**" } )
      AAdd( s_aCommands, { ".prg.obj:", "$(HB_DIR)\bin\harbour -n -go" + iif(s_lGenCsource,"3","") + " -I$(HB_DIR)\include $(HARBOURFLAGS) -I$(FWH)\include -o$** $**" } )
      AAdd( s_aCommands, { ".rc.res:", "$(CC_DIR)\bin\porc $(RFLAGS) $<" } )

   ENDIF

   if s_nlang     == LANG_PT           /* portuguese brazilian */
      s_cAlertMsg := "Selecione os .prgs a compilar"
   elseif s_nLang == LANG_ES           /* spanish              */
      s_cAlertMsg := "Seleccione los .prg a compilar"
   ELSE                                /* english              */
      s_cAlertMsg := "Select the .prg files to compile"
   endif

   aInFiles := GetSourceFiles( s_lRecursive, s_lGcc, cOS )

   nLenaSrc := Len( aInFiles )

   aOutFiles := aClone( aInFiles )

   IF nOption !=2 // not create a makefile
      PickArray( 10, 15, 19, 64, aInFiles, aOutFiles, ArrayAJoin( { oMake:aPrgs, oMake:aCs } ), .T., s_cAlertMsg )
   ELSE
      PickArray( 10, 15, 19, 64, aInFiles, aOutFiles, {}, .T., s_cAlertMsg )
   ENDIF

   AEval( aOutFiles, { | x, y | HB_SYMBOL_UNUSED( x ), aOutFiles[ y ] := Trim( Substr( aOutFiles[ y ], 1, At( " ", aOutFiles[ y ] ) ) ) } )
   AEval( aOutFiles, { | xItem | IIF( At( ".c", xItem ) > 0 .OR. At( ".C", xItem ) > 0 .OR. At( ".cpp", xItem ) > 0 .OR. At( ".CPP", xItem ) > 0, AAdd( aOutc, xitem ), ) } )
   AEval( aOutc, { | x, z | citem := x, z := AScan( aOutFiles, { | t | t = citem } ), IIF( z > 0, aSize( aDel( aOutFiles, z ), Len( aOutFiles ) - 1 ), ) } )

   aOutFiles  := aSort( aOutFiles )
   s_aPrgs := aClone( aOutFiles )

   s_aObjs := aClone( aOutFiles )
   AEval( s_aObjs, { | xItem, x | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cext := Substr( cExt, 2 ), IIF( ! s_lGcc, s_aObjs[ x ] := cObjDir + cTest + "." + Exte( cExt, 2 ), s_aObjs[ x ] := cObjDir + cTest + "." + Exte( cExt, 3 ) ) } )
   s_aCFiles := aClone( aOutc )
   s_aObjsC := aClone( aOutc )
   AEval( aOutc, { | xItem, x | hb_FNAMESPLIT( xiTem, @cPath, @cTest, @cExt, @cDrive ), cext := Substr( cExt, 2 ), IIF( ! s_lGcc, s_aObjsC[ x ] := cObjDir + cTest + "." + Exten( cExt, 2 ), s_aObjsC[ x ] := cObjDir + cTest + "." + Exten( cExt, 1 ) ) } )

   FOR x := 1 TO Len( s_aMacros )

      IF ! Empty( s_aMacros[ x, 2 ] )
         cItem := s_aMacros[ x, 2 ]
         nPos  := AScan( s_aPrgs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

         IF nPos > 0
            AEval( s_aPrgs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aPrgs[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ")\" ) ), ) } )

            IF ! s_aMacros[ x, 3 ]
               FWrite( s_nMakeFileHandle, s_aMacros[ x, 1 ] + " = " + Left( s_aMacros[ x, 2 ], Len( s_aMacros[ x, 2 ] ) - 1 ) + " " + s_cEOL )
               s_aMacros[ x, 3 ] := .T.
            ENDIF

         ENDIF

         nPos := AScan( s_aCFiles, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

         IF nPos > 0
            AEval( s_aCFiles, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aCFiles[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ")\" ) ), ) } )

            IF ! s_aMacros[ x, 3 ]
               FWrite( s_nMakeFileHandle, s_aMacros[ x, 1 ] + " = " + Left( s_aMacros[ x, 2 ], Len( s_aMacros[ x, 2 ] ) - 1 ) + " " + s_cEOL )
               s_aMacros[ x, 3 ] := .T.
            ENDIF

         ENDIF

         nPos := AScan( s_aObjs, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

         IF nPos > 0

            IF ! Empty( cObjDir )
               AEval( s_aObjs, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aObjs[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ")\" ) ), ) } )
               FWrite( s_nMakeFileHandle, s_aMacros[ x, 1 ] + " = " + Left( s_aMacros[ x, 2 ], Len( s_aMacros[ x, 2 ] ) - 1 ) + " " + s_cEOL )
            ENDIF

         ENDIF

         nPos := AScan( s_aObjsC, { | z | hb_FNAMESPLIT( z, @cPath, @cTest, @cExt, @cDrive ), cPath == citem } )

         IF nPos > 0

            IF ! Empty( cObjDir )
               AEval( s_aObjsC, { | a, b | hb_FNAMESPLIT( a, @cPath, @cTest, @cExt, @cDrive ), IIF( cPath == citem, s_aObjsC[ b ] := Strtran( a, cPath, "$(" + s_aMacros[ x, 1 ] + IIF( s_lGcc, ")/", ")\" ) ), ) } )
            ENDIF

         ENDIF

      ENDIF

   NEXT

   IF s_lGcc
      FWrite( s_nMakeFileHandle, "PROJECT = " + IIF( lInstallLib, "$(HB_DIR)/lib/", "" ) + Alltrim( cLibName ) + ".a " + s_cEOL )
   ELSE
      FWrite( s_nMakeFileHandle, "PROJECT = " + IIF( lInstallLib, "$(HB_DIR)\lib\", "" ) + Alltrim( cLibName ) + ".lib $(PR)" + s_cEOL )
   ENDIF

   FWrite( s_nMakeFileHandle, "OBJFILES =" )
   nWriteFiles := 0

   IF Len( s_aObjs ) < 1
      FWrite( s_nMakeFileHandle, + " $(OB) " + s_cEOL )
   ELSE
      AEval( s_aObjs, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aObjs ), FWrite( s_nMakeFileHandle, " " + Alltrim( x ) + IIF( nWriteFiles % 10 == 0, " //" + s_cEOL, "" ) ), FWrite( s_nMakeFileHandle, " " + Alltrim( x ) + " $(OB) " + s_cEOL ) ) } )
   ENDIF

   FWrite( s_nMakeFileHandle, "PRGFILES =" )
   nWriteFiles := 0

   IF Len( s_aPrgs ) < 1
      FWrite( s_nMakeFileHandle, + " $(PS)" + s_cEOL )
   ELSE
      AEval( s_aPrgs, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aPrgs ), FWrite( s_nMakeFileHandle, " " + Alltrim( x ) + IIF( nWriteFiles % 10 == 0, " //" + s_cEOL, "" ) ), FWrite( s_nMakeFileHandle, " " + Alltrim( x ) + " $(PS) " + s_cEOL ) ) } )
   ENDIF

   nWriteFiles := 0

   IF Len( s_aObjsC ) > 0
      FWrite( s_nMakeFileHandle, "OBJCFILES =" )

      IF Len( s_aObjsC ) < 1
         FWrite( s_nMakeFileHandle, + " $(OBC) " + s_cEOL )
      ELSE
         AEval( s_aObjsC, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aObjsC ), FWrite( s_nMakeFileHandle, " " + Alltrim( x ) + IIF( nWriteFiles % 10 == 0, " //" + s_cEOL, "" ) ), FWrite( s_nMakeFileHandle, " " + Alltrim( x ) + " $(OBC) " + s_cEOL ) ) } )
      ENDIF

   ENDIF

   nWriteFiles := 0

   IF Len( s_aCFiles ) > 0
      FWrite( s_nMakeFileHandle, "CFILES =" )

      IF Len( s_aCFiles ) < 1
         FWrite( s_nMakeFileHandle, + " $(CF)" + s_cEOL )
      ELSE
         AEval( s_aCFiles, { | x, i | nWriteFiles ++, IIF( i <> Len( s_aCFiles ), FWrite( s_nMakeFileHandle, " " + Alltrim( x ) + IIF( nWriteFiles % 10 == 0, " //" + s_cEOL, "" ) ), FWrite( s_nMakeFileHandle, " " + Alltrim( x ) + " $(CF) " + s_cEOL ) ) } )
      ENDIF

   ENDIF

   FWrite( s_nMakeFileHandle, ;
      "RESFILES =" + s_cEOL +;
      "RESDEPEN = $(RESFILES)" + s_cEOL +;
      "DEFFILE = " + s_cEOL +;
      "HARBOURFLAGS = " + cHarbourFlags + s_cEOL +;
      "INSTALLLIB = " + IIF( lInstallLib, "YES","NO" ) + s_cEOL +;
      "USERDEFINE = " + alltrim(s_cUserDefine) + s_cEOL +;
      "USERINCLUDE = " + alltrim(s_cUserInclude) + s_cEOL +;
      "EDITOR = " + s_cEditor + s_cEOL )

   IF s_lBcc

      FWrite( s_nMakeFileHandle, ;
         "CFLAG1 =  -OS $(SHELL)  $(CFLAGS) -d -L$(HB_DIR)\lib;$(FWH)\lib -c" + s_cEOL +;
         "CFLAG2 =  -I$(HB_DIR)\include -I$(CC_DIR)\include -I" + Alltrim( s_cUserInclude ) + s_cEOL +;
         "RFLAGS = " + s_cEOL +;
         "LFLAGS = /P32 /0" + s_cEOL +;
         "IFLAGS = " + s_cEOL +;
         "LINKER = tlib $(LFLAGS) $(PROJECT)" + s_cEOL +;
         " " + s_cEOL +;
         "ALLOBJ =  $(OBJFILES) $(OBJCFILES)" + s_cEOL +;
         "ALLRES = $(RESDEPEN)" + s_cEOL +;
         "ALLLIB = " + s_cEOL +;
         ".autodepend" + s_cEOL )

   ELSEIF s_lMSVcc

      FWrite( s_nMakeFileHandle, ;
         "CFLAG1 =  -I$(INCLUDE_DIR) -TP -W3 -nologo $(C_USR) $(SHELL) $(CFLAGS)" + s_cEOL +;
         "CFLAG2 =  -c -I" + Alltrim( s_cUserInclude ) + s_cEOL +;
         "RFLAGS = " + s_cEOL +;
         "LFLAGS = " + s_cEOL +;
         "IFLAGS = " + s_cEOL +;
         "LINKER = lib $(PROJECT)" + s_cEOL +;
         " " + s_cEOL +;
         "ALLOBJ = $(OBJFILES) $(OBJCFILES) " + s_cEOL +;
         "ALLRES = $(RESDEPEN)" + s_cEOL +;
         "ALLLIB = " + s_cEOL )

   ELSEIF s_lPocc

      FWrite( s_nMakeFileHandle, ;
         "CFLAG1 = " + s_cEOL +;
         "CFLAG2 = " + s_cEOL +;
         "RFLAGS = " + s_cEOL +;
         "LFLAGS = " + s_cEOL +;
         "IFLAGS = " + s_cEOL +;
         "LINKER = polib " + s_cEOL +;  // was LINKER = polib $(PROJECT)
         " " + s_cEOL +;
         "ALLOBJ = $(OBJFILES) $(OBJCFILES) " + s_cEOL +;
         "ALLRES = $(RESDEPEN)" + s_cEOL +;
         "ALLLIB = " + s_cEOL )

   ELSEIF s_lGcc

      FWrite( s_nMakeFileHandle, ;
         "CFLAG1 = " + IIF( s_lLinux , "-I/usr/include/harbour", " -I$(HB_DIR)/include " ) + " $(SHELL)  -c -Wall" + s_cEOL +;
         "CFLAG2 = " + IIF( s_lLinux , "-L /usr/lib/harbour", " -L $(HB_DIR)/lib" ) + s_cEOL +;
         "RFLAGS = " + s_cEOL +;
         "LFLAGS = " + s_cEOL +;
         "IFLAGS = " + s_cEOL )

      FWrite( s_nMakeFileHandle, "LINKER = ar -M " + s_cEOL )

      FWrite( s_nMakeFileHandle, ;
         " " + s_cEOL +;
         "ALLOBJ = $(OBJFILES) $(OBJCFILES) " + s_cEOL +;
         "ALLRES = $(RESDEPEN) " + s_cEOL +;
         "ALLLIB = $(HBLIBS) " + s_cEOL +;
         ".autodepend" + s_cEOL )

   ENDIF

   FWrite( s_nMakeFileHandle, ;
      " " + s_cEOL +;
      "#COMMANDS" + s_cEOL )
   AEval( s_aCommands, { | xItem | FWrite( s_nMakeFileHandle, xitem[ 1 ] + s_cEOL ), FWrite( s_nMakeFileHandle, xitem[ 2 ] + s_cEOL ), FWrite( s_nMakeFileHandle, " " + s_cEOL ) } )

   IF s_lBcc

      FWrite( s_nMakeFileHandle, ;
         "#BUILD" + s_cEOL +;
         " " + s_cEOL +;
         "$(PROJECT): $(CFILES) $(OBJFILES)" + s_cEOL +;
         "    $(CC_DIR)\bin\$(LINKER) @&&!" + s_cEOL +;
         "    $(ALLOBJ)" + s_cEOL +;
         "!" + s_cEOL )

   ELSEIF s_lMSVcc .OR. s_lPocc

      FWrite( s_nMakeFileHandle, ;
         "#BUILD" + s_cEOL +;
         " " + s_cEOL +;
         "$(PROJECT): $(CFILES) $(OBJFILES)" + s_cEOL +;
         "    $(CC_DIR)\bin\$(LINKER) @&&!" + s_cEOL +;
         "    $(PROJECT)" + s_cEOL +;
         "    $(ALLOBJ)" + s_cEOL +;
         "!" + s_cEOL )

   ELSEIF s_lGcc

      FWrite( s_nMakeFileHandle, ;
         "#BUILD" + s_cEOL +;
         " " + s_cEOL +;
         "$(PROJECT): $(CFILES) $(OBJFILES) " + s_cEOL )

      IF  "LINUX" $ Upper( Getenv( "HB_ARCHITECTURE" ) )  .OR. cOS == "Linux"
         FWrite( s_nMakeFileHandle, "    $(LINKER) @&&!" + s_cEOL )
      ELSE
         FWrite( s_nMakeFileHandle, "    $(CC_DIR)\bin\$(LINKER) @&&!" + s_cEOL )
      ENDIF

      FWrite( s_nMakeFileHandle, ;
         "    $(PROJECT) " + s_cEOL +;
         "    $(ALLOBJ)  " + s_cEOL +;
         "!" + s_cEOL )

   ENDIF

   FClose( s_nMakeFileHandle  )
   s_nMakeFileHandle:= F_ERROR  // Invalid handle now file is closed

   IF !lCancelMake

      IF s_nLang == LANG_PT .OR.;      /* portuguese brazilian */
         s_nLang == LANG_ES            /* spanish              */
        s_cAlertMsg := "Compilar lib ? (S/N) "

      ELSE                             /* english             */
        s_cAlertMsg := "Build lib ? (Y/N) "
      ENDIF

      @ 20,5 Say s_cAlertMsg Get cBuild PICT "!" Valid cBuild $ iif(s_nLang == LANG_EN,"YN","SN")
      READ

   ENDIF

   /*  Set Parameter for how to continue
    *  Cancell Make    : -1
    *  Compile All     :  1
    */

   RETURN  nBuildReturn := IIF( cBuild $ "SY", 1, -1 )

*---------------------
FUNCTION SetBuildLib( nFHandle )
*---------------------

   LOCAL cRead as String
   LOCAL nPos as Numeric
   LOCAL aMacro as Array
   LOCAL aTemp as Array
   LOCAL nCount as Numeric
   LOCAL aCurobjs as Array
   LOCAL nObjPos as Numeric
   LOCAL cLib
   LOCAL xInfo

   s_lEof:= (hb_FReadLine( nFHandle, @cRead, s_aEOL ) == HB_FEOF)
   cRead := Alltrim( cRead )
   s_nMakeFileHandle := FCreate( s_cLinkFile )

   IF s_nMakeFileHandle == F_ERROR

      IF s_nLang     == LANG_PT        /* portuguese brazilian */
         s_cAlertMsg := "<"+s_cLinkFile + "> n∆o pode ser criado."
      ELSEIF s_nLang == LANG_ES        /* spanish              */
         s_cAlertMsg := "<"+s_cLinkFile + "> no pode ser criado."
      ELSE                             /* english             */
         s_cAlertMsg := "<"+s_cLinkFile + "> cannot be created."
      ENDIF

      Alert( s_cAlertMsg+" FERROR ("+Ltrim(Str(FError()))+")" )
      RETURN NIL

   ENDIF

   s_szProject := cRead
   aMacro      := ListAsArray2( cRead, ":" )

   IF Len( aMacro ) > 1
      aTemp := ListAsArray2( aMacro[ 2 ], " " )
      AEval( aTemp, { | xItem | AAdd( s_aBuildOrder, xItem ) } )
   ENDIF

   AAdd( s_aBuildOrder, aMacro[ 1 ] )
   cRead  := Strtran( cRead, "@&&!", "" )
   aMacro := ListAsArray2( cRead, "\" )
   AEval( aMacro, { | xMacro | Findmacro( xMacro, @cRead ) } )

   IF s_lBcc .OR. s_lMSVcc .OR. s_lPocc
      s_cLinkCommands := cRead + " @" + s_cLinkFile
   ELSE
      s_cLinkCommands := cRead + " < " + s_cLinkFile
   ENDIF

   FOR nPos := 1 TO 7

      s_lEof := (hb_FReadLine( nFHandle, @cRead, s_aEOL ) == HB_FEOF)
      cRead  := Alltrim( cRead )
      aMacro := ListAsArray2( cRead, " " )

      FOR nCount := 1 TO Len( aMacro )

         IF  "$" $ aMacro[ nCount ]

            IF ( aMacro[ nCount ] = "$(PROJECT)" ) .AND. (s_lGcc .OR. s_lMSVcc .OR. s_lPocc)

               FindMacro( aMacro[ nCount ], @cRead )

               IF s_lGcc
                  FWrite( s_nMakeFileHandle, "CREATE  lib" + cRead + s_cEOL )
                  cLib := "lib" + cRead
//                  FWrite( s_nMakeFileHandle, cRead + s_cEOL )
               ELSE
                  cRead := strtran(cRead,",","")
                  cRead := strtran(cRead,"+","")
                  xInfo := iif(s_lMSVcc," -out:","/out:")
                  xInfo += cRead
                  cRead := xInfo
                  FWrite( s_nMakeFileHandle, cRead + s_cEOL )
               ENDIF

            ELSEIF ( aMacro[ nCount ] == "$(ALLOBJ)" )

               Findmacro( aMacro[ nCount ], @cRead )
               aCurObjs := ListAsArray2( cRead, " " )

               FOR nObjPos := 1 TO Len( aCurObjs )

                  IF s_lGcc
                     FWrite( s_nMakeFileHandle, "ADDMOD " + aCurObjs[ nObjPos ] + s_cEOL )
                  ENDIF

                  IF s_lBcc .OR. s_lMSVcc .OR. s_lPocc

                     IF nObjPos < Len( aCurObjs )
                        FWrite( s_nMakeFileHandle, iif(s_lBcc,"+-","") + aCurObjs[ nObjPos ] + iif(s_lBcc," &","") + s_cEOL )
                     ELSE
                        FWrite( s_nMakeFileHandle, iif(s_lBcc,"+-","") + aCurObjs[ nObjPos ] + s_cEOL )
                     ENDIF

                  ENDIF

               NEXT

            ENDIF

         ENDIF

      NEXT

   NEXT

   IF s_lGcc
      FWrite( s_nMakeFileHandle, "SAVE" + s_cEOL +;
         "END " + s_cEOL )
   ENDIF

   FClose( s_nMakeFileHandle )
   s_nMakeFileHandle:= F_ERROR  // Invalid handle now file is closed

   IF s_lLinux
      s_cLinkCommands += " || rm -f " + cLib
   ENDIF

RETURN NIL

*----------------------------
FUNCTION CheckIFfile( nFHandle, cFile )
*----------------------------

   LOCAL cScreen
   LOCAL cNextLine := ""
   LOCAL cCommand  := ""
   LOCAL cTemp

   cTemp := Substr( cFile, At( " ", cFile ) + 1 )

   IF File( cTemp )
      s_lEof    := (hb_FReadLine( nFHandle, @cNextLine, s_aEOL ) == HB_FEOF)
      cNextLine := Trim( cNextLine )

      IF  "!" $ cNextLine
         cCommand := Substr( cNextLine, At( " ", cNextLine ) + 1 )

         cScreen := SAVESCREEN()
         s_lErrors := IIF( hb_run( cCommand ) != 0, .T., .F. )
         RESTSCREEN( ,,,, )
      ENDIF

      RETURN .T.

   ENDIF

RETURN .F.

*----------------------------
FUNCTION Checkstdout( cText )
*----------------------------

   cText := Strtran( cText, "!stdout", "" )
   OutStd( cText )

RETURN NIL

*---------------------------
FUNCTION CheckIFdef( nFHandle, cTemp )
*---------------------------

   LOCAL nPos
   LOCAL cRead    := ""
   LOCAL aSet     := {}
   LOCAL nMakePos
   LOCAL nFilePos

   IF cTemp == "!endif"
      RETURN NIL
   ENDIF

   WHILE At( "!endif", cRead ) == 0

      nFilePos:= FSEEK( nFHandle, 0, FS_RELATIVE )
      s_lEof:= (hb_FReadLine( nFHandle, @cRead, s_aEOL ) == HB_FEOF)
      cRead := Trim( cRead )

      IF  "!endif" $ cRead
         FSEEK( nFHandle, nFilePos, FS_SET )
         EXIT
      ENDIF

      cTemp := Strtran( cTemp, "!ifdef ", "" )

        IF  "=" $ cRead

         IF  "\.." $ cRead
            cRead := Substr( cRead, 1, At( "\..", cRead ) - 1 )
         ELSEIF  "/.." $ cRead
            cRead := Substr( cRead, 1, At( "/..", cRead ) - 1 )
         ENDIF

         aSet := ListAsArray2( cRead, "=" )
         nPos := AScan( s_aDefines, { | x | x[ 1 ] == cTemp } )

         IF nPos > 0
            cRead    := Alltrim( Strtran( aSet[ 1 ], "$(", "" ) )
            cRead    := Strtran( cRead, ")", "" )
            nMakePos := AScan( s_aMacros, { | x | x[ 1 ] == cRead } )

            IF nMakePos == 0
               AAdd( s_aMacros, { aSet[ 1 ], aSet[ 2 ] } )
            ENDIF

         ELSE /* Locate For !ELSE    */

            WHILE At( "!endif", cRead ) == 0
               s_lEof:= (hb_FReadLine( nFHandle, @cRead, s_aEOL ) == HB_FEOF)
               cRead := Trim( cRead )

               IF  "!ELSE" $ cRead

                  WHILE At( "!endif", cRead ) == 0
                     nFilePos:= FSEEK( nFHandle, 0, FS_RELATIVE )
                     s_lEof:= (hb_FReadLine( nFHandle, @cRead, s_aEOL ) == HB_FEOF)
                     cRead := Trim( cRead )

                     IF  "!endif" $ cRead
                        FSEEK( nFHandle, nFilePos, FS_SET )
                        EXIT
                     ENDIF

                     aSet := ListAsArray2( cRead, "=" )
                     AAdd( s_aMacros, { aSet[ 1 ], aSet[ 2 ] } )
                   ENDDO

               ENDIF

            ENDDO

         ENDIF

      ELSEIF "!stdout" $ cRead
         Checkstdout( cRead )
      ENDIF

   ENDDO

RETURN NIL

*-------------------------
FUNCTION BuildBccCfgFile()
*-------------------------
LOCAL cCfg := s_cHarbourDir + "\bin\"+s_cHarbourCFG
LOCAL nCfg

   IF !File( cCfg ) .or. s_lForce

      nCfg := FCreate( cCfg )

      if nCfg == F_ERROR
         IF s_nLang     == LANG_PT     /* portuguese brazilian */
            s_cAlertMsg := cCfg + " n∆o pode ser criado."
         ELSEIF s_nLang == LANG_ES     /* spanish              */
            s_cAlertMsg := cCfg + " no pode ser criado."
         ELSE                          /* english             */
            s_cAlertMsg := cCfg + " cannot be created."
         ENDIF
         Alert( s_cAlertMsg+" FERROR ("+Ltrim(Str(FError()))+")" )
         RETURN NIL
      endif

      FWrite( nCfg, ;
         "CC=BCC32" + s_cEOL +;
         "CFLAGS= -c -DHB_DYNLIB " + ReplaceMacros( "-I$(HB_DIR)\include $(CFLAGS) -d -L$(HB_DIR)\lib" ) + s_cEOL +;  // Added -DHB_DYNLIB flag
         "VERBOSE=YES" + s_cEOL +;
         "DELTMP=YES" + s_cEOL )
      FClose( nCfg )
   ENDIF

RETURN NIL

*-------------------------
FUNCTION BuildMscCfgFile()
*-------------------------
   LOCAL cCfg := s_cHarbourDir + "\bin\"+s_cHarbourCFG
   LOCAL nCfg

   IF !File( cCfg )  .or. s_lForce

      nCfg := FCreate( cCfg )

      if nCfg == F_ERROR
         IF s_nLang     == LANG_PT     /* portuguese brazilian */
            s_cAlertMsg := cCfg + " n∆o pode ser criado."
         ELSEIF s_nLang == LANG_ES     /* spanish              */
            s_cAlertMsg := cCfg + " no pode ser criado."
         ELSE                          /* english             */
            s_cAlertMsg := cCfg + " cannot be created."
         ENDIF
         Alert( s_cAlertMsg+" FERROR ("+Ltrim(Str(FError()))+")" )
         RETURN NIL
      endif

      FWrite( nCfg, ;
         "CC=cl" + s_cEOL +;
         "CFLAGS= -c -DHB_DYNLIB" + ReplaceMacros( "-I$(HB_DIR) -TP -W3 -nologo $(C_USR) $(CFLAGS)" ) + s_cEOL +;
         "VERBOSE=YES" + s_cEOL +;
         "DELTMP=YES" + s_cEOL )
      FClose( nCfg )
   ENDIF

RETURN NIL

*-------------------------
FUNCTION BuildPccCfgFile()
*-------------------------
   LOCAL cCfg := s_cHarbourDir + "\bin\"+s_cHarbourCFG
   LOCAL nCfg

   IF !File( cCfg )  .or. s_lForce

      nCfg := FCreate( cCfg )

      if nCfg == F_ERROR
         IF s_nLang     == LANG_PT     /* portuguese brazilian */
            s_cAlertMsg := cCfg + " n∆o pode ser criado."
         ELSEIF s_nLang == LANG_ES     /* spanish              */
            s_cAlertMsg := cCfg + " no pode ser criado."
         ELSE                          /* english             */
            s_cAlertMsg := cCfg + " cannot be created."
         ENDIF
         Alert( s_cAlertMsg+" FERROR ("+Ltrim(Str(FError()))+")" )
         RETURN NIL
      endif

      FWrite( nCfg, ;
         "CC=POCC" + s_cEOL +;
         "CFLAGS= /Ze /Go /Ot /Tx86-coff /DHB_DYNLIB " + ReplaceMacros( "-I$(HB_DIR)\include $(C_USR) $(CFLAGS)" )  + s_cEOL +;
         "VERBOSE=YES" + s_cEOL +;
         "DELTMP=YES" + s_cEOL )
      FClose( nCfg )

   ENDIF

RETURN NIL

*-------------------------
FUNCTION BuildGccCfgFile()
*-------------------------
   LOCAL cCfg
   LOCAL nCfg
   LOCAL cDir := s_cHarbourDir
//   LOCAL cBhc := Alltrim( Strtran( ReplaceMacros( "$(HB_DIR)" ), "\", "/" ) )

//   cDir := Strtran( cDir, "/", "\" )

   cCfg := s_cHarbourDir + "\bin\"+s_cHarbourCFG

   IF !File( cCfg ) .or. s_lForce

      nCfg := FCreate( cCfg )

      if nCfg == F_ERROR
         IF s_nLang     == LANG_PT     /* portuguese brazilian */
            s_cAlertMsg := cCfg + " n∆o pode ser criado."
         ELSEIF s_nLang == LANG_ES     /* spanish              */
            s_cAlertMsg := cCfg + " no pode ser criado."
         ELSE                          /* english             */
            s_cAlertMsg := cCfg + " cannot be created."
         ENDIF
         Alert( s_cAlertMsg+" FERROR ("+Ltrim(Str(FError()))+")" )
         RETURN NIL
      endif

      FWrite( nCfg, ;
         "CC=gcc" + s_cEOL +;
         "CFLAGS= -c -DHB_DYNLIB " + ReplaceMacros( "-I" + s_cHarbourDir + "/include $(C_USR)  -L" + s_cHarbourDir + "/lib" )  + iif(s_lmingw ," -mno-cygwin ","" )+ s_cEOL +;
         "VERBOSE=YES" + s_cEOL +;
         "DELTMP=YES" + s_cEOL )
      FClose( nCfg )
   ENDIF

RETURN NIL

*--------------------------
FUNCTION BuildGccCfgFileL()
*--------------------------
   LOCAL cCfg := "/etc/"+s_cHarbourCFG
   LOCAL nCfg

   IF !File( cCfg )  .or. s_lForce

      nCfg := FCreate( cCfg )

      if nCfg == F_ERROR
         IF s_nLang     == LANG_PT     /* portuguese brazilian */
            s_cAlertMsg := cCfg + " n∆o pode ser criado."
         ELSEIF s_nLang == LANG_ES     /* spanish              */
            s_cAlertMsg := cCfg + " no pode ser criado."
         ELSE                          /* english             */
            s_cAlertMsg := cCfg + " cannot be created."
         ENDIF
         Alert( s_cAlertMsg+" FERROR ("+Ltrim(Str(FError()))+")" )
         RETURN NIL
      endif

      FWrite( nCfg, ;
         "CC=gcc" + s_cEOL +;
         "CFLAGS= -c -I/usr/include/harbour" + s_cEOL +;
         "VERBOSE=YES" + s_cEOL +;
         "DELTMP=YES" + s_cEOL )
      FClose( nCfg )
   ENDIF

RETURN NIL

*------------------------------
FUNCTION FindHarbourCfg( cCfg )
*------------------------------

   LOCAL cPath AS STRING := ""
   LOCAL lFound AS LOGICAL := .F.
   LOCAL cEnv AS STRING
   LOCAL aEnv as Array of String
   LOCAL lLinux :=  s_lLinux
   LOCAL nPos

   IF ! lLinux .OR. s_lOs2
      cEnv := Gete( "PATH" ) + ";" + Curdir()
      aEnv := ListAsArray2( cEnv, ";" )

      FOR nPos := 1 TO Len( aEnv )

         IF File( aEnv[ nPos ] + "\"+s_cHarbourCFG )
            cPath  := aEnv[ nPos ]
            lFound := .T.
            EXIT
         ENDIF

      NEXT

   ELSE

      IF File( "/etc/"+s_cHarbourCFG )
         lFound := .T.
         cPath  := "/etc/"+s_cHarbourCFG
      ENDIF

      IF !lFound

         IF File( "/usr/local/etc/"+s_cHarbourCFG )
            lFound := .T.
            cPath  := "/usr/local/etc/"+s_cHarbourCFG
         ENDIF

      ENDIF

   ENDIF

   cCfg := cPath

RETURN lFound

*-------------------
FUNCTION GetGccDir()
*-------------------

   LOCAL cPath AS STRING := ""
   LOCAL cEnv AS STRING
   LOCAL aEnv AS Array of string
   LOCAL nPos as Numeric

   IF s_lLinux
      cPath := "."
   ELSE
      cEnv := Gete( "PATH" )
      aEnv := ListAsArray2( cEnv, ";" )

      FOR nPos := 1 TO Len( aEnv )

         IF File( aEnv[ nPos ] + "\gcc.exe" )
            cPath := aEnv[ nPos ]
            cPath := Left( cPath, Rat( "\", cPath ) - 1 )
            EXIT
         ENDIF

      NEXT

   ENDIF

RETURN cPath

*-------------------------------------------------------------
FUNCTION ConvertParams( cFile, aFile, p1, p2, p3, p4, p5, p6 )
*-------------------------------------------------------------

   LOCAL cParam := ""

   IF ! Empty( cFile )

      IF Left( cFile, 1 ) $ "- /?"
         cParam += cFile+" "
      ELSE
         cFile := cFile
         AAdd( aFile, cFile )
      ENDIF

   ENDIF

   IF ! Empty( p1 )

      IF Left( p1, 1 ) $ "- /?"
         cParam += p1+" "
      ELSE
         cFile := p1
         AAdd( aFile, cFile )
      ENDIF

   ENDIF

   IF ! Empty( p2 )

      IF Left( p2, 1 ) $ "- /"
         cParam += p2+" "
      ELSE
         cFile := p2
         AAdd( aFile, cFile )
       ENDIF

   ENDIF

   IF ! Empty( p3 )

      IF Left( p3, 1 ) $ "- /"
         cParam += p3+" "
      ELSE
         cFile := p3
         AAdd( aFile, cFile )
      ENDIF

   ENDIF

   IF ! Empty( p4 )

      IF Left( p4, 1 ) $ "- /"
         cParam += p4+" "
      ELSE
         cFile := p4
         AAdd( aFile, cFile )
      ENDIF

   ENDIF

   IF ! Empty( p5 )

      IF Left( p5, 1 ) $ "- /"
         cParam += p5+" "
      ELSE
         cFile := p5
         AAdd( aFile, cFile )
      ENDIF

   ENDIF

   IF ! Empty( p6 )

      IF Left( p6, 1 ) $ "- /"
         cParam += p6+" "
      ELSE
         cFile := p6
         AAdd( aFile, cFile )
      ENDIF

   ENDIF

   cParam := Strtran( cParam, "/"   , "-"    )
   cParam := Strtran( cParam, "-elx", "-ELX" )
   cParam := Strtran( cParam, "-el" , "-ELX" )
   cParam := Strtran( cParam, "-ex" , "-EX"  )
   cParam := Strtran( cParam, "-e"  , "-EX"  )
   cParam := Strtran( cParam, "-i"  , "-I"   )
   cParam := Strtran( cParam, "-p"  , "-P"   )
   cParam := Strtran( cParam, "-b"  , "-B"   )
   cParam := Strtran( cParam, "-gl" , "-GL"  )
   cParam := Strtran( cParam, "-g"  , "-G"   )
   cParam := Strtran( cParam, "-v"  , "-V"   )
   cParam := Strtran( cParam, "-m"  , "-M"   )
   cParam := Strtran( cParam, "-pc" , "-PC"  )
   cParam := Strtran( cParam, "-f"  , "-F"   )
   cParam := Strtran( cParam, "-r"  , "-R"   )
   cParam := Strtran( cParam, "-nr" , "-NR"  ) // cancel recursive search
   cParam := Strtran( cParam, "-l"  , "-L"   )
   cParam := Strtran( cParam, "-c"  , "-C"   )  // compile after create.
   cParam := Strtran( cParam, "-h"  , "-H"   )

   IF "-ELX" $ cParam
      s_lLibrary := .T.
   ENDIF

   IF "-L" $ cParam
      s_cDefLang := Substr( cParam, At( "-L", cParam ) + 2, 2 )
   ENDIF

RETURN cParam

*-----------------------------
FUNCTION WriteMakeFileHeader()
*-----------------------------
/*
  TODO:
  FWrite( s_nMakeFileHandle, ;
     "#" + s_cEOL +;
     "# "+hbMakeID() + s_cEOL +;
     "# "+hbMakeCopyright() + s_cEOL +;
     "# "+Version() + s_cEOL +;
     "# "+HB_Compiler() + s_cEOL +;
     "# "+OS() + s_cEOL +;
     "# Makefile created at: " + dtoc( date() ) + " - " + time() + s_cEOL +;
     "#" + s_cEOL + s_cEOL )
*/

IF s_lMSVcc
   FWrite( s_nMakeFileHandle, "#MSVC" + s_cEOL )
ELSEIF s_lPocc
   FWrite( s_nMakeFileHandle, "#POCC" + s_cEOL )
ELSEIF s_lGcc
   FWrite( s_nMakeFileHandle, "#GCC" + s_cEOL )
ELSE
   FWrite( s_nMakeFileHandle, ;
      "#BCC" + s_cEOL + ;
      "VERSION=BCB.01" + s_cEOL )
ENDIF

   FWrite( s_nMakeFileHandle, ;
      "!ifndef CC_DIR" + s_cEOL +;
      "CC_DIR = $(MAKE_DIR)" + s_cEOL +;
      "!endif" + s_cEOL + s_cEOL +;
      "!ifndef HB_DIR" + s_cEOL +;
      "HB_DIR = $(HARBOUR_DIR)" + s_cEOL +;
      "!endif" + s_cEOL + s_cEOL )
// FWrite( s_nMakeFileHandle, "RECURSE=" + IIF( s_lRecursive, " YES ", " NO " ) + s_cEOL )
// FWrite( s_nMakeFileHandle, " " + s_cEOL )

RETURN NIL

*-------------------------------
FUNCTION BuildLangArray( nLang )
*-------------------------------
LOCAL aLang := Array( 67 )

   DEFAULT nLang TO LANG_EN

   AFill( aLang, "" )

   aLang[1] := hbMakeID()  // Hbmake identification.

   IF nLang == LANG_EN                 /* english             */

      aLang[ 2]  := "Syntax:  hbmake <cFile>.bc [options] - Example: hbmake hello.bc /ex"
      aLang[ 3]  := "Options:  /e[x]   Create a new Makefile. If /ex is used it create a"
      aLang[ 4]  := "                  new make file in extended mode."
      aLang[ 5]  := "          /el[x]  Create a new Makefile. If /elx is used it create a"
      aLang[ 6]  := "                  new make file to build a LIB in extended mode."
      aLang[ 7]  := "          /D      Define a macro."
      aLang[ 8]  := "          /p      Print all commands and depedencies."
      aLang[ 9]  := "          /b      Use Borland C/C++ as C compiler"
      aLang[10] := "          /g+     Use GNU C/C++ as C compiler"
      aLang[11] := "          /b+     Use Borland C/C++ as C compiler"
      aLang[12] := "          /g      Use GNU C/C++ as C compiler"
      aLang[13] := "          /gl     Use GNU C/C++ as C compiler in Linux"
      aLang[14] := "          /gl+    Use GNU C/C++ as C compiler in Linux"
      aLang[15] := "          /v      Use MS-Visual C/C++ as C compiler"
      aLang[16] := "          /f      Force recompiltion of all files"
      aLang[17] := "          /i      Ignore errors returned by command"
      aLang[18] := "          /r /nr  Activate recursive mode. /nr Deactivate recursive mode."
      aLang[19] := "                  Note: /p and /D can be used together"
      aLang[20] := "                        /r and /e[x]/el[x] can be used together"
      aLang[21] := "                  Options with + are the default values"
      aLang[22] := "                  -D switch can accept multiple macros on the same line"
      aLang[23] := "                  or use one macro per -D switch"
      aLang[24] := "                  /l[LANGID] Specify the language to be used on hbmake"
      aLang[25] := "                  LANGID= (EN/PT/ES). On Windows, the default will be the S.O."
      aLang[26] := "                  language. On OS/2, FreeBSD and LINUX will be English."
      aLang[27] := "Environment options"
      aLang[28] := "Select the OS"
      aLang[29] := "Select the C Compiler"
      aLang[30] := "Graph Lib"
      aLang[31] := "Harbour Options"
      aLang[32] := "Automatic memvar declaration /a"
      aLang[33] := "Variables are assumed M-> /v"
      aLang[34] := "Debug info /b"
      aLang[35] := "Suppress line number information /l"
      aLang[36] := "Generate pre-processed output /p"
      aLang[37] := "compile module only /m"
      aLang[38] := "User Defines "
      aLang[39] := "User include Path"
      aLang[40] := "Use contrib libs"
      aLang[41] := "<Spacebar>-Select <Enter>-Continue process <F5> Sel/Unsel All"
      aLang[42] := "Warning level /w"
      aLang[43] := "Numbers of source files per line on makefile"
      aLang[44] := "Use Multi Thread Library"
      aLang[45] := "Executable file name"
      aLang[46] := "Warning level /w"
      aLang[47] := "<Tab>-Next <Sh-Tab>-Prev <Enter>-Sel <"+chr(24)+chr(25)+">-Change Sel <Spacebar>-Open Box"
      /* Messages Start Here */
      aLang[48] := "3rd Party RDD"
      aLang[49] := "What OS you use"
      aLang[50] := "What C compiler you have"
      aLang[51] := "This app use Graphical libraries"
      aLang[52] := "Do you use 3rd Party RDD"
      aLang[53] := "Compress this app"
      aLang[54] := "Compress the app after Linked (use upx ?)"
      aLang[55] := "Your app will be linked to user harbour.dll"
      aLang[56] := "Where the .obj/.o files will be generated"
      aLang[57] := "Specify executable name (without .exe extension)"
      /* More messages for LIB build */
      aLang[58] := "Specify the lib name (without extension)"
      /* More Harbour options for LIB build */
      aLang[59] := "Lib name:"
      aLang[60] := "Obj dir files:"
      aLang[61] := "Install the lib at the Harbour lib folder"
      aLang[62] := "          /pc     Use Pelles C/C++ as C compiler"
      aLang[63] := "          /m      Use MinGW (GCC) as C compiler"

      aLang[64] := "User Libs: "
      aLang[65] := "Errorlog Editor:"
      aLang[66] := "Inform the log error editor for your choice."

   ELSEIF nLang == LANG_ES             /* spanish              */

      aLang[ 2] := "Sintaxe:  hbmake <cArchivo>.bc [opciones] - Exemplo: hbmake hello.bc /ex"
      aLang[ 3] := "Opciones: /e[x]   Crea un Makefile nuevo. Si se usa /ex se crea un nuevo"
      aLang[ 4] := "                  makefile en modo extendido."
      aLang[ 5] := "          /el[x]  Crea un Makefile nuevo. Si se usa /elx se crea un nuevo"
      aLang[ 6] := "                  makefile para construir una LIB en modo extendido."
      aLang[ 7] := "          /D      Define una macro."
      aLang[ 8] := "          /p      Imprime todos los comandos y dependencias."
      aLang[ 9] := "          /b      Usa Borland C/C++ como compilador C"
      aLang[10] := "          /g+     Usa GNU C/C++ como compilador C"
      aLang[11] := "          /b+     Usa Borland C/C++ como compilador C"
      aLang[12] := "          /g      Usa GNU C/C++ como compilador C"
      aLang[13] := "          /gl     Usa GNU C/C++ como compilador C en Linux"
      aLang[14] := "          /gl+    Usa GNU C/C++ como compilador C en Linux"
      aLang[15] := "          /v      Usa MS-Visual C/C++ como compilador C"
      aLang[16] := "          /f      Forza la recompilaci¢n de todos los archivos"
      aLang[17] := "          /i      Ignora los errores devueltos por el comando"
      aLang[18] := "          /r /nr  Activa modo recursivo.  /nr Desactivar modo recursivo."
      aLang[19] := "                  Nota: /p y /D pueden ser usados juntos"
      aLang[20] := "                        /r y /e[x]/el[x] pueden ser usados juntos"
      aLang[21] := "                  Las opciones con + son los valores por omisi¢n"
      aLang[22] := "                  El parametro -D puede aceptar multiplas macros en la misma"
      aLang[23] := "                  linea ou use una macro por parametro -D"
      aLang[24] := "                  /l[LANGID] especifica una linguagem a ser utilizada por"
      aLang[25] := "                   hbmake. LANGID = (EN/PT/ES). En sistemas Windows, Lo padr¢n"
      aLang[26] := "                   es la linguagem de SO. En OS/2, FreeBSD y LINUX ser†n InglÇs."
      aLang[27] := "Opciones de Ambiente"
      aLang[28] := "Seleccione SO"
      aLang[29] := "Seleccione Compilador C"
      aLang[30] := "Lib Grafica"
      aLang[31] := "Opciones de lo Harbour"
      aLang[32] := "Declaraci¢n automatica de memvar /a"
      aLang[33] := "Variables ser†n assumidas M-> /v "
      aLang[34] := "Info. Debug /b"
      aLang[35] := "Suprime info del n£mero de linea /l"
      aLang[36] := "Gera salida pre-processada /p"
      aLang[37] := "Compila solamente lo modulo /m"
      aLang[38] := "Define del usuarios:"
      aLang[39] := "Path includes del usuario:"
      aLang[40] := "Usar libs externas"
      aLang[41] := "<Espacio>-Seleccionar <Enter>-Continuar proceso <F5> Selec/Deselec todo."
      aLang[42] := "Nivel del aviso de lo compilador /w"
      aLang[43] := "Cuantos .prgs por linea no makefile:"
      aLang[44] := "Usar la libreria Multi Thread"
      aLang[45] := "Nombre del ejecutable"
      aLang[46] := "Nivel de Avisos /w"
      aLang[47] := "<Tab>-Avanzar <Sh-Tab>-Volver <Enter>-Selec <"+chr(24)+chr(25)+">-Mudar Selec <Espacio>-Caja"
      /* Messages Start Here */
      aLang[48] := "Rdd Terceros"
      aLang[49] := "Cual OS usted usa"
      aLang[50] := "Cual compilador C usted usa"
      aLang[51] := "Esta App usa Lib Grafica o No"
      aLang[52] := "Usted usa Rdd de terceros"
      aLang[53] := "Comprimir app"
      aLang[54] := "Prensar la App despuÇs de enlazada (usar upx) ?"
      aLang[55] := "Su aplicacion ser† ligada para usar la harbour.dll"
      aLang[56] := "Donde los ficheros *.obj ser†n generados"
      aLang[57] := "Informe lo nombre de lo executable (sin la extension .exe)"
      /* More messages for LIB build */
      aLang[58] := "Informe lo nombre de la lib (sin la extension)"
      /* More Harbour options for LIB build */
      aLang[59] := "Nombre de la Lib:"
      aLang[60] := "Direct¢rio de los Obj:"
      aLang[61] := "Alojar la lib en el direct¢rio lib de Harbour"
      aLang[62] := "          /pc     Usa Pelles C/C++ como compilador C"
      aLang[63] := "          /m      Usa MinGW (GCC) como compilador C"
      aLang[64] := "Libs del usuario: "
      aLang[65] := "Editor de errorlog:"
      aLang[66] := "Informe lo editor de log de errores de su preferencia."

   ELSEIF nLang == LANG_PT             /* brazilian portuguese */

      aLang[ 2] := "Sintaxe:  hbmake <arquivo>.bc [opá‰es] -  Exemplo: hbmake hello.bc /ex"
      aLang[ 3] := "Opá‰es:  /e[x]  Cria um Makefile novo. Se for usado /ex cria um makefile"
      aLang[ 4] := "                em modo extendido."
      aLang[ 5] := "         /el[x] Cria um Makefile novo. Se for usado /elx cria um makefile"
      aLang[ 6] := "                para construir uma LIB, em modo extendido."
      aLang[ 7] := "         /D     Define uma macro."
      aLang[ 8] := "         /p     Imprime todos os comandos e dependàncias."
      aLang[ 9] := "         /b     Usa Borland C/C++ como compilador C"
      aLang[10] := "         /g+    Usa GNU C/C++ como compilador C"
      aLang[11] := "         /b+    Usa Borland C/C++ como compilador C"
      aLang[12] := "         /g     Usa GNU C/C++ como compilador C"
      aLang[13] := "         /gl    Usa GNU C/C++ como compilador C no Linux"
      aLang[14] := "         /gl+   Usa GNU C/C++ como compilador C no Linux"
      aLang[15] := "         /v     Usa MS-Visual C/C++ como compilador C"
      aLang[16] := "         /f     Foráa a recompilaá∆o de todos os arquivos."
      aLang[17] := "         /i     Ignora os erros devolvidos pelo comando."
      aLang[18] := "         /r /nr Ativa modo recursivo. /nr Desativar modo recursivo."
      aLang[19] := "                Nota:  /p e /D podem ser usados juntos"
      aLang[20] := "                       /r e /e[x]/el[x] podem ser usados juntos"
      aLang[21] := "                As opá‰es com + s∆o os valores padr∆o."
      aLang[22] := "                O parÉmetro -D pode aceitar m£ltiplas macros na mesma linha"
      aLang[23] := "                ou use uma macro por parÉmetro -D"
      aLang[24] := "                /l[LANGID] especifica a linguagem a ser utilizada pelo hbmake,"
      aLang[25] := "                LANGID = (EN/PT/ES). Em Windows, o padr∆o ser† a linguagem"
      aLang[26] := "                definida no S.O. Em OS/2, FreeBSD e LINUX o padr∆o ser† Inglàs."
      aLang[27] := "Opá‰es de Ambiente"
      aLang[28] := "Selecione o SO"
      aLang[29] := "Selecione Compilador C"
      aLang[30] := "Lib Gr†f."
      aLang[31] := "Opá‰es do Harbour"
      aLang[32] := "Declaraá∆o Autom†tica de Memvar /a"
      aLang[33] := "Vari†veis s∆o assumidas M-> /v"
      aLang[34] := "Info. Debug /b"
      aLang[35] := "Suprime info de n£mero da linha /l"
      aLang[36] := "Gera sa°da prÇ-processada /p"
      aLang[37] := "Compila apenas o m¢dulo /m"
      aLang[38] := "User Defines:"
      aLang[39] := "User Include Path:"
      aLang[40] := "Usa Libs Externas ?"
      aLang[41] := "<Espaáo>-Seleciona <Enter> p/ continuar processo <F5>-Sel/DeSel. tudo."
      aLang[42] := "N°vel de aviso do compilador /w"
      aLang[43] := "Qtd de .prgs por linha, no makefile: "
      aLang[44] := "Usar a biblioteca Multi Thread ?"
      aLang[45] := "Nome Execut†vel:"
      aLang[46] := "N°vel de Warnings /w"
      aLang[47] := "<Tab>-Avanáa <Sh-Tab>-Volta <Enter>-Sel. <"+chr(24)+chr(25)+">-Muda Sel. <Espc>-Abre Box"
      /* Messages Start Here */
      aLang[48] := "Rdd Terceiros"
      aLang[49] := "Selecione o Sistema Operacional"
      aLang[50] := "Selecione o compilador C/C++"
      aLang[51] := "Esta aplicaá∆o vai usar Lib Grafica ? Qual ?"
      aLang[52] := "Esta aplicaá∆o vai usar Rdd de terceiros ? Qual ?"
      aLang[53] := "Comprimir App ?"
      aLang[54] := "Comprimir a aplicaá∆o ap¢s linkada (usar upx) ?"
      aLang[55] := "Sua aplicaá∆o ser† linkada para usar a harbour.dll ?"
      aLang[56] := "Informe a pasta onde os arquivos *.obj ser∆o gerados"
      aLang[57] := "Informe o nome do execut†vel (sem a extens∆o .exe)"
      /* More messages for LIB build */
      aLang[58] := "Informe o nome da lib (sem a extens∆o)"
      /* More Harbour options for LIB build */
      aLang[59] := "Nome da Lib:"
      aLang[60] := "Diret¢rio dos Obj:"
      aLang[61] := "Instalar a lib no diret¢rio lib do Harbour"
      aLang[62] := "         /pc    Usa Pelles C/C++ como compilador C"
      aLang[63] := "         /m     Usa MinGW (GCC) como compilador C"
      aLang[64] := "Libs de usu†rio: "
      aLang[65] := "Editor de errorlog:"
      aLang[66] := "Informe o editor de log de erros de sua preferància."

   ENDIF

RETURN aLang

*------------------------------------------
FUNCTION GetSelFiles( aInFiles, aOutFiles )
*------------------------------------------

   LOCAL aRet  := {}
   LOCAL cItem
   LOCAL nPos

   FOR EACH cItem IN aInFiles

      nPos := AScan( aOutFiles, { | x | x == Left( cItem, At( " ", citem ) - 1 ) } )

      IF nPos > 0
         AAdd( aRet, cItem )
      ENDIF

   NEXT

RETURN aRet

*---------------------------
FUNCTION ResetInternalVars()
*---------------------------

   s_lPrint        := .F.
   s_lEdit         := .F.
   s_aDefines      := {}
   s_aBuildOrder   := {}
   s_aCommands     := {}
   s_aMacros       := {}
   s_aPrgs         := {}
   s_aContribLibs  := {}
   s_aCFiles       := {}
   s_aObjs         := {}
   s_aObjsC        := {}
   s_lEof          := .F.
   s_aResources    := {}
// s_cLinkFile := "linkfile.lnk"
   s_cLinkCommands := ""

if s_lBcc
   s_lGcc          := .F.
   s_lMSVcc        := .F.
   s_lPocc         := .F.
   s_lMinGW        := .F.
elseif s_lMSVcc
   s_lBcc          := .F.
   s_lGcc          := .F.
   s_lPocc         := .F.
   s_lMinGw        := .F.
elseif s_lPocc
   s_lBcc          := .F.
   s_lGcc          := .F.
   s_lMSVcc        := .F.
   s_lMinGW        := .F.
endif
   s_lForce        := .F.
   s_lLinux        := .F.
   s_szProject     := ""
   s_lLibrary      := .F.
   s_lIgnoreErrors := .F.
   s_lOs2          := .F.
   s_lRecursive    := .F.
   s_lEditMake     := .F.
   s_aDir          := {}
   s_aLangMessages := {}

RETURN NIL

*----------------------------------
FUNCTION OsSpec(GetList,nPos,cSpec)
*----------------------------------
   LOCAL oGet := GetList[nPos]
   LOCAL oControl
   oControl := oGet:Control
   IF oControl != NIL
      cSpec := oControl:GetData( oControl:Value )
//   keyboard chr( K_TAB )
   ENDIF

RETURN .T.

*--------------------------
FUNCTION CheckCompiler(cOS)
*--------------------------
RETURN ( ("Win32" $ cOS) .or. ("Linux" $ cOS) )

*------------------------------
FUNCTION SetThisLibs(aTempLibs)
*------------------------------

LOCAL c := ""
LOCAL n

for Each n In aTempLibs
     c += "-l"
     c += StrTran( n, ".a", "" )
     c+= " "
next

RETURN c

*------------------
FUNCTION ShowHelp( cMsg )
*------------------
   LOCAL   cCmdLine := ""
   LOCAL   nCmdArgs := 0

   DEFAULT s_cEOL TO hb_OsNewLine()

   /* Create Command Line Args */

   WHILE ! EMPTY( hb_argv( ++nCmdArgs ) )
      cCmdLine += hb_argv( nCmdArgs ) + " "
   ENDDO

   /* Changed from OutStd to OutErr so the Help Message can't be redirected */

   OutErr( ;
      hbMakeID()                                                        + s_cEOL +;
      hbMakeCopyright()                                                 + s_cEOL +;
                                                                           s_cEOL +;
      "Syntax:  hbmake <makefile> [options]"                             + s_cEOL +;
                                                                           s_cEOL +;
      "Options:  -bc  Use BCC for the C compiler. (Default Windows)"     + s_cEOL +;
      "          -gc  Use GCC for the C compiler. (Default non-Windows)" + s_cEOL +;
      "          -vc  Use MSVC for the C compiler"                       + s_cEOL +;
      "          -pc  Use Pelles for the C compiler"                     + s_cEOL +;
      "          -e   Editor mode"                                       + s_cEOL +;
      "          -el  Editor mode for libraries"                         + s_cEOL +;
      "          -f   Force all files to be rebuilt"                     + s_cEOL +;
      "          -h   This help screen"                                  + s_cEOL +;
      "          -i   Ignore errors returned by commands"                + s_cEOL +;
      "          -r   Recurse directories for source code"               + s_cEOL +;
      "          -s   Show the commands and defines created"             + s_cEOL +;
      "                  Need to redirect the output to a file."         + s_cEOL +;
                                                                           s_cEOL +;
      "Note:  Linked with " + Version()                                  + s_cEOL )

   IF ! Empty( cCmdLine ) .OR. ! Empty( cMsg )
      OutErr( s_cEOL  )
   ENDIF

   IF ! Empty( cCmdLine )
      OutErr( "Entered:  " + cCmdLine + s_cEOL )
   ENDIF
   IF ! Empty( cMsg )
      OutErr( "Message:  " + cMsg + s_cEOL )
   ENDIF

   RETURN PGM_ERR

*-------------------
FUNCTION hbMakeID()
*-------------------
   RETURN "Harbour Make"

*--------------------------
FUNCTION hbMakeCopyright()
*--------------------------
   RETURN "Copyright (c) 2000-2008, http://www.harbour-project.org/"

*-------------------------------------------------------------------
* Former tmake.prg
*-------------------------------------------------------------------

CLASS THBMAKE

   EXPORTED:

   DATA  aDefines       INIT  {}
   DATA  aBuildOrder    INIT  {}
   DATA  aCommands      INIT  {}
   DATA  aMacros        INIT  {}
   DATA  aPrgs          INIT  {}
   DATA  aExtLibs       INIT  {}
   DATA  aCs            INIT  {}
   DATA  aObjs          INIT  {}
   DATA  aObjsc         INIT  {}
   DATA  aRes           INIT  {}
   DATA  nLinkHandle
   DATA  cLinkcomm      INIT  ""
   DATA  lCompress      INIT .F.
   DATA  lForce         INIT .F.
   DATA  lLinux         INIT .F.
   DATA  szProject      INIT ""
   DATA  lLibrary       INIT .F.
   DATA  lInstallLib    INIT .F.
   DATA  lIgnoreErrors  INIT .F.
   DATA  lExtended      INIT .T.
   DATA  lOs2           INIT .F.
   DATA  lRecurse       INIT .F.
   DATA  lEditMode      INIT .F.
   DATA  aDir
   DATA  aLangMessages  INIT {}
   DATA  cDefLang
   DATA  lFwh           INIT .F.
   DATA  lxFwh          INIT .F.
   DATA  lCw            INIT .F.
   DATA  lMini          INIT .F.
   DATA  lHwgui         INIT .F.
   DATA  lGui           INIT .F.
   DATA  lGtwvt         INIT .F.
   DATA  lGtwvw         INIT .F.
   DATA  lMWvW          INIT .F.
   DATA  lXWT           INIT .F.
   DATA  lxHGtk         INIT .F.
   DATA  lWhoo          INIT .F.
   DATA  lHBWhat        INIT .F.
   DATA  lRddAds        INIT .F.
   DATA  lMediator      INIT .F.
   DATA  cMakefile      INIT ""
   DATA  lContribLib   INIT .F.
   DATA  cObj           INIT ""
   DATA  cUserdef       INIT ""
   DATA  cUserInclude   INIT ""
   DATA  cUserLib       INIT ""
   DATA  lGenppo        INIT .F.
   DATA  lCompMod       INIT .F.
   DATA  lAutomemvar    INIT .F.
   DATA  lvarismemvar   INIT .F.
   DATA  ldebug         INIT .F.
   DATA  lSupressline   INIT .F.
   DATA  StartPath      INIT ""
   DATA  cFmc           INIT ""
   DATA  cMedpath       INIT ""
   DATA  cAppLibName    INIT ""
   DATA  cOs            INIT ""
   DATA  cTopfile       INIT ""
   DATA  aOut           INIT {}
   DATA  cFilesToAdd    INIT 5
   DATA  lMT            INIT .F.
   DATA  cWarningLevel  INIT 0
   DATA  cTopModule     INIT ""
   DATA  cRes           INIT ""
   DATA  cMacro         INIT ""
   DATA  lGenCsource    INIT .F.      // Ath added 31-05-2006
   DATA  cShell         INIT ""
   DATA  cEditor        INIT ""

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

   RETURN Self

METHOD ReadMakefile(cFile) CLASS THbMake

   LOCAL cBuffer     := ""
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
   LOCAL nHandle
   LOCAL cObjitem
   LOCAL cRes        := ""
   LOCAL cItem
   LOCAL lLinux      := At( "LINUX", Upper( Os() ) ) > 0
   LOCAL lExtended   := .T., szProject
   LOCAL lPrgObjRule := .F.

   LOCAL lEof

   nHandle := FOpen( cFile )
   IF nHandle < 0
      RETURN self
   ENDIF
   lEof := hb_FReadLine( nHandle, @cBuffer, s_aEOL ) == HB_FEOF
   cBuffer := Trim( cBuffer )
   ::lLibrary :=.F.

   WHILE !lEof

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

      lEof := hb_FReadLine( nHandle, @cTemp, s_aEOL ) == HB_FEOF
      cTemp := Trim( cTemp )

      IF At( "//", ctemp ) > 0

         WHILE At( "//", ctemp ) > 0
            ctemp := Strtran( ctemp, " //", "" )
            lEof := hb_FReadLine( nHandle, @cTemp1, s_aEOL ) == HB_FEOF
            cTemp += Trim( cTemp1 )
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

            IF aTemp[ 1 ] == "HBLIBS"
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

            IF aTemp[ 1 ] == "HBWHAT"
               ::cFMC := aTemp[2]
               ::lHBWhat := .T.
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

            IF aTemp[ 1 ] == "CONTRIBS"
               ::lContribLib := "YES" $ aTemp[ 2 ]
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

            IF aTemp[ 1 ] == "CONTRIBLIB"
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

   QOut( nhandle )

   FClose( nHandle )

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

*-------------------------------------------------------------------
* Former pickarry.prg
*-------------------------------------------------------------------

*-------------------------------------------------------------------
FUNCTION PickArray( T, L, B, R, InArray, OutArray, aDefault, lAllowAll, cTitle, lLib )
*-------------------------------------------------------------------

   LOCAL aNewArray  := {}
   LOCAL aTemp
   LOCAL cItem
   LOCAL cMarkChar  := "*"
   LOCAL cOldColor  := Setcolor()
   LOCAL cOldScreen
   LOCAL lAdd       := .F.
   LOCAL lIsChecked := .f.
   LOCAL nChoice    := 1
   LOCAL nLenArray  := Len( InArray )
   LOCAL nOffset                     // Calculate an offset to L to center on screen
   LOCAL nOnItem    := 0
   LOCAL nPadLen    := ( R - 1 ) - ( L + 1 )
   LOCAL x
   LOCAL nMaxWidth  := MAX( R - L + 3, LEN( s_aLangMessages[ 41 ] ) + 2 )
   DEFAULT cTitle    TO ""
   DEFAULT lAllowAll TO .F.
   DEFAULT lLib      TO .F.

   cOldScreen := SaveScreen()
   Setcolor( "gr+/rb,b+/w,w+/b,w/b+,w/b,w+/b" )

   nOffSet := INT( ( MAXCOL() - ( R - L ) ) / 2 )
   R       := nOffSet + ( R - L )       // Right 1st order is important
   L       := nOffSet
   B       := MIN( MAXROW() - 1, T + nLenArray )
   x       := INT( ( MAXCOL() - nMaxWidth )/2 )
   @ T - 2, x - 1 CLEAR TO B + 1, x + nMaxWidth
   @ T - 1, L - 1 TO       B, R + 1 double
   @ T - 2, L     SAY cTitle
   Attention( s_aLangMessages[ 41 ], B + 1 )

   FOR x := 1 TO nLenArray
      InArray[ X ]  := Padr( "   " + InArray[ X ], nPadLen )
      OutArray[ X ] := " " + OutArray[ X ]
   NEXT

   IF Len( aDefault ) > 0

      FOR EACH cItem IN aDefault

         if !lLib
            x := AScan( InArray, { | a | SubStr( a, 4, At(" ", alltrim(a) ) - 1 ) == cItem } )
         else
            x := AScan( InArray, { | a | alltrim(cItem) $ a } )
         endif

         IF x != 0

            InArray[ x ]  := Stuff( InArray[ x ] , 2, 1, IIF( lIsChecked, " ", cMarkChar ) )
            OutArray[ x ] := Stuff( OutArray[ x ], 1, 1, IIF( lIsChecked, " ", cMarkChar ) )
            nOnItem++

         ELSE

            cItem := SubStr( cItem, Rat( "\", cItem ) - 1 )

            if !lLib
               x := AScan( aTemp, { | a | SubStr( a, 4, At( " ", a ) - 1 ) == cItem } )
            else
               x := AScan( InArray, { | a | alltrim(cItem) $ a } )
            endif

            IF x != 0
               InArray[ x ]  := Stuff( InArray[ x ] , 2, 1, IIF( lIsChecked, " ", cMarkChar ) )
               OutArray[ x ] := Stuff( OutArray[ x ], 1, 1, IIF( lIsChecked, " ", cMarkChar ) )
               nOnItem++
            ENDIF

         ENDIF
      NEXT
   ENDIF

   Clear TypeAhead

   B--                               // aChoice bottom 1-less for border
   WHILE nChoice != 0

      nChoice := AChoice( T, L, B, R, InArray,, {|mode| PickArray_keys( mode, @nOnItem, @lAdd ) }, nChoice, 1 )

      IF nChoice > 0

         if lAllowAll

            if lAdd   // only if F5 was pressed

               For nChoice :=  1 to nLenArray

                 lIsChecked := Substr( InArray[ nChoice ], 2, 1 ) == cMarkChar

                 InArray[ nChoice ]  := Stuff( InArray[ nChoice ] , 2, 1, IIF( lIsChecked, " ", cMarkChar ) )
                 OutArray[ nChoice ] := Stuff( OutArray[ nChoice ], 1, 1, IIF( lIsChecked, " ", cMarkChar ) )

                 IF lIsChecked
                    nOnItem--
                 ELSE
                    nOnItem++
                 ENDIF
                 lAdd := .F.
               Next

            else

              lIsChecked := Substr( InArray[ nChoice ], 2, 1 ) == cMarkChar

              InArray[ nChoice ]  := Stuff( InArray[ nChoice ] , 2, 1, IIF( lIsChecked, " ", cMarkChar ) )
              OutArray[ nChoice ] := Stuff( OutArray[ nChoice ], 1, 1, IIF( lIsChecked, " ", cMarkChar ) )

              IF lIsChecked
                 nOnItem--
              ELSE
                 nOnItem++
              ENDIF

              nChoice++

            endif

         else

            lIsChecked := Substr( InArray[ nChoice ], 2, 1 ) == cMarkChar

            InArray[ nChoice ]  := Stuff( InArray[ nChoice ] , 2, 1, IIF( lIsChecked, " ", cMarkChar ) )
            OutArray[ nChoice ] := Stuff( OutArray[ nChoice ], 1, 1, IIF( lIsChecked, " ", cMarkChar ) )

            IF lIsChecked
               nOnItem--
            ELSE
               nOnItem++
            ENDIF

         endif

      ENDIF

   ENDDO

   FOR x := 1 TO nLenArray
      IF Left( OutArray[ X ], 1 ) == cMarkChar
         AAdd( aNewArray, Substr( OutArray[ X ], 2 ) )
      ENDIF
      InArray[ X ] := Substr( InArray[ X ], 4 )
   NEXT

   ASize( OutArray, Len( aNewArray ) )
   ACopy( aNewArray, OutArray )

   RestScreen(,,,,cOldScreen)
   SetColor( cOldColor )

   RETURN Len( aNewArray )

*-------------------------------------------------------------------
STATIC FUNCTION PickArray_Keys( MODE, nOnItem, lAdd )
*-------------------------------------------------------------------

   LOCAL RetVal := AC_CONT
   LOCAL TheKey := Lastkey()

   IF MODE == AC_HITTOP
      KEYBOARD Chr( K_CTRL_PGDN )

   ELSEIF MODE == AC_HITBOTTOM
      KEYBOARD Chr( K_CTRL_PGUP )

   ELSEIF MODE == AC_EXCEPT

      IF TheKey == K_SPACE // space bar to select/unselect
         RetVal := AC_SELECT
      ELSEIF TheKey == K_F5  // (select all itens)
         lAdd := !lAdd
         RetVal := AC_SELECT
      ELSEIF TheKey == K_ESC
         RetVal := AC_ABORT
      ELSEIF TheKey == K_ENTER .AND. nOnItem < 1
         RetVal := AC_ABORT
         KEYBOARD CHR( K_ENTER )
      ELSEIF TheKey == K_ENTER
         KEYBOARD CHR( K_DOWN )
         RetVal := AC_ABORT
      ENDIF

   ENDIF

   RETURN RetVal

*-------------------------------------------------------------------
* Former hbmutils.prg
*-------------------------------------------------------------------

*--------------------------------------------
FUNCTION GetSourceFiles( lSubDir, lGcc, cOs )
*--------------------------------------------

   LOCAL aDirs
   LOCAL aRet      := {}
   LOCAL lLinux    := AT( "LINUX", Upper( cOs ) ) > 0 .OR. lGcc
   LOCAL cDir      := IIF( ! lLinux, "\" + CURDIR() + "\", "/" + CURDIR() + "/" )
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
             nPadr := Max( AT(".prg", LOWER( aData[ y, 1 ] ) )+3 , nPadr )
             nPadr := Max( AT(".c",   LOWER( aData[ y, 1 ] ) )+1 , nPadr )
             nPadr := Max( AT(".cpp", LOWER( aData[ y, 1 ] ) )+3 , nPadr )
         NEXT

         FOR y := 1 TO nDataLen

            IF AT( ".prg", Lower( aData[ y, 1 ] ) ) > 0 .OR. AT( ".c", Lower( aData[ y, 1 ] ) ) > 0 .OR. AT( ".cpp", Lower( aData[ y, 1 ] ) ) > 0

               IF lSubDir

                  nLen := AT( " ", aData[ y, 1 ] ) + 1

                  AADD( aRet, STRTRAN( aStru[ nCounter ], cDir, "" ) +;
                        PadR(aData[ y,1 ] ,nPadr) + ;         // prg name
                        STR(aData[ y, 2 ] , 8 ) + "  " + ;    // prg size
                        DTOC(aData[ y, 3 ] ) + "  " + ;       // prg date
                        aData[ y, 4 ] )                       // prg time

               ELSEIF ! lSubDir .AND. AT( IIF( lLinux, "/", "\" ), STRTRAN( aStru[ nCounter ], cDir, "" ) ) == 0

                  AADD( aRet, PadR(aData[ y, 1 ],nPadr) + ;   // prg name
                        STR( aData[ y, 2 ], 8 ) + "  " + ;    // prg size
                        DTOC( aData[ y, 3 ] ) + "  " + ;      // prg date
                        aData[ y, 4 ] )                       // prg time

               ENDIF

            ENDIF

         NEXT

      ENDIF

   NEXT

   //     For nCounter := 1 To Len( aRet )
   FOR EACH cFile IN aRet

      xItem := SUBSTR( cFile, RAT( IIF( lLinux, "/", "\" ), cFile ) + 1 )
      nPos  := ASCAN( aStru, { | x | x := SUBSTR( x, RAT( IIF( lLinux, "/", "\" ), x ) + 1 ), LEFT( x, AT( ".", x ) ) == LEFT( xitem, AT( ".", xitem ) ) } )

      IF nPos > 0
         ADEL( aStru, nPos )
         ASIZE( aStru, LEN( aStru ) - 1 )
      ENDIF

   NEXT

   FOR EACH cFile IN aStru

      HB_FNAMESPLIT( LEFT( cFile, AT( " ", cFile ) - 1 ), @cPath, @cItem, @cExt, @cDrive )

      IF ( cExt == ".C" ) .OR. ( cExt == ".c" ) .OR. ( cExt == ".CPP" ) .OR. ( cExt == ".cpp" )
         AADD( aRet, cFile )
      ENDIF

   NEXT

   RETURN aRet

*----------------------------------------
STATIC FUNCTION GetDirs( cPattern, lGcc )
*----------------------------------------

   LOCAL aDir   := {}
   LOCAL lLinux := AT( "LINUX", Upper( OS() ) ) > 0 .OR. lGcc

   AEVAL( DIRECTORY( cPattern + IIF( lLinux, "*", "*.*" ), "D" ), ;
          { | xItem | IIF( xItem[ F_ATTR ] == "D" .AND. ;
          ( !( xItem[ F_NAME ] == "." ) .AND. !( xItem[ F_NAME ] == ".." ) ), ;
          AADD( aDir, cPattern + xItem[ F_NAME ] + IIF( lLinux, "/", "\" ) ), ;
          ) } )

   RETURN aDir

*-----------------------
FUNCTION GetHarbourDir()
*-----------------------

   LOCAL lUnix    := "LINUX" $ Upper( OS() ) .OR. ;
                     "UNIX" $ Upper( OS() ) .OR. ;
                     "HP-UX" $ Upper( OS() ) .OR. ;
                     "DARWIN" $ Upper( OS() )

   LOCAL cBar     := iif( lUnix, "/" , "\" )
   LOCAL HBSTRG   := iif( lUnix, "harbour", "harbour.exe" )
   LOCAL cPathUni := GetEnv( "PATH_HARBOUR" )
   LOCAL aEnv     := hb_ATokens( GetEnv( "PATH" ), iif( lUnix, ":", ";" ) )
   LOCAL cCurEnv

   AAdd( aEnv, "." + cBar )

   IF !Empty( cPathUni )
      AAdd( aEnv, cPathUni )
   ENDIF

   FOR EACH cCurEnv IN aEnv

      IF File( cCurEnv + cBar + hbstrg )
         RETURN Left( cCurEnv, RAt( cBar, cCurEnv ) - 1 )
      ENDIF
   NEXT

   RETURN ""

*-------------------
FUNCTION GetBccDir()
*-------------------

   LOCAL cPath   := ""
   LOCAL cEnv    := GETE( "PATH" )
   LOCAL aEnv    := HB_ATokens( cEnv, ";" )
   LOCAL cCurEnv := ""

   FOR EACH cCurEnv IN aEnv

      IF FILE( cCurEnv + "\bcc32.exe" )
         cPath := cCurEnv
         cPath := LEFT( cPath, RAT( "\", cPath ) - 1 )
         EXIT
      ENDIF

   NEXT

   RETURN cPath

*-------------------
FUNCTION GetVccDir()
*-------------------

   LOCAL cPath   := ""
   LOCAL cEnv    := GETE( "PATH" )
   LOCAL aEnv    := HB_ATokens( cEnv, ";" )
   LOCAL cCurEnv := ""

   FOR EACH cCurEnv IN aEnv

      IF FILE( cCurEnv + "\cl.exe" )
         cPath := cCurEnv
         cPath := LEFT( cPath, RAT( "\", cPath ) - 1 )
         EXIT
      ENDIF

   NEXT

   RETURN cPath

*--------------------
FUNCTION GetPoccDir()
*--------------------

   LOCAL cPath   := ""
   LOCAL cEnv    := GETE( "PATH" )
   LOCAL aEnv    := HB_ATokens( cEnv, ";" )
   LOCAL cCurEnv := ""

   FOR EACH cCurEnv IN aEnv

      IF FILE( cCurEnv + "\pocc.exe" )
         cPath := cCurEnv
         cPath := LEFT( cPath, RAT( "\", cPath ) - 1 )
         EXIT
      ENDIF

   NEXT

   RETURN cPath

*----------------------------
FUNCTION Exten( cExt, nType )
*----------------------------

   LOCAL aExt    := { "C", "c", "CPP", "cpp" }
   LOCAL nPos
   LOCAL cTemp   := ""

   nPos := ASCAN( aExt, { | a | a == cExt } )
   IF nPos > 0

      SWITCH  nType
      CASE 1
         cTemp := STRTRAN( cExt, aExt[ nPos ], "o" )
         EXIT

      CASE 2
         cTemp := STRTRAN( cExt, aExt[ nPos ], "obj" )
         EXIT

      END

   ENDIF

   RETURN cTemp

*----------------------------
FUNCTION GetSourceDirMacros()
*----------------------------

   LOCAL aDirs
   LOCAL lLinux    := AT( "LINUX", Upper( OS() ) ) > 0
   LOCAL cDir      := IIF( lLinux, "/" + CURDIR() + "/", "\" + CURDIR() + "\" )
   LOCAL aStru     := { cDir }

   LOCAL nCounter  := 0
   LOCAL aMacros   := {}

   WHILE ++ nCounter <= LEN( aStru )

      IF ! EMPTY( aDirs := GetDirs( aStru[ nCounter ], lLinux ) )                // There are elements!
         AEVAL( aDirs, { | xItem | AADD( aStru, xItem ) } )
      ENDIF

   ENDDO

   FOR nCounter := 1 TO LEN( aStru )
      AADD( aMacros, { "SRC" + STRZERO( nCounter, 2, 0 ), STRTRAN( aStru[ nCounter ], cDir, "" ), .f. } )
   NEXT

   RETURN aMacros

*------------------------------------
FUNCTION HbMake_FileDate( cFileName )
*------------------------------------

   LOCAL aFiles := DIRECTORY( cFileName )

   RETURN IIF( LEN( aFiles ) == 1, aFiles[ 1, 3 ], CTOD( "" ) )

*------------------------------------
FUNCTION HbMake_FileTime( cFileName )
*------------------------------------

   LOCAL aFiles := DIRECTORY( cFileName )

   RETURN IIF( LEN( aFiles ) == 1, aFiles[ 1, 4 ], "" )

*------------------------------
FUNCTION TD2JUL( CTIME, DDATE )
*------------------------------
   RETURN DDATE - CTOD( "01/01/1900" ) + ( PRB_INT( TTOS( CTIME ) / 100000,, 5 ) )

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

   dotat := AT( ".", somestring )

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
      SOMESTRING := "-" + SOMESTRING
   ENDIF

   RETURN VAL( SOMESTRING )

*---------------------------
FUNCTION Exte( cExt, nType )
*---------------------------

   LOCAL aExt  := { "prg", "prG", "pRg", "Prg", "PRg", "PrG", "PRG" }
   LOCAL nPos
   LOCAL cTemp := ""

   nPos := ASCAN( aExt, { | a | a == cExt } )
   IF nPos > 0
      IF nType == 1
         cTemp := STRTRAN( cExt, aExt[ nPos ], "c" )
      ELSEIF nType == 2
         cTemp := STRTRAN( cExt, aExt[ nPos ], "obj" )
      ELSEIF nType == 3
         cTemp := STRTRAN( cExt, aExt[ nPos ], "o" )
      ENDIF

   ENDIF

   RETURN cTemp

*-----------------------------------------------
PROCEDURE Attention( cString, nLineNum, cColor )
*-----------------------------------------------

   LOCAL cOldColor
   LOCAL nColPos

   DEFAULT NLINENUM TO 24
   DEFAULT cColor TO "GR+/R"

   cOldColor := SETCOLOR( cColor )

   CSTRING := "  " + ALLTRIM( CSTRING ) + "  "

   nColPos := MAX( INT( ( MAXCOL() - LEN( cString ) ) / 2 ), 0 )
   DEVPOS( NLINENUM, nColPos )

   DEVOUT( CSTRING )

   SETCOLOR( cOldColor )

   RETURN

*--------------------------------------
FUNCTION GetInstaledLibs( clibs, lGcc )
*--------------------------------------

   LOCAL cSuffix := IIF( lGCC, ".a", ".lib" )
   LOCAL aReturnLibs := {}
   LOCAL aLibs       := DIRECTORY( clibs )
   LOCAL cItem
   LOCAL nCount
   LOCAL aDefLib := {}

   aadd(aDefLib,"ace32"+ cSuffix)
   aadd(aDefLib,"hbcpage"+ cSuffix)
   aadd(aDefLib,"hbcommon"+ cSuffix)
   aadd(aDefLib,"hbct"+cSuffix)
   aadd(aDefLib,"rdddbt"+ cSuffix)
   aadd(aDefLib,"rddcdx"+ cSuffix)
   aadd(aDefLib,"rddfpt"+ cSuffix)
   aadd(aDefLib,"rddntx"+ cSuffix)
   aadd(aDefLib,"hbdebug"+ cSuffix)
   aadd(aDefLib,"gtcgi"+ cSuffix)
   aadd(aDefLib,"gtdos"+ cSuffix)
   aadd(aDefLib,"gtpca"+ cSuffix)
   aadd(aDefLib,"gtsln"+ cSuffix)
   aadd(aDefLib,"gtstd"+ cSuffix)
   aadd(aDefLib,"gttrm"+ cSuffix)
   aadd(aDefLib,"gtwin"+ cSuffix)
   aadd(aDefLib,"gtwvt"+ cSuffix)
   aadd(aDefLib,"hbodbc"+ cSuffix)
   aadd(aDefLib,"hbpgsql"+ cSuffix)
   aadd(aDefLib,"hblang"+ cSuffix)
   aadd(aDefLib,"hbmisc"+ cSuffix)
   aadd(aDefLib,"hbnf"+ cSuffix)
   aadd(aDefLib,"hbgt"+ cSuffix)
   aadd(aDefLib,"hbmysql"+ cSuffix)
   aadd(aDefLib,"hbmacro"+ cSuffix)
   aadd(aDefLib,"hbnulrdd"+ cSuffix)
   aadd(aDefLib,"hbpp"+ cSuffix)
   aadd(aDefLib,"hbrdd"+ cSuffix)
   aadd(aDefLib,"rddads"+ cSuffix)
   aadd(aDefLib,"hbrtl"+ cSuffix)
   aadd(aDefLib,"hbclipsm"+ cSuffix)
   aadd(aDefLib,"hbtip"+cSuffix)
   aadd(aDefLib,"hbw32"+cSuffix)
   aadd(aDefLib,"hbvm"+ cSuffix)
   aadd(aDefLib,"hbziparc"+ cSuffix)

   IF lGcc
      AEval( aLibs, { | x, y | cItem := x[1], IIF( Left( cItem, 3 ) == "lib", aLibs[ y, 1 ] := SubStr( cItem, 4 ), ) } )
   ENDIF

   FOR nCount := 1 TO LEN( aLibs )

      IF AScan( aDefLib, { | a | hb_FileMatch( a, aLibs[ nCount, 1 ] ) } ) == 0
         AAdd( aReturnLibs, aLibs[ nCount, 1 ] )
      ENDIF

   NEXT

   RETURN aReturnLibs

*-----------------------------
FUNCTION GetLibs( lGcc, cDir )
*-----------------------------

   LOCAL lLinux        := AT( "LINUX", Upper( OS() ) ) > 0
   LOCAL cEnv          := GETENV( "HB_LIB_INSTALL" )
   LOCAL aInstaledLibs := GetInstaledLibs( IIF( ! lLinux, IIF( ! lGcc, cDir + "\*.lib", cDir + "\*.a" ),  "/usr/lib/harbour/*.a" ), lGcc )
   LOCAL cExt := iif(lGcc,".a",".lib")

                            /* 1234567890123456789 */
   LOCAL aLibsDesc     := { { "Harbour hbmisc      lib - hbmisc" + cExt  , "hbmisc" + cExt },;
                            { "Harbour NanFor Lib  lib - hbnf" + cExt    , "hbnf" + cExt },;
                            { "Harbour GT Lib      lib - hbgt"+cExt      , "hbgt" + cExt },;
                            { "Harbour ZipArchive  lib - hbziparc"+cExt  , "hbziparc" + cExt },;
                            { "Harbour ole (old)   lib - hbole"+ cExt    , "hbole" + cExt + " ole2" + cExt },;
                            { "Harbour MySQL       lib - hbmysql" + cExt , "hbmysql" + cExt },;
                            { "Harbour PostgreSQL  lib - hbpgsql"+cExt   , "hbpgsql" + cExt },;
                            { "Harbour samples     lib - hbclipsm"+cExt  , "hbclipsm" + cExt }  }

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

   DEFAULT cDelimiter TO ","

   DO WHILE ( nPos := AT( cDelimiter, cList ) ) != 0
      AADD( aList, ALLTRIM( SUBSTR( cList, 1, nPos - 1 ) ) )                    // Add a new element
      cList := SUBSTR( cList, nPos + 1 )

   ENDDO
   AADD( aList, ALLTRIM( cList ) )      // Add final element

   RETURN aList        // Return the array

*--------------------
FUNCTION CreateLink()
*--------------------

    LOCAL nHandle := FCreate( "hbtemp.c" )

    FWrite( nHandle, ;
       '#include "hbapi.h"' + s_cEOL +;
       "extern HB_FUNC( HB_GT_CRS );" + s_cEOL +;
       "void hb_lnk_ForceLink_build( void )" + s_cEOL +;
       "{" + s_cEOL +;
       "   HB_FUNCNAME( HB_GT_CRS )();" + s_cEOL +;
       "}" + s_cEOL )
    FClose( nHandle )

    RETURN NIL

*-------------------------------------------------------------
FUNCTION CmdLineParam( cFile, cCmdParams )
*  Params must be by reference not value
*-------------------------------------------------------------
   LOCAL aDef    := {}
   LOCAL cTemp
   LOCAL cFlag
   LOCAL nParam  := 0                  /* Steps thourgh params */
   LOCAL xReturn := PGM_OK             /* Default to OK        */

   /*  Set the statics we know about. Some may be
    *  reset after command line is parsed
    */

   s_aEOL  := { CHR(13) + CHR(10), CHR(10) }
   s_cEOL  := hb_OSNewLine()
   s_nLang := GETUSERLANG()            /* In hbmlang.c         */

   cTemp    := UPPER( OS() )
   s_lOS2   := ( "OS/2"    $ cTemp )
   s_lLinux := ( "LINUX"   $ cTemp )
   s_lWin32 := ( "WINDOWS" $ cTemp )
   s_lUnix  := ( "UNIX"    $ cTemp .OR. ;
                 "HP-UX"   $ cTemp        )

   DO CASE
   CASE s_lOS2                         /* OS/2                 */

     s_lOS2    := s_lGcc := .T.
     s_cEditor := "mcedit"

   CASE s_lLinux                       /* Linux                */

     s_lLinux  := s_lGcc := .T.
     s_cEditor := "mcedit"

   CASE s_lWin32                       /* Windows              */

     s_lWin32  := s_lBcc := .T.
     s_cEditor := "edit"

   CASE s_lUnix                        /* UXIX                 */

     s_lUnix   := s_lGcc   := .T.
     s_cEditor := "mcedit"

   OTHERWISE /*   Unknown OS - setup for windows as default    */

     s_lWin32  := s_lBcc := .T.
     s_cEditor := "edit"

   END CASE

   /*    Get & Set Command Line Parameters
    */

   cCmdParams := ""                    /* Starts Empty Params  */
   WHILE ! EMPTY( (cFlag:= hb_argv( ++nParam ) ) )

     /*  Change '/' flags to '-'
      */
     cFlag := UPPER( STRTRAN( cFlag, "/", "-" ) )

     /*  Capture FileName & Remove trailing 'X', -EC changed to -E
      */
     IF LEFT( cFlag, 1 ) != '-'
       cFile  := hb_argv( nParam )
       LOOP
     ELSE
       cFlag := STRTRAN( cFlag, "X" , ""  )
       cFlag := STRTRAN( cFlag, "EC", "E" )
     ENDIF

     DO CASE
     CASE cFlag == "-BC"               /* Borland Bcc Default  */
       s_lBcc := .T.
       s_lGcc := s_lMSVcc := s_lPocc := .F.

     CASE cFlag == "-GC"               /* GNU Gcc Default      */
       s_lGcc := .T.
       s_lBcc := s_lMSVcc := s_lPocc := .F.

     CASE cFlag == "-VC"               /* MS Visual Cc Default */
       s_lMSVcc := .T.
       s_lBcc   := s_lGcc := s_lPocc := .F.

     CASE cFlag == "-PC"               /* Pelles Cc Default    */
       s_lPocc := .T.
       s_lBcc  := s_lGcc := s_lMSVcc := .F.

     CASE cFlag == "-F"                /* Force Rebuild All    */
       s_lForce := .T.

     CASE cFlag == "-E"                /* Programs Edit Mode   */
       s_lEdit := .T.

     CASE cFlag == "-EL"               /* Library Edit Mode    */
       s_lLibrary      := .T.
       s_lEdit         := .T.

     CASE cFlag == "-I"                /* Ignore Errors        */
       s_lIgnoreErrors := .T.

     CASE cFlag == "-R"                /* Recursive Dir Search */
        s_lRecursive := .T.

     CASE cFlag == "-S"                /* Show Make Details    */
       s_lPrint := .T.

     CASE "-L" == LEFT( cFlag, 2 )     /* Language Flag        */

       cFlag := SUBSTR( cFlag, 3, 2 )
       DO CASE
         CASE cFlag == "PT"            /* portuguese brazilian */
           s_nLang := LANG_PT

         CASE cFlag == "ES"            /* Spanish              */
           s_nLang := LANG_ES

         OTHERWISE                     /* English  default     */
           s_nLang := LANG_EN

       END CASE

     CASE cFlag == "-?" .OR. ;         /* Show Help            */
          cFlag == "-H"

       xReturn := PGM_ERR              /* Flag to show help()  */

     CASE LEFT( cFlag, 2 ) == "-D"     /* Define a macro       */
       cFlag := "-D" + Strtran( cFlag, "-D", ";" )
       cFlag := Strtran( cFlag, "-D;", "-D" )
       aDef   := ListAsArray2( Alltrim( Substr( cFlag, 3 ) ), ";" )
       AEval( aDef, { | xDef | IIF( At( "=", xDef ) > 0, GetParaDefines( xDef ), ) } )

     OTHERWISE                         /* Unknown flag set '-H' flag */

       xReturn := PGM_ERR
       ALERT( "Unknown Cmd-Line Parameter: " + cFlag +;
              "Setting Help flag."  )
     END CASE

     cCmdParams += cFlag

   END WHILE

   DO CASE
   CASE EMPTY( cFile )
      xReturn := PGM_ERR

   CASE nParam == 2                    /* Was Incremed 1-past actual number */
       cCmdParams := "-F"              /* -F probably not needed - check it */
       s_lForce   := .T.
   END CASE

   RETURN xReturn
