/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HBDOC document Extractor
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/*
 * File......: HBDOC.PRG
 * Author....: Luiz Rafael Culik
 * Date......: $Date$
 * Revision..: $Revision$
 * Log file..: $Logfile:     $
 *
 *
 * Modification history:
 * ---------------------
 *
 *    V1.00
 *    2000/01/05 Initial Version. Based on Leo Letendre FT_DOC
 *
 *    2000/01/06 Added the ProccAlso Function
 *
 *    2000/01/08 Fixed the Line between the Title and the Text
 *               Functions Description is now in font Arial size 12
 *    V1.01
 *    2000/01/09 Added RTF Source output Format
 *
 *    2000/01/11 Remove the code to add the Author name and Source file
 *               Name to the output file.
 *
 *    V1.02
 *    2000/01/12 Added suport for WWW output Format
 *               Striped out the "<" and ">" delimeter for WWW outPut,
 *               since the "<>" are HTML Command delimeters
 *               Output files names are in lower case to Linux Compatibility
 *
 *    2000/01/13 Added the link for the  HARBOUR GPL LICENSE
 *    2000/01/14 Fixed a bug on generating the HTML file
 *    2000/01/15 Strip out the  for Norton guides controls characters
 *               when generating HTML and RTF output
 *
 *    V1.03
 *    2000/01/16 Added Code to generate Norton Guide Source code
 *
 *    V1.04
 *    2000/01/17 Added Code to generate TROFF files
 *    2000/01/18 Added Cleanup procedure when Generating Norton Guide
 *
 *    V1.05
 *    2000/01/22 Added Code to generate OS2 IPF files
 *
 *    V1.06
 *    2000/01/25 Fixed some error that was not generating a valid RTF File
 *    Removed Call TO HB_OEMTOANSI() on the rountines to generate the .Ngi
 *    and Rtf files.
 *    Added support to generate the Docs from .Txt files, See doc\Subcodes.txt
 *    for header file.
 *
 *    V1.07
 *    Added back the "<" and ">" symbols
 *    Fixed the links on the Harbour.htm file           
 *    Fixed the help text when hbdoc is called with out any parameter
 */

#ifdef __HARBOUR__
#define NANFOR
#endif

#include "directry.ch"
#include "fileio.ch"
#include "inkey.ch"
#include 'hbdocdef.ch'
//  output lines on the screen

#define INFILELINE   10
#define MODULELINE   12
#define LINELINE     14
#define ERRORLINE    20
#define LONGLINE     78
#define LONGONELINE  66

//  The delimiter
MEMVAR aDirList
MEMVAR aDocInfo
MEMVAR aLinkInfo
MEMVAR aAuthorList
MEMVAR lAscii
MEMVAR lContinuous
MEMVAR lAuthor
MEMVAR lRtf
MEMVAR lNgi
MEMVAR lOs2
MEMVAR lWww
MEMVAR lChm
MEMVAR lNorton
MEMVAR aWWW
MEMVAR lTroff
MEMVAR aResult
STATIC cTitle:=''

/*
*/

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function MAIN()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION MAIN( cFlags, cLinkName, cAtFile )

   //  LOCAL variables:
   // NG/EH input

   LOCAL aExtensions := { "*.ch", "*.prg", "*.c", "*.asm", "*.txt" }
   LOCAL i
   Local cLast
   LOCAL nItem
   LOCAL nHpj
   LOCAL cItem:=''

   LOCAL cCompiler     // Compiler type
   LOCAL oHtm
   LOCAL oHtm1
   LOCAL nPos
   LOCAL ppp
   LOCAL aMetaContents:={}
   Local aTemp:={}
   LOCAL lAdded:=.f.
   PUBLIC theHandle
   PUBLIC aDirList
   PUBLIC aDocInfo    := {}
   PUBLIC aLinkInfo   := {}
   PUBLIC aAuthorList := {}
   PUBLIC lAscii      := .F.               // Create ascii output instead of NG/EH input
   PUBLIC lContinuous := .F.               // Create continuous ascii output instead of
   PUBLIC lAuthor     := .T.               // Include author in output of ascii output
   PUBLIC lRtf        := .F.
   PUBLIC lNgi        := .F.
   PUBLIC lOs2        := .F.
   PUBLIC lWww        := .F.
   PUBLIC lChm        := .F.
   PUBLIC lNorton     := .F.
   PUBLIC aWWW        := {}
   PUBLIC aResult:={}
   PUBLIC lTroff      := .f.

   //  The following variables are used to allow one to change the delimiter
   //  around the keywords.

   //
   //  Entry Point
   //

   //  Delete log file if present

   IF FILE( "hbdocerr.log" )
      DELETE FILE "hbdocerr.log"
   ENDIF

   //  See if flag is there

   IF .NOT. EMPTY( cFlags )
      IF LEFT( cFlags, 1 ) == "-" .OR. LEFT( cFlags, 1 ) == "/"
         IF ( cFlags := UPPER( RIGHT( cFlags, 3 ) ) ) == "TXT"
            lAscii      := .T.
            lContinuous := .F.
         ELSEIF cFlags = "HPC"
            lNorton := .T.
         ELSEIF cFlags = "NGI"
            lNgi := .T.
         ELSEIF cFlags = "OS2"
            lOs2 := .T.
         ELSEIF cFlags = "RTF"
            lRtf := .T.
         ELSEIF cFlags = "HTM"
            lWww := .T.
         ELSEIF cFlags = "CHM"
            lChm := .T.
         ELSEIF cFlags = "TRF"
            lTroff := .t.
         ELSEIF cFlags = "DOC"
            lAscii      := .T.
            lContinuous := .T.
            lAuthor     := .F.
         ENDIF
      ELSE
         cAtFIle   := cLinkName
         cLinkName := cFlags
      ENDIF
   ENDIF

   //  Get the linkfile name and get the info in it

   IF cLinkName = NIL
      ?? "Harbour Doc Extractor"
      ? "Copyright 1999-2000, http://www.harbour-project.org"
      ? ""
      ? "Syntax:  hbdoc [options] <linkname> [<ifile>]"
      ? ""
      ? "Options:  /txt Create an ASCII file instead of a Norton Guide"
      ? "          /con Create an ASCII file without formfeeds"
      ? "          /hpc Helpc source file"
      ? "          /ngi Adds the -NG switch to EHC command for compile for"
      ? "               DOS/Windows/Linux."
      ? "          /rtf Winhelp source code for Windows"
      ? "          /os2 OS/2 help source code For OS/2"
      ? "          /htm Generate HTML output"
      ? "          /chm Generate HTML source files for Windows .CHM Help files"
      ? "          /trf Gerenate Linux TROFF code"
      ? "          /doc Create continuous ASCII file w/o author information"
      ? " "
      ? "Notes:  - Only one option can be specified at a time."
      ? "        - <linkname> is the name of the Norton Guide Link file."
      ? "        - <iFile> is a file containing a list of files to process"
      ? "          otherwise *.prg, *.c, *.asm, *.ch and *.txt are used."
      RETURN NIL
   ENDIF

   //  check to see if input files are present
   IF .NOT. FILE( cLinkName )
      ? "Link file Not Found:", cLinkName
      RETURN NIL
   ENDIF

   IF .NOT. EMPTY( cAtFIle ) .AND. .NOT. FILE( cAtFile )
      ? "Indirect file Not Found:", cAtFile
      RETURN NIL
   ENDIF

   CLEAR SCREEN
   SET CURSOR OFF

   //  See if ngi subdirectory is present
   IF lNorton
      IF EMPTY( DIRECTORY( "hdf.*", "D" ) )
         FT_MKDIR( "hdf" )
      ENDIF
   ELSEIF lRtf
      IF EMPTY( DIRECTORY( "rtf.*", "D" ) )
         FT_MKDIR( "rtf" )
      ENDIF
      ReadLinkFile( cLinkName )
   ELSEIF lWww
      IF EMPTY( DIRECTORY( "htm.*", "D" ) )
         FT_MKDIR( "htm" )
      ENDIF
   ELSEIF lChm
      IF EMPTY( DIRECTORY( "chm.*", "D" ) )
         FT_MKDIR( "chm" )
      ENDIF

   ELSEIF lNgi
      IF EMPTY( DIRECTORY( "ngi.*", "D" ) )
         FT_MKDIR( "ngi" )
      ENDIF
         ReadLinkFile( cLinkName )
   ELSEIF lTroff
      IF EMPTY( DIRECTORY( "tr.*", "D" ) )
         FT_MKDIR( "tr" )
      ENDIF
   ELSEIF lOs2
      IF EMPTY( DIRECTORY( "ipf.*", "D" ) )
         FT_MKDIR( "ipf" )
      ENDIF
   ENDIF
   if lNgi .or. lRtf
   cCompiler := fill_Link_info( cLinkName )
   endif        

   IF cAtFile = NIL                     // use all files in directory

      //  Loop through each of the types of files

      FOR i := 1 TO LEN( aExtensions )

         //  Get the list of

         //                aDirList=DIRECTORY(&cDir+aExtensions[i])
         aDirList := DIRECTORY( aExtensions[ i ] )

         //  If there are any files then process them

         IF LEN( aDirList ) > 0

            IF lAscii
               ASCIIFiles()
            ELSEIF lNorton
               ProcessFiles()
            ELSEIF lRtf
               ProcessRtf()
            ELSEIF lWww
               ProcessWww()
            ELSEIF lChm
               ProcessChm()
            ELSEIF lNgi
               ProcessiNg()
            ELSEIF lTroff
               ProcessTroff()
            ELSEIF lOs2
               ProcessOs2()
            ENDIF
         ENDIF
      NEXT
   ELSE
      //  an indirect file was given so read it and use it

      aDirList := ReadAtFile( cAtFile )

      //  If there are any files then process them

      IF LEN( aDirList ) > 0

         IF lAscii
            ASCIIFILES()
         ELSEIF lNorton
            ProcessFiles()
         ELSEIF lRtf
            ProcessRtf()
         ELSEIF lWww
            ProcessWww()
         ELSEIF lChm
            ProcessChm()
         ELSEIF lNgi
            ProcessiNg()
         ELSEIF lTroff
            ProcessTroff()
         ELSEIF lOs2
            ProcessOs2()
         ENDIF
      ENDIF

   ENDIF

   //  Now build text files for norton compiler based upon link file
   //  first sort based upon category and filename. Not Fast but easy.

   @ INFILELINE,  0 CLEAR TO INFILELINE, MAXCOL()
   @ MODULELINE,  0 CLEAR TO MODULELINE, MAXCOL()
   @ LINELINE,  0 CLEAR TO LINELINE, MAXCOL()
   @ INFILELINE, 30 SAY "Sorting input files"         

   ASORT( aDocInfo,,, { | a, b | UPPER( a[ 1 ] + " " + a[ 2 ] ) < UPPER( b[ 1 ] + " " + b[ 2 ] ) } )

   //  Now actually build the info

   @ INFILELINE,  0 CLEAR TO INFILELINE, MAXCOL()
   IF lnorton
      @ INFILELINE, 30 SAY "Assembling " + IIF( lAscii, "documentation", "HelpC" ) ;         
              + " input files"
   ELSEIF lRTF
      @ INFILELINE, 30 SAY "Assembling " + IIF( lAscii, "documentation", "WINHELP" ) ;         
              + " input files"
   ELSEIF lWww .or. lChm
      @ INFILELINE, 30 SAY "Assembling " + IIF( lAscii, "documentation", "Html" ) ;         
              + " input files"
   ELSEIF lNgi
      @ INFILELINE, 30 SAY "Assembling " + IIF( lAscii, "documentation", "NG" ) ;         
              + " input files"
   ELSEIF lTroff
      @ INFILELINE, 30 SAY "Assembling " + IIF( lAscii, "documentation", "TROFF" ) ;         
              + " input files"

   ENDIF

   IF FILE( "assembl.bat" )
      DELETE FILE "assembl.bat"
   ENDIF
   SET ALTERNATE TO "assembl.bat"
   SET ALTERNATE ON
   SET CONSOLE OFF

   ? "@Echo OFF"
   ? "ECHO Assembling input files"

   IF lNorton
      FOR i := 1 TO LEN( aDocInfo )

         //  Find match

         nItem := ASCAN( aLinkInfo, { | a | UPPER( ALLTRIM( a[ 1 ] ) ) == UPPER( ALLTRIM( aDocInfo[ i, 1 ] ) ) } )
         IF nItem > 0

            IF i = 1 .OR. .NOT. ( ALLTRIM( aDocInfo[ i - 1, 1 ] ) == ALLTRIM( aDocInfo[ i, 1 ] ) )
               //  Make the first copy
               ? "ECHO Creating", aLinkinfo[ nItem, 2 ]
               ? "COPY hdf\" + ALLTRIM( aDocInfo[ i, 4 ] ) + " HarDoc.hdf  > NUL"

            ELSE
               //  This may be slow but I don't have to worry about line length
               ? "TYPE hdf\" + ALLTRIM( aDocInfo[ i, 4 ] ) + " >> HarDoc.hdf "
            ENDIF
            aLinkInfo[ nItem, 3 ] = .T.
         ELSE
            //  Write the error message
            SET ALTERNATE TO
            SET ALTERNATE OFF
            SET CONSOLE ON
            WRITE_ERROR( "Category not found: " + aDocInfo[ i, 1 ],,,, aDocInfo[ i, 4 ] )
            @ ERRORLINE,  0 CLEAR TO ERRORLINE, MAXCOL()
            @ ERRORLINE, 20 SAY "Category not found: " + aDocInfo[ i, 1 ] + " in " + aDocInfo[ i, 4 ]         
            SET ALTERNATE TO "assembl.bat" ADDITIVE
            SET ALTERNATE ON
            SET CONSOLE OFF
         ENDIF

      NEXT
   ELSEIF lRtf
      nHpj := FCREATE( 'harbour.hpj' )
      FWRITE( nHpj, '[OPTIONS]' + CRLF )
      FWRITE( nHpj, 'HCW=1' + CRLF )
      FWRITE( nHpj, 'COMPRESS=60 Hall Zeck' + CRLF )
      FWRITE( nHpj, 'LCID=0x416 0x0 0x0 ;Portugu阺 (brasileiro)' + CRLF )
      FWRITE( nHpj, 'REPORT=Yes' + CRLF )
      FWRITE( nHpj, 'CONTENTS=IDH_OVERVIEW' + CRLF )
      FWRITE( nHpj, 'TITLE='+cTitle + CRLF )
      FWRITE( nHpj, 'COPYRIGHT=Harbour (C) http://www.harbour-project.org' + CRLF )
      FWRITE( nHpj, 'HLP=.\'+ lower(substr(cLinkName,1,AT(".",cLinkName)-1)) +".hlp"+ CRLF )
      FWRITE( nHpj, 'ROOT=\' + CURDIR() + "\RTF" + CRLF )
      FWRITE( nHpj, 'CNT=.\'+ lower(substr(cLinkName,1,AT(".",cLinkName)-1)) +".cnt"+ CRLF )
      FWRITE( nHpj, '[FILES]' + CRLF )
      FWRITE( nHpj, "harbour.rtf" + CRLF )
      FWRITE( nHpj, '[CONFIG]' + CRLF + 'contents()' + CRLF + 'prev()' + CRLF + 'next()' + CRLF + 'BrowseButtons()' + CRLF )
      FWRITE( nHpj, '[WINDOWS]' + CRLF + 'Commands="Harbour Commands",(653,102,360,600),20736,(r14876671),(r12632256),f2' + CRLF +'API="Harbour Commands",(653,102,360,600),20736,(r14876671),(r12632256),f2' + CRLF +       'Error="Harbour Run Time Errors",(653,102,360,600),20736,(r14876671),(r12632256),f2' + CRLF + 'Tools="Harbour Tools",(653,102,360,600),20736,(r14876671),(r12632256),f2' + CRLF + 'Class="Harbour OOP Commands",(653,102,360,600),20736,(r14876671),(r12632256),f2' + CRLF + 'Funca="Harbour Run Time Functions A-M",(653,102,360,600),20736,(r14876671),(r12632256),f2' + CRLF + 'Funcn="Harbour Run Time Functions N-_",(653,102,360,600),20736,(r14876671),(r12632256),f2' + CRLF + 'Main="HARBOUR",(117,100,894,873),60672,(r14876671),(r12632256),f2' + CRLF )
      FCLOSE( nHpj )
      nHpj := FCREATE( lower(substr(cLinkName,1,AT(".",cLinkName)-1)) +".cnt"  )
      FWRITE( nHpj, ':Base '+ lower(substr(cLinkName,1,AT(".",cLinkName)-1)) +".hlp"+ CRLF )
      FWRITE( nHpj, ':Title '+cTitle+CRLF)
      FWRITE( nHpj, ':Index '+lower(substr(cLinkName,1,AT(".",cLinkName)-1)) +'='+lower(substr(cLinkName,1,AT(".",cLinkName)-1)) +".hlp"+ CRLF )
      FWRITE( nHpj, '1 Harbour'+CRLF)
      asort(aWww,,,{|x,y| x[3]+x[1]<y[3]+y[1]})
      for ppp:=1 to len(aWww)
          if aWww[ppp,3]=='Document'
             fWrite( nHpj, '2 '+aWww[ppp,1]+"="+aWww[ppp,2]+">Main"+CRLF)
          endif
      Next
      asort(aWww,,,{|x,y| x[3]+x[1]<y[3]+y[1]})
      FWRITE( nHpj, '1 Harbour Run Time Error'+CRLF)
      for ppp:=1 to len(aWww)
          if aWww[ppp,3]=='Run Time Errors'
             fWrite( nHpj, '2 '+aWww[ppp,1]+"="+aWww[ppp,2]+">Error"+CRLF)
          endif
      Next
      FWRITE( nHpj, '1 Harbour Runtime functions and Commands by Name'+CRLF)
      asort(aWww,,,{|x,y| x[1]<y[1]})
      for ppp:=1 to len(aWww)
          if aWww[ppp,3]<>'Run Time Errors' .and. aWww[ppp,3] <>"Document"  .and. aWww[ppp,3] <>"The garbage collector"  .and. aWww[ppp,3] <>"OOP Command" .and. aWww[ppp,3] <>"Command"  .and. aWww[ppp,3] <>"The idle states"
             fWrite( nHpj, '2 '+aWww[ppp,1]+"="+aWww[ppp,2]+">Funca"+CRLF)
          endif
      Next

      FWRITE( nHpj, '1 Harbour Runtime functions Category'+CRLF)
      asort(aWww,,,{|x,y| x[3]<y[3]})
        SET CONSOLE ON
      nItem := len(aResult)
      asort(aResult,,,{|x,y| x<y})
      for ppp:=1 to nItem

        cLast:=GetNextContent(ppp)
        if cLast<>'Run Time Errors' .and. cLast <>"Document"  .and. cLast <>"The garbage collector"  .and. cLast <>"OOP Command" .and. cLast <>"Command"  .and. cLast <>"The idle states"
            WriteContentFile(aWww,cLast,nHpj)
        endif
      Next

      FWRITE( nHpj, '1 Harbour Commands'+CRLF)
      for ppp:=1 to len(aWww)
          if aWww[ppp,3]=='Command'
             fWrite( nHpj, '2 '+aWww[ppp,1]+"="+aWww[ppp,2]+">Commands"+CRLF)
          endif
      Next
      FWRITE( nHpj, '1 Harbour OOP commands'+CRLF)
      for ppp:=1 to len(aWww)
          if aWww[ppp,3]=='OOP Command'
             fWrite( nHpj, '2 '+aWww[ppp,1]+"="+aWww[ppp,2]+">Class"+CRLF)
          endif
      Next
      FWRITE( nHpj, '1 The Garbage API'+CRLF)
      for ppp:=1 to len(aWww)
          if aWww[ppp,3]=='The garbage collector'
             fWrite( nHpj, '2 '+aWww[ppp,1]+"="+aWww[ppp,2]+">API"+CRLF)
          endif
      Next
    fClose(nHpj)
set console off
   ELSEIF lWWW

      asort(adocinfo,,,{|x,y| x[1]+x[2]<y[1]+y[2]})
            do while .t.
          citem:=adocinfo[1,1]
          AADD(aMetaContents,{"GENERATOR","HBDOC Harbour document Extractor"})
          aadd(aMetaContents,{'Keywords',"Harbour project, Clipper, xBase, database, Free Software, GNU, compiler, cross platform, 32-bit, FiveWin,"+cItem})
          ohtm:=THTML():new('htm\hb'+strtran(citem," ","")+'.htm',aMetaContents)
          ohtm:WriteText('<h2>'+adocinfo[1,1]+'</h2><br>')
          ohtm:WriteText("<table>")
  
      for ppp:=1 to len(adocinfo)
      
           if citem ==adocinfo[ppp,1]
               oHtm:WritelinkTable(adocinfo[ppp,4],adocinfo[ppp,2],adocinfo[ppp,3])
           else
           ohtm:WriteText("</table>")
           ohtm:close()
           citem:=adocinfo[ppp,1]
           aMetaContents:={}
          AADD(aMetaContents,{"GENERATOR","HBDOC Harbour document Extractor"})
          aadd(aMetaContents,{'Keywords',"Harbour project, Clipper, xBase, database, Free Software, GNU, compiler, cross platform, 32-bit, FiveWin,"+cItem})

           ohtm:=THTML():new('htm\hb'+strtran(adocinfo[ppp,1]," ","")+'.htm',aMetaContents)

//                    oHtm:WriteMetaTag('Keywords',"Harbour project, Clipper, xBase, database, Free Software, GNU, compiler, cross platform, 32-bit, FiveWin,"+cItem)
           ohtm:WriteText('<h2>'+adocinfo[ppp,1]+'</h2><br>')
           ohtm:WriteText("<table>")
           oHtm:WritelinkTable(adocinfo[ppp,4],adocinfo[ppp,2],adocinfo[ppp,3])
           endif
           next
        if ppp>len(adocinfo)
        exit
        endif
      enddo
           ohtm:WriteText("</table>")
        ohtm:close()
           aMetaContents:={}
          AADD(aMetaContents,{"GENERATOR","HBDOC Harbour document Extractor"})
          aadd(aMetaContents,{'Keywords',"Harbour project, Clipper, xBase, database, Free Software, GNU, compiler, cross platform, 32-bit, FiveWin,"+cItem})

      oHtm1 := THTML():New( "htm\harbour.htm" ,aMetaContents)
//          oHtm:WriteMetaTag('Keywords',"Harbour project, Clipper, xBase, database, Free Software, GNU, compiler, cross platform, 32-bit, FiveWin,Harbour Documentation")
      oHtm1:WriteTitle( "Harbour Reference Guide" )
      oHtm1:WriteText( "<H1>Harbour Reference Guide</H1>" )
      oHtm1:WriteText( "<H2>HARBOUR</H2>" + hb_osnEwline() + '<UL>' )
      oHtm1:WriteLink( "overview", UpperLower( "Harbour Read me" ) )
      oHtm1:WriteLink( "license", UpperLower( "Harbour License" ) )
      oHtm1:WriteLink( "http://www.gnu.org/copyleft/gpl.html", "GNU License" )
      oHtm1:WriteLink( "compileroptions.htm", "Compiler Options" )
      oHtm1:WriteLink( "harbourextensions.htm", "Harbour Extensions" )
      oHtm1:WriteLink( "thegarbagecollector.htm", "The Garbage Collector" )
      oHtm1:WriteLink( "theidlestates.htm", "The Idle States" )
      oHtm1:WriteText( "</UL>" )
      oHtm1:WriteText( "<H2>Alphabetical list of functions by Categorie</H2>" )
      ohtm1:writetext('<ul>')

        do  while .t.
          citem:=alltrim(rtrim(ltrim(adocinfo[1,1])))
  //          citem:=strtran(adocinfo[1,1]," ","")
        ohtm1:WriteLink('hb'+strtran(adocinfo[1,1]," ","")+'.htm',cItem)
        for ppp:=1 to len(adocinfo)

          if citem<>adocinfo[ppp,1]  .and. cItem <>"Document"
                              citem:=alltrim(rtrim(ltrim(adocinfo[ppp,1])))
                    ohtm1:WriteLink('hb'+strtran(adocinfo[ppp,1]," ","")+'.htm',cItem)

//            citem:=strtran(adocinfo[ppp,1]," ","")
          endif
          next
        if ppp>len(adocinfo)
        exit
        endif
      enddo
            
      ohtm1:writetext('</ul>')        
        ohtm1:close()

      oHtm:Close()
   ELSEIF lChm
      nHpj := FCREATE( 'chm\'+lower(substr(cLinkName,1,AT(".",cLinkName)-1)) +".hhp" )

      FWRITE( nHpj, '[OPTIONS]' + CRLF )
      FWRITE( nHpj, 'Compatibility=1.1 or later'+CRLF)
      FWRITE( nHpj, 'Auto Index=Yes'+CRLF)
      FWRITE( nHpj,'Full-text search=Yes'+CRLF)
      FWRITE( nHpj, 'Language=0x416 Portugu阺 (brasileiro)' + CRLF )
      FWRITE( nHpj, 'Contents file=.\'+ lower(substr(cLinkName,1,AT(".",cLinkName)-1)) +".hhc"+ CRLF )
      FWRITE( nHpj, 'Compiled file=.\'+ lower(substr(cLinkName,1,AT(".",cLinkName)-1)) +".chm"+ CRLF )
      FWRITE( nHpj, 'Display compile progress=No'+CRLF)
      nPos:=aScan(awww,{|x| Upper(x[1])="OVERVIEW"})
      if nPos > 0
         FWRITE( nHpj,'Default topic='+ lower(awww[npos,2])+".htm"+CRLF)
      Else
         FWRITE( nHpj,'Default topic='+lower(awww[1,2]) +".htm"+CRLF)
      Endif
      FWRITE( nHpj, '[FILES]' + CRLF )
      For nPos:=1 to len(aWww)
         FWRITE( nHpj, lower(awww[npos,2])+".htm" + CRLF )
      Next
      FCLOSE( nHpj )



          ohtm:=THTML():NewContent('chm\'+lower(substr(cLinkName,1,AT(".",cLinkName)-1)) +".hhc")
          ohtm:WriteText('<!--Sitemap 1.0-->')
          ohtm:Addobject("text/site properties")
          oHtm:AddParam("FrameName","Ajuda")
          ohtm:EndObject()
          ohtm:WriteText("<ul>")
          oHtm:ListItem()
          oHtm:AddObject("text/sitemap")
          oHTm:AddParam('Name','HARBOUR')
          ohtm:EndObject()
          ohtm:WriteText("<ul>")
          writeChmContentFile(aDocinfo,"Document",oHtm)
      oHtm:WriteText( "</UL>" )
      oHtm:WriteText( "</UL>" )
        ohtm:WriteText("<ul>")
          oHtm:ListItem()
          oHtm:AddObject("text/sitemap")
          oHTm:AddParam('Name','Harbour Run Time Error')
          ohtm:EndObject()
          ohtm:WriteText("<ul>")
          writeChmContentFile(aDocinfo,"Run Time Errors",oHtm)
      oHtm:WriteText( "</UL>" )
      oHtm:WriteText( "</UL>" )
    oHtm:WriteText( "<UL>" )
          oHtm:ListItem()
          oHtm:AddObject("text/sitemap")
          oHTm:AddParam('Name','Harbour Runtime functions and Commands by Name')
          ohtm:EndObject()
          ohtm:WriteText("<ul>")
          asort(aDocinfo,,,{|x,y|x[2]<y[2]})
          for ppp:=1 to len(aDocinfo)
          if aDocInfo[ppp,1]<>'Run Time Errors' .and. aDocInfo[ppp,1] <>"Document"  .and. aDocInfo[ppp,1] <>"The garbage collector"  .and. aDocInfo[ppp,1] <>"OOP Command" .and. aDocInfo[ppp,1] <>"Command"  .and. aDocInfo[ppp,1] <>"The idle states"
               oHtm:Listitem()
               oHtm:AddObject("text/sitemap")
               oHtm:AddParam("Name",UpperLower(aDocinfo[ppp,2]))
               oHtm:AddParam("Local",lower(aDocInfo[ppp,4]))
               oHtm:EndObject()
               oHtm:WriteChmlink(lower(adocinfo[ppp,4]),adocinfo[ppp,2])
        endif
    next
    oHtm:WriteText( "</UL>" )
    ohtm:WriteText("</ul>")
    oHtm:WriteText( "<UL>" )
    ohtm:ListItem()
    oHtm:AddObject("text/sitemap")
    ohtm:addParam("Name","Harbour Functions by Categorie")
    oHtm:Endobject()
//    oHtm:WriteText( "<UL>" )
      asort(aDocinfo,,,{|x,y| x[1]<y[1]})
//        SET CONSOLE ON
      nItem := len(aResult)
      asort(aResult,,,{|x,y| x<y})
      for ppp:=1 to nItem

        cLast:=GetNextContent(ppp)
        if cLast<>'Run Time Errors' .and. cLast <>"Document"  .and. cLast <>"The garbage collector"  .and. cLast <>"OOP Command" .and. cLast <>"Command"  .and. cLast <>"The idle states"
          ohtm:WriteText("<ul>")
          oHtm:ListItem()
          oHtm:AddObject("text/sitemap")
          oHTm:AddParam('Name',cLast)
          ohtm:EndObject()
          ohtm:WriteText("<ul>")          
        endif
        if cLast<>'Run Time Errors' .and. cLast <>"Document"  .and. cLast <>"The garbage collector"  .and. cLast <>"OOP Command" .and. cLast <>"Command"  .and. cLast <>"The idle states"
        //    oHtm:WriteText( "<UL>" )
            WriteChmContentFile(aDocinfo,cLast,oHtm)
            oHtm:WriteText( "</UL>" )
            oHtm:WriteText( "</UL>" )
        endif
//               oHtm:WriteText( "</UL>" )
      Next
         oHtm:WriteText( "</UL>" )
//         oHtm:WriteText( "</UL>" )
         oHtm:WriteText( "<UL>" )
          oHtm:ListItem()
          oHtm:AddObject("text/sitemap")
          oHTm:AddParam('Name','Harbour Commands')
          ohtm:EndObject()
          ohtm:WriteText("<ul>")
        asort(aDocinfo,,,{|x,y| x[2]<y[2]})
          writeChmContentFile(aDocinfo,"Command",oHtm)
      oHtm:WriteText( "</UL>" )
      oHtm:WriteText( "</UL>" )
         oHtm:WriteText( "<UL>" )
          oHtm:ListItem()
          oHtm:AddObject("text/sitemap")
          oHTm:AddParam('Name','Harbour OOP Commands')
          ohtm:EndObject()
          ohtm:WriteText("<ul>")
        asort(aDocinfo,,,{|x,y| x[2]<y[2]})
          writeChmContentFile(aDocinfo,"OOP Command",oHtm)
      oHtm:WriteText( "</UL>" )
         oHtm:WriteText( "</UL>" )
         oHtm:WriteText( "<UL>" )
          oHtm:ListItem()
          oHtm:AddObject("text/sitemap")
          oHTm:AddParam('Name','The Garbage API')
          ohtm:EndObject()
          ohtm:WriteText("<ul>")
        asort(aDocinfo,,,{|x,y| x[2]<y[2]})
          writeChmContentFile(aDocinfo,"The garbage collector",oHtm)
      oHtm:WriteText( "</UL>" )
      ohtm:WriteText("</ul>")
    ohtm:close()


   ELSEIF lNgi
      SET ALTERNATE TO "assembl.bat" ADDITIVE
      SET ALTERNATE ON
      SET CONSOLE OFF

   ENDIF

   //  Now assemble the output
   IF .NOT. lAscii

      ? "REM Compile the sources"
      ? "Echo Compiling the sources"
      IF lNorton
         ? "Helpc /W31   hardoc.hdf"
         ? "REM  Link the files"
         ? "Echo Linking library"
         ? "hcw hardoc.hpj"
         ? " "
      ELSEIF lRtf

         ? "REM  Link the files"
         ? "Echo Linking library"
         ? "hcw harbour.hpj"
         ? " "

      ELSEIF lNgi
         ? "Processing Input Files"
         SET ALTERNATE TO
         SET ALTERNATE OFF
         SET CONSOLE ON
         ProcNgiInput()
         SET ALTERNATE TO "assembl.bat" ADDITIVE
         SET ALTERNATE ON
         SET CONSOLE OFF
         ? 'Copy ngi\overview.ngi .'
         ? 'Copy ngi\License.ngi  .'
         ? 'Copy ngi\Funcam.txt   .'
         ? 'Copy ngi\funcn_.txt   .'
         ? 'copy ngi\comm.txt     .'
         ? 'Compiling Sources'
         ? 'ngxc overview.ngi'
         ? 'ngxc license.ngi'
         ? 'ngxc funcam.txt'
         ? 'ngxc funcn_.txt '
         ? 'ngxc comm.txt'
         ? 'Linking the Guide'
         ? 'ngxl '+cLinkName
         ? 'del ngi\*.*'
         ? 'del *.ngo'
      ENDIF

      @ INFILELINE,  0 CLEAR TO INFILELINE, MAXCOL()
      @ INFILELINE, 30 SAY "Writing summary file"         

   ENDIF

   SET ALTERNATE TO "hbdoc.log"
   SET ALTERNATE ON
   SET CONSOLE OFF
   FOR i := 1 TO LEN( aDocInfo )
      ? PAD( aDocInfo[ i, 1 ], 15 ), PAD( aDocInfo[ i, 2 ], 15 ), PAD( aDocInfo[ i, 4 ], 15 )
   NEXT

   //  Send out list of authors

/*   @ INFILELINE,  0 CLEAR TO INFILELINE, MAXCOL()
   @ INFILELINE, 30 SAY "Sorting Author file"         

   FOR i := 1 TO LEN( aAuthorList )

      aName := ListAsArray( aAuthorList[ i, 1 ], " " )
      nLen  := 0
      AEVAL( aName, { | a, b | nLen := IIF( !EMPTY( a ), b, nLen ) }, 1, LEN( aName ) )

      IF nLen > 0
         cFName := aName[ 1 ]

         IF nLen > 1
            //  Middle initial
            cMI := aName[ 2 ]
            IF !( LEN( cMi ) = 1 .OR. ( LEN( cMi ) = 2 .AND. RIGHT( cMi, 1 ) = "." ) )
               cMi := NIL
            ENDIF
            //  Last name
            cLName := ""
            FOR j := IIF( !EMPTY( cMi ), 3, 2 ) TO nLen
               cLname := ALLTRIM( cLName ) + " " + aName[ j ]
            NEXT
            cLName := LTRIM( cLName )
         ENDIF

         //  Add to array

         aAuthorList[ i, 3 ] = cLName + "," + cFname + IIF( !EMPTY( cMi ), " " + cMi, " " )

      ENDIF

   NEXT
   //  sort the list bring any CIS ID to the top so it gets printed out

   ASORT( aAuthorList,,, { | a, b | IIF( UPPER( a[ 3 ] ) == UPPER( b[ 3 ] ), a[ 2 ] > b[ 2 ], ;
                           UPPER( a[ 3 ] ) < UPPER( b[ 3 ] ) ) } )

   @ INFILELINE,  0 CLEAR TO INFILELINE, MAXCOL()
   @ INFILELINE, 30 SAY "Writing Author file"         

   IF LEN( aAuthorList ) > 1
      i     := 2
      lDone := .F.
      DO WHILE !lDone

         IF UPPER( ALLTRIM( aAuthorList[ i, 1 ] ) ) == UPPER( ALLTRIM( aAuthorList[ i - 1, 1 ] ) )

            //  Remove duplicate names but capture a CIS ID if we don't have one

            IF EMPTY( aAuthorList[ i - 1, 2 ] ) .AND. !EMPTY( aAuthorList[ i, 2 ] )
               aAuthorList[ i - 1, 2 ] = aAuthorList[ i, 2 ]
            ENDIF
            ADEL( aAuthorList, i )
            ASIZE( aAuthorList, LEN( aAuthorList ) - 1 )
         ELSE
            i ++
         ENDIF
         lDone := ( i > LEN( aAuthorList ) )
      ENDDO
   ENDIF

   //  Now write it out

   SET ALTERNATE TO "author.txt"
   SET ALTERNATE ON
   SET CONSOLE OFF

   FOR i := 1 TO LEN( aAuthorList )

      ? " ", aAuthorList[ i, 1 ], IIF( !EMPTY( aAuthorList[ i, 2 ] ), "[" + aAuthorList[ i, 2 ] + "]", "" )

   NEXT

   SET CONSOLE ON
   SET ALTERNATE OFF
   SET ALTERNATE TO

*/

   @ MAXROW(), 0 SAY "Execute ASSEMBL.BAT to compile and link Guides"         

   //  Return to caller

RETURN NIL

//  End of MAIN()

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function ListAsArray2()
*+
*+    Called from ( genng.prg    )   1 - function procngialso2()
*+                ( genng1.prg   )   1 - function procngialso2()
*+                ( genos2.prg   )   1 - function procos2also()
*+                ( gentrf.prg   )   1 - function proctroffalso()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION ListAsArray2( cList, cDelimiter )

   LOCAL nPos
   LOCAL aList := {}   // Define an empty array

   IF cDelimiter = NIL
      cDelimiter := ","
   ENDIF
   //
   DO WHILE ( nPos := AT( cDelimiter, cList ) ) != 0
      AADD( aList, SUBSTR( cList, 1, nPos - 1 ) )           // Add a new element
      cList := SUBSTR( cList, nPos + 1 )
   ENDDO
   AADD( aList, cList )                 // Add final element
   //
RETURN aList        // Return the array

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function StripNgControls()
*+
*+    Called from ( genhpc.prg   )   3 - function processfiles()
*+                ( gentrf.prg   )   2 - function processtroff()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION StripNgControls( cString )

   LOCAL nPos
   LOCAL lStriped := .f.

   nPos := AT( "^b", cString )
   IF nPos > 0
      cString  := SUBSTR( cString, nPos + 3 )
      lStriped := .t.
   ELSE
      IF !lStriped
         cString := cString
      ENDIF
   ENDIF

   nPos := AT( "^b^", cString )
   IF nPos > 0
      cString  := SUBSTR( cString, 1, nPos - 1 )
      lStriped := .t.
   ELSE
      IF !lStriped
         cString := cString
      ENDIF
   ENDIF

   nPos := AT( "^CFE", cString )
   IF nPos > 0
      cString  := SUBSTR( cString, nPos + 5 )
      lStriped := .t.
   ELSE
      IF !lStriped
         cString := cString
      ENDIF
   ENDIF

   nPos := AT( "^a1f", cString )
   IF nPos > 0
      cString  := SUBSTR( cString, nPos + 5 )
      lStriped := .t.
   ELSE
      IF !lStriped
         cString := cString
      ENDIF
   ENDIF

RETURN cString

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function StripFiles()
*+
*+    Called from ( genasc.prg   )   1 - function asciifiles()
*+                ( genhpc.prg   )   1 - function processfiles()
*+                ( genhtm.prg   )   1 - function processwww()
*+                ( genhtm1.prg  )   1 - function processwww()
*+                ( genhtm2.prg  )   1 - function processwww()
*+                ( genng.prg    )   1 - function processing()
*+                ( genng1.prg   )   1 - function processing()
*+                ( genos2.prg   )   1 - function processos2()
*+                ( genrtf.prg   )   1 - function processrtf()
*+                ( gentrf.prg   )   1 - function processtroff()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION StripFiles( cString )

   //
   //  This routine and all accompaning database structures are
   //  Copyright (C) 1993 Leo J. Letendre. All rights reserved.
   //
   //  Purpose: Determine the position of the first non-blank character
   //
   //  Modification History:
   //         Version    Date      Who       Notes
   //          V1.00     10/30/93  LJL       Initial Version
   //
   //  Calling parameters: cString - The string to remove filenames from
   //
   //  Returns: <cString> with filenames removed
   //
   //  Notes: For example:  func.ngo:FUNC1() proc.ngo:PROC1()
   //             becomes:  FUNC1() PROC1()
   // -
   //  LOCAL variables:
   LOCAL nColon        // location of colon
   LOCAL nSpace        // location of space prior to colon

   //  Look for a colon

   DO WHILE ( nColon := AT( ":", cString ) ) > 0

      //  find the space prior to it
      nSpace := RAT( " ", LEFT( cString, nColon ) )
      IF nSpace > 0
         cString := STUFF( cString, nSpace + 1, nColon - nSpace, "" )
      ELSE
         cString := STUFF( cString, 1, nColon, "" )
      ENDIF
   ENDDO

RETURN cString

//  End of StripFiles

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function FirstNB()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION FirstNB( cString )

   //
   //  This routine and all accompaning database structures are
   //  Copyright (C) 1992 Leo J. Letendre. All rights reserved.
   //
   //  Purpose: Determine the position of the first non-blank character
   //
   //  Modification History:
   //         Version    Date      Who       Notes
   //          V1.00     10/10/92  LJL       Initial Version
   //
   //  Calling parameters: cString - The string to test
   //
   //  Notes: None
   // -
   //  LOCAL variables:
   LOCAL nLen    := LEN( cString )
   LOCAL nReturn := 0
   LOCAL i

   //
   //  Entry Point
   //
   i := 1
   IF !EMPTY( cString )
      DO WHILE nReturn = 0 .AND. i < LEN( cString )
         IF SUBSTR( cString, i, 1 ) != " "
            nReturn := i
         ELSE
            i ++
         ENDIF
      ENDDO
   ENDIF

RETURN nReturn

//  End of file FirstNB

/***
*  ListAsArray( <cList>, <cDelimiter> ) --> aList
*  Convert a delimited string to an array
* Taken from Clipper supplied routines 1/2/90
*
*/

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function ListAsArray()
*+
*+    Called from ( genng.prg    )   1 - function procngialso()
*+                ( genng1.prg   )   1 - function procngialso()
*+                ( hbdoc.prg    )   1 - function main()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION ListAsArray( cList, cDelimiter )

   LOCAL nPos
   LOCAL aList := {}   // Define an empty array

   IF cDelimiter = NIL
      cDelimiter := ","
   ENDIF
   //
   DO WHILE ( nPos := AT( cDelimiter, cList ) ) != 0
      AADD( aList, '"' + SUBSTR( cList, 1, nPos - 1 ) + '"' )                   // Add a new element
      cList := SUBSTR( cList, nPos + 1 )
   ENDDO
   AADD( aList, '"' + cList + '"' )     // Add final element
   //
RETURN aList        // Return the array

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function ReadLN()
*+
*+    Called from ( genasc.prg   )   5 - function asciifiles()
*+                ( genhpc.prg   )   5 - function processfiles()
*+                ( genhtm.prg   )   7 - function processwww()
*+                                   2 - function formathtmbuff()
*+                                   2 - function prochtmdesc()
*+                ( genhtm1.prg  )   7 - function processwww()
*+                                   2 - function formathtmbuff()
*+                                   2 - function prochtmdesc()
*+                ( genhtm2.prg  )   6 - function processwww()
*+                                   2 - function formathtmbuff()
*+                                   2 - function prochtmdesc()
*+                ( genng.prg    )   6 - function processing()
*+                                   4 - function procngiinput()
*+                                   2 - function procngdesc()
*+                                   2 - function formatngbuff()
*+                ( genng1.prg   )   6 - function processing()
*+                                   4 - function procngiinput()
*+                                   2 - function procngdesc()
*+                                   2 - function formatngbuff()
*+                ( genos2.prg   )   6 - function processos2()
*+                                   2 - function formatos2buff()
*+                                   2 - function procos2desc()
*+                ( genrtf.prg   )   6 - function processrtf()
*+                                   2 - function procrtfdesc()
*+                                   2 - function formatrtfbuff()
*+                ( gentrf.prg   )   5 - function processtroff()
*+                ( hbdoc.prg    )   1 - function readatfile()
*+                                   2 - function fill_link_info()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION ReadLN( leof )

   LOCAL cBuffer := ""

   cBuffer := FT_FREADLN()
   FT_FSKIP( 1 )
   lEof := FT_FEOF()

RETURN cBuffer
//  End of ReadLN

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function ReadAtFile()
*+
*+    Called from ( hbdoc.prg    )   1 - function main()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION ReadAtFile( cAtFile )

   //
   //  This routine and all accompaning database structures are
   //  Copyright (C) 1992 Leo J. Letendre.
   //
   //  Purpose: read in the users list of files to act on
   //
   //  Modification History:
   //         Version    Date      Who       Notes
   //          V1.00     1/1/92   LJL       Initial Version
   //
   //  Calling parameters: cAtFile - The name of the file containing a list of
   //                               files to be processed
   //
   //  Returns: an array containing information that looks like it came
   //           from DIRECTORY() but only has the name present.
   //
   //  Notes:
   // -
   //  LOCAL variables:
   LOCAL aDirList := {}
   LOCAL cBuffer
   LOCAL lEof
   LOCAL nCount   := 0

   //
   //  Entry Point
   //
   IF FT_FUSE( cAtFile ) <> NIL

      //  Read each line
      lEof := .F.
      DO WHILE .NOT. lEof

         cBuffer := ALLTRIM( ReadLN( @lEof ) )
         IF .NOT. EMPTY( cBuffer )
            AADD( aDirList, ARRAY( F_LEN ) )
            nCount ++
            aDirList[ nCount, F_NAME ] = UPPER( cBuffer )
         ENDIF
      ENDDO
   ENDIF

   FT_FUSE()

RETURN aDirList

//  End of ReadAtFile

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function WRITE_ERROR()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION WRITE_ERROR( cDescrip, cBadLine, nLineCnt, nMax, cFile )

   //  This routine will send error messages to the error log - hbdocerr.log
   //
   //  Calling parameters: cDesc - Description of info being written
   //                      cBadLine - The offending line - IF NIL then just
   //                                 output cDesc and filename
   //                      nLineCnt - The line number of the bad line
   //                      nMax - The maximum length of the bad line
   //                      cFile - The file currently being processed
   //
   //  Returns: NIL
   //
   //  Entry point
   //
   //  Point output to the log file
   SET ALTERNATE TO "hbdocerr.log" ADDITIVE
   SET CONSOLE OFF
   SET ALTERNATE ON

   //  Send out the output
   IF cBadLine = NIL
      ? cDescrip, "in file", cFile
      ? " "
   ELSE
      ? "Line too long in file", cFile, "at line", ALLTRIM( STR( nLineCnt, 10, 0 ) )
      ? "Reading", cDescrip, "information when line greater than", STR( nMax, 2, 0 ), "encountered:"
      ? cBadLine
      ? " "
   ENDIF

   //  Turn off the log file and return

   SET ALTERNATE OFF
   SET CONSOLE ON
   SET ALTERNATE TO

RETURN NIL

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function fill_link_info()
*+
*+    Called from ( hbdoc.prg    )   1 - function main()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION fill_link_info( cLinkName )

   //
   //  This routine and all accompaning database structures are
   //  Copyright (C) 1992 Leo J. Letendre.
   //
   //  Purpose: read the link information to learn how to assemble database files
   //
   //  Modification History:
   //         Version    Date      Who       Notes
   //          V1.00     1/22/92   LJL       Initial Version
   //          V1.01     4/25/92   LJL       Get the compiler type based upon
   //                                        "object" file extension
   //
   //  Calling parameters: cLinkName - The name of the link file
   //
   //  Returns: cCompiler - The name of the compiler to use
   //
   //  Notes: looks for the !menu command and then reads the lines after it to
   //         get the catagories and filenames associated with each.
   // -
   //  LOCAL variables:
   LOCAL cBuffer
   LOCAL lEof
   LOCAL cSpace
   LOCAL cCategory
   LOCAL cFile
   LOCAL nReadHandle
   LOCAL lMenuMode   := .F.                // Menu infomation being read
   LOCAL lGetType    := .T.                // Get the compiled file type to determine complier
   LOCAL cCompiler     // Compiler Type
   //
   //  Entry Point
   //
   nReadHandle := FT_FUSE( cLinkName )
   lEof        := .F.

   DO WHILE .NOT. lEof

      //  Read a line
      cBuffer := UPPER( ReadLN( @lEof ) )
      if AT("!NAME:",cBuffer)>0
            cTitle:=Substr(cBuffer,AT(":",cBuffer)+1)
      Endif
      //  Does it have a !menu?

      IF AT( "!MENU", cBuffer ) > 0
         lMenuMode := .T.
         cBuffer   := UPPER( ReadLN( @lEof ) )
      ELSEIF LEFT( cBuffer, 1 ) = "!"
         lMenuMode := .F.
      ENDIF

      //  If we are in menu mode and the line has information on it then parse it

      cBuffer := ALLTRIM( cBuffer )

      IF .NOT. EMPTY( cBuffer ) .AND. lMenuMode
         cSpace    := AT( "  ", cBuffer )
         cCategory := UPPER( RTRIM( LEFT( cBuffer, cSpace - 1 ) ) )
         cFile     := UPPER( LTRIM( SUBSTR( cBuffer, cSpace ) ) )
         IF lGetType
            cCompiler := IIF( ".NGO" $ cFile, "NGXC", "EHC" )
            lGetType  := .NOT. ( ".NGO" $ cFile .OR. "EHO" $ cFile )
         ENDIF
         cFile := STRTRAN( cFile, IIF( cCompiler = "NGXC", ".NGO", ".EHO" ), ".TXT" )
         AADD( aLinkInfo, { cCategory, cFile, .F. } )
      ENDIF

   ENDDO

   //  Close the file

   FT_FUSE()
   //  Return to caller

RETURN cCompiler

*+ EOF: HBDOC.PRG

STATIC FUNCTION ReadLinkFile( cFile )

   LOCAL cBuffer   := ''
   LOCAL NPOS      := 0
   Local cLine
   Local cVer:=''
   LOCAL aLocDoc   := {}
   Local nH:=FT_FUSE(cFile) 
   DO WHILE FREADline( nH, @cBuffer, 4096 )
      cBuffer := TRIM( SUBSTR( cBuffer, 1 ) )
      AADD( Alocdoc, CbUFFER )
   ENDDO

   FT_FUSE()
    frename(CFILE,substr(cfile,1,at('.',cFile)-1)+'.old')
    cVer:=docver()
    nH:=fcreate(cfile)
        for nPos:=1 to len(aLocdoc)
            cLine:=alocdoc[nPos]
            if at("%HB_VERSION%",cLine)>0
                cLine:=strtran(cLine,'%HB_VERSION%',cVer)
            endif
            IF AT("%HB_BLDDATE%",cLine)>0
                SET CENTURY ON
                cLine:=strtran(cLine,'%HB_BLDDATE%',DTOC(date()))
                SET CENTURY Off
            endif
            FWRITE(nH,cLine+HB_OSNEWLINE())
        NEXT
        FCLOSE(nh)
RETURN nil


FUNCTION DocVer()
local cVersion:=version()
local cReturn:=''
cReturn:=substr(cVersion,9,4)

RETURN cReturn

Function WriteContentFile(aTop,cCat,nFile)
Local nCount:=0

Local aTemp:={}
asort(aWww,,,{|x,y|x[3]+x[1]<y[3]+y[1]})
for nCount:=1 to Len(aWww)
    if Alltrim(aTop[nCount,3])==alltrim(cCat)
        aadd(aTemp,{aTop[nCount,1],aTop[nCount,2],aTop[nCount,3]})
    endif
Next
asort(aTemp,,,{|x,y| x[1]<y[1]})
    fWrite( nFile, '2 '+cCat+CRLF)
for nCount:=1 to Len(aTemp)
    fWrite( nFile, '3 '+aTemp[nCount,1]+"="+aTemp[nCount,2]+">Funca"+CRLF)
next
return nil


function GetNextContent(nPos)
Local cReturn
if nPos <=Len(aResult)
cReturn := aResult[nPos]
endif
return cReturn
Function WriteChmContentFile(aTop,cCat,oHtm)
Local nCount:=0

Local aTemp:={}
asort(aTop,,,{|x,y|x[1]+x[2]<y[1]+y[2]})
for nCount:=1 to Len(aWww)
    if Alltrim(aTop[nCount,1])==alltrim(cCat)
        aadd(aTemp,{aTop[nCount,1],aTop[nCount,2],aTop[nCount,4]})
    endif
Next
asort(aTemp,,,{|x,y| x[1]<y[1]})

for nCount:=1 to Len(aTemp)
          oHtm:ListItem()
          oHtm:AddObject("text/sitemap")
          oHtm:AddParam("Name",aTemp[nCount,2])
          oHtm:AddParam("Local",aTemp[nCount,3])
          oHtm:EndObject()
          OHTM:WriteChmLink(aTemp[nCount,3],aTemp[nCount,2])
next
return nil
    

