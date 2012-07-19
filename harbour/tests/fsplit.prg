/*
 * $Id$
 */

#include "fileio.ch"

PROCEDURE Main( cSource, cSplit, nByte )

   ? fsplit( cSource, cSplit, Val( nByte ) )

   RETURN

FUNCTION fsplit( csource, csplit, nbyte )

   LOCAL i                           // general counter
   LOCAL ccommand      := ""         // dos command for joining files
   LOCAL cexist        := ""         // batch file error checker
   LOCAL nbufsize      := 8          // default buffer Read/Write size
   LOCAL hsource       := 0          // file handle for source file
   LOCAL hdestination  := 0          // file handle for destination file
   LOCAL cbuffer       := ""         // buffer for read/write
   LOCAL lsplit        := .F.        // return value
   LOCAL nblock        := 0          // bytes read
   LOCAL ncurrent      := 0          // total bytes copied
   LOCAL nsplit        := 1          // destination file name extension
   LOCAL cbat          := "join.bat" // for joining split files
   LOCAL cdestination                // destination filename
   LOCAL hbat                        // file handle for join.bat
   LOCAL afile         := {}         // for information upon completion
   LOCAL nseconds      := Seconds()  // time elapsed
   LOCAL nfilesize     := 0          // file size to be split
   LOCAL hfile         := 0          // split file sizes
   LOCAL cret          := Chr( 13 ) + Chr( 10 )  // carriage return
   LOCAL ctmp          := "@echo off" + cret // 1st line in join.bat
   LOCAL nfile

   // y2k compliance
   SET DATE ANSI
   SET EPOCH TO Year( Date() ) - 50

   // no params passed
   IF PCount() == 0
      ? "Usage : FSPLIT <cSourceFile> [cSplitFileName] [nKBytesEach]"
      RETURN lsplit
   ENDIF

   // default destination name
   csplit := iif( csplit == nil, "split.", csplit + "." )

   // default size of each split file 360 x 1024 bytes
   nbufsize := iif( Empty( nbyte ), 360, nbyte )
   nbyte := nbufsize * 1024

   // open the source file
   BEGIN SEQUENCE
      IF ( hsource := FOpen( csource,FO_READ + FO_SHARED ) ) != F_ERROR
         // is file size smaller than chunk size ?
         IF ( nfilesize := FSeek( hsource, 0 , FS_END ) ) <= nbyte
            Alert( "***** Error *****;File Size Is Smaller Than Chunk Size;" + "Source Size = " + hb_ntos( nfilesize ) + " Chunk Size = " + hb_ntos( nbyte ), { "  Okay  " }, "w+/b" )
            FClose( hsource )
            break
         ENDIF
         FSeek( hsource, 0, FS_SET )                // go to top of file
         cdestination := csplit + hb_ntos( nsplit ) // destination file name
         hbat         := FCreate( cbat )            // join.bat
         IF hbat != F_ERROR
         ELSE
            break
         ENDIF
         ctmp         += "rem source file " + csource + " size " + hb_ntos( nfilesize ) + cret
         ctmp         += "rem split on " + DToC( Date() ) + " " + Time() + cret
         ccommand     := "copy /b "                // line in join.bat
         ccommand     += cdestination + "+"        // line in join.bat
         hdestination := FCreate( cdestination )     // create 1st split file
         IF hdestination != - 1
            nbufsize     *= 1024                      // buffer size
            cbuffer      := Space( nbufsize )         // buffer read/write
            AAdd( afile, cbat )
            AAdd( afile, cdestination )
            DispOutAt( 24, 00, PadR( "Writing " + cdestination,80 ) )
            DO WHILE ! lsplit
               lsplit := ( ( ( nblock := FRead( hsource, @cbuffer, nbufsize ) ) == 0 ) .OR. ( FWrite ( hdestination, cbuffer, nblock ) < nblock ) )
               ncurrent += nblock
               IF ncurrent >= nbyte                   // files size already exceed ?
                  FClose( hdestination )              // close file
                  ncurrent      := 0                  // reset counter
                  cdestination  := csplit + hb_ntos( ++nsplit )  // next file name
                  ccommand      += cdestination + "+"             // line in join.bat
                  hdestination  := FCreate( cdestination )          // create next file
                  IF hdestination != F_ERROR
                     AAdd( afile, cdestination )
                     DispOutAt( 24, 00, PadR( "Writing " + cdestination,80 ) )
                  ELSE
                     BREAK
                  ENDIF
               ENDIF
            ENDDO
            FClose( hsource )         // close source file
            FClose( hdestination )    // close split file
            ccommand := Left( ccommand, RAt( "+",ccommand ) - 1 ) + " " // line in join.bat
            ccommand += csource + cret                // line in join.bat
            ctmp += "rem the following files should be placed in a directory" + Chr( 13 ) + Chr( 10 )
            FOR i := 2 TO Len( afile )
               hfile     := FOpen( afile[ i ], FO_READ + FO_SHARED )
               nfilesize := FSeek( hfile, 0 , FS_END )
               FClose( hfile )
               ctmp += "rem " + afile[ i ] + " - " + hb_ntos( nfilesize ) + cret
            NEXT
            nfile := Len( afile )                            // error checker
            FOR i := 2 TO nfile
               ctmp += "if not exist " + afile[ i ] + " goto error" + hb_ntos( i - 1 ) + cret
            NEXT
            ctmp += ccommand
            ctmp += "goto end" + cret
            FOR i := 2 TO nfile
               ctmp += ":error" + hb_ntos( i - 1 ) + cret
               ctmp += "echo " + cret
               ctmp += "echo missing file " + afile[ i ] + cret
               ctmp += "goto end" + cret
            NEXT
            ctmp += ":end" + cret
            FWrite( hbat, Upper( ctmp ) )   // write join.bat
            FClose( hbat )                  // close handle
            ? "Split succesful ..."
            ?
            ? "Files Created : "
            ?
            AEval( afile, {| e | QOut( Upper( e ) ) } )
            ?
            ? "Done in " + hb_ntos( Seconds() - nseconds ) + " seconds."
            ? "To restore, type JOIN"
         ELSE
            break
         ENDIF
      ELSE
         break
      ENDIF
   RECOVER
      ? Chr( 7 )
      ? "Error ...."
   END SEQUENCE

   RETURN lsplit
