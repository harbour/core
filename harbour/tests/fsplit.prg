/*
 * $Id$
*/

#include "fileio.ch"

function fsplit ( csource, csplit, nbyte )

local i                           // general counter
local ccommand      := ""         // dos command for joining files
local cexist        := ""         // batch file error checker
local nbufsize      := 8          // 8k for buffer Read/Write
local hsource       := 0          // file handle for source file
local hdestination  := 0          // file handle for destination file
local cbuffer       := ""         // buffer for read/write
local lsplit        := .f.        // return value
local nblock        := 0          // bytes read
local ncurrent      := 0          // total bytes copied
local nsplit        := 1          // destination file name extension
local cbat          := "join.bat" // for joining split files
local cdestination                // destination filename
local hbat                        // file handle for join.bat
local afile         := {}         // for information upon completion
local nseconds      := seconds()  // time elapsed
local nfilesize     := 0          // file size to be split
local hfile         := 0          // split file sizes
local cret          := chr(13) + chr(10)  // carriage return
local ctmp          := "@echo off" + cret // 1st line in join.bat
local nfile

// y2k compliance
set(4,"dd/mm/yyyy")
set epoch to year(date()) - 50

// no params passed
if pcount() == 0
   ? "Usage : FSPLIT <cSourceFile> [cSplitFileName] [nKBytesEach]"
   return lsplit
endif

// default destination name
csplit := if( csplit == nil, "split.", csplit + "." )

// default size of each split file 360 x 1024 bytes
nbyte := if( nbyte ==  nil, 368640, val( nbyte ) * 1024 )

// open the source file
BEGIN SEQUENCE
   if ( hsource := fopen(csource,FO_READ+FO_SHARED) ) <> -1
      // is file size smaller than chunk size ?
      if ( nfilesize := fseek( hsource, 0 , FS_END ) ) <= nbyte
         alert("***** Error *****;File Size Is Smaller Than Chunk Size;"+"Source Size = "+ltrim(str(nfilesize))+" Chunk Size = "+ltrim(str(nbyte)),{"  Okay  "},"w+/b")
         fclose( hsource )
         break
      endif
      fseek( hsource, 0, FS_SET )               // go to top of file
      cdestination := csplit+ltrim(str(nsplit)) // destination file name
      hbat         := fcreate( cbat )           // join.bat
      ctmp         += "rem source file " + csource + " size "+ ltrim(str(nfilesize ) ) + cret
      ctmp         += "rem split on " + dtoc(date()) + " "+ time() + cret
      ccommand     := "copy /b "                // line in join.bat
      ccommand     += cdestination + "+"        // line in join.bat
      hdestination := fcreate(cdestination)     // create 1st split file
      if hdestination <> - 1
         nbufsize     *= 1024                      // buffer size
         cbuffer      := space( nbufsize )         // buffer read/write
         aadd( afile, cbat )
         aadd( afile, cdestination )
         dispoutat(24,00,padr("Writing " + cdestination,80))
         while !lsplit
            lsplit := ( ( ( nblock := fread( hsource, @cbuffer, nbufsize ) ) == 0 ) .or. ( fwrite ( hdestination, cbuffer, nblock ) < nblock ) )
            ncurrent += nblock
            if ncurrent >= nbyte                   // files size already exceed ?
               fclose( hdestination )              // close file
               ncurrent      := 0                  // reset counter
               cdestination  := csplit + ltrim(str(++nsplit))  // next file name
               ccommand      += cdestination + "+"             // line in join.bat
               hdestination  := fcreate(cdestination)          // create next file
               aadd( afile, cdestination )
               dispoutat(24,00,padr("Writing " + cdestination,80))
            endif
         enddo
         fclose( hsource )         // close source file
         fclose( hdestination )    // close split file
         ccommand := left( ccommand, rat("+",ccommand ) - 1 ) + " " // line in join.bat
         ccommand += csource + cret                // line in join.bat
         ctmp += "rem the following files should be placed in a directory"+chr(13)+chr(10)
         for i := 2 to len( afile )
            hfile     := fopen( afile[i], FO_READ+FO_SHARED )
            nfilesize := fseek( hfile, 0 , FS_END )
            fclose( hfile )
            ctmp += "rem " + afile[i] + " - " + ltrim( str( nfilesize ) ) + cret
         next
         nfile := len( afile )                            // error checker
         for i := 2 to nfile
            ctmp += "if not exist " + afile[i] + " goto error"+ltrim(stR(i-1)) + cret
         next
         ctmp += ccommand
         ctmp += "goto end" + cret
         for i := 2 to nfile
            ctmp += ":error" + ltrim(str(i-1)) + cret
            ctmp += "echo " + cret
            ctmp += "echo missing file " + afile[i] + cret
            ctmp += "goto end" + cret
         next
         ctmp += ":end" + cret
         fwrite( hbat, upper( ctmp ) )   // write join.bat
         fclose( hbat )                  // close handle
         ? "Split succesful ..."
         ?
         ? "Files Created : "
         ?
         aeval( afile, { |e| qout(upper(e)) } )
         ?
         ? "Done in " + ltrim(str(seconds()-nseconds)) +" seconds."
         ? "To restore, type JOIN"
      else
         break
      endif
   endif
RECOVER
   ? chr(7)
   ? "Error ...."
END SEQUENCE

return lsplit
