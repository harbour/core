@echo off
rem
rem $Id$
rem

rem NOTE:       Purpose of this script is to take the original
rem             files from its source distribution and convert
rem             them to the short filenames we use here in Harbour.
rem             Short filenames are needed for dos compiler support.
rem             Some other automated modifications are also done
rem             to help compiling the sources "as-is", to try to
rem             avoid any manual editing on these foreign sources.
rem             [vszakats]
rem
rem             This tool uses 'GNU gsar' for search and replace.
rem             and 'GNU unix2dos' for line ending conversion.
rem
rem DISCLAIMER: This tool is targeted only to Harbour core
rem             maintainers. If you're not one of them you
rem             don't have to mess with this tool.

copy ori_src\LICENCE         LICENCE
copy ori_src\blocksort.c     blocksor.c
copy ori_src\bzip2.c         bzip2.c
copy ori_src\bzlib.c         bzlib.c
copy ori_src\compress.c      compress.c
copy ori_src\crctable.c      crctable.c
copy ori_src\decompress.c    decompre.c
copy ori_src\huffman.c       huffman.c
copy ori_src\randtable.c     randtabl.c
copy ori_src\bzlib.h         bzlib.h
copy ori_src\bzlib_private.h bzlib_pr.h

unix2dos *.c
unix2dos *.h

gsar -o -s":x22bzlib_private.h:x22" -r":x22bzlib_pr.h:x22" *.c
