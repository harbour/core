@echo off
rem
rem $Id$
rem

rem NOTE:       Purpose of this script is to take the source files
rem             in Harbour repo and convert them back to the filenames
rem             used in the original source distribution.
rem             This is to aid finding local modifications and
rem             apply them after an original source update.
rem             [vszakats]
rem
rem             This tool uses 'GNU gsar' for search and replace.
rem
rem DISCLAIMER: This tool is targeted only to Harbour core
rem             maintainers. If you're not one of them you
rem             don't have to mess with this tool.

md ori_dst
del ori_dst\*.* /Y

copy LICENCE    ori_src\LICENCE
copy blocksor.c ori_src\blocksort.c
copy bzlib.c    ori_src\bzlib.c
copy compress.c ori_src\compress.c
copy crctable.c ori_src\crctable.c
copy decompre.c ori_src\decompress.c
copy huffman.c  ori_src\huffman.c
copy randtabl.c ori_src\randtable.c
copy bzlib.h    ori_src\bzlib.h
copy bzlib_pr.h ori_src\bzlib_private.h

cd ori_dst

gsar -o -s":x22bzlib_pr.h:x22" -r":x22bzlib_private.h:x22" *.c

cd ..
