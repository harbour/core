@echo off
rem
rem $Id$
rem

rem Tested with PCRE 7.7

rem NOTE:       Purpose of this script is to take the PCRE files
rem             in Harbour repo and convert them back to the filenames
rem             used in the original PCRE source distribution.
rem             This is to aid finding local modifications and
rem             apply them after a PCRE source update.
rem             [vszakats]
rem
rem             This tool uses 'GNU gsar' for search and replace.
rem
rem DISCLAIMER: This tool is targeted only to Harbour core
rem             maintainers. If you're not one of them you
rem             don't have to mess with this tool.

md ori_dst
del ori_dst\*.* /Y

copy LICENSE    ori_dst\LICENSE
copy config.h   ori_dst\config.h.generic
copy pcre.h     ori_dst\pcre.h.generic
copy pcreinal.h ori_dst\pcre_internal.h
copy ucp.h      ori_dst\ucp.h
copy chartabs.c ori_dst\pcre_chartables.c.dist
copy pcrecomp.c ori_dst\pcre_compile.c
copy pcreconf.c ori_dst\pcre_config.c
copy pcredfa.c  ori_dst\pcre_dfa_exec.c
copy pcreexec.c ori_dst\pcre_exec.c
copy pcrefinf.c ori_dst\pcre_fullinfo.c
copy pcreget.c  ori_dst\pcre_get.c
copy pcreglob.c ori_dst\pcre_globals.c
copy pcreinfo.c ori_dst\pcre_info.c
copy pcremktb.c ori_dst\pcre_maketables.c
copy pcrenewl.c ori_dst\pcre_newline.c
copy pcreoutf.c ori_dst\pcre_ord2utf8.c
copy pcreprni.h ori_dst\pcre_printint.src
copy pcrerefc.c ori_dst\pcre_refcount.c
copy pcrestud.c ori_dst\pcre_study.c
copy pcretabs.c ori_dst\pcre_tables.c
copy pcretryf.c ori_dst\pcre_try_flipped.c
copy pcreucd.c  ori_dst\pcre_ucd.c
copy pcrevutf.c ori_dst\pcre_valid_utf8.c
copy pcrever.c  ori_dst\pcre_version.c
copy pcrexcls.c ori_dst\pcre_xclass.c

cd ori_dst

gsar -o -s":x22pcreinal.h:x22" -r":x22pcre_internal.h:x22"   *.*
gsar -o -s":x22ucpinter.h:x22" -r":x22ucpinternal.h:x22"     *.*
gsar -o -s":x22pcreprni.h:x22" -r":x22pcre_printint.src:x22" *.*

cd ..
