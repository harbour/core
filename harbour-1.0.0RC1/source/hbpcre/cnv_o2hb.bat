@echo off
rem
rem $Id$
rem

rem Tested with PCRE 7.7

rem NOTE:       Purpose of this script is to take the original 
rem             PCRE file from its source distribution and convert 
rem             them to the short filenames we use here in Harbour.
rem             Short filenames are needed for full DJGPP support.
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

attrib +R _hbconf.h
attrib +R _hbpcreg.c
del *.c
del *.h
attrib -R _hbconf.h
attrib -R _hbpcreg.c

copy ori_src\LICENSE                LICENSE
copy ori_src\config.h.generic       config.h
copy ori_src\pcre.h.generic         pcre.h
copy ori_src\pcre_internal.h        pcreinal.h
copy ori_src\ucp.h                  ucp.h
copy ori_src\ucpinternal.h          ucpinter.h
copy ori_src\ucptable.h             ucptable.h
copy ori_src\pcre_chartables.c.dist chartabs.c
copy ori_src\pcre_compile.c         pcrecomp.c
copy ori_src\pcre_config.c          pcreconf.c
copy ori_src\pcre_dfa_exec.c        pcredfa.c
copy ori_src\pcre_exec.c            pcreexec.c
copy ori_src\pcre_fullinfo.c        pcrefinf.c
copy ori_src\pcre_get.c             pcreget.c
copy ori_src\pcre_info.c            pcreinfo.c
copy ori_src\pcre_maketables.c      pcremktb.c
copy ori_src\pcre_newline.c         pcrenewl.c
copy ori_src\pcre_ord2utf8.c        pcreoutf.c
copy ori_src\pcre_printint.src      pcreprni.h
copy ori_src\pcre_refcount.c        pcrerefc.c
copy ori_src\pcre_study.c           pcrestud.c
copy ori_src\pcre_tables.c          pcretabs.c
copy ori_src\pcre_try_flipped.c     pcretryf.c
copy ori_src\pcre_ucp_searchfuncs.c pcrefind.c
copy ori_src\pcre_valid_utf8.c      pcrevutf.c
copy ori_src\pcre_version.c         pcrever.c
copy ori_src\pcre_xclass.c          pcrexcls.c

unix2dos *.c
unix2dos *.h

gsar -o -s":x22pcre_printint.src:x22" -r":x22pcreprni.h:x22" *.c
gsar -o -s":x22pcre_printint.src:x22" -r":x22pcreprni.h:x22" *.h
gsar -o -s":x22pcre_internal.h:x22"   -r":x22pcreinal.h:x22" *.c
gsar -o -s":x22pcre_internal.h:x22"   -r":x22pcreinal.h:x22" *.h
gsar -o -s":x22ucpinternal.h:x22"     -r":x22ucpinter.h:x22" *.c
gsar -o -s":x22ucpinternal.h:x22"     -r":x22ucpinter.h:x22" *.h
gsar -o -s":x22config.h:x22"          -r":x22_hbconf.h:x22"  *.c
gsar -o -s":x22config.h:x22"          -r":x22_hbconf.h:x22"  *.h
gsar -o -s":x22_hbconf.h:x22"         -r":x22config.h:x22"   _hbconf.h
gsar -o -s"ifdef HAVE_CONFIG_H"       -r"if 2875"            *.c
