@echo off
rem
rem $Id$
rem

rem Tested with PCRE 7.7

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

copy ori_src\README                 README
copy ori_src\hpdf_annotation.c      hpdfanno.c
copy ori_src\hpdf_array.c           hpdfarra.c
copy ori_src\hpdf_binary.c          hpdfbina.c
copy ori_src\hpdf_boolean.c         hpdfbool.c
copy ori_src\hpdf_catalog.c         hpdfcata.c
copy ori_src\hpdf_destination.c     hpdfdest.c
copy ori_src\hpdf_dict.c            hpdfdict.c
copy ori_src\hpdf_doc.c             hpdfdoc.c
copy ori_src\hpdf_doc_png.c         hpdfdocp.c
copy ori_src\hpdf_encoder.c         hpdfenco.c
copy ori_src\hpdf_encoder_cns.c     hpdfencc.c
copy ori_src\hpdf_encoder_cnt.c     hpdfencn.c
copy ori_src\hpdf_encoder_jp.c      hpdfencj.c
copy ori_src\hpdf_encoder_kr.c      hpdfenck.c
copy ori_src\hpdf_encrypt.c         hpdfecy.c
copy ori_src\hpdf_encryptdict.c     hpdfecyd.c
copy ori_src\hpdf_error.c           hpdferro.c
copy ori_src\hpdf_ext_gstate.c      hpdfextg.c
copy ori_src\hpdf_font.c            hpdffont.c
copy ori_src\hpdf_font_cid.c        hpdffonc.c
copy ori_src\hpdf_font_tt.c         hpdffott.c
copy ori_src\hpdf_font_type1.c      hpdffon1.c
copy ori_src\hpdf_fontdef.c         hpdffdf.c
copy ori_src\hpdf_fontdef_base14.c  hpdffdfb.c
copy ori_src\hpdf_fontdef_cid.c     hpdffdfi.c
copy ori_src\hpdf_fontdef_cns.c     hpdffdfc.c
copy ori_src\hpdf_fontdef_cnt.c     hpdffdfn.c
copy ori_src\hpdf_fontdef_jp.c      hpdffdfj.c
copy ori_src\hpdf_fontdef_kr.c      hpdffdfk.c
copy ori_src\hpdf_fontdef_tt.c      hpdffdft.c
copy ori_src\hpdf_fontdef_type1.c   hpdffdf1.c
copy ori_src\hpdf_gstate.c          hpdfgsta.c
copy ori_src\hpdf_image.c           hpdfimag.c
copy ori_src\hpdf_image_png.c       hpdfimap.c
copy ori_src\hpdf_info.c            hpdfinfo.c
copy ori_src\hpdf_list.c            hpdflist.c
copy ori_src\hpdf_mmgr.c            hpdfmmgr.c
copy ori_src\hpdf_name.c            hpdfname.c
copy ori_src\hpdf_null.c            hpdfnull.c
copy ori_src\hpdf_number.c          hpdfnumb.c
copy ori_src\hpdf_objects.c         hpdfobje.c
copy ori_src\hpdf_outline.c         hpdfoutl.c
copy ori_src\hpdf_page_label.c      hpdfpage.c
copy ori_src\hpdf_page_operator.c   hpdfpago.c
copy ori_src\hpdf_pages.c           hpdfpags.c
copy ori_src\hpdf_real.c            hpdfreal.c
copy ori_src\hpdf_streams.c         hpdfstre.c
copy ori_src\hpdf_string.c          hpdfstri.c
copy ori_src\hpdf_u3d.c             hpdfu3d.c
copy ori_src\hpdf_utils.c           hpdfutil.c
copy ori_src\hpdf_xref.c            hpdfxref.c
copy ori_src\hpdf.h                 hpdf.h
copy ori_src\hpdf_annotation.h      hpdfanno.h
copy ori_src\hpdf_catalog.h         hpdfcata.h
copy ori_src\hpdf_conf.h            hpdfconf.h
copy ori_src\hpdf_config.h          hpdfcfg.h
copy ori_src\hpdf_consts.h          hpdfcons.h
copy ori_src\hpdf_destination.h     hpdfdest.h
copy ori_src\hpdf_doc.h             hpdfdoc.h
copy ori_src\hpdf_encoder.h         hpdfenco.h
copy ori_src\hpdf_encrypt.h         hpdfencr.h
copy ori_src\hpdf_encryptdict.h     hpdfency.h
copy ori_src\hpdf_error.h           hpdferro.h
copy ori_src\hpdf_ext_gstate.h      hpdfextg.h
copy ori_src\hpdf_font.h            hpdffont.h
copy ori_src\hpdf_fontdef.h         hpdffond.h
copy ori_src\hpdf_gstate.h          hpdfgsta.h
copy ori_src\hpdf_image.h           hpdfimag.h
copy ori_src\hpdf_info.h            hpdfinfo.h
copy ori_src\hpdf_list.h            hpdflist.h
copy ori_src\hpdf_mmgr.h            hpdfmmgr.h
copy ori_src\hpdf_objects.h         hpdfobje.h
copy ori_src\hpdf_outline.h         hpdfoutl.h
copy ori_src\hpdf_page_label.h      hpdfpage.h
copy ori_src\hpdf_pages.h           hpdfpags.h
copy ori_src\hpdf_streams.h         hpdfstre.h
copy ori_src\hpdf_types.h           hpdftype.h
copy ori_src\hpdf_u3d.h             hpdfu3d.h
copy ori_src\hpdf_utils.h           hpdfutil.h
copy ori_src\hpdf_version.h         hpdfvers.h

unix2dos *.c
unix2dos *.h

gsar -o -s":x22hpdf_annotation.h:x22"  -r":x22hpdfanno.h:x22" *.c
gsar -o -s":x22hpdf_catalog.h:x22"     -r":x22hpdfcata.h:x22" *.c
gsar -o -s":x22hpdf_conf.h:x22"        -r":x22hpdfconf.h:x22" *.c
gsar -o -s":x22hpdf_config.h:x22"      -r":x22hpdfcfg.h:x22"  *.c
gsar -o -s":x22hpdf_consts.h:x22"      -r":x22hpdfcons.h:x22" *.c
gsar -o -s":x22hpdf_destination.h:x22" -r":x22hpdfdest.h:x22" *.c
gsar -o -s":x22hpdf_doc.h:x22"         -r":x22hpdfdoc.h:x22"  *.c
gsar -o -s":x22hpdf_encoder.h:x22"     -r":x22hpdfenco.h:x22" *.c
gsar -o -s":x22hpdf_encrypt.h:x22"     -r":x22hpdfencr.h:x22" *.c
gsar -o -s":x22hpdf_encryptdict.h:x22" -r":x22hpdfency.h:x22" *.c
gsar -o -s":x22hpdf_error.h:x22"       -r":x22hpdferro.h:x22" *.c
gsar -o -s":x22hpdf_ext_gstate.h:x22"  -r":x22hpdfextg.h:x22" *.c
gsar -o -s":x22hpdf_font.h:x22"        -r":x22hpdffont.h:x22" *.c
gsar -o -s":x22hpdf_fontdef.h:x22"     -r":x22hpdffond.h:x22" *.c
gsar -o -s":x22hpdf_gstate.h:x22"      -r":x22hpdfgsta.h:x22" *.c
gsar -o -s":x22hpdf_image.h:x22"       -r":x22hpdfimag.h:x22" *.c
gsar -o -s":x22hpdf_info.h:x22"        -r":x22hpdfinfo.h:x22" *.c
gsar -o -s":x22hpdf_list.h:x22"        -r":x22hpdflist.h:x22" *.c
gsar -o -s":x22hpdf_mmgr.h:x22"        -r":x22hpdfmmgr.h:x22" *.c
gsar -o -s":x22hpdf_objects.h:x22"     -r":x22hpdfobje.h:x22" *.c
gsar -o -s":x22hpdf_outline.h:x22"     -r":x22hpdfoutl.h:x22" *.c
gsar -o -s":x22hpdf_page_label.h:x22"  -r":x22hpdfpage.h:x22" *.c
gsar -o -s":x22hpdf_pages.h:x22"       -r":x22hpdfpags.h:x22" *.c
gsar -o -s":x22hpdf_streams.h:x22"     -r":x22hpdfstre.h:x22" *.c
gsar -o -s":x22hpdf_types.h:x22"       -r":x22hpdftype.h:x22" *.c
gsar -o -s":x22hpdf_u3d.h:x22"         -r":x22hpdfu3d.h:x22"  *.c
gsar -o -s":x22hpdf_utils.h:x22"       -r":x22hpdfutil.h:x22" *.c
gsar -o -s":x22hpdf_version.h:x22"     -r":x22hpdfvers.h:x22" *.c

gsar -o -s":x22hpdf_annotation.h:x22"  -r":x22hpdfanno.h:x22" *.h
gsar -o -s":x22hpdf_catalog.h:x22"     -r":x22hpdfcata.h:x22" *.h
gsar -o -s":x22hpdf_conf.h:x22"        -r":x22hpdfconf.h:x22" *.h
gsar -o -s":x22hpdf_config.h:x22"      -r":x22hpdfcfg.h:x22"  *.h
gsar -o -s":x22hpdf_consts.h:x22"      -r":x22hpdfcons.h:x22" *.h
gsar -o -s":x22hpdf_destination.h:x22" -r":x22hpdfdest.h:x22" *.h
gsar -o -s":x22hpdf_doc.h:x22"         -r":x22hpdfdoc.h:x22"  *.h
gsar -o -s":x22hpdf_encoder.h:x22"     -r":x22hpdfenco.h:x22" *.h
gsar -o -s":x22hpdf_encrypt.h:x22"     -r":x22hpdfencr.h:x22" *.h
gsar -o -s":x22hpdf_encryptdict.h:x22" -r":x22hpdfency.h:x22" *.h
gsar -o -s":x22hpdf_error.h:x22"       -r":x22hpdferro.h:x22" *.h
gsar -o -s":x22hpdf_ext_gstate.h:x22"  -r":x22hpdfextg.h:x22" *.h
gsar -o -s":x22hpdf_font.h:x22"        -r":x22hpdffont.h:x22" *.h
gsar -o -s":x22hpdf_fontdef.h:x22"     -r":x22hpdffond.h:x22" *.h
gsar -o -s":x22hpdf_gstate.h:x22"      -r":x22hpdfgsta.h:x22" *.h
gsar -o -s":x22hpdf_image.h:x22"       -r":x22hpdfimag.h:x22" *.h
gsar -o -s":x22hpdf_info.h:x22"        -r":x22hpdfinfo.h:x22" *.h
gsar -o -s":x22hpdf_list.h:x22"        -r":x22hpdflist.h:x22" *.h
gsar -o -s":x22hpdf_mmgr.h:x22"        -r":x22hpdfmmgr.h:x22" *.h
gsar -o -s":x22hpdf_objects.h:x22"     -r":x22hpdfobje.h:x22" *.h
gsar -o -s":x22hpdf_outline.h:x22"     -r":x22hpdfoutl.h:x22" *.h
gsar -o -s":x22hpdf_page_label.h:x22"  -r":x22hpdfpage.h:x22" *.h
gsar -o -s":x22hpdf_pages.h:x22"       -r":x22hpdfpags.h:x22" *.h
gsar -o -s":x22hpdf_streams.h:x22"     -r":x22hpdfstre.h:x22" *.h
gsar -o -s":x22hpdf_types.h:x22"       -r":x22hpdftype.h:x22" *.h
gsar -o -s":x22hpdf_u3d.h:x22"         -r":x22hpdfu3d.h:x22"  *.h
gsar -o -s":x22hpdf_utils.h:x22"       -r":x22hpdfutil.h:x22" *.h
gsar -o -s":x22hpdf_version.h:x22"     -r":x22hpdfvers.h:x22" *.h
