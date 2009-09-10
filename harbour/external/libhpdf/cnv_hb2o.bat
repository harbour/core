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

copy README     ori_dst\README
copy hpdfanno.c ori_dst\hpdf_annotation.c
copy hpdfarra.c ori_dst\hpdf_array.c
copy hpdfbina.c ori_dst\hpdf_binary.c
copy hpdfbool.c ori_dst\hpdf_boolean.c
copy hpdfcata.c ori_dst\hpdf_catalog.c
copy hpdfdest.c ori_dst\hpdf_destination.c
copy hpdfdict.c ori_dst\hpdf_dict.c
copy hpdfdoc.c  ori_dst\hpdf_doc.c
copy hpdfdocp.c ori_dst\hpdf_doc_png.c
copy hpdfenco.c ori_dst\hpdf_encoder.c
copy hpdfencc.c ori_dst\hpdf_encoder_cns.c
copy hpdfencn.c ori_dst\hpdf_encoder_cnt.c
copy hpdfencj.c ori_dst\hpdf_encoder_jp.c
copy hpdfenck.c ori_dst\hpdf_encoder_kr.c
copy hpdfecy.c  ori_dst\hpdf_encrypt.c
copy hpdfecyd.c ori_dst\hpdf_encryptdict.c
copy hpdferro.c ori_dst\hpdf_error.c
copy hpdfextg.c ori_dst\hpdf_ext_gstate.c
copy hpdffont.c ori_dst\hpdf_font.c
copy hpdffonc.c ori_dst\hpdf_font_cid.c
copy hpdffott.c ori_dst\hpdf_font_tt.c
copy hpdffon1.c ori_dst\hpdf_font_type1.c
copy hpdffdf.c  ori_dst\hpdf_fontdef.c
copy hpdffdfb.c ori_dst\hpdf_fontdef_base14.c
copy hpdffdfi.c ori_dst\hpdf_fontdef_cid.c
copy hpdffdfc.c ori_dst\hpdf_fontdef_cns.c
copy hpdffdfn.c ori_dst\hpdf_fontdef_cnt.c
copy hpdffdfj.c ori_dst\hpdf_fontdef_jp.c
copy hpdffdfk.c ori_dst\hpdf_fontdef_kr.c
copy hpdffdft.c ori_dst\hpdf_fontdef_tt.c
copy hpdffdf1.c ori_dst\hpdf_fontdef_type1.c
copy hpdfgsta.c ori_dst\hpdf_gstate.c
copy hpdfimag.c ori_dst\hpdf_image.c
copy hpdfimap.c ori_dst\hpdf_image_png.c
copy hpdfinfo.c ori_dst\hpdf_info.c
copy hpdflist.c ori_dst\hpdf_list.c
copy hpdfmmgr.c ori_dst\hpdf_mmgr.c
copy hpdfname.c ori_dst\hpdf_name.c
copy hpdfnull.c ori_dst\hpdf_null.c
copy hpdfnumb.c ori_dst\hpdf_number.c
copy hpdfobje.c ori_dst\hpdf_objects.c
copy hpdfoutl.c ori_dst\hpdf_outline.c
copy hpdfpage.c ori_dst\hpdf_page_label.c
copy hpdfpago.c ori_dst\hpdf_page_operator.c
copy hpdfpags.c ori_dst\hpdf_pages.c
copy hpdfreal.c ori_dst\hpdf_real.c
copy hpdfstre.c ori_dst\hpdf_streams.c
copy hpdfstri.c ori_dst\hpdf_string.c
copy hpdfu3d.c  ori_dst\hpdf_u3d.c
copy hpdfutil.c ori_dst\hpdf_utils.c
copy hpdfxref.c ori_dst\hpdf_xref.c
copy hpdf.h     ori_dst\hpdf.h
copy hpdfanno.h ori_dst\hpdf_annotation.h
copy hpdfcata.h ori_dst\hpdf_catalog.h
copy hpdfconf.h ori_dst\hpdf_conf.h
copy hpdfcfg.h  ori_dst\hpdf_config.h
copy hpdfcons.h ori_dst\hpdf_consts.h
copy hpdfdest.h ori_dst\hpdf_destination.h
copy hpdfdoc.h  ori_dst\hpdf_doc.h
copy hpdfenco.h ori_dst\hpdf_encoder.h
copy hpdfencr.h ori_dst\hpdf_encrypt.h
copy hpdfency.h ori_dst\hpdf_encryptdict.h
copy hpdferro.h ori_dst\hpdf_error.h
copy hpdfextg.h ori_dst\hpdf_ext_gstate.h
copy hpdffont.h ori_dst\hpdf_font.h
copy hpdffond.h ori_dst\hpdf_fontdef.h
copy hpdfgsta.h ori_dst\hpdf_gstate.h
copy hpdfimag.h ori_dst\hpdf_image.h
copy hpdfinfo.h ori_dst\hpdf_info.h
copy hpdflist.h ori_dst\hpdf_list.h
copy hpdfmmgr.h ori_dst\hpdf_mmgr.h
copy hpdfobje.h ori_dst\hpdf_objects.h
copy hpdfoutl.h ori_dst\hpdf_outline.h
copy hpdfpage.h ori_dst\hpdf_page_label.h
copy hpdfpags.h ori_dst\hpdf_pages.h
copy hpdfstre.h ori_dst\hpdf_streams.h
copy hpdftype.h ori_dst\hpdf_types.h
copy hpdfu3d.h  ori_dst\hpdf_u3d.h
copy hpdfutil.h ori_dst\hpdf_utils.h
copy hpdfvers.h ori_dst\hpdf_version.h

cd ori_dst

gsar -o -s":x22hpdfanno.h:x22" -r":x22hpdf_annotation.h:x22" *.*
gsar -o -s":x22hpdfcata.h:x22" -r":x22hpdf_catalog.h:x22" *.*
gsar -o -s":x22hpdfconf.h:x22" -r":x22hpdf_conf.h:x22" *.*
gsar -o -s":x22hpdfcfg.h:x22"  -r":x22hpdf_config.h:x22" *.*
gsar -o -s":x22hpdfcons.h:x22" -r":x22hpdf_consts.h:x22" *.*
gsar -o -s":x22hpdfdest.h:x22" -r":x22hpdf_destination.h:x22" *.*
gsar -o -s":x22hpdfdoc.h:x22"  -r":x22hpdf_doc.h:x22" *.*
gsar -o -s":x22hpdfenco.h:x22" -r":x22hpdf_encoder.h:x22" *.*
gsar -o -s":x22hpdfencr.h:x22" -r":x22hpdf_encrypt.h:x22" *.*
gsar -o -s":x22hpdfency.h:x22" -r":x22hpdf_encryptdict.h:x22" *.*
gsar -o -s":x22hpdferro.h:x22" -r":x22hpdf_error.h:x22" *.*
gsar -o -s":x22hpdfextg.h:x22" -r":x22hpdf_ext_gstate.h:x22" *.*
gsar -o -s":x22hpdffont.h:x22" -r":x22hpdf_font.h:x22" *.*
gsar -o -s":x22hpdffond.h:x22" -r":x22hpdf_fontdef.h:x22" *.*
gsar -o -s":x22hpdfgsta.h:x22" -r":x22hpdf_gstate.h:x22" *.*
gsar -o -s":x22hpdfimag.h:x22" -r":x22hpdf_image.h:x22" *.*
gsar -o -s":x22hpdfinfo.h:x22" -r":x22hpdf_info.h:x22" *.*
gsar -o -s":x22hpdflist.h:x22" -r":x22hpdf_list.h:x22" *.*
gsar -o -s":x22hpdfmmgr.h:x22" -r":x22hpdf_mmgr.h:x22" *.*
gsar -o -s":x22hpdfobje.h:x22" -r":x22hpdf_objects.h:x22" *.*
gsar -o -s":x22hpdfoutl.h:x22" -r":x22hpdf_outline.h:x22" *.*
gsar -o -s":x22hpdfpage.h:x22" -r":x22hpdf_page_label.h:x22" *.*
gsar -o -s":x22hpdfpags.h:x22" -r":x22hpdf_pages.h:x22" *.*
gsar -o -s":x22hpdfstre.h:x22" -r":x22hpdf_streams.h:x22" *.*
gsar -o -s":x22hpdftype.h:x22" -r":x22hpdf_types.h:x22" *.*
gsar -o -s":x22hpdfu3d.h:x22"  -r":x22hpdf_u3d.h:x22" *.*
gsar -o -s":x22hpdfutil.h:x22" -r":x22hpdf_utils.h:x22" *.*
gsar -o -s":x22hpdfvers.h:x22" -r":x22hpdf_version.h:x22" *.*

cd ..
