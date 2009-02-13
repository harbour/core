@rem
@rem $Id$
@rem

@set HB_USER_LIBS=gtwvg.lib hbwin.lib comdlg32.lib comctl32.lib shell32.lib ole32.lib oleaut32.lib xhb.lib

@..\..\..\bin\%~nx0 -mt -gui %* trm_appn.prg terminal.prg
