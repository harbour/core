@rem
@rem $Id$
@rem

@set HB_USER_LIBS=user32.lib hbct.lib hbbtree.lib

@..\..\..\bin\hbmk %* test.prg ttest.prg ctest.c
