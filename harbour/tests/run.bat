@rem
@rem $Id$
@rem

@echo off

..\bin\harbour %1 -n -i..\include -gh
..\bin\hbrun %1
del %1.hrb
