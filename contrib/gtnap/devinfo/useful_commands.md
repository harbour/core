win-make

// Makefiles logs
$(info JAJAJAJAJ!!!!)
$(info ! HB_BUILD_PARTS: $(HB_BUILD_PARTS))

cd tests

..\bin\win\msvc\hbmk2 browse.prg
browse

..\bin\win\msvc\hbmk2 browse.prg -gtwin
browse


..\bin\win\msvc\hbmk2 browse.prg -gtwin -gtstd
browse --hb:gtwin
browse --hb:gtdos

..\bin\win\msvc\hbmk2 browse.prg -gtwin -gtstd

https://github.com/harbour/core/blob/master/doc/gtapi.txt

..\bin\win\msvc\hbmk2 browse.prg -gtwin -gtwvt
browse --hb:gtwvt

Compile gtwvw
=============
gtwvw not compile with the project Makefile
Should be compiled apart
https://groups.google.com/g/harbour-users/c/VXw0fKEgLHc

cd contrib\gtwvw

..\..\bin\win\msvc\hbmk2.exe gtwvw.hbp

This will generate gtwvw.lib in lib\win\msvc

cd contril\gtwvw\tests
..\..\..\bin\win\msvc\hbmk2.exe drawimg.prg -gtwvw
drawimg

..\..\..\bin\win\msvc\hbmk2.exe demo.prg -gtwvw
demo

https://github.com/harbour/core/blob/master/utils/hbmk2/doc/hbmk2.en.md


cd contrib\gtwvg
..\..\bin\win\msvc\hbmk2.exe gtwvg.hbp
// Will generate gtwvg.lib in contrib\gtwvg



cd contrib\gtnap

..\..\bin\win\msvc\hbmk2.exe gtnap.hbp
cd contrib\gtnap\tests
..\..\..\bin\win\msvc\hbmk2.exe drawimg.prg -L..\lib\win\msvc -gtnap
drawimg

 _HB_FUN_HB_GT_WVW

 hb_lnk_ForceLink_hbmk

 HB_GT_WVW()