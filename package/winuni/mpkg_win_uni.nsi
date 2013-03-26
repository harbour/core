; ---------------------------------------------------------------
; NSIS installer script for Harbour
;
; Copyright 2009 Viktor Szakats (harbour syenar.net)
; See COPYING.txt for licensing terms.
; ---------------------------------------------------------------

; TODO: option: install .dlls to system paths

SetCompressor /solid lzma

  !include "MUI2.nsh"

  !define MUI_HEADERIMAGE
  !define MUI_HEADERIMAGE_BITMAP "${NSISDIR}\Contrib\Graphics\Header\win.bmp"
  !define MUI_FINISHPAGE_SHOWREADME_NOTCHECKED
  !define MUI_FINISHPAGE_SHOWREADME 'notepad.exe "$\"$INSTDIR\README.md$\""'
  !define MUI_ICON "..\harb_win.ico"
  !define MUI_UNICON "${NSISDIR}\Contrib\Graphics\Icons\orange-uninstall.ico"

SetDateSave on
SetDatablockOptimize on
CRCCheck on

RequestExecutionLevel user

!define PKG_NO_CC_MINGW64
!define PKG_NO_CC_MINGWARM
!define PKG_NO_COMP_DJGPP
!define PKG_NO_COMP_POCC
!define PKG_NO_COMP_POCC64
!define PKG_NO_COMP_POCCARM
!define PKG_NO_COMP_BCC64

!define /date NOW "%Y%m%d"

Name "Harbour $%HB_VM%"
OutFile "$%HB_RT%harbour-$%HB_VF%-win.exe"

InstallDir C:\hb$%HB_VS%

;--------------------------------
; Interface Settings

  !define MUI_ABORTWARNING

;--------------------------------
; Pages

  !insertmacro MUI_PAGE_WELCOME
  !insertmacro MUI_PAGE_LICENSE $(MUILicense)
  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_INSTFILES
  !insertmacro MUI_PAGE_FINISH

  !insertmacro MUI_UNPAGE_WELCOME
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  !insertmacro MUI_UNPAGE_FINISH

;--------------------------------
; Languages

  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
; License Language String

LicenseLangString MUILicense ${LANG_ENGLISH} "$%HB_ABSROOT%COPYING.txt"

;--------------------------------
; The stuff to install

Section "Main components" hb_main

  SectionIn RO

  ; Set output path to the installation directory.
  SetOutPath $INSTDIR

  File /nonfatal "$%HB_ABSROOT%RELNOTES.txt"

  File /nonfatal "$%HB_ABSROOT%README.md"
  File /nonfatal "$%HB_ABSROOT%COPYING.txt"
  File /nonfatal "$%HB_ABSROOT%ChangeLog.txt*"

  SetOutPath $INSTDIR\bin
  File "$%HB_ABSROOT%bin\*-$%HB_VS%.dll"
  File "$%HB_ABSROOT%bin\harbour.exe"
  File "$%HB_ABSROOT%bin\hbformat.exe"
  File "$%HB_ABSROOT%bin\hbi18n.exe"
  File "$%HB_ABSROOT%bin\hbmk2.exe"
  File "$%HB_ABSROOT%bin\hbmk2.*.hbl"
  File "$%HB_ABSROOT%bin\hbnetio.exe"
  File "$%HB_ABSROOT%bin\hbpp.exe"
  File "$%HB_ABSROOT%bin\hbrun.exe"
  File "$%HB_ABSROOT%bin\hbtest.exe"
  File /nonfatal "$%HB_ABSROOT%bin\*.hb"
  File /nonfatal "$%HB_ABSROOT%bin\*.hbr"
  File /nonfatal "$%HB_ABSROOT%bin\*.ucf"
  File /nonfatal "$%HB_ABSROOT%bin\hbmk.hbc"
  File "$%HB_ABSROOT%bin\upx*.*"

  SetOutPath $INSTDIR\include
  File "$%HB_ABSROOT%include\*.*"

  SetOutPath $INSTDIR\doc
  File "$%HB_ABSROOT%doc\*.*"

  SetOutPath $INSTDIR\addons
  File "$%HB_ABSROOT%addons\HARBOUR_README_ADDONS.txt"

  ; Write the installation path into the registry
; WriteRegStr HKLM "SOFTWARE\Harbour $%HB_VM%" "InstallDir" "$INSTDIR"
  WriteRegStr HKCU "Software\Harbour $%HB_VM%" "InstallDir" "$INSTDIR"

  ; Write the uninstall keys for Windows
; WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Harbour $%HB_VM%" "DisplayName" "Harbour"
; WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Harbour $%HB_VM%" "UninstallString" '"$INSTDIR\uninstall.exe"'
; WriteRegDWORD HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Harbour $%HB_VM%" "NoModify" 1
; WriteRegDWORD HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Harbour $%HB_VM%" "NoRepair" 1
  WriteUninstaller "uninstall.exe"

SectionEnd

!ifndef PKG_NO_COMP_MINGW64
Section /o "x64 tools" hb_main_x64
  SetOutPath $INSTDIR\bin
  File "$%HB_ABSROOT%bin\hbmk2-x64.exe"
  File "$%HB_ABSROOT%bin\hbnetio-x64.exe"
  File "$%HB_ABSROOT%bin\hbrun-x64.exe"
  File "$%HB_ABSROOT%bin\hbtest-x64.exe"
SectionEnd
!endif

!ifndef PKG_NO_CC_MINGW
Section "MinGW compiler" hb_mingw
  SetOutPath $INSTDIR\comp\mingw
  File /r "$%HB_ABSROOT%comp\mingw\*.*"
SectionEnd
!endif

!ifndef PKG_NO_CC_MINGW64
Section "MinGW x64 compiler" hb_mingw64
  SetOutPath $INSTDIR\comp\mingw64
  File /r "$%HB_ABSROOT%comp\mingw64\*.*"
SectionEnd
!endif

!ifndef PKG_NO_CC_MINGWARM
Section "MinGW WinCE/ARM compiler" hb_mingwarm
  SetOutPath $INSTDIR\comp\mingwarm
  File /r "$%HB_ABSROOT%comp\mingwarm\*.*"
SectionEnd
!endif

Section "Libs for MinGW" hb_lib_mingw
  SectionIn RO
  SetOutPath $INSTDIR\lib\win\mingw
  File "$%HB_ABSROOT%lib\win\mingw\*.*"
  SetOutPath $INSTDIR\comp\mingw
  File "$%HB_ABSROOT%comp\mingw\HARBOUR_README_MINGW.txt"
SectionEnd

!ifndef PKG_NO_COMP_MINGW64
Section /o "Libs for MinGW x64" hb_lib_mingw64
  SetOutPath $INSTDIR\lib\win\mingw64
  File "$%HB_ABSROOT%lib\win\mingw64\*.*"
  SetOutPath $INSTDIR\comp\mingw64
  File "$%HB_ABSROOT%comp\mingw64\HARBOUR_README_MINGW64.txt"
SectionEnd
!endif

!ifndef PKG_NO_COMP_MINGWARM
Section /o "Libs for MinGW WinCE/ARM" hb_lib_mingwarm
  SetOutPath $INSTDIR\lib\wce\mingwarm
  File "$%HB_ABSROOT%lib\wce\mingwarm\*.*"
  SetOutPath $INSTDIR\comp\mingwarm
  File "$%HB_ABSROOT%comp\mingwarm\HARBOUR_README_MINGWARM.txt"
SectionEnd
!endif

!ifndef PKG_NO_COMP_MSVC
Section "Libs for MSVC" hb_lib_msvc
  SetOutPath $INSTDIR\lib\win\msvc
  File "$%HB_ABSROOT%lib\win\msvc\*.*"
SectionEnd
!endif

!ifndef PKG_NO_COMP_MSVC64
Section /o "Libs for MSVC x64" hb_lib_msvc64
  SetOutPath $INSTDIR\lib\win\msvc64
  File "$%HB_ABSROOT%lib\win\msvc64\*.*"
SectionEnd
!endif

!ifndef PKG_NO_COMP_BCC
Section "Libs for Borland C" hb_lib_bcc
  SetOutPath $INSTDIR\bin
  ; File "$%HB_ABSROOT%bin\harbour-$%HB_VS%-bcc.dll"
  SetOutPath $INSTDIR\lib\win\bcc
  File "$%HB_ABSROOT%lib\win\bcc\*.*"
SectionEnd
!endif

!ifndef PKG_NO_COMP_BCC64
Section "Libs for Borland C x64" hb_lib_bcc64
  SetOutPath $INSTDIR\bin
  SetOutPath $INSTDIR\lib\win\bcc64
  File "$%HB_ABSROOT%lib\win\bcc64\*.*"
SectionEnd
!endif

!ifndef PKG_NO_COMP_WATCOM
Section /o "Libs for Open Watcom" hb_lib_watcom
  SetOutPath $INSTDIR\lib\win\watcom
  File "$%HB_ABSROOT%lib\win\watcom\*.*"
  SetOutPath $INSTDIR\comp\watcom
  File "$%HB_ABSROOT%comp\watcom\HARBOUR_README_WATCOM.txt"
SectionEnd
!endif

!ifndef PKG_NO_COMP_POCC
Section /o "Libs for Pelles C" hb_lib_pocc
  SetOutPath $INSTDIR\lib\win\pocc
  File "$%HB_ABSROOT%lib\win\pocc\*.*"
  SetOutPath $INSTDIR\comp\pocc
  File "$%HB_ABSROOT%comp\pocc\HARBOUR_README_POCC.txt"
SectionEnd
!endif

!ifndef PKG_NO_COMP_POCC64
Section /o "Libs for Pelles C x64" hb_lib_pocc64
  SetOutPath $INSTDIR\lib\win\pocc64
  File "$%HB_ABSROOT%lib\win\pocc64\*.*"
SectionEnd
!endif

!ifndef PKG_NO_COMP_POCCARM
Section /o "Libs for Pelles C WinCE/ARM" hb_lib_poccarm
  SetOutPath $INSTDIR\lib\wce\poccarm
  File "$%HB_ABSROOT%lib\wce\poccarm\*.*"
SectionEnd
!endif

!ifndef PKG_NO_PLAT_LINUX
Section /o "Libs for Open Watcom Linux" hb_lib_linux
  SetOutPath $INSTDIR\lib\linux\watcom
  File "$%HB_ABSROOT%lib\linux\watcom\*.*"
  SetOutPath $INSTDIR\comp\watcom
  File "$%HB_ABSROOT%comp\watcom\HARBOUR_README_WATCOM.txt"
SectionEnd
!endif

!ifndef PKG_NO_PLAT_OS2
Section /o "Libs for Open Watcom OS/2" hb_lib_os2
  SetOutPath $INSTDIR\lib\os2\watcom
  File "$%HB_ABSROOT%lib\os2\watcom\*.*"
  SetOutPath $INSTDIR\comp\watcom
  File "$%HB_ABSROOT%comp\watcom\HARBOUR_README_WATCOM.txt"
SectionEnd
!endif

!ifndef PKG_NO_PLAT_DOS
Section /o "Libs for Open Watcom MS-DOS" hb_lib_dos
  SetOutPath $INSTDIR\lib\dos\watcom
  File "$%HB_ABSROOT%lib\dos\watcom\*.*"
  SetOutPath $INSTDIR\comp\watcom
  File "$%HB_ABSROOT%comp\watcom\HARBOUR_README_WATCOM.txt"
SectionEnd
!endif

!ifndef PKG_NO_COMP_DJGPP
Section /o "Libs for DJGPP MS-DOS" hb_lib_djgpp
  SetOutPath $INSTDIR\lib\dos\djgpp
  File "$%HB_ABSROOT%lib\dos\djgpp\*.*"
  SetOutPath $INSTDIR\comp\djgpp
  File "$%HB_ABSROOT%comp\djgpp\HARBOUR_README_DJGPP.txt"
SectionEnd
!endif

!ifndef PKG_NO_COMP_MINGW64
Section /o "Dlls for x64" hb_dlls_x64
  SetOutPath $INSTDIR\bin
  File "$%HB_ABSROOT%bin\*-$%HB_VS%-x64.dll"
SectionEnd
!endif

!ifndef PKG_NO_COMP_MINGWARM
Section /o "Dlls for WinCE/ARM" hb_dlls_arm
  SetOutPath $INSTDIR\bin
  File "$%HB_ABSROOT%bin\harbour-$%HB_VS%-wce-arm.dll"
SectionEnd
!endif

!ifdef _NEVER_
!ifndef PKG_NO_PLAT_OS2
Section /o "Dlls for OS/2" hb_dlls_os2
  SetOutPath $INSTDIR\bin
  ; TOFIX: .dll name collision with MS-DOS
  File "$%HB_ABSROOT%bin\harbour.dll"
SectionEnd
!endif

!ifndef PKG_NO_PLAT_DOS
Section /o "Dlls for MS-DOS" hb_dlls_dos
  SetOutPath $INSTDIR\bin
  ; TOFIX: .dll name collision with OS/2
  File "$%HB_ABSROOT%bin\harbour.dll"
SectionEnd
!endif
!endif

; The stuff to install
Section "Examples" hb_examples
  SetOutPath $INSTDIR\extras
  File /r "$%HB_ABSROOT%extras\*.*"
  SetOutPath $INSTDIR\tests
  File /r "$%HB_ABSROOT%tests\*.*"
  SetOutPath $INSTDIR\contrib
  File /r "$%HB_ABSROOT%contrib\*.*"
SectionEnd

; Optional section (can be disabled by the user)
Section "Start Menu and Desktop icons" hb_shortcuts

  ; this will be the working dir for shortcuts
  SetOutPath $INSTDIR

  CreateShortCut     "$DESKTOP\Harbour $%HB_VM%.lnk" "$INSTDIR" "" "$INSTDIR" 0
  CreateDirectory "$SMPROGRAMS\Harbour $%HB_VM%"
  CreateShortCut  "$SMPROGRAMS\Harbour $%HB_VM%\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0
  CreateShortCut  "$SMPROGRAMS\Harbour $%HB_VM%\Harbour (Command line).lnk" "cmd.exe" "/k cd $INSTDIR\bin" "cmd.exe" 0
  CreateShortCut  "$SMPROGRAMS\Harbour $%HB_VM%\Harbour.lnk" "$INSTDIR" "" "$INSTDIR" 0
  CreateShortCut  "$SMPROGRAMS\Harbour $%HB_VM%\Harbour (Interactive shell).lnk" "$INSTDIR\bin\hbrun.exe" "" "$INSTDIR\bin\hbrun.exe" 0
  CreateDirectory "$SMPROGRAMS\Harbour $%HB_VM%\Links"
  WriteINIStr     "$SMPROGRAMS\Harbour $%HB_VM%\Links\Homepage.url"                   "InternetShortcut" "URL" "http://harbour-project.org/"
  WriteINIStr     "$SMPROGRAMS\Harbour $%HB_VM%\Links\Users' Mailing List.url"        "InternetShortcut" "URL" "http://groups.google.com/group/harbour-users/"
  WriteINIStr     "$SMPROGRAMS\Harbour $%HB_VM%\Links\Developers' Mailing List.url"   "InternetShortcut" "URL" "http://groups.google.com/group/harbour-devel/"
  WriteINIStr     "$SMPROGRAMS\Harbour $%HB_VM%\Links\Development.url"                "InternetShortcut" "URL" "https://github.com/harbour"
  WriteINIStr     "$SMPROGRAMS\Harbour $%HB_VM%\Links\Development Timeline.url"       "InternetShortcut" "URL" "https://github.com/harbour/core/commits/master"

SectionEnd

;--------------------------------
; Descriptions

  ; Language strings
  LangString DESC_hb_main         ${LANG_ENGLISH} "Harbour main components"
  LangString DESC_hb_shortcuts    ${LANG_ENGLISH} "Add icons to Start Menu and Desktop"
  LangString DESC_hb_examples     ${LANG_ENGLISH} "Harbour samples and tests"
!ifndef PKG_NO_COMP_MINGW64
  LangString DESC_hb_main_x64     ${LANG_ENGLISH} "Harbour x64 tools"
  LangString DESC_hb_dlls_x64     ${LANG_ENGLISH} "Harbour dlls for x64"
!endif
!ifndef PKG_NO_COMP_MINGWARM
  LangString DESC_hb_dlls_arm     ${LANG_ENGLISH} "Harbour dlls for WinCE/ARM"
!endif
!ifdef _NEVER_
!ifndef PKG_NO_PLAT_OS2
  LangString DESC_hb_dlls_os2     ${LANG_ENGLISH} "Harbour dlls for OS/2"
!endif
!ifndef PKG_NO_PLAT_DOS
  LangString DESC_hb_dlls_dos     ${LANG_ENGLISH} "Harbour dlls for MS-DOS"
!endif
!endif
!ifndef PKG_NO_CC_MINGW
  LangString DESC_hb_mingw        ${LANG_ENGLISH} "MinGW compiler"
!endif
!ifndef PKG_NO_CC_MINGW64
  LangString DESC_hb_mingw64      ${LANG_ENGLISH} "MinGW x64 compiler"
!endif
!ifndef PKG_NO_CC_MINGWARM
  LangString DESC_hb_mingwarm     ${LANG_ENGLISH} "MinGW WinCE/ARM compiler"
!endif
  LangString DESC_hb_lib_mingw    ${LANG_ENGLISH} "Harbour libs for MinGW"
!ifndef PKG_NO_COMP_MINGW64
  LangString DESC_hb_lib_mingw64  ${LANG_ENGLISH} "Harbour libs for MinGW x64"
!endif
!ifndef PKG_NO_COMP_MINGWARM
  LangString DESC_hb_lib_mingwarm ${LANG_ENGLISH} "Harbour libs for MinGW WinCE/ARM"
!endif
!ifndef PKG_NO_COMP_MSVC
  LangString DESC_hb_lib_msvc     ${LANG_ENGLISH} "Harbour libs for MSVC"
!endif
!ifndef PKG_NO_COMP_MSVC64
  LangString DESC_hb_lib_msvc64   ${LANG_ENGLISH} "Harbour libs for MSVC x64"
!endif
!ifndef PKG_NO_COMP_BCC
  LangString DESC_hb_lib_bcc      ${LANG_ENGLISH} "Harbour libs for Borland C"
!endif
!ifndef PKG_NO_COMP_BCC64
  LangString DESC_hb_lib_bcc64    ${LANG_ENGLISH} "Harbour libs for Borland C x64"
!endif
!ifndef PKG_NO_COMP_WATCOM
  LangString DESC_hb_lib_watcom   ${LANG_ENGLISH} "Harbour libs for Open Watcom"
!endif
!ifndef PKG_NO_COMP_POCC
  LangString DESC_hb_lib_pocc     ${LANG_ENGLISH} "Harbour libs for Pelles C"
!endif
!ifndef PKG_NO_COMP_POCC64
  LangString DESC_hb_lib_pocc64   ${LANG_ENGLISH} "Harbour libs for Pelles C x64"
!endif
!ifndef PKG_NO_COMP_POCCARM
  LangString DESC_hb_lib_poccarm  ${LANG_ENGLISH} "Harbour libs for Pelles C WinCE/ARM"
!endif
!ifndef PKG_NO_PLAT_LINUX
  LangString DESC_hb_lib_linux    ${LANG_ENGLISH} "Harbour libs for Open Watcom Linux"
!endif
!ifndef PKG_NO_PLAT_OS2
  LangString DESC_hb_lib_os2      ${LANG_ENGLISH} "Harbour libs for Open Watcom OS/2"
!endif
!ifndef PKG_NO_PLAT_DOS
  LangString DESC_hb_lib_dos      ${LANG_ENGLISH} "Harbour libs for Open Watcom MS-DOS"
!endif
!ifndef PKG_NO_COMP_DJGPP
  LangString DESC_hb_lib_djgpp    ${LANG_ENGLISH} "Harbour libs for DJGPP MS-DOS"
!endif

  ; Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_main}         $(DESC_hb_main)
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_shortcuts}    $(DESC_hb_shortcuts)
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_examples}     $(DESC_hb_examples)
!ifndef PKG_NO_COMP_MINGW64
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_main_x64}     $(DESC_hb_main_x64)
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_dlls_x64}     $(DESC_hb_dlls_x64)
!endif
!ifndef PKG_NO_COMP_MINGWARM
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_dlls_arm}     $(DESC_hb_dlls_arm)
!endif
!ifndef PKG_NO_CC_MINGW
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_mingw}        $(DESC_hb_mingw)
!endif
!ifndef PKG_NO_CC_MINGW64
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_mingw64}      $(DESC_hb_mingw64)
!endif
!ifndef PKG_NO_CC_MINGWARM
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_mingwarm}     $(DESC_hb_mingwarm)
!endif
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_mingw}    $(DESC_hb_lib_mingw)
!ifndef PKG_NO_COMP_MINGW64
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_mingw64}  $(DESC_hb_lib_mingw64)
!endif
!ifndef PKG_NO_COMP_MINGWARM
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_mingwarm} $(DESC_hb_lib_mingwarm)
!endif
!ifndef PKG_NO_COMP_MSVC
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_msvc}     $(DESC_hb_lib_msvc)
!endif
!ifndef PKG_NO_COMP_MSVC64
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_msvc64}   $(DESC_hb_lib_msvc64)
!endif
!ifndef PKG_NO_COMP_BCC
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_bcc}      $(DESC_hb_lib_bcc)
!endif
!ifndef PKG_NO_COMP_BCC64
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_bcc64}    $(DESC_hb_lib_bcc64)
!endif
!ifndef PKG_NO_COMP_WATCOM
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_watcom}   $(DESC_hb_lib_watcom)
!endif
!ifndef PKG_NO_COMP_POCC
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_pocc}     $(DESC_hb_lib_pocc)
!endif
!ifndef PKG_NO_COMP_POCC64
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_pocc64}   $(DESC_hb_lib_pocc64)
!endif
!ifndef PKG_NO_COMP_POCCARM
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_poccarm}  $(DESC_hb_lib_poccarm)
!endif
!ifndef PKG_NO_PLAT_LINUX
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_linux}    $(DESC_hb_lib_linux)
!endif
!ifndef PKG_NO_PLAT_OS2
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_os2}      $(DESC_hb_lib_os2)
!endif
!ifndef PKG_NO_PLAT_DOS
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_dos}      $(DESC_hb_lib_dos)
!endif
!ifndef PKG_NO_COMP_DJGPP
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_djgpp}    $(DESC_hb_lib_djgpp)
!endif
  !insertmacro MUI_FUNCTION_DESCRIPTION_END

;--------------------------------
; Uninstaller

Section "Uninstall"

  ; Remove files and uninstaller
  RMDir /r $INSTDIR

  ; Remove directories used
  Delete "$SMPROGRAMS\Harbour $%HB_VM%\Links\*.*"
  RMDir  "$SMPROGRAMS\Harbour $%HB_VM%\Links"
  Delete "$SMPROGRAMS\Harbour $%HB_VM%\*.*"
  RMDir  "$SMPROGRAMS\Harbour $%HB_VM%"
  RMDir  "$INSTDIR"

  Delete "$DESKTOP\Harbour $%HB_VM%.lnk"

; DeleteRegKey HKLM "SOFTWARE\Harbour $%HB_VM%"
  DeleteRegKey HKCU "Software\Harbour $%HB_VM%"

SectionEnd
