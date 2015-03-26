;
; NSIS installer script for Harbour
;
; Copyright 2009-2015 Viktor Szakats (vszakats.net/harbour)
; See COPYING.txt for licensing terms.
;

; Requires these envvars to be set:
; - HB_ABSROOT
; - HB_VS
; - HB_VM
; - HB_RT

!ifdef NSIS_PACKEDVERSION
  Unicode True
  ManifestSupportedOS all
  ManifestDPIAware true
!endif
SetCompressor /solid lzma

  !include "MUI2.nsh"

  !define MUI_HEADERIMAGE
  !define MUI_HEADERIMAGE_BITMAP "${NSISDIR}\Contrib\Graphics\Header\win.bmp"
  !define MUI_FINISHPAGE_SHOWREADME_NOTCHECKED
  !define MUI_FINISHPAGE_SHOWREADME 'notepad.exe "$\"$INSTDIR\README.md$\""'
  !define MUI_ICON "..\harbour.ico"
  !define MUI_UNICON "${NSISDIR}\Contrib\Graphics\Icons\orange-uninstall.ico"

SetDateSave on
SetDatablockOptimize on
CRCCheck on

RequestExecutionLevel user

; See http://nsis.sourceforge.net/Check_if_a_file_exists_at_compile_time for documentation
!macro !defineifexist _VAR_NAME _FILE_NAME
  !tempfile _TEMPFILE
  !ifdef NSIS_WIN32_MAKENSIS
    ; Windows - cmd.exe
    !system 'if exist "${_FILE_NAME}" echo !define ${_VAR_NAME} > "${_TEMPFILE}"'
  !else
    ; Posix - sh
    !system 'if [ -e "${_FILE_NAME}" ]; then echo "!define ${_VAR_NAME}" > "${_TEMPFILE}"; fi'
  !endif
  !include '${_TEMPFILE}'
  !delfile '${_TEMPFILE}'
  !undef _TEMPFILE
!macroend
!define !defineifexist "!insertmacro !defineifexist"

; if NSIS 3.0 or upper
!ifdef NSIS_PACKEDVERSION

; C compilers
!if /FileExists "$%HB_ABSROOT%comp\mingw\*.*"
  !define PKG_CC_MINGW
!endif
!if /FileExists "$%HB_ABSROOT%comp\mingwarm\*.*"
  !define PKG_CC_MINGWARM
!endif
!if /FileExists "$%HB_ABSROOT%comp\djgpp\*.*"
  !define PKG_CC_DJGPP
!endif
!if /FileExists "$%HB_ABSROOT%comp\pocc\*.*"
  !define PKG_CC_POCC
!endif
!if /FileExists "$%HB_ABSROOT%comp\watcom\*.*"
  !define PKG_CC_WATCOM
!endif

; Harbour support for C compilers
!if /FileExists "$%HB_ABSROOT%lib\win\bcc\*.*"
  !define PKG_COMP_BCC
!endif
!if /FileExists "$%HB_ABSROOT%lib\win\bcc64\*.*"
  !define PKG_COMP_BCC64
!endif
!if /FileExists "$%HB_ABSROOT%lib\dos\djgpp\*.*"
  !define PKG_COMP_DJGPP
!endif
!if /FileExists "$%HB_ABSROOT%lib\win\mingw\*.*"
  !define PKG_COMP_MINGW
!endif
!if /FileExists "$%HB_ABSROOT%lib\win\mingw64\*.*"
  !define PKG_COMP_MINGW64
!endif
!if /FileExists "$%HB_ABSROOT%lib\wce\mingwarm\*.*"
  !define PKG_COMP_MINGWARM
!endif
!if /FileExists "$%HB_ABSROOT%lib\win\msvc\*.*"
  !define PKG_COMP_MSVC
!endif
!if /FileExists "$%HB_ABSROOT%lib\win\msvc64\*.*"
  !define PKG_COMP_MSVC64
!endif
!if /FileExists "$%HB_ABSROOT%lib\win\pocc\*.*"
  !define PKG_COMP_POCC
!endif
!if /FileExists "$%HB_ABSROOT%lib\win\pocc64\*.*"
  !define PKG_COMP_POCC64
!endif
!if /FileExists "$%HB_ABSROOT%lib\wce\poccarm\*.*"
  !define PKG_COMP_POCCARM
!endif
!if /FileExists "$%HB_ABSROOT%lib\win\watcom\*.*"
  !define PKG_COMP_WATCOM
!endif
!if /FileExists "$%HB_ABSROOT%lib\dos\watcom\*.*"
  !define PKG_PLAT_DOS
!endif
!if /FileExists "$%HB_ABSROOT%lib\linux\watcom\*.*"
  !define PKG_PLAT_LINUX
!endif
!if /FileExists "$%HB_ABSROOT%lib\os2\watcom\*.*"
  !define PKG_PLAT_OS2
!endif

!else
; C compilers
${!defineifexist} PKG_CC_MINGW      "$%HB_ABSROOT%comp\mingw\*.*"
${!defineifexist} PKG_CC_MINGWARM   "$%HB_ABSROOT%comp\mingwarm\*.*"
${!defineifexist} PKG_CC_DJGPP      "$%HB_ABSROOT%comp\djgpp\*.*"
${!defineifexist} PKG_CC_POCC       "$%HB_ABSROOT%comp\pocc\*.*"
${!defineifexist} PKG_CC_WATCOM     "$%HB_ABSROOT%comp\watcom\*.*"

; Harbour support for C compilers
${!defineifexist} PKG_COMP_BCC      "$%HB_ABSROOT%lib\win\bcc\*.*"
${!defineifexist} PKG_COMP_BCC64    "$%HB_ABSROOT%lib\win\bcc64\*.*"
${!defineifexist} PKG_COMP_DJGPP    "$%HB_ABSROOT%lib\dos\djgpp\*.*"
${!defineifexist} PKG_COMP_MINGW    "$%HB_ABSROOT%lib\win\mingw\*.*"
${!defineifexist} PKG_COMP_MINGW64  "$%HB_ABSROOT%lib\win\mingw64\*.*"
${!defineifexist} PKG_COMP_MINGWARM "$%HB_ABSROOT%lib\wce\mingwarm\*.*"
${!defineifexist} PKG_COMP_MSVC     "$%HB_ABSROOT%lib\win\msvc\*.*"
${!defineifexist} PKG_COMP_MSVC64   "$%HB_ABSROOT%lib\win\msvc64\*.*"
${!defineifexist} PKG_COMP_POCC     "$%HB_ABSROOT%lib\win\pocc\*.*"
${!defineifexist} PKG_COMP_POCC64   "$%HB_ABSROOT%lib\win\pocc64\*.*"
${!defineifexist} PKG_COMP_POCCARM  "$%HB_ABSROOT%lib\wce\poccarm\*.*"
${!defineifexist} PKG_COMP_WATCOM   "$%HB_ABSROOT%lib\win\watcom\*.*"
${!defineifexist} PKG_PLAT_DOS      "$%HB_ABSROOT%lib\dos\watcom\*.*"
${!defineifexist} PKG_PLAT_LINUX    "$%HB_ABSROOT%lib\linux\watcom\*.*"
${!defineifexist} PKG_PLAT_OS2      "$%HB_ABSROOT%lib\os2\watcom\*.*"
!endif

!define /date NOW "%Y%m%d"

Name "Harbour $%HB_VM%"
OutFile "$%HB_RT%harbour-$%HB_VF%-win.exe"

InstallDir C:\hb$%HB_VS%

  ; Interface Settings
  !define MUI_ABORTWARNING

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

  ; Languages
  !insertmacro MUI_LANGUAGE "English"

; License Language String
LicenseLangString MUILicense ${LANG_ENGLISH} "$%HB_ABSROOT%COPYING.txt"

; The stuff to install
Section "Main components" hb_main

  SectionIn RO

  ; Set output path to the installation directory.
  SetOutPath $INSTDIR

  File "$%HB_ABSROOT%ChangeLog*.txt"
  File "$%HB_ABSROOT%CONTRIBUTING.md"
  File "$%HB_ABSROOT%COPYING.txt"
  File "$%HB_ABSROOT%README.md"
  File "$%HB_ABSROOT%RELNOTES.txt"

  SetOutPath $INSTDIR\bin
  File "$%HB_ABSROOT%bin\harbour.exe"
  File "$%HB_ABSROOT%bin\hbi18n.exe"
  File "$%HB_ABSROOT%bin\hbmk2.exe"
  File "$%HB_ABSROOT%bin\hbpp.exe"
  File "$%HB_ABSROOT%bin\hbrun.exe"
  File "$%HB_ABSROOT%bin\hbspeed.exe"
  File "$%HB_ABSROOT%bin\hbtest.exe"

  File /nonfatal "$%HB_ABSROOT%bin\hbformat.exe"
  File /nonfatal "$%HB_ABSROOT%bin\hbnetio.exe"
  File /nonfatal "$%HB_ABSROOT%bin\*.hb"

  File "$%HB_ABSROOT%bin\upx*.*"

  SetOutPath $INSTDIR\include
  File "$%HB_ABSROOT%include\*.*"

  SetOutPath $INSTDIR\doc
  File "$%HB_ABSROOT%doc\*.*"

  SetOutPath $INSTDIR\addons
  File "$%HB_ABSROOT%addons\README.txt"

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

!ifdef PKG_CC_MINGW
Section "MinGW compiler" hb_cc_mingw
  SetOutPath $INSTDIR\comp\mingw
  File /r "$%HB_ABSROOT%comp\mingw\*.*"
SectionEnd
!endif

!ifdef PKG_CC_MINGWARM
Section "MinGW WinCE/ARM compiler" hb_cc_mingwarm
  SetOutPath $INSTDIR\comp\mingwarm
  File /r "$%HB_ABSROOT%comp\mingwarm\*.*"
SectionEnd
!endif

!ifdef PKG_CC_DJGPP
Section "DJGPP MS-DOS compiler" hb_cc_djgpp
  SetOutPath $INSTDIR\comp\djgpp
  File /r "$%HB_ABSROOT%comp\djgpp\*.*"
SectionEnd
!endif

!ifdef PKG_CC_POCC
Section "Pelles C compiler" hb_cc_pocc
  SetOutPath $INSTDIR\comp\pocc
  File /r "$%HB_ABSROOT%comp\pocc\*.*"
SectionEnd
!endif

!ifdef PKG_CC_WATCOM
Section "Watcom C compiler" hb_cc_watcom
  SetOutPath $INSTDIR\comp\watcom
  File /r "$%HB_ABSROOT%comp\watcom\*.*"
SectionEnd
!endif

!ifdef PKG_COMP_MINGW
Section "Libs for MinGW" hb_lib_mingw
  SectionIn RO
  SetOutPath $INSTDIR\lib\win\mingw
  File "$%HB_ABSROOT%lib\win\mingw\*.*"
  SetOutPath $INSTDIR\comp\mingw
  File "$%HB_ABSROOT%comp\mingw\HARBOUR_README_MINGW.txt"
SectionEnd
!endif

!ifdef PKG_COMP_MINGW64
Section /o "Libs for MinGW x64" hb_lib_mingw64
  SetOutPath $INSTDIR\lib\win\mingw64
  File "$%HB_ABSROOT%lib\win\mingw64\*.*"
SectionEnd
!endif

!ifdef PKG_COMP_MINGWARM
Section /o "Libs for MinGW WinCE/ARM" hb_lib_mingwarm
  SetOutPath $INSTDIR\lib\wce\mingwarm
  File "$%HB_ABSROOT%lib\wce\mingwarm\*.*"
  SetOutPath $INSTDIR\comp\mingwarm
  File "$%HB_ABSROOT%comp\mingwarm\HARBOUR_README_MINGWARM.txt"
SectionEnd
!endif

!ifdef PKG_COMP_MSVC
Section "Libs for MSVC" hb_lib_msvc
  SetOutPath $INSTDIR\lib\win\msvc
  File "$%HB_ABSROOT%lib\win\msvc\*.*"
SectionEnd
!endif

!ifdef PKG_COMP_MSVC64
Section /o "Libs for MSVC x64" hb_lib_msvc64
  SetOutPath $INSTDIR\lib\win\msvc64
  File "$%HB_ABSROOT%lib\win\msvc64\*.*"
SectionEnd
!endif

!ifdef PKG_COMP_BCC
Section "Libs for Borland C" hb_lib_bcc
  SetOutPath $INSTDIR\bin
  ; File "$%HB_ABSROOT%bin\harbour-$%HB_VS%-bcc.dll"
  SetOutPath $INSTDIR\lib\win\bcc
  File "$%HB_ABSROOT%lib\win\bcc\*.*"
SectionEnd
!endif

!ifdef PKG_COMP_BCC64
Section "Libs for Borland C x64" hb_lib_bcc64
  SetOutPath $INSTDIR\bin
  SetOutPath $INSTDIR\lib\win\bcc64
  File "$%HB_ABSROOT%lib\win\bcc64\*.*"
SectionEnd
!endif

!ifdef PKG_COMP_WATCOM
Section /o "Libs for Open Watcom" hb_lib_watcom
  SetOutPath $INSTDIR\lib\win\watcom
  File "$%HB_ABSROOT%lib\win\watcom\*.*"
  SetOutPath $INSTDIR\comp\watcom
  File "$%HB_ABSROOT%comp\watcom\HARBOUR_README_WATCOM.txt"
SectionEnd
!endif

!ifdef PKG_COMP_POCC
Section /o "Libs for Pelles C" hb_lib_pocc
  SetOutPath $INSTDIR\lib\win\pocc
  File "$%HB_ABSROOT%lib\win\pocc\*.*"
  SetOutPath $INSTDIR\comp\pocc
  File "$%HB_ABSROOT%comp\pocc\HARBOUR_README_POCC.txt"
SectionEnd
!endif

!ifdef PKG_COMP_POCC64
Section /o "Libs for Pelles C x64" hb_lib_pocc64
  SetOutPath $INSTDIR\lib\win\pocc64
  File "$%HB_ABSROOT%lib\win\pocc64\*.*"
SectionEnd
!endif

!ifdef PKG_COMP_POCCARM
Section /o "Libs for Pelles C WinCE/ARM" hb_lib_poccarm
  SetOutPath $INSTDIR\lib\wce\poccarm
  File "$%HB_ABSROOT%lib\wce\poccarm\*.*"
SectionEnd
!endif

!ifdef PKG_PLAT_LINUX
Section /o "Libs for Open Watcom Linux" hb_lib_linux
  SetOutPath $INSTDIR\lib\linux\watcom
  File "$%HB_ABSROOT%lib\linux\watcom\*.*"
  SetOutPath $INSTDIR\comp\watcom
  File "$%HB_ABSROOT%comp\watcom\HARBOUR_README_WATCOM.txt"
SectionEnd
!endif

!ifdef PKG_PLAT_OS2
Section /o "Libs for Open Watcom OS/2" hb_lib_os2
  SetOutPath $INSTDIR\lib\os2\watcom
  File "$%HB_ABSROOT%lib\os2\watcom\*.*"
  SetOutPath $INSTDIR\comp\watcom
  File "$%HB_ABSROOT%comp\watcom\HARBOUR_README_WATCOM.txt"
SectionEnd
!endif

!ifdef PKG_PLAT_DOS
Section /o "Libs for Open Watcom MS-DOS" hb_lib_dos
  SetOutPath $INSTDIR\lib\dos\watcom
  File "$%HB_ABSROOT%lib\dos\watcom\*.*"
  SetOutPath $INSTDIR\comp\watcom
  File "$%HB_ABSROOT%comp\watcom\HARBOUR_README_WATCOM.txt"
SectionEnd
!endif

!ifdef PKG_COMP_DJGPP
Section /o "Libs for DJGPP MS-DOS" hb_lib_djgpp
  SetOutPath $INSTDIR\lib\dos\djgpp
  File "$%HB_ABSROOT%lib\dos\djgpp\*.*"
  SetOutPath $INSTDIR\comp\djgpp
  File "$%HB_ABSROOT%comp\djgpp\HARBOUR_README_DJGPP.txt"
SectionEnd
!endif

!ifdef PKG_COMP_MINGW
Section /o "Dlls for x86" hb_dlls_x86
  SetOutPath $INSTDIR\bin
  File "$%HB_ABSROOT%bin\*-$%HB_VS%.dll"
SectionEnd
!endif

!ifdef PKG_COMP_MINGW64
Section /o "Dlls for x64" hb_dlls_x64
  SetOutPath $INSTDIR\bin
  File "$%HB_ABSROOT%bin\*-$%HB_VS%-x64.dll"
SectionEnd
!endif

!ifdef PKG_COMP_MINGWARM
Section /o "Dlls for WinCE/ARM" hb_dlls_arm
  SetOutPath $INSTDIR\bin
  File "$%HB_ABSROOT%bin\harbour-$%HB_VS%-wce-arm.dll"
SectionEnd
!endif

!ifdef _NEVER_
!ifdef PKG_PLAT_OS2
Section /o "Dlls for OS/2" hb_dlls_os2
  SetOutPath $INSTDIR\bin
  ; TOFIX: .dll name collision with MS-DOS
  File "$%HB_ABSROOT%bin\harbour.dll"
SectionEnd
!endif

!ifdef PKG_PLAT_DOS
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
  WriteINIStr     "$SMPROGRAMS\Harbour $%HB_VM%\Links\Homepage.url"                   "InternetShortcut" "URL" "https://github.com/vszakats/harbour-core"
; WriteINIStr     "$SMPROGRAMS\Harbour $%HB_VM%\Links\Users' Mailing List.url"        "InternetShortcut" "URL" "https://groups.google.com/group/harbour-users/"
; WriteINIStr     "$SMPROGRAMS\Harbour $%HB_VM%\Links\Developers' Mailing List.url"   "InternetShortcut" "URL" "https://groups.google.com/group/harbour-devel/"
  WriteINIStr     "$SMPROGRAMS\Harbour $%HB_VM%\Links\Development.url"                "InternetShortcut" "URL" "https://github.com/vszakats/harbour-core"
  WriteINIStr     "$SMPROGRAMS\Harbour $%HB_VM%\Links\Development Timeline.url"       "InternetShortcut" "URL" "https://github.com/vszakats/harbour-core/commits/master"

SectionEnd

  ; Descriptions

  ; Language strings
  LangString DESC_hb_main         ${LANG_ENGLISH} "Harbour main components"
  LangString DESC_hb_shortcuts    ${LANG_ENGLISH} "Add icons to Start Menu and Desktop"
  LangString DESC_hb_examples     ${LANG_ENGLISH} "Harbour samples and tests"
!ifdef PKG_COMP_MINGW
  LangString DESC_hb_dlls_x86     ${LANG_ENGLISH} "Harbour dlls for x86"
!endif
!ifdef PKG_COMP_MINGW64
  LangString DESC_hb_dlls_x64     ${LANG_ENGLISH} "Harbour dlls for x64"
!endif
!ifdef PKG_COMP_MINGWARM
  LangString DESC_hb_dlls_arm     ${LANG_ENGLISH} "Harbour dlls for WinCE/ARM"
!endif
!ifdef _NEVER_
!ifdef PKG_PLAT_OS2
  LangString DESC_hb_dlls_os2     ${LANG_ENGLISH} "Harbour dlls for OS/2"
!endif
!ifdef PKG_PLAT_DOS
  LangString DESC_hb_dlls_dos     ${LANG_ENGLISH} "Harbour dlls for MS-DOS"
!endif
!endif
!ifdef PKG_CC_MINGW
  LangString DESC_hb_cc_mingw     ${LANG_ENGLISH} "MinGW compiler"
!endif
!ifdef PKG_CC_MINGWARM
  LangString DESC_hb_cc_mingwarm  ${LANG_ENGLISH} "MinGW WinCE/ARM compiler"
!endif
!ifdef PKG_CC_DJGPP
  LangString DESC_hb_cc_djgpp     ${LANG_ENGLISH} "DJGPP MS-DOS compiler"
!endif
!ifdef PKG_CC_POCC
  LangString DESC_hb_cc_pocc      ${LANG_ENGLISH} "Pelles C compiler"
!endif
!ifdef PKG_CC_WATCOM
  LangString DESC_hb_cc_watcom    ${LANG_ENGLISH} "Watcom C compiler"
!endif
!ifdef PKG_COMP_MINGW
  LangString DESC_hb_lib_mingw    ${LANG_ENGLISH} "Harbour libs for MinGW"
!endif
!ifdef PKG_COMP_MINGW64
  LangString DESC_hb_lib_mingw64  ${LANG_ENGLISH} "Harbour libs for MinGW x64"
!endif
!ifdef PKG_COMP_MINGWARM
  LangString DESC_hb_lib_mingwarm ${LANG_ENGLISH} "Harbour libs for MinGW WinCE/ARM"
!endif
!ifdef PKG_COMP_MSVC
  LangString DESC_hb_lib_msvc     ${LANG_ENGLISH} "Harbour libs for MSVC"
!endif
!ifdef PKG_COMP_MSVC64
  LangString DESC_hb_lib_msvc64   ${LANG_ENGLISH} "Harbour libs for MSVC x64"
!endif
!ifdef PKG_COMP_BCC
  LangString DESC_hb_lib_bcc      ${LANG_ENGLISH} "Harbour libs for Borland C"
!endif
!ifdef PKG_COMP_BCC64
  LangString DESC_hb_lib_bcc64    ${LANG_ENGLISH} "Harbour libs for Borland C x64"
!endif
!ifdef PKG_COMP_WATCOM
  LangString DESC_hb_lib_watcom   ${LANG_ENGLISH} "Harbour libs for Open Watcom"
!endif
!ifdef PKG_COMP_POCC
  LangString DESC_hb_lib_pocc     ${LANG_ENGLISH} "Harbour libs for Pelles C"
!endif
!ifdef PKG_COMP_POCC64
  LangString DESC_hb_lib_pocc64   ${LANG_ENGLISH} "Harbour libs for Pelles C x64"
!endif
!ifdef PKG_COMP_POCCARM
  LangString DESC_hb_lib_poccarm  ${LANG_ENGLISH} "Harbour libs for Pelles C WinCE/ARM"
!endif
!ifdef PKG_PLAT_LINUX
  LangString DESC_hb_lib_linux    ${LANG_ENGLISH} "Harbour libs for Open Watcom Linux"
!endif
!ifdef PKG_PLAT_OS2
  LangString DESC_hb_lib_os2      ${LANG_ENGLISH} "Harbour libs for Open Watcom OS/2"
!endif
!ifdef PKG_PLAT_DOS
  LangString DESC_hb_lib_dos      ${LANG_ENGLISH} "Harbour libs for Open Watcom MS-DOS"
!endif
!ifdef PKG_COMP_DJGPP
  LangString DESC_hb_lib_djgpp    ${LANG_ENGLISH} "Harbour libs for DJGPP MS-DOS"
!endif

  ; Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_main}         $(DESC_hb_main)
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_shortcuts}    $(DESC_hb_shortcuts)
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_examples}     $(DESC_hb_examples)
!ifdef PKG_COMP_MINGW
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_dlls_x86}     $(DESC_hb_dlls_x86)
!endif
!ifdef PKG_COMP_MINGW64
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_dlls_x64}     $(DESC_hb_dlls_x64)
!endif
!ifdef PKG_COMP_MINGWARM
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_dlls_arm}     $(DESC_hb_dlls_arm)
!endif
!ifdef PKG_CC_MINGW
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_cc_mingw}     $(DESC_hb_cc_mingw)
!endif
!ifdef PKG_CC_MINGWARM
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_cc_mingwarm}  $(DESC_hb_cc_mingwarm)
!endif
!ifdef PKG_CC_DJGPP
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_cc_djgpp}     $(DESC_hb_cc_djgpp)
!endif
!ifdef PKG_CC_POCC
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_cc_pocc}      $(DESC_hb_cc_pocc)
!endif
!ifdef PKG_CC_WATCOM
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_cc_watcom}    $(DESC_hb_cc_watcom)
!endif
!ifdef PKG_COMP_MINGW
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_mingw}    $(DESC_hb_lib_mingw)
!endif
!ifdef PKG_COMP_MINGW64
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_mingw64}  $(DESC_hb_lib_mingw64)
!endif
!ifdef PKG_COMP_MINGWARM
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_mingwarm} $(DESC_hb_lib_mingwarm)
!endif
!ifdef PKG_COMP_MSVC
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_msvc}     $(DESC_hb_lib_msvc)
!endif
!ifdef PKG_COMP_MSVC64
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_msvc64}   $(DESC_hb_lib_msvc64)
!endif
!ifdef PKG_COMP_BCC
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_bcc}      $(DESC_hb_lib_bcc)
!endif
!ifdef PKG_COMP_BCC64
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_bcc64}    $(DESC_hb_lib_bcc64)
!endif
!ifdef PKG_COMP_WATCOM
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_watcom}   $(DESC_hb_lib_watcom)
!endif
!ifdef PKG_COMP_POCC
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_pocc}     $(DESC_hb_lib_pocc)
!endif
!ifdef PKG_COMP_POCC64
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_pocc64}   $(DESC_hb_lib_pocc64)
!endif
!ifdef PKG_COMP_POCCARM
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_poccarm}  $(DESC_hb_lib_poccarm)
!endif
!ifdef PKG_PLAT_LINUX
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_linux}    $(DESC_hb_lib_linux)
!endif
!ifdef PKG_PLAT_OS2
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_os2}      $(DESC_hb_lib_os2)
!endif
!ifdef PKG_PLAT_DOS
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_dos}      $(DESC_hb_lib_dos)
!endif
!ifdef PKG_COMP_DJGPP
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_lib_djgpp}    $(DESC_hb_lib_djgpp)
!endif
  !insertmacro MUI_FUNCTION_DESCRIPTION_END

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
