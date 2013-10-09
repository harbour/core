; ---------------------------------------------------------------
; Copyright 2009 Viktor Szakats (vszakats.net/harbour)
; See COPYING.txt for licensing terms.
;
; Harbour Nullsoft installer script (for Windows/MS-DOS)
; [ Do not try to use this script directly. It won't work. ]
;
; See README.md for further information.
; ---------------------------------------------------------------

  SetCompressor /solid lzma

  !include "MUI2.nsh"

  !define MUI_HEADERIMAGE
  !define MUI_HEADERIMAGE_BITMAP "${NSISDIR}\Contrib\Graphics\Header\win.bmp"
  !define MUI_FINISHPAGE_SHOWREADME_NOTCHECKED
  !define MUI_FINISHPAGE_SHOWREADME 'notepad.exe "$\"$INSTDIR\README.md$\""'
  !define MUI_ICON "harb_win.ico"
  !define MUI_UNICON "${NSISDIR}\Contrib\Graphics\Icons\orange-uninstall.ico"

SetDateSave on
SetDatablockOptimize on
CRCCheck on

RequestExecutionLevel user

; The name of the installer
Name "Harbour"

; The file to write
OutFile "$%HB_TOP%\$%HB_PKGNAME%.exe"

InstallDir C:\$%HB_PKGNAMI%

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

LicenseLangString MUILicense ${LANG_ENGLISH} "$%HB_INSTALL_PREFIX%\COPYING.txt"

;--------------------------------
; The stuff to install

Section "Main components" hb_main

  SectionIn RO

  ; Set output path to the installation directory.
  SetOutPath $INSTDIR

  File /nonfatal "$%HB_INSTALL_PREFIX%\COPYING.txt"
  File /nonfatal "$%HB_INSTALL_PREFIX%\README.md"
  ; Let it work also for MS-DOS packages where short filename is used here
  File /nonfatal "$%HB_INSTALL_PREFIX%\Change*"

  SetOutPath $INSTDIR\bin
  File "$%HB_INSTALL_PREFIX%\bin\*.*"

  SetOutPath $INSTDIR\lib
  File /r "$%HB_INSTALL_PREFIX%\lib\*.*"

  SetOutPath $INSTDIR\include
  File "$%HB_INSTALL_PREFIX%\include\*.*"

  SetOutPath $INSTDIR\doc
  File "$%HB_INSTALL_PREFIX%\doc\*.*"

  ; Write the installation path into the registry
; WriteRegStr HKLM "SOFTWARE\Harbour" "InstallDir" "$INSTDIR"
  WriteRegStr HKCU "Software\Harbour" "InstallDir" "$INSTDIR"

  ; Write the uninstall keys for Windows
; WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Harbour" "DisplayName" "Harbour"
; WriteRegStr HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Harbour" "UninstallString" '"$INSTDIR\uninstall.exe"'
; WriteRegDWORD HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Harbour" "NoModify" 1
; WriteRegDWORD HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Harbour" "NoRepair" 1
  WriteUninstaller "uninstall.exe"

SectionEnd

; The stuff to install
Section "Examples" hb_examples

  SectionIn RO

  ; Set output path to the installation directory.
  SetOutPath $INSTDIR\tests

SectionEnd

; Optional section (can be disabled by the user)
Section "Start Menu and Desktop icons" hb_shortcuts

  ; this will be the working dir for shortcuts
  SetOutPath $INSTDIR

  CreateShortCut "$DESKTOP\Harbour.lnk" "$INSTDIR" "" "$INSTDIR" 0
  CreateDirectory "$SMPROGRAMS\Harbour"
  CreateShortCut  "$SMPROGRAMS\Harbour\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0
  CreateShortCut  "$SMPROGRAMS\Harbour\Harbour (Command line).lnk" "cmd.exe" "/k cd $INSTDIR\bin" "cmd.exe" 0
  CreateShortCut  "$SMPROGRAMS\Harbour\Harbour.lnk" "$INSTDIR" "" "$INSTDIR" 0
  CreateShortCut  "$SMPROGRAMS\Harbour\Harbour (Interactive shell).lnk" "$INSTDIR\bin\hbrun.exe" "" "$INSTDIR\bin\hbrun.exe" 0
  CreateDirectory "$SMPROGRAMS\Harbour\Links"
  WriteINIStr     "$SMPROGRAMS\Harbour\Links\Homepage.url"                   "InternetShortcut" "URL" "http://harbour-project.org/"
  WriteINIStr     "$SMPROGRAMS\Harbour\Links\Users' Mailing List.url"        "InternetShortcut" "URL" "https://groups.google.com/group/harbour-users/"
  WriteINIStr     "$SMPROGRAMS\Harbour\Links\Developers' Mailing List.url"   "InternetShortcut" "URL" "https://groups.google.com/group/harbour-devel/"
  WriteINIStr     "$SMPROGRAMS\Harbour\Links\Development.url"                "InternetShortcut" "URL" "https://github.com/harbour"
  WriteINIStr     "$SMPROGRAMS\Harbour\Links\Development Timeline.url"       "InternetShortcut" "URL" "https://github.com/harbour/core/commits/master"

SectionEnd

;--------------------------------
; Descriptions

  ; Language strings
  LangString DESC_hb_main ${LANG_ENGLISH} "Main components"
  LangString DESC_hb_shortcuts ${LANG_ENGLISH} "Add icons to Start Menu and Desktop"
  LangString DESC_hb_examples ${LANG_ENGLISH} "Samples and tests"

  ; Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_main} $(DESC_hb_main)
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_shortcuts} $(DESC_hb_shortcuts)
    !insertmacro MUI_DESCRIPTION_TEXT ${hb_examples} $(DESC_hb_examples)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END

;--------------------------------
; Uninstaller

Section "Uninstall"

  ; Remove files and uninstaller
  RMDir /r $INSTDIR

  ; Remove directories used
  Delete "$SMPROGRAMS\Harbour\Links\*.*"
  RMDir  "$SMPROGRAMS\Harbour\Links"
  Delete "$SMPROGRAMS\Harbour\*.*"
  RMDir  "$SMPROGRAMS\Harbour"
  RMDir  "$INSTDIR"

  Delete "$DESKTOP\Harbour.lnk"

; DeleteRegKey HKLM "SOFTWARE\Harbour"
  DeleteRegKey HKCU "Software\Harbour"

SectionEnd
