/* Originally posted by Bernard Mouille
   https://groups.google.com/d/msg/harbour-users/mI2ehYbLOI8/fw3j75z_RU4J */

/* Complete docs: https://msdn.microsoft.com/en-us/library/aa394554(v=vs.85).aspx */

#include "simpleio.ch"

PROCEDURE Main()

   LOCAL oLocator := win_oleCreateObject( "WbemScripting.SWbemLocator" )
   LOCAL oWMI := oLocator:ConnectServer( ".", "root\cimv2" )
   LOCAL i, nIndex
   LOCAL tmp

   ? "Win32_OperatingSystem"
   FOR EACH i IN oWMI:ExecQuery( "SELECT * FROM Win32_OperatingSystem" )
      ? i:SerialNumber
   NEXT
   ?

   ? "Win32_LogicalDisk"
   FOR EACH i IN oWMI:ExecQuery( "SELECT * FROM Win32_LogicalDisk" )
      IF HB_ISSTRING( i:VolumeSerialNumber )
         ? i:VolumeSerialNumber, i:Description
      ENDIF
   NEXT
   ?

   ? "Win32_NetworkAdapter"
   FOR EACH i IN oWMI:ExecQuery( "SELECT * FROM Win32_NetworkAdapter" )
      IF HB_ISSTRING( i:MACAddress )
         ? i:MACAddress, i:Description
      ENDIF
   NEXT
   ?

   nIndex := 1
   FOR EACH i IN oWMI:ExecQuery( "SELECT * FROM Win32_Bios" )

      ? "Win32_Bios #" + hb_ntos( nIndex++ )

      ? "BiosCharacteristics . :", TypeAndValue( i:BiosCharacteristics )
      IF HB_ISARRAY( i:BiosCharacteristics )
         FOR EACH tmp IN i:BiosCharacteristics
            ? Space( 27 ), Str( tmp, 2 ), "->", WMI_Bios_BiosCharacteristics( tmp )
         NEXT
      ENDIF

      ? "BIOSVersion ......... :", TypeAndValue( i:BIOSVersion )
      IF HB_ISARRAY( i:BIOSVersion )
         FOR EACH tmp IN i:BIOSVersion
            ? Space( 27 ), tmp
         NEXT
      ENDIF

      ? "BuildNumber ......... :", TypeAndValue( i:BuildNumber )
      ? "Caption ............. :", TypeAndValue( i:Caption )
      ? "CodeSet ............. :", TypeAndValue( i:CodeSet )
      ? "CurrentLanguage ..... :", TypeAndValue( i:CurrentLanguage )
      ? "Description ......... :", TypeAndValue( i:Description )
      ? "IdentificationCode .. :", TypeAndValue( i:IdentificationCode )
      ? "InstallableLanguages  :", TypeAndValue( i:InstallableLanguages )
      ? "InstallDate ......... :", TypeAndValue( i:InstallDate )
      ? "LanguageEdition ..... :", TypeAndValue( i:LanguageEdition )

      ? "ListOfLanguages ..... :", TypeAndValue( i:ListOfLanguages )
      IF HB_ISARRAY( i:ListOfLanguages )
         FOR EACH tmp IN i:ListOfLanguages
            ? Space( 27 ), tmp
         NEXT
      ENDIF

      ? "Manufacturer ........ :", TypeAndValue( i:Manufacturer )
      ? "Name ................ :", TypeAndValue( i:Name )
      ? "OtherTargetOS ....... :", TypeAndValue( i:OtherTargetOS )
      ? "PrimaryBIOS ......... :", TypeAndValue( i:PrimaryBIOS )
      ? "ReleaseDate ......... :", TypeAndValue( i:ReleaseDate )
      ? "SerialNumber ........ :", TypeAndValue( i:SerialNumber )
      ? "SMBIOSBIOSVersion ... :", TypeAndValue( i:SMBIOSBIOSVersion )
      ? "SMBIOSMajorVersion .. :", TypeAndValue( i:SMBIOSMajorVersion )
      ? "SMBIOSMinorVersion .. :", TypeAndValue( i:SMBIOSMinorVersion )
      ? "SMBIOSPresent ....... :", TypeAndValue( i:SMBIOSPresent )
      ? "SoftwareElementID ... :", TypeAndValue( i:SoftwareElementID )

      ? "SoftwareElementState  :", TypeAndValue( i:SoftwareElementState )
      IF HB_ISNUMERIC( i:SoftwareElementState )
         ? Space( 27 ), Str( i:SoftwareElementState, 2 ), "->", WMI_Bios_SoftwareElementState( i:SoftwareElementState )
      ENDIF

      ? "Status .............. :", TypeAndValue( i:Status )
      ? "TargetOperatingSystem :", TypeAndValue( i:TargetOperatingSystem )

      IF HB_ISNUMERIC( i:TargetOperatingSystem )
         ? Space( 27 ), Str( i:TargetOperatingSystem, 2 ), "->", WMI_Bios_TargetOperatingSystem( i:TargetOperatingSystem )
      ENDIF

      ? "Version ............. :", TypeAndValue( i:Version )
   NEXT

   ?

   RETURN

STATIC FUNCTION TypeAndValue( x )
   RETURN ValType( x ) + " : " + hb_ValToExp( x )

STATIC FUNCTION WMI_Bios_BiosCharacteristics( nValue )

   DO CASE
   CASE nValue >= 40 .AND. nValue <= 47 ; RETURN "Reserved for BIOS vendor"
   CASE nValue >= 48 .AND. nValue <= 63 ; RETURN "Reserved for system vendor"
   ENDCASE

   SWITCH nValue
   CASE 0  ; RETURN "Reserved"
   CASE 1  ; RETURN "Reserved"
   CASE 2  ; RETURN "Unknown"
   CASE 3  ; RETURN "BIOS Characteristics Not Supported"
   CASE 4  ; RETURN "ISA is supported"
   CASE 5  ; RETURN "MCA is supported"
   CASE 6  ; RETURN "EISA is supported"
   CASE 7  ; RETURN "PCI is supported"
   CASE 8  ; RETURN "PC Card (PCMCIA) is supported"
   CASE 9  ; RETURN "Plug and Play is supported"
   CASE 10 ; RETURN "APM is supported"
   CASE 11 ; RETURN "BIOS is Upgradable (Flash)"
   CASE 12 ; RETURN "BIOS shadowing is allowed"
   CASE 13 ; RETURN "VL-VESA is supported"
   CASE 14 ; RETURN "ESCD support is available"
   CASE 15 ; RETURN "Boot from CD is supported"
   CASE 16 ; RETURN "Selectable Boot is supported"
   CASE 17 ; RETURN "BIOS ROM is socketed"
   CASE 18 ; RETURN "Boot From PC Card (PCMCIA) is supported"
   CASE 19 ; RETURN "EDD (Enhanced Disk Drive) Specification is supported"
   CASE 20 ; RETURN "Int 13h - Japanese Floppy for NEC 9800 1.2mb (3.5, 1k Bytes/Sector, 360 RPM) is supported"
   CASE 21 ; RETURN "Int 13h - Japanese Floppy for Toshiba 1.2mb (3.5, 360 RPM) is supported"
   CASE 22 ; RETURN "Int 13h - 5.25 / 360 KB Floppy Services are supported"
   CASE 23 ; RETURN "Int 13h - 5.25 /1.2MB Floppy Services are supported"
   CASE 24 ; RETURN "Int 13h - 3.5 / 720 KB Floppy Services are supported"
   CASE 25 ; RETURN "Int 13h - 3.5 / 2.88 MB Floppy Services are supported"
   CASE 26 ; RETURN "Int 5h, Print Screen Service is supported"
   CASE 27 ; RETURN "Int 9h, 8042 Keyboard services are supported"
   CASE 28 ; RETURN "Int 14h, Serial Services are supported"
   CASE 29 ; RETURN "Int 17h, printer services are supported"
   CASE 30 ; RETURN "Int 10h, CGA/Mono Video Services are supported"
   CASE 31 ; RETURN "NEC PC-98"
   CASE 32 ; RETURN "ACPI is supported"
   CASE 33 ; RETURN "USB Legacy is supported"
   CASE 34 ; RETURN "AGP is supported"
   CASE 35 ; RETURN "I2O boot is supported"
   CASE 36 ; RETURN "LS-120 boot is supported"
   CASE 37 ; RETURN "ATAPI ZIP Drive boot is supported"
   CASE 38 ; RETURN "1394 boot is supported"
   CASE 39 ; RETURN "Smart Battery is supported"
   ENDSWITCH

   RETURN "(unknown)"

STATIC FUNCTION WMI_Bios_SoftwareElementState( nValue )

   SWITCH nValue
   CASE 0 ; RETURN "Deployable"
   CASE 1 ; RETURN "Installable"
   CASE 2 ; RETURN "Executable"
   CASE 3 ; RETURN "Running"
   ENDSWITCH

   RETURN "(unknown)"

STATIC FUNCTION WMI_Bios_TargetOperatingSystem( nValue )

   SWITCH nValue
   CASE 0  ; RETURN "Unknown"
   CASE 1  ; RETURN "Other"
   CASE 2  ; RETURN "Mac OS"
   CASE 3  ; RETURN "ATTUNIX"
   CASE 4  ; RETURN "DGUX"
   CASE 5  ; RETURN "DECNT"
   CASE 6  ; RETURN "Digital Unix"
   CASE 7  ; RETURN "OpenVMS"
   CASE 8  ; RETURN "HPUX"
   CASE 9  ; RETURN "AIX"
   CASE 10 ; RETURN "MVS"
   CASE 11 ; RETURN "OS400"
   CASE 12 ; RETURN "OS/2"
   CASE 13 ; RETURN "JavaVM"
   CASE 14 ; RETURN "MS-DOS"
   CASE 15 ; RETURN "Win3x"
   CASE 16 ; RETURN "Win95"
   CASE 17 ; RETURN "Win98"
   CASE 18 ; RETURN "WinNT"
   CASE 19 ; RETURN "WinCE"
   CASE 20 ; RETURN "NCR3000"
   CASE 21 ; RETURN "NetWare"
   CASE 22 ; RETURN "OSF"
   CASE 23 ; RETURN "DC/OS"
   CASE 24 ; RETURN "Reliant UNIX"
   CASE 25 ; RETURN "SCO UnixWare"
   CASE 26 ; RETURN "SCO OpenServer"
   CASE 27 ; RETURN "Sequent"
   CASE 28 ; RETURN "IRIX"
   CASE 29 ; RETURN "Solaris"
   CASE 30 ; RETURN "SunOS"
   CASE 31 ; RETURN "U6000"
   CASE 32 ; RETURN "ASERIES"
   CASE 33 ; RETURN "TandemNSK"
   CASE 34 ; RETURN "TandemNT"
   CASE 35 ; RETURN "BS2000"
   CASE 36 ; RETURN "Linux"
   CASE 37 ; RETURN "Lynx"
   CASE 38 ; RETURN "XENIX"
   CASE 39 ; RETURN "VM/ESA"
   CASE 40 ; RETURN "Interactive UNIX"
   CASE 41 ; RETURN "BSD UNIX"
   CASE 42 ; RETURN "FreeBSD"
   CASE 43 ; RETURN "NetBSD"
   CASE 44 ; RETURN "GNU Hurd"
   CASE 45 ; RETURN "OS9"
   CASE 46 ; RETURN "MACH Kernel"
   CASE 47 ; RETURN "Inferno"
   CASE 48 ; RETURN "QNX"
   CASE 49 ; RETURN "EPOC"
   CASE 50 ; RETURN "IxWorks"
   CASE 51 ; RETURN "VxWorks"
   CASE 52 ; RETURN "MiNT"
   CASE 53 ; RETURN "BeOS"
   CASE 54 ; RETURN "HP MPE"
   CASE 55 ; RETURN "NextStep"
   CASE 56 ; RETURN "PalmPilot"
   CASE 57 ; RETURN "Rhapsody"
   CASE 58 ; RETURN "Windows 2000"
   CASE 59 ; RETURN "Dedicated"
   CASE 60 ; RETURN "VSE"
   CASE 61 ; RETURN "TPF"
   ENDSWITCH

   RETURN "(unknown)"
