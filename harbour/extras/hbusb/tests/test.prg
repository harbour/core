// testusb4h.prg

#include "usb4h.ch"  

FUNCTION Main()

  QOut( "Initialising USB4H" )
  int_RetVal := USB4H_Init( 0 )
  QQOut( " returns", int_RetVal )

  int_DeviceList := 0
  QOut( "Getting Device List" ) 
  int_DeviceCount := USB4H_GetDeviceList( 0, @int_DeviceList ) 
  QQOut( " returns", int_DeviceCount, "which is the number of USB devices found" )
  QOut( "Device list address is", int_DeviceList )

  FOR int_DeviceNumber = 0 TO int_DeviceCount - 1
    int_BusNumber := USB4H_GetBusNumber( int_DeviceList, int_DeviceNumber )
    QOut( "Bus Number:", LTrim( Str( int_BusNumber ) ) )
    int_DeviceAddress := USB4H_GetDeviceAddress( int_DeviceList, int_DeviceNumber )
    QQOut( "   Address:", LTrim( Str( int_DeviceAddress ) ) )
    int_Descriptor := 0
    int_VendorID   := 0
    int_ProductID  := 0
    int_NumConfigurations := 0
    int_RetVal := USB4H_GetDeviceDescriptor( int_DeviceList, int_DeviceNumber, @int_Descriptor, @int_VendorID, @int_ProductID, @int_NumConfigurations )
    // QOut( int_Descriptor )
    QQOut( "  Vendor:", LTrim( Str( int_VendorID ) ) )
    QQOut( "  Product:", LTrim( Str( int_ProductID ) ) )
    QQOut( "  Config Count:", LTrim( Str( int_NumConfigurations ) ) )
  NEXT

  QOut( "Freeing Device List" )
  USB4H_FreeDeviceList( 0 )

  QOut()
  QOut( "Opening Device" )
  int_DeviceHandle := USB4H_OpenDeviceWithVIDPID( 0, 1523, 255 )
  QOut( "returns", int_DeviceHandle )

  IF int_DeviceHandle < 1
    QOut( "Cannot open the device" )
   ELSE
    QOut( "Testing for kernel having claimed interface" )
    int_RetVal := USB4H_KernelDriverActive( int_DeviceHandle, 0 )
    QOut( "returns", int_RetVal )

    IF int_RetVal == USB4H_KERNEL_HAS_INTERFACE
      QOut( "Kernel has interface" )
      QOut( "Detaching Kernel from interface" )
      int_RetVal := USB4H_DetachKernelDriver( int_DeviceHandle, 0 )
      QOut( "returns", int_RetVal )
    ENDIF
    QOut( "Claiming Interface" )
    int_RetVal := USB4H_ClaimInterface( int_DeviceHandle, 0 )
    QOut( "returns", int_RetVal )

    str_Data := Space( 512 )
    int_Length := 0
    QOut( "Querying device" )
    FOR jj = 1 TO 500
      int_RetVal := USB4H_BulkTransfer( int_DeviceHandle, USB4H_ENDPOINT_IN, 100, @str_Data, @int_Length )
      IF Len( str_Data ) > 0
        SWITCH Asc( SubStr( str_Data, -2, 1 ) )
          CASE 0
            QOut( "Clear" )
            EXIT
          CASE 1
            QOut( "Left Pedal" )
            EXIT  
          CASE 2 
            QOut( "Middle Pedal" )
            EXIT
          CASE 3
            QOut( "Left and Middle Pedals" )  
            EXIT
          CASE 4 
            QOut( "Right Pedal" )
            EXIT
          CASE 5
            QOut( "Left and Right Pedals" )
            EXIT
          CASE 6
            QOut( "Middle and Right Pedals" )
            EXIT
          CASE 7
            QOut( "All Three Pedals" )
            EXIT
          OTHERWISE
            QOut( "Error" )   
        ENDSWITCH
      ENDIF
    NEXT

    QOut( "Releasing Interface" )
    int_RetVal := USB4H_ReleaseInterface( int_DeviceHandle, 0 )
    QOut( "returns", int_RetVal )

    QOut( "Reattaching Kernel to interface" )
    int_RetVal := USB4H_AttachKernelDriver( int_DeviceHandle, 0 )
    QOut( "returns", int_RetVal )

  ENDIF
  QOut( "Closing Device" )
  USB4H_Close( int_DeviceHandle )

  QOut( "Deinitialising USB4H" )
  USB4H_Exit()
  QQOut( " done" )
  QUIT
