#require "hbusb"

PROCEDURE Main()

   LOCAL nRetVal
   LOCAL pDeviceList
   LOCAL nDeviceCount
   LOCAL nDeviceNumber
   LOCAL pDescriptor
   LOCAL nVendorID
   LOCAL nProductID
   LOCAL nNumConfigurations
   LOCAL pDeviceHandle
   LOCAL cData
   LOCAL nLength
   LOCAL tmp

   ? "Initialising libusb"
   ?? " returns", libusb_init()

   ? "Getting Device List"
   nDeviceCount := libusb_get_device_list( , @pDeviceList )
   ?? " returns", nDeviceCount, "which is the number of USB devices found"
   ? "Device list address is", pDeviceList

   FOR nDeviceNumber := 0 TO nDeviceCount - 1
      ? "Bus Number:", hb_ntos( libusb_get_bus_number( pDeviceList, nDeviceNumber ) )
      ?? "   Address:", hb_ntos( libusb_get_device_address( pDeviceList, nDeviceNumber ) )
      ?? "  return:", hb_ntos( libusb_get_device_descriptor( pDeviceList, nDeviceNumber, @pDescriptor, @nVendorID, @nProductID, @nNumConfigurations ) )
      ?? "  Vendor:", hb_ntos( nVendorID )
      ?? "  Product:", hb_ntos( nProductID )
      ?? "  Config Count:", hb_ntos( nNumConfigurations )
   NEXT

   ? "Freeing Device List"
   libusb_free_device_list()

   ?
   ? "Opening Device"
   ? "returns", pDeviceHandle := libusb_open_device_with_vid_pid( , 1523, 255 )

   IF Empty( pDeviceHandle )
      ? "Cannot open the device"
   ELSE
      ? "Testing for kernel having claimed interface"
      ? "returns", nRetVal := libusb_kernel_driver_active( pDeviceHandle, 0 )

      IF nRetVal == LIBUSB_KERNEL_HAS_INTERFACE
         ? "Kernel has interface"
         ? "Detaching Kernel from interface"
         ? "returns", libusb_detach_kernel_driver( pDeviceHandle, 0 )
      ENDIF
      ? "Claiming Interface"
      ? "returns", libusb_claim_interface( pDeviceHandle, 0 )

      cData := Space( 512 )
      nLength := 0
      ? "Querying device"
      FOR tmp := 1 TO 500
         libusb_bulk_transfer( pDeviceHandle, LIBUSB_ENDPOINT_IN, 100, @cData, @nLength )
         IF ! HB_ISNULL( cData )
            SWITCH hb_BPeek( cData, hb_BLen( cData ) - 1 )
            CASE 0
               ? "Clear"
               EXIT
            CASE 1
               ? "Left Pedal"
               EXIT
            CASE 2
               ? "Middle Pedal"
               EXIT
            CASE 3
               ? "Left and Middle Pedals"
               EXIT
            CASE 4
               ? "Right Pedal"
               EXIT
            CASE 5
               ? "Left and Right Pedals"
               EXIT
            CASE 6
               ? "Middle and Right Pedals"
               EXIT
            CASE 7
               ? "All Three Pedals"
               EXIT
            OTHERWISE
               ? "Error"
            ENDSWITCH
         ENDIF
      NEXT

      ? "Releasing Interface"
      ? "returns", libusb_release_interface( pDeviceHandle, 0 )

      ? "Reattaching Kernel to interface"
      ? "returns", libusb_attach_kernel_driver( pDeviceHandle, 0 )
   ENDIF

   ? "Closing Device"
   libusb_close( pDeviceHandle )

   ? "Deinitialising libusb"
   libusb_exit()
   ?? " done"

   RETURN
