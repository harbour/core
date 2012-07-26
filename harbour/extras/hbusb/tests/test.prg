/*
 * $Id: array.prg 17867 2012-07-21 16:48:00Z vszakats $
 */

#include "simpleio.ch"

#include "hbusb.ch"

PROCEDURE Main()

   LOCAL nRetVal
   LOCAL pDeviceList
   LOCAL nDeviceCount
   LOCAL nDeviceNumber
   LOCAL nBusNumber
   LOCAL nDeviceAddress
   LOCAL pDescriptor
   LOCAL nVendorID
   LOCAL nProductID
   LOCAL nNumConfigurations
   LOCAL pDeviceHandle
   LOCAL cData
   LOCAL nLength
   LOCAL tmp

   ? "Initialising libusb"
   nRetVal := libusb_init()
   ?? " returns", nRetVal

   ? "Getting Device List"
   nDeviceCount := libusb_get_device_list( NIL, @pDeviceList )
   ?? " returns", nDeviceCount, "which is the number of USB devices found"
   ? "Device list address is", pDeviceList

   FOR nDeviceNumber := 0 TO nDeviceCount - 1
      nBusNumber := libusb_get_bus_number( pDeviceList, nDeviceNumber )
      ? "Bus Number:", hb_ntos( nBusNumber )
      nDeviceAddress := libusb_get_device_address( pDeviceList, nDeviceNumber )
      ?? "   Address:", hb_ntos( nDeviceAddress )
      nRetVal := libusb_get_device_descriptor( pDeviceList, nDeviceNumber, @pDescriptor, @nVendorID, @nProductID, @nNumConfigurations )
      ?? "  return:", hb_ntos( nRetVal )
      ?? "  Vendor:", hb_ntos( nVendorID )
      ?? "  Product:", hb_ntos( nProductID )
      ?? "  Config Count:", hb_ntos( nNumConfigurations )
   NEXT

   ? "Freeing Device List"
   libusb_free_device_list( NIL )

   ?
   ? "Opening Device"
   pDeviceHandle := libusb_open_device_with_vid_pid( NIL, 1523, 255 )
   ? "returns", pDeviceHandle

   IF Empty( pDeviceHandle )
      ? "Cannot open the device"
   ELSE
      ? "Testing for kernel having claimed interface"
      nRetVal := libusb_kernel_driver_active( pDeviceHandle, 0 )
      ? "returns", nRetVal

      IF nRetVal == LIBUSB_KERNEL_HAS_INTERFACE
         ? "Kernel has interface"
         ? "Detaching Kernel from interface"
         nRetVal := libusb_detach_kernel_driver( pDeviceHandle, 0 )
         ? "returns", nRetVal
      ENDIF
      ? "Claiming Interface"
      nRetVal := libusb_claim_interface( pDeviceHandle, 0 )
      ? "returns", nRetVal

      cData := Space( 512 )
      nLength := 0
      ? "Querying device"
      FOR tmp := 1 TO 500
         nRetVal := libusb_bulk_transfer( pDeviceHandle, LIBUSB_ENDPOINT_IN, 100, @cData, @nLength )
         HB_SYMBOL_UNUSED( nRetVal )
         IF hb_BLen( cData ) > 0
            SWITCH hb_BCode( hb_BSubStr( cData, -2, 1 ) )
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
      nRetVal := libusb_release_interface( pDeviceHandle, 0 )
      ? "returns", nRetVal

      ? "Reattaching Kernel to interface"
      nRetVal := libusb_attach_kernel_driver( pDeviceHandle, 0 )
      ? "returns", nRetVal
   ENDIF

   ? "Closing Device"
   libusb_close( pDeviceHandle )

   ? "Deinitialising libusb"
   libusb_exit()
   ?? " done"

   RETURN
