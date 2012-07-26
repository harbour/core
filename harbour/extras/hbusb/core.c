// usb4h.c

/*

UU            UU        SSSSSSS       BBBBBBBBB           4444       HH        HH     
UU            UU      SSS     SSS     BB      BBB        44 44       HH        HH 
UU            UU     SS         SS    BB        BB      44  44       HH        HH 
UU            UU     SS               BB        BB     44   44       HH        HH 
UU            UU      SSS             BB      BBB     44    44       HH        HH  
UU            UU        SSSSSS        BBBBBBBBB      444444444444    HHHHHHHHHHHH    
UU            UU             SSS      BB      BBB           44       HH        HH 
 UU          UU                SS     BB        BB          44       HH        HH 
  UU        UU       SS        SS     BB        BB          44       HH        HH 
   UUU    UUU         SSS    SSS      BB      BBB           44       HH        HH 
     UUUUUU             SSSSSS        BBBBBBBBB             44       HH        HH 

*/

#include <hbapi.h>
#include <libusb.h>



/*===============================================================================
  |                                                                             |
  |             library initialisation and deinitialisation                     |
  |                                                                             |
  ===============================================================================*/

/*    USB4H_Init()

      Initialises USB4H (libusb).

      Must be called before calling any other USB4H functions.

      Getting a context is optional.  */


HB_FUNC( USB4H_INIT )
{
  int            success;
  libusb_context * context;
  if ( hb_parl( 1 ) )
    {
      success = libusb_init( &context );
      if ( success == 0 )
      {
        hb_stornl( ( ULONG ) context, 2 );
      }
    }   
   else
    { 
      success = libusb_init( NULL );    
    }
  hb_retni( success );
}

//-------------------------------------------------------------------------------

/*    USB4H_Exit()

      Deinitialise USB4H (libusb).  */

HB_FUNC( USB4H_EXIT )
{
  libusb_exit( ( libusb_context * ) hb_parnl( 1 ) );
  hb_ret();    
}

//-------------------------------------------------------------------------------

/*    USB4H_SetDebug()

      Sets the message verbosity.

      Refer to usb4h.ch for options    */


HB_FUNC( USB4H_SETDEBUG )
{
  libusb_set_debug( ( libusb_context * ) hb_parnl( 1 ), hb_parni( 2 ) );
  hb_ret();    
}

//-------------------------------------------------------------------------------

/*===============================================================================
  |                                                                             |
  |                    device handling and enumeration                          |
  |                                                                             |
  ===============================================================================*/

/*    USB4H_GetDeviceList()

      Returns a list of USB devcices attached to your system.  */

HB_FUNC( USB4H_GETDEVICELIST )
{
  libusb_device ** devicelist;
  ssize_t count;
  count = libusb_get_device_list( ( libusb_context * ) hb_parnl( 1 ), &devicelist );
  hb_stornl( ( ULONG ) devicelist, 2 );
  hb_retni( count );
}

//-------------------------------------------------------------------------------

/*    USB4H_FreeDeviceList()

      Frees the list returned by USB4H_GetDeviceList.  */

HB_FUNC( USB4H_FREEDEVICELIST )
{
  libusb_free_device_list( ( libusb_device ** ) hb_parnl( 1 ), hb_parni( 2 ) );
  hb_ret();
}

//-------------------------------------------------------------------------------

/*    USB4H_GetBusNumber()

      Gets the number of the bus that a device is attached to.  */

HB_FUNC( USB4H_GETBUSNUMBER )
{
  uint8_t busnumber;
  libusb_device ** devicelist;  
  devicelist = ( ( libusb_device ** ) hb_parnl( 1 ) );
  busnumber = libusb_get_bus_number( devicelist[ hb_parni( 2 ) ] );
  hb_retni( busnumber );
}

//-------------------------------------------------------------------------------

/*    USB4H_GetDeviceAddress()

      Gets the address on the bus that a device is attached to.  */

HB_FUNC( USB4H_GETDEVICEADDRESS )
{
  uint8_t deviceaddress;
  libusb_device ** devicelist;  
  devicelist = ( ( libusb_device ** ) hb_parnl( 1 ) );
  deviceaddress = libusb_get_device_address( devicelist[ hb_parni( 2 ) ] );
  hb_retni( deviceaddress );
}

//-------------------------------------------------------------------------------

/*    USB4H_GetDeviceSpeed()

      Gets the negotiated connection speed for a device.  

      Refer to usb4h.ch for speeds.  */

/*
HB_FUNC( USB4H_GETDEVICESPEED )
{
  int devicespeed;
  libusb_device ** devicelist;  
  devicelist = ( ( libusb_device ** ) hb_parnl( 1 ) );
  devicespeed = libusb_get_device_speed( devicelist[ hb_parni( 2 ) ] );
  hb_retni( devicespeed );
}
*/

//-------------------------------------------------------------------------------

/* 
USB4H_GetMaxPacketSize()

Gets the maximum packet size for a particular endpoint in the active device configuration.  */

HB_FUNC( USB4H_GETMAXPACKETSIZE )
{
  int maxpacketsize;
  libusb_device ** devicelist;  
  devicelist = ( libusb_device ** ) hb_parnl( 1 );
  maxpacketsize = libusb_get_max_packet_size( devicelist[ hb_parni( 2 ) ], ( unsigned char )  hb_parni( 3 ) );
  hb_retni( maxpacketsize );
}

//-------------------------------------------------------------------------------

/*    USB4H_RefDevice()

      Increments the reference count of a device.  */

HB_FUNC( USB4H_REFDEVICE )
{
  libusb_device *device;
  libusb_device ** devicelist; 
  devicelist = ( libusb_device ** ) hb_parnl( 1 );
  device = libusb_ref_device( devicelist[ hb_parni( 2 ) ] );
  hb_ret();
}

//-------------------------------------------------------------------------------

/*    USB4H_UnrefDevice()

      Decrements the reference count of a device  */

HB_FUNC( USB4H_UNREFDEVICE )
{
  libusb_device ** devicelist; 
  devicelist = ( libusb_device ** ) hb_parnl( 1 );
  libusb_unref_device( devicelist[ hb_parni( 2 ) ] );
  hb_ret();
}

//-------------------------------------------------------------------------------

/*    USB4H_Open()

      Open a device and obtain a device handle  */

HB_FUNC( USB4H_OPEN )
{
  int success;
  libusb_device_handle * handle;
  libusb_device ** devicelist; 
  devicelist = ( libusb_device ** ) hb_parnl( 1 );
  success = libusb_open( devicelist[ hb_parni( 2 ) ], &handle );
  if ( success == 0 )
  {
    hb_stornl( ( ULONG ) handle, 3 );
  }
  hb_retni( success );
}

//-------------------------------------------------------------------------------

/*    USB4H_OpenDeviceWithVIDPID()
      
      Find a device with known Vendor IID and Product ID.  */


HB_FUNC( USB4H_OPENDEVICEWITHVIDPID )
{
  libusb_device_handle * handle;
  handle = libusb_open_device_with_vid_pid( ( libusb_context * ) hb_parnl( 1 ), hb_parni( 2 ), hb_parni( 3 ) );
  hb_retnl( ( ULONG ) handle );
}

//-------------------------------------------------------------------------------

/*    USB4H_Close()

      Close a device handle.  */

HB_FUNC( USB4H_CLOSE )
{
  libusb_close( ( libusb_device_handle * ) hb_parnl( 1 ) );
  hb_ret();
}

//-------------------------------------------------------------------------------

/*    USB4H_GetDevice()

      Get the underlying device for a handle. */

HB_FUNC( USB4H_GETDEVICE )
{
  libusb_device * device;
  device = libusb_get_device( ( libusb_device_handle * ) hb_parnl( 1 ) );
  hb_retnl( ( ULONG ) device );
}

//-------------------------------------------------------------------------------

/*    USB4H_GetConfiguration()

      Get the configuration value of the currently active configuration.  */

HB_FUNC( USB4H_GETCONFIGURATION )
{
  int configuration;
  int success;
  configuration = 0;
  success = libusb_get_configuration( ( libusb_device_handle * ) hb_parnl( 1 ), &configuration );
  if ( success == 0 )
  {
    hb_storni( configuration, 2 );
  }
  hb_retni( success );
}

//-------------------------------------------------------------------------------

/*    USB4H_SetConfiguration()

      Set the active configuration for a device.  */

HB_FUNC( USB4H_SETCONFIGURATION )
{
  int configuration;
  int success;
  success = libusb_set_configuration( ( libusb_device_handle * ) hb_parnl( 1 ), hb_parni( 2 ) );
  hb_retni( success );  
}

//-------------------------------------------------------------------------------

/*    USB4H_ClaimInterface()

      Claim an interface on a given device handle.

      Required before you can perform I/O on any of its endpoints.  */

HB_FUNC( USB4H_CLAIMINTERFACE )
{
  int success;
  success = libusb_claim_interface( ( libusb_device_handle * ) hb_parnl( 1 ), hb_parni( 2 ) );
  hb_retni( success ); 
}

//-------------------------------------------------------------------------------

/*    USB4H_ReleaseInterface()

      Release a previously claimed interface.  */

HB_FUNC( USB4H_RELEASEINTERFACE )
{
  int success;
  success = libusb_release_interface( ( libusb_device_handle * ) hb_parnl( 1 ), hb_parni( 2 ) );
  hb_retni( success ); 
}

//-------------------------------------------------------------------------------

/*    USB4H_SetInterfaceAltSetting()

      Activate an alternate setting for an interface.  */

HB_FUNC( USB4H_SETINTERFACEALTSETTING )
{
  int success;
  success = libusb_set_interface_alt_setting( ( libusb_device_handle * ) hb_parnl( 1 ), hb_parni( 2 ), hb_parni( 3 ) );
  hb_retni( success );
}

//-------------------------------------------------------------------------------

/*    USB4H_ClearHalt()

      Clear the halt / stall condition for an endpoint  */

HB_FUNC( USB4H_CLEARHALT )
{
  int success;
  success = libusb_clear_halt( ( libusb_device_handle * ) hb_parnl( 1 ), ( unsigned char ) hb_parni( 2 ) );
  hb_retni( success );
}

//-------------------------------------------------------------------------------

/*    USB4H_ResetDevice()

      Perform a USB port reset to reinitialise a device.  */

HB_FUNC( USB4H_RESETDEVICE )
{
  int success;
  success = libusb_reset_device( ( libusb_device_handle * ) hb_parnl( 1 ) );
  hb_retni( success ); 
}

//-------------------------------------------------------------------------------

/*    USB4H_KernelDriverActive()

      Determine if a kernel driver is active on an interfacc.  */

HB_FUNC( USB4H_KERNELDRIVERACTIVE )
{
  int isactive;
  isactive = libusb_kernel_driver_active( ( libusb_device_handle * ) hb_parnl( 1 ), hb_parni( 2 ) );
  hb_retni( isactive );
}

//-------------------------------------------------------------------------------

/*    USB4H_DetachKernelDriver()

      Detach a kernel driver from an interface.  */

HB_FUNC( USB4H_DETACHKERNELDRIVER )
{
  int success;
  success = libusb_detach_kernel_driver( ( libusb_device_handle * ) hb_parnl( 1 ), hb_parni( 2 ) );
  hb_retni( success );
}

//-------------------------------------------------------------------------------

/*    USB4H_AttachKernelDriver()

      Reattach a kernel driver which was previously detached.  */

HB_FUNC( USB4H_ATTACHKERNELDRIVER )
{
  int success;
  success = libusb_attach_kernel_driver( ( libusb_device_handle * ) hb_parnl( 1 ), hb_parni( 2 ) );
  hb_retni( success );
}

//-------------------------------------------------------------------------------

/*===============================================================================
  |                                                                             |
  |                               USB descriptors                               |
  |                                                                             |
  ===============================================================================*/

/*    USB4H_GetDeviceDescriptor()

      Get the USB descriptor for a given device.  */

HB_FUNC( USB4H_GETDEVICEDESCRIPTOR )
{
  struct libusb_device_descriptor desc;
  libusb_device ** devicelist; 
  int success; 
  devicelist = ( libusb_device ** ) hb_parnl( 1 );
  success = libusb_get_device_descriptor( devicelist[ hb_parni( 2 ) ], &desc );
  hb_stornl( ( ULONG ) &desc, 3 );
  hb_storni( desc.idVendor, 4 );
  hb_storni( desc.idProduct, 5 );
  hb_storni( (int ) desc.bNumConfigurations, 6 );
  hb_retni( success );
}

//-------------------------------------------------------------------------------

/*===============================================================================
  |                                                                             |
  |                            synchronous device I/O                           |
  |                                                                             |
  ===============================================================================*/

/*    USB4H_BulkTransfer()

      Perform a USB bulk transfer.  */

HB_FUNC( USB4H_BULKTRANSFER )
{
  int success;
  unsigned char data[512];
  int transferred;
  success = libusb_bulk_transfer( ( libusb_device_handle * ) hb_parnl( 1 ), ( unsigned char ) hb_parni( 2 ), data, 512, &transferred, hb_parni( 3 ) );
  hb_storclen( data, ( long ) transferred, 4 );
  hb_storni( transferred, 5 );
  hb_retni( success );
}

//-------------------------------------------------------------------------------

/*    USB4H_InterruptTransfer()

      Perform a USB interrupt transfer.  */

HB_FUNC( USB4H_INTERRUPTTRANSFER )
{
  int success;
  unsigned char data[512];
  int transferred;
  success = libusb_interrupt_transfer( ( libusb_device_handle * ) hb_parnl( 1 ), ( unsigned char ) hb_parni( 2 ), data, 512, &transferred, hb_parni( 3 ) );
  hb_storclen( data, ( long ) transferred, 4 );
  hb_storni( transferred, 5 );
  hb_retni( success );
}


