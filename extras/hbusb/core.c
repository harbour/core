/*
 * Harbour Project source code
 *
 * Copyright (C) 2012 Doug (dougf at people dot net dot au)
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/* TODO: change raw pointers to GC collected ones? */

#include "hbapi.h"

#include "libusb.h"

/* ------------ library initialisation and deinitialisation -------------------- */

/* Initialises libusb.
   Must be called before calling any other libusb functions.
   Getting a context is optional. */
HB_FUNC( LIBUSB_INIT )
{
   int success;

   if( HB_ISBYREF( 1 ) )
   {
      libusb_context * context;
      success = libusb_init( &context );
      hb_storptr( success == 0 ? context : NULL, 1 );
   }
   else
      success = libusb_init( NULL );

   hb_retni( success );
}

/* Deinitialise libusb. */
HB_FUNC( LIBUSB_EXIT )
{
   libusb_exit( ( libusb_context * ) hb_parptr( 1 ) );
}

/* Sets the message verbosity.
   Refer to hbusb.ch for options */
HB_FUNC( LIBUSB_SET_DEBUG )
{
   libusb_set_debug( ( libusb_context * ) hb_parptr( 1 ), hb_parni( 2 ) );
}

/* ------------------- device handling and enumeration ------------------------- */

/* Returns a list of USB devcices attached to your system. */
HB_FUNC( LIBUSB_GET_DEVICE_LIST )
{
   libusb_device ** devicelist;
   ssize_t          count;

   count = libusb_get_device_list( ( libusb_context * ) hb_parptr( 1 ), &devicelist );

   hb_storptr( devicelist, 2 );

   hb_retns( ( HB_SIZE ) count );
}

/* Frees the list returned by LIBUSB_GetDeviceList(). */
HB_FUNC( LIBUSB_FREE_DEVICE_LIST )
{
   libusb_free_device_list( ( libusb_device ** ) hb_parptr( 1 ), hb_parni( 2 ) );
}

/* Gets the number of the bus that a device is attached to. */
HB_FUNC( LIBUSB_GET_BUS_NUMBER )
{
   uint8_t busnumber;
   libusb_device ** devicelist;

   devicelist = ( libusb_device ** ) hb_parptr( 1 );
   busnumber  = libusb_get_bus_number( devicelist[ hb_parni( 2 ) ] );

   hb_retni( busnumber );
}

/* Gets the address on the bus that a device is attached to. */
HB_FUNC( LIBUSB_GET_DEVICE_ADDRESS )
{
   uint8_t deviceaddress;
   libusb_device ** devicelist;

   devicelist    = ( libusb_device ** ) hb_parptr( 1 );
   deviceaddress = libusb_get_device_address( devicelist[ hb_parni( 2 ) ] );

   hb_retni( deviceaddress );
}

/* Gets the negotiated connection speed for a device.
   Refer to hbusb.ch for speeds. */
#if 0
HB_FUNC( LIBUSB_GET_DEVICE_SPEED )
{
   int devicespeed;
   libusb_device ** devicelist;

   devicelist  = ( libusb_device ** ) hb_parptr( 1 );
   devicespeed = libusb_get_device_speed( devicelist[ hb_parni( 2 ) ] );

   hb_retni( devicespeed );
}
#endif

/* Gets the maximum packet size for a particular endpoint in the active device configuration. */
HB_FUNC( LIBUSB_GET_MAX_PACKET_SIZE )
{
   int maxpacketsize;
   libusb_device ** devicelist;

   devicelist    = ( libusb_device ** ) hb_parptr( 1 );
   maxpacketsize = libusb_get_max_packet_size( devicelist[ hb_parni( 2 ) ], ( unsigned char ) hb_parni( 3 ) );

   hb_retni( maxpacketsize );
}

/* Increments the reference count of a device. */
HB_FUNC( LIBUSB_REF_DEVICE )
{
   libusb_device *  device;
   libusb_device ** devicelist;

   devicelist = ( libusb_device ** ) hb_parptr( 1 );
   device     = libusb_ref_device( devicelist[ hb_parni( 2 ) ] );

   HB_SYMBOL_UNUSED( device );
}

/* Decrements the reference count of a device  */
HB_FUNC( LIBUSB_UNREF_DEVICE )
{
   libusb_device ** devicelist;

   devicelist = ( libusb_device ** ) hb_parptr( 1 );
   libusb_unref_device( devicelist[ hb_parni( 2 ) ] );
}

/* Open a device and obtain a device handle  */
HB_FUNC( LIBUSB_OPEN )
{
   int success;
   libusb_device_handle * handle;
   libusb_device **       devicelist;

   devicelist = ( libusb_device ** ) hb_parptr( 1 );
   success    = libusb_open( devicelist[ hb_parni( 2 ) ], &handle );

   hb_storptr( success == 0 ? handle : NULL, 3 );

   hb_retni( success );
}

/* Find a device with known Vendor IID and Product ID. */
HB_FUNC( LIBUSB_OPEN_DEVICE_WITH_VID_PID )
{
   libusb_device_handle * handle;

   handle = libusb_open_device_with_vid_pid( ( libusb_context * ) hb_parptr( 1 ), ( uint16_t ) hb_parni( 2 ), ( uint16_t ) hb_parni( 3 ) );

   hb_retptr( handle );
}

/* Close a device handle. */
HB_FUNC( LIBUSB_CLOSE )
{
   libusb_close( ( libusb_device_handle * ) hb_parptr( 1 ) );
}

/* Get the underlying device for a handle. */
HB_FUNC( LIBUSB_GET_DEVICE )
{
   libusb_device * device;

   device = libusb_get_device( ( libusb_device_handle * ) hb_parptr( 1 ) );

   hb_retptr( device );
}

/* Get the configuration value of the currently active configuration. */
HB_FUNC( LIBUSB_GET_CONFIGURATION )
{
   int configuration;
   int success;

   configuration = 0;
   success       = libusb_get_configuration( ( libusb_device_handle * ) hb_parptr( 1 ), &configuration );

   hb_storni( success == 0 ? configuration : 0, 2 );

   hb_retni( success );
}

/* Set the active configuration for a device. */
HB_FUNC( LIBUSB_SET_CONFIGURATION )
{
   int success;

   success = libusb_set_configuration( ( libusb_device_handle * ) hb_parptr( 1 ), hb_parni( 2 ) );

   hb_retni( success );
}

/* Claim an interface on a given device handle.
   Required before you can perform I/O on any of its endpoints. */
HB_FUNC( LIBUSB_CLAIM_INTERFACE )
{
   int success;

   success = libusb_claim_interface( ( libusb_device_handle * ) hb_parptr( 1 ), hb_parni( 2 ) );

   hb_retni( success );
}

/* Release a previously claimed interface. */
HB_FUNC( LIBUSB_RELEASE_INTERFACE )
{
   int success;

   success = libusb_release_interface( ( libusb_device_handle * ) hb_parptr( 1 ), hb_parni( 2 ) );

   hb_retni( success );
}

/* Activate an alternate setting for an interface. */
HB_FUNC( LIBUSB_SET_INTERFACE_ALT_SETTING )
{
   int success;

   success = libusb_set_interface_alt_setting( ( libusb_device_handle * ) hb_parptr( 1 ), hb_parni( 2 ), hb_parni( 3 ) );

   hb_retni( success );
}

/* Clear the halt / stall condition for an endpoint  */
HB_FUNC( LIBUSB_CLEAR_HALT )
{
   int success;

   success = libusb_clear_halt( ( libusb_device_handle * ) hb_parptr( 1 ), ( unsigned char ) hb_parni( 2 ) );

   hb_retni( success );
}

/* Perform a USB port reset to reinitialise a device. */
HB_FUNC( LIBUSB_RESET_DEVICE )
{
   int success;

   success = libusb_reset_device( ( libusb_device_handle * ) hb_parptr( 1 ) );

   hb_retni( success );
}

/* Determine if a kernel driver is active on an interfacc. */
HB_FUNC( LIBUSB_KERNEL_DRIVER_ACTIVE )
{
   int isactive;

   isactive = libusb_kernel_driver_active( ( libusb_device_handle * ) hb_parptr( 1 ), hb_parni( 2 ) );

   hb_retni( isactive );
}

/* Detach a kernel driver from an interface. */
HB_FUNC( LIBUSB_DETACH_KERNEL_DRIVER )
{
   hb_retni( libusb_detach_kernel_driver( ( libusb_device_handle * ) hb_parptr( 1 ), hb_parni( 2 ) ) );
}

/* Reattach a kernel driver which was previously detached. */
HB_FUNC( LIBUSB_ATTACH_KERNEL_DRIVER )
{
   int success;

   success = libusb_attach_kernel_driver( ( libusb_device_handle * ) hb_parptr( 1 ), hb_parni( 2 ) );

   hb_retni( success );
}

/* ------------------------------ USB descriptors ------------------------------ */

/* Get the USB descriptor for a given device. */
HB_FUNC( LIBUSB_GET_DEVICE_DESCRIPTOR )
{
   struct libusb_device_descriptor desc;
   libusb_device **                devicelist;
   int success;

   devicelist = ( libusb_device ** ) hb_parptr( 1 );
   success    = libusb_get_device_descriptor( devicelist[ hb_parni( 2 ) ], &desc );

   hb_storptr( &desc, 3 );
   hb_storni( desc.idVendor, 4 );
   hb_storni( desc.idProduct, 5 );
   hb_storni( ( int ) desc.bNumConfigurations, 6 );

   hb_retni( success );
}

/* --------------------------- synchronous device I/O -------------------------- */

/* Perform a USB bulk transfer. */
HB_FUNC( LIBUSB_BULK_TRANSFER )
{
   int success;
   unsigned char data[ 512 ];
   int transferred;

   success = libusb_bulk_transfer( ( libusb_device_handle * ) hb_parptr( 1 ), ( unsigned char ) hb_parni( 2 ), data, sizeof( data ), &transferred, hb_parni( 3 ) );

   hb_storclen( ( char * ) data, ( HB_ISIZ ) transferred, 4 );
   hb_storni( transferred, 5 );

   hb_retni( success );
}

/* Perform a USB interrupt transfer. */
HB_FUNC( LIBUSB_INTERRUPT_TRANSFER )
{
   int success;
   unsigned char data[ 512 ];
   int transferred;

   success = libusb_interrupt_transfer( ( libusb_device_handle * ) hb_parptr( 1 ), ( unsigned char ) hb_parni( 2 ), data, sizeof( data ), &transferred, hb_parni( 3 ) );

   hb_storclen( ( char * ) data, ( HB_ISIZ ) transferred, 4 );
   hb_storni( transferred, 5 );

   hb_retni( success );
}
