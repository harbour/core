/* Serial Library 1.4 (22-Jun-2000) (c) 1998 Karl Stenerud
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#ifndef SERIAL__HEADER
#define SERIAL__HEADER

#ifdef __cplusplus
extern "C" {
#endif

/* COM Ports */
#define COM_1                           0
#define COM_2                           1
#define COM_3                           2
#define COM_4                           3

/* Handshaking Modes */
#define SER_HANDSHAKING_NONE            0
#define SER_HANDSHAKING_XONXOFF         1
#define SER_HANDSHAKING_RTSCTS          2
#define SER_HANDSHAKING_DTRDSR          3

/* Error Codes */
#define SER_SUCCESS                     0   /* Function completed successfully */
#define SER_ERR_UNKNOWN                 -1  /* An unknown error occured */
#define SER_ERR_NOT_OPEN                -2  /* The specified COM port is not opened */
#define SER_ERR_ALREADY_OPEN            -3  /* The specified COM port is already opened */
#define SER_ERR_NO_UART                 -4  /* Could not find a UART for this COM port */
#define SER_ERR_INVALID_COMPORT         -5  /* User specified an invalid COM port */
#define SER_ERR_INVALID_BASE            -6  /* User specified an invalid base address */
#define SER_ERR_INVALID_IRQ             -7  /* User specified an invalid IRQ number */
#define SER_ERR_INVALID_BPS             -8  /* User specified an invalid BPS rate */
#define SER_ERR_INVALID_DATA_BITS       -9  /* User specified an invalid number of data bits */
#define SER_ERR_INVALID_PARITY          -10 /* User specified an invalid parity type */
#define SER_ERR_INVALID_STOP_BITS       -11 /* User specified an invalid number of stop bits */
#define SER_ERR_INVALID_HANDSHAKING     -12 /* User specified an invalid handshaking type */
#define SER_ERR_INVALID_FIFO_THRESHOLD  -13 /* User specified an invalid fifo threshold value */
#define SER_ERR_NULL_PTR                -14 /* User specified a buffer address that was NULL */
#define SER_ERR_IRQ_NOT_FOUND           -15 /* Could not find an IRQ for the specified COM port */
#define SER_ERR_LOCK_MEM                -16 /* Could not lock memory in DPMI mode */
#define SER_ERR_UNLOCK_MEM              -17 /* Could not unlock memory in DPMI mode */


/* Name:   serial_open()
 *
 * Desc:   Finds and initializes the specified COM port.
 *
 * Params: int  com:          Communications port (COM_1, COM_2, COM_3, COM_4)
 *         long bps:          Bits per second.
 *                            (50, 75, 110, 150, 300, 600, 1200, 2400, 4800,
 *                            9600, 19200, 38400, 57600, 115200)
 *         int  data_bits:    Number of data word bits (5-8)
 *         char parity:       Parity mode ('e', 'o', 'n', 'm', 's')
 *         int  stop_bits:    Stop bits (1-2). If data_bits=5 and stop_bits=2,
 *                            actual stop bits is 1.5.
 *         int  handshaking:  Handshaking mode
 *                            (SER_HANDSHAKING_NONE, SER_HANDSHAKING_XONXOFF,
 *                            SER_HANDSHAKING_RTSCTS, SER_HANDSHAKING_DTRDSR)
 *
 * Return: SER_SUCCESS or an error code
 */
int  serial_open(int com, long bps, int data_bits, char parity, int stop_bits, int handshaking);


/* Name:   serial_close()
 *
 * Desc:   Clean up and close the specified serial port.
 *
 * Params: int  com:          Communications port (COM_1, COM_2, COM_3, COM_4)
 *
 * Return: SER_SUCCESS or an error code
 */
int  serial_close(int com);


/* Name:   serial_read()
 *
 * Desc:   Read data from the specified serial port buffer.
 *
 * Params: int   com:         Communications port (COM_1, COM_2, COM_3, COM_4)
 *         char* data:        Pointer to data buffer
 *         int   len:         Number of bytes to read
 *
 * Return: number of bytes read or an error code
 */
int  serial_read(int com, char* data, int len);


/* Name:   serial_write() and serial_write_buffered()
 *
 * Desc:   Write data to the serial port.
 *         serial_write() will write the data directly to serial port and will
 *         block until it has completely sent the data or handshaking has
 *         stopped output transmission while serial_write_buffered() will copy
 *         as much as possible data bytes to the transmit buffer and will return
 *         immediately enabling asynchronous output.
 *
 * Params: int   com:         Communications port (COM_1, COM_2, COM_3, COM_4)
 *         char* data:        Pointer to data buffer
 *         int   len:         Number of bytes to write
 *
 * Return: number of bytes written or an error code
 */
int  serial_write         (int com, const char* data, int len);
int  serial_write_buffered(int com, const char* data, int len);


/* Name:   serial_set()
 *
 * Desc:   Change the specified serial port's settings.
 *
 * Params: int   com:         Communications port (COM_1, COM_2, COM_3, COM_4)
 *         long bps:          Bits per second.
 *                            (50, 75, 110, 150, 300, 600, 1200, 2400, 4800,
 *                            9600, 19200, 38400, 57600, 115200)
 *         int   data_bits:   Data bits (5, 6, 7, or 8)
 *         char  parity:      Parity ('n' = none, 'e' = even, 'o' = odd, 'm' = mark, 's' = space)
 *         int  stop_bits:    Stop bits (1-2). If data_bits=5 and stop_bits=2,
 *                            actual stop bits is 1.5.
 *         int   handshaking: Handshaking type (see handshaking modes at top of file)
 *
 * Return: SER_SUCCESS or an error code
 */
int  serial_set(int com, long bps, int data_bits, char parity, int stop_bits, int handshaking);


/* serial_set_xxx() functions.  These take the same arguments
 * as the corresponding ones in serial_set() and return either
 * SER_SUCCESS or an error code.
 */
int  serial_set_bps           (int com, long bps);
int  serial_set_parity        (int com, char parity);
int  serial_set_data          (int com, int data);
int  serial_set_stop          (int com, int stop);
int  serial_set_handshaking   (int com, int handshaking);

/* Advanced settings.  Use these only if you know what you are doing. */

/* UART's FIFO threshold -- 14, 8, 4, 1, or 0 (off) */
int  serial_set_fifo_threshold(int com, int threshold);
/* UART's base address */
int  serial_set_base          (int com, int base);
/* UART's IRQ */
int  serial_set_irq           (int com, int base);

int serial_set_rts(int comport, int rts);
int serial_set_dtr(int comport, int dtr);

int serial_set_mcr(int comport, int mcr);


/* serial_get_xxx() functions.  These return the same types as are supplied
 * to serial_set().  The returned type may be negative, in which case it
 * is an error code.
 */
int  serial_get_base        (int com);
int  serial_get_irq         (int com);
long serial_get_bps         (int com);
char serial_get_parity      (int com);
int  serial_get_data        (int com);
int  serial_get_stop        (int com);
int  serial_get_handshaking (int com);


int serial_get_rts(int comport);
int serial_get_dtr(int comport);
int serial_get_cts(int comport);
int serial_get_dsr(int comport);

int serial_get_mcr(int comport);
int serial_get_msr(int comport);
int serial_get_lsr(int comport);


/* get number of bytes or discard data in TX/RX buffers
 */
int serial_get_tx_buffered(int comport);
int serial_get_rx_buffered(int comport);
int serial_clear_tx_buffer(int comport);
int serial_clear_rx_buffer(int comport);

#ifdef __cplusplus
}
#endif

#endif
