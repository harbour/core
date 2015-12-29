#include <conio.h>
#include <dos.h>
#include "serial.h"

#if defined( __DJGPP__ )
#include <sys/farptr.h>
#include <go32.h>
#include <dpmi.h>
#define Interrupt
#define Far
#define Farpeekw(s, o)              _farpeekw((s),(o))
#define Outp(a,b)                   (outp(a,b),(b))
#define Outpw(a,w)                  (outpw(a,w),(w))
#define CPU_DISABLE_INTERRUPTS()    asm("CLI")
#define CPU_ENABLE_INTERRUPTS()     asm("STI")
#define FP_ADDR(p)                  ((unsigned long)(p))
#elif defined( _MSC_VER ) || defined( __WATCOMC__ )
#define Interrupt                   __interrupt
#define Far                         __far
#define Farpeekw(s, o)              (*((unsigned short Far*)(MK_FP((s), (o)))))
#define Outp(a,b)                   outp(a,b)
#define Outpw(a,w)                  outpw(a,w)
#define CPU_DISABLE_INTERRUPTS()    __asm CLI
#define CPU_ENABLE_INTERRUPTS()     __asm STI
#else /* __BORLANDC__ */
#define Interrupt                   interrupt
#define Far                         far
#define Farpeekw(s, o)              (*((unsigned short Far*)(MK_FP((s), (o)))))
#define Outp(a,b)                   outp(a,b)
#define Outpw(a,w)                  outpw(a,w)
#define CPU_DISABLE_INTERRUPTS()    asm CLI
#define CPU_ENABLE_INTERRUPTS()     asm STI
#endif /* _MSC_VER */
#if defined( __386__ ) || defined( __DJGPP__ )
#define PROTECTED_MODE
#endif

/* ======================================================================== */
/* =========================== DEFINES & MACROS =========================== */
/* ======================================================================== */


/* Tweak Values */
#define SER_RX_BUFFER_SIZE_BITS     11 /* 2048 */
#define SER_RX_BUFFER_HIGH_PERCENT  80
#define SER_RX_BUFFER_LOW_PERCENT   20

#define SER_TX_BUFFER_SIZE_BITS     11 /* 2048 */
#define SER_TX_BUFFER_HIGH_PERCENT  80
#define SER_TX_BUFFER_LOW_PERCENT   20

#define UART_FIFO_DEFAULT_THRESHOLD 14  /* 14, 8, 4, 1, or 0 (off) */

#define COM1_DEFAULT_IRQ            4
#define COM2_DEFAULT_IRQ            3
#define COM3_DEFAULT_IRQ            4
#define COM4_DEFAULT_IRQ            3

#define COM_MIN                     COM_1
#define COM_MAX                     COM_4

#define IRQ_MIN                     3
#define IRQ_MAX                     15
#define IRQ_NONE                    0xff


/* Receive and transmit buffers
 *
 * Both head and tail work in pre-increment mode:
 *
 *  H->W->W->W
 *     *  *  *
 *  0  1  2  3  4  5  ...
 *     *  *  *
 *  T->R->R->R
 *
 * Buffer empty:
 *
 *     H
 * ... 4  5  6  7  ...
 *     T
 *
 * Buffer full:
 *
 *     H
 * ... 4  5  6  7  ...
 *        T
 */

#define SER_RX_BUFFER_SIZE          (1L<<SER_RX_BUFFER_SIZE_BITS)
#define SER_RX_BUFFER_SIZE_MASK     (~((-1)<<SER_RX_BUFFER_SIZE_BITS))
#define SER_RX_BUFFER_HIGH          (SER_RX_BUFFER_SIZE*SER_RX_BUFFER_HIGH_PERCENT/100UL)
#define SER_RX_BUFFER_LOW           (SER_RX_BUFFER_SIZE*SER_RX_BUFFER_LOW_PERCENT/100UL)
#define SER_RX_BUFFER_READ(C)       (C)->rx_buff[(C)->rx_tail = ((C)->rx_tail+1) & SER_RX_BUFFER_SIZE_MASK]
#define SER_RX_BUFFER_WRITE(C, D)   (C)->rx_buff[(C)->rx_head = ((C)->rx_head+1) & SER_RX_BUFFER_SIZE_MASK] = D
#define SER_RX_BUFFER_INIT(C)       (C)->rx_head = (C)->rx_tail = 0
#define SER_RX_BUFFER_EMPTY(C)      ((C)->rx_head == (C)->rx_tail)
#define SER_RX_BUFFER_FULL(C)       ((((C)->rx_head+1) & SER_RX_BUFFER_SIZE_MASK) == (C)->rx_tail)
#define SER_RX_BUFFER_CURRENT(C)    (((C)->rx_head - (C)->rx_tail) & SER_RX_BUFFER_SIZE_MASK)
#define SER_RX_BUFFER_LOWATER(C)    (SER_RX_BUFFER_CURRENT(C) < SER_RX_BUFFER_LOW)
#define SER_RX_BUFFER_HIWATER(C)    (SER_RX_BUFFER_CURRENT(C) > SER_RX_BUFFER_HIGH)

#define SER_TX_BUFFER_SIZE          (1L<<SER_TX_BUFFER_SIZE_BITS)
#define SER_TX_BUFFER_SIZE_MASK     (~((-1)<<SER_TX_BUFFER_SIZE_BITS))
#define SER_TX_BUFFER_HIGH          (SER_TX_BUFFER_SIZE*SER_TX_BUFFER_HIGH_PERCENT/100UL)
#define SER_TX_BUFFER_LOW           (SER_TX_BUFFER_SIZE*SER_TX_BUFFER_LOW_PERCENT/100UL)
#define SER_TX_BUFFER_READ(C)       (C)->tx_buff[(C)->tx_tail = ((C)->tx_tail+1) & SER_TX_BUFFER_SIZE_MASK]
#define SER_TX_BUFFER_WRITE(C, D)   (C)->tx_buff[(C)->tx_head = ((C)->tx_head+1) & SER_TX_BUFFER_SIZE_MASK] = D
#define SER_TX_BUFFER_INIT(C)       (C)->tx_head = (C)->tx_tail = 0
#define SER_TX_BUFFER_EMPTY(C)      ((C)->tx_head == (C)->tx_tail)
#define SER_TX_BUFFER_FULL(C)       ((((C)->tx_head+1) & SER_TX_BUFFER_SIZE_MASK) == (C)->tx_tail)
#define SER_TX_BUFFER_CURRENT(C)    (((C)->tx_head - (C)->tx_tail) & SER_TX_BUFFER_SIZE_MASK)
#define SER_TX_BUFFER_LOWATER(C)    (SER_TX_BUFFER_CURRENT(C) < SER_TX_BUFFER_LOW)
#define SER_TX_BUFFER_HIWATER(C)    (SER_TX_BUFFER_CURRENT(C) > SER_TX_BUFFER_HIGH)


/* XON/XOFF Flow Control Commands */
#define SER_XON                     0x11
#define SER_XOFF                    0x13


/* PIC Registers & interrupts */
#define INTERRUPT_VECTOR_OFFSET     8

#define PIC_MASTER                  0x20
#define PIC_SLAVE                   0xa0
#define PIC_EOI                     0x20
#define PIC_RR                      0x02

#define PIC_WRITE_IMR(P, D)         outp((P)|1, D)      /* interrupt masks */
#define PIC_WRITE_OCW2(P, D)        outp(P, D)          /* R, SL, EOI, 0, 0, L2, L1, L0 */
#define PIC_WRITE_OCW3(P, D)        outp(P, (D)|8)      /* 0, ESMM, SMM, 0, 1, P, RR, RIS */
#define PIC_READ_IMR(P)             inp((P)|1)          /* interrupt masks */
#define PIC_END_INTERRUPT(P)        PIC_WRITE_OCW2(P, PIC_EOI)
static unsigned char PIC_READ_IRR(unsigned int port){PIC_WRITE_OCW3(port, PIC_RR); return inp(port);}
#define PIC_ENABLE_IRQ(I)           {if(I <= 7) PIC_WRITE_IMR(PIC_MASTER, PIC_READ_IMR(PIC_MASTER) & ~(1 << I)); \
                                     else PIC_WRITE_IMR(PIC_SLAVE, PIC_READ_IMR(PIC_SLAVE) & ~(1 << ((I)-7)));}
#define PIC_DISABLE_IRQ(I)          {if(I <= 7) PIC_WRITE_IMR(PIC_MASTER, PIC_READ_IMR(PIC_MASTER) | (1 << I)); \
                                     else PIC_WRITE_IMR(PIC_SLAVE, PIC_READ_IMR(PIC_SLAVE) | (1 << ((I)-7)));}


/* UART Registers */
#define UART_RX_BUFFER              0
#define UART_TX_BUFFER              0
#define UART_INTERRUPT_ENABLE       1

#define UART_DIVISOR_LATCH_WORD     0
#define UART_DIVISOR_LATCH_LSB      0
#define UART_DIVISOR_LATCH_MSB      1

#define UART_INTERRUPT_IDENTIFY     2
#define UART_FIFO_CONTROL           2
#define UART_LINE_CONTROL           3
#define UART_MODEM_CONTROL          4
#define UART_LINE_STATUS            5
#define UART_MODEM_STATUS           6


/* UART Read commands (B = UART Base Address <int>) */
#define UART_READ_DATA(C)           inp((C)->base+UART_RX_BUFFER)
#define UART_READ_INTERRUPT_ENABLE(C)   inp((C)->base+UART_INTERRUPT_ENABLE)
#define UART_READ_INTERRUPT_IDENTIFY(C) inp((C)->base+UART_INTERRUPT_IDENTIFY)
#define UART_READ_LINE_CONTROL(C)   inp((C)->base+UART_LINE_CONTROL)
#define UART_READ_MODEM_CONTROL(C)  inp((C)->base+UART_MODEM_CONTROL)
#define UART_READ_LINE_STATUS(C)    ((C)->lsr = inp((C)->base+UART_LINE_STATUS))
#define UART_READ_MODEM_STATUS(C)   ((C)->msr = inp((C)->base+UART_MODEM_STATUS))
#define UART_READ_BPS(C)            ((Outp((C)->base+UART_LINE_CONTROL, inp((C)->base+UART_LINE_CONTROL) | UART_LCR_DIVISOR_LATCH) & 0) |   \
                                    inpw((C)->base+UART_DIVISOR_LATCH_WORD)                                                     |   \
                                    (Outp((C)->base+UART_LINE_CONTROL, inp((C)->base+UART_LINE_CONTROL) & ~UART_LCR_DIVISOR_LATCH) & 0))


/* UART Write Commands (B = UART Base Address <int>, D = Data <char>) */
#define UART_WRITE_DATA(C, D)       outp((C)->base+UART_TX_BUFFER, D)
#define UART_WRITE_INTERRUPT_ENABLE(C, D) ((C)->ier = Outp((C)->base+UART_INTERRUPT_ENABLE, D))
#define UART_WRITE_FIFO_CONTROL(C, D)   ((C)->fcr = Outp((C)->base+UART_FIFO_CONTROL, D))
#define UART_WRITE_LINE_CONTROL(C, D)   ((C)->lcr = Outp((C)->base+UART_LINE_CONTROL, D))
#define UART_WRITE_MODEM_CONTROL(C, D)  ((C)->mcr = Outp((C)->base+UART_MODEM_CONTROL, D))
#define UART_WRITE_BPS(C, D)        {outp((C)->base+UART_LINE_CONTROL, inp((C)->base+UART_LINE_CONTROL) | UART_LCR_DIVISOR_LATCH);  \
                                     (C)->dlatch = Outpw((C)->base+UART_DIVISOR_LATCH_WORD, D);                                     \
                                     outp((C)->base+UART_LINE_CONTROL, inp((C)->base+UART_LINE_CONTROL) & ~UART_LCR_DIVISOR_LATCH);}


/* Interrupt Enable Register Components */
#define UART_IER_MODEM_STATUS       0x08
#define UART_IER_ERRORS             0x04
#define UART_IER_TX_HOLD_EMPTY      0x02
#define UART_IER_DATA_READY         0x01


/* Interrupt Identify Register Components */
#define UART_IIR_FIFO_ENABLE_1      0x80
#define UART_IIR_FIFO_ENABLE_0      0x40
#define UART_IIR_IDENTIFY_2         0x08
#define UART_IIR_IDENTIFY_1         0x04
#define UART_IIR_IDENTIFY_0         0x02
#define UART_IIR_NO_INTERRUPT_PENDING   0x01

#define UART_IIR_MASK               (UART_IIR_NO_INTERRUPT_PENDING | UART_IIR_IDENTIFY_0 | UART_IIR_IDENTIFY_1)
#define UART_IIR_LINE_STATUS        (UART_IIR_IDENTIFY_0 | UART_IIR_IDENTIFY_1)
#define UART_IIR_DATA_READY         UART_IIR_IDENTIFY_1
#define UART_IIR_TX_HOLD_EMPTY      UART_IIR_IDENTIFY_0
#define UART_IIR_MODEM_STATUS       0x00
#define UART_IIR_NO_INTERRUPT       UART_IIR_NO_INTERRUPT_PENDING


/* Fifo Control Register Components */
#define UART_FCR_TRIGGER_1          0x80
#define UART_FCR_TRIGGER_0          0x40
#define UART_FCR_DMA_SELECT         0x08
#define UART_FCR_TX_FIFO_RESET      0x04
#define UART_FCR_RX_FIFO_RESET      0x02
#define UART_FCR_FIFO_ENABLE        0x01

#define UART_FCR_TRIGGER_AT_1       0x00
#define UART_FCR_TRIGGER_AT_4       UART_FCR_TRIGGER_0
#define UART_FCR_TRIGGER_AT_8       UART_FCR_TRIGGER_1
#define UART_FCR_TRIGGER_AT_14      (UART_FCR_TRIGGER_0 | UART_FCR_TRIGGER_1)


/* Line Control Register Components */

#define UART_LCR_DIVISOR_LATCH      0x80
#define UART_LCR_SET_BREAK          0x40
#define UART_LCR_STICK_PARITY       0x20
#define UART_LCR_EVEN_PARITY        0x10
#define UART_LCR_PARITY_ENABLE      0x08
#define UART_LCR_STOPBITS           0x04
#define UART_LCR_WORD_LENGTH_1      0x02
#define UART_LCR_WORD_LENGTH_0      0x01

#define UART_DATA_5                 0x00
#define UART_DATA_6                 UART_LCR_WORD_LENGTH_0
#define UART_DATA_7                 UART_LCR_WORD_LENGTH_1
#define UART_DATA_8                 (UART_LCR_WORD_LENGTH_0 | UART_LCR_WORD_LENGTH_1)
#define UART_DATA_MASK              (UART_LCR_WORD_LENGTH_0 | UART_LCR_WORD_LENGTH_1)

#define UART_PARITY_NONE            0x00
#define UART_PARITY_ODD             UART_LCR_PARITY_ENABLE
#define UART_PARITY_EVEN            (UART_LCR_PARITY_ENABLE | UART_LCR_EVEN_PARITY)
#define UART_PARITY_MARK            (UART_LCR_PARITY_ENABLE | UART_LCR_STICK_PARITY)
#define UART_PARITY_SPACE           (UART_LCR_PARITY_ENABLE | UART_LCR_EVEN_PARITY  | UART_LCR_STICK_PARITY)
#define UART_PARITY_MASK            (UART_LCR_PARITY_ENABLE | UART_LCR_EVEN_PARITY  | UART_LCR_STICK_PARITY)

#define UART_STOP_1                 0x00
#define UART_STOP_2                 UART_LCR_STOPBITS
#define UART_STOP_MASK              UART_LCR_STOPBITS


/* Modem Control Register Components */
#define UART_MCR_LOOPBACK           0x10
#define UART_MCR_OUT2               0x08
#define UART_MCR_OUT1               0x04
#define UART_MCR_RTS                0x02
#define UART_MCR_DTR                0x01
#define UART_MCR_MASK               ( UART_MCR_DTR | UART_MCR_RTS | UART_MCR_OUT1 | UART_MCR_OUT2 | UART_MCR_LOOPBACK )


/* Line Status Register Components */
#define UART_LSR_FIFO_ERROR         0x80
#define UART_LSR_TX_EMPTY           0x40
#define UART_LSR_TX_HOLD_EMPTY      0x20
#define UART_LSR_BREAK_INDICATOR    0x10
#define UART_LSR_FRAME_ERROR        0x08
#define UART_LSR_PARITY_ERROR       0x04
#define UART_LSR_OVERRUN_ERROR      0x02
#define UART_LSR_DATA_READY         0x01


/* Modem Status Register Components */
#define UART_MSR_DCD                0x80
#define UART_MSR_RI                 0x40
#define UART_MSR_DSR                0x20
#define UART_MSR_CTS                0x10
#define UART_MSR_DLT_DCD            0x08
#define UART_MSR_NEW_RING           0x04
#define UART_MSR_DLT_DSR            0x02
#define UART_MSR_DLT_CTS            0x01


/* BPS Rates (based on 1.8432 MHz crystal) */
#define UART_BPS_DIVISOR_50         2304
#define UART_BPS_DIVISOR_75         1536
#define UART_BPS_DIVISOR_110        1047 /* 0.026% error */
#define UART_BPS_DIVISOR_134_5       857 /* 0.058% error */
#define UART_BPS_DIVISOR_150         768
#define UART_BPS_DIVISOR_300         384
#define UART_BPS_DIVISOR_600         192
#define UART_BPS_DIVISOR_1200         96
#define UART_BPS_DIVISOR_1800         64
#define UART_BPS_DIVISOR_2400         48
#define UART_BPS_DIVISOR_3800         32
#define UART_BPS_DIVISOR_4800         24
#define UART_BPS_DIVISOR_7200         16
#define UART_BPS_DIVISOR_9600         12
#define UART_BPS_DIVISOR_19200         6
#define UART_BPS_DIVISOR_38400         3
#define UART_BPS_DIVISOR_57600         2
#define UART_BPS_DIVISOR_115200        1


/* ======================================================================== */
/* ==================== PROTOTYPES, TYPEDEFS & GLOBALS ==================== */
/* ======================================================================== */


typedef struct
{
    unsigned char port;
    unsigned char default_irq;
    unsigned char irq;
    unsigned char open;
    unsigned char ier;
    unsigned char fcr;
    unsigned char lcr;
    unsigned char mcr;
    unsigned char lsr;
    unsigned char msr;
    unsigned char flow_mode;
    unsigned char rx_flow_on;
    unsigned char tx_flow_on;
    unsigned char rx_buff[(unsigned short)SER_RX_BUFFER_SIZE];
    unsigned char tx_buff[(unsigned short)SER_TX_BUFFER_SIZE];
    unsigned int  base;
    unsigned int  dlatch;
    unsigned int  rx_head;
    unsigned int  tx_head;
    unsigned int  rx_tail;
    unsigned int  tx_tail;
} serial_struct;


#if defined( __DJGPP__ )
typedef _go32_dpmi_seginfo int_handler_ptr;
static _go32_dpmi_seginfo g_old_isrs[16];
#else
typedef void Interrupt Far (*int_handler_ptr)(void);
static int_handler_ptr g_old_isrs[16];
#endif
static unsigned char   g_isrs_taken[16] = {0};
#ifdef PROTECTED_MODE
static int_handler_ptr g_isr_addr;
static unsigned int    g_isrs_count = 0;
#endif


/* serial port data */
static serial_struct g_comports[COM_MAX+1] =
{
#if defined( __GNUC__ )
    {port: 0, default_irq: COM1_DEFAULT_IRQ, irq: IRQ_NONE},
    {port: 1, default_irq: COM2_DEFAULT_IRQ, irq: IRQ_NONE},
    {port: 2, default_irq: COM3_DEFAULT_IRQ, irq: IRQ_NONE},
    {port: 3, default_irq: COM4_DEFAULT_IRQ, irq: IRQ_NONE}
#else
    {0, COM1_DEFAULT_IRQ, IRQ_NONE},
    {1, COM2_DEFAULT_IRQ, IRQ_NONE},
    {2, COM3_DEFAULT_IRQ, IRQ_NONE},
    {3, COM4_DEFAULT_IRQ, IRQ_NONE}
#endif
};



/* ======================================================================== */
/* ====================== INTERRUPT SERVICE ROUTINE ======================= */
/* ======================================================================== */

static void Interrupt com_general_isr(void)
{
    serial_struct* com_min = (serial_struct*)(g_comports);
    serial_struct* com_max = com_min + COM_MAX;
    unsigned char int_id;
    unsigned char data;
    unsigned char slave_interrupted = 0;
    serial_struct* com;

    /* Disable IRQs for all COM ports, then re-enable interrupts.
     * This allows other faster devices to be serviced.
     */
    for(com=com_min;com<=com_max;com++)
    {
        if(com->open)
        {
            if(com->irq > 7)
                slave_interrupted = 1;
            PIC_DISABLE_IRQ(com->irq);
        }
    }
    CPU_ENABLE_INTERRUPTS();

    /* Process all pending interrupts */
    for(com=com_min;com<=com_max;com++)
    {
        if(com->open)
        {
            while((int_id=UART_READ_INTERRUPT_IDENTIFY(com) & UART_IIR_MASK) != UART_IIR_NO_INTERRUPT)
            {
                switch(int_id)
                {
                    case UART_IIR_DATA_READY:
                        /* Read all data from the UART */
                        while(UART_READ_LINE_STATUS(com) & UART_LSR_DATA_READY)
                        {
                            data = UART_READ_DATA(com);

                            /* Handle XON/XOFF flow control (TX) */
                            if(com->flow_mode == SER_HANDSHAKING_XONXOFF && (data == SER_XOFF || data == SER_XON))
                            {
                                com->tx_flow_on = data == SER_XON;
                                if(!SER_TX_BUFFER_EMPTY(com) && com->tx_flow_on)
                                    UART_WRITE_INTERRUPT_ENABLE(com, UART_READ_INTERRUPT_ENABLE(com) | UART_IER_TX_HOLD_EMPTY);
                            }

                            /* Store it if there's room, or throw it out */
                            else if(!SER_RX_BUFFER_FULL(com))
                            {
                                SER_RX_BUFFER_WRITE(com, data);

                                /* Flow control (RX) - Turn off if buffer almost full */
                                if(com->rx_flow_on && SER_RX_BUFFER_HIWATER(com))
                                {
                                    com->rx_flow_on = 0;

                                    switch(com->flow_mode)
                                    {
                                        case SER_HANDSHAKING_RTSCTS:
                                            UART_WRITE_MODEM_CONTROL(com, UART_READ_MODEM_CONTROL(com) & ~UART_MCR_RTS);
                                            break;
                                        case SER_HANDSHAKING_NONE:
                                            break;
                                        case SER_HANDSHAKING_XONXOFF:
                                            /* Wait until tx hold register is empty */
                                            while(!(UART_READ_LINE_STATUS(com) & UART_LSR_TX_HOLD_EMPTY))
                                            {}
                                            break;
                                        case SER_HANDSHAKING_DTRDSR:
                                            UART_WRITE_MODEM_CONTROL(com, UART_READ_MODEM_CONTROL(com) & ~UART_MCR_DTR);
                                            break;
                                    }
                                }
                            }
                        }
                        break;
                    /* Change in line status */
                    case UART_IIR_LINE_STATUS:
                        UART_READ_LINE_STATUS(com);
                        break;
                    /* Change in modem status */
                    case UART_IIR_MODEM_STATUS:
                        UART_READ_MODEM_STATUS(com);

                        /* Handle RTS/CTS or DSR/DTR flow control (TX) */
                        if(com->flow_mode == SER_HANDSHAKING_RTSCTS)
                            com->tx_flow_on = (com->msr & UART_MSR_CTS) != 0;
                        else if(com->flow_mode == SER_HANDSHAKING_DTRDSR)
                            com->tx_flow_on = (com->msr & UART_MSR_DSR) != 0;
                        if(!SER_TX_BUFFER_EMPTY(com) && com->tx_flow_on)
                            UART_WRITE_INTERRUPT_ENABLE(com, UART_READ_INTERRUPT_ENABLE(com) | UART_IER_TX_HOLD_EMPTY);
                        break;
                    /* UART is empty */
                    case UART_IIR_TX_HOLD_EMPTY:
                        while(com->tx_flow_on && UART_READ_LINE_STATUS(com) & UART_LSR_TX_HOLD_EMPTY && !SER_TX_BUFFER_EMPTY(com))
                            UART_WRITE_DATA(com, SER_TX_BUFFER_READ(com));
                        if(SER_TX_BUFFER_EMPTY(com) || !com->tx_flow_on)
                            UART_WRITE_INTERRUPT_ENABLE(com, UART_READ_INTERRUPT_ENABLE(com) & ~UART_IER_TX_HOLD_EMPTY);
                        break;
                }
            }
        }
    }

    CPU_DISABLE_INTERRUPTS();

    /* End the interrupt on the PIC */
    if(slave_interrupted)
        PIC_END_INTERRUPT(PIC_SLAVE);
    PIC_END_INTERRUPT(PIC_MASTER);

    /* Re-enable all interrupts */
    for(com=com_min;com<=com_max;com++)
        if(com->open)
            PIC_ENABLE_IRQ(com->irq);

    /* We must explicitely call 'sti' before 'iret' because 'iret'
       won't always restore interrupts in a virtual environment */
    CPU_ENABLE_INTERRUPTS();
}



/* ======================================================================== */
/* =========================== UTILITY ROUTINES =========================== */
/* ======================================================================== */


#ifdef PROTECTED_MODE

/* we do not know the exact size of com_general_isr() function
   but for sure it's not longer then 2048 bytes */
#define ISR_SIZE        2048

#if defined( __DJGPP__ )

static void serial_dpmi_get_pvect(int vector, _go32_dpmi_seginfo *info)
{
    _go32_dpmi_get_protected_mode_interrupt_vector(vector, info);
}

static void serial_dpmi_set_pvect(int vector, _go32_dpmi_seginfo *info)
{
    _go32_dpmi_set_protected_mode_interrupt_vector(vector, info);
}

static int serial_dpmi_lock_memory(void)
{
    unsigned long dataaddr, codeaddr;
    __dpmi_meminfo dataregion, coderegion;

    if(__dpmi_get_segment_base_address(_my_cs(), &codeaddr) == 0 &&
       __dpmi_get_segment_base_address(_my_ds(), &dataaddr) == 0)
    {
        coderegion.handle = 0;
        coderegion.size = ISR_SIZE;
        coderegion.address = codeaddr + FP_ADDR(com_general_isr);
        dataregion.handle = 0;
        dataregion.size = sizeof(g_comports);
        dataregion.address = codeaddr + FP_ADDR(g_comports);
        if(__dpmi_lock_linear_region(&coderegion) == 0)
        {
            if(__dpmi_lock_linear_region(&dataregion) == 0)
            {
                g_isr_addr.pm_offset = FP_ADDR(com_general_isr);
                g_isr_addr.pm_selector = _go32_my_cs();
                if(_go32_dpmi_allocate_iret_wrapper(&g_isr_addr) == 0)
                    return SER_SUCCESS;
                __dpmi_unlock_linear_region(&dataregion);
            }
            __dpmi_unlock_linear_region(&coderegion);
        }
    }
    return SER_ERR_LOCK_MEM;
}

static int serial_dpmi_unlock_memory(void)
{
    int rc = SER_SUCCESS;
    unsigned long baseaddr;
    __dpmi_meminfo region;

    if(__dpmi_get_segment_base_address(_my_ds(), &baseaddr) == 0)
    {
        region.handle = 0;
        region.size = sizeof(g_comports);
        region.address = baseaddr + FP_ADDR(g_comports);
        if(__dpmi_unlock_linear_region(&region) != 0)
            rc = SER_ERR_UNLOCK_MEM;
    }
    else
        rc = SER_ERR_UNLOCK_MEM;
    if(__dpmi_get_segment_base_address(_my_cs(), &baseaddr) == 0)
    {
        region.handle = 0;
        region.size = ISR_SIZE;
        region.address = baseaddr + FP_ADDR(com_general_isr);
        if(__dpmi_unlock_linear_region(&region) != 0)
            rc = SER_ERR_UNLOCK_MEM;
    }
    else
        rc = SER_ERR_UNLOCK_MEM;
    if(_go32_dpmi_free_iret_wrapper(&g_isr_addr) != 0)
        rc = SER_ERR_UNLOCK_MEM;
    return rc;
}

#else /* ! __DJGPP__ */

static void serial_dpmi_get_pvect(int vect, int_handler_ptr *handler)
{
    union REGS r;
    unsigned short sel;
    unsigned long off;

    /* DPMI get protected mode interrupt vector: Int 31H, Fn 0204H */
    r.x.eax = 0x0204;
    r.x.ebx = vect;
    int386(0x31, &r, &r);
    sel = (unsigned short) r.x.ecx;
    off = r.x.edx;

    *handler=(int_handler_ptr) MK_FP(sel, off);
}

static void serial_dpmi_set_pvect(int vect, int_handler_ptr *handler)
{
    union REGS r;
    void Far *ptr;

    /* DPMI set protected mode interrupt vector: Int 31H, Fn 0205H */
    ptr = (void Far *)*handler;
    r.x.eax = 0x0205;
    r.x.ebx = vect;
    r.x.ecx = FP_SEG(ptr);
    r.x.edx = FP_OFF(ptr);
    int386(0x31, &r, &r);
}

static int serial_dpmi_lock_linear_memory(void Far *ptr, unsigned long size)
{
    union REGS r;

    /* DPMI get segment base address: Int 31H, Fn 0006H */
    r.x.eax = 0x0006;
    r.x.ebx = FP_SEG(ptr);
    int386(0x31, &r, &r);
    if(r.w.cflag == 0)
    {
        unsigned long addr = FP_OFF( ptr ) + ((r.w.cx << 16) | r.w.dx);

        /* DPMI lock linear region: Int 31H, Fn 0600H */
        r.x.eax = 0x0600;
        r.x.ebx = addr >> 16;
        r.x.ecx = addr & 0xFFFF;
        r.x.esi = size >> 16;
        r.x.edi = size & 0xFFFF;
        int386(0x31, &r, &r);
        if(r.w.cflag == 0)
        {
            g_isr_addr = com_general_isr;
            return SER_SUCCESS;
        }
    }
    return SER_ERR_LOCK_MEM;
}

static int serial_dpmi_unlock_linear_memory(void Far *ptr, unsigned long size)
{
    union REGS r;

    /* DPMI get segment base address: Int 31H, Fn 0006H */
    r.x.eax = 0x0006;
    r.x.ebx = FP_SEG(ptr);
    int386(0x31, &r, &r);
    if(r.w.cflag == 0)
    {
        unsigned long addr = FP_OFF( ptr ) + ((r.w.cx << 16) | r.w.dx);

        /* DPMI unlock linear region: Int 31H, Fn 0601H */
        r.x.eax = 0x0601;
        r.x.ebx = addr >> 16;
        r.x.ecx = addr & 0xFFFF;
        r.x.esi = size >> 16;
        r.x.edi = size & 0xFFFF;
        int386(0x31, &r, &r);
        if(r.w.cflag == 0)
            return SER_SUCCESS;
    }
    return SER_ERR_UNLOCK_MEM;
}

static int serial_dpmi_lock_memory(void)
{
    int rc;

    if((rc=serial_dpmi_lock_linear_memory(com_general_isr, ISR_SIZE)) == SER_SUCCESS)
    {
        if((rc=serial_dpmi_lock_linear_memory(g_comports, sizeof(g_comports))) != SER_SUCCESS)
            serial_dpmi_unlock_linear_memory(com_general_isr, ISR_SIZE);
    }
    return rc;
}

static int serial_dpmi_unlock_memory(void)
{
    int rc1 = serial_dpmi_unlock_linear_memory(com_general_isr, ISR_SIZE),
        rc2 = serial_dpmi_unlock_linear_memory(g_comports, sizeof(g_comports));

    return rc1 != SER_SUCCESS ? rc1 : rc2;
}

#endif /* ! __DJGPP__ */

static int serial_install_irqhandler(int irq)
{
    /* If we haven't taken this IRQ's ISR already, take it */
    if(!g_isrs_taken[irq])
    {
        if( g_isrs_count++ == 0 )
        {
            int rc;

            /* lock memory used by interrupt handler in DPMI mode */
            if((rc=serial_dpmi_lock_memory()) != SER_SUCCESS)
            {
                --g_isrs_count;
                return rc;
            }
        }
        serial_dpmi_get_pvect(irq+INTERRUPT_VECTOR_OFFSET, &g_old_isrs[irq]);
        serial_dpmi_set_pvect(irq+INTERRUPT_VECTOR_OFFSET, &g_isr_addr);
        g_isrs_taken[irq] = 1;
    }
    return SER_SUCCESS;
}

static int serial_remove_irqhandler(int irq)
{
    int rc = SER_SUCCESS;

    if(g_isrs_taken[irq])
    {
        serial_dpmi_set_pvect(irq+INTERRUPT_VECTOR_OFFSET, &g_old_isrs[irq]);
        g_isrs_taken[irq] = 0;

        if( --g_isrs_count == 0 )
        {
            /* unlock memory used by interrupt handler in DPMI mode */
            rc = serial_dpmi_unlock_memory();
        }
    }
    return rc;
}

#else /* ! PROTECTED_MODE */

static int serial_install_irqhandler(int irq)
{
    /* If we haven't taken this IRQ's ISR already, take it */
    if(!g_isrs_taken[irq])
    {
        g_old_isrs[irq] = _dos_getvect(irq+INTERRUPT_VECTOR_OFFSET);
        _dos_setvect(irq+INTERRUPT_VECTOR_OFFSET, com_general_isr);
        g_isrs_taken[irq] = 1;
    }
    return SER_SUCCESS;
}

static int serial_remove_irqhandler(int irq)
{
    _dos_setvect(irq+INTERRUPT_VECTOR_OFFSET, g_old_isrs[irq]);
    g_isrs_taken[irq] = 0;

    return SER_SUCCESS;
}

#endif /* ! PROTECTED_MODE */


static int serial_find_irq(int comport)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);
    unsigned char imr_m = PIC_READ_IMR(PIC_MASTER); /* Interrupt Mask Registers */
    unsigned char imr_s = PIC_READ_IMR(PIC_SLAVE);
    unsigned char irr_m;                                /* Interrupt Request Registers */
    unsigned char irr_s;

    /* Set up the UART */
    UART_WRITE_MODEM_CONTROL(com, UART_MCR_OUT2);
    UART_WRITE_FIFO_CONTROL(com, 0);

    /* Wait until tx hold register is empty */
    while((UART_READ_LINE_STATUS(com) & UART_LSR_TX_HOLD_EMPTY) == 0)
    {}

    CPU_DISABLE_INTERRUPTS();

    /* Allow any interrupt on PIC */
    PIC_WRITE_IMR(PIC_MASTER, 0);
    PIC_WRITE_IMR(PIC_SLAVE, 0);

    /* Do some initial polls to let things settle (win95 needs this) */
    UART_WRITE_INTERRUPT_ENABLE(com, UART_IER_TX_HOLD_EMPTY);
    PIC_READ_IRR(PIC_MASTER);
    PIC_READ_IRR(PIC_SLAVE);
    UART_WRITE_INTERRUPT_ENABLE(com, 0);
    PIC_READ_IRR(PIC_MASTER);
    PIC_READ_IRR(PIC_SLAVE);

    /* Generate an interrupt and record all active IRQs */
    UART_WRITE_INTERRUPT_ENABLE(com, UART_IER_TX_HOLD_EMPTY);
    irr_m = PIC_READ_IRR(PIC_MASTER);
    irr_s = PIC_READ_IRR(PIC_SLAVE);

    /* Remove the interrupt and mask out all IRQs still active */
    UART_WRITE_INTERRUPT_ENABLE(com, 0);
    irr_m &= ~PIC_READ_IRR(PIC_MASTER);
    irr_s &= ~PIC_READ_IRR(PIC_SLAVE);

    /* Interrupt again to make sure */
    UART_WRITE_INTERRUPT_ENABLE(com, UART_IER_TX_HOLD_EMPTY);
    irr_m &= PIC_READ_IRR(PIC_MASTER);
    irr_s &= PIC_READ_IRR(PIC_SLAVE);

    /* Return everything to normal */
    PIC_WRITE_IMR(PIC_MASTER, imr_m);
    PIC_WRITE_IMR(PIC_SLAVE, imr_s);
    UART_WRITE_INTERRUPT_ENABLE(com, 0);

    CPU_ENABLE_INTERRUPTS();

    switch(irr_m)
    {
        case 0x01:  return 0;
        case 0x02:  return 1;
        case 0x04:
            switch(irr_s)
            {
                case 0x01:  return 8;
                case 0x02:  return 9;
                case 0x04:  return 10;
                case 0x08:  return 11;
                case 0x10:  return 12;
                case 0x20:  return 13;
                case 0x40:  return 14;
                case 0x80:  return 15;
                default:    return 2;
            }
        case 0x08:  return 3;
        case 0x10:  return 4;
        case 0x20:  return 5;
        case 0x40:  return 6;
        case 0x80:  return 7;
    }
    return SER_ERR_IRQ_NOT_FOUND;
}

static int serial_free_irq(int comport)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);
    serial_struct* com_min = (serial_struct*)(g_comports);
    serial_struct* com_max = com_min + COM_MAX;
    unsigned int irq = com->irq;
    serial_struct* ptr;

    if(irq == IRQ_NONE)
        return SER_SUCCESS;

    CPU_DISABLE_INTERRUPTS();

    /* Disable interrupts from the UART */
    UART_WRITE_INTERRUPT_ENABLE(com, 0);
    UART_WRITE_MODEM_CONTROL(com, UART_MCR_OUT2);
    /* Clear the FIFO and rx registers */
    UART_WRITE_FIFO_CONTROL(com, 0);
    UART_READ_DATA(com);
    UART_READ_DATA(com);
    UART_READ_DATA(com);
    UART_READ_DATA(com);
    UART_READ_DATA(com);
    UART_READ_DATA(com);
    UART_READ_DATA(com);
    UART_READ_DATA(com);
    UART_READ_DATA(com);
    UART_READ_DATA(com);
    UART_READ_DATA(com);
    UART_READ_DATA(com);
    UART_READ_DATA(com);
    UART_READ_DATA(com);
    UART_READ_INTERRUPT_IDENTIFY(com);
    UART_READ_LINE_STATUS(com);
    UART_READ_DATA(com);
    /* Clear any other possible interrupt causes */
    UART_READ_INTERRUPT_IDENTIFY(com);
    UART_READ_MODEM_STATUS(com);
    UART_READ_INTERRUPT_IDENTIFY(com);
    UART_READ_LINE_STATUS(com);
    UART_READ_INTERRUPT_IDENTIFY(com);

    com->irq = IRQ_NONE;

    CPU_ENABLE_INTERRUPTS();

    for(ptr=com_min;ptr<=com_max;ptr++)
        if(ptr != com && ptr->irq == irq)
            return SER_SUCCESS;

    /* Disable interrupts from the PIC and restore the old vector */
    PIC_DISABLE_IRQ(irq);

    return serial_remove_irqhandler(irq);
}


/* ======================================================================== */
/* ======================== BASIC SERIAL ROUTINES ========================= */
/* ======================================================================== */

int serial_open(int comport, long bps, int data_bits, char parity, int stop_bits, int handshaking)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);
    int rc = SER_SUCCESS;

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;
    if(com->open)
        return SER_ERR_ALREADY_OPEN;

    com->open = 1;

    /* Initialize buffers */
    com->rx_flow_on = 1;
    com->tx_flow_on = 1;
    SER_RX_BUFFER_INIT(com);
    SER_TX_BUFFER_INIT(com);

    /* look in bios tables (0040:0000 - 0040:0006) for com base addresses */
    if(serial_set_base(comport, Farpeekw(0x0040, comport<<1)) != SER_SUCCESS)
        return SER_ERR_NO_UART;

    /* Turn off interrupts from UART */
    UART_WRITE_INTERRUPT_ENABLE(com, 0);

    /* Auto-detect IRQ if we can */
    if((rc=serial_find_irq(comport)) < 0)
        rc = com->default_irq;

    if((rc=serial_set_irq(comport, rc)) != SER_SUCCESS)
        return rc;

    /* Turn off interrupts from PIC */
    PIC_DISABLE_IRQ(com->irq);

    /* Set the comport */
    if((rc=serial_set(comport, bps, data_bits, parity, stop_bits, handshaking)) == SER_SUCCESS)
    {
        UART_WRITE_MODEM_CONTROL(com, UART_MCR_DTR | UART_MCR_RTS | UART_MCR_OUT1 | UART_MCR_OUT2);
        rc = serial_set_fifo_threshold(comport, UART_FIFO_DEFAULT_THRESHOLD);

        /* Get some info */
        UART_READ_LINE_STATUS(com);
        UART_READ_MODEM_STATUS(com);
    }

    /* Re-enable interrupts */
    UART_WRITE_INTERRUPT_ENABLE(com, UART_IER_DATA_READY | UART_IER_MODEM_STATUS | UART_IER_ERRORS);
    PIC_ENABLE_IRQ(com->irq);

    if(rc != SER_SUCCESS)   /* We failed to set the comport */
        serial_close(comport);

    return rc;
}

int serial_close(int comport)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);
    int rc;

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;
    if(!com->open)
        return SER_ERR_NOT_OPEN;

    /* Restore old interrupts */
    rc = serial_free_irq(comport);

    /* Turn everything off */
    UART_WRITE_INTERRUPT_ENABLE(com, 0);
    UART_WRITE_MODEM_CONTROL(com, 0);
    serial_set_fifo_threshold(comport, 0);

    com->open = 0;

    return rc;
}

int serial_read(int comport, char* data, int len)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);
    int i;

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;
    if(!com->open)
        return SER_ERR_NOT_OPEN;
    if(data == 0)
        return SER_ERR_NULL_PTR;

    CPU_DISABLE_INTERRUPTS();
    /* Fill in the array given to us */
    for(i=0;!SER_RX_BUFFER_EMPTY(com) && i < len;i++)
        data[i] = SER_RX_BUFFER_READ(com);

    /* Flow control (RX) - Turn on if buffer almost empty */
    if(!com->rx_flow_on && SER_RX_BUFFER_LOWATER(com))
    {
        com->rx_flow_on = 1;
        if(com->flow_mode == SER_HANDSHAKING_XONXOFF)
        {
            /* Wait until tx hold register is empty */
            while(!(UART_READ_LINE_STATUS(com) & UART_LSR_TX_HOLD_EMPTY))
            {}
            UART_WRITE_DATA(com, SER_XON);
        }
        else if(com->flow_mode == SER_HANDSHAKING_RTSCTS)
            UART_WRITE_MODEM_CONTROL(com, UART_READ_MODEM_CONTROL(com) | UART_MCR_RTS);
        else if(com->flow_mode == SER_HANDSHAKING_DTRDSR)
            UART_WRITE_MODEM_CONTROL(com, UART_READ_MODEM_CONTROL(com) | UART_MCR_DTR);
    }
    CPU_ENABLE_INTERRUPTS();

    return i;
}


int serial_write(int comport, const char* data, int len)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);
    int i;

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;
    if(!com->open)
        return SER_ERR_NOT_OPEN;
    if(data == 0)
        return SER_ERR_NULL_PTR;

    for(i=0;i < len;i++)
    {
        /* Wait until we can write */
        while(!(UART_READ_LINE_STATUS(com) & UART_LSR_TX_HOLD_EMPTY))
        {}

        /* Write 1 char */
        if(com->tx_flow_on)
            UART_WRITE_DATA(com, data[i]);
        else
            break;
    }
    return i;
}

int serial_write_buffered(int comport, const char* data, int len)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);
    int i;

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;
    if(!com->open)
        return SER_ERR_NOT_OPEN;
    if(data == 0)
        return SER_ERR_NULL_PTR;

    CPU_DISABLE_INTERRUPTS();
    for(i=0;i < len ;i++)
    {
        /* stop when sent buffer is full */
        if(SER_TX_BUFFER_FULL(com))
            break;
        /* Write 1 char */
        SER_TX_BUFFER_WRITE(com, data[i]);
    }

    /* If there's data to send, enable TX_HOLD_EMPTY interrupt */
    if(!SER_TX_BUFFER_EMPTY(com))
        UART_WRITE_INTERRUPT_ENABLE(com, UART_READ_INTERRUPT_ENABLE(com) | UART_IER_TX_HOLD_EMPTY);
    CPU_ENABLE_INTERRUPTS();

    return i;
}


int serial_set(int comport, long bps, int data_bits, char parity, int stop_bits, int handshaking)
{
    int rc;

    if((rc=serial_set_bps(comport, bps)) == SER_SUCCESS)
        if((rc=serial_set_data(comport, data_bits)) == SER_SUCCESS)
            if((rc=serial_set_parity(comport, parity)) == SER_SUCCESS)
                if((rc=serial_set_stop(comport, stop_bits)) == SER_SUCCESS)
                      return serial_set_handshaking(comport, handshaking);
    return rc;
}


/* ======================================================================== */
/* ========================== SETTINGS ROUTINES =========================== */
/* ======================================================================== */


int serial_set_base(int comport, int base)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;
    if(base < 1 || base > 0xfff)
        return SER_ERR_INVALID_BASE;

    com->base = base;
    return SER_SUCCESS;
}

int serial_set_irq(int comport, int irq)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);
    int rc;

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;
    if(irq < IRQ_MIN || irq > IRQ_MAX)
        return SER_ERR_INVALID_IRQ;
    if(!com->open)
        return SER_ERR_NOT_OPEN;

    /* Remove any ISRs on this com port's current IRQ */
    serial_free_irq(comport);

    if((rc=serial_install_irqhandler(irq)) == SER_SUCCESS)
    {
       com->irq = irq;
       PIC_ENABLE_IRQ(com->irq);
    }
    return rc;
}

int serial_set_fifo_threshold(int comport, int threshold)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);
    int i;

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;
    if(!com->open)
        return SER_ERR_NOT_OPEN;

    switch(threshold)
    {
        case 14:
            UART_WRITE_FIFO_CONTROL(com, UART_FCR_FIFO_ENABLE | UART_FCR_RX_FIFO_RESET | UART_FCR_TX_FIFO_RESET | UART_FCR_TRIGGER_AT_14);
            break;
        case 8:
            UART_WRITE_FIFO_CONTROL(com, UART_FCR_FIFO_ENABLE | UART_FCR_RX_FIFO_RESET | UART_FCR_TX_FIFO_RESET | UART_FCR_TRIGGER_AT_8);
            break;
        case 4:
            UART_WRITE_FIFO_CONTROL(com, UART_FCR_FIFO_ENABLE | UART_FCR_RX_FIFO_RESET | UART_FCR_TX_FIFO_RESET | UART_FCR_TRIGGER_AT_4);
            break;
        case 1:
            UART_WRITE_FIFO_CONTROL(com, UART_FCR_FIFO_ENABLE | UART_FCR_RX_FIFO_RESET | UART_FCR_TX_FIFO_RESET | UART_FCR_TRIGGER_AT_1);
            break;
        case 0:
            UART_WRITE_FIFO_CONTROL(com, 0);
            break;
        default:
            return SER_ERR_INVALID_FIFO_THRESHOLD;
    }

    /* Clear out any garbage in the UART interrupts and FIFO */
    for(i=0;i<16;i++)
        UART_READ_DATA(com);
    UART_READ_LINE_STATUS(com);

    return SER_SUCCESS;
}


int serial_set_bps(int comport, long bps)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;
    if(!com->open)
        return SER_ERR_NOT_OPEN;

    switch(bps)
    {
        case 115200L:
            UART_WRITE_BPS(com, UART_BPS_DIVISOR_115200);
            return SER_SUCCESS;
        case 57600L:
            UART_WRITE_BPS(com, UART_BPS_DIVISOR_57600);
            return SER_SUCCESS;
        case 38400L:
            UART_WRITE_BPS(com, UART_BPS_DIVISOR_38400);
            return SER_SUCCESS;
        case 19200L:
            UART_WRITE_BPS(com, UART_BPS_DIVISOR_19200);
            return SER_SUCCESS;
        case 9600L:
            UART_WRITE_BPS(com, UART_BPS_DIVISOR_9600);
            return SER_SUCCESS;
        case 7200L:
            UART_WRITE_BPS(com, UART_BPS_DIVISOR_7200);
            return SER_SUCCESS;
        case 4800L:
            UART_WRITE_BPS(com, UART_BPS_DIVISOR_4800);
            return SER_SUCCESS;
        case 3800L:
            UART_WRITE_BPS(com, UART_BPS_DIVISOR_3800);
            return SER_SUCCESS;
        case 1800L:
            UART_WRITE_BPS(com, UART_BPS_DIVISOR_1800);
            return SER_SUCCESS;
        case 2400L:
            UART_WRITE_BPS(com, UART_BPS_DIVISOR_2400);
            return SER_SUCCESS;
        case 1200L:
            UART_WRITE_BPS(com, UART_BPS_DIVISOR_1200);
            return SER_SUCCESS;
        case 600L:
            UART_WRITE_BPS(com, UART_BPS_DIVISOR_600);
            return SER_SUCCESS;
        case 300L:
            UART_WRITE_BPS(com, UART_BPS_DIVISOR_300);
            return SER_SUCCESS;
        case 150L:
            UART_WRITE_BPS(com, UART_BPS_DIVISOR_150);
            return SER_SUCCESS;
        case 110L:
            UART_WRITE_BPS(com, UART_BPS_DIVISOR_110);
            return SER_SUCCESS;
        case 75L:
            UART_WRITE_BPS(com, UART_BPS_DIVISOR_75);
            return SER_SUCCESS;
        case 50L:
            UART_WRITE_BPS(com, UART_BPS_DIVISOR_50);
            return SER_SUCCESS;
    }
    return SER_ERR_INVALID_BPS;
}


int serial_set_data(int comport, int data_bits)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;
    if(!com->open)
        return SER_ERR_NOT_OPEN;

    switch(data_bits)
    {
        case 5:
            UART_WRITE_LINE_CONTROL(com, (UART_READ_LINE_CONTROL(com) & ~UART_DATA_MASK) | UART_DATA_5);
            return SER_SUCCESS;
        case 6:
            UART_WRITE_LINE_CONTROL(com, (UART_READ_LINE_CONTROL(com) & ~UART_DATA_MASK) | UART_DATA_6);
            return SER_SUCCESS;
        case 7:
            UART_WRITE_LINE_CONTROL(com, (UART_READ_LINE_CONTROL(com) & ~UART_DATA_MASK) | UART_DATA_7);
            return SER_SUCCESS;
        case 8:
            UART_WRITE_LINE_CONTROL(com, (UART_READ_LINE_CONTROL(com) & ~UART_DATA_MASK) | UART_DATA_8);
            return SER_SUCCESS;
    }
    return SER_ERR_INVALID_DATA_BITS;
}


int serial_set_parity(int comport, char parity)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;
    if(!com->open)
        return SER_ERR_NOT_OPEN;

    switch(parity)
    {
        case 'n':
            UART_WRITE_LINE_CONTROL(com, (UART_READ_LINE_CONTROL(com) & ~UART_PARITY_MASK) | UART_PARITY_NONE);
            return SER_SUCCESS;
        case 'e':
            UART_WRITE_LINE_CONTROL(com, (UART_READ_LINE_CONTROL(com) & ~UART_PARITY_MASK) | UART_PARITY_EVEN);
            return SER_SUCCESS;
        case 'o':
            UART_WRITE_LINE_CONTROL(com, (UART_READ_LINE_CONTROL(com) & ~UART_PARITY_MASK) | UART_PARITY_ODD);
            return SER_SUCCESS;
        case 'm':
            UART_WRITE_LINE_CONTROL(com, (UART_READ_LINE_CONTROL(com) & ~UART_PARITY_MASK) | UART_PARITY_MARK);
            return SER_SUCCESS;
        case 's':
            UART_WRITE_LINE_CONTROL(com, (UART_READ_LINE_CONTROL(com) & ~UART_PARITY_MASK) | UART_PARITY_SPACE);
            return SER_SUCCESS;
    }
    return SER_ERR_INVALID_PARITY;
}


int serial_set_stop(int comport, int stop_bits)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;
    if(!com->open)
        return SER_ERR_NOT_OPEN;

    switch(stop_bits)
    {
        case 1:
            UART_WRITE_LINE_CONTROL(com, (UART_READ_LINE_CONTROL(com) & ~UART_STOP_MASK) | UART_STOP_1);
            return SER_SUCCESS;
        case 2:
            UART_WRITE_LINE_CONTROL(com, (UART_READ_LINE_CONTROL(com) & ~UART_STOP_MASK) | UART_STOP_2);
            return SER_SUCCESS;
    }
    return SER_ERR_INVALID_STOP_BITS;
}


int serial_set_handshaking(int comport, int handshaking)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;
    if(!com->open)
        return SER_ERR_NOT_OPEN;

    com->tx_flow_on = 1;
    com->rx_flow_on = 1;

    if(handshaking < SER_HANDSHAKING_NONE || handshaking > SER_HANDSHAKING_DTRDSR)
        return SER_ERR_INVALID_HANDSHAKING;

    com->flow_mode = handshaking;
    return SER_SUCCESS;
}


int serial_set_rts(int comport, int rts)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;
    if(!com->open)
        return SER_ERR_NOT_OPEN;

    if(rts)
        UART_WRITE_MODEM_CONTROL(com, com->mcr | UART_MCR_RTS);
    else
        UART_WRITE_MODEM_CONTROL(com, com->mcr & ~UART_MCR_RTS);

    return SER_SUCCESS;
}


int serial_set_dtr(int comport, int dtr)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;
    if(!com->open)
        return SER_ERR_NOT_OPEN;

    if(dtr)
        UART_WRITE_MODEM_CONTROL(com, com->mcr | UART_MCR_DTR);
    else
        UART_WRITE_MODEM_CONTROL(com, com->mcr & ~UART_MCR_DTR);

    return SER_SUCCESS;
}


int serial_set_mcr(int comport, int mcr)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;
    if(!com->open)
        return SER_ERR_NOT_OPEN;

    UART_WRITE_MODEM_CONTROL(com, mcr & UART_MCR_MASK );

    return SER_SUCCESS;
}



int serial_get_base(int comport)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;
    if(com->base < 1 || com->base > 0xfff)
        return SER_ERR_INVALID_BASE;

    return com->base;
}

int serial_get_irq(int comport)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;
    if(com->irq < IRQ_MIN || com->irq > IRQ_MAX)
        return SER_ERR_INVALID_IRQ;

    return com->irq;
}

long serial_get_bps(int comport)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;
    if(!com->open)
        return SER_ERR_NOT_OPEN;

    switch(UART_READ_BPS(com))
    {
        case UART_BPS_DIVISOR_115200:
            return 115200L;
        case UART_BPS_DIVISOR_57600:
            return 57600L;
        case UART_BPS_DIVISOR_38400:
            return 38400L;
        case UART_BPS_DIVISOR_19200:
            return 19200L;
        case UART_BPS_DIVISOR_9600:
            return 9600L;
        case UART_BPS_DIVISOR_7200:
            return 7200L;
        case UART_BPS_DIVISOR_4800:
            return 4800L;
        case UART_BPS_DIVISOR_3800:
            return 3800L;
        case UART_BPS_DIVISOR_2400:
            return 2400L;
        case UART_BPS_DIVISOR_1800:
            return 1800L;
        case UART_BPS_DIVISOR_1200:
            return 1200L;
        case UART_BPS_DIVISOR_600:
            return 600L;
        case UART_BPS_DIVISOR_300:
            return 300L;
        case UART_BPS_DIVISOR_150:
            return 150L;
        case UART_BPS_DIVISOR_110:
            return 110L;
        case UART_BPS_DIVISOR_75:
            return 75L;
        case UART_BPS_DIVISOR_50:
            return 50L;
    }
    return SER_ERR_INVALID_BPS;
}


int serial_get_data(int comport)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;
    if(!com->open)
        return SER_ERR_NOT_OPEN;

    switch(UART_READ_LINE_CONTROL(com) & UART_DATA_MASK)
    {
        case UART_DATA_5:
            return 5;
        case UART_DATA_6:
            return 6;
        case UART_DATA_7:
            return 7;
        case UART_DATA_8:
            return 8;
    }
    return SER_ERR_INVALID_DATA_BITS;
}


char serial_get_parity(int comport)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;
    if(!com->open)
        return SER_ERR_NOT_OPEN;

    switch(UART_READ_LINE_CONTROL(com) & UART_PARITY_MASK)
    {
        case UART_PARITY_NONE:
            return 'n';
        case UART_PARITY_EVEN:
            return 'e';
        case UART_PARITY_ODD:
            return 'o';
        case UART_PARITY_MARK:
            return 'm';
        case UART_PARITY_SPACE:
            return 's';
    }
    return SER_ERR_INVALID_PARITY;
}


int serial_get_stop(int comport)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;
    if(!com->open)
        return SER_ERR_NOT_OPEN;

    switch(UART_READ_LINE_CONTROL(com) & UART_STOP_MASK)
    {
        case UART_STOP_1:
            return 1;
        case UART_STOP_2:
            return 2;
    }
    return SER_ERR_INVALID_STOP_BITS;
}


int serial_get_handshaking(int comport)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;

    return com->flow_mode;
}


int serial_get_rts(int comport)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;

    return (com->mcr & UART_MCR_RTS) != 0;
}


int serial_get_dtr(int comport)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;

    return (com->mcr & UART_MCR_DTR) != 0;
}


int serial_get_mcr(int comport)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;

    return com->mcr;
}


int serial_get_dsr(int comport)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;

    return (com->msr & UART_MSR_DSR) != 0;
}


int serial_get_cts(int comport)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;

    return (com->msr & UART_MSR_CTS) != 0;
}


int serial_get_msr(int comport)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;

    return com->msr;
}


int serial_get_lsr(int comport)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;

    return com->lsr;
}



int serial_get_tx_buffered(int comport)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);
    int count;

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;
    if(!com->open)
        return SER_ERR_NOT_OPEN;

    CPU_DISABLE_INTERRUPTS();
    count = SER_TX_BUFFER_CURRENT(com);
    CPU_ENABLE_INTERRUPTS();

    return count;
}


int serial_get_rx_buffered(int comport)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);
    int count;

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;
    if(!com->open)
        return SER_ERR_NOT_OPEN;

    CPU_DISABLE_INTERRUPTS();
    count = SER_TX_BUFFER_CURRENT(com);
    CPU_ENABLE_INTERRUPTS();

    return count;
}


int serial_clear_tx_buffer(int comport)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;
    if(!com->open)
        return SER_ERR_NOT_OPEN;

    CPU_DISABLE_INTERRUPTS();
    SER_TX_BUFFER_INIT(com);
    CPU_ENABLE_INTERRUPTS();

    return SER_SUCCESS;
}


int serial_clear_rx_buffer(int comport)
{
    serial_struct* com = (serial_struct*)(g_comports + comport);

    if(comport < COM_MIN || comport > COM_MAX)
        return SER_ERR_INVALID_COMPORT;
    if(!com->open)
        return SER_ERR_NOT_OPEN;

    CPU_DISABLE_INTERRUPTS();
    SER_RX_BUFFER_INIT(com);
    CPU_ENABLE_INTERRUPTS();

    return SER_SUCCESS;
}
