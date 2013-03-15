/* com.c */

/* ************************************************************************

                            COMLib, version 1.0

                      RS-232 comunication library

                           started: may/1998

         Program and Text Copyright, ( C ) 1998, Peter Marinov
                          All Rights Reserved.

************************************************************************ */

/*
Compile options:
  /DDISABLE_TIMING -- will disable the library to use the system timer
    and this will disable all the xxxTimed() functions from operation as well
  /DDISABLE_PREEMPTING -- will turn off the ability to be preempted the
    process of handling COM IRQs.
*/

#include <dos.h>
#include <mem.h>
#include <stdlib.h>
#include <string.h>  /* memset() for djgpp is declared here */

#if defined( __WATCOMC__ )
#include <conio.h>
#define inportb(x)      inp(x)
#define outportb(x,y)   outp(x,y)
#define enable()        _enable()
#define disable()       _disable()
#if !defined(__INLINE_FUNCTIONS__)
//#error NO INLINE FUNCTIONS
#endif
#endif

#ifdef _DEBUG
#include <stdio.h>
#endif

#include <assert.h>

#include "irq.h"
#include "com.h"
#ifndef DISABLE_TIMING
#include "timer.h"
#endif

#ifdef _DEBUG
#define ASSERT(x)        assert(x)
#else
#define ASSERT(x)
#endif

#define gettimer()  _peekd(0x40, 0x6c)

#define TRUE 1
#define FALSE 0

/* COM registers */
#define regTHR    0  /* (w) Transmitter holding register */
#define regRBR    0  /* (r) Receive Buffer register */
#define regDLL    0  /* (rw) Divisor latch low byte (DLL) when DLAB = 1 */
#define regDLM    1  /* (rw) Divisor latch high byte (DLM) when DLAB = 1 */
#define regIER    1  /* (rw) Interrupt enable register when DLAB = 0 */
#define regIIR    2  /* (r) Interrupt identification register */
#define regFCR    2  /* (w) FIFO control register */
#define regLCR    3  /* (rw) Line control register */
#define regMCR    4  /* (rw) Modem control register */
#define regLSR    5  /* (r) Line status register */
#define regMSR    6  /* (r) Modem status register */
#define regSCR    7  /* (rw) Scratch register */

/* Some macroses to improve the the source comprehension */
#define THR(pCOM) (pCOM->nCOMAddress + regTHR)
#define RBR(pCOM) (pCOM->nCOMAddress + regRBR)
#define DLL(pCOM) (pCOM->nCOMAddress + regDLL)
#define DLM(pCOM) (pCOM->nCOMAddress + regDLM)
#define IER(pCOM) (pCOM->nCOMAddress + regIER)
#define IIR(pCOM) (pCOM->nCOMAddress + regIIR)
#define FCR(pCOM) (pCOM->nCOMAddress + regFCR)
#define LCR(pCOM) (pCOM->nCOMAddress + regLCR)
#define MCR(pCOM) (pCOM->nCOMAddress + regMCR)
#define LSR(pCOM) (pCOM->nCOMAddress + regLSR)
#define MSR(pCOM) (pCOM->nCOMAddress + regMSR)
#define SCR(pCOM) (pCOM->nCOMAddress + regSCR)

/*
Pair of macroses to arrange proper using of the queues
of a particular COM.
Check if an IRQ service is in progress and disable IRQs
to start a critical section to enable using a queue.
*/
#define ENTER_QUEUE_CRITICAL_SECTION(pCOM)\
  {if (!pCOM->bHandlingIRQ)  disable();}
#define LEAVE_QUEUE_CRITICAL_SECTION(pCOM)\
  {if (!pCOM->bHandlingIRQ)  enable();}

#define DEFAULT_QUEUE_SIZE  255
#define DEFAULT_TX_THRESHOLD 16
#define DEFAULT_RX_THRESHOLD 8

char COM_LIB_ID[] = "*** COM library ***";

static int bCOMInit = FALSE;  /* Set TRUE by COMInit */

struct TQueue
{
  int nTail, nHead;
  char *pBuf;
  int nBufSize;
};

struct COMDesc
{
  /* Hardware settings */
  int nIRQ;  /* IRQ line, not int vector! */
  int nCOMAddress;  /* IO Base address of the COM port */
  int bHardwareSet;  /* Whether harware settings were set */

  /* Hardware FIFO Settings */
  int nChipset;  /* 1: 8250, 2: 16450, 3: 16550, 4: 16550A, 0: No chip */
  int bFIFOAvailable;  /* If nChipset >= 4 this flag is TRUE */
  int bFIFOEnabled;  /* Managed by COMEnable/DisableHardwareFIFO() */
  int nRXThreshold;  /* RX fifo threshold, if not set 8 is used */
  int nTXThreshold;  /* TX fifo threshold (no phisycal meaning!) */

  /* IRQ attachement list */
  struct COMDesc *pNextPort;  /* Next port attached to same IRQ, null - last */

  /* Transmit and receive queues */
  int nRXQueueSize;  /* Allocation size, to be changed by the user */
  int nTXQueueSize;  /* Allocation size, to be changed by the user */
  struct TQueue RXQueue;  /* The receive queue */
  struct TQueue RXStatQueue;  /* The receive queue -- stat info */
  struct TQueue TXQueue;  /* The transmit queue */
  struct TQueue TXStatQueue;  /* The transmit queue -- stat info */

  void (*EventHandler)(int nEvent);  /* Event call-back routine */
  int nFlowControl;  /* NONE, XONOFF, RTSCTS */
  int nFlowState;  /* Current flow control state */
  int bInstalled;  /* Whether COM is activated by COMPortOpen() */
  long nBauds;  /* Transmition parameter */
  int nParity;  /* Transmition parameter */
  int nWordLen;  /* Transmition parameter */
  int nStopBits;  /* Transmition parameter */
  int nCOMError;  /* If an error while processing particular COM */

  int nSaveIER;  /* Used by COMWrap() as a storage of IER */
  int nSaveMSR;  /* COMHandler() stores here the last modem status */
  int nSaveLCR;  /* To ease switching for 9bit protocols */

  int bHandlingIRQ;  /* Indicates whether COMHandler is executing */

  #ifdef _DEBUG
  /* Diagnostic counters */
  int nRX;  /* Received characters, not RX IRQs number */
  int nTX;  /* Transmition IRQs */
  int nModem;  /* Modem IRQs */
  int nStat;  /* Line error characters */
  int nIRQs;  /* Total number of IRQs handled */
  #endif

  int nLastBit9;
  struct COMStat cStat;  /* Collect rx char information here before storing in queue */
  char cBuf;

  char cXon;
  char cXoff;
};

/*
All the COMs that can be opened simultaneously
*/
static struct COMDesc COMs[COMMAX];

/*
Opening COM port attaches a COMDesc instance to specific position
depending on nIRQ value
*/
static struct COMDesc *pAttachedPorts[16] =
{
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL
};

static void COMHandler(struct COMDesc *pCOM);  /* Forward declaration */

/* ************************************************************************
   Function: EnableIRQ
   Description:
     Enables specific IRQ in 8259 interrupt controller.
*/
static void EnableIRQ(int nIRQ)
{
  if (nIRQ >= 8)
    outportb(0xa1, inportb(0xa1) & ~(1 << (nIRQ - 8)));
  else
    outportb(0x21, inportb(0x21) & ~(1 << nIRQ));
}
END_OF_FUNCTION(EnableIRQ);

/* ************************************************************************
   Function: DisableIRQ
   Description:
     Disables specific IRQ in 8259 interrupt controller.
*/
static void DisableIRQ(int nIRQ)
{
  if (nIRQ >= 8)
    outportb(0xa1, inportb(0xa1) | (1 << (nIRQ - 8)));
  else
    outportb(0x21, inportb(0x21) | (1 << nIRQ));
}
END_OF_FUNCTION(DisableIRQ);

/* ************************************************************************
   Function: EndIRQ
   Description:
     Issues and end of IRQ command toward 8259 interrupt controller.
*/
static void EndIRQ(int nIRQ)
{
  outportb(0x20, 0x20);
  if (nIRQ > 7)
    outportb(0xa0, 0x20);
}
END_OF_FUNCTION(EndIRQ);

/* ************************************************************************
   Function: COMDisableIRQs
   Description:
     Disables IRQs depending on DISABLE_PREEMPTING definition
*/
void COMDisableIRQs(void)
{
#ifndef DISABLE_PREEMPTING
  disable();
#endif
}
END_OF_FUNCTION(COMDisableIRQs);

/* ************************************************************************
   Function: COMEnableIRQs
   Description:
     Enables IRQs depending on DISABLE_PREEMPTING definition
*/
void COMEnableIRQs(void)
{
#ifndef DISABLE_PREEMPTING
  enable();
#endif
}
END_OF_FUNCTION(COMEnableIRQs);

/*
Define com port wrappers for each IRQ vector.
1. Disable the specific irq line in 8259 interrupt controller.
2. Issue EndOfInterrupt to 8259. Now we can issue EndOfInterrupt
because this particular interrupt is disabled.
3. sti. This will enable all other IRQs to be handled during processing
this particular COM.
4. Handle all the attached COMs to this IRQ line.
5. cli.
6. Disable and enable all the irq masks for all the attached COMs.
This will generate an edge event to 8259 and that will be registered if
some COM event was not served during this session.
7. Enable back this IRQ in 8259 interrupt controller.

Q: Why is DISABLE_PREEMPTING necessary?
A: Putting COM library to work together with a preempting multi-thread
support library would harm COM IRQ serving if preempting is enabled.
Example: COM IRQ occures while a low priority thread is in progress, then
timer0 IRQ occures and preempts this thread (actually preempts COMHandler)
the thread (and the COMHandler) will take execution quant after
all the high priority threads are serviced, which will improperly delay
the handing of the COM event.
*/
#define COMWRAP(x)\
static int COMWrap##x(void)\
{\
  struct COMDesc *pCOM;\
\
  DisableIRQ(x);\
  EndIRQ(x);\
\
  COMEnableIRQs();\
\
  for (pCOM = pAttachedPorts[x]; pCOM != NULL; pCOM = pCOM->pNextPort)\
    COMHandler(pCOM);\
\
  COMDisableIRQs();\
\
  for (pCOM = pAttachedPorts[x]; pCOM != NULL; pCOM = pCOM->pNextPort)\
    outportb(IER(pCOM), 0);\
  for (pCOM = pAttachedPorts[x]; pCOM != NULL; pCOM = pCOM->pNextPort)\
    outportb(IER(pCOM), pCOM->nSaveIER);\
\
  EnableIRQ(x);\
  return (0);\
}




#undef COMWRAP
#define COMWRAP(x)\
static int COMWrap##x(void)\
{\
  struct COMDesc *pCOM;\
\
  DisableIRQ(x);\
  EndIRQ(x);\
\
  COMEnableIRQs();\
\
  for (pCOM = pAttachedPorts[x]; pCOM != NULL; pCOM = pCOM->pNextPort)\
    COMHandler(pCOM);\
\
  COMDisableIRQs();\
\
  for (pCOM = pAttachedPorts[x]; pCOM != NULL; pCOM = pCOM->pNextPort)\
    outportb(IER(pCOM), 0);\
  for (pCOM = pAttachedPorts[x]; pCOM != NULL; pCOM = pCOM->pNextPort)\
    outportb(IER(pCOM), pCOM->nSaveIER);\
\
  EnableIRQ(x);\
  return (0);\
}


COMWRAP(0);
COMWRAP(1);
COMWRAP(2);
COMWRAP(3);  /* Comment this when uncommenting COMWrap3 below! */
/* This is COMWrap3 -- COM2, uncomment for debugging */
/*
static int COMWrap3(void)
{
  struct COMDesc *pCOM;

  DisableIRQ(3);
  EndIRQ(3);

  COMEnableIRQs();

  for (pCOM = pAttachedPorts[3]; pCOM != NULL; pCOM = pCOM->pNextPort)
    COMHandler(pCOM);

  COMDisableIRQs();

  for (pCOM = pAttachedPorts[3]; pCOM != NULL; pCOM = pCOM->pNextPort)
    outportb(IER(pCOM), 0);
  for (pCOM = pAttachedPorts[3]; pCOM != NULL; pCOM = pCOM->pNextPort)
    outportb(IER(pCOM), pCOM->nSaveIER);

  EnableIRQ(3);
  return (0);
}
*/

COMWRAP(4);
COMWRAP(5);
COMWRAP(6);
COMWRAP(7);
COMWRAP(8);
COMWRAP(9);
COMWRAP(10);
COMWRAP(11);
COMWRAP(12);
COMWRAP(13);
COMWRAP(14);
COMWRAP(15);
void COMWrap0_End(void)
{
}  /* Fictive function to mark the end of the COMWrapX() code region. */

typedef int (*TCOMWrapper)(void);

/*
COMWraperX addresses to be easy accessible at nIRQ index positions
*/
TCOMWrapper COMWrappers[16] =
{
  COMWrap0,
  COMWrap1,
  COMWrap2,
  COMWrap3,
  COMWrap4,
  COMWrap5,
  COMWrap6,
  COMWrap7,
  COMWrap8,
  COMWrap9,
  COMWrap10,
  COMWrap11,
  COMWrap12,
  COMWrap13,
  COMWrap14,
  COMWrap15
};

/*
QUEUE managing functions.

Take care to enclose each call of queue operating functions
with ENTER_QUEUE_CRITICAL_SECTION() and LEAVE_QUEUE_CRITICAL_SECTION()
for the specific COM port.
*/

static void * XMemCpy( void * to, const void * from, int count )
{
  const char * src = ( const char * ) from;
  char * dst = ( char * ) to;

  while( --count >= 0 )
    *dst++ = *src++;

  return to;
}
END_OF_FUNCTION(XMemCpy);

/* ************************************************************************
   Function: QUEUEPut
   Description:
     Stores array of bytes into the FIFO buffer.
   On entry:
     pBuf, nSize -- array of bytes to put in the queue.
   QUEUECalcFifoFree() should be called to calculate the amount
   of free space prior to invoke this function.
*/
static void QUEUEPut(struct TQueue *pQueue, const char *pBuf, int nSize)
{
  int nToCopy;

  ASSERT(pQueue != NULL);

  nToCopy = pQueue->nBufSize - pQueue->nTail;

  /* Don't copy more than nSize */
  if (nToCopy > nSize)
    nToCopy = nSize;  /* This ensures that nTail wont wrap nHead as well */

  XMemCpy(pQueue->pBuf + pQueue->nTail, pBuf, nToCopy);

  if ((nSize -= nToCopy) > 0)
  {
    /* Rest of the bytes at the start of the buffer */
    XMemCpy(pQueue->pBuf, pBuf + nToCopy, nSize);
    pQueue->nTail = nSize;
  }
  else
  {
    /* Adjust the tail */
    if ((pQueue->nTail += nToCopy) == pQueue->nBufSize)
      pQueue->nTail = 0;
  }
}
END_OF_FUNCTION(QUEUEPut);

/* ************************************************************************
   Function: QUEUEGet
   Description:
     Reads a sequence of bytes from the FIFO buffer.
   On exit:
     pBuf will hold nSize bytes from the queue.
*/
static void QUEUEGet(struct TQueue *pQueue, char *pBuf, int nSize)
{
  int nToCopy;

  ASSERT(pQueue != NULL);
  ASSERT(pBuf != NULL);

  nToCopy = pQueue->nBufSize - pQueue->nHead;

  /* Don't copy more than nSize */
  if (nToCopy > nSize)
    nToCopy = nSize;

  XMemCpy(pBuf, pQueue->pBuf + pQueue->nHead, nToCopy);

  if ((nSize -= nToCopy) > 0)
  {
    XMemCpy(pBuf + nToCopy, pQueue->pBuf, nSize);
    pQueue->nHead = nSize;
  }
  else
  {
    /* Adjust head */
    if ((pQueue->nHead += nToCopy) == pQueue->nBufSize)
      pQueue->nHead = 0;
  }
}
END_OF_FUNCTION(QUEUEGet);

/* ************************************************************************
   Function: QUEUECalcFree
   Description:
     Calculates the amount of free space in a queue
*/
static int QUEUECalcFree(struct TQueue *pQueue)
{
  int f;
  int nHead;
  int nTail;

  ASSERT(pQueue != NULL);

  nHead = pQueue->nHead;
  nTail = pQueue->nTail;

  if (nTail >= nHead)
    f = nTail - nHead;
  else  /* Tail wrapped arround */
    f = nTail + (pQueue->nBufSize - nHead);

  return (pQueue->nBufSize - f - 1);  /* -1 because Tail never treads on Head */
}
END_OF_FUNCTION(QUEUECalcFree);

/* ************************************************************************
   Function: QUEUECalcOcupied
   Description:
     Calculates the amount of occupied space in a queue.
*/
static int QUEUECalcOccupied(struct TQueue *pQueue)
{
  int f;
  int nHead;
  int nTail;

  ASSERT(pQueue != NULL);

  nHead = pQueue->nHead;
  nTail = pQueue->nTail;

  if (nTail >= nHead)
    f = nTail - nHead;
  else  /* Tail wrapped arround */
    f = nTail + (pQueue->nBufSize - nHead);

  return (f);
}
END_OF_FUNCTION(QUEUECalcOccupied);

/* ************************************************************************
   Function: QUEUEAdvance
   Description:
     Calculates the amount of occupied space in a queue.
*/
static void QUEUEAdvance(struct TQueue *pQueue, int *pIndex, int nAdvanceBy)
{
  ASSERT(pQueue != NULL);
  ASSERT(pIndex != NULL);

  if ((*pIndex += nAdvanceBy) >= pQueue->nBufSize)
    *pIndex = *pIndex - pQueue->nBufSize;
}
END_OF_FUNCTION(QUEUEAdvance);

/* ************************************************************************
   Function: QUEUEInit
   Description:
     Inital setup of a queue
*/
static void QUEUEInit(struct TQueue *pQueue, char *pBuf, int nBufSize)
{
  ASSERT(pQueue != NULL);

  pQueue->pBuf = pBuf;
  pQueue->nBufSize = nBufSize;
  pQueue->nHead = pQueue->nTail = 0;
}

/* ************************************************************************
   Function: QUEUEDetachBuf
   Description:
     Detaches the queue buffer.
*/
static char *QUEUEDetachBuf(struct TQueue *pQueue)
{
  char *pBuf;
  ASSERT(pQueue != NULL);

  pQueue->nHead = pQueue->nTail = 0;
  pBuf = pQueue->pBuf;
  pQueue->pBuf = NULL;
  return (pBuf);
}

/* ************************************************************************
   Function: COMHandler
   Description:
     Handles a COM port IRQ request.
     Entering this function IRQs are enabled depending on DISABLE_PREEMPTING
     definition.
*/

static void COMHandler(struct COMDesc *pCOM)
{
  int nIntID;
  int nToPut;
  int nLCR;
  int nTXThreshold;

  pCOM->bHandlingIRQ = TRUE;

  pCOM->cStat.nStat = 0;
  pCOM->cStat.nBit9 = 0;

  #ifdef _DEBUG
  ++pCOM->nIRQs;
  #endif

  /* Bit 0 of IIR indicates for pending interrupt requests */
  while (((nIntID = inportb(IIR(pCOM))) & 1) == 0)
    switch (nIntID & 0x6)
    {
      case 0x6:  /* status interrupt */
      #ifdef _DEBUG
      ++pCOM->nStat;
      #endif
      pCOM->cStat.nStat = inportb(LSR(pCOM)) & 0x1e;
      /* TODO: Test BREAK signal -- if rx event will come, otherwise the event will be lost */

      /* Call user event handler -- atomic operation */
      /* User will get info about the stat code on the next
      character read by COMReadChar() in the nStat field */
      if (pCOM->EventHandler != NULL)
      {
        COMDisableIRQs();
        pCOM->EventHandler(evCOMStat);
        COMEnableIRQs();
      }
      break;
      case 0x4:  /* rx interrupt */
      #ifdef _DEBUG
      ++pCOM->nRX;
      #endif
      pCOM->cBuf = inportb(RBR(pCOM));

      if (pCOM->nParity == '9')
      {
        if ((inportb(LCR(pCOM)) & 0x10) == 0)  /* Current parity is mark */
        {
          if ((pCOM->cStat.nStat & statParity) == 0)
            pCOM->cStat.nBit9 = 1;
        }
        else  /* Current parity is space */
          if ((pCOM->cStat.nStat & statParity) != 0)
            pCOM->cStat.nBit9 = 1;
      }

      if (QUEUECalcFree(&pCOM->RXQueue) < 1)
        pCOM->nCOMError = COMERR_RXOVERFLOW;  /* Indicate outside IRQ hndl */
      else
      {
        QUEUEPut(&pCOM->RXQueue, &pCOM->cBuf, 1);

        ASSERT(QUEUECalcFree(&pCOM->RXStatQueue) >= (int)sizeof(struct COMStat));
        QUEUEPut(&pCOM->RXStatQueue, (char *)&pCOM->cStat, sizeof(struct COMStat));
      }

      /* Prepare for the next iteration */
      pCOM->cStat.nStat = 0;
      pCOM->cStat.nBit9 = 0;

      /* Call user event handler -- atomic operation */
      if (pCOM->EventHandler != NULL)
      {
        COMDisableIRQs();
        pCOM->EventHandler(evCOMRx);
        COMEnableIRQs();
      }
      break;
      case 0x2:  /* tx interrupt */
      #ifdef _DEBUG
      ++pCOM->nTX;
      #endif

      /*
      9bit mode:
      Q: How this TX IRQ actually works?
      A: There are 2 registers in COM port 1-transmit reg and 2-shifting
      reg. IRQ occures whenever a byte from reg 1 is moved to reg 2. The
      9th bit adhesion takes place when all the byte has been shifted out
      (i.e. transmitted) from reg 2. So when an IRQ occures our previous
      byte has just been put in the shift reg and in a while will need
      its 9th bit. That's why whenever TX IRQ occures it is necessary to
      put in LCR the 9bit of this byte (previous byte) and then to load the
      transmit reg (reg 1).
      */
      if (pCOM->nParity == '9')
      {
        nLCR = inportb(LCR(pCOM));
        if (pCOM->nLastBit9)  /* Set mark parity */
        {
          if ((nLCR & 0x10) != 0)
            outportb(LCR(pCOM), nLCR & 0xef);
        }
        else
        {
          if ((nLCR & 0x10) == 0)   /* Set space parity */
            outportb(LCR(pCOM), nLCR | 0x10);
        }
      }

        nTXThreshold = 1;  /* Assume no hardware FIFO enabled */
        if (pCOM->bFIFOEnabled)
        {
          nTXThreshold = 16;  /* Assume default value */
          if (pCOM->nTXThreshold != 0)
            nTXThreshold = pCOM->nTXThreshold;
        }

      nToPut = QUEUECalcOccupied(&pCOM->TXQueue);
      if (nToPut > 0)
      {
        if (nToPut > nTXThreshold)
          nToPut = nTXThreshold;

        do
        {
          QUEUEGet(&pCOM->TXQueue, &pCOM->cBuf, 1);
          outportb(THR(pCOM), pCOM->cBuf);
          QUEUEGet(&pCOM->TXStatQueue, (char *)&pCOM->cStat, sizeof(struct COMStat));
          pCOM->nLastBit9 = pCOM->cStat.nBit9;
        }
        while (--nToPut);

        /* Prepare for the next iteration -- if rx event is about to come */
        pCOM->cStat.nStat = 0;
        pCOM->cStat.nBit9 = 0;

        /* Call user event handler -- atomic operation */
        /* Indicate that the last transmition operation has finished */
        if (pCOM->EventHandler != NULL)
        {
          COMDisableIRQs();
          pCOM->EventHandler(evCOMTx);
          COMEnableIRQs();
        }
        break;
      }
      /* No more characters -- mask tx interrupt in IER */
      pCOM->nSaveIER &= 0xd;  /* COMWrap() will put the value to IER */
      break;
      case 0:  /* modem interrupt */
      #ifdef _DEBUG
      ++pCOM->nModem;
      #endif
      /* save delta bits to simulate real MSR read operation */
      pCOM->nSaveMSR &= ( DELTA_CTS | DELTA_DSR | DELTA_RI | DELTA_CD );
      pCOM->nSaveMSR |= inportb(MSR(pCOM));

      /* Call user event handler -- atomic operation */
      /* Get the new modem status by calling COMGetModemStatus() */
      if (pCOM->EventHandler != NULL)
      {
        disable();
        COMDisableIRQs();
        pCOM->EventHandler(evCOMModem);
        COMEnableIRQs();
      }
      break;
    }

  pCOM->bHandlingIRQ = FALSE;

}
END_OF_FUNCTION(COMHandler);

/* ************************************************************************
   Function: _DetachCOMPort
   Description:
     Detaches a comport from the list of attached com ports.
*/
static void _DetachCOMPort(int nCOM)
{
  struct COMDesc *pCOM;
  struct COMDesc *pPort;
  struct COMDesc *pPrevPort;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bHardwareSet);

  /*
  Find the specific COM port in the COM wrapper chain.
  */
  pPort = pAttachedPorts[pCOM->nIRQ];
  ASSERT(pPort != NULL);
  pPrevPort = NULL;
  while (pPort != &COMs[nCOM])
  {
    ASSERT(pPort != NULL);  /* COM not found in the attachement chain */
    pPrevPort = pPort;
    pPort = pPort->pNextPort;
  }
  /*
  Detach from the COM wrapper chain.
  */
  if (pPrevPort == NULL)  /* First in the chain */
    pAttachedPorts[pCOM->nIRQ] = pPort->pNextPort;
  else
    pPrevPort->pNextPort = pPort->pNextPort;
}

/* ************************************************************************
   Function: AttachCOMPort
   Description:
     Attaches COM port to a specific IRQ.
*/
static int AttachCOMPort(int nCOM)
{
  struct COMDesc *pCOM;
  struct COMDesc *pPort;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bHardwareSet);

  /*
  Attach the com port to the specific COM wrapper chain.
  */
  pPort = pAttachedPorts[pCOM->nIRQ];
  pAttachedPorts[pCOM->nIRQ] = pCOM;
  pCOM->pNextPort = pPort;

  /*
  Install specific COM wrapper if still not installed
  */
  if (pPort == NULL)  /* If this is the first com port attached to the chain */
  {
    if (!InstallIRQ(pCOM->nIRQ, COMWrappers[pCOM->nIRQ]))  /* Install the wrapper */
    {
      /* Failed to install IRQ wrapper */
      _DetachCOMPort(nCOM);
      return (FALSE);
    }
    EnableIRQ(pCOM->nIRQ);
  }
  return (TRUE);
}

/* ************************************************************************
   Function: DetachCOMPort
   Description:
     Detaches a COM port from its IRQ.
*/
static void DetachCOMPort(int nCOM)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bHardwareSet);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */

  _DetachCOMPort(nCOM);

  /*
  Check whether to detach the COM wrapper.
  */
  if (pAttachedPorts[pCOM->nIRQ] == NULL)  /* No more COMs attached */
  {
    DisableIRQ(pCOM->nIRQ);
    UninstallIRQ(pCOM->nIRQ);
  }
}

/* ************************************************************************
   Function: COMSetHardwareParameters
   Description:
     Changes the port hardware settings as IRQ vector and IO address.
     NOTE: All COMs > 4 should be set by this function as there are
     no default values.
*/
void COMSetHardwareParameters(int nCOM, int nIRQ, int nCOMAddress)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(nIRQ < 16 && nIRQ >= 0);

  pCOM->nIRQ = nIRQ;
  pCOM->nCOMAddress = nCOMAddress;
  pCOM->bHardwareSet = TRUE;
}

/* ************************************************************************
   Function: COMGetHardwareParameters
   Description:
     Returns the port hardware settings as IRQ vector and IO address.
*/
void COMGetHardwareParameters(int nCOM, int *pIRQ, int *pCOMAddress)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->nIRQ < 16 && pCOM->nIRQ >= 0);
  ASSERT(pCOM->bHardwareSet);
  ASSERT(pIRQ != NULL);
  ASSERT(pCOMAddress != NULL);

  *pIRQ = pCOM->nIRQ;
  *pCOMAddress = pCOM->nCOMAddress;
}

/* ************************************************************************
   Function: COMDetect
   Description:
     Detects COM port chipset
   Returns:
     1: 8250
     2: 16450 or 8250 with scratch register
     3: 16550
     4: 16550A
     5: No chip
*/
int COMDetect(int nCOM)
{
  struct COMDesc *pCOM;
  int x;
  int olddata;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bHardwareSet);
  ASSERT(!pCOM->bInstalled);  /* Should not be called on an open port */

  /*
  Check if a UART is present anyway
  */
  olddata = inportb(MCR(pCOM));
  outportb(MCR(pCOM), 0x10);  /* Loopback and RTS&DTR down */
  if ((inportb(MSR(pCOM)) & 0x30) != 0)  /* Inspect RTS and DTR to be 0 */
    return (NOCHIP);  /* No chip at this port address */

  outportb(MCR(pCOM), 0x1f);  /* Loopback and RTS&DTR up */
  if ((inportb(MSR(pCOM)) & 0x30) != 0x30)  /* Inspect RTS and DTR to be up */
    return (NOCHIP);

  outportb(MCR(pCOM), olddata);  /* Restore original value */

  /*
  Look for the scratch register
  */
  olddata = inportb(SCR(pCOM));
  outportb(SCR(pCOM), 0x55);
  if (inportb(SCR(pCOM)) != 0x55)
    return (CHIP8250);

  outportb(SCR(pCOM), 0xaa);
  if (inportb(SCR(pCOM)) != 0xaa)
    return (CHIP8250);

  outportb(SCR(pCOM), olddata);  /* Restore original value */

  /*
  Check if there's a FIFO
  */
  outportb(FCR(pCOM), 1);
  x = inportb(IIR(pCOM));
  outportb(FCR(pCOM), 0x0);
  if ((x & 0x80) == 0)
    return (CHIP16450);
  if ((x & 0x40) == 0)
    return (CHIP16550);
  return (CHIP16550A);
}

/* ************************************************************************
   Function: COMGetChipset
   Description:
     Returns the COM port chip set as detected after COMPortOpen.
*/
int COMGetChipset(int nCOM)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */

  return (pCOM->nChipset);
}

/* ************************************************************************
   Function: COMIsFIFOAvailable
   Description:
     Detects whether support for hardware tx/rx FIFOs is available.
*/
int COMIsFIFOAvailable(int nCOM)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bHardwareSet);
  ASSERT(pCOM->nChipset > 0);  /* Should be initialized by COMDetect() */

  if (pCOM->nChipset == 4)
    return (TRUE);
  return (FALSE);
}

/* ************************************************************************
   Function: EnableHardwareFIFO
   Description:
     Enables the hardware FIFO if present.
*/
void COMEnableHardwareFIFO(int nCOM)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bHardwareSet);

  if (pCOM->nParity == '9')
    return;

  if (pCOM->bFIFOAvailable)
  {
    switch (pCOM->nRXThreshold)
    {
      case 0:  /* Use default */
      case 8:
      outportb(FCR(pCOM), 0x87);  /* Clear rx/tx FIFOs, set rx FIFO to 8bytes */
      break;
      case 1:
      outportb(FCR(pCOM), 0x07);  /* Clear rx/tx FIFOs, set rx FIFO to 1 byte */
      break;
      case 4:
      outportb(FCR(pCOM), 0x47);  /* Clear rx/tx FIFOs, set rx FIFO to 4 bytes */
      break;
      case 14:
      outportb(FCR(pCOM), 0xc7);  /* Clear rx/tx FIFOs, set rx FIFO to 14 bytes */
      break;
      default:
      ASSERT(0);  /* Invalid RX Threshold value */
    }
    pCOM->bFIFOEnabled = TRUE;
  }
}

/* ************************************************************************
   Function: DisableHardwareFIFO
   Description:
     Disables the hardware FIFO.
*/
void COMDisableHardwareFIFO(int nCOM)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bHardwareSet);

  if (pCOM->bFIFOAvailable)
  {
    outportb(FCR(pCOM), 0);
    pCOM->bFIFOEnabled = FALSE;
  }
}

/* ************************************************************************
   Function: COMIsFIFOEnabled
   Description:
     Returns 0 if the hardware fifos for the specific COM port are disabled.
*/
int COMIsFIFOEnabled(int nCOM)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bHardwareSet);

  return (pCOM->bFIFOEnabled);
}

/* ************************************************************************
   Function: COMSetRXThreshold
   Description:
     Sets threshold to be set as RX hardware fifo threshold.
     Valid values are 1, 4, 8 and 14 to correspond to
     hardware trigger levels.
*/
void COMSetRXThreshold(int nCOM, int nRXThreshold)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(!pCOM->bInstalled);  /* Can be called only prior COMPortOpen() */
  ASSERT(nRXThreshold == 1 || nRXThreshold == 4 ||
    nRXThreshold == 8 || nRXThreshold == 14);

  pCOM->nRXThreshold = nRXThreshold;
}

/* ************************************************************************
   Function: COMSetTXThreshold
   Description:
*/
void COMSetTXThreshold(int nCOM, int nTXThreshold)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(!pCOM->bInstalled);  /* Can be called only prior COMPortOpen() */
  ASSERT(nTXThreshold >= 1 && nTXThreshold <= 16);

  pCOM->nTXThreshold = nTXThreshold;
}

/* ************************************************************************
   Function: COMGetRXThreshold
   Description:
*/
int COMGetRXThreshold(int nCOM)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(nCOM >= 0 && nCOM < COMMAX);

  if (pCOM->nRXThreshold == 0)
    return (DEFAULT_RX_THRESHOLD);
  return (pCOM->nRXThreshold);
}

/* ************************************************************************
   Function: COMGetTXThreshold
   Description:
*/
int COMGetTXThreshold(int nCOM)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(nCOM >= 0 && nCOM < COMMAX);

  if (pCOM->nTXThreshold == 0)
    return (DEFAULT_RX_THRESHOLD);
  return (pCOM->nTXThreshold);
}

/* ************************************************************************
   Function: COMPortOpen
   Description:
     Opens a specific COM port for operation. The initial operating
     conditions are determined by the arguments.
*/
int COMPortOpen(int nCOM, long nBauds, int nWordLen, int nParity,
  int nStopBits, int nFlowControl, void (*EventHandler)(int nEvent))
{
  struct COMDesc *pCOM;
  unsigned char x;
  int nRXQueueSize;
  int nTXQueueSize;
  char *pRXQueue;
  char *pTXQueue;
  char *pRXStatQueue;
  char *pTXStatQueue;

  pCOM = &COMs[nCOM];

  if (!bCOMInit)
    if (!COMInit())
      return (COMERR_GENERAL);

  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bHardwareSet);
  ASSERT(!pCOM->bInstalled);  /* Should be opened only once */

  if ((pCOM->nChipset = COMDetect(nCOM)) == NOCHIP)
    return (COMERR_NOCHIP);

  pCOM->bFIFOAvailable = COMIsFIFOAvailable(nCOM);
  if (nParity == '9')
    COMDisableHardwareFIFO(nCOM);
  else
    COMEnableHardwareFIFO(nCOM);

  /*
  Clear all present events that cause IRQ pending.
  */
  do
  {
    inportb(RBR(pCOM));
    inportb(LSR(pCOM));
    inportb(MSR(pCOM));
    x = inportb(IIR(pCOM));
  }
  while ((x & 0x1) == 0);  /* Until IRQ pending */

  /*
  Set the operating parameters -- bauds, parity, etc.
  */
  COMSetTransmitParameters(nCOM, nBauds, nWordLen, nParity, nStopBits);
  COMSetFlowControl(nCOM, nFlowControl);
  COMSetFlowChars(nCOM, 0x11, 0x13);
  COMSetEventHandler(nCOM, EventHandler);

  /*
  Init receive and transmit queues
  */
  nRXQueueSize = DEFAULT_QUEUE_SIZE;
  nTXQueueSize = DEFAULT_QUEUE_SIZE;
  if (pCOM->nRXQueueSize != 0)
    nRXQueueSize = pCOM->nRXQueueSize;
  if (pCOM->nTXQueueSize != 0)
    nTXQueueSize = pCOM->nTXQueueSize;
  pRXQueue = NULL;
  pTXQueue = NULL;
  pRXStatQueue = NULL;
  pTXStatQueue = NULL;
  pRXQueue = (char*)malloc(nRXQueueSize + 1);  /* + 1: As Head never should tread on Tail */
  pTXQueue = (char*)malloc(nTXQueueSize + 1);
  pRXStatQueue = (char*)malloc(nRXQueueSize * sizeof(struct COMStat) + 1);
  pTXStatQueue = (char*)malloc(nTXQueueSize * sizeof(struct COMStat) + 1);
  if (pRXQueue == NULL || pTXQueue == NULL || pRXStatQueue == NULL || pTXStatQueue == NULL)
  {
free_queues:
    if (pRXQueue != NULL)
      free(pRXQueue);
    if (pTXQueue != NULL)
      free(pTXQueue);
    if (pRXStatQueue != NULL)
      free(pRXStatQueue);
    if (pTXStatQueue != NULL)
      free(pTXStatQueue);
    return (COMERR_NOMEMORY);
  }
  if (LockData(pRXQueue, nRXQueueSize) == -1)
    goto free_queues;
  if (LockData(pTXQueue, nTXQueueSize) == -1)
  {
    UnlockData(pRXQueue, nRXQueueSize);
    goto free_queues;
  }
  if (LockData(pRXStatQueue, nRXQueueSize * sizeof(struct COMStat)) == -1)
  {
    UnlockData(pRXQueue, nRXQueueSize);
    UnlockData(pTXQueue, nTXQueueSize);
    goto free_queues;
  }
  if (LockData(pTXStatQueue, nTXQueueSize * sizeof(struct COMStat)) == -1)
  {
    UnlockData(pRXQueue, nRXQueueSize);
    UnlockData(pTXQueue, nTXQueueSize);
    UnlockData(pRXStatQueue, nRXQueueSize * sizeof(struct COMStat));
    goto free_queues;
  }
  /* Attach the allocated buffers to the queues */
  QUEUEInit(&pCOM->RXQueue, pRXQueue, nRXQueueSize + 1);
  QUEUEInit(&pCOM->TXQueue, pTXQueue, nTXQueueSize + 1);
  QUEUEInit(&pCOM->RXStatQueue, pRXStatQueue, nRXQueueSize * sizeof(struct COMStat) + 1);
  QUEUEInit(&pCOM->TXStatQueue, pTXStatQueue, nTXQueueSize * sizeof(struct COMStat) + 1);

  /*
  Disable the interrupts.
  Install the IRQ handler.
  Set OUT2 bit.
  Enable all IRQs in the Interrupt Enable Register (IER).
  Enable the IRQ in the 8259 Interrupt controller.
  */
  outportb(IER(pCOM), 0);  /* Disable all interrupts (if before were enabled) */
  disable();

  AttachCOMPort(nCOM);

  outportb(MCR(pCOM), inportb(MCR(pCOM)) | 0x8);  /* OUT2 bit */
  outportb(IER(pCOM), 0x0d);  /* Enable all interrupts but tx */
  pCOM->nSaveIER = 0x0d;
  pCOM->bHandlingIRQ = FALSE;  /* Altered in COMHandler */
  enable();

  pCOM->bInstalled = TRUE;
  return (0);
}

/* ************************************************************************
   Function: COMSetTransmitParameters
   Description:
     Sets the transmition parameters for a specific COM port.
*/
void COMSetTransmitParameters(int nCOM, long nBauds, int nWordLen,
  int nParity, int nStopBits)
{
  struct COMDesc *pCOM;
  long nDivisor;
  int nDivLow;
  int nDivHi;
  int nParityIndex;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(nWordLen >= 5 && nWordLen <= 8);
  ASSERT(nStopBits == 1 || nStopBits == 2);

  pCOM->nBauds = nBauds;
  pCOM->nParity = nParity;
  pCOM->nWordLen = nWordLen;
  pCOM->nStopBits = nStopBits;

  nParityIndex = 0;  /* No parity */
  switch (nParity)
  {
    case 'N':
      break;
    case 'O':
      nParityIndex = 1;  /* Odd parity */
      break;
    case 'E':
      nParityIndex = 3;  /* Even parity */
      break;
    case 'M':
      nParityIndex = 5;  /* Mark bit */
      break;
    case 'S':
      nParityIndex = 7;  /* Space bit */
      break;
    case '9':
      nParityIndex = 7;  /* Emulate 9th bit NOT READY */
      break;
    default:
      ASSERT(0);  /* Invalid parity specification */
  }

  nDivisor = 115200L / nBauds;
  nDivLow = (int)(nDivisor & 0xff);
  nDivHi = (int)((nDivisor >> 8) * 0xff);

  outportb(LCR(pCOM), 0x80); /* DLAB = 1 to access baud rate regs */
  outportb(DLL(pCOM), nDivLow);
  outportb(DLM(pCOM), nDivHi);

  /* Set parity flag, stop bits and data size and DLAB = 0 */
  pCOM->nSaveLCR = (nWordLen - 5) | (nParityIndex << 3) | ((nStopBits - 1) << 2);
  outportb(LCR(pCOM), pCOM->nSaveLCR);
}

/* ************************************************************************
   Function: COMGetTransmitParameters
   Description:
     Returns the transmition parameters set by COMSetTransmitParamaters()
     or COMPortOpen().
*/
void COMGetTransmitParameters(int nCOM, long *pBauds, int *pWordLen,
  int *pParity, int *pStopBits)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */
  ASSERT(pBauds != NULL);
  ASSERT(pWordLen != NULL);
  ASSERT(pParity != NULL);
  ASSERT(pStopBits != NULL);

  *pBauds = pCOM->nBauds;
  *pParity = pCOM->nParity;
  *pWordLen = pCOM->nWordLen;
  *pStopBits = pCOM->nStopBits;
}

/* ************************************************************************
   Function: COMSetFlowControl
   Description:
     NOT READY
*/
void COMSetFlowControl(int nCOM, int nFlowControl)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);

  pCOM->nFlowControl = nFlowControl;
}

/* ************************************************************************
   Function: COMGetFlowControl
   Description:
     NOT READY
*/
int COMGetFlowControl(int nCOM)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);

  return (pCOM->nFlowControl);
}

/* ************************************************************************
   Function: COMSetFlowChars
   Description:
     NOT READY
*/
void COMSetFlowChars(int nCOM, int iXONchar, int iXOFFchar)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);

  if( iXONchar >= 0 )
     pCOM->cXon = (char)iXONchar;
  if( iXOFFchar >= 0 )
     pCOM->cXoff = (char)iXOFFchar;
}

/* ************************************************************************
   Function: COMSetEventHandler
   Description:
     Attaches an user event handler to be called on every COM port event
     that occures.
*/
void COMSetEventHandler(int nCOM, void (*EventHandler)(int nEvent))
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);

  pCOM->EventHandler = EventHandler;
}
END_OF_FUNCTION(COMSetEventHandler);

/* ************************************************************************
   Function: COMGetEventHandler
   Description:
     Returns the address of user handler attached to be invoked on COM
     events. Returns NULL if no user handler attahed.
*/
void COMGetEventHandler(int nCOM, void (**EventHandler)(int nEvent))
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */
  ASSERT(EventHandler != NULL);

  *EventHandler = pCOM->EventHandler;
}
END_OF_FUNCTION(COMGetEventHandler);

/* ************************************************************************
   Function: COMSetTXQueueSize
   Description:
     Sets new value for the tx fifo size.
*/
void COMSetTXQueueSize(int nCOM, int nTXQueueSize)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(!pCOM->bInstalled);  /* Can be called only prior COMPortOpen() */
  ASSERT(nTXQueueSize > 0);

  pCOM->nTXQueueSize = nTXQueueSize;
}

/* ************************************************************************
   Function: COMGetTXQueueSize
   Description:
     Returns the current size of the tx fifo.
*/
int COMGetTXQueueSize(int nCOM)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(nCOM >= 0 && nCOM < COMMAX);

  if (pCOM->nTXQueueSize == 0)
    return (DEFAULT_QUEUE_SIZE);
  return (pCOM->nTXQueueSize);
}
END_OF_FUNCTION(COMGetTXQueueSize);

/* ************************************************************************
   Function: COMSetRXQueueSize
   Description:
     Sets new value for the rx fifo size.
*/
void COMSetRXQueueSize(int nCOM, int nRXQueueSize)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(!pCOM->bInstalled);  /* Can be called only prior COMPortOpen() */
  ASSERT(nRXQueueSize > 0);

  pCOM->nRXQueueSize = nRXQueueSize;
}

/* ************************************************************************
   Function: COMGetRXQueueSize
   Description:
     Returns the current size of the rx fifo.
*/
int COMGetRXQueueSize(int nCOM)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(nCOM >= 0 && nCOM < COMMAX);

  if (pCOM->nRXQueueSize == 0)
    return (DEFAULT_QUEUE_SIZE);
  return (pCOM->nRXQueueSize);
}
END_OF_FUNCTION(COMGetRXQueueSize);

/* ************************************************************************
   Function: COMPortClose
   Description:
     Terminates all operations for a specific COM.
*/
void COMPortClose(int nCOM)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */

  /*
  Mask all the interrupts in the COM port
  */
  outportb(MCR(pCOM), inportb(MCR(pCOM)) & ~0x8);  /* OUT2 bit to 0 */
  outportb(IER(pCOM), 0);
  DisableIRQ(pCOM->nIRQ);
  disable();

  /*
  Dispose the rx and tx queues.
  */
  UnlockData(pCOM->RXQueue.pBuf, pCOM->RXQueue.nBufSize);
  UnlockData(pCOM->TXQueue.pBuf, pCOM->TXQueue.nBufSize);
  UnlockData(pCOM->RXStatQueue.pBuf, pCOM->RXStatQueue.nBufSize);
  UnlockData(pCOM->TXStatQueue.pBuf, pCOM->TXStatQueue.nBufSize);
  free(QUEUEDetachBuf(&pCOM->RXQueue));
  free(QUEUEDetachBuf(&pCOM->TXQueue));
  free(QUEUEDetachBuf(&pCOM->RXStatQueue));
  free(QUEUEDetachBuf(&pCOM->TXStatQueue));

  /*
  Detach from the COM wrapper.
  */
  DetachCOMPort(nCOM);
  enable();

  COMDisableHardwareFIFO(nCOM);  /* Some programs expect disabled FIFOs */

  pCOM->bInstalled = FALSE;
}

/* ************************************************************************
   Function: COMPortCloseAll
   Description:
     Closes all the opened COM ports.
*/
void COMPortCloseAll(void)
{
  int i;

  for (i = 0; i < COMMAX; ++i)
    if (COMs[i].bInstalled)
      COMPortClose(i);
}

/* ************************************************************************
   Function: COMActivateTransmition
   Description:
     This function is to be invoked after something is put in the
     tx fifo queue and is necessary to start sending the data.
*/
static void COMActivateTransmition(int nCOM)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */

  /*
  Check if transmittion is activated.
  */
  if ((pCOM->nSaveIER & 0x2) != 0)
    return;  /* Transmittion is already running */

  /*
  Activate transmition:
  Set IER to enable tx interrupts and as the last char
  of the last tx is already sent, by enabling
  IER an IRQ will be generated and tx will be initiated.
  The tx handler will start to extract bytes from the tx fifo queue.
  */
  pCOM->nSaveIER |= 0x2;
  outportb(IER(pCOM), pCOM->nSaveIER);
}
END_OF_FUNCTION(COMActivateTransmition);

/* ************************************************************************
   Function: COMWriteChar
   Description:
     Writes a character to the tx fifo buffer and initiates
     transmition if still not running.
*/
int COMWriteChar(int nCOM, char c, const struct COMStat *pStat)
{
  struct COMDesc *pCOM;
  int nTXFree;
  #ifdef _DEBUG
  int nTXStatFree;
  #endif

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */
  #ifdef _DEBUG
  if (pCOM->nParity == '9')
    ASSERT(pStat != NULL);  /* It is mandatory to supply 9th bit values */
  #endif

  ENTER_QUEUE_CRITICAL_SECTION(pCOM);
  nTXFree = QUEUECalcFree(&pCOM->TXQueue);
  #ifdef _DEBUG
  nTXStatFree = QUEUECalcFree(&pCOM->TXStatQueue);
  #endif
  LEAVE_QUEUE_CRITICAL_SECTION(pCOM);

  ASSERT((nTXStatFree % sizeof(struct COMStat)) == 0);

  if (nTXFree == 0)  /* No room for one more character */
    return (COMERR_TXOVERFLOW);

  ENTER_QUEUE_CRITICAL_SECTION(pCOM);
  QUEUEPut(&pCOM->TXQueue, &c, 1);
  if (pStat != NULL)
    QUEUEPut(&pCOM->TXStatQueue, (char *)pStat, sizeof(struct COMStat));
  else
    QUEUEAdvance(&pCOM->TXStatQueue, &pCOM->TXStatQueue.nTail, sizeof(struct COMStat));
  LEAVE_QUEUE_CRITICAL_SECTION(pCOM);

  COMActivateTransmition(nCOM);
  return (0);
}
END_OF_FUNCTION(COMWriteChar);

#ifndef DISABLE_TIMING
/* ************************************************************************
   Function: COMWriteCharTimed
   Description:
     Writes one characters to the tx buffer, waits for the character
     to be sent, checks timeout.
     nTimeOut = -1 - wait indefinitely
*/
int COMWriteCharTimed(int nCOM, char c, const struct COMStat *pStat, int nTimeOut)
{
#ifdef _DEBUG
  struct COMDesc *pCOM;
#endif
  unsigned long int nTime;

#ifdef _DEBUG
  pCOM = &COMs[nCOM];
#endif

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */
  ASSERT(nTimeOut != 0);

  nTime = nTimerValue;  /* Get current timer value */
  if (nTimeOut != -1)
    nTimeOut /= 50;  /* According to the current timer precision */

  /* Wait to put the char */
  while (COMWriteChar(nCOM, c, pStat) == COMERR_TXOVERFLOW)
  {
    if (nTimeOut != -1)
      if (nTimerValue - nTime > (unsigned long int)nTimeOut)
        return (COM_TIMEOUT);
  }

  /* Wait for the character to be sent */
  while (!COMIsTXBufferSent(nCOM))
  {
    if (nTimeOut != -1)
      if (nTimerValue - nTime > (unsigned long int)nTimeOut)
        return (COM_TIMEOUT);
  }

  return (0);
}
#endif /* ifndef DISABLE_TIMING */

/* ************************************************************************
   Function: COMWriteBuffer
   Description:
     Writes a block of data to the port.
     This routine will only write the number of characters that will fit
     in the output buffer. It may return before the requested number has
     been sent if the buffer fills up with COMERR_TXOVERFLOW. *nCount will
     contain the actual number of characters sent from the buffer.
*/
int COMWriteBuffer(int nCOM, const char *pBuf, const struct COMStat *pStatBuf,
  int nSize, int *nCount)
{
  struct COMDesc *pCOM;
  int nTXFree;
  int nStoreCount;
  #ifdef _DEBUG
  int nTXStatFree;
  #endif

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */
  ASSERT(pBuf != NULL);
  ASSERT(nSize > 0);

  ENTER_QUEUE_CRITICAL_SECTION(pCOM);
  nTXFree = QUEUECalcFree(&pCOM->TXQueue);
  #ifdef _DEBUG
  nTXStatFree = QUEUECalcFree(&pCOM->TXStatQueue);
  #endif
  LEAVE_QUEUE_CRITICAL_SECTION(pCOM);

  ASSERT((nTXStatFree % sizeof(struct COMStat)) == 0);

  nStoreCount = nSize;
  if (nStoreCount > nTXFree)
    nStoreCount = nTXFree;

  if (nStoreCount > 0)
  {
    ENTER_QUEUE_CRITICAL_SECTION(pCOM);
    QUEUEPut(&pCOM->TXQueue, pBuf, nStoreCount);
    if (pStatBuf != NULL)
      QUEUEPut(&pCOM->TXStatQueue, (char *)pStatBuf, nStoreCount * sizeof(struct COMStat));
    else
      QUEUEAdvance(&pCOM->TXStatQueue, &pCOM->TXStatQueue.nTail, nStoreCount * sizeof(struct COMStat));
    LEAVE_QUEUE_CRITICAL_SECTION(pCOM);
    COMActivateTransmition(nCOM);
  }

  if (nCount != NULL)
    *nCount = nStoreCount;
  if (nStoreCount != nSize)
    return (COMERR_TXOVERFLOW);
  return (0);
}
END_OF_FUNCTION(COMWriteBuffer);

#ifndef DISABLE_TIMING
/* ************************************************************************
   Function: COMWriteBufferTimed
   Description:
     Writes block of characters to the tx buffer, waits to be sent,
     checks timeout.
     nTimeOut = -1 - wait indefinitely
     If time out expires *nCount will contain the number of the
     characters sent or stored in the tx buffer.
*/
int COMWriteBufferTimed(int nCOM, const char *pBuf, const struct COMStat *pStatBuf,
  int nSize, int *nCount, int nTimeOut)
{
#ifdef _DEBUG
  struct COMDesc *pCOM;
#endif
  unsigned long int nTime;
  int nBlockCount;

#ifdef _DEBUG
  pCOM = &COMs[nCOM];
#endif

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */
  ASSERT(pBuf != NULL);
  ASSERT(nTimeOut != 0);
  ASSERT(nSize > 0);

  nTime = nTimerValue;  /* Get current timer value */
  if (nTimeOut != -1)
    nTimeOut /= 50;  /* According to the current timer precision */

  /* Wait to put whole block */
  while (nSize > 0)
  {
    if (COMWriteBuffer(nCOM, pBuf, pStatBuf, nSize, &nBlockCount) == COMERR_TXOVERFLOW)
    {
      /* Update nSize, pBuf and pStatBuf by the portion that
      has been sent */
      nSize -= nBlockCount;
      pBuf += nBlockCount;
      if (pStatBuf != NULL)
      pStatBuf += nBlockCount;
    }
    else
    {
      /* All the buffer is sent */
      nSize = 0;
      break;
    }

    if (nTimeOut != -1)
      if (nTimerValue - nTime > (unsigned long int)nTimeOut)
      {
        *nCount = nSize;
        return (COM_TIMEOUT);
      }
  }

  /* Wait for the character block to be sent */
  while (!COMIsTXBufferSent(nCOM))
  {
    if (nTimeOut != -1)
      if (nTimerValue - nTime > (unsigned long int)nTimeOut)
      {
        *nCount = nSize;
        return (COM_TIMEOUT);
      }
  }

  return (0);
}
#endif  /* ifndef DISABLE_TIMING */

/* ************************************************************************
   Function: COMClearTXBuffer
   Description:
     Clear all the characters currently in the tx buffer.
*/
void COMClearTXBuffer(int nCOM)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */

  ENTER_QUEUE_CRITICAL_SECTION(pCOM);
  pCOM->TXQueue.nHead = pCOM->TXQueue.nTail = 0;
  pCOM->TXStatQueue.nHead = pCOM->TXStatQueue.nTail = 0;
  LEAVE_QUEUE_CRITICAL_SECTION(pCOM);
}
END_OF_FUNCTION(COMClearTXBuffer);

/* ************************************************************************
   Function: COMTXBufferFree
   Description:
     Returns an integer indicating how much space is availabe in
     the transmit queue.
*/
int COMTXBufferFree(int nCOM)
{
  struct COMDesc *pCOM;
  int nFree;
  #ifdef _DEBUG
  int nStatFree;
  #endif

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */

  ENTER_QUEUE_CRITICAL_SECTION(pCOM);
  nFree = QUEUECalcFree(&pCOM->TXQueue);
  #ifdef _DEBUG
  nStatFree = QUEUECalcFree(&pCOM->TXStatQueue);
  #endif
  LEAVE_QUEUE_CRITICAL_SECTION(pCOM);

  ASSERT((nStatFree % sizeof(struct COMStat)) == 0);
  ASSERT(nFree == nStatFree / (int)sizeof(struct COMStat));

  return (nFree);
}
END_OF_FUNCTION(COMTXBufferFree);

/* ************************************************************************
   Function: COMTXBufferUsed
   Description:
     Returns an integer telling the number of characters currently in
     the transmit queue.
*/
int COMTXBufferUsed(int nCOM)
{
  struct COMDesc *pCOM;
  int nUsed;
  #ifdef _DEBUG
  int nStatUsed;
  #endif

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */

  ENTER_QUEUE_CRITICAL_SECTION(pCOM);
  nUsed = QUEUECalcOccupied(&pCOM->TXQueue);
  #ifdef _DEBUG
  nStatUsed = QUEUECalcOccupied(&pCOM->TXStatQueue);
  #endif
  LEAVE_QUEUE_CRITICAL_SECTION(pCOM);

  ASSERT((nStatUsed % sizeof(struct COMStat)) == 0);
  ASSERT(nUsed == nStatUsed / (int)sizeof(struct COMStat));

  return (nUsed);
}
END_OF_FUNCTION(COMTXBufferUsed);

/* ************************************************************************
   Function: COMIsTXBufferSent
   Description:
     Checks whether last transmition completed.
     This function doesn't check whether the TX buffer is empty, which
     in case of hardware TX FIFO buffers is very unreliable indication,
     but instead checks whether the COM port transmittion is still
     running or not.
*/
int COMIsTXBufferSent(int nCOM)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */

  /*
  Check if transmittion is activated.
  */
  if ((pCOM->nSaveIER & 0x2) != 0)
    return (FALSE);  /* Transmittion is still running */

  return (TRUE);  /* Last transmittion has completed */
}
END_OF_FUNCTION(COMIsTXBufferSent);

/* ************************************************************************
   Function: COMReadChar
   Description:
     Reads a character from the COM port receive buffer.
*/
int COMReadChar(int nCOM, char *pChar, struct COMStat *pStat)
{
  struct COMDesc *pCOM;
  int nOccupied;
  int nCOMError;
  #ifdef _DEBUG
  int nStatOccupied;
  #endif

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */
  ASSERT(pChar != NULL);

  nCOMError = pCOM->nCOMError;
  if (nCOMError)
  {
    pCOM->nCOMError = 0;  /* Clear the error */
    return (nCOMError);
  }

  ENTER_QUEUE_CRITICAL_SECTION(pCOM);
  nOccupied = QUEUECalcOccupied(&pCOM->RXQueue);
  #ifdef _DEBUG
  nStatOccupied = QUEUECalcOccupied(&pCOM->RXStatQueue);
  #endif
  LEAVE_QUEUE_CRITICAL_SECTION(pCOM);

  ASSERT((nStatOccupied % sizeof(struct COMStat)) == 0);
  ASSERT(nOccupied == nStatOccupied / (int)sizeof(struct COMStat));

  if (nOccupied > 0)
  {
    ENTER_QUEUE_CRITICAL_SECTION(pCOM);
    QUEUEGet(&pCOM->RXQueue, pChar, 1);
    if (pStat != NULL)
      QUEUEGet(&pCOM->RXStatQueue, (char *)pStat, sizeof(struct COMStat));
    else
      QUEUEAdvance(&pCOM->RXStatQueue, &pCOM->RXStatQueue.nHead, sizeof(struct COMStat));
    LEAVE_QUEUE_CRITICAL_SECTION(pCOM);
  }
  else
    return (COM_BUFEMPTY);

  return (0);  /* No error and a character has been extracted */
}
END_OF_FUNCTION(COMReadChar);

#ifndef DISABLE_TIMING
/* ************************************************************************
   Function: COMReadCharTimed
   Description:
     Reads a character from the COM port receive buffer. If no characters
     available will wait up to nTimeOut miliseconds to retrieve one.
     nTimeOut = -1 - wait indefinitely
*/
int COMReadCharTimed(int nCOM, char *pChar, struct COMStat *pStat, int nTimeOut)
{
  struct COMDesc *pCOM;
  int nOccupied;
  int nCOMError;
  int bWaitChar;
  unsigned long int nTime;
  #ifdef _DEBUG
  int nStatOccupied;
  #endif

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */
  ASSERT(pChar != NULL);
  ASSERT(nTimeOut != 0);

  nCOMError = pCOM->nCOMError;
  if (nCOMError)
  {
    pCOM->nCOMError = 0;  /* Clear the error */
    return (nCOMError);
  }

  nTime = nTimerValue;  /* Get current timer value */
  if (nTimeOut != -1)
    nTimeOut /= 50;  /* According to the current timer precision */

  bWaitChar = TRUE;
  while (bWaitChar)
  {
    ENTER_QUEUE_CRITICAL_SECTION(pCOM);
    nOccupied = QUEUECalcOccupied(&pCOM->RXQueue);
    #ifdef _DEBUG
    nStatOccupied = QUEUECalcOccupied(&pCOM->RXStatQueue);
    #endif
    LEAVE_QUEUE_CRITICAL_SECTION(pCOM);

    ASSERT((nStatOccupied % sizeof(struct COMStat)) == 0);
    ASSERT(nOccupied == nStatOccupied / sizeof(struct COMStat));

    if (nTimeOut != -1)
      if (nTimerValue - nTime > (unsigned long int)nTimeOut)
        return (COM_BUFEMPTY);

    if (nOccupied > 0)
    {
      ENTER_QUEUE_CRITICAL_SECTION(pCOM);
      QUEUEGet(&pCOM->RXQueue, pChar, 1);
      if (pStat != NULL)
        QUEUEGet(&pCOM->RXStatQueue, (char *)pStat, sizeof(struct COMStat));
      else
        QUEUEAdvance(&pCOM->RXStatQueue, &pCOM->RXStatQueue.nHead, sizeof(struct COMStat));
      LEAVE_QUEUE_CRITICAL_SECTION(pCOM);
      bWaitChar = FALSE;
    }
  }

  return (0);  /* No error and a character has been extracted */
}
#endif  /* ifndef DISABLE_TIMING */

/* ************************************************************************
   Function: COMReadBuffer
   Description:
     Reads a maximum of nCount characters from COMs rx queue
     to a specific buffer.
     In the rx buffer should have at least nCount characters available
     upon calling this function.
*/
int COMReadBuffer(int nCOM, char *pBuf, struct COMStat *pStatBuf, int nCount)
{
  struct COMDesc *pCOM;
  int nOccupied;
  int nCOMError;
  #ifdef _DEBUG
  int nStatOccupied;
  #endif

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */
  ASSERT(pBuf != NULL);
  ASSERT(nCount > 0);

  nCOMError = pCOM->nCOMError;
  if (nCOMError)
  {
    pCOM->nCOMError = 0;  /* Clear the error */
    return (nCOMError);
  }

  ENTER_QUEUE_CRITICAL_SECTION(pCOM);
  nOccupied = QUEUECalcOccupied(&pCOM->RXQueue);
  #ifdef _DEBUG
  nStatOccupied = QUEUECalcOccupied(&pCOM->RXStatQueue);
  #endif
  LEAVE_QUEUE_CRITICAL_SECTION(pCOM);

  ASSERT((nStatOccupied % sizeof(struct COMStat)) == 0);
  ASSERT(nOccupied == nStatOccupied / (int)sizeof(struct COMStat));

  if (nOccupied < nCount)
    return (COM_BUFEMPTY);

  ENTER_QUEUE_CRITICAL_SECTION(pCOM);
  QUEUEGet(&pCOM->RXQueue, pBuf, nCount);
  if (pStatBuf != NULL)
    QUEUEGet(&pCOM->RXStatQueue, (char *)pStatBuf, nCount * sizeof(struct COMStat));
  else
    QUEUEAdvance(&pCOM->RXStatQueue, &pCOM->RXStatQueue.nHead, nCount * sizeof(struct COMStat));
  LEAVE_QUEUE_CRITICAL_SECTION(pCOM);

  return (0);
}
END_OF_FUNCTION(COMReadBuffer);

#ifndef DISABLE_TIMING
/* ************************************************************************
   Function: COMReadBufferTimed
   Description:
     Reads a maximum of nCount characters from COMs rx queue
     to a specific buffer. Will wait up to nTimeOut milliseconds
     to retreive the requested number of characters.
     *nActual will contain the number of characters retreived in pBuf.
     if nActual is NULL no such information will be exported.
     nTimeOut = -1 - wait indefinitely
*/
int COMReadBufferTimed(int nCOM, char *pBuf, struct COMStat *pStatBuf, int nCount,
  int *nActual, int nTimeOut)
{
  struct COMDesc *pCOM;
  int nOccupied;
  int nCOMError;
  int bWaitChar;
  int nCount2;
  unsigned long int nTime;
  #ifdef _DEBUG
  int nStatOccupied;
  #endif

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */
  ASSERT(pBuf != NULL);
  ASSERT(nTimeOut != 0);
  ASSERT(nCount > 0);

  nCount2 = nCount;  /* Preserve requested */

  nCOMError = pCOM->nCOMError;
  if (nCOMError)
  {
    pCOM->nCOMError = 0;  /* Clear the error */
    return (nCOMError);
  }

  nTime = nTimerValue;  /* Get current timer value */
  if (nTimeOut)
    nTimeOut /= 50;  /* According to the current timer precision */

  bWaitChar = TRUE;
  while (bWaitChar)
  {
    ENTER_QUEUE_CRITICAL_SECTION(pCOM);
    nOccupied = QUEUECalcOccupied(&pCOM->RXQueue);
    #ifdef _DEBUG
    nStatOccupied = QUEUECalcOccupied(&pCOM->RXStatQueue);
    #endif
    LEAVE_QUEUE_CRITICAL_SECTION(pCOM);

    ASSERT((nStatOccupied % sizeof(struct COMStat)) == 0);
    ASSERT(nOccupied == nStatOccupied / sizeof(struct COMStat));

    if (nTimeOut != -1)
      if (nTimerValue - nTime > (unsigned long int)nTimeOut)
      {
        if (nActual != NULL)
          *nActual = nCount2 - nCount;  /* Requested - what remains */
        return (COM_TIMEOUT);
      }

    if (nOccupied > 0)
    {
      if (nOccupied > nCount)
        nOccupied = nCount;  /* Don't exceed pBuf size */
      ENTER_QUEUE_CRITICAL_SECTION(pCOM);
      QUEUEGet(&pCOM->RXQueue, pBuf, nOccupied);
      if (pStatBuf != NULL)
        QUEUEGet(&pCOM->RXStatQueue, (char *)pStatBuf, nOccupied * sizeof(struct COMStat));
      else
        QUEUEAdvance(&pCOM->RXStatQueue, &pCOM->RXStatQueue.nHead, nOccupied * sizeof(struct COMStat));
      LEAVE_QUEUE_CRITICAL_SECTION(pCOM);
      nCount -= nOccupied;  /* nCount shows how much remains to be retrieved */
      pBuf += nOccupied;
      if (pStatBuf != NULL)
        pStatBuf += nOccupied;
      if (nCount == 0)
        bWaitChar = FALSE;
    }
  }

  return (0);
}
#endif

/* ************************************************************************
   Function: COMPeekChar
   Description:
     Reads the next character availabe in the receive buffer. The
     character is not extracted and remains in the buffer. You can
     only peek one-deep into the buffer.
*/
int COMPeekChar(int nCOM, char *pChar, struct COMStat *pStat)
{
  struct COMDesc *pCOM;
  int nOccupied;
  int nCOMError;
  int nHead;
  #ifdef _DEBUG
  int nStatOccupied;
  #endif

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */
  ASSERT(pChar != NULL);

  nCOMError = pCOM->nCOMError;
  if (nCOMError)
  {
    pCOM->nCOMError = 0;  /* Clear the error */
    return (nCOMError);
  }

  ENTER_QUEUE_CRITICAL_SECTION(pCOM);
  nOccupied = QUEUECalcOccupied(&pCOM->RXQueue);
  #ifdef _DEBUG
  nStatOccupied = QUEUECalcOccupied(&pCOM->RXStatQueue);
  #endif
  LEAVE_QUEUE_CRITICAL_SECTION(pCOM);

  ASSERT((nStatOccupied % sizeof(struct COMStat)) == 0);
  ASSERT(nOccupied == nStatOccupied / (int)sizeof(struct COMStat));

  if (nOccupied > 0)
  {
    ENTER_QUEUE_CRITICAL_SECTION(pCOM);
    nHead = pCOM->RXQueue.nHead;  /* Store the current queue head position */
    QUEUEGet(&pCOM->RXQueue, pChar, 1);
    pCOM->RXQueue.nHead = nHead;  /* Restore the prior queue head position */
    if (pStat != NULL)
    {
      nHead = pCOM->RXStatQueue.nHead;  /* Store the current queue head position */
      QUEUEGet(&pCOM->RXStatQueue, (char *)pStat, sizeof(struct COMStat));
      pCOM->RXStatQueue.nHead = nHead;  /* Restore the prior queue head position */
    }
    LEAVE_QUEUE_CRITICAL_SECTION(pCOM);
  }
  else
    return (COM_BUFEMPTY);

  return (0);  /* No error and a character has been extracted */
}
END_OF_FUNCTION(COMPeekChar);

/* ************************************************************************
   Function: COMClearRXBuffer
   Description:
     Clears all the characters currently in the rx buffer.
*/
void COMClearRXBuffer(int nCOM)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */

  ENTER_QUEUE_CRITICAL_SECTION(pCOM);
  pCOM->RXQueue.nHead = pCOM->RXQueue.nTail = 0;
  pCOM->RXStatQueue.nHead = pCOM->RXStatQueue.nTail = 0;
  LEAVE_QUEUE_CRITICAL_SECTION(pCOM);
}
END_OF_FUNCTION(COMClearRXBuffer);

/* ************************************************************************
   Function: COMRXBufferFree
   Description:
     Returns an integer indicating how much space is availabe in
     the receive queue.
*/
int COMRXBufferFree(int nCOM)
{
  struct COMDesc *pCOM;
  int nFree;
  #ifdef _DEBUG
  int nStatFree;
  #endif

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */

  ENTER_QUEUE_CRITICAL_SECTION(pCOM);
  nFree = QUEUECalcFree(&pCOM->RXQueue);
  #ifdef _DEBUG
  nStatFree = QUEUECalcFree(&pCOM->RXStatQueue);
  #endif
  LEAVE_QUEUE_CRITICAL_SECTION(pCOM);

  ASSERT((nStatFree % sizeof(struct COMStat)) == 0);
  ASSERT(nFree == nStatFree / (int)sizeof(struct COMStat));

  return (nFree);
}
END_OF_FUNCTION(COMRXBufferFree);

/* ************************************************************************
   Function: COMRXBufferUsed
   Description:
     Returns an integer telling the number of characters currently in
     the receive queue.
*/
int COMRXBufferUsed(int nCOM)
{
  struct COMDesc *pCOM;
  int nUsed;
  #ifdef _DEBUG
  int nStatUsed;
  #endif

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */

  ENTER_QUEUE_CRITICAL_SECTION(pCOM);
  nUsed = QUEUECalcOccupied(&pCOM->RXQueue);
  #ifdef _DEBUG
  nStatUsed = QUEUECalcOccupied(&pCOM->RXStatQueue);
  #endif
  LEAVE_QUEUE_CRITICAL_SECTION(pCOM);

  ASSERT((nStatUsed % sizeof(struct COMStat)) == 0);
  ASSERT(nUsed == nStatUsed / (int)sizeof(struct COMStat));

  return (nUsed);
}
END_OF_FUNCTION(COMRXBufferUsed);

/* ************************************************************************
   Function: COMSetDtr
   Description:
     Sets the DTR line to 0 or 1 depending on nControl is 0 or nonzero.
*/
void COMSetDtr(int nCOM, int nControl)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */

  if (nControl)
    outportb(MCR(pCOM), inportb(MCR(pCOM)) | 0x1);  /* set DTR bit */
  else
    outportb(MCR(pCOM), inportb(MCR(pCOM)) & ~0x1);  /* clear DTR bit */
}
END_OF_FUNCTION(COMSetDtr);

/* ************************************************************************
   Function: COMSetRts
   Description:
     Sets the RTS line to 0 or 1 depending on nControl is 0 or nonzero.
*/
void COMSetRts(int nCOM, int nControl)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */

  if (nControl)
    outportb(MCR(pCOM), inportb(MCR(pCOM)) | 0x2);  /* set RTS bit */
  else
    outportb(MCR(pCOM), inportb(MCR(pCOM)) & ~0x2);  /* clear RTS bit */
}
END_OF_FUNCTION(COMSetRts);

/* ************************************************************************
   Function: COMGetModemStatus
   Description:
     Returns the contents of the modem status register. It is read from
     a variable. Direct reading of MSR may improperly deactivate pending
     IRQ.
*/
int COMGetModemStatus(int nCOM)
{
  struct COMDesc *pCOM;
  int nMSR;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */

  nMSR = pCOM->nSaveMSR;
  /* clear delta bits to simulate real MSR read operation */
  pCOM->nSaveMSR &= ~( DELTA_CTS | DELTA_DSR | DELTA_RI | DELTA_CD );

  return (nMSR);
}
END_OF_FUNCTION(COMGetModemStatus);

/* ************************************************************************
   Function: COMGetCts
   Description:
     Returns the current state of Clear To Send (CTS) modem status line.
*/
int COMGetCts(int nCOM)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */

  return ((pCOM->nSaveMSR & CTS_LINE) == 0 ? 0 : 1);
}
END_OF_FUNCTION(COMGetCts);

/* ************************************************************************
   Function: COMGetDsr
   Description:
     Returns the current state of Data Set Ready (DSR) modem status line.
*/
int COMGetDsr(int nCOM)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */

  return ((pCOM->nSaveMSR & DSR_LINE) == 0 ? 0 : 1);
}
END_OF_FUNCTION(COMGetDsr);

/* ************************************************************************
   Function: COMGetRI
   Description:
     Returns the state of the incoming modem status line
     Ring Indicator (RI). Will be 1 when the line is ringing.
*/
int COMGetRI(int nCOM)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */

  return ((pCOM->nSaveMSR & RI_LINE) == 0 ? 0 : 1);
}
END_OF_FUNCTION(COMGetRI);

/* ************************************************************************
   Function: COMGetCD
   Description:
     Returns the current state of Carrier Detect (CD) line.
*/
int COMGetCD(int nCOM)
{
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */

  return ((pCOM->nSaveMSR & CD_LINE) == 0 ? 0 : 1);
}
END_OF_FUNCTION(COMGetCD);

/* ************************************************************************
   Function: COMDisplayDiagCounters
   Description:
*/
void COMDisplayDiagCounters(int nCOM)
{
  #ifdef _DEBUG
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */

  printf("rx: %d, tx: %d, stat: %d, modem: %d, IRQs: %d\n",
    pCOM->nRX, pCOM->nTX, pCOM->nStat, pCOM->nModem, pCOM->nIRQs);
  #else
    (void)nCOM;
  #endif
}

/* ************************************************************************
   Function: COMDisplayCompileSettings
   Description:
*/
void COMDisplayCompileSettings(void)
{
  #ifdef _DEBUG
  printf("COM library version %x.%x\n", COMVER >> 8, COMVER & 0xff);
  #ifdef DISABLE_PREEMPTING
  printf("definition: DISABLE_PREEMPTING\n");
  #endif
  #ifdef DISABLE_TIMING
  printf("definition: DISABLE_TIMING\n");
  #endif
  #ifdef PUSH386
  printf("definition: PUSH386\n");
  #endif
  #endif
}

/* ************************************************************************
   Function: COMGetDiagnosticCounters
   Description:
*/
void COMGetDiagnosticCounters(int nCOM, int *nRX, int *nTX, int *nStat,
  int *nModem, int *nIRQs)
{
  #ifdef _DEBUG
  struct COMDesc *pCOM;

  pCOM = &COMs[nCOM];

  ASSERT(bCOMInit);  /* Library should be initialized before calling any function */
  ASSERT(nCOM >= 0 && nCOM < COMMAX);
  ASSERT(pCOM->bInstalled);  /* Call COMPortOpen() first */

  if (nRX != NULL)
    *nRX = pCOM->nRX;
  if (nTX != NULL)
    *nTX = pCOM->nTX;
  if (nStat != NULL)
    *nStat = pCOM->nStat;
  if (nModem != NULL)
    *nModem = pCOM->nModem;
  if (nIRQs != NULL)
    *nIRQs = pCOM->nIRQs;
  #else
    (void)nCOM;
    (void)nRX;
    (void)nTX;
    (void)nStat;
    (void)nModem;
    (void)nIRQs;
  #endif
}

/* ************************************************************************
   Function: COMExit
   Description:
     Call-back function invoked at the end of the program.
*/
static void COMExit(void)
{
  if (bCOMInit)
    COMShutDown();
}

/* ************************************************************************
   Function: COMInit
   Description:
     Initial setup of the library.
*/
int COMInit(void)
{
  ASSERT(!bCOMInit);  /* Should be called only once */

  bCOMInit = TRUE;

  /* Get hardware descriptions from BIOS */
  if (!COMs[COM1].bHardwareSet)
  {
    COMs[COM1].nCOMAddress = _peekw(0x40, COM1 * 2);
    COMs[COM1].nIRQ = 4;
    COMs[COM1].bHardwareSet = TRUE;
  }
  if (!COMs[COM2].bHardwareSet)
  {
    COMs[COM2].nCOMAddress = _peekw(0x40, COM2 * 2);
    COMs[COM2].nIRQ = 3;
    COMs[COM2].bHardwareSet = TRUE;
  }
  if (!COMs[COM3].bHardwareSet)
  {
    COMs[COM3].nCOMAddress = _peekw(0x40, COM3 * 2);
    COMs[COM3].nIRQ = 4;
    COMs[COM3].bHardwareSet = TRUE;
  }
  if (!COMs[COM4].bHardwareSet)
  {
    COMs[COM4].nCOMAddress = _peekw(0x40, COM4 * 2);
    COMs[COM4].nIRQ = 3;
    COMs[COM4].bHardwareSet = TRUE;
  }

  /*
  Lock code and data regions that can be used from IRQ routines.
  */
  if (LOCK_VARIABLE(bCOMInit) == -1)
    return (FALSE);
  if (LOCK_VARIABLE(COMs) == -1)
    return (FALSE);
  if (LOCK_VARIABLE(pAttachedPorts) == -1)
    return (FALSE);
  if (LOCK_VARIABLE(COMWrappers) == -1)
    return (FALSE);

  if (LOCK_FUNCTION(EnableIRQ) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(DisableIRQ) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(EndIRQ) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(COMDisableIRQs) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(COMEnableIRQs) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(COMWrap0) == -1)  /* This will lock COMWrap0()-COMWrap15() */
    return (FALSE);
  if (LOCK_FUNCTION(XMemCpy) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(QUEUEPut) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(QUEUEGet) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(QUEUECalcFree) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(QUEUECalcOccupied) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(QUEUEAdvance) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(COMHandler) == -1)
    return (FALSE);

  if (LOCK_FUNCTION(COMSetEventHandler) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(COMGetEventHandler) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(COMGetTXQueueSize) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(COMGetRXQueueSize) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(COMActivateTransmition) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(COMWriteChar) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(COMWriteBuffer) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(COMClearTXBuffer) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(COMTXBufferFree) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(COMTXBufferUsed) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(COMIsTXBufferSent) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(COMReadChar) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(COMReadBuffer) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(COMPeekChar) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(COMClearRXBuffer) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(COMRXBufferFree) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(COMRXBufferUsed) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(COMSetDtr) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(COMSetRts) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(COMGetModemStatus) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(COMGetCts) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(COMGetDsr) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(COMGetRI) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(COMGetCD) == -1)
    return (FALSE);

  if (atexit(COMExit) != 0)
    return (FALSE);

  #ifndef DISABLE_TIMING
  if (!TIMERInit())
    return (FALSE);
  #endif

  return (TRUE);
}

/* ************************************************************************
   Function: COMShutDown
   Description:
     Terminates libirary session, should be called upon exiting
     the program.
*/
void COMShutDown(void)
{
#ifndef DISABLE_TIMING
  TIMERShutDown();
#endif
  COMPortCloseAll();
  bCOMInit = FALSE;
}
