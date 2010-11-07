/* com.h */

/* ************************************************************************

                            COMLib, version 1.0

                      RS-232 comunication library

                           started: may/1998

         Program and Text Copyright, ( C ) 1998, Peter Marinov
                          All Rights Reserved.

************************************************************************ */

#ifndef COM_H
#define COM_H

#ifdef __cplusplus
extern "C" {
#endif

/* COM library version 1.0 */
#define COMVER 0x0100  /* BCD format */

#define IRQ0   0
#define IRQ1   1
#define IRQ2   2
#define IRQ3   3
#define IRQ4   4
#define IRQ5   5
#define IRQ6   6
#define IRQ7   7
#define IRQ8   8
#define IRQ9   9
#define IRQ10 10
#define IRQ11 11
#define IRQ12 12
#define IRQ13 13
#define IRQ14 14
#define IRQ15 15

#define COM1  0
#define COM2  1
#define COM3  2
#define COM4  3
#define COM5  4
#define COM6  5
#define COM7  6
#define COM8  7
#define COM9  8
#define COM10  9
#define COM11  10
#define COM12  11
#define COM13  12
#define COM14  13
#define COM15  14
#define COM16  15
#define COM17  16
#define COM18  17
#define COM19  18
#define COM20  19
#define COM21  20
#define COM22  21
#define COM23  22
#define COM24  23
#define COM25  24
#define COM26  25
#define COM27  26
#define COM28  27
#define COM29  28
#define COM30  29
#define COM31  30
#define COM32  31
#define COM33  32
#define COM34  33
#define COM35  34
#define COMMAX 35

#define CHIP8250 1
#define CHIP16450 2
#define CHIP16550 3
#define CHIP16550A 4
#define NOCHIP 5

#define COMERR_NOCHIP -1
#define COMERR_NOMEMORY -2  /* No memory allocating rx and tx buffers */
#define COMERR_RXOVERFLOW -3  /* Receive buffer was unable to accept more characters */
#define COM_BUFEMPTY -4  /* No more characters in the rx buffer */
#define COMERR_TXOVERFLOW -5  /* Tx buffer can not accept more characters */
#define COM_TIMEOUT -6  /* Timed functions return this when certain time expires */
#define COMERR_GENERAL -7  /* General error in COMPortOpen() */

/* Events for the call-back user event handler */
#define evCOMStat 1
#define evCOMRx 2
#define evCOMTx 3
#define evCOMModem 4

/* Line status error */
#define statOverrun     0x02
#define statParity      0x04
#define statFraming     0x08
#define statBreak       0x10

/* Modem line status masks */
#define DELTA_CTS 0x01
#define DELTA_DSR 0x02
#define DELTA_RI 0x04
#define DELTA_CD 0x08
#define CTS_LINE 0x10
#define DSR_LINE 0x20
#define RI_LINE 0x40
#define CD_LINE 0x80

/* Flow control parameters */
#define FLOW_RTS  0x01  /* use RTS to stop input */
#define FLOW_CTS  0x02  /* check CTS on output */
#define FLOW_DTR  0x04  /* use DTR to stop input */
#define FLOW_DSR  0x08  /* check DSR on output */
#define FLOW_DCD  0x10  /* respect DCD */
#define FLOW_XOFF 0x20  /* XON/XOFF on input */
#define FLOW_XON  0x40  /* XON/XOFF on output */

struct COMStat
{
  unsigned char nStat;
  unsigned char nBit9;
};

void COMSetHardwareParameters(int nCOM, int nIRQ, int nCOMAddress);
void COMGetHardwareParameters(int nCOM, int *pIRQ, int *pCOMAddress);
int COMDetect(int nCOM);
int COMGetChipset(int nCOM);
int COMIsFIFOAvailable(int nCOM);
void COMEnableHardwareFIFO(int nCOM);
void COMDisableHardwareFIFO(int nCOM);
int COMIsFIFOEnabled(int nCOM);
void COMSetRXThreshold(int nCOM, int nRXThreshold);
void COMSetTXThreshold(int nCOM, int nTXThreshold);
int COMGetRXThreshold(int nCOM);
int COMGetTXThreshold(int nCOM);
int COMPortOpen(int nCOM, long nBauds, int nWordLen, int nParity,
  int nStopBits, int nFlowControl, void (*EventHandler)(int nEvent));
void COMSetTransmitParameters(int nCOM, long nBauds, int nWordLen,
  int nParity, int nStopBits);
void COMGetTransmitParameters(int nCOM, long *pBauds, int *pWordLen,
  int *pParity, int *pStopBits);
void COMSetFlowControl(int nCOM, int nFlowControl);
int COMGetFlowControl(int nCOM);
void COMSetFlowChars(int nCOM, int iXONchar, int iXOFFchar);
void COMSetEventHandler(int nCOM, void (*EventHandler)(int nEvent));
void COMGetEventHandler(int nCOM, void (**EventHandler)(int nEvent));
void COMSetTXQueueSize(int nCOM, int nTXQueueSize);
int COMGetTXQueueSize(int nCOM);
void COMSetRXQueueSize(int nCOM, int nRXQueueSize);
int COMGetRXQueueSize(int nCOM);
void COMPortClose(int nCOM);
int COMWriteChar(int nCOM, char c, const struct COMStat *pStat);
int COMWriteCharTimed(int nCOM, char c, const struct COMStat *pStat, int nTimeOut);
int COMWriteBuffer(int nCOM, const char *pBuf, const struct COMStat *pStatBuf, int nSize, int *nCount);
int COMWriteBufferTimed(int nCOM, const char *pBuf, const struct COMStat *pStatBuf, int nSize, int *nCount, int nTimeOut);
void COMClearTXBuffer(int nCOM);
int COMTXBufferFree(int nCOM);
int COMTXBufferUsed(int nCOM);
int COMIsTXBufferSent(int nCOM);
int COMReadChar(int nCOM, char *pChar, struct COMStat *pStat);
int COMReadCharTimed(int nCOM, char *pChar, struct COMStat *pStat, int nTimeOut);
int COMReadBuffer(int nCOM, char *pBuf, struct COMStat *pStatBuf, int nCount);
int COMReadBufferTimed(int nCOM, char *pBuf, struct COMStat *pStatBuf, int nCount, int *nActual, int nTimeOut);
int COMPeekChar(int nCOM, char *pChar, struct COMStat *pStat);
void COMClearRXBuffer(int nCOM);
int COMRXBufferFree(int nCOM);
int COMRXBufferUsed(int nCOM);
void COMSetDtr(int nCOM, int nControl);
void COMSetRts(int nCOM, int nControl);
int COMGetModemStatus(int nCOM);
int COMGetCts(int nCOM);
int COMGetDsr(int nCOM);
int COMGetRI(int nCOM);
int COMGetCD(int nCOM);
void COMDisplayDiagCounters(int nCOM);
void COMDisplayCompileSettings(void);
void COMGetDiagnosticCounters(int nCOM, int *nRX, int *nTX, int *nStat, int *nModem, int *nIRQs);
int COMInit(void);
void COMShutDown(void);

#ifdef __cplusplus
}
#endif

#endif  /* ifndef COM_H */

