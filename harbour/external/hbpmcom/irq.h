/* irq.h */

int LockData(void *a, long size);
int LockCode(void *a, long size);
int UnlockData(void *a, long size);
int UnlockCode(void *a, long size);

#define END_OF_FUNCTION(x)    static void x##_End() { }

#define LOCK_VARIABLE(x)      LockData((void *)&x, sizeof(x))
#define LOCK_FUNCTION(x)      LockCode((void *)x, (long)x##_End - (long)x)
#define UNLOCK_VARIABLE(x)    UnlockData((void *)&x, sizeof(x))
#define UNLOCK_FUNCTION(x)    UnlockCode((void *)x, (long)x##_End - (long)x)

int InstallIRQ(int nIRQ, int (*IRQHandler)(void));
void UninstallIRQ(int nIRQ);

unsigned char _peekb(int nSeg, int nOfs);
unsigned short int _peekw(int nSeg, int nOfs);
unsigned long _peekd(int nSeg, int nOfs);

