/* irq_g.c */

#include <sys/segments.h>
#include <dpmi.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/movedata.h>
#include "irqwrap.h"
#include "irq.h"

#ifdef _DEBUG
#define ASSERT(x)        assert(x)
#else
#define ASSERT(x)
#endif

#define STACK_SIZE   (4 * 1024)      /* 4k stack should be plenty */

#define TRUE 1
#define FALSE 0

#define PADDR_NULL {0, 0}  /* offset32, selector */

__dpmi_paddr OldIRQVectors[16] =
{
  PADDR_NULL, PADDR_NULL, PADDR_NULL, PADDR_NULL,
  PADDR_NULL, PADDR_NULL, PADDR_NULL, PADDR_NULL,
  PADDR_NULL, PADDR_NULL, PADDR_NULL, PADDR_NULL,
  PADDR_NULL, PADDR_NULL, PADDR_NULL, PADDR_NULL
};

void *IRQStacks[IRQ_STACKS];

static int bInitIRQ = FALSE;

/* ************************************************************************
   Function: LockData
   Description:
     Locks particular data region.
*/
int LockData(void *a, long size)
{
  unsigned long baseaddr;
  __dpmi_meminfo region;

  if (__dpmi_get_segment_base_address(_my_ds(), &baseaddr) == -1)
    return (-1);

  region.handle = 0;
  region.size = size;
  region.address = baseaddr + (unsigned long)a;

  if (__dpmi_lock_linear_region(&region) == -1)
    return (-1);

  return (0);
}

/* ************************************************************************
   Function: LockCode
   Description:
     Locks particular code region.
*/
int LockCode(void *a, long size)
{
  unsigned long baseaddr;
  __dpmi_meminfo region;

  if (__dpmi_get_segment_base_address(_my_cs(), &baseaddr) == -1)
    return (-1);

  region.handle = 0;
  region.size = size;
  region.address = baseaddr + (unsigned long)a;

  if (__dpmi_lock_linear_region(&region) == -1)
    return (-1);

  return (0);
}

/* ************************************************************************
   Function: UnlockData
   Description:
     Unlocks particular data region.
*/
int UnlockData(void *a, long size)
{
  unsigned long baseaddr;
  __dpmi_meminfo region;

  if (__dpmi_get_segment_base_address(_my_ds(), &baseaddr) == -1)
    return (-1);

  region.handle = 0;
  region.size = size;
  region.address = baseaddr + (unsigned long)a;

  if (__dpmi_unlock_linear_region(&region) == -1)
    return (-1);

  return (0);
}

/* ************************************************************************
   Function: UnlockCode
   Description:
     Unlocks particular code region.
*/
int UnlockCode(void *a, long size)
{
  unsigned long baseaddr;
  __dpmi_meminfo region;

  if (__dpmi_get_segment_base_address(_my_cs(), &baseaddr) == -1)
    return (-1);

  region.handle = 0;
  region.size = size;
  region.address = baseaddr + (unsigned long)a;

  if (__dpmi_unlock_linear_region(&region) == -1)
    return (-1);

  return (0);
}

/* Forward definitions */
static int InitIRQ(void);
static void ShutDownIRQ(void);

/* ************************************************************************
   Function: InstallIRQ
   Description:
     Installs handler for specific IRQ.
*/
int InstallIRQ(int nIRQ, int (*IRQHandler)(void))
{
  int nIRQVect;
  __dpmi_paddr IRQWrapAddr;

  ASSERT(nIRQ >= 0 && nIRQ < 16);  /* Invalid IRQ vector */
  ASSERT(OldIRQVectors[nIRQ].selector == 0);  /* Nothing previously attached to this IRQ */
  ASSERT(IRQHandler != NULL);  /* Invalid handler address */
  ASSERT(IRQHandlers[nIRQ] == NULL);

  if (!bInitIRQ)
    if (!InitIRQ())
      return (FALSE);

  if (nIRQ > 7)
    nIRQVect = 0x70 + (nIRQ - 8);
  else
    nIRQVect = 0x8 + nIRQ;

  IRQWrapAddr.selector = _my_cs();
  IRQWrapAddr.offset32 = (int)IRQWrappers[nIRQ];
  __dpmi_get_protected_mode_interrupt_vector(nIRQVect, &OldIRQVectors[nIRQ]);
  IRQHandlers[nIRQ] = IRQHandler;  /* IRQWrapper will call IRQHandler */
  __dpmi_set_protected_mode_interrupt_vector(nIRQVect, &IRQWrapAddr);
  return (TRUE);
}

/* ************************************************************************
   Function: UninstallIRQ
   Description:
     Uninstalls IRQ handler.
*/
void UninstallIRQ(int nIRQ)
{
  int nIRQVect;
  int i;

  ASSERT(bInitIRQ);
  ASSERT(nIRQ < 16 && nIRQ >= 0);
  ASSERT(IRQHandlers[nIRQ] != NULL);

  if(IRQHandlers[nIRQ] != NULL)
  {
    if (nIRQ > 7)
      nIRQVect = 0x70 + (nIRQ - 8);
    else
      nIRQVect = 0x8 + nIRQ;

    __dpmi_set_protected_mode_interrupt_vector(nIRQVect, &OldIRQVectors[nIRQ]);
    IRQHandlers[nIRQ] = NULL;
    OldIRQVectors[nIRQ].selector = 0;

    /*
    Check whether all the IRQs are uninstalled and call ShutDownIRQ().
    */
    for (i = 0; i < 16; ++i)
      if (IRQHandlers[i] != NULL)
        return;  /* Still remains a handler */
    ShutDownIRQ();
  }
}

extern void IRQWrap(void);
extern void IRQWrap_End(void);

/* ************************************************************************
   Function: InitIRQ
   Description:
     Initial setup of the IRQ wrappers
   Returns:
     0 -- failed to allocate memory for the stacks.
       -- or failed to lock necessary memory regions.
*/
static int InitIRQ(void)
{
  int i;

  /*
  Lock IRQWrapers[], IRQHandlers[] and IRWrap0()-IRQWrap15().
  */
  if (LOCK_VARIABLE(IRQWrappers) == -1)
    return (FALSE);
  if (LOCK_VARIABLE(IRQHandlers) == -1)
    return (FALSE);
  if (LOCK_VARIABLE(OldIRQVectors) == -1)
    return (FALSE);
  if (LOCK_FUNCTION(IRQWrap) == -1)
    return (FALSE);

  for (i = 0; i < IRQ_STACKS; ++i)
  {
    if ((IRQStacks[i] = malloc(STACK_SIZE)) == NULL)
    {
fail:
      for (--i; i >= 0; --i)  /* Free what was allocated */
      {
        char *p = (char *)IRQStacks[i] - (STACK_SIZE - 16);
        UnlockData(p, STACK_SIZE);
        free(p);
      }
      return (FALSE);
    }
    if (LockData(IRQStacks[i], STACK_SIZE) == -1)
    {
      free(IRQStacks[i]);  /* Successfully allocated but not locked! */
      goto fail;
    }
    /* Stack is incremented downward */
    IRQStacks[i] = (void*)((char*)IRQStacks[i] + (STACK_SIZE - 16));
  }

  bInitIRQ = TRUE;
  return (TRUE);
}

/* ************************************************************************
   Function: ShutDownIRQ
   Description:
     Deallocates the stacks for IRQ wrappers.
*/
static void ShutDownIRQ(void)
{
  int i;

  ASSERT(bInitIRQ);

  for (i = 0; i < IRQ_STACKS; ++i)
  {
    char *p = (char *)IRQStacks[i] - (STACK_SIZE - 16);
    ASSERT(IRQStacks[i] != NULL);
    UnlockData(p, STACK_SIZE);
    free(p);
  }
  bInitIRQ = FALSE;
  UNLOCK_VARIABLE(IRQWrappers);
  UNLOCK_VARIABLE(IRQHandlers);
  UNLOCK_VARIABLE(OldIRQVectors);
  UNLOCK_FUNCTION(IRQWrap);
}

unsigned char _peekb(int nSeg, int nOfs)
{
  unsigned char c;

  dosmemget(nSeg * 16 + nOfs, sizeof(c), &c);
  return (c);
}

unsigned short int _peekw(int nSeg, int nOfs)
{
  unsigned short int c;

  dosmemget(nSeg * 16 + nOfs, sizeof(c), &c);
  return (c);
}

unsigned long _peekd(int nSeg, int nOfs)
{
  unsigned long c;

  dosmemget(nSeg * 16 + nOfs, sizeof(c), &c);
  return (c);
}
