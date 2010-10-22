/* irq_ow.c */

#include <dos.h>
#include <mem.h>
#include <assert.h>
#include <stdlib.h>

#include "irqwrap.h"
#include "irq.h"

/*
IRQ Installing/Uninstalling functions
*/

#if defined( __WATCOMC__ )
#if 0
#define getvect(n)      _dos_getvect(n)
#define setvect(n,h)    _dos_setvect(n,h)
#else
#define getvect(n)      __dpmi_get_protected_mode_interrupt_vector(n)
#define setvect(n,h)    __dpmi_set_protected_mode_interrupt_vector(n,h)
#endif
#endif

#ifdef _DEBUG
#define ASSERT(x)        assert(x)
#else
#define ASSERT(x)
#endif

#define STACK_SIZE   (4 * 1024)      /* 4k stack should be plenty */

#define TRUE 1
#define FALSE 0

/*
If the irq will use system stack or the specially allocated stack
space in the heap.
*/
#define ALLOCATE_STACKS  FALSE

#if defined( __WATCOMC__ )
typedef void __interrupt __far (*IRQ_ISR)(void);
#else
typedef void interrupt (*IRQ_ISR)(void);
#endif
static int bInitIRQ = FALSE;
void *IRQStacks[IRQ_STACKS];

TIRQWrapper OldIRQVectors[16] =
{
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
  NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
};

static IRQ_ISR __dpmi_get_protected_mode_interrupt_vector(int nIRQVect)
{
  union REGS r;
  unsigned short sel;
  unsigned long off;

  r.x.eax = 0x0204;     /* DPMI get protected mode vector */
  r.x.ebx = nIRQVect;
  int386(0x31, &r, &r);
  sel = (unsigned short) r.x.ecx;
  off = r.x.edx;

  return (IRQ_ISR) MK_FP(sel,off);
}

static void __dpmi_set_protected_mode_interrupt_vector(int nIRQVect,
                                                       IRQ_ISR handler)
{
  union REGS r;
  void far * fh = (void __far *) handler;

  r.x.eax = 0x0205;     /* DPMI set protected mode vector */
  r.x.ebx = nIRQVect;
  r.x.ecx = FP_SEG(fh);
  r.x.edx = FP_OFF(fh);
  int386(0x31, &r, &r);
}

static int __dpmi_lock_linear_region(void * addr, long size)
{
  union REGS r;
  r.x.eax = 0x0600;     /* DPMI lock linear region */
  /* DOS/4GW uses zero-based flat memory model so
   * we can easy convert any pointer to linear address
   */
  r.x.ebx = ((unsigned long) addr) >> 16;
  r.x.ecx = ((unsigned long) addr) & 0xFFFF;
  r.x.esi = size >> 16 & 0xFFFF;
  r.x.edi = size & 0xFFFF;
  int386(0x31, &r, &r);

  return r.w.cflag;
}

static void __dpmi_unlock_linear_region(void * addr, long size)
{
  union REGS r;
  r.x.eax = 0x0601;     /* DPMI unlock linear region */
  r.x.ebx = ((unsigned long) addr) >> 16;
  r.x.ecx = ((unsigned long) addr) & 0xFFFF;
  r.x.esi = size >> 16 & 0xFFFF;
  r.x.edi = size & 0xFFFF;
  int386 (0x31, &r, &r);
}

int LockData(void *a, long size)
{
  return __dpmi_lock_linear_region(a, size);
}

int LockCode(void *a, long size)
{
  return __dpmi_lock_linear_region(a, size);
}

int UnlockData(void *a, long size)
{
  __dpmi_unlock_linear_region(a, size);
  return (0);
}

int UnlockCode(void *a, long size)
{
  __dpmi_unlock_linear_region(a, size);
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

  ASSERT(nIRQ >= 0 && nIRQ < 16);  /* Invalid IRQ vector */
  ASSERT(OldIRQVectors[nIRQ] == NULL);  /* Nothing previously attached to this IRQ */
  ASSERT(IRQHandler != NULL);  /* Invalid handler address */
  ASSERT(IRQHandlers[nIRQ] == NULL);

  if (!bInitIRQ)
    if (!InitIRQ())
      return (FALSE);

  if (nIRQ > 7)
    nIRQVect = 0x70 + (nIRQ - 8);
  else
    nIRQVect = 0x8 + nIRQ;

  OldIRQVectors[nIRQ] = (TIRQWrapper) getvect(nIRQVect);
  IRQHandlers[nIRQ] = IRQHandler;  /* IRQWrapper will call IRQHandler */
  setvect(nIRQVect, (IRQ_ISR)IRQWrappers[nIRQ]);
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

    setvect(nIRQVect, (IRQ_ISR)OldIRQVectors[nIRQ]);
    IRQHandlers[nIRQ] = NULL;
    OldIRQVectors[nIRQ] = NULL;

    /*
    Check whether all the IRQs are uninstalled and call ShutDownIRQ().
    */
    for (i = 0; i < 16; ++i)
      if (IRQHandlers[i] != NULL)
        return;  /* Still remains a handler */
    ShutDownIRQ();
  }
}

extern void interrupt IRQWrap(void);
extern void interrupt IRQWrap_End(void);

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

  #if (!ALLOCATE_STACKS)
  (void)(i=0);
  memset(IRQStacks, 0, sizeof(IRQStacks));
  #else

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
  #endif

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

  #if (!ALLOCATE_STACKS)
  (void)(i=0);
  #else
  for (i = 0; i < IRQ_STACKS; ++i)
  {
    char *p = (char *)IRQStacks[i] - (STACK_SIZE - 16);
    ASSERT(IRQStacks[i] != NULL);
    UnlockData(p, STACK_SIZE);
    free(p);
  }
  #endif
  bInitIRQ = FALSE;
  UNLOCK_VARIABLE(IRQWrappers);
  UNLOCK_VARIABLE(IRQHandlers);
  UNLOCK_VARIABLE(OldIRQVectors);
  UNLOCK_FUNCTION(IRQWrap);
}


/*
Extracting memory contents is compiler dependent as well
*/

/* ************************************************************************
   Function: _peekb
   Description:
*/
unsigned char _peekb(int nSeg, int nOfs)
{
  return (*((unsigned char far *)MK_FP((nSeg), (nOfs))));
}

/* ************************************************************************
   Function: _peekw
   Description:
*/
unsigned short int _peekw(int nSeg, int nOfs)
{
  return (*((unsigned short int far *)MK_FP((nSeg), (nOfs))));
}

/* ************************************************************************
   Function: _peekd
   Description:
*/
unsigned long _peekd(int nSeg, int nOfs)
{
  return (*((unsigned long far *)MK_FP((nSeg), (nOfs))));
}
