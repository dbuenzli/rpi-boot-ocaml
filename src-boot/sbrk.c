/*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
   --------------------------------------------------------------------------*/

#include <errno.h>
#include <stdint.h>

// FIXME heap_max size should set to the ARM/GPU memory split which we
// can get throught the mailbox. Currently we'll happily overwite gpu
// memory since heap_max is at rpi_mmio_base. As it is a bit painful
// to interact with the mailbox before we get into OCaml we should
// simply setup a reasonable amout for heap_max for the OCaml init
// and then set it to the ARM/GPU split in the Rpi module.

extern unsigned int _kernel_end; // Defined by the linker script
extern uint32_t rpi_mmio_base;   // See startup.c

// Returns a pointer to the *start* of new allocated space
void *sbrk (int incr)
{
  static char *heap_end = (char *)(&_kernel_end);
  char *heap_max = (char *)(rpi_mmio_base - 1);

  char *ptr;

  if (heap_end + incr <= heap_max)
  {
    ptr = heap_end;
    heap_end += incr;
    return (void *)ptr;
  } else {
    errno = ENOMEM;
    return (void *)(-1);
  }
}

/*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
   --------------------------------------------------------------------------*/
