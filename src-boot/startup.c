/*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
   --------------------------------------------------------------------------*/

#include <stdint.h>
#include <stddef.h>

extern void caml_startup (char **argv);

// Defined by the initial boot registers, we just save them.
volatile uint32_t rpi_boot_dev; // boot device code (usually 0)
volatile uint32_t rpi_arm_machine; // ARM machine type
volatile uint32_t rpi_hw; // atags or device tree address

// Memory mapped IO base for Rpi2, FIXME this should not be hardcoded.
volatile uint32_t rpi_mmio_base = 0x3F000000;

void halt (int status)
{
  if (status == 0) { while (1) { __asm ("wfe"); } } else
  {
    while (1)
    {
      // Flash ACT led
      *(volatile uint32_t *)(rpi_mmio_base + 0x00200020) = (1 << 15);
      for (volatile int i = 0; i < 60000; i++) ;
      *(volatile uint32_t *)(rpi_mmio_base + 0x0020002C) = (1 << 15);
      for (volatile int i = 0; i < 60000; i++) ;
    }
  }
}

void _startup (uint32_t boot_dev, uint32_t arm_machine, uint32_t hw)
{
  rpi_boot_dev = boot_dev;
  rpi_arm_machine = arm_machine;
  rpi_hw = hw;

  static char *argv[2] = { "rpi-boot-ocaml", NULL };
  caml_startup (argv); // If uncaught exn, this calls exit(2)
  halt (0);
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
