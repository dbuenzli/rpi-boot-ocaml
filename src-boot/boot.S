/*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
   --------------------------------------------------------------------------*/

        .section .boot
        .globl _start

// In the following we preserve the r0, r1, r2 registers as they have
// info from the GPU bootloader.

_start:

// Enable L1 data and instruction cache and branch prediction
enable_l1_cache:
        mrc p15, 0, r3, c1, c0, 0
        orr r3, #0x00000004
        orr r3, #0x00001800
        mcr p15, 0, r3, c1, c0, 0

// Enable Neon MPE (see ARM's Neon programmer's guide §2.1.7)
enable_neon:
        mrc p15, 0, r3, c1, c0, 2
        orr r3, #0x00F00000
        mcr p15, 0, r3, c1, c0, 2
        isb
        mov r3, #0x40000000
        vmsr fpexc, r3

// Clear bss segment
bss_clear:
        ldr r3, =_bss_start
        ldr r4, =_bss_end
        mov r5, #0
        mov r6, #0
        mov r7, #0
        mov r8, #0
        b bss_test_end

bss_clear_addr:
        stmia r3!, {r5-r8}

bss_test_end:
        cmp r3, r4
        blo bss_clear_addr

// Setup C stack, execute the C _startup function and never come again
exec_startup:
        ldr sp, =_stack_end
        ldr r3, =_startup
        blx r3

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
