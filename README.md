rpi-boot-ocaml - Raspberry Pi boot support for the OCaml system
------------------------------------------------------------------------------
Release %%VERSION%%

rpi-boot-ocaml is a minimal static library and linker script for
booting a Raspberry Pi directly into the OCaml system. The rest is
yours.

For now only the Raspberry [Pi 2 Model B][1] is supported.

rpi-boot-ocaml is distributed under the BSD3 license.

[1]: https://www.raspberrypi.org/products/raspberry-pi-2-model-b/

## Prerequisites

We first need an OCaml cross compiler for ARMv7. Add the following
repository to `opam`:
```
opam repo add rpi-boot-ocaml http://erratique.ch/repos/rpi-boot-ocaml.git
opam update
```
Install an OCaml compiler with a version that matches the cross-compiler
(currently 4.02.3). On a 64-bit build system you need a 32-bit OCaml compiler.
```
opam switch 4.02.3       # If you are on a 32-bit platform
opam switch 4.02.3+32bit # If you are on a 64-bit platform
eval `opam config env`
```
You need ARM's gcc [cross-compiler][2] in your path. On Linux and
MacOSX with homebrew it can be installed with:
```
opam depext ocaml-armv7-none-eabihf
```
The OCaml cross compiler can now be installed via:
```
opam install ocaml-armv7-none-eabihf
```
The cross compiler is installed in `$(opam config var
prefix)/armv7-none-eabihf`. It can be invoked by using the
`-toolchain` argument of `ocamlfind`:
```
ocamlfind -toolchain armv7_none_eabihf ocamlopt -config
```
Note that the compiler is configured not to link against `libc` and
`libm` so that you can substitute them with whatever you wish.

[2]: (https://launchpad.net/gcc-arm-embedded)

## Now run an OCaml system kernel

Clone this repository:
```
git clone http://erratique.ch/repos/rpi-boot-ocaml.git
cd rpi-boot-ocaml
```
A simple base example kernel is in the [`src`](src) directory. Adjust the
variables in [Makefile.config](Makefile.config). And:
```
make
```
This will generate the kernel image `_build/rpi-boot-ocaml.img` that
can be loaded by the Raspberry Pi.

To run it, the easiest is to have an SD card loaded with one of the
officially supported operating system images, follow
[these instructions][3]. Simply copy `_build/rpi-boot-ocaml.img` at
the root of the SD card and add the following line to the `config.txt`
file present at that location:
```
kernel=rpi-boot-ocaml.img
```

Unmount the SD card, plug it in the Raspberry Pi. To witness the OCaml
system boot you can connect a display and/or a [serial cable][4]
and/or a display to the Pi. Power the system. The display should show
something like this:

![rpi-boot-ocaml greetings](doc/greet.jpg)

The serial connection should output something along the lines of:
```
Welcome to rpi-boot-ocaml 🐫
Boot time: 1324ms
Framebuffer: 800x480 bpp:32 addr:0x3DA83000
```
Note that the outrageous boot time is not due to the use of OCaml
whose runtime and linked modules initialisation only adds ~4ms to an
equivalent boot sequence into a plain C kernel.

[3]: https://www.raspberrypi.org/documentation/installation/installing-images/README.md
[4]: http://elinux.org/RPi_Serial_Connection

## Boot and halt procedure 

We describe what happens once we get a chance to execute our code. If
you want to know about the slow GPU dance that comes before see for
example this (sligthly outdated) [stackoverflow answer][5].

The kernel starts with [`boot.S`](src-boot/boot.S) which:

1. Enables the L1 cache and branch prediction.
2. Enables the Neon MPE.
3. Zeroes the C [bss section](https://en.wikipedia.org/wiki/.bss).
4. Setups the C call stack and jumps into the `_startup` C function of
   [`startup.c`](src-boot/startup.c) which simply calls
   `caml_startup` and never returns.

If the program ends up in the C `exit` or `abort` function the `halt`
function of [`startup.c`](src-boot/startup.c) gets called with the
status code (255 for `abort`). If the status code is non-zero the ACT
led of the Pi will flash at high-frequency; this will happen for
example in case of an uncaught OCaml exception or an out of memory
condition. If `halt` is called with a status of `0` we just call the
`wfe` ARM instruction in a loop; this will happen on `exit 0` or if
`caml_startup` returns normally.

[5]: http://raspberrypi.stackexchange.com/a/10595

## Kernel development and image build procedure

To build a kernel simply create an OCaml program and link it into an
object cfile using `ocamlopt`'s [`-output-obj`][6] option.

This object file must then be linked using the
[`rpi-boot-ocaml.ld`](rpi-boot-ocaml.ld) linker script with the
following libraries:

* `lib-ocaml-boot.a`, has the boot code from [`src-boot`](src-boot)
* `libbigarray.a`, OCaml's bigarray support.
* `libasmrun.a`, OCaml's runtime library.
* `libgcc.a`, gcc library needed for some of the generated code.
* `libc.a`, a `libc` of your choice.
* `libm.a`, a `libm` of your choice.

This results in an an ELF executable that must be stripped to a raw
binary ARM executable using `objcopy`.

If you hack directly on this repo you should be able to simply add
`.c` and `.ml` files to [`src`](src), mention the `.ml` files in
[`Makefile.config`](Makefile.config) and things should compile into
the `_build/rpi-boot-ocaml.img` kernel image. Generated documentation
for the helper `Rpi` module can be found
[here](http://erratique.ch/software/rpi-boot-ocaml/doc/Rpi.html).

In case of problems you can have a look at the generated assembly code
`_build/rpi-boot-ocaml.dasm` and the map file
`_build/rpi-boot-ocaml.map`.

[6]: http://caml.inria.fr/pub/docs/manual-ocaml/intfc.html#sec454

## Status and future plans

Note that all this is only a starting point and there are quite a few
issues to solve and a few problems you may run into. See the
[`TODO.md`](TODO.md) file and the issue tracker.

Once we get proper cross-compilation support in `opam` the plan is to
eventually package and split away `libc-ocaml` and make this a package
only for the `src-boot` static library and the linker script (and
maybe the helper base [`Rpi`](`src/rpi.mli`) module).  The rest in
`src` should be developed and distributed independently as libraries.
