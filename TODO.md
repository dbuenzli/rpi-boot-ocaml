* Use openlibm for libm.

* libc-ocaml `snprintf`, it seems that some formats are broken, test that fix
  it or replace it.

* Interrupt vector setup and handling strategy. The vectors should
  simply set some global variables. With effects we can then easily
  have code that suspends until the interrupt occurs by making an
  effect.  The main run loop simply checks the interrupt global
  variables to see if there's something new and invokes the
  continuations waiting on them.  If there's nothing to do the main
  loop can sleep with `WFI` until the next os timer deadline.
  
* Stack size ? What's the correct way of setting this up ? See link script.
  Can we honour/specify the OCAMLRUNPARAM variable name. 

* Heap size ? See `sbrk.c`. We should repect the ARM/VideoCore split
  which we can get using the mailbox (tag 0x00010005). Formally it may
  be too late if we are in a hypothetical case were ocaml init allocs
  a lot before we get to the initalisation of the rpi module Rpi where
  we set the split as a toplevel phrase, we can simply assume that Rpi
  must be linked early if not the first and set a minimal

* Determine why that `*(.bss*)` is needed in the build script *for newlib*'s
  libc. Without that sbrk our gets called with negative inputs at the start
  which leads to `ENOMEM`.

* Try to compile the ocaml compiler with `-nostdinc`, and against
  libc-ocaml, this will avoid the need for `newlib-fix.c` in
  libc-ocaml which is there because the newlib header pollutes the
  object files of `libasmrun`. This would also allow to remove
  `bare-includes` from the cross compiler OCaml package.

* Distribute `libc-ocaml` separately.

* Provide a way to allocate bigarrays aligned on 16 bytes boundaries
  (needed for VC communication). Currently a hack is being used.

* Device tree support, need to add a trailer to the kernel image otherwise
  we get atags.

* Sort out the memory addressing and MMU stuff.

* `git grep FIXME`.

* The build system is fucked up w.r.t. to ocaml dependencies. Too tired
  with build systems to fix that.
