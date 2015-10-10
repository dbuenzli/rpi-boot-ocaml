libc-ocaml - Minimal C library to support the OCaml system
------------------------------------------------------------------------------
Release %%VERSION%%

libc-ocaml is a minimal C standard library for bare metal OCaml
projects. It provides the minimal set of functions needed to compile
and link OCaml's runtime libraries.

libc-ocaml is mainly distributed under the BSD3 license, a few bits
are copyrighted under slightly different but at least as liberal
terms. See [LICENSE.md](LICENSE.md) for details.

# Porting

In order for `malloc(3)` to work your system needs to define a `sbrk` function
with the following signature:
```
void *sbrk (int incr);
```
A call to this function must increment the program's heap space by
`incr` bytes and return the address of the previous heap limit. If
there no memory left it should return `(void *)-1` and set `errno` to
`ENOMEM`.

You also need to define a `halt` function with the following signature:
```
void halt (int status)
```
This will be called on POSIX `exit` or `abort`.

# Implementation notes

The only functions that are implemented seriously are memory
allocation, string handling and string formatting functions.

Most of the other function, especially all the file system related
ones, simply error with `ENOSYS`.

# Stolen code

See [LICENSE.md](LICENSE.md) for the copyright details.

* `free`, `malloc`, `calloc` and `realloc` use Doug Lea's [malloc][1]
  implementation, see [`malloc.c`](src/malloc.c). Note that `abort`
  and [`sbrk`] are not required by the OCaml system, but are by this
  code.

* `strtod` is implemented by David M. Gay's [dtoa.c][2] source,
  see [strod.c](src/strtod.c).

* `snprintf` and `vsnprintf` are implemented using Holger Weiss's
   [sprintf][3] implementation, see [snprintf.c](src/snprintf.c).

[1]: ftp://g.oswego.edu/pub/misc/malloc.c
[2]: http://www.netlib.org/fp/dtoa.c
[3]: http://www.jhweiss.de/software/snprintf.html






