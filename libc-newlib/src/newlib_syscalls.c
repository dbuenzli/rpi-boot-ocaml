/*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
   --------------------------------------------------------------------------*/

#include <unistd.h>
#include <stddef.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/times.h>
#include <errno.h>

extern void halt (int status);

void *_sbrk (int incr) { return sbrk (incr); }
void _exit (int status) { halt (status); while (1); }

int _close (int file) { errno = ENOSYS; return -1;}
int chdir (const char *path) { errno = ENOSYS; return -1; }
int _fstat (int file, struct stat *st) { errno = ENOSYS; return -1;}
void _fini (void) { }
pid_t _getpid(void) { return 1; }
pid_t getppid(void) { return 0; }
int _gettimeofday (struct timeval *tv, struct timezone *tz)
{ errno = ENOSYS; return -1; }

char *getcwd (char *buf, size_t size) { errno = ENOSYS; return NULL; }
int _isatty (int file) { errno = ENOSYS; return -1; }
int _kill (int pid, int sig) { errno = ENOSYS; return -1;}
int _link (const char *o, const char *n) { errno = ENOSYS; return -1; }
int _lseek (int file, int ptr, int dir) { errno = ENOSYS; return 0;}
int _open (const char *name, int flags, int mode) { errno = ENOSYS; return -1;}
int _read (int fd, void *buf, size_t count) { errno = ENOSYS; return -1;}
int _stat (const char *pathname, struct stat *st) { errno = ENOSYS; return -1;}
int _unlink (const char *pathname) { errno = ENOSYS; return -1; }
int _write (int file, char *ptr, int len) { errno = ENOSYS; return -1; }
clock_t _times (struct tms *buf) { errno = ENOSYS; return -1;}

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
