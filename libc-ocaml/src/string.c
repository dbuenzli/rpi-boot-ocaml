/*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
   --------------------------------------------------------------------------*/

#include <string.h>
#include <errno.h>

int strcmp (const char *s1, const char *s2)
{
  while (*s1 != '\x00' && *s1 == *s2) { s1++; s2++; }
  return ((unsigned char)*s1) - ((unsigned char)*s2);
}

size_t strlen (const char *s)
{
  const char *start = s;
  while (*s) { s++; }
  return s - start;
}

char *strerror (int errnum)
{
  // We define strings only for errors that we actually return.
  switch (errnum) {
  case ENOSYS: return "Function not implemented";
  case ENOMEM: return "Not enough space";
  case EINVAL: return "Invalid argument";
  default: return "Unknown error";
  }
}

int memcmp (const void *ss1, const void *ss2, size_t n)
{
  const char *s1 = ss1;
  const char *s2 = ss2;

  while (n)
  {
    if (*s1 != *s2) { return (*s1 - *s2); }
    s1++; s2++; n--;
  }
  return 0;
}

void *memcpy (void *restrict ddst, const void *restrict ssrc, size_t n)
{
  char *dst = ddst;
  const char *src = ssrc;

  while (n) { *dst = *src; dst++; src++; n--; }
  return ddst;
}

void *memmove(void *ddst, const void *ssrc, size_t n)
{
  char *dst = ddst;
  const char *src = ssrc;

  if (src < dst && dst < src + n)
  {
    // Backward copy
    dst += n;
    src += n;
    while (n) { dst--; src--; n--; *dst = *src; }
  } else {
    while (n) { *dst = *src; dst++; src++; n--; }
  }
  return ddst;
}

void *memset(void *ss, int cc, size_t n)
{
  char *s = ss;
  unsigned char c = (unsigned char)cc;
  while (n) { *s = c; s++; n--; }
  return ss;
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
