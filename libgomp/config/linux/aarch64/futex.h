/* Low level futex call.  AArch64/Linux version.
   Copyright (C) 2020 Free Software Foundation, Inc.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* Provide target-specific access to the futex system call.  */

#ifndef SYS_futex
# define SYS_futex	98
#endif

static inline long int
sys_futex0 (int *addr, int op, int val)
{
  register long int x8 __asm__ ("x8") = SYS_futex;
  register long int x0 __asm__ ("x0") = (long int) addr;
  register long int x1 __asm__ ("x1") = op;
  register long int x2 __asm__ ("x2") = val;
  __asm__ volatile ("svc 0"
		    : "=r"(x0)
		    : "r"(x8), "0"(x0), "r"(x1), "r"(x2)
		    : "memory", "cc");
  return x0;
}

static inline void
futex_wait (int *addr, int val)
{
  long int err = sys_futex0 (addr, gomp_futex_wait, val);
  if (__builtin_expect (err == ENOSYS, 0))
    {
      gomp_futex_wait &= ~FUTEX_PRIVATE_FLAG;
      gomp_futex_wake &= ~FUTEX_PRIVATE_FLAG;
      sys_futex0 (addr, gomp_futex_wait, val);
    }
}

static inline void
futex_wake (int *addr, int count)
{
  long int err = sys_futex0 (addr, gomp_futex_wake, count);
  if (__builtin_expect (err == ENOSYS, 0))
    {
      gomp_futex_wait &= ~FUTEX_PRIVATE_FLAG;
      gomp_futex_wake &= ~FUTEX_PRIVATE_FLAG;
      sys_futex0 (addr, gomp_futex_wake, count);
    }
}

static inline void
cpu_relax (void)
{
  __asm__ volatile ("yield" ::: "memory");
}
