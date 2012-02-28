/* Public domain.  */
#include <stddef.h>

extern void * memmove (void *dest, const void *src, size_t len);

void *
memmove (void *dest, const void *src, size_t len)
{
  char *d = dest;
  const char *s = src;
  if (d < s)
    while (len--)
      *d++ = *s++;
  else
    {
      s += len;
      d += len;
      while (len--)
        *--d = *--s;
    }
  return dest;
}
