/* Public domain.  */
#include <stddef.h>

extern void * memset (void *dest, int val, size_t len);

void *
memset (void *dest, int val, size_t len)
{
  unsigned char *ptr = dest;
  while (len-- > 0)
    *ptr++ = val;
  return dest;
}
