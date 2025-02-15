#ifndef __XALLOC_H__
#define __XALLOC_H__

extern int volatile exit_failure;

void* reallocarray (void *ptr, size_t nmemb, size_t size);
void* xreallocarray (void *p, size_t n, size_t s);
void* xnmalloc (size_t n, size_t s);
void *x2nrealloc (void *p, size_t *pn, size_t s); /* superseded by xpalloc */
static void * check_nonnull (void *p);
void * xmalloc (size_t s);
void * xcalloc (size_t n, size_t s);
void xalloc_die (void);
int xmemcoll0 (char const *, size_t, char const *, size_t);
int memcoll0 (char const *, size_t, char const *, size_t);

#endif
