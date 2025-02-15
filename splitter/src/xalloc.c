#include <stdlib.h>
#include <errno.h>
#include <error.h>
#include <stdio.h>
#include <string.h>
#include "xalloc.h"

/* # define _GL_INT_MULTIPLY_WRAPV(a, b, r) ckd_mul (r, + (a), + (b)) */
/* #define ckd_mul(r, a, b) ((bool) _GL_INT_MULTIPLY_WRAPV (a, b, r)) */
/* # define ckd_mul(a, b, r) ckd_mul (r, + (a), + (b)) */

void xalloc_die (void) {
	error (EXIT_FAILURE, 0, "%s", "memory exhausted");

	/* _Noreturn cannot be given to error, since it may return if
	 its first argument is 0.  To help compilers understand the
	 xalloc_die does not return, call abort.  Also, the abort is a
	 safety feature if exit_failure is 0 (which shouldn't happen).  */
	abort ();
}

static void * check_nonnull (void *p) {
	if (!p)
		xalloc_die ();
	return p;
}

/* Allocate zeroed memory for N elements of S bytes, with error
   checking.  S must be nonzero.  */

void * xcalloc (size_t n, size_t s) {
  return check_nonnull (calloc (n, s));
}

/* Allocate S bytes of memory dynamically, with error checking.  */

void * xmalloc (size_t s) {
  return check_nonnull (malloc (s));
}

void * reallocarray (void *ptr, size_t nmemb, size_t size) {
	size_t nbytes = nmemb * size;
	/* if (ckd_mul (&nbytes, nmemb, size)) */
	/* { */
	/* 	errno = ENOMEM; */
	/* 	return NULL; */
	/* } */

	/* Call realloc, setting errno to ENOMEM on failure.  */
	printf("realloc mem %d\n", nbytes);
	return realloc (ptr, nbytes);
}
/* Change the size of an allocated block of memory P to an array of N
   objects each of S bytes, with error checking.  */

void * xreallocarray (void *p, size_t n, size_t s) {
	void *r = reallocarray (p, n, s);
	if (!r)
		xalloc_die ();
	return r;
}

/* Allocate an array of N objects, each with S bytes of memory,
   dynamically, with error checking.  S must be nonzero.  */

void * xnmalloc (size_t n, size_t s) {
	return xreallocarray (NULL, n, s);
}

void * x2nrealloc (void *p, size_t *pn, size_t s) {
	size_t n = *pn;

	if (! p) {
		if (! n) {
			/* The approximate size to use for initial small allocation
			 requests, when the invoking code specifies an old size of
			 zero.  This is the largest "small" request for the GNU C
			 library malloc.  */
			enum { DEFAULT_MXFAST = 64 * sizeof (size_t) / 4 };

			n = DEFAULT_MXFAST / s;
			n += !n;
		}
	} else {
		/* Set N = floor (1.5 * N) + 1 to make progress even if N == 0.  */
		/* if (ckd_add (&n, n, (n >> 1) + 1)) */
			xalloc_die ();
	}

	p = xreallocarray (p, n, s);
	*pn = n;
	return p;
}

/* Compare S1 (with size S1SIZE) and S2 (with length S2SIZE) according
   to the LC_COLLATE locale.  S1 and S2 are both blocks of memory with
   nonzero sizes, and the last byte in each block must be a null byte.
   Set errno to an error number if there is an error, and to zero
   otherwise.  */
static int strcoll_loop (char const *s1, size_t s1size, char const *s2, size_t s2size) {
	int diff;

	while (! (errno = 0, (diff = strcoll (s1, s2)) || errno))
	{
		/* strcoll found no difference, but perhaps it was fooled by NUL
		   characters in the data.  Work around this problem by advancing
		   past the NUL chars.  */
		size_t size1 = strlen (s1) + 1;
		size_t size2 = strlen (s2) + 1;
		s1 += size1;
		s2 += size2;
		s1size -= size1;
		s2size -= size2;

		if (s1size == 0)
			return - (s2size != 0);
		if (s2size == 0)
			return 1;
	}

	return diff;
}

static void collate_error (int collation_errno, char const *s1, size_t s1len, char const *s2, size_t s2len) {
	error (0, collation_errno, "string comparison failed");
	error (0, 0, "Set LC_ALL='C' to work around the problem.");
	error (exit_failure, 0, "The strings compared were %s and %s.", s1, s2);
}

/* Compare S1 (a memory block of size S1SIZE, with a NUL as last byte)
   and S2 (a memory block of size S2SIZE, with a NUL as last byte)
   according to the LC_COLLATE locale.  S1SIZE and S2SIZE must be > 0.
   Report an error and exit if there is an error.  */

int xmemcoll0 (char const *s1, size_t s1size, char const *s2, size_t s2size) {
	int diff = memcoll0 (s1, s1size, s2, s2size);
	int collation_errno = errno;
	if (collation_errno)
		collate_error (collation_errno, s1, s1size - 1, s2, s2size - 1);
	return diff;
}

/* Compare S1 (a memory block of size S1SIZE, with a NUL as last byte)
   and S2 (a memory block of size S2SIZE, with a NUL as last byte)
   according to the LC_COLLATE locale.  S1SIZE and S2SIZE must be > 0.
   Set errno to an error number if there is an error, and to zero
   otherwise.  */
int memcoll0 (char const *s1, size_t s1size, char const *s2, size_t s2size) {
	if (s1size == s2size && memcmp (s1, s2, s1size) == 0) {
		errno = 0;
		return 0;
	} else
		return strcoll_loop (s1, s1size, s2, s2size);
}

