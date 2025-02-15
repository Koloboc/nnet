#ifndef __NPROC_H__
#define __NPROC_H__

/* #include "ctype.h" */

/* A "processor" in this context means a thread execution unit, that is either
   - an execution core in a (possibly multi-core) chip, in a (possibly multi-
     chip) module, in a single computer, or
   - a thread execution unit inside a core
     (hyper-threading, see <https://en.wikipedia.org/wiki/Hyper-threading>).
   Which of the two definitions is used, is unspecified.  */

enum nproc_query
{
  NPROC_ALL,                 /* total number of processors */
  NPROC_CURRENT,             /* processors available to the current process */
  NPROC_CURRENT_OVERRIDABLE  /* likewise, but overridable through the
                                OMP_NUM_THREADS environment variable */
};

/* Return the total number of processors.  The result is guaranteed to
   be at least 1.  */
extern unsigned long int num_processors (enum nproc_query query);
static unsigned long int parse_omp_threads (char const* threads);
static unsigned long int num_processors_ignoring_omp (enum nproc_query query);

#endif
