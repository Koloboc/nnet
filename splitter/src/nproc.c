#include "system.h"
#include "nproc.h"
/* #include "ctype.h" */

/* Parse OMP environment variables without dependence on OMP.
   Return 0 for invalid values.  */
static unsigned long int parse_omp_threads (char const* threads) {
	unsigned long int ret = 0;

	if (threads == NULL)
		return ret;

	/* The OpenMP spec says that the value assigned to the environment variables
	 "may have leading and trailing white space".  */
	while (*threads != '\0' && c_isspace (*threads))
		threads++;

	/* Convert it from positive decimal to 'unsigned long'.  */
	if (c_isdigit (*threads))
	{
		char *endptr = NULL;
		unsigned long int value = strtoul (threads, &endptr, 10);

		if (endptr != NULL)
		{
			while (*endptr != '\0' && c_isspace (*endptr))
				endptr++;

			if (*endptr == '\0')
				return value;

			/* Also accept the first value in a nesting level,
			 since we can't determine the nesting level from env vars.  */
			else if (*endptr == ',')
				return value;
		}
	}

	return ret;
}

/* Return the number of processors available to the current process, based
   on a modern system call that returns the "affinity" between the current
   process and each CPU.  Return 0 if unknown or if such a system call does
   not exist.  */
static unsigned long num_processors_via_affinity_mask (void) {
  /* glibc >= 2.3.3 with NPTL and NetBSD 5 have pthread_getaffinity_np,
     but with different APIs.  Also it requires linking with -lpthread.
     Therefore this code is not enabled.
     glibc >= 2.3.4 has sched_getaffinity whereas NetBSD 5 has
     sched_getaffinity_np.  */
#if HAVE_PTHREAD_GETAFFINITY_NP && defined __GLIBC__ && 0
  {
    cpu_set_t set;

    if (pthread_getaffinity_np (pthread_self (), sizeof (set), &set) == 0)
      {
        unsigned long count;

# ifdef CPU_COUNT
        /* glibc >= 2.6 has the CPU_COUNT macro.  */
        count = CPU_COUNT (&set);
# else
        size_t i;

        count = 0;
        for (i = 0; i < CPU_SETSIZE; i++)
          if (CPU_ISSET (i, &set))
            count++;
# endif
        if (count > 0)
          return count;
      }
  }
#elif HAVE_PTHREAD_GETAFFINITY_NP && defined __NetBSD__ && 0
  {
    cpuset_t *set;

    set = cpuset_create ();
    if (set != NULL)
      {
        unsigned long count = 0;

        if (pthread_getaffinity_np (pthread_self (), cpuset_size (set), set)
            == 0)
          {
            cpuid_t i;

            for (i = 0;; i++)
              {
                int ret = cpuset_isset (i, set);
                if (ret < 0)
                  break;
                if (ret > 0)
                  count++;
              }
          }
        cpuset_destroy (set);
        if (count > 0)
          return count;
      }
  }
#elif HAVE_SCHED_GETAFFINITY_LIKE_GLIBC /* glibc >= 2.3.4 */
  /* There are two ways to use the sched_getaffinity() function:
       - With a statically-sized cpu_set_t.
       - With a dynamically-sized cpu_set_t.
     Documentation:
     <https://www.kernel.org/doc/man-pages/online/pages/man2/sched_getaffinity.2.html>
     <https://www.kernel.org/doc/man-pages/online/pages/man3/CPU_SET.3.html>
     The second way has the advantage that it works on systems with more than
     1024 CPUs.  The first way has the advantage that it works also when memory
     is tight.  */
# if defined CPU_ALLOC_SIZE /* glibc >= 2.6 */
	{
		unsigned int alloc_count = 1024;
		for (;;) {
			cpu_set_t *set = CPU_ALLOC (alloc_count);
			if (set == NULL)
			  /* Out of memory.  */
				break;

			unsigned int size = CPU_ALLOC_SIZE (alloc_count);
			if (sched_getaffinity (0, size, set) == 0) {
				unsigned int count = CPU_COUNT_S (size, set);
				CPU_FREE (set);
				return count;
			}
			if (errno != EINVAL)
			{
				/* Some other error.  */
				CPU_FREE (set);
				return 0;
			}
			CPU_FREE (set);
			/* Retry with some larger cpu_set_t.  */
			alloc_count *= 2;
			if (alloc_count == 0)
			  /* Integer overflow.  Avoid an endless loop.  */
				return 0;
		}
	}
# endif
	{
		cpu_set_t set;

		if (sched_getaffinity (0, sizeof (set), &set) == 0) {
			unsigned long count;

# ifdef CPU_COUNT
			/* glibc >= 2.6 has the CPU_COUNT macro.  */
			count = CPU_COUNT (&set);
# else
			size_t i;

			count = 0;
			for (i = 0; i < CPU_SETSIZE; i++)
				if (CPU_ISSET (i, &set))
					count++;
# endif
			if (count > 0)
				return count;
		}
  }
#elif HAVE_SCHED_GETAFFINITY_NP /* NetBSD >= 5 */
	{
		cpuset_t *set;

		set = cpuset_create ();
		if (set != NULL) {
			unsigned long count = 0;

			if (sched_getaffinity_np (getpid (), cpuset_size (set), set) == 0) {
				cpuid_t i;

				for (i = 0;; i++)
				{
					int ret = cpuset_isset (i, set);
					if (ret < 0)
						break;
					if (ret > 0)
						count++;
				}
			}
			cpuset_destroy (set);
			if (count > 0)
				return count;
		}
	}
#endif

	return 0;
}
/* Return the total number of processors.  Here QUERY must be one of
   NPROC_ALL, NPROC_CURRENT.  The result is guaranteed to be at least 1.  */
static unsigned long int num_processors_ignoring_omp (enum nproc_query query) {

  if (query == NPROC_CURRENT)
    {
      /* Try the modern affinity mask system call.  */
      {
        unsigned long nprocs = num_processors_via_affinity_mask ();

        if (nprocs > 0)
          return nprocs;
      }

#if defined _SC_NPROCESSORS_ONLN
      { /* This works on glibc, Mac OS X 10.5, FreeBSD, AIX, OSF/1, Solaris,
           Cygwin, Haiku.  */
        long int nprocs = sysconf (_SC_NPROCESSORS_ONLN);
        if (nprocs > 0)
          return nprocs;
      }
#endif
    }
  else /* query == NPROC_ALL */
    {
#if defined _SC_NPROCESSORS_CONF
      { /* This works on glibc, Mac OS X 10.5, FreeBSD, AIX, OSF/1, Solaris,
           Cygwin, Haiku.  */
        long int nprocs = sysconf (_SC_NPROCESSORS_CONF);

# if __GLIBC__ >= 2 && defined __linux__
        /* On Linux systems with glibc, this information comes from the /sys and
           /proc file systems (see glibc/sysdeps/unix/sysv/linux/getsysstats.c).
           In some situations these file systems are not mounted, and the
           sysconf call returns 1 or 2.  But we wish to guarantee that
           num_processors (NPROC_ALL) >= num_processors (NPROC_CURRENT).  */
        if (nprocs == 1 || nprocs == 2)
          {
            unsigned long nprocs_current = num_processors_via_affinity_mask ();

            if (/* nprocs_current > 0 && */ nprocs_current > nprocs)
              nprocs = nprocs_current;
          }
# endif

        if (nprocs > 0)
          return nprocs;
      }
#endif
    }

#if HAVE_PSTAT_GETDYNAMIC
  { /* This works on HP-UX.  */
    struct pst_dynamic psd;
    if (pstat_getdynamic (&psd, sizeof psd, 1, 0) >= 0)
      {
        /* The field psd_proc_cnt contains the number of active processors.
           In newer releases of HP-UX 11, the field psd_max_proc_cnt includes
           deactivated processors.  */
        if (query == NPROC_CURRENT)
          {
            if (psd.psd_proc_cnt > 0)
              return psd.psd_proc_cnt;
          }
        else
          {
            if (psd.psd_max_proc_cnt > 0)
              return psd.psd_max_proc_cnt;
          }
      }
  }
#endif

#if HAVE_SYSMP && defined MP_NAPROCS && defined MP_NPROCS
	{ /* This works on IRIX.  */
    /* MP_NPROCS yields the number of installed processors.
       MP_NAPROCS yields the number of processors available to unprivileged
       processes.  */
		int nprocs =
		  sysmp (query == NPROC_CURRENT && getuid () != 0 ? MP_NAPROCS : MP_NPROCS);
		if (nprocs > 0)
			return nprocs;
	}
#endif

  /* Finally, as fallback, use the APIs that don't distinguish between
     NPROC_CURRENT and NPROC_ALL.  */

#if HAVE_SYSCTL && !(defined __GLIBC__ && defined __linux__) && defined HW_NCPU
	{ /* This works on macOS, FreeBSD, NetBSD, OpenBSD.
       macOS 10.14 does not allow mib to be const.  */
		int nprocs;
		size_t len = sizeof (nprocs);
		static int mib[][2] = {
# ifdef HW_NCPUONLINE
		  { CTL_HW, HW_NCPUONLINE },
# endif
		  { CTL_HW, HW_NCPU }
		};
		for (int i = 0; i < ARRAY_SIZE (mib); i++) {
			if (sysctl (mib[i], ARRAY_SIZE (mib[i]), &nprocs, &len, NULL, 0) == 0
				&& len == sizeof (nprocs)
				&& 0 < nprocs)
          return nprocs;
		}
	}
#endif

  return 1;
}

unsigned long int num_processors (enum nproc_query query) {

	unsigned long int omp_env_limit = ULONG_MAX;

	if (query == NPROC_CURRENT_OVERRIDABLE) {
		unsigned long int omp_env_threads;
		/* Honor the OpenMP environment variables, recognized also by all
		 programs that are based on OpenMP.  */
		omp_env_threads = parse_omp_threads (getenv ("OMP_NUM_THREADS"));
		omp_env_limit = parse_omp_threads (getenv ("OMP_THREAD_LIMIT"));
		if (! omp_env_limit)
			omp_env_limit = ULONG_MAX;

		if (omp_env_threads)
			return MIN (omp_env_threads, omp_env_limit);

		query = NPROC_CURRENT;
	}
	/* Here query is one of NPROC_ALL, NPROC_CURRENT.  */
	{
	unsigned long nprocs = num_processors_ignoring_omp (query);
	return MIN (nprocs, omp_env_limit);
	}
}

