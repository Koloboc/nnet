#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <unistd.h>
#include <getopt.h>
#include <errno.h>
#include <error.h>
#include <limits.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <assert.h>

#include "system.h"
#include "xalloc.h"
#include "splitter.h"
#include "tempname.h"
#include "progname.h"
#include "ctype.h"
#include "euidaccess.h"
#include "nproc.h"
#include "hash.h"

#ifndef RLIMIT_DATA
struct rlimit { size_t rlim_cur; };
# define getrlimit(Resource, Rlp) (-1)
#endif

/* Define away proper_name (leaving proper_name_utf8, which affects far
   fewer programs), since it's not worth the cost of adding ~17KB to
   the x86_64 text size of every single program.  This avoids a 40%
   (almost ~2MB) increase in the on-disk space utilization for the set
   of the 100 binaries. */
#define proper_name(x) (x)

#define AUTHORS \
	proper_name ("Mike Haertel"), \
	proper_name ("Koloboc"), \
	proper_name ("Paul Eggert")

/* Exit statuses.  */
enum
{
	/* POSIX says to exit with status 1 if invoked with -c and the
	   input is not properly sorted.  */
	SORT_OUT_OF_ORDER = 1,

	/* POSIX says any other irregular exit must exit with a status
	   code greater than 1.  */
	SORT_FAILURE = 2
};

int volatile exit_failure = EXIT_FAILURE;

# define OPEN_MAX 20

#ifndef ARRAY_CARDINALITY
# define ARRAY_CARDINALITY(Array) (sizeof (Array) / sizeof *(Array))
#endif

# define die(status, ...)

#define STREQ(a, b) (strcmp (a, b) == 0)

/* Use SA_NOCLDSTOP as a proxy for whether the sigaction machinery is present.  */
#ifndef SA_NOCLDSTOP
# define SA_NOCLDSTOP 0
# define sigprocmask(How, Set, Oset) (0)
# define sigset_t int
# if ! HAVE_SIGINTERRUPT
#  define siginterrupt(sig, flag) /* empty */
# endif
#endif

static int stable;
# define FLEXIBLE_ARRAY_MEMBER

# ifndef UINTMAX_MAX
#  undef UINTMAX_C
#  undef uintmax_t
#  if ULONG_MAX >> 31 == 1
typedef unsigned long long int gl_uintmax_t;
#   define uintmax_t gl_uintmax_t
#  elif defined GL_UINT64_T
#   define uintmax_t uint64_t
#  else
typedef unsigned long int gl_uintmax_t;
#   define uintmax_t gl_uintmax_t
#  endif
# endif


/* The number of bytes needed for a merge or check buffer, which can
   function relatively efficiently even if it holds only one line.  If
   a longer line is seen, this value is increased.  */
static size_t merge_buffer_size = MAX (MIN_MERGE_BUFFER_SIZE, 256 * 1024);

static bool debug;
bool mergeonly = false;
static size_t sort_size;
static unsigned int nmerge = NMERGE_DEFAULT;

/* Number of temporary directory names used.  */
static size_t temp_dir_count;

/* Number of allocated slots in temp_dirs.  */
static size_t temp_dir_alloc;

/* Array of directory names in which any temporary files are to be created. */
static char const **temp_dirs;

/* Used to implement --unique (-u).  */
static struct line saved_line;

/* Flag to remove consecutive duplicate lines from the output.
   Only the last of a sequence of equal lines will be output. */
static bool unique;

/* The character marking end of line. Default to \n. */
static char eolchar = '\n';


/* The set of signals that are caught.  */
static sigset_t caught_signals;

/* Nonzero if any of the input files are the standard input. */
static bool have_read_stdin;

/* The list of temporary files. */
struct tempnode {
	struct tempnode *volatile next;
	pid_t pid;     /* The subprocess PID; undefined if state == UNCOMPRESSED.  */
	char state;
	char name[FLEXIBLE_ARRAY_MEMBER];
};
static struct tempnode *volatile temphead;
static struct tempnode *volatile *temptail = &temphead;

size_t __fpending (FILE *fp) {
	/* Most systems provide FILE as a struct and the necessary bitmask in
	   <stdio.h>, because they need it for implementing getc() and putc() as
	   fast macros.  */
#if defined _IO_EOF_SEEN || defined _IO_ftrylockfile || __GNU_LIBRARY__ == 1
	/* GNU libc, BeOS, Haiku, Linux libc5 */
	return fp->_IO_write_ptr - fp->_IO_write_base;
#elif defined __sferror || defined __DragonFly__ || defined __ANDROID__
	/* FreeBSD, NetBSD, OpenBSD, DragonFly, Mac OS X, Cygwin < 1.7.34, Minix 3, Android */
	return fp_->_p - fp_->_bf._base;
#elif defined __EMX__                /* emx+gcc */
	return fp->_ptr - fp->_buffer;
#elif defined __minix                /* Minix */
	return fp_->_ptr - fp_->_buf;
#elif defined _IOERR                 /* AIX, HP-UX, IRIX, OSF/1, Solaris, OpenServer, UnixWare, mingw, MSVC, NonStop Kernel, OpenVMS */
	return (fp_->_ptr ? fp_->_ptr - fp_->_base : 0);
#elif defined __UCLIBC__             /* uClibc */
	return (fp->__modeflags & __FLAG_WRITING ? fp->__bufpos - fp->__bufstart : 0);
#elif defined __QNX__                /* QNX */
	return (fp->_Mode & 0x2000 /*_MWRITE*/ ? fp->_Next - fp->_Buf : 0);
#elif defined __MINT__               /* Atari FreeMiNT */
	return fp->__bufp - fp->__buffer;
#elif defined EPLAN9                 /* Plan9 */
	return fp->wp - fp->buf;
#else
# error "Please port gnulib fpending.c to your platform!"
	return 1;
#endif
}

/* Possible states for a temp file.  If compressed, the file's status
   is unreaped or reaped, depending on whether 'sort' has waited for
   the subprocess to finish.  */
enum { UNCOMPRESSED, UNREAPED, REAPED };

/* If TAB has this value, blanks separate fields.  */
enum { TAB_DEFAULT = CHAR_MAX + 1 };

/* Tab character separating fields.  If TAB_DEFAULT, then fields are
   separated by the empty string between a non-blank character and a blank
   character. */
static int tab = TAB_DEFAULT;

/* For long options that have no equivalent short option, use a
   non-character as a pseudo short option, starting with CHAR_MAX + 1.  */
enum
{
	CHECK_OPTION = CHAR_MAX + 1,
	DEBUG_PROGRAM_OPTION,
	/* NMERGE_OPTION, */
	/* RANDOM_SOURCE_OPTION, */
	/* SORT_OPTION, */
	PARALLEL_OPTION
};

/* The number of threads after which there are
   diminishing performance gains.  */
enum { DEFAULT_MAX_THREADS = 8 };

static char const short_options[] = "-mo:t:T:uz";
static struct option const long_options[] =
{
	{"merge", no_argument, NULL, 'm'},
	{"output", required_argument, NULL, 'o'},
	{"field-separator", required_argument, NULL, 't'},
	{"temporary-directory", required_argument, NULL, 'T'},
	{"unique", no_argument, NULL, 'u'},
	{"zero-terminated", no_argument, NULL, 'z'},
	{GETOPT_HELP_OPTION_DECL},
	{GETOPT_VERSION_OPTION_DECL},
	{NULL, 0, NULL, 0},
};

static void sort_die (char const *message, char const *file) {
	die (SORT_FAILURE, errno, "%s: %s", message, quotef (file ? file : "standard output"));
}

int close_stream (FILE *stream) {
	const bool some_pending = (__fpending (stream) != 0);
	const bool prev_fail = (ferror (stream) != 0);
	const bool fclose_fail = (fclose (stream) != 0);

	/* Return an error indication if there was a previous failure or if
	   fclose failed, with one exception: ignore an fclose failure if
	   there was no previous error, no data remains to be flushed, and
	   fclose failed with EBADF.  That can happen when a program like cp
	   is invoked like this 'cp a b >&-' (i.e., with standard output
	   closed) and doesn't generate any output (hence no previous error
	   and nothing to be flushed).  */

	if (prev_fail || (fclose_fail && (some_pending || errno != EBADF))) {
		if (! fclose_fail)
			errno = 0;
		return EOF;
	}

	return 0;
}

void close_stdout (void){

	if (close_stream (stdout) != 0 && !(errno == EPIPE)) {

		char const *write_error = "write error";
		error (0, errno, "%s", write_error);
		_exit (EXIT_FAILURE);
	}

	/* Close stderr only if not sanitizing, as sanitizers may report to
	   stderr after this function returns.  */
	if (close_stream (stderr) != 0)
		_exit (EXIT_FAILURE);
}

/* Clean up any remaining temporary files.  */

static void cleanup (void) {
	struct tempnode const *node;

	for (node = temphead; node; node = node->next)
		unlink (node->name);
	temphead = NULL;
}

int atexit (void (*f) ()) {
	/* If the system doesn't provide a definition for atexit, use on_exit
	   if the system provides that.  */
	on_exit (f, 0);
	return 0;
}
/* Critical section status.  */
struct cs_status {
	bool valid;
	sigset_t sigs;
};

/* Enter a critical section.  */
static void cs_enter (struct cs_status *status) {
	int ret = pthread_sigmask (SIG_BLOCK, &caught_signals, &status->sigs);
	status->valid = ret == 0;
}

/* Leave a critical section.  */
static void cs_leave (struct cs_status const *status) {
	if (status->valid)
	{
		/* Ignore failure when restoring the signal mask. */
		pthread_sigmask (SIG_SETMASK, &status->sigs, NULL);
	}
}
/* Handle interrupts and hangups. */
static void sighandler (int sig) {
	if (! SA_NOCLDSTOP)
		signal (sig, SIG_IGN);

	cleanup ();

	signal (sig, SIG_DFL);
	raise (sig);
}

/* Cleanup actions to take when exiting.  */

static void exit_cleanup (void){
	if (temphead) {
		/* Clean up any remaining temporary files in a critical section so
		   that a signal handler does not try to clean them too.  */
		struct cs_status cs;
		cs_enter (&cs);
		cleanup ();
		cs_leave (&cs);
	}

	close_stdout ();
}

/* Append DIR to the array of temporary directory names.  */
static void add_temp_dir (char const *dir) {
	if (temp_dir_count == temp_dir_alloc)
		temp_dirs = X2NREALLOC (temp_dirs, &temp_dir_alloc);

	temp_dirs[temp_dir_count++] = dir;
}

static void check_inputs (char *const *files, size_t nfiles) {
	for (size_t i = 0; i < nfiles; i++) {
		if (STREQ (files[i], "-"))
			continue;

		if (euidaccess (files[i], R_OK) != 0)
			sort_die ("cannot read", files[i]);
	}
}

/* Move OLDFD to NEWFD.  If OLDFD != NEWFD, NEWFD is not close-on-exec.  */

static void move_fd (int oldfd, int newfd) {
	if (oldfd != newfd) {
		/* This should never fail for our usage.  */
		dup2 (oldfd, newfd);
		close (oldfd);
	}
}

/* Ensure a specified output file can be created or written to,
   and point stdout to it.  Do not truncate the file.
   Exit with a diagnostic on failure.  */

static void check_output (char const *outfile) {
	if (outfile) {
		int oflags = O_WRONLY | O_CLOEXEC | O_CREAT;
		/* int outfd = open (outfile, oflags, MODE_RW_UGO); */
		int outfd = open (outfile, oflags);
		if (outfd < 0)
			sort_die ("open failed", outfile);
		move_fd (outfd, STDOUT_FILENO);
	}
}

/* Create a new temporary file, returning its newly allocated tempnode.
   Store into *PFD the file descriptor open for writing.
   If the creation fails, return NULL and store -1 into *PFD if the
   failure is due to file descriptor exhaustion and
   SURVIVE_FD_EXHAUSTION; otherwise, die.  */

static struct tempnode * create_temp_file (int *pfd, bool survive_fd_exhaustion) {
	static char const slashbase[] = "/splitterXXXXXX";
	static size_t temp_dir_index;
	int fd;
	int saved_errno;
	char const *temp_dir = temp_dirs[temp_dir_index];
	size_t len = strlen (temp_dir);
	struct tempnode *node = xmalloc (FLEXSIZEOF (struct tempnode, name, len + sizeof slashbase));
	char *file = node->name;
	struct cs_status cs;

	memcpy (file, temp_dir, len);
	memcpy (file + len, slashbase, sizeof slashbase);
	node->next = NULL;
	if (++temp_dir_index == temp_dir_count)
		temp_dir_index = 0;

	/* Create the temporary file in a critical section, to avoid races.  */
	cs_enter (&cs);
	fd = gen_tempname (file, 0, O_CLOEXEC, GT_FILE);
	/* fd = mkostemp (file, O_CLOEXEC); */
	if (0 <= fd) {
		*temptail = node;
		temptail = &node->next;
	}

	saved_errno = errno;
	cs_leave (&cs);
	errno = saved_errno;

	if (fd < 0) {
		if (! (survive_fd_exhaustion && errno == EMFILE))
			die (SORT_FAILURE, errno, ("cannot create temporary file in %s"), quoteaf (temp_dir));
		free (node);
		node = NULL;
	}

	*pfd = fd;
	return node;
}

/* Create a temporary file and, if asked for, start a compressor
   to that file.  Set *PFP to the file handle and return
   the address of the new temp node.  If the creation
   fails, return NULL if the failure is due to file descriptor
   exhaustion and SURVIVE_FD_EXHAUSTION; otherwise, die.  */

static struct tempnode * maybe_create_temp (FILE **pfp, bool survive_fd_exhaustion) {
	int tempfd;
	struct tempnode *node = create_temp_file (&tempfd, survive_fd_exhaustion);
	if (! node)
		return NULL;

	node->state = UNCOMPRESSED;
	*pfp = fdopen (tempfd, "w");
	if (! *pfp)
		sort_die ("couldn't create temporary file", node->name);

	return node;
}

/* Create a temporary file and, if asked for, start a compressor
   to that file.  Set *PFP to the file handle and return the address
   of the new temp node.  Die on failure.  */

static struct tempnode * create_temp (FILE **pfp) {
	return maybe_create_temp (pfp, false);
}


/* If PID is in the process table, remove it and return true.
   Otherwise, return false.  */

static bool delete_proc (pid_t pid) {
	struct tempnode test;

	test.pid = pid;
	struct tempnode *node = hash_remove (proctab, &test);
	if (! node)
		return false;
	node->state = REAPED;
	return true;
}

/* If PID is positive, wait for the child process with that PID to
   exit, and assume that PID has already been removed from the process
   table.  If PID is 0 or -1, clean up some child that has exited (by
   waiting for it, and removing it from the proc table) and return the
   child's process ID.  However, if PID is 0 and no children have
   exited, return 0 without waiting.  */

static pid_t reap (pid_t pid) {
	int status;
	pid_t cpid = waitpid ((pid ? pid : -1), &status, (pid ? 0 : WNOHANG));

	if (cpid < 0)
		die (SORT_FAILURE, errno, _("waiting for %s [-d]"), quoteaf (compress_program));
	else if (0 < cpid && (0 < pid || delete_proc (cpid))) {
		if (! WIFEXITED (status) || WEXITSTATUS (status))
			die (SORT_FAILURE, 0, _("%s [-d] terminated abnormally"), quoteaf (compress_program));
		--nprocs;
	}

	return cpid;
}

/* Remove PID from the process table, and wait for it to exit if it
   hasn't already.  */

static void wait_proc (pid_t pid) {
	if (delete_proc (pid))
		reap (pid);
}

/* Fork a child process for piping to and do common cleanup.  The
   TRIES parameter specifies how many times to try to fork before
   giving up.  Return the PID of the child, or -1 (setting errno)
   on failure. */

static pid_t pipe_fork (int pipefds[2], size_t tries) {
#if HAVE_WORKING_FORK
	struct tempnode *saved_temphead;
	int saved_errno;
	double wait_retry = 0.25;
	pid_t pid IF_LINT ( = -1);
	struct cs_status cs;

	if (pipe2 (pipefds, O_CLOEXEC) < 0)
		return -1;

	/* At least NMERGE + 1 subprocesses are needed.  More could be created, but
	   uncontrolled subprocess generation can hurt performance significantly.
	   Allow at most NMERGE + 2 subprocesses, on the theory that there
	   may be some useful parallelism by letting compression for the
	   previous merge finish (1 subprocess) in parallel with the current
	   merge (NMERGE + 1 subprocesses).  */

	if (nmerge + 1 < nprocs)
		reap_some ();

	while (tries--) {
		/* This is so the child process won't delete our temp files
		   if it receives a signal before exec-ing.  */
		cs_enter (&cs);
		saved_temphead = temphead;
		temphead = NULL;

		pid = fork ();
		saved_errno = errno;
		if (pid)
			temphead = saved_temphead;

		cs_leave (&cs);
		errno = saved_errno;

		if (0 <= pid || errno != EAGAIN)
			break;
		else {
			xnanosleep (wait_retry);
			wait_retry *= 2;
			reap_exited ();
		}
	}

	if (pid < 0) {
		saved_errno = errno;
		close (pipefds[0]);
		close (pipefds[1]);
		errno = saved_errno;
	} else if (pid == 0) {
		close (STDIN_FILENO);
		close (STDOUT_FILENO);
	} else
		++nprocs;

	return pid;

#else  /* ! HAVE_WORKING_FORK */
	return -1;
#endif
}

static size_t proctab_hasher (void const *entry, size_t tabsize) {
	struct tempnode const *node = entry;
	return node->pid % tabsize;
}

static bool proctab_comparator (void const *e1, void const *e2) {
	struct tempnode const *n1 = e1;
	struct tempnode const *n2 = e2;
	return n1->pid == n2->pid;
}

/* TEMP represents a new process; add it to the process table.  Create
   the process table the first time it's called.  */

static void register_proc (struct tempnode *temp) {
	if (! proctab) {
		proctab = hash_initialize (INIT_PROCTAB_SIZE, NULL, proctab_hasher, proctab_comparator, NULL);
		if (! proctab)
			xalloc_die ();
	}

	temp->state = UNREAPED;

	if (! hash_insert (proctab, temp))
		xalloc_die ();
}

/* Open a compressed temp file and start a decompression process through
   which to filter the input.  Return NULL (setting errno to
   EMFILE) if we ran out of file descriptors, and die on any other
   kind of failure.  */

static FILE * open_temp (struct tempnode *temp) {
	int tempfd, pipefds[2];
	FILE *fp = NULL;

	if (temp->state == UNREAPED)
		wait_proc (temp->pid);

	tempfd = open (temp->name, O_RDONLY);
	if (tempfd < 0)
		return NULL;

	pid_t child = pipe_fork (pipefds, 9);

	switch (child) {
		case -1:
			if (errno != EMFILE)
				die (SORT_FAILURE, errno, "couldn't create process for %s -d", quoteaf (compress_program));
			close (tempfd);
			errno = EMFILE;
			break;
		default:
			temp->pid = child;
			register_proc (temp);
			close (tempfd);
			close (pipefds[1]);

			fp = fdopen (pipefds[0], "r");
			if (! fp) {
				int saved_errno = errno;
				close (pipefds[0]);
				errno = saved_errno;
			}
			break;
	}

	return fp;
}

/* Return a stream for FILE, opened with mode HOW.  A null FILE means
   standard output; HOW should be "w".  When opening for input, "-"
   means standard input.  To avoid confusion, do not return file
   descriptors STDIN_FILENO, STDOUT_FILENO, or STDERR_FILENO when
   opening an ordinary FILE.  Return NULL if unsuccessful.

   Use fadvise to specify an access pattern for input files.
   There are a few hints we could possibly provide,
   and after careful testing it was decided that
   specifying FADVISE_SEQUENTIAL was not detrimental
   to any cases.  On Linux 2.6.31, this option doubles
   the size of read ahead performed and thus was seen to
   benefit these cases:
   Merging
   Sorting with a smaller internal buffer
   Reading from faster flash devices

   In _addition_ one could also specify other hints...

   FADVISE_WILLNEED was tested, but Linux 2.6.31
   at least uses that to _synchronously_ prepopulate the cache
   with the specified range.  While sort does need to
   read all of its input before outputting, a synchronous
   read of the whole file up front precludes any processing
   that sort could do in parallel with the system doing
   read ahead of the data. This was seen to have negative effects
   in a couple of cases:
   Merging
   Sorting with a smaller internal buffer
   This option was seen to shorten the runtime for sort
   on a multicore system with lots of RAM and other processes
   competing for CPU.  It could be argued that more explicit
   scheduling hints with 'nice' et. al. are more appropriate
   for this situation.

   FADVISE_NOREUSE is a possibility as it could lower
   the priority of input data in the cache as sort will
   only need to process it once.  However its functionality
   has changed over Linux kernel versions and as of 2.6.31
   it does nothing and thus we can't depend on what it might
   do in future.

   FADVISE_DONTNEED is not appropriate for user specified
   input files, but for temp files we do want to drop the
   cache immediately after processing.  This is done implicitly
   however when the files are unlinked.  */

static FILE * stream_open (char const *file, char const *how) {
	FILE *fp;

	if (*how == 'r') {
		if (STREQ (file, "-")) {
			have_read_stdin = true;
			fp = stdin;
		} else {
			int fd = open (file, O_RDONLY | O_CLOEXEC);
			fp = fd < 0 ? NULL : fdopen (fd, how);
		}
		/* fadvise (fp, FADVISE_SEQUENTIAL); */
	} else if (*how == 'w') {
		if (file && ftruncate (STDOUT_FILENO, 0) != 0)
			die (SORT_FAILURE, errno, _("%s: error truncating"), quotef (file));
		fp = stdout;
	} else
		assert (!"unexpected mode passed to stream_open");

	return fp;
}
/* Open FILES (there are NFILES of them) and store the resulting array
   of stream pointers into (*PFPS).  Allocate the array.  Return the
   number of successfully opened files, setting errno if this value is
   less than NFILES.  */
static size_t open_input_files (struct sortfile *files, size_t nfiles, FILE ***pfps) {
	FILE **fps = *pfps = xnmalloc (nfiles, sizeof *fps);
	int i;

	/* Open as many input files as we can.  */
	for (i = 0; i < nfiles; i++) {
		fps[i] = (files[i].temp ? open_temp (files[i].temp) : stream_open (files[i].name, "r"));
		if (!fps[i])
			break;
	}

	return i;
}

/* Initialize BUF.  Reserve LINE_BYTES bytes for each line; LINE_BYTES
   must be at least sizeof (struct line).  Allocate ALLOC bytes
   initially.  */

static void initbuf (struct buffer *buf, size_t line_bytes, size_t alloc) {
	/* Ensure that the line array is properly aligned.  If the desired
	   size cannot be allocated, repeatedly halve it until allocation
	   succeeds.  The smaller allocation may hurt overall performance,
	   but that's better than failing.  */
	while (true) {
		alloc += sizeof (struct line) - alloc % sizeof (struct line);
		buf->buf = malloc (alloc);
		if (buf->buf)
			break;
		alloc /= 2;
		if (alloc <= line_bytes + 1)
			xalloc_die ();
	}

	buf->line_bytes = line_bytes;
	buf->alloc = alloc;
	buf->used = buf->left = buf->nlines = 0;
	buf->eof = false;
}

/* Return one past the limit of the line array.  */

static inline struct line * buffer_linelim (struct buffer const *buf) {
	void *linelim = buf->buf + buf->alloc;
	return linelim;
}

/* Fill BUF reading from FP, moving buf->left bytes from the end
   of buf->buf to the beginning first.  If EOF is reached and the
   file wasn't terminated by a newline, supply one.  Set up BUF's line
   table too.  FILE is the name of the file corresponding to FP.
   Return true if some input was read.  */

static bool fillbuf (struct buffer *buf, FILE *fp, char const *file) {
	char eol = eolchar;
	size_t line_bytes = buf->line_bytes;
	size_t mergesize = merge_buffer_size - MIN_MERGE_BUFFER_SIZE;

	if (buf->eof)
		return false;

	if (buf->used != buf->left)
	{
		memmove (buf->buf, buf->buf + buf->used - buf->left, buf->left);
		buf->used = buf->left;
		buf->nlines = 0;
	}

	while (true)
	{
		char *ptr = buf->buf + buf->used;
		struct line *linelim = buffer_linelim (buf);
		struct line *line = linelim - buf->nlines;
		size_t avail = (char *) linelim - buf->nlines * line_bytes - ptr;
		char *line_start = buf->nlines ? line->text + line->length : buf->buf;

		while (line_bytes + 1 < avail)
		{
			/* Read as many bytes as possible, but do not read so many
			   bytes that there might not be enough room for the
			   corresponding line array.  The worst case is when the
			   rest of the input file consists entirely of newlines,
			   except that the last byte is not a newline.  */
			size_t readsize = (avail - 1) / (line_bytes + 1);
			size_t bytes_read = fread (ptr, 1, readsize, fp);
			char *ptrlim = ptr + bytes_read;
			char *p;
			avail -= bytes_read;

			if (bytes_read != readsize)
			{
				if (ferror (fp))
					sort_die ("read failed", file);
				if (feof (fp))
				{
					buf->eof = true;
					if (buf->buf == ptrlim)
						return false;
					if (line_start != ptrlim && ptrlim[-1] != eol)
						*ptrlim++ = eol;
				}
			}

			/* Find and record each line in the just-read input.  */
			while ((p = memchr (ptr, eol, ptrlim - ptr)))
			{
				/* Delimit the line with NUL. This eliminates the need to
				   temporarily replace the last byte with NUL when calling
				   xmemcoll, which increases performance.  */
				*p = '\0';
				ptr = p + 1;
				line--;
				line->text = line_start;
				line->length = ptr - line_start;
				mergesize = MAX (mergesize, line->length);
				avail -= line_bytes;

				line_start = ptr;
			}

			ptr = ptrlim;
			if (buf->eof)
				break;
		}

		buf->used = ptr - buf->buf;
		buf->nlines = buffer_linelim (buf) - line;
		if (buf->nlines != 0)
		{
			buf->left = ptr - line_start;
			merge_buffer_size = mergesize + MIN_MERGE_BUFFER_SIZE;
			return true;
		}

		{
			/* The current input line is too long to fit in the buffer.
			   Increase the buffer size and try again, keeping it properly
			   aligned.  */
			size_t line_alloc = buf->alloc / sizeof (struct line);
			buf->buf = x2nrealloc (buf->buf, &line_alloc, sizeof (struct line));
			buf->alloc = line_alloc * sizeof (struct line);
		}
	}
}

/* Close FP, whose name is FILE, and report any errors.  */

static void xfclose (FILE *fp, char const *file) {
	switch (fileno (fp)) {
		case STDIN_FILENO:
			/* Allow reading stdin from tty more than once.  */
			if (feof (fp))
				clearerr (fp);
			break;

		case STDOUT_FILENO:
			/* Don't close stdout just yet.  close_stdout does that.  */
			if (fflush (fp) != 0)
				sort_die ("fflush failed", file);
			break;

		default:
			if (fclose (fp) != 0)
				sort_die ("close failed", file);
			break;
	}
}

/* Remove NAME from the list of temporary files.  */

static void zaptemp (char const *name) {
	struct tempnode *volatile *pnode;
	struct tempnode *node;
	struct tempnode *next;
	int unlink_status;
	int unlink_errno = 0;
	struct cs_status cs;

	for (pnode = &temphead; (node = *pnode)->name != name; pnode = &node->next)
		continue;

	if (node->state == UNREAPED)
		wait_proc (node->pid);

	/* Unlink the temporary file in a critical section to avoid races.  */
	next = node->next;
	cs_enter (&cs);
	unlink_status = unlink (name);
	unlink_errno = errno;
	*pnode = next;
	cs_leave (&cs);

	if (unlink_status != 0)
		error (0, unlink_errno, "warning: cannot remove: '%s'", name);
		/* error (0, unlink_errno, "warning: cannot remove: %s", quotef (name)); */
	if (! next)
		temptail = pnode;
	free (node);
}

/* Compare two lines A and B, returning negative, zero, or positive
   depending on whether A compares less than, equal to, or greater than B. */

static int compare (struct line const *a, struct line const *b) {
	int diff;
	size_t alen, blen;

	/* If the keys all compare equal (or no keys were specified)
	   fall through to the default comparison.  */
	alen = a->length - 1, blen = b->length - 1;

	if (alen == 0)
		diff = - NONZERO (blen);
	else if (blen == 0)
		diff = 1;
	else if (hard_LC_COLLATE) {
		/* xmemcoll0 is a performance enhancement as
		   it will not unconditionally write '\0' after the
		   passed in buffers, which was seen to give around
		   a 3% increase in performance for short lines.  */
		diff = xmemcoll0 (a->text, alen + 1, b->text, blen + 1);
	} else if (! (diff = memcmp (a->text, b->text, MIN (alen, blen))))
		diff = alen < blen ? -1 : alen != blen;

	return diff;
}

static void write_line (struct line const *line, FILE *fp, char const *output_file) {
	char *buf = line->text;
	size_t n_bytes = line->length;
	char *ebuf = buf + n_bytes;

	if (!output_file && debug)
	{
		/* Convert TAB to '>' and EOL to \n, and then output debugging info.  */
		char const *c = buf;

		while (c < ebuf)
		{
			char wc = *c++;
			if (wc == '\t')
				wc = '>';
			else if (c == ebuf)
				wc = '\n';
			if (fputc (wc, fp) == EOF)
				sort_die (("write failed"), output_file);
		}

		/* debug_line (line); */
	}
	else
	{
		ebuf[-1] = eolchar;
		if (fwrite (buf, 1, n_bytes, fp) != n_bytes)
			sort_die (("write failed"), output_file);
		ebuf[-1] = '\0';
	}
}
/* Merge lines from FILES onto OFP.  NTEMPS is the number of temporary
   files (all of which are at the start of the FILES array), and
   NFILES is the number of files; 0 <= NTEMPS <= NFILES <= NMERGE.
   FPS is the vector of open stream corresponding to the files.
   Close input and output streams before returning.
   OUTPUT_FILE gives the name of the output file.  If it is NULL,
   the output file is standard output.  */

static void mergefps (struct sortfile *files, size_t ntemps, size_t nfiles, FILE *ofp, char const *output_file, FILE **fps) {
	struct buffer *buffer = xnmalloc (nfiles, sizeof *buffer);
	/* Input buffers for each file. */
	struct line saved;		/* Saved line storage for unique check. */
	struct line const *savedline = NULL;
	/* &saved if there is a saved line. */
	size_t savealloc = 0;		/* Size allocated for the saved line. */
	struct line const **cur = xnmalloc (nfiles, sizeof *cur);
	/* Current line in each line table. */
	struct line const **base = xnmalloc (nfiles, sizeof *base);
	/* Base of each line table.  */
	size_t *ord = xnmalloc (nfiles, sizeof *ord);
	/* Table representing a permutation of fps,
	   such that cur[ord[0]] is the smallest line
	   and will be next output. */
	size_t i;
	size_t j;
	size_t t;
	saved.text = NULL;

	/* Read initial lines from each input file. */
	for (i = 0; i < nfiles; ) {
		initbuf (&buffer[i], sizeof (struct line), MAX (merge_buffer_size, sort_size / nfiles));
		if (fillbuf (&buffer[i], fps[i], files[i].name)) {
			struct line const *linelim = buffer_linelim (&buffer[i]);
			cur[i] = linelim - 1;
			base[i] = linelim - buffer[i].nlines;
			i++;
		} else {
			/* fps[i] is empty; eliminate it from future consideration.  */
			xfclose (fps[i], files[i].name);
			if (i < ntemps) {
				ntemps--;
				zaptemp (files[i].name);
			}
			free (buffer[i].buf);
			--nfiles;
			for (j = i; j < nfiles; ++j) {
				files[j] = files[j + 1];
				fps[j] = fps[j + 1];
			}
		}
	}

	/* Set up the ord table according to comparisons among input lines.
	   Since this only reorders two items if one is strictly greater than
	   the other, it is stable. */
	for (i = 0; i < nfiles; ++i)
		ord[i] = i;
	for (i = 1; i < nfiles; ++i)
		if (0 < compare (cur[ord[i - 1]], cur[ord[i]]))
			t = ord[i - 1], ord[i - 1] = ord[i], ord[i] = t, i = 0;

	/* Repeatedly output the smallest line until no input remains. */
	while (nfiles) {
		struct line const *smallest = cur[ord[0]];

		/* If uniquified output is turned on, output only the first of
		   an identical series of lines. */
		if (unique) {
			if (savedline && compare (savedline, smallest)) {
				savedline = NULL;
				write_line (&saved, ofp, output_file);
			}
			if (!savedline) {
				savedline = &saved;
				if (savealloc < smallest->length) {
					do
						if (! savealloc) {
							savealloc = smallest->length;
							break;
						}
					while ((savealloc *= 2) < smallest->length);

					free (saved.text);
					saved.text = xmalloc (savealloc);
				}
				saved.length = smallest->length;
				memcpy (saved.text, smallest->text, saved.length);
			}
		} else
			write_line (smallest, ofp, output_file);

		/* Check if we need to read more lines into core. */
		if (base[ord[0]] < smallest)
			cur[ord[0]] = smallest - 1;
		else {
			if (fillbuf (&buffer[ord[0]], fps[ord[0]], files[ord[0]].name)) {
				struct line const *linelim = buffer_linelim (&buffer[ord[0]]);
				cur[ord[0]] = linelim - 1;
				base[ord[0]] = linelim - buffer[ord[0]].nlines;
			} else {
				/* We reached EOF on fps[ord[0]].  */
				for (i = 1; i < nfiles; ++i)
					if (ord[i] > ord[0])
						--ord[i];

				--nfiles;
				xfclose (fps[ord[0]], files[ord[0]].name);
				if (ord[0] < ntemps) {
					ntemps--;
					zaptemp (files[ord[0]].name);
				}
				free (buffer[ord[0]].buf);
				for (i = ord[0]; i < nfiles; ++i) {
					fps[i] = fps[i + 1];
					files[i] = files[i + 1];
					buffer[i] = buffer[i + 1];
					cur[i] = cur[i + 1];
					base[i] = base[i + 1];
				}
				for (i = 0; i < nfiles; ++i)
					ord[i] = ord[i + 1];
				continue;
			}
		}

		/* The new line just read in may be larger than other lines
		   already in main memory; push it back in the queue until we
		   encounter a line larger than it.  Optimize for the common
		   case where the new line is smallest.  */
		{
			size_t lo = 1;
			size_t hi = nfiles;
			size_t probe = lo;
			size_t ord0 = ord[0];
			size_t count_of_smaller_lines;

			while (lo < hi) {
				int cmp = compare (cur[ord0], cur[ord[probe]]);
				if (cmp < 0 || (cmp == 0 && ord0 < ord[probe]))
					hi = probe;
				else
					lo = probe + 1;
				probe = (lo + hi) / 2;
			}

			count_of_smaller_lines = lo - 1;
			for (j = 0; j < count_of_smaller_lines; j++)
				ord[j] = ord[j + 1];
			ord[count_of_smaller_lines] = ord0;
		}
	}

	if (unique && savedline) {
		write_line (&saved, ofp, output_file);
		free (saved.text);
	}

	xfclose (ofp, output_file);
	free (fps);
	free (buffer);
	free (ord);
	free (base);
	free (cur);
}

/* Merge lines from FILES onto OFP.  NTEMPS is the number of temporary
   files (all of which are at the start of the FILES array), and
   NFILES is the number of files; 0 <= NTEMPS <= NFILES <= NMERGE.
   Close input and output files before returning.
   OUTPUT_FILE gives the name of the output file.

   Return the number of files successfully merged.  This number can be
   less than NFILES if we ran low on file descriptors, but in this
   case it is never less than 2.  */

static size_t mergefiles (struct sortfile *files, size_t ntemps, size_t nfiles, FILE *ofp, char const *output_file) {
	FILE **fps;
	size_t nopened = open_input_files (files, nfiles, &fps);
	if (nopened < nfiles && nopened < 2)
		sort_die ("open failed", files[nopened].name);
	mergefps (files, ntemps, nopened, ofp, output_file, fps);
	return nopened;
}

/* Scan through FILES[NTEMPS .. NFILES-1] looking for files that are
   the same as OUTFILE.  If found, replace each with the same
   temporary copy that can be merged into OUTFILE without destroying
   OUTFILE before it is completely read.  This temporary copy does not
   count as a merge temp, so don't worry about incrementing NTEMPS in
   the caller; final cleanup will remove it, not zaptemp.

   This test ensures that an otherwise-erroneous use like
   "sort -m -o FILE ... FILE ..." copies FILE before writing to it.
   It's not clear that POSIX requires this nicety.
   Detect common error cases, but don't try to catch obscure cases like
   "cat ... FILE ... | sort -m -o FILE"
   where traditional "sort" doesn't copy the input and where
   people should know that they're getting into trouble anyway.
   Catching these obscure cases would slow down performance in
   common cases.  */

static void avoid_trashing_input (struct sortfile *files, size_t ntemps, size_t nfiles, char const *outfile) {
	bool got_outstat = false;
	struct stat outstat;
	struct tempnode *tempcopy = NULL;

	for (size_t i = ntemps; i < nfiles; i++)
	{
		bool is_stdin = STREQ (files[i].name, "-");
		bool same;
		struct stat instat;

		if (outfile && STREQ (outfile, files[i].name) && !is_stdin)
			same = true;
		else
		{
			if (! got_outstat)
			{
				if (fstat (STDOUT_FILENO, &outstat) != 0)
					break;
				got_outstat = true;
			}

			same = (((is_stdin
							? fstat (STDIN_FILENO, &instat)
							: stat (files[i].name, &instat))
						== 0)
					&& SAME_INODE (instat, outstat));
		}

		if (same)
		{
			if (! tempcopy)
			{
				FILE *tftp;
				tempcopy = create_temp (&tftp);
				mergefiles (&files[i], 0, 1, tftp, tempcopy->name);
			}

			files[i].name = tempcopy->name;
			files[i].temp = tempcopy;
		}
	}
}
/* Merge the input FILES.  NTEMPS is the number of files at the
   start of FILES that are temporary; it is zero at the top level.
   NFILES is the total number of files.  Put the output in
   OUTPUT_FILE; a null OUTPUT_FILE stands for standard output.  */

static void merge (struct sortfile *files, size_t ntemps, size_t nfiles, char const *output_file) {
	while (nmerge < nfiles) {
		/* Number of input files processed so far.  */
		size_t in;

		/* Number of output files generated so far.  */
		size_t out;

		/* nfiles % NMERGE; this counts input files that are left over
		   after all full-sized merges have been done.  */
		size_t remainder;

		/* Number of easily-available slots at the next loop iteration.  */
		size_t cheap_slots;

		/* Do as many NMERGE-size merges as possible. In the case that
		   nmerge is bogus, increment by the maximum number of file
		   descriptors allowed.  */
		for (out = in = 0; nmerge <= nfiles - in; out++) {
			FILE *tfp;
			struct tempnode *temp = create_temp (&tfp);
			size_t num_merged = mergefiles (&files[in], MIN (ntemps, nmerge), nmerge, tfp, temp->name);
			ntemps -= MIN (ntemps, num_merged);
			files[out].name = temp->name;
			files[out].temp = temp;
			in += num_merged;
		}

		remainder = nfiles - in;
		cheap_slots = nmerge - out % nmerge;

		if (cheap_slots < remainder)
		{
			/* So many files remain that they can't all be put into the last
			   NMERGE-sized output window.  Do one more merge.  Merge as few
			   files as possible, to avoid needless I/O.  */
			size_t nshortmerge = remainder - cheap_slots + 1;
			FILE *tfp;
			struct tempnode *temp = create_temp (&tfp);
			size_t num_merged = mergefiles (&files[in], MIN (ntemps, nshortmerge),
					nshortmerge, tfp, temp->name);
			ntemps -= MIN (ntemps, num_merged);
			files[out].name = temp->name;
			files[out++].temp = temp;
			in += num_merged;
		}

		/* Put the remaining input files into the last NMERGE-sized output
		   window, so they will be merged in the next pass.  */
		memmove (&files[out], &files[in], (nfiles - in) * sizeof *files);
		ntemps += out;
		nfiles -= in - out;
	}

	avoid_trashing_input (files, ntemps, nfiles, output_file);

	/* We aren't guaranteed that this final mergefiles will work, therefore we
	   try to merge into the output, and then merge as much as we can into a
	   temp file if we can't. Repeat.  */

	while (true) {
		/* Merge directly into the output file if possible.  */
		FILE **fps;
		size_t nopened = open_input_files (files, nfiles, &fps);

		if (nopened == nfiles)
		{
			FILE *ofp = stream_open (output_file, "w");
			if (ofp) {
				mergefps (files, ntemps, nfiles, ofp, output_file, fps);
				break;
			}
			if (errno != EMFILE || nopened <= 2)
				sort_die ("open failed", output_file);
		}
		else if (nopened <= 2)
			sort_die ("open failed", files[nopened].name);

		/* We ran out of file descriptors.  Close one of the input
		   files, to gain a file descriptor.  Then create a temporary
		   file with our spare file descriptor.  Retry if that failed
		   (e.g., some other process could open a file between the time
		   we closed and tried to create).  */
		FILE *tfp;
		struct tempnode *temp;
		do
		{
			nopened--;
			xfclose (fps[nopened], files[nopened].name);
			temp = maybe_create_temp (&tfp, ! (nopened <= 2));
		}
		while (!temp);

		/* Merge into the newly allocated temporary.  */
		mergefps (&files[0], MIN (ntemps, nopened), nopened, tfp, temp->name, fps);
		ntemps -= MIN (ntemps, nopened);
		files[0].name = temp->name;
		files[0].temp = temp;

		memmove (&files[1], &files[nopened], (nfiles - nopened) * sizeof *files);
		ntemps++;
		nfiles -= nopened - 1;
	}
}

int main(int argc, char **argv){

	char **files;
	size_t nfiles = 0;
	int c = 0;
	char checkonly = 0;
	char const *outfile = NULL;
	size_t nthreads = 0;

	set_program_name (argv[0]);
	initialize_exit_failure (SORT_FAILURE);

	have_read_stdin = false;

	{
		size_t i;
		static int const sig[] =
		{
			/* The usual suspects.  */
			SIGALRM, SIGHUP, SIGINT, SIGPIPE, SIGQUIT, SIGTERM,
#ifdef SIGPOLL
			SIGPOLL,
#endif
#ifdef SIGPROF
			SIGPROF,
#endif
#ifdef SIGVTALRM
			SIGVTALRM,
#endif
#ifdef SIGXCPU
			SIGXCPU,
#endif
#ifdef SIGXFSZ
			SIGXFSZ,
#endif
		};
		enum { nsigs = ARRAY_CARDINALITY (sig) };

#if SA_NOCLDSTOP
		struct sigaction act;

		sigemptyset (&caught_signals);
		for (i = 0; i < nsigs; i++) {
			sigaction (sig[i], NULL, &act);
			if (act.sa_handler != SIG_IGN)
				sigaddset (&caught_signals, sig[i]);
		}

		act.sa_handler = sighandler;
		act.sa_mask = caught_signals;
		act.sa_flags = 0;

		for (i = 0; i < nsigs; i++)
			if (sigismember (&caught_signals, sig[i]))
				sigaction (sig[i], &act, NULL);
#else
		for (i = 0; i < nsigs; i++)
			if (signal (sig[i], SIG_IGN) != SIG_IGN) {
				signal (sig[i], sighandler);
				siginterrupt (sig[i], 1);
			}
#endif
	}
	signal (SIGCHLD, SIG_DFL); /* Don't inherit CHLD handling from parent.  */
	/* The signal mask is known, so it is safe to invoke exit_cleanup.  */
	atexit (exit_cleanup);

	printf("argc = %d\n", argc);
	files = xnmalloc (argc, sizeof *files);

	while (true)
	{
		/* Parse an operand as a file after "--" was seen; or if
		   pedantic and a file was seen, unless the POSIX version
		   is not 1003.1-2001 and -c was not seen and the operand is
		   "-o FILE" or "-oFILE".  */
		int oi = -1;

		if (c == -1
				|| (nfiles != 0
					&& ! (! checkonly
						&& optind != argc
						&& argv[optind][0] == '-' && argv[optind][1] == 'o'
						&& (argv[optind][2] || optind + 1 != argc)))
				|| ((c = getopt_long (argc, argv, short_options, long_options, &oi)) == -1))
		{
			if (argc <= optind)
				break;
			files[nfiles++] = argv[optind++];

		} else switch (c) {
			case 1:
				files[nfiles++] = optarg;
				break;

			case DEBUG_PROGRAM_OPTION:
				debug = true;
				break;

			case 'm':
				mergeonly = true;
				break;

			case 'o':
				if (outfile && !STREQ (outfile, optarg))
					die (SORT_FAILURE, 0, "multiple output files specified");
				outfile = optarg;
				break;

			case 't':	// seporator field
				{
					char newtab = optarg[0];
					if (! newtab)
						die (SORT_FAILURE, 0, "empty tab");
					if (optarg[1]) {
						if (STREQ (optarg, "\\0"))
							newtab = '\0';
						else {
							/* Provoke with 'sort -txx'.  Complain about
							   "multi-character tab" instead of "multibyte tab", so
							   that the diagnostic's wording does not need to be
							   changed once multibyte characters are supported.  */
							die (SORT_FAILURE, 0, "multi-character tab %s", quote (optarg));
						}
					}
					if (tab != TAB_DEFAULT && tab != newtab)
						die (SORT_FAILURE, 0, "incompatible tabs");
					tab = newtab;
				}
				break;

			case 'T':
				add_temp_dir (optarg);
				break;

			case 'u':
				unique = true;
				break;

			case 'z':
				eolchar = 0;
				break;

				case_GETOPT_HELP_CHAR;

				/* case_GETOPT_VERSION_CHAR (PROGRAM_NAME, AUTHORS); */

			default:
				usage (SORT_FAILURE);
		}
	}

	if (temp_dir_count == 0) {
		char const *tmp_dir = getenv ("TMPDIR");
		add_temp_dir (tmp_dir ? tmp_dir : DEFAULT_TMPDIR);
	}

	if (nfiles == 0) {
		nfiles = 1;
		free (files);
		files = xmalloc (sizeof *files);
		*files = (char *) "-";
	}

	/* Need to re-check that we meet the minimum requirement for memory
	   usage with the final value for NMERGE. */
	if (0 < sort_size)
		sort_size = MAX (sort_size, MIN_SORT_SIZE);

	/* Check all inputs are accessible, or exit immediately.  */
	check_inputs (files, nfiles);

	/* Check output is writable, or exit immediately.  */
	check_output (outfile);

	if (mergeonly) {
		struct sortfile *sortfiles = xcalloc (nfiles, sizeof *sortfiles);

		for (size_t i = 0; i < nfiles; ++i)
			sortfiles[i].name = files[i];

		merge (sortfiles, 0, nfiles, outfile);
		/* IF_LINT (free (sortfiles)); */
		free (sortfiles);
	} else {
		if (!nthreads)
		{
			unsigned long int np = num_processors (NPROC_CURRENT_OVERRIDABLE);
			nthreads = MIN (np, DEFAULT_MAX_THREADS);
		}

		/* Avoid integer overflow later.  */
		size_t nthreads_max = SIZE_MAX / (2 * sizeof (struct merge_node));
		nthreads = MIN (nthreads, nthreads_max);

		/* sort (files, nfiles, outfile, nthreads); */
	}

	/* #ifdef lint */
	free (files);
	/* #endif */

	if (have_read_stdin && fclose (stdin) == EOF)
		sort_die ("close failed", "-");

	return EXIT_SUCCESS;
}

