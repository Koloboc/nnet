#ifndef __SPLITTER_H__
#define __SPLITTER_H__

#include "hash.h"

/* The official name of this program (e.g., no 'g' prefix).  */
#define PROGRAM_NAME "splitter"

#ifndef DEFAULT_TMPDIR
# define DEFAULT_TMPDIR "/tmp"
#endif

#define NONZERO(x) ((x) != 0)

/* Nonzero if the corresponding locales are hard.  */
static bool hard_LC_COLLATE;
#if HAVE_NL_LANGINFO
static bool hard_LC_TIME;
#endif

  /* POSIX.  */
# define PSAME_INODE(a, b) (! (((a)->st_dev ^ (b)->st_dev) \
                               | ((a)->st_ino ^ (b)->st_ino)))
/* True if struct objects A and B are known to represent the same file.  */
#define SAME_INODE(a, b) PSAME_INODE (&(a), &(b))

/* The number of unreaped child processes.  */
static pid_t nprocs;

/* Lines are held in core as counted strings. */
struct line {
	char *text;			/* Text of the line. */
	size_t length;		/* Length including final newline. */
};

/* Input buffers. */
struct buffer {
	char *buf;			/* Dynamically allocated buffer,
						   partitioned into 3 regions:
						   - input data;
						   - unused area;
						   - an array of lines, in reverse order.  */
	size_t used;			/* Number of bytes used for input data.  */
	size_t nlines;		/* Number of lines in the line array.  */
	size_t alloc;			/* Number of bytes allocated. */
	size_t left;			/* Number of bytes left from previous reads. */
	size_t line_bytes;		/* Number of bytes to reserve for each line. */
	bool eof;			/* An EOF has been read.  */
};

struct sortfile {
	/* The file's name.  */
	char const *name;

	/* Non-null if this is a temporary file, in which case NAME == TEMP->name.  */
	struct tempnode *temp;
};

/* Binary merge tree node. */
struct merge_node {
	struct line *lo;              /* Lines to merge from LO child node. */
	struct line *hi;              /* Lines to merge from HI child node. */
	struct line *end_lo;          /* End of available lines from LO. */
	struct line *end_hi;          /* End of available lines from HI. */
	struct line **dest;           /* Pointer to destination of merge. */
	size_t nlo;                   /* Total Lines remaining from LO. */
	size_t nhi;                   /* Total lines remaining from HI. */
	struct merge_node *parent;    /* Parent node. */
	struct merge_node *lo_child;  /* LO child node. */
	struct merge_node *hi_child;  /* HI child node. */
	unsigned int level;           /* Level in merge tree. */
	bool queued;                  /* Node is already in heap. */
	pthread_mutex_t lock;         /* Lock for node operations. */
};

/* Map PIDs of unreaped subprocesses to their struct tempnode objects.  */
static Hash_table *proctab;

/* During the merge phase, the number of files to merge at once. */
#define NMERGE_DEFAULT 16

/* Minimum size for a merge or check buffer.  */
#define MIN_MERGE_BUFFER_SIZE (2 + sizeof (struct line))

/* Minimum sort size; the code might not work with smaller sizes.  */
#define MIN_SORT_SIZE (nmerge * MIN_MERGE_BUFFER_SIZE)

enum { INIT_PROCTAB_SIZE = 47 };

#endif

