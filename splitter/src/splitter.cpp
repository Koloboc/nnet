#include <strings.h>
#include <stdio.h>
#include <string>
#include <string.h>
#include <unistd.h>
#include <thread>
#include <sys/types.h>
#include <fstream>
#include <iostream>
#include <math.h>
#include <Magick++.h>
#include "physmem.h"

using namespace Magick;
using namespace std;

#ifndef MAX
# define MAX(a, b) ((a) > (b) ? (a) : (b))
#endif

#ifndef MIN
# define MIN(a,b) (((a) < (b)) ? (a) : (b))
#endif

#define MAX_MERGE(total, level) (((total) >> (2 * ((level) + 1))) + 1)

enum eMode{ BYNARY = 0, TEXT = 1 };
enum{ MERGE_END = 0, MERGE_ROOT = 1 };
enum { SUBTHREAD_LINES_HEURISTIC = 128 * 1024 };

eMode write_mode;
//////////////////////////////////////////////////////////////////////////////////////////////////////////
struct line
{
	double *text;
};

//////////////////////////////////////////////////////////////////////////////////////////////////////////
static struct line saved_line;

struct buffer
{
	double *buf;			/* Dynamically allocated buffer,
                                   partitioned into 3 regions:
                                   - input data;
                                   - unused area;
                                   - an array of lines, in reverse order.  */
	size_t used;			/* Number of bytes used for input data.  */
    size_t nlines;
    size_t alloc;
	size_t left;			/* Number of bytes left from previous reads. */
	bool eof;			/* An EOF has been read.  */
};

//////////////////////////////////////////////////////////////////////////////////////////////////////////
struct merge_node
{
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

//////////////////////////////////////////////////////////////////////////////////////////////////////////
struct merge_node_queue
{
  struct heap *priority_queue;  /* Priority queue of merge tree nodes. */
  pthread_mutex_t mutex;        /* Lock for queue operations. */
  pthread_cond_t cond;          /* Conditional wait for empty queue to populate
                                   when popping. */
};

//////////////////////////////////////////////////////////////////////////////////////////////////////////

struct heap
{
	void **array;     /* array[0] is not used */
	size_t capacity;  /* Array size */
	size_t count;     /* Used as index to last element. Also is num of items. */
};

//////////////////////////////////////////////////////////////////////////////////////////////////////////

size_t chunk;
size_t n_chunk;
int bytes_chunk;
int bytes_fullchunk;
uint   precision;
double dprecision;
int target;
bool append;

#define xalloc_oversized(n, s) ((size_t) (PTRDIFF_MAX < SIZE_MAX ? PTRDIFF_MAX : SIZE_MAX - 1) / (s) < (n))

//////////////////////////////////////////////////////////////////////////////////////////////////////////
void *x2nrealloc (void *p, size_t *pn, size_t s)
{
  size_t n = *pn;

  if (! p)
    {
      if (! n)
        {
          enum { DEFAULT_MXFAST = 64 * sizeof (size_t) / 4 };

          n = DEFAULT_MXFAST / s;
          n += !n;
        }
      if (xalloc_oversized (n, s))
        abort (); //xalloc_die ();
    }
  else
    {
      if ((PTRDIFF_MAX < SIZE_MAX ? PTRDIFF_MAX : SIZE_MAX) / 3 * 2 / s
          <= n)
        abort (); //xalloc_die ();
      n += n / 2 + 1;
    }

  *pn = n;
  return realloc (p, n * s);
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////
void *xnmalloc (size_t n, size_t s)
{
  if (xalloc_oversized (n, s))
    abort(); //xalloc_die ();
  return malloc (n * s);
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////
template <typename T> inline T *
x2nrealloc (T *p, size_t *pn, size_t s)
{
  return (T *) x2nrealloc ((void *) p, pn, s);
}

///////////////////////////////////////////////////////////////////////////////////////////////////
static int compare_nodes (void const *a, void const *b);

static void
heapify_up (struct heap *h)
{
  size_t k = h->count;
  void *new_element = h->array[k];

  while (k != 1 && compare_nodes (h->array[k/2], new_element) <= 0)
    {
      h->array[k] = h->array[k/2];
      k /= 2;
    }

  h->array[k] = new_element;
}
// /////////////////////////////////////////////////////////////////////////////////////////////////////////
static size_t
heapify_down (struct heap *h, size_t initial)
{
  void *element = h->array[initial];

  size_t parent = initial;
  while (parent <= h->count / 2)
    {
      size_t child = 2 * parent;

      if (child < h->count && compare_nodes (h->array[child], h->array[child+1]) < 0)
        child++;

      if (compare_nodes (h->array[child], element) <= 0)
        break;

      h->array[parent] = h->array[child];
      parent = child;
    }

  h->array[parent] = element;
  return parent;
}
///////////////////////////////////////////////////////////////////////////////
void *heap_remove_top (struct heap *heap)
{
  void *top;

  if (heap->count == 0)
    return NULL;

  top = heap->array[1];
  heap->array[1] = heap->array[heap->count--];
  heapify_down (heap, 1);

  return top;
}
///////////////////////////////////////////////////////////////////////////////////
int heap_insert (struct heap *heap, void *item)
{
  if (heap->capacity - 1 <= heap->count)
    heap->array = x2nrealloc (heap->array, &heap->capacity, sizeof *(heap->array));

  heap->array[++heap->count] = item;
  heapify_up (heap);

  return 0;
}
//////////////////////////////////////////////////////////////////////////////////

struct heap *heap_alloc (size_t n_reserve)
{
  struct heap *heap = (struct heap*) malloc (sizeof *heap);

  if (n_reserve == 0)
    n_reserve = 1;

  heap->array = (void**)xnmalloc (n_reserve, sizeof *(heap->array));

  heap->array[0] = NULL;
  heap->capacity = n_reserve;
  heap->count = 0;

  return heap;
}
////////////////////////////////////////////////////////////////////////////////////
void heap_free (struct heap *heap)
{
  free (heap->array);
  free (heap);
}

/////////////////////////////////////////////////////////////////////////////////////


static int
compare (struct line const *a, struct line const *b)
{
	int ansv = 0;
	double *pa = a->text;
	double *pb = b->text;

	for(int i = 0; i < n_chunk; i++, pa++, pb++)
	{
		double diff = *pa - *pb;
		double dif_abs = abs(diff);
		if((dif_abs - dprecision) < 0 || !diff)
		{
			ansv = 0; //printf(" a = b\n");
		} else if(diff < 0)
		{
			ansv = -1; //printf(" a < b\n");
			break;
		}else if(diff > 0){
			ansv = 1; //printf(" a > b\n");
			break;
		}
	}
	return  ansv;
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////
static void
write_line (struct line const *line, ofstream *output_file)
{
	int *t = &target;
	if(write_mode == eMode::TEXT)
	{
		for(int i = 0; i < n_chunk; i++)
			*output_file << line->text[i] << " ";
	   *output_file << *t << endl;
	}else{
		output_file->write((char*)line->text, bytes_chunk);
		output_file->write((char*)t, sizeof(int));
	}
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////
static void
mergelines (struct line *t, size_t nlines, struct line const *lo)
{
	size_t nlo = nlines / 2;
	size_t nhi = nlines - nlo;
	struct line *hi = t - nlo;

	while (true)
		if (compare (lo - 1, hi - 1) <= 0)
		{
			*--t = *--lo;
			if (! --nlo) {return;}
		}else{
			*--t = *--hi;
			if (! --nhi)
			{
				do
					*--t = *--lo;
				while (--nlo);

				return;
			}
		}
}
//////////////////////////////////////////////////////////////////////////////////////////////

static void
sequential_sort (struct line *lines, size_t nlines, struct line *temp, bool to_temp)
{
	if (nlines == 2)
	{
		int swap = (0 < compare (&lines[-1], &lines[-2]));
		if (to_temp)
		{
			temp[-1] = lines[-1 - swap];
			temp[-2] = lines[-2 + swap];
		}
		else if (swap)
		{
			temp[-1] = lines[-1];
			lines[-1] = lines[-2];
			lines[-2] = temp[-1];
		}
	}
	else
	{
		size_t nlo = nlines / 2;
		size_t nhi = nlines - nlo;
		struct line *lo = lines;
		struct line *hi = lines - nlo;

		sequential_sort (hi, nhi, temp - (to_temp ? nlo : 0), to_temp);
		if (1 < nlo)
			sequential_sort (lo, nlo, temp, !to_temp);
		else if (!to_temp)
			temp[-1] = lo[-1];

		struct line *dest;
		struct line const *sorted_lo;
		if (to_temp)
		{
			dest = temp;
			sorted_lo = lines;
		}
		else
		{
			dest = lines;
			sorted_lo = temp;
		}
		mergelines (dest, nlines, sorted_lo);
	}
}
////////////////////////////////////////////////////////////////////////////////////////////////////////
static struct merge_node *init_node (struct merge_node*, struct merge_node*, struct line*, size_t, size_t, bool);
////////////////////////////////////////////////////////////////////////////////////////////////////////
static struct merge_node *
merge_tree_init (size_t nthreads, size_t nlines, struct line *dest)
{

	struct merge_node *merge_tree = (merge_node *)malloc (2 * sizeof *merge_tree * nthreads);

	struct merge_node *root = merge_tree;
	root->lo = root->hi = root->end_lo = root->end_hi = NULL;
	root->dest = NULL;
	root->nlo = root->nhi = nlines;
	root->parent = NULL;
	root->level = MERGE_END;
	root->queued = false;
	pthread_mutex_init (&root->lock, NULL);

	init_node (root, root + 1, dest, nthreads, nlines, false);
	return merge_tree;
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
static void
merge_tree_destroy (size_t nthreads, struct merge_node *merge_tree)
{
	size_t n_nodes = nthreads * 2;
	struct merge_node *node = merge_tree;

	while (n_nodes--)
	{
		pthread_mutex_destroy (&node->lock);
		node++;
	}

	free (merge_tree);
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
static struct merge_node *
init_node (struct merge_node *parent, struct merge_node *node_pool, struct line *dest, size_t nthreads, size_t total_lines, bool is_lo_child)
{
	size_t nlines = (is_lo_child ? parent->nlo : parent->nhi);
	size_t nlo = nlines / 2;
	size_t nhi = nlines - nlo;
	struct line *lo = dest - total_lines;
	struct line *hi = lo - nlo;
	struct line **parent_end = (is_lo_child ? &parent->end_lo : &parent->end_hi);

	struct merge_node *node = node_pool++;
	node->lo = node->end_lo = lo;
	node->hi = node->end_hi = hi;
	node->dest = parent_end;
	node->nlo = nlo;
	node->nhi = nhi;
	node->parent = parent;
	node->level = parent->level + 1;
	node->queued = false;
	pthread_mutex_init (&node->lock, NULL);

	if (nthreads > 1)
	{
		size_t lo_threads = nthreads / 2;
		size_t hi_threads = nthreads - lo_threads;
		node->lo_child = node_pool;
		node_pool = init_node (node, node_pool, lo, lo_threads,
								total_lines, true);
		node->hi_child = node_pool;
		node_pool = init_node (node, node_pool, hi, hi_threads,
								total_lines, false);
	}
	else
	{
		node->lo_child = NULL;
		node->hi_child = NULL;
	}
	return node_pool;
}
////////////////////////////////////////////////////////////////////////////////////////////////////////
static int
compare_nodes (void const *a, void const *b)
{
	struct merge_node const *nodea = (merge_node*)a;
	struct merge_node const *nodeb = (merge_node*)b;
	if (nodea->level == nodeb->level)
		return (nodea->nlo + nodea->nhi) < (nodeb->nlo + nodeb->nhi);
	return nodea->level < nodeb->level;
}
////////////////////////////////////////////////////////////////////////////////////////////////////////
static inline void
lock_node (struct merge_node *node)
{
	pthread_mutex_lock (&node->lock);
}
//////////////////////////////////////////////////////////////////////////////////////////////////////
static inline void
unlock_node (struct merge_node *node)
{
	pthread_mutex_unlock (&node->lock);
}
//////////////////////////////////////////////////////////////////////////////////////////////////////
static void
queue_destroy (struct merge_node_queue *queue)
{
	heap_free (queue->priority_queue);
	pthread_cond_destroy (&queue->cond);
	pthread_mutex_destroy (&queue->mutex);
}
////////////////////////////////////////////////////////////////////////////////////////////////////

static void
queue_init (struct merge_node_queue *queue, size_t nthreads)
{
	queue->priority_queue = heap_alloc (2 * nthreads);
	pthread_mutex_init (&queue->mutex, NULL);
	pthread_cond_init (&queue->cond, NULL);
}
///////////////////////////////////////////////////////////////////////////////////////////////////
static void
queue_insert (struct merge_node_queue *queue, struct merge_node *node)
{
	pthread_mutex_lock (&queue->mutex);
	heap_insert (queue->priority_queue, node);
	node->queued = true;
	pthread_cond_signal (&queue->cond);
	pthread_mutex_unlock (&queue->mutex);
}
////////////////////////////////////////////////////////////////////////////////////////////////////
static struct merge_node *
queue_pop (struct merge_node_queue *queue)
{
	struct merge_node *node;
	pthread_mutex_lock (&queue->mutex);
	while (! (node = (merge_node*)heap_remove_top (queue->priority_queue)))
		pthread_cond_wait (&queue->cond, &queue->mutex);
	pthread_mutex_unlock (&queue->mutex);
	lock_node (node);
	node->queued = false;
	return node;
}
///////////////////////////////////////////////////////////////////////////////////////////////////
static void
write_unique (struct line const *line, ofstream *temp_output)
{
	if (saved_line.text && ! compare (line, &saved_line))
        return;
	saved_line = *line;
	write_line (line, temp_output);
}

///////////////////////////////////////////////////////////////////////////////////////////////////
static void
mergelines_node (struct merge_node *node, size_t total_lines, ofstream* temp_output)
{
	struct line *lo_orig = node->lo;
	struct line *hi_orig = node->hi;
	size_t to_merge = MAX_MERGE (total_lines, node->level);
	size_t merged_lo;
	size_t merged_hi;

	if (node->level > MERGE_ROOT)
	{
		/* Merge to destination buffer. */
		struct line *dest = *node->dest;
		while (node->lo != node->end_lo && node->hi != node->end_hi && to_merge--)
		if (compare (node->lo - 1, node->hi - 1) <= 0)
			*--dest = *--node->lo;
		else
			*--dest = *--node->hi;

		merged_lo = lo_orig - node->lo;
		merged_hi = hi_orig - node->hi;

		if (node->nhi == merged_hi)
		while (node->lo != node->end_lo && to_merge--)
			*--dest = *--node->lo;
		else if (node->nlo == merged_lo)
		while (node->hi != node->end_hi && to_merge--)
			*--dest = *--node->hi;
		*node->dest = dest;
	}
	else
	{
		/* Merge directly to output. */
		while (node->lo != node->end_lo && node->hi != node->end_hi && to_merge--)
		{
			if (compare (node->lo - 1, node->hi - 1) <= 0)
			write_unique (--node->lo, temp_output);
			else
			write_unique (--node->hi, temp_output);
		}

		merged_lo = lo_orig - node->lo;
		merged_hi = hi_orig - node->hi;

		if (node->nhi == merged_hi)
		{
			while (node->lo != node->end_lo && to_merge--)
			write_unique (--node->lo, temp_output);
		}
		else if (node->nlo == merged_lo)
		{
			while (node->hi != node->end_hi && to_merge--)
			write_unique (--node->hi, temp_output);
		}
	}

	/* Update NODE. */
	merged_lo = lo_orig - node->lo;
	merged_hi = hi_orig - node->hi;
	node->nlo -= merged_lo;
	node->nhi -= merged_hi;
}
///////////////////////////////////////////////////////////////////////////////////
static void
queue_check_insert (struct merge_node_queue *queue, struct merge_node *node)
{
	if (! node->queued)
	{
		bool lo_avail = (node->lo - node->end_lo) != 0;
		bool hi_avail = (node->hi - node->end_hi) != 0;
		if (lo_avail ? hi_avail || ! node->nhi : hi_avail && ! node->nlo)
			queue_insert (queue, node);
	}
}
///////////////////////////////////////////////////////////////////////////////////
static void
queue_check_insert_parent (struct merge_node_queue *queue, struct merge_node *node)
{
	if (node->level > MERGE_ROOT)
	{
		lock_node (node->parent);
		queue_check_insert (queue, node->parent);
		unlock_node (node->parent);
	}
	else if (node->nlo + node->nhi == 0)
	{
/* If the MERGE_ROOT NODE has finished merging, insert the MERGE_END node.  */
		queue_insert (queue, node->parent);
	}
}
///////////////////////////////////////////////////////////////////////////////////
static void
merge_loop (struct merge_node_queue *queue,	size_t total_lines, ofstream *temp_output)
{
	while (1)
	{
		struct merge_node *node = queue_pop (queue);

		if (node->level == MERGE_END)
		{
			unlock_node (node);
/* Reinsert so other threads can pop it. */
			queue_insert (queue, node);
			break;
		}
		mergelines_node (node, total_lines, temp_output);
		queue_check_insert (queue, node);
		queue_check_insert_parent (queue, node);

		unlock_node (node);
	}
}
/////////////////////////////////////////////////////////////////////////////////////////

static void sortlines (struct line*, size_t, size_t, struct merge_node *, struct merge_node_queue *, ofstream*);

////////////////////////////////////////////////////////////////////////////////////////////////////////////
struct thread_args
{
	struct line *lines;
	size_t nthreads;
	size_t const total_lines;
	struct merge_node *const node;
	struct merge_node_queue *const queue;
	ofstream *output_temp;
};
////////////////////////////////////////////////////////////////////////////////////////////
static void *sortlines_thread (void *data)
{
	struct thread_args const *args = (thread_args*)data;
	sortlines (args->lines, args->nthreads, args->total_lines, args->node, args->queue, args->output_temp);
	return NULL;
}
//////////////////////////////////////////////////////////////////////////////////////////////////////

static void
sortlines (struct line *lines, size_t nthreads,
           size_t total_lines, struct merge_node *node,
           struct merge_node_queue *queue, ofstream *temp_output)
{
	size_t nlines = node->nlo + node->nhi;

/* Calculate thread arguments. */
	size_t lo_threads = nthreads / 2;
	size_t hi_threads = nthreads - lo_threads;
	pthread_t thread;
	struct thread_args args = {lines, lo_threads, total_lines, node->lo_child, queue, temp_output};

		if (nthreads > 1 && SUBTHREAD_LINES_HEURISTIC <= nlines	&& pthread_create (&thread, NULL, sortlines_thread, &args) == 0)
		{
			sortlines (lines - node->nlo, hi_threads, total_lines, node->hi_child, queue, temp_output);
			pthread_join (thread, NULL);
		}
		else
		{
			size_t nlo = node->nlo;
			size_t nhi = node->nhi;
			struct line *temp = lines - total_lines;
			if (1 < nhi)
				sequential_sort (lines - nlo, nhi, temp - (nlo / 2), false);
			if (1 < nlo)
				sequential_sort (lines, nlo, temp, false);

			node->lo = lines;
			node->hi = lines - nlo;
			node->end_lo = lines - nlo;
			node->end_hi = lines - nlo - nhi;

			queue_insert (queue, node);
			merge_loop (queue, total_lines, temp_output);
		}
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////
# include <sys/sysinfo.h>

static inline struct line *
buffer_linelim (struct buffer const *buf)
{
  void *linelim = ((char*)buf->buf) + buf->alloc;
  return (struct line*)linelim;
}

//////////////////////////////////////////////////////////////////////////////
/* Return the sort buffer size to use with the input files identified
   by FPS and FILES, which are alternate names of the same files.
   NFILES gives the number of input files; NFPS may be less.  Assume
   that each input line requires LINE_BYTES extra bytes' worth of line
   information.  Do not exceed the size bound specified by the user
   (or a default size bound, if the user does not specify one).  */
//static size_t
//sort_buffer_size (FILE *const *fps, size_t line_bytes)
//{
//	/* In the worst case, each input byte is a newline.  */
//	size_t worst_case_per_input_byte = line_bytes + 1;
//
//	/* Keep enough room for one extra input line and an extra byte.
//	 This extra room might be needed when preparing to read EOF.  */
//	size_t size = worst_case_per_input_byte + 1;
//
//	off_t file_size;
//	size_t worst_case;
//
//	if ((i < nfps ? fstat (fileno (fps[i]), &st)
//		   : STREQ (files[i], "-") ? fstat (STDIN_FILENO, &st)
//		   : stat (files[i], &st))
//		  != 0)
//		die (_("stat failed"), files[i]);
//
//	  if (S_ISREG (st.st_mode))
//		file_size = st.st_size;
//	  else
//		{
//		  /* The file has unknown size.  If the user specified a sort
//			 buffer size, use that; otherwise, guess the size.  */
//		  if (sort_size)
//			return sort_size;
//		  file_size = INPUT_FILE_SIZE_GUESS;
//		}
//
//	  if (! size_bound)
//		{
//		  size_bound = sort_size;
//		  if (! size_bound)
//			size_bound = default_sort_size ();
//		}
//
//	  /* Add the amount of memory needed to represent the worst case
//		 where the input consists entirely of newlines followed by a
//		 single non-newline.  Check for overflow.  */
//	  worst_case = file_size * worst_case_per_input_byte + 1;
//	  if (file_size != worst_case / worst_case_per_input_byte
//		  || size_bound - size <= worst_case)
//		return size_bound;
//	  size += worst_case;
//
//	return size;
//}
/////////////////////////////////////////////////////////////////////////////////////////////////////////
static bool
fillbuf (struct buffer *buf, char const *file)
{
	double avail = physmem_available ();
	double total = physmem_total ();
	double mem = MAX (avail, total / 8);
	size_t alloc = 0;
	try{
		Image image(file);
		image.type( GrayscaleType );

		size_t w = image.columns();
		size_t h = image.rows();

		bytes_chunk = n_chunk * sizeof(double);
		size_t nlines = (w - chunk) * (h - chunk);
		alloc = nlines * bytes_chunk + nlines * sizeof(struct line);
		if(avail < alloc)
			throw "not enough memory";

		buf->buf = (double*)malloc (alloc);
		if(!buf->buf)
			throw "malloc error (buf->buf)";

		buf->alloc = alloc;
		buf->nlines = 0;
		struct line *line = buffer_linelim(buf);
		char *ptr = (char*)buf->buf;

		if(chunk >= w && chunk >= h)
			throw "chunk >= w && chunk >= h";

		double *val; // value color
		for(unsigned int row = 0; row < (h - chunk); row++)  // по строкам пикселей
		{
			for(unsigned int column = 0; column < (w - chunk); column++) // по столбцам пикселей
			{
				line--;
				line->text = (double*)ptr;
				for(unsigned int rowChunk = 0; rowChunk < chunk; rowChunk++ )
				{
					for(unsigned int colChumk = 0; colChumk < chunk; colChumk++)
					{
						ColorGray color = image.pixelColor(column + colChumk, row + rowChunk);
						val = (double*)ptr;
						*val = color.shade();
						val++;
						ptr = (char*)val;
					}
				}
				buf->nlines++;
			} // Cols
		} // Rows

		if(buf->nlines != nlines) std::cerr << "error parsing file! " << buf->nlines << " != " << nlines << std::endl;

    }catch(Exception &error_)
	{
		std::cout << "Caught exception: " << error_.what() << std::endl;
	}catch(const char* msg)
	{
		std::cerr << "Error: " << msg << std::endl;
		std::cerr << "mem avaible " << avail << " need mem " << alloc << std::endl;
		return false;
	}
  	return true;
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////
static void
nsort (const char *file, ofstream *output_file, size_t nthreads)
{
	struct buffer buf;
	buf.alloc = 0;
	buf.buf = 0;

	if(fillbuf (&buf, file))
	{
		struct line *line = buffer_linelim(&buf);
		saved_line.text = NULL;
		if (1 < buf.nlines)
		{
			struct merge_node_queue queue;
			queue_init (&queue, nthreads);
			struct merge_node *merge_tree =	merge_tree_init (nthreads, buf.nlines, line);

			sortlines (line, nthreads, buf.nlines, merge_tree + 1, &queue, output_file);
			merge_tree_destroy (nthreads, merge_tree);
			queue_destroy (&queue);
		}else{
			write_unique (line - 1, output_file);
		}
	}
	free (buf.buf);
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////
void Usage (int status)
{
	printf("USAGE:\n");
	printf("%s -i input_image -c bytes_chunk -o out_file [-p precision] [-a] [-t target] [-m mode]\n\n");
	printf("-i:(necessarily) path/name input image (format jpg,png... available in ImageMagick)\n");
	printf("-c:(necessarily) size width or height of chank in pixels\n");
	printf("-o:(necessarily) out file\n");
	printf("-p num: decimal point precision color (default: 6)\n");
	printf("-a: flag append to outfile (not checked for uniqueness)\n");
	printf("-t target: set value target on all elements (default: -1 not defined)\n");
	printf("-m mode write file 0 - bynary or 1 - text (default: 0 bynary)\n");
	exit (status);
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////
static char const short_options[] = "c:i:o:p:t:m:a";

int
main (int argc, char **argv)
{
	append = false;
	size_t nthreads = 0;
	int argRes = 0;
	string filename;
	string outfile;
	precision = 1;
	target = -1;
	write_mode = eMode::BYNARY;

	while((argRes = getopt(argc, argv, short_options)) != -1)
	{
		switch(argRes)
		{
			case 'c':
			chunk = stoi(optarg);
			break;
			case 'i':
			filename = optarg;
			break;
			case 'o':
			outfile = optarg;
			break;
			case 'p':
			precision = stoi(optarg);
			break;
			case 't':
			target = stoi(optarg);
			break;
			case 'm':
			write_mode = (eMode) stoi(optarg);
			break;
			case 'a':
			append = true;
			break;
			case '?':
			Usage(1);
			// return -1;
			break;
		}
	}
	if(chunk == 0 || filename == "" || outfile == ""){Usage(1);}

	dprecision = 1/pow(10, precision);

	n_chunk = chunk * chunk;
	nthreads = std::thread::hardware_concurrency();

	std::ios_base::openmode mod = ios::out | ios::binary ;
	if(append) mod |= std::ios_base::app;

	ofstream of(outfile.c_str(), mod);
	nsort(filename.c_str(), &of, nthreads);
	return 0;
}

