#ifndef __HASH_H__
#define __HASH_H__

typedef size_t (*Hash_hasher) (const void *entry, size_t table_size);
typedef bool (*Hash_comparator) (const void *entry1, const void *entry2);
typedef void (*Hash_data_freer) (void *entry);

struct hash_tuning {
	/* This structure is mainly used for 'hash_initialize', see the block
	   documentation of 'hash_reset_tuning' for more complete comments.  */

	float shrink_threshold;     /* ratio of used buckets to trigger a shrink */
	float shrink_factor;        /* ratio of new smaller size to original size */
	float growth_threshold;     /* ratio of used buckets to trigger a growth */
	float growth_factor;        /* ratio of new bigger size to original size */
	bool is_n_buckets;          /* if CANDIDATE really means table size */
};
typedef struct hash_tuning Hash_tuning;

struct hash_table {
	/* The array of buckets starts at BUCKET and extends to BUCKET_LIMIT-1,
	   for a possibility of N_BUCKETS.  Among those, N_BUCKETS_USED buckets
	   are not empty, there are N_ENTRIES active entries in the table.  */
	struct hash_entry *bucket;
	struct hash_entry const *bucket_limit;
	size_t n_buckets;
	size_t n_buckets_used;
	size_t n_entries;

	/* Tuning arguments, kept in a physically separate structure.  */
	const Hash_tuning *tuning;

	/* Three functions are given to 'hash_initialize', see the documentation
	   block for this function.  In a word, HASHER randomizes a user entry
	   into a number up from 0 up to some maximum minus 1; COMPARATOR returns
	   true if two user entries compare equally; and DATA_FREER is the cleanup
	   function for a user entry.  */
	Hash_hasher hasher;
	Hash_comparator comparator;
	Hash_data_freer data_freer;

	/* A linked list of freed struct hash_entry structs.  */
	struct hash_entry *free_entry_list;

#if USE_OBSTACK
	/* Whenever obstacks are used, it is possible to allocate all overflowed
	   entries into a single stack, so they all can be freed in a single
	   operation.  It is not clear if the speedup is worth the trouble.  */
	/* struct obstack entry_stack; */
#endif
};

struct hash_entry {
	void *data;
	struct hash_entry *next;
};
struct hash_table;
typedef struct hash_table Hash_table;

/* A hash table contains many internal entries, each holding a pointer to
   some user-provided data (also called a user entry).  An entry indistinctly
   refers to both the internal entry and its associated user entry.  A user
   entry contents may be hashed by a randomization function (the hashing
   function, or just "hasher" for short) into a number (or "slot") between 0
   and the current table size.  At each slot position in the hash table,
   starts a linked chain of entries for which the user data all hash to this
   slot.  A bucket is the collection of all entries hashing to the same slot.

   A good "hasher" function will distribute entries rather evenly in buckets.
   In the ideal case, the length of each bucket is roughly the number of
   entries divided by the table size.  Finding the slot for a data is usually
   done in constant time by the "hasher", and the later finding of a precise
   entry is linear in time with the size of the bucket.  Consequently, a
   larger hash table size (that is, a larger number of buckets) is prone to
   yielding shorter chains, *given* the "hasher" function behaves properly.

   Long buckets slow down the lookup algorithm.  One might use big hash table
   sizes in hope to reduce the average length of buckets, but this might
   become inordinate, as unused slots in the hash table take some space.  The
   best bet is to make sure you are using a good "hasher" function (beware
   that those are not that easy to write! :-), and to use a table size
   larger than the actual number of entries.  */

/* If an insertion makes the ratio of nonempty buckets to table size larger
   than the growth threshold (a number between 0.0 and 1.0), then increase
   the table size by multiplying by the growth factor (a number greater than
   1.0).  The growth threshold defaults to 0.8, and the growth factor
   defaults to 1.414, meaning that the table will have doubled its size
   every second time 80% of the buckets get used.  */
#define DEFAULT_GROWTH_THRESHOLD 0.8f
#define DEFAULT_GROWTH_FACTOR 1.414f

/* If a deletion empties a bucket and causes the ratio of used buckets to
   table size to become smaller than the shrink threshold (a number between
   0.0 and 1.0), then shrink the table by multiplying by the shrink factor (a
   number greater than the shrink threshold but smaller than 1.0).  The shrink
   threshold and factor default to 0.0 and 1.0, meaning that the table never
   shrinks.  */
#define DEFAULT_SHRINK_THRESHOLD 0.0f
#define DEFAULT_SHRINK_FACTOR 1.0f

/* Use this to initialize or reset a TUNING structure to
   some sensible values. */
static const Hash_tuning default_tuning =
{
	DEFAULT_SHRINK_THRESHOLD,
	DEFAULT_SHRINK_FACTOR,
	DEFAULT_GROWTH_THRESHOLD,
	DEFAULT_GROWTH_FACTOR,
	false
};


void * hash_remove (Hash_table *table, const void *entry);
static void * hash_find_entry (Hash_table *table, const void *entry, struct hash_entry **bucket_head, bool delete);
static void free_entry (Hash_table *table, struct hash_entry *entry);
static bool check_tuning (Hash_table *table);
bool hash_rehash (Hash_table *table, size_t candidate);
static size_t compute_bucket_size (size_t candidate, const Hash_tuning *tuning);
static bool is_prime (size_t candidate);
static bool transfer_entries (Hash_table *dst, Hash_table *src, bool safe);
extern void *hash_delete (Hash_table *table, const void *entry);
Hash_table * hash_initialize (size_t candidate, const Hash_tuning *tuning, Hash_hasher hasher, Hash_comparator comparator, Hash_data_freer data_freer);
static size_t raw_hasher (const void *data, size_t n);
static bool raw_comparator (const void *a, const void *b);
void * hash_insert (Hash_table *table, void const *entry);
int hash_insert_if_absent (Hash_table *table, void const *entry, void const **matched_ent);

#endif

