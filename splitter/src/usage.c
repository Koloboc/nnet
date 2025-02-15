
#include <stdio.h>
#include <system.h>
#include "usage.h"
#include "splitter.h"

/* #ifndef DEFAULT_TMPDIR */
/* # define DEFAULT_TMPDIR "/tmp" */
/* #endif */

static inline void emit_stdin_note (void) {
	fputs (("\nWith no FILE, or when FILE is -, read standard input.\n"), stdout);
}

static inline void emit_mandatory_arg_note (void) {
	fputs (("\nMandatory arguments to long options are mandatory for short options too.\n"), stdout);
}

void usage (int status) {
	if (status != EXIT_SUCCESS)
		emit_try_help ();
	else {
		  printf ((" Usage: %s [OPTION]... [FILE]...\n"), program_name);
		  fputs (("\
		Write sorted concatenation of all FILE(s) to standard output.\n\
		"), stdout);

		  emit_stdin_note ();
		  emit_mandatory_arg_note ();

		  fputs (("\
		Other options:\n\
		\n\
		"), stdout);
		  fputs (("\
		  --debug               annotate the part of the line used to sort,\n\
								  and warn about questionable usage to stderr\n\
		"), stdout);
		  fputs (("\
		-o, --output=FILE         write result to FILE instead of standard output\n\
		"), stdout);
		  printf (("\
		-t, --field-separator=SEP  use SEP instead of non-blank to blank transition\n\
		-T, --temporary-directory=DIR  use DIR for temporaries, not $TMPDIR or %s;\n\
								  multiple options specify multiple directories\n\
		-u, --unique              check for strict ordering;\n\
		\n\
		"), DEFAULT_TMPDIR);
		  fputs (("\
		-z, --zero-terminated     line delimiter is NUL, not newline\n\
		"), stdout);
		  fputs (HELP_OPTION_DESCRIPTION, stdout);
		  fputs (VERSION_OPTION_DESCRIPTION, stdout);
		  fputs (("\
		\n\
		"), stdout );
		  emit_ancillary_info (PROGRAM_NAME);
	}

	exit (status);
}

