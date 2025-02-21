/* Define PATH_MAX somehow.  Requires sys/types.h.
   Copyright (C) 1992, 1999, 2001, 2003, 2005, 2009-2024 Free Software
   Foundation, Inc.

   This file is free software: you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as
   published by the Free Software Foundation; either version 2.1 of the
   License, or (at your option) any later version.

   This file is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

#ifndef _PATHMAX_H
# define _PATHMAX_H

/* POSIX:2008 defines PATH_MAX to be the maximum number of bytes in a filename,
   including the terminating NUL byte.
   <https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/limits.h.html>
   PATH_MAX is not defined on systems which have no limit on filename length,
   such as GNU/Hurd.

   This file does *not* define PATH_MAX always.  Programs that use this file
   can handle the GNU/Hurd case in several ways:
     - Either with a package-wide handling, or with a per-file handling,
     - Either through a
         #ifdef PATH_MAX
       or through a fallback like
         #ifndef PATH_MAX
         # define PATH_MAX 8192
         #endif
       or through a fallback like
         #ifndef PATH_MAX
         # define PATH_MAX pathconf ("/", _PC_PATH_MAX)
         #endif
 */

# include <unistd.h>

# include <limits.h>

# ifndef _POSIX_PATH_MAX
#  define _POSIX_PATH_MAX 256
# endif

/* Don't include sys/param.h if it already has been.  */
# if defined HAVE_SYS_PARAM_H && !defined PATH_MAX && !defined MAXPATHLEN
#  include <sys/param.h>
# endif

# if !defined PATH_MAX && defined MAXPATHLEN
#  define PATH_MAX MAXPATHLEN
# endif


#endif /* _PATHMAX_H */
