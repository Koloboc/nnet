
/* euidaccess -- check if effective user id can access file

   Copyright (C) 1990-1991, 1995, 1998, 2000, 2003-2006, 2008-2024 Free
   Software Foundation, Inc.

   This file is part of the GNU C Library.

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

/* Written by David MacKenzie and Torbjorn Granlund.
   Adapted for GNU C library by Roland McGrath.  */

#ifndef __EUIDACCESS_H__
#define __EUIDACCESS_H__
# include <system.h>

#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

int group_member(gid_t g);
int euidaccess (const char *file, int mode);

#endif

