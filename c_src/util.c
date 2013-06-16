/*
 * This file is part of erldaemon.
 * Copyright (C) 2013  Jan Klötzke <jan AT kloetzke DOT net>
 *
 * Erldaemon is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option)
 * any later version.
 *
 * Erldaemon is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with erldaemon. If not, see <http://www.gnu.org/licenses/>.
 */

#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "util.h"

char* abspath(char *path)
{
	char *buf;

	if (strlen(path) < 1)
		return NULL;

	if (path[0] == '/')
		return strdup(path);

	buf = malloc(1024);
	getcwd(buf, 1024);
	strcat(buf, "/");
	strcat(buf, path);

	return buf;
}

ssize_t write_safe(int fd, const void *buf, size_t count)
{
	ssize_t ret;

	do {
		ret = write(fd, buf, count);
	} while (ret < 0 && errno == EINTR);

	return ret;
}

ssize_t read_safe(int fd, void *buf, size_t count)
{
	ssize_t ret;

	do {
		ret = read(fd, buf, count);
	} while (ret < 0 && errno == EINTR);

	return ret;
}

