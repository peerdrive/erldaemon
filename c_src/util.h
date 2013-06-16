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
#ifndef UTIL_H
#define UTIL_H

#include <unistd.h>

char* abspath(char *path);
ssize_t write_safe(int fd, const void *_buf, size_t count);
ssize_t read_safe(int fd, void *_buf, size_t count);

#endif
