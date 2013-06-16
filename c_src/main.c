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
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "daemon.h"
#include "util.h"

static int sig_child_died = 0;
static int sig_terminate = 0;

static void sigchld_handler(int signum)
{
	sig_child_died = 1;
}

static void sigterm_handler(int signum)
{
	sig_terminate = 1;
}

static pid_t fork_daemon(struct config *cfg, int *result_pipe_rd, int *control_pipe_wr)
{
	struct sigaction action;
	int result_pipe[2];
	int control_pipe[2];
	pid_t pid;

	memset(&action, 0, sizeof(action));

	action.sa_handler = sigchld_handler;
	sigaction(SIGCHLD, &action, NULL);

	action.sa_handler = sigterm_handler;
	sigaction(SIGHUP, &action, NULL);
	sigaction(SIGINT, &action, NULL);
	sigaction(SIGQUIT, &action, NULL);
	sigaction(SIGTERM, &action, NULL);

	if (pipe(result_pipe) < 0) {
		perror("Cannot create daemon result pipe");
		return -1;
	}
	if (pipe(control_pipe) < 0) {
		perror("Cannot create daemon control pipe");
		return -1;
	}

	/*
	 * Fork the first time to create a new child which will become a new
	 * session leader.  As parent we'll just return from here to enter the main
	 * loop...
	 */
	if ((pid = fork()) < 0) {
		perror("fork failed");
		return pid;
	}
	if (pid) {
		*result_pipe_rd = result_pipe[0];
		close(result_pipe[1]);
		*control_pipe_wr = control_pipe[1];
		close(control_pipe[0]);
		return pid;
	}

	/* reset signals to default handlers for the moment */
	signal(SIGCHLD, SIG_DFL);
	signal(SIGHUP,  SIG_DFL);
	signal(SIGINT,  SIG_DFL);
	signal(SIGQUIT, SIG_DFL);
	signal(SIGTERM, SIG_DFL);
	signal(SIGHUP,  SIG_IGN);

	close(result_pipe[0]);  /* close read end of result pipe */
	close(control_pipe[1]); /* close write end of control pipe */

	if (setsid() < 0) {
		perror("setsid");
		exit(1);
	}

	chdir("/");
	umask(0);
	exit(daemon_main(cfg, result_pipe[1], control_pipe[0]));
}

/*
 * Fork a child process to execute the daemon logic
 *
 * The original process will stay on the terminal until the erlang application
 * signals to detach from the tty.
 */
static int start_daemon(struct config *cfg)
{
	int result_pipe, control_pipe;
	pid_t daemon;

	daemon = fork_daemon(cfg, &result_pipe, &control_pipe);
	if (daemon < 0)
		return 1;

	for (;;) {
		uint8_t result = 0;
		int len = read(result_pipe, &result, sizeof(result));
		if (len < 0 && errno != EINTR) {
			perror("Failed reading result");
			return 1;
		}

		if (sig_child_died) {
			/* daemon has died before us! */
			int waitres;
			do {
				waitres = waitpid(daemon, NULL, WNOHANG);
			} while (waitres < 0 && errno == EINTR);

			if (waitres < 0) {
				perror("waitpid");
				return 1;
			} else if (waitres == daemon)
				return 1;

			sig_child_died = 0;
		}

		if (sig_terminate) {
			/* Tell daemon to exit prematurely */
			write(control_pipe, &result, sizeof(result));
			sig_terminate = 0;
		}

		if (len >= 1)
			return result; /* exit with the status as told by the daemon */
		else if (len == 0)
			return 1; /* daemon died */
	}
}

static void usage(FILE *stream, int exhaustive)
{
	fprintf(stream, "Usage: erldaemon [options] -- <erlang command line>\n");

	if (exhaustive) {
		fprintf(stream, "\nOptions:\n"
		                "  -f            run in the foreground\n"
		                "  -h            show this help\n"
		                "  -l FILE       log stdout of erlang to FILE\n"
		                "  -n NAME       use NAME as syslog identifier\n"
		                "  -p FILE       create FILE with process id\n"
//		                "  -v            show version\n"
		                );
	}
}

/*
static void version(void)
{
	printf("Version: 1.0\n");
}
*/

/*
 * main     - wait for detach result from daemon
 * 1st fork - daemon process, start erlang, handle log
 * 2nd fork - exec's erl, control pipe to daemon process
 *
 * main --- control_pipe --> daemon --- control_pipe --> erl
 *      <-- result_pipe  ---        <-- result_pipe  ---
 *                                  <--     stdout   ---
 */
int main(int argc, char* const argv[])
{
	struct config cfg;
	int opt;

	signal(SIGPIPE, SIG_IGN);

	/* parse options */
	memset(&cfg, 0, sizeof(cfg));
	cfg.grace_time = 10;
	cfg.log_name = "erldaemon";
	while ((opt = getopt(argc, argv, "fg:hl:n:p:")) != -1) {
		switch (opt) {
			case 'f': cfg.foreground = 1; break;
			case 'g': cfg.grace_time = atol(optarg); break;
			case 'h': usage(stdout, 1); exit(0); break;
			case 'l': cfg.log_file = abspath(optarg); break;
			case 'n': cfg.log_name = strdup(optarg); break;
			case 'p': cfg.pid_file = abspath(optarg); break;
//			case 'v': version(); exit(0); break;
			default: usage(stderr, 1); exit(2); break;
		}
	}

	if (optind >= argc) {
		usage(stderr, 0);
		exit(2);
	}

	cfg.argv = argv + optind;

	if (cfg.foreground)
		return daemon_main(&cfg, -1, -1);
	else
		return start_daemon(&cfg);
}
