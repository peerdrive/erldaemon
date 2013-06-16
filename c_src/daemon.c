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
#include <fcntl.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/select.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <syslog.h>
#include <unistd.h>

#include "daemon.h"
#include "util.h"

struct state {
	struct config *cfg;

	unsigned int alive : 1;
	unsigned int detached : 1;
	unsigned int shutdown : 1;

	int result;
	pid_t erl_pid;
	int log_fd;

	int main_result_pipe;
	int main_control_pipe;
	int erl_result_pipe;
	int erl_control_pipe;
	int erl_stdout_pipe;

	struct timeval kill_timer;
};

#define SIG_CHILD_DIED (1 << 0)
#define SIG_TERMINATE  (1 << 1)
#define SIG_LOG_ROTATE (1 << 2)

static int pending_sigs = 0;

static int add_fd(int fd, fd_set *set, int nfds)
{
	if (fd > 0) {
		FD_SET(fd, set);
		return fd > nfds ? fd : nfds;
	} else
		return nfds;
}

static int check_fd(int fd, fd_set *set)
{
	return (fd > 0) ? (FD_ISSET(fd, set)) : 0;
}

static void log_error(struct state *s, char *message)
{
	if (!s->cfg->foreground && s->detached)
		syslog(LOG_ERR, "%s: %s", message, strerror(errno));
	else
		perror(message);
}

static void detach(struct state *s, uint8_t result)
{
	if (s->detached)
		return;

	if (!s->cfg->foreground)
		syslog(LOG_INFO, "ready");

	write_safe(s->main_result_pipe, &result, sizeof(result));
	s->detached = 1;
	s->result = result;

	/* close connection to original process */
	close(s->main_result_pipe);
	s->main_result_pipe = -1;
	close(s->main_control_pipe);
	s->main_control_pipe = -1;

	/* close standard files to detach completely */
	close(0);
	close(1);
	close(2);
}

static int log_open(struct state *s)
{
	if (s->cfg->log_file) {
		s->log_fd = open(s->cfg->log_file, O_WRONLY | O_CREAT | O_APPEND, 0640);
		if (s->log_fd < 0) {
			log_error(s, "Cannot open log file");
			return 1;
		}
		fcntl(s->log_fd, F_SETFD, FD_CLOEXEC);
	}

	return 0;
}

static int prepare(struct state *s)
{
	int pidfd, len;
	char tmp[16];

	/* open syslog */
	openlog(s->cfg->log_name, LOG_PID, LOG_DAEMON);

	/* open log file */
	if ((len = log_open(s)))
		return len;

	/* create pid file */
	if (s->cfg->pid_file) {
		pidfd = open(s->cfg->pid_file, O_WRONLY | O_CREAT | O_EXCL, 0644);
		if (pidfd < 0) {
			perror("Cannot create pid file");
			close(s->log_fd);
			return 1;
		}

		len = snprintf(tmp, sizeof(tmp), "%d\n", getpid());
		write_safe(pidfd, tmp, len);
		close(pidfd);
	}

	return 0;
}

static void log_close(struct state *s)
{
	if (s->log_fd > 0) {
		close(s->log_fd);
		s->log_fd = -1;
	}
}

static void finish(struct state *s)
{
	/* remove pid file */
	if (s->cfg->pid_file)
		unlink(s->cfg->pid_file);

	log_close(s);
	closelog(); /* close syslog */
}

static int spawn_erl(struct state *s)
{
	pid_t pid;
	int result_pipe[2];
	int control_pipe[2];
	int stdout_pipe[2];

	if (pipe(result_pipe) < 0) {
		perror("Cannot create erlang result pipe");
		return 1;
	}

	if (pipe(control_pipe) < 0) {
		perror("Cannot create erlang control pipe");
		return 1;
	}

	if (s->log_fd > 0) {
		if (pipe(stdout_pipe) < 0) {
			perror("Cannot create erlang stdout pipe");
			return 1;
		}
	} else {
		stdout_pipe[0] = -1;
		stdout_pipe[1] = s->cfg->foreground ? 1 : open("/dev/null", O_WRONLY);
	}

	pid = fork();
	if (pid < 0) {
		perror("Daemon fork failed");
		return 1;
	}

	if (pid) {
		setpgid(pid, pid); /* make sure erlang runs as own process group */
		s->erl_pid = pid;
		s->erl_result_pipe  = result_pipe[0];  close(result_pipe[1]);
		s->erl_control_pipe = control_pipe[1]; close(control_pipe[0]);
		s->erl_stdout_pipe  = stdout_pipe[0];  close(stdout_pipe[1]);
		return 0;
	} else {
		char tmp[16];
		int nullfd, saved_stderr;

		setpgid(0, 0); /* run as own process group */

		signal(SIGCHLD, SIG_DFL);
		signal(SIGPIPE, SIG_DFL);

		snprintf(tmp, sizeof(tmp), "%d,%d", control_pipe[0], result_pipe[1]);
		setenv("ERLDAEMON", tmp, 1);

		/* preserve stderr in case exec fails */
		saved_stderr = dup(2);
		if (fcntl(saved_stderr, F_SETFD, FD_CLOEXEC) < 0) {
			perror("fcntl");
			exit(1);
		}

		chdir("/");
		setsid();
		nullfd = open("/dev/null", O_RDONLY);
		dup2(nullfd, 0);
		close(nullfd);
		dup2(stdout_pipe[1], 1);
		dup2(stdout_pipe[1], 2);

		close(result_pipe[0]);
		close(control_pipe[1]);
		close(stdout_pipe[0]);

		execvp(s->cfg->argv[0], s->cfg->argv);

		/* it didn't work out... we're still here... */
		dup2(saved_stderr, 2);
		perror("Could not execute erlang");
		exit(1);
	}
}

static void shutdown_erl(struct state *s, int result)
{
	struct timeval tv, grace;
	char *cmd = "stop\n";

	/* remember we're on our way out... */
	if (s->shutdown)
		return;
	s->shutdown = 1;
	s->result = result;

	/* send shutdown request through control pipe */
	write_safe(s->erl_control_pipe, cmd, strlen(cmd));

	/* kill erlang vm if it doesn't exit after the grace period */
	gettimeofday(&tv, 0);
	grace.tv_sec = s->cfg->grace_time;
	grace.tv_usec = 0;
	timeradd(&tv, &grace, &s->kill_timer);
}

static void kill_erl(struct state *s)
{
	/* kill the whole erlang process group */
	kill(-s->erl_pid, SIGKILL);
}

static void handle_main_control(struct state *s)
{
	/* read command from pipe */
	uint8_t dummy;
	int len = read_safe(s->main_control_pipe, &dummy, sizeof(dummy));
	if (len < 1) {
		if (len < 0)
			log_error(s, "Broken control pipe");
		close(s->main_control_pipe);
		s->main_control_pipe = -1;
		return;
	}

	/* shutdown erlang process */
	shutdown_erl(s, 0);
}

static void handle_erl_result(struct state *s)
{
	int len;
	char result[16];

	len = read_safe(s->erl_result_pipe, result, sizeof(result)-1);
	if (len < 1) {
		if (len < 0)
			log_error(s, "Broken result pipe");
		close(s->erl_result_pipe);
		s->erl_result_pipe = -1;
		shutdown_erl(s, 1);
		return;
	}

	result[len] = 0;
	if (strncmp(result, "detach ", 7) == 0) {
		int code = strtol(&result[7], NULL, 0);
		detach(s, code);
	} else {
		printf("daemon: unknown result: %s\n", result);
	}
}

static void handle_erl_stdout(struct state *s)
{
	char buf[4096], *pbuf;
	int rd_len, wr_len;

	rd_len = read_safe(s->erl_stdout_pipe, buf, sizeof(buf));
	if (rd_len < 1) {
		if (rd_len < 0)
			log_error(s, "Broken stdout pipe");
		close(s->erl_stdout_pipe);
		s->erl_stdout_pipe = -1;
		shutdown_erl(s, 1);
		return;
	}

	if (s->cfg->foreground)
		write(1, buf, rd_len);

	pbuf = buf;
	while (rd_len) {
		wr_len = write_safe(s->log_fd, pbuf, rd_len);
		if (wr_len <= 0) {
			if (wr_len < 0)
				log_error(s, "Log file closed");
			close(s->log_fd);
			s->log_fd = -1;
			close(s->erl_stdout_pipe);
			s->erl_stdout_pipe = -1;
			shutdown_erl(s, 1);
			break;
		}

		pbuf += wr_len;
		rd_len -= wr_len;
	}
}

static void handle_signals(struct state *s, int pending)
{
	if (pending & SIG_CHILD_DIED) {
		int waitres;
		do {
			waitres = waitpid(s->erl_pid, NULL, WNOHANG);
		} while (waitres < 0 && errno == EINTR);

		if (waitres < 0) {
			log_error(s, "Daemon waitpid failed");
		} else {
			s->alive = 0;
			if (!s->shutdown)
				s->result = 1;
		}
	}

	if (pending & SIG_LOG_ROTATE) {
		log_close(s);
		if (log_open(s) == 0)
			syslog(LOG_INFO, "Log file successfully reopened");
	}

	if (pending & SIG_TERMINATE) {
		shutdown_erl(s, 0);
	}
}

static void signal_handler(int signum)
{
	uint8_t code = 0;

	switch (signum) {
		case SIGCHLD:
			code = SIG_CHILD_DIED;
			break;
		case SIGHUP:
			code = SIG_LOG_ROTATE;
			break;
		case SIGINT:
		case SIGQUIT:
		case SIGTERM:
			code = SIG_TERMINATE;
			break;
	}

	if (code)
		pending_sigs |= code;
}

static int signal_setup(struct state *s)
{
	struct sigaction action;
	sigset_t sigmask;

	memset(&action, 0, sizeof(action));
	action.sa_handler = signal_handler;

	sigemptyset(&sigmask);
	sigaddset(&sigmask, SIGCHLD);
	sigaddset(&sigmask, SIGHUP);
	sigaddset(&sigmask, SIGINT);
	sigaddset(&sigmask, SIGQUIT);
	sigaddset(&sigmask, SIGTERM);
	if (sigprocmask(SIG_BLOCK, &sigmask, NULL) == -1) {
		perror("sigprocmask");
		return 1;
	}

	sigaction(SIGCHLD, &action, NULL);
	sigaction(SIGHUP, &action, NULL);
	sigaction(SIGINT, &action, NULL);
	sigaction(SIGQUIT, &action, NULL);
	sigaction(SIGTERM, &action, NULL);

	return 0;
}

int daemon_main(struct config *cfg, int result_pipe, int control_pipe)
{
	struct state s;
	sigset_t empty_mask;

	if (result_pipe > 0) {
		if (fcntl(result_pipe, F_SETFD, FD_CLOEXEC) < 0) {
			perror("fcntl result_pipe");
			return 1;
		}
	}
	if (control_pipe > 0) {
		if (fcntl(control_pipe, F_SETFD, FD_CLOEXEC) < 0) {
			perror("fcntl control_pipe");
			return 1;
		}
	}

	memset(&s, 0, sizeof(s));
	s.cfg = cfg;
	s.alive = 1;
	s.detached = result_pipe < 0 ? 1 : 0;
	s.main_result_pipe = result_pipe;
	s.main_control_pipe = control_pipe;

	sigemptyset(&empty_mask);

	s.result = signal_setup(&s);
	if (s.result)
		goto done;

	s.result = prepare(&s);
	if (s.result)
		goto done;

	s.result = spawn_erl(&s);
	if (s.result)
		goto done;

	if (!s.cfg->foreground)
		syslog(LOG_INFO, "initializing");

	while (s.alive) {
		struct timeval now, tv;
		struct timespec ts, *tsp = NULL;
		fd_set rd_fds;
		int nfds = 0;

		FD_ZERO(&rd_fds);
		nfds = add_fd(s.erl_result_pipe, &rd_fds, nfds);
		nfds = add_fd(s.erl_stdout_pipe, &rd_fds, nfds);
		nfds = add_fd(s.main_control_pipe, &rd_fds, nfds);

		if (timerisset(&s.kill_timer)) {
			gettimeofday(&now, 0);
			if (timercmp(&s.kill_timer, &now, <))
				timerclear(&tv);
			else
				timersub(&s.kill_timer, &now, &tv);

			ts.tv_sec = tv.tv_sec;
			ts.tv_nsec = tv.tv_usec * 1000;
			tsp = &ts;
		}

		nfds = pselect(nfds + 1, &rd_fds, NULL, NULL, tsp, &empty_mask);
		if (nfds < 0 && errno != EINTR) {
			log_error(&s, "Daemon select failed");
			return 1;
		}

		if (s.shutdown && timerisset(&s.kill_timer)) {
			gettimeofday(&now, 0);
			if (timercmp(&s.kill_timer, &now, <)) {
				kill_erl(&s);
				timerclear(&s.kill_timer);
			}
		}

		if (pending_sigs) {
			handle_signals(&s, pending_sigs);
			pending_sigs = 0;
		}

		if (nfds <= 0)
			continue;

		if (check_fd(s.main_control_pipe, &rd_fds))
			handle_main_control(&s);
		if (check_fd(s.erl_result_pipe, &rd_fds))
			handle_erl_result(&s);
		if (check_fd(s.erl_stdout_pipe, &rd_fds))
			handle_erl_stdout(&s);
	}

done:
	detach(&s, s.result);
	if (!s.cfg->foreground)
		syslog(LOG_INFO, "exit %d", s.result);
	finish(&s);
	return s.result;
}
