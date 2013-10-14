erldaemon
=========

erldaemon is a small helper program to create well-behaved UNIX daemons with
the Erlang emulator. In particular erldaemon provides the following
capabilities which are hard to achieve with plain Erlang:

* Detach from console only after the daemon was initialized successfully
* Return proper exit code if initialization fails
* Maintain a PID-file
* Orderly shutdown when receiving SIGTERM
* Log rotation upon receiving SIGHUP

Building
--------

To build erldaemon you will need [rebar](https://github.com/rebar/rebar) and
obviously Erlang.

    $ rebar compile

As erldaemon is an OTP application you have to put it in the Erlang code path.
The erldaemon executable can be found in the ``priv`` directory after
compilation.

Usage
-----

erldaemon is pretty much self contained and doesn't need any specific changes
to your Erlang application(s). Typically you will just start Erlang through
erldaemon which redirects stdout to some log file and creates the PID file:

    $ cd erldaemon
    $ ./priv/erldaemon -l /var/log/myapp.log -p /var/run/myapp.pid -- erl +Bd -noinput \
      -pa "$PWD/ebin" -s myapp -s erldaemon

* Erlang must be started with ``+Bd -noinput`` flags. This disables the break
  handler (erldaemon takes care of this) and disables the shell because Erlang
  will have no stdin when run through erldaemon.
* Your application(s) should be started as ``permanent`` application, that is
  with ``application:start(myapp, permanent)``. This makes sure that the Erlang
  emulator dies if the application cannot be started successfully.
* erldaemon must be started as the last application. The erldaemon application
  makes the detach from the console and listens to SIGTERM shutdown requests.

Use ``erldaemon -h`` to get an overview about the available command line
options.
