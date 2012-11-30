-module(teles_acceptor).
-export([start_link/1, init/2]).

% Creates a new process to start the ETL process
start_link(Listen) ->
    proc_lib:start_link(?MODULE, init, [self(), Listen]).


% Acknowledge that we've started
init(Parent, Listen) ->
   % Acknowledge that we've started
    proc_lib:init_ack(Parent, {ok, self()}),

    % Enter the accept loop
    accept_loop(Listen).

% Loops accepting new clients
accept_loop(Listen) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} -> spawn_handler(Socket),
                        lager:debug("Accepted new client."),
                        accept_loop(Listen);

        {error, Err} -> lager:error("Error accepting client. ~p", [Err]),
                        accept_loop(Listen)
    end.

% Starts a new handler for a client
spawn_handler(Socket) ->
    % Create the child
    {ok, HandlerPid} = teles_conn_manager:start_child(Socket),

    % Hand over control of the socket
    ok = gen_tcp:controlling_process(Socket, HandlerPid),

    % Inform the pid to start
    gen_server:cast(HandlerPid, start).

