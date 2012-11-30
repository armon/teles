-module(teles_conn_manager).
-export([start_handler/1]).

% Starts a new handler process for the given socket
% Return {ok, pid()}.
start_handler(Socket) ->
    % Create the spec
    HandlerSpec = {Socket,
                {teles_handler, start_link, [Socket]},
                temporary, 5000, worker, dynamic},

    % Start the child
    supervisor:start_child(teles_conn_manager_sup, HandlerSpec).

