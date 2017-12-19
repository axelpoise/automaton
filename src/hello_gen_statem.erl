%%%-------------------------------------------------------------------
%%% @author Dave Kuhlman
%%% @copyright (C) 2017, Dave Kuhlman
%%% @doc
%%%
%%% @end
%%% Created : 2017-03-10 09:27:01.772079
%%%-------------------------------------------------------------------
-module(characters01).

-behaviour(gen_statem).

% gen_statem callbacks
-export([
         init/1,
         format_status/2,
         %handle_event/4,
         terminate/3,
         code_change/4,
         callback_mode/0
        ]).

% State functions
-export([
         start/3,
         in_quotes/3,
         not_in_quotes/3,
         finish/3
        ]).

% External/client API
-export([
         start/0,
         feed/1,
         stop/0
        ]).

-define(SERVER, ?MODULE).

-record(state, {accum}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    io:fwrite("(start_link) starting~n", []),
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------

%
% Start the FSM.
start() ->
    start_link().

%%--------------------------------------------------------------------

%
% Feed a list of characters to the FSM.
% Return the result.
feed(Chars) ->
    feed_chars(Chars).

%%--------------------------------------------------------------------

%
% Stop the FSM.
stop() ->
    gen_statem:stop(?SERVER).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {CallbackMode, StateName, State} |
%%                     {CallbackMode, StateName, State, Actions} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, start, #state{accum=[]}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
%%
%% @spec format_status(Opt, [PDict, StateName, State]) -> term()
%% @end
%%--------------------------------------------------------------------
format_status(_Opt, [_PDict, _StateName, _State]) ->
    Status = some_term,
    Status.

%%--------------------------------------------------------------------

%% Return the callback mode.
callback_mode() ->
    % Use a single handler for all states.
    %handle_event_function.
    % Define a separate handler function for each state.
    state_functions.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name.  If callback_mode is statefunctions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------

%~ state_name(_EventType, _EventContent, State) ->
%~     NextStateName = next_state,
%~     {next_state, NextStateName, State}.

start({call, From}, EventContent, #state{accum=Accum}=State) ->
    {NextStateName, State1, Char2} =
    case EventContent of
        {char, 34} ->
            {in_quotes, State, "\""};
        {char, Char} ->
            {not_in_quotes, State#state{accum=[Char | Accum]}, Char};
        'end' ->
            {finish, State, nil}
    end,
    {next_state, NextStateName, State1, [{reply, From, Char2}]}.

%
% Handle a character when we are inside of quotes.
in_quotes({call, From}, EventContent, #state{accum=Accum}=State) ->
    {NextStateName, State1, Char2} =
    case EventContent of
        {char, 34} ->
            {not_in_quotes, State, "\""};
        {char, Char} ->
            Char1 = string:to_upper(Char),
            {in_quotes, State#state{accum=[Char1 | Accum]}, Char1};
        'end' ->
            {finish, State, {error, "missing quote"}}
    end,
    {next_state, NextStateName, State1, [{reply, From, Char2}]}.

%
% Handle a character when we are outside of quotes.
not_in_quotes({call, From}, EventContent, #state{accum=Accum}=State) ->
    {NextStateName, State1, Char2} =
    case EventContent of
        {char, 34} ->
            {in_quotes, State, "\""};
        {char, Char} ->
            Char1 = Char,
            {not_in_quotes, State#state{accum=[Char1 | Accum]}, Char1};
        'end' ->
            Accum1 = lists:reverse(Accum),
            {finish, State, {ok, Accum1}}
    end,
    {next_state, NextStateName, State1, [{reply, From, Char2}]}.

%
% Re-initialize the FSM: set the accumulator to empty and next state to 'start'.
finish({call, From}, _EventContent, State) ->
    {next_state, start, State#state{accum=[]}, [{reply, From, nil}]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%--------------------------------------------------------------------
%~ handle_event(_EventType, _EventContent, _StateName, State) ->
%~     NextStateName = the_next_state_name,
%~     {next_state, NextStateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    io:fwrite("(terminate) stopping~n", []),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%
% Feed each character to the FSM, one at a time.
% Then, capture and return the result.
% Finally, force the FSM to the 'finish' state so that it re-initializes itself.
feed_chars([]) ->
    Reply = gen_statem:call(?SERVER, 'end'),
    gen_statem:call(?SERVER, nil),
    Reply;
feed_chars([Char | MoreChars]) ->
    gen_statem:call(?SERVER, {char, Char}),
    feed_chars(MoreChars).