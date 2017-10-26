-module(council_member).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-type verbosity() :: quiet | moderate | talkative | loudspeaker.

-record(state, {name :: string(),
                members :: string(),
                verbosity :: verbosity(),
                listen :: port(),
                accept :: port(),
                connected :: port(),
                timer :: reference()
                }).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Name, Members) ->
    gen_server:start_link({via, gproc, {n,l,Name}},
                          ?MODULE, {Name, Members}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({Name, Members}) ->
    Rand = rand:uniform(10),
    Verbosity = if Rand =< 2 -> quiet
                 ; Rand =< 6 -> moderate
                 ; Rand =< 9 -> talkative
                 ; Rand =:= 10 -> loudspeaker
                end,
    %% Start the TCP server, and let the OS pick a port
    {Listen, Accept, Connected} = server(),
    %% Register port info in gproc under this name.
    put_port(Name, Connected),
    {ok, #state{name=Name,
                members=list_to_tuple(Members),
                verbosity=Verbosity,
                listen=Listen,
                accept=Accept,
                connected=Connected,
                timer=message_timer(Verbosity)}}.

handle_call(Call, _From, S=#state{name=Name}) ->
    error_logger:warning_report({unexpected_call, Name, Call}),
    {noreply, S}.

handle_cast(Cast, S=#state{name=Name}) ->
    error_logger:warning_report({unexpected_cast, Name, Cast}),
    {noreply, S}.

handle_info(speak, S=#state{name=Name, members=Members, verbosity=Verbosity}) ->
    Message = message(Verbosity),
    Target = element(rand:uniform(tuple_size(Members)), Members),
    case rand:uniform(2) of
        1 -> % message
            gproc:send({n,l,Target}, {msg, Name, Message});
        2 -> % packet
            BinName = iolist_to_binary(Name),
            gen_tcp:send(get_port(Target),
                         <<(byte_size(BinName)):8, BinName/binary,
                           (byte_size(Message)):32, Message/binary>>)
    end,
    {noreply, S#state{timer=message_timer(Verbosity)}};
handle_info({tcp, Port, _Data}, S=#state{accept=Port}) ->
    {noreply, S};
handle_info({msg, _From, _}, S=#state{}) ->
    {noreply, S};
handle_info(Info, S=#state{name=Name}) ->
    error_logger:warning_report({unexpected_info, Name, Info}),
    {noreply, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Spawn an always-active TCP server, auto-connect to it, and provide
%% all required ports as a return value.
-spec server() -> {Listen::port(), Accept::port(), Connected::port()}.
server() ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active,true}]),
    {ok, Port} = inet:port(Listen),
    Self = self(),
    Pid = spawn_link(fun() -> connect(Self, Port) end),
    {ok, Accept} = gen_tcp:accept(Listen),
    receive
        {Pid, Connected} ->
            {Listen, Accept, Connected}
    end.

-spec connect(Parent::pid(), PortNum::pos_integer()) -> ok.
connect(Parent, Port) ->
    {ok, Connect} = gen_tcp:connect({127,0,0,1}, Port, [{active, false}]),
    ok = gen_tcp:controlling_process(Connect, Parent),
    Parent ! {self(), Connect},
    ok.

%% Saves the port as a property in gproc, so that it can be fetched
%% later if the name is known.
-spec put_port(Name::term(), Port::term()) -> ok.
put_port(Name, Port) ->
    gproc:reg({p,l,Name}, Port).

%% Gets the port from gproc
-spec get_port(Name::term()) -> Port::term().
get_port(Name) ->
    [{_Pid, Port}|_] = gproc:lookup_values({p,l,Name}),
    Port.

%% Starts a timer according to verbosity level.
-spec message_timer(verbosity()) -> reference().
message_timer(Level) ->
    erlang:send_after(interval(Level), self(), speak).

%% Shows a message according to verbosity
-spec message(verbosity()) -> binary().
message(Level) ->
    crypto:strong_rand_bytes(msg_size(Level)).

-spec interval(verbosity()) -> Milliseconds::non_neg_integer().
interval(quiet) -> timer:seconds(10);
interval(moderate) -> timer:seconds(5);
interval(talkative) -> timer:seconds(2);
interval(loudspeaker) -> 500.

-spec msg_size(verbosity()) -> Bytes::pos_integer().
msg_size(quiet) -> 10;
msg_size(moderate) -> 50;
msg_size(talkative) -> 100;
msg_size(loudspeaker) -> 1000.
