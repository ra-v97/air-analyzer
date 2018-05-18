%%%-------------------------------------------------------------------
%%% @author rafstach
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. May 2018 13:57
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("rafstach").

-behaviour(gen_server).

%% API
-export([start_link/1,stop/0,addStation/2,addValue/4,removeValue/3,getOneValue/3,getStationMean/2,getDailyMean/2,getAirQualityIndex/3,crash/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).


-record(monitor,{stations =#{},nameKeys=#{},cordKeys=#{}}).


%%%===================================================================
%%% API
%%%===================================================================


start_link(Data) ->
  gen_server:start_link({local, pollution_gen_server}, ?MODULE, Data, []).

stop() ->
  gen_server:cast(pollution_gen_server,stop).

crash() ->
  gen_server:cast(pollution_gen_server,crash).

addStation(Name,{Latitude,Longitude}) ->
  gen_server:call(pollution_gen_server,{addStation,{Name,{Latitude,Longitude}}}).

addValue(Id,Date,Type,Value) ->
  gen_server:call(pollution_gen_server,{addValue,{Id,Date,Type,Value}}).

removeValue(Id,Date,Type) ->
  gen_server:call(pollution_gen_server,{removeValue,{Id,Date,Type}}).

getOneValue(Name,Date,Type) ->
  gen_server:call(pollution_gen_server,{getOneValue,{Name,Date,Type}}).

getStationMean(Name,Type) ->
  gen_server:call(pollution_gen_server,{getStationMean,{Name,Type}}).

getDailyMean(Type,Date) ->
  gen_server:call(pollution_gen_server,{getDailyMean,{Type,Date}}).

getAirQualityIndex(Id,{Date,Hour},NormMap) ->
  gen_server:call(pollution_gen_server,{getAirQualityIndex,{Id,{Date,Hour},NormMap}}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #monitor{}} | {ok, State :: #monitor{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init(Data) ->
  erlang:send_after(10000, self(), {save,Data}),
  [H|[]] = ets:lookup(Data,monitor),
  {ok, H}.
  %{ok, #monitor{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #monitor{}) ->
  {reply, Reply :: term(), NewState :: #monitor{}} |
  {reply, Reply :: term(), NewState :: #monitor{}, timeout() | hibernate} |
  {noreply, NewState :: #monitor{}} |
  {noreply, NewState :: #monitor{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #monitor{}} |
  {stop, Reason :: term(), NewState :: #monitor{}}).

handle_call({addStation,{Name,{Latitude,Longitude}}}, _From, State) ->
  NewState = addStationPrivate(State,Name,{Latitude,Longitude}),
  case NewState of
    stationAlreadyExists -> {reply, stationAlreadyExists, State} ;
    _ -> {reply, NewState, NewState}
  end;

handle_call({addValue,{Id,Date,Type,Value}}, _From, State) ->
  NewState = addValuePrivate(State,Id,Date,Type,Value),
  case NewState of
    stationDoesNotExist -> {reply, stationDoesNotExist, State} ;
    measurementAlreadyExists -> {reply, measurementAlreadyExists, State} ;
    _ -> {reply, NewState, NewState}
  end;

handle_call({removeValue,{Id,Date,Type}}, _From, State) ->
  NewState = removeValuePrivate(State,Id,Date,Type),
  case NewState of
    stationDoesNotExist -> {reply, stationDoesNotExist, State} ;
    _ -> {reply, NewState, NewState}
  end;

handle_call({getOneValue,{Name,Date,Type}}, _From, State) ->
  Answer = getOneValuePrivate(State,Name,Date,Type),
  {reply, Answer, State};

handle_call({getStationMean,{Name,Type}}, _From, State) ->
  Answer =getStationMeanPrivate(State,Name,Type),
  {reply, Answer, State};

handle_call({getDailyMean,{Type,Date}}, _From, State) ->
  Answer =getDailyMeanPrivate(State,Type,Date),
  {reply, Answer, State};

handle_call({getAirQualityIndex,{Id,{Date,Hour},NormMap}}, _From, State) ->
  Answer =getAirQualityIndexPrivate(State,Id,{Date,Hour},NormMap),
  {reply, Answer, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #monitor{}) ->
  {noreply, NewState :: #monitor{}} |
  {noreply, NewState :: #monitor{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #monitor{}}).
handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(crash, State) ->
  crashPrivate(),
  {noreply, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #monitor{}) ->
  {noreply, NewState :: #monitor{}} |
  {noreply, NewState :: #monitor{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #monitor{}}).
handle_info({save,Data}, State) ->
  ets:insert(Data,State),
  erlang:send_after(1000, self(), {save,Data}),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #monitor{}) -> term()).
terminate(_Reason, _State) ->
  io:format("Server: I was stopped "),
  _Reason.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #monitor{},
    Extra :: term()) ->
  {ok, NewState :: #monitor{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

crashPrivate() -> 1/0.

isValidStationPrivate(Monitor,Name,{Latitude,Longitude}) ->
  maps:is_key(Name,Monitor#monitor.nameKeys)
    or
    maps:is_key({Latitude,Longitude},Monitor#monitor.cordKeys).

addStationPrivate(Monitor,Name,{Latitude,Longitude}) ->
  case isValidStationPrivate(Monitor,Name,{Latitude,Longitude}) of
    true -> stationAlreadyExists ;
    false -> NewKey = lists:foldl(fun (X, Acc) -> (X*(Acc+2) rem 10000) end,1,lists:append(Name,float_to_list(Latitude))),
      #monitor{stations = S , nameKeys = N, cordKeys =K} = Monitor,
      Monitor#monitor{  nameKeys = N#{Name => NewKey},
        cordKeys = K#{{Latitude,Longitude} => NewKey},
        stations = S#{NewKey => #{}}}
  end.

addValuePrivate(Monitor,{Latitude,Longitude},Date,Type,Value) ->
  case maps:is_key({Latitude,Longitude},Monitor#monitor.cordKeys) of
    false -> stationDoesNotExist;
    true -> Key = maps:get({Latitude,Longitude},Monitor#monitor.cordKeys),
      #monitor{stations = S } = Monitor,
      Measurements = maps:get(Key,S),
      case maps:is_key({Type,Date},Measurements) of
        true -> measurementAlreadyExists;
        false ->Monitor#monitor{stations = S#{Key := maps:put({Type,Date},Value,Measurements)}}
      end
  end;
addValuePrivate(Monitor,Name,Date,Type,Value) ->
  case maps:is_key(Name,Monitor#monitor.nameKeys) of
    false -> stationDoesNotExist;
    true -> Key = maps:get(Name,Monitor#monitor.nameKeys),
      #monitor{stations = S } = Monitor,
      Measurements = maps:get(Key,S),
      case maps:is_key({Type,Date},Measurements) of
        true -> measurementAlreadyExists;
        false -> Monitor#monitor{stations = S#{Key := maps:put({Type,Date},Value,Measurements)}}
      end
  end.



removeValuePrivate(Monitor,{Latitude,Longitude},Date,Type) ->
  case maps:is_key({Latitude,Longitude},Monitor#monitor.cordKeys) of
    false -> stationDoesNotExist;
    true -> Key = maps:get({Latitude,Longitude},Monitor#monitor.cordKeys),
      #monitor{stations = S } = Monitor,
      Measurements = maps:get(Key,S),
      Monitor#monitor{stations = S#{Key := maps:remove({Type,Date},Measurements)}}
  end ;
removeValuePrivate(Monitor,Name,Date,Type) ->
  case maps:is_key(Name,Monitor#monitor.nameKeys) of
    false -> stationDoesNotExist;
    true -> Key = maps:get(Name,Monitor#monitor.nameKeys),
      #monitor{stations = S } = Monitor,
      Measurements = maps:get(Key,S),
      Monitor#monitor{stations = S#{Key := maps:remove({Type,Date},Measurements)}}
  end.



getOneValuePrivate(Monitor,Name,Date,Type) ->
  case maps:is_key(Name,Monitor#monitor.nameKeys) of
    false -> stationDoesNotExist;
    true -> Key = maps:get(Name,Monitor#monitor.nameKeys),
      Station = maps:get(Key,Monitor#monitor.stations),
      maps:get({Type,Date},Station,noResult)
  end.


getStationMeanPrivate(Monitor,Name,Type) ->
  case maps:is_key(Name,Monitor#monitor.nameKeys) of
    false -> stationDoesNotExist;
    true -> Key = maps:get(Name,Monitor#monitor.nameKeys),
      Station = maps:get(Key,Monitor#monitor.stations),
      Values = maps:values(maps:filter(fun({T,_},_) -> T == Type end , Station)),
      case Values of
        [] -> noResult;
        _ -> lists:foldl(fun (X,Acc) -> X+Acc end,0,Values)/length(Values)
      end
  end.

getDailyMeanPrivate(Monitor,Type,Date) ->
  Stations = Monitor#monitor.stations,
  Values = lists:foldl(
    fun (Measurements,Acc) ->
      lists:merge(maps:values(maps:filter(fun({T,{D,_}},_) -> (T == Type) and (D==Date) end,Measurements)),Acc) end,
    [],
    maps:values(Stations)),
  case Values of
    [] -> noResult;
    _ -> lists:sum(Values)/length(Values)
  end.

getAirQualityIndexPrivate(Monitor,{Latitude,Longitude},{Date,Hour},NormMap) when is_map(NormMap) ->
  G = isValidStationPrivate(Monitor,"",{Latitude,Longitude}),
  if
    G ->
      Key = maps:get({Latitude,Longitude},Monitor#monitor.cordKeys),
      getAirQualityIndexPrivateBody(Monitor,Key,{Date,Hour},NormMap);
    true -> stationDoesNotExist
  end;
getAirQualityIndexPrivate(Monitor,Name,{Date,Hour},NormMap) when is_map(NormMap) ->
  G = isValidStationPrivate(Monitor,Name,{"N","N"}),
  if
    G ->
      Key = maps:get(Name,Monitor#monitor.nameKeys),
      getAirQualityIndexPrivateBody(Monitor,Key,{Date,Hour},NormMap);
    true -> stationDoesNotExist
  end.

getAirQualityIndexPrivateBody(Monitor,Key,{Date,Hour},NormMap) ->
  Measurements = maps:get(Key,Monitor#monitor.stations),
  Values = maps:values(maps:map(fun ({T,_},V) -> {T,V} end, maps:filter(fun ({_,{D,{H,_,_}}},_) -> (H ==Hour) and (D==Date) end,Measurements))),
  F1 = fun(X,Map) -> case maps:is_key(X,Map) of true -> maps:get(X,Map); false -> [] end end,
  S1=lists:foldl(fun({T,V},Acc) -> Acc#{T => [V|F1(T,Acc)]} end,#{},Values),
  try  maps:values(maps:map(fun(V,K) -> 100*(lists:sum(K)/length(K))/maps:get(V,NormMap) end,S1)) of
    [] -> noResult;
    S2 -> lists:max(S2)
  catch
    error:_ -> normError
  end.