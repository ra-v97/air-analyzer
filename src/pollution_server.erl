%%%-------------------------------------------------------------------
%%% @author rafstach
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. May 2018 00:17
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("rafstach").

%% API
-export([init/0,start/0,stop/0,addStation/2,addValue/4,removeValue/3,getOneValue/3,getStationMean/2,getDailyMean/2,getAirQualityIndex/3]).


-record(monitor,{stations =#{},nameKeys=#{},cordKeys=#{}}).

% -record(defaultNormMap,{norm = #{"PM10" => 75, "PM2.5" => 60}}).

%% API functions
start() ->
  register(pollutionServer, spawn(fun() -> init() end)).

stop() ->
  pollutionServer ! {stop, self()},
  ok.


addStation(Name,{Latitude,Longitude}) ->
  pollutionServer ! {addStation,{Name,Latitude,Longitude},self()},
  receive
    {responseM,{ok,Msg}} ->
      %io:format("Client: ~p\n",[Msg]),
      Msg;
    {responseM,{I,_}} ->
      %io:format("Client: ~p\n",[I]),
      I
  end.

addValue(Id,Date,Type,Value) ->
  pollutionServer ! {addValue,{Id,Date,Type,Value},self()},
  receive
    {responseM,{ok,Msg}} ->
      %io:format("Client: ~p\n",[Msg]),
      Msg;
    {responseM,{I,_}} ->
      %io:format("Client: ~p\n",[I]),
      I
  end.

removeValue(Id,Date,Type) ->
  pollutionServer ! {removeValue,{Id,Date,Type},self()},
  receive
    {responseM,{ok,Msg}} ->
      %io:format("Client: ~p\n",[Msg]),
      Msg;
    {responseM,{_,Msg}} ->
      %io:format("Client: ~p\n",[Msg]),
      Msg
  end.

getOneValue(Name,Date,Type) ->
  pollutionServer ! {getOneValue,{Name,Date,Type},self()},
  receive
    {responseM,stationDoesNotExist} ->
      %io:format("Client: ~p\n",[Msg]),
      stationDoesNotExist;

    {responseV,Msg} ->
      io:format("Client: Value{~s , ~s , ~p} >> ~p\n",[Name,Type, Date,Msg]),
      Msg
  end.

getStationMean(Name,Type) ->
  pollutionServer ! {getStationMean,{Name,Type},self()},
  receive
    {responseM,stationDoesNotExist} ->
      %io:format("Client: ~p\n",[Msg]),
      stationDoesNotExist;

    {responseV,Msg} ->
      io:format("Client: Station mean{~s , ~s} >> ~p\n",[Name,Type,Msg]),
      Msg
  end.

getDailyMean(Type,Date) ->
  pollutionServer ! {getDailyMean,{Type,Date},self()},
  receive
    {responseV,Msg} ->
      io:format("Client: Daily mean{~s , ~p} >> ~p\n",[Type,Date,Msg]),
      Msg
  end.

getAirQualityIndex(Id,{Date,Hour},NormMap) ->
  pollutionServer ! {getAirQualityIndex,{Id,Date,Hour,NormMap},self()},
  receive
    {responseV,Msg} ->
      io:format("Client: AirQuality index{~p} >> ~p\n",[Date,Msg]),
      Msg
  end.


%% Private functions
init() ->
  Monitor = #monitor{},
  loop(Monitor).

loop(Monitor) ->
  receive

    {addStation,{Name,Latitude,Longitude},ClientPid} ->
      {I,NewMonitor} = addStationPrivate(Monitor,Name,{Latitude,Longitude}),
      ClientPid ! {responseM,{I,NewMonitor}},
      loop(NewMonitor);

    {addValue,{Id,Date,Type,Value}, ClientPid} ->
      {I,NewMonitor} =  addValuePrivate(Monitor,Id,Date,Type,Value),
      ClientPid ! {responseM,{I,NewMonitor}},
      loop(NewMonitor);

    {removeValue, {Id ,Date,Type},ClientPid} ->
      {I,NewMonitor} = removeValuePrivate(Monitor,Id,Date,Type),
      ClientPid ! {responseM,{I,NewMonitor}},
      loop(NewMonitor);

    {getOneValue,{Name,Date,Type}, ClientPid} ->
      Value = getOneValuePrivate(Monitor,Name,Date,Type),
      ClientPid ! {responseV,Value},
      loop(Monitor);

    {getStationMean,{Name,Type}, ClientPid} ->
      Value = getStationMeanPrivate(Monitor,Name,Type),
      ClientPid ! {responseV,Value},
      loop(Monitor);

    {getDailyMean,{Type,Date}, ClientPid} ->
      Value = getDailyMeanPrivate(Monitor,Type,Date),
      ClientPid ! {responseV,Value},
      loop(Monitor);

    {getAirQualityIndex,{Id,Date,Hour,NormMap}, ClientPid} ->
      Value = getAirQualityIndexPrivate(Monitor,Id,{Date,Hour},NormMap),
      ClientPid ! {responseV,Value},
      loop(Monitor);

    {stop, ClientPid} -> terminate(ClientPid)

  end.

terminate(ClientPid) -> io:format("Server: I was stopped by ~p. . .\n",[ClientPid]),ok.


isValidStationPrivate(Monitor,Name,{Latitude,Longitude}) ->
    maps:is_key(Name,Monitor#monitor.nameKeys)
    or
    maps:is_key({Latitude,Longitude},Monitor#monitor.cordKeys).

addStationPrivate(Monitor,Name,{Latitude,Longitude}) ->
  case isValidStationPrivate(Monitor,Name,{Latitude,Longitude}) of
    true -> {stationAlreadyExists,Monitor} ;
    false -> NewKey = lists:foldl(fun (X, Acc) -> (X*(Acc+2) rem 10000) end,1,lists:append(Name,float_to_list(Latitude))),
             #monitor{stations = S , nameKeys = N, cordKeys =K} = Monitor,
              {ok, Monitor#monitor{  nameKeys = N#{Name => NewKey},
                cordKeys = K#{{Latitude,Longitude} => NewKey},
                stations = S#{NewKey => #{}}}}
  end.

addValuePrivate(Monitor,{Latitude,Longitude},Date,Type,Value) ->
  case maps:is_key({Latitude,Longitude},Monitor#monitor.cordKeys) of
    false -> {stationDoesNotExist,Monitor};
    true -> Key = maps:get({Latitude,Longitude},Monitor#monitor.cordKeys),
      #monitor{stations = S } = Monitor,
      Measurements = maps:get(Key,S),
      case maps:is_key({Type,Date},Measurements) of
        true -> {measurementAlreadyExists,Monitor};
        false ->{ok,Monitor#monitor{stations = S#{Key := maps:put({Type,Date},Value,Measurements)}}}
      end
  end;
addValuePrivate(Monitor,Name,Date,Type,Value) ->
  case maps:is_key(Name,Monitor#monitor.nameKeys) of
    false -> {stationDoesNotExist,Monitor};
    true -> Key = maps:get(Name,Monitor#monitor.nameKeys),
      #monitor{stations = S } = Monitor,
      Measurements = maps:get(Key,S),
      case maps:is_key({Type,Date},Measurements) of
        true -> {measurementAlreadyExists,Monitor};
        false -> {ok,Monitor#monitor{stations = S#{Key := maps:put({Type,Date},Value,Measurements)}}}
      end
  end.



removeValuePrivate(Monitor,{Latitude,Longitude},Date,Type) ->
  case maps:is_key({Latitude,Longitude},Monitor#monitor.cordKeys) of
    false -> {stationDoesNotExist,Monitor};
    true -> Key = maps:get({Latitude,Longitude},Monitor#monitor.cordKeys),
            #monitor{stations = S } = Monitor,
            Measurements = maps:get(Key,S),
            {ok,Monitor#monitor{stations = S#{Key := maps:remove({Type,Date},Measurements)}}}
  end ;
removeValuePrivate(Monitor,Name,Date,Type) ->
  case maps:is_key(Name,Monitor#monitor.nameKeys) of
    false -> {stationDoesNotExist,Monitor};
    true -> Key = maps:get(Name,Monitor#monitor.nameKeys),
            #monitor{stations = S } = Monitor,
            Measurements = maps:get(Key,S),
            {ok,Monitor#monitor{stations = S#{Key := maps:remove({Type,Date},Measurements)}}}
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


