%%%-------------------------------------------------------------------
%%% @author rafstach
%%% @copyright (C) 2018, AGH
%%% @doc
%%%
%%% @end
%%% Created : 20. Apr 2018 13:42
%%%-------------------------------------------------------------------
-module(pollution).
-author("rafstach").

%% API
-export([createMonitor/0,addStation/3,addValue/5,removeValue/4,getOneValue/4,getStationMean/3,getDailyMean/3,getAirQualityIndex/4]).


-record(monitor,{stations =#{},nameKeys=#{},cordKeys=#{}}).


createMonitor() -> #monitor{}.


isValidStation(Monitor,Name,{Latitude,Longitude}) ->
    maps:is_key(Name,Monitor#monitor.nameKeys)
      or
    maps:is_key({Latitude,Longitude},Monitor#monitor.cordKeys).

addStation(Monitor,Name,{Latitude,Longitude}) ->
  case isValidStation(Monitor,Name,{Latitude,Longitude}) of
    true -> stationAlreadyExists ;
    false -> NewKey = lists:foldl(fun (X, Acc) -> (X*(Acc+2) rem 10000) end,1,lists:append(Name,float_to_list(Latitude))),
             #monitor{stations = S , nameKeys = N, cordKeys =K} =Monitor,
             Monitor#monitor{  nameKeys = N#{Name => NewKey},
             cordKeys = K#{{Latitude,Longitude} => NewKey},
             stations = S#{NewKey => #{}} }
  end.

addValue(Monitor,{Latitude,Longitude},Date,Type,Value) ->
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
addValue(Monitor,Name,Date,Type,Value) ->
  case maps:is_key(Name,Monitor#monitor.nameKeys) of
    false -> stationDoesNotExist;
    true -> Key = maps:get(Name,Monitor#monitor.nameKeys),
            #monitor{stations = S } = Monitor,
            Measurements = maps:get(Key,S),
            Monitor#monitor{stations = S#{Key := maps:put({Type,Date},Value,Measurements)}}
  end.



removeValue(Monitor,{Latitude,Longitude},Date,Type) ->
  Key = maps:get({Latitude,Longitude},Monitor#monitor.cordKeys),
  #monitor{stations = S } = Monitor,
  Measurements = maps:get(Key,S),
  Monitor#monitor{stations = S#{Key := maps:remove({Type,Date},Measurements)}};
removeValue(Monitor,Name,Date,Type) ->
  Key = maps:get(Name,Monitor#monitor.nameKeys),
  #monitor{stations = S } = Monitor,
  Measurements = maps:get(Key,S),
  Monitor#monitor{stations = S#{Key := maps:remove({Type,Date},Measurements)}}.



getOneValue(Monitor,Name,Date,Type) ->
  Key = maps:get(Name,Monitor#monitor.nameKeys),
  Station = maps:get(Key,Monitor#monitor.stations),
  maps:get({Type,Date},Station).


getStationMean(Monitor,Name,Type) ->
  Key = maps:get(Name,Monitor#monitor.nameKeys),
  Station = maps:get(Key,Monitor#monitor.stations),
  Values = maps:values(maps:filter(fun({T,_},_) -> T == Type end , Station)),
  lists:foldl(fun (X,Acc) -> X+Acc end,0,Values)/length(Values).


getDailyMean(Monitor,Type,Date) ->
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


getAirQualityIndex(Monitor,{Latitude,Longitude},{Date,Hour},NormMap) when is_map(NormMap) ->
  Key = maps:get({Latitude,Longitude},Monitor#monitor.cordKeys),
  Measurements = maps:get(Key,Monitor#monitor.stations),
  Values = maps:values(maps:map(fun ({T,_},V) -> {T,V} end, maps:filter(fun ({_,{D,{H,_,_}}},_) -> (H ==Hour) and (D==Date) end,Measurements))),
  F1 = fun(X,Map) -> case maps:is_key(X,Map) of true -> maps:get(X,Map); false -> [] end end,
  S1=lists:foldl(fun({T,V},Acc) -> Acc#{T => [V|F1(T,Acc)]} end,#{},Values),
  try  maps:map(fun(V,K) -> 100*(lists:sum(K)/length(K))/maps:get(V,NormMap) end,S1) of
    S2 -> lists:max(maps:values(S2))
  catch
    error:_ -> normError
  end;
getAirQualityIndex(Monitor,{Latitude,Longitude},Hour,NormMap) when is_map(NormMap) ->
  Key = maps:get({Latitude,Longitude},Monitor#monitor.cordKeys),
  Measurements = maps:get(Key,Monitor#monitor.stations),
  Values = maps:values(maps:map(fun ({T,_},V) -> {T,V} end, maps:filter(fun ({_,{_,{H,_,_}}},_) -> H ==Hour end,Measurements))),
  F1 = fun(X,Map) -> case maps:is_key(X,Map) of true -> maps:get(X,Map); false -> [] end end,
  S1=lists:foldl(fun({T,V},Acc) -> Acc#{T => [V|F1(T,Acc)]} end,#{},Values),
  try  maps:map(fun(V,K) -> 100*(lists:sum(K)/length(K))/maps:get(V,NormMap) end,S1) of
    S2 -> lists:max(maps:values(S2))
    catch
      error:_ -> normError
  end.

%P = pollution:createMonitor().
%P1 = pollution:addStation(P,"XXX",{10.11,10.12}).
%P2 = pollution:addStation(P1,"YYY",{20.11,20.12}).
%P3 = pollution:addStation(P2,"ZZZ",{30.11,30.12}).
%P4 = pollution:addValue(P3,"XXX",calendar:local_time(),"PM10",1000).
%P5 = pollution:addValue(P4,"XXX",calendar:local_time(),"PM10",500).
%P6 = pollution:addValue(P5,"XXX",calendar:local_time(),temperature,30).
%P7 = pollution:addValue(P6,"YYY",calendar:local_time(),"PM10",300).
%P8 = pollution:addValue(P7,"YYY",calendar:local_time(),"PM10",100).
%P9 = pollution:addValue(P8,"YYY",calendar:local_time(),"PM2,5",100).
%P10 = pollution:addValue(P9,"ZZZ",calendar:local_time(),"PM10",1000).
%P11= pollution:addValue(P10,"ZZZ",calendar:local_time(),temperature,25).
%P12 = pollution:addValue(P11,"ZZZ",calendar:local_time(),"PM2,5",1000).