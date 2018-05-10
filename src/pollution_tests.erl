%%%-------------------------------------------------------------------
%%% @author rafstach
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Apr 2018 13:44
%%%-------------------------------------------------------------------
-module(pollution_tests).
-author("rafstach").

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-record(monitor, {stations = #{}, nameKeys = #{}, cordKeys = #{}}).

createMonitor_test() ->
  ?assertEqual(#monitor{stations = #{}}, pollution:createMonitor()).

addStation_test() ->

  P = pollution:createMonitor(),
  P1 = pollution:addStation(P, "Aleja Słowackiego", {50.2345, 18.3445}),
  ?assert(maps:is_key("Aleja Słowackiego",P1#monitor.nameKeys)),

  P2 = pollution:addStation(P1, "Aleja Słowackiego", {50.2345, 18.3445}),
  ?assertEqual(P2, stationAlreadyExists),

  P3 = pollution:addStation(P1, "Aleja Słowackiego", {0.2345, 0.3445}),
  ?assertEqual(P3, stationAlreadyExists),

  P4 = pollution:addStation(P1, "Aleja Mickiewicza", {50.2345, 18.3445}),
  ?assertEqual(stationAlreadyExists,P4).


addValue_test() ->
  Date = calendar:local_time(),
  P = pollution:addStation(pollution:createMonitor(),"Aleja Słowackiego", {50.2345,18.3445}),
  P1 = pollution:addValue(P,"Aleja Słowackiego", Date, "PM10", 1000),
  P2 = maps:get("Aleja Słowackiego",P1#monitor.nameKeys),
  P3 = maps:get(P2,P1#monitor.stations),
  ?assertEqual(P3,#{{"PM10",Date} => 1000}),
  P4 = pollution:addValue(P1,{50.2345,18.3445}, Date, "PM10", 500),
  ?assertEqual(measurementAlreadyExists, P4).

removeValue_test() ->
  Date = calendar:local_time(),
  P = pollution:addStation(pollution:createMonitor(),"Aleja Słowackiego", {50.2345,18.3445}),
  P1 = pollution:addValue(P,"Aleja Słowackiego", Date, "PM10", 1000),
  P2 = pollution:removeValue(P,"Aleja Słowackiego", Date, "PM10"),
  P3 = maps:get("Aleja Słowackiego",P1#monitor.nameKeys),
  P4 = maps:get(P3,P2#monitor.stations),
  ?assertEqual(#{},P4).

getOneValue_test()->
  Date = calendar:local_time(),
  P = pollution:addStation(pollution:createMonitor(),"Aleja Słowackiego", {50.2345,18.3445}),
  P1 = pollution:addValue(P,"Aleja Słowackiego", Date, "PM10", 1000),
  P2 = pollution:getOneValue(P1,"Aleja Słowackiego", Date, "PM10"),
  ?assertEqual(1000,P2).

getStationMean_test() ->
  Date = calendar:local_time(),
  P = pollution:addStation(pollution:createMonitor(),"Aleja Słowackiego", {50.2345,18.3445}),
  P1 = pollution:addValue(P,"Aleja Słowackiego", Date, "PM10", 1000),
  P2 = pollution:addValue(P1,"Aleja Słowackiego", {{1,2,3}, {4,5,6}}, "PM10", 500),
  P3 = pollution:getStationMean(P2,"Aleja Słowackiego", "PM10"),
  ?assertEqual(750.0, P3).


getDailyMean_test() ->
  P = pollution:createMonitor(),
  P1 = pollution:addStation(P, "Aleja Słowackiego", {50.2345, 18.3445}),
  P2 = pollution:addValue(P1, "Aleja Słowackiego", {{2018, 4, 22}, {4, 5, 6}}, "PM10", 100),
  P3 = pollution:addValue(P2, "Aleja Słowackiego", {{2018, 4, 22}, {14, 15, 6}}, "PM10", 500),
  P4 = pollution:addValue(P3, "Aleja Słowackiego", {{2018, 4, 22}, {14, 5, 7}}, "PM10", 300),
  P5 = pollution:addValue(P4, "Aleja Słowackiego", {{2018, 4, 3}, {18, 22, 30}}, "PM10", 500),
  P6 = pollution:addValue(P5, "Aleja Słowackiego", {{2018, 4, 14}, {18, 22, 30}}, "PM10", 10),
  P7 = pollution:getDailyMean(P6, "PM10", {2018, 4, 22}),
  ?assertEqual(300.0, P7).


