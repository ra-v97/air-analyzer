%%%-------------------------------------------------------------------
%%% @author rafstach
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. May 2018 00:17
%%%-------------------------------------------------------------------
-module(pollution_server_tests).
-author("rafstach").

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-record(monitor,{stations =#{},nameKeys=#{},cordKeys=#{}}).

start_stop_test() ->
  ?assertEqual(true,pollution_server:start()),
  ?assertEqual(ok,pollution_server:stop()).


addStation_test() ->
  pollution_server:start(),
  ?assertEqual({monitor,#{7442 => #{}}, #{"Station1" => 7442}, #{{0.3,0.3} => 7442}},pollution_server:addStation("Station1",{0.3,0.3})),
  pollution_server:stop().

addStationTwo_test() ->
  pollution_server:start(),
  pollution_server:addStation("Station1",{0.3,0.3}),
  ?assertEqual({monitor,#{562 => #{},7442 => #{}},
                        #{"Station1" => 7442,"Station2" => 562},
                        #{{0.1,0.1} => 562,{0.3,0.3} => 7442}},pollution_server:addStation("Station2",{0.1,0.1})),
  pollution_server:stop().

addStation_Err_test() ->
  pollution_server:start(),
  pollution_server:addStation("Station1",{0.3,0.3}),
  ?assertEqual(stationAlreadyExists,pollution_server:addStation("Station1",{0.3,0.3})),
  pollution_server:stop().

addValueNSN_test() ->
  pollution_server:start(),
  pollution_server:addStation("Station1",{0.3,0.3}),
  ?assertEqual(stationDoesNotExist,pollution_server:addValue("Station2",{{2018,5,10},{16,48,0}},"PM10",10)),
  pollution_server:stop().

addValueNSTC_test() ->
  pollution_server:start(),
  ?assertEqual(stationDoesNotExist,pollution_server:addValue({1.0,1.0},{{2018,5,10},{16,48,0}},"PM10",10)),
  pollution_server:stop().

addValue_test() ->
  pollution_server:start(),
  pollution_server:addStation("Station1",{0.3,0.3}),
  ?assertEqual({monitor,#{7442 =>#{{"PM10",{{2018,5,10},{16,48,0}}} =>10}},
                        #{"Station1" => 7442},
                        #{{0.3,0.3} => 7442}},pollution_server:addValue("Station1",{{2018,5,10},{16,48,0}},"PM10",10)),
  pollution_server:stop().

addValueSAME_test() ->
  pollution_server:start(),
  pollution_server:addStation("Station1",{0.3,0.3}),
  pollution_server:addValue("Station1",{{2018,5,10},{16,48,0}},"PM10",10),
  ?assertEqual(measurementAlreadyExists,pollution_server:addValue("Station1",{{2018,5,10},{16,48,0}},"PM10",10)),
  pollution_server:stop().

removeValue_test() ->
  pollution_server:start(),
  pollution_server:addStation("Station1",{0.3,0.3}),
  pollution_server:addValue("Station1",{{2018,5,10},{16,48,0}},"PM10",10),
  ?assertEqual({monitor,#{7442 => #{}}, #{"Station1" => 7442}, #{{0.3,0.3} => 7442}},pollution_server:removeValue("Station1",{{2018,5,10},{16,48,0}},"PM10")),
  pollution_server:stop().

removeValueNV_test() ->
  pollution_server:start(),
  pollution_server:addStation("Station1",{0.3,0.3}),
  ?assertEqual({monitor,#{7442 => #{}}, #{"Station1" => 7442}, #{{0.3,0.3} => 7442}},pollution_server:removeValue("Station1",{{2018,5,10},{16,48,0}},"PM10")),
  pollution_server:stop().

removeValueTWO_test() ->
  pollution_server:start(),
  pollution_server:addStation("Station1",{0.3,0.3}),
  pollution_server:addValue("Station1",{{2018,5,10},{16,48,0}},"PM10",10),
  pollution_server:addValue("Station1",{{2018,5,10},{16,48,0}},"PM2.5",10),
  ?assertEqual({monitor,#{7442 =>#{{"PM2.5", {{2018,5,10},{16,48,0}}} =>10}},
                        #{"Station1" => 7442},
                        #{{0.3,0.3} => 7442}},
    pollution_server:removeValue("Station1",{{2018,5,10},{16,48,0}},"PM10")),
  pollution_server:stop().

getOneValue_test() ->
  pollution_server:start(),
  pollution_server:addStation("Station1",{0.3,0.3}),
  pollution_server:addValue("Station1",{{2018,5,10},{16,48,0}},"PM10",10),
  ?assertEqual(10,pollution_server:getOneValue("Station1",{{2018,5,10},{16,48,0}},"PM10")),
  pollution_server:stop().

getOneValueMTO_test() ->
  pollution_server:start(),
  pollution_server:addStation("Station1",{0.3,0.3}),
  pollution_server:addValue("Station1",{{2018,5,10},{16,48,0}},"PM10",10),
  pollution_server:addValue("Station1",{{2018,5,10},{16,48,0}},"PM2.5",40),
  ?assertEqual(40,pollution_server:getOneValue("Station1",{{2018,5,10},{16,48,0}},"PM2.5")),
  pollution_server:stop().

getOneValueNV_test() ->
  pollution_server:start(),
  pollution_server:addStation("Station1",{0.3,0.3}),
  ?assertEqual(noResult,pollution_server:getOneValue("Station1",{{2018,5,10},{16,48,0}},"PM10")),
  pollution_server:stop().

getOneValueNVSI_test() ->
  pollution_server:start(),
  pollution_server:addStation("Station1",{0.3,0.3}),
  pollution_server:addValue("Station1",{{2018,5,10},{16,48,0}},"PM10",10),
  ?assertEqual(noResult,pollution_server:getOneValue("Station1",{{2018,5,10},{16,48,0}},"NOTinMONITOR")),
  pollution_server:stop().

getStationMean_test() ->
  pollution_server:start(),
  pollution_server:addStation("Station1",{0.3,0.3}),
  pollution_server:addValue("Station1",{{2018,5,10},{20,48,0}},"PM10",10),
  pollution_server:addValue("Station1",{{2018,5,11},{16,48,0}},"PM10",20),
  pollution_server:addValue("Station1",{{2018,5,10},{16,48,0}},"PM30",300),
  pollution_server:addValue("Station1",{{2018,5,13},{16,48,0}},"PM10",30),
  ?assertEqual(20.0,pollution_server:getStationMean("Station1","PM10")),
  pollution_server:stop().

getStationMeanOV_test() ->
  pollution_server:start(),
  pollution_server:addStation("Station1",{0.3,0.3}),
  pollution_server:addValue("Station1",{{2018,5,10},{20,48,0}},"PM10",10),
  pollution_server:addValue("Station1",{{2018,5,10},{16,48,0}},"PM30",300),
  ?assertEqual(300.0,pollution_server:getStationMean("Station1","PM30")),
  pollution_server:stop().

getStationMeanNV_test() ->
  pollution_server:start(),
  pollution_server:addStation("Station1",{0.3,0.3}),
  ?assertEqual(noResult,pollution_server:getStationMean("Station1","PM10")),
  pollution_server:stop().

getStationMeanNS_test() ->
  pollution_server:start(),
  pollution_server:addStation("NOsuchSTATION",{0.3,0.3}),
  ?assertEqual(stationDoesNotExist,pollution_server:getStationMean("Station1","PM10")),
  pollution_server:stop().

getDailyMeanNV_test() ->
  pollution_server:start(),
  pollution_server:addStation("Station1",{0.3,0.3}),
  ?assertEqual(noResult,pollution_server:getDailyMean("PM10",{{2018, 5 ,12},{0 ,0 , 0}})),
  pollution_server:stop().

getDailyMean_test() ->
  pollution_server:start(),
  pollution_server:addStation("Station1",{0.3,0.3}),
  pollution_server:addValue("Station1",{{2018,5,10},{16,48,0}},"PM30",300),
  pollution_server:addValue("Station1",{{2018,5,10},{17,48,0}},"PM30",400),
  pollution_server:addValue("Station1",{{2018,5,10},{18,48,0}},"PM30",500),
  ?assertEqual(400.0,pollution_server:getDailyMean("PM30",{2018,5,10})),
  pollution_server:stop().

getDailyMeanTS_test() ->
  pollution_server:start(),
  pollution_server:addStation("Station1",{0.3,0.3}),
  pollution_server:addStation("Station2",{0.4,0.4}),
  pollution_server:addValue("Station1",{{2018,5,10},{16,48,0}},"PM30",300),
  pollution_server:addValue("Station1",{{2018,5,10},{17,48,0}},"PM30",400),
  pollution_server:addValue("Station1",{{2018,5,10},{18,48,0}},"PM30",500),
  pollution_server:addValue("Station2",{{2018,5,10},{16,48,0}},"PM30",100),
  pollution_server:addValue("Station2",{{2018,5,10},{17,48,0}},"PM30",200),
  pollution_server:addValue("Station2",{{2000,5,10},{18,48,0}},"PM30",500),
  ?assertEqual(300.0,pollution_server:getDailyMean("PM30",{2018,5,10})),
  pollution_server:stop().

getAirQualityIndexPrivate_test() ->
  pollution_server:start(),
  pollution_server:addStation("Station1",{0.3,0.3}),
  pollution_server:addValue("Station1",{{2018,5,10},{16,18,0}},"PM10",300),
  pollution_server:addValue("Station1",{{2018,5,10},{16,58,0}},"PM10",400),
  pollution_server:addValue("Station1",{{2018,5,10},{16,48,0}},"PM10",500),
  ?assertEqual(400.0,pollution_server:getAirQualityIndex("Station1",{{2018,5,10},16},#{"PM10" => 100, "PM2.5" => 60})),
  pollution_server:stop().

getAirQualityIndexPrivateTS_test() ->
  pollution_server:start(),
  pollution_server:addStation("Station2",{0.3,0.3}),
  pollution_server:addValue("Station2",{{2018,5,10},{16,18,0}},"PM10",300),
  pollution_server:addValue("Station2",{{2018,5,10},{16,58,0}},"PM2.5",800),
  pollution_server:addValue("Station2",{{2018,5,10},{16,48,0}},"PM10",500),
  ?assertEqual(1600.0,pollution_server:getAirQualityIndex("Station2",{{2018,5,10},16},#{"PM10" => 100, "PM2.5" => 50})),
  pollution_server:stop().

getAirQualityIndexPrivateNN_test() ->
  pollution_server:start(),
  pollution_server:addStation("Station1",{0.3,0.3}),
  pollution_server:addValue("Station1",{{2018,5,10},{16,18,0}},"PM",300),
  pollution_server:addValue("Station1",{{2018,5,10},{16,58,0}},"PM",400),
  pollution_server:addValue("Station1",{{2018,5,10},{16,48,0}},"PM",500),
  ?assertEqual(normError,pollution_server:getAirQualityIndex("Station1",{{2018,5,10},16},#{"PM10" => 100, "PM2.5" => 60})),
  pollution_server:stop().

getAirQualityIndexPrivateDD_test() ->
  pollution_server:start(),
  pollution_server:addStation("Station1",{0.3,0.3}),
  pollution_server:addValue("Station1",{{2018,5,10},{16,18,0}},"PM10",300),
  pollution_server:addValue("Station1",{{2018,5,10},{16,58,0}},"PM10",400),
  pollution_server:addValue("Station1",{{2018,5,10},{16,48,0}},"PM10",500),
  pollution_server:addValue("Station1",{{2018,5,10},{16,18,0}},"PM2.5",300),
  pollution_server:addValue("Station1",{{2018,5,10},{16,58,0}},"PM2.5",400),
  pollution_server:addValue("Station1",{{2018,5,10},{16,48,0}},"PM2.5",500),
  ?assertEqual(800.0,pollution_server:getAirQualityIndex("Station1",{{2018,5,10},16},#{"PM10" => 100, "PM2.5" => 50})),
  pollution_server:stop().