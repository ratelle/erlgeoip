%%% Test the erlgeoip module
-module(erlgeoip_SUITE).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

-define(PROPTEST(P), ?assert(proper:quickcheck(P, [{to_file, user}]))).

%%%=============================================================================
%%% common_test callbacks
%%%=============================================================================

all() -> [random_ips_ipv4,
          undefined_ip,
          valid_ip,
          invalid_ip,
          valid_ipv6
         ].

suite() -> [{timetrap, {seconds, 15}}].

init_per_suite(Conf) ->
    ok = application:start(erlgeoip),
    Conf.

end_per_suite(_Conf) ->
    ok = application:stop(erlgeoip).

init_per_testcase(_Module, Conf) ->
    Conf.

end_per_testcase(_Module, _Conf) ->
    ok.

%%%=============================================================================
%%% Tests
%%%=============================================================================

%% @doc Tests that random ips get a result always in a proplist of the right types
random_ips_ipv4(_) ->
  ?PROPTEST(
    ?FORALL(Ip, union([ip_address_ipv4(), ip_address_ipv6()]),
      begin
        Erlgeoip = erlgeoip:lookup_pl(Ip),
        io:format("~p", [{Ip, Erlgeoip}]),
        [?assert(is_binary(proplists:get_value(Field, Erlgeoip)))
          || Field <- binary_fields()],
        [?assert(is_float(proplists:get_value(Field, Erlgeoip)))
          || Field <- float_fields()],
        [?assert(is_integer(proplists:get_value(Field, Erlgeoip)))
          || Field <- integer_fields()],
        true
      end
    )
  ).


%% @doc Tests that some ips get the right results.
valid_ip(_Conf) ->
    [?assertMatch([{country_code,<<"ES">>},
                  {country_code3,<<"ESP">>},
                  {country_name,<<"Spain">>},
                  {region,<<"29">>},
                  {city,<<"Madrid">>},
                  {postal_code,<<>>},
                  {latitude,40.408599853515625},
                  {longitude,-3.692199945449829},
                  {area_code,0},
                  {dma_code,0},
                  {organization_name,<<>>},
                  {isp_name,<<>>},
                  {netspeed,<<>>}], erlgeoip:lookup_pl(Ip))
  || Ip <- [<<"37.11.55.162">>,
            {37, 11, 55, 162}]].


%% @doc: Test when there is no ip provided
undefined_ip(_Conf) ->
    ?assertMatch([], erlgeoip:lookup_pl(undefined)).


%% @doc: Used to test invalid, unknown or non applicable targets
invalid_ip(_Conf) ->
    [?assertMatch([
                   {country_code,<<>>},
                   {country_code3,<<>>},
                   {country_name,<<>>},
                   {region,<<>>},
                   {city,<<>>},
                   {postal_code,<<>>},
                   {latitude,0.0},
                   {longitude,0.0},
                   {area_code,0},
                   {dma_code,0},
                   {organization_name,<<>>},
                   {isp_name,<<>>},
                   {netspeed,<<>>}],
                  erlgeoip:lookup_pl(Ip)) || Ip <-
                                                 [<<"3388.333.333.33">>,
                                                  <<"0.0.0.0">>,
                                                  <<"255.255.255.255">>,
                                                  <<"999.0.0.0">>,
                                                  <<"0.0.0.256">>,
                                                   {3388,333,333,33},
                                                   {0,0,0,0},
                                                   {255,255,255,255},
                                                   {999,0,0,0},
                                                   {0,0,0,256}]].


%% @doc: Test ipv6 ips
valid_ipv6(_Conf) ->
    [?assertMatch([
                  {country_code,<<"US">>},
                  {country_code3,<<"USA">>},
                  {country_name,<<"United States">>},
                  {region,<<>>},
                  {city,<<>>},
                  {postal_code,<<>>},
                  {latitude,38.0},
                  {longitude,-97.0},
                  {area_code,0},
                  {dma_code,0},
                  {organization_name,<<>>},
                  {isp_name,<<>>},
                  {netspeed,<<>>}],
                 erlgeoip:lookup_pl(Ip))
    || Ip <- [<<"2607:f0d0:1002:0051:0000:0000:0000:0004">>,
              "2607:f0d0:1002:0051:0000:0000:0000:0004",
              {9735,61648,4098,81,0,0,0,4}]].

%%%=============================================================================
%%% Internal functions
%%%=============================================================================


ip_address_ipv4() ->
  ?LET({I1, I2, I3, I4},
    {integer(0, 255), integer(0, 255), integer(0, 255), integer(0, 255)},
    {I1, I2, I3, I4}).

%% Select ranges that are in the database
ip_address_ipv6() ->
  ?LET({I1, I2, I3, I4, I5, I6, I7, I8},
    {integer(8193, 11279), integer(512, 65520), integer(0, 65535), integer(0, 65535),
     integer(1, 65535), integer(0, 65535), integer(0, 65535), integer(0, 65535)},
    {I1, I2, I3, I4, I5, I6, I7, I8}).

binary_fields() ->
  [country_code,
    country_code3,
    country_name,
    region,
    city,
    postal_code,
    organization_name,
    isp_name,
    netspeed].

float_fields() ->
  [latitude,
    longitude].

integer_fields() ->
  [area_code,
    dma_code].
