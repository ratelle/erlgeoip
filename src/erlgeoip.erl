-module(erlgeoip).

-export([
         init/0,
         lookup_pl/1,
         lookup/1,
         lookup_v6/1,
         normalize_city/1
        ]).

-include_lib("erlgeoip/include/erlgeoip.hrl").

-on_load(init/0).

%%%=============================================================================
%%% External API
%%%=============================================================================

%% @doc: Lookups an internet address and returns a proplist with the values.
%% Ip can be:
%%  a valid tuple ip_address() ipv4 {A, B, C, D} or ipv6 {A, B, C, D, E, F, G, H}
%%  a binary or a list <<"1.1.1.1">>, "18.18.3.7"
-spec lookup_pl(IpAddress :: tuple() | binary() | undefined) ->
                       list({Key :: atom(),
                             Value :: binary() | integer() | float()}).
lookup_pl(undefined) ->
    [];
lookup_pl(IpAddress) when is_tuple(IpAddress) ->
    lookup_pl(inet:ntoa(IpAddress));
lookup_pl(IpStr) when is_list(IpStr) ->
    lookup_pl(list_to_binary(IpStr));
lookup_pl(Ip) when is_binary(Ip) ->
    lookup_pl_int(Ip).

%% @doc: Loads the nif
-spec init() -> ok.
init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, _} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Dir -> Dir
              end,
    SoName = filename:join(PrivDir, "geoip_nif"),
    case catch erlang:load_nif(SoName, []) of
        ok -> ok;
        LoadError -> error_logger:error_msg("erlgeoip: error loading NIF (~p): ~p",
                                            [SoName, LoadError])
    end.

%% @doc: Lookups an ipv4 address and returns a #geoip{} record
-spec lookup(Ip :: list() | binary()) ->
  {ok, #geoip{}} | {error, Reason :: term()}.
lookup(_Ip) ->
    {error, geoip_nif_not_loaded}.

%% @doc: Lookups an ipv6 address and returns a #geoip{} record
-spec lookup_v6(Ip :: list() | binary()) ->
    {ok, #geoip{}} | {error, Reason :: term()}.
lookup_v6(_Ip) ->
    {error, geoip_nif_not_loaded}.


normalize_city(_City) ->
    {error, geoip_nif_not_loaded}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

lookup_pl_int(Ip) ->
    Result = case address_type(Ip) of
                 ipv4 ->
                     lookup(Ip);
                 ipv6 ->
                     lookup_v6(Ip)
             end,
    case Result of
        {ok, #geoip{} = GeoIp} ->
            geoip_to_proplist(GeoIp);
        {error, Reason} ->
            {error, Reason}
    end .

geoip_to_proplist(#geoip{} = GeoIp) ->
    [{country_code, GeoIp#geoip.country_code},
     {country_code3, GeoIp#geoip.country_code3},
     {country_name, GeoIp#geoip.country_name},
     {region, GeoIp#geoip.region},
     {city, GeoIp#geoip.city},
     {postal_code, GeoIp#geoip.postal_code},
     {latitude, GeoIp#geoip.latitude},
     {longitude, GeoIp#geoip.longitude},
     {area_code, GeoIp#geoip.area_code},
     {dma_code, GeoIp#geoip.dma_code},
     {organization_name, GeoIp#geoip.organization_name},
     {isp_name, GeoIp#geoip.isp_name},
     {netspeed, GeoIp#geoip.netspeed}
    ].

address_type(Ip) ->
    case binary:match(Ip, <<".">>) of
        nomatch ->
            ipv6;
        _ ->
            ipv4
    end .