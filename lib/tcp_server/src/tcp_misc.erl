-module(tcp_misc).

-include_lib("kernel/include/inet.hrl").

-export([tcp_name/3, tcp_host/1]).

%%
%% ip_address() = {N1,N2,N3,N4}              % IPv4
%%              | {K1,K2,K3,K4,K5,K6,K7,K8}  % IPv6
%%
%% #hostent{h_addr_list = [ip_address()]  % list of addresses for this host
%%          h_addrtype  = inet | inet6
%%          h_aliases = [hostname()]      % list of aliases
%%          h_length = int()              % length of address in bytes
%%          h_name = hostname()           % official name for host
%%   The record is defined in the Kernel include file "inet.hrl"
%%   Add the following directive to the module:
%%   -include_lib("kernel/include/inet.hrl").
%%

%%
%% tcp_name(Prefix, IPAddress, Port) -> Tcpname:atom()
%%
tcp_name(Prefix, IPAddress, Port) ->
    list_to_atom(
     lists:flatten(
      io_lib:format("~p_~s:~p", [Prefix, inet_parse:ntoa(IPAddress), Port]))).

%%
%% get the name for the host, if we can't get the hostname,
%% we will return the ip of string format.
%% tcp_host(IPAddress) -> Hostname:string() | IP:string()
%% 
tcp_host({0, 0, 0, 0}) ->
    {ok, Hostname} = inet:gethostname(), %% return the local hostname, will never failed :)
    case inet:gethostbyname(Hostname) of
	{ok, #hostent{h_name = Name}} ->
	    Name;
	{error, _Reason} ->
	    Hostname
    end;
tcp_host(IPAddress) ->
    case inet:gethostbyaddr(IPAddress) of
	{ok, #hostent{h_name = Name}} ->
	    Name;
	{error, _Reason} ->
	    inet_parse:ntoa(IPAddress)
    end.
