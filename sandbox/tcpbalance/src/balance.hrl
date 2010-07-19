%% Name of the balancer application
-define(BALANCER_APP, balance).

%% Module name for the TCP proxy
-define(TCPPROXY, tcp_proxy).

%% Atom used to inform tcp_proxy proc that no backends are available.
-define(TIMEOUT_BE, timeout_be).
