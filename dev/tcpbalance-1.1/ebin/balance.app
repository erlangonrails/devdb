{application, balance,
 [
  {description, "Load balancing TCP proxy"},
  {vsn, "1.0"},
  {id, "Balance"},
  {modules, [
             balance,
             bal_sup,
             bal_proxy,
             tcp_proxy
            ]
   },
   {registered, [ xxx_update_this_list ] },
   %% NOTE: do not list applications which are load-only!
   {applications, [ kernel, sasl ] },
   %%
   %% mod: Specify the module name to start the application, plus args
   %%
   %% balance's args: none
   %%      -OR-
   %%                 TCP port to listen, back-end connection timeout,
   %%                 back-end activity timeout
   %%
   {mod, {balance, []}}
  ]
}.
