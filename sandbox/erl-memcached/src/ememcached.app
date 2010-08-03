{application, ememcached,
  [{description,
    "ememcached description"},
   {vsn,          "1.0.0"},
   {modules,      [ememcached, ememcached_app, ememcached_server, ememcached_storage,
                   ememcached_listener, ememcached_sup]},
   {registered,   [ememcached_sup, ememcached_listener]},
   {applications, [kernel, stdlib]},
   {mod, {ememcached_app,[]}}]}.
