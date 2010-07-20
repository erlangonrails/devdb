{application, dispatchserver,
  [{description, "dispatch server"},
   {vsn, "0.1"},
   {id, "dispatchserver"},
   {modules, [dispatchserver, 
	      dispatchserver_reader,
	      dispatchserver_route,
	      dispatchserver_ctl]},
   {registered, [dispatchserver_sup,
                 dispatchserver_route]},
   {applications, [kernel, stdlib]},
   {mod, {dispatchserver, []}}]
}.
