{application, ds,
  [{description, "dispatch server"},
   {vsn, "0.1"},
   {id, "ds"},
   {modules, [ds, 
	      ds_reader,
	      ds_route]},
   {registered, [ds_sup,
                 ds_route]},
   {applications, [kernel, stdlib]},
   {mod, {ds, []}}]
}.
