{application, e2d_mgr,
 [{description, "e2d manager application"},
  {vsn, "0.1"},
  {modules, [e2d_mgr,
             e2d_mgr_httpd,
             e2d_mgr_nodes,
             e2d_mgr_sysconf
            ]},
  {registered, [e2d_mgr_sup, e2d_mgr_nodes, e2d_mgr_sysconf]},
  {application, [kernel, stdlib, sasl, crypto]},
  {mod, {e2d_mgr, []}}
]}.
