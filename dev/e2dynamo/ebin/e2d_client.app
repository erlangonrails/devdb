{application, e2d_client,
 [{description, "e2dynamo client library application"},
  {vsn, "0.1"},
  {modules, []},
  {registered, []},
  {start_phases, [go]},
  {application, [kernel, stdlib, sasl, crypto]},
  {mod, {e2d_c, []}}
 }.
