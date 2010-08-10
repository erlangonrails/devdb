{application, sysmon,
 [{description, "sysmon"},
  {vsn, "0.01"},
  {modules, [
    sysmon,
    sysmon_app,
    sysmon_sup,
    sysmon_web,
    sysmon_deps
  ]},
  {registered, []},
  {mod, {sysmon_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
