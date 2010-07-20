{application, tcp_server,
 [
  {description, "tcp server"},
  {vsn, "0.1"},
  {id, "tcp_server"},
  {modules, [tcp_acceptor_sup,  
	     tcp_listener,      
	     tcp_misc,
	     tcp_acceptor,        
	     tcp_client_sup,    
	     tcp_listener_sup,  
	     tcp_server]},
  {registered, [tcp_server_sup,
		tcp_client_sup]},
  {applications, [kernel, stdlib]},
  {mod, {tcp_server, []}}
  ]
}.
