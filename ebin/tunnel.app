{application, 'tunnel', [
	{description, "Findök"},
	{vsn, "0.1.0"},
	{modules, ['actions','board','clock','door','hall','join_handler','player_status_handler','res','res_test','room','room_test','s','seats','table','table_info_handler','table_play_handler','table_test','tunnel_app','tunnel_sup']},
	{registered, [tunnel_sup]},
	{applications, [kernel,stdlib,cowboy,jiffy]},
	{mod, {tunnel_app, []}},
	{env, []}
]}.