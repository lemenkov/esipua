-record(sdp,{origin, session_name, connection, attributes=[], media=[]}).
-record(sdp_origin, {username, session_id, version, network_type, address_type,
	    address}).
-record(sdp_media,{media, port, transport, fmts, connection, attributes=[]}).
-record(sdp_connection,{network_type, address_type, address}).
