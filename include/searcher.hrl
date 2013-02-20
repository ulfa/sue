-define(MULTICAST_GROUP, {239,255,255,250}).
-define(MULTICAST_PORT, 1900).
-define(OPTIONS,  [binary, inet,{broadcast,true},{reuseaddr, true},{multicast_ttl, 32}, {ip, ip_device:get_ip()}, {multicast_if, ip_device:get_ip()},{multicast_loop,true}, {active,true}]).

