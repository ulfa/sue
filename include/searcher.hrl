-define(MULTICAST_GROUP, {239,255,255,250}).
-define(MULTICAST_PORT, 1900).
-define(OPTIONS,  [list,  {active,true}, {ip, ?MULTICAST_GROUP},{multicast_ttl, 255}, {reuseaddr,true},{multicast_loop,true}]).

