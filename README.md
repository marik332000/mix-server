# MIX Game Server

This package provides a [MIX game server][mix] for several
[Synthetic Reality][sr] games. It registers with the master server so
that players will be able to find your server. If you're behind a NAT,
you'll need to forward *both* UDP and TCP on the selected port
(`mix-port`).

Use `M-x mix-start` and `M-x mix-stop` to start and stop the
server. Set the server name as it will appear in the master server
listing with `mix-name`. Stopping the server does not unregister from
the master server (TODO), so it will remain in the list until it times
out.

## Supported Games

 * [Arcadia](http://www.synthetic-reality.com/arcadia.htm)
 * [Well of Souls](http://www.synthetic-reality.com/wosHome.htm)
 * [Warpath](http://www.synthetic-reality.com/warpath32.htm)

On startup, the MIX server registers with all three master servers at
the same time. All games can be played at once on the same server.


[sr]: http://www.synthetic-reality.com/
[mix]: http://www.synthetic-reality.com/mixgame.htm
