# mc
##build
```sh
$ git clone https://github.com/mingchaoyan/mc.git
...
$ cd mc
$ ./rebar prepare-deps
...
$ ./rebar compile
...
$ mkdir rel
$ cd rel
$ ./rebar create-node nodeid=mc
$ # edit rel.config
$ ../rebar generate
$ # edit sys.config
...
```

##start
```sh
$ cd sp
$ ./bin/mc start
$ ./bin/mc getpid
...
$ ./bin/mc attach
```

##stop
```sh
$./bin/sp stop
```
