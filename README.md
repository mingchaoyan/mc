# mc
##build
```sh
$ git clone https://github.com/mingchaoyan/mc.git
...
$ cd mc
$ rebar prepare-deps
...
$ rebar compile
...
$ madir rel
$ cd rel
$ rebar generate
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
