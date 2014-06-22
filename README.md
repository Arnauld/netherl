
# Notes & Diary

## Erlang & rebar setup

1. Install Erlang
2. [rebar](https://github.com/rebar/rebar/wiki/Getting-started)
3. openssl missing... 
4. [brew](http://brew.sh/) `brew install openssl`

Also

* [Winning the Erlang Edit•Build•Test Cycle](http://fr.slideshare.net/rklophaus/winning-the-edit-build-test-cycle): Many erlang tips'n tricks
* [sync](https://github.com/rustyio/sync): On-the-fly recompiling and reloading in Erlang. Code without friction.

## REPL

    erl -pa ebin/
    1> ne_store:pre_init().
    2> ne_store:start_link().
    3> ne_store:append_events(id1, [e1, e2]).
    4> ne_store:append_events(id1, [e3, e4]).
    5> ne_store:get_events(id1).
    [e1,e2,e3,e4]
    ...

    erl -pa .eunit/
    1> debugger:start().
    2> 

## Dev.

Execute tests:

    ./rebar compile eunit skip_deps=true

Execute tests for dependencies too:

    ./rebar compile eunit

### Rake

    $ rake
    $ rake test
