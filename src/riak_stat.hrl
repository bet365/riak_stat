%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-define(CACHE, riak_stat_config:get_env(exometer_cache, {cache, 5000})).
-define(PFX, riak_stat:prefix()).
-define(TIMESTAMP, riak_stat_exometer:timestamp()).

-define(META_ENABLED, metadata_enabled).
-define(EXOSKEL_ENABLED, exoskeleskin_enabled).

-define(IS_ENABLED(Arg),  riak_stat_config:get_env(Arg, false)).
%%  == true).
-define(IS_DISABLED(Arg), riak_stat_config:get_env(Arg) == false).

-type exometererror() :: no_template | exists | not_found.
-type profileerror()  :: profile_exists_already | no_stats | no_data | no_profile.
-type metaerror()     :: unregistered | no_stat | no_status.
-type reason()        :: exometererror() | profileerror() | metaerror() | any().
-type error()         :: {error, reason()}.
-type arg()               :: any().

-type value()         :: any().
-type exo_value()     :: {ok, value()}.
-type aliases()       :: list() | atom().
-type info()          :: name | type | module | value | cache| status |
                         timestamp | options | ref | datapoints | entry.
-type datapoint()     :: info() | list() | integer().
-type opt_tup()       :: {atom(), any()}.
-type options()       :: list() | opt_tup().
-type acc()           :: any().

-type app()           :: atom().
-type statname()      :: atom() | list().
-type type()          :: atom() | tuple().
-type status()        :: enabled | disabled | unregistered.
-type print()         :: any().
-type attr()          :: [info()].
-type stats()         :: list() | tuple().
-type priority()      :: metadata | exometer.
-type data()          :: any().
-type pfx()           :: riak.
-type incrvalue()     :: non_neg_integer().
-type response()      :: ok | term() | error().

-type profilename()   :: list() | binary().
-type pattern()       :: ets:match_spec().
-type timestamp()     :: non_neg_integer().
-type ttl()           :: atom() | integer().