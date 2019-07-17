%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-author("savannahallsop").

-type exometererror() :: no_template | exists | no_found.
-type profileerror()  :: no_stats | profile_exists_already | no_data | no_profile.
-type metaerror()     :: unregistered | no_stat | no_status.
-type reason()        :: exometererror() | profileerror() | metaerror() | any().
-type error()         :: {error, reason()}.

-type value()         :: any().
-type exo_value()     :: {ok, value()}.
-type aliases()       :: list() | atom().
-type info()          :: name | type | module | value | cache| status |
                         timestamp | options | ref | datapoints | entry.
-type datapoint()     :: info() | list().
-type opt_tup()       :: {atom(), any()}.
-type options()       :: list() | opt_tup().
-type statlist()      :: list().
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
-type pfx()           :: riak_stat:prefix().
-type incrvalue()     :: non_neg_integer().
-type response()      :: ok | term() | error().

-type profilename()   :: list() | binary().
-type pattern()       :: ets:match_spec().
-type timestamp()     :: non_neg_integer().
-type ttl()           :: atom() | integer().