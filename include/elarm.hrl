%%%-------------------------------------------------------------------
%%% @author Anders Nygren <anders.nygren@erlang-solutions.com>
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% Created : 26 Jul 2013 by Anders Nygren <erlang-solutions.com>
%%%-------------------------------------------------------------------

-ifndef(ELARM_HRL).
-define(ELARM_HRL, 1).

-type user_id()                :: binary().
%% -type type()                   :: atom().
%% -type id()                     :: atom() | integer() | string() | binary().
%% -type rdn()                    :: {type(), id()}.
%% -type fdn()                    :: [rdn()].
-type alarm_id()               :: term().
-type alarm_type()             :: communication | qos | processing | equipment | environment.
-type alarm_src()              :: term().
-type clear_reason()           :: ok | {manual, UserId ::term()} | source_gone.
-type timestamp()              :: calendar:datetime().
-type event_id()               :: tuple(). %% FIXME binary(). %% This must be globally unique
-type severity()               :: critical | major | minor | warning | indeterminate | cleared.
-type probable_cause()         :: binary().
-type proposed_repair_action() :: binary().
-type description()            :: binary().
-type additional_information() :: term().
-type text()                   :: binary().
-record(comment, {user         :: user_id(),
                  time         :: timestamp(),
                  text         :: text()}).
-type comment()                :: #comment{}.
-type comments()               :: [comment()].
-type trend()                  :: undefined | more_severe | less_severe | unchanged.
-type alarm_state()            :: new | acknowledged.
-type sub_filter_type()        :: all | {type,alarm_type()} | {src, alarm_src()} | summary.
-type sub_filter()             :: [sub_filter_type()].
-type log_filter()             :: term(). %% Should be a match spec or similar
-record(ack_info, {user        :: user_id(),
                   time        :: timestamp()}).
-type ack_info()               :: #ack_info{}.

-record(threshold, {
          id     :: atom(),
          level  :: number(),
          value  :: number()
         }).
-type threshold() :: undefined | #threshold{}.

-record(alarm, {
          alarm_id                                :: alarm_id(),
          alarm_type                              :: alarm_type(),
          src                                     :: alarm_src(),   
          event_time = calendar:universal_time()  :: timestamp(),
          event_id                                :: event_id(),
          severity = indeterminate                :: severity(), 
          probable_cause = <<"">>                 :: probable_cause(),
          proposed_repair_action = <<"">>         :: proposed_repair_action(),
          description = <<"">>                    :: description(),
          additional_information                  :: additional_information(),
          correlated_events = []                  :: [event_id()],
          comments = []                           :: comments(),
          trend                                   :: trend(),
          threshold                               :: undefined | threshold(),
          state = new                             :: alarm_state(),
          ack_info                                :: ack_info()
         }).

-type alarm() :: #alarm{}.

-record(alarm_summary, {
          new_critical         = false :: boolean(),
          new_major            = false :: boolean(),
          new_minor            = false :: boolean(),
          new_warning          = false :: boolean(),
          new_indeterminate    = false :: boolean(),
          critical             = false :: boolean(),
          major                = false :: boolean(),
          minor                = false :: boolean(),
          warning              = false :: boolean(),
          indeterminate        = false :: boolean()
         }).

-type alarm_summary() :: #alarm_summary{}.

-record(alarm_config, {
          alarm_type                      :: alarm_type(),
          severity = indeterminate        :: severity(), 
          probable_cause = <<"">>         :: probable_cause(),
          proposed_repair_action = <<"">> :: proposed_repair_action(),
          description = <<"">>            :: description(),
          additional_information          :: additional_information(),
          trend                           :: trend(),
          threshold                       :: threshold(),
          manual_clear_allowed            :: boolean(),
          ack_required                    :: boolean(),
          log                             :: boolean(),
          ignore                          :: boolean()
         }).

-type alarm_config()                      :: #alarm_config{}.

-endif.
