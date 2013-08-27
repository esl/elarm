elarm
=====

# Introduction #

Elarm is an Alarm Manager for Erlang. It is designed to be easy to include in an Erlang based system. Most functions are implemented through plugins so it is easy to change the behaviour if necessary.

## Alarms vs Events ##

Alarms and Events are often mixed up, but there are some important differences.

An Event is stateless, it just says "Something happened", e.g. "failed to open file" or "invoice created", and that is all. Logging tools like lager do event logging.

An Alarm on the other hand has state, it is an indicator that something is wrong in the system. Once an alarm is raised it remains active until the error condition connected to the alarm no longer applies. When the error condition is removed the alarm is cleared.

## Functions ##

### Alarm List ###

Elarm keeps a list of all currently active alarms. It is possible for a Manager application to subscribe to all changes in the alarm list.

A user can acknowledge alarm, that is to tell elarm that he is aware of the alarm. It is also possible to add comments to an alarm. Finally it is possible to manually clear an alarm. Normally an alarm is cleared by the application that raised the alarm, but in some cases that is not done, e.g. if the alarms are received as SNMP traps from another system.

### Alarm Log ###

All changes to an alarm are logged in an alarm log.

### Alarm Configuration ###

An application raising an alarm should not have to know too much about the alarm handling, so when an alarm is raised the application only has to supply a name of the alarm and the identity of the entity the alarm applies to and optionally some additional information, as an example the name could be 'partition_full' and the entity '/dev/hda1' and the additional information could be "90%".

For an operator it is useful to have some additional information when handling the alarms. This information includes, "severity" how serious is the alarm, "probable_cause" what is the likely reason for the alarm, "proposed_repair_action" how can I fix the problem. In elarm it is possible to add this information to the alarms by adding configuration data for an alarm. If no configuration data is found when an alarm is raised, default data is used. The default data is defined in the elarm.app file and can be overridden by data in sys.config.

To know which alarms need configuration data elarm is recording all alarms that have been raised for which there is no configuration. It is possible to query elarm for a list of alarms that are missing configuration using elarm:get_unconfigured/0,1. Alarm configuration can be added via elarm:add_configuration/2.

# Using #

## Starting ##

When elarm is started it starts one instance of the alarm manager, with the name elarm_server. It is possible to run several alarm managers at once. By putting an environment variable named servers in the system configuration file, with the value a list of tuples {ServerName, Opts}, elarm will start one alarm manager for each tuple. It is also possible to manually start a new alarm manager using elarm:start_server(Name, Opts).

## Application ##

An application wanting to raise an alarm just have to call

   ok = elarm:raise(partition_full, "/dev/hda2", [{level,90}])

and to clear it

   ok = elarm:clear(partition_full, "/dev/hda2")

So Name and Entity uniquely identifies an alarm.

## Manager ##

### Get all alarms ###

A Management application can request a list of all currently active alarms by elarm:get_alarms/1.

### Subscriptions ###

#### Alarm List ####

To subscribe to all alarm events use elarm:subscribe(Server, Filter). This will return a {Ref, AlarmList}, where Ref is a reference that will be included in all received messages and to cancel the subscription. AlarmList is a list of all the currently active alarms that match the filter. For all changes to the alarms, matching the filter, a message will be received.

The filter is a list of filter elements, if one filter element matches
then the filter matches.

FilterElement = all | {type, alarm_type()} | {src, alarm_src()}

* all, matches all alarms</dd>
* {type, Type}, matches alarms with alarm_type == Type
* {src, Src}, matches alarms with alarm_src == Src

The type and src filter elements may appear several times, in that case each one is tried and if one matches then the filter matches.

The format of the messages are:

* new alarm

. {elarm, Ref, alarm()}

* acknowledged alarm

. {elarm, Ref, {ack, alarm_id(), alarm_src(), event_id(), ack_info()}}

* cleared alarm

. {elarm, Ref, {clear, alarm_id(), alarm_src(), event_id()}}

* comment added

. {elarm, Ref, {add_comment, alarm_id(), alarm_src(), event_id, comment()}}

#### Alarm Summary ####

Alarm Summary gives a summary of the presence or absence of unacknowledged and acknowledged alarms of the various severities. This is useful for e.g. show the status on maps or other overview user interfaces.

To start a subscription use elarm:summary_subscription(Server, Filter).

Filter is the same as for alarm list subscriptions. Every time the summary changes a message will be received.

The format of the messages is:

* {elarm, Ref, #alarm_summary{}}

### Acknowledge Alarms ###

An alarm has two states, initially it is new, to show the user that it is a new alarm. When the user has seen the alarm he can acknowledge it using elarm:acknowledge/3. This will change the alarm state to acknowledged, and a timestamp and the UserId will be added to the alarm.

### Comment Alarms ###

It is possible to add comments to alarms, e.g. to add notes of troubleshoting or corrective actions taken. Each comment will be stored with a timestamp and the UserId of the user.

### Clear Alarm ###

Normally alarms are cleared automatically by the application by calling elarm:clear, but in some cases it may be necessary to manually clear an alarm, this can be done using elarm:manual_clear/3.

# Plugins #

