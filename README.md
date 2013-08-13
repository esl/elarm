elarm
=====

# Introduction #

Elarm is an Alarm Manager for Erlang. It is designed to be easy to include in an Erlang based system. Most functions are implemented through plugins so it is easy to change the behaviour if necessary.

## Alarms vs Events ##

Alarms and Events are often mixed up, but there are some important differences.

An Event is stateless, it just says "Something happened", e.g. "filed to open file" or "invoice created", and that is all. Logging tools like lager do event logging. 

An Alarm on the other hand has state, it is an indicator that something is wrong in the system. Once an alarm is raised it remains active until the error condition connected to the alarm no longer applies. When the error condition is removed the alarm is cleared.

## Functions ##

### Alarm List ###

Elarm keeps a list of all currently active alarms. It is possible for a Manager application to subscribe to all changes in the alarm list.

A user can acknowledge alarm, that is to tell elarm that he is aware of the alarm. It is also possible to add comments to an alarm. Finally it is possible to manually clear an alarm. Normally an alarm is cleared by the application that raised the alarm, but in some cases that is not done, e.g. if the alarms are received as SNMP traps fromanother system.

### Alarm Log ###

All changes to an alarm is logged in an alarm log.

### Alarm Configuration ###

An application raising an alarm should not have to know too much about the alarm handling, so when an alarm is raised the application only has to supply a name of the alarm and the identity of the entity the alarm applies to and optionally some additional information, as an example the name could be 'partition_full' and the entity '/dev/hda1' and the additional information could be "90%".

For an operator it is useful to have some additional information when handling the alarms. This information includes, "severity" how serious is the alarm, "probable_cause" what is the likely reason for the alarm, "proposed_repair_action" how can I fix the problem. In elarm it is possible to add this information to the alarms by adding configuration data for an alarm.

# Using #

## Application ##

An application wanting to raise an alarm just have to call

   ok = elarm:raise(partition_full, "/dev/hda2", [{level,90}])

and to clear it

   ok = elarm:clear(partition_full, "/dev/hda2")

So Name and Entity uniquely identifies an alarm.

## Manager ##

