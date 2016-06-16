Allows quick and easy parsing of a timestring into a regular timestamp.

# Example #

    > (parsetime:parse-time-string
       "2016-06 (Jun)"
       '(:year #\- (:month 2) " (" :short-month ")"))
    @2016-06-01T00:00:00.000000Z

# Details #
This function is a partial inverse function of `local-time:format-timestring`.
Given a string, and `local-time`'s format string,
it will attempt to create a timestamp that can emit the same timestring
if sent through `format-timestring` with that format string.

To do this, it first scans through the timestring,
skipping past any literal strings and characters
and putting parsed values in a temporary plist.
(This means if the same keyword is repeated twice,
the one further down the list overwrites the one in the beginning.)

After parsing is completed, a timestamp object is then created.
Since timestamps are not necessarily complete or consistent,
several details are set:

1. Year (defaults to this year), month (defaults to 1), day (defaults to 1)
   are the most important and reliable values.
   One can use month names for this, but not week names.
2. ISO year (defaults to this year), week number (defaults to 1)
   and day-of-week (defaults to 0) are used if all of year, month and day
   are missing. Note that ISO year is not the same as the regular year field.
3. Day-of-week information comes at the end, only if the real day is missing.
