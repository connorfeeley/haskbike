# Toronto Bike Share Historical Data Explorer

I started collecting station-level data from Toronto's Bike Share API so
that I could get a historical view of the system's usage. I've been
collecting data about every station in the network roughly every 30s,
since September 24th, 2023.

This project was borne out of my frustration for why Wellesley Station
has had a complement of at least 9 - often more - disabled e-bikes ever
since the station was converted to a charging station a few months ago

## *The Frustration*

I think Toronto's Bike Share program is, for the most part, a fairly
well-run program and a stunningly good value-for-money proposition for
both users as well as for the city[^1].

Those of us who think the system is great, and who therefore use the
system regularly, are also probably dearly familiar with with the ways
in with the system is *not* great.

Depending on the time of day and how close you are to the core, your
local station probably spends most of it's time (nearly) full or
(nearly) empty[^2].

E-Bikes - dramatically more common now than earlier even this year
(which is great!) - seem to be disabled whenever you want to use
one[^3].

The limited number of charging stations doesn't help much: [see
list](https://bikes.cfeeley.org/visualization/station-list?station-type=Charging).

[^1]: Bike Share's operating loss of a handful of millions of dollars
    per year for it's TKTK yearly rides gives us a rough subsidy of
    \$TKTK/ride. When considered against the subsudies for various other
    transportation modes (TTC: TKTK; driving: unknown, but immense) this
    strikes me as superb value given the near-zero carbon impact of
    biking - not to mention the positive health impacts.

[^2]: I don't actually have a reliable statistic for whether or not this
    is true. It *feels* true, but I realize that may just be an
    availability bias. By collecting this data and making it viewable -
    and, in the future, making the raw data easily exportable - I hope
    to someday be able to statistically confirm or deny that statement.

[^3]: Between October 1st and October 28th, the average\[fn:4\] number
    of available e-bikes across the entire system was 266.
