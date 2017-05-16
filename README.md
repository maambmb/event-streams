# event-streams

## Problem

  1. Storing ordered events from a high throughput source results in a large file.
  2. Given a classifier which classifies events into one of N groups, isolated playback of a single group is tricky.

## Solutions

#### Scanning through the entire file

Scanning through the entire file is the naive solution. Its easy to implement and requires no additional processing to the source data. Unfortunately due to the fact that it must scan through **all** records, a lot of time is wasted on events which belong to an incorrect group.

### Splitting the file

Splitting the file N-ways such that each group has its own file is slightly smarter. Isolated playback is fast, as you can simply select the relevant group file and scan through it. Book-keeping becomes more troublesome as there are more files to know about and playback of the entire data-set requires a sort of file-based merge-sort approach which is unwanted hassle and slower *(assumption made as each event emission needs to be preceeded by an n-sized sort)*. We could remedy this by redundantly storing the whole data set, but then our storage requirements double as a result.

### Indexing the file

We can index the file, such that we extend each element with the byte/event offset of the next event that is from the same group. In this way, we are able to do playback of the entire file whilst also being able to quickly play individual groups by skipping over non-relevant events.

## Project

### Random Data Generator (DONE)

Build a random data generator that can generate appropriate event data

### Data Indexer (DONE)

Build an indexer that can transform the event data appropriately. It should be able to handle unbounded file sizes

### Data Player

Build a player that can both walk through all events, and events pertaining to a specified group/set of groups.

### Analysis + Visualisations

Time the player on various file sizes and group sizes, and compare the performance with other solutions (naive + split)
