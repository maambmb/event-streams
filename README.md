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

### Example

Below we have a dump of an unindexed file with 2 possible groups. The first byte is `0x00` which means the row is a regular event. The second byte is either `0x01` or `0x02` - describing which group the event belongs to. Obviously with this setup we are capped at 255 groups, but it is trivial to extend this.

```
00000000: 00 01 97 99 27 60 d7 5b 4e 4e 3e 3d  ....'`.[NN>=
0000000c: 00 01 2a d6 38 92 a8 d8 cd 82 a9 24  ..*.8......$
00000018: 00 02 53 40 2f fd 49 1d 4e e0 02 98  ..S@/.I.N...
00000024: 00 01 10 5b 3a d1 6f 2d b7 f1 4b b7  ...[:.o-..K.
00000030: 00 02 44 d1 62 18 4c 83 f3 57 1a 0d  ..D.b.L..W..
0000003c: 00 02 00 e5 30 a4 d2 da 61 a4 92 ce  ....0...a...
00000048: 00 02 d1 b0 6c c0 52 cd 70 3a 2e 85  ....l.R.p:..
00000054: 00 01 b5 85 eb ab 25 73 c3 2f 6a ab  ......%s./j.
00000060: 00 02 d1 75 18 03 7b 3b 70 85 a1 a9  ...u..{;p...
0000006c: 00 02 2f 5a 5c 85 8e 5a e0 27 92 1d  ../Z\..Z.'..
```

After indexing, the data looks like thiss:

```
00000000: 01 00 00 00 04 02 00 00 00 00 00 00 00 00 00 00  ................
00000010: 01 00 00 00 01 01 00 00 00 00 00 00 00 00 00 00  ................
00000020: 00 00 00 00 01 01 97 99 27 60 d7 5b 4e 4e 3e 3d  ........'`.[NN>=
00000030: 00 00 00 00 02 01 2a d6 38 92 a8 d8 cd 82 a9 24  ......*.8......$
00000040: 00 00 00 00 02 02 53 40 2f fd 49 1d 4e e0 02 98  ......S@/.I.N...
00000050: 00 00 00 00 04 01 10 5b 3a d1 6f 2d b7 f1 4b b7  .......[:.o-..K.
00000060: 00 00 00 00 01 02 44 d1 62 18 4c 83 f3 57 1a 0d  ......D.b.L..W..
00000070: 00 00 00 00 01 02 00 e5 30 a4 d2 da 61 a4 92 ce  ........0...a...
00000080: 00 00 00 00 02 02 d1 b0 6c c0 52 cd 70 3a 2e 85  ........l.R.p:..
00000090: 00 00 00 00 00 01 b5 85 eb ab 25 73 c3 2f 6a ab  ..........%s./j.
000000a0: 00 00 00 00 01 02 d1 75 18 03 7b 3b 70 85 a1 a9  .......u..{;p...
000000b0: 00 00 00 00 00 02 2f 5a 5c 85 8e 5a e0 27 92 1d  ....../Z\..Z.'..
```

As before, a `0x00` as the first byte of the row means the row is a regular event. If the byte is `0x01`, the row is a meta header row that defines a group and describes the offset to find the first event of said group. By reading these header rows (they live at the top of the file, we can get a list of all possible groups available). 

The following 4 bytes are the offset information. This tells us how many rows we need to jump to find the next event of the same group. If the value is 0 then we have reached the final event.

As you can see, the data has grown 4 bytes wider (the offset information), and 2 rows longer (the header rows for each group). My argument is that this increase in file-size is a small price to pay for better isolated playback of given groups.

## Project

### Random Data Generator (DONE)

Build a random data generator that can generate appropriate event data

### Data Indexer (DONE)

Build an indexer that can transform the event data appropriately. It should be able to handle unbounded file sizes

### Data Player

Build a player that can both walk through all events, and events pertaining to a specified group/set of groups.

### Analysis + Visualisations

Time the player on various file sizes and group sizes, and compare the performance with other solutions (naive + split)
