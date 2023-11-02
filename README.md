# utils1090
Filter utilities for dump1090-mutability output. See the ticket.sh file for examples, each program responds correctly to --help etc.

In brief, the match1090 program can select records according to criteria such as altitude (the --match) option, and either output specified fields in each matched record (--show) or only output records which have all specified fields present (--strict). Output will normally be timestamped (--time or --utc) and terminated with a blank line (--nl). It is expected that this will be used multiple times, e.g. to select all records which indicate a plane below a certain altitude to go into a file and then later to post-process the file.

The poly1090 program filters records for incursion into a defined polygon.

The ticket1090 program spits out a "moving traffic violation" file in .txt and/or .gpx format showing the track of incursions; it opens/commits automatically-named files, and is the only program in this collection which isn't expected to output to stdout for use in a pipeline.

Generated .gpx files work well with QGIS.

Building expects the Lazarus IDE in order to process the .lpi files (or the overall .lpg file), but the individual programs should compile fine with FPC invoked directly. This time round I'm not creating a makefile, sorry.

There might still be spurious references to Subversion utilities, which in your case you have not got.
