#!/bin/sh

# Generate a ticket for any incursion into the Deanland Lewes Rd. "Avoid" polygon.

dump1090-mutability \
 | ./match1090-x86_64-linux '--match=alt:<750' --strict=hex,lat,long,alt \
 | ./poly1090-x86_64-linux -- 50.8952,0.1581 50.9023,0.1732 50.8926,0.1780 50.8839,0.1601 \
 | ./ticket1090-x86_64-linux --gpx=only

