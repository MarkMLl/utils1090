(* Lazarus+FPC 2.2.6+3.2.2 on Linux Lazarus+FPC 2.2.6+3.2.2 on Linux Lazarus+FP *)

program MakeTrack1090;

(* Generate a track comprising (by default) a 11x11 grid traversed as a zig-zag *)
(* course at (of the order of) 0.01 degree spacings about a defined point, with *)
(* 10 seconds between points.                                                   *)
(*                                                                              *)
(* This is intended as input test data for Poly1090 etc.        MarkMLl         *)

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, IniFilesAbout;

(* Default midpoint is somewhere convenient to me (i.e. I don't have to zoom a  *)
(* mapping program too far out to see it close to my home location).            *)

const
  midLat= 51.0;
  midLong= 0.0;

(* Nominally a 10x10 grid. Actually it's 11x11 so as to put an origin firmly on *)
(* the midpoint.                                                                *)

  lats= 10;
  longs= 10;

(* Step sizes applied to latitude and longitude. It might or might not be       *)
(* convenient to keep these the same.                                           *)

  stepLat= 0.02;
  stepLong= 0.04;
  alt= 1000;

(* With the above, adequate test bounds for a large square are something like   *)
(*                                                                              *)
(* -- 51.09,0.18 50.91,0.18 50.91,-0.18 51.09,-0.18                             *)
(*                                                                              *)
(* Smaller square                                                               *)
(*                                                                              *)
(* -- 51.05,0.10 50.95,0.10 50.95,-0.10 51.05,-0.10                             *)
(*                                                                              *)
(* One of the edges can be trivially made reentrant                             *)
(*                                                                              *)
(* -- 51.05,0.10 50.95,0.10 50.97,0.03 50.95,-0.10 51.05,-0.10                  *)
(*                                                                              *)
(* Larger and smaller squares with extra points on matching edge                *)
(*                                                                              *)
(* -- 51.09,0.18 50.91,0.18  50.91,0.02 50.91,-0.02  50.91,-0.18 51.09,-0.18    *)
(* -- 51.05,0.10 50.95,0.10  50.95,0.02 50.95,-0.02  50.95,-0.10 51.05,-0.10    *)
(*                                                                              *)
(* Order of smaller (but not larger) square reversed                            *)
(*                                                                              *)
(* -- 51.09,0.18 50.91,0.18  50.91,0.02 50.91,-0.02  50.91,-0.18 51.09,-0.18    *)
(* -- 51.05,-0.10 50.95,-0.10  50.95,-0.02 50.95,0.02  50.95,0.10 51.05,0.10    *)
(*                                                                              *)
(* Extra points joined to make a corridor                                       *)
(*                                                                              *)
(* -- 51.09,0.18 50.91,0.18  50.91,0.02 \                                       *)
(*     50.95,0.02  50.95,0.10 51.05,0.10 51.05,-0.10 50.95,-0.10  50.95,-0.02 \ *)
(*     50.91,-0.02  50.91,-0.18 51.09,-0.18                                     *)
(*                                                                              *)
(* Hence a workflow testing a reentrant edge something like                     *)
(*                                                                              *)
(* ./maketrack1090 -- 51.05,0.10 50.95,0.10 50.97,0.0 50.95,-0.10 51.05,-0.10 \ *)
(*   | ./poly1090 -- | ./adsb1090_to_gpx > test.gpx                             *)
(*                                                                              *)
(* Or                                                                           *)
(*                                                                              *)
(* ./maketrack1090 -- 51.05,0.10 50.95,0.10 50.97,0.0 50.95,-0.10 51.05,-0.10 \ *)
(*   > test.txt                                                                 *)
(*   | ./poly1090 -- test.txt | ./adsb1090_to_gpx > test.gpx                    *)
(*                                                                              *)
(* That specific test should show 22 points within the polygon.                 *)
(*                                                                              *)
(* Or a corridor                                                                *)
(*                                                                              *)
(* ./maketrack1090 -- 51.09,0.18 50.91,0.18  50.91,0.02 \                       *)
(*    50.95,0.02  50.95,0.10 51.05,0.10  51.05,-0.10 50.95,-0.10  50.95,-0.02 \ *)
(*    50.91,-0.02  50.91,-0.18 51.09,-0.18 \                                    *)
(*    | ./poly1090 --  | ./adsb1090_to_gpx > test.gpx                           *)
(*                                                                              *)
(* which should show 54 points within the polygon.                              *)
(*                                                                              *)
(* Add a wall along the middle of the corridor                                  *)
(*                                                                              *)
(* ./maketrack1090 -- 51.09,0.18 50.91,0.18  50.91,0.02 \                       *)
(*    50.95,0.02  50.95,0.10 51.05,0.10 \                                       *)
(*    51.05,0.01 50.93,0.01 50.93,-0.01 51.05,-0.01 \                           *)
(*    51.05,-0.10 50.95,-0.10  50.95,-0.02 \                                    *)
(*    50.91,-0.02  50.91,-0.18 51.09,-0.18 \                                    *)
(*    | ./poly1090 --  | ./adsb1090_to_gpx > test.gpx                           *)
(*                                                                              *)
(* which should show 60 points within the polygon.                              *)

var
  i, j: integer;
  lat: single= midLat - (lats div 2) * stepLat;
  long: single= midLong - (longs div 2) * stepLong;
  time: TDateTime;


procedure writePoint(time: TDateTime; lat, long: single);

var
  timeNow: ansistring;

begin
  timeNow := IsoFormatDateTime(time, IsoDateTTime, 2) + 'Z ';
  WriteLn(timeNow, 'ICAO Address: 123456 (Mode S / ADS-B)');
  WriteLn(timeNow, 'Altitude: ', alt, ' ft barometric');

(* I am making no attempt to insert (raw?) values in parentheses, since related *)
(* programs don't use it (that's my story and I'm sticking to it).              *)

  WriteLn(timeNow, 'CPR latitude: ', lat:8:5, ' (0)');
  WriteLn(timeNow, 'CPR longitude: ', long:8:5, ' (0)');
  WriteLn
end { writePoint } ;


begin
  if ParamCount > 0 then begin
    Write('# BOUNDS ');
    for i := 1 to ParamCount() do
      if ParamStr(i) <> '--' then begin
        Write(Trim(ParamStr(i)));
        if ParamStr(i)[Length(ParamStr(i))] <> ',' then
          Write(' ')
      end;
    WriteLn;
    WriteLn
  end;
  time := UTC_Now();
  for i := 0 to lats do begin
    for j := 0 to longs + 1 do begin
      writePoint(time, lat, long);
      time += (1.0 / SecsPerDay);
      if j in [1..longs] then
        if Odd(i) then
          long -= stepLong
        else
          long += stepLong
    end;
    lat += stepLat
  end
end.

