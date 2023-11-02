(* Lazarus+FPC 2.0.12+3.2.0 on Linux Lazarus+FPC 2.0.12+3.2.0 on Linux Lazarus+ *)

unit GpxCommon;

(* Various routines useful for reading and writing .gpx files. Note that some   *)
(* of the calculations relating to latitude and longitude are rather dodgy,     *)
(* I've commented that these result in "quasi" distances etc. and marked the    *)
(* output with ~ indicating an approximation.                   MarkMLl         *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

(* In the region of the UK, the approximation that a degree of latitude or      *)
(* longitude is roughly 111.2 km is reasonable. Since GPX shouldn't be expected *)
(* to provide a location to better than 1mm resolution it's appropriate to      *)
(* limit the number of decimal digits displayed to 8, and since the distances   *)
(* considered by this unit and associated programs are small the overall        *)
(* precision can be limited to 10 characters: this is adequate to show short-   *)
(* term jitter even if errors average out.                                      *)
(*                                                                              *)
(* Distances in metres and bearings in degrees will be restricted to 2 decimal  *)
(* digits, since these are likely to be the ones watched by anybody who should  *)
(* not be encouraged to have unrealistic expectations.                          *)
(*                                                                              *)
(* In both cases the precision might be tweaked by an inline incremement or     *)
(* decrement to e.g. ensure that a bearing of up to 360 degrees is displayed    *)
(* with a resolution similar to that of a small offset in metres.               *)

const
  DLLD= 8;                              (* Degrees latitude/longitude decimals  *)
  DLLP= 10;                             (* Degrees latitude/longitude precision *)
  GLLD= 12;                             (* GPX file output uses full precision  *)
  GLLP= 14;                             (* GPX file output uses full precision  *)
  MCD= 2;                               (* Metres Cartesian decimals            *)
  MCP= 4;                               (* Metres Cartesian precision           *)

type
  TGpxLines= (GpxJunk, GpxText, GpxPos, GpxTime);
  Twelford= record
               samples: qword;
               current, delta, mean, mean2: extended
             end;

var
  GpxComment: string= '';               (* GPX metadata <text> line delimited | *)

(* Return Now() expressed as a UTC string, with T separator and Z terminator as
  required by the .gpx format.
*)
FUNCTION UtcNow(fractionDigits: integer= 0): STRING;

(* Write the first part of a .gpx (XML) file, up to and including the end of
  the metadata.
*)
procedure WriteGpxHeaderA(var gpx: text; const comment: string= '');

(* Write mean position as a waypoint and standard deviation as an ellipse of
  routepoints.
*)
procedure WriteGpxStats(var gpx: text; meanLat, meanLong, bearing, sigmaLat, sigmaLong,
                                        minLat, maxLat, minLong, maxLong: extended);

(* Write the second part of a .gpx (XML) file, after which a sequence of
  trackpoints should follow.
*)
procedure WriteGpxHeaderB(var gpx: text; const name: string);

(* Wrap up a .gpx file after a trackpoint sequence, after which some other
  sequence might follow.
*)
procedure WriteGpxFooterA(var gpx: text);

(* Wrap up a .gpx file after all sequences.
*)
procedure WriteGpxFooterB(var gpx: text);

(* Decide on the content of a line. Junk simply represents stuff that we aren't
  going to manipulate, it's still significant as far as the format is concerned.
*)
function LineType(const s: string): TGpxLines;

(* Advance to and read the <text> lines. Return false if they differ.
*)
function TextSame(var a, b: text): boolean;

(* Read the significant lines from a trackpoint.
*)
function ReadTrkPt(var gpx: text; out trkPt, time: string): boolean;

(* Advance both files to the next time, return false at EOF.
*)
function Next(var a: text; var trkA, timA: string; var b: text; var trkB, timB: string): boolean;

(* Assume both files are positioned on times, but they might not match (in
  particular, near the start of the captured data). Advance until they match, or
  return false if not.
*)
function Jiggle(var a: text; var trkA, timA: string; var b: text; var trkB, timB: string): boolean;

(* I'm tempted to make this into a record helper but it would make the program
  even less portable than it currently is for very little advantage.
*)
procedure InitWelford(out w: Twelford);

(* Update the running data structure to take a new value into account.
*)
function UpdateWelford(var w: Twelford; const datum: extended): extended;

(* Extract the final values from the Welford data structure.
*)
procedure ExtractWelford(const w: Twelford; out mean, sigma: extended);

(* Convert a small North-South distance lat expressed in degrees to metres,
  assuming that the approximate latitude is meanLat and that the Earth is
  spherical.
*)
function LatToMetres(lat: extended; {%H-}meanLat: extended= 0.0; {%H-}meanLong: extended= 0.0): double;

(* Convert a small East-West distance long expressed in degrees to metres,
  assuming that the approximate latitude is meanLat and that the Earth is
  spherical.
*)
function LongToMetres(long, meanLat: extended; {%H-}meanLong: extended= 0.0): double;

(* Convert a small pair of distances lat and long expressed in degrees to metres,
  assuming that the approximate position in given by meanLat and meanLong and
  that the Earth is spherical.
*)
function LatLongToMetres(lat, long, meanLat: extended; {%H-}meanLong: extended= 0.0): double;

(* Convert a small distance expressed in degrees along a bearing in degrees (+ve
  is clockwise i.e. East of North) to metres, assuming that the approximate
  position is given by meanLat and meanLong and that the Earth is spherical.

  This assumes that the parameter is a quasi-distance derived from combining
  Cartesian latitude and longitude, which I have noted elsewhere as being dodgy.
  I expect the result, however, to be reasonable, so it doesn't need to be
  qualified by ~ as an approximation.
*)
function DegsToMetres(degs, bearing, meanLat: extended; meanLong: extended= 0.0): double;


implementation

uses
  StrUtils, IniFilesAbout, Math;


(* Return Now() expressed as a UTC string, with T separator and Z terminator as
  required by the .gpx format.
*)
FUNCTION UtcNow(fractionDigits: integer= 0): STRING;

begin
  result := IsoFormatDateTime(UTC_Now(), IsoDateTTime, fractionDigits) + 'Z'
end { UtcNow } ;


(* Write the first part of a .gpx (XML) file, up to and including the end of
  the metadata.
*)
procedure WriteGpxHeaderA(var gpx: text; const comment: string= '');


  function flatten(const str: string): string;

  begin
    result := Trim(ReplaceStr(DelChars(str, #$0d), #$0a, ' '))
  end { flatten } ;


begin
  WriteLn(gpx, '<?xml version="1.0" encoding="UTF-8" standalone="no" ?>');
  WriteLn(gpx);
  WriteLn(gpx, '<gpx xmlns="http://www.topografix.com/GPX/1/1" xmlns:gpxx="http://www.garmin.com/xmlschemas/GpxExtensions/v3" xmlns:gpxtpx="http://www.garmin.com/xmlschemas/TrackPointExtension/v1" creator="Oregon 400t" version="1.1" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd http://www.garmin.com/xmlschemas/GpxExtensions/v3 http://www.garmin.com/xmlschemas/GpxExtensionsv3.xsd http://www.garmin.com/xmlschemas/TrackPointExtension/v1 http://www.garmin.com/xmlschemas/TrackPointExtensionv1.xsd">');
  WriteLn(gpx, '  <metadata>');
  WriteLn(gpx, '    <link href="https://www.topografix.com/gpx.asp">');
  Write(gpx, '      <text>KDG Instruments Ltd. ' +
                        flatten(ExtractFileName(ParamStr(0)) + ' ' + AboutText()));
  if comment <> '' then
    Write(gpx, '|' + Trim(comment));
  WriteLn(gpx, '</text>');
  WriteLn(gpx, '    </link>');
  WriteLn(gpx, '    <time>' + utcNow() + '</time>');
  WriteLn(gpx, '  </metadata>')
end { WriteGpxHeaderA } ;


(* Write mean position as a waypoint and standard deviation as an ellipse of
  routepoints.
*)
procedure WriteGpxStats(var gpx: text; meanLat, meanLong, bearing, sigmaLat, sigmaLong,
                                        minLat, maxLat, minLong, maxLong: extended);

{$define RECT_LIMIT  }
{ define RECT_SD     }
{ define RECT_SD_B   }
{ define ELLIPT_SD   }
{$define ELLIPT_SD_B }

type
  Tpt= record
         x, y: extended
       end;
  Tpts= array of TPt;

var
  pts: TPts;
  fudge: extended;
  i: integer;


  (* This preserves lengths but not angles/shapes. It's good enough.
  *)
  function rotate(const pts: TPts; degrees: double): TPts;

  var
    i: integer;
    s, c: extended;

  begin
    s := Sin(degrees * (2 * Pi) / 360);
    c := Cos(degrees * (2 * Pi) / 360);

    SetLength(result{%H-}, Length(pts));
    for i := 0 to High(pts) do begin
      result[i].x := (pts[i].x * c) - (pts[i].y * s);
      result[i].y := (pts[i].x * s) + (pts[i].y * c)
    end
  end { rotate } ;


  procedure drawRte(offsetLat, offsetLong: double; const pts: Tpts);

  var
    i: integer;

  begin
    for i := 0 to High(pts) do
      WriteLn(gpx, '    <rtept lat="', (offsetLat + pts[i].y):GLLP:GLLD, '" lon="', (offsetLong + pts[i].x):GLLP:GLLD, '"/>')
  end { drawRte } ;


begin

(* Add a waypoint to indicate the mean point.                                   *)

  WriteLn(gpx, '  <wpt lat="', meanLat:GLLP:GLLD, '" lon="', meanLong:GLLP:GLLD, '">');
  WriteLn(gpx, '    <name>Mean position</name>');
  WriteLn(gpx, '  </wpt>');

{$ifdef RECT_LIMIT }

(* Add a route (with the endpoints overlapping) to delineate the excursions.    *)

  WriteLn(gpx, '  <rte>');
  WriteLn(gpx, '    <name>Cartesian excursion limits</name>');
  SetLength(pts{%H-}, 5);
  pts[0].x := minLong;
  pts[0].y := maxLat;
  pts[1].x := maxLong;
  pts[1].y := maxLat;
  pts[2].x := maxLong;
  pts[2].y := minLat;
  pts[3].x := minLong;
  pts[3].y := minLat;
  pts[4] := pts[0];
  drawRte(0.0, 0.0, pts);
  WriteLn(gpx, '  </rte>');
{$endif RECT_LIMIT }

{$ifdef RECT_SD    }

(* Add a route (with the endpoints overlapping) to delineate the standard       *)
(* deviation where this is assumed to be oriented on the standard Cartesian     *)
(* (longitude/latitude) grid i.e. the bearing parameter is zero.                *)

  WriteLn(gpx, '  <rte>');
  WriteLn(gpx, '    <name>Cartesian standard deviation</name>');
  SetLength(pts, 5);
  pts[0].x := +sigmaLong;
  pts[0].y := +sigmaLat;
  pts[1].x := +sigmaLong;
  pts[1].y := -sigmaLat;
  pts[2].x := -sigmaLong;
  pts[2].y := -sigmaLat;
  pts[3].x := -sigmaLong;
  pts[3].y := +sigmaLat;
  pts[4] := pts[0];
  drawRte(meanLat, meanLong, pts);
  WriteLn(gpx, '  </rte>');
{$endif RECT_SD    }

{$ifdef RECT_SD_B  }

(* Add a route (with the endpoints overlapping) to delineate the standard       *)
(* deviation where this is assumed to be oriented relative to the bearing       *)
(* parameter (x parallel, y perpendicular).                                     *)

  WriteLn(gpx, '  <rte>');
  WriteLn(gpx, '    <name>Bearing-relative standard deviation</name>');
  SetLength(pts, 5);
  pts[0].x := +sigmaLong;
  pts[0].y := +sigmaLat;
  pts[1].x := +sigmaLong;
  pts[1].y := -sigmaLat;
  pts[2].x := -sigmaLong;
  pts[2].y := -sigmaLat;
  pts[3].x := -sigmaLong;
  pts[3].y := +sigmaLat;
  pts[4] := pts[0];
  drawRte(meanLat, meanLong, rotate(pts, bearing));
  WriteLn(gpx, '  </rte>');
{$endif RECT_SD_B  }

{$ifdef ELLIPT_SD  }

(* Add a route (with the endpoints overlapping) with the same orientation and   *)
(* area as the rectangular standard deviation.                                  *)

  fudge := 2.0 / Sqrt(Pi);              (* Correct area relative to rectangle   *)

  WriteLn(gpx, '  <rte>');
  WriteLn(gpx, '    <name>Cartesian standard deviation</name>');
  SetLength(pts, 31);
  for i := 0 to 30 do begin
    pts[i].x := sigmaLong * fudge * Cos(i * (2 * Pi / 30));
    pts[i].y := sigmaLat * fudge * Sin(i * (2 * Pi / 30));
  end;
  drawRte(meanLat, meanLong, pts);
  WriteLn(gpx, '  </rte>');
{$endif ELLIPT_SD    }

{$ifdef ELLIPT_SD_B  }

(* Add a route (with the endpoints overlapping) with the same orientation and   *)
(* area as the rectangular standard deviation where this is assumed to be       *)
(* oriented relative to the bearing parameter (x parallel, y perpendicular).    *)

  fudge := 2.0 / Sqrt(Pi);              (* Correct area relative to rectangle   *)

  WriteLn(gpx, '  <rte>');
  WriteLn(gpx, '    <name>Bearing-relative standard deviation</name>');
  SetLength(pts, 31);
  for i := 0 to 30 do begin
    pts[i].x := sigmaLong * fudge * Cos(i * (2 * Pi / 30));
    pts[i].y := sigmaLat * fudge * Sin(i * (2 * Pi / 30));
  end;
  drawRte(meanLat, meanLong, rotate(pts, bearing));
  WriteLn(gpx, '  </rte>');
{$endif ELLIPT_SD_B  }
end { WriteGpxStats } ;


(* Write the second part of a .gpx (XML) file, after which a sequence of
  trackpoints should follow.
*)
procedure WriteGpxHeaderB(var gpx: text; const name: string);

begin
  WriteLn(gpx, '  <trk>');
  WriteLn(gpx, '    <name>', name, '</name>');
  WriteLn(gpx, '    <trkseg>')
end { WriteGpxHeaderB } ;


(* Wrap up a .gpx file after a trackpoint sequence, after which some other
  sequence might follow.
*)
procedure WriteGpxFooterA(var gpx: text);

begin
  WriteLn(gpx, '    </trkseg>');
  WriteLn(gpx, '  </trk>')
end { WriteGpxFooterA } ;


(* Wrap up a .gpx file after all sequences.
*)
procedure WriteGpxFooterB(var gpx: text);

begin
  WriteLn(gpx, '</gpx>')
end { WriteGpxFooterB } ;


(* Decide on the content of a line. Junk simply represents stuff that we aren't
  going to manipulate, it's still significant as far as the format is concerned.
*)
function LineType(const s: string): TGpxLines;

begin
  if Pos(' <text>', s) > 0 then begin
    if (Pos('|', s) > 0) and (GpxComment = '') then begin
      GpxComment := s;
      Delete(GpxComment, 1, Pos('|', GpxComment));
      SetLength(GpxComment, Pos('<', GpxComment) - 1)
    end;
    exit(GpxText)
  end;
  if Pos(' <trkpt lat="', s) > 0 then
    exit(GpxPos);
  if Pos(' <time>2', s) > 0 then
    exit(GpxTime);
  result := GpxJunk
end { LineType } ;


(* Advance to and read the <text> lines. Return false if they differ.
*)
function TextSame(var a, b: text): boolean;

var
  lA, lB: string;

begin
  result := false;
  repeat
    ReadLn(a, lA)
  until Eof(a) or (LineType(lA) = GpxText);
  repeat
    ReadLn(b, lB)
  until Eof(b) or (LineType(lB) = GpxText);
  if not (Eof(a) or Eof(b)) then
    result := lA = lB
end { TextSame } ;


(* Read the significant lines from a trackpoint.
*)
function ReadTrkPt(var gpx: text; out trkPt, time: string): boolean;

var
  scratch1, scratch2: string;

begin
  repeat
    ReadLn(gpx, scratch1)
  until Eof(gpx) or (LineType(scratch1) = GpxPos);
  if Eof(gpx) then
    exit(false);
  repeat
    ReadLn(gpx, scratch2)
  until Eof(gpx) or (LineType(scratch2) = GpxTime);
  if Eof(gpx) then
    exit(false);
  trkPt := scratch1;
  time := scratch2;
  result := true
end { ReadTrkPt } ;


(* Advance both files to the next time, return false at EOF.
*)
function Next(var a: text; var trkA, timA: string; var b: text; var trkB, timB: string): boolean;

begin
  if not (ReadTrkPt(a, trkA, timA) and ReadTrkPt(b, trkB, timB)) then
    exit(false);
  result := true
end { Next } ;


(* Assume both files are positioned on times, but they might not match (in
  particular, near the start of the captured data). Advance until they match, or
  return false if not.
*)
function Jiggle(var a: text; var trkA, timA: string; var b: text; var trkB, timB: string): boolean;

begin
  repeat
    case Sign(CompareStr(timA, timB)) of
       -1: if not ReadTrkPt(a, trkA, timA) then
             exit(false);
        0: exit(true);
       +1: if not ReadTrkPt(b, trkB, timB) then
             exit(false)
    end
  until false
end { Jiggle } ;


(* I'm tempted to make this into a record helper but it would make the program
  even less portable than it currently is for very little advantage.
*)
procedure InitWelford(out w: Twelford);

begin
  with w do begin
    samples := 0;
    current := 0.0;
    delta := 0.0;
    mean := 0.0;
    mean2 := 0.0
  end
end { InitWelford };


(* Update the running data structure to take a new value into account.
*)
function UpdateWelford(var w: Twelford; const datum: extended): extended;

begin
  with w do begin
    current := datum;
    if samples = 0 then                 (* Guess initial mean                   *)
      mean := datum;
    samples += 1;
    delta := datum - mean;
    mean := mean + (delta / samples);
    mean2 := mean2 + (delta * (datum - mean));
    result := mean
  end
end { UpdateWelford } ;


(* Extract the final values from the Welford data structure.
*)
procedure ExtractWelford(const w: Twelford; out mean, sigma: extended);

begin
  mean := w.mean;
  sigma := Sqrt(w.mean2 / (w.samples - 1))
end { ExtractWelford } ;


(* Convert a small North-South distance lat expressed in degrees to metres,
  assuming that the approximate latitude is meanLat and that the Earth is
  spherical.
*)
function LatToMetres(lat: extended; meanLat: extended= 0.0; meanLong: extended= 0.0): double;

begin
  result := Abs(lat * 111200)           (* Approximation for spherical Earth    *)
end { LatToMetres } ;


(* Convert a small East-West distance long expressed in degrees to metres,
  assuming that the approximate latitude is meanLat and that the Earth is
  spherical.
*)
function LongToMetres(long, meanLat: extended; meanLong: extended= 0.0): double;

var
  radians: extended;

begin
  radians := meanLat * (2 * Pi) / 360;
  result := Abs(long * 111200 * Cos(radians))
end { LongToMetres } ;


(* Convert a small pair of distances lat and long expressed in degrees to metres,
  assuming that the approximate position in given by meanLat and meanLong and
  that the Earth is spherical.
*)
function LatLongToMetres(lat, long, meanLat: extended; meanLong: extended= 0.0): double;

begin
  result := Sqrt(Sqr(LatToMetres(lat, meanLat)) + Sqr(LongToMetres(long, meanLat)))
end { LatLongToMetres } ;


(* Convert a small distance expressed in degrees along a bearing in degrees (+ve
  is clockwise i.e. East of North) to metres, assuming that the approximate
  position is given by meanLat and meanLong and that the Earth is spherical.

  This assumes that the parameter is a quasi-distance derived from combining
  Cartesian latitude and longitude, which I have noted elsewhere as being dodgy.
  I expect the result, however, to be reasonable, so it doesn't need to be
  qualified by ~ as an approximation.
*)
function DegsToMetres(degs, bearing, meanLat: extended; meanLong: extended= 0.0): double;

var
  radians, lat, long: extended;

begin
  radians := bearing * (2 * Pi) / 360;
  lat := degs * Cos(radians);
  long := degs * Sin(radians);
  result := LatLongToMetres(lat, long, meanLat, meanLong)
end { DegsToMetres } ;


begin
  Assert(Abs(LatLongToMetres(1.0, 0.0, 50.0) - LatToMetres(1.0, 50.0)) < 0.01);
  Assert(Abs(LatLongToMetres(0.0, 1.0, 50.0) - LongToMetres(1.0, 50.0)) < 0.01);
  Assert(Abs(DegsToMetres(1.0, 0.0, 50.0) - LatToMetres(1.0, 50.0)) < 0.01);
  Assert(Abs(DegsToMetres(1.0, 90.0, 50.0) - LongToMetres(1.0, 50.0)) < 0.01)
end.

