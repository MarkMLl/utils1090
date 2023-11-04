(* Lazarus+FPC 2.2.6+3.2.2 on Linux Lazarus+FPC 2.2.6+3.2.2 on Linux Lazarus+FP *)

program poly1090;

(* The commandline comprises a sequence of comma-separated latitude,longitude   *)
(* pairs with each ordinate given as a degree.decimal floating-point number. In *)
(* combination they describe the vertices (moving consistently in either a      *)
(* clockwise or anticlockwise direction) of a simple polygon i.e. where none of *)
(* the bounding edges intersect, if the final point is not the same as the      *)
(* first one then the polygon is closed automatically.                          *)
(*                                                                              *)
(* Assume that piped input comprises ADS-B records produced by Dump1090,        *)
(* normally filtered to represent a single plane plus possibly a limited height *)
(* range by Match1090. Records that refer to a point within the defined polygon *)
(* are output verbatim, other records are discarded. No correction is attempted *)
(* to compensate for altitude or the curvature of the Earth.                    *)
(*                                                                              *)
(*  $ cat 1090.txt | match1090 '--match=alt:<750' --strict=hex,lat,long,alt \   *)
(*              --utc --nl | poly1090 51.8876,0.1545 51.8941,0.1483 \           *)
(*              51.9026,0.1720 51.8929,0.1790                                   *)
(*                                                              MarkMLl         *)

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, StrUtils, Math, Params1090;

label
  reassignInput;

type
  vertex= record
            lat, long: double
          end;

var
  commandline: TCommandLine;
  line: ansistring;
  lines: TStringList;
  coordinates: integer= 0;
  vertices: array of vertex;


(* Parse comma-separated ordinate pairs from the command line, return the number
  safely processed or -1; in practice anything less than 2 is an error since
  they cannot describe a meaningful polygon. If there are at least two vertices
  then if necessary close them by appending the first to the end, then convert
  them to a list of number pairs.
*)
function parsePoly(): integer;

var
  param: ansistring;
  p1, p2: ansistring;
  vertexStrings: TStringList;
  i: integer;
  v: vertex;

begin
  result := 0;
  vertexStrings := TStringList.Create;
  try
    while result < ParamCount(commandline) do begin
      param := ParamStr(commandline, result + 1);
      if not ParseFloatPairStr(param, p1, p2) then
        break;
      vertexStrings.Append(p1 + ',' + p2);
      result += 1
    end;
    if vertexStrings.Count > 0 then begin

(* We have a good list of vertices. Make sure it's closed, this is done using a *)
(* text comparison (having already got rid of spurious leading and trailing     *)
(* zeros) since it will be easier than having to diagnose any "not quite the    *)
(* same" errors in binary floating point format.                                *)

      if vertexStrings[vertexStrings.Count - 1] <> vertexStrings[0] then
        vertexStrings.Append(vertexStrings[0]);

(* Parse it into an array.                                                      *)

      for i := 0 to vertexStrings.Count - 1 do begin
        v.lat := StrToFloat(ExtractDelimited(1, vertexStrings[i], [',']));
        v.long := StrToFloat(ExtractDelimited(2, vertexStrings[i], [',']));
        vertices := Concat(vertices, [v])
      end
    end
  finally
    vertexStrings.Free
  end
end { parsePoly } ;


(* Parse comma-separated ordinate pairs from the parameter, return the number
  safely processed or -1; in practice anything less than 2 is an error since
  they cannot describe a meaningful polygon. If there are at least two vertices
  then if necessary close them by appending the first to the end, then convert
  them to a list of number pairs.
*)
function parseBounds(line: ansistring): integer;

var
  param: ansistring;
  p1, p2: ansistring;
  vertexStrings: TStringList;
  i: integer;
  v: vertex;

begin
  result := 0;
  vertexStrings := TStringList.Create;
  try
    Delete(line, 1, Length('# BOUNDS'));
    line := Trim(line);
    while line <> '' do begin
      param := line;
      if Pos(' ', param) > 0 then begin
        SetLength(param, Pos(' ', param) - 1);
        Delete(line, 1, Length(param) + 1);
        line := Trim(line)
      end else
        line := '';
      if not ParseFloatPairStr(param, p1, p2) then
        break;
      vertexStrings.Append(p1 + ',' + p2);
      result += 1
    end;
    if vertexStrings.Count > 0 then begin

(* We have a good list of vertices. Make sure it's closed, this is done using a *)
(* text comparison (having already got rid of spurious leading and trailing     *)
(* zeros) since it will be easier than having to diagnose any "not quite the    *)
(* same" errors in binary floating point format.                                *)

      if vertexStrings[vertexStrings.Count - 1] <> vertexStrings[0] then
        vertexStrings.Append(vertexStrings[0]);

(* Parse it into an array.                                                      *)

      for i := 0 to vertexStrings.Count - 1 do begin
        v.lat := StrToFloat(ExtractDelimited(1, vertexStrings[i], [',']));
        v.long := StrToFloat(ExtractDelimited(2, vertexStrings[i], [',']));
        vertices := Concat(vertices, [v])
      end
    end
  finally
    vertexStrings.Free
  end
end { parseBounds } ;


(* The bounding polygon might be also used by a program which generates a .gpx
  file etc.
*)
procedure dumpPoly(var t: text);

var
  i: integer;

begin
  Write(t, '# BOUNDS');
  for i := 0 to Length(vertices) - 1 do
    Write(t, ' ', vertices[i].lat:7:5, ',', vertices[i].long:7:5);
  WriteLn(t);
  WriteLn(t)
end { dumpPoly } ;


(* Working from the current aircraft location, find the angle subtended by each
  edge of the polygon by looking at successive pairs of vertices. Assume that
  the current location is inside the polygon if the overall angle is 360 degrees.
*)
function inPolyDegs(here: vertex): boolean;

(* References: http://web.archive.org/web/20080812141848/http://local.wasp.uwa.edu.au/~pbourke/geometry/insidepoly/ *)
(* https://stackoverflow.com/questions/26076656/calculating-angle-between-two-points-java *)

const
  slop= 0.01;
  lLim= 360.0 - slop;
  uLim= 360.0 + slop;

var
  i: integer;
  deg1, deg2, subtends: double;
  total: double= 0.0;

begin

// Test case: assuming this as a command line:
//
// 50.8876,0.1545 50.8941,0.1483 50.9026,0.1720 50.8929,0.1790 40352D.txt
//
// this point is inside the polygon:

// here.lat := 50.891148;
// here.long := 0.153551;

// Measured angles are of the order of 145, 120, 55 and 100 degrees. Computed
// result is 360.0 degrees.

// Each of these points is outside the polygon:

// here.lat := 50.8811; // 25.44
// here.long := 0.1170;

// here.lat := 50.9045; // 125.91
// here.long := 0.1825;

// here.lat := 50.8963; // 61.04
// here.long := 0.1929;

// here.lat := 50.8718; // 45.86
// here.long := 0.1256;

(* Relative to the fixed point, determine the angle subtended by each edge of   *)
(* the polygon. Save this in degrees for relatively easy debugging.             *)

// Should test with courses flown in at arbitrary angles, place it on the Meridian
// and so on.

  for i := 0 to Length(vertices) - 2 do begin
    deg1 := ArcTan2(vertices[i].lat - here.lat, vertices[i].long - here.long) * (180 / Pi);
    deg2 := ArcTan2(vertices[i + 1].lat - here.lat, vertices[i + 1].long - here.long) * (180 / Pi);

// This from Alpine on the Lazarus forum.

    subtends := deg1 - deg2;
    while subtends > 180 do
      subtends := subtends - 360;
    while subtends < -180 do
      subtends := subtends + 360;

    total += subtends;
// WriteLn(deg1:12:5, deg2:12:5, subtends:12:5);
  end;
// WriteLn(total:12:5);

(* If the total angle is 360 degrees, then we must be inside the polygon. Any   *)
(* smaller angle is outside, but I don't know quite how much slop I should      *)
(* allow... testing suggests that the transition is abrupt.                     *)

  result := (total >= lLim) and (total < uLim)
// WriteLn('Score: ', total:12:5)
end { inPolyDegs } ;


(* Working from the current aircraft location, find the angle subtended by each
  edge of the polygon by looking at successive pairs of vertices. Assume that
  the current location is inside the polygon if the overall angle is 2Pi degrees.
*)
function inPolyRads(here: vertex): boolean;

(* References: http://web.archive.org/web/20080812141848/http://local.wasp.uwa.edu.au/~pbourke/geometry/insidepoly/ *)
(* https://stackoverflow.com/questions/26076656/calculating-angle-between-two-points-java *)

const
  slop= 0.01 * (2 * Pi) / 360.0;
  lLim= (2 * Pi) - slop;
  uLim= (2 * Pi) + slop;

var
  i: integer;
  rad1, rad2, subtends: double;
  total: double= 0.0;

begin

(* Relative to the fixed point, determine the angle subtended by each edge of   *)
(* the polygon. Save this in degrees for relatively easy debugging.             *)

// This variant might be slightly more efficient due to removing the conversion
// to degrees from the loop, but I suggest using the other one for debugging.

  for i := 0 to Length(vertices) - 2 do begin
    rad1 := ArcTan2(vertices[i].lat - here.lat, vertices[i].long - here.long);
    rad2 := ArcTan2(vertices[i + 1].lat - here.lat, vertices[i + 1].long - here.long);

// This from Alpine on the Lazarus forum.

    subtends := rad1 - rad2;
    while subtends > Pi do
      subtends := subtends - (2 * Pi);
    while subtends < -Pi do
      subtends := subtends + (2 * Pi);
    total += subtends
  end;

(* If the total angle is 2Pi radians, then we must be inside the polygon. Any   *)
(* smaller angle is outside, but I don't know quite how much slop I should      *)
(* allow... testing suggests that the transition is abrupt.                     *)

  result := (total >= lLim) and (total < uLim)
end { inPolyRads } ;


(* This from Niglo https://forum.lazarus.freepascal.org/index.php/topic,65082.msg495525.html
  It might possibly be very slightly more efficient, but this is overshadowed by
  I/O operations and I feel that any gain is offset by the loss of clarity.
*)
function PointInPolygon(here: vertex) : Boolean;

 // The code below is from Wm. Randolph Franklin <wrf@ecse.rpi.edu>
 // with some minor modifications for speed.  It returns 1 for strictly
 // interior points, 0 for strictly exterior, and 0 or 1 for points on
 // the boundary.
var
  I, J: Integer;
begin
  Result:=False;
   for I:=0 to pred(Length(vertices) - 1) do begin
    j := succ(i);
    if ((((vertices[i].lat<=here.lat) and (here.lat<vertices[j].lat)) or
      ((vertices[j].lat<=here.lat) and (here.lat<vertices[i].lat))) and
      (here.long<(vertices[j].long-vertices[i].long)*(here.lat-vertices[i].lat)/(vertices[j].lat-vertices[i].lat)+vertices[i].long)) then
      Result:=not Result;
  end;
end;


function inPoly(here: vertex): boolean; inline;

(* Testing suggests that there's no meaningful difference in speed, and that    *)
(* inPoly1() (using transcendentals) might actually perform better on a modern  *)
(* CPU than PointInPolygon() (using simple operations).                         *)

begin
//  result := inPolyDegs(here)
  result := inPolyRads(here)
//  result := PointInPolygon(here)
end { inPoly } ;


procedure emptyLine;

var
  ordinates: integer= 0;
  i, p: integer;
  scratch: ansistring;
  markers: TPosSeqLog;
  here: vertex;

begin
  try

(* Do we have latitude and longitude lines both containing a valid floating     *)
(* point number? If not then there's nothing more to do.                        *)
(*                                                                              *)
(* I wrote the PosSeq() function specifically for this job. It was quite a lot  *)
(* of work to get right, but I think it is going to be more efficient than      *)
(* either repeated use of Str() or using a regex.                               *)

(* 2023-10-22 08:38:19.95Z CPR latitude: 50.88025 (44395)                       *)
(* 123456789012345678901234567890123456789012345678901234567890                 *)
(*          1         2         3         4         5                           *)

    for i := 0 to lines.Count - 1 do begin
      if PosSeq(['CPR latitude: ', '', '.', ' '], lines[i] + ' ', markers) > 0 then begin
//        scratch := Copy(lines[i], Pos(': ', lines[i]) + 2, 999);
//        SetLength(scratch, Pos(' ', scratch) - 1);
        scratch := Copy(lines[i], markers[1], markers[3] - markers[1]);
        here.lat := StrToFloat(scratch);
        ordinates += 1
      end else
        if PosSeq(['CPR longitude: ', '', '.', ' '], lines[i] + ' ', markers) > 0 then begin
//          scratch := Copy(lines[i], Pos(': ', lines[i]) + 2, 999);
//          SetLength(scratch, Pos(' ', scratch) - 1);
          scratch := Copy(lines[i], markers[1], markers[3] - markers[1]);
          here.long := StrToFloat(scratch);
          ordinates += 1
        end;
      if ordinates > 1 then
        break
    end;
    if ordinates <> 2 then
      exit;                             (* Via finally clause                   *)

(* Is the current aircraft position, defined by its latitude and longitude,     *)
(* inside the polygon?                                                          *)

    if inPoly(here) xor commandline.option.knot then begin
      for i := 0 to lines.Count - 1 do
        WriteLn( { 'IN  ', } lines[i]);
      WriteLn
    end else begin
{      for i := 0 to lines.Count - 1 do
        WriteLn('OUT  ', lines[i]);
      WriteLn }
    end
  finally
    lines.Clear
  end
end { emptyLine } ;


procedure nonEmptyLine;

begin
  lines.append(line)
end { nonEmptyLine } ;


begin
  if not ParseCommandline(commandline, {$I %FILE% }) then begin;
    WriteLn(stderr);
    halt
  end;
//  DumpCommandline(commandline);

(* This program is expected to normally process piped input, but being able to  *)
(* read an ordinary file makes debugging easier (and if I didn't expect         *)
(* debugging to be an issue, I'd probably use Perl for the job).                *)
(*                                                                              *)
(* If parsePoly() swallows three parameters (where each is a comma-separated    *)
(* pair of ordinates) then coordinates will be 3. Fewer than 3 makes no sense,  *)
(* unless in the future I want to do something like defining a circle (as a     *)
(* series of segments, so that it can be stored as a route in a .gpx file). If  *)
(* ParamCount() is 3 then there is no input filename, and so on.                *)

  coordinates := parsePoly();
  case ParamCount(commandline) - coordinates of
    0: ;
    1: begin
reassignInput:
         Close(input);
         AssignFile(input, ParamStr(commandline, coordinates + 1));
         reset(input)
       end
  otherwise
    Close(output);
    AssignFile(output, ParamStr(commandline, coordinates + 2));
    reset(output);
    goto reassignInput
  end;
  lines := TStringList.Create;
  if Length(vertices) <> 0 then
    dumpPoly(output);
  while not Eof() do begin
    ReadLn(line);

(* I don't entirely know what the use case of specifying the bounding polygon   *)
(* in the input file is going to be, other than testing odd shapes and tracks   *)
(* generated by MakeTrack1090.                                                  *)

    if Pos('# BOUNDS', line) = 1 then begin
      parseBounds(line);
      dumpPoly(output)
    end else begin
      if Length(vertices) = 0 then begin
        WriteLn(stderr);
        halt
      end;
      if line = '' then
        emptyLine
      else
        nonEmptyLine
    end
  end;
  FreeAndNil(lines)
end.

