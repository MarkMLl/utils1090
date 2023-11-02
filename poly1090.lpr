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

// TODO : I have a niggling feeling that inPoly() could be improved.
// Possibly by making sure the vertices are sorted (relative to the centroid?)
// and retaining the sign of each angle. If I am correct, this would result in
// the total either being (very close to) 360 or (very close to) 0 degrees.
//
// HOWEVER: I suspect that this approach would fail under circumstances related
// to the "Illumination Problem" described by Penrose, Tokarsky and others.

// Test with courses flown in at arbitrary angles, place it on the Meridian and
// so on.

  for i := 0 to Length(vertices) - 2 do begin
    deg1 := ArcTan2(vertices[i].lat - here.lat, vertices[i].long - here.long) * (180 / Pi);
    deg2 := ArcTan2(vertices[i + 1].lat - here.lat, vertices[i + 1].long - here.long) * (180 / Pi);
    subtends := Abs(deg1 - deg2);
    if subtends > 180.0 then
      subtends := 360.0 - subtends;
    total += subtends;
// WriteLn(deg1:12:5, deg2:12:5, subtends:12:5);
  end;
// WriteLn(total:12:5);

(* If the total angle is 360 degrees, then we must be inside the polygon. Any   *)
(* smaller angle is outside, but I don't know quite how much slop I should      *)
(* allow... testing suggests that the transition is abrupt even without taking  *)
(* the sign of the angles into account.                                         *)

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
    subtends := Abs(rad1 - rad2);
    if subtends > Pi then
      subtends := (2 * Pi) - subtends;
    total += subtends
  end;

(* If the total angle is 2Pi radians, then we must be inside the polygon. Any   *)
(* smaller angle is outside, but I don't know quite how much slop I should      *)
(* allow... testing suggests that the transition is abrupt even without taking  *)
(* the sign of the angles into account.                                         *)

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
  score: integer= 0;
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

    for i := 0 to lines.Count - 1 do
      if PosSeq(['CPR latitude: ', '', '.', ' '], lines[i] + ' ', markers) > 0 then begin
//        scratch := Copy(lines[i], Pos(': ', lines[i]) + 2, 999);
//        SetLength(scratch, Pos(' ', scratch) - 1);
        scratch := Copy(lines[i], markers[1], markers[3] - markers[1]);
        here.lat := StrToFloat(scratch);
        score += 1
      end else
        if PosSeq(['CPR longitude: ', '', '.', ' '], lines[i] + ' ', markers) > 0 then begin
//          scratch := Copy(lines[i], Pos(': ', lines[i]) + 2, 999);
//          SetLength(scratch, Pos(' ', scratch) - 1);
          scratch := Copy(lines[i], markers[1], markers[3] - markers[1]);
          here.long := StrToFloat(scratch);
          score += 1
        end;
    if score <> 2 then
      exit;                             (* Via finally clause                   *)

(* Is the current aircraft position, defined by its latitude and longitude,     *)
(* inside the polygon?                                                          *)

    if inPoly(here) then begin
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
(* if parsePoly() swallows two parameters (where each is a comma-separated pair *)
(* of ordinates) then coordinates will be 2. If ParamCount() is 2 then there is *)
(* no input filename, and so on.                                                *)

  coordinates := parsePoly();
  if coordinates < 2 then begin
    WriteLn(stderr);
    halt
  end;
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
  dumpPoly(output);
  while not Eof() do begin
    ReadLn(line);
    if line = '' then
      emptyLine
    else
      nonEmptyLine
  end;
  FreeAndNil(lines)
end.

