(* Lazarus+FPC 2.2.6+3.2.2 on Linux Lazarus+FPC 2.2.6+3.2.2 on Linux Lazarus+FP *)

program adsb1090_to_gpx;

(* Process input records typically generated by Match1090 which are assumed to  *)
(* contain time, altitude, latitude and longitude. Generate a .gpx file which   *)
(* may be imported into Google Earth, QGIS and so on.                           *)
(*                                                                              *)
(* $ cat 1090.txt | match1090 --match=hex:4D3520 --strict=lat,long,alt --nl | \ *)
(*      match1090 '--match=alt:-325<>750' --strict=lat,long,alt --utc --nl | \  *)
(*      adsb1090_to_gpx > 1090.gpx                                              *)
(*                                                                              *)
(* Referring to the above, the reported barometric altitude appears be low by   *)
(* about 325'. It would be nice to be able to correct for this automatically by *)
(* monitoring the reported altitude when the plane was stationary, but the      *)
(* bottom line is that it's the pilot's responsibility to get this right and    *)
(* such an enormous reporting error could easily have safety implications.      *)
(*                                                                              *)
(*                                                              MarkMLl         *)

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, StrUtils, Params1090, GpxCommon;

label
  reassignInput;

type
  vertex= record
            lat, long: double
          end;

var
  commandline: TCommandLine;
  writtenHeaders: boolean= false;
  line: ansistring;
  lines: TStringList;
  icaoAddress: ansistring= 'ADS-B';
  vertices: array of vertex;


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
      SetLength(param, Pos(' ', param) - 1);
      Delete(line, 1, Length(param) + 1);
      line := Trim(line);
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


procedure writeBounds(var t: text);

var
  i: integer;

begin
  WriteLn(t, '  <rte>');
  for i := 0 to Length(vertices) - 1 do begin
    Write(t, '    <rtept');
    Write(t, ' lat="', vertices[i].lat:7:5, '"');
    Write(t, ' lon="', vertices[i].long:7:5, '"');
    WriteLn(t, '/>')
//    WriteLn(t, '></rtept>')
  end;
  WriteLn(t, '  </rte>')
end { writeBounds } ;


procedure emptyLine;

var
  i: integer;
  time: string= '';
  alt: string='';
  lat: string='';
  long: string='';
  score: integer= 0;

begin
  try
    for i := 0 to lines.Count - 1 do
      if Pos('Altitude:', lines[i]) > 0 then begin
        time := lines[i];
        SetLength(time, Pos(' Alt', time) - 1);
        time := AnsiReplaceStr(time, ' ', 'T');
//        if Pos('Z', time) = 0 then
//          time += 'Z';
        score += 1;
        alt := lines[i];
        SetLength(alt, Pos(' ft', alt) - 1);
        Delete(alt, 1, Pos(': ', alt) + 1);
        score += 1
      end else
        if Pos('latitude:', lines[i]) > 0 then begin
          lat := lines[i];
          SetLength(lat, Pos(' (', lat) - 1);
          Delete(lat, 1, Pos(': ', lat) + 1);
          score += 1
        end else
          if Pos('longitude:', lines[i]) > 0 then begin
            long := lines[i];
            SetLength(long, Pos(' (', long) - 1);
            Delete(long, 1, Pos(': ', long) + 1);
            score += 1
          end;
    if score = 4 then begin
      WriteLn(output, '      <trkpt lat="', lat, '" lon="', long, '">');
      WriteLn(output, '        <ele>', (StrToFloat(alt) * 0.3048):5:3, '</ele>');
      WriteLn(output, '        <time>', time, '</time>');
      WriteLn(output, '      </trkpt>')
    end
  finally
    lines.Clear
  end
end { emptyLine } ;


procedure nonEmptyLine;

begin
  lines.append(line);
  if (not writtenHeaders) and AnsiContainsText(line, 'ICAO address: ') then begin
    Delete(line, 1, Pos(': ', line));
    line := Trim(line);
    SetLength(line, Pos(' ', line) - 1);
    icaoAddress := line
  end
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

  case ParamCount(commandline) of
    0: ;
    1: begin
reassignInput:
         Close(input);
         AssignFile(input, ParamStr(commandline, 1));
         reset(input)
       end
  otherwise
    Close(output);
    AssignFile(output, ParamStr(commandline, 2));
    reset(output);
    goto reassignInput
  end;
  lines := TStringList.Create;
  SetLength(vertices, 0);
  while not Eof() do begin
    ReadLn(line);
    if Pos('# BOUNDS', line) = 1 then
      parseBounds(line)
    else
      if line = '' then begin
        if not writtenHeaders then begin
          WriteGpxHeaderA(output);
          writeBounds(output);
          WriteGpxHeaderB(output, icaoAddress)
        end;
        writtenHeaders := true;
        emptyLine
      end else
        nonEmptyLine                    (* Saves ICAO address when first seen   *)
  end;
  if writtenHeaders then begin
    WriteGpxFooterA(output);
    WriteGpxFooterB(output)
  end;
  FreeAndNil(lines)
end.

