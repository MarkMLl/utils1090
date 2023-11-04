(* Lazarus+FPC 2.2.6+3.2.2 on Linux Lazarus+FPC 2.2.6+3.2.2 on Linux Lazarus+FP *)

program ticket1090;

(* Expect the input to be piped records, originally emitted by Dump1090 but     *)
(* aggressively filtered such that they typically indicate that an aircraft has *)
(* strayed into a prohibited area at too low an altitude.                       *)
(*                                                                              *)
(*  $ dump1090 | match1090 '--match=alt:<750' --strict=hex,lat,long,alt --utc \ *)
(*              --nl | poly1090 51.8876,0.1545 51.8941,0.1483 51.9026,0.1720 \  *)
(*              51.8929,0.1790 | ticket1090 --gpx                               *)
(*                                                                              *)
(* Start accumulating ADSB records when incursion is detected, and save them as *)
(* a file with a name based on the timestamp some short time after it leaves.   *)
(* Output will be in Dump1090 and/or GPX format, with normal text (stdout)      *)
(* output reserved for status messages.                         MarkMLl         *)

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, StrUtils, Params1090, GpxCommon, IniFilesAbout;

type
  TIncursion= class(TObject)
              strict private
                hex: ansistring;
                fileBase: ansistring;
                textFile: text;
                gpxFile: text;
                firstSeen, lastSeen: qword;
                procedure writeText(var t: text; const r: TStringList);
                procedure writeGpx(var g: text; const r: TStringList);
              public
                constructor Create(const hexId: ansistring; const firstRecord: TStringList);
                destructor Destroy; override;
                procedure Append(const currentRecord: TStringList);
                function Expired(): boolean;
                property WhoAmI: ansistring read hex;
              end;
  vertex= record
            lat, long: double
          end;

var
  commandline: TCommandLine;
  incursions: TStringList;
  line: ansistring;
  dateTimeTxt: ansistring;
  dateTimeNow: qword;
  lines: TStringList;
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


(* Parse an (approximately) ISO-format date and time to an (approximately) unix
  seconds value for elapsed-time calculations. Under no circumstances attempt
  to correct for non-UTC timezone etc.
*)
function isoToUnix(const iso: ansistring): qword;

(* We are confident that one of the records will look like                      *)
(*                                                                              *)
(* 2023-10-17 12:02:25.35 ICAO Address: 4d3520 (ADS-B, non-transponder)         *)
(* 2023-10-17T12:02:25.35 ICAO Address: 4d3520 (ADS-B, non-transponder)         *)
(* 2023-10-17T12:02:25.35Z ICAO Address: 4d3520 (ADS-B, non-transponder)        *)
(*                                                                              *)
(* 12345678901234567890123456789012345678901234567890                           *)
(*          1         2         3         4         5                           *)

begin
  result := (StrToInt(Copy(iso, 1, 4)) - 1970) * Round(365.25 * 24 * 60 * 60);
  result += StrToInt(Copy(iso, 6, 2)) * Round((365.25 / 12) * 24 * 60 * 60);
  result += StrToInt(Copy(iso, 9, 2)) * (24 * 60 * 60);
  result += StrToInt(Copy(iso, 12, 2)) * (60 * 60);
  result += StrToInt(Copy(iso, 15, 2)) * (60);
  result += StrToInt(Copy(iso, 18, 2))          (* Ignore mSec here             *)
end { isoToUnix } ;


(* Create an object associated with the incursion. This open an output file and
  writes the first record.
*)
constructor TIncursion.Create(const hexId: ansistring; const firstRecord: TStringList);

var
  dateAndTime: ansistring= '';
  i: integer;

begin
  inherited Create;
  hex := hexId;

(* Generate a filename. We are confident that one of the records will look like *)
(*                                                                              *)
(* 2023-10-17 12:02:25.35 ICAO Address: 4d3520 (ADS-B, non-transponder)         *)
(* 2023-10-17T12:02:25.35 ICAO Address: 4d3520 (ADS-B, non-transponder)         *)
(* 2023-10-17T12:02:25.35Z ICAO Address: 4d3520 (ADS-B, non-transponder)        *)
(*                                                                              *)
(* 12345678901234567890123456789012345678901234567890                           *)
(*          1         2         3         4         5                           *)

  for i := 0 to firstRecord.Count - 1 do
    if Pos('ICAO Address: ', firstRecord[i]) > 0 then begin
      dateAndTime := Copy(firstRecord[i], 1, 19);       (* Ignore mSec          *)
      fileBase := AnsiReplaceStr(dateAndTime, ' ', '_') + '_' + hexId;
      break
    end;
  firstSeen := isoToUnix(dateAndTime);
  lastSeen := firstSeen;
  if commandline.option.gpx in [0, 1] then begin
    Assign(textFile, fileBase + '.txt');
    Rewrite(textFile);
    writeText(textFile, firstRecord)
  end;
  if commandline.option.gpx in [1, 2] then begin
    Assign(gpxFile, fileBase + '.gpx');
    Rewrite(gpxFile);
    WriteGpxHeaderA(gpxFile);
    writeBounds(gpxFile);
    WriteGpxHeaderB(gpxFile, hex);
    writeGpx(gpxFile, firstRecord)
  end
end { TIncursion.Create } ;


(* Flush and close the output file associated with the incursion.
*)
destructor TIncursion.Destroy;

const
  cutoff= 5;

begin
  if commandline.option.gpx in [0, 1] then
    CloseFile(textFile);
  if commandline.option.gpx in [1, 2] then begin
    WriteGpxFooterA(gpxFile);
    WriteGpxFooterB(gpxFile);
    CloseFile(gpxFile)
  end;

// TODO : Needs a test that the incursion has lasted more than (say) five secs.
// This is mostly to support a situation where concave (reentrant) polygons are
// known to be reliable, and two areas have been intentionally joined by a
// "wasp waist" of negligible thickness in lieu of separate definition of more
// than one.

// Code below not tested, pending the new flying season. Could a --utc and --iso
// decision be made by inspecting incoming data, to keep the timestamping
// consistent?

(*  if lastSeen - firstSeen < cutoff then begin
    Write(IsoFormatDateTime(UTC_Now(), IsoDateTTime, 2) + 'Z');
    Write(' Discarding ticket for ', lastSeen - firstSeen, '-sec incursion by ',
                                        TIncursion(incursions.Objects[0]).WhoAmI, '.');
    if commandline.option.gpx in [0, 1] then
      DeleteFile(fileBase + '.txt');
    if commandline.option.gpx in [1, 2] then
      DeleteFile(fileBase + '.gpx')
  end; *)
  inherited destroy
end { TIncursion.Destroy } ;


(* Write a record associated with the incursion.
*)
procedure TIncursion.Append(const currentRecord: TStringList);

var
  dateAndTime: ansistring;

begin

(* We are confident that the records will look like                             *)
(*                                                                              *)
(* 2023-10-17 12:02:25.35 ICAO Address: 4d3520 (ADS-B, non-transponder)         *)
(* 2023-10-17T12:02:25.35 ICAO Address: 4d3520 (ADS-B, non-transponder)         *)
(* 2023-10-17T12:02:25.35Z ICAO Address: 4d3520 (ADS-B, non-transponder)        *)
(*                                                                              *)
(* 12345678901234567890123456789012345678901234567890                           *)
(*          1         2         3         4         5                           *)

  dateAndTime := Copy(currentRecord[0], 1, 22);
  lastSeen := isoToUnix(dateAndTime);
  if commandline.option.gpx in [0, 1] then
    writeText(textFile, currentRecord);
  if commandline.option.gpx in [1, 2] then
    writeGpx(gpxFile, currentRecord)
end { TIncursion.Append } ;


procedure TIncursion.writeText(var t: text; const r: TStringList);

var
  i: integer;

begin
  for i := 0 to r.Count - 1 do
    WriteLn(t, r[i]);
  WriteLn(t)
end { TIncursion.writeText } ;


procedure TIncursion.writeGpx(var g: text; const r: TStringList);

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
      WriteLn(g, '      <trkpt lat="', lat, '" lon="', long, '">');
      WriteLn(g, '        <ele>', (StrToFloat(alt) * 0.3048):5:3, '</ele>');
      WriteLn(g, '        <time>', time, '</time>');
      WriteLn(g, '      </trkpt>')
    end
  finally
    lines.Clear
  end
end { TIncursion.writegpx } ;


(* If the most recent record associated with the incursion is more than a minute
  old then return true.
*)
function TIncursion.Expired(): boolean;

begin
  result := dateTimeNow - lastSeen > 1.0 / MinsPerDay
end { TIncursion.Expired } ;


procedure emptyLine;

const
  lastFlush: qword= 0;                  (* Static variable                      *)

var
  hex: ansistring= '';
  dateTime: ansistring;
  i, index: integer;

begin
  if lines.Count = 0 then
    exit;

(* The records should look like                                                 *)
(*                                                                              *)
(* 2023-10-17 12:02:25.35 ICAO Address: 4d3520 (ADS-B, non-transponder)         *)
(* 2023-10-17T12:02:25.35 ICAO Address: 4d3520 (ADS-B, non-transponder)         *)
(* 2023-10-17T12:02:25.35Z ICAO Address: 4d3520 (ADS-B, non-transponder)        *)
(*                                                                              *)
(* 12345678901234567890123456789012345678901234567890                           *)
(*          1         2         3         4         5                           *)

  if (not (lines[0][1] in ['0'..'9'])) or (lines[0][5] <> '-') or
                                        not (lines[0][11] in [' ', 'T']) then begin
    WriteLn(stderr, '# Input lines must have ISO-format date and time');
    ExitCode := 5;
    Halt
  end;
  try

(* Get the hex code which we use as an index for storage. Also record the       *)
(* timestamp in lieu of IsoNow() etc. since we might be looking at archived     *)
(* data.                                                                        *)

    for i := 0 to lines.Count - 1 do
      if Pos('ICAO Address: ', lines[i]) > 0 then begin
        hex := lines[i];
        Delete(hex, 1, Pos(': ', hex) + 2);
        SetLength(hex, Pos(' ', hex) - 1);
        dateTime := lines[i];
        SetLength(dateTime, 22);
        dateTimeTxt := dateTime;
        dateTimeNow := isoToUnix(dateTime);
        break
      end;
    if hex = '' then begin
      WriteLn(stderr, '# Input lines must have ICAO address (hex identifier)');
      ExitCode := 5;
      Halt
    end;

(* If we've got this far then we've got enough information to identify the      *)
(* storage (i.e. file etc.) associated with the intruder.                       *)

    index := incursions.IndexOf(hex);
    if index < 0 then begin
      WriteLn(dateTime, ' Creating ticket for incursion by ', hex, '.');
      index := incursions.Add(hex);
      incursions.Objects[index] := TIncursion.Create(hex, lines)
    end else
      TIncursion(incursions.Objects[index]).Append(lines);

(* Is it more than a minute since we last checked for expired incursions?       *)
(* Assume that freeing the incursion object flushes and closes the file etc.    *)

    if dateTimeNow - LastFlush > 1.0 / MinsPerDay then begin
      for i := incursions.Count - 1 downto 0 do
        if TIncursion(incursions.Objects[i]).Expired then begin
          WriteLn(dateTime, ' Committing ticket for incursion by ',
                                        TIncursion(incursions.Objects[i]).WhoAmI, '.');
          TIncursion(incursions.Objects[i]).Free;
          incursions.Delete(i);
        end;
      lastFlush := dateTimeNow
    end
  finally
    lines.Clear
  end
end { emptyLine } ;


(* Finalise everything. Messages won't be in chronological order, sorry.
*)
procedure wrapup;

var
  i: integer;

begin
  for i := incursions.Count - 1 downto 0 do begin
    WriteLn(dateTimeTxt, ' Committing ticket for incursion by ',
                                        TIncursion(incursions.Objects[i]).WhoAmI, '.');
    TIncursion(incursions.Objects[i]).Free;
    incursions.Delete(i)
  end
end { wrapup } ;


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

  if ParamCount(commandline) > 0 then begin
    Close(input);
    AssignFile(input, ParamStr(commandline, 1));
    reset(input)
  end;
  lines := TStringList.Create;
  incursions := TStringList.Create;
  incursions.Sorted := true;
  SetLength(vertices, 0);
  while not Eof() do begin
    ReadLn(line);
    if Pos('# BOUNDS', line) = 1 then
      parseBounds(line)
    else
      if line = '' then
        emptyLine
      else
        nonEmptyLine
  end;
  wrapUp;
  FreeAndNil(incursions);
  FreeAndNil(lines)
end.

