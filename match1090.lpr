(* Lazarus+FPC 2.2.6+3.2.2 on Linux Lazarus+FPC 2.2.6+3.2.2 on Linux Lazarus+FP *)

program Match1090;

(* Expect piped text from Dump1090, or a file specified on the command line.    *)
(* Accumulate intact records, and look for a field specified using the --match  *)
(* option with a value which is a case-insensitive header name from the output  *)
(* of Dump1090 --interactive followed by a colon and a text string. If the      *)
(* record matches and is of the appropriate type, output the content of the     *)
(* fields specified by --show, if the --once option is present then terminate.  *)
(*                                                                              *)
(* Each field will be presented on its own line, optionally preceded by the UTC *)
(* or local time (--utc or --time option respectively) and always preceded by   *)
(* the field name. It may be followed by a blank line if --nl is specified.     *)
(*                                                                              *)

// --tee and --filter need much more testing. To a very large extent they have
// been obsoleted by improvments to the --show and --strict options.

(* Output will be on stdout, unless the --tee option is set in which case it    *)
(* will be on stderr. If the --tee option is not set the original records-      *)
(* whether matched or unmatched- will be discarded, if the --tee option is set  *)
(* and the --filter option is not set then all input will be passed through on  *)
(* stdout allowing for daisychained programs to operate simultaneously. If the  *)
(* --filter option is set then only matched records will be output.             *)
(*                                                                              *)
(* For example:                                                                 *)
(*                                                                              *)

// See doHelp3() for revised examples. I'm leaving these pending revisiting
// --tee and --filter.

(*  $ match1090 --once --match Flight,GBTIT --show Hex < 1090.txt               *)
(*                                                                              *)
(*  $ cat 1090.txt | match1090 --utc --match Hex:4D3520 --show Alt --tee | \    *)
(*                  match1090 --utc --match Hex:4D3520 --show Lat --tee | \     *)
(*                  match1090 --utc --match Hex:4D3520 --show Long -nl          *)
(*                                                                              *)
(*  $ cat 1090.txt | match1090 --match Hex:4D3520 --show Lat --tee --filter | \ *)
(*                  match1090 --show Long --tee | match1090 --show Alt          *)
(*                                                                              *)
(*  $ cat 1090.txt | match1090 --utc --match=Hex:4D3520 --show=alt,lat,long     *)
(*                                                                              *)
(* However in all cases note that different fields might come from different    *)
(* records, so timestamping etc. might be slightly erratic.                     *)
(*                                                                              *)
(* Note https://github.com/nasa/cpr for background reading.     MarkMLl         *)

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, StrUtils, Params1090, IniFilesAbout;

var
  commandline: TCommandLine;
  line: ansistring;
  lines: TStringList;


procedure emptyLine;

var
  i, j: integer;
  alreadyTimestamped: boolean= false;
  match: boolean= false;
  show: boolean= false;
  timeline: string= '';
  timestamp: string= '';
  ts: Extended;


  (* Test whether a field name is present in a line. If --strict and the field
    name appears to indicate latitude or longitude then also check for a decimal
    point to validate the value.
  *)
  function testHaveField(const fld, line: string): boolean;

  begin
    case fld of
      '_HOG_': result := testHaveField('CPR type:', line) or testHaveField('Air/Ground:', line)
    otherwise
      result := AnsiStartsStr(fld, line);
      if result and commandline.option.strict and
                ((Pos('atitude', fld) > 0) or (Pos('ongitude', fld) > 0)) then
        result := Pos('.', line) > 0
    end
  end { testHaveField } ;


  (* There is a ground correction, rewrite any altitude lines. This is done
    after a record has been matched and leaves the original plus correction in
    braces; the match might (or might not) have involved the ground correction.

    This combination allows, for example, a specific aircraft to be selected
    and have all its altitudes corrected, then at a later stage its position
    may be checked for incursion into a specified polygon below the permitted
    altitude.
  *)
  procedure rewriteAltitude;

  var
    i, j, k: integer;
    line, val: ansistring;
    val2: integer;

  begin
    for i := 0 to lines.Count - 1 do begin
      line := lines[i];
      j := Pos('Altitude: ', line);
      k := Pos(' ft ', line);
      if j * k = 0 then
        continue;
      j += Length('Altitude: ');
      val := Copy(line, j, k - j);
      Delete(line, j, k - j);
      val2 := StrToInt(val) + -commandline.option.ground;
      line += ' {' + val + '+' + IntToStr(-commandline.option.ground) + '}';
      Insert(IntToStr(val2), line, j);
      lines[i] := line
    end
  end { rewriteAltitude } ;


  (* Test whether a value matches a field, with numeric "slop" etc. as needed.
  *)
  function testMatchField(const fld, val, line: string): boolean;

  var
    scratch, lim1, lim2: string;
    num1, num2, numX: integer;


    function sign(i: integer): integer; inline;

    begin
      if i < 0 then
        result := -1
      else
        if i = 0 then
          result := 0
        else
          result := 1
    end { sign } ;


  begin
    case fld of

  (* In the case of RSSI it's a numeric "stronger than" comparison with the     *)
  (* value (from the command line) being assumed to be -ve irrespective of      *)
  (* whether that's explicitly given.                                           *)

      'RSSI:':     if Pos('-', line) < 1 then
                     result := false
                   else begin
                     scratch := Copy(line, Pos('-', line), 99);
                     SetLength(scratch, Pos(' ', scratch) - 1);
                     result := StrToFloat(scratch) >= -Abs(StrToFloat(val))
                   end;

  (* Speed is a relatively straightforward comparison.                          *)

      'Speed:':    case val[1] of
                     '<': begin         (* Strictly less than                   *)
                            lim1 := Copy(val, 2, 999);
                            num1 := StrToInt(lim1);
                            scratch := Trim(Copy(line, Pos(': ', line) + 2, 99));
                            SetLength(scratch, Pos(' ', scratch) - 1);
                            numX := StrToInt(scratch);
                            result := numX < num1
                          end;
                     '>': begin         (* Greater than or equal                *)
                            lim1 := Copy(val, 2, 999);
                            num1 := StrToInt(lim1);
                            scratch := Trim(Copy(line, Pos(': ', line) + 2, 99));
                            SetLength(scratch, Pos(' ', scratch) - 1);
                            numX := StrToInt(scratch);
                            result := numX >= num1
                          end;
                     '=': begin         (* Within 2.5%                          *)
                            lim1 := Copy(val, 2, 999);
                            num1 := StrToInt(lim1);
                            scratch := Trim(Copy(line, Pos(': ', line) + 2, 99));
                            SetLength(scratch, Pos(' ', scratch) - 1);
                            numX := StrToInt(scratch);
                            result := Abs(numX - num1) * 1000 / num1 <= 25
                          end
                   otherwise
                     if Pos('<>', val) > 0 then begin
                       lim1 := Copy(val, 1, Pos('<>', val) - 1);
                       lim2 := Copy(val, Pos('<>', val) +2, 999);
                       num1 := StrToInt(lim1);
                       num2 := StrToInt(lim2);
                       scratch := Trim(Copy(line, Pos(': ', line) + 2, 99));
                       SetLength(scratch, Pos(' ', scratch) - 1);
                       numX := StrToInt(scratch);
                       result := (numX >= num1) and (numX < num2)
                     end else
                       result := AnsiContainsText(line + ' ', ' ' + val + ' ')
                   end;

  (* Altitude might need to take a ground correction into account.              *)

      'Altitude:': case val[1] of
                     '<': begin         (* Strictly less than                   *)
                            lim1 := Copy(val, 2, 999);
                            num1 := StrToInt(lim1) + -commandline.option.ground;
                            scratch := Trim(Copy(line, Pos(': ', line) + 2, 99));
                            SetLength(scratch, Pos(' ', scratch) - 1);
                            numX := StrToInt(scratch);
                            result := numX < num1
                          end;
                     '>': begin         (* Greater than or equal                *)
                            lim1 := Copy(val, 2, 999);
                            num1 := StrToInt(lim1) + -commandline.option.ground;
                            scratch := Trim(Copy(line, Pos(': ', line) + 2, 99));
                            SetLength(scratch, Pos(' ', scratch) - 1);
                            numX := StrToInt(scratch);
                            result := numX >= num1
                          end;
                     '=': begin         (* Within 2.5%                          *)
                            lim1 := Copy(val, 2, 999);
                            num1 := StrToInt(lim1) + -commandline.option.ground;
                            scratch := Trim(Copy(line, Pos(': ', line) + 2, 99));
                            SetLength(scratch, Pos(' ', scratch) - 1);
                            numX := StrToInt(scratch);
                            result := Abs(numX - num1) * 1000 / num1 <= 25
                          end
                   otherwise
                     if Pos('<>', val) > 0 then begin
                       lim1 := Copy(val, 1, Pos('<>', val) - 1);
                       lim2 := Copy(val, Pos('<>', val) +2, 999);
                       num1 := StrToInt(lim1) + -commandline.option.ground;
                       num2 := StrToInt(lim2) + -commandline.option.ground;
                       scratch := Trim(Copy(line, Pos(': ', line) + 2, 99));
                       SetLength(scratch, Pos(' ', scratch) - 1);
                       numX := StrToInt(scratch);
                       result := (numX >= num1) and (numX < num2)
                     end else
                       result := AnsiContainsText(line + ' ', ' ' + val + ' ')
                   end;

  (* Heading needs to wrap at due North.                                        *)

// TODO : Wrapped heading comparisons to be tested carefully.
  'Heading:':  case val[1] of
                     '<': begin         (* Strictly less than                   *)
                            lim1 := Copy(val, 2, 999);
                            num1 := StrToInt(lim1) mod 360;
                            scratch := Trim(Copy(line, Pos(': ', line) + 2, 99));
                            SetLength(scratch, Pos(' ', scratch) - 1);
                            numX := StrToInt(scratch) mod 360;
                            if (num1 >= 180) and (numX <= 179) then
                              num1 -= 360;
                            result := numX < num1
                          end;
                     '>': begin         (* Greater than or equal                *)
                            lim1 := Copy(val, 2, 999);
                            num1 := StrToInt(lim1) mod 360;
                            scratch := Trim(Copy(line, Pos(': ', line) + 2, 99));
                            SetLength(scratch, Pos(' ', scratch) - 1);
                            numX := StrToInt(scratch) mod 360;
                            if (num1 >= 180) and (numX <= 179) then
                              num1 -= 360;
                            result := numX >= num1
                          end;
                     '=': begin         (* Within 2.5%                          *)
                            lim1 := Copy(val, 2, 999);
                            num1 := StrToInt(lim1) mod 360;
                            scratch := Trim(Copy(line, Pos(': ', line) + 2, 99));
                            SetLength(scratch, Pos(' ', scratch) - 1);
                            numX := StrToInt(scratch) mod 360;
                            result := Abs(numX - num1) * 1000 / num1 <= 25
                          end
                   otherwise
                     if Pos('<>', val) > 0 then begin
                       lim1 := Copy(val, 1, Pos('<>', val) - 1);
                       lim2 := Copy(val, Pos('<>', val) + 2, 999);
                       num1 := StrToInt(lim1) mod 360;
                       num2 := StrToInt(lim2) mod 360;
                       scratch := Trim(Copy(line, Pos(': ', line) + 2, 99));
                       SetLength(scratch, Pos(' ', scratch) - 1);
                       numX := StrToInt(scratch) mod 360;
                       if (num1 >= 180) and (num2 <= 179) and (num1 > num2) then
                         num1 -= 360;
                       result := (numX >= num1) and (numX < num2)
                     end else
                       result := AnsiContainsText(line + ' ', ' ' + val + ' ')
                   end;
      '_HOG_': begin
                 scratch := val;

(* Special cases here. I believe this applies unambiguously to the "CPR type:"  *)
(* field:                                                                       *)
(*                                                                              *)
(* "If Ground Speed > 100 knots OR Airspeed >100 knots OR Radio Altitude > 50   *)
(* feet, then the Air/Ground status shall be changed to “Airborne” and the      *)
(* Airborne Position Message (see §2.2.3.2.3) shall be broadcast irrespective   *)
(* of the automatically determined Air/Ground status.                           *)
(*                                                                              *)
(* The "Air/Ground:" field can also have an "airborne?" state, my best guess at *)
(* present is that this indicates a guess based on (barometric?) altitude  with *)
(* no explicit confirmation from undercarriage sensors. I'm going to err here   *)
(* on the side of "not surface", since the less ambiguity associated with being *)
(* on the ground the better the chance of deducing what barometric correction   *)
(* should be applied when a small plane's reported altitude suggests this is in *)
(* error.                                                                       *)

                 if Pos('irborne?', scratch) > 0 then
                   scratch := 'airborne';
                 result := testMatchField('CPR type:', scratch, line) or testMatchField('Air/Ground:', scratch, line)
               end
    otherwise
      result := AnsiContainsText(line + ' ', ' ' + val + ' ')   (* Case-insensitive *)
    end
  end { testMatchField } ;


  (* Does the record have a line that starts with the match field name and
    contains the match field value, with the record also containing at least
    one line to be shown?
  *)
  function matches(): boolean;

  var
    i, j, k: integer;
    line: string= '';

  begin

  (* If there is no match option then always return true. If there is a match   *)
  (* option and the field to be matched doesn't appear in the record then don't *)
  (* return true.                                                               *)

    if (commandline.option.matchFld = '') or (commandline.option.matchVal = '') then
      result := true
    else begin
      result := false;
      for i := 0 to lines.Count - 1 do begin
        line := lines[i];
        if testHaveField(commandline.option.matchFld, line) then begin
          result := true;
          break
        end
      end;

  (* If true we have the match line, possibly containing the correct value with *)
  (* special handling for RSSI.                                                 *)

      if result then
        result := testMatchField(commandline.option.matchFld, commandline.option.matchVal, line)
    end;

  (* If true there was either no match option or value was correct. If there is *)
  (* no show list always return true otherwise return true depending on whether *)
  (* the --show or --strict option has been specified which imply that either   *)
  (* at least one or all of the fields from the list are present.               *)

    if result then
      if not Assigned(commandline.showlist) then
        exit
      else
        if result then begin
          result := false;
          k := 0;
          for i := 0 to lines.Count - 1 do
            for j := 0 to commandline.showlist.Count - 1 do begin
              line := Trim(lines[i]);
              if testHaveField(commandline.showlist[j], line) then begin
                k += 1
              end
            end;

(* On the balance, I'm not very happy with hog since it introduces an ambiguity *)
(* when records are being matched: one record might have both "Air/Ground:" and *)
(* "CPR: type", and allowing for this possibility while still being strict with *)
(* the number of lines matched is tricky (or at least, more trouble than it's   *)
(* worth).                                                                      *)

(* We have to have a >= comparison here if --strict is in force since the _HOG_ *)
(* might have resulted in two lines matching in the same record.                *)

          if commandline.option.strict then
            result := k >= commandline.showlist.Count
          else
            result := k > 0
        end
  end { matches } ;


begin
  if lines.Count = 0 then
    exit;
  try

(* The records should look like one of                                          *)
(*                                                                              *)
(* ICAO Address: 4d3520 (ADS-B, non-transponder)                                *)
(* 2023-10-17 12:02:25.35 ICAO Address: 4d3520 (ADS-B, non-transponder)         *)
(* 2023-10-17T12:02:25.35Z ICAO Address: 4d3520 (ADS-B, non-transponder)        *)
(*                                                                              *)
(* 12345678901234567890123456789012345678901234567890                           *)
(*          1         2         3         4         5                           *)
(*                                                                              *)
(* Assume that if there is a timestamp, then the same instant applies to all    *)
(* lines in the record.                                                         *)

(* Does the first line start with a timestamp? If so then use it, optionally    *)
(* toggling the optional T separator but henceforth ignoring the --utc and      *)
(* --time options and in particular not attempting to switch between UTC and    *)
(* local timezones.                                                             *)

    alreadyTimestamped := (lines[0][1] in ['0'..'9']) and (lines[0][5] = '-');
    if alreadyTimestamped then begin
      timestamp := Trim(Copy(lines[0], 1, 23)) + ' ';
      if commandline.option.iso then
        case timestamp[11] of
          ' ': timestamp[11] := 'T';
          'T': timestamp[11] := ' '
        otherwise
        end;
      commandline.option.newline := true (* Assume we want to propagate this    *)
    end else
      if commandline.option.utc or commandline.option.time then begin

(* For each line of the current record, look for the time line. This is usually *)
(* fairly early (but not early enough to be guaranteed to precede the RSSI      *)
(* etc.), so this isn't as big a performance hit as it would appear.            *)

        for i := 0 to lines.Count - 1 do
          if AnsiStartsStr('Time: ', Trim(lines[i])) then begin
            timeline := lines[i];
            break
          end;
        if timeline = '' then
          ts := 0.0
        else begin
          timeline := Trim(ExtractDelimited(2, timeline, [':']));
          SetLength(timeline, Length(timeline) - 2);
          ts := StrToFloat(timeline);
          ts := ts / (mSecsPerDay * 1000.0)
        end;
        if commandline.option.iso then
          timestamp := IsoFormatDateTime(commandline.option.runstart + ts, IsoDateTTime, 2)
        else
          timestamp := IsoFormatDateTime(commandline.option.runstart + ts, IsoDateTime, 2);
        if commandline.option.utc then
          timestamp += 'Z '
        else
          timestamp += ' '              (* Note: still only a single space here *)
      end;

(* Above and below: if already timestamped, we can split around chars 23/24     *)
(* irrespective of whether the timestamp has an appended Z indicating UTC,      *)
(* provided that we apply Trim() to both the timestamp and the remainder of the *)
(* line.                                                                        *)

(* We're going to be applying the match and show lists multiple times to each   *)
(* line, so it's likely to be faster to chop off any existing timestamp than to *)
(* do a lot of extra substring operations.                                      *)

    if alreadyTimestamped then
      for i := 0 to lines.Count - 1 do
        lines[i] := Trim(DelSpace1(Copy(lines[i], 24, 999)))
    else
      for i := 0 to lines.Count - 1 do
        lines[i] := Trim(DelSpace1(lines[i]));

(* Does the current record have a line that starts with the match field name    *)
(* and contains the match field value, and if there is a show list at least one *)
(* line that starts with one of the match field names?                          *)
(*                                                                              *)
(* The matches() function will take the --ground value into account if it is to *)
(* compare an altitude. If the record is matched on any field, e.g. on the hex  *)
(* code, if there is a --ground value then rewrite an altitude line if present  *)
(* appending a comment in braces to show what's been done.                      *)

    if matches() then begin
      match := true;
      if commandline.option.ground <> 0 then
        rewriteAltitude;

(* If there is a show list then iterate over the record presenting the correct  *)
(* fields. Otherwise present the entire record.                                 *)

      if Assigned(commandline.showlist) then begin
        for i := 0 to lines.Count - 1 do
          for j := 0 to commandline.showList.Count - 1 do
            if testHaveField(commandline.showList[j], lines[i]) then begin
              show := true;

(* Got a hit. Output the line of interest to stdout, except if --tee is active  *)
(* in which case it goes to stderr. I claim compliance with ISO 3307 and with   *)
(* ISO-8601 preceding the 2019 revision which required T as a delimiter.        *)

              if not commandline.option.tee then
                WriteLn(timestamp + lines[i])
              else
                WriteLn(stderr, timestamp + lines[i])
            end
        end;
        if show then begin
          if not commandline.option.tee then begin
            if commandline.option.newLine then
              WriteLn
          end else begin
            if commandline.option.newLine then
              WriteLn(stderr)
          end;
          if commandline.option.once then
            Halt(0);
        exit                            (* Via finally clause                   *)
      end else begin
        for i := 0 to lines.Count - 1 do
          if not commandline.option.tee then
            WriteLn(timestamp + lines[i])
          else
            WriteLn(stderr, timestamp + lines[i]);
          if not commandline.option.tee then begin
            if commandline.option.newLine then
              WriteLn
          end else begin
            if commandline.option.newLine then
              WriteLn(stderr)
          end;
          if commandline.option.once then
            Halt(0);
        exit                            (* Via finally clause                   *)
      end
    end
  finally

(* If --tee is active, and if we either have a match or --filter is not active, *)
(* output the record to stdout.                                                 *)

    if commandline.option.tee then
      if match or not commandline.option.filter then begin
        for i := 0 to lines.Count - 1 do
          WriteLn(lines[i]);
        WriteLn
      end;
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

  If ParamCount(commandline) > 0 then begin
    Close(input);
    AssignFile(input, ParamStr(commandline, 1));
    reset(input)
  end;
  lines := TStringList.Create;
  while not Eof() do begin
    ReadLn(line);
    if Pos('# BOUNDS', line) = 1 then begin
      WriteLn(output, line);
      WriteLn(output)
    end else
      if line = '' then
        emptyLine
      else
        nonEmptyLine
  end;
  FreeAndNil(lines)
end.

