(* Lazarus+FPC 2.2.6+3.2.2 on Linux Lazarus+FPC 2.2.6+3.2.2 on Linux Lazarus+FP *)

unit Params1090;

(* Commandline parsing, pinched from MavParams.                 MarkMLl         *)

{$mode ObjFPC}{$H+}
interface

uses
  Classes, SysUtils;

type
  TParamKind= (pkNone, pkText);

  TParameter= record
                kind: TParamKind;
                param: ansistring
              end;
  TParameters= array of TParameter;

  String255= string[255];
  TOptions= record
              matchFld: string255;
              matchVal: string255;
              runstart: TDateTime;
              ground: integer;
              strict: boolean;
              once: boolean;
              utc: boolean;
              time: boolean;
              iso: boolean;
              newline: boolean;
              tee: boolean;
              filter: boolean;
              gpx: integer
            end;

  TCommandLine= record
                  option: TOptions;
                  showList: TStringList;
                  parameter: TParameters
                end;
  PCommandLine= ^TCommandLine;
  TPosSeqLog= array of integer;
  PPosSeqLog= ^TPosSeqLog;

(* Parse the command line, including --version and --help processing which might
  suggest possible combinations which aren't relevant to a particular program.
  Emit appropriate error messages, set the program result code, and return false
  on error: expect the caller to abort silently.
*)
function ParseCommandline(out commandLine: TCommandLine; projName: ansistring= ''): boolean;

(* Parse but ignore the command line, except for --version and --help with error
  handling etc. as above.
*)
function ParseCommandline(const projName: ansistring= ''): boolean;

(* Return the number of parameters, ignoring the zeroeth.
*)
function ParamCount(const commandline: TCommandLine): integer;

(* The zeroeth parameter is the program name. Parameters indexed by 1 upwards
  might have been parsed from the commandline provided by the OS.

  This is compatible with System.ParamCount(), but is distinguished by its extra
  parameter.
*)
function ParamStr(const commandline: TCommandLine; i: integer): ansistring;

(* The zeroeth parameter is the program name. Parameters indexed by 1 upwards
  might have been parsed from the commandline provided by the OS.

  This is compatible with System.ParamStr(), but is distinguished by its extra
  parameter.
*)
function ParamKind(const commandline: TCommandLine; i: integer): TParamKind;

(* The zeroeth parameter is the program name. Parameters indexed by 1 upwards
  might have been parsed from the commandline provided by the OS.
*)
function ParamName(const commandline: TCommandLine; i: integer): ansistring;

(* For debugging purposes, dump the options and parameters which have been
  parsed from the command line.
*)
procedure DumpCommandline(const commandLine: TCommandLine);

(* If the pattern array is empty there is no match i.e. zero is returned, while
  a blank in the array is a wildcard which matches anything. Apart from the
  above, if pat contains a single element, this works like the standard PosEx()
  function.

  Working from the first towards the final element, check that each successive
  element is present in str and return the one-based position of the final
  element. In practice, if the final element is blank and the string is not
  empty then return the position of the character immediately after the
  penultimate pattern element.

  The nullable log parameter is filled in with the position of each successive
  match, this might be useful where substrings are to be extracted or deleted.
  This applies even if the function result is zero, in which case the log array
  will be shorter than the pattern array.
*)
function PosSeq(const pat: array of ansistring; const str: ansistring;
                                        var log: TPosSeqLog; ofs: integer= 1): integer;

(* If the pattern array is empty there is no match i.e. zero is returned, while
  a blank in the array is a wildcard which matches anything. Apart from the
  above, if pat contains a single element, this works like the standard PosEx()
  function.

  Working from the first towards the final element, check that each successive
  element is present in str and return the one-based position of the final
  element. In practice, if the final element is blank and the string is not
  empty then return the position of the character immediately after the
  penultimate pattern element.
*)
function PosSeq(const pat: array of ansistring; const str: ansistring;
                                        ofs: integer= 1): integer;

(* Parse and delete the text corresponding to a single floating point number,
  plus the delimiter that follows it which is assumed to be 0x0 if we're at the
  end of the string.
*)
function ParseFloatStr(var s: ansistring; out t: ansistring; out d: ansichar): boolean;

(* Parse and delete the text corresponding to a pair of floating point numbers.
  Expect the parameter to be blank on completion.
*)
function ParseFloatPairStr(var s: ansistring; out t1, t2: ansistring): boolean;


implementation

uses
  StrUtils, GetOpts, IniFilesAbout;

{$inline on }
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{$macro on  }
{$define IGNORECOMMANDLINE:= PCommandLine(Nil)^ }
{$define IGNOREPOSSEQLOG:= PPosSeqLog(Nil)^ }


function valid(var commandLine: TCommandLine): boolean; inline;

begin
  result := Assigned(@commandLine)
end { valid } ;


procedure doVersion(projName: ansistring= '');

begin
  WriteLn();
  WriteLn(projName + ' ' + AboutText());
  WriteLn()
end { doVersion } ;


procedure doHelp0(projName: ansistring= ''; const arg: string= '');

begin
  if arg = '' then begin
    WriteLn();
    WriteLn('Enter ', projName, ' --help for usage information.');
    WriteLn()
  end
end { doHelp0 } ;


procedure doHelp1(projName: ansistring= ''; const arg: string= '');

begin
  if arg = '' then begin
    WriteLn();
    WriteLn('SYNOPSIS');
    WriteLn();
    WriteLn('        ', projName, ' [OPTION]... [PARAMETER]...');
    WriteLn();
    case projName of
(*                                       1         2         3         4         5         6         7         8 *)
(*                              12345678901234567890123456789012345678901234567890123456789012345678901234567890 *)
       'match1090':   begin
                        WriteLn('Expect piped text from Dump1090, or a file specified on the command line.');
                        WriteLn('Accumulate intact records, and look for a field specified using the --match');
                        WriteLn('option with a value which is a case-insensitive header name from the output');
                        WriteLn('of Dump1090 --interactive followed by a colon and a text string. If the');
                        WriteLn('record matches and is of the appropriate type, output the content of the');
                        WriteLn('fields specified by --show or --strict, if the --once option is present then.');
                        WriteLn('terminate.');
                        WriteLn;
                        WriteLn('Each field will be presented on its own line, optionally preceded by the UTC');
                        WriteLn('or local time (--utc or --time option respectively) and always preceded by');
                        WriteLn('the field name. It may be followed by a blank line if --nl is specified.');
                        WriteLn;
{                        WriteLn('Output will be on stdout, unless the --tee option is set in which case it');
                        WriteLn('will be on stderr. If the --tee option is not set the original records-');
                        WriteLn('whether matched or unmatched- will be discarded, if the --tee option is set');
                        WriteLn('and the --filter option is not set then all input will be passed through on');
                        WriteLn('stdout allowing for daisychained programs to operate simultaneously. If the');
                        WriteLn('--filter option is set then only matched records will be output.') }
                      end;
       'poly1090':    begin
                        WriteLn('Expect piped input from Dump1090, typically filtered by match1090 to limit it');
                        WriteLn('to e.g. aircraft below a certain height. Using a list of lat,long coordinates');
                        WriteLn('on the command line, ignore all records which don''t indicate an aircraft');
                        WriteLn('intruding into the defined polygon.');
                        WriteLn;
                        WriteLn('Hint: it''s worth getting into the habit of always putting -- before the');
                        WriteLn('parameters, since otherwise the command line handling might be confused by a');
                        WriteLn('negative (i.e. Southern hemisphere) latidude.');
                      end;
       'ticket1090':  begin
                        WriteLn('Expect the input to be piped records, originally emitted by Dump1090 but');
                        WriteLn('aggressively filtered such that they typically indicate that an aircraft has');
                        WriteLn('strayed into a prohibited area at too low an altitude.');
                        WriteLn;
                        WriteLn('Start accumulating ADSB records when incursion is detected, and save them as a');
                        WriteLn('file with a name based on the timestamp some short time after it leaves. Output');
                        WriteLn('will be in Dump1090 and/or GPX format, with normal text (stdout) output reserved');
                        WriteLn('output reserved for status messages.')
                      end;
       'adsb_to_gpx': begin
                        WriteLn('Expect piped input from Dump1090, typically filtered by match1090 to limit it');
                        WriteLn('to a single aircraft. Output a .gpx file, suitable for input to Google Earth');
                        WriteLn('or QGIS.')
                      end
    otherwise
    end;
(*                    1         2         3         4         5         6         7         8 *)
(*           12345678901234567890123456789012345678901234567890123456789012345678901234567890 *)
    WriteLn();
    WriteLn('PARAMETER is the name of an input file, see examples below.');
    WriteLn();
    WriteLn('Brackets i.e. [NUM] etc. indicate that a value is optional. Ellipsis ...');
    WriteLn('indicates that a value may be repeated, typically as a comma-separated pair');
    WriteLn('or list. If the long form of an option is used, then any optional value must be');
    WriteLn('separated by = rather than by a space; this requirement is relaxed only in the');
    WriteLn('case of --help.');
    WriteLn();
    WriteLn('OPTIONS');
    WriteLn();
    WriteLn('  -V, --version');
    WriteLn('        Version information.');
    WriteLn();
    WriteLn('  -H [CMD], --help[=CMD]');
    WriteLn('        This help text, or a description of a command CMD.');
    WriteLn()
  end
end { doHelp1 } ;


procedure doHelp2(projName: ansistring= ''; const arg: string= ''); forward;


procedure doHelp2(projName: ansistring= ''; const args: array of string);

var
  i: integer;

begin
  for i := 0 to Length(args) - 1 do
    doHelp2(projName, args[i])
end { doHelp2 } ;


procedure doHelp2(projName: ansistring= ''; const arg: string= '');

begin
  case arg of
(*                                  1         2         3         4         5         6         7         8 *)
(*                         12345678901234567890123456789012345678901234567890123456789012345678901234567890 *)
    'm',
    'match':    begin
                  WriteLn('  -m, --match=FLD:TXT');
                  WriteLn('        Match a record with field FLD containing value TXT. Except for RSSI,');
                  WriteLn('        altitude and speed comparisons are as case-insensitive strings, in the');
                  WriteLn('        case of RSSI a successful comparison requires the signal strength to be');
                  WriteLn('        at least the value while altitude and speed comparisons allow prefix <');
                  WriteLn('        = > operators plus also an infix <> to match a range. Refer to Dump1090');
                  WriteLn('        interactive output for acceptable field names.');
                  WriteLn
                end;
    's',
    'show':     begin
                  WriteLn('  -s, --show=FLD[,FLD]...');
                  WriteLn('        For a matched record, output lines which correspond to one or more of');
                  WriteLn('        the indicated fields. Refer to Dump1090 interactive output for');
                  WriteLn('        acceptable field names.');
                  WriteLn
                end;
    'S',
    'strict':   begin
                  WriteLn('  -S, --strict=FLD[,FLD]...');
                  WriteLn('        For a matched record, output lines which correspond to the indicated');
                  WriteLn('        fields all of which must be present. Refer to Dump1090 interactive');
                  WriteLn('        output for acceptable field names.');
                  WriteLn
                end;
    'G',
    'ground':   begin
                  WriteLn('  -G, --ground=feet');
                  WriteLn('        When matching reported altitude, apply an offset in feet derived from');
                  WriteLn('        the altitude reported when the aircraft is stationary. So if altitude is');
                  WriteLn('        reported as -350 when the aircraft is on a runway which is at 66 feet');
                  WriteLn('        above Mean Sea Level, apply -416 as the value here. *** EXPERIMENTAL ***');
                  WriteLn
                end;
    '1',
    'once':     begin
                  WriteLn('  -1, --once');
                  WriteLn('        Terminate after a single matched record.');
                  WriteLn
                end;
    'u',
    'utc':      begin
                  WriteLn('  -u, --utc');
                  WriteLn('        Prefix matched lines with a UTC timestamp relative to the start of the');
                  WriteLn('        program run. This is ignored if the data has already been timestamped,');
                  WriteLn('        on the assumption that this was done when it was received by a radio.');
                  WriteLn('        Generated timestamps will have a Z suffix, in compliance with ISO 8601');
                  WriteLn('        and its predecessor ISO 3307.');
                  WriteLn
                end;
    't',
    'time':     begin
                  WriteLn('  -t, --time');
                  WriteLn('        Prefix matched lines with a local timestamp relative to the start of the');
                  WriteLn('        program run. This is ignored if the data has already been timestamped,');
                  WriteLn('        on the assumption that this was done when it was received by a radio.');
                  WriteLn
                end;
    'I',
    'iso':      begin
                  WriteLn('  -O, --iso');
                  WriteLn('        Generated timestamps will comply with ISO 8601-1:2019 in that they have');
                  WriteLn('        T as a delimiter between the date and time parts and Z as a suffix if');
                  WriteLn('        the time is based on UTC. If timestamps were created at an earlier stage');
                  WriteLn('        of a processing pipline then T will be toggled.');
                  WriteLn
                end;
    'N',
    'nl':       begin
                  WriteLn('  -n, --nl');
                  WriteLn('        Append a newline to each group of matched lines.');
                  WriteLn
                end;
    'T',
    'tee':      begin
                  WriteLn('  -T, --tee');
                  WriteLn('        Send matched lines to stderr and all records to stdout.');
                  WriteLn
                end;
    'F',
    'filter':   begin
                  WriteLn('  -f, --filter');
                  WriteLn('        Send matched lines to stderr and matched records to stdout.');
                  WriteLn
                end;
    'g',
    'gpx':      begin
                  WriteLn('  -g, --gpx[=only]');
                  WriteLn('        Create files in .gpx format as well as Dump1090-compatible .txt format.');
                  WriteLn('        The "only" modifier will suppress .txt output.');
                  WriteLn
                end
  otherwise
    case projName of
      'match1090':   doHelp2(projName, ['match', 'show', 'ground', 'once', 'utc', 'time', 'iso', 'nl' { , 'tee', 'filter' } ]);
      'ticket1090':  doHelp2(projName, ['gpx']);
      'poly1090',
      'adsb_to_gpx': doHelp2(projname, []);
    otherwise
    end
  end
end { doHelp2 } ;


procedure doHelp3(projName: ansistring= ''; const arg: string= '');

begin
  if arg = '' then begin
    WriteLn('EXAMPLES');
    WriteLn();
    case projName of
(*                                       1         2         3         4         5         6         7         8 *)
(*                              12345678901234567890123456789012345678901234567890123456789012345678901234567890 *)
      'match1090':   begin
                       WriteLn('        $ match1090 --once --match=Flight,GBTIT --show=Hex < 1090.txt');
                       WriteLn;
                       WriteLn('Display the hex code (ICAO address) associated with the flight GBTIT.');
                       WriteLn;
                       WriteLn('        $ match1090 --utc --match=Hex:4D3520 --show=Alt,Lat,Long --nl < 1090.txt');
                       WriteLn;
                       WriteLn('Display the first field associated with the indicated ICAO code that has any of');
                       WriteLn('the altitude, latitude or longitude fields.');
                       WriteLn;
                       WriteLn('        $ match1090 --utc --match=Hex:4D3520 --strict=Alt,Lat,Long < 1090.txt');
                       WriteLn;
                       WriteLn('Display the first field associated with the indicated ICAO code that has all of');
                       WriteLn('the altitude, latitude or longitude fields and where latitude and longitude pass');
                       WriteLn('minimal consistency checks.');
                       WriteLn;
                       WriteLn('        $ cat 1090.txt | match1090 --match=hex:4D3520 --strict=lat,long,alt \');
                       WriteLn('                --nl | match1090 ''--match=alt:-325<>750'' --strict=lat,long,alt \');
                       WriteLn('                --utc --nl | adsb1090_to_gpx > 1090.gpx');
                       WriteLn;
                       WriteLn('Note that there are no consistency checks for altitude, and that -ve values from');
                       WriteLn('poorly configured barometric altimeters are common.');
                       WriteLn;
                       WriteLn('UTC time:   ' + IsoFormatDateTime(UTC_Now(), 2) + 'Z');
                       WriteLn('Local time: ' + IsoFormatDateTime(Now(), 2))
                     end;
      'poly1090':    begin
                       WriteLn('        $ dump1090 | match1090 ''--match=alt:<750'' --strict=hex,lat,long,alt \');
                       WriteLn('                --utc --nl | poly1090 -- 51.8876,0.1545 51.8941,0.1483 \');
                       WriteLn('                51.9026,0.1720 51.8929,0.1790');
                       WriteLn;
                       WriteLn('The lat,long coordinates are separated by spaces. The polygon will be closed');
                       WriteLn('automatically.')
                     end;
      'ticket1090':  begin
                       WriteLn('        $ dump1090 | match1090 ''--match=alt:<750'' --strict=hex,lat,long,alt --utc \');
                       WriteLn('                --nl | poly1090 -- 51.8876,0.1545 51.8941,0.1483 51.9026,0.1720 \');
                       WriteLn('                51.8929,0.1790 | ticket1090 --gpx=only');
                       WriteLn;
                       WriteLn('In the above example only a .gpx file will be created. Generating graphical output');
                       WriteLn('using e.g. QGIS scripting is left as an exercise.')
                     end;
      'adsb_to_gpx': begin
                       WriteLn('        $ cat 1090.txt | match1090 --match=hex:4D3520 --strict=lat,long,alt \');
                       WriteLn('                --nl | match1090 ''--match=alt:-325<>750'' --strict=lat,long,alt \');
                       WriteLn('                --utc --nl | adsb1090_to_gpx > 1090.gpx');
                       WriteLn;
                       WriteLn('Note that in this case the hex field is only needed for the initial match, so');
                       WriteLn('doesn''t need to be output. However if it is passed through it might be used as');
                       WriteLn('the name tag in the .gpx header.')
                     end
    otherwise
    end;
    WriteLn()
  end
end { doHelp3 } ;


procedure doHelp4(projName: ansistring= ''; const arg: string= '');

begin
  if arg = '' then begin
    WriteLn('EXIT STATUS');
    WriteLn();
    WriteLn(' 0  Normal termination');
    WriteLn(' 1  Cannot parse device identifier');
    WriteLn(' 2  Named device cannot be opened');
    WriteLn(' 3  Named device is unresponsive');
    WriteLn(' 4  Data access error');
    WriteLn(' 5  Data format error');
    WriteLn(' 9  Bad command-line parameters');
    WriteLn()
  end
end { doHelp4 } ;


(* Expect an unsigned integer of up to 64 bits. Return false if not found.
*)
function Expect_Num(var lineBuffer: string; out num: qword): boolean;

label
  666;

var
  radix: integer= 10;
  i: integer;
  acceptable: set of char= [];

begin
  result := false;
  try
    while (lineBuffer <> '') and (lineBuffer[1] in [#$09, ' ']) do
      Delete(lineBuffer, 1, 1);
    num := 0;
    if not (lineBuffer[1] in ['0'..'9', '$', '&', '%']) then
      exit(false);

(* Special cases: leading 0x, $ or 0 change the default radix, as does trailing *)
(* H. Anybody who thinks that he can mix them is in for a rude awakening.       *)

    if Pos('0x', lineBuffer) = 1 then begin
      radix := 16;
      Delete(lineBuffer, 1, 2)
    end else
      if lineBuffer[Length(lineBuffer)] in ['H', 'h'] then begin
        radix := 16;
        SetLength(lineBuffer, Length(lineBuffer) - 1)
      end else
        if Pos('$', lineBuffer) = 1 then begin
          radix := 16;
          Delete(lineBuffer, 1, 1)
        end else
          if Pos('&', lineBuffer) = 1 then begin
            radix := 8;
            Delete(lineBuffer, 1, 1)
          end else
            if Pos('%', lineBuffer) = 1 then begin
              radix := 2;
              Delete(lineBuffer, 1, 1)
            end else
              if lineBuffer[1] = '0' then
                radix := 8;

(* Parse a sequence of digits. There must be at least one digit in either the   *)
(* radix or value part, note that we /didn't/ remove octal's leading zero since *)
(* doing so would make it impossible to enter 0 (i.e. a single character) as a  *)
(* parameter.                                                                   *)

  666:
    acceptable := [];
    for i := 0 to radix - 1 do
      if i <= 9 then
        acceptable += [Chr(Ord('0') + i)]
      else
        acceptable += [Chr(Ord('a') + i - 10)];
    while (lineBuffer <> '') and (lineBuffer[1] in acceptable) do begin
      num *= radix;
      if lineBuffer[1] <= '9' then
        num += Ord(lineBuffer[1]) - Ord('0')
      else
        num += Ord(LowerCase(lineBuffer[1])) - Ord('a') + 10;
      Delete(lineBuffer, 1, 1);
      result := true
    end;

(* If we encounter R then we've just read a new radix, iterate.                 *)

    if (lineBuffer <> '') and (lineBuffer[1] in ['R', 'r']) then begin
      radix := num;
      num := 0;
      Delete(lineBuffer, 1, 1);
      goto 666
    end
  finally
  end
end { Expect_Num } ;


(* Expect a pair of unsigned integers of up to 64 bits. Return false if not found.
*)
function Expect_Num2(lineBuffer: string; out num1, num2: qword): boolean;

begin
  result := Expect_Num(lineBuffer, num1);
  if result and (Copy(lineBuffer, 1, 1) = ',') then begin
    Delete(lineBuffer, 1, 1);
    result := Expect_Num(lineBuffer, num2)
  end else
    result := false
end { Expect_Num2 } ;


function guessKind(const a: ansistring): TParamKind;

begin
  if Trim(a) = '' then
    exit(pkNone)
  else
    exit(pkText)
end { guessKind } ;


(* Parse the command line, including --version and --help processing which might
  suggest possible combinations which aren't relevant to a particular program.
  Emit appropriate error messages, set the program result code, and return false
  on error: expect the caller to abort silently.
*)
function ParseCommandline(out commandLine: TCommandLine; projName: ansistring= ''): boolean;

label s;

var
  options: array of TOption;
  index: longint= 1;
  flag: char;
  shortOpts: string= '';


  (* Add an option which might be present on the command line as presented by
    the OS.
  *)
  function addOption(const nm: string; val: char; hasArg: integer= No_Argument): integer;

  begin
    if Pos(val, shortOpts) > 0 then begin
      WriteLn(stderr, 'Internal error: attempting to reuse ', val, ' as a short option.');
      ExitCode := 127;
      Halt
    end;
    if shortOpts = '' then              (* Set ordering to require_order        *)
      shortOpts := '+';
    SetLength(options, Length(options) + 1);
    result := Length(options) - 1;
    with options[result] do begin
      name := nm;
      has_arg := hasArg;
      flag := nil;
      value := val;
      shortOpts += val;
      case hasArg of
        Required_Argument: shortOpts += ':';
        Optional_Argument: shortOpts += '::'
      otherwise
      end
    end
  end { addOption } ;


  (* Add a parameter to the command line structure being returned to the caller.
  *)
  function addParam(pm: string; knd: TParamKind= pkText): integer;

  begin
    if not valid(commandLine) then
      result := -1
    else begin
      SetLength(commandLine.parameter, Length(commandLine.parameter) + 1);
      result := Length(commandLine.parameter) - 1;
      commandLine.parameter[result].kind := knd;
      commandLine.parameter[result].param := pm
    end
  end { addParam } ;


  function expandfield(var field: string255): boolean;

  begin
    result := true;
    case LowerCase(field) of
      'hex':      field := 'ICAO Address:';
      'squawk;',
      'sqwk':     field := 'Squawk:';
      'flight':   field := 'Ident:';
      'altitude',
      'alt':      field := 'Altitude:';
      'speed',
      'spd':      field := 'Speed:';
      'heading',
      'hdg':      field := 'Heading:';
      'lat':      field := 'CPR latitude:';
      'long':     field := 'CPR longitude:';
      'rssi':     field := 'RSSI:';

(* Not sure I entirely trust the CRC, where Dump1090 isn't reporting it as zero *)
(* it's very often putting in the (hex) address which suggests that it contains *)
(* rubbish if the message is truncated. Also add "type" and "hog", where the    *)
(* latter is expanded when used.                                                *)

      'crc':      field := 'CRC:';
      'type':     field := 'CPR type:';
      'ag':       field := 'Air/Ground:';
      'hog':      field := '_HOG_'      (* Both "CPR type:" and "Air/Ground:"   *)

(* On the balance, I'm not very happy with hog since it introduces an ambiguity *)
(* when records are being matched: one record might have both "Air/Ground:" and *)
(* "CPR: type", and allowing for this possibility while still being strict with *)
(* the number of lines matched is tricky (or at least, more trouble than it's   *)
(* worth).                                                                      *)

    otherwise
      result := false
    end
  end { expandField } ;


  function expandfield(list: TStringList): boolean;

  var
    i: integer;
    scratch: string255;

  begin
    result := true;
    for i := 0 to list.Count - 1 do begin
      scratch := list[i];
      result := result and expandField(scratch);
      if result then
        list[i] := scratch
      else
        break
    end
  end { expandField } ;


begin
  result := true;
  ExitCode := 0;
  projName := ChangeFileExt(ExtractFileName(projName), '');

(* Special case: no parameters at all or a Windows-style /H etc. tells the user *)
(* about the --help option. It's useful to be able to suppress this for a       *)
(* program which is usually run without options or parameters to avoid the user *)
(* having to use -- all the time.                                               *)

  case projName of
    'ticket1090',
    'adsb1090_to_gpx': ;
  otherwise
    if (System.ParamCount() = 0) or (System.ParamStr(1) = '/?') or (System.ParamStr(1) = '-?') or
                (System.ParamStr(1) = '/h') or (Copy(System.ParamStr(1), 1, 2) = '/H') then begin
      doHelp0(projName);
      exit(false)
    end
  end;

(* GetLongOpts() either returns the (Unicode?) character specified in the value *)
(* field, or writes it to flag^. Any option value string will be presented in   *)
(* the global OptArg variable.                                                  *)

  addOption('version', 'V');
  addOption('help', 'H', Optional_Argument);
  addOption('match', 'm', Required_Argument);
  addOption('show', 's', Required_Argument);
  addOption('strict', 'S', Required_Argument);
  addOption('ground', 'G', Required_Argument);
  addOption('once', '1');
  addOption('utc', 'u');
  addOption('time', 't');
  addOption('iso', 'I');
  addOption('nl', 'N');
  addOption('tee', 'T');
  addOption('filter', 'F');
  addOption('gpx', 'g', Optional_Argument); (* Resist temptation of changing to G *)
  addOption('', #$00);                  (* Mandatory end-array marker           *)

(* Assume that most options default to zero or false. Also assume that the      *)
(* parameter string array is adequately initialised by the compiler/runtimes,   *)
(* but that when we add elements to it we will need to set kind to pkNone       *)
(* explicitly.                                                                  *)

  if valid(commandLine) then
    FillByte(commandLine.option, SizeOf(commandLine.option), 0);
  commandline.showList := nil;
  index := 0;                           (* Must be initially zero               *)
  while true do
    case GetLongOpts(shortOpts, @options[0], index) of
      #0:  begin
             if {%H-}flag = ' ' then
               flag := flag               // Development breakpoint
           end;
      'V': begin
             doVersion(projName);
             exit(false)
           end;

(* Special case here. The RTL's GetLongOpt() appears to ignore an optional      *)
(* argument separated from its option by a space (as distinct from = which is   *)
(* respected). Since --help will interpret anything it doesn't understand as a  *)
(* request to list everything, be greedy and claim anything following it that   *)
(* isn't obviously another option, rather than leaving is as a parameter: this  *)
(* is safe because the main part of the program won't attempt to run.           *)

      'H': begin
             if OptArg = '' then
               if (System.ParamCount() >= index) and (System.ParamStr(index)[1] <> '-') then begin
                 OptArg := System.ParamStr(index);
                 index += 1
               end;
             doHelp1(projName, OptArg);
             if valid(commandLine) then
               doHelp2(projName, OptArg);
             doHelp3(projName, OptArg);
             doHelp4(projName, OptArg);
             exit(false)
           end;
      'm': if valid(commandLine) then
             if Pos(':', OptArg) < 1 then begin
               WriteLn(stderr, '-m or --match option expects "field:value".');
               ExitCode := 1;
               exit(false)
             end else begin
               commandline.option.matchFld := ExtractDelimited(1, OptArg, [':']);
               if not expandfield(commandline.option.matchFld) then begin
                 WriteLn(stderr, 'Field name "', commandline.option.matchFld, '" unknown.');
                 ExitCode := 1;
                 exit(false)
               end;
               commandline.option.matchval := ExtractDelimited(2, OptArg, [':'])
             end;
      's': if valid(commandLine) then begin
s:           commandline.showList := TStringList.Create;
             commandline.showList.CommaText := OptArg;
             if not expandfield(commandline.showList) then begin
               WriteLn(stderr, 'Field name in "', OptArg, '" unknown.');
               ExitCode := 1;
               exit(false)
             end
           end;
      'S': if valid(commandLine) then begin
             commandline.option.strict := true;
             goto s
           end;
      'G': if valid(commandLine) then
             commandline.option.ground := StrToInt(OptArg);
      '1': if valid(commandLine) then
             commandline.option.once := true;
      'u': if valid(commandLine) then begin
             commandline.option.utc := true;
             commandline.option.runstart := UTC_Now()
           end;
      't': if valid(commandLine) then begin
             commandline.option.time := true;
             commandline.option.runstart := Now()
           end;
      'I': if valid(commandLine) then
             commandline.option.iso := true;
      'N': if valid(commandLine) then
             commandline.option.newline := true;
      'T': if valid(commandLine) then
             commandline.option.tee := true;
      'F': if valid(commandLine) then
             commandline.option.filter := true;

(* The -g or --gpx option is used only by ticker1090, and is the only one it's  *)
(* interested in. It's tempting to use -G here rather than -g, but it would be  *)
(* incongruous having it by itself on the commandline.                          *)
(*                                                                              *)
(* 0 here is text output, 1 is text and .gpx, 2 is .gpx only.                   *)

      'g': if valid(commandLine) then
             if OptArg = '' then
               commandline.option.gpx := 1
             else
               commandline.option.gpx := 2;
      EndOfOptions: break
    otherwise
      break
    end;

(* That should have left index identifying the final option or value on the     *)
(* command line. Assume that we can ignore all parameters etc. if the           *)
(* commandLine parameter is nil.                                                *)

  index := OptInd - 1;
  while (index < System.ParamCount()) and valid(commandLine) do begin
    index += 1;
    shortOpts := System.ParamStr(index);
    addParam(shortOpts, guessKind(shortOpts))
  end;
  index := index                        (* Good place for breakpoint            *)
end { ParseCommandline } ;


(* Parse but ignore the command line, except for --version and --help with error
  handling etc. as above.
*)
function ParseCommandline(const projName: ansistring= ''): boolean;

begin
  result := ParseCommandline(IGNORECOMMANDLINE, projName)
end { ParseCommandline } ;


(* Return the number of parameters, ignoring the zeroeth.

  This is compatible with System.ParamCount(), but is distinguished by its extra
  parameter.
*)
function ParamCount(const commandline: TCommandLine): integer;

begin
  result := Length(commandline.parameter)
end { ParamCount } ;


(* The zeroeth parameter is the program name. Parameters indexed by 1 upwards
  might have been parsed from the commandline provided by the OS.

  This is compatible with System.ParamStr(), but is distinguished by its extra
  parameter.
*)
function ParamStr(const commandline: TCommandLine; i: integer): ansistring;

begin
  if i = 0 then
    result := System.ParamStr(0)
  else
    if i <= Length(commandline.parameter) then
      result := commandline.parameter[i - 1].param
    else
      result := ''
end { ParamStr } ;


(* The zeroeth parameter is the program name. Parameters indexed by 1 upwards
  might have been parsed from the commandline provided by the OS.
*)
function ParamKind(const commandline: TCommandLine; i: integer): TParamKind;

begin
  if i = 0 then
    result := pkNone
  else
    if i <= Length(commandline.parameter) then
      result := commandline.parameter[i - 1].kind
    else
      result := pkNone
end { ParamKind } ;


(* The zeroeth parameter is the program name. Parameters indexed by 1 upwards
  might have been parsed from the commandline provided by the OS.
*)
function ParamName(const commandline: TCommandLine; i: integer): ansistring;

var
  t: ansistring;

begin
  if i = 0 then
    result := 'None'
  else
    if i <= Length(commandline.parameter) then begin
      Str(commandline.parameter[i].kind, t);
      Delete(t, 1, 2);
      result := t
    end else
      result := 'None'
end { ParamName } ;


(* For debugging purposes, dump the options and parameters which have been
  parsed from the command line.
*)
procedure DumpCommandline(const commandLine: TCommandLine);

var
  i: integer;
  t: string;

begin
  t := AddCharR('-', '', 80);
  WriteLn(t);
  WriteLn('Match:       "', commandline.option.matchFld, '" : "', commandline.option.matchVal, '"');
  WriteLn('Strict:      ', commandline.option.strict);
  WriteLn('Once:        ', commandline.option.once);
  WriteLn('UTC:         ', commandline.option.utc);
  WriteLn('Time:        ', commandline.option.time);
  WriteLn('Nl:          ', commandline.option.newline);
  WriteLn('Tee:         ', commandline.option.tee);
  WriteLn('Filter:      ', commandline.option.filter);
  WriteLn('GPX:         ', commandline.option.gpx);
  if not Assigned(commandline.showList) then
    WriteLn('Show fields: 0')
  else begin
    WriteLn('Show fields: ', commandline.showList.Count);
    for i := 0 to commandline.showList.Count - 1 do
      WriteLn(i:4, ('         "' + commandline.showList[i] + '"'))
  end;
  WriteLn('Parameters:  ', Length(commandline.parameter));
  for i := 0 to Length(commandline.parameter) - 1 do begin
    Str(commandline.parameter[i].kind, t);
    Delete(t, 1, 2);
    WriteLn(i:4, t:23, ' "', commandline.parameter[i].param, '"')
  end;
  t := AddCharR('-', '', 80);
  WriteLn(t)
end { DumpCommandline } ;


{$ifopt C+ Predicated on assertions generating code }
var
  posSeqEarlyout: boolean;
{$endif                                             }


(* If the pattern array is empty there is no match i.e. zero is returned, while
  a blank in the array is a wildcard which matches anything. Apart from the
  above, if pat contains a single element, this works like the standard PosEx()
  function.

  Working from the first towards the final element, check that each successive
  element is present in str and return the one-based position of the final
  element. In practice, if the final element is blank and the string is not
  empty then return the position of the character immediately after the
  penultimate pattern element.

  The nullable log parameter is filled in with the position of each successive
  match, this might be useful where substrings are to be extracted or deleted.
  This applies even if the function result is zero, in which case the log array
  will be shorter than the pattern array.
*)
function PosSeq(const pat: array of ansistring; const str: ansistring;
                                        var log: TPosSeqLog; ofs: integer= 1): integer;
var
  p: integer;


  function greater(i1, i2: integer): integer; inline;

  begin
    if i1 > i2 then
      result := i1
    else
      result := i2
  end { greater } ;


begin
{$ifopt C+ Predicated on assertions generating code }
  posSeqEarlyout := true;
{$endif                                             }
  if Assigned(@log) then
    SetLength(log, 0);

(* Return no-match if the string is blank or the pattern array is empty or the  *)
(* offset is outside the string (assume these are efficient due to Boolean      *)
(* short-circuits)...                                                           *)

  if (str = '') or (Length(pat) = 0) or (ofs < 1) or (ofs > Length(str)) or

(* ...or if on closer inspection the offset places the end of the final pattern *)
(* beyond the end of the string.                                                *)

(* 2023-10-17T12:02:25.35Z ICAO Address: 4d3520 (ADS-B, non-transponder)        *)
(*                                                                   ICAO       *)
(* 1234567890123456789012345678901234567890123456789012345678901234567890       *)
(*          1         2         3         4         5         6         7       *)
(*                                                                              *)
(* In this example the length of the pattern is 4, the length of the string is  *)
(* 69, so if the offset where we start looking for the pattern is >66 we can    *)
(* never have a match.                                                          *)

                (ofs > (Length(str) - (Length(pat[Length(pat) - 1]) - 1))) then
    exit(0);
{$ifopt C+ Predicated on assertions generating code }
  posSeqEarlyout := false;
{$endif                                             }
  if pat[0] = '' then
    result := 1
  else
    result := PosEx(pat[0], str, ofs);
  if (result > 0) and Assigned(@log) then
    log := Concat(log, [result]);
  for p := 1 to Length(pat) - 1 do begin
    if result < 1 then
      exit;
    if pat[p] = '' then
      result := result + greater(Length(pat[p - 1]), 1)
    else
      result := PosEx(pat[p], str, result + greater(Length(pat[p - 1]), 1));
    if (result > 0) and Assigned(@log) then
      log := Concat(log, [result])
  end
end { PosSeq } ;


(* If the pattern array is empty there is no match i.e. zero is returned, while
  a blank in the array is a wildcard which matches anything. Apart from the
  above, if pat contains a single element, this works like the standard PosEx()
  function.

  Working from the first towards the final element, check that each successive
  element is present in str and return the one-based position of the final
  element. In practice, if the final element is blank and the string is not
  empty then return the position of the character immediately after the
  penultimate pattern element.
*)
function PosSeq(const pat: array of ansistring; const str: ansistring;
                                        ofs: integer= 1): integer; inline;

begin
  result := PosSeq(pat, str, IGNOREPOSSEQLOG, ofs)
end { PosSeq } ;


procedure posSeqTest;

const
  testTxt= '2023-10-17T12:02:25.35Z ICAO Address: 4d3520 (ADS-B, non-transponder)';
(*                                                                            ICAO *)
(*          1234567890123456789012345678901234567890123456789012345678901234567890 *)
(*                   1         2         3         4         5         6         7 *)

var
  log: TPosSeqLog;

begin
{$ifopt C+ Predicated on assertions generating code }
  Assert(PosSeq([], testTxt) = 0);
  Assert(PosSeq([''], testTxt) = 1);
  Assert(PosSeq(['Z '], testTxt) = 23);
  Assert(PosSeq(['Z ', ''], testTxt) = 25);
  Assert(PosSeq(['Z ', ' '], testTxt) = 29);
  Assert(PosSeq(['Z ', ' ', ''], testTxt) = 30);
  Assert(PosSeq(['Z ', ' ', '', ' '], testTxt) = 38);
  Assert(PosSeq(['Z ', ': '], testTxt) = 37);
  Assert(PosSeq(['Z ', ': ', ''], testTxt) = 39);
  Assert(PosSeq(['Z ', '::'], testTxt) = 0);
  Assert(PosSeq(['Z ', '::', ''], testTxt) = 0);

(* Check the early-out optimisation.                                            *)

  Assert(PosSeq(['Z ', 'ICAO'], testTxt, 66) = 0);
  Assert(posSeqEarlyout = false);
  Assert(PosSeq(['Z ', 'ICAO'], testTxt, 67) = 0);
  Assert(posSeqEarlyout = true);

(* Check the match log works if the pattern sequence is fully satisfied.        *)

  Assert(PosSeq([':', ' ', '', ': '], testTxt, log) = 37);
  Assert(Length(log) = 4, 'got ' + IntToStr(Length(log)) + ', expected 4.');
  Assert(log[0] = 14);
  Assert(log[1] = 24);
  Assert(log[2] = 25);
  Assert(log[3] = 37);
  Assert(PosSeq([':', ' ', '', ': ', ''], testTxt, log) = 39);
  Assert(Length(log) = 5, 'got ' + IntToStr(Length(log)) + ', expected 5.');
  Assert(log[0] = 14);
  Assert(log[1] = 24);
  Assert(log[2] = 25);
  Assert(log[3] = 37);
  Assert(log[4] = 39);

(* Check the match log works even if the pattern sequence is not satisfied.     *)

  Assert(PosSeq(['.', '  '], testTxt, log) = 0);
  Assert(Length(log) = 1, 'got ' + IntToStr(Length(log)) + ', expected 1.');
  Assert(log[0] = 20);
  Assert(PosSeq([':', '', ' ', '.'], testTxt, log, 30) = 0);
  Assert(Length(log) = 3, 'got ' + IntToStr(Length(log)) + ', expected 3.');
  Assert(log[2] = 45);

(* This will work irrespective of whether the date and time are separated by T  *)
(* or space, whether there are fractional seconds, whether a non-UTC time is    *)
(* indicated by a missing Z or a substituted space, and so on.                  *)

  Assert(PosSeq([':', ' ', '', ':', 'ZZZZZZZZ'], testTxt, log) = 0);
  Assert(Trim(Copy(testTxt, log[2], log[3] - log[2])) = 'ICAO Address')
{$endif                                             }
end { posSeqTest } ;


(* Parse and delete the text corresponding to a single floating point number,
  plus the delimiter that follows it which is assumed to be 0x0 if we're at the
  end of the string.
*)
function ParseFloatStr(var s: ansistring; out t: ansistring; out d: ansichar): boolean;

begin
  result := false;
  t := '';
  while (s <> '') and (s[1] in ['0'..'9', '+', '-', '.']) do begin
    t += s[1];
    Delete(s, 1, 1)
  end;
  if s = '' then begin
    d := #$00;
    result := true
  end else begin
    d := s[1];
    Delete(s, 1, 1);
    result := d = ','
  end;

(* Before the decimal point, remove spurious leading zeros.                     *)

  if Pos('.', t) < 1 then
    while (Length(t) > 1) and (t[1] = '0') do
      Delete(t, 1, 1)
  else
    while (Length(t) > 2) and (t[1] = '0') and (t[2] <> '.') do
      Delete(t, 1, 1);

(* After the decimal point, remove trailing zeros to make comparison safer.     *)

  if Pos('.', t) > 0 then
    while (t[Length(t)] = '0') and (t[Length(t)] <> '.') do
      SetLength(t, Length(t) - 1)
end { ParseFloatStr } ;


(* Parse and delete the text corresponding to a pair of floating point numbers.
  Expect the parameter to be blank on completion.
*)
function ParseFloatPairStr(var s: ansistring; out t1, t2: ansistring): boolean;

var
  d1, d2: ansichar;

begin
  result := true;
  if (not ParseFloatStr(s, t1, d1)) or (d1 <> ',') then
    result := false
  else
    if (not ParseFloatStr(s, t2, d2)) or (d2 <> #$00) then
      result := false
end { ParseFloatPairStr } ;


begin
  posSeqTest
end.

