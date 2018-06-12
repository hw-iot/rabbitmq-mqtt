-module(bin_utils).

-export([bcd_encode/2, bcd_decode/1]).
-export([dump/1, dump/2]).
-export([pprint/1, pprint/2, pprint/3]).
-export([compare/2]).
-export([from_str/1]).
-export([convert/1, convert/2]).

-export_type([opt/0, opts/0]).

-opaque opt()  :: {return, iolist} | {return, binary} | {printer, function()}.
-opaque opts() :: list(opt()).

%% Printer constants
-define(SPACE,    $ ).
-define(SPECIAL,  $.).
-define(FILL,     $0).

%% Comparison glyphs
-define(MISSING,  "??").
-define(EQUAL,    "--").

-ifndef(PRINT).
-define(PRINT(Var), io:format("DEBUG: ~p:~p - ~p~n~n ~p~n~n", [?MODULE, ?LINE, ??Var, Var])).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                   API                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% pack the digits of an integer as BCD in a given size of binary
%% pad with leading zeros
bcd_encode(N, Size) when is_list(N) ->
    bcd_encode(list_to_integer(N), Size);
bcd_encode(N, Size) ->
    << <<X:4>> || X <- lists:flatten(io_lib:fwrite("~*..0B", [Size*2, N])) >>.
%% bcd_encode(N, S) when N >= 0, S > 0 ->
%%     << <<(X-$0):4>> || X <- tl(integer_to_list(trunc(math:pow(10,S*2)) + (N rem trunc(math:pow(10,S*2))))) >>.

%% unpack the given size of BCD binary into an integer
%% strip leading zeros
bcd_decode(Bits) ->
    list_to_integer([X+$0 || <<X:4>> <= Bits]).

dump(M, B) when is_binary(B) ->
    io:format("bin| ~p // ~p~n", [B, M]),

    {ok, Octets} = convert(B, hex),
    io:format("~s~n", [Octets]),

    bin_utils:pprint(B);

dump(M, B) when is_bitstring(B) ->
    <<X/bitstring>> = B,
    io:format("bit| ~p  // ~p ~n", [X, M]);

dump(M, B) when is_integer(B) ->
    io:format("int| ~p ~p  // ~p ~n", [integer_to_list(B, 2), B, M]);
dump(M, B) ->
    io:format(" _ | ~p  // ~p ~n", [B, M]).

dump(B) -> dump(nil, B).

%% @doc Pretty print a binary to stdout using <em>io:format/2</em>
%% Note: <em>io:format/2</em> is a blocking call.
-spec pprint(binary() | bitstring()) -> ok.
pprint(Bin) ->
    pprint(Bin, []).

%% @doc Print a binary using custom function or return the formatted result.
%% Valid option is one of:
%% <ul>
%%  <li>{return, binary}</li>
%%  <li>{return, iolist}</li>
%%  <li>{printer, CustomFunction}</li>
%% </ul>
%%
%% Custom printers should be of arity 1, accepting the prettified result as
%% an <em>iolist()</em> input. Returned value can be <em>any()</em>.
-spec pprint(binary() | bitstring(), opts()) -> ok | any().
pprint(Bin, Opts) when is_list(Opts) ->
    %% OLD {ok, Octets} = convert(Bin, hex),
    %% OLD Buckets = buckets(16, Octets),
    {ok, Octets} = convert(Bin, bin),
    Buckets = buckets(8, Octets),
    Printed = print_buckets(Buckets),
    apply_opts(Printed, Opts).

%% @doc Pretty print a slice of binary.
-spec pprint(binary() | bitstring(), {non_neg_integer(), non_neg_integer()},
             opts()) -> ok | any().
pprint(Bin, {Pos, Len}, Opts) when Len =< byte_size(Bin), (Pos+Len) =< byte_size(Bin) ->
    pprint(binary:part(Bin, Pos, Len), Opts);
pprint(Bin, {Pos, _}, Opts) ->
    pprint(binary:part(Bin, Pos, byte_size(Bin)-Pos), Opts).

%% @doc Pretty print byte-to-byte comparsion of two binaries to stdout.
-spec compare(binary() | bitstring(), binary() | bitstring()) -> ok.
compare(Bin1, Bin2) when is_binary(Bin1) orelse is_bitstring(Bin1),
                         is_binary(Bin2) orelse is_bitstring(Bin2) ->
    {ok, Octets1} = convert(Bin1, hex),
    {ok, Octets2} = convert(Bin2, hex),
    {ok, {D1, D2}} = diff(Octets1, Octets2),
    print_comparison(buckets(16, D1), buckets(16, D2)).

%% @doc Construct binary from hex string.
%% Hex string octets can be optionally separated with spaces.
%% Valid hex strings:
%% <ul>
%% <li>"AA BB FF 01"</li>
%% <li>"AABBFF01"</li>
%% </ul>
-spec from_str(string()) -> binary().
from_str(Str) when is_list(Str) ->
    Bytes = case lists:member(?SPACE, Str) of
                true ->
                    string:tokens(Str, [?SPACE]);
                false when length(Str) rem 2 =:= 0 ->
                    buckets(2, Str)
            end,
    list_to_binary([list_to_integer(B, 16) || B <- Bytes]).

%% @doc Convert binary to hex string.
-spec convert(binary() | bitstring()) -> {ok, list()}.
convert(Bin) when is_binary(Bin) orelse is_bitstring(Bin) ->
    convert(Bin, hex).

%% @doc Convert binary to hex string or binary (base-2) string.
-spec convert(binary() | bitstring(), hex | bin) -> {ok, list()}.
convert(Bin, hex) when is_binary(Bin) orelse is_bitstring(Bin) ->
    convert(Bin, [], fun byte_to_hexstr/1);
convert(Bin, bin) when is_binary(Bin) orelse is_bitstring(Bin) ->
    convert(Bin, [], fun byte_to_binstr/1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                               Internals                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec apply_opts(iolist(), opts()) -> ok | iolist() | binary().
apply_opts(IoList, []) ->
    io:format("~s~n", [IoList]);
apply_opts(IoList, [{return, iolist}]) ->
    IoList;
apply_opts(IoList, [{return, binary}]) ->
    iolist_to_binary(IoList);
apply_opts(IoList, [{printer, Fun}]) when is_function(Fun) ->
    Fun(IoList);
apply_opts(_, _) -> erlang:error(badarg).

-spec convert(binary() | bitstring(), list(), function()) -> {ok, string()}.
convert(<<>>, Acc, _) ->
    {ok, lists:reverse(Acc)};
convert(Bin, [], FormatFun) when is_bitstring(Bin), not is_binary(Bin) ->
    %% byte align bistring() to make a complementary binary()
    Align = (8 - (bit_size(Bin) rem 8)),
    error_logger:info_msg("Aligned bitstring with ~.10B bit(s).~n", [Align]),
    convert(<<Bin/bitstring, 0:Align>>, [], FormatFun);
convert(<<Bin:8/integer, Rest/binary>>, SoFar, FormatFun) ->
    convert(Rest, [FormatFun(Bin)|SoFar], FormatFun).

print_buckets(Buckets) ->
    {Printed, _} = lists:mapfoldl(
                     fun(Bucket, Offset) ->
                             B = print_bucket(Bucket),
                             %% OLD Annotated = io_lib:format("~4.16.0B ~s", [Offset, B]),
                             Annotated = io_lib:format(" ~2.16.0B| ~s", [Offset, B]),
                             {Annotated, Offset+1}
                     end, 0, Buckets),
    Printed.

print_bucket(Bucket) ->
    OctetLine = string:join(Bucket, [?SPACE]),
    OctetHex = lists:map(
                 fun(B) ->
                         io_lib:format("~2.16.0B ", [list_to_integer(B, 2)])
                 end,
                 Bucket),
    OctetRepr = lists:map(
                  fun(B) ->
                          case list_to_integer(B, 2) of
                              Code when Code >= ?SPACE -> Code;
                              _ -> ?SPECIAL
                          end
                  end,
                  Bucket),

    io_lib:format(
      "~s | ~s| ~s~n",
      [
       string:pad(OctetLine, 8 * 8 + 7),
       string:pad(OctetHex,  8 * 3),
       OctetRepr
      ]).

-spec print_comparison(list(), list()) -> ok.
print_comparison([], []) ->
    ok;
print_comparison([L|LRest], [R|RRest]) ->
    Zfill = fun(Line) -> string:left(Line, 16*2 + 16, ?SPACE) end,
    DiffL = Zfill(string:join(L, [?SPACE])),
    DiffR = Zfill(string:join(R, [?SPACE])),
    io:format("~s  ~s~n", [DiffL, DiffR]),
    print_comparison(LRest, RRest).

-spec diff(list(), list()) -> {ok, {list(), list()}}.
diff(L1, L2) when is_list(L1), is_list(L2) ->
    diff(L1, L2, [], []).

-spec diff(list(), list(), list(), list()) -> {ok, {list(), list()}}.
diff([], [], LD, RD) ->
    {ok, {lists:reverse(LD), lists:reverse(RD)}};
diff([], [H2|R2], LD, RD) ->
    diff([], R2, [?MISSING|LD], [H2|RD]);
diff([H1|R1], [], LD, RD) ->
    diff(R1, [], [H1|LD], [?MISSING|RD]);
diff([H1|R1], [H1|R2], LD, RD) -> %% H1 =:= H2
    diff(R1, R2, [?EQUAL|LD], [?EQUAL|RD]);
diff([H1|R1], [H2|R2], LD, RD) ->
    diff(R1, R2, [H1|LD], [H2|RD]).

-spec byte_to_hexstr(byte()) -> string().
byte_to_hexstr(B) when B >= 0, B =< 255 ->
    to_hexstr(B, 16, 2).

-spec byte_to_binstr(byte()) -> string().
byte_to_binstr(B) when B >= 0, B =< 255 ->
    to_hexstr(B, 2, 8).

-spec to_hexstr(byte(), non_neg_integer(), non_neg_integer()) -> string().
to_hexstr(B, Base, Len) ->
    string:right(integer_to_list(B, Base), Len, ?FILL).

%% @doc Divide list L into X lists of size N
%% courtesy of MononcQc
-spec buckets(non_neg_integer(), list()) -> list(list()).
buckets(N, L) ->
    buckets(1, N, length(L) div N, L, [[]]).
buckets(_, _, 0, [], [[]|Acc]) ->
    lists:reverse(Acc);
buckets(_, _, 0, Rest, [[]|Acc]) ->
    lists:reverse([Rest|Acc]);
buckets(N, N, M, [H|T], [A|Acc]) ->
    buckets(1, N, M-1, T, [[], lists:reverse([H|A]) | Acc]);
buckets(X, N, M, [H|T], [A|Acc]) ->
    buckets(X+1, N, M, T, [[H|A]|Acc]).
