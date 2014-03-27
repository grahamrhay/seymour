-module(seymour).

-export([parse_file/1]).

parse_file(FilePath) ->
    {ok, Raw} = file:read_file(FilePath),
    parse(binary_to_list(Raw)).

parse(Opml) ->
    opml_parser:parse(Opml).
