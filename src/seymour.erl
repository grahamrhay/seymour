-module(seymour).

-export([parse_file/1]).

parse_file(FilePath) ->
    {ok, Raw} = file:read_file(FilePath),
    opml_parser:parse(Raw).
