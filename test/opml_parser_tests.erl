-module(opml_parser_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../src/seymour.hrl").

opml_parse_test_() ->
    {foreach, fun read_file/0, [fun test_list_length/1, fun test_feed1/1, fun test_feed2/1, fun test_feed3/1]}.

read_file() ->
    {ok, Raw} = file:read_file("../test/feedly.opml"),
    opml_parser:parse(Raw).

test_list_length(FeedList) ->
    [?_assertEqual(3, length(FeedList))].

test_feed1(FeedList) ->
    Feed1 = lists:nth(1, lists:filter(fun(F) -> F#feed.title =:= "feed1" end, FeedList)),
    [
        ?_assertEqual("http://feed1.com/home", Feed1#feed.htmlUrl),
        ?_assertEqual("http://feed1.com/rss", Feed1#feed.xmlUrl)
    ].

test_feed2(FeedList) ->
    Feed2 = lists:nth(1, lists:filter(fun(F) -> F#feed.title =:= "feed2" end, FeedList)),
    [
        ?_assertEqual("http://feed2.com/home", Feed2#feed.htmlUrl),
        ?_assertEqual("http://feed2.com/rss", Feed2#feed.xmlUrl)
    ].

test_feed3(FeedList) ->
    Feed3 = lists:nth(1, lists:filter(fun(F) -> F#feed.title =:= "feed3" end, FeedList)),
    [
        ?_assertEqual("http://feed3.com/home", Feed3#feed.htmlUrl),
        ?_assertEqual("http://feed3.com/rss", Feed3#feed.xmlUrl)
    ].
