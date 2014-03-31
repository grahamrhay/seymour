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
    Feed1 = lists:nth(1, lists:filter(fun(F) -> F#seymour_feed.title =:= "feed1" end, FeedList)),
    [
        ?_assertEqual("http://feed1.com/home", Feed1#seymour_feed.htmlUrl),
        ?_assertEqual("http://feed1.com/rss", Feed1#seymour_feed.xmlUrl),
        ?_assertEqual(["category1", "category2"], Feed1#seymour_feed.categories)
    ].

test_feed2(FeedList) ->
    Feed2 = lists:nth(1, lists:filter(fun(F) -> F#seymour_feed.title =:= "feed2" end, FeedList)),
    [
        ?_assertEqual("http://feed2.com/home", Feed2#seymour_feed.htmlUrl),
        ?_assertEqual("http://feed2.com/rss", Feed2#seymour_feed.xmlUrl),
        ?_assertEqual(["category1"], Feed2#seymour_feed.categories)
    ].

test_feed3(FeedList) ->
    Feed3 = lists:nth(1, lists:filter(fun(F) -> F#seymour_feed.title =:= "feed3" end, FeedList)),
    [
        ?_assertEqual("http://feed3.com/home", Feed3#seymour_feed.htmlUrl),
        ?_assertEqual("http://feed3.com/rss", Feed3#seymour_feed.xmlUrl),
        ?_assertEqual([], Feed3#seymour_feed.categories)
    ].
