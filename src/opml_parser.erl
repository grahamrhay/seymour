-module(opml_parser).

-export([parse/1]).

-include("seymour.hrl").

parse(Opml) ->
    {ok, State, _} = erlsom:parse_sax(Opml, [], fun handle_event/2),
    State.

handle_event({startElement, _NS, "outline", _, Attrs}, State) ->
    case is_feed_element(Attrs) of
        true ->
            Feed = build_record(Attrs),
            update_list(State, Feed);
        false ->
            State
    end;

handle_event(Event, State) ->
    io:format("Event: ~p, State: ~p~n", [Event, State]),
    State.

is_feed_element(Attrs) ->
    Predicate = fun(A) ->
            {attribute, Name, _, _, _} = A,
            Name =:= "xmlUrl"
    end,
    lists:any(Predicate, Attrs).

build_record(Attrs) ->
    Title = get_attribute_value(Attrs, "title"),
    HtmlUrl = get_attribute_value(Attrs, "htmlUrl"),
    XmlUrl = get_attribute_value(Attrs, "xmlUrl"),
    #feed{
       title=Title,
       htmlUrl=HtmlUrl,
       xmlUrl=XmlUrl
    }.

get_attribute_value(Attrs, AttrName) ->
    {attribute, _, _, _, Value} = lists:nth(1, lists:filter(fun(A) -> {attribute, Name, _, _, _} = A, Name =:= AttrName end, Attrs)),
    Value.

update_list(List, Feed) ->
    case length(lists:filter(fun(F) -> F#feed.xmlUrl =:= Feed#feed.xmlUrl end, List)) of
        0 ->
            [Feed|List];
        _ ->
            List
    end.
