-module(opml_parser).

-export([parse/1]).

-include("seymour.hrl").

parse(Opml) ->
    {ok, {_, _, Feeds}, _} = erlsom:parse_sax(Opml, {0, "", []}, fun handle_event/2),
    Feeds.

handle_event({startElement, _, "outline", _, Attrs}, {Depth, Category, Feeds}) ->
    NewDepth = Depth + 1,
    case is_feed_element(Attrs) of
        true ->
            Feed = build_record(Attrs),
            {NewDepth, Category, update_list(Feeds, Feed, Category)};
        false ->
            {NewDepth, get_attribute_value(Attrs, "text"), Feeds}
    end;

handle_event({endElement, _, "outline", _}, {Depth, Category, Feeds}) ->
    NewDepth = Depth - 1,
    NewCategory = case NewDepth of
        0 -> "";
        _ -> Category
    end,
    {NewDepth, NewCategory, Feeds};

handle_event(_Event, State) ->
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

update_list(List, Feed, Category) ->
    {MatchingFeeds, OtherFeeds} = lists:partition(fun(F) -> F#feed.xmlUrl =:= Feed#feed.xmlUrl end, List),
    case length(MatchingFeeds) of
        0 ->
            [Feed#feed{categories = update_category_list(Category, Feed#feed.categories)}|List];
        _ ->
            [ExistingFeed] = MatchingFeeds,
            [ExistingFeed#feed{categories = update_category_list(Category, ExistingFeed#feed.categories)}|OtherFeeds]
    end.

update_category_list(NewCategory, Categories) ->
    case NewCategory of
        "" -> Categories;
        _ -> lists:reverse([NewCategory|Categories])
    end.
