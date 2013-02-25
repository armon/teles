%%%
% This module is used to do proper Geographic queries
% which take into consideration the non-2D nature of the Earth.
%
% We use the underlying R-Tree to represent the Earth's
% surface as 2D in Lat/Lng, which is effiecient as a primarily filter.
% This module provides the secondary filtering and data manipulation to
% translate to reality.
%
%%%
-module(teles_geo_query).
-export([search_around/3, search_nearest/3,
         distance/2, latitudinal_width/1, longitudinal_width/1]).
-include_lib("rstar/include/rstar.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% Earth's radius in meters
-define(RADIUS_METERS, 6378137.0).

% Multiplier to convert degrees to radians
-define(DEGREES_TO_RAD, 0.017453292519943295).

% Constants
-define(PI, 3.141592653589793).
-define(E_SQ, 0.00669437999014).


% Search around a point. We replace this query with a search rectangle
% that adjusts for narrowing longitude. Distance is in meters.
search_around(Tree, SearchPoint, Distance) ->
    % Perform the primary search
    SearchBox = search_box(SearchPoint, Distance),
    Primary = rstar:search_within(Tree, SearchBox),

    % Perform secondary filter on the distance
    [G || G <- Primary, distance(SearchPoint, G) =< Distance].


% Search around a point. We replace the K with 2*K, and sort on true
% distance and select the first K
search_nearest(Tree, SearchPoint, K) ->
    % Do a primary search with 2*K
    Primary =  rstar:search_nearest(Tree, SearchPoint, 2 * K),

    % Sort on the distance to the search point
    Sorted = lists:sort([{distance(SearchPoint, G), G} || G <- Primary]),

    % Select the first K
    [G || {_Dist, G} <- lists:sublist(Sorted, K)].


% Generates a search box from a point and a distance
search_box(SearchPoint, Distance) ->
    % Extract the Lat/Lng
    #geometry{mbr=[{Lat, _}, {Lng, _}]} = SearchPoint,

    % Pad the distance a bit so we over-query
    DistancePad = 1.5 * Distance,

    % Get the lat/lng binding box. We compute the width of a degree
    % of latitude in meters, and then create the bounding box using
    % the padded distance and the width
    LatWidth = latitudinal_width(Lat),
    LatSpread = DistancePad / LatWidth,
    MinLat = Lat - LatSpread,
    MaxLat = Lat + LatSpread,

    % Determine the widest latitude. The value farthest from
    % the equator will result in the smallest longitude width,
    % which creates the largest spread in turn.
    WidestLat = if
        Lat >= 0 -> MaxLat;
        Lat < 0 -> MinLat
    end,
    LngWidth = longitudinal_width(WidestLat),
    LngSpread = DistancePad / LngWidth,
    MinLng = Lng - LngSpread,
    MaxLng = Lng + LngSpread,

    % Create a search box
    #geometry{dimensions=2, mbr=[{MinLat, MaxLat}, {MinLng, MaxLng}]}.


% Estimates the distance between two points using the
% Law of Haversines. Provides a better estimate of distance
% than the Euclidean distance of the R-Tree. Result is in
% meters.
% From http://en.wikipedia.org/wiki/Law_of_haversines
distance(A, B) ->
    #geometry{mbr=[{LatA, _}, {LngA, _}]} = A,
    #geometry{mbr=[{LatB, _}, {LngB, _}]} = B,
    LatArc = (LatA - LatB) * ?DEGREES_TO_RAD,
    LngArc = (LngA - LngB) * ?DEGREES_TO_RAD,
    LatitudeH = math:pow(math:sin(LatArc * 0.5), 2),
    LongitudeH = math:pow(math:sin(LngArc * 0.5), 2),
    T1 = math:cos(LatA * ?DEGREES_TO_RAD) * math:cos(LatB * ?DEGREES_TO_RAD),
    T2 = LatitudeH + T1*LongitudeH,
    DistanceAngle = 2.0* math:asin(math:sqrt(T2)),
    DistanceAngle * ?RADIUS_METERS.


% Returns the width of a latitudinal degree
% in meters for the given Latitude
latitudinal_width(Lat) ->
    LatRad = Lat * ?DEGREES_TO_RAD,
    111132.954 - 559.822 * math:cos(2.0 * LatRad) + 1.175 * math:cos(4.0 * LatRad).


% Returns the width of a longitudinal degree
% in meters for the given Latitude
longitudinal_width(Lat) ->
    LatRad = Lat * ?DEGREES_TO_RAD,
    Numerator = ?PI * ?RADIUS_METERS * math:cos(LatRad),
    Denom = 180 * math:sqrt(1 - ?E_SQ * math:pow(math:sin(LatRad), 2)),
    Numerator / Denom.


-ifdef(TEST).

distance_test() ->
    A = rstar_geometry:point2d(47.123, 120.567, undefined),
    B = rstar_geometry:point2d(45.876, 123.876, undefined),
    ?assertEqual(289038078, trunc(1000*distance(A, B))).

distance_near_test() ->
    A = rstar_geometry:point2d(47.123, 120.567, undefined),
    B = rstar_geometry:point2d(47.276, 120.576, undefined),
    ?assertEqual(17045.480008358903, distance(A, B)).

latitudinal_width_test() ->
    ?assertEqual(110574, round(latitudinal_width(0))),
    ?assertEqual(110649, round(latitudinal_width(15))),
    ?assertEqual(111132, round(latitudinal_width(45))),
    ?assertEqual(111412, round(latitudinal_width(60))),
    ?assertEqual(111694, round(latitudinal_width(90))).


longitudinal_width_test() ->
    ?assertEqual(111319, round(longitudinal_width(0))),
    ?assertEqual(107550, round(longitudinal_width(15))),
    ?assertEqual(78847, round(longitudinal_width(45))),
    ?assertEqual(55800, round(longitudinal_width(60))),
    ?assertEqual(0, round(longitudinal_width(90))).

search_box_equator_test() ->
    Point = rstar_geometry:point2d(0, 0, undefined),
    Box = search_box(Point, 10000), % 10km
    #geometry{mbr=[{MinLat, MaxLat}, {MinLng, MaxLng}]} = Box,

    ?assertEqual(-0.13565538330708235, MinLat),
    ?assertEqual(0.13565538330708235, MaxLat),
    ?assertEqual(-0.1347476677660395, MinLng),
    ?assertEqual(0.1347476677660395, MaxLng).


search_box_offset_test() ->
    Point = rstar_geometry:point2d(45, -120, undefined),
    Box = search_box(Point, 10000), % 10km
    #geometry{mbr=[{MinLat, MaxLat}, {MinLng, MaxLng}]} = Box,

    assert_close(45.0 - 0.134974625, MinLat),
    assert_close(45.0 + 0.134974625, MaxLat),
    assert_close(-120.0 - 0.19069, MinLng),
    assert_close(-120.0 + 0.19069, MaxLng).

assert_close(A, B) ->
    ?assertEqual(trunc(A*10000), trunc(B*10000)).

-endif.
