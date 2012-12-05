%%%
% This module is used to do proper Geographic queries
% which take into consideration the non-2D nature of the Earth.
%
% We use the data manager and underlying R-Tree to represent the Earth's
% surface as 2D in Lat/Lng, which is effiecient as a primarily filter.
% This module provides the secondary filtering and data manipulation to
% translate to reality.
%
%%%
-module(teles_geo_query).
-export([query_within/2, query_around/3, query_nearest/3, distance/2]).
-include_lib("rstar/include/rstar.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% Earth's radius in meters
-define(RADIUS_METERS, 6378137.0).

% Multiplier to convert degrees to radians
-define(DEGREES_TO_RAD, 0.017453292519943295).

% Queries within a box, since the box is provided already, no adjustment
% is necessary
query_within(Space, SearchBox) ->
    teles_data_manager:query_within(Space, SearchBox).


% Search around a point. We replace this query with a search rectangle
% that adjusts for narrowing longitude
query_around(Space, SearchPoint, Distance) ->
    % TODO
    teles_data_manager:query_around(Space, SearchPoint, Distance).


% Search around a point. We replace the K with 2*K, and sort on true
% distance and select the first K
query_nearest(Space, SearchPoint, K) ->
    % TODO
    teles_data_manager:query_nearest(Space, SearchPoint, 2*K).


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


-ifdef(TEST).

distance_test() ->
    A = rstar_geometry:point2d(47.123, 120.567, undefined),
    B = rstar_geometry:point2d(45.876, 123.876, undefined),
    ?assertEqual(289038.07836511626, distance(A, B)).

distance_near_test() ->
    A = rstar_geometry:point2d(47.123, 120.567, undefined),
    B = rstar_geometry:point2d(47.276, 120.576, undefined),
    ?assertEqual(17045.480008358903, distance(A, B)).

-endif.
