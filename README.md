Teles
=====

Teles is an Erlang network service for manipulating geographic data.
Specifically, it uses in-memory R*-trees to store and manipulate geo-spacial
indexes. It can be used to quickly provide answers to geo-spacial queries
that are otherwise difficult or tedious to embed within an application.

It was originally written for targetting rewards at Kiip.


Features
--------

* Scalable implementation allows for many connected
  clients and concurrent queries
* Uses R*-trees for efficient queries
* Simple ASCII protocol
* Relatively fast


Install
-------

Download and build from source::

    $ git clone https://armon@github.com/armon/teles.git
    $ cd teles
    $ make deps
    $ make rel

At this point, rel/teles will contain a release build


Usage
-----

Teles can be configured using the sys.config file.
Here is an example configuration file:

    [
     {teles, [
                {teles_agent_concurrency, 4},
                {teles_port, 2856},
                {teles_accept_pool, 8}
            ]}
    ].


Here is a brief description of the configurations:

* teles\_agent\_concurrency : This controls how many duplicate copies of data
  is held. By having more than 1 R-tree, we can perform more queries in parallel.
  This trade-off is made at the cost of memory utilization. If we have 4 agents,
  then we can perform 4 queries at a time, and consequently use 4x the RAM. Additionally,
  if an agent dies, recovery can be performed using another agent to avoid data loss. Sane
  configurations are probably either 1 for no duplication, or the number of CPU's on the box
  if memory allows.

* teles\_port : The port that teles listens on
* teles\_accept\_pool : The number of processes accepting new connections. Default of 8
  probably does not need to be modified.


Protocol
--------

By default, Teles will listen for TCP connections on port 2956.
It uses a simple ASCII protocol that is meant to be fairly human readable,
while being easy to parse.

Each command is newline, and optionally carriage return delimited:

    cmd [args][\r]\n

We start each line by specifying a command, providing optional arguments,
and ending the line in a newline (carriage return is optional).

There are a total of 14 commands:

* use space - Sets the namespace of the connection
* create space - Creates a new namespace. These are for logical seperation of data
  similar to a new 'database' for an RDBMS
* delete space - Deletes a namespace
* list spaces - Lists all available name spaces
* in - This is a command prefix, and executes the next command in a specific namespace
* add object - Adds a named object to a namespace. An object has an opaque name (Teles doesn't care),
and can be associated with various geospacial points.
* delete object - Deletes an object
* list objects - Lists all the objects in the namespace
* list associations - Lists all the points associated with an object
* associate point - Associates an object with a Lat / Lng point
* disassociate - Disassociations an object with a Lat / Lng point
* query within - Queries within a search box for any objects
* query nearest - Queries for the K nearest objects to a given Lat / Lng
* query around - Queries around a Lat / Lng for a given distance for objects


For the ``use`` command, we only space the name of a space to use:

    use cities

This will either return "Done\n" on success or will return "Space does not exist\n",
if a space which is not yet created is specified.

The ``create space`` command just takes the name of a space, which can
already exist, and creates it. It always returns "Done\n":

    > create space states
    Done

Issuing a ``create space`` also causes that space to be used. For this reason, if you
want to use a space and ensure it exists, it is safe to always use create.

The ``delete space`` command is like ``create space`` but may return "Space does not exist".

The ``list spaces`` command takes no arguments but will list all the spaces in a
START/END block like such:

    > list spaces
    START
    cities
    states
    testing
    END

This format is common to all commands that may return a list of results.

The ``in`` command is not a command by itself, but it takes a space and a command
and executes teh command in a given space. For example, to add an object to the 'cities' space,
it is possible to do:

    > in cities add object sanfrancisco

This is an alternative do doing:

    > use cities
    Done

    > add object sanfrancisco
    Done

These are considered equivilent. However, some clients may prefer to use the ``in`` format
as then a single connection can be used to issue commands to multiple spaces. It is possible
to change the space of a connection at any time by doing ``use`` again.

The ``add object`` command takes an opaque name and always returns "Done\n". The object may already
exist. The name has no meaning to Teles and can be an application level ID.

The ``delete object`` command takes an opaque name and returns "Done\n" if successful.
If the object does not exist, then it will return "Object not found\n".

The ``list objects`` command takes no arguments but will list all the objects in the space.
It will also use a START/END block:

    > in cities list objects
    START
    sanfrancisco
    portland
    seattle
    END

The ``list associations with`` command takes an object as an argument and lists all the associated
points in a START/END block:

    > in people list associations with jane
    START
    GID=3059837609 lat=38.8730 lng=-118.3570
    GID=3277219006 lat=40.1230 lng=-120.5120
    END

The results are a single row for each point (lat/lng) that the object is associated with.
Objects can have a one-to-many mapping of points. For example, this could be all the places
that jane has visited. The GID is the Geometry ID, and it is used to remove associations.

To first create an association, the ``associate point <lat> <lng> with <obj>`` command is
used. The latitude and longitude are given as decimal floating point representations, and
the object ID is also provide::

    > in people associate point 38.873 -118.357 with jane
    Done

If an object does not exist, then "Object not found" will be returned.
If an object is already associated with the given point, nothing will change.

Removing an association is done with the ``disassociate`` command::

    > in people disassociate 3277219006 with jane
    Done

This will disassociate jane with the GID (Geometry ID) 3277219006. These
can be found using list associations. This command can also return "Object not found\n"
or "GID not associated\n"

Finally, after objects are added, queries can be done to find all matching objects.
There are 3 types of queries: window, nearest, and around. The window query creates
a lat/lng box and finds all points inside. Nearest takes a point and finds the K nearest
neighbors. Finally, around takes a point and searches for some distance around it.

A window query is done by using ``query within``:

    > in people query within 40.123 45.451 -122.1 -120.3
    START
    mary
    jane
    joe
    END

This will find all objects where latitude is between 40.123 and 45.451,
and longitude is between -122.1 and -120.3.

A nearest query is done by using ``query nearest``:

    > in people query nearest 5 to 40.11 -120.3
    START
    courtney
    grantland
    adam
    amadeus
    jack
    END

This finds the 5 nearest objects to the point 40.11 -120.3.

An around query is done using ``query around``:

    > in cities query around 40.11 -120.3 for 15mi
    START
    sanfrancisco
    oakland
    marin
    marin
    dalycity
    END

This finds all the objects within 15 miles of 40.11 / -120.3.
THe distance measure is expected to be in units of meters, however
the following suffixes are understood:

* m: Meters      (1000m)
* km: Kilometers (15km)
* mi: Miles      (5mi)
* y: Yard        (2000y)
* ft: Feet       (5000ft)

There are also various error codes that are possible. If arguments
are bad, the server can return: "Client Error: Bad arguments\n".
If latitude or longitude is bad, then "Client Error: Bad lat/lng format\n"
may be returned. Lastly commands that require a namespace (either through use
of ``in`` or ``use space``) will return "Client Error: Must use a namespace\n"
if one is not provided.


Example
----------

Here is an example of a client flow, assuming teles is
running on the default port using just telnet::

    $ telnet localhost 2856
    > list spaces
    START
    END

    > create space testing
    Done

    > list spaces
    START
    testing
    END

    > list objects
    START
    END

    > add object jill
    Done

    > add object jack
    Done

    > add object joe
    Done

    > delete object joe
    Done

    > list objects
    START
    jack
    jill
    END

    > associate point 40.123 -120.515 with jack
    Done

    > associate point 40.123 -120.515 with jill
    Done

    > list associations with jack
    START
    GID=2192486008 lat=40.1230 lng=-120.5150
    END

    > disassociate 2192486008 with jack
    Done

    > list associations with jack
    START
    END

    > query nearest 5 to 40.123 -120.515
    START
    jill
    END

    > delete space testing
    Done


Clients
----------
Here is a list of known client implementations:

* Python : https://github.com/armon/pyteles


Here is a list of "best-practices" for client implementations:

* Maintain a set of open connections to the server to minimize connection time
* Doing a 'use space' at the start of the connection avoids the need to
  specify 'in <space>' before each connection

Performance
-----------

Casual testing was performed on a 2012 Macbook Pro with default configurations,
and a concurrency of 4 per space. Average insert time was 500μs, resulting in an
total of 2000 inserts per second. Performing ``query around`` with a random
lat/lng and random distance between 1 and 100 miles took 300μs but with mutilple clients
about 6000 QPS can be achieved. Lastly, doing ``query nearest`` with a random lat/lng
and between 1 and 50 neighbors took 1.6ms on average, and with 4 clients 2400 QPS
was possible.


References
----------

Packages:

* The R*-tree implemetation is from the [erl-rstar package](http://github.com/armon/erl-rstar).

Related works:

* [R-trees: A dynamic index structure for spacial searching](http://www.cs.jhu.edu/~misha/ReadingSeminar/Papers/Guttman84.pdf)
* [The R*-tree: An Efficient and Robust Access Method for Points and Rectangles](http://www.cs.ucr.edu/~tsotras/cs236/F11/rstar.pdf)
* [Nearest Neighbor Queries](http://postgis.refractions.net/support/nearestneighbor.pdf)
* [Enhanced Nearest Neighbor Search on the R-tree](http://www.cse.cuhk.edu.hk/~adafu/Pub/rtree.ps)

