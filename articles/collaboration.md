# Collaborative analysis

One of the core purposes of `orderly` is to allow collaborative
workflows. For this to work we need to be able to find packets that were
run by someone else, somewhere else, or to share packets that we have
run with others. To do that, `orderly` has the idea of “locations”.

Locations behave in a similar way to “remotes” in `git`, and some of the
terminology for interacting with them is similar (notably we can `pull`
from and perhaps `push` to a location). However, we have used a
different name because every `orderly` working directory has at least
one location - the `local` location - which is not remote at all. We
also imagine that almost everyone using `orderly` will be using `git`
and as `git` remotes and `orderly` locations refer to totally different
things this slightly different terminology may help keep things clearer.

Packets that are distributed by locations might have been created by
`orderly`, but may have been created by some other system that
implements the `outpack` spec (see
[`vignette("metadata")`](https://mrc-ide.github.io/orderly/articles/metadata.md)).
As a result, through this documentation you will see reference to
software or details to do with `outpack` but if you only use `orderly`
then you can read that as being synonymous with `orderly`.

``` r
library(orderly)
```

## Type of location

`orderly` supports two types of locations by default:

- `path`: these are any `orderly` working copy that you can find on your
  filesystem. Note that this could be a copy that other people can see
  (for example on a network share, or on a cloud-synced file system such
  as Dropbox or OneDrive) see [Sharing packets with collaborators using
  a shared file
  system](#sharing-packets-with-collaborators-using-a-shared-file-system)
  for more details
- `http`: these require running an HTTP API, either via
  [`outpack_server`](https://github.com/mrc-ide/outpack_server) or
  [`packit`](https://github.com/mrc-ide/packit)

The location system is somewhat extensible, but the details of this are
subject to change.

All the documentation below will behave the same regardless of where the
location is stored and the mechanism of transport, so we can focus
instead on workflows.

## An example

Suppose we have two researchers, [Alice and
Bob](https://en.wikipedia.org/wiki/Alice_and_Bob), who are collaborating
on a piece of analysis. For the purposes of this example (and we find
the most common situation in practice) they share a source tree with
their analysis in git.

Both Alice and Bob clone this repo onto their machines, using their
favourite git client (at the terminal with `git`, via GitHub desktop, or
as here with [`gert`](https://docs.ropensci.org/gert/))

At the moment, including hidden files (except `.git`), Alice sees:

    ## /tmp/RtmpdTZBAe/file1f5972186fc9/alice
    ## ├── orderly_config.json
    ## └── src
    ##     └── data
    ##         └── data.R

At this point, after cloning Alice does not have the `.outpack`
directory (see below)

``` r
orderly_metadata_extract()
## Error in `orderly_metadata_extract()`:
## ! orderly directory '/tmp/RtmpdTZBAe/file1f5972186fc9/alice' not
##   initialised
## ✖ Did not find an '.outpack' directory within path
## ℹ Please run orderly_init("/tmp/RtmpdTZBAe/file1f5972186fc9/alice") to
##   initialise
## ℹ See ?orderly_init for more arguments to this function
```

Alice needs to run
[`orderly_init()`](https://mrc-ide.github.io/orderly/reference/orderly_init.md)
first:

``` r
orderly_init(path_alice)
## ✔ Created orderly root at '/tmp/RtmpdTZBAe/file1f5972186fc9/alice'
## ✔ Wrote '.gitignore'
```

after which Alice can use `orderly` commands:

``` r
orderly_metadata_extract()
## [1] id         name       parameters
## <0 rows> (or 0-length row.names)
```

The plan is to work with Bob, sharing results on their shared
“[Packit](https://github.com/mrc-ide/packit)” server, so Alice adds that
to her `orderly` configuration with
[`orderly_location_add_packit()`](https://mrc-ide.github.io/orderly/reference/orderly_location_add.md):

``` r
orderly_location_add_packit("server", "http://packit.example.com")
```

Once done, she can run any analysis:

``` r
id <- orderly_run("data")
## ℹ Starting packet 'data' `20260121-095938-d4230366` at 2026-01-21 09:59:38.834348
## > orderly_artefact("data.rds", description = "Final data")
## > saveRDS(mtcars, "data.rds")
## ✔ Finished running data.R
## ℹ Finished 20260121-095938-d4230366 at 2026-01-21 09:59:38.891744 (0.05739522 secs)
```

Perhaps it takes several goes for Alice to be happy with the analysis,
but at some point she has something ready to share. She can then “push”
the final packet up onto their server:

``` r
orderly_location_push(id, "server")
## ℹ Pushing 2 files for 1 packet
## ⠙ Pushing file 1 / 2 (85 B)
## ✔ Uploaded 2 files in 20ms
## 
## ℹ Fetching metadata from 1 location: 'server'
## ✔ Found 1 packet at 'server', of which 0 are new
```

Now, consider Bob. He also needs the source code cloned, `orderly`
initialised, and the server added:

``` r
orderly_init(path_bob)
## ✔ Created orderly root at '/tmp/RtmpdTZBAe/file1f5972186fc9/bob'
## ✔ Wrote '.gitignore'
```

``` r
orderly_location_add_packit("server", "http://packit.example.com")
```

(Here, Bob has also used the name `server` to refer to their shared
server, but could have used anything; this is similar to git’s use of
`origin`.)

Bob can now query for packets available on the server:

``` r
orderly_metadata_extract(
  name = "data",
  allow_remote = TRUE, fetch_metadata = TRUE)
## ℹ Fetching metadata from 1 location: 'server'
## ✔ Found 1 packet at 'server', of which 1 is new
##                         id local location name parameters
## 1 20260121-095938-d4230366 FALSE   server data
```

Having seen there is a new “data” packet here, he can pull this down
locally:

``` r
orderly_location_pull(id)
## ℹ Pulling 1 packet: '20260121-095938-d4230366'
## ℹ Looking for suitable files already on disk
## ℹ Need to fetch 2 files (1.3 kB) from 1 location
## ✔ Unpacked 1 packet
```

Now Bob is in a position to develop against the same packet that Alice
ran (`20260121-095938-d4230366`)

## Possible working patterns

We have seen several broad patterns of distributing packets.

**Central server, push allowed**: This is the case as above; this is
very flexible but requires that everyone is relaxed about how packets
are created as you are ultimately trusting that your colleagues are
exercising good environment hygiene. Just because rules are not being
enforced by the computer though, doesn’t mean that you might not have
ideas within the group about who can or should push. It may be useful to
only push from your HPC system after running computationally demanding
tasks there.

**Central server, final copy is run here**: In this case, Alice and Bob
can both run things on the server, and these are the “canonical” copies,
nothing is pushed to the server. Reports can be run on the server
managed by [Packit](https://github.com/mrc-ide/packit/) using a friendly
web interface.

This approach works well where a primary goal is confidence that the
packets everyone works with as dependencies are created under a “clean”
environment with no unexpected global dependencies. With the web
version, you can also enforce things like only running off the default
branch. Alice and Bob will then end up with a collection of packets in
their local archives that are a mix of canonical ones (from the server)
and locally-created ones, but the local ones are always a dead end as
they never get shared with anyone. As a result, Alice and Bob may delete
their archive directories without any great concern.

When running their own packets, to make sure that the packet will run in
a similar way to a packet run on the server, Bob may want to ensure that
only dependencies that could be found on the server will be considered.
To that, he can pass the `location` argument to
[`orderly_run()`](https://mrc-ide.github.io/orderly/reference/orderly_run.md),
which controls how search is performed:

``` r
orderly_run(..., location = "server")
```

and then pass this in to `orderly_run` when running a report. In
developing, Bob may find passing this into
[`orderly_interactive_set_search_options()`](https://mrc-ide.github.io/orderly/reference/orderly_interactive_set_search_options.md)
helpful as that will mean that any calls to
[`orderly_description()`](https://mrc-ide.github.io/orderly/reference/orderly_description.md)
will use only packets from the server.

**Staging and production servers**: You might set up multiple servers,
one or more for staging, and one for production. The staging servers are
never the source of truth and you might (or might not) allow people to
push to them, or have less strict rules about what gets run on them
(perhaps allowing running packets from a feature branch). In this
setting, users will end up with packets that were created locally, from
staging machines and from the production server in their local archive.
The staging server will probably end up with a mix of its own packets
and packets from production. The same approach with specifying search
options as above will be useful here for choosing what is included in
any final packet.

## Pulling complete trees, or not

When you pull packets from a server (with
[`orderly_location_pull()`](https://mrc-ide.github.io/orderly/reference/orderly_location_pull.md)),
depending on your settings you will either end up with just the packet
that you request, or that packet **plus** all of its recursive
dependencies. This is controlled by the configuration option
`core.require_complete_tree`, which is `FALSE` by default; this is
suitable for most users, but servers should be set with
`core.require_complete_tree = TRUE`.

In most of the situations above, the users are never holding the
canonical source of the packets - so it does not matter if they don’t
have all if their dependencies. They do hold all the metadata for all
packets (again, recursively) and could download the dependencies later
if they wanted to inspect them. Typically though, this would just be
extra space used up on disk.

When you push a packet to a server, it always pushes the complete tree.

## Moving as little data as possible

When pulling packets, first the user pulls metadata from the server.
When `orderly_location_metadata_pull` is run, all metadata from the
server is updated on the client. We now know everything the server
holds, and the hashes of all the files contained in each packet.

It is very likely that we already know of some of the files within a
packet being requested, so the client first looks to see if these exist
within its archive, verifying that they have not been changed. It then
requests from the server all the files that it cannot resolve locally.
If doing a recursive pull (with `core.require_complete_tree = TRUE`)
then we look at the union over all files in the set of packets that are
currently missing from our archive. This is frequently much less than
the full set of files within the packets.

The algorithm is similar for a push:

- look at the full dependency chain of our target packet and find all
  file ids and ask the server which if these it does not yet know about
- in the unknown subset we get the union of file hashes and ask the
  server which subset of files it does not yet know about
- we then push up files - this is done one at a time at present but
  could be done simultaneously with async requests in future
- once all files are on the sever we upload the missing metadata, in
  chronological order and the server verifies that for each packet it
  has metadata for all dependencies and all referenced files

This multi-step process means that we avoid copying data that is already
known about, as well as avoiding moving the same data multiple times. It
also means that if the process of pushing (or pulling) is interrupted it
can be safely resumed as the state is always consistent.

## Interaction between source tree and `.outpack`

It is important that the `.outpack` directory is *not* shared via git;
we warn about this now, and you can use `ordery_gitignore_update()` to
automatically create a suitable `.gitignore` file that will prevent it
being accidentally committed. See
[`vignette("troubleshooting")`](https://mrc-ide.github.io/orderly/articles/troubleshooting.md)
for more information on this.

However,

If Alice and Bob were starting on a new machine they would:

- clone their source repository
- run
  [`orderly_init()`](https://mrc-ide.github.io/orderly/reference/orderly_init.md)
  from within the directory
- run `orderly` commands as normal

Without running
[`orderly_init()`](https://mrc-ide.github.io/orderly/reference/orderly_init.md),
they will get an error prompting them to initialise the repo (see the
initial example).

There is no requirement for different repositories to share
configuration options passed to
[`orderly_init()`](https://mrc-ide.github.io/orderly/reference/orderly_init.md);
so you can enable or disable the file store or have different sets of
locations enabled, depending on your workflows. This means that there
is, in practice, only an association *by convention* between a set of
related `orderly` locations and their source tree and you can have an
`orderly` repository that points at locations that refer to different
source trees!

## Sharing packets with collaborators using a shared file system

One of the simplest ways to share packets with a collaborator is through
a shared file system, for example on a network share, or on a
cloud-synced file system such as Dropbox or OneDrive. You can do this as
follows.

1.  Create a new folder for your `orderly` remote in the shared file
    system. Here Alice has synced to `path_sharepoint_alice`.

2.  Initialise an `orderly` location on the shared file system

``` r
orderly_init(
  root = path_sharepoint_alice,
  path_archive = NULL,
  use_file_store = TRUE,
  require_complete_tree = TRUE
)
## ✔ Created orderly root at '/tmp/RtmpdTZBAe/file1f5972186fc9/sharepoint'
```

which creates an `orderly` store with a file store and a complete tree.
See
[`orderly_init()`](https://mrc-ide.github.io/orderly/reference/orderly_init.md)
for more details.

3.  Add this as a location

``` r
orderly_location_add_path("sharepoint", path = path_sharepoint_alice)
## ℹ Testing location
## ✔ Location configured successfully
## ✔ Added location 'sharepoint' (path)
```

4.  Push any packets you want to share

``` r
orderly_location_push(id, "sharepoint")
## ℹ Pushing 2 files for 1 packet
## ℹ Fetching metadata from 1 location: 'sharepoint'
## ✔ Found 1 packet at 'sharepoint', of which 0 are new
```

Then these will be available for your collaborator to pull. Note that
the data is pushed into a file store in the `.outpack` directory. `.dot`
files are considered hidden by your operating system, so if you have
“show hidden files” off in your file browser you will not see the pushed
packet. But your collaborator can now pull the shared file. To do so,
they will have to:

1.  Sync the same drive to a location on their machine. Here Bob has
    synced to `path_sharepoint_bob`

2.  Add the location

``` r
orderly_location_add_path("alices_orderly", path_sharepoint_bob)
## ℹ Testing location
## ✔ Location configured successfully
## ✔ Added location 'alices_orderly' (path)
```

3.  Pull the metadata and use the packets as desired

``` r
orderly_location_fetch_metadata("alices_orderly")
## ℹ Fetching metadata from 1 location: 'alices_orderly'
## ✔ Found 1 packet at 'alices_orderly', of which 0 are new
```
