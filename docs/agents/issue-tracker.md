# Issue tracker: repo docs (`docs/issues/`)

Issues and specs for this repo live as markdown files under `docs/issues/`, committed with the code. Nothing is published to an external tracker unless the user asks for it.

## Conventions

- One feature per directory: `docs/issues/<feature-slug>/`
- The spec is the first file, `docs/issues/<feature-slug>/01-<slug>.md`. Its user stories carry story IDs (`^link-k3x9`, see the `to-spec` skill).
- Implementation tickets are one file per ticket at `docs/issues/<feature-slug>/<NN>-<slug>.md`, numbered in dependency order after the spec, never a single combined tickets file.
- Frontmatter holds the state:

  ```yaml
  ---
  status: ready-for-agent      # a triage role from triage-labels.md, or done
  category: bug                # bug | enhancement, set by triage
  blocked_by:
    - "[[02-<slug>]]"
  ---
  ```

- A ticket that implements part of a spec lists the story IDs it covers in a `**Covers:** link-k3x9, link-p02m` line. Every story ID of the spec appears in at least one ticket.
- Comments and conversation history append to the bottom of the file under a `## Comments` heading.
- Rejected enhancements are recorded in `docs/out-of-scope/` (see the `triage` skill).

## When a skill says "publish to the issue tracker"

Create a new file under `docs/issues/<feature-slug>/` (creating the directory if needed). For a spec, apply the triage role as `status:`; for tickets, write the blocking edges as `blocked_by:` wikilinks to the blocking files.

## When a skill says "fetch the relevant ticket"

Read the file at the referenced path. The user will normally pass the path, the feature slug, or a story ID; find a story ID with `git grep` under `docs/issues/`.

## When work is closed

A ticket closes when its slice has landed and been verified (`status: done`) or when triage closes it (`status: wontfix`). Set the status, then move the file to `docs/issues/archive/`. The archive is history: never edit archived tickets except to move them back.

## External tracker

If this repo also projects its issues to Jira, the user-invoked `/to-jira` command does the synchronization. Do not publish, comment, or transition on Jira unless the user asks for it.

## Pull requests as a triage surface

**PRs as a request surface: no.** _(Set to `yes` if this repo treats external PRs as feature requests; `/triage` reads this flag.)_

## Wayfinding operations

Used by `/wayfinder`. The **map** is a file with one **child** file per ticket.

- **Map**: `docs/wayfinder/<effort>/map.md` (the Destination / Notes / Decisions-so-far / Fog body).
- **Child ticket**: `docs/wayfinder/<effort>/issues/NN-<slug>.md`, numbered from `01`, with the question in the body. A `Type:` line records the ticket type (`research`/`prototype`/`grilling`/`task`); a `Status:` line records `open`/`claimed`/`resolved` (new tickets start `open`).
- **Blocking**: a `Blocked by: NN, NN` line near the top. A ticket is unblocked when every file it lists is `resolved`.
- **Frontier**: scan `docs/wayfinder/<effort>/issues/` for files that are open, unblocked, and unclaimed; first by number wins.
- **Claim**: set `Status: claimed` and save before any work.
- **Resolve**: append the answer under an `## Answer` heading, set `Status: resolved`, then append a context pointer (gist + link) to the map's Decisions-so-far in `map.md`.
