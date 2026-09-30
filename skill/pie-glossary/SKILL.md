---
name: pie-glossary
description: "Create or update a Pie glossary so dictation recognizes the user's words. Use when asked to write or refresh .pie/glossary.md, when Pie mishears project names, tools, or commands, or when adding vocabulary the speech recognizer should spell correctly."
disable-model-invocation: false
---

# Pie glossary

Create or update a glossary so Pie's dictation spells this user's words correctly. The usual file is `.pie/glossary.md` in a project root. Words used in every project go in the global glossary instead.

The user dictates Polish full of English programming jargon. Recognition often writes those English words as Polish phonetic spellings, or with Polish inflection.

## What Pie does with the file

Pie reads the glossary at every key press and gives it to both passes. Saving the file is enough. Do not restart Pie.

- Pass 1 is speech to text. The glossary is recognition context, so the recognizer favors these spellings.
- Pass 2 is the optional correction or translation step. The same text is inserted through the `{{glossary}}` placeholder in the user's prompt. Pie adds no glossary instructions of its own.
- Both files are included, the global file first, each under a heading that names its path. A missing file is skipped. An edit is visible on the next key press.

## Where the files are

**Project glossary.** `.pie/glossary.md` in the nearest ancestor of the focused window's working directory. That directory is the process inside the window (the shell or editor under the terminal), not the directory the terminal was launched from. Pie walks upward through `$HOME` inclusive, and through `/` only when the working directory is outside `$HOME`. The nearest file wins. A browser or Slack window has no project directory, so only the global glossary is used.

Put the project file in the project root the user dictates from. Do not put cross-project words in `$HOME/.pie/glossary.md`; that file would apply to every directory under `$HOME` that has no closer glossary. Those words belong in the global file.

**Global glossary.** `glossary.md` beside Pie's `stt.toml` (default `~/.config/pie/stt.toml`, or `$XDG_CONFIG_HOME/pie/stt.toml` when that variable is set). `PIE_GLOSSARY` names a different file when it is set. The systemd service can set that variable; the file is still read at every key press.

Symlinks are followed. There is no size cap.

## Format

A Markdown list, one term per line. Optional first line: one short sentence describing the project, in the language the user dictates (usually Polish). That line is not a term.

```markdown
Pie — asystent dyktowania.

- Eio — ejo, e i o
- OpenRouter — open router, open routera
- Nema — NEMa, nema
```

- The correct spelling comes first, then an em dash (`—`), then the ways speech recognition mishears it, comma-separated.
- Keep the correct spelling in the form the user wants typed: English technical terms stay English. Do not translate them into Polish.
- Mishearings are wrong spellings, not definitions. Prefer a mishearing you have actually seen. Otherwise write the Polish phonetic spelling a recognizer that expects Polish would produce, including a split form (`e i o`, `open router`) and a common Polish inflection (`open routera`) when the word is declined in speech.
- Do not list the correct spelling as a mishearing.
- One term per line. No tables, no nested lists, no headings for individual terms.

Read [references/examples.md](references/examples.md) when you are about to write the file and want a worked project glossary.

## What to collect

Collect names the user actually says and that a general recognizer gets wrong:

- project and package names
- module and library names that are spoken
- CLI commands and flags actually used, written as they are typed (`dune`, `--force`)
- tools, services, and model names
- people and product names specific to this project

Leave out:

- ordinary English or Polish words the recognizer already gets right
- every identifier in the code; a name that is never spoken does not belong
- secrets, tokens, passwords, and hostnames that are credentials

A short glossary of the words that fail is more useful than a dump of the repository. Not every identifier in the code belongs here.

## How to gather it

Read, do not invent a tour of the tree:

- README and CONTEXT.md, for the names the project uses for itself
- build files and package manifests (`dune`, `dune-project`, `*.opam`, `package.json`, `Cargo.toml`, `pyproject.toml`)
- `bin/` entry points and the commands those entry points document
- recent git log subjects, for names people actually say
- the glossary file if it already exists
- `pie history`, when it is available: compare a raw transcript with the final text and keep mishearings that already happened

Ask the user only when a spoken form is ambiguous. Do not interview them for a complete word list.

## Global terms and project terms

A term used across projects (the user's name, a tool they always dictate, a model name) goes in the global `glossary.md`, not copied into every project. A term specific to this repository goes in `.pie/glossary.md`. When you update one file, do not delete terms from the other.

## Updating in place

Read the existing file first.

- Keep every line the user added, including terms you would not have collected and the description line.
- Add missing terms at the end. Extend a term's mishearings when you have a new one; do not drop a mishearing the user wrote.
- Do not delete a line, reorder the user's lines, or rewrite the description unless the user asked for that.
- Do not replace the file with a fresh one.

## Done when

- `.pie/glossary.md` exists in the project root, or the global file was the one that needed the change, and you said which file you edited.
- New lines follow the format above.
- User-added lines are still there.
- You did not restart Pie. The next key press reads the file.
