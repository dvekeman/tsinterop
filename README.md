# tsinterop

## Goal

Given a TypeScript definition file (_somelib.d.ts_), generate a definition file for another language.

Additional goals

- Allow splitting up files to avoid huge definition files (*)
- Retain comments

(*) In extreme cases, the IDE will be unable to deal with it

Current supported languages:

**In**

- TypeScript

**Out**

## State

Work in progress and **experimental**

The initial version of was built to convert smartclient.d.ts (140K lines, including comments)