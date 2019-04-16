# tsinterop

## Goal

Given a TypeScript definition file (_somelib.d.ts_), generate a definition file for another language.

Additional goals

- Retain comments
- Allow splitting up files to avoid huge definition files (*) (**)

(*) In extreme cases, the IDE will be unable to deal with it. When the original definition file is well documented this will additionally increase the file size!

(**) Splitting up files is challenging and hard to do right but it has some additional advantages. In this first version many shortcuts have been taken!

Current supported languages:

**In**

- TypeScript

**Out**

## State

Work in progress and **experimental**

The initial version of was built to convert smartclient.d.ts (140K lines, including comments)

## tsinterop vs ts2kt

[`ts2kt`](https://github.com/Kotlin/ts2kt) is the official tool for converting files.
As far as I know it does not support comments neither splitting up the output in several files.

## Next steps

See issues section