## Day 5: Print Queue

https://adventofcode.com/2024/day/5

Completed in QBasic using the [QB64](https://qb64.com) compiler.

Uses [watchexec](https://github.com/watchexec/watchexec) and [shunit2](https://github.com/kward/shunit2) to create a test loop around the qb64 compiler.

Install dependencies (see [below](#dependencies)) and then from the `/Day05` folder, run `./test.sh` to start a hot reload test loop.

You can then edit the .BAS files in any editor, or in the QB64 IDE (slower, but benefit of real time errors/warnings) and auto-formatting.

### Dependencies

I was unable to get [QB64](https://qb64.com) installed in [Nix](https://nixos.org), so Day 05 will require installing a few dependencies manually:

* [QB64](https://qb64.com)
    * Running `aoc-day05.test.sh` assumes that qb64 is in the path and will accept arguments
        * This may or may not be the case depending on how you launch qb64 and on what platform:
            * qb64 must be run from within it's folder (it needs relative access to the `internal` folder) and so scripts are provided to change into that folder, but the default scripts don't pass through arguments.
            * If you add it to the path by creating a symlink from, e.g. `/usr/local/bin` then it won't change into the correct folder.
            * My approach was to create `run_qb64.sh` in the qb64 folder:                
                ```bash
                cd "$(dirname "$(readlink -f "$0")")"
                ./qb64 "$@"
                ```
            * And then: `ln -s /path/to/run_qb64.sh /usr/local/bin/qb64`
* [watchexec](https://github.com/watchexec/watchexec)
* [shunit2](https://github.com/kward/shunit2)