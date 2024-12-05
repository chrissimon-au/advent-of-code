Since I was unable to get [QB64](https://qb64.com) installed in [Nix](https://nixos.org), Day 05 will require installing a few dependencies manually:

* [QB64](https://qb64.com)
    * Running `aoc-day05.test.sh` assumes that qb64 is in the path and will accept arguments
* [watchexec](https://github.com/watchexec/watchexec)
* [shunit2](https://github.com/kward/shunit2)

Then, from the `/Day05` folder, run `./test.sh` to setup the hot reload test loop.

You can then edit the .BAS files in vscode, or in the QB64 IDE (slower, but benefit of real time errors/warnings).