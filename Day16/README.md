## Day 16: Reindeer Maze

https://adventofcode.com/2024/day/16

Completed in [Pharo](https://pharo.org/) with [GToolkit](https://gtoolkit.com/) using [moldable development](https://moldabledevelopment.com/) and [example-driven development](https://lepiter.io/feenk/example-driven-development-ekmic0u0o8swpblzkcdzy608s).

Glamorous Toolkit doesn't run in nix, so to get this working, download and install it from https://gtoolkit.com/.

To get it running, either:

#### Let GToolkit Load It

Glamorous Toolkit likes to control your use of git, so this could be a bit easier, but if the repo is already downloaded, see below.

Once Glamorous Toolkit is running, in a pharo code block - e.g. in a knowledge base, paste the following pharo code which will clone and load the Pharo code. 

```st
Metacello new
	repository: 'github://chrissimon-au/advent-of-code-2024:main/Day16/src';
	baseline: 'AoCDay16';
	load
```

#### Manually clone and load

Clone the repo, and in Glamorous Toolkit, open the 'git' section and choose to load an existing repo. IT should find the `.project` file in the root which will direct it to load the Pharo code module `AoC-Day16`.
