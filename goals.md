### display
easy:
- History a = List (Input a | Output a)
- input buffer, (++) in view
- processes can write to parent stdout/stderr at any time

medium:
- cursor over command/stream => visual

### commands
- save commands to FileSystem as executables
- cat executable -> name?
- provide "null" to stream
type File = Executable Name CommandIO | Stream String

library:
- ok: ls, cat, echo, pwd, cd, write
- ok: spawn, daemons
- todo: grep, xargs?


### execution
easy:
- fixed command list

medium:
- File = Executable Name CommandIO | Stream String
- executables are single input, single output
- cat executable => "no such file"
- parseDisk: FileSystem a -> (a -> Maybe (CommandIO a)) -> Maybe (CommandIO a)

hard:
- commands are strings, executed by an interpreter
- basic scheme modified for IO
- but also somehow composable?? from extracted resources??
