### display
easy:
- History a = List (Input a | Output a)
- input buffer, (++) in view
- processes can write to parent stdout/stderr at any time

medium:
- cursor over command/stream => visual

### commands
- compile commands to FileSystem as executables
- cat executable -> name?
- make commands more generic with Config a =
    - {null : a, fromString : String -> a, append: a -> a -> a}
- need to provide a FileSystem and view map (toString, toHtml, etc)

type File = Executable Name CommandIO | Stream String

compile (expr)
- echo command, upgrade config.fromString to recognize functions
- compile command, passes string and system to config.compile
   - advantageous if we want to compile differently across configs
- compile command, do the compilation and map result with config.fromIOCommand


library:
- ok: ls, cat, echo, pwd, cd, write
- ok: spawn, daemons, kill
- todo: compile, grep, xargs?


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
