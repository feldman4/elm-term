### display

simplest
list of (input, List output) pairs
or, list of Input | Output
current line is kept in a separate buffer, (++) in view
multiple sequential outputs lets processes write to parent stdout/stderr at any
time

- cursor over command/stream => visual

### commands
- ok: ls, cat, echo, pwd, cd, write
- todo: grep, xargs?

  
### execution
easy:
- fixed command list

medium:
- commands masquerade as files

hard:
- commands are strings, executed by an interpreter
- basic scheme modified for IO
- but also somehow composable?? from extracted resources??
