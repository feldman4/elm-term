```
$ help

  help           - print help to stdout
  l              - show directory contents
  ls             - show directory contents
  pwd            - print working directory
  cd [directory] - change directory
  echo [string]  - send text to stdout
  cat [file]     - send file contents to stdout
  write [file]   - write stdin to file, if file doesn't exist
  append [file]  - append stdin to file, if file exists
  rm [file]      - remove file or directory
  daemons        - show running processes
  spawn [int] [string]
                 - create a process from an executable or by compiling a string,
                   specifying lifetime and name
                     echo "echo blah" | spawn 10 nuisance
                     echo "echo cat ! append hello" | spawn 1 catWriter
                     echo "echo cat" | write exe | cat exe | spawn 1 runOnce
  kill [string]  - kill process
  [cmd] | [cmd]  - combine commands by piping stdout to stdin
  compile [string]
                 - create a command with given name from stdin, with the
                   pipe symbol `|` replaced by `!`
  run [file]     - run a command on the contents of a file
                   the following are equivalent if tmp does not exist
                     echo [expr] | compile exe | run file
                     echo [expr] | compile exe | write tmp | cat file | tmp | rm tmp
                     
```
