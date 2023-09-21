```bash
me@MyMachine:~/projects/fss$ cabаl ./fileify readme.fileify ./readme
... Up to date
me@MyMachine:~/projects/fss$ tree ./readme
./readme
├── "# Have you ever needed to program something, but you forgot to write in a file?"
│   ├── "Alas, me neither"
│   └── "But it's so annoying when it happens!"
├── "...So that's why I created this!"
│   ├── "A language where you never have to use a pesky text editor again!"
│   ├── "It transpiles straight to JS, giving you access to a *huge* ecosystem"
│   └── "Start on your new enterprise today!"
├── "From you, it only needs:"
│   ├── "Access to GHC"
│   ├── "Access to a Linux (or possibly Mac) based machine"
│   └── "And every last bit of your sanity"
└── "Some things of note:"
    ├── "0@The error handling leaves some stuff to be desired"
    ├── "1@The fss compiler only cares about issues that break transpilation"
    ├── "2@To see an example of it working, look at demo.js"
    ├── "3@(It's the formatted output of demo\program\Main.fs)"
    └── "4@Not going to document it rn because why would I, but here're some quirks:"
        ├── "0@For import statements, you need to use backslashes instead of forwards"
        ├── "1@Also the code got a bit messy because uh, I stopped caring, sorry not sorry :("
        └── "2@TODO()"

5 directories, 15 files
me@MyMachine:~/projects/fss$
```