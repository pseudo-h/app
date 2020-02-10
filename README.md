copied from [FPtje/miso-isomorphic-example](https://github.com/FPtje/miso-isomorphic-example)
see the readme there, because it's still relevant.

client:
1. nix-shell
2. cabal configure --ghcjs
3. cabal new-build

server:
1. nix-shell --arg useServer true
2. cabal configure --ghc
3. cp dist-newstyle/yadda/yadda/yadda/all.js static/
4. cabal new-build
5. cabal new-exec server



## TODO

- [ ] **Automatically recompile**

Introduce ag into the workflow as mentioned in the Miso README:

```
For incremental development inside of the nix-shell we recommend using a tool like entr to automatically rebuild on file changes, or roll your own solution with inotify.

    ag -l | entr sh -c 'cabal build'
```
