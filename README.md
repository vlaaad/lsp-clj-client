# Example usage
1. Download a language server, e.g. https://github.com/clojure-lsp/clojure-lsp/releases
2. Run lint
```shell
clj -Sdeps '{:deps {io.github.vlaaad/lsp-clj-client {:git/sha "57c618d7ecfc9f94fbef9157cfe4534a4816be45"}}}' \
    -X io.github.vlaaad.lsp/lint \
    :cmd '"/Users/vlaaad/Downloads/clojure-lsp"' \
    :path '"."' \ 
    :ext '"clj"'
```

Example output on this repo:
```
file:///Users/vlaaad/Projects/lsp-clj-client/src/io/github/vlaaad/lsp.clj at 168:22:  Redundant let expression.
```