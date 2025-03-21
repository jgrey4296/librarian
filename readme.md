# Librarian

General controller for looking up words, documentation, and searching online

- librarian-file-management : for managing config files
- librarian-backends        : generalized backends for the other areas of librarian 
- librarian-bibliography    : bibtex utils
- librarian-browser         : for opening browsers
- librarian-configs         : tracking and editing emacs config files and spec defs
- librarian-documentation   : xref and lookup configuration
- librarian-man             : caching and calling man/woman
- librarian-online          : handlers for querying the internet in a browser
- librarian-regular         : quick access to regular urls
- librarian-tagging         : mode-agnostic tag setting/reading/database
- librarian-utils           : misc utils for other areas of librarian
- librarian-words           : access to dictionaries


## Browser Control
`librarian-browse-open` uses local file system calls, http browsers, and `eww`, to open the relevant program.

## Documentation Lookup
Using `thingatpt`, `x-ref`, `eldoc`

## Docset lookup
`librarian-docsets-consult` are aliases over `dash-docs`.

## Online Search
`librarian-online` uses `counsel-search` or `helm` into the selected browser above,
while looking in `librarian--online-providers`, which are registered with `librarian--online-register-provider`.


## Spelling
`librarian-words-definition` `librarian-words-synonyms` are facades over:
- `wordnut`,
- `synosaurus`,
- `osx-dictionary`,
- `define-word`,
- `flyspell`,
- `wordnut`
- `helm-wordnet`

## Bibliography Search
in `bibtex`
maybe compilation to latex
metadata retrieval using `calibre`

## Bookmark Search

## Config file lookup

## Man customisation

## Tagging

## Envs

### marker file formats
Looks for one of `librarian-envs-handling--markers`, typically `.lenvs`.
Format is:
```
# comments
# single lines
# {handler} arg arg arg
# eg:
mamba doot-dev312
lsp   

```
