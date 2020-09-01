# MMT LSP Server

**experimental!**

An implementation of an [LSP Server](https://microsoft.github.io/language-server-protocol/) for the MMT language. For an example client, see [VSCode-MMT](https://github.com/UniFormal/VSCode-MMT).

## Current Capabilities

### Unicode Abbreviations
...are implemented as code completion triggered by a "j" keystroke (all abbreviations start with the letter "j").

### Syntax Highlighting (partial)

Due to its intrinsically contextual grammar, MMT uses semantic highlighting for all syntax highlighting. Semantic highlighting is currently not part of the *official* specification. Our implementation follows [this proposal](https://github.com/Microsoft/language-server-protocol/pull/124) as implemented in [LSP4J](https://github.com/eclipse/lsp4j/).

The associated color indices are:

| Index | Type  |
| -----:|:----- | 
| 0| keywords |
| 1| normal comments |
| 2| semantic comments |
| 3| identifier names |
| 4| module delimiter |
| 5| declaration delimiter |
| 6| object delimiter |
| 7| unchecked terms |
| 8| checked terms |
| 9| errored terms |
|10| notations |


