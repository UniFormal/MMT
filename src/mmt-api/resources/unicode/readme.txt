These files contains convenient abbreviations for creating unicode characters.
Feel free to add more abbreviations.

unicode-latex-map: We use jCOMMAND if \COMMAND is the LaTeX command creating the symbol.
unicode-ascii-map: We use ASCII art to indicate the Unicode symbol.

These abbreviations are automatically loaded into jEdit.

There are also lexing rules that can be loaded manually inorder to process unexpanded abbreviations.
E.g., saying "rule scala://parser.api.mmt.kwarc.info?UnicodeReplacer" in your .mmt file, makes MMT lex --> as ⟶.
That can be useful if you use an editor that does not support Unicode.