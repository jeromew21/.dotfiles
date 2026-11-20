# jeromemacs

My personal emacs distribution.
Inspired by Spacemacs, Doom Emacs, and AstroNvim.

Some goals are
- Fast startup time.
- Works out of the box
- Support power user features (fuzzy search, LSP, AI completion, etc.)
- Support multiple languages and file formats
- Hide all the shit you don't need

## Languages supported (with LSP)
- C/C++
- Rust
- Python
- Java
- Go
- Zig
- Latex

## File formats supported
- EPUB
- PDF

## AI Features
- GitHub Copilot (via @github/copilot-language-server)

## Dependencies (incomplete)
- Skim (PDF)
- @github/copilot-language-server

### LSP 
- clangd
- rust-analyzer
- gopls
- zls
- pyright
- ruff 

## Fonts
Fonts are important to the user experience. 
Right now, they are managed by a custom manager.
We have custom keybindings for cycling through fonts, and for randomizing fonts.

## Documentation
TODO

## Future
- Experiment with different AI completion engines
- Support Kotlin, C#, and other languages (gasp... JavaScript)
- Make keybindings more ergonomic and consistent
- Explicitly show ending newline
- Font manager: stack, better randomization, bundle fonts, Fontaine?
- Auto generate documentation for keybindings and features
- Write some games in emacs lisp for fun
- Markdown and Latex auto preview
- Statistical analysis of keybinding usage and performance

## Deprecation List
- vterm
- embark
