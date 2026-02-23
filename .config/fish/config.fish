# Fish configuration
# (This file goes in ~/.config/fish/config.fish)

set fish_greeting

# Homebrew (macOS only)
if test (uname) = Darwin
    fish_add_path /opt/homebrew/bin
    fish_add_path /usr/local/bin
end

# Linux user binaries
if test (uname) = Linux
    fish_add_path ~/.local/bin
    fish_add_path ~/.cargo/bin
end

# Alias
alias vim='nvim'

# VI mode with cursor shapes
fish_vi_key_bindings

# Ctrl-r for history search in vi mode (works in insert mode)
bind -M insert \cr history-pager

# Different cursor shapes for insert/normal mode
set fish_cursor_default block
set fish_cursor_insert line
set fish_cursor_replace_one underscore
set fish_cursor_visual block

# Custom prompt function
function fish_prompt
    # Print directory in yellow
    set_color yellow
    echo -n (prompt_pwd)
    set_color normal
    echo -n ' '

    # Print git branch in cyan (no parentheses)
    set branch (git branch --show-current 2>/dev/null)
    if test -n "$branch"
        set_color cyan
        echo -n 'git:'
        echo -n $branch
        set_color normal
        echo -n ' '
    end

    set_color green
    echo -n (whoami)
    echo -n '@'
    echo -n (prompt_hostname)
    set_color normal

    # New line and prompt symbol
    echo
    echo -n '$ '
end
