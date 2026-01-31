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
# alias vim='nvim'

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

    # Print git branch in cyan
    set_color cyan
    echo -n (fish_git_prompt)
    set_color normal

    # New line and prompt symbol
    echo
    echo -n '$ '
end

# Configure git prompt format (optional - customize as needed)
set -g __fish_git_prompt_show_informative_status 0
set -g __fish_git_prompt_showdirtystate 0
set -g __fish_git_prompt_showuntrackedfiles 0
set -g __fish_git_prompt_showupstream none

# Git prompt colors (to match your zsh config)
set -g __fish_git_prompt_color_branch cyan
