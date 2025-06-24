# I've understood that adding PATH in here is incorrect.
# So, this is all mostly cosmetic. Source a different environment if you need it.

# Enable color support
autoload -Uz colors && colors

# Prompt components
setopt PROMPT_SUBST

# Get current git branch function
autoload -Uz vcs_info

precmd() {
  vcs_info
}

zstyle ':vcs_info:git:*' formats '%F{blue}%b%f'

git_branch() {
  echo $vcs_info_msg_0_
}

# Command line prompt
PROMPT='%{$fg[yellow]%}%~ %{$fg[cyan]%}$(git_branch)
%{$reset_color%}$ '

# Useful aliases
alias ls='ls --color=auto'
alias vim='nvim'

# VI mode
bindkey -v

# Autosuggestions
source ~/.zsh-autosuggestions/zsh-autosuggestions.zsh

# Goes last
source ~/.zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Mess with path. Be careful with this and try to limit the additional path directories(see above note).
export PATH="$HOME/.local/bin:$PATH"

# NVM
load-nvm() {
    unset -f node npm nvm npx
    export NVM_DIR="$HOME/.config/nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
}

node() { load-nvm; node "$@"; }
npm() { load-nvm; npm "$@"; }
nvm() { load-nvm; nvm "$@"; }
npx() { load-nvm; npx "$@"; }
