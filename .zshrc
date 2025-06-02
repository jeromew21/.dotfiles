# I've understood that adding PATH in here is incorrect.
# So, this is all mostly cosmetic. Source a different environment if you need it.

# Enable color support
autoload -Uz colors && colors

# Prompt components
setopt PROMPT_SUBST

# Git branch function
git_branch() {
  local branch
  branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
  [[ -n $branch ]] && echo "%{$fg[blue]%}$branch%{$reset_color%}"
}

# Prompt
PROMPT='%{$fg[yellow]%}%~ %{$fg[cyan]%}$(git_branch)
%{$reset_color%}$ '

alias ls='ls --color=auto'

bindkey -v

# Autosuggestions
source ~/.zsh-autosuggestions/zsh-autosuggestions.zsh

# LAST
source ~/.zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
