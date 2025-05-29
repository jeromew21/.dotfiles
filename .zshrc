
# Enable color support
autoload -Uz colors && colors

# Prompt components
setopt PROMPT_SUBST

# Git branch function
git_branch() {
  local branch
  branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
  [[ -n $branch ]] && echo "on %{$fg[blue]%}$branch%{$reset_color%}"
}

# Prompt
PROMPT='%{$fg[green]%}%n@%m %{$fg[yellow]%}%~ %{$fg[cyan]%}$(git_branch)
%{$reset_color%}➜ '

# Optional: set a colorful right prompt (RPROMPT)
RPROMPT='%{$fg[magenta]%}[%*]%{$reset_color%}'

alias ls='ls --color=auto'

source ~/.zsh-autosuggestions/zsh-autosuggestions.zsh

# LAST
source ~/.zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
