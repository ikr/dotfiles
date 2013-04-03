export PATH=/Users/ikr/bin:/usr/local/bin:/usr/local/sbin:/usr/local/php-5.4.0/bin:/usr/local/Cellar/ruby/1.9.3-p327/bin:/usr/local/share/npm/bin:$PATH
export EDITOR=emacsclient
export NODE_PATH=/usr/local/share/npm/lib/node_modules
export CLASSPATH=$CLASSPATH:/usr/local/Cellar/clojure-contrib/1.2.0/clojure-contrib.jar

export HISTSIZE=1000000
export HISTFILESIZE=1000000
shopt -s histappend

set completion-query-items 10000
set completion-ignore-case On

alias lg="git log --graph --pretty=tformat:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%an %cr)%Creset' --abbrev-commit --date=relative"

if [ -f `brew --prefix`/etc/bash_completion ]; then
    . `brew --prefix`/etc/bash_completion
fi

parse_git_branch () {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

parse_git_tag () {
  git describe --tags 2> /dev/null
}

parse_git_branch_or_tag() {
  local OUT="$(parse_git_branch)"

  if [ "$OUT" == " ((no branch))" ]; then
    OUT="[$(parse_git_tag)]";
  fi

  echo $OUT
}

RED="\[\033[0;31m\]"
YELLOW="\[\033[0;33m\]"
GREEN="\[\033[0;32m\]"
NO_COLOUR="\[\033[0m\]"

PS1="\w$YELLOW\$(parse_git_branch_or_tag)$NO_COLOUR\$ "

# {{{
# Node Completion - Auto-generated, do not touch.
shopt -s progcomp
for f in $(command ls ~/.node-completion); do
  f="$HOME/.node-completion/$f"
  test -f "$f" && . "$f"
done
# }}}

#sync history after every command
shopt -s histappend
export PROMPT_COMMAND='history -a;'
