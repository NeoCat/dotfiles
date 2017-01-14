# ホスト別設定読み込み
[ -r ~/.zshrc_by_host ] && source ~/.zshrc_by_host

###
# 環境変数設定
###
export PATH=$HOME/bin:$PATH:/usr/sbin
export MANPATH=/usr/local/man:$MANPATH
#export EDITOR=emacs
export LSCOLORS="hxExcxdxBxhghdabagacad"
export LS_COLORS="di=01;33:tw=01;33;41:ow=01;33;45"
export NLSPATH=/usr/share/locale/%N
export FTP_PASSIVE=1
export RSH=ssh
export CVS_RSH=ssh
export REPORTTIME=5
which npm >/dev/null && export NODE_PATH=`npm root -g`

# パス重複排除
typeset -U path PATH
path=(
    # allow directories only (-/)
    # reject world-writable directories (^W)
    $path(N-/^W)
)

SYSTEM=`uname`
if [ "$SYSTEM" = "CYGWIN_NT-5.1" ]; then
	SYSTEM="Cygwin"
fi

###
# エイリアスいろいろ
###
if [ $SYSTEM = Darwin ]; then
	alias l="ls -FGv"
	alias ll="ls -FGlav"
	[ ! -e /usr/sbin/ip ] && function ip() { ifconfig }
elif [ $SYSTEM = Linux ]; then
	alias l="ls -F --color"
	alias ll="ls -Fla --color"
else
	alias l="ls -FG"
	alias ll="ls -FGla"
fi
alias j=jobs
alias c=clear
alias cl="c;l"
alias df="df -h"
alias du="du -h"
alias rm="rm -i"
alias cvs="cvs -q -z7"
alias rmmd="rm (*~|.*~|\#*|.\#*)"
alias jp='export LANG=ja_JP.utf8'
alias java='java -Dfile.encoding=UTF-8'
alias javac='javac -J-Dfile.encoding=UTF-8'
alias egrepr="egrep -rnI --color=auto"
alias rsyncr="rsync -avz --del"
alias hex="noglob printf '0x%x\n'"
alias mv='nocorrect mv'			#スペルチェックをしない
alias cp='nocorrect cp'			#スペルチェックをしない
alias mkdir='nocorrect mkdir'	#スペルチェックをしない
alias sudo='nocorrect sudo'	#スペルチェックをしない
alias ssh-config="zed ~/.ssh/config"
alias sc="sudo systemctl"
alias E="emacsclient -a '' -t"
alias -g H='| head'
alias -g T='| tail'
alias -g G='| egrep --color=auto'
alias -g W='| wc'
alias -g P='| peco'
alias -g NL='>/dev/null'
alias -g NLL='&>/dev/null'
alias -g L="| less"
export PAGER=less

#管理者権限で書き込み: /proc, /sys 経由の設定用
function suwrite { echo "$@[-1]" | sudo tee "${@:1:-1}" }

if [ $SYSTEM = 'Linux' ]; then
    netstat_tcp_opts="--tcp"
else
    netstat_tcp_opts="-ptcp"
fi

alias ts="netstat -a $netstat_tcp_opts"
function tf() { netstat $netstat_tcp_opts "$@"  | egrep -v '(::1|127.0.0.1|localhost)[:.].*(::1|127.0.0.1|localhost)[:.]' }

function p() { if [ \! -z "$1" ]; then
    if pids=$(pgrep -f "$@[-1]"); then
	ps "$@[1,-2]" -p $(echo $pids); else echo "No process found."; fi;
    else ps ax; fi }
alias p="nocorrect p"

if [ $SYSTEM = Cygwin ]; then
	alias ifconfig='ipconfig | iconv -f Shift_JIS -t UTF-8'
fi

###
# zsh環境設定
###

#履歴
HISTFILE=$HOME/.zsh-history
HISTSIZE=1000000
SAVEHIST=1000000
setopt hist_ignore_dups
setopt share_history
setopt extended_history
setopt HIST_IGNORE_SPACE
setopt HIST_REDUCE_BLANKS
function history-all { history -E 1 }

#キーバインド
bindkey "[3~" delete-char
bindkey "[3D" backward-word
bindkey "[3C" forward-word
bindkey "[1;3D" backward-word
bindkey "[1;3C" forward-word
bindkey "[5D" backward-word
bindkey "[5C" forward-word
bindkey "[1;5D" backward-word
bindkey "[1;5C" forward-word
if zle -la | grep -q '^history-incremental-pattern-search'; then
  # zsh 4.3.10 以降でのみ有効
  bindkey '^R' history-incremental-pattern-search-backward
  bindkey '^S' history-incremental-pattern-search-forward
fi

autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^p" history-beginning-search-backward-end
bindkey "^n" history-beginning-search-forward-end

#補完
zstyle ':completion:*:default' menu select=1	#tab二回で候補を矢印キーで選択
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'	#小文字は大文字にも一致
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}	#補完候補に色を付ける
setopt completeinword	#単語の途中にカーソルをおいて補完する
setopt magic_equal_subst	#=以降でも補完
autoload -U compinit
compinit -u
setopt extended_glob
setopt correctall
CORRECT_IGNORE='_*'
CORRECT_IGNORE_FILE='.*'
setopt no_beep
setopt auto_param_slash
setopt list_packed		# 補完候補を詰めて表示
setopt list_types		# 補完候補の種類を表示
setopt auto_cd			# ディレクトリ名を実行するとcd, ディレクトリ名も補完されるように
setopt auto_pushd		# cd -[tabキー]で過去のディレクトリを補完
setopt auto_param_keys	# 括弧等の補完

hosts=( ${(@)${${(M)${(s:# :)${(zj:# :)${(Lf)"$([[ -f ~/.ssh/config ]] && < ~/.ssh/config)"}%%\#*}}##host(|name) *}#host(|name) }/\*} )
zstyle ':completion:*:hosts' hosts $hosts      # .ssh/configに指定したホストをsshなどの補完候補に

#プロンプト
unsetopt promptcr		# 改行のない出力をプロンプトで上書きするのを防ぐ
setopt PROMPT_SUBST
PROMPT="%F{green}[%m-%T]%f%# "
RPROMPT="%(?..%F{red}-%?-)%F{green}[%1(v|%F{yellow}%1v%F{green} |)%n:%~]%f"

#自動ログアウト
autoload colors && colors
zmodload zsh/datetime # $EPOCHSECONDS, strftime等
reset_autologout() { IDLETIME=$EPOCHSECONDS }
check_autologout() {
    if [ "$AUTOLOGOUT" -gt 0 -a $[EPOCHSECONDS-IDLETIME] -ge "$AUTOLOGOUT" ]; then
        print "\n$fg[red]*** Auto logout ***$reset_color"
	kill -HUP $$
    fi
}
#プロンプトの時刻を更新
reset_tmout() { TMOUT=$[60-EPOCHSECONDS%60] }
precmd_functions=($precmd_functions reset_tmout reset_autologout)
redraw_tmout() { zle reset-prompt; reset_tmout }
TRAPALRM() { check_autologout; redraw_tmout }

#ウィンドウタイトル
case "$TERM" in
    xterm*|screen*|kterm*)
	print_esc() { print -nr ${1//[^\ -~]/_} }  #unprintableな文字を??に置換
	precmd_wintitle() { print -n "\e]0;"; print_esc "[${PWD/~HOME/~}:$HOST]"; print -n "\a" }
	preexec_wintitle() { print -n "\e]0;"; print_esc "$1 [${PWD/~HOME/~}:$HOST]"; print -n "\a" }
	precmd_functions=($precmd_functions precmd_wintitle)
	preexec_functions=($preexec_functions preexec_wintitle)
	;;
esac

#その他
autoload -U url-quote-magic  && zle -N self-insert url-quote-magic	#URLを貼付時に自動的にエスケープ

autoload zmv				#まとめてリネーム用
alias zmv='noglob zmv'	# zmv -W *.htm *.html など

autoload zed			# 簡易エディタ
autoload zargs			# zargs command

autoload -U zcalc
zmodload -i zsh/mathfunc	# 数値演算
setopt pushd_ignore_dups
setopt no_tify
setopt PRINT_EIGHT_BIT
setopt noflowcontrol
setopt numeric_glob_sort								#数値順でソート
export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'	#単語の区切り

#csh互換
function setenv () {
	if [ $# -eq 0 ]; then
		/usr/bin/env
	else
		export $1=$*[2,-1]
	fi
}
trap "" USR1

#TCP file transfer
function file_recv {
	if [ $# = 0 ]; then
		echo file_recv listen_port
		return
	fi
	(
	autoload -U tcp_open; tcp_open >/dev/null 2>/dev/null # define ztcp
	ztcp -l "$1"
	fd_listen=$REPLY
	echo "Waiting on port $1 (fd $fd_listen) ..."
	ztcp -a $fd_listen || echo failed.
	fd_accept=$REPLY
	echo "Connected. (fd $fd_accept)"
	tar vxf - <&$fd_accept
	ztcp -c $fd_listen
	ztcp -c $fd_accept
	)
}
function file_send {
	if [ $# -lt 3 ]; then
		echo file_send host port files ...
		return
	fi
	(
	autoload -U tcp_open; tcp_open >/dev/null 2>/dev/null # define ztcp
	ztcp "$1" "$2"
	shift 2
	tar vcf - "$@" >&$REPLY
	ztcp -c $REPLY
	)
}

###### git 関連 ######

#gitブランチ名表示
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git:*' formats '%c%u%b'
zstyle ':vcs_info:git:*' actionformats '%c%u%b|%a'

#カレントディレクトリ/コマンド記録
local _cmd=''
local _lastdir=''
preexec_gitupdate() {
  _cmd="$1"
  _lastdir="$PWD"
}
preexec_functions=($preexec_functions preexec_gitupdate)
#git情報更新
update_vcs_info() {
  psvar=()
  LANG=en_US.UTF-8 vcs_info
  [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
}
#カレントディレクトリ変更時/git関連コマンド実行時に情報更新
precmd_gitupdate() {
  _r=$?
  case "${_cmd}" in
    git*|stg*) update_vcs_info ;;
    *) [ "${_lastdir}" != "$PWD" ] && update_vcs_info ;;
  esac
  return $_r
}
precmd_functions=($precmd_functions precmd_gitupdate)

############

# dotfilesの自動アップデート(1日1回)
(: ~/.dotfiles/.git/FETCH_HEAD(md-1)) NLL || (cd ~/.dotfiles; [ "$(git pull)" = "Already up-to-date." ] || echo .dotfiles updated)&!

# .zshrc_by_hostで定義した後処理の実行
if declare -f _postinit_by_host >/dev/null; then
  _postinit_by_host
  unset -f _postinit_by_host
fi

