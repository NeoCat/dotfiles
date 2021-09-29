#!/bin/sh
RUBOCOP="rubocop --format emacs"

exists () {
    which "$1" >/dev/null 2>&1
}

# use rubocop in the bundle
CWD="$PWD"
abspath="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
until [ "$PWD" = "/" ]; do
    if [ -f Gemfile.lock ]; then
	if grep rubocop Gemfile.lock >/dev/null; then
	    opt=""
	    [ -f .rubocop.yml ] && opt="-c .rubocop.yml"
	    exec bundle exec $RUBOCOP $opt "$abspath" 2>/dev/null
	fi
	break
    fi
    cd ..
done
cd "$CWD"

# use rubocop directly
exists rubocop && exec $RUBOCOP "$@"

# if no rubocop is available, just use ruby syntax check
ruby -c "$@"
