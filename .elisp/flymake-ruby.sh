#!/bin/sh
RUBOCOP="rubocop --format emacs"

function exists {
    which "$1" >/dev/null 2>&1
}

# use rubocop directly
exists rubocop && exec $RUBOCOP "$@"

# use rubocop in the bundle
CWD="$PWD"
abspath="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
until [ "$PWD" = "/" ]; do
    if [ -f Gemfile.lock ]; then
	if grep rubocop Gemfile.lock >/dev/null; then
	    exec bundle exec $RUBOCOP $abspath 2>&1
	fi
	break
    fi
    cd ..
done
cd "$CWD"

# if no rubocop is available, just use ruby syntax check
ruby -c "$@"
