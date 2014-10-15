#!/bin/bash

# Test for shellshock vulnerabilities
#
# Joe Moss (joseph.v.moss@intel.com) - 9/27/2014

mkdir ./tmp
DIR="./tmp" || exit 1
cd $DIR

trap "" SIGSEGV

DEFAULT_SHL=/bin/bash
SHELLS=
if [ -n "$1" ]; then
	case "$1" in
		-a|-all|--all|all)
			for d in `echo $PATH | tr : '\n'` ; do
				[ -x "$d/sh"   ] && SHELLS="$SHELLS $d/sh"
				[ -x "$d/bash" ] && SHELLS="$SHELLS $d/bash"
			done
			;;
		*)
			SHELLS="$@" ;;
	esac
else
	SHELLS=$DEFAULT_SHL
fi

run_tests() {
	export SHELL=$SHL
	( env var='() { ignore this;}; echo vulnerable' $SHL -c /bin/true ) > CVE-2014-6172.out 2>&1

	( env var='() {(a)=>\' $SHL -c "echo date"; cat echo ) > CVE-2014-7169.out 2>&1

	( $SHL -c 'true <<EOF <<EOF <<EOF <<EOF <<EOF <<EOF <<EOF <<EOF <<EOF <<EOF <<EOF <<EOF <<EOF <<EOF' || echo "vulnerable" ) > CVE-2014-7186.out 2>&1

	( (for x in {1..200} ; do echo "for x$x in ; do :"; done; for x in {1..200} ; do echo done ; done) | $SHL || echo "vulnerable" ) > CVE-2014-7187.out 2>&1

	# See https://lists.gnu.org/archive/html/bug-bash/2014-09/msg00238.html
	$SHL -c "export f=1 g='() {'; f() { echo 2;}; export -f f; $SHL -c 'echo \$f \$g; f; env | grep ^f='" > any_parser_bug.out 2>&1
}

collect_cve_results() {
	VULN=false
	if grep -q vulnerable CVE-2014-6172.out ; then
		echo "CVE-2014-6172: VULNERABLE"
		VULN=true
	else
		echo "CVE-2014-6172: not vulnerable"
	fi

	if grep -q '[0-9]:[0-5][0-9]:[0-5][0-9]' CVE-2014-7169.out ; then
		echo "CVE-2014-7169: VULNERABLE"
		VULN=true
	else
		echo "CVE-2014-7169: not vulnerable"
	fi

	if grep -q vulnerable CVE-2014-7186.out ; then
		echo "CVE-2014-7186: VULNERABLE"
		VULN=true
	else
		echo "CVE-2014-7186: not vulnerable"
	fi

	if grep -q vulnerable CVE-2014-7187.out ; then
		echo "CVE-2014-7187: VULNERABLE"
		VULN=true
	else
		echo "CVE-2014-7187: not vulnerable"
	fi
}

check_shell_type() {
	if env -i $SHL -c 'set TESTVAR=csh; echo $TESTVAR' | grep -q csh; then
		echo csh ; return
	fi
	# Check for sash-style shells
	$SHL > functest.out 2>&1 << EOF
		f() { echo hello; }
		f
EOF
	if ! grep -q hello functest.out ; then
		echo sash ; return
	fi
	if ! $SHL -c "func() { echo hello; }; export -f func > /dev/null 2>&1; env" | grep -q func ; then
		# Pretty much any Bourne derivative not derived from bash
		echo sh ; return
	fi
	echo bash
}

EXITCODE=0
for SHL in $SHELLS; do
	rm -f *.out echo
	if [ ! -x $SHL ]; then
		echo "Can't execute shell $SHL"
		exit 1
	fi

	echo "Evaluating $SHL..."
	SKIPTESTS=true
	type=`check_shell_type`

	case "$type" in
		csh)	echo "This shell is a csh derivative." ;;
		sash)	echo "This shell does not fully support functions" ;;
		sh)	echo "This shell does not support exporting functions to the environment." ;;
		bash)	SKIPTESTS=false ;;
	esac

	FUTUREPROOF=true
	if $SKIPTESTS; then
		echo "It is NOT vulnerable to shellshock attacks. Skipping tests..."
		VULN=false
	else
		echo "Running tests..."
		run_tests

		echo "Tests completed. Determining results..."
		collect_cve_results

		if grep -q '^1 () {$' any_parser_bug.out && grep -q '^2$' any_parser_bug.out ; then
			echo "This shell should be immune to shellshock attack via any other parser bugs"
		else
			echo "VULNERABLE TO FUTURE PARSER BUGS"
			FUTUREPROOF=false
		fi
	fi

	if $VULN ; then
		echo "Overall status: VULNERABLE"
		EXITCODE=1
	else
		if $FUTUREPROOF ; then
			echo "Overall status: Not vulnerable"
		else
			echo "Overall status: Not vulnerable to current exploits"
		fi
	fi
	echo ""
done

exit $EXITCODE

