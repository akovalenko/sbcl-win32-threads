# For some build environments, file system hierarchy, style and
# filename constraints are different for host CL and target SBCL.  In
# that case, targetpath and hostpath should be set to file name
# conversion commands or shell functions.
#

targetpath=${SBCL_ENV_TARGETPATH:-"echo -n"}
hostpath=${SBCL_ENV_HOSTPATH:-"echo -n"}
devnull=/dev/null

# Cygwin and MSYS set OSTYPE by themselves (hence a strange name that
# I wouldn't choose but have to accept). For "combined cross-native"
# builds with e.g. Linux host and wine environment, OSTYPE has to be
# set manually.

if [ "$OSTYPE" = "cygwin" -o "$OSTYPE" = "msys" -o "$OSTYPE" = "wine" \
     -o -n "$windir" ] ;
then
    definstallroot="${PROGRAMFILES:-C:/Program Files}/Steel Bank Common Lisp/"
    installroot=${SBCL_ENV_INSTALL_ROOT:-$definstallroot}
    runtime=sbcl.exe
    sbcl_os=win32
    sbcl_arch=x86
    case "$OSTYPE" in
	cygwin)
	    targetpath=${SBCL_ENV_TARGETPATH:-"cygpath -w"}
	    hostpath=${SBCL_ENV_HOSTPATH:-"cygpath -u"}
	    lncp="ln -s"
	    ;;
	wine)
	    targetpath=${SBCL_ENV_TARGETPATH:-"winepath -w"}
	    hostpath=${SBCL_ENV_HOSTPATH:-"winepath -u"}
	    lncp="ln -s"
	    ;;
	*)
	    lncp="cp -r"
	    ;;
    esac
else
    installroot=${SBCL_ENV_INSTALL_ROOT:-"/usr/local"}
    runtime=sbcl
fi

if [ -z "$sbcl_os" ] ; then
    case `uname` in
	Linux)
            sbcl_os="linux"
            ;;
	OSF1)
        # it's changed name twice since it was called OSF/1: clearly
        # the marketers forgot to tell the engineers about Digital Unix
        # _or_ OSF/1 ...
            sbcl_os="osf1"
            ;;
	*BSD)
            case `uname` in
		FreeBSD)
                    sbcl_os="freebsd"
                    ;;
		OpenBSD)
                    sbcl_os="openbsd"
                    ;;
		NetBSD)
                    sbcl_os="netbsd"
                    ;;
		*)
                    echo unsupported BSD variant: `uname`
                    exit 1
                    ;;
            esac
            ;;
	Darwin)
            sbcl_os="darwin"
            ;;
	SunOS)
            sbcl_os="sunos"
            ;;
	CYGWIN* | WindowsNT | MINGW*)
            sbcl_os="win32"
            ;;
	HP-UX)
            sbcl_os="hpux"
            ;;
	*)
            echo unsupported OS type: `uname`
            exit 1
            ;;
    esac
    case `uname -m` in
	*86) guessed_sbcl_arch=x86 ;;
	i86pc) guessed_sbcl_arch=x86 ;;
	*x86_64) guessed_sbcl_arch=x86-64 ;;
	amd64) guessed_sbcl_arch=x86-64 ;;
	[Aa]lpha) guessed_sbcl_arch=alpha ;;
	sparc*) guessed_sbcl_arch=sparc ;;
	sun*) guessed_sbcl_arch=sparc ;;
	*ppc) guessed_sbcl_arch=ppc ;;
	ppc64) guessed_sbcl_arch=ppc ;;
	Power*Macintosh) guessed_sbcl_arch=ppc ;;
	parisc) guessed_sbcl_arch=hppa ;;
	9000/800) guessed_sbcl_arch=hppa ;;
	mips*) guessed_sbcl_arch=mips ;;
	*)
        # If we're not building on a supported target architecture, we
        # we have no guess, but it's not an error yet, since maybe
        # target architecture will be specified explicitly below.
            guessed_sbcl_arch=''
            ;;
    esac
    # Under Solaris, uname -m returns "i86pc" even if CPU is amd64.
    if [ "$sbcl_os" = "sunos" ] && [ `isainfo -k` = "amd64" ]; then
	guessed_sbcl_arch=x86-64
    fi

    # Under Darwin, uname -m returns "i386" even if CPU is x86_64.
    if [ "$sbcl_os" = "darwin" ] &&
	[ "`/usr/sbin/sysctl -n hw.optional.x86_64`" = "1" ] ; then
	guessed_sbcl_arch=x86-64
    fi

    sbcl_arch=${SBCL_ARCH:-$guessed_sbcl_arch}
    if [ "$sbcl_arch" = "" ] ; then
	echo "can't guess target SBCL architecture, need SBCL_ARCH environment var"
	exit 1
    fi
fi

# Under Darwin x86-64, guess whether Darwin 9+ or below.
if [ "$sbcl_os" = "darwin" ] && [ "$sbcl_arch" = "x86-64" ]; then
    darwin_version=`uname -r`
    darwin_version_major=${DARWIN_VERSION_MAJOR:-${darwin_version%%.*}}
fi

link_or_copy() {
    $lncp "$1" "$2"
}

