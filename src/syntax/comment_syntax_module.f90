! Comment tokens per language, keyed on filename.
!
! This is deliberately independent of syntax_highlighter_module: the
! highlighter sets enabled=.false. for extensions it has no theme for, so it
! has no token to offer for the majority of files. Toggling a comment has to
! work regardless of whether the file is highlighted, and LSP is no help here
! either -- the protocol has no "what is a comment in this language" request,
! which is why VSCode ships static per-language configuration for it too.
module comment_syntax_module
    implicit none
    private

    public :: comment_syntax_t, get_comment_syntax, has_comment_syntax

    type :: comment_syntax_t
        ! Line-comment token, e.g. '//' or '#'. Empty when the language has
        ! none (html, css); callers fall back to the block tokens.
        character(len=8) :: line = ''
        ! Block-comment delimiters, e.g. '/*' and '*/'. Both empty when the
        ! language has no block form.
        character(len=8) :: block_start = ''
        character(len=8) :: block_end = ''
    end type comment_syntax_t

contains

    ! Resolve comment tokens for a path. Unknown files get an all-empty
    ! result; has_comment_syntax() reports whether anything usable came back.
    function get_comment_syntax(filename) result(syn)
        character(len=*), intent(in) :: filename
        type(comment_syntax_t) :: syn
        character(len=:), allocatable :: basename, extension, lower_base
        integer :: slash_pos, dot_pos

        slash_pos = index(filename, '/', back=.true.)
        if (slash_pos > 0) then
            basename = trim(filename(slash_pos+1:))
        else
            basename = trim(filename)
        end if
        if (len(basename) == 0) return

        lower_base = to_lower(basename)

        ! Extensionless files (and files whose extension is a red herring,
        ! like CMakeLists.txt) are matched by name first.
        select case(lower_base)
        case('makefile', 'gnumakefile', 'dockerfile', 'containerfile', &
             'jenkinsfile', 'vagrantfile', 'brewfile', 'gemfile', 'rakefile', &
             'procfile', 'pkgbuild', 'cmakelists.txt', '.gitignore', &
             '.gitattributes', '.dockerignore', '.editorconfig', '.env')
            syn%line = '#'
            return
        case('.bashrc', '.bash_profile', '.zshrc', '.profile', '.inputrc')
            syn%line = '#'
            return
        end select

        dot_pos = index(basename, '.', back=.true.)
        if (dot_pos <= 0) return
        extension = to_lower(basename(dot_pos:))

        select case(extension)

        ! ---- C family and friends: // with /* */ ----
        case('.c', '.h', '.cpp', '.cc', '.cxx', '.c++', '.hpp', '.hxx', '.hh', &
             '.h++', '.ino', '.cu', '.cuh', '.m', '.mm', &
             '.java', '.kt', '.kts', '.scala', '.sc', '.groovy', '.gradle', &
             '.cs', '.fs', '.fsx', '.go', '.rs', '.swift', '.dart', '.zig', &
             '.js', '.jsx', '.mjs', '.cjs', '.ts', '.tsx', '.mts', '.cts', &
             '.json5', '.jsonc', '.php', '.d', '.v', '.sv', '.svh', '.vh', &
             '.pde', '.as', '.hx', '.qml', '.proto', '.thrift', '.glsl', &
             '.vert', '.frag', '.geom', '.comp', '.hlsl', '.metal', '.wgsl', &
             '.jq', '.pas', '.pp', '.less', '.sass', '.scss', '.styl', &
             '.rego', '.sol', '.gohtml')
            syn%line = '//'
            syn%block_start = '/*'
            syn%block_end = '*/'

        ! ---- Wolf: // only, no block form ----
        ! Kept out of the C-family list above on purpose. Wolf has no block
        ! comment at all, so a '/*' here is two operator tokens; handing the
        ! toggle a block form it could fall back to would let it write a
        ! comment the compiler does not recognise.
        case('.lu', '.wolfi')
            syn%line = '//'

        ! ---- Hash-comment languages ----
        case('.py', '.pyw', '.pyi', '.rb', '.rbw', '.gemspec', '.pl', '.pm', &
             '.t', '.raku', '.rakumod', '.sh', '.bash', '.zsh', '.ksh', &
             '.ash', '.fish', '.nu', '.r', '.jl', '.cr', '.nim', '.nims', &
             '.ex', '.exs', '.eex', '.heex', '.tcl', '.awk', '.sed', '.mk', &
             '.mak', '.make', '.cmake', '.yml', '.yaml', '.toml', '.ini', &
             '.cfg', '.conf', '.properties', '.dockerfile', '.gitconfig', &
             '.gitmodules', '.env', '.tf', '.tfvars', '.hcl', '.nix', &
             '.ps1', '.psm1', '.psd1', '.gd', '.gni', '.gn', '.bzl', &
             '.bazel', '.starlark', '.star', '.pkgbuild', '.spec', '.desktop', &
             '.service', '.socket', '.timer', '.rules', '.pri', '.pro', &
             '.qrc', '.editorconfig', '.coffee', '.elv')
            syn%line = '#'

        ! ---- Fortran ----
        case('.f90', '.f95', '.f03', '.f08', '.f18', '.f', '.for', '.ftn', &
             '.fpp', '.finc')
            syn%line = '!'

        ! ---- Lisp family ----
        case('.lisp', '.lsp', '.cl', '.el', '.scm', '.ss', '.rkt', '.clj', &
             '.cljs', '.cljc', '.edn', '.fnl')
            syn%line = ';'

        ! ---- SQL-ish ----
        case('.sql', '.psql', '.mysql', '.plsql', '.hql', '.ada', '.adb', &
             '.ads', '.applescript', '.lua', '.hs', '.lhs', '.elm', '.purs', &
             '.idr', '.agda')
            select case(extension)
            case('.lua')
                syn%line = '--'
                syn%block_start = '--[['
                syn%block_end = ']]'
            case('.hs', '.lhs', '.elm', '.purs', '.idr', '.agda')
                syn%line = '--'
                syn%block_start = '{-'
                syn%block_end = '-}'
            case('.sql', '.psql', '.mysql', '.plsql', '.hql')
                syn%line = '--'
                syn%block_start = '/*'
                syn%block_end = '*/'
            case default
                syn%line = '--'
            end select

        ! ---- Erlang ----
        case('.erl', '.hrl', '.escript')
            syn%line = '%'

        ! ---- TeX / matlab / postscript ----
        case('.tex', '.sty', '.cls', '.dtx', '.bib', '.mat', '.ps', '.eps')
            syn%line = '%'

        ! ---- Vim ----
        case('.vim', '.vimrc')
            syn%line = '"'

        ! ---- Assembly. Intel-syntax assemblers use ';'; GNU as (.s/.S)
        ! treats '#' as the comment character on most targets.
        case('.asm', '.nasm')
            syn%line = ';'
        case('.s')
            syn%line = '#'

        ! ---- VHDL ----
        case('.vhd', '.vhdl')
            syn%line = '--'

        ! ---- Batch ----
        case('.bat', '.cmd')
            syn%line = 'REM'

        ! ---- Block-only languages ----
        case('.css')
            syn%block_start = '/*'
            syn%block_end = '*/'
        case('.html', '.htm', '.xhtml', '.xml', '.svg', '.xsl', '.xslt', &
             '.vue', '.svelte', '.md', '.markdown', '.plist', '.ui', '.rss', &
             '.atom', '.xaml', '.resx')
            syn%block_start = '<!--'
            syn%block_end = '-->'

        end select
    end function get_comment_syntax

    ! True when the language has at least one usable comment form.
    function has_comment_syntax(syn) result(res)
        type(comment_syntax_t), intent(in) :: syn
        logical :: res
        res = len_trim(syn%line) > 0 .or. &
              (len_trim(syn%block_start) > 0 .and. len_trim(syn%block_end) > 0)
    end function has_comment_syntax

    function to_lower(str) result(res)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: res
        integer :: i, code

        do i = 1, len(str)
            code = iachar(str(i:i))
            if (code >= iachar('A') .and. code <= iachar('Z')) then
                res(i:i) = achar(code + 32)
            else
                res(i:i) = str(i:i)
            end if
        end do
    end function to_lower

end module comment_syntax_module
